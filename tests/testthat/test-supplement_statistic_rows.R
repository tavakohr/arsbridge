## S-3: a supplement may propose statistic MEANING, never an executable binding.
##
## General defect class: when the label grammar cannot read a statistic row,
## arsbridge refuses it -- correctly -- and there was no channel by which
## anyone could say what the row means. A sponsor dialect the vocabulary did
## not know could only be fixed by editing the shell or shipping a release.
##
## General invariant: a supplement names SEMANTIC statistics, never an ARS
## operation and never an engine statistic name; only `status = "reviewed"` is
## applied; the row's METHOD still decides whether the request can be met; and
## where the label also resolved, the deterministic reading wins unless that
## one row carries a reviewed override.

CONT <- "MTH_SUMMARY_STATISTICS_CONTINUOUS"

## A statistic row under a continuous parent, resolved directly. `entry` is the
## layout record the fill stage sees; `stat_tokens` is what a REVIEWED
## supplement put there.
.s3_bind <- function(label, tokens = NULL, override = FALSE, method_id = CONT,
                     scope = "") {
  entry <- list(label = label, sheet_row = 6L)
  if (!is.null(tokens)) {
    entry$stat_tokens <- tokens
    entry$stat_tokens_source <- "supplement"
    entry$stat_tokens_override <- override
  }
  if (nzchar(scope)) entry$stat_expected_scope <- scope
  .fill_row_binding(
    entry  = entry,
    parent = list(analysis_id = "AN_1", label = "Measure [ADQX.MEAS]"),
    methods  = .STANDARD_METHODS,
    analyses = list(list(id = "AN_1", methodId = method_id)))
}

.s3_ops <- function(b) vapply(b$stats, function(s) s$operation_id, character(1))

## A minimal two-column table sheet holding one statistic row under one
## analysis, so the diagnostics `.build_table_cells()` emits can be read the
## way a reader actually receives them. Two columns on purpose: a per-row
## report must not fire once per cell.
.s3_columns <- function() {
  list(list(col = 2L, order = 1L, label = "Drug A"),
       list(col = 3L, order = 2L, label = "Placebo"))
}

.s3_slot <- function() {
  list(list(token = "xx.x", type = "num", decimals = 1L, start = 1L, stop = 4L))
}

.s3_cells <- function(scope = "", tokens = c("q1", "q3"), method_id = CONT) {
  entry <- list(order = 2L, label = "Interquartile spread", indent = 0L,
                analysis_id = NA_character_, kind = "label_row", sheet_row = 6L,
                stat_tokens = tokens, stat_tokens_source = "supplement",
                stat_tokens_override = FALSE)
  if (nzchar(scope)) entry$stat_expected_scope <- scope
  layout <- list(
    list(order = 1L, label = "Measure [ADQX.MEAS]", indent = 0L,
         analysis_id = "AN_1", kind = "label_row", sheet_row = 5L),
    entry)
  grid <- data.frame(row = c(6L, 6L), col = c(2L, 3L), ref = c("B6", "C6"),
                     kind = "placeholder", n_slots = 1L,
                     stringsAsFactors = FALSE)
  grid$slots <- list(.s3_slot(), .s3_slot())
  .build_table_cells(
    section      = list(sheet_name = "T14-2-1", tlf_number = "14.2.1"),
    shell_layout = layout,
    analyses     = list(list(id = "AN_1", methodId = method_id)),
    methods      = .STANDARD_METHODS,
    columns      = .s3_columns(),
    grid         = grid)
}

.s3_rows <- function(...) .supp_statistic_rows(list(statisticRows = list(...)))

.s3_row <- function(label, tokens, status = "reviewed", source = "llm",
                    reviewer = "H.T.", ...) {
  out <- list(row_label = label, semantic_tokens = as.list(tokens),
              status = status, source = source, ...)
  if (nzchar(reviewer)) out$reviewed_by <- reviewer
  out
}


test_that("a supplement may name statistics but never operations or engine names", {
  ok <- .s3_rows(.s3_row("Adjusted mean (95% CI)", c("mean", "ci_low", "ci_high")))
  expect_equal(length(ok$rows), 1L)
  expect_equal(ok$rows[[1]]$semantic_tokens, c("mean", "ci_low", "ci_high"))
  expect_equal(length(ok$problems), 0L)

  ## An ARS operation id gets its own message: reaching one layer too far down
  ## is the commonest proposer mistake, and "unknown statistic" would not tell
  ## the author what to do about it.
  op <- .s3_rows(.s3_row("X", c("OP_MEAN")))
  expect_equal(length(op$rows), 0L)
  expect_match(op$problems[[1]], "operation id", fixed = TRUE)

  ## Engine-only names are rejected by the same closed enum.
  for (name in c("conf.low", "p25", "N", "p")) {
    bad <- .s3_rows(.s3_row("X", c(name)))
    expect_equal(length(bad$rows), 0L, info = name)
    expect_match(bad$problems[[1]], "unknown statistic", info = name)
  }

  ## But the six tokens that SHARE a spelling with an engine name are ordinary
  ## valid tokens -- a blanket "no engine names" rule would reject the
  ## commonest entry the format can carry.
  shared <- .s3_rows(.s3_row("X", c("mean", "sd", "median", "min", "max", "events")))
  expect_equal(length(shared$rows), 1L)
  expect_equal(length(shared$problems), 0L)
})


test_that("a malformed entry is reported, never crashed on", {
  ## Each of these is a shape that turns a validation run into an ERROR rather
  ## than a finding -- which loses every other finding in the file with it.
  bad <- .s3_rows(
    list(row_label = "A", semantic_tokens = list("mean"),
         status = list("reviewed"), source = "llm", reviewed_by = "x"),
    list(row_label = "B", semantic_tokens = "mean",
         status = "reviewed", source = "llm", reviewed_by = "x"),
    list(row_label = "C", semantic_tokens = list("mean"),
         status = "reviewed", source = "llm"),
    list(row_label = "D", semantic_tokens = list("mean"),
         status = "proposal", source = "llm", override = TRUE),
    list(row_label = "", semantic_tokens = list("mean"),
         status = "reviewed", source = "llm", reviewed_by = "x"),
    list(row_label = "F", semantic_tokens = list(),
         status = "reviewed", source = "llm", reviewed_by = "x"))
  expect_equal(length(bad$rows), 0L)
  expect_equal(length(bad$problems), 6L)
  expect_true(any(grepl("reviewed_by", bad$problems)))
  expect_true(any(grepl("override", bad$problems)))
  expect_true(any(grepl("ARRAY", bad$problems)))
})


test_that("a reviewed row answers a label the grammar could not read", {
  none <- .s3_bind("Interquartile spread")
  expect_true(isTRUE(none$unreadable))
  expect_equal(length(none$stats), 0L)

  ## With it, the row binds -- to the OPERATIONS the method declares, chosen
  ## by arsbridge, not named by the supplement.
  got <- .s3_bind("Interquartile spread", tokens = c("q1", "q3"))
  expect_equal(.s3_ops(got), c("OP_Q1", "OP_Q3"))
  expect_equal(got$token_source, "supplement")
  expect_null(got$conflict)
})


test_that("a proposal never binds, whatever it says", {
  sec <- list(tlf_number = "14.2.1",
              stub_rows = list(list(label = "Interquartile spread")))
  applied <- suppressMessages(.apply_supplement_statistic_rows(
    sec, list(statisticRows = list(
      .s3_row("Interquartile spread", c("q1", "q3"),
              status = "proposal", reviewer = "")))))
  expect_null(applied$stub_rows[[1]]$supplement_stat_tokens)
  ## It IS recorded, so a reviewer can see what was offered.
  expect_equal(applied$stub_rows[[1]]$supplement_stat_proposal$status, "proposal")

  reviewed <- suppressMessages(.apply_supplement_statistic_rows(
    sec, list(statisticRows = list(.s3_row("Interquartile spread", c("q1", "q3"))))))
  expect_equal(reviewed$stub_rows[[1]]$supplement_stat_tokens, c("q1", "q3"))
})


test_that("the method gates a reviewed request exactly as it gates a label", {
  ## Review makes a request legible, not possible. No reviewer can make an
  ## engine produce a statistic the method has no operation for.
  got <- .s3_bind("Interquartile spread", tokens = c("se"))
  expect_equal(length(got$stats), 0L)
  expect_equal(got$unsupported, "se")
  expect_true("OP_MEAN" %in% got$available)

  ## The refusal is whole: a request the method can only PARTLY meet binds
  ## nothing, or the remaining placeholders shift onto wrong statistics.
  part <- .s3_bind("Interquartile spread", tokens = c("q1", "se"))
  expect_equal(length(part$stats), 0L)
  expect_equal(part$unsupported, "se")
})


test_that("the label wins a disagreement unless that row is overridden", {
  ## fill_gaps, the default: the deterministic reading stands and both sides
  ## are recorded, so the disagreement is visible rather than silent.
  keep <- .s3_bind("Mean (SD)", tokens = c("median"))
  expect_equal(.s3_ops(keep), c("OP_MEAN", "OP_SD"))
  expect_equal(keep$token_source, "grammar")
  expect_equal(keep$conflict$resolved_to, "grammar")
  expect_equal(keep$conflict$grammar, c("mean", "sd"))
  expect_equal(keep$conflict$supplement, "median")

  ## Per-row reviewed override: the supplement replaces a reading that DID
  ## resolve. Still recorded, and still gated by the method.
  over <- .s3_bind("Mean (SD)", tokens = c("median"), override = TRUE)
  expect_equal(.s3_ops(over), "OP_MEDIAN")
  expect_equal(over$token_source, "supplement")
  expect_equal(over$conflict$resolved_to, "supplement")

  ## Agreement is not a conflict, and the grammar is credited: it needed no
  ## review to get there.
  same <- .s3_bind("Mean (SD)", tokens = c("mean", "sd"))
  expect_null(same$conflict)
  expect_equal(same$token_source, "grammar")
})


test_that("a categorical parent never consults a supplement statistic row", {
  ## The S-2 invariant, still holding: under a per-category method an
  ## unannotated child is a LEVEL, and no channel changes that. A supplement
  ## mislabelling a codelist value as a statistic must not turn a level row
  ## into a statistic row.
  lvl <- .s3_bind("Median", tokens = c("median"),
                  method_id = "MTH_COUNT_AND_PERCENTAGE")
  expect_equal(lvl$variable_level, "Median")
  expect_null(lvl$token_source)
  expect_null(lvl$conflict)
})


test_that("two entries claiming one row resolve to unresolved, not first-match", {
  sec <- list(tlf_number = "14.2.1",
              stub_rows = list(list(label = "Interquartile spread")))
  applied <- suppressMessages(.apply_supplement_statistic_rows(
    sec, list(statisticRows = list(
      .s3_row("Interquartile spread", c("q1", "q3")),
      .s3_row("Interquartile spread", c("min", "max"))))))
  expect_null(applied$stub_rows[[1]]$supplement_stat_tokens)
})


test_that("provenance is recorded only when something other than the label answered", {
  ## An absent field means the label was read. Stamping every statistic cell
  ## with "grammar" would say nothing new and would move the cell map of every
  ## shell that has one.
  expect_equal(.s3_bind("Mean (SD)")$token_source, "grammar")
  expect_equal(.s3_bind("Interquartile spread", tokens = c("q1", "q3"))$token_source,
               "supplement")
})


test_that("nothing in this channel reaches an LLM, and no code writes 'reviewed'", {
  ## The whole path runs with every provider key unset. If any part of it
  ## called out, this would abort rather than resolve.
  withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""), {
      got <- .s3_bind("Interquartile spread", tokens = c("q1", "q3"))
      expect_equal(.s3_ops(got), c("OP_Q1", "OP_Q3"))
    })

  ## `source = "llm"` is provenance and never a permission: an LLM-sourced row
  ## binds when reviewed and does not when it is not, exactly like any other.
  expect_equal(.s3_bind("Interquartile spread", tokens = c("q1", "q3"))$token_source,
               "supplement")

  ## And the package itself never writes the status that binds. A reviewed row
  ## can only come from outside, which is what makes review a human act.
  ##
  ## `test_path()` is rooted at tests/testthat, so two levels up is the package
  ## root when the suite runs from source. An INSTALLED package still HAS an
  ## R/ directory -- it holds the lazy-load database, not sources -- so the
  ## condition is whether .R files are actually there to read, never whether
  ## the directory exists. A scan of nothing would pass vacuously, so that
  ## case skips rather than pretending to have checked.
  r_dir <- testthat::test_path("..", "..", "R")
  files <- if (dir.exists(r_dir)) {
    list.files(r_dir, pattern = "[.]R$", full.names = TRUE)
  } else {
    character(0)
  }
  skip_if(length(files) == 0L, "package sources not available (installed run)")
  expect_gt(length(files), 20L)
  src <- unlist(lapply(files, readLines, warn = FALSE))
  ## Comments are dropped first. The invariant is about executable code, and
  ## the prose in this package DISCUSSES `status = "reviewed"` at length --
  ## scanning it would flag the documentation of the rule as a breach of it.
  code <- grep("^\\s*#", src, value = TRUE, invert = TRUE)
  writes <- grep('status\\s*(<-|=)\\s*"reviewed"', code, value = TRUE)
  expect_equal(length(writes), 0L)
  ## Non-vacuity, on the CODE rather than the file: a scan that stripped
  ## everything would pass this trivially.
  expect_gt(length(code), 1000L)
})


test_that("the shipped schema describes exactly the tokens the code accepts", {
  ## Drift detection. The enum is generated from `.STAT_TOKENS`; if the two
  ## part company, a supplement the code accepts fails its own schema (or the
  ## reverse), and the shipped document stops describing the package.
  path <- system.file("schema", .SUPPLEMENT_SCHEMA_FILE, package = "arsbridge")
  if (!nzchar(path)) path <- file.path("../../inst/schema", .SUPPLEMENT_SCHEMA_FILE)
  skip_if_not(file.exists(path), "shipped schema not available")
  schema <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  row <- schema$definitions$statisticRow
  expect_false(is.null(row))
  enum <- as.character(unlist(row$properties$semantic_tokens$items$enum))
  expect_equal(enum, .STAT_TOKENS)
  expect_equal(length(enum), 16L)
  ## The field is reachable from a TLF entry, or nothing could ever use it.
  expect_false(is.null(schema$definitions$tlfEntry$properties$statisticRows))
})

## --- FAIL must mean no binding ---------------------------------------------

test_that("a fatal statisticRows finding drops the entry and binds nothing", {
  ## General defect class: a validator can report a finding and then apply the
  ## entry anyway. The diagnostic then states the opposite of what happened --
  ## "the entry was dropped" while its tokens sit on a row -- and a reviewer
  ## reading the FAIL believes a cell is unresolved when it is bound.
  ##
  ## General invariant: every problem this parser reports is FATAL. An entry
  ## that produced one is not returned as applicable and cannot reach a stub
  ## row, so severity and action text describe what actually happened.
  base <- function(...) .s3_row("Interquartile spread", c("q1", "q3"), ...)
  sec  <- list(tlf_number = "14.2.1",
               stub_rows = list(list(label = "Interquartile spread")))
  apply_one <- function(entry) suppressMessages(
    .apply_supplement_statistic_rows(sec, list(statisticRows = list(entry))))

  ## Non-vacuity first: the entry every case below is built from DOES bind, so
  ## a case that stops binding stopped for the reason under test rather than
  ## because the fixture was malformed to begin with.
  expect_equal(apply_one(base())$stub_rows[[1]]$supplement_stat_tokens,
               c("q1", "q3"))

  fatal <- list(
    ## A field the format does not define -- usually a misspelling of one that
    ## DECIDES something, where "ignored" binds the row without the qualifier
    ## its author wrote.
    "unknown field"         = base(overide = TRUE),
    "unknown field (prose)" = base(comment = "looks right to me"),
    ## A scope outside the enum: the cross-check it asks for cannot run.
    "unknown scope"         = base(expected_scope = "contnuous"),
    ## And a scope that is not a scalar at all. `.supp_scalar()` returns NA
    ## here, and reading NA as "absent" would bind while silently discarding
    ## the safeguard the author attached.
    "non-scalar scope"      = base(expected_scope = list("continuous")))

  for (nm in names(fatal)) {
    parsed <- .s3_rows(fatal[[nm]])
    expect_equal(length(parsed$rows), 0L, info = nm)
    expect_gt(length(parsed$problems), 0L)

    applied <- apply_one(fatal[[nm]])
    expect_null(applied$stub_rows[[1]]$supplement_stat_tokens, info = nm)
    expect_null(applied$stub_rows[[1]]$supplement_stat_proposal, info = nm)
    expect_null(applied$stub_rows[[1]]$supplement_stat_scope, info = nm)
  }

  ## The diagnostic agrees with the runtime behaviour, which is the whole
  ## point: FAIL, and an action that says the entry was dropped.
  diag_reset()
  invisible(apply_one(base(overide = TRUE)))
  recs <- ars_diagnostics()
  hit  <- recs[recs$severity == "FAIL" & grepl("statisticRows", recs$problem), ]
  expect_gt(nrow(hit), 0L)
  expect_true(any(grepl("dropped", hit$action, fixed = TRUE)))
  diag_reset()
})


## --- one stub row, one entry -----------------------------------------------

test_that("two labels resolving to one stub row bind nothing, not last-write-wins", {
  ## General defect class: ambiguity was detected on the text a supplement
  ## SUPPLIED rather than on the row it RESOLVES to. The matcher accepts
  ## prefix and containment, so two spellings reach one row, pass a check that
  ## compares only the supplied labels, and the second overwrites the first.
  ##
  ## General invariant: two entries claiming one actual stub row is ambiguity,
  ## and ambiguity resolves to unresolved -- neither binds.
  sec <- list(tlf_number = "14.2.1",
              stub_rows = list(list(label = "Mean (SD)")))

  ## Scope assertion: these two labels really do collide on one row. Were the
  ## matcher ever made exact-only, this goes red rather than the test passing
  ## while checking nothing.
  stub <- .norm_label("Mean (SD)")
  expect_equal(.match_stub_label(.norm_label("Mean"), stub), 1L)
  expect_equal(.match_stub_label(.norm_label("Mean (SD)"), stub), 1L)

  ## And each binds on its own, so the collision case below is not passing
  ## merely because one of them never bound.
  for (lbl in c("Mean", "Mean (SD)")) {
    solo <- suppressMessages(.apply_supplement_statistic_rows(
      sec, list(statisticRows = list(.s3_row(lbl, c("mean", "sd"))))))
    expect_equal(solo$stub_rows[[1]]$supplement_stat_tokens, c("mean", "sd"),
                 info = lbl)
  }

  both <- suppressMessages(.apply_supplement_statistic_rows(
    sec, list(statisticRows = list(
      .s3_row("Mean", c("mean", "sd")),
      .s3_row("Mean (SD)", c("min", "max"))))))
  expect_null(both$stub_rows[[1]]$supplement_stat_tokens)
  expect_null(both$stub_rows[[1]]$supplement_stat_proposal)

  ## Order is not a tie-breaker: reversing the entries changes nothing.
  rev <- suppressMessages(.apply_supplement_statistic_rows(
    sec, list(statisticRows = list(
      .s3_row("Mean (SD)", c("min", "max")),
      .s3_row("Mean", c("mean", "sd"))))))
  expect_null(rev$stub_rows[[1]]$supplement_stat_tokens)

  ## Reported as a contested ROW, naming both claimants.
  diag_reset()
  invisible(suppressMessages(.apply_supplement_statistic_rows(
    sec, list(statisticRows = list(
      .s3_row("Mean", c("mean", "sd")),
      .s3_row("Mean (SD)", c("min", "max")))))))
  recs <- ars_diagnostics()
  hit  <- recs[recs$severity == "FAIL" & grepl("resolve to it", recs$problem), ]
  expect_equal(nrow(hit), 1L)
  ## Guarded: indexing a zero-row frame ERRORS, and an error reads as "the
  ## mutant was not detected" -- the one direction that hides a real gap.
  if (nrow(hit) == 1L) {
    expect_match(hit$problem[[1]], "'Mean'", fixed = TRUE)
    expect_match(hit$problem[[1]], "'Mean (SD)'", fixed = TRUE)
  }
  diag_reset()

  ## Two entries on two different rows are ordinary and unaffected.
  two <- list(tlf_number = "14.2.1",
              stub_rows = list(list(label = "Mean (SD)"),
                               list(label = "Interquartile spread")))
  ok <- suppressMessages(.apply_supplement_statistic_rows(
    two, list(statisticRows = list(
      .s3_row("Mean (SD)", c("mean", "sd")),
      .s3_row("Interquartile spread", c("q1", "q3"))))))
  expect_equal(ok$stub_rows[[1]]$supplement_stat_tokens, c("mean", "sd"))
  expect_equal(ok$stub_rows[[2]]$supplement_stat_tokens, c("q1", "q3"))
})


## --- the contract's smaller promises ---------------------------------------

test_that("expected_scope is cross-checked against the method and never applied", {
  ## The schema calls it "a cross-check, never an input: a disagreement is
  ## reported and never overrides the method". So it has to be checked, the
  ## disagreement has to reach a diagnostic, and it must change nothing.
  sec <- list(tlf_number = "14.2.1",
              stub_rows = list(list(label = "Interquartile spread")))
  applied <- suppressMessages(.apply_supplement_statistic_rows(
    sec, list(statisticRows = list(
      .s3_row("Interquartile spread", c("q1", "q3"),
              expected_scope = "categorical")))))
  expect_equal(applied$stub_rows[[1]]$supplement_stat_scope, "categorical")

  ## Declared scope disagrees with the method: reported, and the tokens still
  ## bind exactly as they would without it.
  bad <- .s3_bind("Interquartile spread", tokens = c("q1", "q3"),
                  scope = "categorical")
  expect_equal(.s3_ops(bad), c("OP_Q1", "OP_Q3"))
  expect_equal(bad$scope_conflict$declared, "categorical")
  expect_equal(bad$scope_conflict$actual, "continuous")

  ## Agreement is not a conflict, and an entry that declared nothing has
  ## nothing to check.
  expect_null(.s3_bind("Interquartile spread", tokens = c("q1", "q3"),
                       scope = "continuous")$scope_conflict)
  expect_null(.s3_bind("Interquartile spread", tokens = c("q1", "q3"))$scope_conflict)

  ## Checked in BOTH directions: a per-category parent reports it too, which
  ## is the half that never fires if the check sits below the decode gate.
  cat_side <- .s3_bind("Median", tokens = c("median"), scope = "continuous",
                       method_id = "MTH_COUNT_AND_PERCENTAGE")
  expect_equal(cat_side$variable_level, "Median")
  expect_equal(cat_side$scope_conflict$actual, "categorical")

  ## And it reaches the reader as a WARN naming the row, the declaration and
  ## the method -- a cross-check nobody is told about is not a cross-check.
  diag_reset()
  invisible(.s3_cells(scope = "categorical"))
  recs <- ars_diagnostics()
  hit  <- recs[recs$severity == "WARN" & grepl("expected_scope", recs$problem), ]
  expect_equal(nrow(hit), 1L)
  if (nrow(hit) == 1L) {
    expect_match(hit$problem[[1]], "categorical", fixed = TRUE)
    expect_match(hit$problem[[1]], CONT, fixed = TRUE)
  }
  diag_reset()

  ## One report per sheet row, not one per cell: the harness above has two
  ## result columns.
  expect_gt(length(.s3_columns()), 1L)
})


test_that("proposed_at survives the parse", {
  ## It is in the schema and in the field whitelist, so an author can write
  ## it, watch validation pass, and reasonably expect it to still be there.
  got <- .s3_rows(.s3_row("Interquartile spread", c("q1", "q3"),
                          proposed_at = "2026-08-18T09:00:00Z"))
  expect_equal(length(got$rows), 1L)
  expect_equal(got$rows[[1]]$proposed_at, "2026-08-18T09:00:00Z")
  ## Absent is empty, never NA: it is provenance text, and NA would print.
  expect_equal(.s3_rows(.s3_row("X", c("mean")))$rows[[1]]$proposed_at, "")
})
