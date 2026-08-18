## The statistic-label grammar: what a label asks for, and what a method can
## actually provide.
##
## General defect class: a shell row's statistics were decided by matching its
## label against a closed list of exact spellings. Any other way of writing the
## same statistics matched nothing, the row was reclassified as a category
## level, and its placeholder slots were bound POSITIONALLY to the method's
## first operations -- so a "Q1; Q3" line filled with a count and a mean. The
## number written was real, correctly formatted, and the wrong statistic.
##
## General invariant, in three parts:
##
##   1. A label names SEMANTIC statistics, in the order it names them,
##      independent of spelling, separator, bracketing, decoration or Unicode
##      form. It never names an operation and never names an engine statistic.
##   2. Whether those statistics can be produced is the METHOD's answer, not
##      the label's. A statistic the method does not declare refuses the whole
##      row -- every slot of it -- with a diagnostic naming the label, the
##      statistic, the method, and what that method does declare.
##   3. No recognised label ever falls through to positional matching.
##
## The assurance being tested is not "it recognises many labels". It is: for
## every recognised label the operation set is correct, and for every
## unsupported or ambiguous label nothing is bound and the reason is reported.

CONT <- "MTH_SUMMARY_STATISTICS_CONTINUOUS"
CATG <- "MTH_COUNT_AND_PERCENTAGE"
KM   <- "MTH_KAPLAN_MEIER_ESTIMATE"

## A parent analysis and one unannotated child row under it, which is the
## shape every statistic line has.
.slg_bind <- function(label, method_id, kind = NULL) {
  analyses <- list(list(id = "AN_1", methodId = method_id))
  .fill_row_binding(
    entry   = list(label = label, analysis_id = NA_character_, kind = kind),
    parent  = list(analysis_id = "AN_1", label = "parent"),
    methods = .STANDARD_METHODS,
    analyses = analyses)
}

## The operation ids a label binds under one method, or character(0) when it
## binds nothing.
.slg_ops <- function(label, method_id) {
  b <- .slg_bind(label, method_id)
  vapply(b$stats %||% list(),
         function(s) as.character(s$operation_id %||% NA_character_),
         character(1))
}


## ---------------------------------------------------------------------------
## Stage 1: the label, on its own
## ---------------------------------------------------------------------------

test_that("n, percentage and n (%) are three different requests", {
  ## The distinction the whole design turns on. These are one, one, and two
  ## statistics -- not one visible text resolved three ways.
  expect_equal(.parse_stat_label("n"), "count")
  expect_equal(.parse_stat_label("%"), "pct")
  expect_equal(.parse_stat_label("n (%)"), c("count", "pct"))

  ## Order is the label's, because slot i binds statistic i.
  expect_equal(.parse_stat_label("(%) n"), c("pct", "count"))

  ## And the spellings authors actually use for each.
  for (l in c("count", "Number of subjects", "Non-missing")) {
    expect_equal(.parse_stat_label(l), "count", info = l)
  }
  for (l in c("Percent", "Percentage", "Proportion", "Column %")) {
    expect_equal(.parse_stat_label(l), "pct", info = l)
  }
})


test_that("paired statistics survive any punctuation and spacing", {
  ## One pair, written every way a sponsor might write it. The assertion is
  ## that they are IDENTICAL -- this is what proves the grammar keys on the
  ## statistic rather than on a spelling somebody once used.
  quartiles <- c("Q1, Q3", "Q1; Q3", "Q1/Q3", "Q1 - Q3", "Q1 – Q3",
                 "Q1 — Q3", "Q1,Q3", "Q1  ;  Q3", "[Q1, Q3]",
                 "(Q1, Q3)", "Q1 to Q3", "Q1 and Q3",
                 "25th percentile, 75th percentile",
                 "Lower quartile, Upper quartile",
                 "Q1 ; Q3", "Q1;​Q3")
  for (l in quartiles) {
    expect_equal(.parse_stat_label(l), c("q1", "q3"), info = l)
  }
  expect_gt(length(quartiles), 10L)

  ranges <- c("Min, Max", "Min-Max", "Min – Max", "Min; Max",
              "Min to Max", "Minimum, Maximum", "Range")
  for (l in ranges) {
    expect_equal(.parse_stat_label(l), c("min", "max"), info = l)
  }

  spreads <- c("Mean (SD)", "Mean ± SD", "Mean +/- SD", "Mean, SD",
               "Mean (standard deviation)", "Mean (Std Dev)")
  for (l in spreads) {
    expect_equal(.parse_stat_label(l), c("mean", "sd"), info = l)
  }
})


test_that("order follows the label, and is never sorted", {
  expect_equal(.parse_stat_label("Q1, Q3"), c("q1", "q3"))
  expect_equal(.parse_stat_label("Q3, Q1"), c("q3", "q1"))
  expect_equal(.parse_stat_label("Max, Min"), c("max", "min"))
  expect_equal(.parse_stat_label("Median (Q3, Q1)"), c("median", "q3", "q1"))
})


test_that("confidence intervals name a lower and an upper limit, in order", {
  for (l in c("95% CI", "95% Confidence Interval", "97.5% CI", "CI",
              "Confidence Interval")) {
    expect_equal(.parse_stat_label(l), c("ci_low", "ci_high"), info = l)
  }
  ## The confidence LEVEL is not a percentage statistic. Without the numeric
  ## wildcard "95% CI" would read as a percentage followed by an interval.
  expect_false("pct" %in% .parse_stat_label("95% CI"))
})


test_that("standard error is recognised, and distinct from standard deviation", {
  for (l in c("SE", "Standard Error", "Std Error", "SEM")) {
    expect_equal(.parse_stat_label(l), "se", info = l)
  }
  for (l in c("SD", "Standard Deviation", "StDev")) {
    expect_equal(.parse_stat_label(l), "sd", info = l)
  }
  expect_equal(.parse_stat_label("Mean (SE)"), c("mean", "se"))
})


test_that("percentile forms resolve to the quartile they name", {
  expect_equal(.parse_stat_label("25th percentile"), "q1")
  expect_equal(.parse_stat_label("75th percentile"), "q3")
  expect_equal(.parse_stat_label("50th percentile"), "median")
  expect_equal(.parse_stat_label("P25, P75"), c("q1", "q3"))
})


test_that("a label naming anything unrecognised is refused whole", {
  ## Partial acceptance would pair a short statistic vector against a longer
  ## placeholder -- a plausible wrong number rather than a visible gap.
  refuse <- c("Female", "American Indian or Alaska Native", "Not Reported",
              "Unknown", "Other", "Total", "Missing",
              "Median of prior therapies", "Range of motion",
              "Mean arterial pressure", "Standard of care",
              "Sum of ADQX.EXPY", "Number of prior lines", "", "Summary:")
  for (l in refuse) expect_null(.parse_stat_label(l), info = l)
  expect_gt(length(refuse), 10L)
})


test_that("a unit is stripped but a statistic in the same position is not", {
  expect_equal(.parse_stat_label("Mean (kg)"), "mean")
  expect_equal(.parse_stat_label("Mean (per 100 PY)"), "mean")
  expect_equal(.parse_stat_label("Median (months)"), "median")
  ## The distinction is whether the bracket parses, not a list of known units.
  expect_equal(.parse_stat_label("Mean (SD)"), c("mean", "sd"))
  expect_equal(.parse_stat_label("Mean (standard deviation)"), c("mean", "sd"))
})


test_that("footnote decoration does not change what a label names", {
  for (l in c("Mean (SD)a", "Mean (SD)*", "Mean (SD)†", "Mean (SD)¹",
              "Mean (SD) ")) {
    expect_equal(.parse_stat_label(l), c("mean", "sd"), info = l)
  }
})


test_that("no label can request a denominator", {
  ## Structural, not a blocklist: a denominator has no semantic token, so no
  ## phrase in the vocabulary can name one. This is what keeps a continuous
  ## count from ever being read as a percentage denominator.
  expect_false("denom" %in% .STAT_TOKENS)
  expect_false("OP_DENOM" %in% names(.OP_TOKENS))
  for (l in c("Denominator", "N total", "Total N", "Big N")) {
    toks <- .parse_stat_label(l)
    expect_false(isTRUE("denom" %in% toks), info = l)
  }
})


## ---------------------------------------------------------------------------
## Stage 2: the same request, resolved by different methods
## ---------------------------------------------------------------------------

test_that("one token resolves to different engine names by method", {
  ## The reason a label may not name an engine statistic: "n" is a count in
  ## both places and the engine spells it differently in each.
  cont <- .resolve_stat_tokens("count", .STANDARD_METHODS, CONT)
  catg <- .resolve_stat_tokens("count", .STANDARD_METHODS, CATG)

  expect_length(cont$unsupported, 0L)
  expect_length(catg$unsupported, 0L)
  expect_equal(cont$stats[[1]]$operation_id, "OP_N")
  expect_equal(catg$stats[[1]]$operation_id, "OP_N")

  ## Same operation id, different ARD spelling -- asserted rather than
  ## assumed, because this is the whole point of the split.
  expect_false(identical(cont$stats[[1]]$stat_name,
                         catg$stats[[1]]$stat_name))
  expect_equal(catg$stats[[1]]$stat_name, "n")
  expect_true("N" %in% (cont$stats[[1]]$stat_names %||% cont$stats[[1]]$stat_name))
})


test_that("a method that cannot supply a statistic refuses the whole row", {
  ## An SE line over a continuous summary: read correctly, and unproducible.
  res <- .resolve_stat_tokens(c("mean", "se"), .STANDARD_METHODS, CONT)
  expect_equal(res$unsupported, "se")
  ## Not "mean bound, se unbound" -- NOTHING is bound, because a part-bound
  ## row shifts its remaining slots onto the wrong statistics.
  expect_length(res$stats, 0L)
  ## And the diagnostic has what it needs to be actionable.
  expect_true("OP_MEAN" %in% res$available)
  expect_true(length(res$available) > 1L)

  ## A percentage over a continuous summary is the same shape of refusal.
  expect_equal(.resolve_stat_tokens("pct", .STANDARD_METHODS, CONT)$unsupported,
               "pct")
  ## A confidence interval resolves under Kaplan-Meier and nowhere else.
  expect_length(
    .resolve_stat_tokens(c("ci_low", "ci_high"), .STANDARD_METHODS, KM)$unsupported,
    0L)
  expect_gt(
    length(.resolve_stat_tokens(c("ci_low", "ci_high"),
                                .STANDARD_METHODS, CONT)$unsupported), 0L)
})


test_that("every alias resolves somewhere, or is knowingly unsupported", {
  ## No alias may exist in a third state where it silently binds to nothing.
  ## An alias that no method can serve is fine -- it refuses with a reason --
  ## but it must be KNOWN to be that, not discovered in a filled workbook.
  supported_tokens <- unique(unlist(lapply(.STANDARD_METHODS, function(m) {
    ops <- vapply(m$operations %||% list(),
                  function(o) as.character(o$id), character(1))
    unname(.OP_TOKENS[ops[ops %in% names(.OP_TOKENS)]])
  })))
  ## Recorded, not asserted away: these are recognised and no current method
  ## produces them, so they refuse with a named reason rather than misbind.
  known_unsupported <- c("se", "cv", "geomean", "pvalue")

  checked <- 0L
  for (tok in names(.STAT_ALIASES)) {
    for (phrase in .STAT_ALIASES[[tok]]) {
      expect_equal(.parse_stat_label(phrase), tok, info = phrase)
      checked <- checked + 1L
    }
    expect_true(tok %in% c(supported_tokens, known_unsupported), info = tok)
  }
  expect_gt(checked, 30L)
})


## ---------------------------------------------------------------------------
## The invariant: no recognised label falls through to positional matching
## ---------------------------------------------------------------------------

test_that("a recognised label never falls through to positional matching", {
  ## The continuous method lists its operations n, mean, sd, median, q1, q3,
  ## min, max. Positional binding would give a two-slot line OP_N and OP_MEAN
  ## no matter what the line is called -- which is exactly how "Q1; Q3" came
  ## to be filled with a count and a mean.
  positional <- vapply(
    .method_operation_slots(.STANDARD_METHODS, CONT),
    function(s) as.character(s$operation_id), character(1))
  expect_equal(positional[1:2], c("OP_N", "OP_MEAN"))

  cases <- list(
    list(label = "Q1; Q3",    ops = c("OP_Q1", "OP_Q3")),
    list(label = "Min, Max",  ops = c("OP_MIN", "OP_MAX")),
    list(label = "Median",    ops = "OP_MEDIAN"),
    list(label = "Mean (SD)", ops = c("OP_MEAN", "OP_SD")),
    list(label = "Q3, Q1",    ops = c("OP_Q3", "OP_Q1")),
    list(label = "25th percentile, 75th percentile",
         ops = c("OP_Q1", "OP_Q3"))
  )
  for (case in cases) {
    got <- .slg_ops(case$label, CONT)
    expect_equal(got, case$ops, info = case$label)
    ## And specifically NOT the positional prefix of the same length, unless
    ## the label genuinely names those operations.
    if (!identical(case$ops, unname(positional[seq_along(case$ops)]))) {
      expect_false(identical(got, unname(positional[seq_along(case$ops)])),
                   info = case$label)
    }
  }
  expect_gt(length(cases), 4L)
})


test_that("an unsupported label leaves every slot unbound", {
  b <- .slg_bind("SE", CONT)
  expect_length(b$stats, 0L)
  expect_equal(b$unsupported, "se")
  expect_true(length(b$available) > 1L)

  ## A label the grammar cannot read at all under a non-category parent binds
  ## nothing either -- it is not silently turned into a variable level, which
  ## for a continuous analysis could never match anything.
  u <- .slg_bind("Something the grammar cannot read", CONT)
  expect_length(u$stats, 0L)
  expect_true(isTRUE(u$unreadable))
  expect_null(u$variable_level)
})


## ---------------------------------------------------------------------------
## Statistic labels that resemble category levels
## ---------------------------------------------------------------------------

test_that("under a per-category parent a child row is a level, whatever it reads as", {
  ## A codelist value is arbitrary sponsor text and may legitimately read as a
  ## statistic. Deciding by how the label reads would reserve or misfill a
  ## genuine category level on nothing better than lexical resemblance.
  lookalikes <- c("Range", "Median", "Q1, Q3", "Mean ± SD", "n (%)",
                  "Missing", "Total", "Unknown", "SE")
  for (m in .DECODE_METHOD_IDS) {
    for (l in lookalikes) {
      b <- .slg_bind(l, m)
      expect_equal(b$variable_level, l, info = paste(m, l))
      expect_null(b$stat_line, info = paste(m, l))
      ## It keeps the method's own statistics, as any level row does.
      expect_gt(length(b$stats), 0L)
    }
  }
  expect_gt(length(.DECODE_METHOD_IDS), 2L)
})


test_that("a builder-typed level row never consults the grammar", {
  ## Its label is a codelist VALUE that the builder already resolved.
  b <- .fill_row_binding(
    entry = list(label = "Range", analysis_id = "AN_1", kind = "level",
                 level = "Range"),
    parent = NULL, methods = .STANDARD_METHODS,
    analyses = list(list(id = "AN_1", methodId = CATG)))
  expect_equal(b$variable_level, "Range")
  expect_null(b$stat_line)
})


## ---------------------------------------------------------------------------
## The shipped documentation, and its drift detection
## ---------------------------------------------------------------------------

test_that("the shipped support table still describes what the code does", {
  ## Documentation that says a statistic is supported when it is not is worse
  ## than none: it is the reason somebody trusts a blank cell. The table is
  ## generated, committed, and compared here, so it cannot fall behind the
  ## vocabulary or the method catalogue without the suite going red.
  ##
  ## Regenerate with: Rscript data-raw/build_statistic_label_doc.R
  path <- system.file("extdata", "statistic_label_support.md",
                      package = "arsbridge")
  expect_true(nzchar(path) && file.exists(path))

  committed   <- readLines(path, warn = FALSE)
  regenerated <- strsplit(.stat_label_support_md(), "\n", fixed = TRUE)[[1]]
  expect_equal(committed, regenerated)
  ## Non-vacuity: a table that rendered to nothing would compare equal to an
  ## empty file and pass.
  expect_gt(length(committed), 40L)
  expect_true(any(grepl("MTH_SUMMARY_STATISTICS_CONTINUOUS", committed)))
})


test_that("the support table records both halves of the promise", {
  tab <- .stat_label_support()
  ## Every method x statistic pair is accounted for, one way or the other.
  expect_equal(nrow(tab),
               length(.STANDARD_METHODS) * length(.STAT_ALIASES))
  expect_false(anyNA(tab$supported))

  ## The documented distinction this table exists for: the same statistic,
  ## resolved to a different ARD name depending on the method.
  count <- tab[tab$statistic == "count" & tab$supported, , drop = FALSE]
  expect_gt(nrow(count), 1L)
  expect_gt(length(unique(count$ard_stat)), 1L)

  ## And a statistic no current method produces is recorded as unsupported
  ## everywhere rather than being silently absent from the table.
  se <- tab[tab$statistic == "se", , drop = FALSE]
  expect_equal(nrow(se), length(.STANDARD_METHODS))
  expect_false(any(se$supported))
})


## ---------------------------------------------------------------------------
## End to end: the same table, authored in two dialects
## ---------------------------------------------------------------------------

## A continuous block over two arms and a Total column. `lines` is the only
## thing that varies between dialects.
.slg_build <- function(dir, lines) {
  ds <- "ADQX"; arm <- "TRTP"; num <- "AGE"
  a <- "Drug A"; b <- "Placebo"; sheet <- "Table 14.1.2"

  sw <- openxlsx2::wb_workbook()$add_worksheet("Variables")
  sw$add_data(sheet = "Variables", x = data.frame(
    Dataset  = c("ADSL", "ADSL", rep(ds, 3)),
    Variable = c("USUBJID", arm, "USUBJID", arm, num),
    Label    = c("Subject", "Treatment", "Subject", "Treatment", "Measure"),
    Type     = c("Char", "Char", "Char", "Char", "Num"),
    Origin = "Derived", Codelist = "", Length = "40", Mandatory = "Req",
    stringsAsFactors = FALSE))
  spec <- file.path(dir, "spec.xlsx"); sw$save(spec)

  adam <- file.path(dir, "adam"); dir.create(adam, showWarnings = FALSE)
  subjects <- data.frame(
    USUBJID = sprintf("S%02d", 1:12),
    ARM = rep(c(a, b), each = 6),
    VAL = c(41, 52, 63, 47, 58, NA, 44, 55, 66, 50, 61, 72),
    stringsAsFactors = FALSE)
  names(subjects) <- c("USUBJID", arm, num)
  utils::write.csv(subjects, file.path(adam, paste0(ds, ".csv")),
                   row.names = FALSE)
  utils::write.csv(subjects, file.path(adam, "ADSL.csv"), row.names = FALSE)

  black <- openxlsx2::wb_color(hex = "FF000000")
  red   <- openxlsx2::wb_color(hex = "FFC00000")
  ann <- function(l, an) {
    openxlsx2::fmt_txt(l, color = black, size = 10) +
      openxlsx2::fmt_txt(paste0("\n", an), color = red, size = 8, italic = TRUE)
  }

  wb <- openxlsx2::wb_workbook()$add_worksheet(sheet)
  put <- function(x, row, col = 1L) {
    wb$add_data(sheet = sheet, x = x, start_row = row, start_col = col,
                col_names = FALSE)
  }
  put(sheet, 1); put("Continuous summary", 2); put("Item", 3)
  put(ann(sprintf("%s (N=XX)", a), sprintf("%s.%s='%s'", ds, arm, a)), 3, 2L)
  put(ann(sprintf("%s (N=XX)", b), sprintf("%s.%s='%s'", ds, arm, b)), 3, 3L)
  put(ann("Total (N=XX)",
          sprintf("[%s.%s IN ('%s','%s')]", ds, arm, a, b)), 3, 4L)
  put(ann("Measure (units)", sprintf("[%s.%s]", ds, num)), 4)
  for (i in seq_along(lines)) {
    put(lines[[i]]$label, 4L + i)
    for (j in 2:4) put(lines[[i]]$placeholder, 4L + i, j)
  }
  shell <- file.path(dir, "shell.xlsx"); wb$save(shell)
  list(shell = shell, spec = spec, adam = adam, sheet = sheet)
}

.slg_fill <- function(paths, dir) {
  ars <- file.path(dir, "ars.json")
  built <- withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(suppressWarnings(spec_to_ars(
      shell_path = paths$shell, adam_spec_path = paths$spec, api_key = "",
      use_llm = FALSE, verbose = FALSE, output_path = ars,
      report_path = file.path(dir, "report.xlsx"), emit_code = FALSE))))
  ## Captured here, not after the fill: `ars_to_ard()` resets the collector
  ## for its own execution run, so a build-stage diagnostic read afterwards
  ## is always empty.
  ## Normalised to a frame: a run that emits nothing returns NULL, and
  ## `nrow(NULL)` makes an assertion ERROR rather than fail -- which hides a
  ## caught regression behind what looks like a broken test.
  build_diagnostics <- built$diagnostics
  if (is.null(build_diagnostics)) {
    build_diagnostics <- data.frame(problem = character(),
                                    stringsAsFactors = FALSE)
  }
  ard <- suppressMessages(suppressWarnings(ars_to_ard(ars, paths$adam)))
  filled <- file.path(dir, "filled.xlsx")
  res <- suppressMessages(suppressWarnings(ars_fill_shell(
    shell_path = paths$shell, ars = ars, ard = ard, output_path = filled,
    adam_dir = paths$adam, overwrite = TRUE)))
  book <- openxlsx2::wb_to_df(openxlsx2::wb_load(filled), sheet = paths$sheet,
                              col_names = FALSE)
  list(census = res$census, diagnostics = build_diagnostics,
       cell = function(row, col) as.character(book[row, col]))
}

## The numbers a cell shows, in the order it shows them.
.slg_nums <- function(text) {
  regmatches(text, gregexpr("[0-9]+\\.?[0-9]*", text))[[1]]
}

.SLG_CANON <- list(
  list(label = "n",         placeholder = "xx"),
  list(label = "Mean (SD)", placeholder = "xx.x (xx.xx)"),
  list(label = "Median",    placeholder = "xx.x"),
  list(label = "Q1, Q3",    placeholder = "xx.x, xx.x"),
  list(label = "Min, Max",  placeholder = "xx, xx")
)

## The same five statistics, none of them spelled the same way.
.SLG_DIALECT <- list(
  list(label = "Number of subjects", placeholder = "xx"),
  list(label = "Mean ± SD",          placeholder = "xx.x (xx.xx)"),
  list(label = "Median",             placeholder = "xx.x"),
  list(label = "25th percentile; 75th percentile",
       placeholder = "xx.x, xx.x"),
  list(label = "Min – Max",          placeholder = "xx, xx")
)

test_that("a table authored in a second dialect fills identically", {
  ## This is the test that proves the grammar keys on the statistic rather
  ## than on a spelling. Same data, same layout, same Total column; every
  ## label written differently. The numbers must be identical cell for cell.
  skip_if_not_installed("openxlsx2")

  d1 <- withr::local_tempdir(); d2 <- withr::local_tempdir()
  a <- .slg_fill(.slg_build(d1, .SLG_CANON), d1)
  b <- .slg_fill(.slg_build(d2, .SLG_DIALECT), d2)

  checked <- 0L
  for (row in 5:9) {
    for (col in 2:4) {
      va <- a$cell(row, col); vb <- b$cell(row, col)
      ## Non-vacuity: both actually filled, so this is not two placeholders
      ## agreeing with each other.
      expect_false(grepl("x", va, ignore.case = TRUE),
                   info = paste("canonical", row, col))
      expect_equal(.slg_nums(vb), .slg_nums(va),
                   info = paste("row", row, "col", col))
      checked <- checked + 1L
    }
  }
  expect_equal(checked, 15L)

  ## Including the Total column specifically, and the count line whose value
  ## differs from both arms.
  expect_equal(a$cell(5, 4), b$cell(5, 4))
  expect_equal(a$cell(5, 2), "5")
  expect_equal(a$cell(5, 3), "6")
  expect_equal(a$cell(5, 4), "11")

  ## And every body cell of both filled.
  for (out in list(a, b)) {
    body <- out$census[out$census$row >= 5L, , drop = FALSE]
    expect_equal(nrow(body), 15L)
    expect_equal(sum(body$status %in% "filled"), 15L)
  }
})


test_that("a statistic the method cannot produce refuses its row and says so", {
  ## An SE line over a continuous summary: read correctly, unproducible, and
  ## reported -- rather than bound to whichever operation comes first.
  skip_if_not_installed("openxlsx2")
  dir <- withr::local_tempdir()
  lines <- c(.SLG_CANON, list(list(label = "SE", placeholder = "xx.xx")))
  out <- .slg_fill(.slg_build(dir, lines), dir)

  ## Row 10 is the SE line. Every one of its cells is pending, with the
  ## reason that names the actual problem.
  se_cells <- out$census[out$census$row == 10L, , drop = FALSE]
  expect_equal(nrow(se_cells), 3L)
  expect_true(all(se_cells$status %in% "pending"))
  ## NA-guarded: a regression that FILLS this row leaves `reason` NA, and a
  ## bare comparison would error rather than fail -- which reads as a broken
  ## test instead of a caught defect.
  expect_true(all(!is.na(se_cells$reason) & se_cells$reason ==
    "the row's label names a statistic this analysis does not produce"))

  ## And nothing was written into them.
  for (col in 2:4) {
    expect_true(grepl("x", out$cell(10, col), ignore.case = TRUE), info = as.character(col))
  }

  ## The diagnostic names the label, the statistic, the method, and what that
  ## method does declare -- the four things needed to act on it.
  rec <- out$diagnostics
  expect_gt(nrow(rec), 0L)
  hit <- rec[grepl("SE", rec$problem, fixed = TRUE), , drop = FALSE]
  expect_gt(nrow(hit), 0L)
  msg <- paste(hit$problem, collapse = " ")
  expect_true(grepl("se", msg, fixed = TRUE))
  expect_true(grepl("MTH_SUMMARY_STATISTICS_CONTINUOUS", msg, fixed = TRUE))
  expect_true(grepl("OP_MEAN", msg, fixed = TRUE))

  ## The rest of the block is unaffected: a refusal is per row, not per block.
  rest <- out$census[out$census$row %in% 5:9, , drop = FALSE]
  expect_equal(sum(rest$status %in% "filled"), 15L)
})


## ---------------------------------------------------------------------------
## Rename metamorphic: the grammar reads statistics, not identifiers
## ---------------------------------------------------------------------------

test_that("the grammar is independent of every dataset and variable name", {
  ## The label carries no identifiers at all, which is the point -- so the
  ## same labels under analyses named from an unrelated vocabulary must
  ## produce identical bindings.
  first  <- .slg_ops("Q1; Q3", CONT)
  second <- {
    analyses <- list(list(id = "AN_ZZ_9", methodId = CONT))
    b <- .fill_row_binding(
      entry = list(label = "Q1; Q3", analysis_id = NA_character_),
      parent = list(analysis_id = "AN_ZZ_9", label = "ZZ parent"),
      methods = .STANDARD_METHODS, analyses = analyses)
    vapply(b$stats, function(s) as.character(s$operation_id), character(1))
  }
  expect_equal(first, second)
  expect_equal(first, c("OP_Q1", "OP_Q3"))
})
