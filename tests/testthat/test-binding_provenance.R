## A-18a: the provenance of a statistic-row binding is recorded, and records
## only. Nothing branches on it.
##
## General defect class: whether a refused or fallback binding is REPORTED was
## a property of the branch that bound the row, not of the row. Three paths
## treat the same input -- a label the grammar cannot read -- three ways: a row
## with its own analysis binds the method's operations positionally, a child of
## a per-category parent becomes a level, and a child of any other parent
## refuses and is queued. Only the third leaves a trace, so the unresolved
## queue is silent about the other two.
##
## General invariant (this PR): every finalised binding records what decided
## its statistics (`stat_basis`) and what the grammar made of its label
## (`label_parse`), on every path. Neither field may change a classification,
## an operation selection, the positional fallback, the per-category gate, or
## any filled cell. They are observations.

CONT <- "MTH_SUMMARY_STATISTICS_CONTINUOUS"
CAT  <- "MTH_COUNT_AND_PERCENTAGE"

.bp_bind <- function(label, method_id = CONT, own = TRUE, kind = "label") {
  .fill_row_binding(
    entry = list(label = label, sheet_row = 6L, kind = kind,
                 analysis_id = if (own) "AN_1" else NA_character_),
    parent = list(analysis_id = "AN_1", label = "Measure [ADQX.MEAS]"),
    methods = .STANDARD_METHODS,
    analyses = list(list(id = "AN_1", methodId = method_id)))
}
.bp_ops <- function(b) vapply(b$stats %||% list(),
                              function(s) as.character(s$operation_id), character(1))


test_that("the grammar outcome is reported without being acted on", {
  ## "whole" and "none" already existed implicitly; "partial" is the
  ## distinction nothing could make before -- a label that was trying to name
  ## statistics and failed, versus one that never tried.
  expect_equal(.label_parse_outcome("Mean (SD)"), "whole")
  expect_equal(.label_parse_outcome("Q1, Q3"), "whole")
  expect_equal(.label_parse_outcome("Response rate, % (95% CI)"), "partial")
  expect_equal(.label_parse_outcome("standard deviation of prior therapies"),
               "partial")
  expect_equal(.label_parse_outcome("Age (years)"), "none")
  expect_equal(.label_parse_outcome("Preferred Term"), "none")
  expect_equal(.label_parse_outcome(""), "none")
  expect_equal(.label_parse_outcome(NA_character_), "none")

  ## Substring traps. A naive scan finds "n" inside "duration" and "se" inside
  ## "response", and a record that cried wolf on ordinary titles would be worth
  ## less than no record at all.
  for (title in c("Duration of exposure", "Response to therapy",
                  "Baseline characteristics")) {
    expect_equal(.label_parse_outcome(title), "none", info = title)
  }

  ## And an honest counter-example, asserted rather than hidden: "events" is a
  ## real statistic term, so an ordinary clinical heading containing it reads
  ## as "partial". The record is a measurement, not a verdict -- this is
  ## exactly the noise any future queue policy must weigh, and pretending it
  ## away here would hide the evidence for that decision.
  expect_equal(.label_parse_outcome("Serious adverse events"), "partial")

  ## Scope: the vocabulary is real and non-trivial, so a table that emptied
  ## would turn the "partial" cases red rather than passing them as "none".
  expect_gt(length(.STAT_VOCAB_PHRASES), 40L)
  expect_gte(.STAT_VOCAB_MAX_WORDS, 3L)
})


test_that("every path records its basis, and none of them binds differently", {
  ## The SAME binding results as before, now distinguishable. Operation ids
  ## are asserted alongside the provenance so a change to either turns red.
  cases <- list(
    list(what = "own analysis, label read",
         b = .bp_bind("Mean (SD)"),
         basis = "label", parse = "whole", ops = c("OP_MEAN", "OP_SD")),
    ## The two rows nothing could tell apart.
    list(what = "own analysis, label unreadable but attempted",
         b = .bp_bind("Response rate, % (95% CI)"),
         basis = "positional", parse = "partial", ops = NULL),
    list(what = "own analysis, ordinary title",
         b = .bp_bind("Age (years)"),
         basis = "positional", parse = "none", ops = NULL),
    list(what = "per-category parent, statistic-looking level",
         b = .bp_bind("Mean (SD)", CAT, own = FALSE),
         basis = "level", parse = "whole", ops = c("OP_N", "OP_PCT")),
    list(what = "per-category parent, ordinary level",
         b = .bp_bind("Female", CAT, own = FALSE),
         basis = "level", parse = "none", ops = c("OP_N", "OP_PCT")),
    list(what = "continuous parent, label read",
         b = .bp_bind("Mean (SD)", CONT, own = FALSE),
         basis = "label", parse = "whole", ops = c("OP_MEAN", "OP_SD")),
    list(what = "continuous parent, label refused",
         b = .bp_bind("Response rate, % (95% CI)", CONT, own = FALSE),
         basis = "label", parse = "partial", ops = character(0)))

  for (k in cases) {
    expect_equal(k$b$stat_basis,  k$basis, info = k$what)
    expect_equal(k$b$label_parse, k$parse, info = k$what)
    if (!is.null(k$ops)) expect_equal(.bp_ops(k$b), k$ops, info = k$what)
  }

  ## The positional pair bind IDENTICALLY -- that is what makes them
  ## indistinguishable without the record, and what must not change.
  a <- .bp_bind("Response rate, % (95% CI)")
  b <- .bp_bind("Age (years)")
  expect_equal(.bp_ops(a), .bp_ops(b))
  expect_gt(length(.bp_ops(a)), 0L)
  expect_null(a$stat_line)
  expect_null(b$stat_line)

  ## The per-category gate is untouched: a statistic-looking label is still a
  ## LEVEL, and its parse outcome neither rescues nor reclassifies it.
  lvl <- .bp_bind("Mean (SD)", CAT, own = FALSE)
  expect_equal(lvl$variable_level, "Mean (SD)")
  expect_null(lvl$stat_line)
})


test_that("the record is observational -- no code reads it", {
  ## The invariant that keeps this PR honest. Both fields are written and
  ## serialised; if anything ever branches on them they have stopped being
  ## observations and this is no longer provenance-only.
  r_dir <- testthat::test_path("..", "..", "R")
  files <- if (dir.exists(r_dir)) {
    list.files(r_dir, pattern = "[.]R$", full.names = TRUE)
  } else {
    character(0)
  }
  skip_if(length(files) == 0L, "package sources not available (installed run)")
  expect_gt(length(files), 20L)
  src  <- unlist(lapply(files, readLines, warn = FALSE))
  code <- grep("^\\s*#", src, value = TRUE, invert = TRUE)
  expect_gt(length(code), 1000L)

  ## Assignment and serialisation are fine; a condition on the VALUE is not.
  ##
  ## One shape is allowed, and narrowly: a STANDALONE presence guard, whose
  ## whole condition is `is.null(x$stat_basis)`. It decides only whether there
  ## is anything to record. The pattern deliberately admits no `&&`, no `||`
  ## and nothing after the closing brace, so a mixed condition that also
  ## branches on the value stays flagged.
  guard <- "^\\s*if \\(!?is[.]null\\([^()]*[$](stat_basis|label_parse)\\)\\) [{]?\\s*$"
  reads <- grep("(if|while)\\s*\\(.*\\b(stat_basis|label_parse)\\b",
                code, value = TRUE)
  allowed <- grep(guard, reads, value = TRUE)
  expect_equal(length(setdiff(reads, allowed)), 0L)
  ## And the exception is real rather than theoretical -- if the guard shape
  ## disappears, this test should be simplified rather than quietly carrying
  ## an unused escape hatch.
  expect_gt(length(allowed), 0L)
  cmp <- grep(paste0("\\b(stat_basis|label_parse)\\b\\s*(==|!=)|",
                     "identical\\([^)]*\\b(stat_basis|label_parse)\\b"),
              code, value = TRUE)
  expect_equal(length(cmp), 0L)
})

## --- the record has to survive into the file ------------------------------

.bp_build <- function(dir) {
  ds <- "ADQX"
  sw <- openxlsx2::wb_workbook()$add_worksheet("Variables")
  sw$add_data(sheet = "Variables", x = data.frame(
    Dataset  = c("ADSL", "ADSL", ds, ds, ds),
    Variable = c("USUBJID", "TRTGRP", "USUBJID", "TRTGRP", "MEASVAL"),
    Label    = c("Subject", "Group", "Subject", "Group", "Measure"),
    Type     = c("Char", "Char", "Char", "Char", "Num"),
    Origin = "Derived", Codelist = "", Length = "40", Mandatory = "Req",
    stringsAsFactors = FALSE))
  spec <- file.path(dir, "spec.xlsx"); sw$save(spec)

  black <- openxlsx2::wb_color(hex = "FF000000")
  red   <- openxlsx2::wb_color(hex = "FFC00000")
  ann <- function(l, a) {
    openxlsx2::fmt_txt(l, color = black, size = 10) +
      openxlsx2::fmt_txt(paste0("\n", a), color = red, size = 8, italic = TRUE)
  }
  sheet <- "Table 92.1"
  wb <- openxlsx2::wb_workbook()$add_worksheet(sheet)
  put <- function(x, row, col = 1L) {
    wb$add_data(sheet = sheet, x = x, start_row = row, start_col = col,
                col_names = FALSE)
  }
  put(sheet, 1); put("Provenance fixture", 2); put("Item", 3)
  put(ann("Regimen P (N=XX)", sprintf("%s.TRTGRP='Regimen P'", ds)), 3, 2L)
  put(ann("Measure (units)", sprintf("[%s.MEASVAL]", ds)), 4)
  lines <- list(c("n", "xx"), c("Mean (SD)", "xx.x (xx.xx)"),
                c("Q1, Q3", "xx.x, xx.x"))
  for (i in seq_along(lines)) {
    put(lines[[i]][[1]], 4L + i); put(lines[[i]][[2]], 4L + i, 2L)
  }
  shell <- file.path(dir, "shell.xlsx"); wb$save(shell)
  list(shell = shell, spec = spec, rows = 5:7)
}

test_that("the provenance record reaches the output metadata", {
  skip_if_not_installed("openxlsx2")
  dir <- withr::local_tempdir()
  p   <- .bp_build(dir)
  out <- file.path(dir, "ars.json")
  withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(suppressWarnings(spec_to_ars(
      shell_path = p$shell, adam_spec_path = p$spec, api_key = "",
      use_llm = FALSE, verbose = FALSE, output_path = out,
      report_path = file.path(dir, "report.xlsx"), emit_code = FALSE))))
  j  <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  sf <- j$outputs[[1]]$`_meta`$shell_fill
  expect_false(is.null(sf))

  ## Serialised, or the measurement this PR exists for cannot be taken.
  prov <- sf$row_provenance
  expect_false(is.null(prov))
  expect_gt(length(prov %||% list()), 0L)

  by_row <- list()
  for (e in prov %||% list()) by_row[[as.character(e$row)]] <- e
  checked <- 0L
  for (r in p$rows) {
    e <- by_row[[as.character(r)]]
    if (is.null(e)) next
    checked <- checked + 1L
    expect_true(e$stat_basis %in% c("label", "positional", "level",
                                    "method_default"), info = r)
    expect_true(e$label_parse %in% c("whole", "partial", "none"), info = r)
  }
  ## Scope: a run that recorded nothing for these rows would otherwise pass.
  expect_equal(checked, length(p$rows))

  ## The statistic rows of a continuous block: read from their labels.
  expect_equal(by_row[["6"]]$stat_basis, "label")
  expect_equal(by_row[["6"]]$label_parse, "whole")
})
