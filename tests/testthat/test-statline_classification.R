## S-2: one tested source of truth for "is this row a statistic row?"
##
## General defect class: the same question was answered in three places by
## three restatements of the statistic vocabulary -- the grammar, a closed list
## of fifteen exact spellings gating whether an analysis may bind to a row, and
## a chain of six label comparisons choosing a generated row's placeholder. An
## author writing "Standard Deviation" instead of "SD" got a different answer
## in two of the three, for no reason anyone could state.
##
## General invariant: the vocabulary is stated once. Where a site needs a
## NARROWER decision than the grammar's -- and the parse-time site does, having
## no method in hand -- the narrowing is explicit, named and tested, not a
## second copy of the words.
##
## This changes no binding behaviour. Every label the historical list accepted
## is still accepted; the additions are spelling variants of statistics already
## in scope, and no new KIND of statistic joins them.

## The fifteen spellings the retired list held, verbatim.
.S2_HISTORICAL <- c(
  "mean sd", "mean", "sd", "median", "min max", "min", "max",
  "q1 q3", "q1", "q3", "n", "se", "cv", "geometric mean", "n missing"
)

## Labels that must NOT be read as statistic rows at parse time. Each is a
## plausible sponsor-authored codelist value, a bare composite word, or a
## statistic KIND deliberately outside the parse-time scope.
.S2_REJECTED <- c(
  "Missing", "Unknown", "Not Reported", "Other", "Total", "Female",
  "Range", "IQR", "Interquartile range",
  "95% CI", "p-value", "events",
  "Median of prior therapies", "Range of motion", ""
)


test_that("every historical statistic-row spelling is still recognised", {
  for (k in .S2_HISTORICAL) {
    expect_true(.is_statline_row_label(k), info = k)
  }
  ## Scope: a change that stopped reading the fixtures would otherwise pass
  ## this vacuously.
  expect_equal(length(.S2_HISTORICAL), 15L)
})


test_that("n missing stays a statistic row without becoming resolvable", {
  ## Two different answers for one label, and both are deliberate.
  ##
  ## At PARSE time it is a layout row of the block above, so no analysis binds
  ## to it -- exactly as the retired list said.
  expect_true(.is_statline_row_label("n missing"))
  expect_true(.is_statline_row_label("N Missing"))

  ## At FILL time it is NOT a resolvable statistic: no method declares a
  ## missing-count operation, so the grammar refuses it and it surfaces in
  ## ars_unresolved_labels() rather than binding to whatever comes first.
  ## Classifying it as a statistic row must never be mistaken for supporting
  ## it -- that would be a real number of a statistic nobody computed.
  expect_null(.parse_stat_label("n missing"))
  expect_false("missing" %in% .STAT_TOKENS)

  ## And it is carried as the named exception it is, not as a token.
  expect_true("n missing" %in% .STATLINE_LEGACY_LABELS)
})


test_that("n (%) stays a statistic row, which is why pct is in scope", {
  ## Preserved for backward compatibility. It qualifies today only as a side
  ## effect of normalisation -- .norm_label("n (%)") is "n" -- so the scope
  ## has to include pct or an n (%) row would newly become bindable.
  expect_equal(.norm_label("n (%)"), "n")
  expect_true(.is_statline_row_label("n (%)"))
  expect_true(.is_statline_row_label("N (%)"))
  expect_true("pct" %in% .STATLINE_TOKEN_SCOPE)
  ## The grammar still reads it as two statistics, not one.
  expect_equal(.parse_stat_label("n (%)"), c("count", "pct"))
})


test_that("a bare composite word is a codelist value, not a statistic row", {
  ## One word standing for a pair is exactly the shape of a category label,
  ## and the historical set contained no such entry.
  for (k in c("Range", "IQR", "Interquartile range")) {
    expect_false(.is_statline_row_label(k), info = k)
    ## The grammar still READS them -- this is a parse-time narrowing, not a
    ## change to what a composite means where a method is known.
    expect_false(is.null(.parse_stat_label(k)), info = k)
  }
  ## Spelling the pair out is unambiguous and stays recognised.
  expect_true(.is_statline_row_label("Min - Max"))
  expect_true(.is_statline_row_label("Q1, Q3"))
})


test_that("no new KIND of statistic becomes a parse-time statistic row", {
  ## The widening is spelling only. These four tokens stay outside the scope,
  ## so a CI, p-value or events row classifies exactly as it did before.
  for (tok in c("ci_low", "ci_high", "events", "pvalue")) {
    expect_false(tok %in% .STATLINE_TOKEN_SCOPE, info = tok)
  }
  for (k in c("95% CI", "95% Confidence Interval", "p-value", "p value",
              "events", "Number of events")) {
    expect_false(.is_statline_row_label(k), info = k)
  }
})


test_that("S-2 does not touch how the grammar RESOLVES those labels", {
  ## The PR #63 contract, asserted unchanged. Classification and resolution
  ## are different questions, and only the first is narrowed here.
  expect_equal(.parse_stat_label("95% CI"), c("ci_low", "ci_high"))
  expect_equal(.parse_stat_label("p-value"), "pvalue")
  expect_equal(.parse_stat_label("Number of events"), "events")
  expect_equal(.parse_stat_label("Mean (SD)"), c("mean", "sd"))
  expect_equal(.parse_stat_label("Q1; Q3"), c("q1", "q3"))
})


test_that("ordinary codelist values are never statistic rows", {
  for (k in .S2_REJECTED) expect_false(.is_statline_row_label(k), info = k)
  expect_gt(length(.S2_REJECTED), 10L)
})


test_that("the widening is spelling variants of in-scope statistics only", {
  ## Generated from the vocabulary rather than transcribed, so a new alias is
  ## covered automatically and a new TOKEN is not silently admitted.
  newly <- character()
  for (tok in .STATLINE_TOKEN_SCOPE) {
    for (phrase in .STAT_ALIASES[[tok]]) {
      if (!.norm_label(phrase) %in% .S2_HISTORICAL) newly <- c(newly, phrase)
      ## Whatever the spelling, it resolves to the token it belongs to.
      expect_equal(.parse_stat_label(phrase), tok, info = phrase)
      expect_true(.is_statline_row_label(phrase), info = phrase)
    }
  }
  expect_gt(length(newly), 0L)
  ## Every alias of an OUT-of-scope token stays out.
  for (tok in setdiff(names(.STAT_ALIASES), .STATLINE_TOKEN_SCOPE)) {
    for (phrase in .STAT_ALIASES[[tok]]) {
      expect_false(.is_statline_row_label(phrase), info = phrase)
    }
  }
})


test_that("the generated placeholder shapes are the ones the chain produced", {
  ## The third retired list. Same six outputs, now keyed on the statistics the
  ## label names rather than on six exact spellings.
  expect_equal(.statline_placeholder("n"), "xx")
  expect_equal(.statline_placeholder("Mean (SD)"), "xx.x (x.xx)")
  expect_equal(.statline_placeholder("Median"), "xx.x")
  expect_equal(.statline_placeholder("Min, Max"), "(xx.x, xx.x)")
  expect_equal(.statline_placeholder("Q1, Q3"), "(xx.x, xx.x)")
  expect_equal(.statline_placeholder("Median (Q1, Q3)"), "xx.x (xx.x, xx.x)")
  ## Anything else keeps the chain's single-number default.
  expect_equal(.statline_placeholder("SD"), "xx.x")
  expect_equal(.statline_placeholder("Some prose label"), "xx.x")
  ## And the spelling no longer decides it -- which is the point.
  expect_equal(.statline_placeholder("Mean (Standard Deviation)"),
               .statline_placeholder("Mean (SD)"))
})


## ---------------------------------------------------------------------------
## The fill stage: a codelist value that reads like a statistic is a LEVEL
## ---------------------------------------------------------------------------

.s2_cat_build <- function(dir, levels) {
  sw <- openxlsx2::wb_workbook()$add_worksheet("Variables")
  sw$add_data(sheet = "Variables", x = data.frame(
    Dataset  = c("ADSL", "ADSL", "ADQX", "ADQX", "ADQX"),
    Variable = c("USUBJID", "TRTP", "USUBJID", "TRTP", "RESP"),
    Label    = c("Subject", "Treatment", "Subject", "Treatment", "Response"),
    Type     = "Char", Origin = "Derived", Codelist = "", Length = "40",
    Mandatory = "Req", stringsAsFactors = FALSE))
  spec <- file.path(dir, "spec.xlsx")
  sw$save(spec)

  adam <- file.path(dir, "adam")
  dir.create(adam, showWarnings = FALSE)
  ## Every level observed, with DIFFERENT counts per arm, so a row filled from
  ## the wrong level or the wrong column cannot coincidentally match.
  resp <- c(rep(levels, times = c(3, 2, 1, 2)),
            rep(levels, times = c(1, 1, 3, 3)))
  subjects <- data.frame(
    USUBJID = sprintf("S%02d", seq_along(resp)),
    TRTP    = rep(c("Drug A", "Placebo"), each = 8),
    RESP    = resp, stringsAsFactors = FALSE)
  utils::write.csv(subjects, file.path(adam, "ADQX.csv"), row.names = FALSE)
  utils::write.csv(subjects, file.path(adam, "ADSL.csv"), row.names = FALSE)

  black <- openxlsx2::wb_color(hex = "FF000000")
  red   <- openxlsx2::wb_color(hex = "FFC00000")
  ann <- function(l, a) {
    openxlsx2::fmt_txt(l, color = black, size = 10) +
      openxlsx2::fmt_txt(paste0("\n", a), color = red, size = 8, italic = TRUE)
  }
  sheet <- "Table 14.5.1"
  wb <- openxlsx2::wb_workbook()$add_worksheet(sheet)
  put <- function(x, row, col = 1L) {
    wb$add_data(sheet = sheet, x = x, start_row = row, start_col = col,
                col_names = FALSE)
  }
  put(sheet, 1)
  put("Response by category", 2)
  put("Item", 3)
  put(ann("Drug A (N=XX)", "ADQX.TRTP='Drug A'"), 3, 2L)
  put(ann("Placebo (N=XX)", "ADQX.TRTP='Placebo'"), 3, 3L)
  put(ann("Response category, n (%)", "[ADQX.RESP]"), 4)
  for (i in seq_along(levels)) {
    put(levels[[i]], 4L + i)
    for (j in 2:3) put("xx (xx.x)", 4L + i, j)
  }
  shell <- file.path(dir, "shell.xlsx")
  wb$save(shell)
  list(shell = shell, spec = spec, adam = adam, sheet = sheet,
       rows = stats::setNames(4L + seq_along(levels), levels))
}


test_that("Median and Range as codelist values stay categorical levels", {
  ## The requirement this file exists to prove at the stage that decides what
  ## a reader sees. A codelist value is arbitrary sponsor text and may read
  ## exactly like a statistic. Under a per-category parent the grammar is
  ## never consulted -- the method is the evidence -- so these rows are levels
  ## and fill with their own counts.
  ##
  ## Note "Median" is classified as a statistic ROW at parse time, and always
  ## has been. That decision governs only whether an ANALYSIS may bind to the
  ## row; it does not reach the fill stage, and this test is what holds those
  ## two apart.
  skip_if_not_installed("openxlsx2")
  skip_if_not_installed("cards")
  dir    <- withr::local_tempdir()
  levels <- c("Median", "Range", "Missing", "Unknown")
  paths  <- .s2_cat_build(dir, levels)

  ars <- file.path(dir, "ars.json")
  withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(suppressWarnings(spec_to_ars(
      shell_path = paths$shell, adam_spec_path = paths$spec, api_key = "",
      use_llm = FALSE, verbose = FALSE, output_path = ars,
      report_path = file.path(dir, "report.xlsx"), emit_code = FALSE))))
  ard    <- suppressMessages(suppressWarnings(ars_to_ard(ars, paths$adam)))
  filled <- file.path(dir, "filled.xlsx")
  res <- suppressMessages(suppressWarnings(ars_fill_shell(
    shell_path = paths$shell, ars = ars, ard = ard, output_path = filled,
    adam_dir = paths$adam, overwrite = TRUE)))
  book <- openxlsx2::wb_to_df(openxlsx2::wb_load(filled), sheet = paths$sheet,
                              col_names = FALSE)

  ## Each level row shows its own counts, per arm, and the placeholder is gone.
  expected <- list(Median  = c("3", "1"), Range   = c("2", "1"),
                   Missing = c("1", "3"), Unknown = c("2", "3"))
  for (lvl in levels) {
    r <- paths$rows[[lvl]]
    for (j in 2:3) {
      txt <- as.character(book[r, j])
      expect_false(identical(txt, "xx (xx.x)"),
                   info = paste(lvl, "col", j))
      expect_match(txt, paste0("^", expected[[lvl]][[j - 1L]], " \\("),
                   info = paste(lvl, "col", j))
    }
  }

  ## And none of them was reserved or refused.
  refused <- ars_unresolved_labels(ars)
  expect_equal(nrow(refused[refused$label %in% levels, , drop = FALSE]), 0L)
  ## Non-vacuity: the block really did produce cells.
  expect_gt(nrow(res$census), 0L)
})
