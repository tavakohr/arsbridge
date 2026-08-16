## B4: the row's intent selects the method, or the row is reserved.
##
## General defect class: method selection read the *spelling* of a row's
## annotation and the *type* of the variable it named, never what the row said
## it was showing. Two consequences, both of which produce a number rather than
## a complaint:
##
##   * a row restricting a numeric variable was summarised instead of counted,
##     so a count slot received the mean of the unrestricted variable
##   * a row asking for a statistic this package has no method for was built
##     with whichever method the variable's type suggested
##
## General invariant: a condition restricts a row whatever syntax expresses it,
## and a stated statistic that cannot be computed reserves the row rather than
## being replaced by one that can. Equivalent conditions must select the same
## method; incidental differences in wording must not change it at all.

.mfi_vocabs <- list(
  first  = list(ds = "ADQX", num = "MEASURE", cat = "MEASGR", flag = "AEFL"),
  ## No identifier shared with the first, so a rule keyed on a familiar name
  ## rather than on the relationship would differ between the two halves.
  second = list(ds = "ADZZ", num = "SCOREN", cat = "SCORECAT", flag = "EVFL")
)

.mfi_infer <- function(ann, label = "n (%)", slots = 2L, categorical = FALSE) {
  .infer_row_method(
    list(label = label, annotation = ann, has_annot = TRUE, n_slots = slots),
    categorical
  )
}

## What a caller acts on: the catalogue name, or the fact that the row is
## reserved. Collapsing to one string lets equivalent inputs be compared
## directly rather than field by field.
.mfi_verdict <- function(inferred) {
  if (is.null(inferred)) return("<section default>")
  if (!is.null(inferred$unsupported)) return("<reserved>")
  inferred$method
}


test_that("equivalent conditions select the same method, however spelled", {
  checked <- 0L

  for (vname in names(.mfi_vocabs)) {
    v <- .mfi_vocabs[[vname]]

    ## The same restriction, five ways. A keyword threshold, its symbolic
    ## twin, and a quoted equality are one idea in three notations; the method
    ## must not depend on which the author reached for.
    ##
    ## The keyword/symbolic pair is the load-bearing one: the symbolic form is
    ## not yet in the where-clause grammar (A-07a), so if intent were being
    ## read from a successful parse rather than from the presence of a
    ## condition, these two would diverge.
    forms <- c(
      sprintf("%s.%s LT 16", v$ds, v$num),
      sprintf("%s.%s < 16",  v$ds, v$num),
      sprintf("%s.%s GE 16", v$ds, v$num),
      sprintf("%s.%s >= 16", v$ds, v$num),
      sprintf("%s.%s != 16", v$ds, v$num)
    )
    verdicts <- vapply(forms, function(f) .mfi_verdict(.mfi_infer(f)),
                       character(1))
    expect_length(unique(verdicts), 1L)

    ## And they agree with the character contrast -- the form that always
    ## worked. This is what makes the assertion above mean "counted" rather
    ## than merely "consistent": five forms agreeing on the WRONG method would
    ## also be consistent.
    character_form <- .mfi_verdict(
      .mfi_infer(sprintf("%s.%s='LOW'", v$ds, v$cat), categorical = TRUE)
    )
    expect_identical(unname(verdicts[[1]]), character_form)
    expect_identical(character_form, "Subject Count and Percentage")

    checked <- checked + 1L
  }
  expect_equal(checked, length(.mfi_vocabs))
})


test_that("incidental wording does not change the method", {
  ## The row's label describes the cell; it is not where the restriction is
  ## stated. Two rows with the same annotation must therefore get the same
  ## method whatever their labels say -- including a label naming a completely
  ## different statistic, which must not be able to override the annotation.
  v <- .mfi_vocabs$first
  ann <- sprintf("%s.%s GE 16", v$ds, v$num)

  labels <- c("n (%)", "Mean (SD)", "Subjects", "", "  ")
  verdicts <- vapply(labels, function(l) .mfi_verdict(.mfi_infer(ann, label = l)),
                     character(1))
  expect_length(unique(verdicts), 1L)
  expect_identical(unname(verdicts[[1]]), "Subject Count and Percentage")

  ## Whitespace and case in the condition itself are equally incidental.
  spellings <- c(
    sprintf("%s.%s GE 16", v$ds, v$num),
    sprintf("%s.%s  ge  16", v$ds, v$num),
    sprintf("  %s.%s Ge 16  ", v$ds, v$num)
  )
  expect_length(
    unique(vapply(spellings, function(s) .mfi_verdict(.mfi_infer(s)),
                  character(1))),
    1L
  )
})


test_that("a condition on another variable still summarises by type", {
  ## The distinction the change must NOT lose. A condition on the primary
  ## variable says "count subjects in this state"; a condition on a different
  ## variable only scopes the data, and the primary variable is still
  ## summarised by its own type. Losing this would turn every scoped summary
  ## row into a count.
  checked <- 0L
  for (vname in names(.mfi_vocabs)) {
    v <- .mfi_vocabs[[vname]]
    got <- .mfi_infer(
      sprintf("%s.AVAL WHERE %s.PARAMCD='DURD'", v$ds, v$ds),
      label = "Mean (SD)", slots = 2L, categorical = FALSE
    )
    expect_identical(.mfi_verdict(got), "Summary Statistics - Continuous")
    checked <- checked + 1L
  }
  expect_equal(checked, length(.mfi_vocabs))
})


test_that("a statistic with no method reserves the row instead of substituting", {
  checked <- 0L

  for (vname in names(.mfi_vocabs)) {
    v <- .mfi_vocabs[[vname]]

    ## An aggregation over records. The variable is numeric, so before this
    ## change the row was built as a summary and the cell filled with a mean.
    expect_identical(
      .mfi_verdict(.mfi_infer(sprintf("Sum of %s.%s", v$ds, v$num),
                              label = "Patient-years", slots = 1L)),
      "<reserved>"
    )

    ## A rate over exposure time, stated in the label. The control beside it is
    ## what makes this a statement about intent rather than about the
    ## annotation: the SAME annotation with an ordinary label still computes.
    rate_ann <- sprintf("%s.%s='Y'", v$ds, v$flag)
    expect_identical(
      .mfi_verdict(.mfi_infer(rate_ann, label = "Any event, E (E/100 PY)",
                              categorical = TRUE)),
      "<reserved>"
    )
    expect_identical(
      .mfi_verdict(.mfi_infer(rate_ann, label = "Any event, n (%)",
                              categorical = TRUE)),
      "Subject Count and Percentage"
    )
    ## Spelled out rather than abbreviated -- the same statistic either way.
    expect_identical(
      .mfi_verdict(.mfi_infer(rate_ann, label = "n/1000 patient-years",
                              categorical = TRUE)),
      "<reserved>"
    )

    ## A bare numeric variable is not an aggregation, and must still compute.
    expect_identical(
      .mfi_verdict(.mfi_infer(sprintf("%s.%s", v$ds, v$num),
                              label = "Mean (SD)", slots = 2L)),
      "Summary Statistics - Continuous"
    )

    checked <- checked + 1L
  }
  expect_equal(checked, length(.mfi_vocabs))
})


test_that("an intent word inside a quoted value is data, not intent", {
  ## The same rule the where-clause grammar follows: quoted spans are opaque.
  ## A level whose text happens to contain "sum of", or a unit, describes data
  ## and must not reserve the row that displays it.
  v <- .mfi_vocabs$first

  expect_identical(
    .mfi_verdict(.mfi_infer(sprintf("%s.%s='Sum of prior therapies'",
                                    v$ds, v$cat), categorical = TRUE)),
    "Subject Count and Percentage"
  )
  expect_identical(
    .mfi_verdict(.mfi_infer(sprintf("%s.%s='Events/100 patient-years'",
                                    v$ds, v$cat), categorical = TRUE)),
    "Subject Count and Percentage"
  )
  ## Non-vacuity: the identical text OUTSIDE quotes does reserve, so the two
  ## assertions above are about the masking and not about the pattern simply
  ## failing to match.
  expect_identical(
    .mfi_verdict(.mfi_infer(sprintf("Sum of %s.%s", v$ds, v$num))),
    "<reserved>"
  )
})


test_that("a reserved row reaches the built event as a reserved analysis", {
  ## The unit tests above pin the decision; this one proves the decision is
  ## acted on -- the analysis is built against the unsupported method, which is
  ## what makes the engine reserve a manual_pending cell rather than compute.
  skip_if_not_installed("openxlsx2")

  v <- .mfi_vocabs$first
  td <- withr::local_tempdir()

  spec <- file.path(td, "spec.xlsx")
  swb <- openxlsx2::wb_workbook()$add_worksheet("Variables")
  swb$add_data(sheet = "Variables", x = data.frame(
    Dataset  = c("ADSL", "ADSL", rep(v$ds, 4)),
    Variable = c("USUBJID", "TRTP", "USUBJID", "TRTP", v$num, v$flag),
    Label    = c("Subject", "Treatment", "Subject", "Treatment",
                 "Measure", "Event Flag"),
    Type = c("Char", "Char", "Char", "Char", "Num", "Char"),
    Origin = "Derived", Codelist = "", Length = "40", Mandatory = "Req",
    stringsAsFactors = FALSE))
  swb$save(spec)

  sheet <- "Table 14.7.7"
  wb <- openxlsx2::wb_workbook()$add_worksheet(sheet)
  put <- function(x, row, col = 1L) {
    wb$add_data(sheet = sheet, x = x, start_row = row, start_col = col,
                col_names = FALSE)
  }
  ann <- function(label, annotation) {
    openxlsx2::fmt_txt(label, color = openxlsx2::wb_color(hex = "FF000000"),
                       size = 10) +
      openxlsx2::fmt_txt(paste0("\n", annotation),
                         color = openxlsx2::wb_color(hex = "FFC00000"),
                         size = 8, italic = TRUE)
  }

  put(sheet, 1)
  put("Method-from-intent regression", 2)
  put(ann("Analysed Population ", sprintf("(%s.%s='Y')", v$ds, v$flag)), 3)
  put(ann("Item", sprintf("[columns -> ADSL.TRTP; source %s]", v$ds)), 4)
  put("Drug A", 4, 2L)
  put("Placebo", 4, 3L)
  ## The reserved row, and beside it a row that must keep computing -- a build
  ## that reserved everything would satisfy the first assertion alone.
  put(ann("Patient-years", sprintf("[Sum of %s.%s]", v$ds, v$num)), 5)
  for (j in 2:3) put("xx", 5, j)
  put(ann("Subjects, n", sprintf("[%s.USUBJID]", v$ds)), 6)
  for (j in 2:3) put("xx", 6, j)
  shell <- file.path(td, "shell.xlsx")
  wb$save(shell)

  res <- withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(suppressWarnings(spec_to_ars(
      shell_path = shell, adam_spec_path = spec, api_key = "",
      use_llm = FALSE, verbose = FALSE,
      output_path = file.path(td, "ars.json"),
      report_path = file.path(td, "report.xlsx"),
      emit_code = FALSE))))

  model <- ars_to_model(res$ars_path)
  methods_by_analysis <- model$analyses$methodId

  ## Non-vacuity: the shell really produced both rows.
  expect_gte(length(methods_by_analysis), 2L)
  ## The stated sum is reserved ...
  expect_true("MTH_UNSUPPORTED_ANALYSIS" %in% methods_by_analysis)
  ## ... and it did not take the neighbouring row with it.
  expect_gt(sum(methods_by_analysis != "MTH_UNSUPPORTED_ANALYSIS"), 0L)
})
