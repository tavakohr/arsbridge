## parse_shell_xlsx() on figure sheets.
##
## A figure has no rows or columns to annotate, so its shell states the plot in
## prose instead: red "X axis -> ADVS.AVISITN" lines under a "Programming
## annotations" heading. This grammar has no Word precedent -- it exists
## because an Excel sheet gave the shell author somewhere to put it.

figure <- function() {
  secs <- suppressMessages(suppressWarnings(
    parse_shell_xlsx(test_path("fixtures", "shells_apx_drm_301.xlsx"))))
  hit <- Filter(function(s) identical(s$tlf_type, "FIGURE"), secs)
  expect_length(hit, 1L)
  hit[[1]]
}

test_that("a figure sheet becomes a FIGURE section with its banner", {
  sec <- figure()
  expect_equal(sec$tlf_number, "F-14-3-1")
  expect_equal(sec$title, "Mean (+/- SE) Pulse Rate Over Time by Treatment")
  expect_match(sec$population_annot, "ADSL\\.SAFFL='Y'")
})

test_that("a figure has no stub rows and no column headers", {
  ## There is nothing to tabulate. A section with zero rows is correct here,
  ## which is why the no-rows diagnostic exempts figures.
  sec <- figure()
  expect_length(sec$stub_rows, 0L)
  expect_length(sec$col_headers, 0L)
  expect_equal(sec$n_header_rows, 0L)
})

test_that("the specification is attached as figure_spec", {
  ## Class 3, additive: no existing consumer knows about it, and none may
  ## require it.
  sec <- figure()
  expect_true(is.list(sec$figure_spec))
  expect_equal(sort(names(sec$figure_spec$directives)),
               c("error_bars", "filter", "series", "x_axis", "y_axis"))
})

test_that("each directive keeps the value the shell stated", {
  d <- figure()$figure_spec$directives
  expect_equal(d$x_axis$value, "ADVS.AVISITN (label ADVS.AVISIT)")
  expect_equal(d$y_axis$value, "mean of ADVS.AVAL")
  expect_equal(d$series$value, "ADVS.TRTA")
  expect_equal(d$filter$value, "ADVS.PARAMCD='PULSE'")
  expect_equal(d$error_bars$value, "SE = sd(ADVS.AVAL) / sqrt(n)")
})

test_that("the source dataset comes off the figure's Source line", {
  expect_equal(figure()$source_datasets, "ADVS")
})

test_that("an aspect arsbridge does not know is reported and kept", {
  ## "Reference line -> 0" is a real instruction arsbridge cannot act on.
  ## Dropping it would mean the reviewer never learns the shell asked for it.
  diag_reset()
  secs <- suppressMessages(suppressWarnings(
    parse_shell_xlsx(test_path("fixtures", "shells_apx_drm_301.xlsx"))))
  sec <- Filter(function(s) identical(s$tlf_type, "FIGURE"), secs)[[1]]

  expect_equal(sec$figure_spec$unmatched, "Reference line -> 0")
  expect_true("Reference line -> 0" %in% sec$programmer_annotations)
  expect_true(any(grepl("not recognised as a directive",
                        diag_records()$problem)))
})

test_that("the annotation block records where it starts", {
  ## The anchor is where the computed series data gets written when the shell
  ## is filled, so it has to survive parsing.
  sec <- figure()
  expect_true(is.numeric(sec$figure_spec$anchor_row))
  expect_equal(sec$figure_spec$anchor_row, 7L)
})

test_that("the black specification block is not read as instructions", {
  ## Row 5 is a human-facing "Chart type / Line plot with error bars" pair in
  ## black. Only colour distinguishes it from a directive.
  sec <- figure()
  expect_false("chart_type" %in% names(sec$figure_spec$directives))
  expect_false(any(grepl("Line plot", sec$programmer_annotations)))
})

test_that("a figure's specification never becomes footnotes", {
  ## The defect ADR 0003 names for the Word reader: programmer instructions
  ## shipped as display footnotes. A figure sheet is where that would recur,
  ## because its instruction lines are merged across the sheet exactly as a
  ## footnote is.
  sec <- figure()
  expect_false(any(grepl("->", sec$footnotes, fixed = TRUE)))
})
