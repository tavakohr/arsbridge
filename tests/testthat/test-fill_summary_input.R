## A-14: ars_fill_summary() on the object a fill actually returns.
##
## General defect class: an exported helper documented to take one piece of a
## result died with an internal message when handed the result itself. The
## failure was "missing value where TRUE/FALSE needed" -- raised inside a
## comparison, naming neither the argument at fault nor what to pass instead.
## The census was well formed the whole time.
##
## General invariant: a reader has the fill result in hand, so the summary
## takes either that result or the census inside it. Every other input class is
## decided in one place and named plainly, rather than by whichever later
## expression happens to fail first.

.fsi_census <- function() {
  ## Shaped like a real census -- the columns the summary actually reads --
  ## without running a fill, so this file stays a test of the helper.
  data.frame(
    sheet     = c("Table 14.1.1", "Table 14.1.1", "Table 14.1.1"),
    col       = c(2L, 3L, 2L),
    col_label = c("Alfa", "Bravo", "Alfa"),
    row_label = c("Subjects, n", "Subjects, n", "Mean (SD)"),
    ref       = c("B5", "C5", "B6"),
    status    = c("filled", "pending", "filled"),
    reason    = c(NA_character_, "no analysis covers this row", NA_character_),
    stringsAsFactors = FALSE
  )
}

## The shape `ars_fill_shell()` returns: the census plus its siblings.
.fsi_result <- function(census = .fsi_census()) {
  list(path = "somewhere.xlsx", filled = 2L, pending = 1L, skipped = 0L,
       census = census, findings = census[0, , drop = FALSE])
}


test_that("the summary accepts the result a fill returns, and the census in it", {
  from_result <- ars_fill_summary(.fsi_result())
  from_census <- ars_fill_summary(.fsi_census())

  ## The same answer either way -- the unwrapping must not summarise something
  ## different from what the census alone would.
  expect_equal(from_result, from_census)

  ## Non-vacuity: a real summary, not three empty tables. Without this the
  ## equality above would hold on a build that returned nothing for both.
  expect_equal(nrow(from_result$sheets), 1L)
  expect_equal(from_result$sheets$cells, 3L)
  expect_equal(from_result$sheets$filled, 2L)
  expect_equal(from_result$sheets$pending, 1L)
  expect_equal(nrow(from_result$reasons), 1L)
  expect_equal(from_result$reasons$reason, "no analysis covers this row")
})


test_that("an empty or absent census summarises to three empty tables", {
  for (input in list(NULL, .fsi_census()[0, , drop = FALSE])) {
    res <- ars_fill_summary(input)
    expect_named(res, c("sheets", "columns", "reasons"))
    for (tbl in res) expect_equal(nrow(tbl), 0L)
  }

  ## A result whose census is empty behaves as the empty census does, so the
  ## unwrapping does not turn "nothing to summarise" into an error.
  res <- ars_fill_summary(.fsi_result(.fsi_census()[0, , drop = FALSE]))
  for (tbl in res) expect_equal(nrow(tbl), 0L)
})


test_that("an input that is neither is refused by name", {
  ## The point of the change: the message says which argument is wrong and
  ## what to pass. A regression here would most likely be the OLD behaviour --
  ## an error from deep inside a comparison -- so the text is asserted, not
  ## merely the fact that something was thrown.
  expect_error(ars_fill_summary("not a census"), "must be a fill census",
               class = "rlang_error")
  expect_error(ars_fill_summary(42), "must be a fill census")

  ## A list that carries no census is not silently treated as one.
  expect_error(ars_fill_summary(list(path = "x.xlsx", filled = 1L)),
               "must be a fill census")

  ## And the old failure mode is gone: none of these reports a comparison.
  for (bad in list("not a census", 42, list(path = "x.xlsx"))) {
    msg <- tryCatch(ars_fill_summary(bad), error = conditionMessage)
    expect_false(grepl("TRUE/FALSE", msg), info = class(bad)[[1]])
  }
})
