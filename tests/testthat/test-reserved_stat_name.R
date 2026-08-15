## The cell map and the ARD must name a reserved statistic the same way.
##
## The defect class: two independent code paths derive a statistic's name.
## `.build_table_cells()` derives a cell's stat_name from the method's
## operation, via `.operation_stat_name()`; `.UNEXECUTABLE_METHODS` declares
## what the ARD will write when the method reserves instead of computing.
## `.ard_value()` joins them on that name, so a disagreement does not fail
## loudly -- the lookup simply misses and returns "no result in the ARD".
##
## For a reserved cell that message is actively wrong. "No result in the ARD"
## reads as "the analysis was skipped or failed"; the truth is "this is
## reserved for someone to derive by hand". The reviewer is sent hunting for a
## broken analysis that does not exist.
##
## The invariant: for a declarative method whose operations correspond one to
## one with the statistics it reserves, the name the cell map produces IS the
## name the ARD carries.


## The stat names the cell map will write for a method object, derived exactly
## as `.build_table_cells()` derives them.
.rsn_cell_stats <- function(method) {
  vapply(method$operations,
         function(op) .operation_stat_name(op$id, op$name),
         character(1))
}

## The stat names the ARD will carry when that method reserves.
.rsn_ard_stats <- function(method_id) {
  as.character(.UNEXECUTABLE_METHODS[[method_id]]$stats)
}


test_that("a reserved cell is named the same on both sides", {
  ## Built through arsbridge's own constructor, so the test compares what the
  ## package actually produces rather than a transcription of it.
  method <- .build_unsupported_method(list(unsupported_reason = "no executor"))

  cell_side <- .rsn_cell_stats(method)
  ard_side  <- .rsn_ard_stats(method$id)

  expect_length(cell_side, 1L)
  expect_length(ard_side, 1L)
  expect_equal(cell_side, ard_side)

  ## And specifically that it is no longer the lower-cased operation name,
  ## which is what it used to fall through to.
  expect_false(identical(cell_side, tolower(method$operations[[1]]$name)))
})


test_that("a reserved cell resolves to its reason, not to a missing result", {
  ## The consequence the invariant exists for, asserted end to end rather than
  ## on the constants: a cell whose ARD row is reserved must report WHY.
  expect_equal(.pending_reason("manual_pending"),
               "reserved for manual derivation")

  ## The join key the two sides meet on. If these drift, `.ard_value()` finds
  ## no row and the reason above is never reached.
  expect_equal(.operation_stat_name("OP_MANUAL", "Manual derivation"),
               .MANUAL_STAT_NAME)
  expect_equal(.rsn_ard_stats("MTH_UNSUPPORTED_ANALYSIS"), .MANUAL_STAT_NAME)
})


## The scope of this fix is the reservation method -- the one every
## unresolvable row lands on, and the one the no-drop path depends on. The
## inferential declarative methods (MTH_CMH_TEST, MTH_PROPORTION_CI_EXACT)
## reach the ARD by a different route and are not asserted here either way:
## repairing them means giving each one operation per declared statistic,
## which is a change to the method catalogue rather than to this mapping.


test_that("a reserved analysis fills as reserved, end to end", {
  ## The whole point of the join, exercised through the engine rather than
  ## through the constants: the ARD row the cell map will look for has to be
  ## the row the ARD actually writes.
  adam <- withr::local_tempdir()
  subjects <- data.frame(
    USUBJID = sprintf("S%02d", 1:6),
    QXARM   = rep(c("A1", "A2"), each = 3),
    QXFL    = "Y",
    QXCAT   = c("P", "P", "Q", "Q", "R", "R"),
    stringsAsFactors = FALSE
  )
  utils::write.csv(subjects, file.path(adam, "ADQX.csv"), row.names = FALSE)

  ## arsbridge's own reservation method. A method id the ARD engine does not
  ## know is a different problem -- it never reaches the stub path at all --
  ## and is covered where that gap is closed, not here.
  method <- .rsv_reserved_method(id = "MTH_UNSUPPORTED_ANALYSIS")
  event <- .rsv_event(method = method)
  path <- file.path(withr::local_tempdir(), "re.json")
  writeLines(
    jsonlite::toJSON(event, auto_unbox = TRUE, pretty = TRUE, null = "null"),
    path
  )

  ard <- as.data.frame(
    suppressMessages(suppressWarnings(ars_to_ard(path, adam)))
  )

  reserved <- ard[ard$analysis_id == "AN_SYNTH_001", , drop = FALSE]
  expect_gt(nrow(reserved), 0L)

  ## Reserved, not computed -- and named the way the cell map will ask for it.
  expect_true(all(reserved$result_status == "manual_pending"))
  expect_true(all(is.na(reserved$stat)))
  expect_true(.MANUAL_STAT_NAME %in% reserved$stat_name)

  ## And that name is exactly what the cell map derives for the same method,
  ## which is what makes the lookup meet rather than miss.
  expect_true(
    all(.rsn_cell_stats(method) %in% reserved$stat_name)
  )
})


test_that("reintroducing the drift breaks the agreement", {
  ## Mutation: take OP_MANUAL back out of the mapping, so it falls through to
  ## the lower-cased operation name again, and assert the agreement asserted
  ## above no longer holds.
  original <- get(".OP_STAT_NAMES", envir = asNamespace("arsbridge"))
  withr::defer(.rsv_restore(".OP_STAT_NAMES", original))

  .rsv_install(".OP_STAT_NAMES", original[names(original) != "OP_MANUAL"])

  method <- .build_unsupported_method(list(unsupported_reason = "no executor"))

  expect_false(identical(.rsn_cell_stats(method),
                         .rsn_ard_stats(method$id)))
  expect_equal(.rsn_cell_stats(method), "manual derivation")
})
