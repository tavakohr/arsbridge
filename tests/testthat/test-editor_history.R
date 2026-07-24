## Undo/redo and crash recovery.
##
## Both exist so that a review session cannot be lost -- to a mis-click or to a
## browser that dies. These tests are about that promise rather than about the
## mechanics.

.history_state <- function(source_path = NULL) {
  model <- ars_to_model(
    test_path("fixtures", "ars_apx_drm_301_deterministic.json")
  )
  .editor_state(model, NULL, NULL, source_path, "edit")
}

.with_reactives <- function() {
  shiny::reactiveConsole(TRUE)
  withr::defer(shiny::reactiveConsole(FALSE), envir = parent.frame())
}


test_that("an edit can be undone and redone", {
  skip_if_not_installed("shiny")
  .with_reactives()

  state <- .history_state()
  target <- state$model()$analyses$id[1]
  original <- state$model()$analyses$label[1]

  expect_false(.can_undo(state))
  expect_false(.can_redo(state))

  apply_edit(state, "analyses", target, "label", "Changed")
  expect_true(.can_undo(state))

  .undo(state)
  expect_equal(state$model()$analyses$label[1], original)
  expect_equal(nrow(state$edit_log()), 0)
  expect_true(.can_redo(state))

  .redo(state)
  expect_equal(state$model()$analyses$label[1], "Changed")
  expect_equal(nrow(state$edit_log()), 1)
})

test_that("undo steps back one edit at a time", {
  skip_if_not_installed("shiny")
  .with_reactives()

  state <- .history_state()
  target <- state$model()$analyses$id[1]
  original <- state$model()$analyses$label[1]

  apply_edit(state, "analyses", target, "label", "One")
  apply_edit(state, "analyses", target, "label", "Two")
  apply_edit(state, "analyses", target, "label", "Three")

  .undo(state)
  expect_equal(state$model()$analyses$label[1], "Two")
  .undo(state)
  expect_equal(state$model()$analyses$label[1], "One")
  .undo(state)
  expect_equal(state$model()$analyses$label[1], original)

  expect_false(.can_undo(state))
  expect_false(.undo(state))
})

test_that("undo restores the findings, not just the model", {
  skip_if_not_installed("shiny")
  .with_reactives()

  state <- .history_state()
  target <- state$model()$analyses$id[1]
  expect_equal(sum(state$findings()$severity == "FAIL"), 0)

  apply_edit(state, "analyses", target, "methodId", "MTH_GONE")
  expect_gt(sum(state$findings()$severity == "FAIL"), 0)

  .undo(state)
  expect_equal(sum(state$findings()$severity == "FAIL"), 0)
})

test_that("a new edit after undoing abandons the redo branch", {
  skip_if_not_installed("shiny")
  .with_reactives()

  state <- .history_state()
  target <- state$model()$analyses$id[1]

  apply_edit(state, "analyses", target, "label", "One")
  .undo(state)
  expect_true(.can_redo(state))

  apply_edit(state, "analyses", target, "label", "Different")
  expect_false(.can_redo(state))
  expect_false(.redo(state))
})

test_that("a no-op edit does not consume a history step", {
  skip_if_not_installed("shiny")
  .with_reactives()

  state <- .history_state()
  target <- state$model()$analyses$id[1]
  current <- state$model()$analyses$label[1]

  apply_edit(state, "analyses", target, "label", current)
  expect_false(.can_undo(state))
})

test_that("structural edits are undoable too", {
  skip_if_not_installed("shiny")
  .with_reactives()

  state <- .history_state()
  before <- nrow(state$model()$analyses)

  updated <- model_add_analysis(
    state$model(), output_id = "T_14_1_2", label = "Added",
    dataset = "ADSL", variable = "SMOKFL",
    method_id = "MTH_COUNT_AND_PERCENTAGE",
    analysis_set_id = state$model()$analysis_sets$id[1]
  )
  .record_structural_edit(state, updated, "analyses",
                          attr(updated, "last_added"), "added", "", "Added")

  expect_equal(nrow(state$model()$analyses), before + 1)

  .undo(state)
  expect_equal(nrow(state$model()$analyses), before)
  expect_equal(nrow(state$edit_log()), 0)
})

test_that("undo tells the panels to redraw", {
  skip_if_not_installed("shiny")
  .with_reactives()

  ## The detail panels deliberately do not follow every model change, so
  ## without this signal an undone edit stays visible in its input box while
  ## the model underneath says otherwise.
  state <- .history_state()
  target <- state$model()$analyses$id[1]
  before <- state$refresh()

  apply_edit(state, "analyses", target, "label", "Changed")
  expect_equal(state$refresh(), before)

  .undo(state)
  expect_gt(state$refresh(), before)

  after_undo <- state$refresh()
  .redo(state)
  expect_gt(state$refresh(), after_undo)
})

test_that("history does not grow without limit", {
  skip_if_not_installed("shiny")
  .with_reactives()

  state <- .history_state()
  target <- state$model()$analyses$id[1]

  for (i in seq_len(.HISTORY_LIMIT + 10L)) {
    apply_edit(state, "analyses", target, "label", paste("Edit", i))
  }

  expect_equal(length(state$history()$past), .HISTORY_LIMIT)
})


test_that("an edit is autosaved without touching the file being edited", {
  skip_if_not_installed("shiny")
  .with_reactives()

  dir <- withr::local_tempdir()
  path <- file.path(dir, "reporting_event.json")
  file.copy(
    test_path("fixtures", "ars_apx_drm_301_deterministic.json"), path
  )
  before <- readLines(path, warn = FALSE)
  withr::defer(.clear_autosave(path))

  expect_null(.read_autosave(path))

  state <- .history_state(path)
  apply_edit(state, "analyses", state$model()$analyses$id[1], "label",
             "Recovered")

  recovered <- .read_autosave(path)
  expect_false(is.null(recovered))
  expect_equal(nrow(recovered$edit_log), 1)
  expect_equal(recovered$model$analyses$label[1], "Recovered")

  ## The whole point: the file on disk is untouched until an explicit save.
  expect_identical(readLines(path, warn = FALSE), before)
})

test_that("two files do not share recovery data", {
  dir <- withr::local_tempdir()
  first <- file.path(dir, "study-a.json")
  second <- file.path(dir, "nested")
  dir.create(second)
  second <- file.path(second, "study-a.json")

  expect_false(.autosave_path(first) == .autosave_path(second))
  ## Same path asked twice resolves to the same slot.
  expect_equal(.autosave_path(first), .autosave_path(first))
  expect_null(.autosave_path(NULL))
})

test_that("a session with nothing changed offers nothing to recover", {
  skip_if_not_installed("shiny")
  .with_reactives()

  dir <- withr::local_tempdir()
  path <- file.path(dir, "reporting_event.json")
  file.copy(
    test_path("fixtures", "ars_apx_drm_301_deterministic.json"), path
  )
  withr::defer(.clear_autosave(path))

  state <- .history_state(path)
  .write_autosave(state)

  ## Written, but there is nothing worth offering back.
  expect_null(.read_autosave(path))
})

test_that("saving clears the recovery copy", {
  skip_if_not_installed("shiny")
  .with_reactives()

  dir <- withr::local_tempdir()
  path <- file.path(dir, "reporting_event.json")
  file.copy(
    test_path("fixtures", "ars_apx_drm_301_deterministic.json"), path
  )
  withr::defer(.clear_autosave(path))

  state <- .history_state(path)
  apply_edit(state, "analyses", state$model()$analyses$id[1], "label", "X")
  expect_false(is.null(.read_autosave(path)))

  suppressMessages(.edit_ars_finish(
    list(model = state$model(), edit_log = state$edit_log(),
         source_path = path),
    path
  ))

  expect_null(.read_autosave(path))
})

test_that("a corrupt recovery file is ignored rather than fatal", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "reporting_event.json")
  file.create(path)
  withr::defer(.clear_autosave(path))

  autosave_path <- .autosave_path(path)
  dir.create(dirname(autosave_path), recursive = TRUE, showWarnings = FALSE)
  writeLines("not an rds file", autosave_path)

  expect_null(.read_autosave(path))
})
