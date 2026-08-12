## Undo/redo and crash recovery.
##
## Both exist so that a review session cannot be lost -- to a mis-click or to a
## browser that dies. These tests are about that promise rather than about the
## mechanics.

.history_state <- function(source_path = NULL) {
  .editor_state(.valid_fixture_model(), NULL, NULL, source_path, "edit")
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

test_that("structural edits tell the panels to redraw", {
  skip_if_not_installed("shiny")
  .with_reactives()

  ## A moved line that stays put on screen and stale fields after a raw-JSON
  ## replacement were both this: the model changed, the panel did not.
  state <- .history_state()
  before <- state$refresh()

  moved <- model_move_analysis(
    state$model(), "T_14_1_2",
    .split_values(state$model()$outputs$referenced_analysis_ids[
      state$model()$outputs$id == "T_14_1_2"
    ])[2],
    -1
  )
  .record_structural_edit(state, moved, "outputs", "T_14_1_2",
                          "analysis order", "x", "moved up")

  expect_gt(state$refresh(), before)
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

# The suite must leave the machine it ran on exactly as it found it. These
# pin the redirection itself, because the thing being prevented -- a stray
# .rds under the real user cache -- is invisible from inside a passing test.

test_that("the autosave directory is redirected off the real user cache", {
  redirected <- .autosave_dir()
  real <- withr::with_options(
    list(arsbridge.autosave_dir = NULL),
    tools::R_user_dir("arsbridge", "cache")
  )

  expect_false(identical(normalizePath(redirected, mustWork = FALSE),
                         normalizePath(real, mustWork = FALSE)))

  # And it is genuinely somewhere disposable, not just a different fixed path.
  expect_true(startsWith(normalizePath(redirected, mustWork = FALSE),
                         normalizePath(tempdir(), mustWork = FALSE)))
})

test_that("writing an autosave adds nothing to the real user cache", {
  skip_if_not_installed("shiny")
  .with_reactives()

  real <- withr::with_options(
    list(arsbridge.autosave_dir = NULL),
    tools::R_user_dir("arsbridge", "cache")
  )
  before <- list.files(real, all.files = TRUE, no.. = TRUE)

  path <- file.path(withr::local_tempdir(), "reporting_event.json")
  state <- .history_state(path)
  withr::defer(.clear_autosave(path))

  apply_edit(state, "analyses", state$model()$analyses$id[1], "label", "Edited")
  .write_autosave(state)

  # The autosave was really written -- otherwise this test would pass by
  # doing nothing at all.
  expect_false(is.null(.read_autosave(path)))
  expect_true(file.exists(.autosave_path(path)))
  expect_true(startsWith(normalizePath(.autosave_path(path), mustWork = FALSE),
                         normalizePath(tempdir(), mustWork = FALSE)))

  after <- list.files(real, all.files = TRUE, no.. = TRUE)
  expect_identical(after, before)
})

# Retention. An autosave is cleared on save and on "start fresh", so what
# accumulates is what nobody came back for. These pin that the sweep bounds
# that pile without touching work still worth offering.

## The suite redirects the cache once for the whole run, which is right for
## keeping the machine clean but wrong here: these tests age files and then
## count what a sweep took, so one test's stale file would be swept by the
## next test's call. Each gets its own directory.
.local_autosave_dir <- function(envir = parent.frame()) {
  withr::local_options(
    list(arsbridge.autosave_dir = withr::local_tempdir(.local_envir = envir)),
    .local_envir = envir
  )
}

## Put an autosave on disk and backdate it, standing in for a session that
## died that many days ago.
.aged_autosave <- function(days_old, envir = parent.frame()) {
  path <- file.path(withr::local_tempdir(.local_envir = envir),
                    "reporting_event.json")
  state <- .history_state(path)
  apply_edit(state, "analyses", state$model()$analyses$id[1], "label", "Edited")
  rds <- .write_autosave(state)
  Sys.setFileTime(rds, Sys.time() - as.difftime(days_old, units = "days"))
  list(source = path, rds = rds)
}

test_that("an autosave nobody came back for is swept once it expires", {
  skip_if_not_installed("shiny")
  .with_reactives()
  .local_autosave_dir()

  stale <- .aged_autosave(31)
  expect_true(file.exists(stale$rds))

  swept <- .sweep_autosaves()

  expect_false(file.exists(stale$rds))
  expect_identical(normalizePath(swept, mustWork = FALSE),
                   normalizePath(stale$rds, mustWork = FALSE))
  ## And the offer goes with it -- the sweep is what decides, not the reader.
  expect_null(.read_autosave(stale$source))
})

test_that("work still inside the window is left alone", {
  skip_if_not_installed("shiny")
  .with_reactives()
  .local_autosave_dir()

  fresh <- .aged_autosave(29)
  expect_length(.sweep_autosaves(), 0)
  expect_true(file.exists(fresh$rds))
  expect_false(is.null(.read_autosave(fresh$source)))
})

test_that("the retention window is configurable", {
  skip_if_not_installed("shiny")
  .with_reactives()
  .local_autosave_dir()

  aged <- .aged_autosave(10)
  ## Untouched by the default window, gone under a shorter one.
  expect_length(.sweep_autosaves(), 0)
  expect_true(file.exists(aged$rds))

  withr::with_options(list(arsbridge.autosave_max_age_days = 7), {
    expect_length(.sweep_autosaves(), 1)
  })
  expect_false(file.exists(aged$rds))
})

test_that("the sweep only deletes files it wrote", {
  skip_if_not_installed("shiny")
  .with_reactives()
  .local_autosave_dir()

  ## The cache directory is shared -- the recent-projects list lives beside
  ## these -- so age alone must not be the whole test for deletion.
  stale <- .aged_autosave(60)
  bystander <- file.path(.autosave_dir(), "recent_projects.json")
  writeLines("[]", bystander)
  Sys.setFileTime(bystander, Sys.time() - as.difftime(60, units = "days"))

  .sweep_autosaves()

  expect_false(file.exists(stale$rds))
  expect_true(file.exists(bystander))
})

test_that("sweeping an empty or absent cache is harmless", {
  withr::with_options(
    list(arsbridge.autosave_dir = file.path(tempfile("no_cache_"), "nested")),
    expect_length(.sweep_autosaves(), 0)
  )
  withr::with_options(
    list(arsbridge.autosave_dir = withr::local_tempdir()),
    expect_length(.sweep_autosaves(), 0)
  )
})
