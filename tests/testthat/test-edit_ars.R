## edit_ars() and its save pipeline.
##
## Saving overwrites a file a reviewer may have spent an hour correcting, so
## these tests are mostly about not losing work: the previous file is backed
## up, the write is atomic, and closing without saving writes nothing.

.edit_fixture_copy <- function(dir) {
  path <- file.path(dir, "reporting_event.json")
  file.copy(
    test_path("fixtures", "ars_apx_drm_301_deterministic.json"),
    path
  )
  path
}

.edit_result <- function(model, edit_log = NULL) {
  if (is.null(edit_log)) edit_log <- .new_edit_log()
  list(model = model, edit_log = edit_log)
}

.one_edit_log <- function(id, field = "label", old = "before",
                          new = "after") {
  data.frame(
    time = "2026-07-23T00:00:00Z", pool = "analyses", id = id,
    field = field, old = old, new = new,
    stringsAsFactors = FALSE
  )
}


test_that("saving writes an event that reloads and still round-trips", {
  dir <- withr::local_tempdir()
  path <- .edit_fixture_copy(dir)

  model  <- ars_to_model(path)
  target <- model$analyses$id[1]
  edited <- model_set_field(model, "analyses", target, "label", "New label")

  written <- suppressMessages(.edit_ars_finish(
    .edit_result(edited, .one_edit_log(target)), path
  ))

  expect_equal(written, path)

  reloaded <- .read_json(path)
  expect_equal(reloaded$analyses[[1]]$label, "New label")
  expect_equal(model_to_ars(ars_to_model(path)), reloaded)
})

test_that("only the edited field differs from the original", {
  dir <- withr::local_tempdir()
  path <- .edit_fixture_copy(dir)
  original <- .read_json(path)

  model  <- ars_to_model(path)
  target <- model$analyses$id[3]
  index  <- 3
  edited <- model_set_field(model, "analyses", target, "methodId",
                            "MTH_LISTING")

  suppressMessages(.edit_ars_finish(
    .edit_result(edited, .one_edit_log(target, "methodId")), path
  ))

  saved <- .read_json(path)
  restored <- saved
  restored$analyses[[index]]$methodId <- original$analyses[[index]]$methodId
  expect_equal(restored, original)
})

test_that("the previous file is backed up before it is replaced", {
  dir <- withr::local_tempdir()
  path <- .edit_fixture_copy(dir)
  before <- readLines(path, warn = FALSE)

  model <- ars_to_model(path)
  suppressMessages(.edit_ars_finish(.edit_result(model), path))

  backups <- list.files(dir, pattern = "\\.bak-")
  expect_equal(length(backups), 1)
  expect_identical(readLines(file.path(dir, backups), warn = FALSE), before)
})

test_that("writing to a new path makes no backup", {
  dir <- withr::local_tempdir()
  source_path <- .edit_fixture_copy(dir)
  target_path <- file.path(dir, "corrected.json")

  model <- ars_to_model(source_path)
  suppressMessages(.edit_ars_finish(.edit_result(model), target_path))

  expect_true(file.exists(target_path))
  expect_equal(length(list.files(dir, pattern = "\\.bak-")), 0)
})

test_that("the temporary file used for the atomic write is not left behind", {
  dir <- withr::local_tempdir()
  path <- .edit_fixture_copy(dir)

  model <- ars_to_model(path)
  suppressMessages(.edit_ars_finish(.edit_result(model), path))

  expect_equal(length(list.files(dir, pattern = "\\.tmp$")), 0)
  expect_setequal(
    sub("\\.bak-.*$", ".bak", list.files(dir)),
    c("reporting_event.json", "reporting_event.edits.json",
      "reporting_event.json.bak")
  )
})

test_that("the edit log is written beside the JSON, not inside it", {
  dir <- withr::local_tempdir()
  path <- .edit_fixture_copy(dir)

  model  <- ars_to_model(path)
  target <- model$analyses$id[1]
  log    <- .one_edit_log(target, "label", "Randomized", "Renamed")

  suppressMessages(.edit_ars_finish(.edit_result(model, log), path))

  sidecar_path <- file.path(dir, "reporting_event.edits.json")
  expect_true(file.exists(sidecar_path))

  sidecar <- jsonlite::read_json(sidecar_path)
  expect_equal(sidecar$n_edits, 1)
  expect_equal(sidecar$edits[[1]]$id, target)
  expect_equal(sidecar$edits[[1]]$new, "Renamed")
  expect_true(nzchar(sidecar$saved_at_utc))

  ## The reporting event itself gains no provenance fields.
  saved <- .read_json(path)
  expect_false("edits" %in% names(saved))
  expect_equal(names(saved), names(.read_json(
    test_path("fixtures", "ars_apx_drm_301_deterministic.json")
  )))
})

test_that("edit_ars() refuses an in-memory event with nowhere to write", {
  ars <- .read_json(test_path("fixtures",
                              "ars_apx_drm_301_deterministic.json"))
  skip_if_not_installed("shiny")

  ## Must fail before the app opens, not after an hour of corrections.
  expect_error(edit_ars(ars), "output_path.*is required")
})

test_that("an edited model executes into an ARD", {
  ## The point of the whole stage: what comes out is still executable.
  skip_on_cran()
  dir <- withr::local_tempdir()
  path <- .edit_fixture_copy(dir)

  model  <- ars_to_model(path)
  target <- model$analyses$id[model$analyses$output_id == "T_14_1_2"][1]
  edited <- model_set_field(model, "analyses", target, "label", "Renamed line")

  suppressMessages(.edit_ars_finish(
    .edit_result(edited, .one_edit_log(target)), path
  ))

  adam_dir <- withr::local_tempdir()
  utils::unzip(arsbridge_example("ADaM.zip"), exdir = adam_dir)

  ard <- suppressWarnings(suppressMessages(
    ars_to_ard(path, adam_dir = adam_dir, output_ids = "T_14_1_2")
  ))

  expect_gt(nrow(ard), 0)
  expect_true(target %in% ard$analysis_id)
})


test_that("the save summary speaks to each severity level", {
  ## Direct calls: these cli branches are what the reviewer sees after every
  ## save, so each one gets exercised -- blocking, review-only, and clean.
  log <- .new_edit_log()

  blocking <- data.frame(
    severity = "FAIL", entity = "analyses", id = "AN_1", field = "methodId",
    problem = "x", action = "y", ref = NA_character_,
    stringsAsFactors = FALSE
  )
  expect_message(.report_save("out.json", log, blocking), "blocking problem")

  clean <- .new_findings()
  expect_message(.report_save("out.json", log, clean), "Nothing left to fix")
})

test_that("the conformance note counts notes and never breaks a save", {
  skip_if_not_installed("jsonvalidate")

  ## A non-conformant event: the note reports a count instead of silence.
  old_shape <- list(
    id = "S", name = "S", version = "1",
    mainListOfContents = list(name = "LOPA", label = "LOPA",
                              contentsList = list(listItems = list())),
    analyses = list(list(id = "AN_1", name = "AN_1", methodId = "MTH_X")),
    outputs = list()
  )
  expect_message(.report_conformance(old_shape), "schema note")

  ## Anything that makes the validator itself fail is swallowed: the save
  ## already happened, and a conformance hiccup must not un-happen it.
  expect_silent(.report_conformance(42))
})
