## export_edit_log(): the review session as a QC record.
##
## In a regulated setting, a deliverable a human changed has to say what was
## changed, by whom and when. The ARS JSON deliberately carries none of that,
## so this is where it lives.

.export_log <- function() {
  data.frame(
    time = c("2026-07-23T10:00:00Z", "2026-07-23T10:05:00Z",
             "2026-07-23T10:06:00Z"),
    pool = "analyses",
    id = c("AN_1", "AN_1", "AN_2"),
    field = c("label", "label", "methodId"),
    old = c("First", "Second", "MTH_X"),
    new = c("Second", "Third", "MTH_Y"),
    stringsAsFactors = FALSE
  )
}

.saved_session <- function(dir) {
  path <- file.path(dir, "reporting_event.json")
  file.copy(
    test_path("fixtures", "ars_apx_drm_301_deterministic.json"), path
  )

  model <- ars_to_model(path)
  target <- model$analyses$id[1]
  edited <- model_set_field(model, "analyses", target, "label", "Renamed")

  log <- data.frame(
    time = "2026-07-23T10:00:00Z", pool = "analyses", id = target,
    field = "label", old = "Randomized", new = "Renamed",
    stringsAsFactors = FALSE
  )
  suppressMessages(.edit_ars_finish(
    list(model = edited, edit_log = log, source_path = path), path
  ))
  path
}


test_that("the sidecar a save wrote can be exported as a workbook", {
  skip_if_not_installed("openxlsx2")
  skip_if_not_installed("readxl")

  dir <- withr::local_tempdir()
  path <- .saved_session(dir)

  out <- suppressMessages(export_edit_log(path))

  expect_true(file.exists(out))
  expect_match(out, "\\.xlsx$")
  expect_setequal(readxl::excel_sheets(out),
                  c("Summary", "All changes", "Session"))

  summary <- readxl::read_excel(out, sheet = "Summary")
  expect_equal(nrow(summary), 1)
  expect_equal(summary$Before, "Randomized")
  expect_equal(summary$After, "Renamed")
})

test_that("the session sheet records who saved it and with what", {
  skip_if_not_installed("openxlsx2")
  skip_if_not_installed("readxl")

  dir <- withr::local_tempdir()
  path <- .saved_session(dir)
  out <- suppressMessages(export_edit_log(path))

  session <- readxl::read_excel(out, sheet = "Session")
  version <- session$Value[session$Item == "arsbridge version"]

  expect_equal(version, as.character(utils::packageVersion("arsbridge")))
  expect_true(nzchar(session$Value[session$Item == "Saved by"]))
  expect_true(nzchar(session$Value[session$Item == "Saved at (UTC)"]))
})

test_that("the sidecar path is found from either name", {
  dir <- withr::local_tempdir()
  path <- .saved_session(dir)
  sidecar <- file.path(dir, "reporting_event.edits.json")

  expect_equal(.edit_log_path(path), sidecar)
  expect_equal(.edit_log_path(sidecar), sidecar)
})

test_that("repeated edits to one field collapse to a single row", {
  skip_if_not_installed("openxlsx2")
  skip_if_not_installed("readxl")

  out <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(export_edit_log(.export_log(), out))

  summary <- readxl::read_excel(out, sheet = "Summary")
  expect_equal(nrow(summary), 2)

  first <- summary[summary$Entity == "AN_1", ]
  expect_equal(first$Before, "First")
  expect_equal(first$After, "Third")

  ## Every individual edit is still there for anyone who needs the detail.
  all_changes <- readxl::read_excel(out, sheet = "All changes")
  expect_equal(nrow(all_changes), 3)
})

test_that("a session that changed nothing exports without failing", {
  skip_if_not_installed("openxlsx2")
  skip_if_not_installed("readxl")

  out <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(export_edit_log(.new_edit_log(), out))

  summary <- readxl::read_excel(out, sheet = "Summary")
  expect_equal(nrow(summary), 1)
  expect_match(summary[[1]][1], "No changes")
})

test_that("a data frame needs an explicit destination", {
  expect_error(export_edit_log(.export_log()), "output_path")
})

test_that("something that is not an edit log is refused", {
  out <- withr::local_tempfile(fileext = ".xlsx")
  expect_error(
    export_edit_log(data.frame(nope = 1), out),
    "does not look like an arsbridge edit log"
  )
})

test_that("a missing sidecar is reported clearly", {
  dir <- withr::local_tempdir()
  expect_error(
    export_edit_log(file.path(dir, "never_saved.json")),
    class = "rlang_error"
  )
})

test_that("review_ars() is the same function as edit_ars()", {
  expect_identical(review_ars, edit_ars)
  expect_true(is.function(review_ars))
})
