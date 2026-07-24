## view_ars() and its input handling.
##
## The contract worth pinning here is the one a user meets first: you may pass
## a path, an already parsed event, or the whole spec_to_ars() result -- and in
## the last case the spec and validation report come along for free.

.view_fixture_path <- function() {
  test_path("fixtures", "ars_apx_drm_301_deterministic.json")
}

test_that("a path is normalized into a model", {
  input <- .normalize_ars_input(.view_fixture_path())

  expect_s3_class(input$model, "ars_model")
  expect_equal(input$source_path, .view_fixture_path())
  expect_null(input$spec)
  expect_null(input$report)
})

test_that("an already parsed event is normalized into a model", {
  ars <- .read_json(.view_fixture_path())
  input <- .normalize_ars_input(ars)

  expect_s3_class(input$model, "ars_model")
  expect_null(input$source_path)
  expect_gt(nrow(input$model$analyses), 0)
})

test_that("a spec_to_ars() result brings its own report and paths", {
  ## Stand in for the result rather than running the pipeline: the contract
  ## is about which fields are read, not how they were produced.
  result <- list(
    reporting_event = .read_json(.view_fixture_path()),
    validation      = utils::read.csv(
      test_path("fixtures", "ars_apx_drm_301_validation.csv"),
      stringsAsFactors = FALSE
    ),
    ars_path        = .view_fixture_path(),
    report_path     = NULL,
    adam_spec_path  = arsbridge_example("adam_spec.xlsx")
  )

  input <- .normalize_ars_input(result)

  expect_s3_class(input$model, "ars_model")
  expect_equal(input$source_path, .view_fixture_path())
  expect_s3_class(input$report, "data.frame")
  expect_gt(nrow(input$report), 0)
  expect_false(is.null(input$spec))
  expect_true("lookup" %in% names(input$spec))
})

test_that("an explicit spec path is read into dropdown-ready lookups", {
  input <- .normalize_ars_input(
    .view_fixture_path(),
    adam_spec_path = arsbridge_example("adam_spec.xlsx")
  )

  expect_true(all(c("variables", "lookup", "codelists") %in% names(input$spec)))
  expect_true("ADSL.AGE" %in% names(input$spec$lookup))
})

test_that("the validation report is read from the sheet by name", {
  ## Blockers push a sheet in front of "Validation", so position is not
  ## reliable -- this is the case that would silently read the wrong table.
  path <- withr::local_tempfile(fileext = ".xlsx")
  validation <- data.frame(
    tlf_number  = "T-14-1-1",
    stub_label  = "Age",
    annotation  = "ADSL.AGE",
    variable_ref = "ADSL.AGE",
    status      = "PASS",
    message     = "Variable found in ADaM spec",
    stringsAsFactors = FALSE
  )
  blockers <- data.frame(
    severity = "FAIL", stage = "build_ars", tlf_number = "T-14-1-1",
    location = "", problem = "something", action = "fix it",
    stringsAsFactors = FALSE
  )
  write_validation_report(validation, path, blockers = blockers)

  expect_false(identical(readxl::excel_sheets(path)[1], "Validation"))

  report <- .read_validation_report(path)
  expect_equal(nrow(report), 1)
  expect_equal(report$variable_ref, "ADSL.AGE")
})

test_that("a report with no findings comes back with zero rows", {
  ## A clean run writes a placeholder sheet rather than the real columns.
  path <- withr::local_tempfile(fileext = ".xlsx")
  write_validation_report(
    data.frame(
      tlf_number = character(0), stub_label = character(0),
      annotation = character(0), variable_ref = character(0),
      status = character(0), message = character(0),
      stringsAsFactors = FALSE
    ),
    path
  )

  report <- .read_validation_report(path)
  expect_s3_class(report, "data.frame")
  expect_equal(nrow(report), 0)
})

test_that("a csv report is read directly", {
  report <- .read_validation_report(
    test_path("fixtures", "ars_apx_drm_301_validation.csv")
  )
  expect_gt(nrow(report), 0)
  expect_true("variable_ref" %in% names(report))
})

test_that("the app object is built for both a full and a minimal event", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  model <- ars_to_model(.view_fixture_path())
  expect_s3_class(.ars_editor_app(model), "shiny.appobj")

  ## An older event with no analyses at all must still open.
  minimal <- ars_to_model(test_path("fixtures", "tfrmt_reporting_event.json"))
  expect_s3_class(.ars_editor_app(minimal), "shiny.appobj")

  expect_error(.ars_editor_app(list(a = 1)), "must be an")
})
