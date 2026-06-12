## Diagnostics collector unit tests + integration with the pipeline's
## silent-fallback touch-points.

test_that("diag_reset clears and diag_records returns a typed empty frame", {
  diag_reset()
  recs <- diag_records()
  expect_s3_class(recs, "data.frame")
  expect_equal(nrow(recs), 0)
  expect_named(recs, c("stage", "severity", "tlf_number", "location",
                       "problem", "action"))
})

test_that("diag_add appends records with full column set", {
  diag_reset()
  diag_add(stage = "parse_shell", severity = "WARN",
           problem = "test problem", tlf_number = "T-14-1-1",
           location = "loc", action = "did a thing")
  diag_add(stage = "build_ars", severity = "FAIL", problem = "second")
  recs <- diag_records()
  expect_equal(nrow(recs), 2)
  expect_equal(recs$stage, c("parse_shell", "build_ars"))
  expect_equal(recs$severity, c("WARN", "FAIL"))
  expect_equal(recs$tlf_number[1], "T-14-1-1")
  expect_true(is.na(recs$tlf_number[2]))
})

test_that("ars_diagnostics() exposes the same records", {
  diag_reset()
  diag_add(stage = "enrich_llm", severity = "INFO", problem = "x")
  expect_equal(nrow(ars_diagnostics()), 1)
  expect_equal(ars_diagnostics()$stage, "enrich_llm")
})

test_that("unparsed where-clause condition is recorded as a diagnostic", {
  diag_reset()
  ## "like" is not a supported comparator -- the part contains a
  ## DATASET.VARIABLE shape, so it must be logged when dropped.
  wc <- parse_where_clause("ADSL.AETERM like 'rash%'")
  recs <- diag_records()
  expect_true(any(recs$stage == "where_clause" & recs$severity == "WARN"))
  expect_true(any(grepl("ADSL.AETERM", recs$location, fixed = TRUE)))
})

test_that("plain prose (no DATASET.VARIABLE shape) is not logged", {
  diag_reset()
  expect_null(parse_where_clause("Safety Population"))
  recs <- diag_records()
  expect_equal(nrow(recs[recs$stage == "where_clause", ]), 0)
})

test_that("parseable where-clause adds no diagnostics", {
  diag_reset()
  wc <- parse_where_clause("ADSL.SAFFL='Y'")
  expect_named(wc, "condition")
  expect_equal(nrow(diag_records()), 0)
})

test_that("unknown ARS method name is recorded by .build_method", {
  diag_reset()
  sec <- list(tlf_number = "T-99-9-9",
              ars_method_name = "Some Novel Bayesian Method")
  mth <- .build_method(sec)
  expect_true(is.list(mth))
  recs <- diag_records()
  expect_true(any(recs$stage == "build_ars" & recs$severity == "WARN" &
                    grepl("Some Novel Bayesian Method", recs$problem)))
  expect_equal(recs$tlf_number[recs$stage == "build_ars"][1], "T-99-9-9")
})

test_that("known ARS method adds no diagnostics", {
  diag_reset()
  sec <- list(tlf_number = "T-1-1-1",
              ars_method_name = "Count and Percentage")
  mth <- .build_method(sec)
  expect_equal(nrow(diag_records()), 0)
})

test_that("spec parser records skipped sheets without a Variable column", {
  skip_if_not_installed("openxlsx2")
  diag_reset()
  path <- tempfile(fileext = ".xlsx")
  wb <- openxlsx2::wb_workbook()
  ## Sheet 1: valid variable-level sheet.
  wb$add_worksheet("ADSL")
  wb$add_data("ADSL", data.frame(Dataset = "ADSL", Variable = "AGE",
                                 Label = "Age", stringsAsFactors = FALSE))
  ## Sheet 2: metadata sheet with no Variable column -> skipped + logged.
  wb$add_worksheet("Notes")
  wb$add_data("Notes", data.frame(Topic = "x", Detail = "y",
                                  stringsAsFactors = FALSE))
  openxlsx2::wb_save(wb, path)

  spec <- parse_adam_spec(path)
  expect_true("ADSL.AGE" %in% names(spec$lookup))
  recs <- diag_records()
  expect_true(any(recs$stage == "parse_spec" & recs$location == "Notes"))
})

test_that("sheet-name dataset fallback that is not ADaM-shaped is flagged", {
  skip_if_not_installed("openxlsx2")
  diag_reset()
  path <- tempfile(fileext = ".xlsx")
  wb <- openxlsx2::wb_workbook()
  ## Variable column but no Dataset column, sheet name not AD*.
  wb$add_worksheet("Demographics")
  wb$add_data("Demographics",
              data.frame(Variable = "AGE", Label = "Age",
                         stringsAsFactors = FALSE))
  openxlsx2::wb_save(wb, path)

  spec <- parse_adam_spec(path)
  recs <- diag_records()
  expect_true(any(recs$stage == "parse_spec" & recs$severity == "WARN" &
                    grepl("Demographics", recs$problem)))
})

test_that("write_validation_report adds Diagnostics sheet when records exist", {
  skip_if_not_installed("openxlsx2")
  validation <- data.frame(
    tlf_number = "T-14-1-1", stub_label = "Age", annotation = "ADSL.AGE",
    variable_ref = "ADSL.AGE", status = "PASS", message = "ok",
    stringsAsFactors = FALSE
  )
  diagnostics <- data.frame(
    stage = "enrich_llm", severity = "FAIL", tlf_number = "T-14-1-1",
    location = "Demographics", problem = "LLM call failed",
    action = "fallback", stringsAsFactors = FALSE
  )
  path <- tempfile(fileext = ".xlsx")
  write_validation_report(validation, path, diagnostics = diagnostics)
  expect_true(file.exists(path))
  wb <- openxlsx2::wb_load(path)
  expect_setequal(wb$get_sheet_names(), c("Validation", "Diagnostics"))

  ## Round-trip the Diagnostics sheet content.
  diag_back <- openxlsx2::wb_to_df(wb, sheet = "Diagnostics")
  expect_equal(nrow(diag_back), 1)
  expect_equal(diag_back$severity, "FAIL")
})

test_that("write_validation_report omits Diagnostics sheet when empty", {
  skip_if_not_installed("openxlsx2")
  validation <- data.frame(
    tlf_number = "T-1", stub_label = "x", annotation = "ADSL.AGE",
    variable_ref = "ADSL.AGE", status = "PASS", message = "ok",
    stringsAsFactors = FALSE
  )
  path <- tempfile(fileext = ".xlsx")
  write_validation_report(validation, path, diagnostics = diag_records()[0, ])
  wb <- openxlsx2::wb_load(path)
  expect_equal(unname(wb$get_sheet_names()), "Validation")
})

test_that("shell parser records section-quality diagnostics", {
  diag_reset()
  sections <- parse_shell_docx(
    test_path("fixtures/annotated_shell_2tlf_minimal.docx")
  )
  expect_gt(length(sections), 0)
  ## The minimal fixture is annotated, so no zero-annotation WARN expected
  ## for sections that carry annotations.
  recs <- diag_records()
  annotated <- vapply(sections, function(s) {
    any(vapply(s$stub_rows, function(r) isTRUE(r$has_annot), logical(1)))
  }, logical(1))
  for (i in which(annotated)) {
    expect_false(any(
      recs$stage == "parse_shell" &
        recs$tlf_number == sections[[i]]$tlf_number &
        grepl("no annotations were detected", recs$problem)
    ))
  }
})
