test_that("parse_adam_spec returns variables and lookup", {
  spec <- parse_adam_spec(test_path("fixtures/adam_spec_minimal.xlsx"))
  expect_named(spec, c("variables", "lookup"))
  expect_true(is.data.frame(spec$variables))
  expect_equal(nrow(spec$variables), 10)
  expect_setequal(unique(spec$variables$dataset), c("ADSL", "ADAE"))
})

test_that("lookup is keyed by DATASET.VARIABLE and resolves known vars", {
  spec <- parse_adam_spec(test_path("fixtures/adam_spec_minimal.xlsx"))
  expect_true("ADSL.AGE"     %in% names(spec$lookup))
  expect_true("ADAE.TRTEMFL" %in% names(spec$lookup))
  expect_equal(spec$lookup$ADSL.AGE$label, "Age")
  expect_equal(spec$lookup$ADAE.AEDECOD$label, "Dictionary-Derived Term")
})

test_that("parse_adam_spec aborts on missing file", {
  expect_error(parse_adam_spec("nonexistent.xlsx"), "not found")
})

test_that("parse_adam_spec dispatches to the define.xml branch on .xml input", {
  spec <- parse_adam_spec(test_path("fixtures/adam_define_minimal.xml"))
  expect_named(spec, c("variables", "lookup"))
  expect_true(is.data.frame(spec$variables))
  expect_setequal(unique(spec$variables$dataset), c("ADSL", "ADAE"))
  expect_true("ADSL.AGE"     %in% names(spec$lookup))
  expect_true("ADSL.SAFFL"   %in% names(spec$lookup))
  expect_true("ADAE.TRTEMFL" %in% names(spec$lookup))
  expect_equal(spec$lookup$ADSL.AGE$label, "Age")
})

test_that("parse_adam_spec rejects unsupported extensions (e.g. .csv)", {
  bad <- tempfile(fileext = ".csv")
  writeLines("dataset,variable\nADSL,AGE", bad)
  expect_error(parse_adam_spec(bad), "Unsupported")
})
