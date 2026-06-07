test_that("simple DATASET.VARIABLE is extracted", {
  expect_equal(extract_annotation_vars("ADSL.AGE"), "ADSL.AGE")
})

test_that("flag-condition annotation strips the value, returns var ref only", {
  expect_equal(extract_annotation_vars("ADSL.SAFFL='Y'"), "ADSL.SAFFL")
})

test_that("compound OR returns both variables", {
  result <- extract_annotation_vars("ADSL.SFENRLFL='Y' or ADSL.WTHTYP='Withdrawal'")
  expect_true("ADSL.SFENRLFL" %in% result)
  expect_true("ADSL.WTHTYP"   %in% result)
})

test_that("count expression returns USUBJID + referenced flag variable", {
  result <- extract_annotation_vars("unique USUBJID in ADCM where ADCM.CONTRTFL='Y'")
  expect_true("ADCM.USUBJID"  %in% result)
  expect_true("ADCM.CONTRTFL" %in% result)
})

test_that("empty / NULL / plain English returns empty character vector", {
  expect_equal(extract_annotation_vars(""), character())
  expect_equal(extract_annotation_vars(NULL), character())
  expect_equal(extract_annotation_vars("Plain English with no ADaM"), character())
})

test_that("ARS comparator form ('EQ', 'NE') also extracts the variable", {
  expect_equal(extract_annotation_vars("ADTTE.PARAMCD EQ 'OS'"), "ADTTE.PARAMCD")
})
