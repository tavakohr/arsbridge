## Direct exercise of split_label_annotation (the Layer 3 plain-text path)
## and the false-positive guards. This file is the single source of truth
## for "no ADaM-style match => no annotation" behaviour.

test_that("Layer 3 detects ADSL.AGE in plain text with no colour", {
  result <- split_label_annotation("Age at Informed Consent (years)  ADSL.AGE")
  expect_equal(result$label,      "Age at Informed Consent (years)")
  expect_equal(result$annotation, "ADSL.AGE")
})

test_that("Layer 3 detects flag-condition annotation in plain text", {
  result <- split_label_annotation("Screen Failure  ADSL.SCRFFL='Y'")
  expect_equal(result$label,      "Screen Failure")
  expect_equal(result$annotation, "ADSL.SCRFFL='Y'")
})

test_that("Layer 3 detects bracket-enclosed annotation", {
  result <- split_label_annotation("Age (years) [ADSL.AGE]")
  expect_equal(result$annotation, "ADSL.AGE")
  expect_equal(result$label,      "Age (years)")
})

test_that("Layer 3 does NOT falsely detect plain English as annotation", {
  result <- split_label_annotation("Number of subjects enrolled")
  expect_equal(result$annotation, "")
  expect_equal(result$label,      "Number of subjects enrolled")
})

test_that("statistical sub-rows are never flagged as annotations", {
  for (lbl in c("n", "Mean (SD)", "Median", "Q1, Q3", "Min, Max", "xxx", "[a]", "N=125")) {
    result <- split_label_annotation(lbl)
    expect_equal(result$annotation, "", info = paste("Failed for:", lbl))
  }
})

test_that("count-expression in plain text is matched", {
  result <- split_label_annotation(
    "Concomitant Medications  unique USUBJID in ADCM where ADCM.CONTRTFL='Y'"
  )
  expect_equal(result$label, "Concomitant Medications")
  expect_match(result$annotation, "unique USUBJID in ADCM")
})

test_that("NULL / empty input returns empty label and annotation", {
  expect_equal(split_label_annotation(""),   list(label = "", annotation = ""))
  expect_equal(split_label_annotation(NULL), list(label = "", annotation = ""))
})
