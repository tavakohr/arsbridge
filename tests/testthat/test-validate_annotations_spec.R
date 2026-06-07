.mk_section <- function(tlf, annot_rows, pop = "ADSL.SAFFL='Y'") {
  list(
    tlf_number       = tlf,
    population_annot = pop,
    stub_rows        = lapply(annot_rows, function(a) list(
      label = a$label, annotation = a$annot, has_annot = nzchar(a$annot),
      detection_method = "pattern", detection_confidence = "high"
    ))
  )
}

test_that("PASS status for variables present in spec", {
  spec <- parse_adam_spec(test_path("fixtures/adam_spec_minimal.xlsx"))
  secs <- list(.mk_section("T-1", list(list(label = "Age", annot = "ADSL.AGE"))))
  rep  <- validate_annotations_spec(secs, spec$lookup)
  expect_true(any(rep$variable_ref == "ADSL.AGE" & rep$status == "PASS"))
})

test_that("WARN status when dataset exists but variable doesn't", {
  spec <- parse_adam_spec(test_path("fixtures/adam_spec_minimal.xlsx"))
  secs <- list(.mk_section("T-1", list(list(label = "Bogus",
                                            annot = "ADSL.BOGUSXX"))))
  rep  <- validate_annotations_spec(secs, spec$lookup)
  expect_true(any(rep$variable_ref == "ADSL.BOGUSXX" & rep$status == "WARN"))
})

test_that("FAIL status when dataset is unknown", {
  spec <- parse_adam_spec(test_path("fixtures/adam_spec_minimal.xlsx"))
  secs <- list(.mk_section("T-1", list(list(label = "Bogus",
                                            annot = "ADXX.AGE"))))
  rep  <- validate_annotations_spec(secs, spec$lookup)
  expect_true(any(rep$variable_ref == "ADXX.AGE" & rep$status == "FAIL"))
})

test_that("population annotation is included as a row", {
  spec <- parse_adam_spec(test_path("fixtures/adam_spec_minimal.xlsx"))
  secs <- list(.mk_section("T-1", list(list(label = "Age", annot = "ADSL.AGE"))))
  rep  <- validate_annotations_spec(secs, spec$lookup)
  expect_true(any(rep$stub_label == "<population>"))
})
