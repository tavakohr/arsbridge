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

## --- Value-vs-spec gate (Fix A) --------------------------------------------

test_that("FAIL when a value literal exceeds the spec's declared length", {
  ## SAFFL is declared length 1 in adam_spec_minimal.xlsx; a real-world
  ## incident bound a 1-char seriousness flag (AESER-like) to a multi-char
  ## severity term -- the existence-only gate called this clean.
  spec <- parse_adam_spec(test_path("fixtures/adam_spec_minimal.xlsx"))
  secs <- list(.mk_section("T-1", list(list(label = "Serious",
                                            annot = "ADSL.SAFFL='Yes'"))))
  rep <- validate_annotations_spec(secs, spec$lookup, spec$codelists)
  hit <- rep[rep$variable_ref == "ADSL.SAFFL" & rep$stub_label == "Serious", ]
  expect_equal(hit$status, "FAIL")
  expect_match(hit$message, "3 characters")
  expect_match(hit$message, "length 1")
})

test_that("WARN when a value literal is not in the variable's declared codelist", {
  ## The spec's `codelist` column is a codelist NAME to resolve (e.g. "NY"),
  ## not inline terms -- build a synthetic spec_codelists list directly
  ## (adam_spec_minimal.xlsx carries no codelist-terms sheet of its own).
  spec <- parse_adam_spec(test_path("fixtures/adam_spec_minimal.xlsx"))
  spec_codelists <- list(NY = list(
    name = "NY",
    terms = data.frame(term = c("N", "Y"), stringsAsFactors = FALSE),
    used_by = character()
  ))

  secs_bad <- list(.mk_section("T-1", list(list(label = "Serious",
                                                 annot = "ADSL.SAFFL='M'"))))
  rep_bad <- validate_annotations_spec(secs_bad, spec$lookup, spec_codelists)
  hit_bad <- rep_bad[rep_bad$variable_ref == "ADSL.SAFFL" & rep_bad$stub_label == "Serious", ]
  expect_equal(hit_bad$status, "WARN")
  expect_match(hit_bad$message, "codelist")

  ## A value that IS in the codelist must not false-positive.
  secs_ok <- list(.mk_section("T-1", list(list(label = "Serious",
                                                annot = "ADSL.SAFFL='Y'"))))
  rep_ok <- validate_annotations_spec(secs_ok, spec$lookup, spec_codelists)
  hit_ok <- rep_ok[rep_ok$variable_ref == "ADSL.SAFFL" & rep_ok$stub_label == "Serious", ]
  expect_equal(hit_ok$status, "PASS")
})

test_that("value check degrades gracefully with no spec_codelists (2-arg call)", {
  spec <- parse_adam_spec(test_path("fixtures/adam_spec_minimal.xlsx"))
  secs <- list(.mk_section("T-1", list(list(label = "Age", annot = "ADSL.AGE=45"))))
  rep  <- validate_annotations_spec(secs, spec$lookup)
  hit  <- rep[rep$variable_ref == "ADSL.AGE" & rep$stub_label == "Age", ]
  expect_equal(hit$status, "PASS")
})
