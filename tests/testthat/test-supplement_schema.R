## The shipped JSON Schema for supplement format v4, and the golden example
## fixture. jsonvalidate is a Suggests-only second opinion; the pure-R
## ars_validate_supplement() is the in-package authority (see test-supplement).

test_that("the v4 JSON Schema ships with the package and is valid JSON", {
  p <- system.file("schema", "arsbridge_supplement_v4.schema.json",
                   package = "arsbridge")
  expect_true(nzchar(p) && file.exists(p))
  schema <- jsonlite::fromJSON(p, simplifyVector = FALSE)
  expect_equal(schema$`$schema`, "http://json-schema.org/draft-07/schema#")
  expect_equal(schema$properties$supplement_version$const, 4L)
})

test_that("the golden v4 example fixture is valid JSON with two TLFs", {
  fx <- jsonlite::fromJSON(test_path("fixtures/supplement_v4_example.json"),
                           simplifyVector = FALSE)
  expect_equal(fx$supplement_version, 4L)
  expect_setequal(names(fx$tlfs), c("14.1.1", "14.3.1"))
})

test_that("the golden fixture passes ars_validate_supplement with no FAIL", {
  out <- suppressMessages(ars_validate_supplement(
    test_path("fixtures/supplement_v4_example.json"),
    adam_spec_path = test_path("fixtures/adam_spec_minimal.xlsx")))
  expect_equal(sum(out$severity == "FAIL"), 0)
})

test_that("the golden fixture validates against the shipped schema (jsonvalidate)", {
  skip_if_not_installed("jsonvalidate")
  schema_path <- system.file("schema", "arsbridge_supplement_v4.schema.json",
                             package = "arsbridge")
  skip_if_not(nzchar(schema_path) && file.exists(schema_path))
  validate <- jsonvalidate::json_validator(schema_path, engine = "ajv")
  expect_true(validate(test_path("fixtures/supplement_v4_example.json")))
})

test_that("the schema rejects an unknown top-level field (jsonvalidate)", {
  skip_if_not_installed("jsonvalidate")
  schema_path <- system.file("schema", "arsbridge_supplement_v4.schema.json",
                             package = "arsbridge")
  skip_if_not(nzchar(schema_path) && file.exists(schema_path))
  validate <- jsonvalidate::json_validator(schema_path, engine = "ajv")
  bad <- '{"supplement_version": 4, "tlfs": {}, "surprise": true}'
  expect_false(validate(bad))
})
