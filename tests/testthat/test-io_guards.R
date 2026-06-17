## Tests for the general input-guard / plain-English error contract.

valid_json <- function() {
  p <- tempfile(fileext = ".json"); writeLines("{}", p); p
}

test_that(".require_file rejects NULL / missing without a base-R error", {
  expect_error(.require_file(NULL, "ars_path", INPUT_ARS), "ARS JSON")
  expect_error(.require_file(character(0), "ars_path", INPUT_ARS), "ARS JSON")
  expect_error(.require_file("does_not_exist.json", "ars_path", INPUT_ARS),
               "not found")
})

test_that(".require_dir rejects NULL with a clean, document-named message", {
  msg <- tryCatch(.require_dir(NULL, "adam_dir", INPUT_DATA),
                  error = function(e) conditionMessage(e))
  expect_match(msg, "ADaM dataset")
  expect_false(grepl("invalid filename argument", msg, fixed = TRUE))
})

test_that("ars_to_ard gives a clean, input-named error on NULL adam_dir", {
  msg <- tryCatch(ars_to_ard(valid_json(), NULL),
                  error = function(e) conditionMessage(e))
  expect_match(msg, "ADaM dataset")
  expect_false(grepl("invalid filename argument", msg, fixed = TRUE))
})

test_that("ars_to_ard names the ARS JSON on NULL ars_path", {
  expect_error(ars_to_ard(NULL, tempdir()), "ARS JSON")
})

test_that("a corrupt ARS JSON is reported as unreadable, not cryptically", {
  bad <- tempfile(fileext = ".json"); writeLines("{ not valid", bad)
  msg <- tryCatch(ars_to_ard(bad, tempdir()),
                  error = function(e) conditionMessage(e))
  expect_match(msg, "valid JSON")
})

test_that("render functions guard NULL ard / ars_path", {
  expect_error(ars_render_tlf(valid_json(), NULL, "T_14_1_1"), "No ARD")
  expect_error(ars_render_tlf(NULL, mtcars, "T_14_1_1"), "ARS JSON")
})

test_that("diag carries the input document and a 'To fix:' action", {
  diag_reset()
  .diag_gap(stage = "parse_spec", severity = "FAIL", input = INPUT_SPEC,
            problem = "Something is wrong.", why = "It blocks the ARS.",
            fix = "Edit the spec.")
  d <- ars_diagnostics()
  expect_true("input" %in% names(d))
  expect_identical(d$input[1], INPUT_SPEC)
  expect_match(d$problem[1], "It blocks the ARS")
  expect_match(d$action[1], "To fix: Edit the spec")
})

test_that("ars_blockers returns only FAIL rows with the standard columns", {
  diag_reset()
  diag_add("parse_spec", "WARN", "minor", input = INPUT_SPEC)
  .diag_gap("execute_ard", "FAIL", INPUT_DATA, "blocker", fix = "do x")
  bl <- ars_blockers()
  expect_setequal(names(bl),
                  c("input", "problem", "action", "stage", "tlf_number", "location"))
  expect_equal(nrow(bl), 1L)
  expect_identical(bl$input[1], INPUT_DATA)
})

test_that("ars_blockers is empty when there are no FAIL findings", {
  diag_reset()
  diag_add("parse_spec", "WARN", "just a warning", input = INPUT_SPEC)
  expect_equal(nrow(ars_blockers()), 0L)
})
