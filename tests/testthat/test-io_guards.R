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

# ---- error branches of the readers / writer (coverage) ---------------------

test_that(".require_dir reports a missing directory cleanly", {
  expect_error(.require_dir(tempfile("no_such_dir_"), "adam_dir", INPUT_DATA),
               regexp = "not found")
})

test_that(".read_dataset returns NULL and records a FAIL on an unreadable file", {
  diag_reset()
  bad <- tempfile(fileext = ".xpt")
  writeLines("this is not a SAS transport file", bad)
  out <- .read_dataset(bad, "ADSL")
  expect_null(out)
  expect_true(any(diag_records()$severity == "FAIL"))
})

test_that(".read_dataset and .listing_load read a .sas7bdat cut", {
  skip_if_not_installed("haven")
  td <- withr::local_tempdir()
  df <- data.frame(USUBJID = c("01", "02"), AGE = c(40, 50),
                   stringsAsFactors = FALSE)
  ## write_sas() only builds the fixture; deprecated in haven 2.5.2, so silence
  ## it and skip if a future haven drops it -- read_sas is the path under test.
  built <- tryCatch({
    suppressWarnings(haven::write_sas(df, file.path(td, "adsl.sas7bdat")))
    TRUE
  }, error = function(e) FALSE)
  skip_if_not(built, "haven::write_sas() unavailable to build the sas7bdat fixture")

  ## The engine's SOFT reader reads it by full path.
  eng <- .read_dataset(file.path(td, "adsl.sas7bdat"), "ADSL")
  expect_s3_class(eng, "data.frame")
  expect_equal(nrow(eng), 2)

  ## The renderer loader finds it case-insensitively by dataset name.
  lst <- arsbridge:::.listing_load(td, "ADSL")
  expect_s3_class(lst, "data.frame")
  expect_true(all(c("USUBJID", "AGE") %in% names(lst)))
})

test_that(".read_docx aborts on a file that is not a real .docx", {
  bad <- tempfile(fileext = ".docx")
  writeLines("not a docx", bad)
  expect_error(.read_docx(bad), regexp = "could not be opened")
})

test_that(".read_docx muffles the DocuSign core.xml namespace warning", {
  ## The RWE fixture ships a docProps/core.xml with an off-standard
  ## namespace, the way some e-signature tools write it. officer's reader
  ## warns "Undefined namespace prefix"; arsbridge muffles just that one.
  expect_no_warning(
    .read_docx(test_path("fixtures/annotated_shell_rwe_style.docx")))
})

test_that(".read_lines aborts on a missing file", {
  expect_error(.read_lines(tempfile("missing_"), "prompt template"),
               regexp = "Could not read")
})

test_that(".write_text aborts when the target folder does not exist", {
  target <- file.path(tempfile("no_dir_"), "out.json")
  expect_error(.write_text("x", target, "ARS JSON"), regexp = "Could not write")
})

test_that(".doc_label appends the basename when a path is given", {
  expect_equal(.doc_label("ARS JSON", "/a/b/reporting_event.json"),
               "ARS JSON 'reporting_event.json'")
  expect_equal(.doc_label("ARS JSON", NULL), "ARS JSON")
})
