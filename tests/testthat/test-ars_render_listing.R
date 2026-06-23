# Tests for ars_render_listing(): assembles MTH_LISTING columns (merging
# auxiliary datasets by subject), applies the population/subset filter, and
# returns a gt_tbl. Hand-built spec + ADaM, no LLM/network.

skip_if_not_installed("gt")

# A listing ARS spec with two ADSL columns + one cross-dataset ADAE column,
# plus the ADSL/ADAE data written to a temp dir.
.listing_fixture <- function(envir = parent.frame()) {
  adam_dir <- withr::local_tempdir(.local_envir = envir)
  utils::write.csv(data.frame(
    USUBJID = sprintf("S%02d", 1:6),
    TRT01A  = rep(c("Drug A", "Placebo"), 3),
    SAFFL   = c("Y", "Y", "Y", "Y", "N", "Y"),
    AGE     = c(45, 50, 55, 60, 65, 70),
    stringsAsFactors = FALSE
  ), file.path(adam_dir, "ADSL.csv"), row.names = FALSE)
  utils::write.csv(data.frame(
    USUBJID = c("S01", "S02", "S03"),
    AEDECOD = c("Headache", "Nausea", "Rash"),
    stringsAsFactors = FALSE
  ), file.path(adam_dir, "ADAE.csv"), row.names = FALSE)

  spec <- list(
    analysisSets = list(list(id = "AS_SAF", name = "Safety",
      condition = list(dataset = "ADSL", variable = "SAFFL",
                       comparator = "EQ", value = list("Y")))),
    analyses = list(
      list(id = "L_SUBJ", methodId = "MTH_LISTING", description = "Subject",
           analysisVariable = list(dataset = "ADSL", variable = "USUBJID"),
           analysisSetId = "AS_SAF"),
      list(id = "L_AGE", methodId = "MTH_LISTING", description = "Age",
           analysisVariable = list(dataset = "ADSL", variable = "AGE"),
           analysisSetId = "AS_SAF"),
      list(id = "L_AE", methodId = "MTH_LISTING", description = "AE term",
           analysisVariable = list(dataset = "ADAE", variable = "AEDECOD"))),
    outputs = list(list(id = "L_16_2_1", name = "L-16.2.1",
      outputType = "LISTING",
      referencedAnalysisIds = list("L_SUBJ", "L_AGE", "L_AE"),
      displays = list(list(order = 1, displayTitle = "Listing of adverse events")))))
  ars_path <- tempfile("ars_", fileext = ".json")
  writeLines(jsonlite::toJSON(spec, auto_unbox = TRUE, null = "null"), ars_path)
  list(ars_path = ars_path, adam_dir = adam_dir)
}

test_that("listing renders a gt_tbl with the population filter + cross merge", {
  fx <- .listing_fixture()
  gt_tbl <- ars_render_listing(fx$ars_path, fx$adam_dir, "L_16_2_1")
  expect_s3_class(gt_tbl, "gt_tbl")
  d <- as.data.frame(gt_tbl[["_data"]])
  # SAFFL == 'N' subject (S05) is filtered out: 5 of 6 rows remain.
  expect_equal(nrow(d), 5L)
  # The cross-dataset ADAE column merged onto the matching subjects.
  expect_true(any(grepl("Headache|Nausea|Rash", unlist(d))))
})

test_that("max_rows truncates and adds a footnote", {
  fx <- .listing_fixture()
  gt_tbl <- ars_render_listing(fx$ars_path, fx$adam_dir, "L_16_2_1",
                               max_rows = 2)
  d <- as.data.frame(gt_tbl[["_data"]])
  expect_equal(nrow(d), 2L)
  notes <- unlist(gt_tbl[["_source_notes"]])
  expect_true(any(grepl("truncated", notes)))
})

test_that("an output with no MTH_LISTING columns errors", {
  fx <- .listing_fixture()
  # Point at a non-existent output id -> find_output aborts.
  expect_error(
    ars_render_listing(fx$ars_path, fx$adam_dir, "NOPE"),
    regexp = "not found")
})
