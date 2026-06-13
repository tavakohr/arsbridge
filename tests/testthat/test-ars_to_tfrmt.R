# Tests for ars_to_tfrmt() / ars_render_tlf() / ars_to_tfrmt_list().
#
# These exercise the formatting layer against a small, deterministic ARD built
# from a bundled ARS JSON + the bundled ADaM.zip. The fixture is created once
# per test session and skipped entirely when the optional rendering packages
# (tfrmt/gt) or the bundled data are unavailable.

skip_if_not_installed("tfrmt")
skip_if_not_installed("gt")

# ---- shared fixture --------------------------------------------------------
# A trimmed ARS spec + ARD covering one categorical output (disposition) so the
# tests stay fast and do not depend on network or LLM calls.

make_fixture <- function() {
  ars_src <- testthat::test_path("fixtures", "tfrmt_reporting_event.json")
  ard_src <- testthat::test_path("fixtures", "tfrmt_ard.rds")
  if (!file.exists(ars_src) || !file.exists(ard_src)) {
    testthat::skip("tfrmt test fixtures not present")
  }
  list(
    ars_path  = ars_src,
    ard       = readRDS(ard_src),
    output_id = "T_14_1_1"
  )
}

test_that("ars_to_tfrmt returns a tfrmt object", {
  fx <- make_fixture()
  result <- ars_to_tfrmt(fx$ars_path, fx$ard, fx$output_id)
  expect_s3_class(result, "tfrmt")
})

test_that("ars_render_tlf returns a gt_tbl", {
  fx <- make_fixture()
  gt_obj <- ars_render_tlf(fx$ars_path, fx$ard, fx$output_id)
  expect_s3_class(gt_obj, "gt_tbl")
})

test_that("ars_render_tlf scales percentages and combines n (p%)", {
  fx <- make_fixture()
  gt_obj <- ars_render_tlf(fx$ars_path, fx$ard, fx$output_id)
  body <- as.data.frame(gt_obj[["_data"]])
  # Percentages must be on a 0-100 scale, not raw proportions.
  expect_true(any(grepl("\\([0-9]+\\.[0-9]\\%\\)", unlist(body))))
  expect_true(any(grepl("100\\.0%", unlist(body))))
})

test_that("ars_to_tfrmt errors on unknown output_id", {
  fx <- make_fixture()
  expect_error(
    ars_to_tfrmt(fx$ars_path, fx$ard, "NONEXISTENT_OUTPUT"),
    regexp = "not found"
  )
})

test_that("ars_to_tfrmt errors when ARD has no rows for output_id", {
  fx <- make_fixture()
  empty_ard <- fx$ard[FALSE, ]
  expect_error(
    ars_to_tfrmt(fx$ars_path, empty_ard, fx$output_id),
    regexp = "zero rows"
  )
})

test_that("ars_to_tfrmt_list returns a named list with tfrmt elements", {
  fx <- make_fixture()
  result <- ars_to_tfrmt_list(fx$ars_path, fx$ard)
  expect_type(result, "list")
  expect_true(length(result) >= 1)
  non_null <- result[!vapply(result, is.null, logical(1))]
  expect_true(all(vapply(non_null, inherits, logical(1), "tfrmt")))
  expect_true(fx$output_id %in% names(result))
})
