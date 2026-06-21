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

# ---- Phase 4: partial rendering with [‡ manual] markers --------------------
# A mixed output: one supported analysis (count n/%) and one declared-but-
# unexecutable analysis (CMH p-value) over the same response variable, sharing
# one output. ars_to_ard() computes the counts and reserves a manual_pending
# stub cell for the CMH; the renderer fills the counts and marks the CMH cell.

.mixed_fixture <- function() {
  adam_dir <- withr::local_tempdir(.local_envir = parent.frame())
  utils::write.csv(data.frame(
    USUBJID = sprintf("S%02d", 1:8),
    TRT01A  = rep(c("Drug A", "Placebo"), each = 4),
    SAFFL   = rep("Y", 8),
    RESP    = c("Y", "Y", "N", "N", "Y", "N", "N", "N"),
    stringsAsFactors = FALSE
  ), file.path(adam_dir, "ADSL.csv"), row.names = FALSE)

  grp <- list(list(order = 1, groupingId = "GF_TRT", resultsByGroup = TRUE))
  spec <- list(
    id = "MIX", name = "Mixed", version = "1",
    analysisSets = list(list(id = "AS_ITT", name = "ITT",
      condition = list(dataset = "ADSL", variable = "SAFFL",
                       comparator = "EQ", value = list("Y")))),
    analysisGroupings = list(list(id = "GF_TRT", name = "TRT01A",
      groupingVariable = list(dataset = "ADSL", variable = "TRT01A"))),
    methods = list(list(id = "MTH_COUNT_AND_PERCENTAGE", name = "Count"),
                   list(id = "MTH_CMH_TEST", name = "CMH", supported = FALSE)),
    analyses = list(
      list(id = "AN_CNT", name = "Responders", analysisSetId = "AS_ITT",
           methodId = "MTH_COUNT_AND_PERCENTAGE",
           analysisVariable = list(dataset = "ADSL", variable = "RESP"),
           orderedGroupings = grp),
      list(id = "AN_CMH", name = "CMH p-value", analysisSetId = "AS_ITT",
           methodId = "MTH_CMH_TEST",
           analysisVariable = list(dataset = "ADSL", variable = "RESP"),
           orderedGroupings = grp)),
    outputs = list(list(id = "T_X", name = "T-X",
      referencedAnalysisIds = list("AN_CNT", "AN_CMH")))
  )
  ars_path <- tempfile("ars_", fileext = ".json")
  writeLines(jsonlite::toJSON(spec, auto_unbox = TRUE, null = "null"), ars_path)
  ard <- ars_to_ard(ars_path, adam_dir)
  list(ars_path = ars_path, ard = ard)
}

test_that("a mixed ARD reserves manual_pending cells alongside computed counts", {
  skip_if_not_installed("cards")
  fx <- .mixed_fixture()
  expect_true(any(fx$ard$result_status == "computed"))
  expect_true(any(fx$ard$result_status == "manual_pending"))
  expect_setequal(
    unique(fx$ard$method_id[fx$ard$result_status == "manual_pending"]),
    "MTH_CMH_TEST")
})

test_that("ars_render_tlf renders the [‡ manual] marker and its footnote", {
  skip_if_not_installed("cards")
  marker <- paste0("[", intToUtf8(0x2021), " manual]")
  fx <- .mixed_fixture()
  gt_obj <- ars_render_tlf(fx$ars_path, fx$ard, "T_X")
  expect_s3_class(gt_obj, "gt_tbl")
  body <- as.data.frame(gt_obj[["_data"]])
  # Reserved CI cells show the loud marker, not blank / NA / a number.
  expect_true(any(grepl(marker, unlist(body), fixed = TRUE)))
  # Computed counts are still present as n (p%).
  expect_true(any(grepl("\\([0-9]+\\.[0-9]\\%\\)", unlist(body))))
  # The marker is keyed to a source-note footnote.
  notes <- unlist(gt_obj[["_source_notes"]])
  expect_true(any(grepl("manual derivation", notes)))
})

test_that("a filled manual cell renders its value, not the marker (phase 5)", {
  skip_if_not_installed("cards")
  marker <- paste0("[", intToUtf8(0x2021), " manual]")
  fx <- .mixed_fixture()
  ard <- fx$ard
  # Fill the reserved CMH cell with a validated value + derivation_ref.
  pend <- which(ard$result_status == "manual_pending")
  expect_gt(length(pend), 0)
  one <- pend[1]
  ard$stat[[one]]          <- 0.123
  ard$result_status[one]   <- "manual_filled"
  ard$value_source[one]    <- "manual"
  ard$derivation_ref[one]  <- "cmh_t_x.R"

  expect_equal(nrow(ars_validate_manual_fills(ard)), 0L)

  gt_obj <- ars_render_tlf(fx$ars_path, ard, "T_X")
  flat <- unlist(as.data.frame(gt_obj[["_data"]]))
  # The filled value is rendered (3 dp), not the marker.
  expect_true(any(grepl("0.123", flat, fixed = TRUE)))
  expect_false(any(grepl(marker, flat, fixed = TRUE)))
})

# ---- cardx descriptors: Clopper-Pearson CI computes (ADR 0001) -------------
# With {cardx} installed the exact-CI method is no longer reserved -- it is
# emitted as a cardx::ard_categorical_ci() call and computed like any other
# cell. Without {cardx} it degrades to a manual_pending stub.

.ci_fixture <- function() {
  adam_dir <- withr::local_tempdir(.local_envir = parent.frame())
  utils::write.csv(data.frame(
    USUBJID = sprintf("S%02d", 1:8),
    TRT01A  = rep(c("Drug A", "Placebo"), each = 4),
    SAFFL   = rep("Y", 8),
    RESP    = c("Y", "Y", "N", "N", "Y", "N", "N", "N"),
    stringsAsFactors = FALSE
  ), file.path(adam_dir, "ADSL.csv"), row.names = FALSE)
  grp <- list(list(order = 1, groupingId = "GF_TRT", resultsByGroup = TRUE))
  spec <- list(
    id = "CI", name = "CI", version = "1",
    analysisSets = list(list(id = "AS_ITT", name = "ITT",
      condition = list(dataset = "ADSL", variable = "SAFFL",
                       comparator = "EQ", value = list("Y")))),
    analysisGroupings = list(list(id = "GF_TRT", name = "TRT01A",
      groupingVariable = list(dataset = "ADSL", variable = "TRT01A"))),
    methods = list(list(id = "MTH_PROPORTION_CI_EXACT", name = "Exact CI",
                        supported = FALSE)),
    analyses = list(list(id = "AN_CI", name = "Exact CI", analysisSetId = "AS_ITT",
      methodId = "MTH_PROPORTION_CI_EXACT",
      analysisVariable = list(dataset = "ADSL", variable = "RESP"),
      orderedGroupings = grp)),
    outputs = list(list(id = "T_CI", name = "T-CI",
      referencedAnalysisIds = list("AN_CI")))
  )
  ars_path <- tempfile("ars_", fileext = ".json")
  writeLines(jsonlite::toJSON(spec, auto_unbox = TRUE, null = "null"), ars_path)
  list(ars_path = ars_path, ard = ars_to_ard(ars_path, adam_dir))
}

test_that("Clopper-Pearson CI is computed via cardx, not reserved", {
  skip_if_not_installed("cards")
  skip_if_not(cardx_ci_works(), "cardx cannot compute a CI in this environment")
  fx <- .ci_fixture()
  ci <- fx$ard[fx$ard$method_id == "MTH_PROPORTION_CI_EXACT", , drop = FALSE]
  expect_gt(nrow(ci), 0)
  # Computed, sourced to cardx, no manual_pending cell left.
  expect_true(all(ci$result_status == "computed"))
  expect_true(all(ci$value_source == "cardx"))
  expect_false(any(fx$ard$result_status == "manual_pending"))
  # Real CI bounds present in [0, 1].
  bounds <- ci[ci$stat_name %in% c("conf.low", "conf.high"), ]
  vals <- vapply(bounds$stat, function(x) as.numeric(x[[1]]), numeric(1))
  expect_true(all(vals >= 0 & vals <= 1))
})
