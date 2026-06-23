# Tests for ars_render_all(): the Word driver that renders every ARS output
# (tables from the ARD, listings + figures from ADaM) into one .docx and returns
# a manifest. Exercises the rendered / placeholder / partial branches and the
# listing + figure dispatch. Hand-built spec + ADaM, no LLM/network.

skip_if_not_installed("cards")
skip_if_not_installed("gt")
skip_if_not_installed("flextable")
skip_if_not_installed("officer")

# One reporting event covering all four output kinds + the gated branches:
#   T_14_1_1  supported count table          -> rendered
#   T_14_2_1  gated, NO computable cells      -> placeholder (gate = TRUE)
#   T_14_3_1  gated, WITH a computed cell     -> partial render
#   L_16_2_1  listing                         -> rendered
#   F_14_2_1  figure                          -> rendered
.render_all_fixture <- function(envir = parent.frame()) {
  adam_dir <- withr::local_tempdir(.local_envir = envir)
  utils::write.csv(data.frame(
    USUBJID = sprintf("S%02d", 1:8),
    TRT01A  = rep(c("Drug A", "Placebo"), 4),
    SAFFL   = "Y",
    SEX     = rep(c("M", "F"), 4),
    AGEGR1  = rep(c("<65", ">=65"), each = 4),
    AGE     = c(45, 50, 55, 60, 65, 70, 75, 80),
    stringsAsFactors = FALSE
  ), file.path(adam_dir, "ADSL.csv"), row.names = FALSE)
  utils::write.csv(data.frame(
    USUBJID = sprintf("S%02d", 1:8),
    TRT01A  = rep(c("Drug A", "Placebo"), 4),
    AVISITN = rep(c(0, 8), 4),
    AVISIT  = rep(c("Baseline", "Week 8"), 4),
    PARAMCD = "EASI75",
    AVAL    = c(0, 1, 0, 1, 1, 0, 1, 1),
    stringsAsFactors = FALSE
  ), file.path(adam_dir, "ADEFF.csv"), row.names = FALSE)

  grp <- list(list(order = 1, groupingId = "GF_TRT", resultsByGroup = TRUE))
  spec <- list(
    id = "S", name = "S", version = "1",
    analysisSets = list(list(id = "AS_SAF", name = "Safety",
      condition = list(dataset = "ADSL", variable = "SAFFL",
                       comparator = "EQ", value = list("Y")))),
    analysisGroupings = list(list(id = "GF_TRT", name = "TRT01A",
      groupingVariable = list(dataset = "ADSL", variable = "TRT01A"))),
    methods = list(list(id = "MTH_COUNT_AND_PERCENTAGE", name = "Count"),
                   list(id = "MTH_LISTING", name = "Listing")),
    analyses = list(
      list(id = "AN_SEX", methodId = "MTH_COUNT_AND_PERCENTAGE",
           analysisSetId = "AS_SAF",
           analysisVariable = list(dataset = "ADSL", variable = "SEX"),
           orderedGroupings = grp),
      list(id = "AN_AGEGR", methodId = "MTH_COUNT_AND_PERCENTAGE",
           analysisSetId = "AS_SAF",
           analysisVariable = list(dataset = "ADSL", variable = "AGEGR1"),
           orderedGroupings = grp),
      list(id = "L_SUBJ", methodId = "MTH_LISTING", description = "Subject",
           analysisVariable = list(dataset = "ADSL", variable = "USUBJID"),
           analysisSetId = "AS_SAF"),
      list(id = "L_AGE", methodId = "MTH_LISTING", description = "Age",
           analysisVariable = list(dataset = "ADSL", variable = "AGE"),
           analysisSetId = "AS_SAF")),
    outputs = list(
      list(id = "T_14_1_1", name = "T-14.1.1", outputType = "TABLE",
           referencedAnalysisIds = list("AN_SEX"),
           displays = list(list(order = 1, displayTitle = "Demographics"))),
      list(id = "T_14_2_1", name = "T-14.2.1", outputType = "TABLE",
           referencedAnalysisIds = list(),
           displays = list(list(order = 1, displayTitle = "EASI 75 (gated)"))),
      list(id = "T_14_3_1", name = "T-14.3.1", outputType = "TABLE",
           referencedAnalysisIds = list("AN_AGEGR"),
           displays = list(list(order = 1, displayTitle = "Partial table"))),
      list(id = "L_16_2_1", name = "L-16.2.1", outputType = "LISTING",
           referencedAnalysisIds = list("L_SUBJ", "L_AGE"),
           displays = list(list(order = 1, displayTitle = "AE listing"))),
      list(id = "F_14_2_1", name = "F-14.2.1", outputType = "FIGURE",
           referencedAnalysisIds = list(),
           displays = list(list(order = 1, displayTitle = "Mean EASI over time")))),
    `_meta` = list(unsupported_outputs = list(
      list(id = "T_14_2_1", reason = "requires Cochran-Mantel-Haenszel test"),
      list(id = "T_14_3_1", reason = "requires Newcombe difference"))))
  ars_path <- tempfile("ars_", fileext = ".json")
  writeLines(jsonlite::toJSON(spec, auto_unbox = TRUE, null = "null"), ars_path)
  list(ars_path = ars_path, adam_dir = adam_dir)
}

test_that("ars_render_all writes a docx and a manifest across all output kinds", {
  skip_if_not_installed("ggplot2")
  fx  <- .render_all_fixture()
  ard <- ars_to_ard(fx$ars_path, fx$adam_dir)
  file <- tempfile(fileext = ".docx")

  man <- ars_render_all(fx$ars_path, ard, adam_dir = fx$adam_dir, file = file)

  # The document was written and is non-empty.
  expect_true(file.exists(file))
  expect_gt(file.info(file)$size, 0)

  # Manifest shape + per-output disposition.
  expect_true(all(c("output_id", "type", "status", "reason") %in% names(man)))
  st <- stats::setNames(man$status, man$output_id)
  expect_equal(unname(st["T_14_1_1"]), "rendered")           # supported table
  expect_equal(unname(st["T_14_2_1"]), "placeholder")        # no computable cell
  expect_equal(unname(st["T_14_3_1"]), "rendered")           # partial (has a cell)
  # Partial render is flagged in the manifest reason.
  expect_match(man$reason[man$output_id == "T_14_3_1"], "partial")
})

test_that("type filter restricts what ars_render_all renders", {
  fx  <- .render_all_fixture()
  ard <- ars_to_ard(fx$ars_path, fx$adam_dir)
  file <- tempfile(fileext = ".docx")
  man <- ars_render_all(fx$ars_path, ard, adam_dir = fx$adam_dir, file = file,
                        types = "table")
  expect_true(file.exists(file))
  # Listings/figures are skipped (status "skipped", reason "type not requested").
  expect_true(any(man$status == "skipped"))
  expect_false(any(man$type %in% c("listing", "figure") & man$status == "rendered"))
})

test_that("ars_render_all aborts when no ARD is supplied", {
  fx <- .render_all_fixture()
  expect_error(
    ars_render_all(fx$ars_path, ard = NULL, adam_dir = fx$adam_dir),
    regexp = "No ARD")
})
