## Engine equivalence: the new default path (ars_to_ard sourcing the emitted
## {cards} blocks) must reproduce the retired .ARD_EXECUTORS path
## (legacy = TRUE) cell-for-cell across the common idioms -- continuous,
## count n(%), AE frequency, subject count, and includeTotal -- including a
## cross-dataset population filter. (Disposition / bare-flag intentionally
## differs; that fix is asserted in test-ars_to_code.)

.eq_adam <- function(td) {
  utils::write.csv(data.frame(
    USUBJID = sprintf("S%02d", 1:10),
    TRT01A  = rep(c("Drug A", "Placebo"), each = 5),
    SAFFL   = c(rep("Y", 9), "N"),
    AGE     = c(40, 50, 60, 70, 80, 45, 55, 65, 75, 85),
    SEX     = rep(c("M", "F"), 5),
    AGEGR1  = rep(c("<65", ">=65"), 5),
    stringsAsFactors = FALSE
  ), file.path(td, "adsl.csv"), row.names = FALSE)
  utils::write.csv(data.frame(
    USUBJID = c("S01", "S01", "S02", "S06", "S07"),
    TRT01A  = c("Drug A", "Drug A", "Drug A", "Placebo", "Placebo"),
    AEDECOD = c("Headache", "Nausea", "Headache", "Headache", "Rash"),
    TRTEMFL = c("Y", "Y", "Y", "Y", "N"),
    stringsAsFactors = FALSE
  ), file.path(td, "adae.csv"), row.names = FALSE)
}

.eq_spec <- function() {
  grp <- list(list(order = 1, groupingId = "GF_TRT", resultsByGroup = TRUE))
  list(
    analysisSets = list(list(id = "AS_SAF", name = "Safety",
      condition = list(dataset = "ADSL", variable = "SAFFL",
                       comparator = "EQ", value = list("Y")))),
    dataSubsets = list(list(id = "DS_TEAE", name = "TEAE",
      condition = list(dataset = "ADAE", variable = "TRTEMFL",
                       comparator = "EQ", value = list("Y")))),
    analysisGroupings = list(list(id = "GF_TRT", name = "TRT01A",
                                  groupingVariable = "TRT01A")),
    methods = list(),
    outputs = list(
      list(id = "OUT_DM", name = "T-DM",
           referencedAnalysisIds = list("AN_AGE", "AN_AGEGR", "AN_N")),
      list(id = "OUT_AE", name = "T-AE",
           referencedAnalysisIds = list("AN_AE"))),
    analyses = list(
      list(id = "AN_AGE", methodId = "MTH_SUMMARY_STATISTICS_CONTINUOUS",
           label = "Age", dataset = "ADSL", variable = "AGE",
           analysisVariable = list(dataset = "ADSL", variable = "AGE"),
           analysisSetId = "AS_SAF", dataSubsetId = "",
           orderedGroupings = grp, includeTotal = TRUE),
      list(id = "AN_AGEGR", methodId = "MTH_COUNT_AND_PERCENTAGE",
           label = "Age group", dataset = "ADSL", variable = "AGEGR1",
           analysisVariable = list(dataset = "ADSL", variable = "AGEGR1"),
           analysisSetId = "AS_SAF", dataSubsetId = "",
           orderedGroupings = grp, includeTotal = TRUE),
      list(id = "AN_N", methodId = "MTH_SUBJECT_COUNT",
           label = "N", dataset = "ADSL", variable = "USUBJID",
           analysisVariable = list(dataset = "ADSL", variable = "USUBJID"),
           analysisSetId = "AS_SAF", dataSubsetId = "",
           orderedGroupings = grp, includeTotal = FALSE),
      list(id = "AN_AE", methodId = "MTH_AE_FREQUENCY_COUNT",
           label = "AE term", dataset = "ADAE", variable = "AEDECOD",
           analysisVariable = list(dataset = "ADAE", variable = "AEDECOD"),
           analysisSetId = "AS_SAF", dataSubsetId = "DS_TEAE",
           orderedGroupings = grp, includeTotal = FALSE))
  )
}

## Stable, comparable projection of an ARD (flatten list-cols; keep value rows).
.eq_norm <- function(a) {
  a <- as.data.frame(a)
  keep <- intersect(c("analysis_id", "group1", "group1_level", "variable",
                      "variable_level", "stat_name", "stat"), names(a))
  a <- a[, keep, drop = FALSE]
  for (cn in setdiff(names(a), "stat")) {
    if (is.list(a[[cn]])) a[[cn]] <- vapply(a[[cn]], function(x)
      if (length(x)) as.character(x[[1]]) else NA_character_, character(1))
  }
  if (is.list(a$stat)) a$stat <- vapply(a$stat, function(x)
    if (length(x)) suppressWarnings(as.numeric(x[[1]])) else NA_real_, numeric(1))
  a <- a[a$stat_name %in% c("n", "p", "N", "mean", "sd", "median",
                            "min", "max", "p25", "p75"), , drop = FALSE]
  a <- a[do.call(order, lapply(a, as.character)), ]
  rownames(a) <- NULL
  a
}

test_that("emitted-block engine equals the legacy executor path", {
  skip_if_not_installed("cards")
  td <- withr::local_tempdir()
  .eq_adam(td)
  ars <- file.path(td, "ars.json")
  writeLines(jsonlite::toJSON(.eq_spec(), auto_unbox = TRUE, null = "null"), ars)

  ard_new <- ars_to_ard(ars, td)
  ard_leg <- ars_to_ard(ars, td, legacy = TRUE)

  expect_false(is.null(ard_new))
  expect_false(is.null(ard_leg))
  expect_equal(.eq_norm(ard_new), .eq_norm(ard_leg))
})
