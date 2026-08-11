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
    ## Subject-level only, so a Total column scoped on it is the cross-dataset
    ## case: an ADAE analysis cannot answer it from its own rows.
    COHORTN = c(1, 2, 1, 2, 1, 1, 2, 1, 2, 1),
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
    analysisGroupings = list(list(
      id = "GF_TRT",
      name = "TRT01A",
      groupingVariable = "TRT01A",
      groupingDataset = "ADSL",
      dataDriven = TRUE,
      groups = list()
    )),
    methods = list(
      list(id = "MTH_SUMMARY_STATISTICS_CONTINUOUS",
           name = "Summary Statistics - Continuous"),
      list(id = "MTH_COUNT_AND_PERCENTAGE",
           name = "Count and Percentage"),
      list(id = "MTH_SUBJECT_COUNT", name = "Subject Count"),
      list(id = "MTH_SUBJECT_COUNT_PCT",
           name = "Subject Count and Percentage"),
      list(id = "MTH_AE_FREQUENCY_COUNT", name = "AE Frequency Count")
    ),
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

test_that("subject-key percentages use each treatment arm denominator", {
  skip_if_not_installed("cards")
  td <- withr::local_tempdir()
  .eq_adam(td)

  spec <- .eq_spec()
  spec$outputs <- list(list(
    id = "OUT_N", name = "T-N",
    referencedAnalysisIds = list("AN_N")
  ))
  spec$analyses <- Filter(
    function(analysis) identical(analysis$id, "AN_N"),
    spec$analyses
  )
  spec$analyses[[1]]$methodId <- "MTH_SUBJECT_COUNT_PCT"
  spec$analyses[[1]]$includeTotal <- TRUE
  spec$analyses[[1]]$totalLabel <- "Total"

  ars <- file.path(td, "subject_count_pct.json")
  writeLines(jsonlite::toJSON(spec, auto_unbox = TRUE, null = "null"), ars)

  ard_by_engine <- lapply(c(FALSE, TRUE), function(legacy) {
    ars_to_ard(ars, td, legacy = legacy)
  })
  expect_equal(.eq_norm(ard_by_engine[[1]]), .eq_norm(ard_by_engine[[2]]))

  for (i in seq_along(ard_by_engine)) {
    legacy <- c(FALSE, TRUE)[[i]]
    ard <- ard_by_engine[[i]]
    n_rows <- ard$stat_name == "N"
    p_rows <- ard$stat_name == "p"
    group_levels <- vapply(ard$group1_level, function(x) {
      if (length(x)) as.character(x[[1]]) else NA_character_
    }, character(1))
    variable_levels <- vapply(ard$variable_level, function(x) {
      if (length(x)) as.character(x[[1]]) else NA_character_
    }, character(1))

    expect_equal(
      sort(as.numeric(unlist(ard$stat[n_rows]))),
      c(4, 5, 9),
      info = paste("legacy =", legacy)
    )
    expect_equal(
      sort(as.numeric(unlist(ard$stat[p_rows]))),
      c(1, 1, 1),
      info = paste("legacy =", legacy)
    )
    expect_true(
      any(group_levels[p_rows] == "Total", na.rm = TRUE),
      info = paste("legacy =", legacy)
    )
    expect_true(
      all(variable_levels[p_rows] == "AN_N"),
      info = paste("legacy =", legacy)
    )
  }
})

## A spec with one analysis that carries a scoped Total column. `cohort` is
## the where-clause the Total pass is scoped by; leaving it NULL gives the
## unscoped Total, which is the count the scoped one has to come in under.
.eq_total_spec <- function(method_id = "MTH_AE_FREQUENCY_COUNT",
                           cohort = NULL) {
  spec <- .eq_spec()
  spec$outputs <- list(list(id = "OUT_AE", name = "T-AE",
                            referencedAnalysisIds = list("AN_AE")))
  spec$analyses <- Filter(function(a) identical(a$id, "AN_AE"), spec$analyses)
  spec$analyses[[1]]$methodId     <- method_id
  spec$analyses[[1]]$includeTotal <- TRUE
  spec$analyses[[1]]$totalLabel   <- "Total"
  if (!is.null(cohort)) spec$analyses[[1]]$totalWhere <- cohort
  spec
}

.eq_write <- function(spec, td, name) {
  path <- file.path(td, name)
  writeLines(jsonlite::toJSON(spec, auto_unbox = TRUE, null = "null"), path)
  path
}

## One statistic out of an ARD, by group level and variable level.
.eq_stat <- function(ard, group_level, variable_level, stat_name = "n") {
  flat <- function(column) vapply(ard[[column]], function(x) {
    if (length(x)) as.character(x[[1]]) else NA_character_
  }, character(1))
  rows <- which(flat("group1_level") == group_level &
                  flat("variable_level") == variable_level &
                  ard$stat_name == stat_name)
  if (length(rows) != 1L) {
    return(NA_real_)
  }
  as.numeric(ard$stat[[rows]])
}

test_that("a Total column scoped on another dataset counts the right rows", {
  skip_if_not_installed("cards")
  td <- withr::local_tempdir()
  .eq_adam(td)

  ## ADSL-only variable: the ADAE frame this analysis runs on has no COHORTN
  ## column at all, so a row-wise predicate reads FALSE for every AE.
  cohort_1 <- list(condition = list(dataset = "ADSL", variable = "COHORTN",
                                    comparator = "IN", value = list(1)))
  scoped   <- .eq_write(.eq_total_spec(cohort = cohort_1), td, "scoped.json")
  unscoped <- .eq_write(.eq_total_spec(), td, "unscoped.json")

  ard_new <- ars_to_ard(scoped, td)
  ard_leg <- ars_to_ard(scoped, td, legacy = TRUE)
  expect_equal(.eq_norm(ard_new), .eq_norm(ard_leg))

  ## Recomputed from the CSVs rather than stored: safety subjects in cohort 1
  ## with a treatment-emergent Headache are S01 and S06.
  adsl <- utils::read.csv(file.path(td, "adsl.csv"), stringsAsFactors = FALSE)
  adae <- utils::read.csv(file.path(td, "adae.csv"), stringsAsFactors = FALSE)
  in_scope <- adsl$USUBJID[adsl$SAFFL == "Y" & adsl$COHORTN == 1]
  teae <- adae[adae$TRTEMFL == "Y" & adae$USUBJID %in% adsl$USUBJID[adsl$SAFFL == "Y"], ]
  expected <- length(unique(teae$USUBJID[teae$AEDECOD == "Headache" &
                                           teae$USUBJID %in% in_scope]))
  all_subjects <- length(unique(teae$USUBJID[teae$AEDECOD == "Headache"]))

  for (ard in list(ard_new, ard_leg)) {
    got <- .eq_stat(ard, "Total", "Headache")
    expect_equal(got, expected)
    ## The two ways this can be wrong are both counts: dropping the clause
    ## gives every subject, and evaluating it on the AE frame gives none.
    expect_gt(got, 0)
    expect_lt(got, all_subjects)
  }
})

test_that("a Total column scoped on its own dataset is unchanged", {
  skip_if_not_installed("cards")
  td <- withr::local_tempdir()
  .eq_adam(td)

  ## Same-dataset clause: the AE frame can answer this one row by row, and
  ## that branch must keep behaving exactly as it did.
  headache <- list(condition = list(dataset = "ADAE", variable = "AEDECOD",
                                    comparator = "EQ", value = list("Headache")))
  ars <- .eq_write(.eq_total_spec(cohort = headache), td, "same_ds.json")

  ard_new <- ars_to_ard(ars, td)
  ard_leg <- ars_to_ard(ars, td, legacy = TRUE)
  expect_equal(.eq_norm(ard_new), .eq_norm(ard_leg))

  adsl <- utils::read.csv(file.path(td, "adsl.csv"), stringsAsFactors = FALSE)
  adae <- utils::read.csv(file.path(td, "adae.csv"), stringsAsFactors = FALSE)
  teae <- adae[adae$TRTEMFL == "Y" &
                 adae$USUBJID %in% adsl$USUBJID[adsl$SAFFL == "Y"], ]
  expected <- length(unique(teae$USUBJID[teae$AEDECOD == "Headache"]))

  for (ard in list(ard_new, ard_leg)) {
    expect_equal(.eq_stat(ard, "Total", "Headache"), expected)
    ## Nausea is outside the Total's own scope, so it has no Total cell.
    expect_true(is.na(.eq_stat(ard, "Total", "Nausea")))
  }
})

test_that("a scoped Total takes its denominator from the same clause", {
  skip_if_not_installed("cards")
  td <- withr::local_tempdir()
  .eq_adam(td)

  cohort_1 <- list(condition = list(dataset = "ADSL", variable = "COHORTN",
                                    comparator = "IN", value = list(1)))
  ars <- .eq_write(.eq_total_spec(cohort = cohort_1), td, "denominator.json")

  ard_new <- ars_to_ard(ars, td)
  ard_leg <- ars_to_ard(ars, td, legacy = TRUE)
  expect_equal(.eq_norm(ard_new), .eq_norm(ard_leg))

  adsl <- utils::read.csv(file.path(td, "adsl.csv"), stringsAsFactors = FALSE)
  ## The percentage is out of the cohort-restricted population, not the whole
  ## safety population: numerator and denominator are scoped by one clause,
  ## each masked on its own frame -- the AEs on ADAE, the population on ADSL.
  denominator <- sum(adsl$SAFFL == "Y" & adsl$COHORTN == 1)
  expect_lt(denominator, sum(adsl$SAFFL == "Y"))

  for (ard in list(ard_new, ard_leg)) {
    n <- .eq_stat(ard, "Total", "Headache", "n")
    expect_equal(.eq_stat(ard, "Total", "Headache", "N"), denominator)
    expect_equal(.eq_stat(ard, "Total", "Headache", "p"), n / denominator)
  }
})

test_that("decoded categorical analyses stay equivalent across engines", {
  skip_if_not_installed("cards")
  td <- withr::local_tempdir()
  utils::write.csv(data.frame(
    USUBJID  = sprintf("S%02d", 1:10),
    TRT01A   = rep(c("Drug A", "Placebo"), each = 5),
    SAFFL    = "Y",
    DCSREASN = c(1, 1, 2, NA, NA, 3, 1, NA, NA, NA),
    stringsAsFactors = FALSE
  ), file.path(td, "adsl.csv"), row.names = FALSE)

  spec <- list(
    analysisSets = list(list(id = "AS_SAF", name = "Safety",
      condition = list(dataset = "ADSL", variable = "SAFFL",
                       comparator = "EQ", value = list("Y")))),
    dataSubsets = list(),
    analysisGroupings = list(list(
      id = "GF_TRT",
      name = "TRT01A",
      groupingVariable = "TRT01A",
      groupingDataset = "ADSL",
      dataDriven = TRUE,
      groups = list()
    )),
    methods = list(list(
      id = "MTH_COUNT_AND_PERCENTAGE",
      name = "Count and Percentage"
    )),
    outputs = list(list(id = "OUT_DS", name = "T-DS",
                        referencedAnalysisIds = list("AN_REAS"))),
    analyses = list(list(
      id = "AN_REAS", methodId = "MTH_COUNT_AND_PERCENTAGE",
      label = "Discontinuation reason", dataset = "ADSL",
      variable = "DCSREASN",
      analysisVariable = list(dataset = "ADSL", variable = "DCSREASN"),
      analysisSetId = "AS_SAF", dataSubsetId = "",
      orderedGroupings = list(list(order = 1, groupingId = "GF_TRT",
                                   resultsByGroup = TRUE)),
      includeTotal = TRUE)),
    `_meta` = list(value_decodes = list(
      "ADSL.DCSREASN" = list(
        list(value = "1", label = "DEATH",             order = 1),
        list(value = "2", label = "LOST TO FOLLOW-UP", order = 2),
        list(value = "3", label = "OTHER",             order = 3),
        list(value = "4", label = "PREGNANCY",         order = 4)
      )
    ))
  )
  ars <- file.path(td, "ars.json")
  writeLines(jsonlite::toJSON(spec, auto_unbox = TRUE, null = "null"), ars)

  ard_new <- ars_to_ard(ars, td)
  ard_leg <- ars_to_ard(ars, td, legacy = TRUE)

  expect_false(is.null(ard_new))
  expect_false(is.null(ard_leg))
  expect_equal(.eq_norm(ard_new), .eq_norm(ard_leg))

  ## Both engines decoded -- including the zero-count PREGNANCY level.
  lv <- vapply(ard_new$variable_level, function(x)
    if (length(x)) as.character(x[[1]]) else NA_character_, character(1))
  expect_true(all(c("DEATH", "LOST TO FOLLOW-UP", "OTHER", "PREGNANCY") %in% lv))
  expect_false(any(lv %in% c("1", "2", "3", "4"), na.rm = TRUE))
})

## A subject count run over an occurrence frame, which is where the two
## engines used to part company. ADAE gives S01 two rows -- Headache and
## Nausea -- so deduplicating on the subject alone keeps whichever came
## first and reports the other term as zero. No Total column is involved;
## the disagreement is in the ordinary grouped pass.
test_that("subject counts on an occurrence frame count every term", {
  skip_if_not_installed("cards")
  td <- withr::local_tempdir()
  .eq_adam(td)

  for (method_id in c("MTH_SUBJECT_COUNT", "MTH_SUBJECT_COUNT_PCT")) {
    spec <- .eq_spec()
    spec$outputs <- list(list(id = "OUT_AE", name = "T-AE",
                              referencedAnalysisIds = list("AN_AE")))
    spec$analyses <- Filter(function(a) identical(a$id, "AN_AE"),
                            spec$analyses)
    spec$analyses[[1]]$methodId     <- method_id
    spec$analyses[[1]]$includeTotal <- FALSE

    ars <- .eq_write(spec, td, paste0(tolower(method_id), "_occurrence.json"))
    ard_new <- ars_to_ard(ars, td)
    ard_leg <- ars_to_ard(ars, td, legacy = TRUE)

    expect_equal(.eq_norm(ard_new), .eq_norm(ard_leg),
                 info = paste("method =", method_id))

    ## S01 and S02 report Headache, S01 alone reports Nausea. Nausea is the
    ## term carried by S01's second row, so it is the one a subject-level
    ## dedup drops.
    for (ard in list(ard_new, ard_leg)) {
      expect_equal(.eq_stat(ard, "Drug A", "Headache"), 2,
                   info = paste("method =", method_id))
      expect_equal(.eq_stat(ard, "Drug A", "Nausea"), 1,
                   info = paste("method =", method_id))
      expect_equal(.eq_stat(ard, "Placebo", "Headache"), 1,
                   info = paste("method =", method_id))
    }
  }
})
