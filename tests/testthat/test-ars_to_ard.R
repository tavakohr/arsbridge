library(testthat)

test_that("ars_to_ard works on synthetic ARS and ADaM datasets", {
  # 1. Create temporary directory for ADaM datasets
  adam_dir <- tempfile("adam_")
  dir.create(adam_dir)

  # Write synthetic ADSL
  adsl <- data.frame(
    USUBJID = c("SUBJ1", "SUBJ2", "SUBJ3", "SUBJ4"),
    TRT01A  = c("Drug A", "Drug A", "Placebo", "Placebo"),
    SAFFL   = c("Y", "Y", "Y", "N"),
    AGE     = c(45, 50, 55, 60),
    SEX     = c("M", "F", "M", "F"),
    stringsAsFactors = FALSE
  )
  utils::write.csv(adsl, file.path(adam_dir, "ADSL.csv"), row.names = FALSE)

  # Write synthetic ADAE
  adae <- data.frame(
    USUBJID = c("SUBJ1", "SUBJ1", "SUBJ2", "SUBJ3"),
    TRT01A  = c("Drug A", "Drug A", "Drug A", "Placebo"),
    AEDECOD = c("Headache", "Nausea", "Headache", "Headache"),
    TRTEMFL = c("Y", "Y", "Y", "N"),
    stringsAsFactors = FALSE
  )
  utils::write.csv(adae, file.path(adam_dir, "ADAE.csv"), row.names = FALSE)

  # 2. Build mock ARS JSON structure
  spec <- list(
    id = "MOCK-STUDY",
    name = "Mock Study",
    version = "1",
    analysisSets = list(
      list(
        id = "AS_SAFETY",
        name = "Safety Population",
        condition = list(
          dataset = "ADSL",
          variable = "SAFFL",
          comparator = "EQ",
          value = list("Y")
        )
      )
    ),
    dataSubsets = list(
      list(
        id = "DS_TEAE",
        name = "Treatment-Emergent AEs",
        condition = list(
          dataset = "ADAE",
          variable = "TRTEMFL",
          comparator = "EQ",
          value = list("Y")
        )
      )
    ),
    analysisGroupings = list(
      list(
        id = "GF_TRT01A",
        name = "TRT01A",
        groupingVariable = list(
          dataset = "ADSL",
          variable = "TRT01A"
        )
      )
    ),
    methods = list(
      list(id = "MTH_SUMMARY_STATISTICS_CONTINUOUS", name = "Summary Statistics - Continuous"),
      list(id = "MTH_COUNT_AND_PERCENTAGE", name = "Count and Percentage"),
      list(id = "MTH_AE_FREQUENCY_COUNT", name = "AE Frequency Count"),
      list(id = "MTH_SUBJECT_COUNT", name = "Subject Count")
    ),
    analyses = list(
      # Demographics: Age (Continuous)
      list(
        id = "AN_DEMOG_AGE",
        name = "Age Analysis",
        analysisSetId = "AS_SAFETY",
        methodId = "MTH_SUMMARY_STATISTICS_CONTINUOUS",
        analysisVariable = list(dataset = "ADSL", variable = "AGE"),
        orderedGroupings = list(list(groupingId = "GF_TRT01A"))
      ),
      # Demographics: Sex (Categorical)
      list(
        id = "AN_DEMOG_SEX",
        name = "Sex Analysis",
        analysisSetId = "AS_SAFETY",
        methodId = "MTH_COUNT_AND_PERCENTAGE",
        analysisVariable = list(dataset = "ADSL", variable = "SEX"),
        orderedGroupings = list(list(groupingId = "GF_TRT01A"))
      ),
      # AEs: AE Frequency Count (Categorical, unique subject counts)
      list(
        id = "AN_AE_FREQ",
        name = "AE Frequency",
        analysisSetId = "AS_SAFETY",
        dataSubsetId = "DS_TEAE",
        methodId = "MTH_AE_FREQUENCY_COUNT",
        analysisVariable = list(dataset = "ADAE", variable = "AEDECOD"),
        orderedGroupings = list(list(groupingId = "GF_TRT01A"))
      )
    ),
    outputs = list(
      list(
        id = "T_DEMOG",
        name = "T-Demog",
        referencedAnalysisIds = list("AN_DEMOG_AGE", "AN_DEMOG_SEX")
      ),
      list(
        id = "T_AE",
        name = "T-AE",
        referencedAnalysisIds = list("AN_AE_FREQ")
      )
    )
  )

  ars_path <- tempfile("ars_", fileext = ".json")
  writeLines(jsonlite::toJSON(spec, auto_unbox = TRUE, pretty = TRUE, null = "null"), ars_path)

  # 3. Test: Full execution
  ard_full <- ars_to_ard(ars_path, adam_dir)
  expect_s3_class(ard_full, "card")
  expect_true("analysis_id" %in% names(ard_full))
  expect_true("method_id" %in% names(ard_full))
  expect_true("output_id" %in% names(ard_full))

  # Provenance columns (ADR 0002, phase 1): every computed row self-describes.
  for (col in c("result_status", "value_source", "derivation_ref",
                "derived_by", "derived_dt")) {
    expect_true(col %in% names(ard_full), info = paste("missing column", col))
  }
  expect_true(all(ard_full$result_status == "computed"))
  expect_true(all(ard_full$value_source == "cards"))
  expect_true(all(ard_full$derived_by == "arsbridge"))
  expect_true(all(grepl("^arsbridge:emitted:", ard_full$derivation_ref)))
  # derived_dt is stamped once per run -> a single value across all rows.
  expect_equal(length(unique(ard_full$derived_dt)), 1L)

  # Check demographics: AGE (Continuous) has been calculated
  age_ard <- dplyr::filter(ard_full, analysis_id == "AN_DEMOG_AGE")
  expect_gt(nrow(age_ard), 0)
  expect_equal(unique(age_ard$variable), "AGE")

  # Safety Population has 3 subjects (SUBJ1, SUBJ2, SUBJ3) - SUBJ4 is excluded (SAFFL='N').
  # AGE values: SUBJ1 (45), SUBJ2 (50), SUBJ3 (55).
  # Drug A has SUBJ1, SUBJ2 (mean = 47.5)
  # Placebo has SUBJ3 (mean = 55)
  mean_drug_a <- age_ard |>
    dplyr::filter(group1_level == "Drug A", stat_name == "mean") |>
    dplyr::pull(stat) |>
    unlist()
  expect_equal(mean_drug_a, 47.5)

  # Check selective filtering by output_ids
  ard_demog <- ars_to_ard(ars_path, adam_dir, output_ids = "T_DEMOG")
  expect_true(all(ard_demog$output_id == "T_DEMOG"))
  expect_false("AN_AE_FREQ" %in% ard_demog$analysis_id)

  # Check selective filtering by analysis_ids
  ard_sex_only <- ars_to_ard(ars_path, adam_dir, analysis_ids = "AN_DEMOG_SEX")
  expect_equal(unique(ard_sex_only$analysis_id), "AN_DEMOG_SEX")

  # Check AE frequency count unique subject logic
  # In ADAE: SUBJ1 has Headache & Nausea, SUBJ2 has Headache, SUBJ3 has Headache (but TRTEMFL = N, so excluded by subset).
  # Under Safety Pop:
  # Drug A: SUBJ1 (Headache, Nausea), SUBJ2 (Headache). Total unique Headache in Drug A = 2 subjects.
  ae_ard <- dplyr::filter(ard_full, analysis_id == "AN_AE_FREQ")
  headache_drug_a <- ae_ard |>
    dplyr::filter(group1_level == "Drug A", variable_level == "Headache", stat_name == "n") |>
    dplyr::pull(stat) |>
    unlist()
  expect_equal(headache_drug_a, 2)

  # Clean up
  unlink(adam_dir, recursive = TRUE)
  unlink(ars_path)
})

test_that("declared-but-unexecutable method reserves manual_pending stub rows", {
  skip_if_not_installed("cards")
  adam_dir <- withr::local_tempdir()
  utils::write.csv(data.frame(
    USUBJID = sprintf("S%02d", 1:6),
    TRT01A  = rep(c("Drug A", "Placebo"), each = 3),
    SAFFL   = rep("Y", 6),
    stringsAsFactors = FALSE
  ), file.path(adam_dir, "ADSL.csv"), row.names = FALSE)
  utils::write.csv(data.frame(
    USUBJID = sprintf("S%02d", 1:6),
    TRT01A  = rep(c("Drug A", "Placebo"), each = 3),
    SAFFL   = rep("Y", 6),
    AVAL    = c(1, 0, 1, 0, 1, 0),
    stringsAsFactors = FALSE
  ), file.path(adam_dir, "ADEFF.csv"), row.names = FALSE)

  spec <- list(
    id = "MOCK", name = "Mock", version = "1",
    analysisSets = list(list(id = "AS_ITT", name = "ITT",
      condition = list(dataset = "ADEFF", variable = "SAFFL",
                       comparator = "EQ", value = list("Y")))),
    analysisGroupings = list(list(id = "GF_TRT", name = "TRT01A",
      groupingVariable = list(dataset = "ADEFF", variable = "TRT01A"))),
    methods = list(list(id = "MTH_CMH_TEST", name = "CMH test")),
    analyses = list(list(
      id = "AN_CMH", name = "EASI75 CMH", analysisSetId = "AS_ITT",
      methodId = "MTH_CMH_TEST",
      analysisVariable = list(dataset = "ADEFF", variable = "AVAL"),
      orderedGroupings = list(list(groupingId = "GF_TRT")))),
    outputs = list(list(id = "T_14_2_1", name = "T-14.2.1",
      referencedAnalysisIds = list("AN_CMH")))
  )
  ars_path <- tempfile("ars_", fileext = ".json")
  writeLines(jsonlite::toJSON(spec, auto_unbox = TRUE, null = "null"), ars_path)

  ard <- ars_to_ard(ars_path, adam_dir)
  expect_s3_class(ard, "card")

  cmh <- dplyr::filter(ard, analysis_id == "AN_CMH")
  # MTH_CMH_TEST declares a single (not by-group) p.value stat -> one stub row.
  expect_equal(nrow(cmh), 1L)
  expect_equal(unique(cmh$result_status), "manual_pending")
  expect_equal(unique(cmh$stat_name), "p.value")
  expect_true(is.na(unlist(cmh$stat)))
  expect_true(is.na(unique(cmh$value_source)))
  expect_true(is.na(unique(cmh$derived_dt)))   # not stamped until filled
  # keyed to the analysis/method/output so the value is never an orphan
  expect_equal(unique(cmh$method_id), "MTH_CMH_TEST")
  expect_equal(unique(cmh$output_id), "T_14_2_1")

  # Worklist surfaces the pending cell
  wl <- ars_manual_worklist(ard)
  expect_equal(nrow(wl), 1L)
  expect_equal(wl$analysis_id, "AN_CMH")
  expect_equal(wl$stat_name, "p.value")
})

test_that("ars_manual_worklist returns an empty frame when nothing is pending", {
  expect_equal(nrow(ars_manual_worklist(NULL)), 0L)
  fake <- data.frame(result_status = c("computed", "computed"),
                     stringsAsFactors = FALSE)
  expect_equal(nrow(ars_manual_worklist(fake)), 0L)
})

test_that("ars_validate_manual_fills flags untraceable and unfilled manual cells", {
  ard <- data.frame(
    output_id      = c("T1", "T1", "T1", "T1"),
    analysis_id    = c("A1", "A2", "A3", "A4"),
    method_id      = "MTH_CMH_TEST",
    stat_name      = "p.value",
    result_status  = c("manual_filled", "manual_filled", "manual_filled",
                       "computed"),
    derivation_ref = c("cmh.R", NA, "cmh.R", "arsbridge:emitted:A4"),
    stringsAsFactors = FALSE)
  ard$stat <- list(0.02, 0.03, NA_real_, 0.5)   # A2 no ref, A3 no value

  bad <- ars_validate_manual_fills(ard)
  expect_equal(nrow(bad), 2L)
  expect_setequal(bad$analysis_id, c("A2", "A3"))
  expect_match(bad$problem[bad$analysis_id == "A2"], "derivation_ref")
  expect_match(bad$problem[bad$analysis_id == "A3"], "NA")
})

test_that("ars_validate_manual_fills passes a fully traceable fill", {
  ard <- data.frame(
    output_id = "T1", analysis_id = "A1", method_id = "MTH_CMH_TEST",
    stat_name = "p.value", result_status = "manual_filled",
    derivation_ref = "cmh_t1421.R", stringsAsFactors = FALSE)
  ard$stat <- list(0.012)
  expect_equal(nrow(ars_validate_manual_fills(ard)), 0L)
  expect_equal(nrow(ars_validate_manual_fills(NULL)), 0L)
})

test_that("ard_cmh_test returns a one-row CMH p-value card", {
  skip_if_not_installed("cards")
  d <- data.frame(
    RESP   = rep(c("Y", "N"), 30),
    TRT    = rep(c("A", "B"), each = 30),
    REGION = rep(c("NA", "EU", "APAC"), 20),
    stringsAsFactors = FALSE)
  card <- ard_cmh_test(d, response = "RESP", by = "TRT", strata = "REGION")
  expect_equal(nrow(card), 1L)
  expect_equal(card$stat_name, "p.value")
  p <- as.numeric(card$stat[[1]])
  expect_true(is.na(p) || (p >= 0 && p <= 1))
  # Missing column is a clear error.
  expect_error(ard_cmh_test(d, "RESP", "TRT", "NOPE"), "not found")
})

test_that("MTH_CMH_TEST computes with a strata operand, reserves without one", {
  skip_if_not_installed("cards")
  adam_dir <- withr::local_tempdir()
  utils::write.csv(data.frame(
    USUBJID = sprintf("S%03d", 1:60),
    TRT01A  = rep(c("Drug A", "Placebo"), 30),
    SAFFL   = "Y",
    REGION  = rep(c("NA", "EU", "APAC"), 20),
    AVAL    = rep(c(1, 0), 30),
    stringsAsFactors = FALSE
  ), file.path(adam_dir, "ADSL.csv"), row.names = FALSE)

  mk_spec <- function(strata) {
    grp <- list(list(order = 1, groupingId = "GF_TRT", resultsByGroup = TRUE))
    ana <- list(id = "AN_CMH", name = "CMH", analysisSetId = "AS_ITT",
                methodId = "MTH_CMH_TEST",
                analysisVariable = list(dataset = "ADSL", variable = "AVAL"),
                orderedGroupings = grp)
    if (!is.null(strata)) ana$strata <- strata
    list(id = "S", name = "S", version = "1",
      analysisSets = list(list(id = "AS_ITT", name = "ITT",
        condition = list(dataset = "ADSL", variable = "SAFFL",
                         comparator = "EQ", value = list("Y")))),
      analysisGroupings = list(list(id = "GF_TRT", name = "TRT01A",
        groupingVariable = list(dataset = "ADSL", variable = "TRT01A"))),
      methods = list(list(id = "MTH_CMH_TEST", name = "CMH")),
      analyses = list(ana),
      outputs = list(list(id = "T_CMH", name = "T-CMH",
        referencedAnalysisIds = list("AN_CMH"))))
  }

  p1 <- tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(mk_spec("REGION"), auto_unbox = TRUE, null = "null"), p1)
  ard1 <- ars_to_ard(p1, adam_dir)
  cmh1 <- ard1[ard1$method_id == "MTH_CMH_TEST", , drop = FALSE]
  expect_equal(unique(cmh1$result_status), "computed")
  expect_equal(unique(cmh1$value_source), "stats")
  expect_equal(unique(cmh1$stat_name), "p.value")
  expect_false(is.na(as.numeric(cmh1$stat[[1]])))

  p2 <- tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(mk_spec(NULL), auto_unbox = TRUE, null = "null"), p2)
  ard2 <- ars_to_ard(p2, adam_dir)
  cmh2 <- ard2[ard2$method_id == "MTH_CMH_TEST", , drop = FALSE]
  expect_equal(unique(cmh2$result_status), "manual_pending")
})
