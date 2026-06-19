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
