## Hand-built enriched section -- no LLM dependency.
.demo_section <- function(tlf = "T-14-1-1", title = "Demographics") {
  list(
    tlf_number       = tlf,
    tlf_type         = "TABLE",
    title            = title,
    population_text  = "Safety Population",
    population_annot = "ADSL.SAFFL='Y'",
    footnotes        = list("[a] Synthetic note"),
    source_datasets  = c("ADSL"),
    col_headers      = c("Characteristic", "Treatment A", "Placebo"),
    n_data_cols      = 2L,
    stub_rows        = list(
      list(label = "Age (years)", annotation = "ADSL.AGE", has_annot = TRUE,
           detection_method = "pattern", detection_confidence = "high"),
      list(label = "n", annotation = "", has_annot = FALSE,
           detection_method = NA_character_, detection_confidence = NA_character_)
    ),
    analysis_type    = "CONTINUOUS",
    ars_method_name  = "Summary Statistics - Continuous",
    by_variable      = "TRT01A",
    enriched_rows    = list(list(
      label = "Age (years)", primary_dataset = "ADSL",
      primary_variable = "AGE", data_subset = NULL,
      variable_role = "ANALYSIS"
    ))
  )
}

test_that("build_ars_json produces a structured ReportingEvent", {
  re <- build_ars_json(list(.demo_section()), study_id = "STUDY-001",
                       study_name = "Demo")
  expect_named(re, c("id", "name", "version", "listOfPlannedAnalyses",
                     "analysisSets", "dataSubsets", "analysisGroupings",
                     "methods", "analyses", "outputs", "_meta"))
  expect_equal(re$id, "STUDY-001")
  expect_length(re$analyses,     1)
  expect_length(re$outputs,      1)
  expect_length(re$analysisSets, 1)
})

test_that("AnalysisSet condition encodes the population flag", {
  re <- build_ars_json(list(.demo_section()))
  cond <- re$analysisSets[[1]]$condition
  expect_equal(cond$dataset,    "ADSL")
  expect_equal(cond$variable,   "SAFFL")
  expect_equal(cond$comparator, "EQ")
  expect_equal(cond$value[[1]], "Y")
})

test_that("IDs follow the deterministic convention", {
  re <- build_ars_json(list(.demo_section()))
  expect_equal(re$analysisSets[[1]]$id, "AS_SAFETY_POPULATION")
  expect_equal(re$analysisGroupings[[1]]$id, "GF_TRT01A")
  expect_equal(re$methods[[1]]$id, "MTH_SUMMARY_STATISTICS_CONTINUOUS")
  expect_equal(re$analyses[[1]]$id, "AN_T_14_1_1_001")
  expect_equal(re$outputs[[1]]$id, "T_14_1_1")
})

test_that("AnalysisSets de-duplicate across TLFs with same population", {
  re <- build_ars_json(list(.demo_section("T-14-1-1"),
                            .demo_section("T-14-3-1", "AE Summary")))
  expect_length(re$analysisSets, 1)
  expect_length(re$outputs,      2)
})

test_that("AnalysisMethod uses standard catalogue with operations populated", {
  re <- build_ars_json(list(.demo_section()))
  mth <- re$methods[[1]]
  expect_equal(mth$name, "Summary Statistics - Continuous")
  expect_gte(length(mth$operations), 5)
})

test_that("JSON serialises and round-trips cleanly", {
  re <- build_ars_json(list(.demo_section()))
  tmp <- tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(re, auto_unbox = TRUE, pretty = TRUE,
                              null = "null"), tmp)
  rt <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
  expect_equal(rt$id, re$id)
  expect_length(rt$outputs, length(re$outputs))
})
