## resolve_analysis(): the shared, data-independent resolver that keeps the
## emitted cards code (ars_to_code.R) and the executed ARD (ars_to_ard.R) in
## lock-step. These tests pin its contract on a hand-built ARS spec.

.ra_spec <- function() {
  list(
    analysisSets = list(
      list(id = "AS_SAF", name = "Safety",
           condition = list(dataset = "ADSL", variable = "SAFFL",
                            comparator = "EQ", value = list("Y")))
    ),
    dataSubsets = list(
      list(id = "DS_RAND",
           condition = list(dataset = "ADSL", variable = "RANDFL",
                            comparator = "EQ", value = list("Y")))
    ),
    analysisGroupings = list(
      list(id = "GF_TRT", name = "TRT01A", groupingVariable = "TRT01A"),
      list(id = "GF_SEX", name = "SEX",    groupingVariable = "SEX")
    ),
    outputs = list(
      list(id = "OUT_T1", name = "T-14-1-1",
           referencedAnalysisIds = list("AN_1"))
    ),
    analyses = list(
      list(
        id = "AN_1", methodId = "MTH_COUNT_AND_PERCENTAGE",
        label = "Randomized", description = "Subjects randomized",
        annotation = "ADSL.RANDFL='Y'",
        analysisVariable = list(dataset = "ADSL", variable = "AGEGR1"),
        analysisSetId = "AS_SAF", dataSubsetId = "DS_RAND",
        orderedGroupings = list(
          list(order = 1, groupingId = "GF_TRT", resultsByGroup = TRUE),
          list(order = 2, groupingId = "GF_SEX", resultsByGroup = TRUE)
        ),
        includeTotal = TRUE
      )
    )
  )
}

test_that("resolve_analysis flattens an analysis into execution args", {
  spec <- .ra_spec()
  res  <- resolve_analysis(spec$analyses[[1]], spec)

  expect_equal(res$analysis_id, "AN_1")
  expect_equal(res$output_id, "OUT_T1")
  expect_equal(res$method_id, "MTH_COUNT_AND_PERCENTAGE")
  expect_equal(res$dataset, "ADSL")
  expect_equal(res$variable, "AGEGR1")
  expect_equal(res$by, c("TRT01A", "SEX"))
  expect_true(res$include_total)
  expect_equal(res$subject_key, "USUBJID")
  expect_equal(res$label, "Randomized")
  expect_equal(res$annotation, "ADSL.RANDFL='Y'")
  expect_equal(res$description, "Subjects randomized")
})

test_that("resolve_analysis resolves pop / subset WhereClauses by id", {
  spec <- .ra_spec()
  res  <- resolve_analysis(spec$analyses[[1]], spec)

  expect_equal(res$pop_where$condition$variable, "SAFFL")
  expect_equal(res$subset_where$condition$variable, "RANDFL")
})

test_that("resolve_analysis tolerates missing optional fields", {
  spec <- .ra_spec()
  ana  <- spec$analyses[[1]]
  ana$analysisSetId <- ""
  ana$dataSubsetId  <- ""
  ana$orderedGroupings <- NULL
  ana$includeTotal  <- NULL
  ana$label <- NULL
  res <- resolve_analysis(ana, spec)

  expect_null(res$pop_where)
  expect_null(res$subset_where)
  expect_identical(res$by, character(0))
  expect_false(res$include_total)
  expect_null(res$label)
  expect_null(res$sap_description)
})

test_that("resolve_analysis honours a custom subject_key", {
  spec <- .ra_spec()
  res  <- resolve_analysis(spec$analyses[[1]], spec, subject_key = "SUBJID")
  expect_equal(res$subject_key, "SUBJID")
})
