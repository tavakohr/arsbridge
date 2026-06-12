## Dynamic grouping resolution (P2): spec-grounded by_variable, dataset
## resolution, spec-detected treatment fallback, needs_review flagging.

.mk_lookup <- function(keys) {
  setNames(lapply(keys, function(k) list(key = k)), keys)
}

## --- .resolve_grouping_from_spec -------------------------------------------

test_that("bare variable resolves with its spec dataset (ADSL preferred)", {
  lk <- .mk_lookup(c("ADSL.TRT01A", "ADSL.SEX", "ADAE.TRTEMFL"))
  g <- .resolve_grouping_from_spec("SEX", lk)
  expect_equal(g$variable, "SEX")
  expect_equal(g$dataset, "ADSL")
  expect_true(g$in_spec)
})

test_that("variable living only in a BDS dataset resolves to that dataset", {
  lk <- .mk_lookup(c("ADSL.TRT01A", "ADLB.AVISIT", "ADLB.PARAM"))
  g <- .resolve_grouping_from_spec("AVISIT", lk)
  expect_equal(g$dataset, "ADLB")
  expect_true(g$in_spec)
})

test_that("dataset-qualified input keeps the qualifier when spec confirms it", {
  lk <- .mk_lookup(c("ADSL.AVISIT", "ADLB.AVISIT"))
  g <- .resolve_grouping_from_spec("ADLB.AVISIT", lk)
  expect_equal(g$variable, "AVISIT")
  expect_equal(g$dataset, "ADLB")
})

test_that("variable not in spec flags in_spec = FALSE, defaults dataset ADSL", {
  lk <- .mk_lookup(c("ADSL.TRT01A"))
  g <- .resolve_grouping_from_spec("DOSEGRP", lk)
  expect_false(g$in_spec)
  expect_equal(g$dataset, "ADSL")
})

test_that("lower-case input is normalised", {
  lk <- .mk_lookup(c("ADSL.AGEGR1"))
  g <- .resolve_grouping_from_spec("agegr1", lk)
  expect_equal(g$variable, "AGEGR1")
  expect_true(g$in_spec)
})

test_that("NULL / empty by_variable returns NULL", {
  lk <- .mk_lookup(c("ADSL.TRT01A"))
  expect_null(.resolve_grouping_from_spec(NULL, lk))
  expect_null(.resolve_grouping_from_spec("", lk))
})

test_that("no spec available returns in_spec = NA with ADSL default", {
  g <- .resolve_grouping_from_spec("TRT01A", NULL)
  expect_true(is.na(g$in_spec))
  expect_equal(g$dataset, "ADSL")
})

## --- .default_treatment_var -------------------------------------------------

test_that("prefers TRT01A, then TRT01P, then other TRTxx, then ACTARM/ARM", {
  expect_equal(.default_treatment_var(
    .mk_lookup(c("ADSL.TRT01P", "ADSL.TRT01A"))), "TRT01A")
  expect_equal(.default_treatment_var(
    .mk_lookup(c("ADSL.TRT01P", "ADSL.ARM"))), "TRT01P")
  expect_equal(.default_treatment_var(
    .mk_lookup(c("ADSL.TRT02A", "ADSL.AGE"))), "TRT02A")
  expect_equal(.default_treatment_var(
    .mk_lookup(c("ADSL.ACTARM", "ADSL.ARM"))), "ACTARM")
  expect_equal(.default_treatment_var(
    .mk_lookup(c("ADSL.ARM"))), "ARM")
})

test_that("returns NULL when spec has no treatment-like ADSL variable", {
  expect_null(.default_treatment_var(.mk_lookup(c("ADSL.AGE", "ADAE.ARM"))))
  expect_null(.default_treatment_var(NULL))
  expect_null(.default_treatment_var(list()))
})

## --- .build_grouping dataset propagation ------------------------------------

test_that(".build_grouping uses the spec-resolved dataset", {
  sec <- list(by_variable = "AVISIT", by_variable_dataset = "ADLB")
  gf <- .build_grouping(sec)
  expect_equal(gf$groupingDataset, "ADLB")
  expect_equal(gf$groupingVariable, "AVISIT")
})

test_that(".build_grouping falls back to ADSL when dataset not resolved", {
  sec <- list(by_variable = "TRT01A")
  gf <- .build_grouping(sec)
  expect_equal(gf$groupingDataset, "ADSL")
})

## --- prompt anchor removed ---------------------------------------------------

test_that("enrichment prompt no longer anchors to TRT01A", {
  path <- system.file("prompts", "enrich_tlf_prompt.txt", package = "arsbridge")
  if (!nzchar(path)) path <- file.path("..", "..", "inst", "prompts",
                                       "enrich_tlf_prompt.txt")
  skip_if(!file.exists(path), "prompt template not found")
  tmpl <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_false(grepl("Use \"TRT01A\"", tmpl, fixed = TRUE))
  expect_true(grepl("available_variables", tmpl, fixed = TRUE))
  ## Ungrouped outputs are signalled by an empty string in the schema.
  expect_true(grepl("empty string", tmpl, fixed = TRUE))
})

## --- _meta.sections_needing_review -------------------------------------------

test_that("build_ars_json lists needs_review sections in _meta", {
  sec_ok <- list(
    tlf_number = "T-1-1-1", tlf_type = "TABLE", title = "Demog",
    population_text = "Safety", population_annot = "ADSL.SAFFL='Y'",
    analysis_type = "CONTINUOUS",
    ars_method_name = "Summary Statistics - Continuous",
    by_variable = "TRT01A", by_variable_dataset = "ADSL",
    stub_rows = list(list(label = "Age", annotation = "ADSL.AGE",
                          has_annot = TRUE)),
    enriched_rows = list(list(label = "Age", primary_dataset = "ADSL",
                              primary_variable = "AGE",
                              data_subset = NULL,
                              variable_role = "ANALYSIS"))
  )
  sec_review <- sec_ok
  sec_review$tlf_number <- "T-2-2-2"
  sec_review$by_variable <- ""
  sec_review$by_variable_dataset <- "ADSL"
  sec_review$needs_review <- TRUE

  re <- build_ars_json(list(sec_ok, sec_review), study_id = "S1")
  expect_equal(unlist(re$`_meta`$sections_needing_review), "T-2-2-2")
})

test_that("sections_needing_review empty when all groupings resolved", {
  sec_ok <- list(
    tlf_number = "T-1-1-1", tlf_type = "TABLE", title = "Demog",
    population_text = "Safety", population_annot = "",
    analysis_type = "CONTINUOUS",
    ars_method_name = "Summary Statistics - Continuous",
    by_variable = "TRT01A", by_variable_dataset = "ADSL",
    stub_rows = list(), enriched_rows = list()
  )
  re <- build_ars_json(list(sec_ok), study_id = "S1")
  expect_length(re$`_meta`$sections_needing_review, 0)
})
