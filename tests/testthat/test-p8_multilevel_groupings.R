## P8: multi-level groupings -- by_variables array, ordered GroupingFactors,
## orderedGroupings 1..n, include_total execution.
##
## These drive enrich_with_llm() on its LIVE branch (a provider and key are
## passed, rather than courier answers or offline = TRUE), which builds the
## request schema from ellmer. ellmer is a Suggests.
skip_if_not_installed("ellmer")

.mk_lookup2 <- function(keys) setNames(lapply(keys, function(k) list()), keys)

## --- enrichment: multi-variable resolution ------------------------------------

test_that("by_variables array resolves to ordered groupings + include_total", {
  testthat::local_mocked_bindings(
    .enrich_structured = function(...) list(
      analysis_type   = "CATEGORICAL",
      ars_method_name = "Count and Percentage",
      by_variables    = list("TRT01A", "SEX"),
      include_total   = TRUE,
      row_enrichments = list(list(label = "Age group",
                                  primary_dataset = "ADSL",
                                  primary_variable = "AGEGR1",
                                  variable_role = "ANALYSIS"))
    )
  )
  diag_reset()
  sec <- list(
    tlf_number = "T-14-1-2", tlf_type = "TABLE",
    title = "Demographics by Treatment and Sex",
    population_text = "Safety", population_annot = "",
    col_headers = c("", "Drug A male", "Drug A female", "Placebo male",
                    "Placebo female", "Total"),
    stub_rows = list(list(label = "Age group", annotation = "ADSL.AGEGR1",
                          has_annot = TRUE))
  )
  lk <- .mk_lookup2(c("ADSL.TRT01A", "ADSL.SEX", "ADSL.AGEGR1"))
  out <- enrich_with_llm(sec, spec_lookup = lk,
                         provider = "anthropic", model = "m", api_key = "k")
  expect_length(out$groupings, 2)
  expect_equal(out$groupings[[1]]$variable, "TRT01A")
  expect_equal(out$groupings[[2]]$variable, "SEX")
  expect_true(out$include_total)
  ## Back-compat fields = outermost grouping.
  expect_equal(out$by_variable, "TRT01A")
  expect_equal(out$by_variable_dataset, "ADSL")
  expect_equal(nrow(diag_records()), 0)
})

test_that("out-of-spec member of by_variables gets a WARN diagnostic", {
  testthat::local_mocked_bindings(
    .enrich_structured = function(...) list(
      analysis_type = "CATEGORICAL",
      by_variables  = list("TRT01A", "DOSEGRP"),
      row_enrichments = list()
    )
  )
  diag_reset()
  sec <- list(tlf_number = "T-1", tlf_type = "TABLE", title = "t",
              population_text = "", population_annot = "",
              col_headers = character(), stub_rows = list())
  lk <- .mk_lookup2(c("ADSL.TRT01A"))
  out <- enrich_with_llm(sec, spec_lookup = lk,
                         provider = "anthropic", model = "m", api_key = "k")
  expect_length(out$groupings, 2)
  recs <- diag_records()
  expect_true(any(grepl("DOSEGRP", recs$problem)))
})

## --- builder: multiple GroupingFactors + ordered groupings --------------------

.p8_section <- function(groupings, include_total = FALSE) {
  list(
    tlf_number = "T-14-1-2", tlf_type = "TABLE", title = "Demog",
    population_text = "Safety", population_annot = "",
    analysis_type = "CATEGORICAL", ars_method_name = "Count and Percentage",
    groupings = groupings, include_total = include_total,
    by_variable = if (length(groupings)) groupings[[1]]$variable else "",
    by_variable_dataset = if (length(groupings)) groupings[[1]]$dataset else "ADSL",
    stub_rows = list(list(label = "Age group", annotation = "ADSL.AGEGR1",
                          has_annot = TRUE)),
    enriched_rows = list(list(label = "Age group", primary_dataset = "ADSL",
                              primary_variable = "AGEGR1", data_subset = NULL,
                              variable_role = "ANALYSIS"))
  )
}

test_that("build emits one GroupingFactor per grouping, ordered 1..n", {
  sec <- .p8_section(list(
    list(variable = "TRT01A", dataset = "ADSL", in_spec = TRUE),
    list(variable = "SEX",    dataset = "ADSL", in_spec = TRUE)
  ), include_total = TRUE)
  re <- build_ars_json(list(sec), study_id = "S1")
  gf_vars <- vapply(re$analysisGroupings, function(g) g$groupingVariable,
                    character(1))
  expect_setequal(gf_vars, c("TRT01A", "SEX"))

  an <- re$analyses[[1]]
  og <- an$orderedGroupings
  expect_length(og, 2)
  expect_equal(og[[1]]$order, 1L)
  expect_equal(og[[2]]$order, 2L)
  expect_equal(og[[1]]$groupingId, "GF_TRT01A")
  expect_equal(og[[2]]$groupingId, "GF_SEX")
  expect_true(an$includeTotal)
})

test_that("grouping dataset propagates per grouping (BDS variable)", {
  sec <- .p8_section(list(
    list(variable = "TRT01A", dataset = "ADSL", in_spec = TRUE),
    list(variable = "AVISIT", dataset = "ADLB", in_spec = TRUE)
  ))
  re <- build_ars_json(list(sec), study_id = "S1")
  ds_by_var <- setNames(
    vapply(re$analysisGroupings, function(g) g$groupingDataset, character(1)),
    vapply(re$analysisGroupings, function(g) g$groupingVariable, character(1))
  )
  expect_equal(unname(ds_by_var["AVISIT"]), "ADLB")
})

test_that("legacy single by_variable section still builds one grouping", {
  sec <- .p8_section(list())
  sec$groupings <- NULL
  sec$by_variable <- "TRT01A"; sec$by_variable_dataset <- "ADSL"
  re <- build_ars_json(list(sec), study_id = "S1")
  expect_equal(re$analysisGroupings[[1]]$groupingVariable, "TRT01A")
  expect_length(re$analyses[[1]]$orderedGroupings, 1)
  expect_false(re$analyses[[1]]$includeTotal)
})

## --- executor: multi-by + Total pass -------------------------------------------

.write_p8_ars <- function(td, include_total = FALSE,
                          method_id = "MTH_COUNT_AND_PERCENTAGE",
                          variable = "AGEGR1") {
  spec <- list(
    analysisSets = list(), dataSubsets = list(),
    analysisGroupings = list(
      list(id = "GF_TRT01A", name = "TRT01A", groupingDataset = "ADSL",
           groupingVariable = "TRT01A", dataDriven = TRUE, groups = list()),
      list(id = "GF_SEX", name = "SEX", groupingDataset = "ADSL",
           groupingVariable = "SEX", dataDriven = TRUE, groups = list())
    ),
    methods = list(list(id = method_id, name = method_id)),
    outputs = list(),
    analyses = list(list(
      id = "AN_P8_001", methodId = method_id,
      dataset = "ADSL", variable = variable,
      analysisVariable = list(dataset = "ADSL", variable = variable),
      analysisSetId = "", dataSubsetId = "",
      orderedGroupings = list(
        list(order = 1, groupingId = "GF_TRT01A", resultsByGroup = TRUE),
        list(order = 2, groupingId = "GF_SEX",    resultsByGroup = TRUE)
      ),
      includeTotal = include_total,
      totalLabel = if (include_total) "Total" else ""
    ))
  )
  p <- file.path(td, "p8_ars.json")
  writeLines(jsonlite::toJSON(spec, auto_unbox = TRUE, null = "null"), p)
  p
}

.p8_adsl <- function(td) {
  utils::write.csv(data.frame(
    USUBJID = sprintf("%02d", 1:8),
    TRT01A  = rep(c("Drug A", "Placebo"), each = 4),
    SEX     = rep(c("M", "F"), times = 4),
    AGEGR1  = rep(c("<65", ">=65"), times = 4)
  ), file.path(td, "adsl.csv"), row.names = FALSE)
}

test_that("executor runs nested two-variable grouping", {
  skip_if_not_installed("cards")
  td <- withr::local_tempdir()
  .p8_adsl(td)
  ard <- ars_to_ard(.write_p8_ars(td), td)
  expect_false(is.null(ard))
  expect_true(all(c("group1", "group2") %in% names(ard)))
  expect_true("TRT01A" %in% ard$group1)
  expect_true("SEX" %in% ard$group2)
})

test_that("includeTotal preserves non-column groupings on Total rows", {
  skip_if_not_installed("cards")
  td <- withr::local_tempdir()
  .p8_adsl(td)
  ard <- ars_to_ard(.write_p8_ars(td, include_total = TRUE), td)

  group1_level <- .flat_chr(ard$group1_level)
  group2_level <- .flat_chr(ard$group2_level)
  total_rows <- group1_level == "Total"
  total_rows[is.na(total_rows)] <- FALSE

  expect_true(any(total_rows))
  expect_true(all(ard$group1[total_rows] == "TRT01A"))
  expect_true(all(ard$group2[total_rows] == "SEX"))
  expect_setequal(unique(group2_level[total_rows]), c("M", "F"))

  total_n <- total_rows & ard$stat_name == "N"
  total_p <- total_rows & ard$stat_name == "p"
  expect_true(all(as.numeric(unlist(ard$stat[total_n])) == 8))
  expect_setequal(
    unique(as.numeric(unlist(ard$stat[total_p]))),
    c(0, 0.5)
  )
})

test_that("legacy subject-key percentage Total retains row grouping", {
  skip_if_not_installed("cards")
  td <- withr::local_tempdir()
  .p8_adsl(td)

  ard <- suppressWarnings(ars_to_ard(
    .write_p8_ars(
      td,
      include_total = TRUE,
      method_id = "MTH_SUBJECT_COUNT_PCT",
      variable = "USUBJID"
    ),
    td,
    legacy = TRUE
  ))

  group1_level <- .flat_chr(ard$group1_level)
  group2_level <- .flat_chr(ard$group2_level)
  total_rows <- group1_level == "Total"
  total_rows[is.na(total_rows)] <- FALSE

  expect_true(any(total_rows))
  expect_true(all(ard$group2[total_rows] == "SEX"))
  expect_setequal(unique(group2_level[total_rows]), c("M", "F"))

  total_n <- total_rows & ard$stat_name == "N"
  total_p <- total_rows & ard$stat_name == "p"
  expect_true(all(as.numeric(unlist(ard$stat[total_n])) == 8))
  expect_setequal(unique(as.numeric(unlist(ard$stat[total_p]))), 0.5)
})

test_that("subject-key Total counts retain their row grouping", {
  skip_if_not_installed("cards")
  td <- withr::local_tempdir()
  .p8_adsl(td)

  for (method_id in c("MTH_SUBJECT_COUNT", "MTH_SUBJECT_COUNT_PCT")) {
    ard <- suppressWarnings(ars_to_ard(
      .write_p8_ars(
        td,
        include_total = TRUE,
        method_id = method_id,
        variable = "USUBJID"
      ),
      td
    ))

    group1_level <- .flat_chr(ard$group1_level)
    group2_level <- .flat_chr(ard$group2_level)
    total_rows <- group1_level == "Total"
    total_rows[is.na(total_rows)] <- FALSE

    expect_true(any(total_rows), info = method_id)
    expect_true(all(ard$group2[total_rows] == "SEX"), info = method_id)
    expect_setequal(
      unique(group2_level[total_rows]),
      c("M", "F")
    )
  }
})
