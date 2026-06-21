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
  expect_named(re, c("id", "name", "version",
                     "otherListsOfContents", "mainListOfContents",
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

test_that("AnalysisSet has level + order fields (siera requirement)", {
  re <- build_ars_json(list(.demo_section()))
  as_obj <- re$analysisSets[[1]]
  expect_equal(as_obj$level, 1L)
  expect_equal(as_obj$order, 1L)
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

test_that("AnalysisGrouping has FLAT groupingDataset/groupingVariable (siera requirement)", {
  re <- build_ars_json(list(.demo_section()))
  gf <- re$analysisGroupings[[1]]
  expect_equal(gf$groupingDataset,  "ADSL")
  expect_equal(gf$groupingVariable, "TRT01A")   # flat string, not list
  expect_true(is.character(gf$groupingVariable))
  expect_false(is.list(gf$groupingVariable))
  expect_true(is.list(gf$groups))               # empty array, not NULL
})

test_that("Analysis emits both flat dataset/variable AND nested analysisVariable", {
  re <- build_ars_json(list(.demo_section()))
  an <- re$analyses[[1]]
  expect_equal(an$dataset,  "ADSL")             # flat (siera reads this)
  expect_equal(an$variable, "AGE")
  expect_equal(an$analysisVariable$dataset,  "ADSL")  # nested (ARS spec)
  expect_equal(an$analysisVariable$variable, "AGE")
  expect_equal(an$version, "1")
  expect_true(is.list(an$categoryIds))
})

test_that("AnalysisMethod has codeTemplate with code + parameters (siera requirement)", {
  re <- build_ars_json(list(.demo_section()))
  mth <- re$methods[[1]]
  expect_true(!is.null(mth$codeTemplate))
  expect_true(nzchar(mth$codeTemplate$code))
  expect_match(mth$codeTemplate$code, "df3_analysisidhere")
  expect_match(mth$codeTemplate$context, "siera")
  expect_true(is.list(mth$codeTemplate$parameters))
  expect_true(!is.null(mth$label))              # siera reads label
})

test_that("Every standard method has a non-empty codeTemplate", {
  for (name in names(arsbridge:::.STANDARD_METHODS)) {
    mth <- arsbridge:::.STANDARD_METHODS[[name]]
    expect_true(!is.null(mth$codeTemplate),     info = name)
    expect_true(nzchar(mth$codeTemplate$code),  info = name)
    expect_match(mth$codeTemplate$code, "df3_analysisidhere", info = name)
  }
})

test_that("capability-gated section keeps a declarative analysis + method (ADR 0002 ph3)", {
  sec <- .demo_section("T-14-2-1", "EASI75 CMH (primary endpoint)")
  sec$unsupported        <- TRUE
  sec$unsupported_reason <- "requires Cochran-Mantel-Haenszel test"
  re <- build_ars_json(list(sec))

  # Output is no longer stripped: it references its analysis.
  out <- re$outputs[[1]]
  expect_equal(out$id, "T_14_2_1")
  expect_gt(length(out$referencedAnalysisIds), 0)

  # The analysis uses the declarative unsupported method id (so the engine
  # reserves a manual_pending stub row for it).
  an <- re$analyses[[1]]
  expect_equal(an$methodId, "MTH_UNSUPPORTED_ANALYSIS")

  # The method is carried in the catalogue, flagged supported = FALSE with the
  # capability reason -- the Output -> Analysis -> Method chain stays intact.
  mth <- Filter(function(m) identical(m$id, "MTH_UNSUPPORTED_ANALYSIS"),
                re$methods)
  expect_length(mth, 1)
  expect_false(mth[[1]]$supported)
  expect_match(mth[[1]]$description, "Mantel")

  # Still recorded for the renderer's numbered placeholder.
  expect_length(re$`_meta`$unsupported_outputs, 1)
  expect_equal(re$`_meta`$unsupported_outputs[[1]]$id, "T_14_2_1")
})

test_that("a gated section with detectable methods builds executable analyses", {
  sec <- .demo_section("T-14-2-1", "EASI 75 responders")
  sec$unsupported <- TRUE
  sec$unsupported_reason <- "requires Cochran-Mantel-Haenszel test"
  sec$footnotes <- list("Clopper-Pearson 95% CI",
                        "Cochran-Mantel-Haenszel stratified by REGION")
  re <- build_ars_json(list(sec))

  mids <- vapply(re$analyses, function(a) a$methodId %||% "", character(1))
  expect_true("MTH_PROPORTION_CI_EXACT" %in% mids)
  expect_true("MTH_CMH_TEST" %in% mids)
  cmh <- Filter(function(a) identical(a$methodId, "MTH_CMH_TEST"), re$analyses)[[1]]
  expect_equal(cmh$strata, "REGION")
  # The exact-CI / CMH methods are carried as supported.
  ci_m <- Filter(function(m) identical(m$id, "MTH_PROPORTION_CI_EXACT"), re$methods)[[1]]
  expect_true(ci_m$supported)
  # Residual is empty -> the output is NOT reserved as a whole-table placeholder.
  ph_ids <- vapply(re$`_meta`$unsupported_outputs, function(u) u$id %||% "",
                   character(1))
  expect_false("T_14_2_1" %in% ph_ids)
})

test_that("classified methods compute end-to-end (CI + CMH) from a gated section", {
  skip_if_not_installed("cards")
  skip_if_not(cardx_ci_works(), "cardx cannot compute a CI in this environment")
  adam_dir <- withr::local_tempdir()
  utils::write.csv(data.frame(
    USUBJID = sprintf("S%03d", 1:60),
    TRT01A  = rep(c("Drug A", "Placebo"), 30),
    SAFFL   = "Y",
    REGION  = rep(c("NA", "EU", "APAC"), 20),
    AVAL    = rep(c(1, 0), 30),
    stringsAsFactors = FALSE
  ), file.path(adam_dir, "ADSL.csv"), row.names = FALSE)

  sec <- list(
    tlf_number = "T-14-2-1", tlf_type = "TABLE", title = "EASI 75 responders",
    population_text = "ITT", population_annot = "ADSL.SAFFL='Y'",
    footnotes = list("Clopper-Pearson 95% CI",
                     "Cochran-Mantel-Haenszel stratified by REGION"),
    col_headers = c("Statistic", "Drug A", "Placebo"),
    by_variable = "TRT01A",
    stub_rows = list(list(label = "Responders, n (%)", annotation = "ADSL.AVAL",
                          has_annot = TRUE)),
    enriched_rows = list(list(label = "Responders, n (%)",
                              primary_dataset = "ADSL", primary_variable = "AVAL",
                              variable_role = "ANALYSIS")),
    analysis_type = "COUNT", ars_method_name = "Count and Percentage",
    unsupported = TRUE,
    unsupported_reason = "requires Cochran-Mantel-Haenszel test")

  re <- build_ars_json(list(sec), study_id = "S1")
  ars_path <- tempfile("ars_", fileext = ".json")
  writeLines(jsonlite::toJSON(re, auto_unbox = TRUE, null = "null"), ars_path)

  ard <- ars_to_ard(ars_path, adam_dir)
  expect_s3_class(ard, "card")
  src <- function(m) unique(ard$value_source[ard$method_id == m])
  expect_equal(src("MTH_PROPORTION_CI_EXACT"), "cardx")
  expect_equal(src("MTH_CMH_TEST"), "stats")
  # No reserved cells: both inferential methods computed.
  expect_false(any(ard$result_status == "manual_pending"))
})

test_that("gated section flows end-to-end to a manual_pending ARD row", {
  skip_if_not_installed("cards")
  sec <- .demo_section("T-14-2-1", "EASI75 CMH (primary endpoint)")
  sec$unsupported        <- TRUE
  sec$unsupported_reason <- "requires Cochran-Mantel-Haenszel test"
  re <- build_ars_json(list(sec))

  adam_dir <- withr::local_tempdir()
  utils::write.csv(data.frame(
    USUBJID = c("S1", "S2", "S3", "S4"),
    TRT01A  = c("A", "A", "B", "B"),
    SAFFL   = rep("Y", 4),
    AGE     = c(60L, 65L, 70L, 75L),
    stringsAsFactors = FALSE
  ), file.path(adam_dir, "ADSL.csv"), row.names = FALSE)

  ars_path <- tempfile("ars_", fileext = ".json")
  writeLines(jsonlite::toJSON(re, auto_unbox = TRUE, null = "null"), ars_path)

  ard <- ars_to_ard(ars_path, adam_dir)
  expect_s3_class(ard, "card")
  expect_true(any(ard$result_status == "manual_pending"))
  pend <- ard[ard$result_status == "manual_pending", , drop = FALSE]
  expect_equal(unique(pend$method_id), "MTH_UNSUPPORTED_ANALYSIS")
  expect_equal(unique(pend$output_id), "T_14_2_1")
  # Surfaced on the worklist for the analyst.
  expect_gt(nrow(ars_manual_worklist(ard)), 0)
})

test_that("otherListsOfContents (LOPO) is shaped like siera's exampleARS_*.json", {
  re <- build_ars_json(list(.demo_section("T-14-1-1"),
                            .demo_section("T-14-3-1", "AE Summary")))
  expect_true(is.list(re$otherListsOfContents))
  expect_length(re$otherListsOfContents, 1)
  lopo <- re$otherListsOfContents[[1]]
  expect_equal(lopo$label, "LOPO")
  expect_length(lopo$contentsList$listItems, 2)  # one per Output
  item <- lopo$contentsList$listItems[[1]]
  expect_named(item, c("name", "level", "order", "outputId"),
               ignore.order = TRUE)
  expect_equal(item$outputId, "T_14_1_1")
})

test_that("mainListOfContents (LOPA) carries sublist with analysisIds", {
  re <- build_ars_json(list(.demo_section()))
  lopa <- re$mainListOfContents
  expect_equal(lopa$label, "LOPA")
  items <- lopa$contentsList$listItems
  expect_length(items, 1)
  sub <- items[[1]]$sublist$listItems
  ## We pad sublists to >= 3 entries to work around siera's hard-coded
  ## anas[3, ] access (AnalysisSet.R:141). The first entry must be the
  ## real analysis; duplicates after it are harmless.
  expect_gte(length(sub), 3L)
  expect_equal(sub[[1]]$analysisId, "AN_T_14_1_1_001")
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

test_that("Required siera sections are all present (sanity gate)", {
  ## Mirrors siera::.read_ars_json_metadata() line 45-53
  re <- build_ars_json(list(.demo_section()))
  required <- c("otherListsOfContents", "mainListOfContents",
                "dataSubsets", "analysisSets", "analysisGroupings",
                "analyses", "methods")
  for (s in required) expect_true(s %in% names(re), info = s)
})

test_that("Round-trip through siera::readARS produces non-empty ARD scripts", {
  skip_if_not_installed("siera")

  re   <- build_ars_json(list(.demo_section()))
  json <- tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(re, auto_unbox = TRUE, pretty = TRUE,
                              null = "null"), json)

  out_dir  <- tempfile("ard_scripts_")
  adam_dir <- tempfile("adam_csvs_")
  dir.create(out_dir);  dir.create(adam_dir)

  ## Stub ADaM CSVs siera will look for. Minimal viable.
  utils::write.csv(
    data.frame(USUBJID = c("S1", "S2", "S3"),
               TRT01A  = c("A",  "A",  "B"),
               SAFFL   = c("Y",  "Y",  "Y"),
               AGE     = c(60L,  65L,  70L)),
    file.path(adam_dir, "ADSL.csv"), row.names = FALSE
  )

  ## Capture cli warnings -- siera warns rather than errors on issues.
  res <- withCallingHandlers(
    suppressWarnings(suppressMessages(
      siera::readARS(json, output_path = out_dir, adam_path = adam_dir)
    )),
    error = function(e) e
  )

  scripts <- list.files(out_dir, pattern = "ARD_.*\\.R$", full.names = TRUE)
  expect_gte(length(scripts), 1L)
  if (length(scripts) > 0) {
    content <- paste(readLines(scripts[1]), collapse = "\n")
    expect_gt(nchar(content), 200L)
    expect_match(content, "df3_")   # confirms the method template expanded
  }
})
