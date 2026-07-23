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

test_that("distinct authored rows with different labels are not collapsed", {
  ## Two continuous summary rows on the same variable but distinct labels must
  ## each produce their own analysis -- the widened dedup key keeps them apart.
  sec <- .demo_section()
  sec$stub_rows <- list(
    list(label = "Age (years)", annotation = "ADSL.AGE", has_annot = TRUE,
         detection_method = "pattern", detection_confidence = "high"),
    list(label = "Age (years) - total", annotation = "ADSL.AGE", has_annot = TRUE,
         detection_method = "pattern", detection_confidence = "high")
  )
  sec$enriched_rows <- list(
    list(label = "Age (years)", primary_dataset = "ADSL",
         primary_variable = "AGE", variable_role = "ANALYSIS"),
    list(label = "Age (years) - total", primary_dataset = "ADSL",
         primary_variable = "AGE", variable_role = "ANALYSIS")
  )
  re <- build_ars_json(list(sec))
  expect_length(re$analyses, 2)
  expect_setequal(vapply(re$analyses, function(a) a$label, character(1)),
                  c("Age (years)", "Age (years) - total"))
})

test_that("repeated categorical count rows on one variable still collapse", {
  ## AE-template case: placeholder + example rows all counting the same
  ## variable draw the same distribution -- one analysis, despite the labels.
  sec <- .demo_section(title = "AE by Preferred Term")
  sec$analysis_type   <- "CATEGORICAL"
  sec$ars_method_name <- "Count and Percentage"
  sec$stub_rows <- list(
    list(label = "<Preferred Term>", annotation = "ADAE.AEDECOD", has_annot = TRUE,
         detection_method = "pattern", detection_confidence = "high"),
    list(label = "Preferred Term", annotation = "ADAE.AEDECOD", has_annot = TRUE,
         detection_method = "pattern", detection_confidence = "high")
  )
  sec$enriched_rows <- list(
    list(label = "<Preferred Term>", primary_dataset = "ADAE",
         primary_variable = "AEDECOD", variable_role = "ANALYSIS"),
    list(label = "Preferred Term", primary_dataset = "ADAE",
         primary_variable = "AEDECOD", variable_role = "ANALYSIS")
  )
  re <- build_ars_json(list(sec),
                       spec_lookup = list(ADAE.AEDECOD = list(type = "char")))
  expect_length(re$analyses, 1)
})

test_that("an unparseable population filter is carried as annotationText", {
  sec <- .demo_section()
  sec$population_text  <- "Cohort subset"
  sec$population_annot <- "ADSL.COHORTN in the unknown set"  # not a real clause
  re <- build_ars_json(list(sec))
  as_obj <- re$analysisSets[[1]]
  expect_null(as_obj$condition)
  expect_equal(as_obj$annotationText, "ADSL.COHORTN in the unknown set")
})

test_that("a supplement's differing proposal becomes an ADDITIONAL analysis, not dropped", {
  ## Row already annotated by the shell (regex wins) AND carrying a differing
  ## supplement proposal: the shell analysis stands and the supplement's is
  ## built alongside it -- nothing the supplement contributed is ignored.
  sec <- .demo_section()
  sec$stub_rows <- list(
    list(label = "Disposition", annotation = "ADSL.EOSSTT", has_annot = TRUE,
         detection_method = "pattern", detection_confidence = "high",
         supplement_proposed_annotation = "ADSL.DCSREAS",
         supplement_conflict = TRUE, supplement_conflict_with = "ADSL.EOSSTT")
  )
  sec$enriched_rows <- list(
    list(label = "Disposition", primary_dataset = "ADSL",
         primary_variable = "EOSSTT", variable_role = "ANALYSIS")
  )
  re <- build_ars_json(list(sec),
                       spec_lookup = list(ADSL.EOSSTT = list(type = "char"),
                                          ADSL.DCSREAS = list(type = "char")))
  vars <- vapply(re$analyses, function(a) a$variable, character(1))
  expect_true("EOSSTT"  %in% vars)   ## the shell's own analysis
  expect_true("DCSREAS" %in% vars)   ## the supplement's additional analysis
  expect_length(re$analyses, 2)
})

test_that("a supplement binding matching no stub row is built as a free-standing analysis", {
  ## The shell authored no row for this binding, but it passed the spec gate,
  ## so it must appear as its own analysis on the output rather than vanish.
  sec <- .demo_section()
  sec$supplement_extra_rows <- list(
    list(label = "Time to discontinuation", annotation = "ADSL.EOSSTT")
  )
  re <- build_ars_json(list(sec),
                       spec_lookup = list(ADSL.AGE = list(type = "num"),
                                          ADSL.EOSSTT = list(type = "char")))
  vars   <- vapply(re$analyses, function(a) a$variable, character(1))
  labels <- vapply(re$analyses, function(a) a$label, character(1))
  expect_true("EOSSTT" %in% vars)
  expect_true("Time to discontinuation" %in% labels)
  ## and the output references it
  ref_ids <- unlist(re$outputs[[1]]$referencedAnalysisIds)
  free_id <- re$analyses[[which(labels == "Time to discontinuation")]]$id
  expect_true(free_id %in% ref_ids)
})

## --- supplement v3 typed conditions ----------------------------------------

test_that(".build_group_levels prefers a typed condition over the annotation string", {
  ## A v3 supplement group carries the typed condition directly; when both a
  ## typed condition and a (bogus) annotation are present, the typed one wins.
  cg <- list(variable = "SEX", dataset = "ADSL", groups = list(
    list(label = "Male", order = 1L, annotation = "ADSL.SEX='WRONG'",
         condition = list(condition = list(dataset = "ADSL", variable = "SEX",
                                           comparator = "EQ", value = list("M")))),
    list(label = "Female", order = 2L,
         condition = list(condition = list(dataset = "ADSL", variable = "SEX",
                                           comparator = "EQ", value = list("F"))))))
  gl <- .build_group_levels("SEX", cg)
  expect_length(gl, 2)
  expect_equal(gl[[1]]$condition$condition$value, list("M"))   ## typed, not "WRONG"
  expect_equal(gl[[2]]$condition$condition$value, list("F"))
})

test_that("a supplement per-row methodId overrides the section method for that row", {
  ## MIXED_SUMMARY section (continuous default) with a supplement-bound row
  ## carrying an explicit count method -- the row's method must be that id.
  sec <- .demo_section()
  sec$stub_rows <- list(
    list(label = "Age (years)", annotation = "ADSL.AGE", has_annot = TRUE,
         detection_method = "supplement", detection_confidence = "medium",
         supplement_method_id = "MTH_COUNT_AND_PERCENTAGE")
  )
  sec$enriched_rows <- list(list(label = "Age (years)", primary_dataset = "ADSL",
                                 primary_variable = "AGE", variable_role = "ANALYSIS"))
  re <- build_ars_json(list(sec), spec_lookup = list(ADSL.AGE = list(type = "num")),
                       extraction_mode = "supplement", supplement_trust = "fill_gaps")
  age_an <- Filter(function(a) identical(a$variable, "AGE"), re$analyses)[[1]]
  expect_equal(age_an$methodId, "MTH_COUNT_AND_PERCENTAGE")
})

test_that(".build_data_subset emits a compoundExpression from a compound row filter", {
  er <- list(data_subset_compound = list(compoundExpression = list(
    logicalOperator = "OR",
    whereClauses = list(
      list(condition = list(dataset = "ADAE", variable = "AEDECOD",
                            comparator = "EQ", value = list("HEADACHE"))),
      list(condition = list(dataset = "ADAE", variable = "AEDECOD",
                            comparator = "EQ", value = list("NAUSEA")))))))
  ds <- .build_data_subset(er, "T-14-3-1", 1L)
  expect_false(is.null(ds$compoundExpression))
  expect_null(ds$condition)
  expect_equal(ds$compoundExpression$logicalOperator, "OR")
  expect_length(ds$compoundExpression$whereClauses, 2)
})

test_that(".build_data_subset tolerates a missing-value (empty array) filter", {
  ## v3 supplement encodes an "is missing" test as EQ/NE with value = [].
  er <- list(data_subset = list(
    dataset = "ADSL", variable = "DCSREAS",
    comparator = "NE", value = list()))
  ds <- .build_data_subset(er, "T-14-1-1", 1L)
  expect_false(is.null(ds))
  expect_equal(ds$condition$dataset, "ADSL")
  expect_equal(ds$condition$variable, "DCSREAS")
  expect_length(ds$condition$value, 0)
})

## --- Codelist decodes (spec codelist -> ARS _meta$value_decodes) ------------

## Disposition-style section: a coded categorical parent row with one
## authored level row, grouped by a coded cohort variable.
.cl_section <- function(column_groups = NULL) {
  sec <- list(
    tlf_number       = "T-14-1-1",
    tlf_type         = "TABLE",
    title            = "Subject Disposition",
    population_text  = "Screened Subjects",
    population_annot = "ADSL.SCRNFL='Y'",
    source_datasets  = "ADSL",
    col_headers      = c("", "Cohort A", "Cohort B", "Total"),
    n_data_cols      = 3L,
    stub_rows        = list(
      list(label = "Primary reason for discontinuation",
           annotation = "ADSL.DCSREASN", has_annot = TRUE,
           raw_text = "Primary reason for discontinuation"),
      list(label = "  Death", annotation = "ADSL.DCSREASN=1",
           has_annot = TRUE, raw_text = "  Death")
    ),
    analysis_type    = "CATEGORICAL",
    ars_method_name  = "Count and Percentage",
    by_variable      = "COHORTN",
    by_variable_dataset = "ADSL",
    enriched_rows    = list()
  )
  if (!is.null(column_groups)) sec$column_groups <- column_groups
  sec
}

.cl_codelists <- function() {
  list(
    DCSREAS = list(
      name = "DCSREAS",
      terms = data.frame(term = c("1", "2", "3"),
                         decode = c("DEATH", "LOST TO FOLLOW-UP", "OTHER"),
                         order = 1:3, stringsAsFactors = FALSE),
      used_by = "ADSL.DCSREASN"),
    COHORT = list(
      name = "COHORT",
      terms = data.frame(term = c("1", "2", "99"),
                         decode = c("Cohort A", "Cohort B", "Missing"),
                         order = 1:3, stringsAsFactors = FALSE),
      used_by = "ADSL.COHORTN")
  )
}

.cl_lookup <- function() {
  list(
    "ADSL.DCSREASN" = list(dataset = "ADSL", variable = "DCSREASN",
                           type = "integer", codelist = "DCSREAS"),
    "ADSL.COHORTN"  = list(dataset = "ADSL", variable = "COHORTN",
                           type = "integer", codelist = "COHORT")
  )
}

test_that("value_decodes ships the codelist for a coded categorical row", {
  re <- build_ars_json(list(.cl_section()), spec_lookup = .cl_lookup(),
                       codelists = .cl_codelists())
  vd <- re$`_meta`$value_decodes
  expect_true("ADSL.DCSREASN" %in% names(vd))
  entry <- vd[["ADSL.DCSREASN"]]
  expect_equal(vapply(entry, function(e) e$value, character(1)),
               c("1", "2", "3"))
  expect_equal(vapply(entry, function(e) e$label, character(1)),
               c("DEATH", "LOST TO FOLLOW-UP", "OTHER"))
})

test_that("authored level slots are stamped with the decoded label", {
  re  <- build_ars_json(list(.cl_section()), spec_lookup = .cl_lookup(),
                        codelists = .cl_codelists())
  lay <- re$outputs[[1]]$`_meta`$shell_layout
  lvl <- Filter(function(e) identical(e$kind, "level"), lay)
  expect_length(lvl, 1)
  expect_equal(lvl[[1]]$level, "DEATH")
  expect_equal(lvl[[1]]$level_code, "1")
})

test_that("grouping factor groups fall back to the codelist when headers have none", {
  diag_reset()
  re <- build_ars_json(list(.cl_section()), spec_lookup = .cl_lookup(),
                       codelists = .cl_codelists())
  gf <- re$analysisGroupings[[1]]
  expect_equal(gf$id, "GF_COHORTN")
  expect_length(gf$groups, 3)
  expect_equal(vapply(gf$groups, function(g) g$label, character(1)),
               c("Cohort A", "Cohort B", "Missing"))
  cond1 <- gf$groups[[1]]$condition$condition
  expect_equal(cond1$variable, "COHORTN")
  expect_equal(cond1$comparator, "EQ")
  expect_equal(unlist(cond1$value), "1")
  ## The fallback announces itself for review.
  d <- ars_diagnostics()
  expect_true(any(grepl("derived from the spec codelist", d$problem)))
})

test_that("header-annotated column groups beat the codelist fallback", {
  re <- build_ars_json(list(.cl_section(column_groups = list(
    variable = "COHORTN", dataset = "ADSL",
    groups = list(
      list(label = "Cohort One", annotation = "ADSL.COHORTN=1", order = 1L),
      list(label = "Cohort Two", annotation = "ADSL.COHORTN=2", order = 2L)
    )))), spec_lookup = .cl_lookup(), codelists = .cl_codelists())
  gf <- re$analysisGroupings[[1]]
  expect_length(gf$groups, 2)
  expect_equal(vapply(gf$groups, function(g) g$label, character(1)),
               c("Cohort One", "Cohort Two"))
})

test_that("an oversized codelist is skipped with a diagnostic", {
  diag_reset()
  big <- .cl_codelists()
  big$DCSREAS$terms <- data.frame(
    term   = as.character(1:20),
    decode = paste("Reason", 1:20),
    order  = 1:20, stringsAsFactors = FALSE
  )
  re <- build_ars_json(list(.cl_section()), spec_lookup = .cl_lookup(),
                       codelists = big)
  expect_false("ADSL.DCSREASN" %in% names(re$`_meta`$value_decodes))
  ## Authored level slot keeps its raw coded value -- nothing translated.
  lay <- re$outputs[[1]]$`_meta`$shell_layout
  lvl <- Filter(function(e) identical(e$kind, "level"), lay)
  expect_equal(lvl[[1]]$level, "1")
  d <- ars_diagnostics()
  expect_true(any(grepl("decode skipped", d$problem)))
})

test_that("no codelists means no decodes and unchanged groupings", {
  re <- build_ars_json(list(.cl_section()), spec_lookup = .cl_lookup())
  expect_length(re$`_meta`$value_decodes, 0)
  expect_length(re$analysisGroupings[[1]]$groups, 0)
})
