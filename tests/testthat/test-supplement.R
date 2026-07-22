## Supplement mode (format v3): reading/validating the typed supplement,
## gap-filling analyses (regex wins), enrichment passthrough, and the 3-tier
## mode resolution in spec_to_ars().

.supp_spec <- list(ADSL.AGE = list(), ADSL.SEX = list(), ADSL.SAFFL = list(),
                   ADSL.TRT01A = list(), ADSL.EOSSTT = list())

.mk_supp_section <- function() {
  list(
    tlf_number = "T-14-1-1", tlf_type = "TABLE", title = "Demographics",
    population_annot = "",
    stub_rows = list(
      list(label = "Age (years)", annotation = "", has_annot = FALSE,
           detection_method = NA_character_, detection_confidence = NA_character_,
           raw_text = "Age (years)"),
      list(label = "Sex", annotation = "ADSL.SEX", has_annot = TRUE,
           detection_method = "colour", detection_confidence = "high",
           raw_text = "Sex (ADSL.SEX)"),
      list(label = "Mean (SD)", annotation = "", has_annot = FALSE,
           detection_method = NA_character_, detection_confidence = NA_character_,
           raw_text = "Mean (SD)")
    )
  )
}

.write_supp <- function(x, ...) {
  path <- tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE), path)
  path
}

.supp_minimal <- function(tlfs) {
  list(supplement_version = 3L, tlfs = tlfs)
}

## A typed v3 where-clause condition, and an analysis-variable object.
.wc <- function(dataset, variable, comparator = "EQ", ...) {
  list(condition = list(dataset = dataset, variable = variable,
                        comparator = comparator, value = list(...)))
}
.av <- function(dataset, variable) list(dataset = dataset, variable = variable)

no_llm_keys <- function(code) {
  withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    code
  )
}

## --- read_supplement -------------------------------------------------------

test_that("read_supplement accepts plain, fenced, and smart-quoted JSON", {
  supp <- .supp_minimal(list(`14.1.1` = list(
    title = "Demographics", analysis_type = "CONTINUOUS", is_supported = TRUE,
    analyses = list(list(rowLabel = "Age (years)", variable = .av("ADSL", "AGE"))))))
  path <- .write_supp(supp)
  expect_equal(read_supplement(path)$supplement_version, 3L)

  fenced <- tempfile(fileext = ".json")
  writeLines(c("Here is the supplement:", "```json",
               readLines(path), "```", "Hope this helps!"), fenced)
  expect_equal(
    read_supplement(fenced)$tlfs[[1]]$analyses[[1]]$variable$variable, "AGE")

  smart <- tempfile(fileext = ".json")
  writeLines(gsub('"', "“", readLines(path)), smart)  ## all quotes smart
  expect_equal(read_supplement(smart)$supplement_version, 3L)
})

test_that("read_supplement aborts on malformed JSON, bad version, missing tlfs", {
  bad <- tempfile(fileext = ".json")
  writeLines("{not json", bad)
  expect_error(read_supplement(bad), "not valid JSON")

  ## A v2 file now fails loudly (typed-condition hard cut).
  v2 <- .write_supp(list(supplement_version = 2L,
                         tlfs = list(`14.1.1` = list())))
  expect_error(read_supplement(v2), "version mismatch")

  no_tlfs <- .write_supp(list(supplement_version = 3L))
  expect_error(read_supplement(no_tlfs), "tlfs")
})

test_that("v3 dropped the v2 double-quote repair: a title with = survives untouched", {
  ## In v2 `.clean_supplement_text` rewrote `="..."` to `='...'`; v3 must not,
  ## or it would corrupt a legitimate title. Conditions are typed now.
  ok <- '{"tlfs":{"14.1.1":{"title":"Change = baseline"}}}'
  expect_identical(arsbridge:::.clean_supplement_text(ok), ok)
})

test_that("malformed typed JSON aborts with the typed-condition hint", {
  bad <- tempfile(fileext = ".json")
  writeLines("{ not json,, }", bad)
  expect_error(read_supplement(bad), "not valid JSON")
  expect_error(read_supplement(bad), "typed object")
})

## --- ars_validate_supplement ----------------------------------------------

test_that("validator passes a clean supplement and flags bad fields", {
  clean <- .write_supp(.supp_minimal(list(`14.1.1` = list(
    title = "Demographics", analysis_type = "CONTINUOUS", is_supported = TRUE,
    analyses = list(list(rowLabel = "Age (years)", variable = .av("ADSL", "AGE")))))))
  out <- suppressMessages(ars_validate_supplement(clean))
  expect_equal(nrow(out), 0)

  dirty <- .write_supp(.supp_minimal(list(`14.1.1` = list(
    title = "x", is_supported = TRUE,
    analyses = list(
      list(rowLabel = "", variable = .av("ADSL", "AGE")),      ## empty label
      list(rowLabel = "Sex", variable = list(dataset = "not"))  ## bad variable
    ),
    analysis_type = "REGRESSION",                               ## bad enum
    mystery_field = "x"))))                                     ## unknown field
  out <- suppressMessages(ars_validate_supplement(dirty))
  expect_true(any(out$severity == "FAIL" & grepl("rowLabel", out$problem)))
  expect_true(any(out$severity == "FAIL" & grepl("variable", out$problem)))
  expect_true(any(out$severity == "FAIL" & out$where == "analysis_type"))
  expect_true(any(out$severity == "INFO" & grepl("mystery_field", out$problem)))
})

test_that("validator attaches a paste-ready repair prompt when there are FAILs", {
  dirty <- .write_supp(.supp_minimal(list(`14.1.1` = list(
    title = "x", analysis_type = "CATEGORICAL", is_supported = TRUE,
    analyses = list(list(rowLabel = "", variable = .av("ADSL", "AGE")))))))
  out <- suppressMessages(ars_validate_supplement(dirty))
  rp <- attr(out, "repair_prompt")
  expect_true(is.character(rp) && nzchar(rp))
  expect_match(rp, "Fix ONLY the following")
})

test_that("validator applies the spec gate when a spec path is given", {
  supp <- .write_supp(.supp_minimal(list(`14.1.1` = list(
    title = "x", analysis_type = "CONTINUOUS", is_supported = TRUE,
    analyses = list(list(rowLabel = "Weight", variable = .av("ADSL", "WEIGHTBL")))))))
  out <- suppressMessages(ars_validate_supplement(
    supp, adam_spec_path = test_path("fixtures/adam_spec_minimal.xlsx")))
  expect_true(any(out$severity == "FAIL" &
                    grepl("ADSL.WEIGHTBL", out$problem) &
                    grepl("not in the ADaM spec", out$problem)))
})

test_that("validator checks typed groupings and flags a single-group axis", {
  ok <- .write_supp(.supp_minimal(list(`14.1.2` = list(
    title = "Key Protocol Deviations", analysis_type = "CATEGORICAL",
    is_supported = TRUE,
    groupings = list(list(
      groupingDataset = "ADSL", groupingVariable = "COHORTN", dataDriven = FALSE,
      groups = list(
        list(label = "Cohort A", order = 1L, condition = .wc("ADSL", "COHORTN", "EQ", "1")$condition),
        list(label = "Unknown", order = 2L, condition = .wc("ADSL", "COHORTN", "EQ")$condition))))))))
  out <- suppressMessages(ars_validate_supplement(ok))
  expect_false(any(grepl("groupings", out$where) & out$severity == "FAIL"))

  ## A single condition-defined group is not an axis -> WARN.
  lone <- .write_supp(.supp_minimal(list(`14.1.2` = list(
    title = "x", analysis_type = "CATEGORICAL", is_supported = TRUE,
    groupings = list(list(
      groupingDataset = "ADSL", groupingVariable = "COHORTN", dataDriven = FALSE,
      groups = list(
        list(label = "Only", order = 1L, condition = .wc("ADSL", "COHORTN", "EQ", "1")$condition))))))))
  out <- suppressMessages(ars_validate_supplement(lone))
  expect_true(any(out$severity == "WARN" & grepl("groupings", out$where)))
})

test_that("a self-referential parentRowLabel FAILs; a stub-row parent is only advisory", {
  ## Points at its own row -> FAIL.
  bad <- .write_supp(.supp_minimal(list(`14.1.1` = list(
    title = "x", analysis_type = "CATEGORICAL", is_supported = TRUE,
    analyses = list(
      list(rowLabel = "Headache", variable = .av("ADAE", "AEDECOD"),
           parentRowLabel = "Headache"))))))
  out <- suppressMessages(ars_validate_supplement(bad))
  expect_true(any(out$severity == "FAIL" & grepl("parentRowLabel", out$problem)))

  ## Points at a shell stub the validator cannot see -> INFO, never FAIL.
  ok <- .write_supp(.supp_minimal(list(`14.1.1` = list(
    title = "x", analysis_type = "CATEGORICAL_HIERARCHICAL", is_supported = TRUE,
    analyses = list(
      list(rowLabel = "Headache", variable = .av("ADAE", "AEDECOD"),
           parentRowLabel = "Nervous system disorders"))))))
  out <- suppressMessages(ars_validate_supplement(ok))
  expect_false(any(out$severity == "FAIL" & grepl("parentRowLabel", out$problem)))
  expect_true(any(out$severity == "INFO" & grepl("parentRowLabel", out$problem)))
})

## --- .apply_supplement_bindings --------------------------------------------

test_that("supplement fills only unannotated rows; regex wins conflicts", {
  diag_reset()
  supp_tlf <- list(analyses = list(
    list(rowLabel = "Age (years)", variable = .av("ADSL", "AGE")),   ## gap -> filled
    list(rowLabel = "Sex", variable = .av("ADSL", "EOSSTT"))         ## conflict -> kept
  ))
  sec <- .apply_supplement_bindings(.mk_supp_section(), supp_tlf, .supp_spec)

  expect_true(sec$stub_rows[[1]]$has_annot)
  expect_equal(sec$stub_rows[[1]]$annotation, "ADSL.AGE")
  expect_equal(sec$stub_rows[[1]]$detection_method, "supplement")
  expect_equal(sec$stub_rows[[1]]$detection_confidence, "low")

  ## Conflict: the shell's ADSL.SEX stands, a WARN names both variables.
  expect_equal(sec$stub_rows[[2]]$annotation, "ADSL.SEX")
  expect_equal(sec$stub_rows[[2]]$detection_method, "colour")
  expect_true(isTRUE(sec$stub_rows[[2]]$supplement_conflict))
  expect_equal(sec$stub_rows[[2]]$supplement_proposed_annotation, "ADSL.EOSSTT")
  expect_equal(sec$stub_rows[[2]]$supplement_conflict_with, "ADSL.SEX")
  recs <- diag_records()
  expect_true(any(recs$stage == "supplement" & recs$severity == "WARN" &
                    grepl("ADSL.EOSSTT", recs$problem) &
                    grepl("ADSL.SEX", recs$problem)))
})

test_that("prefer_supplement overrides a row conflict and keeps the shell as secondary", {
  diag_reset()
  supp_tlf <- list(analyses = list(
    list(rowLabel = "Sex", variable = .av("ADSL", "EOSSTT"))   ## conflicts with shell ADSL.SEX
  ))
  sec <- .apply_supplement_bindings(.mk_supp_section(), supp_tlf, .supp_spec,
                                    trust = "prefer_supplement")
  ## Supplement value now wins the row; the shell's original is kept as secondary.
  expect_equal(sec$stub_rows[[2]]$annotation, "ADSL.EOSSTT")
  expect_equal(sec$stub_rows[[2]]$detection_method, "supplement")
  expect_equal(sec$stub_rows[[2]]$secondary_annotation, "ADSL.SEX")
  expect_equal(sec$stub_rows[[2]]$shell_overridden_annotation, "ADSL.SEX")
  recs <- diag_records()
  expect_true(any(recs$severity == "WARN" & grepl("overrides the shell", recs$problem)))
})

test_that("prefer_supplement never bypasses the spec gate", {
  diag_reset()
  supp_tlf <- list(analyses = list(
    list(rowLabel = "Sex", variable = .av("ADSL", "FAKEVAR"))
  ))
  sec <- .apply_supplement_bindings(.mk_supp_section(), supp_tlf, .supp_spec,
                                    trust = "prefer_supplement")
  ## The hallucination is rejected in prefer mode too; the shell value stands.
  expect_equal(sec$stub_rows[[2]]$annotation, "ADSL.SEX")
  recs <- diag_records()
  expect_true(any(recs$severity == "FAIL" & grepl("ADSL.FAKEVAR", recs$problem)))
})

test_that("HIGH confidence maps to medium, and a typed whereClause is stored", {
  diag_reset()
  supp_tlf <- list(analyses = list(
    list(rowLabel = "Age (years)", variable = .av("ADSL", "EOSSTT"),
         whereClause = .wc("ADSL", "EOSSTT", "EQ", "COMPLETED"),
         confidence = "HIGH")))
  sec <- .apply_supplement_bindings(.mk_supp_section(), supp_tlf, .supp_spec)
  expect_equal(sec$stub_rows[[1]]$detection_confidence, "medium")
  expect_equal(sec$stub_rows[[1]]$annotation,
               "ADSL.EOSSTT WHERE ADSL.EOSSTT='COMPLETED'")
  ## The typed clause is stored authoritatively, never re-parsed from a string.
  expect_equal(sec$stub_rows[[1]]$supplement_where$condition$variable, "EOSSTT")
  expect_equal(sec$stub_rows[[1]]$supplement_where$condition$value, list("COMPLETED"))
})

test_that("statline rows are skipped, unmatched labels kept, spec gate rejects", {
  diag_reset()
  supp_tlf <- list(analyses = list(
    list(rowLabel = "Mean (SD)", variable = .av("ADSL", "AGE")),     ## statline -> skip
    list(rowLabel = "No Such Row", variable = .av("ADSL", "AGE")),   ## unmatched
    list(rowLabel = "Age (years)", variable = .av("ADSL", "FAKEVAR")) ## out of spec
  ))
  sec <- .apply_supplement_bindings(.mk_supp_section(), supp_tlf, .supp_spec)

  expect_false(sec$stub_rows[[3]]$has_annot)   ## statline untouched

  extra_vars <- vapply(sec$supplement_extra_rows %||% list(),
                       function(e) e$annotation, character(1))
  expect_true("ADSL.AGE" %in% extra_vars)          ## unmatched binding kept
  expect_false(any(grepl("FAKEVAR", extra_vars)))  ## hallucination not kept

  recs <- diag_records()
  expect_true(any(recs$severity == "INFO" & grepl("statistic sub-row", recs$problem)))
  expect_true(any(recs$severity == "WARN" & grepl("No Such Row", recs$problem)))
  expect_true(any(recs$severity == "FAIL" & grepl("ADSL.FAKEVAR", recs$problem)))
})

test_that("an invalid whereClause drops the whole analysis with a FAIL", {
  diag_reset()
  supp_tlf <- list(analyses = list(
    list(rowLabel = "Age (years)", variable = .av("ADSL", "AGE"),
         whereClause = list(condition = list(dataset = "ADSL", variable = "AGE",
                                             comparator = "LIKE", value = list("x"))))))
  sec <- .apply_supplement_bindings(.mk_supp_section(), supp_tlf, .supp_spec)
  expect_false(sec$stub_rows[[1]]$has_annot)   ## not filled
  recs <- diag_records()
  expect_true(any(recs$severity == "FAIL" & grepl("whereClause", recs$problem)))
})

test_that("analysisSet population and groupings apply only when the shell is empty", {
  sec <- .mk_supp_section()
  supp_tlf <- list(
    analysisSet = list(label = "Safety Population", condition = .wc("ADSL", "SAFFL", "EQ", "Y")$condition),
    groupings = list(list(groupingDataset = "ADSL", groupingVariable = "TRT01A",
                          dataDriven = TRUE)))
  out <- .apply_supplement_bindings(sec, supp_tlf, .supp_spec)
  expect_equal(out$column_annotation, "ADSL.TRT01A")
  expect_equal(out$population_annot, "ADSL.SAFFL='Y'")
  expect_equal(out$population_where$condition$variable, "SAFFL")
  expect_equal(out$population_text, "Safety Population")

  sec$column_annotation <- "ADSL.TRT01A"      ## already set by the shell
  sec$population_annot  <- "ADSL.SAFFL='Y'"
  out <- .apply_supplement_bindings(sec, list(
    analysisSet = list(condition = .wc("ADSL", "AGE", "EQ", "X")$condition),
    groupings = list(list(groupingDataset = "ADSL", groupingVariable = "SEX",
                          dataDriven = TRUE))), .supp_spec)
  expect_equal(out$column_annotation, "ADSL.TRT01A")
  expect_equal(out$population_annot, "ADSL.SAFFL='Y'")
})

test_that("a per-row methodId is stored (catalogue only) and consumed for the row", {
  supp_tlf <- list(analyses = list(
    list(rowLabel = "Age (years)", variable = .av("ADSL", "AGE"),
         methodId = "MTH_COUNT_AND_PERCENTAGE"),
    list(rowLabel = "Sex", variable = .av("ADSL", "SEX"),
         methodId = "MTH_NONSENSE")))            ## shell row -> conflict path
  sec <- .apply_supplement_bindings(.mk_supp_section(), supp_tlf, .supp_spec)
  expect_equal(sec$stub_rows[[1]]$supplement_method_id, "MTH_COUNT_AND_PERCENTAGE")
})

test_that("listingColumns bind like analyses through the same channel", {
  sec <- .mk_supp_section()
  supp_tlf <- list(listingColumns = list(
    list(label = "Age (years)", variable = .av("ADSL", "AGE"))))
  out <- .apply_supplement_bindings(sec, supp_tlf, .supp_spec)
  expect_true(out$stub_rows[[1]]$has_annot)
  expect_equal(out$stub_rows[[1]]$annotation, "ADSL.AGE")
  expect_equal(out$stub_rows[[1]]$detection_method, "supplement")
})

test_that("record filter, sorting and provenance are recorded (not computed) with an INFO", {
  diag_reset()
  sec <- .mk_supp_section()
  supp_tlf <- list(
    recordFilter = .wc("ADSL", "SAFFL", "EQ", "Y"),
    sorting = list(list(dataset = "ADSL", variable = "AGE", direction = "ASC", order = 1L)),
    provenance = list(blueprintStatus = "READY_FOR_PHASE_2"))
  out <- .apply_supplement_bindings(sec, supp_tlf, .supp_spec)
  expect_false(is.null(out$supplement_extras$recordFilter))
  expect_equal(out$supplement_extras$recordFilter$condition$variable, "SAFFL")
  expect_false(is.null(out$supplement_extras$sorting))
  expect_false(is.null(out$supplement_extras$provenance))
  recs <- diag_records()
  expect_true(any(recs$severity == "INFO" & grepl("Recorded but not yet computed", recs$problem)))
})

test_that("an anchor mismatch raises a WARN", {
  diag_reset()
  sec <- .mk_supp_section()   ## first stub label is "Age (years)", 3 rows
  supp_tlf <- list(anchors = list(firstRowLabel = "Something Else", rowCount = 99L))
  .apply_supplement_bindings(sec, supp_tlf, .supp_spec)
  recs <- diag_records()
  expect_true(any(recs$severity == "WARN" & grepl("Anchor mismatch", recs$problem)))
})

## --- supplement typed groupings -> column groups ---------------------------

.cg_supp_spec <- c(.supp_spec, list(ADSL.COHORTN = list(), ADDV.DVCAT = list()))

.cohort_grouping <- function(...) {
  list(list(groupingDataset = "ADSL", groupingVariable = "COHORTN",
            dataDriven = FALSE, groups = list(...)))
}

test_that("supplement groupings build the per-column axis when the shell has none", {
  diag_reset()
  sec <- .mk_supp_section()
  sec$by_variable <- "COHORTN"; sec$by_variable_dataset <- "ADSL"
  supp_tlf <- list(groupings = .cohort_grouping(
    list(label = "Cohort A", order = 1L, condition = .wc("ADSL", "COHORTN", "EQ", "1")$condition),
    list(label = "Cohort B", order = 2L, condition = .wc("ADSL", "COHORTN", "EQ", "2")$condition),
    list(label = "Unknown Cohort", order = 3L, condition = .wc("ADSL", "COHORTN", "EQ")$condition)))
  out <- .apply_supplement_bindings(sec, supp_tlf, .cg_supp_spec)

  cg <- out$column_groups
  expect_equal(cg$variable, "COHORTN")
  expect_equal(cg$dataset, "ADSL")
  expect_equal(vapply(cg$groups, `[[`, character(1), "label"),
               c("Cohort A", "Cohort B", "Unknown Cohort"))
  expect_equal(out$column_annotation, "ADSL.COHORTN")

  ## Downstream: the builder emits three groups[] straight from the typed
  ## conditions (no string re-parse), the Unknown one carrying the empty-value EQ.
  gf <- .build_grouping(out)
  expect_length(gf$groups, 3)
  expect_equal(unlist(gf$groups[[1]]$condition$condition$value), "1")
  expect_equal(gf$groups[[1]]$condition$condition$comparator, "EQ")
  expect_length(gf$groups[[3]]$condition$condition$value, 0)

  recs <- diag_records()
  expect_true(any(recs$severity == "INFO" &
                    grepl("column-group condition", recs$problem)))
})

test_that("shell-derived column groups are not overwritten by the supplement", {
  sec <- .mk_supp_section()
  sec$column_groups <- list(
    variable = "COHORTN", dataset = "ADSL",
    groups = list(list(label = "Shell A", annotation = "ADSL.COHORTN=1",
                       order = 1L)))
  supp_tlf <- list(groupings = .cohort_grouping(
    list(label = "Supp A", order = 1L, condition = .wc("ADSL", "COHORTN", "EQ", "1")$condition),
    list(label = "Supp B", order = 2L, condition = .wc("ADSL", "COHORTN", "EQ", "2")$condition)))
  out <- .apply_supplement_bindings(sec, supp_tlf, .cg_supp_spec)
  expect_length(out$column_groups$groups, 1)
  expect_equal(out$column_groups$groups[[1]]$label, "Shell A")
})

test_that("an out-of-spec group condition is dropped with a FAIL; valid ones stay", {
  diag_reset()
  sec <- .mk_supp_section()
  sec$by_variable <- "COHORTN"
  supp_tlf <- list(groupings = .cohort_grouping(
    list(label = "Cohort A", order = 1L, condition = .wc("ADSL", "COHORTN", "EQ", "1")$condition),
    list(label = "Cohort B", order = 2L, condition = .wc("ADSL", "COHORTN", "EQ", "2")$condition),
    list(label = "Bad", order = 3L, condition = .wc("ADSL", "FAKEVAR", "EQ", "9")$condition)))
  out <- .apply_supplement_bindings(sec, supp_tlf, .cg_supp_spec)

  expect_length(out$column_groups$groups, 2)   ## bad dropped, two valid kept
  recs <- diag_records()
  expect_true(any(recs$severity == "FAIL" & grepl("ADSL.FAKEVAR", recs$problem)))
})

## --- enrichment passthrough -------------------------------------------------

test_that(".supplement_enrich_answer maps v3 fields into the live-answer shape", {
  ans <- .supplement_enrich_answer(list(
    analysis_type = "MIXED_SUMMARY",
    methodId = "MTH_SUMMARY_STATISTICS_CONTINUOUS",
    groupings = list(list(groupingDataset = "ADSL", groupingVariable = "TRT01A",
                          dataDriven = TRUE)),
    includeTotal = TRUE,
    is_supported = FALSE, unsupported_reason = "needs ANCOVA"))
  expect_equal(ans$analysis_type, "CONTINUOUS")               ## folded via .V3_TYPE_MAP
  expect_equal(ans$ars_method_name, "Summary Statistics - Continuous")
  expect_equal(ans$by_variables, list("ADSL.TRT01A"))
  expect_true(ans$include_total)
  expect_false(ans$is_supported)
  expect_equal(ans$unsupported_reason, "needs ANCOVA")

  expect_equal(.supplement_enrich_answer(NULL), list())
  ## CATEGORICAL_HIERARCHICAL folds to CATEGORICAL; MODEL_BASED to OTHER.
  expect_equal(.supplement_enrich_answer(
    list(analysis_type = "CATEGORICAL_HIERARCHICAL"))$analysis_type, "CATEGORICAL")
  expect_equal(.supplement_enrich_answer(
    list(analysis_type = "MODEL_BASED"))$analysis_type, "OTHER")
})

test_that("enrich_with_llm consumes a supplement answer without any key", {
  diag_reset()
  sec <- .mk_supp_section()
  out <- no_llm_keys(enrich_with_llm(
    sec, spec_lookup = .supp_spec,
    courier_answers = list(analysis_type = "CATEGORICAL",
                           by_variables = list("TRT01A"),
                           include_total = TRUE)))
  expect_equal(out$analysis_type, "CATEGORICAL")
  expect_equal(out$by_variable, "TRT01A")
  expect_true(out$include_total)
})

test_that("enrich_with_llm offline mode runs heuristics quietly, no fail bump", {
  diag_reset()
  out <- no_llm_keys(enrich_with_llm(.mk_supp_section(),
                                     spec_lookup = .supp_spec, offline = TRUE))
  expect_equal(out$analysis_type, "CATEGORICAL")
  expect_equal(.diag_llm_fail_count(), 0)
})

test_that("missing supplement entry for a TLF -> heuristics plus one WARN", {
  diag_reset()
  out <- no_llm_keys(enrich_with_llm(.mk_supp_section(),
                                     spec_lookup = .supp_spec,
                                     courier_answers = list()))
  expect_true(nzchar(out$analysis_type))
  recs <- diag_records()
  expect_true(any(recs$severity == "WARN" & grepl("no entry for this TLF", recs$problem)))
  expect_equal(.diag_llm_fail_count(), 0)
})

## --- TLF key matching --------------------------------------------------------

test_that("supplement keys match the parser's TLF ids across spellings", {
  supp <- list(tlfs = list(`14.1.1` = list(a = 1),
                           `Table 14.3.1` = list(a = 2)))
  expect_equal(.match_supplement_tlf(supp, "T-14-1-1")$a, 1)
  expect_equal(.match_supplement_tlf(supp, "T-14-3-1")$a, 2)
  expect_null(.match_supplement_tlf(supp, "T-99-9-9"))
  expect_equal(.supplement_unmatched_tlfs(supp, c("T-14-1-1")), "Table 14.3.1")
})

## --- ars_copilot_instructions ------------------------------------------------

test_that("ars_copilot_instructions writes the file and respects overwrite", {
  dir <- withr::local_tempdir()
  path <- suppressMessages(ars_copilot_instructions(dir, open = FALSE))
  expect_true(file.exists(path[[1]]))
  txt <- paste(readLines(path[[1]], warn = FALSE), collapse = "\n")
  expect_match(txt, "supplement_version", fixed = TRUE)

  ## Second call keeps the existing copy (no error, same path).
  path2 <- suppressMessages(ars_copilot_instructions(dir, open = FALSE))
  expect_equal(path[[1]], path2[[1]])
})

test_that("ars_copilot_instructions creates a not-yet-existing dir", {
  base <- withr::local_tempdir()
  sub  <- file.path(base, "nested", "copilot_out")
  expect_false(dir.exists(sub))
  path <- suppressMessages(ars_copilot_instructions(sub, open = FALSE))
  expect_true(dir.exists(sub))
  expect_true(file.exists(path[[1]]))
})

test_that("all copilot resources ship with the package", {
  for (f in c("arsbridge_copilot_instructions.md",
              "arsbridge_phase1_blueprint_instructions.md",
              "arsbridge_phase2_build_instructions.md")) {
    p <- system.file("copilot", f, package = "arsbridge")
    expect_true(nzchar(p) && file.exists(p))
  }
  s <- system.file("schema", "arsbridge_supplement_v3.schema.json",
                   package = "arsbridge")
  expect_true(nzchar(s) && file.exists(s))
})

test_that("every instruction file carries a 'How to run this' block with a fenced prompt", {
  for (f in c("arsbridge_copilot_instructions.md",
              "arsbridge_phase1_blueprint_instructions.md",
              "arsbridge_phase2_build_instructions.md")) {
    p <- system.file("copilot", f, package = "arsbridge")
    skip_if_not(nzchar(p) && file.exists(p))
    txt <- paste(readLines(p, warn = FALSE), collapse = "\n")
    expect_match(txt, "## How to run this", fixed = TRUE)
    expect_match(txt, "```text", fixed = TRUE)
    expect_match(txt, "Prompt to paste:", fixed = TRUE)
  }
})

test_that("ars_copilot_instructions writes the single-file set (instructions + schema)", {
  dir <- withr::local_tempdir()
  paths <- suppressMessages(ars_copilot_instructions(dir, open = FALSE))
  expect_length(paths, 2)
  expect_true(file.exists(file.path(dir, "arsbridge_copilot_instructions.md")))
  expect_true(file.exists(file.path(dir, "arsbridge_supplement_v3.schema.json")))
})

test_that("ars_copilot_instructions(workflow = 'two_phase') writes both phases + schema", {
  dir <- withr::local_tempdir()
  paths <- suppressMessages(
    ars_copilot_instructions(dir, workflow = "two_phase", open = FALSE))
  expect_length(paths, 3)
  expect_true(file.exists(file.path(dir, "arsbridge_phase1_blueprint_instructions.md")))
  expect_true(file.exists(file.path(dir, "arsbridge_phase2_build_instructions.md")))
  expect_true(file.exists(file.path(dir, "arsbridge_supplement_v3.schema.json")))
})

## --- spec_to_ars 3-tier integration (fully offline) --------------------------

test_that("keyless spec_to_ars runs deterministically with NO key-related error/warning", {
  out_json <- tempfile(fileext = ".json")
  res <- suppressMessages(no_llm_keys(spec_to_ars(
    shell_path     = test_path("fixtures/annotated_shell_2tlf_minimal.docx"),
    adam_spec_path = test_path("fixtures/adam_spec_minimal.xlsx"),
    output_path    = out_json,
    report_path    = tempfile(fileext = ".xlsx"),
    verbose        = FALSE
  )))
  expect_equal(res$extraction_mode, "deterministic")
  expect_true(file.exists(res$ars_path))
  expect_equal(res$reporting_event$`_meta`$extraction_mode, "deterministic")

  d <- res$diagnostics
  setup <- d[d$stage == "setup", ]
  expect_equal(nrow(setup), 1)
  expect_equal(setup$severity, "INFO")
  expect_match(setup$problem, "deterministic mode")

  key_noise <- d[d$severity %in% c("WARN", "FAIL") &
                   grepl("API key|LLM key|no LLM key|provider not|set_.*_key",
                         d$problem, ignore.case = TRUE), ]
  expect_equal(nrow(key_noise), 0)
})

test_that("spec_to_ars(supplement=) binds gaps and records the mode", {
  supp_path <- .write_supp(.supp_minimal(list(
    `14.1.1` = list(
      title = "Demographics", analysis_type = "CATEGORICAL", is_supported = TRUE,
      analyses = list(
        list(rowLabel = "Male",   variable = .av("ADSL", "SEX"),
             whereClause = .wc("ADSL", "SEX", "EQ", "M")),
        list(rowLabel = "Female", variable = .av("ADSL", "SEX"),
             whereClause = .wc("ADSL", "SEX", "EQ", "F"))
      ),
      groupings = list(list(groupingDataset = "ADSL", groupingVariable = "TRT01A",
                            dataDriven = TRUE)),
      includeTotal = TRUE
    ),
    `99.9.9` = list(title = "x", analysis_type = "OTHER", is_supported = TRUE)  ## typo key -> WARN
  )))
  out_json <- tempfile(fileext = ".json")
  res <- suppressMessages(no_llm_keys(spec_to_ars(
    shell_path     = test_path("fixtures/annotated_shell_2tlf_minimal.docx"),
    adam_spec_path = test_path("fixtures/adam_spec_minimal.xlsx"),
    supplement     = supp_path,
    output_path    = out_json,
    report_path    = tempfile(fileext = ".xlsx"),
    verbose        = FALSE
  )))
  expect_equal(res$extraction_mode, "supplement")
  expect_equal(res$reporting_event$`_meta`$extraction_mode, "supplement")

  recs <- res$diagnostics
  expect_true(any(recs$stage == "supplement" &
                    grepl("99.9.9", recs$problem, fixed = TRUE)))
  expect_true(any(recs$stage == "supplement" & grepl("applied", recs$problem)))
  expect_false(any(recs$stage == "setup"))
})

test_that("spec_to_ars records supplement_trust and keeps both sides of an override", {
  ## prefer_supplement: the supplement overrides the shell's ADSL.AGE on the
  ## Age row (ADSL.RACE is in-spec but differs), keeping the shell as secondary.
  supp_path <- .write_supp(.supp_minimal(list(
    `14.1.1` = list(
      title = "Demographics", analysis_type = "CONTINUOUS", is_supported = TRUE,
      analyses = list(list(rowLabel = "Age (years)", variable = .av("ADSL", "RACE")))
    ))))
  out_json <- tempfile(fileext = ".json")
  res <- suppressMessages(no_llm_keys(spec_to_ars(
    shell_path       = test_path("fixtures/annotated_shell_2tlf_minimal.docx"),
    adam_spec_path   = test_path("fixtures/adam_spec_minimal.xlsx"),
    supplement       = supp_path,
    supplement_trust = "prefer_supplement",
    output_path      = out_json,
    report_path      = tempfile(fileext = ".xlsx"),
    verbose          = FALSE
  )))
  expect_equal(res$reporting_event$`_meta`$supplement_trust, "prefer_supplement")
  recs <- res$diagnostics
  expect_true(any(recs$stage == "supplement" & recs$severity == "WARN" &
                    grepl("overrides the shell", recs$problem)))
})

test_that("supplement_trust set without a supplement warns and is ignored", {
  expect_warning(
    suppressMessages(no_llm_keys(spec_to_ars(
      shell_path       = test_path("fixtures/annotated_shell_2tlf_minimal.docx"),
      adam_spec_path   = test_path("fixtures/adam_spec_minimal.xlsx"),
      supplement_trust = "prefer_supplement",
      output_path      = tempfile(fileext = ".json"),
      report_path      = tempfile(fileext = ".xlsx"),
      verbose          = FALSE
    ))),
    "has no effect without"
  )
})

test_that("spec_to_ars emits populated groups[] from typed supplement groupings", {
  ## End-to-end: a supplement grouping on a value-conditioned axis (SEX, which
  ## the shell does not carry as a machine-readable header filter) must reach
  ## the ARS JSON as a grouping factor with per-column groups[] -- built
  ## straight from the typed conditions with no string re-parse.
  supp_path <- .write_supp(.supp_minimal(list(
    `14.1.1` = list(
      title = "Demographics", analysis_type = "CONTINUOUS", is_supported = TRUE,
      analyses = list(list(rowLabel = "Age (years)", variable = .av("ADSL", "AGE"))),
      groupings = list(list(
        groupingDataset = "ADSL", groupingVariable = "SEX", dataDriven = FALSE,
        groups = list(
          list(label = "Male",   order = 1L, condition = .wc("ADSL", "SEX", "EQ", "M")$condition),
          list(label = "Female", order = 2L, condition = .wc("ADSL", "SEX", "EQ", "F")$condition))))
    ))))
  out_json <- tempfile(fileext = ".json")
  suppressMessages(no_llm_keys(spec_to_ars(
    shell_path     = test_path("fixtures/annotated_shell_2tlf_minimal.docx"),
    adam_spec_path = test_path("fixtures/adam_spec_minimal.xlsx"),
    supplement     = supp_path,
    output_path    = out_json,
    report_path    = tempfile(fileext = ".xlsx"),
    verbose        = FALSE
  )))

  ars <- jsonlite::fromJSON(out_json, simplifyVector = FALSE)
  sex_gf <- Filter(function(g)
    identical(toupper(g$groupingVariable %||% ""), "SEX"),
    ars$analysisGroupings)
  expect_length(sex_gf, 1)
  groups <- sex_gf[[1]]$groups
  expect_length(groups, 2)
  expect_setequal(vapply(groups, function(g) g$label, character(1)),
                  c("Male", "Female"))
  expect_equal(groups[[1]]$condition$condition$variable, "SEX")
})

test_that("a compound whereClause reaches the ARS JSON as a compoundExpression subset", {
  ## Bind an unannotated shell row (Headache) with a compound filter so it
  ## flows through the typed main-row path -> .build_data_subset compound branch.
  supp_path <- .write_supp(.supp_minimal(list(
    `14.3.1` = list(
      title = "Adverse Events", analysis_type = "AE_FREQUENCY", is_supported = TRUE,
      analyses = list(list(
        rowLabel = "Headache", variable = .av("ADAE", "AEDECOD"),
        whereClause = list(compoundExpression = list(
          logicalOperator = "OR",
          whereClauses = list(
            .wc("ADAE", "AEDECOD", "EQ", "HEADACHE"),
            .wc("ADAE", "AEDECOD", "EQ", "NAUSEA")))))))
    )))
  out_json <- tempfile(fileext = ".json")
  suppressMessages(no_llm_keys(spec_to_ars(
    shell_path     = test_path("fixtures/annotated_shell_2tlf_minimal.docx"),
    adam_spec_path = test_path("fixtures/adam_spec_minimal.xlsx"),
    supplement     = supp_path,
    output_path    = out_json,
    report_path    = tempfile(fileext = ".xlsx"),
    verbose        = FALSE
  )))

  ars <- jsonlite::fromJSON(out_json, simplifyVector = FALSE)
  has_compound <- any(vapply(ars$dataSubsets %||% list(),
    function(d) !is.null(d$compoundExpression), logical(1)))
  expect_true(has_compound)
})

## --- title: known field, fill, and the table-set cross-check ---------------

test_that("a 'title' field is recognised (not flagged as unknown)", {
  path <- .write_supp(.supp_minimal(list(`14.1.1` = list(
    title = "Summary of Disposition", analysis_type = "CONTINUOUS", is_supported = TRUE,
    analyses = list(list(rowLabel = "Age (years)", variable = .av("ADSL", "AGE")))
  ))))
  out <- suppressMessages(ars_validate_supplement(path))
  expect_false(any(out$where == "fields" & grepl("title", out$problem)))
})

test_that("ars_validate_supplement suggests adding a missing title (INFO)", {
  path <- .write_supp(.supp_minimal(list(`14.1.1` = list(
    analysis_type = "CONTINUOUS", is_supported = TRUE,
    analyses = list(list(rowLabel = "Age (years)", variable = .av("ADSL", "AGE")))
  ))))
  out <- suppressMessages(ars_validate_supplement(path))
  expect_true(any(out$severity == "INFO" & out$where == "title"))
})

test_that("an empty parsed title is filled from the supplement, with an INFO", {
  diag_reset()
  sec <- .mk_supp_section()
  sec$title <- ""                                   # shell gave no title
  out <- .apply_supplement_bindings(
    sec, list(title = "Summary of Disposition"), .supp_spec)
  expect_equal(out$title, "Summary of Disposition")
  recs <- diag_records()
  expect_true(any(recs$severity == "INFO" & grepl("title sourced from the supplement", recs$problem)))
})

test_that("a parsed title is never overwritten by the supplement", {
  sec <- .mk_supp_section()                          # title = "Demographics"
  out <- .apply_supplement_bindings(
    sec, list(title = "Something Else Entirely"), .supp_spec)
  expect_equal(out$title, "Demographics")
})

test_that(".titles_agree tolerates trimming but flags real differences", {
  expect_true(.titles_agree("Summary of Disposition",
                            "Summary of Disposition - Safety Population"))
  expect_true(.titles_agree("Summary of Disposition", "summary of disposition"))
  expect_false(.titles_agree("Summary of Disposition", "Baseline Characteristics"))
  expect_true(.titles_agree("", "anything"))         # nothing to compare
})

test_that(".supplement_crosscheck flags extra, missing, and mismatched-title TLFs", {
  sections <- list(
    list(tlf_number = "T-14-1-1", title = "Summary of Disposition"),
    list(tlf_number = "T-14-1-2", title = "Demographics"),
    list(tlf_number = "T-14-1-3", title = "Adverse Events")
  )
  supp <- .supp_minimal(list(
    `14.1.1` = list(title = "Summary of Disposition"),        # agrees
    `14.1.2` = list(title = "Baseline Characteristics"),      # title mismatch
    `14.9.9` = list(title = "Ghost")                          # extra
  ))
  f <- .supplement_crosscheck(supp, sections)
  probs <- vapply(f, function(x) x$problem, character(1))
  expect_true(all(vapply(f, function(x) x$severity, character(1)) == "WARN"))
  expect_true(any(grepl("14.9.9", probs) & grepl("no TLF parsed", probs)))
  expect_true(any(grepl("T-14-1-3", probs) & grepl("no entry", probs)))
  expect_true(any(grepl("T-14-1-2", probs) & grepl("supplement says", probs)))
  expect_false(any(grepl("T-14-1-1", probs)))
})

test_that(".supplement_crosscheck is silent when titles are absent and coverage is complete", {
  sections <- list(
    list(tlf_number = "T-14-1-1", title = "Demographics"),
    list(tlf_number = "T-14-1-2", title = "Adverse Events")
  )
  supp <- .supp_minimal(list(`14.1.1` = list(), `14.1.2` = list()))
  expect_length(.supplement_crosscheck(supp, sections), 0)
})
