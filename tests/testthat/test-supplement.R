## Supplement mode: reading/validating the Copilot supplement, gap-filling
## bindings (regex wins), enrichment passthrough, and the 3-tier mode
## resolution in spec_to_ars().

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
  list(supplement_version = 2L, tlfs = tlfs)
}

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
    bindings = list(list(label = "Age (years)", variable = "ADSL.AGE")))))
  path <- .write_supp(supp)
  expect_equal(read_supplement(path)$supplement_version, 2L)

  fenced <- tempfile(fileext = ".json")
  writeLines(c("Here is the supplement:", "```json",
               readLines(path), "```", "Hope this helps!"), fenced)
  expect_equal(
    read_supplement(fenced)$tlfs[[1]]$bindings[[1]]$variable, "ADSL.AGE")

  smart <- tempfile(fileext = ".json")
  writeLines(gsub('"', "“", readLines(path)), smart)  ## all quotes smart
  expect_equal(read_supplement(smart)$supplement_version, 2L)
})

test_that("read_supplement aborts on malformed JSON, bad version, missing tlfs", {
  bad <- tempfile(fileext = ".json")
  writeLines("{not json", bad)
  expect_error(read_supplement(bad), "not valid JSON")

  wrong_ver <- .write_supp(list(supplement_version = 99L,
                                tlfs = list(`14.1.1` = list())))
  expect_error(read_supplement(wrong_ver), "version mismatch")

  no_tlfs <- .write_supp(list(supplement_version = 2L))
  expect_error(read_supplement(no_tlfs), "tlfs")
})

test_that("a double-quoted where value is auto-repaired to single quotes", {
  ## The classic Copilot mistake: a where-value quoted with double quotes,
  ## which would close the JSON string early. arsbridge repairs `="..."` to
  ## `='...'` before parsing, so the file loads instead of aborting.
  dq <- tempfile(fileext = ".json")
  writeLines(
    paste0('{"supplement_version":2,"tlfs":{"14.1.1":{"bindings":',
           '[{"label":"Underlying","variable":"ADMH.MHTERM",',
           '"where":"MHTERM not null and MHSCAT="UNDERLYING CONDITIONS""}]}}}'),
    dq)
  supp <- read_supplement(dq)
  w <- supp$tlfs[["14.1.1"]]$bindings[[1]]$where
  expect_match(w, "MHSCAT='UNDERLYING CONDITIONS'")
  expect_false(grepl('"', w, fixed = TRUE))
})

test_that(".clean_supplement_text repairs double-quoted values but leaves valid JSON alone", {
  ## Multiple double-quoted comparisons in one compound clause.
  repaired <- arsbridge:::.clean_supplement_text(
    '{"where":"A="X" and B="Y""}')
  expect_false(grepl('="', repaired, fixed = TRUE))
  expect_match(repaired, "A='X'")
  expect_match(repaired, "B='Y'")

  ## An already-correct single-quoted value is untouched.
  ok <- '{"population":"ADSL.SAFFL=\'Y\'"}'
  expect_identical(arsbridge:::.clean_supplement_text(ok), ok)

  ## An escaped (already-valid) double-quoted value is left as-is.
  esc <- '{"note":"say =\\"hi\\""}'
  expect_identical(arsbridge:::.clean_supplement_text(esc), esc)
})

test_that("genuinely malformed JSON still aborts with the single-quote hint", {
  bad <- tempfile(fileext = ".json")
  writeLines("{ not json,, }", bad)
  expect_error(read_supplement(bad), "not valid JSON")
  expect_error(read_supplement(bad), "single quote")
})

## --- ars_validate_supplement ----------------------------------------------

test_that("validator passes a clean supplement and flags bad fields", {
  clean <- .write_supp(.supp_minimal(list(`14.1.1` = list(
    title = "Demographics",
    bindings = list(list(label = "Age (years)", variable = "ADSL.AGE")),
    analysis_type = "CONTINUOUS"))))
  out <- suppressMessages(ars_validate_supplement(clean))
  expect_equal(nrow(out), 0)

  dirty <- .write_supp(.supp_minimal(list(`14.1.1` = list(
    bindings = list(
      list(label = "", variable = "ADSL.AGE"),          ## empty label
      list(label = "Sex", variable = "not a var")       ## bad syntax
    ),
    analysis_type = "REGRESSION",                        ## bad enum
    mystery_field = "x"))))                              ## unknown field
  out <- suppressMessages(ars_validate_supplement(dirty))
  expect_true(any(out$severity == "FAIL" & grepl("label", out$problem)))
  expect_true(any(out$severity == "FAIL" & grepl("DATASET.VARIABLE", out$problem)))
  expect_true(any(out$severity == "FAIL" & out$where == "analysis_type"))
  expect_true(any(out$severity == "INFO" & grepl("mystery_field", out$problem)))
})

test_that("validator applies the spec gate when a spec path is given", {
  supp <- .write_supp(.supp_minimal(list(`14.1.1` = list(
    bindings = list(list(label = "Weight", variable = "ADSL.WEIGHTBL"))))))
  out <- suppressMessages(ars_validate_supplement(
    supp, adam_spec_path = test_path("fixtures/adam_spec_minimal.xlsx")))
  expect_true(any(out$severity == "FAIL" &
                    grepl("ADSL.WEIGHTBL", out$problem) &
                    grepl("not in the ADaM spec", out$problem)))
})

test_that("validator accepts column_groups and flags a single-entry axis", {
  ok <- .write_supp(.supp_minimal(list(`14.1.2` = list(
    title = "Key Protocol Deviations",
    bindings = list(list(label = "Category", variable = "ADDV.DVCAT")),
    columns = "ADSL.COHORTN",
    column_groups = list(
      list(label = "Cohort A", where = "ADSL.COHORTN=1"),
      list(label = "Unknown Cohort", where = "is.na(ADSL.COHORTN)"))))))
  out <- suppressMessages(ars_validate_supplement(ok))
  expect_false(any(out$where == "column_groups" & out$severity == "FAIL"))

  ## A single column group is not a grouping axis -> WARN.
  lone <- .write_supp(.supp_minimal(list(`14.1.2` = list(
    columns = "ADSL.COHORTN",
    column_groups = list(list(label = "Only", where = "ADSL.COHORTN=1"))))))
  out <- suppressMessages(ars_validate_supplement(lone))
  expect_true(any(out$severity == "WARN" & out$where == "column_groups"))

  ## A group with no where is a FAIL.
  nowhere <- .write_supp(.supp_minimal(list(`14.1.2` = list(
    columns = "ADSL.COHORTN",
    column_groups = list(
      list(label = "A", where = "ADSL.COHORTN=1"),
      list(label = "B"))))))
  out <- suppressMessages(ars_validate_supplement(nowhere))
  expect_true(any(out$severity == "FAIL" & grepl("column_groups", out$where)))
})

## --- .apply_supplement_bindings --------------------------------------------

test_that("supplement fills only unannotated rows; regex wins conflicts", {
  diag_reset()
  supp_tlf <- list(bindings = list(
    list(label = "Age (years)", variable = "ADSL.AGE"),   ## gap -> filled
    list(label = "Sex", variable = "ADSL.EOSSTT")         ## conflict -> kept
  ))
  sec <- .apply_supplement_bindings(.mk_supp_section(), supp_tlf, .supp_spec)

  expect_true(sec$stub_rows[[1]]$has_annot)
  expect_equal(sec$stub_rows[[1]]$annotation, "ADSL.AGE")
  expect_equal(sec$stub_rows[[1]]$detection_method, "supplement")
  expect_equal(sec$stub_rows[[1]]$detection_confidence, "medium")

  ## Conflict: the shell's ADSL.SEX stands, a WARN names both variables.
  expect_equal(sec$stub_rows[[2]]$annotation, "ADSL.SEX")
  expect_equal(sec$stub_rows[[2]]$detection_method, "colour")
  recs <- diag_records()
  expect_true(any(recs$stage == "supplement" & recs$severity == "WARN" &
                    grepl("ADSL.EOSSTT", recs$problem) &
                    grepl("ADSL.SEX", recs$problem)))
})

test_that("where clauses, statline rows, unmatched labels, spec gate", {
  diag_reset()
  supp_tlf <- list(bindings = list(
    list(label = "Age (years)", variable = "ADSL.EOSSTT",
         where = "EOSSTT='COMPLETED'"),
    list(label = "Mean (SD)", variable = "ADSL.AGE"),     ## statline -> skip
    list(label = "No Such Row", variable = "ADSL.AGE"),   ## unmatched
    list(label = "Age (years)", variable = "ADSL.FAKEVAR") ## out of spec
  ))
  sec <- .apply_supplement_bindings(.mk_supp_section(), supp_tlf, .supp_spec)

  expect_equal(sec$stub_rows[[1]]$annotation,
               "ADSL.EOSSTT WHERE EOSSTT='COMPLETED'")
  expect_false(sec$stub_rows[[3]]$has_annot)   ## statline untouched

  recs <- diag_records()
  expect_true(any(recs$severity == "INFO" & grepl("statistic sub-row", recs$problem)))
  expect_true(any(recs$severity == "WARN" & grepl("No Such Row", recs$problem)))
  expect_true(any(recs$severity == "FAIL" & grepl("ADSL.FAKEVAR", recs$problem)))
})

test_that("columns and population apply only when the shell left them empty", {
  sec <- .mk_supp_section()
  supp_tlf <- list(columns = "ADSL.TRT01A", population = "ADSL.SAFFL='Y'")
  out <- .apply_supplement_bindings(sec, supp_tlf, .supp_spec)
  expect_equal(out$column_annotation, "ADSL.TRT01A")
  expect_equal(out$population_annot, "ADSL.SAFFL='Y'")

  sec$column_annotation <- "ADSL.TRT01A"      ## already set by the shell
  sec$population_annot  <- "ADSL.SAFFL='Y'"
  out <- .apply_supplement_bindings(
    sec, list(columns = "ADSL.SEX", population = "ADSL.AGE='X'"), .supp_spec)
  expect_equal(out$column_annotation, "ADSL.TRT01A")
  expect_equal(out$population_annot, "ADSL.SAFFL='Y'")
})

## --- supplement column_groups ----------------------------------------------

.cg_supp_spec <- c(.supp_spec, list(ADSL.COHORTN = list(), ADDV.DVCAT = list()))

test_that("supplement column_groups build the per-column axis when the shell has none", {
  diag_reset()
  sec <- .mk_supp_section()
  sec$by_variable <- "COHORTN"; sec$by_variable_dataset <- "ADSL"
  supp_tlf <- list(
    columns = "ADSL.COHORTN",
    column_groups = list(
      list(label = "Cohort A",   where = "ADSL.COHORTN=1"),
      list(label = "Cohort B", where = "ADSL.COHORTN=2"),
      list(label = "Unknown Cohort",    where = "is.na(ADSL.COHORTN)")))
  out <- .apply_supplement_bindings(sec, supp_tlf, .cg_supp_spec)

  cg <- out$column_groups
  expect_equal(cg$variable, "COHORTN")
  expect_equal(cg$dataset, "ADSL")
  expect_equal(vapply(cg$groups, `[[`, character(1), "label"),
               c("Cohort A", "Cohort B", "Unknown Cohort"))
  expect_equal(out$column_annotation, "ADSL.COHORTN")

  ## Downstream: the existing builder emits three groups[], the Unknown one
  ## carrying the empty-value EQ that is.na() parses to.
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
  supp_tlf <- list(columns = "ADSL.COHORTN", column_groups = list(
    list(label = "Supp A", where = "ADSL.COHORTN=1"),
    list(label = "Supp B", where = "ADSL.COHORTN=2")))
  out <- .apply_supplement_bindings(sec, supp_tlf, .cg_supp_spec)
  expect_length(out$column_groups$groups, 1)
  expect_equal(out$column_groups$groups[[1]]$label, "Shell A")
})

test_that("an out-of-spec column-group where is dropped with a FAIL; valid ones stay", {
  diag_reset()
  sec <- .mk_supp_section()
  sec$by_variable <- "COHORTN"
  supp_tlf <- list(columns = "ADSL.COHORTN", column_groups = list(
    list(label = "Cohort A",   where = "ADSL.COHORTN=1"),
    list(label = "Cohort B", where = "ADSL.COHORTN=2"),
    list(label = "Bad",              where = "ADSL.FAKEVAR=9")))
  out <- .apply_supplement_bindings(sec, supp_tlf, .cg_supp_spec)

  expect_length(out$column_groups$groups, 2)   ## bad dropped, two valid kept
  recs <- diag_records()
  expect_true(any(recs$severity == "FAIL" & grepl("ADSL.FAKEVAR", recs$problem)))
})

## --- enrichment passthrough -------------------------------------------------

test_that(".supplement_enrich_answer maps fields into the live-answer shape", {
  ans <- .supplement_enrich_answer(list(
    analysis_type = "continuous", ars_method_name = "Summary Statistics - Continuous",
    by_variables = list("TRT01A"), include_total = TRUE,
    is_supported = FALSE, unsupported_reason = "needs ANCOVA"))
  expect_equal(ans$analysis_type, "CONTINUOUS")
  expect_equal(ans$by_variables, list("TRT01A"))
  expect_true(ans$include_total)
  expect_false(ans$is_supported)
  expect_equal(ans$unsupported_reason, "needs ANCOVA")

  expect_equal(.supplement_enrich_answer(NULL), list())
  ## Invalid enum is dropped rather than shipped.
  expect_null(.supplement_enrich_answer(list(analysis_type = "REGRESSION"))$analysis_type)
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
  ## Keyword heuristic: no bare statistic label in this synthetic section,
  ## so the default CATEGORICAL fallback applies.
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
  expect_true(file.exists(path))
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(txt, "supplement_version", fixed = TRUE)
  expect_match(txt, "Never invent a variable", fixed = TRUE)

  ## Second call keeps the existing copy (no error, same path).
  path2 <- suppressMessages(ars_copilot_instructions(dir, open = FALSE))
  expect_equal(path, path2)
})

test_that("ars_copilot_instructions creates a not-yet-existing dir", {
  base <- withr::local_tempdir()
  sub  <- file.path(base, "nested", "copilot_out")
  expect_false(dir.exists(sub))
  path <- suppressMessages(ars_copilot_instructions(sub, open = FALSE))
  expect_true(dir.exists(sub))
  expect_true(file.exists(path))
})

test_that("the copilot instruction file ships with the package", {
  ## Under R CMD check the package is installed; under load_all() pkgload
  ## maps inst/ -- either way system.file() must resolve the shipped resource.
  p <- system.file("copilot", "arsbridge_copilot_instructions.md",
                   package = "arsbridge")
  expect_true(nzchar(p) && file.exists(p))
})

test_that("the copied file is byte-identical to the shipped source", {
  src <- system.file("copilot", "arsbridge_copilot_instructions.md",
                     package = "arsbridge")
  skip_if_not(nzchar(src) && file.exists(src))
  dir  <- withr::local_tempdir()
  dest <- suppressMessages(ars_copilot_instructions(dir, open = FALSE))
  expect_equal(unname(tools::md5sum(dest)), unname(tools::md5sum(src)))
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
  ## Deterministic is a first-class mode: the mode is recorded as a neutral
  ## INFO, never a WARN or blocker.
  setup <- d[d$stage == "setup", ]
  expect_equal(nrow(setup), 1)
  expect_equal(setup$severity, "INFO")
  expect_match(setup$problem, "deterministic mode")

  ## The package must NEVER ask for a key or raise a key-related error/warning
  ## in deterministic (or supplement) mode.
  key_noise <- d[d$severity %in% c("WARN", "FAIL") &
                   grepl("API key|LLM key|no LLM key|provider not|set_.*_key",
                         d$problem, ignore.case = TRUE), ]
  expect_equal(nrow(key_noise), 0)
})

test_that("spec_to_ars(supplement=) binds gaps and records the mode", {
  supp_path <- .write_supp(.supp_minimal(list(
    `14.1.1` = list(
      bindings = list(
        list(label = "Male",   variable = "ADSL.SEX", where = "SEX='M'"),
        list(label = "Female", variable = "ADSL.SEX", where = "SEX='F'")
      ),
      columns = "ADSL.TRT01A",
      analysis_type = "CATEGORICAL",
      by_variables = list("TRT01A"),
      include_total = TRUE
    ),
    `99.9.9` = list(bindings = list())   ## typo key -> WARN, ignored
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
  ## No 'deterministic mode' setup WARN in supplement mode.
  expect_false(any(recs$stage == "setup"))
})

test_that("spec_to_ars emits populated groups[] from supplement column_groups", {
  ## End-to-end: a supplement column_groups on a value-conditioned axis (SEX,
  ## which the shell does not carry as a machine-readable header filter) must
  ## reach the ARS JSON as a grouping factor with per-column groups[].
  supp_path <- .write_supp(.supp_minimal(list(
    `14.1.1` = list(
      bindings = list(list(label = "Age (years)", variable = "ADSL.AGE")),
      columns = "ADSL.SEX",
      column_groups = list(
        list(label = "Male",   where = "ADSL.SEX='M'"),
        list(label = "Female", where = "ADSL.SEX='F'")),
      analysis_type = "CONTINUOUS",
      by_variables = list("SEX")
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

## --- title: known field, fill, and the table-set cross-check ---------------

test_that("a 'title' field is recognised (not flagged as unknown)", {
  path <- .write_supp(.supp_minimal(list(`14.1.1` = list(
    title = "Summary of Disposition",
    bindings = list(list(label = "Age (years)", variable = "ADSL.AGE"))
  ))))
  out <- ars_validate_supplement(path)
  expect_false(any(out$where == "fields" & grepl("title", out$problem)))
})

test_that("ars_validate_supplement suggests adding a missing title (INFO)", {
  path <- .write_supp(.supp_minimal(list(`14.1.1` = list(
    bindings = list(list(label = "Age (years)", variable = "ADSL.AGE"))
  ))))
  out <- ars_validate_supplement(path)
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
    ## 14.1.3 deliberately absent -> coverage gap
  ))
  f <- .supplement_crosscheck(supp, sections)
  probs <- vapply(f, function(x) x$problem, character(1))
  expect_true(all(vapply(f, function(x) x$severity, character(1)) == "WARN"))
  expect_true(any(grepl("14.9.9", probs) & grepl("no TLF parsed", probs)))
  expect_true(any(grepl("T-14-1-3", probs) & grepl("no entry", probs)))
  expect_true(any(grepl("T-14-1-2", probs) & grepl("supplement says", probs)))
  ## The agreeing pair produces nothing.
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
