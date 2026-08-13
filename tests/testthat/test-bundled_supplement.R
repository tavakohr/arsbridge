# The bundled supplement: inst/extdata/example_cdsc_alz_201/supplement.json.
#
# It is a REVIEWED example -- what a chat assistant could return for the
# bundled Word shell inside a closed environment, corrected by hand and
# committed. Shipping it makes the offline workflow demonstrable from the
# bundle alone: deterministic parse -> reviewed supplement -> enriched ARS,
# with no API key, no network, and no `ellmer`.
#
# Two things have to be true at once, and both are asserted here:
#   * shipping it changes NOTHING by default -- it is an example artifact, not
#     something spec_to_ars() discovers or applies on its own;
#   * supplied explicitly, each enrichment it carries actually arrives in the
#     ARS, survives a save/re-read, and adds no blocking finding.
#
# Every enrichment asserted below is paired with the baseline assertion that
# the deterministic parse does NOT already produce it. Without that pairing a
# passing test would prove nothing about the supplement.

.bs_shell <- function() arsbridge_example("annotated_shell.docx")
.bs_spec  <- function() arsbridge_example("adam_spec.xlsx")
.bs_supp  <- function() arsbridge_example("supplement.json")

## The analysis set of Table 14.1.1. Its id is built from the shell's prose
## population line, which is why it is this long -- and why the shell alone
## leaves it with no condition to filter on.
.BS_ANALYSIS_SET <- paste0("AS_ALL_SUBJECTS_ENROLLED_ADSL_ALL_ENROLLED_",
                           "SUBJECTS_NO_ITTFL_DCSREAS_IN_THIS_ADSL")

## Convert the bundled Word shell with every live-LLM route closed. Returns
## the parsed ARS plus the diagnostics, so a test can assert on either.
.bs_convert <- function(supplement = NULL, use_llm = FALSE, extra_env = character(),
                        env = parent.frame()) {
  out <- withr::local_tempfile(fileext = ".json", .local_envir = env)
  blank <- c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
             GLM_API_KEY = "", ARS_LLM_PROVIDER = "")
  blank[names(extra_env)] <- extra_env
  res <- withr::with_envvar(
    blank,
    suppressMessages(suppressWarnings(spec_to_ars(
      shell_path     = .bs_shell(),
      adam_spec_path = .bs_spec(),
      output_path    = out,
      study_id       = "CDSC-ALZ-201",
      supplement     = supplement,
      use_llm        = use_llm,
      verbose        = FALSE))))
  list(ars = jsonlite::fromJSON(out, simplifyVector = FALSE),
       diagnostics = res$diagnostics)
}

## Both conversions are several seconds each and every test needs one of them,
## so each is built once for the file rather than per test.
.bs_cache <- new.env(parent = emptyenv())
.bs_run <- function(key, ...) {
  if (is.null(.bs_cache[[key]])) .bs_cache[[key]] <- .bs_convert(..., env = .bs_cache)
  .bs_cache[[key]]
}
.bs_baseline <- function() .bs_run("baseline")
.bs_enriched <- function() .bs_run("enriched", supplement = .bs_supp())

.bs_by_id <- function(entries, id) {
  hit <- Filter(function(x) identical(as.character(x$id), id), entries %||% list())
  if (length(hit) == 0) NULL else hit[[1]]
}
.bs_output <- function(ars, id) .bs_by_id(ars$outputs, id)

## An output's analyses, in the order the output references them.
.bs_analyses_of <- function(ars, output_id) {
  ids <- unlist(.bs_output(ars, output_id)$referencedAnalysisIds %||% list())
  lapply(ids, function(id) .bs_by_id(ars$analyses, id))
}

.bs_var_ref <- function(analysis) {
  paste(unlist(analysis$analysisVariable %||% list()), collapse = ".")
}

test_that("the committed supplement passes its own pre-flight validation", {
  findings <- ars_validate_supplement(.bs_supp(), adam_spec_path = .bs_spec())
  n_fail <- if (is.data.frame(findings)) sum(findings$severity == "FAIL") else 0L
  expect_equal(n_fail, 0L)
})

test_that("the supplement is an example artifact, not something auto-applied", {
  ## The guardrail that matters most about shipping it: a file sitting in the
  ## bundle directory next to the shell must not change what the shell alone
  ## converts to. Nothing may discover it.
  baseline <- .bs_baseline()
  expect_equal(baseline$ars[["_meta"]][["extraction_mode"]], "deterministic")
  expect_null(baseline$ars[["_meta"]][["supplement_trust"]])

  ## And its most distinctive contribution is genuinely absent.
  set <- .bs_by_id(baseline$ars$analysisSets, .BS_ANALYSIS_SET)
  expect_false(is.null(set))
  expect_null(set$condition)
})

test_that("supplying it explicitly puts the run in supplement mode", {
  enriched <- .bs_enriched()
  expect_equal(enriched$ars[["_meta"]][["extraction_mode"]], "supplement")
  expect_equal(enriched$ars[["_meta"]][["supplement_trust"]], "fill_gaps")
})

test_that("the population becomes a typed condition the shell never gave", {
  ## Table 14.1.1's population is prose ("All Subjects Enrolled (ADSL; all
  ## enrolled subjects ...)"), so the deterministic pass produces an analysis
  ## set with a name and NO condition -- nothing an engine can filter on. The
  ## reviewed supplement states the condition.
  before <- .bs_by_id(.bs_baseline()$ars$analysisSets, .BS_ANALYSIS_SET)
  after  <- .bs_by_id(.bs_enriched()$ars$analysisSets, .BS_ANALYSIS_SET)

  expect_null(before$condition)
  expect_equal(after$condition$dataset, "ADSL")
  expect_equal(after$condition$variable, "STUDYID")
  expect_equal(after$condition$comparator, "EQ")
  expect_equal(unlist(after$condition$value), "CDISCPILOT01")
})

test_that("the treatment axis becomes fixed, labelled columns", {
  ## Deterministically the column axis is data-driven with no groups, so the
  ## columns are whatever the data happens to hold -- which for this ADSL
  ## includes a "Screen Failure" arm the tables do not show. The supplement
  ## states the three columns the shell actually prints, each with its own
  ## typed condition.
  before <- .bs_by_id(.bs_baseline()$ars$analysisGroupings, "GF_TRT01A")
  after  <- .bs_by_id(.bs_enriched()$ars$analysisGroupings, "GF_TRT01A")

  expect_true(isTRUE(as.logical(unlist(before$dataDriven)[1])))
  expect_length(before$groups %||% list(), 0)

  expect_false(isTRUE(as.logical(unlist(after$dataDriven)[1])))
  expect_length(after$groups, 3)
  expect_equal(vapply(after$groups, function(g) as.character(g$label), character(1)),
               c("Placebo", "Xanomeline Low", "Xanomeline High"))
  expect_equal(vapply(after$groups, function(g) unlist(g$condition$value)[1], character(1)),
               c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
})

test_that("an unannotated row becomes an analysis with its own data subset", {
  ## The Word shell leaves "Subjects with any TEAE" -- the row that heads
  ## Table 14.3.2 -- with no annotation at all, so the deterministic pass
  ## builds only the SOC and PT analyses under it. The supplement binds it.
  before <- .bs_analyses_of(.bs_baseline()$ars, "T_14_3_2")
  after  <- .bs_analyses_of(.bs_enriched()$ars, "T_14_3_2")

  expect_equal(vapply(before, .bs_var_ref, character(1)),
               c("ADAE.AESOC", "ADAE.AEDECOD"))
  expect_equal(vapply(after, .bs_var_ref, character(1)),
               c("ADAE.USUBJID", "ADAE.AESOC", "ADAE.AEDECOD"))

  ## It arrives first, as the shell prints it, counting subjects restricted to
  ## treatment-emergent records. The method is NOT stated by the supplement:
  ## the shell cell shows "xx (xx.x)", and arsbridge reads those two slots.
  bound <- after[[1]]
  expect_equal(bound$methodId, "MTH_SUBJECT_COUNT_PCT")
  expect_equal(bound$dataSubsetId, "DS_ADAE_TRTEMFL_Y")

  subset <- .bs_by_id(.bs_enriched()$ars$dataSubsets, "DS_ADAE_TRTEMFL_Y")
  expect_false(is.null(subset))
})

test_that("review context the engine does not compute is still carried", {
  ## recordFilter, sorting and provenance are recorded rather than executed.
  ## Recorded is not the same as discarded: a reviewer has to be able to see
  ## what the supplement supplied, so it travels on the output's _meta.
  before <- .bs_output(.bs_baseline()$ars, "T_14_3_1")[["_meta"]][["supplement"]]
  after  <- .bs_output(.bs_enriched()$ars, "T_14_3_1")[["_meta"]][["supplement"]]

  expect_null(before)
  expect_equal(after$recordFilter$condition$variable, "TRTEMFL")
  expect_equal(unlist(after$recordFilter$condition$value), "Y")
  expect_true(length(after$provenance$reviewItems) > 0)

  ## Sorting rides along on the table that declares it.
  sorting <- .bs_output(.bs_enriched()$ars, "T_14_3_2")[["_meta"]][["supplement"]]$sorting
  expect_equal(vapply(sorting, function(s) as.character(s$variable), character(1)),
               c("AESOC", "AEDECOD"))
})

test_that("the enrichments survive a save and re-read", {
  ## The editor reads an ARS into a model and writes it back. An enrichment
  ## that a round trip drops is an enrichment a user loses the moment they
  ## open the file, so the supplement's contributions are checked across it.
  enriched <- .bs_enriched()$ars
  roundtrip <- model_to_ars(ars_to_model(enriched))

  expect_equal(.bs_by_id(roundtrip$analysisSets, .BS_ANALYSIS_SET)$condition,
               .bs_by_id(enriched$analysisSets, .BS_ANALYSIS_SET)$condition)
  expect_equal(.bs_by_id(roundtrip$analysisGroupings, "GF_TRT01A")$groups,
               .bs_by_id(enriched$analysisGroupings, "GF_TRT01A")$groups)
  expect_equal(vapply(.bs_analyses_of(roundtrip, "T_14_3_2"), .bs_var_ref, character(1)),
               c("ADAE.USUBJID", "ADAE.AESOC", "ADAE.AEDECOD"))
  expect_equal(.bs_output(roundtrip, "T_14_3_1")[["_meta"]][["supplement"]],
               .bs_output(enriched, "T_14_3_1")[["_meta"]][["supplement"]])
})

test_that("the supplement adds no blocking finding", {
  ## Structural validation must be no worse for taking the supplement. The
  ## deterministic run of this shell is already FAIL-free, so the bar is zero
  ## -- and the baseline count is asserted too, or a regression that broke
  ## both would read as "no new failures".
  n_fail <- function(d) sum(d$severity == "FAIL")
  expect_equal(n_fail(.bs_baseline()$diagnostics), 0L)
  expect_equal(n_fail(.bs_enriched()$diagnostics), 0L)
})

test_that("applying it needs no key, no network and no ellmer", {
  ## `.enrich_structured()` is the single point at which either reader reaches
  ## a model, so counting calls through it answers "did this run go out to an
  ## LLM?" behaviourally. Zero is the whole claim of the offline workflow.
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  local_mocked_bindings(
    .enrich_structured = function(...) {
      calls$n <- calls$n + 1L
      NULL
    },
    .package = "arsbridge")

  ## A usable key IS configured here, and the run still takes the supplement:
  ## a supplement is resolved before use_llm is even consulted, so opting in
  ## cannot drag a live call -- or ellmer -- into it.
  res <- .bs_convert(supplement = .bs_supp(), use_llm = TRUE,
                     extra_env = c(ANTHROPIC_API_KEY = "sk-ant-fake-key-0001",
                                   ARS_LLM_PROVIDER  = "anthropic"))
  expect_equal(res$ars[["_meta"]][["extraction_mode"]], "supplement")
  expect_equal(calls$n, 0L)

  ## And the enrichment still lands, so this is not passing by doing nothing.
  expect_equal(.bs_by_id(res$ars$analysisSets, .BS_ANALYSIS_SET)$condition$variable,
               "STUDYID")
})

test_that("use_llm = FALSE does not suppress the supplement", {
  ## Opting out of the live LLM is not opting out of offline enrichment; the
  ## two are separate mechanisms and the default run above is already
  ## use_llm = FALSE. Asserted explicitly so a future guard cannot conflate
  ## them without failing here.
  enriched <- .bs_enriched()$ars
  expect_equal(enriched[["_meta"]][["extraction_mode"]], "supplement")
  expect_equal(.bs_by_id(enriched$analysisGroupings, "GF_TRT01A")$groups[[1]]$label,
               "Placebo")
})

test_that("the bundle exposes the supplement by name", {
  expect_true("supplement.json" %in% arsbridge_example())
  expect_true(file.exists(.bs_supp()))
})
