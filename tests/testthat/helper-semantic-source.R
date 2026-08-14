## helper-semantic-source.R ----------------------------------------------------
## Fixtures for the semantic-source detector.
##
## The unit-level helpers build a minimal ars_model directly, so the rule can be
## exercised without parsing a shell. The study-level ones build a real
## reporting event from a shell and spec, because the defects being pinned are
## produced by the build stage and cannot be hand-written without assuming the
## very thing under test.

#' One analysis node, in the shape .pool_analyses() reads.
ssrc_analysis <- function(id, dataset, variable, annotation,
                          data_subset_id = "") {
  list(
    id = id, name = id, label = id, description = id,
    analysisSetId = "AS_1", dataSubsetId = data_subset_id,
    methodId = "MTH_SUBJECT_COUNT_PCT",
    dataset = dataset, variable = variable,
    analysisVariable = list(dataset = dataset, variable = variable),
    annotation = annotation, orderedGroupings = list()
  )
}
.ssrc_analysis <- ssrc_analysis

#' One single-condition DataSubset.
ssrc_subset <- function(id, dataset, variable, value, comparator = "EQ") {
  list(id = id, name = id, label = id, level = 1L, order = 1L,
       condition = list(dataset = dataset, variable = variable,
                        comparator = comparator, value = list(value)))
}
.ssrc_subset <- ssrc_subset

#' A minimal ars_model carrying just these analyses and subsets.
ssrc_model <- function(analyses, subsets = list()) {
  ars <- list(
    id = "RE_TEST", name = "semantic source fixture", version = "1",
    analysisSets = list(list(id = "AS_1", name = "Safety", label = "Safety",
                             level = 1L, order = 1L)),
    dataSubsets = subsets, analysisGroupings = list(),
    methods = list(list(id = "MTH_SUBJECT_COUNT_PCT", name = "n (%)",
                        label = "n (%)", operations = list())),
    analyses = analyses, outputs = list()
  )
  path <- withr::local_tempfile(fileext = ".json", .local_envir = parent.frame())
  jsonlite::write_json(ars, path, auto_unbox = TRUE, null = "null")
  ars_to_model(path)
}
.ssrc_model <- ssrc_model

#' Only the semantic-source findings from a full validation run.
ssrc_hits <- function(model) {
  f <- as.data.frame(validate_ars_model(model))
  ref <- .SEMANTIC_SOURCE_REF
  out <- f[!is.na(f$ref) & f$ref == ref, , drop = FALSE]
  rownames(out) <- NULL
  out
}
.ssrc_hits <- ssrc_hits

#' Build a reporting event from real study material and return its model.
#' Every LLM route is closed so the run is deterministic.
ssrc_study_model <- function(shell, spec, tag, supplement = NULL,
                             envir = parent.frame()) {
  out <- withr::local_tempfile(fileext = ".json", .local_envir = envir)
  args <- list(
    shell_path = shell, adam_spec_path = spec, output_path = out,
    study_id = tag, use_llm = FALSE, api_key = "",
    report_path = withr::local_tempfile(fileext = ".xlsx", .local_envir = envir),
    verbose = FALSE
  )
  if (!is.null(supplement)) args$supplement <- supplement
  withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(suppressWarnings(do.call(spec_to_ars, args))))
  ars_to_model(out)
}
.ssrc_study_model <- ssrc_study_model

#' The APX-DRM-301 material, or NULL when it is not present.
#'
#' `inputs/DRM/` is the authoritative current DRM input set. It is gitignored
#' study material, so it is absent on CI and anywhere the study has not been
#' copied in -- every test using it skips rather than fails.
ssrc_drm_inputs <- function() {
  ## No rprojroot: it is not a declared dependency, and calling it would turn
  ## "the study material is absent" into an error before the skip can fire.
  ## test_path() is rooted at tests/testthat, so the package root is two up;
  ## the extra candidates cover a run from the package root itself.
  roots <- c(
    testthat::test_path("..", "..", "inputs", "DRM"),
    file.path("inputs", "DRM"),
    file.path(getwd(), "inputs", "DRM")
  )
  for (root in roots) {
    shell <- file.path(root, "APX-DRM-301_TLF_Shells_v2.1_annotated.xlsx")
    spec  <- file.path(root, "adam_spec_APX-DRM-301.xlsx")
    if (file.exists(shell) && file.exists(spec)) {
      return(list(root = root, shell = shell, spec = spec,
                  supplement = file.path(root, "supplement.json"),
                  adam = file.path(root, "ADaM")))
    }
  }
  NULL
}
.ssrc_drm_inputs <- ssrc_drm_inputs
