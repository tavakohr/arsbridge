## tests/testthat/fixtures/build_ars_fixture.R
## ---------------------------------------------------------------------------
## Re-generates the ARS reporting-event fixture used by the round-trip and
## editor tests. Run from the package root:
##
##   Rscript tests/testthat/fixtures/build_ars_fixture.R
##
## The fixture is produced in DETERMINISTIC mode (regex extraction, keyword
## heuristics, no LLM call), so it is reproducible on any machine without an
## API key. Every LLM environment variable is blanked for the run so a
## configured key cannot leak into the fixture.
##
## Two files are written:
##
##   ars_apx_drm_301_deterministic.json  the reporting event itself
##   ars_apx_drm_301_validation.csv      the annotation-vs-spec validation
##                                       table, used by the gap-detection tests
##
## The JSON is trimmed to the first `.KEEP_OUTPUTS` outputs so the fixture
## stays small enough to live comfortably in git. Trimming keeps those
## outputs, the analyses they reference and every shared entity, then rebuilds
## the two tables of contents from the trimmed outputs.

suppressPackageStartupMessages({
  library(pkgload)
  library(withr)
})

here <- if (dir.exists("tests/testthat/fixtures")) {
  "tests/testthat/fixtures"
} else if (dir.exists("fixtures")) {
  "fixtures"
} else {
  "."
}

pkg_root <- if (identical(here, "tests/testthat/fixtures")) {
  "."
} else {
  file.path(here, "..", "..", "..")
}
pkgload::load_all(pkg_root, quiet = TRUE)

## How many outputs to keep. Deterministic extraction currently resolves 12
## TLFs from the bundled shell, so this is a ceiling rather than a cut today;
## it keeps the fixture bounded if extraction later resolves more.
keep_outputs <- 12L

## ---------------------------------------------------------------------------
## Generate (deterministic -- no API key, no network)
## ---------------------------------------------------------------------------

cat("Generating the APX-DRM-301 reporting event in deterministic mode...\n")

res <- withr::with_envvar(
  c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
    GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
  suppressMessages(spec_to_ars_example(
    api_key     = "",
    output_path = tempfile(fileext = ".json"),
    report_path = tempfile(fileext = ".xlsx"),
    verbose     = FALSE
  ))
)

if (!identical(res$extraction_mode, "deterministic")) {
  stop("Expected deterministic extraction, got: ", res$extraction_mode)
}

re <- res$reporting_event
cat("Generated", length(re$outputs), "outputs and", length(re$analyses), "analyses.\n")

## ---------------------------------------------------------------------------
## Trim to the first .KEEP_OUTPUTS outputs
## ---------------------------------------------------------------------------
## Shared entities (methods, analysis sets, data subsets, groupings) are kept
## whole. They are small, and keeping them means the fixture still exercises
## the "shared entity referenced by many analyses" paths the editor cares
## about.

trim_reporting_event <- function(re, keep_n) {
  if (length(re$outputs) <= keep_n) return(re)

  kept_outputs <- re$outputs[seq_len(keep_n)]

  kept_analysis_ids <- unique(unlist(lapply(
    kept_outputs,
    function(o) unlist(o$referencedAnalysisIds %||% list())
  )))

  re$outputs  <- kept_outputs
  re$analyses <- Filter(
    function(a) a$id %in% kept_analysis_ids,
    re$analyses
  )

  ## Both tables of contents are pure derivations of the outputs list, so
  ## rebuild them rather than editing their nested structure.
  re$mainListOfContents   <- arsbridge:::.build_lopa(re$outputs)
  re$otherListsOfContents <- arsbridge:::.build_lopo(re$outputs)

  re
}

re <- trim_reporting_event(re, keep_outputs)
cat("Kept", length(re$outputs), "outputs and",
    length(re$analyses), "analyses.\n")

## ---------------------------------------------------------------------------
## Write, using the same serialization the pipeline itself uses
## ---------------------------------------------------------------------------

json_path <- file.path(here, "ars_apx_drm_301_deterministic.json")
json_text <- jsonlite::toJSON(re, auto_unbox = TRUE, pretty = TRUE, null = "null")
writeLines(as.character(json_text), json_path, useBytes = TRUE)
cat("Wrote:", json_path,
    sprintf("(%.0f KB)\n", file.info(json_path)$size / 1024))

## The validation table drives the gap-detection tests. Keep it whole: rows
## for trimmed-away outputs simply never match an analysis, which is a
## realistic shape for those tests to see.
csv_path <- file.path(here, "ars_apx_drm_301_validation.csv")
utils::write.csv(res$validation, csv_path, row.names = FALSE)
cat("Wrote:", csv_path, sprintf("(%d rows)\n", nrow(res$validation)))
