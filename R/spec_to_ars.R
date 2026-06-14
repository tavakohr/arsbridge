## arsbridge -- spec_to_ars.R
## ---------------------------------------------------------------------------
## The single exported function. Reads the annotated TLF shell + ADaM spec,
## extracts and validates annotations, calls the LLM once per TLF for
## semantic enrichment, and writes a CDISC ARS v1.0 JSON file.

#' Convert annotated TLF shell and ADaM spec to CDISC ARS JSON
#'
#' Reads a lead programmer's already-annotated TLF shells Word document and
#' the study's ADaM specification Excel, and produces a valid CDISC Analysis
#' Results Standard (ARS) v1.0 ARM-TS JSON file consumable by
#' \code{siera::readARS()}.
#'
#' @param shell_path     Path to annotated TLF shells `.docx`.
#' @param adam_spec_path Path to the ADaM specification. Accepts either:
#'   * `.xml` -- ADaM `define.xml` (preferred when available)
#'   * `.xlsx` / `.xls` -- ADaM specification Excel (fallback used during
#'     development before `define.xml` is produced)
#'
#'   One of the two is required. The SDTM spec is NOT a valid input --
#'   TLF annotations reference ADaM variables, so the grounding source
#'   must be the ADaM spec.
#' @param output_path    Path for the ARS JSON. Default `"reporting_event.json"`.
#' @param study_id       Study identifier. Default `"STUDY-001"`.
#' @param study_name     Human-readable study name. Defaults to `study_id`.
#' @param model          LLM model. Defaults to the active provider's default model.
#' @param api_key        LLM API key. Defaults to the active provider's key.
#' @param provider       LLM provider: `"anthropic"`, `"openai"`, or `"gemini"`.
#'   Defaults to the active provider.
#' @param spec_column_aliases Optional named list of extra column-name
#'   aliases for the ADaM spec Excel (see `parse_adam_spec()`); useful when
#'   a workbook uses non-standard or non-English headers. Example:
#'   `list(variable = "nom de variable", dataset = "domaine")`.
#' @param validate       If `TRUE` (default), cross-reference annotations
#'   against the ADaM spec and write a validation report.
#' @param report_path    Path for the validation report `.xlsx`.
#'   Default `"spec_validation_report.xlsx"`.
#' @param verbose        Print progress messages. Default `TRUE`.
#'
#' @return Invisibly returns a named list:
#'   \describe{
#'     \item{`ars_path`}{Path to the generated ARS JSON file.}
#'     \item{`report_path`}{Path to the validation report (if validate=TRUE).}
#'     \item{`n_tlfs`}{Number of TLF sections processed.}
#'     \item{`n_analyses`}{Number of ARS Analysis objects created.}
#'     \item{`n_warnings`}{Number of spec validation warnings.}
#'     \item{`reporting_event`}{The full ARS ReportingEvent as a nested R
#'       list -- the same content that was serialised to `ars_path`. Inspect
#'       interactively with e.g. `str(res$reporting_event, max.level = 2)`.}
#'     \item{`validation`}{Data frame of per-annotation validation results
#'       (`tlf_number`, `stub_label`, `annotation`, `variable_ref`, `status`,
#'       `message`). `NULL` when `validate = FALSE`.}
#'     \item{`diagnostics`}{Data frame of pipeline diagnostics -- every
#'       fallback, parsing miss, skipped sheet, LLM failure, unknown method,
#'       and dropped where-clause condition recorded during the run
#'       (`stage`, `severity`, `tlf_number`, `location`, `problem`,
#'       `action`). Also written to the "Diagnostics" sheet of the
#'       validation report and retrievable via [ars_diagnostics()].}
#'   }
#'
#' @section Human review:
#' The generated ARS JSON is a draft. A qualified clinical programmer MUST
#' review it before downstream use. The JSON includes a
#' `_meta.requires_human_review = TRUE` field that consumers can key on.
#'
#' @examples
#' \dontrun{
#' spec_to_ars(
#'   shell_path     = "inputs/annotated_shells.docx",
#'   adam_spec_path = "inputs/adam_spec.xlsx",
#'   output_path    = "outputs/reporting_event.json",
#'   report_path    = "outputs/spec_validation_report.xlsx"
#' )
#' }
#' @export
spec_to_ars <- function(shell_path,
                        adam_spec_path,
                        output_path  = "reporting_event.json",
                        study_id     = "STUDY-001",
                        study_name   = NULL,
                        model        = NULL,
                        api_key      = NULL,
                        provider     = NULL,
                        spec_column_aliases = NULL,
                        validate     = TRUE,
                        report_path  = "spec_validation_report.xlsx",
                        verbose      = TRUE) {

  if (!file.exists(shell_path)) {
    cli::cli_abort("Shell file not found: {.path {shell_path}}")
  }
  if (!file.exists(adam_spec_path)) {
    cli::cli_abort("ADaM spec file not found: {.path {adam_spec_path}}")
  }
  if (!grepl("\\.docx?$", shell_path, ignore.case = TRUE)) {
    cli::cli_abort("Shell file must have a {.val .docx} extension: {.path {shell_path}}")
  }
  if (!grepl("\\.(xml|xlsx?)$", adam_spec_path, ignore.case = TRUE)) {
    cli::cli_abort(c(
      "ADaM spec must be {.val .xml} (define.xml) or {.val .xlsx} / {.val .xls} (Excel).",
      "x" = "Got: {.path {adam_spec_path}}",
      "i" = "The SDTM spec is not a valid input -- TLF annotations reference ADaM variables."
    ))
  }

  active <- get_active_llm()
  if (is.null(provider)) {
    provider <- active$provider
  }
  if (is.null(provider)) {
    cli::cli_abort(c(
      "No active LLM API key found.",
      "i" = "Please set up an API key for Anthropic, OpenAI, or Gemini.",
      " " = "You can run {.code arsbridge::set_anthropic_key()}, {.code arsbridge::set_openai_key()}, or {.code arsbridge::set_gemini_key()}."
    ))
  }

  if (is.null(model)) {
    model <- active$model
  }
  if (is.null(api_key)) {
    env_var <- switch(provider,
      anthropic = "ANTHROPIC_API_KEY",
      openai    = "OPENAI_API_KEY",
      gemini    = "GEMINI_API_KEY"
    )
    api_key <- Sys.getenv(env_var)
  }

  if (is.null(api_key) || !nzchar(api_key)) {
    cli::cli_abort(c(
      "API key for {.val {provider}} is not set.",
      "i" = "Please configure your API key using the appropriate set function (e.g. {.code set_openai_key()})."
    ))
  }

  if (verbose) cli::cli_h1("arsbridge::spec_to_ars")

  ## Fresh diagnostics for this run -- every parser/LLM/builder fallback is
  ## recorded and lands on the "Diagnostics" sheet of the validation report.
  diag_reset()

  ## --- Parse inputs --------------------------------------------------
  ## Spec first: the shell parser uses the spec lookup to validate listing
  ## column-header variable candidates.
  if (verbose) cli::cli_alert_info("Parsing ADaM spec {.path {basename(adam_spec_path)}}...")
  spec <- parse_adam_spec(adam_spec_path, column_aliases = spec_column_aliases)

  if (verbose) cli::cli_alert_info("Parsing annotated shell {.path {basename(shell_path)}}...")
  sections <- parse_shell_docx(shell_path, spec_lookup = spec$lookup)
  if (length(sections) == 0) {
    cli::cli_abort("No TLF sections found in {.path {shell_path}}.")
  }

  ## --- Validation ----------------------------------------------------
  ## The report itself is written AFTER the build, so its Diagnostics sheet
  ## also captures enrichment- and build-stage findings.
  validation <- NULL
  if (isTRUE(validate)) {
    if (verbose) cli::cli_alert_info("Cross-referencing annotations against ADaM spec...")
    validation <- validate_annotations_spec(sections, spec$lookup)
    n_warn <- sum(validation$status %in% c("WARN", "FAIL"))
    if (n_warn > 0) {
      cli::cli_alert_warning("{n_warn} validation finding{?s} -- see {.path {report_path}}")
    } else if (verbose) {
      cli::cli_alert_success("All annotations validated cleanly.")
    }
  }

  ## --- LLM enrichment, one call per TLF -----------------------------
  if (verbose) cli::cli_alert_info("Enriching {length(sections)} TLF section{?s} with {toupper(provider)} ({model})...")
  enriched <- vector("list", length(sections))
  for (i in seq_along(sections)) {
    sec <- sections[[i]]
    if (verbose) {
      cli::cli_alert("  [{i}/{length(sections)}] {.val {sec$tlf_number}}: {substr(sec$title, 1, 60)}")
    }
    enriched[[i]] <- enrich_with_llm(sec, spec_lookup = spec$lookup,
                                     model = model, api_key = api_key, provider = provider)
  }

  ## --- Build and write ARS JSON --------------------------------------
  if (verbose) cli::cli_alert_info("Building CDISC ARS v1.0 ReportingEvent...")
  re <- build_ars_json(enriched, study_id = study_id,
                       study_name = study_name %||% study_id,
                       spec_lookup = spec$lookup)

  json_text <- jsonlite::toJSON(re, auto_unbox = TRUE, pretty = TRUE, null = "null")
  writeLines(json_text, output_path, useBytes = TRUE)

  if (verbose) {
    cli::cli_alert_success("Wrote ARS JSON to {.path {output_path}}")
  }

  ## --- Diagnostics summary + report ----------------------------------
  diagnostics <- diag_records()
  if (nrow(diagnostics) > 0) {
    n_fail <- sum(diagnostics$severity == "FAIL")
    n_warn_diag <- sum(diagnostics$severity == "WARN")
    cli::cli_alert_warning(
      paste0("{nrow(diagnostics)} pipeline diagnostic{?s} ",
             "({n_fail} FAIL, {n_warn_diag} WARN)",
             if (isTRUE(validate)) " -- see the Diagnostics sheet of {.path {report_path}}"
             else " -- inspect with {.code ars_diagnostics()}")
    )
  }
  if (isTRUE(validate)) {
    write_validation_report(validation, report_path, diagnostics = diagnostics)
  }

  result <- list(
    ars_path        = output_path,
    report_path     = if (isTRUE(validate)) report_path else NULL,
    n_tlfs          = length(enriched),
    n_analyses      = length(re$analyses),
    n_warnings      = if (!is.null(validation))
                        sum(validation$status %in% c("WARN", "FAIL")) else 0L,
    reporting_event = re,
    validation      = validation,
    diagnostics     = diagnostics
  )
  invisible(result)
}
