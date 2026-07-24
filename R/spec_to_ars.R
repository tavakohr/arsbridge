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
#' @param sap_path       Optional path to the Statistical Analysis Plan `.docx`.
#'   When supplied, its prose is matched per TLF and carried into each analysis
#'   as `sapDescription`, becoming the human-readable comment above the emitted
#'   `{cards}` block. Gracefully ignored when absent or unreadable.
#' @param adam_spec_path Path to the ADaM specification. Accepts either:
#'   * `.xml` -- ADaM `define.xml` (preferred when available)
#'   * `.xlsx` / `.xls` -- ADaM specification Excel (fallback used during
#'     development before `define.xml` is produced)
#'
#'   One of the two is required. The SDTM spec is NOT a valid input --
#'   TLF annotations reference ADaM variables, so the grounding source
#'   must be the ADaM spec.
#' @param output_path    Path for the ARS JSON. Defaults to
#'   `reporting_event.json` in [tempdir()]; pass an explicit path to write it
#'   somewhere permanent.
#' @param study_id       Study identifier. Default `"STUDY-001"`.
#' @param study_name     Human-readable study name. Defaults to `study_id`.
#' @param model          LLM model. Defaults to the active provider's default model.
#' @param api_key        LLM API key. Defaults to the active provider's key.
#' @param provider       LLM provider: `"anthropic"`, `"openai"`, or `"gemini"`.
#'   Defaults to the active provider.
#' @param supplement     Optional path to a supplement `.json` produced by a
#'   chat assistant from the instruction file written by
#'   [ars_copilot_instructions()]. When supplied, NO live LLM calls are made
#'   (even if a key is set): the supplement's bindings fill only the rows the
#'   deterministic pass left unannotated (shell annotations always win;
#'   disagreements are WARN findings) and its per-TLF fields feed the same
#'   enrichment path a live LLM answer would. Every supplement variable
#'   passes the hard ADaM-spec gate. Pre-flight a file with
#'   [ars_validate_supplement()].
#'
#'   Regex is the always-on baseline and the default; the LLM is opt-in (see
#'   `use_llm`). Deterministic and supplement are first-class modes -- the
#'   function never asks for a key nor raises a key-related error or warning in
#'   them; the mode that ran is recorded as a neutral INFO note and in
#'   `extraction_mode` / `_meta.extraction_mode`.
#' @param supplement_trust How a supplement value resolves against the regex on
#'   a conflict. `"fill_gaps"` (default): a supplement value lands only where
#'   the regex left a gap; the shell annotation wins a disagreement.
#'   `"prefer_supplement"`: a validated, spec-gated supplement value overrides
#'   the shell on a conflict, with a WARN recording both and the shell's
#'   original kept as a secondary analysis. The hard ADaM-spec gate is never
#'   bypassed in either mode. Ignored (with a warning) without `supplement`;
#'   recorded at `_meta.supplement_trust`.
#' @param use_llm Opt in to the live LLM tier. Default `FALSE` -- the pipeline
#'   runs regex-only (deterministic) and makes NO live LLM call, *even when an
#'   API key is configured*. Set `TRUE` to use the LLM for annotation
#'   extraction and semantic enrichment when a key is available; with `TRUE`
#'   but no key, the run still degrades silently to deterministic (never an
#'   error). Ignored when `supplement` is given (that path makes no live LLM
#'   calls either).
#' @param spec_column_aliases Optional named list of extra column-name
#'   aliases for the ADaM spec Excel (see `parse_adam_spec()`); useful when
#'   a workbook uses non-standard or non-English headers. Example:
#'   `list(variable = "nom de variable", dataset = "domaine")`.
#' @param extract_with_llm If `TRUE` (default), the LLM re-reads each section's
#'   raw shell cells as the primary annotation reader, separating display label
#'   from variable reference in variant layouts. Every proposed
#'   `DATASET.VARIABLE` is gated against the ADaM spec -- out-of-spec proposals
#'   are rejected and logged as blockers, never shipped. This is a sub-control
#'   of the `llm` tier: it only has any effect when `use_llm = TRUE`. Set
#'   `FALSE` to keep the LLM enrichment pass but skip the LLM extraction pass.
#' @param ship_annotations If `FALSE` (default), programmer annotation lines
#'   found outside the stub cells (e.g. red `Label -> DATASET.VAR` paragraphs
#'   below a table) are kept for row binding and the validation report but are
#'   NEVER emitted into the ARS Footnote display section -- rendered footnotes
#'   then contain only true footnotes. Set `TRUE` to append them to the
#'   footnotes (debug escape hatch).
#' @param heading_patterns Optional character vector of PCRE patterns tried
#'   BEFORE the built-in TLF heading grammars, for sponsor shells whose
#'   headings the built-ins do not recognise. Each pattern must use named
#'   capture groups: `(?<number>...)` (required -- the dotted TLF number),
#'   `(?<type>...)` (optional, matching Table/Figure/Listing; defaults to
#'   Table), and `(?<title>...)` (optional inline title; the title tail is
#'   then decomposed into title/population/source datasets the same way
#'   built-in headings are). Custom patterns are accepted as-is -- the
#'   built-in prose/TOC rejection rules are not applied to them. Not needed
#'   for the built-in formats -- a bare `"Table 14.1.1"`, a colon inline
#'   title `"Table 14.1.1: Title"`, and one-line headings that carry the
#'   title, a dash-separated population, an inline annotation, and a
#'   programming-datasets suffix together.
#'   Example: `"^(?i)Output\\s+(?<number>\\d+(?:\\.\\d+)*)\\s*:\\s*(?<title>.*)$"`.
#' @param validate       If `TRUE` (default), cross-reference annotations
#'   against the ADaM spec and write a validation report.
#' @param report_path    Path for the validation report `.xlsx`. Defaults to
#'   `spec_validation_report.xlsx` in [tempdir()].
#' @param code_dir       Directory for the emitted per-TLF pure-`{cards}` `.R`
#'   deliverables. When `NULL` (default) a `code/` folder next to `output_path`
#'   is used. These scripts are both the human-readable deliverable and the
#'   engine `ars_to_ard()` sources to build the ARD.
#' @param adam_dir       ADaM directory baked into each emitted script's header
#'   (the reader can edit it). Default `"."`.
#' @param verbose        Print progress messages. Default `TRUE`.
#'
#' @return Invisibly returns a named list:
#'   \describe{
#'     \item{`ars_path`}{Path to the generated ARS JSON file.}
#'     \item{`extraction_mode`}{Which tier ran: `"llm"`, `"supplement"`, or
#'       `"deterministic"`. Also stored in the JSON as
#'       `_meta.extraction_mode`.}
#'     \item{`report_path`}{Path to the validation report (if validate=TRUE).}
#'     \item{`adam_spec_path`}{The ADaM spec this run read, so the review
#'       stage can be opened with `edit_ars(result)` alone.}
#'     \item{`code_dir`}{Directory holding the emitted per-TLF `{cards}` `.R`
#'       deliverables.}
#'     \item{`code_paths`}{Named character vector of the emitted `.R` paths
#'       (names = output ids).}
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
#' @section Writing identifiable TLF headings:
#' arsbridge splits the shell into outputs by finding TLF heading paragraphs.
#' For a heading to be recognised reliably, write it as **its own ordinary
#' paragraph** (not inside a text box, shape, table cell, or field code)
#' that **begins with `Table`, `Figure`, or `Listing` followed by the output
#' number**. A title should follow the number. All of these are read:
#'
#' \preformatted{Table 14.1.1
#' Table 14.1.1: Summary of Demographics
#' Table 14.1.1 Summary of Demographics
#' Table 14.1.1 Summary of Demographics - Safety Population ADSL.SAFFL='Y'
#' Table 14.1.1 Demographics - Screened Subjects ADSL.SCRNFL='Y' [PROGRAMMING DATASETS USED: ADSL]}
#'
#' The population, an inline annotation, and a
#' `[PROGRAMMING DATASETS USED: ...]` suffix may all ride on the same line;
#' annotation values may use single quotes, double quotes, or an unquoted
#' number (`ADSL.COHORTN=1`). The **recommended** form for a clean, portable
#' shell is the explicit colon title -- `Table 14.1.1: Descriptive Title` --
#' with the population on the next line.
#'
#' These are deliberately **not** treated as headings, to avoid false splits:
#' prose that mentions a number (`Table 14.1.1 shows ...`), cross-references
#' (`See Table 14.1.1 ...`), table-of-contents lines, and bare section
#' numbers with no designator (`14.1 Demographic and Baseline Tables`). When
#' the parser finds no heading, or finds a number but no title, it says so
#' and repeats this guidance. For a sponsor template whose headings genuinely
#' differ, pass `heading_patterns` rather than reformatting the shell.
#'
#' @section Column-group headers (annotation-defined column axis):
#' A table's column axis can be defined entirely in its header cells: when
#' two or more headers carry a filter on the SAME variable -- e.g.
#' `Cohort A (N=XX) ADSL.COHORTN=1`, `Cohort B (N=XX) ADSL.COHORTN=2`,
#' `Unknown Cohort (N=XX) ADSL.COHORTN is missing` -- each condition becomes
#' one display column, in shell order. This is how a merged or derived
#' column (an "Unknown" bucket collecting missing values) is produced
#' without changing the ADaM data: the engine derives the grouping in
#' memory from the annotated conditions, identically in the executed ARD
#' and the emitted `{cards}` scripts. Supported condition forms include
#' `=value` (quoted or numeric), `IN ('a','b')`, and `is missing` /
#' `not missing`. Rows matching no column are excluded from the group
#' columns and counted (WARN); a `Total (N=XX) ...` header is read as the
#' overall column, not a group.
#'
#' @section Human review:
#' The generated ARS JSON is a draft. A qualified clinical programmer MUST
#' review it before downstream use. The JSON includes a
#' `_meta.requires_human_review = TRUE` field that consumers can key on.
#'
#' @seealso [ars_copilot_instructions()] and [ars_validate_supplement()] for
#'   the no-API `supplement =` workflow; [set_llm_key()] to configure a live
#'   LLM. Background: `vignette("no-api-access")`.
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
                        sap_path     = NULL,
                        output_path  = file.path(tempdir(), "reporting_event.json"),
                        study_id     = "STUDY-001",
                        study_name   = NULL,
                        model        = NULL,
                        api_key      = NULL,
                        provider     = NULL,
                        supplement   = NULL,
                        supplement_trust = c("fill_gaps", "prefer_supplement"),
                        use_llm      = FALSE,
                        spec_column_aliases = NULL,
                        extract_with_llm = TRUE,
                        ship_annotations = FALSE,
                        heading_patterns = NULL,
                        validate     = TRUE,
                        report_path  = file.path(tempdir(), "spec_validation_report.xlsx"),
                        code_dir     = NULL,
                        adam_dir     = ".",
                        verbose      = TRUE) {

  supplement_trust <- match.arg(supplement_trust)
  if (!identical(supplement_trust, "fill_gaps") && is.null(supplement)) {
    cli::cli_warn(c(
      "{.arg supplement_trust} = {.val {supplement_trust}} has no effect without a {.arg supplement}.",
      "i" = "It is ignored; the run proceeds in the resolved extraction mode."
    ))
  }

  .require_file(shell_path, "shell_path", INPUT_SHELL)
  .require_file(adam_spec_path, "adam_spec_path", INPUT_SPEC)
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

  ## --- Mode resolution: supplement > opt-in LLM > deterministic ------------
  ## Regex is the always-on baseline and the DEFAULT: the LLM is opt-in via
  ## `use_llm = TRUE`. With `use_llm = FALSE` (default) the run is deterministic
  ## and makes no live LLM call even when an API key is configured -- the
  ## package never asks for a key nor raises a key-related error/warning. A
  ## supplement takes precedence over both (it also makes no live LLM calls).
  supp <- NULL
  if (!is.null(supplement)) {
    supp <- read_supplement(supplement)
    extraction_mode <- "supplement"
  } else if (!isTRUE(use_llm)) {
    extraction_mode <- "deterministic"
    provider <- NULL
    api_key  <- NULL
  } else {
    active <- get_active_llm()
    if (is.null(provider)) {
      provider <- active$provider
    }
    if (!is.null(provider)) {
      if (is.null(model)) {
        model <- active$model
      }
      if (is.null(api_key)) {
        api_key <- Sys.getenv(.llm_env_var(provider))
      }
    }
    if (is.null(provider) || is.null(api_key) || !nzchar(api_key)) {
      extraction_mode <- "deterministic"
      provider <- NULL
      api_key  <- NULL
    } else {
      extraction_mode <- "llm"
    }
  }

  if (verbose) cli::cli_h1("arsbridge::spec_to_ars")

  ## Fresh diagnostics for this run -- every parser/LLM/builder fallback is
  ## recorded and lands on the "Diagnostics" sheet of the validation report.
  diag_reset()

  ## Deterministic and supplement are FIRST-CLASS modes, not degraded
  ## fallbacks: with no key the pipeline is meant to run on regex (optionally
  ## Copilot-supplemented), so it must never ask for a key nor raise a
  ## key-related error/warning. The mode is recorded as a neutral INFO note
  ## (provenance for the Diagnostics sheet), never a WARN or blocker.
  if (identical(extraction_mode, "deterministic")) {
    if (verbose) cli::cli_alert_info(
      "Running in deterministic mode (regex extraction, keyword heuristics; no LLM).")
    diag_add(
      stage = "setup", severity = "INFO",
      problem = "Run executed in deterministic mode (regex extraction, no LLM).",
      action = "Deterministic and supplement are fully supported modes -- no API key is required.")
  } else if (identical(extraction_mode, "supplement") && verbose) {
    cli::cli_alert_info("Using supplement {.path {basename(supplement)}} -- no live LLM calls will be made.")
  }

  ## --- Parse inputs --------------------------------------------------
  ## Spec first: the shell parser uses the spec lookup to validate listing
  ## column-header variable candidates.
  if (verbose) cli::cli_alert_info("Parsing ADaM spec {.path {basename(adam_spec_path)}}...")
  spec <- parse_adam_spec(adam_spec_path, column_aliases = spec_column_aliases)

  if (verbose) cli::cli_alert_info("Parsing annotated shell {.path {basename(shell_path)}}...")
  sections <- parse_shell_docx(shell_path, spec_lookup = spec$lookup,
                               heading_patterns = heading_patterns,
                               progress = verbose)
  if (length(sections) == 0) {
    ## The parser has already said WHY each heading-shaped line was
    ## rejected; repeat those reasons in the abort so they survive into
    ## non-interactive logs, and point at the escape hatch.
    near_misses <- attr(sections, "near_misses") %||% list()
    miss_bullets <- vapply(near_misses, function(nm) {
      gsub("}", "}}",
           gsub("{", "{{",
                sprintf("%s -- %s", dQuote(nm$text, q = FALSE), nm$reason),
                fixed = TRUE),
           fixed = TRUE)
    }, character(1))
    if (length(miss_bullets) > 0) {
      names(miss_bullets) <- rep("x", length(miss_bullets))
    }
    cli::cli_abort(c(
      "No TLF sections found in {.path {shell_path}}.",
      miss_bullets,
      "i" = .RECOMMENDED_HEADING_HINT,
      "i" = "If one of these IS a real heading in an unusual format, pass a custom {.arg heading_patterns} pattern (see {.code ?spec_to_ars})."
    ))
  }

  ## --- Annotation gap-filling on top of the deterministic pass --------------
  ## llm mode: the LLM re-reads each section's raw cells to separate label
  ##   from variable in variant layouts.
  ## supplement mode: the chat-assistant supplement's label-keyed bindings
  ##   land only on rows the regex left unannotated.
  ## Either way every proposed variable passes a hard ADaM-spec gate;
  ## hallucinations are rejected and logged. Deterministic mode skips this.
  if (identical(extraction_mode, "llm") && isTRUE(extract_with_llm)) {
    if (verbose) cli::cli_alert_info("Extracting annotations with {toupper(provider)} (spec-gated)...")
    sections <- lapply(sections, function(sec)
      extract_shell_llm(sec, spec_lookup = spec$lookup,
                        provider = provider, model = model, api_key = api_key))
  } else if (identical(extraction_mode, "supplement")) {
    if (verbose) {
      win <- if (identical(supplement_trust, "prefer_supplement"))
               "supplement values win conflicts" else "shell annotations win"
      cli::cli_alert_info("Applying supplement (spec-gated, {win})...")
    }
    sections <- lapply(sections, function(sec)
      .apply_supplement_bindings(sec, .match_supplement_tlf(supp, sec$tlf_number),
                                 spec$lookup, trust = supplement_trust))
    ## Cross-check the supplement's table inventory against what was parsed:
    ## supplement entries with no matching table, tables the supplement never
    ## mentions, and title disagreements -- so the user can confirm arsbridge
    ## is using the correct set of tables. All non-blocking.
    for (f in .supplement_crosscheck(supp, sections)) {
      diag_add(
        stage = "supplement", severity = f$severity, input = INPUT_SUPPLEMENT,
        problem = f$problem, tlf_number = f$tlf_number, action = f$action
      )
    }
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

  ## --- Semantic enrichment, one unit per TLF -------------------------
  ## llm: one live call per section; supplement: the per-TLF answer from the
  ## supplement through the same path; deterministic: heuristics only.
  if (verbose) {
    src_label <- switch(extraction_mode,
      llm        = sprintf("with %s (%s)", toupper(provider), model),
      supplement = "from the supplement",
      "with keyword heuristics")
    cli::cli_alert_info("Enriching {length(sections)} TLF section{?s} {src_label}...")
  }
  enriched <- vector("list", length(sections))
  for (i in seq_along(sections)) {
    sec <- sections[[i]]
    if (verbose) {
      cli::cli_alert("  [{i}/{length(sections)}] {.val {sec$tlf_number}}: {substr(sec$title, 1, 60)}")
    }
    enriched[[i]] <- switch(extraction_mode,
      llm = enrich_with_llm(sec, spec_lookup = spec$lookup,
                            model = model, api_key = api_key,
                            provider = provider),
      supplement = enrich_with_llm(sec, spec_lookup = spec$lookup,
                                   courier_answers = .supplement_enrich_answer(
                                     .match_supplement_tlf(supp, sec$tlf_number))),
      enrich_with_llm(sec, spec_lookup = spec$lookup, offline = TRUE))
  }

  ## One summary finding for a wholesale LLM outage (almost always a bad/expired
  ## API key or model id), instead of one identical FAIL per TLF.
  n_llm_fail <- .diag_llm_fail_count()
  if (n_llm_fail > 0) {
    .diag_gap(
      stage = "enrich_llm", severity = "FAIL", input = INPUT_LLM,
      problem = sprintf("LLM enrichment was unavailable for %d of %d TLF section%s (provider %s, model %s).",
                        n_llm_fail, length(sections),
                        if (length(sections) == 1) "" else "s",
                        provider %||% "?", model %||% "default"),
      why = "Those sections fell back to keyword heuristics, so analysis type / method / grouping may be less accurate.",
      fix = "Check the API key and model id (set_anthropic_key() or the app's key field), then re-run for full enrichment."
    )
    if (verbose) {
      cli::cli_alert_warning(
        "LLM unavailable for {n_llm_fail}/{length(sections)} TLF{?s} -- ran on keyword heuristics.")
    }
  }

  ## --- Capability gate -----------------------------------------------
  ## Flag TLFs whose analysis is beyond arsbridge's descriptive {cards}
  ## methods (inferential / model-based). These are NOT executed into an ARD
  ## (which would coerce them into a meaningless count); they are carried to
  ## the final output as a numbered placeholder, and raised as blockers so the
  ## programmer knows to produce them manually.
  n_unsupported <- 0L
  for (i in seq_along(enriched)) {
    cap <- assess_capability(enriched[[i]])
    if (!cap$supported) {
      enriched[[i]]$unsupported        <- TRUE
      enriched[[i]]$unsupported_reason <- cap$reason
      n_unsupported <- n_unsupported + 1L
      .diag_gap(
        stage = "capability", severity = "FAIL", input = INPUT_CAPABILITY,
        problem = sprintf("Table %s cannot be generated by arsbridge: %s.",
                          enriched[[i]]$tlf_number, cap$reason),
        why = "arsbridge builds descriptive summaries, counts, AE frequencies, subject counts, listings, and basic figures; it has no method for inferential or model-based analyses.",
        fix = "Produce this table from a separate validated analysis script. arsbridge emits a numbered placeholder for it in the final output so the table numbering still matches your shell. See adr/0001-statistical-method-extensibility.md for why this boundary exists and how it is extended.",
        tlf_number = enriched[[i]]$tlf_number,
        location = enriched[[i]]$title %||% "")
    }
  }
  if (n_unsupported > 0 && verbose) {
    cli::cli_alert_warning(
      "{n_unsupported} table{?s} beyond arsbridge's methods -- placeholder{?s} will be emitted; produce manually.")
  }

  ## --- SAP enrichment (optional) -------------------------------------
  ## Match SAP prose per TLF and attach it to the enriched section; .build_analysis
  ## persists it as sapDescription, which the emitter prints as the block comment.
  if (!is.null(sap_path) && nzchar(sap_path)) {
    sap_df <- tryCatch(parse_sap_docx(sap_path), error = function(e) {
      cli::cli_warn("Could not parse SAP {.path {sap_path}}: {conditionMessage(e)}")
      NULL
    })
    n_sap <- 0L
    if (!is.null(sap_df)) {
      for (i in seq_along(enriched)) {
        txt <- match_sap_section(sap_df, enriched[[i]]$tlf_number,
                                 enriched[[i]]$title)
        if (!is.na(txt)) {
          enriched[[i]]$sap_text <- .clip_sap(txt)
          n_sap <- n_sap + 1L
        }
      }
    }
    if (verbose) {
      cli::cli_alert_info("Matched SAP text to {n_sap}/{length(enriched)} TLF{?s}.")
    }
  }

  ## --- Build and write ARS JSON --------------------------------------
  if (verbose) cli::cli_alert_info("Building CDISC ARS v1.0 ReportingEvent...")
  re <- build_ars_json(enriched, study_id = study_id,
                       study_name = study_name %||% study_id,
                       spec_lookup = spec$lookup,
                       codelists = spec$codelists,
                       ship_annotations = ship_annotations,
                       extraction_mode = extraction_mode,
                       supplement_trust = if (identical(extraction_mode, "supplement"))
                         supplement_trust else NULL)

  json_text <- jsonlite::toJSON(re, auto_unbox = TRUE, pretty = TRUE, null = "null")
  .write_text(json_text, output_path, "the ARS JSON", useBytes = TRUE)

  if (verbose) {
    cli::cli_alert_success("Wrote ARS JSON to {.path {output_path}}")
  }

  ## --- Emit pure-{cards} deliverables --------------------------------
  ## One self-contained <TLF>.R per output. This is the final deliverable AND
  ## the code ars_to_ard() sources to compute the ARD. Read back the written
  ## JSON so emission parses the spec exactly as the engine will.
  if (is.null(code_dir)) code_dir <- file.path(dirname(output_path), "code")
  code_paths <- tryCatch(
    write_tlf_code(output_path, code_dir, adam_dir = adam_dir,
                   log = if (verbose) function(m) cli::cli_alert(m) else NULL),
    error = function(e) {
      cli::cli_warn("Could not emit {.path code/} scripts: {conditionMessage(e)}")
      character(0)
    }
  )
  if (verbose && length(code_paths)) {
    cli::cli_alert_success("Emitted {length(code_paths)} {.path .R} deliverable{?s} to {.path {code_dir}}")
  }

  ## --- Diagnostics summary + report ----------------------------------
  diagnostics <- diag_records()
  blockers    <- ars_blockers(diagnostics)
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
  if (nrow(blockers) > 0) {
    cli::cli_alert_danger(
      paste0("{nrow(blockers)} blocking gap{?s} mean the ARS/ARD/R code may be ",
             "incomplete -- inspect with {.code ars_blockers()} and fix the named input{?s}.")
    )
  }
  if (isTRUE(validate)) {
    write_validation_report(validation, report_path,
                            diagnostics = diagnostics, blockers = blockers)
  }

  result <- list(
    ars_path        = output_path,
    extraction_mode = extraction_mode,
    report_path     = if (isTRUE(validate)) report_path else NULL,
    ## Carried so the review stage can wire up spec-driven dropdowns and
    ## spec validation from the result alone: edit_ars(result).
    adam_spec_path  = adam_spec_path,
    code_dir        = code_dir,
    code_paths      = code_paths,
    n_tlfs          = length(enriched),
    n_analyses      = length(re$analyses),
    n_warnings      = if (!is.null(validation))
                        sum(validation$status %in% c("WARN", "FAIL")) else 0L,
    reporting_event = re,
    validation      = validation,
    diagnostics     = diagnostics,
    blockers        = blockers
  )
  invisible(result)
}
