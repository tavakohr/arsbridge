## arsbridge -- extract_shell_llm.R
## ---------------------------------------------------------------------------
## LLM-PRIMARY annotation extraction.
##
## The deterministic 4-layer detector in parse_shell_docx.R handles KNOWN
## annotation conventions (bracketed / coloured / "DATASET.VARIABLE" text).
## Future shells arrive in unknown layouts where a regex cannot reliably tell
## the display label from the variable annotation. For those, the LLM reads
## the raw cell and separates label vs variable vs where-clause.
##
## Design (decided with the package owner):
##   * LLM is the PRIMARY reader of each annotated section.
##   * The deterministic regex result is kept as a CROSS-CHECK, not the source
##     of truth: agreement raises confidence, disagreement raises a WARN.
##   * Every LLM-proposed "DATASET.VARIABLE" passes a HARD SPEC GATE: if it is
##     not in the ADaM spec it is REJECTED (dropped, not shipped) and logged as
##     a blocking finding for human review. The spec is the hallucination
##     seatbelt -- the model can only pick variables that actually exist.
##   * No API key -> DEGRADED mode: keep the deterministic regex result and
##     emit one WARN. The pipeline still runs offline / in CI.
## ---------------------------------------------------------------------------

#' ellmer type for one extraction pass over a TLF section.
#'
#' One entry per shell row the model judges to carry a variable annotation.
#' `row_index` ties the entry back to `section$stub_rows[[row_index]]`.
#' @noRd
.extract_type <- function() {
  ellmer::type_object(
    .description = paste(
      "Variable annotations extracted from the rows of one TLF shell",
      "section. Separate the human display label from the machine variable",
      "reference. Only include rows that actually reference an ADaM",
      "dataset variable."),
    rows = ellmer::type_array(
      items = ellmer::type_object(
        row_index = ellmer::type_integer(
          "1-based index of the shell row this annotation came from."),
        display_label = ellmer::type_string(
          "The human-readable row label only, with the variable reference removed."),
        dataset = ellmer::type_string(
          "ADaM dataset name of the referenced variable (e.g. ADSL, ADAE). Must be a real dataset in available_variables.",
          required = FALSE),
        variable = ellmer::type_string(
          "ADaM variable name (e.g. AGE, AEDECOD). Must be a real variable in available_variables. Never invent one.",
          required = FALSE),
        where_clause = ellmer::type_string(
          "Optional row-level filter exactly as expressed in the cell (e.g. \"AEREL='RELATED'\"); omit when none.",
          required = FALSE),
        confidence = ellmer::type_enum(
          values = c("high", "medium", "low"),
          description = "How sure you are this row carries this variable.",
          required = FALSE)
      ),
      description = "One entry per annotated row; omit rows with no variable.",
      required = FALSE
    )
  )
}

#' Render the extraction prompt for one section.
#'
#' Uses the same `<<>>` glue delimiters as the enrichment prompt so literal
#' braces in shell text do not break interpolation.
#' @noRd
.render_extract_prompt <- function(section, spec_vars) {
  rows <- section$stub_rows %||% list()
  row_lines <- vapply(seq_along(rows), function(i) {
    sprintf("  [%d] %s", i,
            gsub("\n", " | ", as.character(rows[[i]]$raw_text %||%
                                             rows[[i]]$label %||% "")))
  }, character(1))

  glue::glue(
    .open = "<<", .close = ">>",
    "You are reading one section of an annotated clinical TLF shell. Each row\n",
    "below is the raw text of a table stub cell (or listing header). The lead\n",
    "programmer has embedded ADaM variable references among the display text,\n",
    "in an unknown layout. For every row that references an ADaM variable,\n",
    "return: the display label with the variable reference stripped out, the\n",
    "dataset, the variable, and any row-level where-clause.\n\n",
    "Rules:\n",
    "- Use ONLY datasets/variables that appear in available_variables.\n",
    "- Never invent a variable. If a row has no real variable reference, omit it.\n",
    "- Keep the display_label human-readable (drop brackets / the DATASET.VAR token).\n\n",
    "TLF: <<section$tlf_number>> (<<section$tlf_type>>)\n",
    "Title: <<section$title>>\n\n",
    "available_variables (DATASET.VARIABLE):\n<<paste(spec_vars, collapse = ', ')>>\n\n",
    "rows:\n<<paste(row_lines, collapse = '\n')>>"
  )
}

#' LLM-primary extraction over one parsed section.
#'
#' @param section     One section from `parse_shell_docx()`.
#' @param spec_lookup `"DATASET.VARIABLE"`-keyed spec lookup (the gate oracle).
#' @param provider,model,api_key  LLM routing (defaults to the active provider).
#' @param call_fn     Injectable structured-call function (for tests); defaults
#'   to `.enrich_structured`.
#'
#' @return The section with `stub_rows` updated in place: each row's `label`,
#'   `annotation`, `has_annot`, `detection_method`, `detection_confidence`
#'   reflect the LLM extraction after the spec gate. Rejected (out-of-spec)
#'   proposals leave the deterministic result untouched and log a blocker.
#'
#' @keywords internal
#' @noRd
## Normalised stub labels that are statistic sub-lines of the row above,
## never analyses of their own (compared after .norm_label()).
.STATLINE_ROW_LABELS <- c(
  "mean sd", "mean", "sd", "median", "min max", "min", "max",
  "q1 q3", "q1", "q3", "n", "se", "cv", "geometric mean", "n missing"
)

extract_shell_llm <- function(section, spec_lookup = NULL,
                              provider = NULL, model = NULL, api_key = NULL,
                              call_fn = .enrich_structured) {
  rows <- section$stub_rows %||% list()
  if (length(rows) == 0) return(section)

  ## --- Degraded (keyless) mode: keep deterministic regex result. ----------
  active <- get_active_llm()
  provider <- provider %||% active$provider
  if (is.null(provider)) {
    .diag_gap(
      stage = "extract_llm", severity = "WARN", input = INPUT_LLM,
      problem = sprintf("No LLM key set; section %s read with deterministic regex only.",
                        section$tlf_number),
      why = "Variant shell layouts need an LLM to separate label from variable; regex only recognises known conventions.",
      fix = "Set an API key (set_anthropic_key() / set_llm_key()) to enable variant-format extraction.",
      tlf_number = section$tlf_number, location = section$title %||% "")
    return(section)
  }
  model   <- model   %||% active$model
  api_key <- api_key %||% Sys.getenv(.llm_env_var(provider))

  spec_keys <- toupper(names(spec_lookup %||% list()))

  ## Variables already bound to a row by a HIGH-confidence deterministic
  ## detection (in-cell colour / below-table arrow). An LLM proposal that
  ## re-reads one of these onto an UNANNOTATED row without any filter is a
  ## duplicate of the same shell annotation (e.g. the parenthesised
  ## "(End-of-study status)" caption re-annotated with the EOSSTT variable
  ## already carried by the Completed/Discontinued rows) -- it would expand
  ## a block the authored rows already show.
  det_refs <- unique(toupper(unlist(lapply(rows, function(r) {
    if (!isTRUE(r$has_annot)) return(NULL)
    if (!identical(r$detection_confidence, "high")) return(NULL)
    v <- extract_annotation_vars(r$annotation %||% "")
    if (length(v) > 0) v[1] else NULL
  }))))

  prompt    <- .render_extract_prompt(section, spec_keys)

  parsed <- call_fn(prompt, .extract_type(), provider = provider,
                    model = model, api_key = api_key)
  proposals <- parsed$rows %||% list()
  if (length(proposals) == 0) {
    ## Whole-section extraction failure: count it (spec_to_ars emits one
    ## summary), keep the deterministic result.
    .diag_llm_fail_bump()
    return(section)
  }

  for (p in proposals) {
    idx <- suppressWarnings(as.integer(p$row_index %||% NA))
    if (is.na(idx) || idx < 1 || idx > length(rows)) next

    ## A stub row whose label is a bare statistic line ("Mean (SD)",
    ## "Median", "Min, Max", ...) is a layout sub-row of the analysis row
    ## above it, not an analysis of its own -- annotating it would create a
    ## duplicate analysis block (ADR 0003). Leave it label-only; the
    ## layout-driven renderer fills it from the parent analysis.
    if (.norm_label(rows[[idx]]$label %||% "") %in% .STATLINE_ROW_LABELS) next

    ds  <- toupper(trimws(p$dataset  %||% ""))
    var <- toupper(trimws(p$variable %||% ""))
    if (!nzchar(ds) || !nzchar(var)) next
    ref <- paste0(ds, ".", var)

    ## --- HARD SPEC GATE -----------------------------------------------------
    if (!ref %in% spec_keys) {
      .diag_gap(
        stage = "extract_llm", severity = "FAIL", input = INPUT_SHELL,
        problem = sprintf("Proposed variable %s for row '%s' is not in the ADaM spec; rejected.",
                          ref, rows[[idx]]$label %||% p$display_label %||% ""),
        why = "An LLM-proposed variable absent from the ADaM spec is treated as a hallucination, never shipped.",
        fix = sprintf("Confirm the intended variable for this row and either fix the shell annotation or add %s to the ADaM spec.", ref),
        tlf_number = section$tlf_number, location = rows[[idx]]$label %||% "")
      next
    }

    ## Build the annotation string (variable + optional where-clause).
    where <- trimws(p$where_clause %||% "")
    annotation <- if (nzchar(where)) paste0(ref, " WHERE ", where) else ref

    ## Bare re-read of a variable another row already carries with high
    ## deterministic confidence -> duplicate annotation, not a new analysis.
    ## (A proposal WITH a filter -- e.g. SEX='M' for a "Male" row -- is a
    ## genuine distinct analysis and is kept.)
    if (!nzchar(where) && !isTRUE(rows[[idx]]$has_annot) &&
        ref %in% det_refs) {
      diag_add(
        stage = "extract_llm", severity = "INFO", input = INPUT_SHELL,
        problem = sprintf("Row '%s': proposed %s duplicates a variable already bound to another row; left label-only",
                          rows[[idx]]$label %||% "", ref),
        tlf_number = section$tlf_number,
        action = "Caption/spacer rows keep their layout position without their own analysis"
      )
      next
    }

    ## --- CROSS-CHECK vs the deterministic regex result. ---------------------
    regex_annot <- toupper(trimws(rows[[idx]]$annotation %||% ""))
    if (nzchar(regex_annot) && !startsWith(regex_annot, ref)) {
      .diag_gap(
        stage = "extract_llm", severity = "WARN", input = INPUT_SHELL,
        problem = sprintf("Row '%s': LLM read %s but the text pattern matched %s.",
                          rows[[idx]]$label %||% "", ref, regex_annot),
        why = "LLM and deterministic parser disagree on this row's variable.",
        fix = "Review this row in the shell; the annotation may be ambiguous.",
        tlf_number = section$tlf_number, location = rows[[idx]]$label %||% "")
    }

    ## A high-confidence deterministic binding (in-cell colour or a bound
    ## below-table arrow line, ADR 0003) is authored ground truth -- the LLM
    ## pass must not overwrite it: the rewrite typically drops the value
    ## filter (ADSL.EOSSTT='COMPLETED' -> ADSL.EOSSTT), collapsing distinct
    ## authored rows onto one variable. Disagreement is already logged above.
    if (isTRUE(rows[[idx]]$has_annot) &&
        identical(rows[[idx]]$detection_confidence, "high") &&
        (rows[[idx]]$detection_method %||% "") %in%
          c("colour", "below_table_arrow")) {
      next
    }

    rows[[idx]]$label                <- trimws(p$display_label %||% rows[[idx]]$label %||% "")
    rows[[idx]]$annotation           <- annotation
    rows[[idx]]$has_annot            <- TRUE
    rows[[idx]]$detection_method     <- "llm"
    rows[[idx]]$detection_confidence <- p$confidence %||% "medium"
  }

  section$stub_rows <- rows
  section
}
