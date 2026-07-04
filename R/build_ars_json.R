## arsbridge -- build_ars_json.R
## ---------------------------------------------------------------------------
## Assembles enriched TLF sections into a CDISC ARS v1.0 ReportingEvent
## object suitable for jsonlite serialisation AND consumable by
## siera::readARS().
##
## Siera (pharmaverse) reads ARS JSON via .read_ars_json_metadata() which
## demands a specific field shape. Where the ARS v1.0 spec and siera's
## shape diverge, we emit BOTH:
##
##   - Flat fields (siera-required): dataset / variable / groupingDataset
##     / groupingVariable / level / order on every set.
##   - Nested fields (ARS-correct): analysisVariable / condition objects
##     kept alongside so other ARS consumers (cards, ARS Excel exporters)
##     still see the structured form.
##
## Two table-of-contents structures (otherListsOfContents = LOPO,
## mainListOfContents = LOPA) are emitted in the format siera demands,
## because if either is missing, siera silently writes nothing.

## Per-operation placeholder for `referencedOperationRelationships`. siera
## reads `json_from$methods$operations[[i]]$referencedOperationRelationships`
## and merges by `operation_id` -- an entirely empty JSONAML3 crashes the
## merge ('by' must specify a uniquely valid column). Self-reference is
## harmless: siera uses these only for NUM/DEN cross-operation lookups.
.op_self_rel <- function(op_id) {
  list(list(
    id          = paste0(op_id, "_SELF"),
    operationId = op_id,
    description = "",
    referencedOperationRole = list(controlledTerm = "")
  ))
}

#' Attach a placeholder `referencedOperationRelationships` to each operation
#' of a method spec. Mutates a copy of the spec; original is left alone.
#' @noRd
.with_op_self_rels <- function(method_spec) {
  method_spec$operations <- lapply(method_spec$operations, function(op) {
    op$referencedOperationRelationships <- .op_self_rel(op$id)
    op
  })
  method_spec
}

## ---------------------------------------------------------------------------
## Standard AnalysisMethod catalogue. Each entry includes a `codeTemplate`
## block (code + parameters) so siera can expand a runnable R script per
## analysis. Templates use siera's substitution placeholders -- siera
## replaces `analysisidhere` -> Analysis.id, `methodidhere` -> Method.id,
## and each parameter `name` -> the value resolved from `valueSource`
## (`ana_var`, `AG_var1`, `by_vars`, etc., populated by siera's readARS()
## loop). Templates write into `df3_analysisidhere` because siera's
## post-processing then attaches AnalysisId/MethodId/OutputId columns.
##
## Templates here are intentionally minimal but RUNNABLE -- the lead
## programmer is expected to refine them per study, but the generated
## ARD_*.R files will at least parse and execute on the bundled
## 60-subject example data.
## ---------------------------------------------------------------------------

## Long character vectors here for readability; collapse with newlines on use.
.STANDARD_METHODS <- list(
  "Summary Statistics - Continuous" = list(
    id          = "MTH_SUMMARY_STATISTICS_CONTINUOUS",
    name        = "Summary Statistics - Continuous",
    label       = "Summary Statistics - Continuous",
    description = "n, mean, SD, median, Q1, Q3, min, max",
    operations = list(
      list(id = "OP_N",      name = "n",      label = "n",      order = 1L, resultPattern = "XXX"),
      list(id = "OP_MEAN",   name = "Mean",   label = "Mean",   order = 2L, resultPattern = "XXX.X"),
      list(id = "OP_SD",     name = "SD",     label = "SD",     order = 3L, resultPattern = "XXX.XX"),
      list(id = "OP_MEDIAN", name = "Median", label = "Median", order = 4L, resultPattern = "XXX.X"),
      list(id = "OP_Q1",     name = "Q1",     label = "Q1",     order = 5L, resultPattern = "XXX.X"),
      list(id = "OP_Q3",     name = "Q3",     label = "Q3",     order = 6L, resultPattern = "XXX.X"),
      list(id = "OP_MIN",    name = "Min",    label = "Min",    order = 7L, resultPattern = "XXX"),
      list(id = "OP_MAX",    name = "Max",    label = "Max",    order = 8L, resultPattern = "XXX")
    ),
    codeTemplate = list(
      context = "R (siera)",
      code = paste(
        "df3_analysisidhere <- df2_analysisidhere |>",
        "  dplyr::select(USUBJID, anavarhere) |>",
        "  unique() |>",
        "  dplyr::summarise(",
        "    OP_N      = sum(!is.na(anavarhere)),",
        "    OP_MEAN   = mean(anavarhere, na.rm = TRUE),",
        "    OP_SD     = stats::sd(anavarhere, na.rm = TRUE),",
        "    OP_MEDIAN = stats::median(anavarhere, na.rm = TRUE),",
        "    OP_Q1     = stats::quantile(anavarhere, 0.25, na.rm = TRUE),",
        "    OP_Q3     = stats::quantile(anavarhere, 0.75, na.rm = TRUE),",
        "    OP_MIN    = suppressWarnings(min(anavarhere, na.rm = TRUE)),",
        "    OP_MAX    = suppressWarnings(max(anavarhere, na.rm = TRUE))",
        "  ) |>",
        "  tidyr::pivot_longer(dplyr::everything(), names_to = 'operation', values_to = 'res') |>",
        "  dplyr::mutate(pattern = 'XXX.X')",
        sep = "\n"
      ),
      parameters = list(
        list(name = "anavarhere", valueSource = "ana_var",
             description = "Analysis variable name (resolved from Analyses.variable)")
      )
    )
  ),
  "Count and Percentage" = list(
    id          = "MTH_COUNT_AND_PERCENTAGE",
    name        = "Count and Percentage",
    label       = "Count and Percentage",
    description = "n (%) per category",
    operations = list(
      list(id = "OP_N",     name = "Count",       label = "Count",       order = 1L, resultPattern = "XXX"),
      list(id = "OP_PCT",   name = "Percentage",  label = "Percentage",  order = 2L, resultPattern = "XX.X"),
      list(id = "OP_DENOM", name = "Denominator", label = "Denominator", order = 3L, resultPattern = "XXX")
    ),
    codeTemplate = list(
      context = "R (siera)",
      code = paste(
        "denom_n <- length(unique(df2_analysisidhere$USUBJID))",
        "df3_analysisidhere <- df2_analysisidhere |>",
        "  dplyr::group_by(anavarhere) |>",
        "  dplyr::summarise(",
        "    OP_N     = dplyr::n_distinct(USUBJID),",
        "    OP_DENOM = denom_n,",
        "    .groups  = 'drop'",
        "  ) |>",
        "  dplyr::mutate(OP_PCT = 100 * OP_N / OP_DENOM) |>",
        "  tidyr::pivot_longer(",
        "    dplyr::starts_with('OP_'),",
        "    names_to  = 'operation',",
        "    values_to = 'res'",
        "  ) |>",
        "  dplyr::mutate(pattern = 'XXX')",
        sep = "\n"
      ),
      parameters = list(
        list(name = "anavarhere", valueSource = "ana_var",
             description = "Categorical variable being counted")
      )
    )
  ),
  "Subject Count" = list(
    id          = "MTH_SUBJECT_COUNT",
    name        = "Subject Count",
    label       = "Subject Count",
    description = "Unique subject count",
    operations = list(
      list(id = "OP_N", name = "n", label = "n", order = 1L, resultPattern = "XXX")
    ),
    codeTemplate = list(
      context = "R (siera)",
      code = paste(
        "df3_analysisidhere <- df2_analysisidhere |>",
        "  dplyr::distinct(USUBJID) |>",
        "  dplyr::summarise(res = dplyr::n()) |>",
        "  dplyr::mutate(operation = 'OP_N', pattern = 'XXX')",
        sep = "\n"
      ),
      parameters = list()
    )
  ),
  "Kaplan-Meier Estimate" = list(
    id          = "MTH_KAPLAN_MEIER_ESTIMATE",
    name        = "Kaplan-Meier Estimate",
    label       = "Kaplan-Meier Estimate",
    description = "KM event rate, median survival, 95% CI",
    operations = list(
      list(id = "OP_EVENTS",  name = "Events",          label = "Events",          order = 1L, resultPattern = "XXX"),
      list(id = "OP_MEDIAN",  name = "Median (months)", label = "Median (months)", order = 2L, resultPattern = "XXX.X"),
      list(id = "OP_CI_LOW",  name = "95% CI Lower",    label = "95% CI Lower",    order = 3L, resultPattern = "XXX.X"),
      list(id = "OP_CI_HIGH", name = "95% CI Upper",    label = "95% CI Upper",    order = 4L, resultPattern = "XXX.X")
    ),
    codeTemplate = list(
      context = "R (siera)",
      code = paste(
        "## Kaplan-Meier template (placeholder -- refine per study).",
        "## Expects ADTTE-style df with AVAL (time) and CNSR (1=censored).",
        "df3_analysisidhere <- data.frame(",
        "  operation = c('OP_EVENTS', 'OP_MEDIAN', 'OP_CI_LOW', 'OP_CI_HIGH'),",
        "  res = c(",
        "    sum(df2_analysisidhere$CNSR == 0, na.rm = TRUE),",
        "    suppressWarnings(stats::median(df2_analysisidhere$AVAL, na.rm = TRUE)),",
        "    NA_real_, NA_real_",
        "  ),",
        "  pattern = c('XXX', 'XXX.X', 'XXX.X', 'XXX.X')",
        ")",
        sep = "\n"
      ),
      parameters = list()
    )
  ),
  "AE Frequency Count" = list(
    id          = "MTH_AE_FREQUENCY_COUNT",
    name        = "AE Frequency Count",
    label       = "AE Frequency Count",
    description = "Unique subjects with event, n (%)",
    operations = list(
      list(id = "OP_N",   name = "n",   label = "n",   order = 1L, resultPattern = "XXX"),
      list(id = "OP_PCT", name = "(%)", label = "(%)", order = 2L, resultPattern = "XX.X")
    ),
    codeTemplate = list(
      context = "R (siera)",
      code = paste(
        "denom_n <- length(unique(df2_analysisidhere$USUBJID))",
        "df3_analysisidhere <- df2_analysisidhere |>",
        "  dplyr::distinct(USUBJID, anavarhere) |>",
        "  dplyr::group_by(anavarhere) |>",
        "  dplyr::summarise(OP_N = dplyr::n(), .groups = 'drop') |>",
        "  dplyr::mutate(OP_PCT = 100 * OP_N / denom_n) |>",
        "  tidyr::pivot_longer(",
        "    dplyr::starts_with('OP_'),",
        "    names_to  = 'operation',",
        "    values_to = 'res'",
        "  ) |>",
        "  dplyr::mutate(pattern = 'XXX')",
        sep = "\n"
      ),
      parameters = list(
        list(name = "anavarhere", valueSource = "ana_var",
             description = "Event-categorising variable (e.g. AEDECOD)")
      )
    )
  ),
  "Listing" = list(
    id          = "MTH_LISTING",
    name        = "Listing",
    label       = "Listing",
    description = "Subject-level data listing",
    operations  = list(
      list(id = "OP_PASS", name = "Passthrough", label = "Passthrough", order = 1L, resultPattern = "X")
    ),
    codeTemplate = list(
      context = "R (siera)",
      code = paste(
        "## Listing: pass through the filtered ADaM rows as-is.",
        "df3_analysisidhere <- df2_analysisidhere |>",
        "  dplyr::mutate(operation = 'OP_PASS', res = NA_real_, pattern = 'X')",
        sep = "\n"
      ),
      parameters = list()
    )
  )
)

#' Infer the analysis method for one stub row from its bound annotation form
#' (ADR 0003 Layer C). Deterministic: the annotation is authored ground truth,
#' so it overrides the section-level LLM method for this row.
#'
#' @param row  Stub row (needs `annotation`).
#' @param var_is_categorical NA/TRUE/FALSE -- the spec's verdict on the row's
#'   primary variable (from `.var_is_categorical`).
#' @return list(method = standard-catalogue name, kind = layout kind), or
#'   NULL when the form is unrecognised (caller keeps the section default).
#' @noRd
.infer_row_method <- function(row, var_is_categorical = NA) {
  ann <- as.character(row$annotation %||% "")
  if (!nzchar(trimws(ann))) return(NULL)
  ## Count expression or a bare USUBJID reference -> distinct subject count.
  if (grepl("(?i)\\bcount\\s+of\\b|(?i)\\bunique\\s+USUBJID\\b", ann, perl = TRUE) ||
      grepl(paste0("\\b", .ADAM_DS, "\\.USUBJID\\b"), ann, perl = TRUE)) {
    return(list(method = "Subject Count", kind = "subject_count"))
  }
  ## Value filter present. A filter ON the primary variable itself
  ## ("ADSL.SAFFL='Y'") means "count subjects in this state" -> subject count
  ## within the subset. A filter on ANOTHER variable
  ## ("ADEX.AVAL WHERE PARAMCD='DURD'") only scopes the data -- the primary
  ## variable is still summarised by its own type below.
  if (grepl("=\\s*'[^']*'", ann) || grepl("(?i)\\bwhere\\b", ann, perl = TRUE)) {
    primary <- extract_annotation_vars(ann)
    primary <- if (length(primary) > 0) sub("^.*\\.", "", primary[1]) else ""
    fs <- flat_data_subset(ann)
    filter_var <- toupper(fs$variable %||% "")
    if (!nzchar(filter_var) || identical(filter_var, toupper(primary))) {
      return(list(method = "Subject Count", kind = "filtered_count"))
    }
    ## fall through: subset on another variable; type decides the method
  }
  ## Primary variable type (from the ADaM spec) decides the method.
  if (isTRUE(var_is_categorical)) {
    return(list(method = "Count and Percentage", kind = "categorical"))
  }
  if (identical(var_is_categorical, FALSE)) {
    return(list(method = "Summary Statistics - Continuous", kind = "continuous"))
  }
  NULL
}

#' Build a CDISC ARS v1.0 ReportingEvent list from enriched sections.
#'
#' Emits a JSON-ready structure that satisfies BOTH the CDISC ARS v1.0
#' logical model AND `siera::readARS()`'s expected JSON shape. Where the
#' two disagree (flat vs nested fields), we emit both forms.
#'
#' @param sections   List of enriched TLF sections (output of
#'   `enrich_with_llm()` applied to each section).
#' @param study_id   Study identifier.
#' @param study_name Human-readable study name.
#' @param ship_annotations When `TRUE`, programmer annotation lines captured
#'   below the shell tables are appended to each output's Footnote display
#'   section (debug escape hatch). Default `FALSE`: annotations are kept in
#'   the parsed sections / validation report only and never shipped.
#'
#' @return Named list ready for [jsonlite::toJSON()] (use
#'   `auto_unbox = TRUE, pretty = TRUE, null = "null"`).
#'
#' @keywords internal
#' @noRd
build_ars_json <- function(sections,
                           study_id   = "STUDY-001",
                           study_name = NULL,
                           spec_lookup = NULL,
                           ship_annotations = FALSE) {
  if (length(sections) == 0) {
    cli::cli_abort("Cannot build ReportingEvent: no TLF sections provided.")
  }

  ## TRUE when the ADaM spec marks this variable as character-typed or
  ## controlled-terminology (has a codelist) -- i.e. a categorical variable
  ## that must be summarised by counts, never by continuous statistics.
  .var_is_categorical <- function(dataset, variable) {
    if (is.null(spec_lookup) || is.null(variable) || !nzchar(variable)) return(NA)
    variable <- toupper(sub("^.*\\.", "", variable))
    dataset  <- toupper(dataset %||% "")
    rec <- spec_lookup[[paste0(dataset, ".", variable)]]
    if (is.null(rec)) {
      hits <- spec_lookup[vapply(spec_lookup, function(r)
        identical(toupper(r$variable %||% ""), variable), logical(1))]
      if (!length(hits)) return(NA)
      rec <- hits[[1]]
    }
    type <- tolower(as.character(rec$type %||% ""))
    cl   <- as.character(rec$codelist %||% "")
    grepl("char|text|string|^c$", type) || (nzchar(cl) && !is.na(cl))
  }
  count_method_id <- .STANDARD_METHODS[["Count and Percentage"]]$id

  ## TRUE when a "DATASET.VARIABLE" annotation reference is resolvable against
  ## the ADaM spec (exact DATASET.VAR key, or VAR present in any dataset).
  ## Used to warn, per TLF, when a shell references variables the spec lacks.
  ## spec_lookup is named by DATASET.VARIABLE (that's how .var_is_categorical
  ## indexes it). A reference is "developable" only when its exact
  ## dataset+variable key exists -- a same-named variable in a DIFFERENT
  ## dataset must not satisfy "ADSL.WEIGHT" (that would mask a genuine gap).
  .spec_keys_up <- toupper(names(spec_lookup %||% list()))
  .ref_present <- function(ref) toupper(ref) %in% .spec_keys_up

  analysis_sets    <- list(); seen_as  <- character()
  data_subsets     <- list(); seen_ds  <- character()
  grouping_factors <- list(); seen_gf  <- character()
  methods          <- list(); seen_mth <- character()
  analyses         <- list()
  outputs          <- list()
  unsupported      <- list()   ## output_id -> reason, for _meta + placeholders

  for (sec in sections) {
    ## --- Capability-gated (unsupported) section ---------------------------
    ## arsbridge cannot compute these statistics, but -- unlike before -- the
    ## ARS still carries the analysis + a declarative (supported = FALSE) method
    ## so the Output -> Analysis -> Method chain stays intact (ADR 0002 phase 3).
    ## The engine reserves manual_pending stub ARD rows for them; the renderer
    ## still shows a numbered placeholder (recorded in _meta.unsupported_outputs)
    ## until partial rendering lands. Never coerce into a meaningless count.
    sec_unsupported <- isTRUE(sec$unsupported)
    ## Classify which gated statistics arsbridge can now actually compute (ADR
    ## 0001). A gated section with executable methods is built as a *partial*
    ## section: descriptive rows compute, each executable inferential method is
    ## appended as its own analysis, and only the residual is reserved. A gated
    ## section with nothing executable is reserved wholesale (Phase 3).
    cls <- if (sec_unsupported) classify_section_methods(sec) else
      list(executable = list(), residual = character())
    has_exec      <- length(cls$executable) > 0
    gated_generic <- sec_unsupported && !has_exec
    oid_ph        <- make_output_id(sec$tlf_number)
    if (gated_generic) {
      unsupported[[length(unsupported) + 1L]] <- list(
        id     = oid_ph,
        reason = sec$unsupported_reason %||% "not supported by arsbridge")
    } else if (sec_unsupported && length(cls$residual) > 0) {
      ## Partial: some cells compute, but a residual (e.g. a Newcombe interval)
      ## stays reserved -- numbered placeholder text names it.
      unsupported[[length(unsupported) + 1L]] <- list(
        id     = oid_ph,
        reason = paste(cls$residual, collapse = "; "))
    }

    ## --- TLF-level developability check ---
    ## Warn (once per TLF) when the shell references variables the ADaM spec
    ## does not contain -- those rows can't be developed and will be skipped.
    ## Skipped for gated sections: their rows are manual by definition.
    if (!sec_unsupported && !is.null(spec_lookup) && length(spec_lookup)) {
      ann_refs <- unique(unlist(lapply(
        Filter(function(r) isTRUE(r$has_annot), sec$stub_rows),
        function(r) extract_annotation_vars(r$annotation))))
      miss <- ann_refs[!vapply(ann_refs, .ref_present, logical(1))]
      if (length(miss)) {
        cli::cli_warn(
          "TLF {sec$tlf_number}: cannot be fully developed -- {length(miss)} variable{?s} not in the ADaM spec: {.val {miss}}")
        diag_add(
          stage = "build_ars", severity = "WARN", tlf_number = sec$tlf_number,
          location = sec$title %||% "",
          problem = sprintf("TLF %s references %d variable(s) not in the ADaM spec: %s",
                            sec$tlf_number, length(miss), paste(miss, collapse = ", ")),
          action = "These rows will be skipped at execution. Add the variable(s) to the ADaM dataset and spec to develop this TLF fully.")
      }
    }

    ## --- AnalysisSet from population ---
    as_obj <- .build_analysis_set(sec)
    if (!as_obj$id %in% seen_as) {
      as_obj$order <- length(analysis_sets) + 1L
      as_obj$level <- 1L
      analysis_sets[[length(analysis_sets) + 1L]] <- as_obj
      seen_as <- c(seen_as, as_obj$id)
    }

    ## --- GroupingFactors from the (ordered) grouping list ---
    gf_objs <- .build_groupings(sec)
    for (gf_obj in gf_objs) {
      if (!gf_obj$id %in% seen_gf) {
        grouping_factors[[length(grouping_factors) + 1L]] <- gf_obj
        seen_gf <- c(seen_gf, gf_obj$id)
      }
    }
    gf_ids <- vapply(gf_objs, function(g) g$id, character(1))

    ## --- AnalysisMethod (standard catalogue, or declarative-unsupported) ---
    ## A wholesale-gated section's descriptive rows are reserved; a partial
    ## section's descriptive rows compute with the normal method. A LISTING
    ## section's method is structural, not statistical -- force MTH_LISTING
    ## regardless of the LLM's analysis-type guess, otherwise the listing
    ## renderer finds no MTH_LISTING columns and the output degrades to a
    ## placeholder (ADR 0003 Phase 5).
    mth_obj <- if (gated_generic) {
      .build_unsupported_method(sec)
    } else if (identical(sec$tlf_type, "LISTING")) {
      .with_op_self_rels(.STANDARD_METHODS[["Listing"]])
    } else {
      .build_method(sec)
    }
    if (!mth_obj$id %in% seen_mth) {
      methods[[length(methods) + 1L]] <- mth_obj
      seen_mth <- c(seen_mth, mth_obj$id)
    }

    ## --- One Analysis per annotated row; layout entry per authored row ---
    ## For a plain TABLE section every authored stub row is walked in order:
    ## annotated rows become analyses (method inferred from the annotation
    ## form when it is recognisable), label-only rows become layout entries
    ## with no analysis, and no annotated row is ever dropped (an
    ## unresolvable one is reserved as manual_pending). Gated sections and
    ## listings/figures keep the previous annotated-rows-only path.
    build_layout <- identical(sec$tlf_type %||% "TABLE", "TABLE") &&
      !sec_unsupported
    rows_iter <- if (build_layout) sec$stub_rows %||% list() else
      Filter(function(r) isTRUE(r$has_annot), sec$stub_rows)
    enriched_rows  <- sec$enriched_rows %||% list()
    er_by_label    <- setNames(
      enriched_rows,
      vapply(enriched_rows, function(e) e$label %||% "", character(1))
    )

    shell_layout <- list()
    analysis_ids <- character()
    seen_row_sig <- character()
    for (ridx in seq_along(rows_iter)) {
      row <- rows_iter[[ridx]]
      raw <- as.character(row$raw_text %||% "")
      indent <- nchar(regmatches(raw, regexpr("^ *", raw))[[1]] %||% "")

      if (!isTRUE(row$has_annot)) {
        ## Authored label-only row (section header / spacer): persisted in
        ## the layout so the renderer keeps it, but it has no analysis.
        shell_layout[[length(shell_layout) + 1L]] <- list(
          order = length(shell_layout) + 1L,
          label = row$label %||% "", indent = indent,
          analysis_id = NA_character_, kind = "label")
        next
      }

      idx <- length(analysis_ids) + 1L
      er  <- er_by_label[[row$label]] %||% list()
      ## Deterministic safety net: when the LLM enrichment omitted this row,
      ## derive dataset/variable and the subset filter straight from the
      ## bound annotation so the authored row still computes.
      if (!nzchar(er$primary_variable %||% "")) {
        refs <- extract_annotation_vars(row$annotation)
        if (length(refs) > 0) {
          pieces <- strsplit(refs[1], ".", fixed = TRUE)[[1]]
          er$label            <- er$label %||% row$label
          er$primary_dataset  <- pieces[1]
          er$primary_variable <- if (length(pieces) >= 2) pieces[2] else ""
          er$variable_role    <- er$variable_role %||% "ANALYSIS"
        }
      }
      if (is.null(er$data_subset) || length(er$data_subset) == 0) {
        er$data_subset <- flat_data_subset(row$annotation)
      }
      ds_obj <- .build_data_subset(er, sec$tlf_number, idx)
      if (!is.null(ds_obj) && !ds_obj$id %in% seen_ds) {
        ds_obj$order <- length(data_subsets) + 1L
        ds_obj$level <- 1L
        data_subsets[[length(data_subsets) + 1L]] <- ds_obj
        seen_ds <- c(seen_ds, ds_obj$id)
      }

      row_method_id <- mth_obj$id
      row_kind      <- "row"
      cat_verdict   <- .var_is_categorical(er$primary_dataset,
                                           er$primary_variable)
      ## Annotation-form inference applies to TABLE rows only: a listing
      ## column annotated "ADAE.AEDECOD" is a passthrough column, never a
      ## count analysis.
      inferred <- if (build_layout) .infer_row_method(row, cat_verdict) else NULL

      if (build_layout &&
          !nzchar(er$primary_variable %||% "") &&
          !nzchar(er$primary_dataset %||% "")) {
        ## Annotated row whose variable never resolved (ADR 0003 no-drop):
        ## reserve a traceable manual_pending cell instead of dropping the
        ## row. ADSL.USUBJID keys the stub so the engine's dataset/variable
        ## guards pass.
        er$primary_dataset  <- "ADSL"
        er$primary_variable <- "USUBJID"
        row_method_id <- "MTH_UNSUPPORTED_ANALYSIS"
        row_kind      <- "manual"
        if (!"MTH_UNSUPPORTED_ANALYSIS" %in% seen_mth) {
          methods[[length(methods) + 1L]] <- .build_unsupported_method(sec)
          seen_mth <- c(seen_mth, "MTH_UNSUPPORTED_ANALYSIS")
        }
        diag_add(
          stage = "build_ars", severity = "WARN",
          problem = sprintf("Annotated row '%s' has no resolvable variable",
                            row$label %||% "?"),
          tlf_number = sec$tlf_number,
          action = "Reserved as manual_pending so the authored row is kept -- see ars_manual_worklist()"
        )
      } else if (!is.null(inferred)) {
        ## Deterministic method from the annotation form -- overrides the
        ## section-level (LLM) method for this row.
        row_kind <- inferred$kind
        cand <- .STANDARD_METHODS[[inferred$method]]
        if (!is.null(cand)) {
          if (!cand$id %in% seen_mth) {
            methods[[length(methods) + 1L]] <- .with_op_self_rels(cand)
            seen_mth <- c(seen_mth, cand$id)
          }
          if (!identical(cand$id, mth_obj$id)) {
            diag_add(
              stage = "build_ars", severity = "INFO",
              problem = sprintf("Row '%s': annotation form implies %s (section method was %s)",
                                row$label %||% "?", cand$id, mth_obj$id),
              tlf_number = sec$tlf_number,
              action = "Used the annotation-inferred method for this row"
            )
          }
          row_method_id <- cand$id
        }
      } else if (!gated_generic &&
                 identical(mth_obj$id, "MTH_SUMMARY_STATISTICS_CONTINUOUS") &&
                 isTRUE(cat_verdict)) {
        ## Per-row method correction: a section classified as continuous may
        ## still contain categorical rows (e.g. SEX, RACE in a demographics
        ## table). Summarising those with continuous stats yields NaN -- so when
        ## the ADaM spec marks the row variable as categorical, route it to the
        ## count-and-percentage method instead.
        row_method_id <- count_method_id
        if (!count_method_id %in% seen_mth) {
          methods[[length(methods) + 1L]] <-
            .with_op_self_rels(.STANDARD_METHODS[["Count and Percentage"]])
          seen_mth <- c(seen_mth, count_method_id)
        }
        diag_add(
          stage = "build_ars", severity = "INFO",
          problem = sprintf("Row variable %s is categorical but its TLF was classified continuous",
                            er$primary_variable %||% row$label %||% "?"),
          tlf_number = sec$tlf_number,
          action = "Routed this row to Count and Percentage instead of continuous summary"
        )
      }

      ## Duplicate-template dedup (nested AE shells author example blocks:
      ## "<Preferred Term>" placeholder rows plus repeated "Preferred Term"
      ## mock rows all annotated AEDECOD). Two rows resolving to the same
      ## method + variable + subset would expand the SAME distribution twice
      ## and collide in the renderer -- keep the first, collapse the rest.
      row_sig <- paste(row_method_id,
                       toupper(er$primary_dataset  %||% ""),
                       toupper(er$primary_variable %||% ""),
                       if (!is.null(ds_obj)) ds_obj$id else "",
                       sep = "|")
      if (build_layout && row_sig %in% seen_row_sig) {
        diag_add(
          stage = "build_ars", severity = "INFO",
          problem = sprintf("Row '%s' duplicates an earlier row's analysis (%s); collapsed",
                            row$label %||% "?", row_sig),
          tlf_number = sec$tlf_number,
          action = "Template/example rows expand once -- the first matching row carries the analysis"
        )
        next
      }
      seen_row_sig <- c(seen_row_sig, row_sig)

      an_obj <- .build_analysis(
        section = sec, row = row, enrichment = er,
        index = idx, as_id = as_obj$id,
        gf_ids = gf_ids,
        method_id = row_method_id,
        ds_id = if (!is.null(ds_obj)) ds_obj$id else NULL
      )
      analyses[[length(analyses) + 1L]] <- an_obj
      analysis_ids <- c(analysis_ids, an_obj$id)
      shell_layout[[length(shell_layout) + 1L]] <- list(
        order = length(shell_layout) + 1L,
        label = row$label %||% "", indent = indent,
        analysis_id = an_obj$id,
        kind = if (identical(row_method_id, "MTH_COUNT_AND_PERCENTAGE") &&
                     identical(row_kind, "row")) "categorical"
               else if (identical(row_method_id, "MTH_SUMMARY_STATISTICS_CONTINUOUS") &&
                          identical(row_kind, "row")) "continuous"
               else row_kind)
    }
    if (!build_layout) shell_layout <- list()

    ## Executable inferential methods (ADR 0001): one analysis each on the
    ## section's primary response variable, carrying any operand (e.g. CMH
    ## strata). These compute through the arsbridge engine.
    if (has_exec) {
      resp_er  <- .section_primary_enrichment(sec)
      resp_row <- list(label = sec$title %||% sec$tlf_number,
                       annotation = "", has_annot = TRUE)
      for (k in seq_along(cls$executable)) {
        em <- cls$executable[[k]]
        if (!em$method_id %in% seen_mth) {
          methods[[length(methods) + 1L]] <- .build_exec_method(em$method_id)
          seen_mth <- c(seen_mth, em$method_id)
        }
        an <- .build_analysis(
          section = sec, row = resp_row, enrichment = resp_er,
          index = length(analysis_ids) + 1L, as_id = as_obj$id,
          gf_ids = gf_ids, method_id = em$method_id, ds_id = NULL)
        if (!is.null(em$strata)) an$strata <- em$strata
        analyses[[length(analyses) + 1L]] <- an
        analysis_ids <- c(analysis_ids, an$id)
      }
    }

    ## Residual reserve: a generic manual_pending cell for indicators that are
    ## still not computable (e.g. a Newcombe difference), so they appear marked.
    if (!gated_generic && sec_unsupported && length(cls$residual) > 0) {
      if (!"MTH_UNSUPPORTED_ANALYSIS" %in% seen_mth) {
        methods[[length(methods) + 1L]] <- .build_unsupported_method(sec)
        seen_mth <- c(seen_mth, "MTH_UNSUPPORTED_ANALYSIS")
      }
      an <- .build_analysis(
        section = sec, row = list(label = "Manual", annotation = "",
                                  has_annot = TRUE),
        enrichment = .section_primary_enrichment(sec),
        index = length(analysis_ids) + 1L, as_id = as_obj$id,
        gf_ids = gf_ids, method_id = "MTH_UNSUPPORTED_ANALYSIS", ds_id = NULL)
      analyses[[length(analyses) + 1L]] <- an
      analysis_ids <- c(analysis_ids, an$id)
    }

    outputs[[length(outputs) + 1L]] <-
      .build_output(sec, analysis_ids, ship_annotations = ship_annotations,
                    shell_layout = shell_layout)
  }

  ## Siera iterates `seq_len(nrow(JSON_DataSubsets))` and
  ## `seq_len(nrow(JSON_AG_1))` without guarding for empty arrays
  ## (metadata.R:128, 194). Emit a placeholder no-op entry when either
  ## list is empty so siera doesn't crash on `nrow(NULL)`.
  if (length(data_subsets) == 0L) {
    data_subsets <- list(.default_data_subset())
    diag_add(
      stage = "build_ars", severity = "INFO",
      problem = "No DataSubsets derived from any annotation",
      action = "Emitted a placeholder no-op DataSubset for siera compatibility"
    )
  }
  if (length(grouping_factors) == 0L) {
    grouping_factors <- list(.default_grouping())
    diag_add(
      stage = "build_ars", severity = "WARN",
      problem = "No grouping variable was derived for any TLF",
      action = "Emitted placeholder TRT01A grouping -- verify the study's treatment/grouping variable"
    )
  }

  list(
    id                    = study_id,
    name                  = study_name %||% study_id,
    version               = "1",
    ## siera-required tables of contents (formerly only listOfPlannedAnalyses)
    otherListsOfContents  = .build_lopo(outputs),
    mainListOfContents    = .build_lopa(outputs),
    analysisSets          = analysis_sets,
    dataSubsets           = data_subsets,
    analysisGroupings     = grouping_factors,
    methods               = methods,
    analyses              = analyses,
    outputs               = outputs,
    `_meta` = list(
      generator             = paste0("arsbridge ", utils::packageVersion("arsbridge")),
      generated_at_utc      = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      ars_model_version     = "1.0",
      requires_human_review = TRUE,
      ## TLFs where no grouping variable could be resolved (built
      ## ungrouped) -- start the human review here.
      sections_needing_review = as.list(vapply(
        Filter(function(s) isTRUE(s$needs_review), sections),
        function(s) s$tlf_number %||% "", character(1)
      )),
      ## Outputs arsbridge cannot generate (inferential / model-based). The
      ## renderer emits a numbered placeholder for each; the programmer
      ## produces them manually.
      unsupported_outputs = unsupported
    )
  )
}


## --- Per-object builders --------------------------------------------------

.build_analysis_set <- function(sec) {
  pop_text  <- sec$population_text %||% "All Subjects"
  pop_annot <- sec$population_annot %||% ""
  cond      <- parse_where_clause(pop_annot)
  obj <- list(
    id    = make_analysis_set_id(pop_text),
    name  = pop_text,
    label = pop_text
  )
  if (!is.null(cond)) {
    obj <- modifyList(obj, cond)
  }
  ## level + order are stamped by build_ars_json() at the dedup append site.
  obj
}

#' All GroupingFactors for a section, ordered outermost first. Prefers the
#' multi-level `sec$groupings` list (P8); falls back to the legacy single
#' `by_variable` / `by_variable_dataset` pair.
#' @noRd
.build_groupings <- function(sec) {
  groupings <- sec$groupings
  if (is.null(groupings) || length(groupings) == 0) {
    single <- .build_grouping(sec)
    return(if (is.null(single)) list() else list(single))
  }
  out <- lapply(groupings, function(g) {
    if (!nzchar(g$variable %||% "")) return(NULL)
    .build_grouping_one(g$variable, g$dataset %||% "ADSL")
  })
  Filter(Negate(is.null), out)
}

#' Legacy single-grouping builder (kept for sections enriched before the
#' multi-level model and for direct unit tests).
#' @noRd
.build_grouping <- function(sec) {
  by_var <- sec$by_variable %||% ""
  if (!nzchar(by_var)) return(NULL)
  .build_grouping_one(by_var, sec$by_variable_dataset %||% "ADSL")
}

.build_grouping_one <- function(variable, dataset) {
  list(
    id               = make_grouping_id(variable),
    name             = variable,
    label            = paste0("Grouping by ", variable),
    ## siera reads these as FLAT strings (metadata.R lines 188-189).
    ## Dataset comes from the spec-resolved enrichment field -- grouping
    ## variables are not always ADSL (e.g. AVISIT in a BDS dataset).
    groupingDataset  = dataset,
    groupingVariable = variable,
    dataDriven       = FALSE,
    ## siera iterates JSON_AnalysisGroupings$groups[[e]]; emit empty array
    ## so the iteration succeeds when no per-level groups are specified.
    groups           = list()
  )
}

.build_method <- function(sec) {
  name <- sec$ars_method_name %||% "Count and Percentage"
  std  <- .STANDARD_METHODS[[name]]
  if (is.null(std)) {
    diag_add(
      stage = "build_ars", severity = "WARN",
      problem = sprintf("Analysis method '%s' is not in the standard catalogue", name),
      tlf_number = sec$tlf_number,
      action = "Emitted a placeholder no-op method -- this TLF's results will not compute until the method is implemented"
    )
    ## Unknown method name -- still emit a minimal codeTemplate so siera
    ## generates a runnable (no-op) ARD_*.R rather than failing at metadata.
    fallback <- list(
      id          = make_method_id(name),
      name        = name,
      label       = name,
      description = name,
      operations  = list(list(id = "OP_PASS", name = "Passthrough",
                              label = "Passthrough", order = 1L,
                              resultPattern = "X")),
      codeTemplate = list(
        context    = "R (siera)",
        code       = paste(
          "## Unknown method '", name, "' -- placeholder template.",
          "df3_analysisidhere <- data.frame(operation = 'OP_PASS',",
          "                                  res = NA_real_,",
          "                                  pattern = 'X')",
          sep = "\n"
        ),
        parameters = list()
      )
    )
    return(.with_op_self_rels(fallback))
  }
  .with_op_self_rels(std)
}

## Declarative method for a capability-gated (unsupported) section. The ARS
## still carries the analysis + this method so the Output -> Analysis -> Method
## chain stays intact (ADR 0002 phase 3); the engine reserves manual_pending
## stub ARD rows for it (id matches .UNEXECUTABLE_METHODS in ars_to_ard.R).
## Tagged supported = FALSE with the capability reason for traceability.
#' @noRd
.build_unsupported_method <- function(sec) {
  reason <- sec$unsupported_reason %||% "not supported by arsbridge"
  .with_op_self_rels(list(
    id          = "MTH_UNSUPPORTED_ANALYSIS",
    name        = "Unsupported analysis (manual)",
    label       = "Unsupported analysis (manual)",
    description = reason,
    supported   = FALSE,
    operations  = list(list(id = "OP_MANUAL", name = "Manual derivation",
                            label = "Manual derivation", order = 1L,
                            resultPattern = "X")),
    codeTemplate = list(
      context    = "R (siera)",
      code       = paste0(
        "## Manual derivation required -- ", reason, "\n",
        "## arsbridge reserves a manual_pending ARD cell for this analysis;\n",
        "## compute it with a validated script and fill the reserved row\n",
        "## (see ars_manual_worklist())."),
      parameters = list())
  ))
}

## A supported AnalysisMethod for an arsbridge-executable inferential method
## (ADR 0001) -- the exact CI or the CMH p-value. Unlike .build_unsupported_method
## this is tagged supported = TRUE; the arsbridge engine emits the cardx / base-R
## call for it. A minimal codeTemplate keeps siera happy.
#' @noRd
.build_exec_method <- function(method_id) {
  nm <- switch(method_id,
    MTH_CMH_TEST            = "Cochran-Mantel-Haenszel test",
    MTH_PROPORTION_CI_EXACT = "Clopper-Pearson exact confidence interval",
    method_id)
  .with_op_self_rels(list(
    id          = method_id,
    name        = nm,
    label       = nm,
    description = nm,
    supported   = TRUE,
    operations  = list(list(id = "OP_STAT", name = nm, label = nm, order = 1L,
                            resultPattern = "X")),
    codeTemplate = list(
      context    = "R (arsbridge)",
      code       = paste0("## ", nm,
                          " -- computed by the arsbridge engine (cardx / base R)."),
      parameters = list())
  ))
}

## The enrichment (primary dataset + variable) of a section's main response row,
## used as the analysis variable for the section-level inferential methods.
## Returns the first enriched row that names a primary variable, or list().
#' @noRd
.section_primary_enrichment <- function(sec) {
  for (er in sec$enriched_rows %||% list()) {
    if (!is.null(er$primary_variable) && nzchar(er$primary_variable %||% ""))
      return(er)
  }
  list()
}

.build_data_subset <- function(enrichment, tlf_number, index) {
  ds <- enrichment$data_subset
  if (is.null(ds) || length(ds) == 0) return(NULL)
  tag <- if (!is.null(ds$variable)) {
    paste0(ds$dataset, "_", ds$variable, "_", ds$value[[1]] %||% "")
  } else {
    paste0(tlf_number, "_", index)
  }
  list(
    id    = make_data_subset_id(tag),
    name  = tag,
    label = tag,
    condition = list(
      dataset    = ds$dataset    %||% "",
      variable   = ds$variable   %||% "",
      comparator = ds$comparator %||% "EQ",
      value      = ds$value      %||% list()
    )
    ## level + order stamped by build_ars_json() at dedup append site.
  )
}

.build_analysis <- function(section, row, enrichment, index,
                            as_id, gf_ids, method_id, ds_id) {
  gf_ids <- gf_ids[nzchar(gf_ids %||% character())]
  groupings <- lapply(seq_along(gf_ids), function(i) {
    list(order = i, groupingId = gf_ids[[i]], resultsByGroup = TRUE)
  })

  dataset_str  <- enrichment$primary_dataset  %||% ""
  variable_str <- enrichment$primary_variable %||% ""
  self_id      <- make_analysis_id(section$tlf_number, index)

  ## siera's metadata.R only appends rows to AN_refs (and creates the "id"
  ## column it later merges by) when `referencedAnalysisOperations` is
  ## non-null on at least one analysis. If no analysis carries refs,
  ## `merge(JSON_AnalysesL1, AN_refs, by = "id")` fails with "'by' must
  ## specify a uniquely valid column". And siera's downstream code reads
  ## both `*_analysisId1` (NUM) and `*_analysisId2` (DEN) -- if either is
  ## missing, the grouping-variable lookup returns character(0) and a
  ## bare `if (... %in% ...)` errors with "argument is of length zero".
  ##
  ## We have no genuine NUM/DEN relationship to express, so emit two
  ## self-references. siera's filter then resolves both NUM and DEN to
  ## this same analysis -- a harmless no-op that keeps the pipeline
  ## flowing.
  ref_ops <- list(
    list(referencedOperationRelationshipId = "SELF_NUM", analysisId = self_id),
    list(referencedOperationRelationshipId = "SELF_DEN", analysisId = self_id)
  )

  list(
    id            = self_id,
    name          = paste0("Analysis ", index, " for ", section$tlf_number),
    label         = row$label %||% "",
    description   = row$label %||% "",
    version       = "1",
    categoryIds   = list(),
    analysisSetId = as_id,
    ## siera reads `dataset` and `variable` as FLAT strings (metadata.R
    ## lines 232-233). The nested analysisVariable is kept alongside for
    ## ARS-spec-correct consumers.
    dataset       = dataset_str,
    variable      = variable_str,
    analysisVariable = list(
      dataset  = dataset_str,
      variable = variable_str
    ),
    dataSubsetId                 = if (is.null(ds_id)) "" else ds_id,
    orderedGroupings             = groupings,
    referencedAnalysisOperations = ref_ops,
    methodId                     = method_id,
    annotation                   = row$annotation,
    ## SAP prose matched to this TLF (when a SAP was supplied); the emitter
    ## prints it as the human-readable comment above the {cards} block.
    sapDescription               = section$sap_text %||% "",
    variableRole                 = enrichment$variable_role %||% "ANALYSIS",
    ## Extension field: TRUE when the shell carries an overall/Total column
    ## in addition to the per-group columns. The executor then also
    ## computes an ungrouped pass and binds it into the ARD.
    includeTotal                 = isTRUE(section$include_total)
  )
}

.build_output <- function(section, analysis_ids, ship_annotations = FALSE,
                          shell_layout = NULL) {
  ## Shipped footnotes are the true footnotes only; programmer annotation
  ## lines are mapping instructions, not display text (ADR 0003 Layer B).
  ## ship_annotations = TRUE re-attaches them for debugging.
  shipped_notes <- as.character(section$footnotes %||% character())
  if (isTRUE(ship_annotations)) {
    shipped_notes <- c(shipped_notes,
                       as.character(section$programmer_annotations %||% character()))
  }
  ## Output-private metadata (ADR 0003 Layer C). ARS v1.0 has no first-class
  ## stub model, so the authored layout travels in an arsbridge `_meta` block
  ## that standard consumers ignore and the renderer keys on.
  out_meta <- list(
    source_datasets = as.list(as.character(section$source_datasets %||% character()))
  )
  if (length(shell_layout %||% list()) > 0) {
    out_meta$shell_layout <- shell_layout
  }
  list(
    id                    = make_output_id(section$tlf_number),
    name                  = section$tlf_number,
    label                 = section$title %||% "",
    version               = "1",
    outputType            = section$tlf_type %||% "TABLE",
    displays              = list(list(
      order        = 1L,
      displayTitle = section$title %||% "",
      ## Carry the shell's column-header order so the renderer lays treatment
      ## columns out as the author wrote them (build_col_levels reads this),
      ## instead of falling back to alphabetical ARD order.
      columns      = lapply(
        Filter(nzchar, as.character(section$col_headers %||% character())),
        function(h) list(label = h)),
      displaySections = list(list(
        sectionType = "Footnote",
        subSections = lapply(as.list(shipped_notes),
                             function(f) list(text = f))
      ))
    )),
    fileSpecifications    = list(list(
      name     = paste0(section$tlf_number, ".rtf"),
      fileType = "rtf"
    )),
    referencedAnalysisIds = as.list(analysis_ids),
    `_meta`               = out_meta
  )
}


## --- siera-empty-array safety nets ----------------------------------------
##
## siera's .read_ars_json_metadata() unconditionally walks
## `seq_len(nrow(JSON_DataSubsets))` and `seq_len(nrow(JSON_AG_1))`, so
## an empty `dataSubsets: []` or `analysisGroupings: []` crashes the
## reader with "argument must be coercible to non-negative integer".
## These fallbacks emit a structurally valid no-op so siera proceeds.

#' Placeholder DataSubset representing "all subjects" (no filter).
#' Condition is USUBJID != "" which matches every record.
#' @noRd
.default_data_subset <- function() {
  list(
    id        = "DS_ALL",
    name      = "All subjects",
    label     = "All subjects (no filter)",
    level     = 1L,
    order     = 1L,
    condition = list(
      dataset    = "ADSL",
      variable   = "USUBJID",
      comparator = "NE",
      value      = list("")
    )
  )
}

#' Placeholder GroupingFactor for the default TRT01A grouping.
#' @noRd
.default_grouping <- function() {
  list(
    id               = "GF_TRT01A",
    name             = "TRT01A",
    label            = "Default treatment grouping",
    groupingDataset  = "ADSL",
    groupingVariable = "TRT01A",
    dataDriven       = FALSE,
    groups           = list()
  )
}

## --- siera-required tables of contents ------------------------------------

#' Build otherListsOfContents (List of Planned Outputs / LOPO).
#'
#' siera reads this as `json_from$otherListsOfContents$contentsList$listItems[[1]]`
#' (metadata.R:64) -- the outer array contains one LOPO; the inner listItems
#' is the array of outputs.
#' @noRd
.build_lopo <- function(outputs) {
  list(list(
    name  = "List of Planned Outputs",
    label = "LOPO",
    contentsList = list(
      listItems = lapply(seq_along(outputs), function(i) list(
        name     = outputs[[i]]$label %||% outputs[[i]]$name,
        level    = 1L,
        order    = i,
        outputId = outputs[[i]]$id
      ))
    )
  ))
}

#' Build mainListOfContents (List of Planned Analyses / LOPA).
#'
#' siera reads this as `json_from$mainListOfContents$contentsList$listItems`
#' and iterates `$sublist$listItems[[a]]$analysisId` per output (metadata.R:73,82).
#'
#' siera's `.generate_analysis_set_code()` indexes `anas[3, ]$listItem_analysisId`
#' unconditionally (AnalysisSet.R:141). When an Output has <3 analyses, that
#' returns NA and `gsub("analysisADAMhere", NA, ...)` crashes with "invalid
#' 'replacement' argument". We pad the sublist by repeating the last
#' analysisId to a minimum of 3 entries -- the duplicates are filtered out
#' by siera's downstream `unique()` calls or land on the same-dataset path
#' (a no-op).
#' @noRd
.build_lopa <- function(outputs) {
  pad_to_min <- function(ids, min_n = 3L) {
    if (length(ids) == 0L) return(ids)
    if (length(ids) >= min_n) return(ids)
    c(ids, rep(utils::tail(ids, 1L), min_n - length(ids)))
  }

  list(
    name  = "List of Planned Analyses",
    label = "LOPA",
    contentsList = list(
      listItems = lapply(seq_along(outputs), function(i) {
        o <- outputs[[i]]
        an_ids <- unlist(o$referencedAnalysisIds %||% list())
        an_ids_padded <- pad_to_min(an_ids)
        list(
          name     = o$label %||% o$name,
          level    = 1L,
          order    = i,
          outputId = o$id,
          sublist  = list(
            listItems = lapply(seq_along(an_ids_padded), function(j) list(
              analysisId = an_ids_padded[j],
              level      = 2L,
              order      = j
            ))
          )
        )
      })
    )
  )
}
