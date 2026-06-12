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

#' Build a CDISC ARS v1.0 ReportingEvent list from enriched sections.
#'
#' Emits a JSON-ready structure that satisfies BOTH the CDISC ARS v1.0
#' logical model AND `siera::readARS()`'s expected JSON shape. Where the
#' two disagree (flat vs nested fields), we emit both forms.
#'
#' @param sections   List of enriched TLF sections (output of
#'   [enrich_with_llm()] applied to each section).
#' @param study_id   Study identifier.
#' @param study_name Human-readable study name.
#'
#' @return Named list ready for [jsonlite::toJSON()] (use
#'   `auto_unbox = TRUE, pretty = TRUE, null = "null"`).
#'
#' @keywords internal
#' @noRd
build_ars_json <- function(sections,
                           study_id   = "STUDY-001",
                           study_name = NULL) {
  if (length(sections) == 0) {
    cli::cli_abort("Cannot build ReportingEvent: no TLF sections provided.")
  }

  analysis_sets    <- list(); seen_as  <- character()
  data_subsets     <- list(); seen_ds  <- character()
  grouping_factors <- list(); seen_gf  <- character()
  methods          <- list(); seen_mth <- character()
  analyses         <- list()
  outputs          <- list()

  for (sec in sections) {
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

    ## --- AnalysisMethod (standard catalogue) ---
    mth_obj <- .build_method(sec)
    if (!mth_obj$id %in% seen_mth) {
      methods[[length(methods) + 1L]] <- mth_obj
      seen_mth <- c(seen_mth, mth_obj$id)
    }

    ## --- One Analysis per annotated row ---
    annotated_rows <- Filter(function(r) isTRUE(r$has_annot), sec$stub_rows)
    enriched_rows  <- sec$enriched_rows %||% list()
    er_by_label    <- setNames(
      enriched_rows,
      vapply(enriched_rows, function(e) e$label %||% "", character(1))
    )

    analysis_ids <- character()
    for (idx in seq_along(annotated_rows)) {
      row <- annotated_rows[[idx]]
      er  <- er_by_label[[row$label]] %||% list()
      ds_obj <- .build_data_subset(er, sec$tlf_number, idx)
      if (!is.null(ds_obj) && !ds_obj$id %in% seen_ds) {
        ds_obj$order <- length(data_subsets) + 1L
        ds_obj$level <- 1L
        data_subsets[[length(data_subsets) + 1L]] <- ds_obj
        seen_ds <- c(seen_ds, ds_obj$id)
      }
      an_obj <- .build_analysis(
        section = sec, row = row, enrichment = er,
        index = idx, as_id = as_obj$id,
        gf_ids = gf_ids,
        method_id = mth_obj$id,
        ds_id = if (!is.null(ds_obj)) ds_obj$id else NULL
      )
      analyses[[length(analyses) + 1L]] <- an_obj
      analysis_ids <- c(analysis_ids, an_obj$id)
    }

    outputs[[length(outputs) + 1L]] <- .build_output(sec, analysis_ids)
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
      ))
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
    variableRole                 = enrichment$variable_role %||% "ANALYSIS",
    ## Extension field: TRUE when the shell carries an overall/Total column
    ## in addition to the per-group columns. The executor then also
    ## computes an ungrouped pass and binds it into the ARD.
    includeTotal                 = isTRUE(section$include_total)
  )
}

.build_output <- function(section, analysis_ids) {
  list(
    id                    = make_output_id(section$tlf_number),
    name                  = section$tlf_number,
    label                 = section$title %||% "",
    version               = "1",
    outputType            = section$tlf_type %||% "TABLE",
    displays              = list(list(
      order        = 1L,
      displayTitle = section$title %||% "",
      displaySections = list(list(
        sectionType = "Footnote",
        subSections = lapply(section$footnotes %||% list(),
                             function(f) list(text = f))
      ))
    )),
    fileSpecifications    = list(list(
      name     = paste0(section$tlf_number, ".rtf"),
      fileType = "rtf"
    )),
    referencedAnalysisIds = as.list(analysis_ids)
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
