## arsbridge -- build_ars_json.R
## ---------------------------------------------------------------------------
## Assembles enriched TLF sections into a CDISC ARS v1.0 ReportingEvent
## object suitable for jsonlite serialisation. De-duplicates AnalysisSets,
## GroupingFactors, AnalysisMethods, and DataSubsets across all TLFs in the
## run so the JSON stays compact.

## Standard AnalysisMethod definitions -- covers >90% of clinical TLFs.
.STANDARD_METHODS <- list(
  "Summary Statistics - Continuous" = list(
    id          = "MTH_SUMMARY_STATISTICS_CONTINUOUS",
    name        = "Summary Statistics - Continuous",
    description = "n, mean, SD, median, Q1, Q3, min, max",
    operations  = list(
      list(id = "OP_N",      name = "n",      order = 1L, resultPattern = "XXX"),
      list(id = "OP_MEAN",   name = "Mean",   order = 2L, resultPattern = "XXX.X"),
      list(id = "OP_SD",     name = "SD",     order = 3L, resultPattern = "XXX.XX"),
      list(id = "OP_MEDIAN", name = "Median", order = 4L, resultPattern = "XXX.X"),
      list(id = "OP_Q1",     name = "Q1",     order = 5L, resultPattern = "XXX.X"),
      list(id = "OP_Q3",     name = "Q3",     order = 6L, resultPattern = "XXX.X"),
      list(id = "OP_MIN",    name = "Min",    order = 7L, resultPattern = "XXX"),
      list(id = "OP_MAX",    name = "Max",    order = 8L, resultPattern = "XXX")
    )
  ),
  "Count and Percentage" = list(
    id          = "MTH_COUNT_AND_PERCENTAGE",
    name        = "Count and Percentage",
    description = "n (%)",
    operations  = list(
      list(id = "OP_N",     name = "Count",       order = 1L, resultPattern = "XXX"),
      list(id = "OP_PCT",   name = "Percentage",  order = 2L, resultPattern = "XX.X"),
      list(id = "OP_DENOM", name = "Denominator", order = 3L, resultPattern = "XXX")
    )
  ),
  "Subject Count" = list(
    id          = "MTH_SUBJECT_COUNT",
    name        = "Subject Count",
    description = "Unique subject count",
    operations  = list(
      list(id = "OP_N", name = "n", order = 1L, resultPattern = "XXX")
    )
  ),
  "Kaplan-Meier Estimate" = list(
    id          = "MTH_KAPLAN_MEIER_ESTIMATE",
    name        = "Kaplan-Meier Estimate",
    description = "KM event rate, median survival, confidence interval",
    operations  = list(
      list(id = "OP_EVENTS",  name = "Events",          order = 1L, resultPattern = "XXX"),
      list(id = "OP_MEDIAN",  name = "Median (months)", order = 2L, resultPattern = "XXX.X"),
      list(id = "OP_CI_LOW",  name = "95% CI Lower",    order = 3L, resultPattern = "XXX.X"),
      list(id = "OP_CI_HIGH", name = "95% CI Upper",    order = 4L, resultPattern = "XXX.X")
    )
  ),
  "AE Frequency Count" = list(
    id          = "MTH_AE_FREQUENCY_COUNT",
    name        = "AE Frequency Count",
    description = "Unique subjects with event, n (%)",
    operations  = list(
      list(id = "OP_N",   name = "n",   order = 1L, resultPattern = "XXX"),
      list(id = "OP_PCT", name = "(%)", order = 2L, resultPattern = "XX.X")
    )
  ),
  "Listing" = list(
    id          = "MTH_LISTING",
    name        = "Listing",
    description = "Subject-level data listing",
    operations  = list()
  )
)

#' Build a CDISC ARS v1.0 ReportingEvent list from enriched sections.
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
      analysis_sets[[length(analysis_sets) + 1L]] <- as_obj
      seen_as <- c(seen_as, as_obj$id)
    }

    ## --- GroupingFactor from by_variable ---
    gf_obj <- .build_grouping(sec)
    if (!is.null(gf_obj) && !gf_obj$id %in% seen_gf) {
      grouping_factors[[length(grouping_factors) + 1L]] <- gf_obj
      seen_gf <- c(seen_gf, gf_obj$id)
    }

    ## --- AnalysisMethod (standard catalogue) ---
    mth_obj <- .build_method(sec)
    if (!mth_obj$id %in% seen_mth) {
      methods[[length(methods) + 1L]] <- mth_obj
      seen_mth <- c(seen_mth, mth_obj$id)
    }

    ## --- One Analysis per annotated row ---
    annotated_rows <- Filter(function(r) isTRUE(r$has_annot), sec$stub_rows)
    enriched_rows  <- sec$enriched_rows %||% list()
    ## Index enriched rows by label for join-by-label.
    er_by_label <- setNames(enriched_rows,
                            vapply(enriched_rows, function(e) e$label %||% "",
                                   character(1)))

    analysis_ids <- character()
    for (idx in seq_along(annotated_rows)) {
      row <- annotated_rows[[idx]]
      er  <- er_by_label[[row$label]] %||% list()
      ds_obj <- .build_data_subset(er, sec$tlf_number, idx)
      if (!is.null(ds_obj) && !ds_obj$id %in% seen_ds) {
        data_subsets[[length(data_subsets) + 1L]] <- ds_obj
        seen_ds <- c(seen_ds, ds_obj$id)
      }
      an_obj <- .build_analysis(
        section = sec, row = row, enrichment = er,
        index = idx, as_id = as_obj$id, gf_id = if (!is.null(gf_obj)) gf_obj$id else NULL,
        method_id = mth_obj$id, ds_id = if (!is.null(ds_obj)) ds_obj$id else NULL
      )
      analyses[[length(analyses) + 1L]] <- an_obj
      analysis_ids <- c(analysis_ids, an_obj$id)
    }

    outputs[[length(outputs) + 1L]] <- .build_output(sec, analysis_ids)
  }

  list(
    id                    = study_id,
    name                  = study_name %||% study_id,
    version               = "1",
    listOfPlannedAnalyses = list(
      listItems = lapply(analyses, function(a) list(analysisId = a$id))
    ),
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
      requires_human_review = TRUE
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
  obj
}

.build_grouping <- function(sec) {
  by_var <- sec$by_variable %||% ""
  if (!nzchar(by_var)) return(NULL)
  list(
    id               = make_grouping_id(by_var),
    name             = by_var,
    label            = paste0("Grouping by ", by_var),
    groupingVariable = list(dataset = "ADSL", variable = by_var),
    dataDriven       = FALSE
  )
}

.build_method <- function(sec) {
  name <- sec$ars_method_name %||% "Count and Percentage"
  std  <- .STANDARD_METHODS[[name]]
  if (is.null(std)) {
    return(list(
      id          = make_method_id(name),
      name        = name,
      description = name,
      operations  = list()
    ))
  }
  std
}

.build_data_subset <- function(enrichment, tlf_number, index) {
  ds <- enrichment$data_subset
  if (is.null(ds) || length(ds) == 0) return(NULL)
  tag <- if (!is.null(ds$variable)) paste0(ds$dataset, "_", ds$variable, "_",
                                            ds$value[[1]] %||% "")
         else paste0(tlf_number, "_", index)
  list(
    id        = make_data_subset_id(tag),
    name      = tag,
    label     = tag,
    condition = list(
      dataset    = ds$dataset    %||% "",
      variable   = ds$variable   %||% "",
      comparator = ds$comparator %||% "EQ",
      value      = ds$value      %||% list()
    )
  )
}

.build_analysis <- function(section, row, enrichment, index,
                            as_id, gf_id, method_id, ds_id) {
  groupings <- if (is.null(gf_id)) list() else list(list(
    order = 1L, groupingId = gf_id, resultsByGroup = TRUE
  ))
  primary_var <- list(
    dataset  = enrichment$primary_dataset  %||% "",
    variable = enrichment$primary_variable %||% ""
  )

  list(
    id                = make_analysis_id(section$tlf_number, index),
    name              = paste0("Analysis ", index, " for ", section$tlf_number),
    description       = row$label %||% "",
    analysisSetId     = as_id,
    dataSubsetId      = if (is.null(ds_id)) "" else ds_id,
    orderedGroupings  = groupings,
    methodId          = method_id,
    analysisVariable  = primary_var,
    annotation        = row$annotation,
    variableRole      = enrichment$variable_role %||% "ANALYSIS",
    programmingCode   = list(
      context = "R",
      code    = sprintf("## %s: annotation = %s", section$tlf_number, row$annotation)
    )
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
