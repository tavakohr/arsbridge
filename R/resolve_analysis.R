## arsbridge -- resolve_analysis.R
## ---------------------------------------------------------------------------
## Shared, data-INDEPENDENT resolver that turns one ARS analysis object into a
## flat list of the arguments needed to execute (or EMIT cards code for) it.
##
## This is the single source of truth so the cards code arsbridge EMITS
## (R/ars_to_code.R) and the ARD arsbridge EXECUTES (R/ars_to_ard.R) are derived
## from identical resolved arguments -- "emitted == executed" by construction.
## See Plan B 7.0. All helpers internal.

## Scalar character coercion: protect against list-columns / length-0 fields
## that appear when jsonlite reads ARS JSON with simplifyVector = FALSE.
#' @noRd
.as_scalar_char <- function(x) {
  if (is.null(x)) return(NULL)
  val <- unlist(x)
  if (length(val) == 0) return(NULL)
  as.character(val[1])
}

## groupingId -> grouping variable name, built once from the ARS spec.
#' @noRd
.build_grouping_map <- function(spec) {
  grouping_map <- list()
  for (gf in spec[["analysisGroupings"]]) {
    gf_id <- .as_scalar_char(gf[["id"]])
    if (is.null(gf_id)) next
    gf_var <- if (is.list(gf[["groupingVariable"]])) {
      gf[["groupingVariable"]][["variable"]]
    } else {
      gf[["groupingVariable"]]
    }
    if (is.null(gf_var) || !nzchar(gf_var)) gf_var <- gf[["name"]]
    gf_var_str <- .as_scalar_char(gf_var)
    if (!is.null(gf_var_str)) grouping_map[[gf_id]] <- gf_var_str
  }
  grouping_map
}

## analysisId -> outputId, built once from the ARS outputs' referencedAnalysisIds.
#' @noRd
.build_analysis_to_output <- function(spec) {
  m <- list()
  for (out in spec[["outputs"]]) {
    out_id <- .as_scalar_char(out[["id"]])
    if (is.null(out_id)) next
    for (an_id in unlist(out[["referencedAnalysisIds"]])) {
      an_id_str <- .as_scalar_char(an_id)
      if (!is.null(an_id_str)) m[[an_id_str]] <- out_id
    }
  }
  m
}

## Locate a WhereClause-bearing object (analysisSet / dataSubset) by id.
#' @noRd
.find_by_id <- function(spec, key, id) {
  if (is.null(id) || !nzchar(id)) return(NULL)
  for (obj in spec[[key]]) {
    if (identical(.as_scalar_char(obj[["id"]]), id)) return(obj)
  }
  NULL
}

#' Resolve one ARS analysis into flat execution/emission arguments
#'
#' Pure (data-independent) resolution of a single ARS `analysis` object against
#' its parent `spec`. The returned arguments drive both `ars_to_ard()` execution
#' and `ars_to_code.R` emission, guaranteeing the two stay in lock-step.
#'
#' @param ana One analysis object from `spec$analyses`.
#' @param spec The full ARS spec (parsed with `simplifyVector = FALSE`).
#' @param subject_key Subject-level identifier (default `"USUBJID"`).
#' @param grouping_map,analysis_to_output Optional pre-built lookup maps (see
#'   `.build_grouping_map` / `.build_analysis_to_output`); rebuilt from `spec`
#'   when `NULL`. Pass them in when resolving many analyses to avoid rework.
#'
#' @return A list with `analysis_id`, `output_id`, `method_id`, `dataset`,
#'   `variable` (raw, uncleaned), `by` (character vector of grouping vars),
#'   `pop_where`, `subset_where` (WhereClause objects or `NULL`),
#'   `include_total` (logical), `strata` (stratification variable for methods
#'   like CMH, or `NULL`), `subject_key`, `label`, `annotation`,
#'   `description`, and `sap_description`.
#' @noRd
resolve_analysis <- function(ana, spec, subject_key = "USUBJID",
                             grouping_map = NULL, analysis_to_output = NULL) {
  if (is.null(grouping_map))       grouping_map       <- .build_grouping_map(spec)
  if (is.null(analysis_to_output)) analysis_to_output <- .build_analysis_to_output(spec)

  analysis_id <- .as_scalar_char(ana[["id"]])
  output_id   <- if (!is.null(analysis_id)) analysis_to_output[[analysis_id]] else NULL

  method_id <- .as_scalar_char(ana[["methodId"]])
  variable  <- .as_scalar_char(ana[["analysisVariable"]][["variable"]] %||% ana[["variable"]])
  dataset   <- .as_scalar_char(ana[["analysisVariable"]][["dataset"]] %||% ana[["dataset"]])
  pop_id    <- .as_scalar_char(ana[["analysisSetId"]])
  subset_id <- .as_scalar_char(ana[["dataSubsetId"]])

  pop_where    <- .find_by_id(spec, "analysisSets", pop_id)
  subset_where <- .find_by_id(spec, "dataSubsets",  subset_id)

  ## Grouping variables in display order. Raw names -- the executor/emitter
  ## clean them against the actual dataset columns (a `.`-qualified name like
  ## ADSL.TRT01A only resolves once the data frame is known).
  by <- character(0)
  for (grp in ana[["orderedGroupings"]] %||% list()) {
    gf_id <- .as_scalar_char(grp[["groupingId"]])
    if (is.null(gf_id)) next
    gf_var <- grouping_map[[gf_id]]
    if (!is.null(gf_var) && nzchar(gf_var)) by <- c(by, gf_var)
  }

  include_total <- isTRUE(as.logical(unlist(ana[["includeTotal"]])[1] %||% FALSE))

  ## Stratification operand for stratified methods (e.g. CMH). An arsbridge
  ## extension field on the analysis; a bare variable name resolved against the
  ## data later. Absent for the descriptive methods.
  strata <- .as_scalar_char(ana[["strata"]]) %||%
    .as_scalar_char(ana[["stratificationVariable"]])

  description <- .as_scalar_char(ana[["description"]]) %||%
    .as_scalar_char(ana[["name"]]) %||% analysis_id

  list(
    analysis_id     = analysis_id,
    output_id       = output_id,
    method_id       = method_id,
    dataset         = dataset,
    variable        = variable,
    by              = by,
    pop_where       = pop_where,
    subset_where    = subset_where,
    include_total   = include_total,
    strata          = strata,
    subject_key     = subject_key,
    label           = .as_scalar_char(ana[["label"]]),
    annotation      = .as_scalar_char(ana[["annotation"]]),
    description     = description,
    sap_description = .as_scalar_char(ana[["sapDescription"]])
  )
}
