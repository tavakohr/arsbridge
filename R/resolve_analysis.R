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

## groupingId -> the DATASET its grouping variable comes from, when the ARS
## says. A column axis on a domain variable (ADCM.TRTA) rather than an ADSL one
## needs that variable carried onto the denominator, and this is the fact that
## says so -- knowable from the spec alone, so the emitted script and the
## executor decide it the same way. See .denom_join_expr() / .denominator_by_subject().
#' @noRd
.build_grouping_ds_map <- function(spec) {
  out <- list()
  for (gf in spec[["analysisGroupings"]]) {
    gf_id <- .as_scalar_char(gf[["id"]])
    if (is.null(gf_id)) next
    gv <- gf[["groupingVariable"]]
    ds <- if (is.list(gv)) .as_scalar_char(gv[["dataset"]]) else NULL
    if (!is.null(ds) && nzchar(ds)) out[[gf_id]] <- toupper(ds)
  }
  out
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

## groupingId -> per-level group definitions, for grouping factors that carry
## condition-defined groups[] (an annotation-defined column axis). Entries
## exist ONLY for factors with at least one usable {label, condition} level,
## so an empty map means "raw data-driven levels" exactly as before.
#' @noRd
.build_grouping_groups_map <- function(spec) {
  out <- list()
  for (gf in spec[["analysisGroupings"]]) {
    gf_id <- .as_scalar_char(gf[["id"]])
    if (is.null(gf_id)) next
    gf_var <- if (is.list(gf[["groupingVariable"]])) {
      .as_scalar_char(gf[["groupingVariable"]][["variable"]])
    } else {
      .as_scalar_char(gf[["groupingVariable"]])
    }
    if (is.null(gf_var)) next

    defs <- list()
    for (grp in gf[["groups"]] %||% list()) {
      label <- .as_scalar_char(grp[["label"]]) %||% .as_scalar_char(grp[["name"]])
      ## Groups arrive in the official ARS shape (condition directly on the
      ## node); .group_where() rewraps for the executor/emitter.
      condition <- .group_where(grp)
      if (is.null(label) || !nzchar(label) || is.null(condition)) next
      order_val <- suppressWarnings(as.integer(.as_scalar_char(grp[["order"]]) %||% NA))
      if (is.na(order_val)) order_val <- length(defs) + 1L
      defs[[length(defs) + 1L]] <- list(
        label     = label,
        order     = order_val,
        condition = condition
      )
    }
    if (length(defs) > 0) {
      defs <- defs[order(vapply(defs, function(d) d$order, integer(1)))]
      out[[gf_id]] <- list(variable = gf_var, groups = defs)
    }
  }
  out
}

## outputId -> resolved result-group paths, for outputs that declare the
## hierarchical resultGroupPaths extension. Each resolved path carries its
## COMPOSED condition (the AND of its referenced groups' conditions), the
## per-variable display levels, and its role/order, so the executor and the
## emitter iterate declared columns instead of crossing grouping variables.
## Empty map when no output declares paths (flat/crossed tables).
#' @noRd
.build_output_paths_map <- function(spec) {
  ## group id -> {variable, label, where} across every grouping factor.
  group_index <- list()
  for (gf in spec[["analysisGroupings"]] %||% list()) {
    gf_var <- if (is.list(gf[["groupingVariable"]])) {
      .as_scalar_char(gf[["groupingVariable"]][["variable"]])
    } else {
      .as_scalar_char(gf[["groupingVariable"]])
    }
    for (grp in gf[["groups"]] %||% list()) {
      gid <- .as_scalar_char(grp[["id"]])
      if (is.null(gid) || !nzchar(gid)) next
      group_index[[gid]] <- list(
        variable = gf_var %||% "",
        label    = .as_scalar_char(grp[["label"]]) %||%
                   .as_scalar_char(grp[["name"]]) %||% "",
        where    = .group_where(grp)
      )
    }
  }

  out <- list()
  for (output in spec[["outputs"]] %||% list()) {
    out_id <- .as_scalar_char(output[["id"]])
    rgp    <- output[["resultGroupPaths"]]
    if (is.null(out_id) || is.null(rgp)) next
    raw_paths <- rgp[["paths"]] %||% list()
    if (length(raw_paths) == 0) next

    resolved <- list()
    broken   <- FALSE
    for (p in raw_paths) {
      gids <- as.character(unlist(p[["groupIds"]] %||% list()))
      wheres <- list()
      group_levels <- list()
      for (gid in gids) {
        entry <- group_index[[gid]]
        if (is.null(entry)) {
          broken <- TRUE
          break
        }
        wheres[[length(wheres) + 1L]] <- entry$where
        if (nzchar(entry$variable)) group_levels[[entry$variable]] <- entry$label
      }
      if (broken) break

      resolved[[length(resolved) + 1L]] <- list(
        path_id        = .as_scalar_char(p[["pathId"]]) %||% "",
        order          = as.integer(.as_scalar_char(p[["order"]]) %||%
                                      (length(resolved) + 1L)),
        role           = .as_scalar_char(p[["role"]]) %||% "DETAIL",
        label_path     = as.character(unlist(p[["labelPath"]] %||% list())),
        group_levels   = group_levels,
        condition      = do.call(combine_conditions, wheres),
        total_strategy = .as_scalar_char(p[["totalStrategy"]])
      )
    }
    ## An unresolvable path invalidates the whole declaration: executing a
    ## partial column set would silently drop display columns. The executor
    ## gate reports it (validate_ars_model names the exact problem).
    if (broken) next
    if (length(resolved) > 0) {
      resolved <- resolved[order(vapply(resolved, function(x) x$order, integer(1)))]
      out[[out_id]] <- list(
        mode  = .as_scalar_char(rgp[["mode"]]) %||% "NESTED",
        paths = resolved
      )
    }
  }
  out
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
#' @param grouping_map,analysis_to_output,grouping_groups,output_paths
#'   Optional pre-built lookup maps (see `.build_grouping_map` /
#'   `.build_analysis_to_output` / `.build_grouping_groups_map` /
#'   `.build_output_paths_map`); rebuilt from `spec` when `NULL`. Pass them in
#'   when resolving many analyses to avoid rework.
#'
#' @return A list with `analysis_id`, `output_id`, `method_id`, `dataset`,
#'   `variable` (raw, uncleaned), `by` (character vector of grouping vars),
#'   `pop_where`, `subset_where` (WhereClause objects or `NULL`),
#'   `include_total` (logical), `strata` (stratification variable for methods
#'   like CMH, or `NULL`), `group_defs` (named by grouping variable: ordered
#'   per-level `{label, order, condition}` definitions for annotation-defined
#'   column axes; empty list otherwise), `decode` (ordered list of
#'   `{value, label}` pairs from the spec codelist shipped in the
#'   ReportingEvent's `_meta$value_decodes` for this analysis variable, or
#'   `NULL` -- the executor/emitter derive a decoded factor from it),
#'   `subject_key`, `label`, `annotation`, `description`,
#'   `sap_description`, plus `paths` (the output's resolved result-group
#'   paths in display order, or `NULL` for flat/crossed tables) and
#'   `grouping_mode` (`"FLAT"` unless the output declares a hierarchy).
#' @noRd
resolve_analysis <- function(ana, spec, subject_key = "USUBJID",
                             grouping_map = NULL, analysis_to_output = NULL,
                             grouping_groups = NULL, output_paths = NULL) {
  if (is.null(grouping_map))       grouping_map       <- .build_grouping_map(spec)
  if (is.null(analysis_to_output)) analysis_to_output <- .build_analysis_to_output(spec)
  if (is.null(grouping_groups))    grouping_groups    <- .build_grouping_groups_map(spec)
  if (is.null(output_paths))       output_paths       <- .build_output_paths_map(spec)

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
  by_datasets <- character(0)
  group_defs <- list()
  grouping_ds <- .build_grouping_ds_map(spec)
  for (grp in ana[["orderedGroupings"]] %||% list()) {
    gf_id <- .as_scalar_char(grp[["groupingId"]])
    if (is.null(gf_id)) next
    gf_var <- grouping_map[[gf_id]]
    if (!is.null(gf_var) && nzchar(gf_var)) {
      by <- c(by, gf_var)
      by_datasets <- c(by_datasets, grouping_ds[[gf_id]] %||% NA_character_)
    }
    ## Condition-defined levels ride along keyed by the variable name, so
    ## the executor/emitter can derive the display grouping in-memory.
    entry <- grouping_groups[[gf_id]]
    if (!is.null(entry) && !is.null(gf_var) && nzchar(gf_var)) {
      group_defs[[gf_var]] <- entry$groups
    }
  }

  include_total <- isTRUE(as.logical(unlist(ana[["includeTotal"]])[1] %||% FALSE))

  ## Stratification operand for stratified methods (e.g. CMH). An arsbridge
  ## extension field on the analysis; a bare variable name resolved against the
  ## data later. Absent for the descriptive methods.
  strata <- .as_scalar_char(ana[["strata"]]) %||%
    .as_scalar_char(ana[["stratificationVariable"]])

  ## Spec-codelist decode for the analysis variable, shipped by
  ## build_ars_json() in _meta$value_decodes. Ordered {value, label} pairs;
  ## kept in the shipped order (build_ars_json already ordered the terms).
  decode <- NULL
  if (!is.null(variable) && nzchar(variable %||% "")) {
    bare_var <- toupper(sub("^.*\\.", "", variable))
    ds       <- toupper(dataset %||% "")
    entry <- spec[["_meta"]][["value_decodes"]][[paste0(ds, ".", bare_var)]]
    if (!is.null(entry) && length(entry) > 0) {
      decode <- lapply(entry, function(e) list(
        value = .as_scalar_char(e[["value"]]) %||% "",
        label = .as_scalar_char(e[["label"]]) %||% ""
      ))
      keep <- vapply(decode, function(d) nzchar(d$value) && nzchar(d$label),
                     logical(1))
      decode <- decode[keep]
      if (length(decode) == 0) decode <- NULL
    }
  }

  description <- .as_scalar_char(ana[["description"]]) %||%
    .as_scalar_char(ana[["name"]]) %||% analysis_id

  ## Declared result-group paths of this analysis's output. Non-NULL switches
  ## the executor/emitter to declared-path mode: one pass per path, never a
  ## cross of the grouping variables, and no separate include_total pass (the
  ## grand total is one of the paths).
  path_entry <- if (!is.null(output_id)) output_paths[[output_id]] else NULL
  paths <- path_entry$paths
  grouping_mode <- path_entry$mode %||% "FLAT"
  if (!is.null(paths)) include_total <- FALSE

  list(
    analysis_id     = analysis_id,
    output_id       = output_id,
    method_id       = method_id,
    dataset         = dataset,
    variable        = variable,
    by              = by,
    ## Parallel to `by`: the dataset each grouping variable comes from, when
    ## the ARS says. Drives the denominator join -- see .denom_join_expr().
    by_datasets     = by_datasets,
    pop_where       = pop_where,
    subset_where    = subset_where,
    include_total   = include_total,
    strata          = strata,
    group_defs      = group_defs,
    decode          = decode,
    paths           = paths,
    grouping_mode   = grouping_mode,
    subject_key     = subject_key,
    label           = .as_scalar_char(ana[["label"]]),
    annotation      = .as_scalar_char(ana[["annotation"]]),
    description     = description,
    sap_description = .as_scalar_char(ana[["sapDescription"]])
  )
}
