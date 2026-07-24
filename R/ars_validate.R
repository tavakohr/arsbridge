## arsbridge -- ars_validate.R
## ---------------------------------------------------------------------------
## Integrity checks over an ars_model. This is the layer that makes the review
## stage GUIDED correction rather than a generic JSON grid: it answers "what is
## wrong with this reporting event, and what should I do about it?"
##
## Everything here takes the model (and optionally an ADaM spec and the
## annotation validation report) as plain arguments -- no Shiny, no LLM, no
## file IO -- so the same findings are available from a script and from the
## editor's validation panel.
##
## Findings follow the shape ars_validate_supplement() established: one row per
## problem, ordered severity-first, with an action a reviewer can act on.

## Methods the engine can compute natively, plus the listing pass-through.
#' @noRd
.NATIVE_METHOD_IDS <- function() {
  c(names(.ARD_EXECUTORS), "MTH_LISTING")
}

#' @noRd
.new_findings <- function() {
  data.frame(
    severity = character(0),
    entity   = character(0),
    id       = character(0),
    field    = character(0),
    problem  = character(0),
    action   = character(0),
    stringsAsFactors = FALSE
  )
}

#' @noRd
.add_finding <- function(findings, severity, entity, id, field, problem,
                         action) {
  rbind(findings, data.frame(
    severity = severity,
    entity   = entity,
    id       = id,
    field    = field,
    problem  = problem,
    action   = action,
    stringsAsFactors = FALSE
  ))
}

## How a method will behave when ars_to_ard() reaches it. This is the check
## that tells a reviewer "this line will not produce numbers" before they run
## the engine rather than after.
#' @noRd
.method_execution_class <- function(method_id, strata = NA_character_) {
  if (is.na(method_id) || !nzchar(method_id)) return("missing")

  if (method_id %in% .NATIVE_METHOD_IDS()) return("native")

  if (identical(method_id, "MTH_CMH_TEST")) {
    return(if (!is.na(strata) && nzchar(strata)) "conditional" else "blocked")
  }
  if (identical(method_id, "MTH_PROPORTION_CI_EXACT")) return("conditional")
  if (method_id %in% names(.UNEXECUTABLE_METHODS)) return("unsupported")

  "fallback"
}

## Ids referenced by the tables of contents. The LOPA sublists are padded by
## repeating the last analysis id (a siera workaround), so duplicates here are
## expected and must not be reported as a problem.
#' @noRd
.contents_referenced_ids <- function(template) {
  analysis_ids <- character(0)
  output_ids   <- character(0)

  lopa <- template[["mainListOfContents"]]
  for (item in lopa[["contentsList"]][["listItems"]] %||% list()) {
    output_ids <- c(output_ids, .chr_field(item[["outputId"]]))
    for (sub in item[["sublist"]][["listItems"]] %||% list()) {
      analysis_ids <- c(analysis_ids, .chr_field(sub[["analysisId"]]))
    }
  }

  for (lopo in template[["otherListsOfContents"]] %||% list()) {
    for (item in lopo[["contentsList"]][["listItems"]] %||% list()) {
      output_ids <- c(output_ids, .chr_field(item[["outputId"]]))
    }
  }

  list(
    analyses = unique(stats::na.omit(analysis_ids)),
    outputs  = unique(stats::na.omit(output_ids))
  )
}

#' @noRd
.check_ids <- function(findings, model) {
  labels <- c(
    analyses      = "analysis",
    methods       = "method",
    analysis_sets = "analysis set",
    data_subsets  = "data subset",
    groupings     = "grouping",
    outputs       = "output"
  )

  for (pool in names(labels)) {
    ids <- model[[pool]]$id

    missing <- which(is.na(ids) | !nzchar(ids))
    for (i in missing) {
      findings <- .add_finding(
        findings, "FAIL", pool, paste0("row ", i), "id",
        paste0("This ", labels[[pool]], " has no id."),
        "Give it a unique id -- every reference in the event resolves by id."
      )
    }

    duplicated_ids <- unique(ids[duplicated(ids) & !is.na(ids)])
    for (dup in duplicated_ids) {
      findings <- .add_finding(
        findings, "FAIL", pool, dup, "id",
        paste0("Id ", dup, " is used by more than one ", labels[[pool]], "."),
        "Make each id unique -- references to it are ambiguous."
      )
    }
  }

  findings
}

#' @noRd
.check_references <- function(findings, model) {
  analyses <- model$analyses

  ## Shared entities referenced by each analysis.
  reference_checks <- list(
    list(column = "methodId",      pool = "methods",
         what = "method"),
    list(column = "analysisSetId", pool = "analysis_sets",
         what = "analysis set"),
    list(column = "dataSubsetId",  pool = "data_subsets",
         what = "data subset")
  )

  for (check in reference_checks) {
    known <- model[[check$pool]]$id
    values <- analyses[[check$column]]

    for (i in seq_len(nrow(analyses))) {
      value <- values[i]
      ## An empty dataSubsetId means "no subset", which is a valid state.
      if (is.na(value) || !nzchar(value)) next
      if (value %in% known) next

      findings <- .add_finding(
        findings, "FAIL", "analyses", analyses$id[i], check$column,
        paste0("References ", check$what, " ", value,
               ", which is not in the reporting event."),
        paste0("Point it at an existing ", check$what,
               ", or add ", value, " to the ", check$what, " pool.")
      )
    }
  }

  ## Grouping references.
  known_groupings <- model$groupings$id
  for (i in seq_len(nrow(analyses))) {
    for (grouping_id in .split_values(analyses$grouping_ids[i])) {
      if (grouping_id %in% known_groupings) next
      findings <- .add_finding(
        findings, "FAIL", "analyses", analyses$id[i], "grouping_ids",
        paste0("References grouping ", grouping_id,
               ", which is not in the reporting event."),
        "Point it at an existing grouping, or add that grouping."
      )
    }
  }

  ## Output -> analysis references, and analyses no output shows.
  known_analyses <- analyses$id
  referenced <- character(0)
  for (i in seq_len(nrow(model$outputs))) {
    ids <- .split_values(model$outputs$referenced_analysis_ids[i])
    referenced <- c(referenced, ids)
    for (analysis_id in setdiff(ids, known_analyses)) {
      findings <- .add_finding(
        findings, "FAIL", "outputs", model$outputs$id[i],
        "referenced_analysis_ids",
        paste0("References analysis ", analysis_id,
               ", which is not in the reporting event."),
        "Remove the reference, or add the analysis it points at."
      )
    }
  }

  for (analysis_id in setdiff(known_analyses, referenced)) {
    findings <- .add_finding(
      findings, "WARN", "analyses", analysis_id, "output_id",
      "No output references this analysis, so nothing will display it.",
      "Add it to an output's analysis list, or delete it."
    )
  }

  ## Tables of contents. These are regenerated on a structural save, so a
  ## stale reference is a note rather than a blocker.
  contents <- .contents_referenced_ids(model$template)
  for (analysis_id in setdiff(contents$analyses, known_analyses)) {
    findings <- .add_finding(
      findings, "WARN", "contents", analysis_id, "mainListOfContents",
      paste0("The table of contents lists analysis ", analysis_id,
             ", which is not in the reporting event."),
      "Saving after any structural change rebuilds the contents lists."
    )
  }
  for (output_id in setdiff(contents$outputs, model$outputs$id)) {
    findings <- .add_finding(
      findings, "WARN", "contents", output_id, "listOfContents",
      paste0("The table of contents lists output ", output_id,
             ", which is not in the reporting event."),
      "Saving after any structural change rebuilds the contents lists."
    )
  }

  findings
}

#' @noRd
.check_methods <- function(findings, model) {
  analyses <- model$analyses

  for (i in seq_len(nrow(analyses))) {
    method_id <- analyses$methodId[i]
    strata    <- analyses$strata[i]
    class     <- .method_execution_class(method_id, strata)

    if (identical(class, "missing")) {
      findings <- .add_finding(
        findings, "FAIL", "analyses", analyses$id[i], "methodId",
        "No method is assigned, so this analysis cannot be executed.",
        "Assign a method -- the engine computes results from it."
      )
    } else if (identical(class, "blocked")) {
      findings <- .add_finding(
        findings, "WARN", "analyses", analyses$id[i], "strata",
        "A CMH test needs a stratification variable, and none is set.",
        "Set the stratification variable, or the engine reserves an empty cell."
      )
    } else if (identical(class, "unsupported")) {
      findings <- .add_finding(
        findings, "WARN", "analyses", analyses$id[i], "methodId",
        paste0("Method ", method_id,
               " has no executor, so the engine reserves an empty cell."),
        "Plan to compute this result manually, or choose an executable method."
      )
    } else if (identical(class, "fallback")) {
      findings <- .add_finding(
        findings, "WARN", "analyses", analyses$id[i], "methodId",
        paste0("Method ", method_id,
               " has no executor; the generic summarizer runs instead."),
        "Check the result is what the shell asks for, or change the method."
      )
    } else if (identical(class, "conditional")) {
      findings <- .add_finding(
        findings, "INFO", "analyses", analyses$id[i], "methodId",
        paste0("Method ", method_id,
               " executes only when its prerequisites are met."),
        "No action needed if the required package and operands are present."
      )
    }
  }

  findings
}

## Populations arsbridge could not parse into a where-clause keep the raw
## annotation text instead. They are honest, but the engine cannot filter on
## them, so the reviewer needs to know.
#' @noRd
.check_unparsed_populations <- function(findings, model) {
  sets <- model$analysis_sets
  for (i in seq_len(nrow(sets))) {
    if (is.na(sets$annotationText[i])) next
    findings <- .add_finding(
      findings, "WARN", "analysis_sets", sets$id[i], "annotationText",
      paste0("The population \"", sets$annotationText[i],
             "\" was not parsed into a condition, so it filters nothing."),
      "Express it as a condition, or confirm the analysis is unfiltered."
    )
  }
  findings
}

## Composite columns join several values with ";". A value that contains the
## separator would split wrongly on the next edit.
#' @noRd
.check_separator_safety <- function(findings, model) {
  contains_separator <- function(x) {
    !is.na(x) & grepl(.MODEL_SEP, x, fixed = TRUE)
  }

  for (pool in c("analysis_sets", "data_subsets")) {
    df <- model[[pool]]
    hits <- which(contains_separator(df$condition_value) & !df$is_compound)
    for (i in hits) {
      findings <- .add_finding(
        findings, "INFO", pool, df$id[i], "condition_value",
        paste0("A condition value contains a semicolon, which the editor ",
               "uses to separate values."),
        "Edit this condition through the raw-JSON escape hatch instead."
      )
    }
  }

  findings
}

## --- spec overlay ----------------------------------------------------------
## Wired in phase 2, when the editor loads the ADaM spec alongside the JSON.

#' @noRd
.check_against_spec <- function(findings, model, spec) {
  known_datasets <- unique(spec$variables$dataset)

  check_reference <- function(findings, entity, id, field, dataset, variable) {
    if (is.na(dataset) || !nzchar(dataset)) return(findings)

    if (!dataset %in% known_datasets) {
      return(.add_finding(
        findings, "FAIL", entity, id, field,
        paste0("Dataset ", dataset, " is not in the ADaM spec."),
        "Correct the dataset, or add it to the spec."
      ))
    }
    if (is.na(variable) || !nzchar(variable)) return(findings)

    key <- paste0(dataset, ".", variable)
    if (!is.null(spec$lookup[[key]])) return(findings)

    .add_finding(
      findings, "WARN", entity, id, field,
      paste0("Variable ", key, " is not in the ADaM spec."),
      "Correct the variable, or confirm it is derived downstream."
    )
  }

  for (i in seq_len(nrow(model$analyses))) {
    findings <- check_reference(
      findings, "analyses", model$analyses$id[i], "variable",
      model$analyses$dataset[i], model$analyses$variable[i]
    )
  }

  for (i in seq_len(nrow(model$groupings))) {
    findings <- check_reference(
      findings, "groupings", model$groupings$id[i], "groupingVariable",
      model$groupings$groupingDataset[i], model$groupings$groupingVariable[i]
    )
  }

  for (pool in c("analysis_sets", "data_subsets")) {
    df <- model[[pool]]
    for (i in seq_len(nrow(df))) {
      if (isTRUE(df$is_compound[i])) next
      findings <- check_reference(
        findings, pool, df$id[i], "condition_variable",
        df$condition_dataset[i], df$condition_variable[i]
      )
    }
  }

  findings
}

## --- gap detection ---------------------------------------------------------
## The annotation validation report already knows every line the programmer
## annotated in the shell. An annotation with no matching analysis is a line
## the generator missed -- the single most valuable thing this tool surfaces.

#' @noRd
.check_gaps <- function(findings, model, report) {
  required <- c("tlf_number", "stub_label", "annotation", "variable_ref")
  if (!all(required %in% names(report))) {
    cli::cli_warn(c(
      "Ignoring {.arg report}: it does not look like a validation report.",
      "i" = "Expected the columns {.val {required}}."
    ))
    return(findings)
  }

  analyses <- model$analyses

  ## Match on DATASET.VARIABLE rather than the annotation string: the
  ## dataset and variable are always on the analysis, whereas the annotation
  ## text is only shipped when spec_to_ars(ship_annotations = TRUE).
  analysis_refs <- paste0(analyses$dataset, ".", analyses$variable)

  for (i in seq_len(nrow(report))) {
    ## A <population> row describes the analysis set, not an analysis line.
    if (identical(report$stub_label[i], "<population>")) next

    tlf <- report$tlf_number[i]
    if (is.na(tlf) || !nzchar(tlf)) next

    output_id <- make_output_id(tlf)
    if (!output_id %in% model$outputs$id) next

    variable_ref <- report$variable_ref[i]
    if (is.na(variable_ref) || !nzchar(variable_ref)) next

    in_output <- !is.na(analyses$output_id) & analyses$output_id == output_id
    if (variable_ref %in% analysis_refs[in_output]) next

    stub <- report$stub_label[i]
    described <- if (is.na(stub) || !nzchar(stub)) variable_ref else stub

    findings <- .add_finding(
      findings, "WARN", "outputs", output_id, "analyses",
      paste0("The shell annotates \"", described, "\" (", variable_ref,
             ") but no analysis in this output uses that variable."),
      "Add the missing analysis, or confirm the line is not an analysis."
    )
  }

  findings
}


#' Check an ARS model for integrity, spec and coverage problems
#'
#' Runs the checks that make the review stage guided rather than generic: that
#' every reference resolves, that every method can actually be executed, that
#' variables exist in the ADaM spec, and that no annotated shell line was
#' missed by the generator.
#'
#' @param model An `ars_model` from [ars_to_model()].
#' @param spec Optional ADaM spec, as returned by the package's spec reader
#'   (a list with `variables` and `lookup`). When supplied, datasets and
#'   variables are checked against it.
#' @param report Optional annotation validation report -- the data frame
#'   `spec_to_ars()` returns as `$validation`, or the "Validation" sheet of the
#'   report it writes. When supplied, annotated shell lines with no
#'   corresponding analysis are reported as gaps.
#'
#' @return A data frame of findings, most severe first, with columns
#'   `severity` (`"FAIL"`, `"WARN"` or `"INFO"`), `entity` (the pool the
#'   finding is about), `id`, `field`, `problem` and `action`. Zero rows means
#'   nothing to fix.
#'
#' @section What is checked:
#' \describe{
#'   \item{Identity}{Every entity has an id, and no id is used twice.}
#'   \item{References}{Every `methodId`, `analysisSetId`, `dataSubsetId` and
#'     grouping id resolves, and every output references analyses that exist.
#'     An empty `dataSubsetId` means "no subset" and is not a dangling
#'     reference. Analyses no output displays are reported.}
#'   \item{Executability}{Whether [ars_to_ard()] can compute each analysis
#'     natively, needs a prerequisite, falls back to the generic summarizer,
#'     or will reserve an empty cell for manual computation.}
#'   \item{Populations}{Analysis sets whose population text could not be
#'     parsed into a condition, and so filter nothing.}
#'   \item{Spec}{With `spec`: datasets and variables that are not in the ADaM
#'     spec.}
#'   \item{Coverage}{With `report`: shell annotations that no analysis
#'     carries -- lines the generator missed.}
#' }
#'
#' @seealso [ars_to_model()], [model_to_ars()].
#'
#' @examples
#' \dontrun{
#' model <- ars_to_model("reporting_event.json")
#' findings <- validate_ars_model(model)
#' subset(findings, severity == "FAIL")
#' }
#' @export
validate_ars_model <- function(model, spec = NULL, report = NULL) {
  .assert_ars_model(model)

  findings <- .new_findings()
  findings <- .check_ids(findings, model)
  findings <- .check_references(findings, model)
  findings <- .check_methods(findings, model)
  findings <- .check_unparsed_populations(findings, model)
  findings <- .check_separator_safety(findings, model)

  if (!is.null(spec)) {
    findings <- .check_against_spec(findings, model, spec)
  }
  if (!is.null(report) && nrow(report) > 0) {
    findings <- .check_gaps(findings, model, report)
  }

  ## Most severe first, so the panel and the console summary agree on order.
  severity_rank <- match(findings$severity, c("FAIL", "WARN", "INFO"))
  findings <- findings[order(severity_rank), , drop = FALSE]
  rownames(findings) <- NULL
  findings
}
