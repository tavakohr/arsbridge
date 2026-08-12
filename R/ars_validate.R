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
    ref      = character(0),
    stringsAsFactors = FALSE
  )
}

## `ref` carries whatever the finding is about in machine-readable form -- a
## DATASET.VARIABLE for a gap, for instance -- so the editor can act on a
## finding rather than only describe it.
#' @noRd
.add_finding <- function(findings, severity, entity, id, field, problem,
                         action, ref = NA_character_) {
  rbind(findings, data.frame(
    severity = severity,
    entity   = entity,
    id       = id,
    field    = field,
    problem  = problem,
    action   = action,
    ref      = ref,
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

  ## An output with nothing behind it renders empty. The generator leaves
  ## these behind when it cannot derive any analysis for a display, so this is
  ## one of the most common things a reviewer has to fix.
  for (i in seq_len(nrow(model$outputs))) {
    if (length(.split_values(model$outputs$referenced_analysis_ids[i])) > 0) {
      next
    }
    findings <- .add_finding(
      findings, "WARN", "outputs", model$outputs$id[i],
      "referenced_analysis_ids",
      "This output has no analyses, so it would render empty.",
      "Add the analyses it should display, or drop the output."
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
.check_grouping_shapes <- function(findings, model) {
  groupings <- model$groupings

  is_fixed <- is.na(groupings$dataDriven) | !groupings$dataDriven
  invalid <- which(is_fixed & groupings$n_groups == 0L)
  for (i in invalid) {
    findings <- .add_finding(
      findings, "FAIL", "groupings", groupings$id[i], "groups",
      "This fixed grouping declares no groups, so it defines no result columns.",
      "Add the fixed groups, or mark the grouping as data-driven.",
      ref = "FIXED_GROUPING_EMPTY"
    )
  }

  findings
}

#' Result-column labels carried by the display for both Word and Excel shells.
#' Arsbridge-authored shell layouts retain the physical stub as the first
#' display column. Displays without shell layout metadata use the compact ARS
#' shape and carry result columns only.
#' @noRd
.flat_display_labels <- function(output_node) {
  display <- .display_node(output_node)
  labels <- vapply(
    display[["columns"]] %||% list(),
    function(column) .chr_field(column[["label"]]),
    character(1)
  )
  if (!is.null(.shell_layout(output_node)) && length(labels) > 0L) {
    labels <- labels[-1]
  }

  labels <- .strip_n_placeholder(labels[!is.na(labels)])
  labels
}

#' @noRd
.check_flat_axes <- function(findings, model) {
  for (i in seq_len(nrow(model$outputs))) {
    if (!identical(model$outputs$outputType[i], "TABLE")) next

    output_node <- model$outputs$raw[[i]]

    tree_mode <- .chr_field(output_node[["_meta"]][["column_tree"]][["mode"]])
    if (!is.na(tree_mode) &&
        tree_mode %in% c("NESTED", "ASYMMETRIC_NESTED")) {
      next
    }

    analysis_ids <- .split_values(model$outputs$referenced_analysis_ids[i])
    analysis_rows <- match(analysis_ids, model$analyses$id)
    analysis_rows <- analysis_rows[!is.na(analysis_rows)]
    if (length(analysis_rows) == 0) next

    fixed_by_analysis <- vapply(analysis_rows, function(analysis_row) {
      grouping_ids <- .split_values(
        model$analyses$grouping_ids[analysis_row]
      )
      grouping_rows <- match(grouping_ids, model$groupings$id)
      grouping_rows <- grouping_rows[!is.na(grouping_rows)]
      fixed_rows <- grouping_rows[
        !is.na(model$groupings$dataDriven[grouping_rows]) &
          !model$groupings$dataDriven[grouping_rows] &
          model$groupings$n_groups[grouping_rows] > 0L
      ]
      if (length(fixed_rows) == 0L) NA_integer_ else fixed_rows[[1]]
    }, integer(1))
    fixed_rows <- unique(stats::na.omit(fixed_by_analysis))
    if (length(fixed_rows) == 0L) next

    signatures <- vapply(fixed_rows, function(grouping_row) {
      .grouping_signature(model$groupings$raw[[grouping_row]])
    }, character(1))
    if (length(unique(signatures)) > 1L) {
      group_counts <- model$groupings$n_groups[fixed_rows]
      ref <- if (length(unique(group_counts)) > 1L) {
        "FLAT_AXIS_COLUMN_COUNT_MISMATCH"
      } else {
        "FLAT_AXIS_COLUMN_LABEL_MISMATCH"
      }
      findings <- .add_finding(
        findings, "FAIL", "outputs", model$outputs$id[i], "columns",
        "The analyses displayed on this flat output use different fixed grouping definitions.",
        "Make every displayed analysis reference the same flat column grouping definition.",
        ref = ref
      )
      next
    }

    ## The first fixed grouping in orderedGroupings is the flat column axis.
    ## Resolving through each analysis reference is essential: registration may
    ## have renamed this definition to a variant id during deduplication.
    grouping_row <- fixed_rows[[1]]
    groups <- model$groupings$raw[[grouping_row]][["groups"]] %||% list()
    group_order <- vapply(groups, function(group) {
      .int_field(group[["order"]])
    }, integer(1))
    groups <- groups[order(group_order, na.last = TRUE)]
    group_labels <- vapply(groups, function(group) {
      .chr_field(group[["label"]] %||% group[["name"]])
    }, character(1))
    group_labels <- .strip_n_placeholder(group_labels)

    result_labels <- .flat_display_labels(output_node)

    analysis_nodes <- model$analyses$raw[analysis_rows]
    includes_total <- vapply(analysis_nodes, function(analysis) {
      isTRUE(analysis[["includeTotal"]])
    }, logical(1))
    if (length(unique(includes_total)) > 1L) {
      findings <- .add_finding(
        findings, "FAIL", "outputs", model$outputs$id[i], "columns",
        "The analyses displayed on this flat output disagree on whether a Total column is included.",
        "Make every displayed analysis use the same includeTotal setting.",
        ref = "FLAT_AXIS_COLUMN_COUNT_MISMATCH"
      )
      next
    }

    has_total <- includes_total[[1]]
    expected_count <- length(group_labels) + as.integer(has_total)

    if (length(result_labels) != expected_count) {
      findings <- .add_finding(
        findings, "FAIL", "outputs", model$outputs$id[i], "columns",
        sprintf(
          "The flat shell displays %d result columns but grouping %s defines %d.",
          length(result_labels), model$groupings$id[grouping_row], expected_count
        ),
        "Make the display columns match the grouping levels and optional Total column.",
        ref = "FLAT_AXIS_COLUMN_COUNT_MISMATCH"
      )
      next
    }

    displayed_groups <- result_labels
    total_matches <- integer(0)
    if (has_total) {
      total_labels <- vapply(analysis_nodes, function(analysis) {
        label <- .chr_field(analysis[["totalLabel"]])
        if (is.na(label) || !nzchar(label)) "Total" else label
      }, character(1))
      total_labels <- .strip_n_placeholder(total_labels)
      if (length(unique(.fold_label(total_labels))) > 1L) {
        findings <- .add_finding(
          findings, "FAIL", "outputs", model$outputs$id[i], "columns",
          "The analyses displayed on this flat output use different Total labels.",
          "Give every displayed analysis the same Total label.",
          ref = "FLAT_AXIS_COLUMN_LABEL_MISMATCH"
        )
        next
      }

      total_matches <- which(
        .fold_label(result_labels) == .fold_label(total_labels[[1]])
      )
      if (length(total_matches) == 1L) {
        displayed_groups <- result_labels[-total_matches]
      }
    }

    labels_match <- identical(displayed_groups, group_labels)
    if (has_total) labels_match <- labels_match && length(total_matches) == 1L
    if (!labels_match) {
      findings <- .add_finding(
        findings, "FAIL", "outputs", model$outputs$id[i], "columns",
        sprintf(
          "The flat shell column labels do not match grouping %s.",
          model$groupings$id[grouping_row]
        ),
        "Use the grouping's labels in display order, including the declared Total label.",
        ref = "FLAT_AXIS_COLUMN_LABEL_MISMATCH"
      )
    }
  }

  findings
}

#' @noRd
.check_method_placeholder_slots <- function(findings, model) {
  method_ids <- model$methods$id[
    !is.na(model$methods$id) & nzchar(model$methods$id)
  ]
  stats_by_method <- stats::setNames(lapply(method_ids, function(method_id) {
    slots <- .method_operation_slots(model$methods$raw, method_id)
    available <- vapply(slots, function(slot) {
      slot$stat_name %||% NA_character_
    }, character(1))
    stats::na.omit(available)
  }), method_ids)
  method_by_analysis <- stats::setNames(
    model$analyses$methodId,
    model$analyses$id
  )

  check_request <- function(findings, analysis_id, description,
                            requested = character(0), n_slots = length(requested)) {
    if (is.na(analysis_id) || !nzchar(analysis_id)) return(findings)
    method_id <- method_by_analysis[[analysis_id]] %||% NA_character_
    if (is.na(method_id) || !nzchar(method_id)) return(findings)

    available <- stats_by_method[[method_id]] %||% character(0)
    missing <- setdiff(requested, available)
    too_many <- n_slots > length(available)
    if (length(missing) == 0L && !too_many) return(findings)

    problem <- if (length(missing) > 0L) {
      sprintf(
        "%s requests %s, which method %s does not provide.",
        description, paste(missing, collapse = ", "), method_id
      )
    } else {
      sprintf(
        "%s has %d slots, but method %s provides %d visible operation%s.",
        description, n_slots, method_id, length(available),
        if (length(available) == 1L) "" else "s"
      )
    }

    .add_finding(
      findings, "FAIL", "analyses", analysis_id, "methodId",
      problem,
      "Assign a method whose operation slots cover every statistic on this line.",
      ref = "METHOD_PLACEHOLDER_SLOT_MISMATCH"
    )
  }

  for (i in seq_len(nrow(model$outputs))) {
    output_node <- model$outputs$raw[[i]]
    layout <- .shell_layout(output_node)
    if (is.null(layout)) next

    ## Excel persists the concrete placeholder tokens and their statistic
    ## bindings. Check each distinct row request once, not once per result
    ## column.
    cells <- output_node[["_meta"]][["shell_fill"]][["cells"]] %||% list()
    seen_cells <- character(0)
    for (cell in cells) {
      if (!identical(.chr_field(cell[["kind"]]), "result")) next
      slots <- cell[["slots"]] %||% list()
      if (length(slots) == 0L) next

      analysis_id <- .chr_field(cell[["analysis_id"]])
      placeholder <- .chr_field(cell[["placeholder"]])
      if (is.na(placeholder) || !nzchar(placeholder)) placeholder <- "?"
      requested <- vapply(slots, function(slot) {
        .chr_field(slot[["stat_name"]])
      }, character(1))
      requested <- unique(requested[!is.na(requested) & nzchar(requested)])
      key <- paste(
        analysis_id, placeholder, paste(requested, collapse = ","),
        length(slots), sep = "|"
      )
      if (key %in% seen_cells) next
      seen_cells <- c(seen_cells, key)

      findings <- check_request(
        findings,
        analysis_id,
        sprintf("Placeholder '%s'", placeholder),
        requested = requested,
        n_slots = length(slots)
      )
    }

    ## Word has no cell addresses, but its parser records the number of
    ## placeholders on each authored row. Use that count when no Excel cell map
    ## is available.
    if (length(cells) == 0L) {
      for (j in seq_len(nrow(layout))) {
        n_slots <- layout$n_slots[j]
        if (is.na(n_slots) || n_slots == 0L) next
        findings <- check_request(
          findings,
          layout$analysis_id[j],
          sprintf("Shell row '%s'", layout$label[j]),
          n_slots = n_slots
        )
      }
    }

    ## Statistic-line labels remain useful for older repair models that predate
    ## persisted placeholder metadata.
    rows <- .shell_table_data(output_node, model)$rows
    for (j in seq_len(nrow(rows))) {
      requested <- .stats_for_line(rows$label[j])
      analysis_id <- rows$owner_analysis_id[j]
      method_id <- rows$method_id[j]
      if (is.null(requested) || is.na(analysis_id) || is.na(method_id)) next

      available <- stats_by_method[[method_id]] %||% character(0)
      missing <- setdiff(requested, available)
      if (length(missing) == 0L) next

      findings <- .add_finding(
        findings, "FAIL", "analyses", analysis_id, "methodId",
        sprintf(
          "Shell line '%s' requests %s, which method %s does not provide.",
          rows$label[j], paste(missing, collapse = ", "), method_id
        ),
        "Assign a method whose operation slots cover every statistic on this line.",
        ref = "METHOD_PLACEHOLDER_SLOT_MISMATCH"
      )
    }
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
      if (isTRUE(df$is_compound[i])) {
        ## The flat columns hold nothing for a compound, so the loop above
        ## skipped it entirely and never looked at its clauses.
        for (ref in .where_clause_refs(df$raw[[i]])) {
          findings <- check_reference(
            findings, pool, df$id[i], "condition_variable",
            ref$dataset, ref$variable
          )
        }
        next
      }
      findings <- check_reference(
        findings, pool, df$id[i], "condition_variable",
        df$condition_dataset[i], df$condition_variable[i]
      )
    }
  }

  ## A grouping's CHILD groups define the result columns, and each carries its
  ## own where clause -- the one place a wrong variable is both easiest to
  ## write and hardest to notice, because the column still renders, just
  ## empty. Nothing looked at them until now.
  for (i in seq_len(nrow(model$groupings))) {
    groups <- model$groupings$raw[[i]][["groups"]] %||% list()
    for (group in groups) {
      group_id <- .chr_field(group[["id"]])
      for (ref in .where_clause_refs(group)) {
        findings <- check_reference(
          findings, "groupings", model$groupings$id[i],
          paste0("group ", .blank_na(group_id), " condition"),
          ref$dataset, ref$variable
        )
      }
    }
  }

  findings
}

## Every (dataset, variable) a where clause names, however deeply nested.
##
## A condition contributes one pair; a compoundExpression contributes its
## clauses, recursively, so a wrong variable is found at any depth rather than
## only at the top. Duplicates are dropped so one misspelling in a clause used
## twice is one finding, not two.
#' @noRd
.where_clause_refs <- function(where) {
  if (is.null(where) || !is.list(where)) return(list())

  refs <- list()
  if (!is.null(where[["condition"]])) {
    condition <- where[["condition"]]
    refs <- list(list(dataset  = .chr_field(condition[["dataset"]]),
                      variable = .chr_field(condition[["variable"]])))
  } else if (!is.null(where[["compoundExpression"]])) {
    clauses <- where[["compoundExpression"]][["whereClauses"]] %||% list()
    refs <- unlist(lapply(clauses, .where_clause_refs), recursive = FALSE)
  }

  refs <- Filter(function(ref) {
    !is.na(ref$dataset) || !is.na(ref$variable)
  }, refs %||% list())

  keys <- vapply(refs, function(ref) {
    paste0(.blank_na(ref$dataset), ".", .blank_na(ref$variable))
  }, character(1))
  refs[!duplicated(keys)]
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
      "Add the missing analysis, or confirm the line is not an analysis.",
      ref = variable_ref
    )
  }

  findings
}


#' Structural checks over each output's declared result-group paths (the
#' hierarchical column model). The codes ride in `ref` so the editor and
#' tests can key on them:
#'
#'   HEADER_TREE_MISSING                      WARN
#'   DISPLAY_COLUMN_COUNT_MISMATCH            FAIL
#'   UNMAPPED_LEAF_COLUMN                     FAIL
#'   INVALID_CARTESIAN_PRODUCT                FAIL
#'   GROUPING_VARIABLE_NOT_LINKED             FAIL
#'   SUBTOTAL_SCOPE_UNDEFINED                 FAIL
#'   DUPLICATE_RESULT_PATH                    FAIL
#'   GROUPING_ORDER_AMBIGUOUS                 WARN
#'   SUBTOTAL_EXCLUDES_UNDISPLAYED_CATEGORIES INFO
#'
## Nested block integrity (HANDOFF_nested_soc_pt_hierarchy, Phase N4). A
## nested_child layout row must resolve to a nested_parent row through
## parent_order, and its analysis must carry the parent's variable as a row
## grouping -- without the link the renderer degrades to a flat block,
## without the grouping the child's results cannot nest at all.
##
##   NESTED_CHILD_UNLINKED    WARN
##   NESTED_GROUPING_MISSING  WARN
#' @noRd
.check_nested_layout <- function(findings, model) {
  for (i in seq_len(nrow(model$outputs))) {
    layout <- .shell_layout(model$outputs$raw[[i]])
    if (is.null(layout)) next
    output_id <- model$outputs$id[i]

    for (j in which(layout$kind == "nested_child")) {
      parent_hit <- if (is.na(layout$parent_order[j])) {
        integer(0)
      } else {
        which(layout$order == layout$parent_order[j] &
                layout$kind == "nested_parent")
      }
      if (length(parent_hit) != 1) {
        findings <- .add_finding(
          findings, "WARN", "outputs", output_id, "shell_layout",
          sprintf(
            "Nested child row '%s' (order %d) has no linked parent row",
            layout$label[j], layout$order[j]),
          paste0("Relink parent_order to the parent-level row (or re-author ",
                 "the block) -- the renderer falls back to a flat block ",
                 "until then"),
          ref = "NESTED_CHILD_UNLINKED"
        )
        next
      }

      child_id  <- layout$analysis_id[j]
      parent_id <- layout$analysis_id[parent_hit]
      child_at  <- match(child_id, model$analyses$id)
      parent_at <- match(parent_id, model$analyses$id)
      ## Dangling analysis references are another check's finding.
      if (is.na(child_at) || is.na(parent_at)) next

      parent_var <- model$analyses$variable[parent_at]
      if (is.na(parent_var) || !nzchar(parent_var)) next
      parent_var <- toupper(parent_var)

      grouping_ids <- .split_values(model$analyses$grouping_ids[child_at])
      grouping_vars <- toupper(model$groupings$groupingVariable[
        match(grouping_ids, model$groupings$id)
      ])
      if (!parent_var %in% stats::na.omit(grouping_vars)) {
        findings <- .add_finding(
          findings, "WARN", "analyses", child_id, "grouping_ids",
          sprintf(
            "Nested child analysis does not group by its parent's variable (%s)",
            parent_var),
          paste0("Add ", parent_var, " as a data-driven grouping on this ",
                 "analysis -- without it the child's results cannot nest ",
                 "under the parent levels"),
          ref = "NESTED_GROUPING_MISSING"
        )
      }
    }
  }
  findings
}

#' @noRd
.check_result_paths <- function(findings, model) {
  outputs <- model$outputs
  if (nrow(outputs) == 0) return(findings)

  ## Group id -> its grouping factor and condition, across the pool.
  group_index <- list()
  for (i in seq_len(nrow(model$groupings))) {
    gf_node <- model$groupings$raw[[i]]
    for (g in gf_node[["groups"]] %||% list()) {
      gid <- .chr_field(g[["id"]])
      if (is.na(gid) || !nzchar(gid)) next
      group_index[[gid]] <- list(
        gf_id     = .chr_field(gf_node[["id"]]),
        condition = .group_where(g)
      )
    }
  }

  path_condition <- function(path) {
    conds <- lapply(unlist(path[["groupIds"]] %||% list()), function(gid) {
      entry <- group_index[[gid]]
      if (is.null(entry)) NULL else entry$condition
    })
    do.call(combine_conditions, conds)
  }
  path_label <- function(path) {
    paste(unlist(path[["labelPath"]] %||% list()), collapse = " > ")
  }

  for (i in seq_len(nrow(outputs))) {
    node   <- outputs$raw[[i]]
    out_id <- outputs$id[i]
    rgp    <- node[["resultGroupPaths"]]
    tree   <- node[["_meta"]][["column_tree"]]
    tree_mode <- .chr_field(tree[["mode"]])

    if (is.null(rgp)) {
      if (!is.na(tree_mode) &&
          tree_mode %in% c("NESTED", "ASYMMETRIC_NESTED")) {
        findings <- .add_finding(
          findings, "WARN", "outputs", out_id, "resultGroupPaths",
          "The shell header parsed as a hierarchical column tree, but this output declares no result-group paths.",
          "Regenerate the event -- without declared paths the executor falls back to flat groupings and the child columns are lost.",
          ref = "HEADER_TREE_MISSING"
        )
      }
      next
    }

    paths <- rgp[["paths"]] %||% list()
    tree_nodes <- tree[["nodes"]] %||% list()
    tree_ids   <- vapply(tree_nodes, function(n) .chr_field(n[["id"]]) %||% "",
                         character(1))
    leaf_nodes <- Filter(function(n) {
      .chr_field(n[["nodeType"]]) %in% c("leaf", "subtotal", "grand_total")
    }, tree_nodes)
    path_node_ids <- vapply(paths, function(p) .chr_field(p[["nodeId"]]) %||% "",
                            character(1))

    ## The shell's own column count is the ground truth: a path list that
    ## disagrees with it means display columns were added or lost.
    if (length(leaf_nodes) > 0 && length(paths) != length(leaf_nodes)) {
      findings <- .add_finding(
        findings, "FAIL", "outputs", out_id, "resultGroupPaths",
        sprintf("The shell header has %d result columns but %d path(s) are declared.",
                length(leaf_nodes), length(paths)),
        "Each display column needs exactly one declared path -- regenerate or fix the path list.",
        ref = "DISPLAY_COLUMN_COUNT_MISMATCH"
      )
    }
    for (leaf in leaf_nodes) {
      leaf_id <- .chr_field(leaf[["id"]])
      if (!is.na(leaf_id) && nzchar(leaf_id) && !leaf_id %in% path_node_ids) {
        findings <- .add_finding(
          findings, "FAIL", "outputs", out_id, "resultGroupPaths",
          sprintf("Shell column '%s' has no declared result path.",
                  .chr_field(leaf[["label"]]) %||% leaf_id),
          "Add the missing path (or regenerate) -- this display column would otherwise be silently dropped.",
          ref = "UNMAPPED_LEAF_COLUMN"
        )
      }
    }

    used_gf_order <- character(0)
    seen_conditions <- list()
    for (p in paths) {
      label <- path_label(p)
      role  <- .chr_field(p[["role"]]) %||% "DETAIL"
      gids  <- unlist(p[["groupIds"]] %||% list())

      ## A path pointing at no shell column is an invented combination --
      ## exactly the Cartesian product this model exists to prevent.
      nid <- .chr_field(p[["nodeId"]])
      if (length(tree_ids) > 0 && !is.na(nid) && nzchar(nid) &&
          !nid %in% tree_ids) {
        findings <- .add_finding(
          findings, "FAIL", "outputs", out_id, "resultGroupPaths",
          sprintf("Path '%s' matches no column of the shell header tree.", label),
          "Remove it -- only columns the shell displays may be declared.",
          ref = "INVALID_CARTESIAN_PRODUCT"
        )
      }
      gf_of <- vapply(gids, function(gid) {
        entry <- group_index[[gid]]
        if (is.null(entry)) NA_character_ else entry$gf_id
      }, character(1))
      if (anyDuplicated(stats::na.omit(gf_of))) {
        findings <- .add_finding(
          findings, "FAIL", "outputs", out_id, "resultGroupPaths",
          sprintf("Path '%s' references two groups of the same grouping factor.", label),
          "A column cannot be two levels of one variable at once -- fix the path's groupIds.",
          ref = "INVALID_CARTESIAN_PRODUCT"
        )
      }
      for (gid in gids) {
        if (is.null(group_index[[gid]])) {
          findings <- .add_finding(
            findings, "FAIL", "outputs", out_id, "resultGroupPaths",
            sprintf("Path '%s' references unknown group id '%s'.", label, gid),
            "Point the path at an existing group level, or add the missing group to its grouping factor.",
            ref = "GROUPING_VARIABLE_NOT_LINKED"
          )
        }
      }

      if (identical(role, "SUBTOTAL") &&
          (is.na(.chr_field(p[["totalStrategy"]])) || length(gids) == 0)) {
        findings <- .add_finding(
          findings, "FAIL", "outputs", out_id, "resultGroupPaths",
          sprintf("Subtotal path '%s' has no defined scope.", label),
          "Give it totalStrategy 'condition_based' and its parent group id -- an unscoped subtotal is ambiguous (parent condition vs sum of children).",
          ref = "SUBTOTAL_SCOPE_UNDEFINED"
        )
      }

      cond_key <- paste(deparse(canonicalize_condition(path_condition(p))),
                        collapse = "")
      prior <- seen_conditions[[cond_key]]
      if (!is.null(prior)) {
        findings <- .add_finding(
          findings, "FAIL", "outputs", out_id, "resultGroupPaths",
          sprintf("Paths '%s' and '%s' compose the same condition -- two columns would compute identical results.",
                  prior, label),
          "Make each path a distinct subject set (check the groupIds).",
          ref = "DUPLICATE_RESULT_PATH"
        )
      } else {
        seen_conditions[[cond_key]] <- label
      }

      used_gf_order <- union(used_gf_order, stats::na.omit(gf_of))
    }

    ## The subtotal-vs-children scope question is the one a reviewer must
    ## answer deliberately: a subtotal equal to the OR of its displayed
    ## children may be excluding unknown-category subjects.
    detail_paths <- Filter(function(p) identical(.chr_field(p[["role"]]), "DETAIL"), paths)
    for (p in paths) {
      if (!identical(.chr_field(p[["role"]]), "SUBTOTAL")) next
      lp <- unlist(p[["labelPath"]] %||% list())
      if (length(lp) < 2) next
      prefix <- lp[-length(lp)]
      siblings <- Filter(function(d) {
        dlp <- unlist(d[["labelPath"]] %||% list())
        length(dlp) == length(lp) && identical(dlp[-length(dlp)], prefix)
      }, detail_paths)
      if (length(siblings) == 0) next
      union_cond <- do.call(
        combine_conditions,
        c(lapply(siblings, path_condition), list(operator = "OR"))
      )
      if (conditions_equal(path_condition(p), union_cond)) {
        findings <- .add_finding(
          findings, "INFO", "outputs", out_id, "resultGroupPaths",
          sprintf("Subtotal '%s' equals the union of its displayed children -- it may exclude subjects whose category is unknown.",
                  path_label(p)),
          "Confirm the intended scope: a parent-condition subtotal includes undisplayed categories; a child-union subtotal does not.",
          ref = "SUBTOTAL_EXCLUDES_UNDISPLAYED_CATEGORIES"
        )
      }
    }

    ## Every analysis this output displays must link all grouping factors the
    ## paths reference, in the same level order.
    analyses <- model$analyses[
      !is.na(model$analyses$output_id) & model$analyses$output_id == out_id, ,
      drop = FALSE
    ]
    for (j in seq_len(nrow(analyses))) {
      an_gids <- .split_values(analyses$grouping_ids[j] %||% "")
      missing <- setdiff(used_gf_order, an_gids)
      if (length(missing) > 0) {
        findings <- .add_finding(
          findings, "FAIL", "analyses", analyses$id[j], "grouping_ids",
          sprintf("This analysis does not reference grouping factor(s) %s that the output's result paths require.",
                  paste(missing, collapse = ", ")),
          "Add the grouping(s) in 'Grouped by' -- without them this line cannot fill the hierarchical columns.",
          ref = "GROUPING_VARIABLE_NOT_LINKED"
        )
        next
      }
      shared <- an_gids[an_gids %in% used_gf_order]
      if (length(shared) > 1 &&
          !identical(shared, used_gf_order[used_gf_order %in% shared])) {
        findings <- .add_finding(
          findings, "WARN", "analyses", analyses$id[j], "grouping_ids",
          "This analysis orders its groupings differently from the output's header levels.",
          "Match the 'Grouped by' order to the header (outermost level first) so the columns come out in shell order.",
          ref = "GROUPING_ORDER_AMBIGUOUS"
        )
      }
    }
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
#'   \item{Result paths}{For an output with declared hierarchical result-group
#'     paths: the path count matches the shell's column count, every shell
#'     column has a path and every path a shell column (no invented
#'     Cartesian combinations), subtotals have a defined scope, no two paths
#'     compose the same condition, and every displayed analysis links all
#'     required grouping factors in header order.}
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
  findings <- .check_grouping_shapes(findings, model)
  findings <- .check_flat_axes(findings, model)
  findings <- .check_methods(findings, model)
  findings <- .check_method_placeholder_slots(findings, model)
  findings <- .check_unparsed_populations(findings, model)
  findings <- .check_separator_safety(findings, model)
  findings <- .check_result_paths(findings, model)
  findings <- .check_nested_layout(findings, model)

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

## One representation for every place that decides whether an ARS event may
## become an executable deliverable. WARN and INFO remain visible, but only a
## FAIL closes the gate.
#' @noRd
.validation_gate <- function(findings) {
  blocking <- findings[findings$severity %in% "FAIL", , drop = FALSE]
  refs <- blocking$ref[
    !is.na(blocking$ref) & nzchar(blocking$ref)
  ]
  refs <- unique(refs)

  actions <- blocking$action[
    !is.na(blocking$action) & nzchar(blocking$action)
  ]
  actions <- unique(actions)

  blocked <- nrow(blocking) > 0L
  summary <- if (blocked) {
    action_text <- if (length(actions) > 0L) {
      paste(actions, collapse = " ")
    } else {
      "Review the blocking findings and repair the reporting event."
    }
    paste0(
      nrow(blocking), " blocking finding",
      if (nrow(blocking) == 1L) "" else "s",
      ". To fix: ", action_text
    )
  } else {
    "No blocking findings. WARN and INFO findings remain available for review."
  }

  list(
    blocked = blocked,
    status = if (blocked) "needs-fixes" else "ready",
    findings = findings,
    blocking_findings = blocking,
    blocking_refs = refs,
    summary = summary
  )
}

#' @noRd
.model_validation_gate <- function(model, spec = NULL, report = NULL) {
  .validation_gate(validate_ars_model(model, spec = spec, report = report))
}

## Refuse to turn a structurally invalid event into a runnable artifact. This is
## deliberately called at each direct execution boundary, not only by the
## higher-level workflow, because these helpers are also callable on their own.
#' @noRd
.assert_runnable_ars <- function(ars) {
  model <- if (inherits(ars, "ars_model")) ars else ars_to_model(ars)
  gate <- .model_validation_gate(model)
  if (!gate$blocked) return(invisible(gate))

  refs <- if (length(gate$blocking_refs) > 0L) {
    paste(gate$blocking_refs, collapse = ", ")
  } else {
    "unreferenced structural finding"
  }
  cli::cli_abort(c(
    "This reporting event cannot be executed because structural validation failed.",
    "x" = gate$summary,
    "i" = paste("Blocking references:", refs),
    "i" = "Repair the reporting event and validate it again before execution."
  ))
}

## Translate model findings into the workflow diagnostics contract without
## throwing away entity, id and field context.
#' @noRd
.validation_gate_diagnostics <- function(gate) {
  findings <- gate$findings
  if (is.null(findings) || nrow(findings) == 0L) {
    return(.EMPTY_DIAGNOSTICS())
  }

  locations <- vapply(seq_len(nrow(findings)), function(i) {
    parts <- c(findings$entity[i], findings$id[i], findings$field[i])
    parts <- parts[!is.na(parts) & nzchar(parts)]
    if (length(parts) == 0L) NA_character_ else paste(parts, collapse = " / ")
  }, character(1))

  data.frame(
    stage = "validate_ars",
    severity = findings$severity,
    input = INPUT_ARS,
    tlf_number = NA_character_,
    location = locations,
    problem = findings$problem,
    action = findings$action,
    stringsAsFactors = FALSE
  )
}
