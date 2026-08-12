## arsbridge -- mod_detail.R
## ---------------------------------------------------------------------------
## The panel for whatever is selected in the tree: one analysis line, or a
## whole output.
##
## The point of this panel over raw JSON is that every id is resolved into
## what it means -- a methodId becomes the method's name plus whether the
## engine can actually execute it, an analysisSetId becomes the population's
## condition, a groupingId becomes the variable the results are split by.

## An analysis as label/value pairs, with ids resolved through the pools.
## Returned as a named character vector so it can be tested without a browser.
#' @noRd
.analysis_summary_fields <- function(row, model) {
  ## Resolve an id to "<name> (<id>)", falling back to the bare id so a
  ## dangling reference is visible rather than blank.
  resolve <- function(pool, id) {
    if (is.na(id) || !nzchar(id)) return("--")
    df <- model[[pool]]
    index <- match(id, df$id)
    if (is.na(index)) return(paste0(id, " (not in this reporting event)"))

    label <- df$label[index]
    if (is.na(label) || !nzchar(label)) label <- df$name[index]
    if (is.na(label) || !nzchar(label)) return(id)
    paste0(label, " (", id, ")")
  }

  population <- if (is.na(row$analysisSetId) || !nzchar(row$analysisSetId)) {
    "--"
  } else {
    index <- match(row$analysisSetId, model$analysis_sets$id)
    if (is.na(index)) {
      paste0(row$analysisSetId, " (not in this reporting event)")
    } else {
      condition <- model$analysis_sets$condition_summary[index]
      if (is.na(condition)) {
        resolve("analysis_sets", row$analysisSetId)
      } else {
        paste0(resolve("analysis_sets", row$analysisSetId), ": ", condition)
      }
    }
  }

  subset_text <- if (is.na(row$dataSubsetId) || !nzchar(row$dataSubsetId)) {
    "None (all records)"
  } else {
    index <- match(row$dataSubsetId, model$data_subsets$id)
    condition <- if (is.na(index)) {
      NA_character_
    } else {
      model$data_subsets$condition_summary[index]
    }
    if (is.na(condition)) {
      resolve("data_subsets", row$dataSubsetId)
    } else {
      paste0(resolve("data_subsets", row$dataSubsetId), ": ", condition)
    }
  }

  grouping_ids <- .split_values(row$grouping_ids)
  groupings <- if (length(grouping_ids) == 0) {
    "--"
  } else {
    paste(vapply(grouping_ids,
                 function(id) resolve("groupings", id), character(1)),
          collapse = ", ")
  }

  variable <- if (is.na(row$dataset) || is.na(row$variable)) {
    "--"
  } else {
    paste0(row$dataset, ".", row$variable)
  }

  fields <- c(
    "Analysis id"      = row$id,
    "Label"            = row$label,
    "Description"      = row$description,
    "Variable"         = variable,
    "Method"           = resolve("methods", row$methodId),
    "Executed as"      = .execution_note(row$methodId, row$strata),
    "Reason"           = row$reason,
    "Purpose"          = row$purpose,
    "Population"       = population,
    "Data subset"      = subset_text,
    "Grouped by"       = groupings,
    "Stratified by"    = row$strata,
    "Include total"    = row$includeTotal,
    "Shell annotation" = row$annotation,
    "SAP description"  = row$sapDescription,
    "Shown in output"  = row$output_id
  )

  fields <- vapply(fields, function(value) {
    if (length(value) == 0 || is.na(value) || !nzchar(as.character(value))) {
      "--"
    } else {
      as.character(value)
    }
  }, character(1))

  fields
}

## What the engine will actually do with this method, in a reviewer's words.
#' @noRd
.execution_note <- function(method_id, strata = NA_character_) {
  switch(
    .method_execution_class(method_id, strata),
    native      = "Computed by the engine",
    conditional = "Computed when its prerequisites are met",
    blocked     = "Needs a stratification variable before it can run",
    fallback    = "No executor -- the generic summarizer runs instead",
    unsupported = "Reserved for manual computation -- no result is computed",
    missing     = "No method assigned"
  )
}


#' @noRd
mod_detail_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("detail"))
}

#' @noRd
mod_detail_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Which analysis the inputs currently belong to. The inputs are rebuilt
    ## only when this changes -- never when the model changes -- so an edit
    ## does not tear down the field being typed into.
    selected_analysis <- shiny::reactive({
      selected <- state$selected()
      if (is.null(selected) || !identical(selected$pool, "analyses")) {
        return(NULL)
      }
      selected$id
    })

    output$detail <- shiny::renderUI({
      ## Redraw on a new selection, and when something outside the panel
      ## changed the model beneath it (undo, redo, a restored session).
      state$refresh()

      selected <- state$selected()
      if (is.null(selected)) {
        return(shiny::div(
          class = "text-muted",
          "Select an output or an analysis on the left."
        ))
      }

      ## Read the model without taking a reactive dependency on it: the panel
      ## belongs to the selection, and re-rendering on every keystroke would
      ## fight the reviewer for the cursor.
      model <- shiny::isolate(state$model())
      df <- model[[selected$pool]]
      index <- match(selected$id, df$id)

      if (is.na(index)) {
        return(shiny::div(
          class = "text-warning",
          paste0(selected$id, " is no longer in this reporting event.")
        ))
      }

      row <- df[index, , drop = FALSE]

      if (!identical(selected$pool, "analyses")) {
        return(.output_detail_ui(row, model, state, ns))
      }

      ## A shell-row click selects the owning analysis, but the reviewer is
      ## still looking at the table: keep the output view on screen, with the
      ## analysis form in its side panel rather than replacing the whole tab.
      shell_output <- .shell_redirect_output(state$shell_row(), selected$id,
                                             model)
      if (!is.null(shell_output)) {
        out_index <- match(shell_output, model$outputs$id)
        if (!is.na(out_index)) {
          return(.output_detail_ui(
            model$outputs[out_index, , drop = FALSE], model, state, ns
          ))
        }
      }

      if (identical(state$mode, "edit")) {
        .analysis_edit_ui(row, model, state, ns)
      } else {
        .analysis_detail_ui(row, model, state, ns)
      }
    })

    ## A click on a shell-table row. Rows owned by an analysis also move the
    ## real selection so the tree, findings and history stay coherent;
    ## authored text rows (section headers, stat lines with no analysis)
    ## leave the selection where it is.
    shiny::observeEvent(input$shell_row, {
      clicked <- list(output = input$shell_row$output,
                      order  = as.integer(input$shell_row$order))
      state$shell_header(NULL)
      state$shell_row(clicked)

      model <- state$model()
      index <- match(clicked$output, model$outputs$id)
      if (is.na(index)) return()
      data <- .shell_table_data(model$outputs$raw[[index]], model)
      row <- data$rows[data$rows$order == clicked$order, , drop = FALSE]
      if (nrow(row) == 0 || is.na(row$owner_analysis_id)) return()
      state$selected(list(pool = "analyses", id = row$owner_analysis_id))
    })

    ## A click on the shell table's column header: explain what defines the
    ## columns, without moving the selection anywhere.
    shiny::observeEvent(input$shell_header, {
      state$shell_header(list(output = input$shell_header$output))
    })

    ## "Edit in Entities": hand the entity to the library and let the app
    ## switch tabs.
    shiny::observeEvent(input$open_entity, {
      state$entity_request(list(pool = input$open_entity$pool,
                                id   = input$open_entity$id))
    })

    ## Selecting from inside this panel (an output's line list) uses the same
    ## delegated input the tree does.
    shiny::observeEvent(input$selected, {
      state$selected(list(pool = input$selected$pool, id = input$selected$id))
    })

    if (identical(state$mode, "edit")) {
      .observe_analysis_inputs(input, state, selected_analysis, session)
      .observe_structural_inputs(input, state, ns)
    }
  })
}

## Adding, reordering, removing and detaching -- the edits that change the
## shape of the event rather than the content of one field.
#' @noRd
.observe_structural_inputs <- function(input, state, ns) {
  shiny::observeEvent(input$add_to_output, {
    state$add_request(.add_request(output_id = input$add_to_output$id))
  })

  shiny::observeEvent(input$move, {
    model <- model_move_analysis(
      state$model(), input$move$output, input$move$id,
      as.integer(input$move$offset)
    )
    .record_structural_edit(
      state, model, "outputs", input$move$output, "analysis order",
      input$move$id,
      if (as.integer(input$move$offset) < 0) "moved up" else "moved down"
    )
  })

  shiny::observeEvent(input$remove_analysis, {
    analysis_id <- input$remove_analysis$id
    model <- state$model()
    index <- match(analysis_id, model$analyses$id)
    if (is.na(index)) return()
    label <- model$analyses$label[index]

    shiny::showModal(shiny::modalDialog(
      title = "Remove this analysis?",
      shiny::p("The line ", shiny::strong(.blank_na(label)),
               " will be removed from this reporting event, along with every ",
               "reference to it."),
      shiny::p(class = "text-muted small",
               "Nothing is written until you save."),
      footer = shiny::tagList(
        shiny::modalButton("Keep it"),
        shiny::tags$button(
          class = "btn btn-danger",
          onclick = .select_js(ns("confirm_remove"), "analyses", analysis_id),
          "Remove"
        )
      )
    ))
  })

  shiny::observeEvent(input$confirm_remove, {
    analysis_id <- input$confirm_remove$id
    model <- state$model()
    index <- match(analysis_id, model$analyses$id)
    if (is.na(index)) return()
    label <- model$analyses$label[index]

    updated <- model_remove_analysis(model, analysis_id)
    .record_structural_edit(state, updated, "analyses", analysis_id,
                            "removed", .blank_na(label), "(removed)")
    shiny::removeModal()
    state$selected(NULL)
  })

  ## Detaching gives this one analysis its own copy of a shared entity, so it
  ## can be changed without changing every other line that uses it.
  shiny::observeEvent(input$detach, {
    analysis_id <- input$detach$id
    pool <- input$detach$pool
    entity_id <- input$detach$entity

    model <- model_detach_entity(state$model(), pool, entity_id, analysis_id)
    variant_id <- attr(model, "last_added")

    .record_structural_edit(state, model, pool, variant_id, "detached",
                            entity_id, variant_id)
    state$selected(list(pool = "analyses", id = analysis_id))
  })
}

## The way back from an analysis panel to the table it belongs to. Without
## it, selecting a line from the tree or the bottom list strands the reviewer
## on the full-width analysis form.
#' @noRd
.analysis_back_link <- function(row, ns) {
  if (is.na(row$output_id)) return(NULL)
  shiny::tags$a(
    href = "#", class = "small text-decoration-none d-inline-block mb-1",
    onclick = .select_js(ns("selected"), "outputs", row$output_id),
    shiny::HTML("&larr; Back to the output table")
  )
}

#' @noRd
.analysis_detail_ui <- function(row, model, state, ns) {
  fields <- .analysis_summary_fields(row, model)
  findings <- state$findings()
  own <- findings[findings$id == row$id, , drop = FALSE]

  shiny::tagList(
    .analysis_back_link(row, ns),
    shiny::h5(if (is.na(row$label)) row$id else row$label),
    if (nrow(own) > 0) .findings_list(own),
    shiny::div(
      class = "mt-3",
      lapply(names(fields), function(label) {
        .detail_row(label, fields[[label]])
      })
    ),
    shiny::tags$details(
      class = "mt-3",
      shiny::tags$summary(class = "small text-muted", "Raw JSON"),
      .json_block(row$raw[[1]])
    )
  )
}

#' @noRd
.output_detail_ui <- function(row, model, state, ns) {
  analysis_ids <- .split_values(row$referenced_analysis_ids)
  analyses <- model$analyses[model$analyses$id %in% analysis_ids, , drop = FALSE]
  findings <- state$findings()
  own <- findings[findings$id == row$id, , drop = FALSE]

  node <- row$raw[[1]]
  display <- .display_node(node)
  columns <- vapply(
    display[["columns"]] %||% list(),
    function(column) .chr_field(column[["label"]]),
    character(1)
  )
  footnotes <- unlist(lapply(
    display[["displaySections"]] %||% list(),
    function(section) {
      vapply(.section_subsections(section),
             function(sub) .chr_field(sub[["text"]]), character(1))
    }
  ))

  ## The shell-faithful view: the authored table on the left, the clicked
  ## row's panel on the right. Built from the raw node's _meta.shell_layout,
  ## so it shows every authored row -- including the label and level lines no
  ## analysis owns.
  data <- .shell_table_data(node, model)
  active <- .shell_active_row(state$shell_row(), state$selected(), data,
                              row$id)
  header_selected <- identical(state$shell_header()$output, row$id)

  shiny::tagList(
    shiny::h5(if (is.na(row$label)) row$name else row$label),
    if (nrow(own) > 0) .findings_list(own),
    .shell_table_ui(data, ns, row$id, state$mode,
                    selected_order = if (!is.null(active)) active$order,
                    header_selected = header_selected),
    shiny::div(
      class = "mt-3",
      if (header_selected) {
        .shell_header_panel(row, model, ns)
      } else {
        .shell_side_panel(active, model, state, ns)
      }
    ),
    shiny::tags$details(
      class = "mt-3",
      shiny::tags$summary(class = "small text-muted", "Output properties"),
      shiny::div(
        class = "mt-2",
        .detail_row("Output id", row$id),
        .detail_row("Type", row$outputType),
        .detail_row("Display title", row$display_title),
        .detail_row("File", row$file_name),
        .detail_row("Source datasets", row$source_datasets),
        .detail_row("Analyses", row$n_analyses)
      ),
      if (length(columns) > 0) shiny::tagList(
        shiny::h6(class = "mt-3", "Columns"),
        shiny::tags$ul(class = "small", lapply(columns, shiny::tags$li))
      ),
      if (length(footnotes) > 0) shiny::tagList(
        shiny::h6(class = "mt-3", "Footnotes"),
        shiny::tags$ul(class = "small", lapply(footnotes, shiny::tags$li))
      )
    ),
    shiny::div(
      class = "d-flex justify-content-between align-items-center mt-3",
      shiny::h6(class = "mb-0", "Analyses in this output"),
      if (identical(state$mode, "edit")) {
        shiny::actionButton(
          ns("add_analysis"), "Add analysis",
          class = "btn-sm btn-primary",
          onclick = .select_js(ns("add_to_output"), "outputs", row$id)
        )
      }
    ),

    ## Display order is part of the specification, so the lines are listed in
    ## order with the controls to change it rather than sorted for browsing.
    if (identical(state$mode, "edit")) {
      .analysis_order_ui(analysis_ids, model, row$id, ns)
    } else {
      DT::datatable(
        analyses[, c("id", "label", "dataset", "variable", "methodId"),
                 drop = FALSE],
        rownames = FALSE,
        selection = "none",
        options = list(dom = "tp", pageLength = 10, scrollX = TRUE)
      )
    }
  )
}

## The panel beside the shell table: the clicked row's analysis form (edit
## mode), its read-only detail (view mode), or a note for authored text rows
## that have no analysis behind them. All writes still flow through the
## existing .analysis_edit_ui inputs and apply_edit() -- the panel adds no
## mutation path of its own.
#' @noRd
.shell_side_panel <- function(active, model, state, ns) {
  if (is.null(active)) {
    return(shiny::div(
      class = "text-muted small border rounded p-3",
      "Click a row in the table to review it here."
    ))
  }

  if (is.na(active$owner_analysis_id)) {
    label <- if (nzchar(active$label)) active$label else "(blank row)"
    return(shiny::div(
      class = "border rounded p-3",
      shiny::div(
        class = "d-flex justify-content-between align-items-center",
        shiny::span(class = "fw-semibold small", label),
        shiny::span(class = "badge text-bg-secondary", active$kind)
      ),
      shiny::div(
        class = "text-muted small mt-2",
        "Authored shell text with no analysis behind it. Editing these rows ",
        "arrives in a later phase."
      )
    ))
  }

  index <- match(active$owner_analysis_id, model$analyses$id)
  if (is.na(index)) {
    return(shiny::div(
      class = "text-warning small border rounded p-3",
      paste0(active$owner_analysis_id,
             " is no longer in this reporting event.")
    ))
  }
  analysis <- model$analyses[index, , drop = FALSE]

  shiny::div(
    class = "border rounded p-3",
    if (identical(state$mode, "edit")) {
      .analysis_edit_ui(analysis, model, state, ns)
    } else {
      .analysis_detail_ui(analysis, model, state, ns)
    }
  )
}

## What defines the columns of this output, shown when the reviewer clicks
## the table header. Read-only by design: the column axis is produced by
## grouping entities shared across analyses (and, for spanning shells, by
## declared result paths), so a header click has no single safe write target
## -- editing a shared grouping here would silently change every table that
## uses it. The panel says where each piece IS edited instead.
#' @noRd
.shell_header_panel <- function(row, model, ns) {
  analysis_ids <- .split_values(row$referenced_analysis_ids)
  index <- match(analysis_ids, model$analyses$id)
  grouping_ids <- unique(unlist(lapply(
    model$analyses$grouping_ids[index[!is.na(index)]], .split_values
  )))
  usage <- .entity_usage(model)$groupings

  grouping_rows <- lapply(grouping_ids, function(id) {
    i <- match(id, model$groupings$id)
    if (is.na(i)) {
      return(shiny::div(
        class = "text-warning small py-1",
        paste0(id, " is not in this reporting event.")
      ))
    }
    label <- model$groupings$label[i]
    if (is.na(label) || !nzchar(label)) label <- model$groupings$name[i]
    if (is.na(label) || !nzchar(label)) label <- id

    variable <- model$groupings$groupingVariable[i]
    dataset <- model$groupings$groupingDataset[i]
    variable_text <- if (is.na(variable)) {
      NULL
    } else if (is.na(dataset)) {
      variable
    } else {
      paste0(dataset, ".", variable)
    }

    levels <- .split_values(model$groupings$group_labels[i])
    count <- .usage_count(usage, id)

    shiny::div(
      class = "d-flex justify-content-between align-items-start py-1",
      shiny::div(
        shiny::strong(class = "small", label),
        shiny::span(class = "text-muted small",
                    paste0(" (", id, ") -- shared by ", count, " analyses")),
        if (!is.null(variable_text)) {
          shiny::div(class = "small",
                     "Variable: ", shiny::tags$code(variable_text))
        },
        if (length(levels) > 0) {
          shiny::div(class = "small",
                     "Groups: ", paste(levels, collapse = ", "))
        }
      ),
      shiny::tags$button(
        class = "btn btn-sm btn-outline-primary py-0",
        onclick = .select_js(ns("open_entity"), "groupings", id),
        "Edit in Entities"
      )
    )
  })

  shiny::div(
    class = "border rounded p-3",
    shiny::div(class = "fw-semibold small mb-1",
               "What defines these columns"),
    if (length(grouping_ids) == 0) {
      shiny::div(class = "text-muted small",
                 "No analysis in this output declares a column grouping.")
    } else {
      grouping_rows
    },
    shiny::div(
      class = "text-muted small mt-2",
      "The header text comes from the shell; the columns themselves are ",
      "produced by the grouping above. Rename or restructure its groups in ",
      "the Entities tab -- a shared grouping changes every table that uses ",
      "it. Column roles (detail / subtotal / total scope) are reviewed in ",
      "the Columns tab, and one line's groupings are edited on the line ",
      "itself."
    )
  )
}

## The output's lines in display order, each with move and remove controls.
#' @noRd
.analysis_order_ui <- function(analysis_ids, model, output_id, ns) {
  if (length(analysis_ids) == 0) {
    return(shiny::div(
      class = "text-muted small",
      "No analyses yet. Add the lines this display should show."
    ))
  }

  shiny::div(
    class = "list-group mt-2",
    lapply(seq_along(analysis_ids), function(i) {
      analysis_id <- analysis_ids[i]
      index <- match(analysis_id, model$analyses$id)
      label <- if (is.na(index)) {
        analysis_id
      } else {
        value <- model$analyses$label[index]
        if (is.na(value) || !nzchar(value)) analysis_id else value
      }

      shiny::div(
        class = "list-group-item d-flex justify-content-between align-items-center py-1",
        shiny::tags$a(
          href = "#", class = "link-body-emphasis text-decoration-none small",
          onclick = .select_js(ns("selected"), "analyses", analysis_id),
          label
        ),
        shiny::div(
          class = "btn-group btn-group-sm",
          shiny::tags$button(
            class = "btn btn-outline-secondary py-0",
            disabled = if (i == 1) "disabled",
            onclick = .move_js(ns("move"), output_id, analysis_id, -1),
            shiny::HTML("&uarr;")
          ),
          shiny::tags$button(
            class = "btn btn-outline-secondary py-0",
            disabled = if (i == length(analysis_ids)) "disabled",
            onclick = .move_js(ns("move"), output_id, analysis_id, 1),
            shiny::HTML("&darr;")
          )
        )
      )
    })
  )
}

#' @noRd
.move_js <- function(input_id, output_id, analysis_id, offset) {
  paste0(
    "Shiny.setInputValue('", input_id, "', ",
    "{output: '", output_id, "', id: '", analysis_id, "', offset: ", offset,
    "}, {priority: 'event'}); return false;"
  )
}

## How many rows the findings block may occupy before it hides itself. An
## entity's findings sit ABOVE its editor, so a long stack pushes the fields
## the reviewer came for off the bottom of the panel.
.FINDINGS_ROWS_SHOWN <- 3L

## Child-group id -> the label a reviewer knows it by. Empty for entities
## that have no children, which is most of them.
#' @noRd
.child_labels <- function(entity) {
  groups <- entity[["groups"]] %||% list()
  if (length(groups) == 0) return(stats::setNames(character(0), character(0)))

  ids <- vapply(groups, function(g) .chr_field(g[["id"]]), character(1))
  labels <- vapply(groups, function(g) {
    label <- .chr_field(g[["label"]] %||% g[["name"]])
    if (is.na(label) || !nzchar(label)) .chr_field(g[["id"]]) else label
  }, character(1))

  keep <- !is.na(ids) & nzchar(ids)
  stats::setNames(labels[keep], ids[keep])
}

## The part of an entity a finding is about, named the way the panel names
## it. A finding on a child group carries "group <id> condition"; the id is
## no use to a reader looking at a card headed "Low".
#' @noRd
.finding_scope <- function(field, child_labels) {
  if (is.na(field) || !nzchar(field)) return(NA_character_)

  parts <- regmatches(field, regexec("^group (.+) condition$", field))[[1]]
  if (length(parts) != 2L) return(field)

  ## Single-bracket lookup: `[[` on a name the vector does not carry is an
  ## error, and a finding about a child that has since been renamed or
  ## deleted must still render -- as its raw id.
  label <- unname(child_labels[parts[[2]]])
  if (length(label) != 1L || is.na(label) || !nzchar(label)) {
    parts[[2]]
  } else {
    label
  }
}

## Findings for one entity, collapsed to one row per distinct problem.
##
## The spec check reports per clause, which is right -- each is a separate
## place to fix -- but a grouping with three compound children then shows the
## same sentence six times over, and the panel rendered only the sentence. So
## six distinct findings read as one repeated six times, with nothing saying
## which child each belonged to. One row per problem, listing the parts it
## affects, says strictly more in less space.
#' @noRd
.findings_list <- function(findings, entity = NULL) {
  labels <- if (is.null(entity)) {
    stats::setNames(character(0), character(0))
  } else {
    .child_labels(entity)
  }

  fields <- if ("field" %in% names(findings)) {
    as.character(findings$field)
  } else {
    rep(NA_character_, nrow(findings))
  }
  scope <- vapply(fields, .finding_scope, character(1),
                  child_labels = labels, USE.NAMES = FALSE)

  key <- paste(findings$severity, findings$problem, findings$action, sep = "\r")
  rows <- lapply(unique(key), function(k) {
    hit <- which(key == k)
    affects <- unique(scope[hit])
    affects <- affects[!is.na(affects) & nzchar(affects)]

    shiny::div(
      class = paste0(
        "alert alert-", .severity_class(findings$severity[hit[1]]),
        " py-2 px-3 mb-2 small"
      ),
      shiny::strong(findings$problem[hit[1]]),
      if (length(affects) > 0) {
        shiny::div(class = "mt-1",
                   shiny::tags$em("Affects: "),
                   paste(affects, collapse = " \u00b7 "))
      },
      shiny::br(),
      findings$action[hit[1]]
    )
  })

  block <- shiny::div(class = "mt-2", rows)
  if (length(rows) <= .FINDINGS_ROWS_SHOWN) return(block)

  ## Too many to sit above the editor. Summarise, and keep every one of them
  ## one click away rather than dropping any.
  counts <- table(findings$severity)
  summary <- paste(
    vapply(names(counts), function(s) paste(counts[[s]], s), character(1)),
    collapse = ", "
  )
  shiny::tags$details(
    class = "mt-2",
    shiny::tags$summary(
      class = "small",
      shiny::strong(paste(nrow(findings), "to review on this entity")),
      shiny::span(class = "text-muted", paste0(" -- ", summary))
    ),
    block
  )
}
