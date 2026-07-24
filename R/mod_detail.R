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
      if (identical(state$mode, "edit")) {
        .analysis_edit_ui(row, model, state, ns)
      } else {
        .analysis_detail_ui(row, model, state)
      }
    })

    ## Selecting from inside this panel (an output's line list) uses the same
    ## delegated input the tree does.
    shiny::observeEvent(input$selected, {
      state$selected(list(pool = input$selected$pool, id = input$selected$id))
    })

    if (identical(state$mode, "edit")) {
      .observe_analysis_inputs(input, state, selected_analysis)
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

#' @noRd
.analysis_detail_ui <- function(row, model, state) {
  fields <- .analysis_summary_fields(row, model)
  findings <- state$findings()
  own <- findings[findings$id == row$id, , drop = FALSE]

  shiny::tagList(
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

  shiny::tagList(
    shiny::h5(if (is.na(row$label)) row$name else row$label),
    if (nrow(own) > 0) .findings_list(own),
    shiny::div(
      class = "mt-3",
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

#' @noRd
.findings_list <- function(findings) {
  shiny::div(
    class = "mt-2",
    lapply(seq_len(nrow(findings)), function(i) {
      shiny::div(
        class = paste0(
          "alert alert-", .severity_class(findings$severity[i]),
          " py-2 px-3 mb-2 small"
        ),
        shiny::strong(findings$problem[i]),
        shiny::br(),
        findings$action[i]
      )
    })
  )
}
