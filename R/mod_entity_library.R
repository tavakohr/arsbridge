## arsbridge -- mod_entity_library.R
## ---------------------------------------------------------------------------
## The shared-entity pools: methods, analysis sets, data subsets and
## groupings.
##
## These exist because ARS entities are shared by reference -- one method can
## drive dozens of analyses. The tree alone would hide that, so each table
## carries a "used by" count: editing an entity here is editing every analysis
## that points at it, and the count is what makes that visible.

## The columns worth showing per pool. Everything else stays in the raw node.
#' @noRd
.library_columns <- function(pool) {
  switch(
    pool,
    methods       = c("id", "name", "n_operations", "operation_summary"),
    analysis_sets = c("id", "label", "condition_summary"),
    data_subsets  = c("id", "label", "condition_summary"),
    groupings     = c("id", "label", "groupingDataset", "groupingVariable",
                      "n_groups", "group_labels")
  )
}

#' @noRd
.library_title <- function(pool) {
  switch(
    pool,
    methods       = "Methods",
    analysis_sets = "Analysis sets",
    data_subsets  = "Data subsets",
    groupings     = "Groupings"
  )
}

## One pool as a display table, with the usage count joined on.
#' @noRd
.library_table <- function(model, pool) {
  df <- model[[pool]][, .library_columns(pool), drop = FALSE]
  usage <- .entity_usage(model)[[pool]]

  df$used_by <- vapply(
    model[[pool]]$id,
    function(id) .usage_count(usage, id),
    integer(1)
  )

  names(df)[names(df) == "used_by"] <- "Used by"
  df
}


#' @noRd
mod_entity_library_ui <- function(id) {
  ns <- shiny::NS(id)
  pools <- c("methods", "analysis_sets", "data_subsets", "groupings")

  panels <- lapply(pools, function(pool) {
    bslib::nav_panel(
      .library_title(pool),
      DT::DTOutput(ns(paste0("table_", pool))),
      shiny::uiOutput(ns(paste0("detail_", pool)))
    )
  })

  do.call(bslib::navset_pill, panels)
}

#' @noRd
mod_entity_library_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    pools <- c("methods", "analysis_sets", "data_subsets", "groupings")

    for (pool in pools) {
      local({
        this_pool <- pool
        table_id  <- paste0("table_", this_pool)
        detail_id <- paste0("detail_", this_pool)

        ## Rendered once; later model changes flow through the proxy below.
        ## Re-rendering the table on every edit would reset its row selection,
        ## which closes the very panel the reviewer is editing in.
        output[[table_id]] <- DT::renderDT(
          {
            DT::datatable(
              .library_table(shiny::isolate(state$model()), this_pool),
              rownames = FALSE,
              selection = "single",
              options = list(pageLength = 15, scrollX = TRUE)
            )
          },
          server = TRUE
        )

        proxy <- DT::dataTableProxy(table_id)
        shiny::observe({
          DT::replaceData(
            proxy,
            .library_table(state$model(), this_pool),
            rownames       = FALSE,
            resetPaging    = FALSE,
            clearSelection = "none"
          )
        })

        output[[detail_id]] <- shiny::renderUI({
          state$refresh()

          selected <- input[[paste0(table_id, "_rows_selected")]]
          if (length(selected) == 0) {
            return(shiny::div(
              class = "text-muted small mt-2",
              "Select a row to see the full definition."
            ))
          }

          ## The panel belongs to the selected row, not to the model, so an
          ## edit does not re-render the field being typed into.
          model <- shiny::isolate(state$model())
          row <- model[[this_pool]][selected, , drop = FALSE]

          if (identical(state$mode, "edit")) {
            .entity_edit_ui(row, this_pool, model, state, session$ns)
          } else {
            .entity_detail_ui(row, this_pool, model, state)
          }
        })
      })
    }

    if (identical(state$mode, "edit")) {
      .observe_entity_inputs(input, state, session$ns)
    }
  })
}

## Editing a shared entity here changes every analysis that uses it, which is
## exactly why the library exists as its own view: this is the place to make a
## change once instead of thirty times.
#' @noRd
.entity_edit_ui <- function(row, pool, model, state, ns) {
  usage <- .usage_count(.entity_usage(model)[[pool]], row$id)
  findings <- state$findings()
  own <- findings[findings$id == row$id, , drop = FALSE]

  shiny::tagList(
    shiny::hr(),
    shiny::div(
      class = "d-flex align-items-center gap-2 mb-2",
      shiny::h6(class = "mb-0", if (is.na(row$label)) row$id else row$label),
      shiny::span(class = "badge text-bg-light",
                  paste("used by", usage,
                        if (usage == 1) "analysis" else "analyses"))
    ),
    if (usage > 1) {
      shiny::div(
        class = "alert alert-warning py-2 px-3 small",
        paste0("Changes here apply to all ", usage,
               " analyses that use this. To change one line only, open that ",
               "line and detach it first.")
      )
    },
    if (nrow(own) > 0) .findings_list(own),

    ## Every pool shares a name and a label; the rest is per-pool.
    bslib::layout_columns(
      col_widths = c(6, 6),
      shiny::textInput(ns(.entity_input_id(pool, row$id, "name")), "Name",
                       value = .blank_na(row$name)),
      shiny::textInput(ns(.entity_input_id(pool, row$id, "label")), "Label",
                       value = .blank_na(row$label))
    ),
    .entity_edit_fields(row, pool, ns),

    ## Nested shapes the flat fields cannot express -- compound conditions,
    ## method operations, grouping levels -- are edited as JSON. It is an
    ## escape hatch rather than the main road, but without it those parts of
    ## the standard would be unreachable.
    shiny::tags$details(
      class = "mt-3",
      shiny::tags$summary(class = "small text-muted",
                          "Edit the raw JSON for this entity"),
      shiny::textAreaInput(
        ns(.entity_input_id(pool, row$id, "json")), NULL,
        value = as.character(jsonlite::toJSON(row$raw[[1]], auto_unbox = TRUE,
                                              pretty = TRUE, null = "null")),
        rows = 14, width = "100%"
      ),
      shiny::tags$button(
        class = "btn btn-sm btn-outline-primary",
        onclick = .entity_json_js(ns("apply_json"), pool, row$id),
        "Apply JSON"
      ),
      shiny::uiOutput(ns("json_problem"))
    )
  )
}

## Input ids carry the pool and entity, so switching rows cannot leave one
## row's value sitting in another row's box.
#' @noRd
.entity_input_id <- function(pool, id, field) {
  paste0(pool, "__", .slug(id), "__", field)
}

#' @noRd
.entity_json_js <- function(input_id, pool, entity_id) {
  paste0(
    "Shiny.setInputValue('", input_id, "', ",
    "{pool: '", pool, "', id: '", entity_id, "'}, ",
    "{priority: 'event'}); return false;"
  )
}

#' @noRd
.entity_edit_fields <- function(row, pool, ns) {
  field_input <- function(field, label, value) {
    shiny::textInput(ns(.entity_input_id(pool, row$id, field)), label,
                     value = .blank_na(value))
  }

  if (identical(pool, "methods")) {
    return(shiny::tagList(
      field_input("description", "Description", row$description),
      shiny::div(class = "text-muted small",
                 "Which statistics this method computes is decided by the ",
                 "engine from the method id, not from the operations below."),
      shiny::h6(class = "mt-3", "Operations"),
      DT::datatable(
        .operations_table(row$raw[[1]][["operations"]] %||% list()),
        rownames = FALSE, selection = "none",
        options = list(dom = "t", paging = FALSE)
      )
    ))
  }

  if (identical(pool, "groupings")) {
    return(bslib::layout_columns(
      col_widths = c(6, 6),
      field_input("groupingDataset", "Dataset", row$groupingDataset),
      field_input("groupingVariable", "Variable", row$groupingVariable)
    ))
  }

  ## Analysis sets and data subsets: the simple condition is editable as
  ## fields; a compound expression is shown and edited as JSON.
  if (isTRUE(row$is_compound)) {
    return(shiny::div(
      class = "small",
      shiny::div(class = "text-muted", "Condition (compound):"),
      shiny::tags$code(.blank_na(row$condition_summary))
    ))
  }

  shiny::tagList(
    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      field_input("condition_dataset", "Dataset", row$condition_dataset),
      field_input("condition_variable", "Variable", row$condition_variable),
      shiny::selectizeInput(
        ns(.entity_input_id(pool, row$id, "condition_comparator")),
        "Comparator",
        choices = c("EQ", "NE", "IN", "NOTIN", "GT", "GE", "LT", "LE"),
        selected = .blank_na(row$condition_comparator)
      ),
      field_input("condition_value", "Value(s)", row$condition_value)
    ),
    shiny::div(class = "text-muted small",
               "Separate multiple values with a semicolon.")
  )
}

## One observer per editable field of every entity. The ids are stable and
## derived from the entity, so this is set up once rather than rebuilt as the
## selection moves.
#' @noRd
.observe_entity_inputs <- function(input, state, ns) {
  fields <- list(
    methods       = c("name", "label", "description"),
    analysis_sets = c("name", "label", "condition_dataset",
                      "condition_variable", "condition_comparator",
                      "condition_value"),
    data_subsets  = c("name", "label", "condition_dataset",
                      "condition_variable", "condition_comparator",
                      "condition_value"),
    groupings     = c("name", "label", "groupingDataset", "groupingVariable")
  )

  model <- shiny::isolate(state$model())

  for (pool in names(fields)) {
    for (entity_id in model[[pool]]$id) {
      for (field in fields[[pool]]) {
        local({
          this_pool <- pool
          this_id <- entity_id
          this_field <- field
          input_id <- .entity_input_id(this_pool, this_id, this_field)

          shiny::observeEvent(input[[input_id]], {
            apply_edit(state, this_pool, this_id, this_field,
                       .input_to_value(input[[input_id]]))
          }, ignoreInit = TRUE)
        })
      }
    }
  }

  ## The raw-JSON escape hatch replaces a whole node, so a mistake here is
  ## reported rather than applied.
  shiny::observeEvent(input$apply_json, {
    pool <- input$apply_json$pool
    entity_id <- input$apply_json$id
    text <- input[[.entity_input_id(pool, entity_id, "json")]]

    updated <- tryCatch(
      model_set_node_json(state$model(), pool, entity_id, text),
      error = function(e) e
    )

    if (inherits(updated, "error")) {
      reason <- conditionMessage(updated)
      shiny::showNotification(
        paste("Not applied:", reason), type = "error", duration = 10
      )
      return()
    }

    .record_structural_edit(state, updated, pool, entity_id, "raw JSON",
                            "(edited as JSON)", "(replaced)")
    shiny::showNotification("Applied.", type = "message", duration = 4)
  })
}

#' @noRd
.entity_detail_ui <- function(row, pool, model, state) {
  usage <- .usage_count(.entity_usage(model)[[pool]], row$id)

  findings <- state$findings()
  own <- findings[findings$id == row$id, , drop = FALSE]

  shiny::tagList(
    shiny::hr(),
    shiny::div(
      class = "d-flex align-items-center gap-2",
      shiny::h6(class = "mb-0", if (is.na(row$label)) row$id else row$label),
      shiny::span(
        class = "badge text-bg-light",
        paste("used by", usage, if (usage == 1) "analysis" else "analyses")
      )
    ),
    if (nrow(own) > 0) .findings_list(own),
    shiny::div(class = "mt-2", .entity_detail_rows(row, pool)),
    shiny::tags$details(
      class = "mt-2",
      shiny::tags$summary(class = "small text-muted", "Raw JSON"),
      .json_block(row$raw[[1]])
    )
  )
}

#' @noRd
.entity_detail_rows <- function(row, pool) {
  if (identical(pool, "methods")) {
    operations <- row$raw[[1]][["operations"]] %||% list()
    return(shiny::tagList(
      .detail_row("Id", row$id),
      .detail_row("Name", row$name),
      .detail_row("Description", row$description),
      .detail_row("Executed as", .execution_note(row$id)),
      shiny::h6(class = "mt-3", "Operations"),
      DT::datatable(
        .operations_table(operations),
        rownames = FALSE, selection = "none",
        options = list(dom = "t", paging = FALSE)
      )
    ))
  }

  if (identical(pool, "groupings")) {
    return(shiny::tagList(
      .detail_row("Id", row$id),
      .detail_row("Variable",
                  paste0(row$groupingDataset, ".", row$groupingVariable)),
      .detail_row("Data driven", row$dataDriven),
      .detail_row("Groups", row$group_labels)
    ))
  }

  shiny::tagList(
    .detail_row("Id", row$id),
    .detail_row("Name", row$name),
    .detail_row("Condition", row$condition_summary),
    if (!is.null(row$annotationText)) {
      .detail_row("Unparsed population", row$annotationText)
    },
    .detail_row("Compound expression", row$is_compound)
  )
}

## A method's operations as a table. Kept as its own helper because phase 2
## makes these cells editable.
#' @noRd
.operations_table <- function(operations) {
  if (length(operations) == 0) {
    return(data.frame(
      id = character(0), name = character(0),
      label = character(0), resultPattern = character(0),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, lapply(operations, function(op) {
    data.frame(
      id            = .chr_field(op[["id"]]),
      name          = .chr_field(op[["name"]]),
      label         = .chr_field(op[["label"]]),
      resultPattern = .chr_field(op[["resultPattern"]]),
      stringsAsFactors = FALSE
    )
  }))
}
