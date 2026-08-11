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
      value = pool,
      ## Groupings are the one pool a reviewer routinely has to CREATE (a
      ## missing column axis), so that pool gets lifecycle actions.
      if (identical(pool, "groupings")) {
        shiny::uiOutput(ns("grouping_actions"))
      },
      DT::DTOutput(ns(paste0("table_", pool))),
      shiny::uiOutput(ns(paste0("detail_", pool)))
    )
  })

  do.call(bslib::navset_pill, c(panels, list(id = ns("pools"))))
}

#' @noRd
mod_entity_library_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    pools <- c("methods", "analysis_sets", "data_subsets", "groupings")

    ## One table proxy per pool, so an entity_request can select a row from
    ## outside the render loop.
    proxies <- list()

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
        ## Initialized even while the Entities tab is hidden, so an
        ## "Edit in Entities" jump can select a row before the first visit.
        shiny::outputOptions(output, table_id, suspendWhenHidden = FALSE)

        proxy <- DT::dataTableProxy(table_id)
        proxies[[this_pool]] <<- proxy
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

          ## The panel belongs to the selected row, not to the model, so an
          ## edit does not re-render the field being typed into. A click in
          ## the table always wins; failing that, an "Edit in Entities" jump
          ## opens its entity directly (the table row cannot be pre-selected
          ## from the server while the tab has never been shown).
          model <- shiny::isolate(state$model())
          selected <- input[[paste0(table_id, "_rows_selected")]]
          if (length(selected) == 0) {
            request <- state$entity_request()
            if (!is.null(request) && identical(request$pool, this_pool)) {
              selected <- match(request$id, model[[this_pool]]$id)
              if (is.na(selected)) selected <- integer(0)
            }
          }
          if (length(selected) == 0) {
            return(shiny::div(
              class = "text-muted small mt-2",
              "Select a row to see the full definition."
            ))
          }

          row <- model[[this_pool]][selected, , drop = FALSE]

          if (identical(state$mode, "edit")) {
            .entity_edit_ui(row, this_pool, model, state, session$ns)
          } else {
            .entity_detail_ui(row, this_pool, model, state)
          }
        })
      })
    }

    output$grouping_actions <- shiny::renderUI({
      if (!identical(state$mode, "edit")) return(NULL)
      shiny::div(
        class = "d-flex gap-2 mb-2",
        shiny::actionButton(session$ns("grouping_add"), "Add grouping",
                            class = "btn-sm btn-outline-primary"),
        shiny::actionButton(session$ns("grouping_clone"), "Clone selected",
                            class = "btn-sm btn-outline-secondary"),
        shiny::actionButton(session$ns("grouping_delete"), "Delete selected",
                            class = "btn-sm btn-outline-danger")
      )
    })

    ## A jump from another panel ("Edit in Entities"): open the right pool
    ## and select the entity's row, which opens its detail/edit panel.
    shiny::observeEvent(state$entity_request(), {
      request <- state$entity_request()
      if (is.null(request) || !request$pool %in% pools) return()

      bslib::nav_select("pools", request$pool, session = session)
      index <- match(request$id, state$model()[[request$pool]]$id)
      if (!is.na(index)) {
        DT::selectRows(proxies[[request$pool]], index)
      }
    })

    if (identical(state$mode, "edit")) {
      .observe_entity_inputs(input, state)
      .observe_grouping_actions(input, state, session)
    }
  })
}

## The grouping lifecycle: add, clone, delete -- each recorded as a
## structural edit, each guarded by what it would touch. Delete is refused
## while any analysis still points at the grouping; the dependent lines are
## named so the unassign-first order of operations is obvious.
#' @noRd
.observe_grouping_actions <- function(input, state, session) {
  selected_grouping <- function() {
    idx <- input$table_groupings_rows_selected
    if (length(idx) == 0) return(NULL)
    model <- shiny::isolate(state$model())
    if (idx > nrow(model$groupings)) return(NULL)
    model$groupings$id[idx]
  }

  shiny::observeEvent(input$grouping_add, {
    spec <- state$spec
    dataset_input <- if (is.null(spec)) {
      shiny::textInput(session$ns("new_grouping_dataset"), "Dataset",
                       value = "ADSL")
    } else {
      shiny::selectizeInput(session$ns("new_grouping_dataset"), "Dataset",
                            choices = unique(spec$variables$dataset),
                            selected = "ADSL")
    }
    variable_input <- if (is.null(spec)) {
      shiny::textInput(session$ns("new_grouping_variable"), "Variable")
    } else {
      shiny::selectizeInput(session$ns("new_grouping_variable"), "Variable",
                            choices = .variable_choices(spec, NULL),
                            options = list(create = TRUE))
    }
    shiny::showModal(shiny::modalDialog(
      title = "Add a grouping",
      bslib::layout_columns(
        col_widths = c(6, 6),
        dataset_input,
        variable_input
      ),
      shiny::textInput(session$ns("new_grouping_label"), "Label (optional)"),
      shiny::p(class = "text-muted small",
               "The grouping starts data-driven (columns = the variable's ",
               "observed values). Define condition-based column levels ",
               "afterwards in this panel's raw-JSON editor, or regenerate ",
               "from an annotated shell."),
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton(session$ns("confirm_grouping_add"), "Add",
                            class = "btn-primary")
      )
    ))
  })

  shiny::observeEvent(input$confirm_grouping_add, {
    variable <- trimws(input$new_grouping_variable %||% "")
    if (!nzchar(variable)) {
      shiny::showNotification("The grouping needs a variable.",
                              type = "warning")
      return()
    }
    shiny::removeModal()
    updated <- tryCatch(
      model_add_grouping(state$model(),
                         dataset  = input$new_grouping_dataset %||% "ADSL",
                         variable = variable,
                         label    = .input_to_value(input$new_grouping_label)),
      error = function(e) e
    )
    if (inherits(updated, "error")) {
      shiny::showNotification(conditionMessage(updated), type = "error")
      return()
    }
    new_id <- attr(updated, "last_added")
    .record_structural_edit(state, updated, "groupings", new_id,
                            "grouping", "(none)", "(added)")
    shiny::showNotification(paste("Added", new_id), type = "message",
                            duration = 5)
  })

  shiny::observeEvent(input$grouping_clone, {
    id <- selected_grouping()
    if (is.null(id)) {
      shiny::showNotification("Select a grouping to clone.", type = "warning")
      return()
    }
    updated <- model_clone_grouping(state$model(), id)
    new_id <- attr(updated, "last_added")
    .record_structural_edit(state, updated, "groupings", new_id,
                            "grouping", "(none)", paste("(cloned from", id, ")"))
    shiny::showNotification(paste("Cloned", id, "as", new_id),
                            type = "message", duration = 5)
  })

  shiny::observeEvent(input$grouping_delete, {
    id <- selected_grouping()
    if (is.null(id)) {
      shiny::showNotification("Select a grouping to delete.", type = "warning")
      return()
    }
    dependents <- .grouping_dependents(shiny::isolate(state$model()), id)
    if (length(dependents) > 0) {
      shiny::showModal(shiny::modalDialog(
        title = "This grouping is in use",
        shiny::p(shiny::strong(id), " is referenced by ",
                 shiny::strong(length(dependents)), " analysis line(s):"),
        shiny::tags$ul(class = "small",
                       lapply(dependents, function(x) shiny::tags$li(x))),
        shiny::p("Remove it from those lines' ", shiny::em("Grouped by"),
                 " first, then delete it here."),
        footer = shiny::modalButton("Close")
      ))
      return()
    }
    shiny::showModal(shiny::modalDialog(
      title = "Delete this grouping?",
      shiny::p(shiny::strong(id),
               " is not referenced by any analysis and will be removed from ",
               "the reporting event."),
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton(session$ns("confirm_grouping_delete"), "Delete",
                            class = "btn-danger")
      )
    ))
  })

  shiny::observeEvent(input$confirm_grouping_delete, {
    shiny::removeModal()
    id <- selected_grouping()
    if (is.null(id)) return()
    updated <- tryCatch(
      model_remove_grouping(state$model(), id),
      error = function(e) e
    )
    if (inherits(updated, "error")) {
      shiny::showNotification(conditionMessage(updated), type = "error",
                              duration = 8)
      return()
    }
    .record_structural_edit(state, updated, "groupings", id,
                            "grouping", "(present)", "(deleted)")
    shiny::showNotification(paste("Deleted", id), type = "message",
                            duration = 5)
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

## Editable fields are registered by entity id rather than by the selected row.
## The registry follows model membership so entities created during the session
## receive observers, while removed entities release theirs.
#' @noRd
.entity_input_fields <- function() {
  list(
    methods       = c("name", "label", "description"),
    analysis_sets = c("name", "label", "condition_dataset",
                      "condition_variable", "condition_comparator",
                      "condition_value"),
    data_subsets  = c("name", "label", "condition_dataset",
                      "condition_variable", "condition_comparator",
                      "condition_value"),
    groupings     = c("name", "label", "groupingDataset", "groupingVariable")
  )
}

#' @noRd
.entity_observer_specs <- function(model) {
  fields <- .entity_input_fields()
  specs <- list()

  for (pool in names(fields)) {
    for (entity_id in model[[pool]]$id) {
      for (field in fields[[pool]]) {
        input_id <- .entity_input_id(pool, entity_id, field)
        specs[[input_id]] <- list(
          pool = pool,
          entity_id = entity_id,
          field = field,
          input_id = input_id
        )
      }
    }
  }

  specs
}

#' @noRd
.reconcile_entity_observers <- function(registry, desired, create_observer) {
  current_keys <- ls(registry, all.names = TRUE)
  desired_keys <- names(desired)

  removed_keys <- setdiff(current_keys, desired_keys)
  for (key in removed_keys) {
    observer <- get(key, envir = registry, inherits = FALSE)
    observer$destroy()
    rm(list = key, envir = registry)
  }

  added_keys <- setdiff(desired_keys, current_keys)
  for (key in added_keys) {
    observer <- create_observer(desired[[key]])
    assign(key, observer, envir = registry)
  }

  invisible(registry)
}

#' @noRd
.observe_entity_field <- function(input, state, spec) {
  pool <- spec$pool
  entity_id <- spec$entity_id
  field <- spec$field
  input_id <- spec$input_id

  shiny::observeEvent(input[[input_id]], {
    apply_edit(
      state,
      pool,
      entity_id,
      field,
      .input_to_value(input[[input_id]])
    )
  }, ignoreInit = TRUE)
}

#' @noRd
.observe_entity_inputs <- function(input, state) {
  registry <- new.env(parent = emptyenv())

  create_observer <- function(spec) {
    .observe_entity_field(input, state, spec)
  }

  shiny::observe({
    desired <- .entity_observer_specs(state$model())
    .reconcile_entity_observers(registry, desired, create_observer)
  })

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
    shiny::div(class = "mt-2", .entity_detail_rows(row, pool, model)),
    shiny::tags$details(
      class = "mt-2",
      shiny::tags$summary(class = "small text-muted", "Raw JSON"),
      .json_block(row$raw[[1]])
    )
  )
}

#' @noRd
.entity_detail_rows <- function(row, pool, model = NULL) {
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
    dependents <- if (is.null(model)) character(0) else
      .grouping_dependents(model, row$id)
    return(shiny::tagList(
      .detail_row("Id", row$id),
      .detail_row("Variable",
                  paste0(row$groupingDataset, ".", row$groupingVariable)),
      .detail_row("Data driven", row$dataDriven),
      .detail_row("Groups", row$group_labels),
      if (length(dependents) > 0) {
        .detail_row("Used by", paste(dependents, collapse = ", "))
      }
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
