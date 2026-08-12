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
      .observe_entity_inputs(input, state, session)
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
    ## The raw node goes with the findings so a finding about a child group
    ## can name the child the way the cards below do.
    if (nrow(own) > 0) .findings_list(own, row$raw[[1]]),

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
## `child` addresses one group inside a grouping. It defaults to NULL, so
## every flat field keeps the id it has always had.
#' @noRd
## `clause` addresses one where clause inside a child's compound expression.
## It is a position rather than an id -- clauses have none in the ARS -- which
## is safe because the panel re-renders after every structural action, so the
## ids never outlive the ordering they were minted from.
.entity_input_id <- function(pool, id, field, child = NULL, clause = NULL) {
  stem <- paste0(pool, "__", .slug(id))
  if (!is.null(child)) stem <- paste0(stem, "__", .slug(child))
  if (!is.null(clause)) stem <- paste0(stem, "__clause", as.integer(clause))
  paste0(stem, "__", field)
}

#' @noRd
.entity_json_js <- function(input_id, pool, entity_id) {
  paste0(
    "Shiny.setInputValue('", input_id, "', ",
    "{pool: '", pool, "', id: '", entity_id, "'}, ",
    "{priority: 'event'}); return false;"
  )
}

## Buttons that carry which grouping (and which child) they belong to, so one
## observer serves every row -- the same shape apply_json already uses. The
## child fields themselves are never observed: a child write is a NODE write,
## so it goes through .record_structural_edit(), which bumps state$refresh()
## and re-renders this panel. A per-keystroke observer would therefore destroy
## the input being typed into, and a four-field condition is meaningless until
## all four are in hand anyway.
## The same payload with an `action`, so up / down / clone / delete share one
## observer instead of minting four per child.
#' @noRd
.entity_action_js <- function(input_id, grouping_id, group_id, action) {
  paste0(
    "Shiny.setInputValue('", input_id, "', ",
    "{grouping: '", grouping_id, "', group: '", group_id, "', ",
    "action: '", action, "'}, ",
    "{priority: 'event'}); return false;"
  )
}

## The same payload again with a clause position, so add / up / down / clone /
## delete across every clause of every child share one observer.
#' @noRd
.entity_clause_js <- function(input_id, grouping_id, group_id, index, action) {
  paste0(
    "Shiny.setInputValue('", input_id, "', ",
    "{grouping: '", grouping_id, "', group: '", group_id, "', ",
    "clause: ", as.integer(index), ", action: '", action, "'}, ",
    "{priority: 'event'}); return false;"
  )
}

#' @noRd
.entity_group_js <- function(input_id, grouping_id, group_id = NULL) {
  payload <- paste0("{grouping: '", grouping_id, "'",
                    if (!is.null(group_id)) paste0(", group: '", group_id, "'"),
                    "}")
  paste0(
    "Shiny.setInputValue('", input_id, "', ", payload,
    ", {priority: 'event'}); return false;"
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
    groups <- row$raw[[1]][["groups"]] %||% list()
    return(shiny::tagList(
      bslib::layout_columns(
        col_widths = c(6, 6),
        field_input("groupingDataset", "Dataset", row$groupingDataset),
        field_input("groupingVariable", "Variable", row$groupingVariable)
      ),
      shiny::checkboxInput(
        ns(.entity_input_id(pool, row$id, "dataDriven")),
        "Data driven (levels discovered from the data at run time)",
        value = isTRUE(row$dataDriven)
      ),
      .grouping_shape_alert(row, ns),
      shiny::h6(class = "mt-3", "Groups"),
      .groups_editor(row, groups, ns)
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
    ## dataDriven is a genuine flat column, so the existing machinery carries
    ## it. The CHILD fields are not here on purpose -- see .groups_editor().
    groupings     = c("name", "label", "groupingDataset", "groupingVariable",
                      "dataDriven")
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
.observe_entity_inputs <- function(input, state, session) {
  registry <- new.env(parent = emptyenv())
  ## Which grouping the Add-a-group modal belongs to. The modal's own inputs
  ## cannot carry it, and the panel may re-render between opening and
  ## confirming.
  group_add_target <- shiny::reactiveVal(NULL)

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

  ## One child group, committed as a whole. The handler is isolated, so
  ## reading the child's inputs by name here creates no dependency on them --
  ## which is the point of not observing them.
  shiny::observeEvent(input$group_apply, {
    grouping_id <- input$group_apply$grouping
    group_id    <- input$group_apply$group
    field <- function(name) {
      input[[.entity_input_id("groupings", grouping_id, name,
                              child = group_id)]]
    }

    model  <- state$model()
    before <- .group_condition_summary(model, grouping_id, group_id)
    node   <- .editor_group_node(model, grouping_id, group_id)

    clause_field <- function(index, name) {
      input[[.entity_input_id("groupings", grouping_id, name,
                              child = group_id, clause = index)]]
    }

    updated <- tryCatch({
      out <- model_set_group_field(model, grouping_id, group_id, "label",
                                   .input_to_value(field("label")))

      if (!is.null(node[["compoundExpression"]])) {
        ## A compound child commits its operator and every editable clause
        ## together. The clause count is read from the node rather than from
        ## the inputs: the panel was rendered from that node, so it is what
        ## the boxes on screen correspond to.
        clauses <- node[["compoundExpression"]][["whereClauses"]] %||% list()
        out <- model_set_group_operator(
          out, grouping_id, group_id,
          field("clause_operator") %||% "OR"
        )
        for (index in seq_along(clauses)) {
          ## A nested clause has no boxes to read; it keeps what it had.
          if (!is.null(clauses[[index]][["compoundExpression"]])) next
          ## Only commit boxes that actually exist. An empty box reads as ""
          ## and is a real edit ("is missing"); a box that was never rendered
          ## reads as NULL, and writing that would blank a clause the panel
          ## never showed.
          if (is.null(clause_field(index, "clause_variable"))) next
          out <- model_set_clause_condition(
            out, grouping_id, group_id, index,
            dataset    = clause_field(index, "clause_dataset"),
            variable   = clause_field(index, "clause_variable"),
            comparator = clause_field(index, "clause_comparator"),
            values     = .split_values(
              .blank_na(clause_field(index, "clause_value")))
          )
        }
        out
      } else {
        model_set_group_condition(
          out, grouping_id, group_id,
          dataset    = field("condition_dataset"),
          variable   = field("condition_variable"),
          comparator = field("condition_comparator"),
          values     = .split_values(.blank_na(field("condition_value")))
        )
      }
    }, error = function(e) e)

    if (inherits(updated, "error")) {
      shiny::showNotification(paste("Not applied:", conditionMessage(updated)),
                              type = "error", duration = 10)
      return()
    }

    ## The child id goes in the FIELD, because .diff_summary() collapses on
    ## pool|id|field -- a constant label would fold every child into one line.
    .record_structural_edit(
      state, updated, "groupings", grouping_id,
      paste0("group ", group_id),
      before, .group_condition_summary(updated, grouping_id, group_id)
    )
    shiny::showNotification("Group saved.", type = "message", duration = 4)
  })

  ## Up / down / clone / delete, one observer for every child of every
  ## grouping -- the action rides in the payload.
  shiny::observeEvent(input$group_action, {
    grouping_id <- input$group_action$grouping
    group_id    <- input$group_action$group
    action      <- input$group_action$action
    model       <- state$model()

    result <- tryCatch(switch(
      action,
      up     = list(model_move_group(model, grouping_id, group_id, -1L),
                    "(moved up)"),
      down   = list(model_move_group(model, grouping_id, group_id, 1L),
                    "(moved down)"),
      clone  = list(.clone_child_group(model, grouping_id, group_id),
                    "(cloned)"),
      delete = list(model_remove_group(model, grouping_id, group_id),
                    "(deleted)"),
      NULL
    ), error = function(e) e)

    if (inherits(result, "error")) {
      ## A refusal names what is in the way -- a result path that would be
      ## left dangling -- rather than the button simply doing nothing.
      shiny::showNotification(conditionMessage(result), type = "error",
                              duration = 10)
      return()
    }
    if (is.null(result)) return()

    updated <- result[[1]]
    new_id  <- attr(updated, "last_added") %||% group_id
    .record_structural_edit(state, updated, "groupings", grouping_id,
                            paste0("group ", new_id), "(present)", result[[2]])
  })

  ## Add / up / down / clone / delete for one clause of one compound child.
  ## The clause is addressed by position, which is safe because every action
  ## here re-renders the panel, so a stale index is never left on screen.
  shiny::observeEvent(input$clause_action, {
    grouping_id <- input$clause_action$grouping
    group_id    <- input$clause_action$group
    index       <- as.integer(input$clause_action$clause)
    action      <- input$clause_action$action
    model       <- state$model()
    before      <- .group_condition_summary(model, grouping_id, group_id)

    node    <- .editor_group_node(model, grouping_id, group_id)
    clauses <- node[["compoundExpression"]][["whereClauses"]] %||% list()

    result <- tryCatch(switch(
      action,
      ## Adding is what turns a simple condition into a compound, so it is
      ## also reachable from a child that has no clauses yet.
      add    = list(model_add_clause(model, grouping_id, group_id,
                                     after = index), "(clause added)"),
      up     = list(model_move_clause(model, grouping_id, group_id,
                                      index, -1L), "(clause moved up)"),
      down   = list(model_move_clause(model, grouping_id, group_id,
                                      index, 1L), "(clause moved down)"),
      ## Cloning copies the clause as it stands, next to itself -- including
      ## a nested one, which has no other way to be duplicated. The index is
      ## checked here because reading past the end would be a subscript error
      ## rather than the sentence the other refusals give.
      clone  = if (index >= 1L && index <= length(clauses)) {
        list(model_add_clause(model, grouping_id, group_id,
                              clause = clauses[[index]], after = index),
             "(clause cloned)")
      } else {
        simpleError(sprintf("Group %s has no clause %s.", group_id, index))
      },
      delete = list(model_remove_clause(model, grouping_id, group_id, index),
                    "(clause deleted)"),
      NULL
    ), error = function(e) e)

    if (inherits(result, "error")) {
      shiny::showNotification(conditionMessage(result), type = "error",
                              duration = 10)
      return()
    }
    if (is.null(result)) return()

    .record_structural_edit(
      state, result[[1]], "groupings", grouping_id,
      paste0("group ", group_id), before,
      paste(.group_condition_summary(result[[1]], grouping_id, group_id),
            result[[2]])
    )
  })

  ## Add a child. The dataset and variable default to the grouping's own, so
  ## the ordinary case is a label and a value.
  shiny::observeEvent(input$group_add, {
    grouping_id <- input$group_add$grouping
    model <- state$model()
    row   <- model$groupings[match(grouping_id, model$groupings$id), ]

    shiny::showModal(shiny::modalDialog(
      title = "Add a group",
      shiny::textInput(session$ns("new_group_label"), "Label"),
      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        shiny::textInput(session$ns("new_group_dataset"), "Dataset",
                         value = .blank_na(row$groupingDataset)),
        shiny::textInput(session$ns("new_group_variable"), "Variable",
                         value = .blank_na(row$groupingVariable)),
        shiny::selectizeInput(
          session$ns("new_group_comparator"), "Comparator",
          choices = c("EQ", "NE", "IN", "NOTIN", "GT", "GE", "LT", "LE")
        ),
        shiny::textInput(session$ns("new_group_value"), "Value(s)")
      ),
      shiny::p(class = "text-muted small",
               "The label is what the column prints. Separate multiple ",
               "values with a semicolon; leave them empty to mean a missing ",
               "value."),
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton(session$ns("confirm_group_add"), "Add",
                            class = "btn-primary")
      )
    ))
    group_add_target(grouping_id)
  })

  shiny::observeEvent(input$confirm_group_add, {
    grouping_id <- group_add_target()
    if (is.null(grouping_id)) return()

    label <- trimws(input$new_group_label %||% "")
    if (!nzchar(label)) {
      shiny::showNotification("The group needs a label: it is what the column prints.",
                              type = "warning")
      return()
    }
    shiny::removeModal()

    values <- .split_values(.blank_na(input$new_group_value))
    updated <- tryCatch(
      model_add_group(
        state$model(), grouping_id, label,
        condition = .cond_multi(input$new_group_dataset %||% "",
                                input$new_group_variable %||% "",
                                input$new_group_comparator %||% "EQ",
                                values)
      ),
      error = function(e) e
    )
    if (inherits(updated, "error")) {
      shiny::showNotification(conditionMessage(updated), type = "error",
                              duration = 10)
      return()
    }

    new_id <- attr(updated, "last_added")
    .record_structural_edit(state, updated, "groupings", grouping_id,
                            paste0("group ", new_id), "(none)", "(added)")
  })

  ## The way out of a fixed grouping with no groups. Explicit and logged: the
  ## alternative is changing what the columns mean without being asked.
  shiny::observeEvent(input$group_make_data_driven, {
    grouping_id <- input$group_make_data_driven$grouping
    updated <- model_set_field(state$model(), "groupings", grouping_id,
                               "dataDriven", TRUE)
    .record_structural_edit(state, updated, "groupings", grouping_id,
                            "dataDriven", "FALSE", "TRUE")
  })
}

## Copy one child group, next to the one it was copied from.
##
## " (copy)" on the label is what gives it a distinct id -- ids are minted
## from labels -- so it is the mechanism, not decoration, and the label must
## be edited before a second copy of the same group can be made.
#' @noRd
.clone_child_group <- function(model, grouping_id, group_id) {
  index  <- .row_index(model$groupings, grouping_id, "groupings")
  groups <- model$groupings$raw[[index]][["groups"]] %||% list()
  hit <- which(vapply(groups, function(g) identical(.chr_field(g[["id"]]),
                                                    group_id), logical(1)))
  if (length(hit) == 0) {
    cli::cli_abort("Grouping {.val {grouping_id}} has no group {.val {group_id}}.")
  }

  source <- groups[[hit[[1]]]]
  label  <- paste0(.chr_field(source[["label"]] %||% source[["name"]]), " (copy)")
  clause <- source[c(intersect(names(source),
                               c("condition", "compoundExpression")))]
  model_add_group(model, grouping_id, label,
                  condition = if (length(clause)) clause else NULL,
                  after = hit[[1]])
}

## How a child group reads now, for the edit log: the filter it stands for,
## or its label when it has no condition at all.
#' @noRd
.group_condition_summary <- function(model, grouping_id, group_id) {
  group <- .editor_group_node(model, grouping_id, group_id)
  if (is.null(group)) return("(none)")

  text  <- .group_condition_text(group)
  label <- .chr_field(group[["label"]] %||% group[["name"]])
  if (is.na(text)) paste0(.blank_na(label), " (no condition)") else
    paste0(.blank_na(label), ": ", text)
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
    if (nrow(own) > 0) .findings_list(own, row$raw[[1]]),
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
      if (length(dependents) > 0) {
        .detail_row("Used by", paste(dependents, collapse = ", "))
      },
      shiny::h6(class = "mt-3", "Groups"),
      .groups_view(row$raw[[1]][["groups"]] %||% list(), row$dataDriven)
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

## Said where the reviewer is standing, not only in the findings panel across
## the app. Validation blocks the save either way; this is the same sentence
## next to the control that caused it, with the one-click way out beside it.
##
## The flip is offered, never applied automatically: it changes what the
## columns MEAN, from a declared set to whatever the data happens to contain.
## Guessing that is the class of thing this package refuses everywhere else.
#' @noRd
.grouping_shape_alert <- function(row, ns) {
  if (isTRUE(row$dataDriven) || (row$n_groups %||% 0L) > 0L) return(NULL)

  shiny::div(
    class = "alert alert-danger py-2 px-3 small mt-2",
    shiny::div("This fixed grouping declares no groups, so it defines no ",
               "result columns. It cannot be saved until that is resolved."),
    shiny::tags$button(
      type = "button", class = "btn btn-sm btn-outline-danger mt-2",
      onclick = .entity_group_js(ns("group_make_data_driven"), row$id),
      "Mark data-driven"
    )
  )
}

## The child groups, editable. One Save button per child: the four condition
## fields only mean something together, and committing per keystroke would
## walk through states like "IN with one value" -- each of them a history
## entry and an edit-log row.
#' @noRd
.groups_editor <- function(row, groups, ns) {
  if (length(groups) == 0) {
    return(.groups_view(groups, row$dataDriven))
  }

  field_id <- function(child, field) {
    ns(.entity_input_id("groupings", row$id, field, child = child))
  }

  cards <- lapply(groups, function(group) {
    group_id <- .chr_field(group[["id"]])
    label    <- .chr_field(group[["label"]] %||% group[["name"]])

    ## A compound child is edited clause by clause. Its own four fields are
    ## meaningless -- there is no single condition to put in them -- so the
    ## clause editor takes their place entirely.
    body <- if (!is.null(group[["compoundExpression"]])) {
      .clauses_editor(row, group, group_id, ns)
    } else {
      condition <- group[["condition"]] %||% list()
      shiny::tagList(
        bslib::layout_columns(
          col_widths = c(3, 3, 3, 3),
          shiny::textInput(field_id(group_id, "condition_dataset"), "Dataset",
                           value = .blank_na(.chr_field(condition[["dataset"]]))),
          shiny::textInput(field_id(group_id, "condition_variable"), "Variable",
                           value = .blank_na(.chr_field(condition[["variable"]]))),
          shiny::selectizeInput(
            field_id(group_id, "condition_comparator"), "Comparator",
            choices  = c("EQ", "NE", "IN", "NOTIN", "GT", "GE", "LT", "LE"),
            selected = .blank_na(.chr_field(condition[["comparator"]]))
          ),
          shiny::textInput(field_id(group_id, "condition_value"), "Value(s)",
                           value = .blank_na(.join_values(condition[["value"]])))
        ),
        shiny::div(
          class = "d-flex align-items-center gap-2",
          shiny::div(class = "text-muted small",
                     "Separate multiple values with a semicolon. Leave empty ",
                     "to mean a missing value."),
          ## The only way into a compound expression from the UI. The
          ## condition above becomes the first clause rather than being
          ## replaced, so nothing is lost by trying it.
          shiny::tags$button(
            type = "button", class = "btn btn-sm btn-outline-secondary",
            onclick = .entity_clause_js(ns("clause_action"), row$id, group_id,
                                        1L, "add"),
            "Add a clause (AND / OR)"
          )
        )
      )
    }

    action <- function(label, name, class = "btn-outline-secondary") {
      shiny::tags$button(
        type = "button", class = paste("btn btn-sm", class),
        onclick = .entity_action_js(ns("group_action"), row$id, group_id, name),
        label
      )
    }

    shiny::div(
      class = "border rounded p-2 mb-2",
      shiny::div(
        class = "d-flex align-items-center gap-2 mb-2",
        shiny::span(class = "badge text-bg-light", group_id),
        shiny::textInput(field_id(group_id, "label"), NULL,
                         value = .blank_na(label))
      ),
      body,
      shiny::div(
        class = "d-flex gap-2 mt-2",
        ## Both shapes save through the same button: the handler reads the
        ## label either way and then either the four condition fields or every
        ## clause, depending on which the child actually carries.
        shiny::tags$button(
          type = "button", class = "btn btn-sm btn-outline-primary",
          onclick = .entity_group_js(ns("group_apply"), row$id, group_id),
          "Save this group"
        ),
        action("Up", "up"),
        action("Down", "down"),
        action("Clone", "clone"),
        action("Delete", "delete", "btn-outline-danger")
      )
    )
  })

  shiny::div(
    cards,
    shiny::tags$button(
      type = "button", class = "btn btn-sm btn-outline-primary",
      onclick = .entity_group_js(ns("group_add"), row$id),
      "Add group"
    )
  )
}

## A grouping's child groups, or a sentence saying why there are none.
##
## The summary counted them and listed their labels, which left the panel
## saying "3 groups" while showing none of them. An empty collection means
## two different things, so it is worth a sentence either way: a data-driven
## grouping discovers its levels at run time, while a fixed one that declares
## none defines no result columns at all -- which validation blocks.
#' @noRd
.groups_view <- function(groups, data_driven = NA) {
  if (length(groups) > 0) {
    return(DT::datatable(
      .groups_table(groups),
      rownames = FALSE, selection = "none",
      options = list(dom = "t", paging = FALSE)
    ))
  }

  message <- if (isTRUE(data_driven)) {
    "No fixed groups: the levels come from the data at run time."
  } else {
    paste("No groups, and not data-driven: this grouping defines no result",
          "columns.")
  }
  shiny::div(class = "small text-muted", message)
}

## The clause editor for one compound child: the operator that joins the
## clauses, the clauses themselves, and a preview of what they add up to.
##
## Like the child fields around it, the clause inputs are rendered but never
## observed. A clause write is a node write, so it goes through
## .record_structural_edit() and re-renders this panel -- a per-keystroke
## observer would destroy the box being typed into. One Save commits every
## clause of the child at once, which is also the only honest unit: a
## half-saved expression is a different filter, not a partial one.
#' @noRd
.clauses_editor <- function(row, group, group_id, ns) {
  clauses  <- group[["compoundExpression"]][["whereClauses"]] %||% list()
  operator <- .chr_field(group[["compoundExpression"]][["logicalOperator"]])
  operator <- if (is.na(operator) || !nzchar(operator)) "OR" else
    toupper(operator)

  field_id <- function(index, field) {
    ns(.entity_input_id("groupings", row$id, field, child = group_id,
                        clause = index))
  }

  rows <- lapply(seq_along(clauses), function(index) {
    clause <- clauses[[index]]

    ## Nesting the editor cannot safely flatten stays visible and read-only,
    ## with its structural buttons still live -- it can be reordered or
    ## removed as a whole even though its insides are the hatch's business.
    body <- if (!is.null(clause[["compoundExpression"]])) {
      shiny::div(
        class = "small",
        shiny::tags$code(.clause_phrase(clause)),
        shiny::div(class = "text-muted mt-1",
                   "A nested expression: edit it through Raw JSON below.")
      )
    } else {
      condition <- clause[["condition"]] %||% list()
      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        shiny::textInput(field_id(index, "clause_dataset"), "Dataset",
                         value = .blank_na(.chr_field(condition[["dataset"]]))),
        shiny::textInput(field_id(index, "clause_variable"), "Variable",
                         value = .blank_na(.chr_field(condition[["variable"]]))),
        shiny::selectizeInput(
          field_id(index, "clause_comparator"), "Comparator",
          choices  = c("EQ", "NE", "IN", "NOTIN", "GT", "GE", "LT", "LE"),
          selected = .blank_na(.chr_field(condition[["comparator"]]))
        ),
        shiny::textInput(field_id(index, "clause_value"), "Value(s)",
                         value = .blank_na(.join_values(condition[["value"]])))
      )
    }

    clause_action <- function(label, name, class = "btn-outline-secondary") {
      shiny::tags$button(
        type = "button", class = paste("btn btn-sm", class),
        onclick = .entity_clause_js(ns("clause_action"), row$id, group_id,
                                    index, name),
        label
      )
    }

    shiny::div(
      class = "border-start ps-2 mb-2",
      shiny::div(class = "text-muted small",
                 paste("Clause", index, "of", length(clauses))),
      body,
      shiny::div(
        class = "d-flex gap-1 mt-1",
        clause_action("Up", "up"),
        clause_action("Down", "down"),
        clause_action("Clone", "clone"),
        clause_action("Delete", "delete", "btn-outline-danger")
      )
    )
  })

  shiny::tagList(
    shiny::div(
      class = "d-flex align-items-center gap-2 mb-2",
      shiny::div(
        style = "min-width: 8rem;",
        shiny::selectizeInput(
          ns(.entity_input_id("groupings", row$id, "clause_operator",
                              child = group_id)),
          "Join clauses with", choices = c("AND", "OR"), selected = operator
        )
      ),
      shiny::div(
        class = "small",
        shiny::div(class = "text-muted", "Reads as:"),
        shiny::tags$code(.clause_phrase(group))
      )
    ),
    rows,
    shiny::div(
      class = "d-flex gap-2",
      shiny::tags$button(
        type = "button", class = "btn btn-sm btn-outline-primary",
        onclick = .entity_clause_js(ns("clause_action"), row$id, group_id,
                                    length(clauses), "add"),
        "Add clause"
      )
    ),
    shiny::div(class = "text-muted small mt-1",
               "Separate multiple values with a semicolon. Leave a value ",
               "empty to mean a missing value. Deleting the second-to-last ",
               "clause turns this group back into a simple condition.")
  )
}

## One row per child group. `level` earns its column because a nested
## grouping puts a child under a parent, and reading the table without it
## would flatten the hierarchy into a list of peers.
#' @noRd
.groups_table <- function(groups) {
  empty <- data.frame(
    order = integer(0), level = integer(0), id = character(0),
    label = character(0), condition = character(0),
    stringsAsFactors = FALSE
  )
  if (length(groups) == 0) return(empty)

  rows <- lapply(groups, function(group) {
    data.frame(
      order     = .int_field(group[["order"]]),
      level     = .int_field(group[["level"]]),
      id        = .chr_field(group[["id"]]),
      label     = .chr_field(group[["label"]] %||% group[["name"]]),
      condition = .group_condition_text(group),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

## A condition as the reviewer reads it, rather than as R evaluates it.
##
## .group_condition_text() answers an execution predicate, which is the right
## thing in the child table and the edit log -- it is what will run. It is the
## wrong register inside the compound editor, though: it renders an empty EQ
## as `(is.na(X) | X == "")` and drops the dataset entirely, so two clauses on
## different datasets read identically while the reviewer is deciding whether
## the expression says what the shell meant. This spells the same clause the
## way the shell annotation did.
##
## Every field goes through .chr_field() (always length 1, NA when absent) and
## then .blank_na() (NA becomes ""), so nzchar() below never meets an NA.
#' @noRd
.condition_phrase <- function(condition) {
  dataset    <- .blank_na(.chr_field(condition[["dataset"]]))
  variable   <- .blank_na(.chr_field(condition[["variable"]]))
  comparator <- toupper(.blank_na(.chr_field(condition[["comparator"]])))
  values     <- as.character(unlist(condition[["value"]] %||% list()))
  values     <- values[!is.na(values)]

  subject <- if (nzchar(dataset) && nzchar(variable)) {
    paste0(dataset, ".", variable)
  } else if (nzchar(variable)) {
    variable
  } else {
    "(no variable)"
  }
  if (!nzchar(comparator)) comparator <- "EQ"

  ## An empty value list is how a where clause says "is missing". Printing
  ## "EQ ()" instead would read as a contradiction rather than a condition.
  if (length(values) == 0) {
    return(switch(
      comparator,
      EQ = , IN = paste(subject, "is missing"),
      NE = , NOTIN = paste(subject, "is not missing"),
      paste(subject, comparator)
    ))
  }
  if (length(values) == 1) return(paste(subject, comparator, values[[1]]))
  paste0(subject, " ", comparator, " (", paste(values, collapse = ", "), ")")
}

## One clause, or a whole expression, as a sentence. Recurses so nesting the
## editor cannot edit is still readable; the nested part is parenthesised so
## it is visibly one operand rather than more clauses of the outer operator.
#' @noRd
.clause_phrase <- function(clause, depth = 0L) {
  if (!is.null(clause[["compoundExpression"]])) {
    ce      <- clause[["compoundExpression"]]
    op      <- .chr_field(ce[["logicalOperator"]])
    op      <- if (is.na(op) || !nzchar(op)) "OR" else toupper(op)
    clauses <- ce[["whereClauses"]] %||% list()
    if (length(clauses) == 0) return("(no clauses)")

    parts <- vapply(clauses, .clause_phrase, character(1), depth = depth + 1L)
    text  <- paste(parts, collapse = paste0(" ", op, " "))
    return(if (depth > 0L) paste0("(", text, ")") else text)
  }
  if (!is.null(clause[["condition"]])) {
    return(.condition_phrase(clause[["condition"]]))
  }
  "(no condition)"
}

## A usable id is one real string. NA is not a lookup key: `match(NA, ids)`
## finds a grouping whose own id is missing, and identical(NA, NA) is TRUE, so
## an idless child would answer to a lost group_id. Both would return the
## wrong node rather than nothing.
#' @noRd
.usable_id <- function(id) {
  length(id) == 1L && !is.na(id) && nzchar(trimws(as.character(id)))
}

## One child group's node, or NULL when either id has gone. Both the edit-log
## summary and the clause editor need it, and looking it up twice by hand is
## how the two drift apart.
#' @noRd
.editor_group_node <- function(model, grouping_id, group_id) {
  if (!.usable_id(grouping_id) || !.usable_id(group_id)) return(NULL)

  index <- match(as.character(grouping_id), model$groupings$id)
  if (is.na(index)) return(NULL)
  groups <- model$groupings$raw[[index]][["groups"]] %||% list()
  hit <- which(vapply(groups, function(g) {
    id <- .chr_field(g[["id"]])
    !is.na(id) && identical(id, as.character(group_id))
  }, logical(1)))
  if (length(hit) == 0) return(NULL)
  groups[[hit[[1]]]]
}

## The filter a child group stands for, in the same words the analysis-set
## and data-subset panels use. A group carrying neither shape has no
## condition: handing an empty where-clause to where_to_filter_expr() would
## call it TRUE, which reads as "every row" rather than "unspecified".
#' @noRd
.group_condition_text <- function(group) {
  where <- if (!is.null(group[["compoundExpression"]])) {
    list(compoundExpression = group[["compoundExpression"]])
  } else if (!is.null(group[["condition"]])) {
    list(condition = group[["condition"]])
  } else {
    return(NA_character_)
  }

  tryCatch(where_to_filter_expr(where), error = function(e) NA_character_)
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
