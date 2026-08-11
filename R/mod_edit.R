## arsbridge -- mod_edit.R
## ---------------------------------------------------------------------------
## The editing half: every change to the model goes through apply_edit(), the
## editable analysis panel, and the save flow.
##
## Two rules make the editing behave under Shiny's reactivity:
##
##   * apply_edit() ignores no-op writes. Inputs echo their value back when
##     they are repopulated, and without this every selection change would log
##     a phantom edit.
##   * inputs are repopulated only when the SELECTION changes, never when the
##     model changes, so typing into a field is not interrupted by the
##     re-render its own edit triggered.

## Record one edit and re-validate. Returns nothing; it mutates the state.
#' @noRd
apply_edit <- function(state, pool, id, field, value) {
  model <- state$model()
  df <- model[[pool]]
  index <- match(id, df$id)
  if (is.na(index)) return(invisible(FALSE))

  old <- df[[field]][index]

  ## An input echoing back its current value is not an edit. This has to come
  ## before the history push, or undo would fill up with no-op steps.
  if (identical(as.character(old), as.character(value))) {
    return(invisible(FALSE))
  }
  if (is.na(old) && (is.null(value) || is.na(value))) {
    return(invisible(FALSE))
  }

  .push_history(state)

  updated <- model_set_field(model, pool, id, field, value)
  state$model(updated)

  state$edit_log(rbind(state$edit_log(), data.frame(
    time  = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    pool  = pool,
    id    = id,
    field = field,
    old   = if (is.na(old)) "(not set)" else as.character(old),
    new   = if (is.null(value) || is.na(value)) "(not set)"
            else as.character(value),
    stringsAsFactors = FALSE
  )))

  state$findings(validate_ars_model(updated, state$spec, state$report))
  .write_autosave(state)
  invisible(TRUE)
}

## Choices for a shared-entity dropdown, labelled with how many analyses each
## entity is used by -- so it is clear that changing a shared method here
## changes it everywhere.
#' @noRd
.entity_choices <- function(model, pool, include_none = FALSE) {
  df <- model[[pool]]
  usage <- .entity_usage(model)[[pool]]

  labels <- vapply(seq_len(nrow(df)), function(i) {
    label <- df$label[i]
    if (is.na(label) || !nzchar(label)) label <- df$name[i]
    if (is.na(label) || !nzchar(label)) label <- df$id[i]

    count <- .usage_count(usage, df$id[i])
    if (count > 1) {
      paste0(label, " -- shared by ", count, " analyses")
    } else {
      label
    }
  }, character(1))

  choices <- stats::setNames(as.list(df$id), labels)
  if (include_none) choices <- c(list("None (all records)" = ""), choices)
  choices
}

## Method choices, split into what is already in the file and what can be
## added from the standard catalogue. Each is labelled with what the engine
## will do with it, which is the thing that decides whether a result appears.
#' @noRd
.method_choices <- function(model) {
  in_file <- model$methods$id
  usage <- .entity_usage(model)$methods

  describe <- function(id, shared) {
    name <- model$methods$name[match(id, model$methods$id)]
    if (is.na(name) || !nzchar(name)) name <- id

    note <- switch(
      .method_execution_class(id),
      native      = "computed",
      conditional = "needs a prerequisite",
      fallback    = "no executor -- generic summary",
      unsupported = "reserved for manual computation",
      "no method"
    )
    label <- paste0(name, " (", note, ")")
    if (shared > 1) paste0(label, " -- shared by ", shared, " analyses")
    else label
  }

  existing <- stats::setNames(
    as.list(in_file),
    vapply(in_file, function(id) describe(id, .usage_count(usage, id)),
           character(1))
  )

  catalogue_ids <- setdiff(.standard_method_ids(), in_file)
  catalogue <- stats::setNames(
    as.list(catalogue_ids),
    vapply(catalogue_ids, function(id) {
      name <- NULL
      for (entry in .STANDARD_METHODS) {
        if (identical(.chr_field(entry[["id"]]), id)) {
          name <- .chr_field(entry[["name"]])
          break
        }
      }
      paste0(name %||% id, " (add to this file)")
    }, character(1))
  )

  list(`In this reporting event` = existing, `Standard methods` = catalogue)
}

## Variable choices from the ADaM spec, grouped by dataset so a reviewer picks
## from what the study actually has rather than typing a name.
#' @noRd
.variable_choices <- function(spec, dataset = NULL) {
  if (is.null(spec)) return(NULL)

  variables <- spec$variables
  if (!is.null(dataset) && !is.na(dataset) && nzchar(dataset)) {
    variables <- variables[variables$dataset == dataset, , drop = FALSE]
  }
  if (nrow(variables) == 0) return(NULL)

  labels <- ifelse(
    is.na(variables$label) | !nzchar(variables$label),
    variables$variable,
    paste0(variables$variable, " -- ", variables$label)
  )
  stats::setNames(as.list(variables$variable), labels)
}


## --- editable analysis panel ------------------------------------------------

#' @noRd
.analysis_edit_ui <- function(row, model, state, ns) {
  spec <- state$spec
  findings <- state$findings()
  own <- findings[findings$id == row$id, , drop = FALSE]

  dataset_choices <- if (is.null(spec)) {
    NULL
  } else {
    unique(spec$variables$dataset)
  }

  shiny::tagList(
    .analysis_back_link(row, ns),
    shiny::div(
      class = "d-flex justify-content-between align-items-start",
      shiny::div(
        shiny::h5(if (is.na(row$label)) row$id else row$label),
        shiny::div(class = "text-muted small mb-2", row$id)
      ),
      shiny::tags$button(
        class = "btn btn-sm btn-outline-danger",
        onclick = .select_js(ns("remove_analysis"), "analyses", row$id),
        "Remove line"
      )
    ),
    if (nrow(own) > 0) .findings_list(own),

    ## Shared entities can be changed for every analysis that uses them, or
    ## detached so this line can differ. Both are one click, and which one you
    ## are about to do is stated rather than implied.
    .detach_controls(row, model, ns),

    bslib::layout_columns(
      col_widths = c(6, 6),

      shiny::textInput(ns("label"), "Label", value = .blank_na(row$label)),

      shiny::selectizeInput(
        ns("methodId"), "Method",
        choices  = .method_choices(model),
        selected = row$methodId
      ),

      if (is.null(dataset_choices)) {
        shiny::textInput(ns("dataset"), "Dataset",
                         value = .blank_na(row$dataset))
      } else {
        shiny::selectizeInput(
          ns("dataset"), "Dataset",
          choices  = dataset_choices,
          selected = row$dataset
        )
      },

      if (is.null(spec)) {
        shiny::textInput(ns("variable"), "Variable",
                         value = .blank_na(row$variable))
      } else {
        shiny::selectizeInput(
          ns("variable"), "Variable",
          choices  = .variable_choices(spec, row$dataset),
          selected = row$variable,
          options  = list(create = TRUE)
        )
      },

      shiny::selectizeInput(
        ns("analysisSetId"), "Population",
        choices  = .entity_choices(model, "analysis_sets"),
        selected = row$analysisSetId
      ),

      shiny::selectizeInput(
        ns("dataSubsetId"), "Data subset",
        choices  = .entity_choices(model, "data_subsets", include_none = TRUE),
        selected = if (is.na(row$dataSubsetId)) "" else row$dataSubsetId
      ),

      shiny::selectizeInput(
        ns("grouping_ids"), "Grouped by",
        choices  = .entity_choices(model, "groupings"),
        selected = .split_values(row$grouping_ids),
        multiple = TRUE,
        options  = list(plugins = list("drag_drop"))
      ),

      shiny::selectizeInput(
        ns("reason"), "Reason (why this analysis is planned)",
        choices  = c("", .ANALYSIS_REASONS),
        selected = .blank_na(row$reason)
      ),

      shiny::selectizeInput(
        ns("purpose"), "Purpose (role in the body of evidence)",
        choices  = c("", .ANALYSIS_PURPOSES),
        selected = .blank_na(row$purpose)
      ),

      shiny::selectizeInput(
        ns("strata"), "Stratified by",
        choices  = c("", .variable_choices(spec, row$dataset) %||%
                       list(row$strata)),
        selected = .blank_na(row$strata),
        options  = list(create = TRUE)
      )
    ),

    ## One table usually shares one column layout: the same "Grouped by"
    ## correction should not need repeating on every line.
    shiny::div(
      class = "mb-2",
      shiny::actionButton(
        ns("apply_groupings_all"),
        "Apply this line's groupings to every line in this output",
        class = "btn-sm btn-outline-secondary"
      )
    ),

    shiny::checkboxInput(ns("includeTotal"), "Include a total column",
                         value = isTRUE(row$includeTotal)),
    shiny::textInput(ns("totalLabel"), "Total column label",
                     value = .blank_na(row$totalLabel),
                     placeholder = "Total"),
    shiny::textAreaInput(ns("description"), "Description",
                         value = .blank_na(row$description), rows = 2),

    shiny::div(
      class = "text-muted small mt-2",
      "Shell annotation: ", shiny::tags$code(.blank_na(row$annotation))
    ),

    shiny::tags$details(
      class = "mt-3",
      shiny::tags$summary(class = "small text-muted", "Raw JSON"),
      .json_block(row$raw[[1]])
    )
  )
}

## A "detach" control for each shared entity this analysis points at, shown
## only when the entity really is shared. Editing a population used by thirty
## analyses is sometimes exactly right and sometimes a mistake; the count says
## which situation you are in, and detaching is the way out of the second.
##
## Methods are listed but not detachable: the engine dispatches on the method
## id, so a per-analysis copy would have no executor. Changing which method a
## line uses is the method dropdown's job.
#' @noRd
.detach_controls <- function(row, model, ns) {
  usage <- .entity_usage(model)

  shared <- list(
    list(pool = "methods",       id = row$methodId,      detachable = FALSE),
    list(pool = "analysis_sets", id = row$analysisSetId, detachable = TRUE),
    list(pool = "data_subsets",  id = row$dataSubsetId,  detachable = TRUE)
  )
  for (grouping_id in .split_values(row$grouping_ids)) {
    shared[[length(shared) + 1L]] <- list(
      pool = "groupings", id = grouping_id, detachable = TRUE
    )
  }

  controls <- list()
  for (entry in shared) {
    if (is.na(entry$id) || !nzchar(entry$id)) next
    count <- .usage_count(usage[[entry$pool]], entry$id)
    if (count < 2) next

    controls[[length(controls) + 1L]] <- shiny::div(
      class = "d-flex justify-content-between align-items-center small py-1",
      shiny::span(
        shiny::tags$code(entry$id),
        shiny::span(class = "text-muted",
                    paste0(" is shared by ", count, " analyses"))
      ),
      shiny::div(
        class = "btn-group btn-group-sm",
        shiny::tags$button(
          class = "btn btn-outline-primary py-0",
          onclick = .select_js(ns("open_entity"), entry$pool, entry$id),
          "Edit in Entities"
        ),
        if (entry$detachable) {
          shiny::tags$button(
            class = "btn btn-outline-secondary py-0",
            onclick = .detach_js(ns("detach"), entry$pool, entry$id, row$id),
            "Detach for this line"
          )
        }
      )
    )
  }

  if (length(controls) == 0) return(NULL)

  shiny::div(
    class = "alert alert-light border py-2 px-3 mb-3",
    shiny::div(
      class = "text-muted small mb-1",
      "Editing these changes every analysis that uses them."
    ),
    controls
  )
}

## A bare "this happened" input, for buttons that carry no payload.
#' @noRd
.event_js <- function(input_id) {
  paste0(
    "Shiny.setInputValue('", input_id, "', Date.now(), ",
    "{priority: 'event'}); return false;"
  )
}

#' @noRd
.detach_js <- function(input_id, pool, entity_id, analysis_id) {
  paste0(
    "Shiny.setInputValue('", input_id, "', ",
    "{pool: '", pool, "', entity: '", entity_id, "', id: '", analysis_id,
    "'}, {priority: 'event'}); return false;"
  )
}

#' @noRd
.blank_na <- function(x) {
  if (length(x) == 0 || is.na(x)) "" else as.character(x)
}

## Turn an input value back into what the model stores: an empty box means
## "not set", which for an optional field means the key goes away.
#' @noRd
.input_to_value <- function(value) {
  if (is.null(value) || length(value) == 0) return(NA_character_)
  if (!nzchar(value)) return(NA_character_)
  value
}

## Wire one input per editable field. Every observer ignores its first firing
## (that is just the input being created) and writes through apply_edit(),
## which drops no-ops.
#' @noRd
.observe_analysis_inputs <- function(input, state, selected_id, session) {
  simple_fields <- c("label", "description", "dataset", "variable",
                     "analysisSetId", "dataSubsetId", "reason", "purpose",
                     "totalLabel")

  for (field in simple_fields) {
    local({
      this_field <- field
      shiny::observeEvent(input[[this_field]], {
        id <- selected_id()
        if (is.null(id)) return()

        value <- input[[this_field]]
        ## dataSubsetId uses "" as a real value meaning "no subset"; every
        ## other field treats an empty box as "not set".
        value <- if (identical(this_field, "dataSubsetId")) {
          value %||% ""
        } else {
          .input_to_value(value)
        }
        apply_edit(state, "analyses", id, this_field, value)
      }, ignoreInit = TRUE)
    })
  }

  shiny::observeEvent(input$strata, {
    id <- selected_id()
    if (is.null(id)) return()
    apply_edit(state, "analyses", id, "strata", .input_to_value(input$strata))
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$includeTotal, {
    id <- selected_id()
    if (is.null(id)) return()
    apply_edit(state, "analyses", id, "includeTotal", isTRUE(input$includeTotal))
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$grouping_ids, {
    id <- selected_id()
    if (is.null(id)) return()
    value <- if (length(input$grouping_ids) == 0) {
      NA_character_
    } else {
      paste(input$grouping_ids, collapse = .MODEL_SEP)
    }
    apply_edit(state, "analyses", id, "grouping_ids", value)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  ## Bulk grouping assignment: copy this line's ordered "Grouped by" onto
  ## every other analysis of the same output, after a preview of what will
  ## change. One history push, so one undo reverses the whole action.
  shiny::observeEvent(input$apply_groupings_all, {
    id <- selected_id()
    if (is.null(id)) return()
    model <- state$model()
    idx <- match(id, model$analyses$id)
    if (is.na(idx)) return()

    out_id <- model$analyses$output_id[idx]
    value  <- model$analyses$grouping_ids[idx]
    if (is.na(out_id)) return()

    same_output <- !is.na(model$analyses$output_id) &
      model$analyses$output_id == out_id & model$analyses$id != id
    targets <- model$analyses$id[same_output]
    differs <- vapply(targets, function(other) {
      !identical(model$analyses$grouping_ids[match(other, model$analyses$id)],
                 value)
    }, logical(1))
    targets <- targets[differs]

    if (length(targets) == 0) {
      shiny::showNotification(
        "Every line in this output already has these groupings.",
        type = "message", duration = 5
      )
      return()
    }

    labels <- vapply(targets, function(other) {
      lbl <- model$analyses$label[match(other, model$analyses$id)]
      if (is.na(lbl) || !nzchar(lbl)) other else lbl
    }, character(1))
    shiny::showModal(shiny::modalDialog(
      title = "Apply groupings to the whole output?",
      shiny::p(
        shiny::tags$code(if (is.na(value)) "(no groupings)" else value),
        " will replace the groupings of ", shiny::strong(length(targets)),
        " other line(s):"
      ),
      shiny::tags$ul(class = "small",
                     lapply(labels, function(x) shiny::tags$li(x))),
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton(session$ns("confirm_apply_groupings_all"),
                            "Apply to all", class = "btn-primary")
      )
    ))
  })

  shiny::observeEvent(input$confirm_apply_groupings_all, {
    shiny::removeModal()
    id <- selected_id()
    if (is.null(id)) return()
    model <- state$model()
    idx <- match(id, model$analyses$id)
    if (is.na(idx)) return()
    out_id <- model$analyses$output_id[idx]
    value  <- model$analyses$grouping_ids[idx]

    same_output <- !is.na(model$analyses$output_id) &
      model$analyses$output_id == out_id & model$analyses$id != id
    targets <- model$analyses$id[same_output]

    updated <- model
    log_rows <- list()
    for (other in targets) {
      old <- updated$analyses$grouping_ids[match(other, updated$analyses$id)]
      if (identical(old, value)) next
      updated <- model_set_field(updated, "analyses", other,
                                 "grouping_ids", value)
      log_rows[[length(log_rows) + 1L]] <- data.frame(
        time  = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        pool  = "analyses",
        id    = other,
        field = "grouping_ids",
        old   = if (is.na(old)) "(not set)" else as.character(old),
        new   = if (is.na(value)) "(not set)" else as.character(value),
        stringsAsFactors = FALSE
      )
    }
    ## Nothing actually differed: no history entry, no phantom undo step.
    if (length(log_rows) == 0) return()

    .push_history(state)
    state$model(updated)
    state$edit_log(rbind(state$edit_log(), do.call(rbind, log_rows)))
    state$findings(validate_ars_model(updated, state$spec, state$report))
    .write_autosave(state)
    shiny::showNotification(
      paste("Groupings applied to", length(log_rows), "line(s)."),
      type = "message", duration = 5
    )
  })

  ## Choosing a standard method that is not in the file yet adds it first, so
  ## the analysis never points at a method the reporting event does not carry.
  shiny::observeEvent(input$methodId, {
    id <- selected_id()
    if (is.null(id) || is.null(input$methodId)) return()

    model <- state$model()
    if (!input$methodId %in% model$methods$id &&
          input$methodId %in% .standard_method_ids()) {
      state$model(model_add_method_from_catalogue(model, input$methodId))
    }
    apply_edit(state, "analyses", id, "methodId", input$methodId)
  }, ignoreInit = TRUE)
}


## --- save flow --------------------------------------------------------------

#' @noRd
mod_save_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("dirty"), inline = TRUE),
    shiny::uiOutput(ns("history"), inline = TRUE),
    ## Saving and leaving are separate decisions: Save writes and stays,
    ## Save As writes a copy elsewhere, Save and close writes and ends the
    ## session. Each says what it does.
    shiny::actionButton(ns("save_stay"), "Save",
                        class = "btn-primary btn-sm"),
    shiny::actionButton(ns("save_as"), "Save As", class = "btn-sm"),
    shiny::actionButton(ns("save"), "Save and close", class = "btn-sm"),
    shiny::actionButton(ns("discard"), "Discard", class = "btn-sm")
  )
}

## Recompute from the current model every time a save is attempted. The
## reactive findings panel can be one flush behind a just-completed edit, so it
## is informative state, not authority to write a deliverable.
#' @noRd
.current_save_gate <- function(state) {
  gate <- .model_validation_gate(
    state$model(), spec = state$spec, report = state$report
  )
  state$findings(gate$findings)
  gate
}

#' @noRd
.show_save_blocker <- function(gate) {
  refs <- gate$blocking_refs
  shiny::showModal(shiny::modalDialog(
    title = "Save blocked",
    size = "l",
    shiny::div(
      class = "alert alert-danger",
      shiny::strong("This reporting event still has blocking problems."),
      shiny::br(),
      gate$summary
    ),
    if (length(refs) > 0L) {
      shiny::p(
        class = "small",
        "Finding references: ", shiny::tags$code(paste(refs, collapse = ", "))
      )
    },
    shiny::p(
      class = "text-muted small",
      "Your working changes remain in the editor and in crash recovery."
    ),
    footer = shiny::modalButton("Keep editing")
  ))
  invisible(FALSE)
}

#' @noRd
.save_is_ready <- function(state) {
  gate <- .current_save_gate(state)
  if (gate$blocked) return(.show_save_blocker(gate))
  TRUE
}

#' @noRd
mod_save_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    output$dirty <- shiny::renderUI({
      n <- nrow(state$edit_log())
      if (n == 0) {
        return(shiny::span(class = "text-muted small", "No changes yet"))
      }
      shiny::span(
        class = "badge text-bg-primary",
        paste(n, if (n == 1) "unsaved change" else "unsaved changes")
      )
    })

    output$history <- shiny::renderUI({
      shiny::div(
        class = "btn-group btn-group-sm",
        shiny::tags$button(
          class = "btn btn-outline-secondary",
          title = "Undo",
          disabled = if (!.can_undo(state)) "disabled",
          onclick = .event_js(session$ns("undo")),
          shiny::HTML("&larr;")
        ),
        shiny::tags$button(
          class = "btn btn-outline-secondary",
          title = "Redo",
          disabled = if (!.can_redo(state)) "disabled",
          onclick = .event_js(session$ns("redo")),
          shiny::HTML("&rarr;")
        )
      )
    })

    shiny::observeEvent(input$undo, {
      .undo(state)
      .write_autosave(state)
    })

    shiny::observeEvent(input$redo, {
      .redo(state)
      .write_autosave(state)
    })

    ## A session that died mid-review left its work behind; offer it back
    ## rather than letting the reviewer discover the loss themselves. Asked
    ## once, when the editor opens.
    shiny::observeEvent(TRUE, once = TRUE, {
      recovered <- .read_autosave(state$source_path)
      if (is.null(recovered)) return()

      shiny::showModal(shiny::modalDialog(
        title = "Unsaved changes from an earlier session",
        shiny::p(
          "A previous session left ", shiny::strong(nrow(recovered$edit_log)),
          " unsaved change(s) to this file, last touched ",
          format(recovered$saved_at, "%Y-%m-%d %H:%M"), "."
        ),
        shiny::p(class = "text-muted small",
                 "The file on disk was never modified."),
        footer = shiny::tagList(
          shiny::actionButton(session$ns("discard_recovery"),
                              "Start fresh"),
          shiny::actionButton(session$ns("accept_recovery"),
                              "Restore them", class = "btn-primary")
        )
      ))
    })

    shiny::observeEvent(input$accept_recovery, {
      recovered <- .read_autosave(state$source_path)
      if (!is.null(recovered)) {
        .push_history(state)
        .restore_snapshot(state, recovered)
      }
      shiny::removeModal()
    })

    shiny::observeEvent(input$discard_recovery, {
      .clear_autosave(state$source_path)
      shiny::removeModal()
    })

    ## Show what is about to be written before writing it. This doubles as
    ## the QC record a clinical reviewer needs.
    shiny::observeEvent(input$save, {
      if (!.save_is_ready(state)) return()

      shiny::showModal(shiny::modalDialog(
        title = "Save these changes?",
        size = "l",

        if (!is.null(state$source_path)) {
          shiny::p(
            class = "small",
            "Destination: ", shiny::tags$code(state$source_path),
            shiny::br(),
            shiny::span(class = "text-muted",
                        "The previous file is backed up with a timestamp first.")
          )
        },

        .diff_table_ui(state$edit_log()),

        footer = shiny::tagList(
          shiny::modalButton("Keep editing"),
          shiny::actionButton(session$ns("confirm_save"), "Save and close",
                              class = "btn-primary")
        )
      ))
    })

    shiny::observeEvent(input$confirm_save, {
      if (!.save_is_ready(state)) return()
      shiny::removeModal()
      shiny::stopApp(list(
        model       = state$model(),
        edit_log    = state$edit_log(),
        source_path = state$source_path
      ))
    })

    ## Save in place: write now, stay in the editor. Shows the destination
    ## before writing -- the user should never have to guess which file a
    ## save will touch.
    shiny::observeEvent(input$save_stay, {
      if (is.null(state$source_path)) {
        shiny::showNotification(
          "This reporting event was not opened from a file -- use Save As.",
          type = "warning", duration = 6
        )
        return()
      }
      if (!.save_is_ready(state)) return()

      shiny::showModal(shiny::modalDialog(
        title = "Save these changes?",
        size = "l",
        shiny::p(
          class = "small",
          "Destination: ", shiny::tags$code(state$source_path),
          shiny::br(),
          shiny::span(class = "text-muted",
                      "The previous file is backed up with a timestamp first.")
        ),
        .diff_table_ui(state$edit_log()),
        footer = shiny::tagList(
          shiny::modalButton("Keep editing"),
          shiny::actionButton(session$ns("confirm_save_stay"), "Save",
                              class = "btn-primary")
        )
      ))
    })

    shiny::observeEvent(input$confirm_save_stay, {
      if (!.save_is_ready(state)) return()
      shiny::removeModal()
      .app_write_event(state, state$source_path, reset_dirty = TRUE)
    })

    ## Save As: write a copy elsewhere; the original file and this editing
    ## session are untouched.
    shiny::observeEvent(input$save_as, {
      if (!.save_is_ready(state)) return()

      default_dir <- dirname(state$source_path %||% file.path(getwd(), "x"))
      default_name <- paste0(
        sub("\\.json$", "",
            basename(state$source_path %||% "reporting_event.json")),
        format(Sys.time(), "_%Y%m%d_%H%M"), ".json"
      )
      shiny::showModal(shiny::modalDialog(
        title = "Save a copy as",
        size = "l",
        shiny::textInput(session$ns("save_as_path"), "Destination path",
                         value = file.path(default_dir, default_name),
                         width = "100%"),
        shiny::p(class = "text-muted small",
                 "The original file stays untouched, and this session keeps ",
                 "editing it -- Save As is an export, not a switch."),
        .diff_table_ui(state$edit_log()),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("confirm_save_as"), "Save copy",
                              class = "btn-primary")
        )
      ))
    })

    shiny::observeEvent(input$confirm_save_as, {
      if (!.save_is_ready(state)) return()

      path <- trimws(input$save_as_path %||% "")
      if (!nzchar(path)) {
        shiny::showNotification("Give the copy a destination path.",
                                type = "warning")
        return()
      }
      if (!grepl("\\.json$", path, ignore.case = TRUE)) {
        path <- paste0(path, ".json")
      }
      if (!dir.exists(dirname(path))) {
        shiny::showNotification(
          paste0("Folder does not exist: ", dirname(path)),
          type = "error", duration = 8
        )
        return()
      }
      shiny::removeModal()
      .app_write_event(state, path, reset_dirty = FALSE)
    })

    shiny::observeEvent(input$discard, {
      if (nrow(state$edit_log()) == 0) {
        shiny::stopApp(NULL)
        return()
      }
      shiny::showModal(shiny::modalDialog(
        title = "Discard your changes?",
        paste(nrow(state$edit_log()),
              "change(s) will be lost. Nothing has been written yet."),
        footer = shiny::tagList(
          shiny::modalButton("Keep editing"),
          shiny::actionButton(session$ns("confirm_discard"),
                              "Discard and close", class = "btn-danger")
        )
      ))
    })

    shiny::observeEvent(input$confirm_discard, {
      shiny::removeModal()
      shiny::stopApp(NULL)
    })
  })
}

## Write the working copy from inside the app, through the same backup +
## atomic-write + sidecar path a Save-and-close uses. `reset_dirty` marks the
## session clean afterwards (a Save in place); Save As leaves the dirty state
## alone because the original file still differs from the working copy.
#' @noRd
.app_write_event <- function(state, path, reset_dirty) {
  result <- list(
    model    = state$model(),
    edit_log = state$edit_log(),
    ## For a Save As, do not clear the ORIGINAL file's crash-recovery copy:
    ## the working changes are still unsaved relative to that file.
    source_path = if (isTRUE(reset_dirty)) state$source_path else NULL
  )

  written <- tryCatch(
    .edit_ars_finish(result, path, state$spec, state$report),
    error = function(e) e
  )
  if (inherits(written, "error")) {
    shiny::showNotification(
      paste("Save failed:", conditionMessage(written)),
      type = "error", duration = 10
    )
    return(invisible(FALSE))
  }
  if (is.list(written) && identical(written$saved, FALSE)) {
    .show_save_blocker(written$validation_gate)
    return(invisible(FALSE))
  }

  if (isTRUE(reset_dirty)) {
    state$edit_log(state$edit_log()[0, , drop = FALSE])
  }

  shiny::showModal(shiny::modalDialog(
    title = "Saved",
    shiny::p("File: ", shiny::tags$code(path)),
    shiny::p("Saved: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    if (!isTRUE(reset_dirty)) {
      shiny::p(class = "text-muted small",
               "This session still edits the original file; the copy is a ",
               "snapshot of the current working state.")
    },
    footer = shiny::modalButton("Continue editing"),
    easyClose = TRUE
  ))
  invisible(TRUE)
}

## The edit log as a "what changed" table, collapsed to the latest value per
## field so a field edited five times reads as one change.
#' @noRd
.diff_summary <- function(edit_log) {
  if (nrow(edit_log) == 0) return(edit_log[, c("pool", "id", "field")])

  key <- paste(edit_log$pool, edit_log$id, edit_log$field, sep = "|")
  first_seen <- !duplicated(key)
  last_seen  <- !duplicated(key, fromLast = TRUE)

  summary <- edit_log[first_seen, c("pool", "id", "field", "old"),
                      drop = FALSE]
  summary$new <- edit_log$new[last_seen]

  ## A field edited back to where it started is not a change.
  summary <- summary[summary$old != summary$new, , drop = FALSE]
  rownames(summary) <- NULL
  summary
}

#' @noRd
.diff_table_ui <- function(edit_log) {
  summary <- .diff_summary(edit_log)

  if (nrow(summary) == 0) {
    return(shiny::div(
      class = "text-muted",
      "Nothing has changed -- saving will rewrite the file unchanged."
    ))
  }

  shiny::tagList(
    shiny::p(paste(nrow(summary), "field(s) changed:")),
    shiny::tags$table(
      class = "table table-sm small",
      shiny::tags$thead(shiny::tags$tr(
        shiny::tags$th("Entity"), shiny::tags$th("Field"),
        shiny::tags$th("From"), shiny::tags$th("To")
      )),
      shiny::tags$tbody(lapply(seq_len(nrow(summary)), function(i) {
        shiny::tags$tr(
          shiny::tags$td(summary$id[i]),
          shiny::tags$td(summary$field[i]),
          shiny::tags$td(shiny::tags$code(summary$old[i])),
          shiny::tags$td(shiny::tags$code(summary$new[i]))
        )
      }))
    )
  )
}
