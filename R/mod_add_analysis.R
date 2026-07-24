## arsbridge -- mod_add_analysis.R
## ---------------------------------------------------------------------------
## Adding the lines the generator missed, and the structural edits that go with
## it: removing a line, reordering an output's lines, and detaching a shared
## entity so one analysis can differ from the rest.
##
## The wizard is reuse-first by construction: every picker offers what the
## reporting event already contains, so the default outcome of adding a line is
## that no new shared entity appears. Near-duplicate methods and populations
## are the thing that makes a hand-corrected event unreadable, and the cheapest
## way to avoid them is to make reuse the path of least resistance.
##
## A gap finding carries the DATASET.VARIABLE it is about, so "the shell
## annotates this but no analysis uses it" turns into a pre-filled wizard
## rather than a re-typing exercise.

## What the wizard needs to know before it opens: which output, and anything
## already known about the line being added.
#' @noRd
.add_request <- function(output_id, dataset = NA_character_,
                         variable = NA_character_, label = NA_character_,
                         annotation = NA_character_, after = NULL) {
  list(
    output_id  = output_id,
    dataset    = dataset,
    variable   = variable,
    label      = label,
    annotation = annotation,
    after      = after
  )
}

## Split a "DATASET.VARIABLE" reference the way the validation report writes
## it. Anything that is not in that shape comes back as no dataset.
#' @noRd
.split_variable_ref <- function(ref) {
  if (is.null(ref) || is.na(ref) || !nzchar(ref)) {
    return(list(dataset = NA_character_, variable = NA_character_))
  }
  parts <- strsplit(ref, ".", fixed = TRUE)[[1]]
  if (length(parts) < 2) {
    return(list(dataset = NA_character_, variable = ref))
  }
  list(dataset = parts[1], variable = paste(parts[-1], collapse = "."))
}

## Choices for "insert after": the output's existing lines, plus the top.
#' @noRd
.position_choices <- function(model, output_id) {
  index <- match(output_id, model$outputs$id)
  if (is.na(index)) return(list("At the end" = ""))

  references <- .split_values(model$outputs$referenced_analysis_ids[index])
  if (length(references) == 0) return(list("As the first line" = ""))

  labels <- vapply(references, function(id) {
    row <- match(id, model$analyses$id)
    if (is.na(row)) return(paste("After", id))
    label <- model$analyses$label[row]
    if (is.na(label) || !nzchar(label)) label <- id
    paste("After", label)
  }, character(1))

  c(list("At the end" = ""), stats::setNames(as.list(references), labels))
}

## The most common shape in this output, offered as the starting point so a
## missing line lands looking like its neighbours rather than like a default.
#' @noRd
.sibling_defaults <- function(model, output_id) {
  siblings <- model$analyses[
    !is.na(model$analyses$output_id) & model$analyses$output_id == output_id, ,
    drop = FALSE
  ]
  if (nrow(siblings) == 0) siblings <- model$analyses

  most_common <- function(values) {
    values <- values[!is.na(values)]
    if (length(values) == 0) return(NA_character_)
    names(sort(table(values), decreasing = TRUE))[1]
  }

  list(
    method_id       = most_common(siblings$methodId),
    analysis_set_id = most_common(siblings$analysisSetId),
    data_subset_id  = "",
    grouping_ids    = .split_values(most_common(siblings$grouping_ids)),
    dataset         = most_common(siblings$dataset),
    include_total   = isTRUE(most_common(siblings$includeTotal) == "TRUE")
  )
}


## The wizard is a modal, which mounts at the app level, so this module has no
## UI half -- only a server that opens the dialog when something requests it.
#' @noRd
mod_add_analysis_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Opening the wizard is a request from elsewhere in the app: an output's
    ## "Add analysis" button, or a gap finding.
    shiny::observeEvent(state$add_request(), {
      request <- state$add_request()
      if (is.null(request)) return()
      shiny::showModal(.add_analysis_modal(request, state, ns))
    })

    ## Keep the variable list in step with the dataset while the wizard is
    ## open, so the choices are always ones that dataset really has.
    shiny::observeEvent(input$dataset, {
      if (is.null(state$spec)) return()
      shiny::updateSelectizeInput(
        session, "variable",
        choices  = c(list("Choose a variable" = ""),
                     .variable_choices(state$spec, input$dataset)),
        selected = shiny::isolate(input$variable)
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$confirm_add, {
      request <- state$add_request()
      if (is.null(request)) return()

      ## A line with no variable analyses nothing, so the wizard says so
      ## rather than adding something the engine will skip.
      if (is.null(input$variable) || !nzchar(input$variable)) {
        output$variable_problem <- shiny::renderUI({
          shiny::div(class = "alert alert-warning py-2 mt-2 mb-0",
                     "Choose the variable this line analyses.")
        })
        return()
      }

      label <- input$label
      if (is.null(label) || !nzchar(label)) label <- input$variable

      model <- state$model()

      ## A standard method chosen from the catalogue has to exist before an
      ## analysis can point at it.
      if (!input$methodId %in% model$methods$id &&
            input$methodId %in% .standard_method_ids()) {
        model <- model_add_method_from_catalogue(model, input$methodId)
      }

      model <- model_add_analysis(
        model,
        output_id       = request$output_id,
        label           = label,
        dataset         = input$dataset %||% "",
        variable        = input$variable %||% "",
        method_id       = input$methodId,
        analysis_set_id = input$analysisSetId,
        data_subset_id  = input$dataSubsetId %||% "",
        grouping_ids    = input$grouping_ids %||% character(0),
        annotation      = .blank_na(request$annotation),
        include_total   = isTRUE(input$includeTotal),
        after           = if (is.null(input$after) || !nzchar(input$after)) {
          NULL
        } else {
          input$after
        }
      )

      added_id <- attr(model, "last_added")
      .record_structural_edit(state, model, "analyses", added_id, "added",
                              "(new line)", label)

      shiny::removeModal()
      state$add_request(NULL)
      ## Land the reviewer on what they just created, so they can check it.
      state$selected(list(pool = "analyses", id = added_id))
    })

    shiny::observeEvent(input$cancel_add, {
      shiny::removeModal()
      state$add_request(NULL)
    })
  })
}

#' @noRd
.add_analysis_modal <- function(request, state, ns) {
  model <- state$model()
  defaults <- .sibling_defaults(model, request$output_id)
  spec <- state$spec

  dataset <- if (!is.na(request$dataset)) request$dataset else defaults$dataset
  dataset_choices <- if (is.null(spec)) NULL else unique(spec$variables$dataset)

  output_label <- {
    index <- match(request$output_id, model$outputs$id)
    label <- model$outputs$label[index]
    if (is.na(label) || !nzchar(label)) request$output_id else label
  }

  shiny::modalDialog(
    title = "Add an analysis",
    size = "l",

    shiny::div(
      class = "text-muted small mb-3",
      "Adding a line to ", shiny::strong(output_label), ". ",
      "Everything is chosen from what this reporting event already contains, ",
      "so the new line shares the methods and populations of its neighbours."
    ),

    bslib::layout_columns(
      col_widths = c(6, 6),

      shiny::textInput(ns("label"), "Label (as it reads in the shell)",
                       value = .blank_na(request$label)),

      shiny::selectInput(ns("after"), "Position",
                         choices = .position_choices(model, request$output_id),
                         selected = request$after %||% ""),

      if (is.null(dataset_choices)) {
        shiny::textInput(ns("dataset"), "Dataset", value = .blank_na(dataset))
      } else {
        shiny::selectizeInput(ns("dataset"), "Dataset",
                              choices = dataset_choices, selected = dataset)
      },

      if (is.null(spec)) {
        shiny::textInput(ns("variable"), "Variable",
                         value = .blank_na(request$variable))
      } else {
        ## No default: which variable the line analyses is the one thing the
        ## reviewer must decide, so it starts empty rather than at whichever
        ## variable happens to sort first.
        shiny::selectizeInput(
          ns("variable"), "Variable",
          choices  = c(list("Choose a variable" = ""),
                       .variable_choices(spec, dataset)),
          selected = .blank_na(request$variable),
          options  = list(create = TRUE)
        )
      },

      shiny::selectizeInput(ns("methodId"), "Method",
                            choices = .method_choices(model),
                            selected = defaults$method_id),

      shiny::selectizeInput(ns("analysisSetId"), "Population",
                            choices = .entity_choices(model, "analysis_sets"),
                            selected = defaults$analysis_set_id),

      shiny::selectizeInput(
        ns("dataSubsetId"), "Data subset",
        choices = .entity_choices(model, "data_subsets", include_none = TRUE),
        selected = ""
      ),

      shiny::selectizeInput(ns("grouping_ids"), "Grouped by",
                            choices = .entity_choices(model, "groupings"),
                            selected = defaults$grouping_ids,
                            multiple = TRUE)
    ),

    shiny::checkboxInput(ns("includeTotal"), "Include a total column",
                         value = defaults$include_total),
    shiny::uiOutput(ns("variable_problem")),

    footer = shiny::tagList(
      shiny::actionButton(ns("cancel_add"), "Cancel"),
      shiny::actionButton(ns("confirm_add"), "Add analysis",
                          class = "btn-primary")
    )
  )
}

## Structural edits are logged like field edits, so the diff-before-save panel
## and the sidecar record them the same way -- and, like field edits, they can
## be undone.
#' @noRd
.record_structural_edit <- function(state, model, pool, id, field, old, new) {
  .push_history(state)
  state$model(model)
  state$edit_log(rbind(state$edit_log(), data.frame(
    time  = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    pool  = pool,
    id    = id,
    field = field,
    old   = old,
    new   = new,
    stringsAsFactors = FALSE
  )))
  state$findings(validate_ars_model(model, state$spec, state$report))

  ## Unlike a typed field edit, a structural edit comes from a button, so
  ## there is no cursor to fight -- and the panel showing the old structure
  ## (a moved line still in its old place, stale values after a raw-JSON
  ## replacement) must catch up now.
  state$refresh(state$refresh() + 1L)

  .write_autosave(state)
  invisible(TRUE)
}

## Move one analysis up or down inside its output. Display order is part of
## the specification, so it has to be editable.
#' @noRd
model_move_analysis <- function(model, output_id, analysis_id, offset) {
  .assert_ars_model(model)

  index <- match(output_id, model$outputs$id)
  if (is.na(index)) {
    cli::cli_abort("No output {.val {output_id}} in this reporting event.")
  }

  references <- .split_values(model$outputs$referenced_analysis_ids[index])
  position <- match(analysis_id, references)
  if (is.na(position)) return(model)

  target <- position + offset
  if (target < 1 || target > length(references)) return(model)

  references[c(position, target)] <- references[c(target, position)]
  model$outputs$referenced_analysis_ids[index] <- paste(
    references, collapse = .MODEL_SEP
  )
  .model_refresh_row(model, "outputs", output_id)
}
