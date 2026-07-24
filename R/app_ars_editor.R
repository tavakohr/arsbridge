## arsbridge -- app_ars_editor.R
## ---------------------------------------------------------------------------
## The Shiny application behind view_ars() and edit_ars(). One app, two modes:
## "view" renders everything read-only, "edit" swaps the detail panels for
## inputs and enables saving.
##
## Everything Shiny in this package lives in this file and the mod_*.R files.
## They are only reachable through view_ars() / edit_ars(), which check that
## shiny, bslib and DT are installed first -- so the package still works
## normally when they are not.
##
## Modules are plain internal functions rather than files sourced from
## inst/shiny, so they can be unit-tested with shiny::testServer() and are
## visible to R CMD check like any other code.

## Shared state passed to every module server. The model is a reactiveVal so
## an edit anywhere re-renders everything that reads it.
#' @noRd
.editor_state <- function(model, spec, report, source_path, mode) {
  list(
    model       = shiny::reactiveVal(model),
    selected    = shiny::reactiveVal(NULL),
    findings    = shiny::reactiveVal(
      validate_ars_model(model, spec, report)
    ),
    edit_log    = shiny::reactiveVal(.new_edit_log()),
    ## Set to an .add_request() to open the add-analysis wizard; the request
    ## can come from an output panel or from a gap finding.
    add_request = shiny::reactiveVal(NULL),
    spec        = spec,
    report      = report,
    source_path = source_path,
    mode        = mode
  )
}

#' @noRd
.new_edit_log <- function() {
  data.frame(
    time  = character(0),
    pool  = character(0),
    id    = character(0),
    field = character(0),
    old   = character(0),
    new   = character(0),
    stringsAsFactors = FALSE
  )
}

## Severity -> bslib theme colour, used by every badge and value box so the
## whole app reads one way.
#' @noRd
.severity_class <- function(severity) {
  switch(
    severity,
    FAIL = "danger",
    WARN = "warning",
    INFO = "secondary",
    "success"
  )
}

## The worst severity among a set of findings, or NA when there are none.
#' @noRd
.worst_severity <- function(severities) {
  for (level in c("FAIL", "WARN", "INFO")) {
    if (level %in% severities) return(level)
  }
  NA_character_
}

#' @noRd
.severity_badge <- function(severity) {
  if (is.na(severity)) return(NULL)
  shiny::span(
    class = paste0("badge rounded-pill text-bg-", .severity_class(severity)),
    severity
  )
}

## A read-only label/value row, used by every detail panel.
#' @noRd
.detail_row <- function(label, value) {
  if (is.null(value) || (length(value) == 1 && is.na(value))) value <- "--"
  shiny::div(
    class = "row mb-1",
    shiny::div(class = "col-4 text-muted small", label),
    shiny::div(class = "col-8", shiny::tags$code(as.character(value)))
  )
}

#' @noRd
.json_block <- function(node) {
  shiny::tags$pre(
    class = "small bg-body-tertiary p-2 rounded",
    style = "max-height: 30rem; overflow: auto;",
    jsonlite::toJSON(node, auto_unbox = TRUE, pretty = TRUE, null = "null")
  )
}


## --- application ------------------------------------------------------------

#' @noRd
.ars_editor_app <- function(model, spec = NULL, report = NULL,
                            source_path = NULL,
                            mode = c("view", "edit")) {
  mode <- match.arg(mode)
  .assert_ars_model(model)

  study <- .chr_field(model$template[["id"]])
  title <- if (is.na(study)) "ARS review" else paste("ARS review --", study)

  ui <- bslib::page_sidebar(
    title = title,
    theme = bslib::bs_theme(version = 5),

    sidebar = bslib::sidebar(
      width = 380,
      title = "Outputs",
      mod_tree_ui("tree")
    ),

    ## The header carries the one thing a reviewer always wants to know:
    ## whether this event is safe to execute.
    mod_status_ui("status"),

    bslib::navset_card_underline(
      id = "main_tabs",
      bslib::nav_panel("Details", mod_detail_ui("detail")),
      bslib::nav_panel("Entities", mod_entity_library_ui("library")),
      bslib::nav_panel("Validation", mod_validation_ui("validation")),
      bslib::nav_panel("JSON", mod_json_ui("json"))
    )
  )

  server <- function(input, output, session) {
    state <- .editor_state(model, spec, report, source_path, mode)

    mod_tree_server("tree", state)
    mod_detail_server("detail", state)
    mod_entity_library_server("library", state)
    mod_validation_server("validation", state)
    mod_json_server("json", state)
    mod_status_server("status", state, session)

    if (identical(mode, "edit")) {
      mod_add_analysis_server("add", state)
    }
  }

  shiny::shinyApp(ui, server)
}


## --- status header ----------------------------------------------------------

#' @noRd
mod_status_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("status"))
}

#' @noRd
mod_status_server <- function(id, state, parent_session = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    output$status <- shiny::renderUI({
      findings <- state$findings()
      counts <- vapply(
        c("FAIL", "WARN", "INFO"),
        function(level) sum(findings$severity == level),
        integer(1)
      )

      summary_text <- if (counts[["FAIL"]] > 0) {
        paste0(counts[["FAIL"]], " blocking problem",
               if (counts[["FAIL"]] == 1) "" else "s")
      } else {
        "No blocking problems"
      }

      shiny::div(
        class = "d-flex align-items-center gap-2 mb-2",
        shiny::span(
          class = paste0(
            "badge text-bg-",
            if (counts[["FAIL"]] > 0) "danger" else "success"
          ),
          summary_text
        ),
        shiny::span(class = "badge text-bg-warning",
                    paste(counts[["WARN"]], "to review")),
        shiny::span(class = "badge text-bg-secondary",
                    paste(counts[["INFO"]], "notes")),
        if (identical(state$mode, "edit")) {
          shiny::div(class = "ms-auto", mod_save_ui(session$ns("save")))
        }
      )
    })

    if (identical(state$mode, "edit")) {
      mod_save_server("save", state)
    }
  })
}
