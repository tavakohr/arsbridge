## arsbridge -- mod_validation.R
## ---------------------------------------------------------------------------
## The findings panel, and the raw-JSON view.
##
## Findings are the work list: clicking one navigates to the entity it is
## about, so "what is wrong" and "where do I fix it" are the same gesture.

#' @noRd
mod_validation_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("summary")),
    DT::DTOutput(ns("findings"))
  )
}

#' @noRd
mod_validation_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    output$summary <- shiny::renderUI({
      findings <- state$findings()
      if (nrow(findings) == 0) {
        return(shiny::div(
          class = "alert alert-success py-2",
          "Nothing to fix: every reference resolves and every method is executable."
        ))
      }

      counts <- vapply(
        c("FAIL", "WARN", "INFO"),
        function(level) sum(findings$severity == level),
        integer(1)
      )

      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        bslib::value_box(
          title = "Blocking",
          value = counts[["FAIL"]],
          theme = "danger",
          shiny::span(class = "small", "References or methods that will fail")
        ),
        bslib::value_box(
          title = "To review",
          value = counts[["WARN"]],
          theme = "warning",
          shiny::span(class = "small", "Results may not be what the shell asks")
        ),
        bslib::value_box(
          title = "Notes",
          value = counts[["INFO"]],
          theme = "secondary",
          shiny::span(class = "small", "Nothing to do unless something looks off")
        )
      )
    })

    output$findings <- DT::renderDT(
      {
        findings <- state$findings()
        ## `ref` is machine-readable context for the app, not something a
        ## reviewer needs to read.
        DT::datatable(
          findings[, setdiff(names(findings), "ref"), drop = FALSE],
          rownames = FALSE,
          selection = "single",
          options = list(pageLength = 20, scrollX = TRUE)
        )
      },
      server = TRUE
    )

    ## A finding names the entity it is about, so selecting one navigates
    ## there rather than leaving the reviewer to find it. A gap goes further:
    ## the shell says a line should exist, so selecting it offers to add that
    ## line, pre-filled with the variable the shell named.
    shiny::observeEvent(input$findings_rows_selected, {
      findings <- state$findings()
      row <- findings[input$findings_rows_selected, , drop = FALSE]
      if (nrow(row) == 0) return()

      if (.is_gap_finding(row) && identical(state$mode, "edit")) {
        parts <- .split_variable_ref(row$ref)
        state$add_request(.add_request(
          output_id  = row$id,
          dataset    = parts$dataset,
          variable   = parts$variable,
          annotation = row$ref
        ))
        return()
      }

      if (!row$entity %in% names(.pool_registry())) return()
      model <- state$model()
      if (!row$id %in% model[[row$entity]]$id) return()

      state$selected(list(pool = row$entity, id = row$id))
    })
  })
}

## A gap is the one finding that names something that should exist but does
## not, so it is the one the app can act on directly.
#' @noRd
.is_gap_finding <- function(row) {
  identical(row$entity, "outputs") &&
    identical(row$field, "analyses") &&
    !is.na(row$ref)
}


## --- raw JSON ---------------------------------------------------------------
## Rendered only when the tab is opened: a full reporting event is large, and
## nobody needs it until they ask.

#' @noRd
mod_json_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "text-muted small mb-2",
      "The reporting event as it would be written, including every field the panels do not show."
    ),
    shiny::actionButton(ns("render"), "Show JSON", class = "btn-sm"),
    shiny::uiOutput(ns("json"))
  )
}

#' @noRd
mod_json_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    output$json <- shiny::renderUI({
      if (is.null(input$render) || input$render == 0) return(NULL)
      .json_block(model_to_ars(state$model()))
    })
  })
}
