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
## `parent_session` is the app's own session, so selecting a finding can
## switch the top-level tab -- the tabs belong to the app, not to this module.
## It follows mod_status_server(), which takes the same argument for the same
## reason, and defaults to NULL so the module is still testable on its own.
#' @noRd
mod_validation_server <- function(id, state, parent_session = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    output$summary <- shiny::renderUI({
      findings <- state$findings()
      if (nrow(findings) == 0) {
        return(shiny::div(
          class = "alert alert-success py-2",
          "Nothing to fix: every reference resolves and every method is executable."
        ))
      }

      ## `.FINDING_SEVERITY_RANK` is the severity LABELS, most serious first
      ## ("FAIL", "GAP", "WARN", "INFO"); vapply over it names the counts.
      counts <- vapply(
        .FINDING_SEVERITY_RANK,
        function(level) sum(findings$severity == level, na.rm = TRUE),
        integer(1)
      )
      ## GAP and FAIL name one thing in two vocabularies; a payload written
      ## before this release carries the older word. `[` yields NA on a label
      ## the frame does not use, so the sum stays safe.
      n_gap <- sum(counts[.GAP_SEVERITIES], na.rm = TRUE)

      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        bslib::value_box(
          title = "Reserved",
          value = n_gap,
          theme = "danger",
          shiny::span(class = "small", "Results this run will not produce")
        ),
        bslib::value_box(
          title = "To review",
          value = sum(counts["WARN"], na.rm = TRUE),
          theme = "warning",
          shiny::span(class = "small", "Results may not be what the shell asks")
        ),
        bslib::value_box(
          title = "Notes",
          value = sum(counts["INFO"], na.rm = TRUE),
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
        shown <- findings[, setdiff(names(findings), "ref"), drop = FALSE]
        ## An id alone ("F_14_3_1") is not what the sidebar calls the output
        ## ("Mean (+/- SE) Pulse Rate Over Time by Treatment"), so the same
        ## finding read as two unrelated things depending on where you saw
        ## it. Carry the name the rest of the app uses, next to the id.
        shown <- .with_finding_names(shown, state$model())

        DT::datatable(
          shown,
          rownames = FALSE,
          selection = "single",
          options = list(pageLength = 20, scrollX = TRUE),
          caption = shiny::tags$caption(
            style = "caption-side: top; padding: 0 0 .5rem 0;",
            class = "small text-muted",
            "Select a row to open the entity it is about."
          )
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
        parts <- .split_variable_ref(row$detail)
        state$add_request(.add_request(
          output_id  = row$id,
          dataset    = parts$dataset,
          variable   = parts$variable,
          annotation = row$detail
        ))
        return()
      }

      if (!row$entity %in% names(.pool_registry())) return()
      model <- state$model()
      if (!row$id %in% model[[row$entity]]$id) return()

      state$selected(list(pool = row$entity, id = row$id))

      ## Setting the selection is not enough to feel like navigation: the
      ## panel it drives is on another tab, so from here the click looked
      ## like nothing happened. Go to the tab that actually shows this
      ## entity -- Entities for the shared pools, which brings its row with
      ## it, and Details for an output or an analysis.
      if (row$entity %in% .library_pools()) {
        state$entity_request(list(pool = row$entity, id = row$id))
      } else if (!is.null(parent_session)) {
        bslib::nav_select("main_tabs", "Details", session = parent_session)
      }
    })
  })
}

## The pools whose entities live in the Entities tab. Everything else an
## output or an analysis names is reached through the sidebar and the Details
## tab, which is why a finding cannot simply always jump to Entities.
#' @noRd
.library_pools <- function() {
  c("methods", "analysis_sets", "data_subsets", "groupings")
}

## Put the entity's own name beside its id, in the column order a reviewer
## reads: what it is, then which one, then what is wrong.
##
## The name is whatever the rest of the app calls it -- `label` where there is
## one, else `name` -- so the validation table, the sidebar and the detail
## header all say the same words about the same thing. A finding whose id no
## longer resolves keeps an empty name rather than dropping the row: the
## finding is still true, and saying nothing is better than guessing.
#' @noRd
.with_finding_names <- function(findings, model) {
  if (nrow(findings) == 0 || !all(c("entity", "id") %in% names(findings))) {
    return(findings)
  }

  findings$name <- vapply(seq_len(nrow(findings)), function(i) {
    pool <- findings$entity[[i]]
    if (is.na(pool) || !pool %in% names(model)) return(NA_character_)
    df <- model[[pool]]
    if (!is.data.frame(df) || !"id" %in% names(df)) return(NA_character_)

    index <- match(findings$id[[i]], df$id)
    if (is.na(index)) return(NA_character_)
    for (field in c("label", "name")) {
      if (field %in% names(df)) {
        value <- .chr_field(df[[field]][[index]])
        if (!is.na(value) && nzchar(value)) return(value)
      }
    }
    NA_character_
  }, character(1))

  lead <- intersect(c("severity", "entity", "id", "name"), names(findings))
  findings[, c(lead, setdiff(names(findings), lead)), drop = FALSE]
}

## A gap is the one finding that names something that should exist but does
## not, so it is the one the app can act on directly.
#' @noRd
## A gap finding says the shell annotated a line no analysis covers, and it
## carries the variable the shell named -- enough to offer "add that analysis,
## pre-filled". Keyed on the code rather than on the entity/field pair it used
## to be inferred from, so a future finding on the same entity cannot be
## mistaken for one.
.is_gap_finding <- function(row) {
  identical(row$ref, "SHELL_LINE_NOT_ANALYSED") && !is.na(row$detail)
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
