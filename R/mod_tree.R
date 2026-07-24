## arsbridge -- mod_tree.R
## ---------------------------------------------------------------------------
## Output/analysis navigation: the shell's structure, expressed in the
## standard. Each output is a collapsible panel; each analysis beneath it is a
## clickable line carrying its worst validation finding as a badge, so the
## places needing attention are visible before anything is opened.
##
## Every analysis link writes to ONE delegated input rather than getting its
## own observer, so a few hundred lines cost nothing and testServer() can
## drive selection directly.

## The tree's content as a plain data frame, so the shape can be tested
## without a browser.
#' @noRd
.tree_data <- function(model, findings = NULL) {
  outputs <- model$outputs
  if (nrow(outputs) == 0) {
    return(data.frame(
      output_id    = character(0),
      output_label = character(0),
      analysis_id  = character(0),
      analysis_label = character(0),
      badge        = character(0),
      stringsAsFactors = FALSE
    ))
  }

  analyses <- model$analyses

  ## Findings reach the tree by the entity they name: an analysis finding
  ## badges its line, and anything else badges the output it belongs to.
  severity_for <- function(ids) {
    if (is.null(findings) || nrow(findings) == 0) return(NA_character_)
    .worst_severity(findings$severity[findings$id %in% ids])
  }

  rows <- list()
  for (i in seq_len(nrow(outputs))) {
    output_id <- outputs$id[i]
    label <- outputs$label[i]
    if (is.na(label) || !nzchar(label)) label <- outputs$name[i]

    analysis_ids <- .split_values(outputs$referenced_analysis_ids[i])

    if (length(analysis_ids) == 0) {
      rows[[length(rows) + 1L]] <- data.frame(
        output_id      = output_id,
        output_label   = label,
        analysis_id    = NA_character_,
        analysis_label = NA_character_,
        badge          = severity_for(output_id),
        stringsAsFactors = FALSE
      )
      next
    }

    for (analysis_id in analysis_ids) {
      index <- match(analysis_id, analyses$id)
      analysis_label <- if (is.na(index)) {
        analysis_id
      } else {
        value <- analyses$label[index]
        if (is.na(value) || !nzchar(value)) analyses$id[index] else value
      }

      rows[[length(rows) + 1L]] <- data.frame(
        output_id      = output_id,
        output_label   = label,
        analysis_id    = analysis_id,
        analysis_label = analysis_label,
        badge          = severity_for(analysis_id),
        stringsAsFactors = FALSE
      )
    }
  }

  do.call(rbind, rows)
}

## The badge an output panel shows: the worst of its own findings and those of
## every analysis beneath it.
#' @noRd
.output_badge <- function(tree, output_id, findings) {
  rows <- tree[tree$output_id == output_id, , drop = FALSE]
  severities <- rows$badge[!is.na(rows$badge)]

  if (!is.null(findings) && nrow(findings) > 0) {
    severities <- c(severities, findings$severity[findings$id == output_id])
  }
  .worst_severity(severities)
}

## Case-insensitive substring match across everything a reviewer might type:
## output number, title, analysis label or id.
#' @noRd
.tree_filter <- function(tree, pattern) {
  if (is.null(pattern) || !nzchar(trimws(pattern))) return(tree)

  haystack <- paste(
    tree$output_id, tree$output_label,
    tree$analysis_id, tree$analysis_label
  )
  matched <- grepl(trimws(pattern), haystack, ignore.case = TRUE, fixed = FALSE)

  ## Keep whole outputs whose title matched, so a search for a table number
  ## still shows its lines.
  output_matched <- grepl(
    trimws(pattern),
    paste(tree$output_id, tree$output_label),
    ignore.case = TRUE
  )
  tree[matched | output_matched, , drop = FALSE]
}


#' @noRd
mod_tree_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::textInput(
      ns("filter"), label = NULL,
      placeholder = "Filter outputs and analyses"
    ),
    shiny::uiOutput(ns("tree"))
  )
}

#' @noRd
mod_tree_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    tree <- shiny::reactive({
      .tree_data(state$model(), state$findings())
    })

    visible <- shiny::reactive({
      .tree_filter(tree(), input$filter)
    })

    output$tree <- shiny::renderUI({
      rows <- visible()
      if (nrow(rows) == 0) {
        return(shiny::div(class = "text-muted small p-2",
                          "Nothing matches that filter."))
      }

      findings <- state$findings()
      panels <- lapply(unique(rows$output_id), function(output_id) {
        output_rows <- rows[rows$output_id == output_id, , drop = FALSE]
        label <- output_rows$output_label[1]

        links <- lapply(seq_len(nrow(output_rows)), function(i) {
          analysis_id <- output_rows$analysis_id[i]
          if (is.na(analysis_id)) {
            return(shiny::div(class = "text-muted small",
                              "No analyses in this output."))
          }

          shiny::div(
            class = "d-flex justify-content-between align-items-center py-1",
            shiny::tags$a(
              href = "#",
              class = "link-body-emphasis text-decoration-none small",
              onclick = .select_js(ns("selected"), "analyses", analysis_id),
              output_rows$analysis_label[i]
            ),
            .severity_badge(output_rows$badge[i])
          )
        })

        bslib::accordion_panel(
          value = output_id,
          title = shiny::div(
            class = "d-flex justify-content-between align-items-center w-100",
            shiny::tags$a(
              href = "#",
              class = "link-body-emphasis text-decoration-none fw-semibold",
              onclick = .select_js(ns("selected"), "outputs", output_id),
              label
            ),
            .severity_badge(.output_badge(tree(), output_id, findings))
          ),
          links
        )
      })

      do.call(
        bslib::accordion,
        c(panels, list(id = ns("accordion"), open = FALSE, multiple = TRUE))
      )
    })

    ## One delegated input for every link in the tree.
    shiny::observeEvent(input$selected, {
      state$selected(list(
        pool = input$selected$pool,
        id   = input$selected$id
      ))
    })
  })
}

## Selecting the same node twice must re-fire (a reviewer clicking back to
## where they were), hence the event priority.
#' @noRd
.select_js <- function(input_id, pool, id) {
  paste0(
    "Shiny.setInputValue('", input_id, "', ",
    "{pool: '", pool, "', id: '", id, "'}, ",
    "{priority: 'event'}); return false;"
  )
}
