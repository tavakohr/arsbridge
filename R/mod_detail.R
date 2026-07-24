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
    output$detail <- shiny::renderUI({
      selected <- state$selected()
      if (is.null(selected)) {
        return(shiny::div(
          class = "text-muted",
          "Select an output or an analysis on the left."
        ))
      }

      model <- state$model()
      df <- model[[selected$pool]]
      index <- match(selected$id, df$id)

      if (is.na(index)) {
        return(shiny::div(
          class = "text-warning",
          paste0(selected$id, " is no longer in this reporting event.")
        ))
      }

      row <- df[index, , drop = FALSE]
      if (identical(selected$pool, "analyses")) {
        .analysis_detail_ui(row, model, state)
      } else {
        .output_detail_ui(row, model, state)
      }
    })
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
.output_detail_ui <- function(row, model, state) {
  analysis_ids <- .split_values(row$referenced_analysis_ids)
  analyses <- model$analyses[model$analyses$id %in% analysis_ids, , drop = FALSE]
  findings <- state$findings()
  own <- findings[findings$id == row$id, , drop = FALSE]

  node <- row$raw[[1]]
  display <- (node[["displays"]] %||% list())[[1]]
  columns <- vapply(
    display[["columns"]] %||% list(),
    function(column) .chr_field(column[["label"]]),
    character(1)
  )
  footnotes <- unlist(lapply(
    display[["displaySections"]] %||% list(),
    function(section) {
      vapply(section[["subSections"]] %||% list(),
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
    shiny::h6(class = "mt-3", "Analyses in this output"),
    DT::datatable(
      analyses[, c("id", "label", "dataset", "variable", "methodId"),
               drop = FALSE],
      rownames = FALSE,
      selection = "none",
      options = list(dom = "tp", pageLength = 10, scrollX = TRUE)
    )
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
