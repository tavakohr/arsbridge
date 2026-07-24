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

        output[[table_id]] <- DT::renderDT(
          {
            DT::datatable(
              .library_table(state$model(), this_pool),
              rownames = FALSE,
              selection = "single",
              options = list(pageLength = 15, scrollX = TRUE)
            )
          },
          server = TRUE
        )

        output[[detail_id]] <- shiny::renderUI({
          selected <- input[[paste0(table_id, "_rows_selected")]]
          if (length(selected) == 0) {
            return(shiny::div(
              class = "text-muted small mt-2",
              "Select a row to see the full definition."
            ))
          }

          model <- state$model()
          row <- model[[this_pool]][selected, , drop = FALSE]
          .entity_detail_ui(row, this_pool, model, state)
        })
      })
    }
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
