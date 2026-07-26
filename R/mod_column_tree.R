## arsbridge -- mod_column_tree.R
## ---------------------------------------------------------------------------
## The column-tree review panel: the parsed header hierarchy, the declared
## result-group paths, and the structural warnings, side by side. This is
## where a reviewer confirms the one judgment a machine cannot make alone --
## a subtotal's scope (parent condition vs sum of displayed children) -- and
## corrects a path's role or total strategy when the shell was ambiguous.

## The validation codes this panel surfaces (subset of .check_result_paths).
#' @noRd
.PATH_FINDING_CODES <- c(
  "HEADER_TREE_MISSING", "DISPLAY_COLUMN_COUNT_MISMATCH",
  "UNMAPPED_LEAF_COLUMN", "INVALID_CARTESIAN_PRODUCT",
  "GROUPING_VARIABLE_NOT_LINKED", "SUBTOTAL_SCOPE_UNDEFINED",
  "DUPLICATE_RESULT_PATH", "GROUPING_ORDER_AMBIGUOUS",
  "SUBTOTAL_EXCLUDES_UNDISPLAYED_CATEGORIES"
)

#' @noRd
mod_column_tree_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("chooser")),
    shiny::uiOutput(ns("warnings")),
    shiny::uiOutput(ns("tree")),
    DT::DTOutput(ns("paths")),
    shiny::uiOutput(ns("path_controls"))
  )
}

#' @noRd
mod_column_tree_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Outputs that carry a column tree or declared paths.
    tree_outputs <- shiny::reactive({
      df <- state$model()$outputs
      keep <- vapply(seq_len(nrow(df)), function(i) {
        node <- df$raw[[i]]
        !is.null(node[["resultGroupPaths"]]) ||
          !is.null(node[["_meta"]][["column_tree"]])
      }, logical(1))
      df[keep, , drop = FALSE]
    })

    selected_output <- shiny::reactive({
      df <- tree_outputs()
      if (nrow(df) == 0) return(NULL)
      wanted <- input$output %||% df$id[1]
      idx <- match(wanted, df$id)
      if (is.na(idx)) idx <- 1L
      df$raw[[idx]]
    })

    output$chooser <- shiny::renderUI({
      df <- tree_outputs()
      if (nrow(df) == 0) {
        return(shiny::div(
          class = "text-muted",
          "No output in this reporting event declares a hierarchical column tree."
        ))
      }
      labels <- ifelse(is.na(df$label) | !nzchar(df$label), df$id, df$label)
      shiny::selectInput(ns("output"), "Output",
                         choices = stats::setNames(as.list(df$id), labels),
                         selected = input$output %||% df$id[1])
    })

    ## --- the parsed header tree, indented ---
    output$tree <- shiny::renderUI({
      node <- selected_output()
      if (is.null(node)) return(NULL)
      tree <- node[["_meta"]][["column_tree"]]
      if (is.null(tree)) {
        return(shiny::div(class = "text-muted small mb-2",
                          "No parsed header tree travelled with this output."))
      }

      badge_class <- c(group = "text-bg-primary", leaf = "text-bg-secondary",
                       subtotal = "text-bg-warning", grand_total = "text-bg-info")
      rows <- lapply(tree[["nodes"]] %||% list(), function(n) {
        ntype  <- .chr_field(n[["nodeType"]]) %||% "leaf"
        indent <- (as.integer(.chr_field(n[["level"]]) %||% "1") - 1L) * 24L
        hint   <- .chr_field(n[["nHint"]])
        annot  <- .chr_field(n[["annotation"]])
        shiny::div(
          style = sprintf("margin-left: %dpx;", indent),
          class = "py-1",
          shiny::span(class = paste("badge me-2",
                                    badge_class[[ntype]] %||% "text-bg-secondary"),
                      gsub("_", " ", ntype)),
          shiny::strong(.chr_field(n[["label"]]) %||% ""),
          if (!is.na(hint)) shiny::span(class = "text-muted small ms-2",
                                        paste0("(N=", hint, ")")),
          if (!is.na(annot) && nzchar(annot))
            shiny::code(class = "small ms-2", annot)
        )
      })
      shiny::div(
        class = "border rounded p-2 mb-3",
        shiny::div(class = "text-muted small mb-1",
                   paste("Header tree --", .chr_field(tree[["mode"]]) %||% "")),
        rows
      )
    })

    ## --- the declared paths, one row per display column ---
    paths_df <- shiny::reactive({
      node <- selected_output()
      if (is.null(node)) return(NULL)
      paths <- node[["resultGroupPaths"]][["paths"]] %||% list()
      if (length(paths) == 0) return(NULL)
      data.frame(
        order    = vapply(paths, function(p) as.integer(.chr_field(p[["order"]]) %||% NA), integer(1)),
        path_id  = vapply(paths, function(p) .chr_field(p[["pathId"]]) %||% "", character(1)),
        column   = vapply(paths, function(p)
          paste(unlist(p[["labelPath"]] %||% list()), collapse = " > "), character(1)),
        role     = vapply(paths, function(p) .chr_field(p[["role"]]) %||% "DETAIL", character(1)),
        total_strategy = vapply(paths, function(p)
          .chr_field(p[["totalStrategy"]]) %||% "", character(1)),
        groups   = vapply(paths, function(p)
          paste(unlist(p[["groupIds"]] %||% list()), collapse = ", "), character(1)),
        stringsAsFactors = FALSE
      )
    })

    output$paths <- DT::renderDT({
      df <- paths_df()
      if (is.null(df)) return(NULL)
      DT::datatable(
        df[, c("order", "column", "role", "total_strategy", "groups")],
        rownames = FALSE,
        selection = "single",
        options = list(pageLength = 12, dom = "t", ordering = FALSE)
      )
    }, server = TRUE)

    selected_path <- shiny::reactive({
      df <- paths_df()
      idx <- input$paths_rows_selected
      if (is.null(df) || is.null(idx) || length(idx) == 0) return(NULL)
      df[idx, , drop = FALSE]
    })

    ## --- per-path controls (edit mode): role and total strategy ---
    output$path_controls <- shiny::renderUI({
      if (!identical(state$mode, "edit")) return(NULL)
      row <- selected_path()
      if (is.null(row)) {
        return(shiny::div(class = "text-muted small mt-2",
                          "Select a path to adjust its role or total scope."))
      }
      shiny::div(
        class = "border rounded p-2 mt-2",
        shiny::div(class = "small fw-bold mb-2", row$column),
        bslib::layout_columns(
          col_widths = c(6, 6),
          shiny::selectInput(
            ns("path_role"), "Role",
            choices = c("DETAIL", "SUBTOTAL", "GRAND_TOTAL"),
            selected = row$role
          ),
          shiny::selectInput(
            ns("path_total_strategy"), "Total strategy",
            choices = c("(none)" = "", "condition_based", "analysis_set"),
            selected = row$total_strategy
          )
        ),
        shiny::div(
          class = "text-muted small",
          "A condition-based subtotal is scoped by its PARENT's condition and includes subjects whose child category is unknown; an analysis-set total spans the whole population."
        )
      )
    })

    apply_path_edit <- function(field, value) {
      row <- selected_path()
      node <- selected_output()
      if (is.null(row) || is.null(node)) return(invisible(FALSE))
      output_id <- .chr_field(node[["id"]])

      current <- switch(field,
                        role          = row$role,
                        totalStrategy = row$total_strategy)
      if (identical(as.character(current), as.character(value))) {
        return(invisible(FALSE))
      }

      .push_history(state)
      updated <- model_set_result_path(state$model(), output_id,
                                       row$path_id, field, value)
      state$model(updated)
      state$edit_log(rbind(state$edit_log(), data.frame(
        time  = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        pool  = "outputs",
        id    = output_id,
        field = paste0("resultGroupPaths/", row$path_id, "/", field),
        old   = if (!nzchar(current %||% "")) "(not set)" else as.character(current),
        new   = if (!nzchar(value %||% "")) "(not set)" else as.character(value),
        stringsAsFactors = FALSE
      )))
      state$findings(validate_ars_model(updated, state$spec, state$report))
      .write_autosave(state)
      invisible(TRUE)
    }

    shiny::observeEvent(input$path_role, {
      apply_path_edit("role", input$path_role)
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$path_total_strategy, {
      apply_path_edit("totalStrategy", input$path_total_strategy)
    }, ignoreInit = TRUE)

    ## --- structural warnings for this output ---
    output$warnings <- shiny::renderUI({
      node <- selected_output()
      if (is.null(node)) return(NULL)
      output_id <- .chr_field(node[["id"]])
      findings <- state$findings()
      related <- findings[
        !is.na(findings$ref) & findings$ref %in% .PATH_FINDING_CODES &
          (findings$id == output_id | findings$entity == "analyses"), ,
        drop = FALSE
      ]
      if (nrow(related) == 0) {
        return(shiny::div(
          class = "alert alert-success py-2",
          "The declared paths match the shell header: every column has exactly one path."
        ))
      }
      shiny::div(
        class = "alert alert-warning py-2",
        shiny::tags$ul(
          class = "mb-0 small",
          lapply(seq_len(nrow(related)), function(i) {
            shiny::tags$li(shiny::strong(related$ref[i]), " -- ",
                           related$problem[i])
          })
        )
      )
    })
  })
}
