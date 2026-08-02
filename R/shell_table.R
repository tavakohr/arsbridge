## arsbridge -- shell_table.R
## ---------------------------------------------------------------------------
## The shell-faithful view of one output: the authored row skeleton the
## generator persisted in `_meta.shell_layout`, the column header, the
## population line and the footnotes, assembled into the table a clinical
## reviewer actually thinks in -- a title band, columns, and stub rows with
## `xx.x (x.xx)`-style placeholders.
##
## Everything in this file is pure (no Shiny reactivity), so every shape is
## testable with expect_equal() like .tree_data(). The reactive wiring lives
## in mod_detail.R.

## The methods whose cells read as count-and-percentage in the shell.
.SHELL_COUNT_METHODS <- c("MTH_COUNT_AND_PERCENTAGE", "MTH_AE_FREQUENCY_COUNT",
                          "MTH_SUBJECT_COUNT_PCT")

## What a body cell shows before real numbers exist -- the same placeholder
## conventions the authored shell document uses. `kind` is the row's own kind
## from the layout; `owner_kind` is the kind of the analysis row the line
## belongs to (NA for section headers and spacers that belong to nothing).
#' @noRd
.shell_placeholder <- function(kind, label, method_id = NA_character_,
                               owner_kind = NA_character_) {
  if (kind %in% c("subject_count", "filtered_count")) return("xxx")
  ## The same count, in a shell that also shows what share of the arm it is.
  if (identical(kind, "filtered_count_pct")) return("xx (xx.x%)")

  ## Parent header lines print their label in the stub and nothing in the
  ## body; their numbers live on the sub-rows beneath them.
  if (kind %in% c("continuous", "categorical")) return("")

  ## An authored category slot is filled from its categorical parent.
  if (identical(kind, "level")) return("xx (xx.x%)")

  if (identical(kind, "manual")) return(.MANUAL_MARKER)

  if (identical(kind, "label")) {
    if (is.na(owner_kind)) return("")
    if (identical(owner_kind, "categorical")) return("xx (xx.x%)")
    if (identical(owner_kind, "manual")) return(.MANUAL_MARKER)

    ## A stat line under a continuous parent, keyed on its authored text the
    ## same way the renderer matches it (.norm_label).
    key <- .norm_label(label)
    if (identical(key, "n"))            return("xx")
    if (identical(key, "mean sd"))      return("xx.x (x.xx)")
    if (identical(key, "median"))       return("xx.x")
    if (identical(key, "min max"))      return("(xx.x, xx.x)")
    if (identical(key, "q1 q3"))        return("(xx.x, xx.x)")
    if (identical(key, "median q1 q3")) return("xx.x (xx.x, xx.x)")
    return("xx.x")
  }

  ## Synthesized fallback rows and supplement additions: read the method.
  if (!is.na(method_id) && method_id %in% .SHELL_COUNT_METHODS) {
    return("xx (xx.x%)")
  }
  if (identical(method_id, "MTH_SUBJECT_COUNT")) return("xxx")
  "xx.x"
}

## The <thead> as rows of cell specs {label, colspan, rowspan, hint}. When
## the output carries a parsed `_meta.column_tree`, each tree level becomes a
## header row with colspans from descendant-leaf counts and a stub cell
## spanning every level; otherwise the display's flat column labels make a
## single row. `n_body_cols` counts the data columns after the stub.
#' @noRd
.shell_header_rows <- function(output_raw) {
  display <- .display_node(output_raw)
  flat_labels <- vapply(
    display[["columns"]] %||% list(),
    function(column) .chr_field(column[["label"]]),
    character(1)
  )
  flat_labels <- flat_labels[!is.na(flat_labels)]
  stub_label <- if (length(flat_labels) > 0) flat_labels[1] else ""

  nodes <- output_raw[["_meta"]][["column_tree"]][["nodes"]] %||% list()

  if (length(nodes) == 0) {
    if (length(flat_labels) == 0) {
      return(list(rows = list(), n_body_cols = 0L))
    }
    cells <- lapply(flat_labels, function(label) {
      list(label = label, colspan = 1L, rowspan = 1L, hint = NA_character_)
    })
    return(list(rows = list(cells), n_body_cols = length(flat_labels) - 1L))
  }

  chr_or <- function(x, fallback) {
    value <- .chr_field(x)
    if (is.na(value)) fallback else value
  }
  tree <- data.frame(
    id     = vapply(nodes, function(node)
      chr_or(node[["id"]], ""), character(1)),
    label  = vapply(nodes, function(node)
      chr_or(node[["label"]], ""), character(1)),
    level  = vapply(nodes, function(node)
      as.integer(chr_or(node[["level"]], "1")), integer(1)),
    order  = vapply(nodes, function(node)
      as.integer(chr_or(node[["order"]], "0")), integer(1)),
    parent = vapply(nodes, function(node)
      .chr_field(node[["parentId"]]), character(1)),
    hint   = vapply(nodes, function(node)
      .chr_field(node[["nHint"]]), character(1)),
    stringsAsFactors = FALSE
  )

  n_levels <- max(tree$level)
  parents <- tree$parent[!is.na(tree$parent) & nzchar(tree$parent)]
  is_leaf <- !(tree$id %in% parents)

  ## A node spans as many leaf columns as sit beneath it.
  leaf_count <- function(id) {
    children <- tree$id[!is.na(tree$parent) & tree$parent == id]
    if (length(children) == 0) return(1L)
    sum(vapply(children, leaf_count, integer(1)))
  }

  rows <- lapply(seq_len(n_levels), function(level) {
    at_level <- tree[tree$level == level, , drop = FALSE]
    at_level <- at_level[order(at_level$order), , drop = FALSE]
    lapply(seq_len(nrow(at_level)), function(i) {
      leaf <- !(at_level$id[i] %in% parents)
      list(
        label   = at_level$label[i],
        colspan = leaf_count(at_level$id[i]),
        ## A leaf above the bottom level reaches down through the remaining
        ## header rows.
        rowspan = if (leaf) n_levels - level + 1L else 1L,
        hint    = at_level$hint[i]
      )
    })
  })

  ## The stub column heads the whole header once.
  rows[[1]] <- c(
    list(list(label = stub_label, colspan = 1L, rowspan = n_levels,
              hint = NA_character_)),
    rows[[1]]
  )

  list(rows = rows, n_body_cols = sum(is_leaf))
}

## The population line under the title: the distinct analysis sets the
## output's analyses run on, resolved to their labels. The generator's labels
## already carry the defining condition ("Safety Population (ADSL.SAFFL='Y')"),
## so the condition summary is only appended when the label has none.
#' @noRd
.shell_population <- function(analysis_ids, model) {
  index <- match(analysis_ids, model$analyses$id)
  set_ids <- model$analyses$analysisSetId[index[!is.na(index)]]
  set_ids <- unique(set_ids[!is.na(set_ids) & nzchar(set_ids)])
  if (length(set_ids) == 0) return("")

  labels <- vapply(set_ids, function(id) {
    i <- match(id, model$analysis_sets$id)
    if (is.na(i)) return(id)
    label <- model$analysis_sets$label[i]
    if (is.na(label) || !nzchar(label)) label <- model$analysis_sets$name[i]
    if (is.na(label) || !nzchar(label)) label <- id
    condition <- model$analysis_sets$condition_summary[i]
    if (!is.na(condition) && nzchar(condition) &&
          !grepl("(", label, fixed = TRUE)) {
      label <- paste0(label, " (", condition, ")")
    }
    label
  }, character(1), USE.NAMES = FALSE)

  paste(labels, collapse = "; ")
}

## Everything the shell view needs for one output, from its raw node. The
## rows start from the authored layout (.shell_layout, reused as-is) and gain
## `owner_analysis_id` -- the analysis a label/level row belongs to, mirroring
## the renderer's consumption rule in .tfrmt_prep_ard_layout(): categorical,
## continuous and manual parents own their trailing label rows, level rows
## carry their parent's id, and a label after a scalar row belongs to nothing.
##
## Outputs without a layout (listings, figures, older events) synthesize one
## plain row per referenced analysis with `has_layout = FALSE`, so the same
## view still works and move controls stay on the legacy refs path there --
## correct, because the renderer is refs-driven there too.
#' @noRd
.shell_table_data <- function(output_raw, model) {
  layout <- .shell_layout(output_raw)
  has_layout <- !is.null(layout)
  refs <- unlist(output_raw[["referencedAnalysisIds"]] %||% list())

  if (!has_layout) {
    labels <- vapply(refs, function(id) {
      index <- match(id, model$analyses$id)
      if (is.na(index)) return(id)
      value <- model$analyses$label[index]
      if (is.na(value) || !nzchar(value)) id else value
    }, character(1), USE.NAMES = FALSE)
    layout <- data.frame(
      order       = seq_along(refs),
      label       = labels,
      indent      = rep(0L, length(refs)),
      analysis_id = as.character(refs),
      kind        = rep("row", length(refs)),
      level       = rep(NA_character_, length(refs)),
      stringsAsFactors = FALSE
    )
  }

  n <- nrow(layout)
  owner <- layout$analysis_id
  owner_kind <- rep(NA_character_, n)
  block_owner <- NA_character_
  block_kind <- NA_character_
  for (i in seq_len(n)) {
    if (!is.na(layout$analysis_id[i])) {
      block_owner <- layout$analysis_id[i]
      block_kind <- layout$kind[i]
      owner_kind[i] <- layout$kind[i]
    } else if (identical(layout$kind[i], "label") &&
                 block_kind %in% c("categorical", "continuous", "manual")) {
      owner[i] <- block_owner
      owner_kind[i] <- block_kind
    } else {
      ## A section header or spacer of its own; it also ends the block, since
      ## the renderer never hands it to the analysis above.
      block_owner <- NA_character_
      block_kind <- NA_character_
    }
  }
  layout$owner_analysis_id <- owner
  layout$owner_kind <- owner_kind
  layout$method_id <- model$analyses$methodId[match(owner, model$analyses$id)]
  layout$placeholder <- vapply(seq_len(n), function(i) {
    .shell_placeholder(layout$kind[i], layout$label[i],
                       layout$method_id[i], layout$owner_kind[i])
  }, character(1))

  header <- .shell_header_rows(output_raw)
  display <- .display_node(output_raw)
  title <- .chr_field(display[["displayTitle"]])
  if (is.na(title)) title <- .chr_field(output_raw[["label"]])
  if (is.na(title)) title <- .chr_field(output_raw[["name"]])

  list(
    title       = title,
    population  = .shell_population(refs, model),
    header      = header$rows,
    n_body_cols = header$n_body_cols,
    rows        = layout,
    footnotes   = extract_footnotes(output_raw),
    has_layout  = has_layout
  )
}

## Which row of an output's shell table is the live selection, or NULL. An
## analysis-owned row stays active only while its analysis is still selected
## (otherwise the reviewer has moved on and the click is stale); an authored
## text row stays active while the selection points anywhere in its output.
#' @noRd
.shell_active_row <- function(shell_row, selected, data, output_id) {
  if (is.null(shell_row) || !identical(shell_row$output, output_id)) {
    return(NULL)
  }
  row <- data$rows[data$rows$order == as.integer(shell_row$order), ,
                   drop = FALSE]
  if (nrow(row) == 0) return(NULL)

  if (!is.na(row$owner_analysis_id)) {
    if (is.null(selected) || !identical(selected$pool, "analyses") ||
          !identical(selected$id, row$owner_analysis_id)) {
      return(NULL)
    }
  }
  row
}

## Which output's shell view an analysis selection should keep on screen. A
## shell-row click moves the selection to the owning analysis (so the tree,
## findings and history stay coherent), but the Details panel must keep the
## table visible -- this resolves the clicked row back to its output, or NULL
## when the shell_row no longer matches the selection.
#' @noRd
.shell_redirect_output <- function(shell_row, selected_id, model) {
  if (is.null(shell_row) || is.null(selected_id)) return(NULL)
  index <- match(shell_row$output, model$outputs$id)
  if (is.na(index)) return(NULL)

  data <- .shell_table_data(model$outputs$raw[[index]], model)
  row <- data$rows[data$rows$order == as.integer(shell_row$order), ,
                   drop = FALSE]
  if (nrow(row) == 0) return(NULL)

  if (!is.na(row$owner_analysis_id)) {
    if (identical(row$owner_analysis_id, selected_id)) return(shell_row$output)
    return(NULL)
  }

  refs <- .split_values(model$outputs$referenced_analysis_ids[index])
  if (selected_id %in% refs) shell_row$output else NULL
}

## ---------------------------------------------------------------------------
## Rendering -- plain Bootstrap tag building, the house pattern.

## Sibling of .move_js() / .select_js(): ids and integers only, never label
## text, so nothing needs JS quoting.
#' @noRd
.shell_row_js <- function(input_id, output_id, order) {
  paste0(
    "Shiny.setInputValue('", input_id, "', ",
    "{output: '", output_id, "', order: ", as.integer(order),
    "}, {priority: 'event'});"
  )
}

#' @noRd
.shell_header_js <- function(input_id, output_id) {
  paste0(
    "Shiny.setInputValue('", input_id, "', ",
    "{output: '", output_id, "'}, {priority: 'event'});"
  )
}

## The shell-shaped table: centered title band, muted population line, header
## rows, one clickable row per authored line, footnotes underneath. Row stubs
## indent by the authored indent (rescaled mod_column_tree pattern).
#' @noRd
.shell_table_ui <- function(data, ns, output_id, mode, selected_order = NULL,
                            header_selected = FALSE) {
  if (nrow(data$rows) == 0 && length(data$header) == 0) {
    return(shiny::div(class = "text-muted small",
                      "This output has no rows to lay out."))
  }

  head_rows <- lapply(data$header, function(cells) {
    shiny::tags$tr(
      class = if (isTRUE(header_selected)) "table-active",
      lapply(cells, function(cell) {
        shiny::tags$th(
          colspan = if (cell$colspan > 1L) cell$colspan,
          rowspan = if (cell$rowspan > 1L) cell$rowspan,
          class = "small",
          style = "cursor: pointer;",
          title = "What defines these columns?",
          onclick = .shell_header_js(ns("shell_header"), output_id),
          cell$label,
          if (!is.na(cell$hint)) {
            shiny::span(class = "text-muted fw-normal",
                        paste0(" (N=", cell$hint, ")"))
          }
        )
      })
    )
  })

  body_rows <- lapply(seq_len(nrow(data$rows)), function(i) {
    row <- data$rows[i, , drop = FALSE]
    active <- !is.null(selected_order) &&
      identical(as.integer(selected_order), row$order)

    shiny::tags$tr(
      class = if (active) "table-active",
      style = "cursor: pointer;",
      onclick = .shell_row_js(ns("shell_row"), output_id, row$order),
      shiny::tags$td(
        class = "small",
        style = sprintf("padding-left: %dpx;",
                        8L + row$indent %/% 4L * 24L),
        row$label,
        if (identical(row$kind, "supplement_added")) {
          shiny::span(class = "badge text-bg-info ms-2", "supplement")
        },
        if (row$kind %in% c("nested_parent", "nested_child")) {
          shiny::span(class = "badge text-bg-secondary ms-2", "nested")
        }
      ),
      lapply(seq_len(max(data$n_body_cols, 0L)), function(j) {
        shiny::tags$td(class = "small text-muted text-center", row$placeholder)
      })
    )
  })

  shiny::div(
    if (!is.na(data$title)) {
      shiny::div(class = "text-center fw-semibold", data$title)
    },
    if (nzchar(data$population)) {
      shiny::div(class = "text-center text-muted small mb-2", data$population)
    },
    shiny::div(
      style = "overflow-x: auto;",
      shiny::tags$table(
        class = "table table-sm table-hover shell-table",
        shiny::tags$thead(head_rows),
        shiny::tags$tbody(body_rows)
      )
    ),
    if (length(data$footnotes) > 0) {
      shiny::div(class = "text-muted small",
                 lapply(data$footnotes, shiny::div))
    }
  )
}
