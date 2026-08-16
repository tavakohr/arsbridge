## arsbridge -- column_tree.R
## ---------------------------------------------------------------------------
## The column tree is the normalized internal model of a table's result
## columns. It is built from the raw header-cell geometry the parser captures
## (rows, grid spans, vertical merges) BEFORE that geometry is flattened into
## one label per column, so a two-level header like
##
##   ROOT
##   |- Cohort A            (spans its child columns)
##   |  |- Mild / Moderate / Severe
##   |  `- Total            (subtotal: the PARENT's condition)
##   |- Cohort B            (level-1 leaf, no children)
##   `- Total               (grand total: the analysis set)
##
## keeps its parent-child structure instead of collapsing into a flat label
## list. Each visible result column becomes one explicit PATH with a composed
## condition, and downstream stages emit exactly the declared paths -- never
## a Cartesian product of the grouping variables.
##
## Nothing here is study-specific: nodes, levels, and conditions all come
## from the shell's own header geometry and annotations.

#' Convert captured header-cell geometry into a column tree.
#'
#' `grid` is the list of header cell records the parser captures in
#' `.populate_table()`: one entry per physical header cell with `row`,
#' `col_start`, `col_end`, `text` (annotation-stripped label), `annotation`,
#' and `vmerge_continue`. Returns a column tree: `list(mode, nodes)`.
#'
#' A cell in row r becomes a child of the nearest node above it whose column
#' range contains the cell's range. A cell vertically merged down through the
#' sub-header rows therefore stays a level-1 node, which is exactly how a
#' no-children sibling column ("Cohort B") sits beside a spanning parent.
#'
#' @noRd
.header_grid_to_tree <- function(grid) {
  empty <- list(mode = "FLAT", nodes = list())
  if (length(grid %||% list()) == 0) return(empty)

  ## The stub column is whatever the first cell of row 1 covers; result
  ## columns start after it.
  row1 <- Filter(function(cell) cell$row == 1L, grid)
  if (length(row1) == 0) return(empty)
  stub_end <- 0L
  for (cell in row1) {
    if (cell$col_start == 1L) stub_end <- cell$col_end
  }

  nodes <- list()
  for (cell in grid) {
    if (isTRUE(cell$vmerge_continue)) next
    if (cell$col_start <= stub_end) next
    label_blank <- !nzchar(trimws(cell$text %||% ""))
    annot_blank <- !nzchar(cell$annotation %||% "")
    if (label_blank && annot_blank) next

    ## Parent: nearest node in an earlier row whose span contains this cell.
    parent_id <- NULL
    parent_level <- 0L
    for (candidate in rev(nodes)) {
      if (candidate$source$header_row >= cell$row) next
      contains <- candidate$source$col_start <= cell$col_start &&
        candidate$source$col_end >= cell$col_end
      spans <- candidate$source$col_end > candidate$source$col_start
      if (contains && spans) {
        parent_id    <- candidate$id
        parent_level <- candidate$level
        break
      }
    }

    label  <- .strip_n_placeholder(cell$text %||% "")
    n_hint <- .parse_n_hint(cell$text %||% "")

    ## A header whose condition could not be read declares NO usable condition,
    ## so `condition` stays NULL and every structural decision below -- whether
    ## the axis is a hierarchy, what each path composes to -- treats it as
    ## unconditioned, which it is. The author's text travels separately so the
    ## grouping built from this node carries the marker and validation can
    ## reserve on it. Storing the unreadable object in `condition` instead
    ## would let it be composed into a result path as though it selected
    ## records.
    parsed <- parse_where_clause(cell$annotation %||% "")
    unreadable <- .is_unresolved_condition(parsed)

    nodes[[length(nodes) + 1L]] <- list(
      id           = sprintf("CT%02d", length(nodes) + 1L),
      label        = label,
      level        = parent_level + 1L,
      order        = 0L,   ## assigned once siblings are known
      parent_id    = parent_id,
      node_type    = "leaf",
      grouping_ref = .annotation_first_ref(cell$annotation %||% ""),
      annotation   = cell$annotation %||% "",
      condition    = if (unreadable) NULL else parsed,
      unresolved_condition = if (unreadable) {
        .unresolved_condition_text(parsed)
      } else {
        NULL
      },
      n_hint       = n_hint,
      source       = list(
        header_row = cell$row,
        col_start  = cell$col_start,
        col_end    = cell$col_end
      )
    )
  }
  if (length(nodes) == 0) return(empty)

  ## Sibling display order by column position.
  parent_key <- vapply(nodes, function(n) n$parent_id %||% "", character(1))
  for (key in unique(parent_key)) {
    idx    <- which(parent_key == key)
    starts <- vapply(nodes[idx], function(n) n$source$col_start, integer(1))
    ranks  <- rank(starts, ties.method = "first")
    for (j in seq_along(idx)) nodes[[idx[j]]]$order <- as.integer(ranks[j])
  }

  ## Node types. A node with children is a group; a "Total"-labelled child is
  ## a subtotal; a "Total"-labelled top-level node with no children is the
  ## grand total. Everything else is a detail leaf.
  ids_with_children <- unique(parent_key[nzchar(parent_key)])
  for (i in seq_along(nodes)) {
    node <- nodes[[i]]
    is_total_label <- grepl("(?i)^total\\b", node$label, perl = TRUE)
    if (node$id %in% ids_with_children) {
      nodes[[i]]$node_type <- "group"
    } else if (is_total_label && !is.null(node$parent_id)) {
      nodes[[i]]$node_type <- "subtotal"
    } else if (is_total_label && is.null(node$parent_id)) {
      nodes[[i]]$node_type <- "grand_total"
    }
  }

  tree <- list(mode = "FLAT", nodes = nodes)
  tree$mode <- .detect_grouping_mode(tree)
  tree
}

#' Classify the grouping structure of a column tree.
#'
#' The decision is condition-driven, not purely geometric: a spanned header
#' whose sub-columns are statistic labels ("n" / "(%)") has depth 2 but no
#' conditioned child level, and stays FLAT so the existing single-axis
#' pipeline handles it untouched. Real hierarchy means conditions at two or
#' more levels. It is ASYMMETRIC when the hierarchy is uneven -- a top-level
#' leaf sits beside a group, or a group carries a subtotal column -- because
#' a plain cross of the level variables would then invent columns the shell
#' does not have.
#'
#' @noRd
.detect_grouping_mode <- function(tree) {
  nodes <- tree$nodes %||% list()
  if (length(nodes) == 0) return("FLAT")

  has_cond   <- vapply(nodes, function(n) !is.null(n$condition), logical(1))
  levels     <- vapply(nodes, function(n) n$level, integer(1))
  types      <- vapply(nodes, function(n) n$node_type, character(1))

  ## A hierarchy needs conditioned child columns AND a top level that
  ## declares the axis. A top-level node declares it either by carrying a
  ## condition of its own ("Cohort A [ADSL.COHGRPN=1]") or by being a GROUP
  ## over conditioned children ("Alpha Cohort [ADSL.COHORTN]" spanning
  ## Low / Medium / High) -- naming the variable and letting the children
  ## carry the values is just as common, and treating it as flat collapses
  ## an unambiguous two-level header into a row of undifferentiated columns.
  deep_conditions <- any(has_cond & levels >= 2L)
  top_declares    <- any((has_cond | types == "group") & levels == 1L)
  if (!deep_conditions || !top_declares) return("FLAT")

  has_subtotal  <- any(types == "subtotal")
  top_is_group  <- types[levels == 1L] == "group"
  mixed_depth   <- any(top_is_group) && !all(top_is_group | types[levels == 1L] == "grand_total")

  if (has_subtotal || mixed_depth) return("ASYMMETRIC_NESTED")
  "NESTED"
}

#' The declared result-group paths of a column tree, in display order.
#'
#' One path per visible result column (leaf, subtotal, or grand total),
#' depth-first down the tree. Each path carries its composed condition:
#'
#' - a detail leaf is AND(ancestor conditions..., own condition),
#' - a subtotal is AND(ancestor conditions) ONLY -- the parent's condition by
#'   construction, so its N may exceed the sum of the displayed children,
#' - a grand total has no condition (the analysis set alone scopes it).
#'
#' @noRd
column_tree_paths <- function(tree) {
  nodes <- tree$nodes %||% list()
  if (length(nodes) == 0) return(list())

  by_id <- stats::setNames(nodes, vapply(nodes, function(n) n$id, character(1)))
  children_of <- function(id) {
    kids <- Filter(function(n) identical(n$parent_id, id), nodes)
    kids[order(vapply(kids, function(n) n$order, integer(1)))]
  }

  paths <- list()
  emit <- function(node, ancestors) {
    if (identical(node$node_type, "group")) {
      for (kid in children_of(node$id)) emit(kid, c(ancestors, list(node)))
      return(invisible(NULL))
    }

    ancestor_conditions <- lapply(ancestors, function(a) a$condition)
    condition <- switch(
      node$node_type,
      subtotal    = do.call(combine_conditions, ancestor_conditions),
      grand_total = NULL,
      do.call(combine_conditions, c(ancestor_conditions, list(node$condition)))
    )
    role <- switch(node$node_type,
                   subtotal    = "SUBTOTAL",
                   grand_total = "GRAND_TOTAL",
                   "DETAIL")
    total_strategy <- switch(node$node_type,
                             subtotal    = "condition_based",
                             grand_total = "analysis_set",
                             NULL)

    node_ids <- c(vapply(ancestors, function(a) a$id, character(1)), node$id)
    paths[[length(paths) + 1L]] <<- list(
      path_id        = node$id,
      order          = length(paths) + 1L,
      node_ids       = node_ids,
      label_path     = c(vapply(ancestors, function(a) a$label, character(1)),
                         node$label),
      role           = role,
      condition      = condition,
      total_strategy = total_strategy,
      source         = node$source
    )
    invisible(NULL)
  }

  top <- Filter(function(n) is.null(n$parent_id), nodes)
  top <- top[order(vapply(top, function(n) n$order, integer(1)))]
  for (node in top) emit(node, list())
  paths
}

#' The visible result columns of the tree (everything that is not a spanning
#' group header), in display order.
#' @noRd
column_tree_leaves <- function(tree) {
  paths <- column_tree_paths(tree)
  lapply(paths, function(p) tree$nodes[[match(p$path_id, vapply(tree$nodes, function(n) n$id, character(1)))]])
}

#' Cheap structural invariants of a column tree. Returns a character vector
#' of problems, empty when the tree is sound.
#' @noRd
.validate_column_tree <- function(tree) {
  nodes <- tree$nodes %||% list()
  problems <- character(0)
  if (length(nodes) == 0) return(problems)

  ids <- vapply(nodes, function(n) n$id, character(1))
  if (anyDuplicated(ids)) {
    problems <- c(problems, "duplicate node ids")
  }
  for (node in nodes) {
    if (!is.null(node$parent_id) && !node$parent_id %in% ids) {
      problems <- c(problems,
                    sprintf("node %s references missing parent %s",
                            node$id, node$parent_id))
    }
    if (!node$node_type %in% c("group", "leaf", "subtotal", "grand_total")) {
      problems <- c(problems,
                    sprintf("node %s has unknown type %s", node$id, node$node_type))
    }
  }

  ## Every leaf-level path must be unique as a condition, otherwise two
  ## display columns would compute the same thing.
  paths <- column_tree_paths(tree)
  if (length(paths) >= 2) {
    canon <- vapply(paths, function(p) {
      paste(deparse(canonicalize_condition(p$condition)), collapse = "")
    }, character(1))
    if (anyDuplicated(canon)) {
      problems <- c(problems, "two result paths share the same composed condition")
    }
  }
  problems
}

#' Strip a trailing "(N=...)" placeholder from a header label.
#' @noRd
.strip_n_placeholder <- function(label) {
  out <- sub("\\s*\\(\\s*[Nn]\\s*=\\s*[^)]*\\)\\s*$", "", label)
  trimws(gsub("\\s+", " ", out))
}

#' Parse a numeric N from a "(N=40)" header placeholder; NA when the shell
#' uses "(N=XX)" or carries no placeholder.
#' @noRd
.parse_n_hint <- function(label) {
  m <- regmatches(label, regexec("\\(\\s*[Nn]\\s*=\\s*(\\d+)\\s*\\)", label))[[1]]
  if (length(m) == 2) as.integer(m[2]) else NA_integer_
}

#' First DATASET.VARIABLE reference in an annotation, uppercased; "" when the
#' annotation carries none.
#' @noRd
.annotation_first_ref <- function(annotation) {
  refs <- extract_annotation_vars(annotation)
  if (length(refs) > 0) toupper(refs[[1]]) else ""
}
