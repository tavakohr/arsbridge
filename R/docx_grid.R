## arsbridge -- docx_grid.R
## ---------------------------------------------------------------------------
## Grid-first table model. A Word table is not a rectangle: gridSpan makes a
## row carry fewer <w:tc> nodes than the table has columns, and vMerge makes
## a cell's lower half show up as a ghost cell in the next row. Everything
## that reasons about "which column is this" must therefore expand the table
## into its physical R x C occupancy grid first -- guessing from raw cell
## indices is how a spanned data row misaligns against the header.
##
## .table_grid() does that expansion once per table. Each grid position
## records the <w:tc> that occupies it, which physical columns/rows that
## cell covers, and whether the position is the cell's anchor or covered
## space (a gridSpan tail or a vMerge continuation).

#' Number of physical columns of a `<w:tbl>`, from its `<w:tblGrid>`
#' declaration. Falls back to the widest row (sum of spans) when the
#' declaration is missing or empty -- Word always writes it, but a
#' hand-built or repaired document may not.
#' @noRd
.grid_n_cols <- function(tbl_node) {
  declared <- length(xml2::xml_find_all(
    tbl_node, "./*[local-name()='tblGrid']/*[local-name()='gridCol']"))
  if (declared > 0L) return(declared)

  rows <- xml2::xml_find_all(tbl_node, "./*[local-name()='tr']")
  widest <- 0L
  for (row in rows) {
    cells <- xml2::xml_find_all(row, "./*[local-name()='tc']")
    width <- sum(vapply(cells, .grid_span, integer(1)))
    widest <- max(widest, width)
  }
  widest
}

#' The physical grid column each cell of a row is anchored at, accounting
#' for gridSpan: cell i starts where the spans of cells 1..i-1 end.
#' @noRd
.cell_anchor_cols <- function(cells) {
  cols <- integer(length(cells))
  col <- 1L
  for (i in seq_along(cells)) {
    cols[[i]] <- col
    col <- col + .grid_span(cells[[i]])
  }
  cols
}

#' Expand a `<w:tbl>` into its R x C occupancy grid.
#'
#' Returns `list(n_rows, n_cols, rows)`. `rows[[r]][[c]]` describes physical
#' position (r, c):
#'
#'   kind        "cell"    the anchor position of a real cell
#'               "span"    covered by a gridSpan cell anchored to the left
#'               "vmerge"  covered by a vMerge cell anchored above
#'               "missing" the row is ragged and never reaches this column
#'   node        the `<w:tc>` occupying the position (the anchor's node for
#'               "span"/"vmerge"; NULL for "missing")
#'   anchor_row, the position of the anchor cell whose content this
#'   anchor_col  position displays (itself, for kind = "cell")
#'
#' A vMerge continuation inherits its anchor from the position directly
#' above, so a chain of continuations all point at the top cell. The stub
#' column is column 1 of this grid by definition; a ghost row is one whose
#' position (r, 1) has kind "vmerge".
#' @noRd
.table_grid <- function(tbl_node) {
  row_nodes <- xml2::xml_find_all(tbl_node, "./*[local-name()='tr']")
  n_rows <- length(row_nodes)
  n_cols <- .grid_n_cols(tbl_node)
  if (n_rows == 0L || n_cols == 0L) {
    return(list(n_rows = n_rows, n_cols = n_cols, rows = list()))
  }

  rows <- vector("list", n_rows)
  for (r in seq_len(n_rows)) {
    positions <- vector("list", n_cols)
    cells <- xml2::xml_find_all(row_nodes[[r]], "./*[local-name()='tc']")
    col <- 1L
    for (cell in cells) {
      span <- .grid_span(cell)
      continues_up <- .is_vmerge_continuation(cell) && r > 1L

      for (k in seq_len(span)) {
        pos <- col + k - 1L
        if (pos > n_cols) break   ## overwide row: extra cells are dropped
        entry <- if (continues_up) {
          ## Inherit the anchor from directly above, so continuation chains
          ## resolve to the top cell in one hop.
          above <- rows[[r - 1L]][[pos]]
          list(kind = "vmerge", node = cell,
               anchor_row = above$anchor_row %||% r,
               anchor_col = above$anchor_col %||% pos)
        } else if (k == 1L) {
          list(kind = "cell", node = cell, anchor_row = r, anchor_col = col)
        } else {
          list(kind = "span", node = cell, anchor_row = r, anchor_col = col)
        }
        positions[[pos]] <- entry
      }
      col <- col + span
    }
    ## Ragged row: pad columns the row never reaches.
    for (pos in seq_len(n_cols)) {
      if (is.null(positions[[pos]])) {
        positions[[pos]] <- list(kind = "missing", node = NULL,
                                 anchor_row = r, anchor_col = pos)
      }
    }
    rows[[r]] <- positions
  }

  list(n_rows = n_rows, n_cols = n_cols, rows = rows)
}

#' Text at a grid position: the anchor cell's text for a real or covered
#' position, "" for a missing one.
#' @noRd
.grid_text <- function(grid, r, c) {
  entry <- grid$rows[[r]][[c]]
  if (is.null(entry$node)) return("")
  anchor <- grid$rows[[entry$anchor_row]][[entry$anchor_col]]
  if (is.null(anchor$node)) return("")
  .cell_text(anchor$node)
}

#' TRUE when grid row `r` is a vMerge ghost row: its stub position (column
#' 1) is the continuation half of a vertically merged cell above.
#' @noRd
.grid_row_is_ghost <- function(grid, r) {
  identical(grid$rows[[r]][[1L]]$kind, "vmerge")
}
