## helper-gridgen.R -------------------------------------------------------------
## Builds synthetic worksheet objects for the xlsx layout tests -- the Excel
## analogue of helper-rowgen.R.
##
## xlsx_grid.R reads nothing but the cell tables xlsx_cells.R returns, so a
## layout test does not need a workbook on disk: it needs a sheet with an
## exact geometry. Generating the object directly is what makes the toggle
## sweep practical -- a test can build every combination of (population line,
## column headers, footnote, leading offset, group rows, spacer rows) and
## assert the banner is still located, which is where the layout bugs live.
## The hand-built fixtures each reproduce ONE shape; the combinations are the
## interesting part.
##
## The shape produced here has to stay identical to the real reader's output,
## or these tests pass against a fiction. test-xlsx_grid.R asserts that
## directly against a fixture read through xlsx_read_shell_cells().

.gridgen_col_letter <- function(col) {
  out <- character()
  n <- as.integer(col)
  while (n > 0L) {
    out <- c(LETTERS[((n - 1L) %% 26L) + 1L], out)
    n <- (n - 1L) %/% 26L
  }
  paste(out, collapse = "")
}

#' One seam-1 run.
gridgen_run <- function(text, color = "000000", italic = FALSE) {
  list(text = text, raw_text = text, color_hex = color,
       highlight = NA_character_, bold = FALSE, italic = italic,
       underline = FALSE, strike = FALSE)
}

#' One cell.
#'
#' @param annot Annotation text. When given, the cell gets TWO runs -- a black
#'   label and a red annotation after a newline -- the way an annotated stub
#'   is authored.
#' @param red Whole-cell red instead: a single styled run, the way a figure
#'   sheet annotates.
#' @param kind "string" or "number".
gridgen_cell <- function(row, col, text, annot = NULL, red = FALSE,
                         kind = "string") {
  full <- if (is.null(annot)) text else paste0(text, "\n", annot)
  runs <- if (!is.null(annot)) {
    list(gridgen_run(text),
         gridgen_run(paste0("\n", annot), color = "C00000", italic = TRUE))
  } else if (red) {
    list(gridgen_run(text, color = "C00000", italic = TRUE))
  } else {
    list(gridgen_run(text))
  }
  list(row = as.integer(row), col = as.integer(col),
       ref = paste0(.gridgen_col_letter(col), as.integer(row)),
       kind = kind, text = full,
       cell_color = if (red) "C00000" else NA_character_,
       cell_bold = FALSE, cell_italic = red,
       runs = runs)
}

#' Assemble cells (a list from gridgen_cell) and merge refs into a sheet
#' object shaped exactly like one entry of `xlsx_read_shell_cells()$sheets`.
gridgen_sheet <- function(name, cells, merges = character(), sheet_id = 1L) {
  df <- data.frame(
    row        = vapply(cells, function(c) c$row, integer(1)),
    col        = vapply(cells, function(c) c$col, integer(1)),
    ref        = vapply(cells, function(c) c$ref, character(1)),
    kind       = vapply(cells, function(c) c$kind, character(1)),
    text       = vapply(cells, function(c) c$text, character(1)),
    cell_color = vapply(cells, function(c) c$cell_color, character(1)),
    cell_bold  = vapply(cells, function(c) c$cell_bold, logical(1)),
    cell_italic = vapply(cells, function(c) c$cell_italic, logical(1)),
    stringsAsFactors = FALSE)
  df$runs <- lapply(cells, function(c) c$runs)
  df <- df[order(df$row, df$col), , drop = FALSE]

  merge_df <- data.frame(ref = character(), row_start = integer(),
                         row_end = integer(), col_start = integer(),
                         col_end = integer(), stringsAsFactors = FALSE)
  for (ref in merges) {
    halves <- strsplit(ref, ":", fixed = TRUE)[[1]]
    from <- .xlsx_a1_to_rc(halves[[1]])
    to   <- .xlsx_a1_to_rc(halves[[2]])
    merge_df <- rbind(merge_df, data.frame(
      ref = ref, row_start = from[["row"]], row_end = to[["row"]],
      col_start = from[["col"]], col_end = to[["col"]],
      stringsAsFactors = FALSE))
  }

  list(name = name, sheet_id = as.integer(sheet_id), order = 1L,
       part = "xl/worksheets/sheet1.xml", dimension = NA_character_,
       n_rows = if (nrow(df)) max(df$row) else 0L,
       n_cols = if (nrow(df)) max(df$col) else 0L,
       cells = df, merges = merge_df)
}

#' A conventional TLF sheet, with each part of the convention independently
#' switchable. This is the toggle matrix: the defaults produce the canonical
#' rows 1-4 banner, and every argument removes or displaces one part of it.
#'
#' @param offset Blank rows inserted ABOVE the banner (a logo row, say), so
#'   the whole convention shifts down.
#' @param population,header,footnote Include that part of the banner/body.
#' @param groups Add parent rows with no result columns.
#' @param spacers Leave gaps between body rows, as a real shell does.
#' @param n_cols Number of displayed columns (stub plus results).
gridgen_tlf <- function(name = "Table 14.1.1", offset = 0L,
                        population = TRUE, header = TRUE, footnote = TRUE,
                        groups = FALSE, spacers = FALSE, n_cols = 4L,
                        placeholder = "xx (xx.x)") {
  cells  <- list()
  merges <- character()
  r <- offset

  banner <- function(text, annot = NULL) {
    r <<- r + 1L
    cells[[length(cells) + 1L]] <<- gridgen_cell(r, 1L, text, annot = annot)
    merges <<- c(merges, sprintf("A%d:%s%d", r,
                                 .gridgen_col_letter(n_cols), r))
  }

  banner(name)
  banner(paste("Summary of", sub("^\\w+\\s+", "", name)))
  if (population) banner("Safety Population", annot = "(ADSL.SAFFL='Y')")

  if (header) {
    r <- r + 1L
    cells[[length(cells) + 1L]] <-
      gridgen_cell(r, 1L, "Category", annot = "[columns -> ADSL.TRT01A]")
    for (j in seq_len(n_cols - 1L) + 1L) {
      cells[[length(cells) + 1L]] <-
        gridgen_cell(r, j, paste("Arm", j - 1L))
    }
  }

  data_row <- function(label, annot = NULL) {
    r <<- r + 1L
    cells[[length(cells) + 1L]] <<- gridgen_cell(r, 1L, label, annot = annot)
    for (j in seq_len(n_cols - 1L) + 1L) {
      cells[[length(cells) + 1L]] <<- gridgen_cell(r, j, placeholder)
    }
  }

  if (groups) {
    r <- r + 1L
    cells[[length(cells) + 1L]] <-
      gridgen_cell(r, 1L, "Age group, n (%)", annot = "[ADSL.AGEGR1]")
  }
  data_row("Subjects treated", annot = "[ADSL.SAFFL = 'Y']")
  if (spacers) r <- r + 1L
  data_row("Completed study", annot = "[ADSL.EOSSTT = 'COMPLETED']")

  if (footnote) {
    r <- r + 1L
    cells[[length(cells) + 1L]] <-
      gridgen_cell(r, 1L, "Percentages are based on treated subjects.")
    merges <- c(merges, sprintf("A%d:%s%d", r,
                                .gridgen_col_letter(n_cols), r))
  }

  gridgen_sheet(name, cells, merges)
}

#' A figure sheet: banner, a black key/value specification block, then the red
#' arrow-prose annotation block.
gridgen_figure <- function(name = "Figure 14.3.1",
                           directives = c("X axis -> ADVS.AVISITN",
                                          "Y axis -> mean of ADVS.AVAL",
                                          "Series (colour) -> ADVS.TRTA"),
                           source_line = "Source: ADVS.",
                           heading = TRUE) {
  cells  <- list()
  merges <- character()
  add <- function(row, col, text, annot = NULL, red = FALSE) {
    cells[[length(cells) + 1L]] <<-
      gridgen_cell(row, col, text, annot = annot, red = red)
  }

  add(1L, 1L, name);                       merges <- c(merges, "A1:B1")
  add(2L, 1L, "Mean Pulse Rate Over Time"); merges <- c(merges, "A2:B2")
  add(3L, 1L, "Safety Population", annot = "(ADSL.SAFFL='Y')")
  merges <- c(merges, "A3:B3")

  ## Black specification block -- display material for the human reader, not
  ## instructions. Row 6 is deliberately written in the SAME arrow form as a
  ## real directive: only its colour says it is not one.
  add(5L, 1L, "Chart type")
  add(5L, 2L, "Line plot with mean points")
  add(6L, 1L, "Chart type -> stacked bar")

  r <- 8L
  if (heading) {
    add(r, 1L, "Programming annotations", red = TRUE)
    merges <- c(merges, sprintf("A%d:B%d", r, r))
    r <- r + 1L
  }
  if (!is.null(source_line)) {
    add(r, 1L, source_line, red = TRUE)
    merges <- c(merges, sprintf("A%d:B%d", r, r))
    r <- r + 1L
  }
  for (d in directives) {
    add(r, 1L, d, red = TRUE)
    merges <- c(merges, sprintf("A%d:B%d", r, r))
    r <- r + 1L
  }
  gridgen_sheet(name, cells, merges)
}
