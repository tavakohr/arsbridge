## arsbridge -- xlsx_grid.R
## ---------------------------------------------------------------------------
## Sheet-layout semantics: what the cells of a worksheet MEAN. xlsx_cells.R
## says where the text and the red runs are; this file decides which sheet is
## an output, which rows are its banner, which are its body, which cell is a
## placeholder waiting for a number, and what a figure sheet's arrow prose
## declares. It reads nothing but the cell tables from xlsx_cells.R, so it can
## be exercised without a workbook on disk.
##
## The convention these shells follow is rigid, which is the entire reason for
## moving to Excel: one worksheet per output, named by the output number, and
##
##   row 1   output number      ("Table 14.1.1"), merged across the width
##   row 2   title                                merged across the width
##   row 3   population line                      merged across the width
##   row 4   column headers      (tables and listings; a figure has none)
##   row 5+  body, with blank spacer rows between logical groups
##   last    footnote, merged across the width -- when there is one
##
## Rigid, but not trusted blindly. Every locator here takes the convention as
## its first guess and falls back to a tolerant scan, because a shell that has
## had a row inserted at the top is a shell a user still expects to parse. A
## deviation from the convention is reported as a diagnostic and the parse
## continues; only a sheet with no recognisable output number at all is
## skipped.
##
## Nothing here emits ARS. The section object is assembled in
## parse_shell_xlsx.R from what these functions return.

## ---------------------------------------------------------------------------
## Row and cell access
## ---------------------------------------------------------------------------

#' The populated row numbers of a sheet, ascending. A blank spacer row is
#' absent from the file, so this is a sparse sequence with gaps -- the gaps
#' being exactly the group separators the shell author typed.
#' @noRd
.sheet_rows <- function(sheet) {
  if (nrow(sheet$cells) == 0) return(integer(0))
  sort(unique(sheet$cells$row))
}

#' The cells of one sheet row, ordered by column.
#' @noRd
.sheet_row_cells <- function(sheet, row) {
  hit <- sheet$cells[sheet$cells$row == row, , drop = FALSE]
  hit[order(hit$col), , drop = FALSE]
}

#' The text of one cell, "" when the cell is not populated.
#' @noRd
.sheet_cell_text <- function(sheet, row, col) {
  hit <- sheet$cells[sheet$cells$row == row & sheet$cells$col == col, ,
                     drop = FALSE]
  if (nrow(hit) == 0) return("")
  trimws(hit$text[[1]])
}

#' The columns of a row that carry non-empty text.
#' @noRd
.sheet_row_filled_cols <- function(sheet, row) {
  cells <- .sheet_row_cells(sheet, row)
  if (nrow(cells) == 0) return(integer(0))
  cells$col[nzchar(trimws(cells$text))]
}

#' TRUE when the row's populated content is one cell merged across the full
#' width of the sheet -- how the convention writes the number, the title, the
#' population line, and the footnote. This is the signal that separates a
#' footnote from a group-header row: both put their text in column 1 alone,
#' but only the footnote is merged across.
#' @noRd
.sheet_row_is_banner_merge <- function(sheet, row, n_cols = NULL) {
  n_cols <- n_cols %||% sheet$n_cols
  if (is.null(n_cols) || n_cols < 2L) return(FALSE)
  m <- sheet$merges
  if (nrow(m) == 0) return(FALSE)
  any(m$row_start == row & m$row_end == row &
        m$col_start == 1L & m$col_end >= n_cols)
}

#' The last column a merge starting at (row, col) covers; `col` itself when
#' the cell is not merged. Gives a header cell its column span.
#' @noRd
.sheet_merge_col_end <- function(sheet, row, col) {
  m <- sheet$merges
  if (nrow(m) == 0) return(col)
  hit <- m[m$row_start <= row & m$row_end >= row & m$col_start == col, ,
           drop = FALSE]
  if (nrow(hit) == 0) return(col)
  max(hit$col_end)
}

## ---------------------------------------------------------------------------
## Sheet classification
## ---------------------------------------------------------------------------

## A sheet that documents the workbook rather than carrying an output. These
## shells ship a "Formatting Notes" legend; the same sheet turns up as
## "Notes", "Legend", "Read me", or a table of contents. Recognised and
## skipped WITHOUT a warning -- it is a legitimate part of the workbook, not a
## sheet that failed to parse.
.XLSX_NOTES_NAME_RE <- paste0(
  "(?i)^\\s*(?:formatting\\s+notes|notes?|legend|key|instructions?|",
  "read\\s*me|conventions?|about|cover(?:\\s+page)?|toc|",
  "table\\s+of\\s+contents|contents|index|definitions?)\\s*$")

#' Decide what one worksheet is.
#'
#' The sheet NAME is the primary signal, because the convention is that the
#' name is the output number -- and unlike row 1 it cannot be pushed down by an
#' inserted row. Row 1 is the fallback for a workbook whose tabs were renamed
#' ("Sheet1") but whose content is still an output.
#'
#' @return list(role, tlf_number, tlf_type, heading_text, source). `role` is
#'   "tlf" (an output to parse), "notes" (documentation, skip quietly), or
#'   "unknown" (skip, and say so). `source` records which signal named the
#'   output, so a mismatch between name and row 1 can be reported later.
#' @noRd
.classify_sheet <- function(sheet, heading_patterns = NULL) {
  name <- trimws(as.character(sheet$name %||% ""))

  hit <- .match_tlf_heading(name, heading_patterns)$hit
  if (!is.null(hit)) {
    id <- .tlf_identity(hit)
    return(list(role = "tlf", tlf_number = id$tlf_number,
                tlf_type = id$tlf_type, heading_text = name,
                source = "sheet_name"))
  }

  if (grepl(.XLSX_NOTES_NAME_RE, name, perl = TRUE)) {
    return(list(role = "notes", tlf_number = NA_character_,
                tlf_type = NA_character_, heading_text = name,
                source = "sheet_name"))
  }

  rows <- .sheet_rows(sheet)
  if (length(rows) > 0) {
    first <- .sheet_cell_text(sheet, rows[[1]], 1L)
    hit1  <- .match_tlf_heading(first, heading_patterns)$hit
    if (!is.null(hit1)) {
      id <- .tlf_identity(hit1)
      return(list(role = "tlf", tlf_number = id$tlf_number,
                  tlf_type = id$tlf_type, heading_text = first,
                  source = "row1"))
    }
    if (grepl(.XLSX_NOTES_NAME_RE, first, perl = TRUE)) {
      return(list(role = "notes", tlf_number = NA_character_,
                  tlf_type = NA_character_, heading_text = first,
                  source = "row1"))
    }
  }

  list(role = "unknown", tlf_number = NA_character_, tlf_type = NA_character_,
       heading_text = name, source = NA_character_)
}

## ---------------------------------------------------------------------------
## Banner rows
## ---------------------------------------------------------------------------

## How far down a sheet the banner is still looked for. The convention puts
## the whole banner in rows 1-4; scanning a couple further absorbs an inserted
## logo row or a blank first row without letting a body row be mistaken for a
## title.
.XLSX_BANNER_SCAN_ROWS <- 6L

#' Locate the banner of one output sheet: which row carries the number, the
#' title, the population line, the column headers, and where the body starts.
#'
#' The convention is tried first and, when it holds, nothing else runs. When
#' it does not, each part is searched for by what it IS rather than where it
#' sits: the number row is the one whose text parses as a TLF heading, the
#' population row is the one `.looks_like_population()` accepts, and the
#' header row is the first row after them that fills more than one column.
#' A figure sheet has no column headers at all, so none is looked for.
#'
#' Every field can come back NA -- a shell with no population line is common,
#' and a shell whose header row cannot be found still has a parseable body.
#' Each deviation is reported once against the output.
#'
#' @return list(number_row, title_row, population_row, header_row,
#'   first_body_row, last_row).
#' @noRd
.locate_banner_rows <- function(sheet, tlf_type, tlf_number = NA_character_,
                                heading_patterns = NULL) {
  rows <- .sheet_rows(sheet)
  out <- list(number_row = NA_integer_, title_row = NA_integer_,
              population_row = NA_integer_, header_row = NA_integer_,
              header_rows = integer(0), first_body_row = NA_integer_,
              last_row = if (length(rows)) max(rows) else NA_integer_)
  if (length(rows) == 0) return(out)

  scan <- rows[rows <= .XLSX_BANNER_SCAN_ROWS]
  if (length(scan) == 0) scan <- rows[seq_len(min(2L, length(rows)))]

  ## The number row: the first scanned row whose column-1 text is a heading.
  for (r in scan) {
    if (!is.null(.match_tlf_heading(.sheet_cell_text(sheet, r, 1L),
                                   heading_patterns)$hit)) {
      out$number_row <- r
      break
    }
  }
  if (is.na(out$number_row)) {
    ## The sheet NAME already identified the output, so this is a layout
    ## deviation, not a failure: the body is still read, just without a
    ## row-1 cross-check.
    diag_add(
      stage = "parse_shell", severity = "INFO", input = INPUT_SHELL,
      problem = "Sheet has no output-number row; the sheet name was used instead",
      tlf_number = tlf_number, location = sheet$name %||% "",
      action = paste("Put the output number in row 1 (e.g. 'Table 14.1.1')",
                     "to keep the sheet name and its content cross-checked"))
  }

  after <- function(row) rows[rows > row]
  cursor <- out$number_row
  if (is.na(cursor)) cursor <- 0L

  ## The title: the next populated row that is not itself a population line.
  for (r in after(cursor)) {
    if (r > .XLSX_BANNER_SCAN_ROWS) break
    txt <- .sheet_cell_text(sheet, r, 1L)
    if (!nzchar(txt)) next
    if (.looks_like_population(txt)) break
    out$title_row <- r
    cursor <- r
    break
  }

  ## The population line: the next populated row that reads like one, or a
  ## full-width merged row carrying an annotation (some shells state the
  ## population only as a red "(ADSL.SAFFL='Y')").
  for (r in after(cursor)) {
    if (r > .XLSX_BANNER_SCAN_ROWS) break
    txt <- .sheet_cell_text(sheet, r, 1L)
    if (!nzchar(txt)) next
    styled <- .sheet_row_has_styled_run(sheet, r)
    if (.looks_like_population(txt) ||
        (styled && .sheet_row_is_banner_merge(sheet, r))) {
      out$population_row <- r
      cursor <- r
    }
    break
  }

  ## The column header row -- tables and listings only. A figure sheet's
  ## key/value spec rows fill two columns and would be mistaken for one.
  if (!identical(tlf_type, "FIGURE")) {
    for (r in after(cursor)) {
      if (length(.sheet_row_filled_cols(sheet, r)) >= 2L) {
        out$header_row  <- r
        out$header_rows <- .xlsx_header_row_block(sheet, r, rows)
        cursor <- max(out$header_rows)
        break
      }
    }
    if (is.na(out$header_row)) {
      diag_add(
        stage = "parse_shell", severity = "WARN", input = INPUT_SHELL,
        problem = "No column-header row found; the output has no column axis to read",
        tlf_number = tlf_number, location = sheet$name %||% "",
        action = paste("Give the output a header row with one cell per",
                       "displayed column, under the population line"))
    }
  }

  body <- after(cursor)
  if (length(body) > 0) out$first_body_row <- body[[1]]
  out
}

#' The rows forming the column-header block, starting at `first`.
#'
#' Usually just `first`. A header cell merged across several columns is a
#' GROUP header, though, and the row beneath it carries that group's
#' sub-columns -- the shape `column_tree.R` turns into a hierarchy. That
#' second row is only taken when it looks like labels rather than results: a
#' placeholder anywhere in it means the header ended and the body began.
#'
#' This is the Excel counterpart of the Word reader inferring a multi-row
#' header from a spanned first row, and it is likewise a guess: only evidence
#' in the sheet (a real merge, and no placeholders below it) can extend the
#' block.
#' @noRd
.xlsx_header_row_block <- function(sheet, first, rows) {
  spans_group <- any(
    sheet$merges$row_start == first & sheet$merges$row_end == first &
      sheet$merges$col_start > 1L &
      sheet$merges$col_end > sheet$merges$col_start)
  if (!spans_group) return(first)

  below <- rows[rows > first]
  if (length(below) == 0) return(first)
  candidate <- below[[1]]

  cells <- .sheet_row_cells(sheet, candidate)
  if (nrow(cells) == 0) return(first)
  has_placeholder <- any(vapply(cells$text, function(t) {
    identical(.parse_placeholder(t)$kind, "placeholder")
  }, logical(1)))
  if (has_placeholder) return(first)

  c(first, candidate)
}

#' TRUE when any cell of the row carries an annotation-styled run (the red
#' font these shells annotate with).
#' @noRd
.sheet_row_has_styled_run <- function(sheet, row) {
  cells <- .sheet_row_cells(sheet, row)
  if (nrow(cells) == 0) return(FALSE)
  any(vapply(seq_len(nrow(cells)), function(i) {
    any(vapply(cells$runs[[i]], .is_annotation_styled_run, logical(1)))
  }, logical(1)))
}

## ---------------------------------------------------------------------------
## Header records (seam 2)
## ---------------------------------------------------------------------------

#' Turn the header row(s) of a sheet into seam-2 header-grid records, the same
#' shape `.header_grid()` produces from a Word table -- so `column_tree.R`
#' builds the column hierarchy from an Excel header with no change at all.
#'
#' `row` in each record is the ordinal WITHIN the header block (1 for the
#' first header row), not the sheet row: `.header_grid_to_tree()` reads row 1
#' as the outermost header level. A cell's column span comes from the merge
#' table, so a merged group header spanning three data columns emits one
#' record covering them -- which is what makes a multi-row Excel header feed
#' the nested-tree path unchanged, once a shell uses one.
#'
#' `vmerge_continue` is always FALSE: a vertically merged Excel cell is
#' written once, at its anchor, with no ghost cell below to mark.
#'
#' @param header_rows Sheet row numbers forming the header block, in order.
#' @noRd
.grid_header_records <- function(sheet, header_rows) {
  header_rows <- header_rows[!is.na(header_rows)]
  grid <- list()
  for (i in seq_along(header_rows)) {
    cells <- .sheet_row_cells(sheet, header_rows[[i]])
    for (k in seq_len(nrow(cells))) {
      det <- .detect_annotation(cells$text[[k]], cells$runs[[k]])
      grid[[length(grid) + 1L]] <- list(
        row             = i,
        col_start       = cells$col[[k]],
        col_end         = .sheet_merge_col_end(sheet, header_rows[[i]],
                                               cells$col[[k]]),
        text            = trimws(det$label %||% cells$text[[k]]),
        annotation      = det$annotation %||% "",
        vmerge_continue = FALSE
      )
    }
  }
  grid
}

#' Flatten header-grid records to one label and one annotation per physical
#' column -- what a consumer that only understands a flat column axis needs.
#'
#' A column's label is the distinct non-empty texts down the header block
#' joined with a space ("Drug 10 mg" over "Week 12" reads "Drug 10 mg
#' Week 12"); its annotation is the first non-empty one going down, because
#' the outermost header states the axis and the inner ones refine it. The full
#' hierarchy is not lost -- it travels on the grid to `column_tree.R`.
#'
#' This mirrors `.combine_header_rows_detected()` in parse_shell_docx.R
#' exactly. The two are separate only because the Word version reads XML nodes
#' rather than records; unify them when the Word header path is next touched.
#' @noRd
.xlsx_combine_header_grid <- function(grid) {
  empty <- list(labels = character(0), annotations = character(0))
  if (length(grid) == 0) return(empty)

  width  <- max(vapply(grid, function(g) g$col_end, integer(1)))
  n_rows <- max(vapply(grid, function(g) g$row, integer(1)))
  if (width < 1L || n_rows < 1L) return(empty)

  lab <- matrix("", nrow = n_rows, ncol = width)
  ann <- matrix("", nrow = n_rows, ncol = width)
  for (g in grid) {
    cols <- seq.int(g$col_start, min(g$col_end, width))
    lab[g$row, cols] <- g$text %||% ""
    ann[g$row, cols] <- g$annotation %||% ""
  }

  labels <- vapply(seq_len(width), function(col) {
    parts <- unique(lab[, col][nzchar(lab[, col])])
    trimws(paste(parts, collapse = " "))
  }, character(1))
  annotations <- vapply(seq_len(width), function(col) {
    down <- ann[, col][nzchar(ann[, col])]
    if (length(down) > 0) down[[1]] else ""
  }, character(1))

  list(labels = labels, annotations = annotations)
}

## ---------------------------------------------------------------------------
## Body rows
## ---------------------------------------------------------------------------

#' Classify every body row of a sheet.
#'
#' The distinctions that matter downstream:
#'
#'   "data"       a stub label plus at least one filled result column -- an
#'                analysis row.
#'   "group"      a stub label with NO result columns. In these shells that is
#'                a parent row: "Age group, n (%)" sits above its own levels,
#'                and carries the annotation the levels are grouped by.
#'   "annotation" merged across the sheet width AND carrying a red run -- a
#'                programmer instruction written as its own line, which is how
#'                a figure sheet states its whole specification.
#'   "footnote"   merged across the sheet width with no red run, or opening
#'                with a footnote marker.
#'   "spacer"     present in the file but empty. (A truly blank row is absent
#'                from the file entirely and never reaches here.)
#'
#' Merged-ness is what separates the last three from a group row: a group row
#' fills column 1 only but is NOT merged across, because it heads the columns
#' below it rather than spanning them.
#'
#' `is_template` marks a stub written as a `<...>` placeholder
#' ("<System Organ Class>"), i.e. a row the output repeats per data value
#' rather than a row that exists once.
#'
#' @return data.frame(row, kind, stub_text, n_filled_cols, is_template,
#'   has_annotation).
#' @noRd
.collect_body_rows <- function(sheet, layout) {
  rows <- .sheet_rows(sheet)
  start <- layout$first_body_row
  empty <- data.frame(row = integer(), kind = character(),
                      stub_text = character(), n_filled_cols = integer(),
                      is_template = logical(), has_annotation = logical(),
                      stringsAsFactors = FALSE)
  if (is.na(start)) return(empty)
  body <- rows[rows >= start]
  if (length(body) == 0) return(empty)

  out <- empty
  for (r in body) {
    filled    <- .sheet_row_filled_cols(sheet, r)
    stub      <- .sheet_cell_text(sheet, r, 1L)
    data_cols <- filled[filled > 1L]
    styled    <- .sheet_row_has_styled_run(sheet, r)
    spanning  <- .sheet_row_is_banner_merge(sheet, r)

    kind <- if (length(filled) == 0) {
      "spacer"
    } else if (length(data_cols) > 0) {
      "data"
    } else if (spanning && styled) {
      "annotation"
    } else if (spanning || .looks_like_footnote_lead(stub)) {
      "footnote"
    } else {
      "group"
    }

    out <- rbind(out, data.frame(
      row = r, kind = kind, stub_text = stub,
      n_filled_cols = length(data_cols),
      is_template = .is_template_placeholder(stub),
      has_annotation = styled,
      stringsAsFactors = FALSE))
  }
  out
}

## ---------------------------------------------------------------------------
## Placeholder lexicon
## ---------------------------------------------------------------------------

## The token shapes a shell writes where a number belongs. A cell is a
## placeholder when every token in it comes from this table and nothing else
## in the cell is a word -- which is what lets one rule cover "xx",
## "xx (xx.x)", "xx.x (xx.xx)", "xx, xx" and "dd-mmm / dd-mmm" without
## enumerating the compositions.
##
##   value     a run of x's, optionally with a decimal part. The x's after the
##             dot ARE the decimal-place specification: "xx.x" means one
##             decimal, "xx" means none. This is why the placeholder does not
##             need a separate format declaration anywhere.
##   date      a date mask.
##   template  "<Preferred Term>" -- a row repeated per data value, not a
##             number to compute.
##
## Word boundaries are required so the x of "Max" or "Exposure" is never read
## as a placeholder.
.PLACEHOLDER_TOKENS <- list(
  value    = "\\b[xX]+(?:\\.[xX]+)?\\b",
  date     = paste0("\\b(?:(?i)dd-mmm-yyyy|dd-mmm|ddmmmyyyy|mmm-dd|",
                    "yyyy-mm-dd|dd/mm/yyyy|mm/dd/yyyy|hh:mm(?::ss)?)\\b"),
  template = "<[^<>]+>"
)

#' TRUE for a stub written as a `<...>` template.
#' @noRd
.is_template_placeholder <- function(text) {
  grepl("<[^<>]+>", as.character(text %||% ""), perl = TRUE)
}

#' Read one cell's text as a placeholder specification.
#'
#' @return list(kind, slots, n_slots). `kind` is "placeholder" (at least one
#'   value or date token and no prose), "template" (a `<...>` form),
#'   "literal" (real text -- a stub label, a footnote, a hard-coded value the
#'   fill writer must leave alone), or "empty". Each slot is
#'   list(token, type, start, stop, width, decimals): its character span in
#'   the cell text, so the fill writer can substitute in place and keep the
#'   surrounding punctuation the shell author chose.
#' @noRd
.parse_placeholder <- function(text) {
  text <- as.character(text %||% "")
  none <- list(kind = "empty", slots = list(), n_slots = 0L)
  if (!nzchar(trimws(text))) return(none)

  slots <- list()
  for (type in names(.PLACEHOLDER_TOKENS)) {
    m <- gregexpr(.PLACEHOLDER_TOKENS[[type]], text, perl = TRUE)[[1]]
    if (m[[1]] == -1L) next
    lens <- attr(m, "match.length")
    for (i in seq_along(m)) {
      token <- substr(text, m[[i]], m[[i]] + lens[[i]] - 1L)
      dec <- if (identical(type, "value") && grepl(".", token, fixed = TRUE)) {
        nchar(sub("^.*\\.", "", token))
      } else if (identical(type, "value")) {
        0L
      } else {
        NA_integer_
      }
      slots[[length(slots) + 1L]] <- list(
        token = token, type = type,
        start = m[[i]], stop = m[[i]] + lens[[i]] - 1L,
        width = if (identical(type, "value")) {
          nchar(sub("\\..*$", "", token))
        } else NA_integer_,
        decimals = as.integer(dec)
      )
    }
  }
  if (length(slots) == 0) return(list(kind = "literal", slots = list(),
                                      n_slots = 0L))

  slots <- slots[order(vapply(slots, function(s) s$start, integer(1)))]

  ## Anything left once the tokens are cut out must be punctuation. A single
  ## surviving word means the cell is a label, not a placeholder -- and a
  ## label must never be overwritten with a number.
  remainder <- text
  for (s in rev(slots)) {
    remainder <- paste0(substr(remainder, 1L, s$start - 1L),
                        substr(remainder, s$stop + 1L, nchar(remainder)))
  }
  if (grepl("[[:alnum:]]", remainder)) {
    return(list(kind = "literal", slots = list(), n_slots = 0L))
  }

  kind <- if (all(vapply(slots, function(s) identical(s$type, "template"),
                         logical(1)))) {
    "template"
  } else {
    "placeholder"
  }
  list(kind = kind, slots = slots, n_slots = length(slots))
}

#' Classify every body cell of a sheet as placeholder, template, literal or
#' empty -- the map the fill writer works from, and the evidence the parser
#' uses to tell a result column from a label column.
#'
#' Unknown shapes are not guessed at: a cell whose text is neither a clean
#' placeholder nor obviously prose comes back "literal", which means the fill
#' writer leaves it exactly as the author wrote it.
#'
#' @return data.frame(row, col, ref, kind, n_slots) plus the list-column
#'   `slots`.
#' @noRd
.cell_placeholder_grid <- function(sheet, layout = NULL) {
  cells <- sheet$cells
  if (!is.null(layout) && !is.na(layout$first_body_row)) {
    cells <- cells[cells$row >= layout$first_body_row, , drop = FALSE]
  }
  n <- nrow(cells)
  kind <- character(n); n_slots <- integer(n); slots <- vector("list", n)
  for (i in seq_len(n)) {
    ## A numeric cell is already a value, not a placeholder for one.
    p <- if (identical(cells$kind[[i]], "number")) {
      list(kind = "literal", slots = list(), n_slots = 0L)
    } else {
      .parse_placeholder(cells$text[[i]])
    }
    kind[[i]]    <- p$kind
    n_slots[[i]] <- p$n_slots
    slots[[i]]   <- p$slots
  }
  out <- data.frame(row = cells$row, col = cells$col, ref = cells$ref,
                    kind = kind, n_slots = n_slots, stringsAsFactors = FALSE)
  out$slots <- slots
  out
}

## ---------------------------------------------------------------------------
## Figure sheets
## ---------------------------------------------------------------------------

## A figure has no rows and columns to annotate, so its shell states the plot
## in prose instead: an "X axis -> ADVS.AVISITN" line per aspect, in red,
## under a "Programming annotations" heading. The left side of the arrow names
## the aspect; these are the spellings accepted for each.
.FIGURE_DIRECTIVE_KEYS <- c(
  "x axis"          = "x_axis",
  "x-axis"          = "x_axis",
  "x"               = "x_axis",
  "horizontal axis" = "x_axis",
  "y axis"          = "y_axis",
  "y-axis"          = "y_axis",
  "y"               = "y_axis",
  "vertical axis"   = "y_axis",
  "series"          = "series",
  "series colour"   = "series",
  "series color"    = "series",
  "colour"          = "series",
  "color"           = "series",
  "group"           = "series",
  "by"              = "series",
  "filter"          = "filter",
  "filters"         = "filter",
  "where"           = "filter",
  "population"      = "filter",
  "error bars"      = "error_bars",
  "error bar"       = "error_bars",
  "errorbars"       = "error_bars",
  "chart type"      = "chart_type",
  "plot type"       = "chart_type",
  "graph type"      = "chart_type"
)

## A red line that heads the annotation block rather than declaring anything
## ("Programming annotations"). Skipped without complaint.
.FIGURE_BLOCK_HEADING_RE <-
  "(?i)^\\s*(?:programming\\s+)?(?:annotations?|notes?|specification)\\s*:?\\s*$"

#' Read one "Aspect -> value" figure directive.
#'
#' The left side is normalised before lookup -- lower-cased, parentheses
#' dropped, whitespace collapsed -- so "Series (colour)" and "series color"
#' are the same aspect. An unrecognised aspect comes back with `key = NA`
#' rather than being dropped, so the caller can report it and keep the line.
#'
#' @return list(key, label, value, raw), or NULL when the line has no arrow.
#' @noRd
.parse_arrow_directive <- function(text) {
  text <- trimws(as.character(text %||% ""))
  if (!nzchar(text)) return(NULL)
  ## The arrow is written as an escape, not the character: R CMD check
  ## rejects non-ASCII in code, and a shell author may type either form.
  pos <- regexpr("->|\u2192", text)
  if (pos < 1) return(NULL)

  lhs <- substr(text, 1L, pos - 1L)
  rhs <- trimws(substr(text, pos + attr(pos, "match.length"), nchar(text)))
  norm <- tolower(trimws(gsub("\\s+", " ", gsub("[()]", " ", lhs))))
  norm <- sub("\\s*:\\s*$", "", norm)

  list(
    key   = if (norm %in% names(.FIGURE_DIRECTIVE_KEYS)) {
      unname(.FIGURE_DIRECTIVE_KEYS[[norm]])
    } else NA_character_,
    label = trimws(lhs),
    value = rhs,
    raw   = text
  )
}

#' Read a figure sheet's annotation block.
#'
#' Only the red lines are read: a figure sheet also carries a black key/value
#' specification block and a caption, which are display material for the
#' human, not instructions for the programmer. Of the red lines, a "Source:"
#' line contributes source datasets, an arrow line contributes a directive,
#' and a block heading is skipped. Anything else is reported and kept verbatim
#' so it still reaches the validation report -- a figure aspect arsbridge
#' cannot read must be visible, not silently absent.
#'
#' @return list(directives, source_datasets, unmatched, anchor_row). `anchor_row`
#'   is the first red row of the block, i.e. where the computed series data
#'   goes when the shell is filled.
#' @noRd
.parse_figure_block <- function(sheet, layout, tlf_number = NA_character_) {
  out <- list(directives = list(), source_datasets = character(),
              unmatched = character(), anchor_row = NA_integer_)
  rows <- .sheet_rows(sheet)
  skip <- c(layout$number_row, layout$title_row, layout$population_row)
  rows <- rows[!rows %in% skip[!is.na(skip)]]

  for (r in rows) {
    if (!.sheet_row_has_styled_run(sheet, r)) next
    text <- trimws(paste(.sheet_row_cells(sheet, r)$text, collapse = " "))
    if (!nzchar(text)) next
    if (is.na(out$anchor_row)) out$anchor_row <- r

    if (grepl(.FIGURE_BLOCK_HEADING_RE, text, perl = TRUE)) next

    src <- regmatches(text, regexec(.SOURCE_LINE_RE, text,
                                    ignore.case = TRUE, perl = TRUE))[[1]]
    if (length(src) == 2) {
      out$source_datasets <- unique(c(out$source_datasets,
                                      .split_source_list(src[2])))
      next
    }

    directive <- .parse_arrow_directive(text)
    if (!is.null(directive) && !is.na(directive$key)) {
      ## First statement of an aspect wins; a repeat is reported rather than
      ## silently replacing it.
      if (is.null(out$directives[[directive$key]])) {
        out$directives[[directive$key]] <- directive
      } else {
        diag_add(
          stage = "parse_shell", severity = "WARN", input = INPUT_SHELL,
          problem = sprintf(
            "Figure declares %s more than once; the first statement was kept",
            directive$key),
          tlf_number = tlf_number, location = substr(text, 1, 80),
          action = "Remove the duplicate line so the figure has one statement per aspect")
      }
      next
    }

    out$unmatched <- c(out$unmatched, text)
    diag_add(
      stage = "parse_shell", severity = "INFO", input = INPUT_SHELL,
      problem = paste("Figure annotation line not recognised as a directive;",
                      "kept for review"),
      tlf_number = tlf_number, location = substr(text, 1, 80),
      action = paste("Write it as 'Aspect -> value' (X axis, Y axis, Series,",
                     "Filter, Error bars) if it should drive the figure"))
  }
  out
}
