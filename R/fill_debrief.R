## The fill debrief: the census of every cell the fill touched, the rollups
## that answer "which column lost everything, and why", and the hint each
## pending reason carries.
##
## This exists for the locked-down machine: the person staring at a half
## empty workbook cannot send it anywhere, so the app has to hold the whole
## diagnosis -- what happened, where, and what an author can do about it.

#' The census frame: one row per cell record, filled cells included.
#'
#' The fill writer's records carry more than the old unfilled-only frame
#' kept: the position, the owning analysis, the column's display label.
#' Keeping all of it is what makes per-column fill rates computable at all
#' -- a rate needs a denominator, and the denominator is the filled cells.
#' @noRd
.fill_census <- function(records) {
  chr <- function(record, field) {
    as.character(record[[field]] %||% NA_character_)
  }
  int <- function(record, field) {
    value <- record[[field]]
    if (is.null(value) || length(value) == 0 || is.na(value[[1]])) {
      return(NA_integer_)
    }
    as.integer(value[[1]])
  }
  data.frame(
    output_id   = vapply(records, chr, character(1), "output_id"),
    sheet       = vapply(records, chr, character(1), "sheet"),
    ref         = vapply(records, chr, character(1), "ref"),
    row         = vapply(records, int, integer(1), "row"),
    col         = vapply(records, int, integer(1), "col"),
    col_label   = vapply(records, chr, character(1), "col_label"),
    analysis_id = vapply(records, chr, character(1), "analysis_id"),
    status      = vapply(records, chr, character(1), "status"),
    reason      = vapply(records, chr, character(1), "reason"),
    stringsAsFactors = FALSE
  )
}

#' Roll a fill census up into the three tables a reader actually asks for.
#'
#' @param census The `census` frame `ars_fill_shell()` returns: one row per
#'   cell record, filled cells included.
#' @return A list of three data frames:
#'   \describe{
#'     \item{`sheets`}{One row per sheet: cell counts by status.}
#'     \item{`columns`}{One row per display column: how many of its cells
#'       filled, and -- when some did not -- the reason most of them share.}
#'     \item{`reasons`}{One row per distinct reason across the run, with the
#'       author-facing hint for it.}
#'   }
#' @export
ars_fill_summary <- function(census) {
  empty <- function(...) {
    data.frame(..., stringsAsFactors = FALSE)[0, , drop = FALSE]
  }
  if (is.null(census) || nrow(census) == 0) {
    return(list(
      sheets  = empty(sheet = character(), cells = integer(),
                      filled = integer(), partial = integer(),
                      pending = integer(), skipped = integer()),
      columns = empty(sheet = character(), col = integer(),
                      col_label = character(), cells = integer(),
                      filled = integer(), modal_reason = character()),
      reasons = empty(reason = character(), n_cells = integer(),
                      n_sheets = integer(), hint = character())
    ))
  }

  count_by <- function(rows, statuses) {
    sum(census$status[rows] %in% statuses)
  }

  sheets <- do.call(rbind, lapply(unique(census$sheet), function(sheet) {
    rows <- which(census$sheet %in% sheet)
    data.frame(
      sheet   = sheet,
      cells   = length(rows),
      filled  = count_by(rows, "filled"),
      partial = count_by(rows, "partial"),
      pending = count_by(rows, "pending"),
      skipped = count_by(rows, "skipped"),
      stringsAsFactors = FALSE
    )
  }))

  ## Columns: only cells that sit in a display column (row-less records and
  ## stub-column records carry no col_label).
  on_axis <- which(!is.na(census$col_label))
  column_keys <- unique(census[on_axis, c("sheet", "col", "col_label")])
  columns <- do.call(rbind, lapply(seq_len(nrow(column_keys)), function(i) {
    key <- column_keys[i, ]
    rows <- which(census$sheet %in% key$sheet & !is.na(census$col) &
                    census$col %in% key$col)
    unfilled <- rows[!census$status[rows] %in% c("filled", "partial")]
    modal <- if (length(unfilled) == 0) NA_character_ else {
      names(sort(table(census$reason[unfilled]), decreasing = TRUE))[1]
    }
    data.frame(
      sheet = key$sheet, col = key$col, col_label = key$col_label,
      cells = length(rows),
      filled = count_by(rows, c("filled", "partial")),
      modal_reason = modal %||% NA_character_,
      stringsAsFactors = FALSE
    )
  }))
  if (is.null(columns)) {
    columns <- empty(sheet = character(), col = integer(),
                     col_label = character(), cells = integer(),
                     filled = integer(), modal_reason = character())
  }

  explained <- which(!is.na(census$reason))
  reason_names <- unique(census$reason[explained])
  reasons <- do.call(rbind, lapply(reason_names, function(reason) {
    rows <- explained[census$reason[explained] %in% reason]
    data.frame(
      reason   = reason,
      n_cells  = length(rows),
      n_sheets = length(unique(census$sheet[rows])),
      hint     = .fill_reason_hint(reason),
      stringsAsFactors = FALSE
    )
  }))
  if (is.null(reasons)) {
    reasons <- empty(reason = character(), n_cells = integer(),
                     n_sheets = integer(), hint = character())
  }
  reasons <- reasons[order(reasons$n_cells, decreasing = TRUE), ,
                     drop = FALSE]

  list(sheets = sheets, columns = columns, reasons = reasons)
}

## One author-facing hint per reason the fill can report. The coverage test
## in test-fill_debrief.R holds the contract: a new reason string must ship
## with a hint, or the test names it.
.FILL_REASON_HINTS <- list(
  "a template row of the categorical block above -- awaiting row expansion" =
    "Nothing to fix: the block is recognised and expands at fill time. If it stayed on placeholder, read the block's own FAIL/WARN in the diagnostics.",
  "the column is not on the output's column axis" =
    "The column header's annotation must name a variable the ADaM spec has ([columns -> DATASET.VARIABLE]), and the column labels must match that variable's values. The build stage logged a WARN naming the header it could not resolve.",
  "no analysis covers this row" =
    "The stub row's annotation never bound to an analysis: check it names DATASET.VARIABLE the spec knows, or add the row through the supplement.",
  "the method declares no statistic for this placeholder" =
    "The row's method produces no statistic for this cell: the annotation typed the row as something else. Annotate the row as what the placeholder shows (a count, a count with percentage, a summary).",
  "not bound to an analysis" =
    "The cell reached the writer without an analysis behind it: the row's annotation never resolved. Check the stub row's annotation against the ADaM spec.",
  "the cell is not in the workbook" =
    "The cell map points at a cell the sheet does not have -- the shell changed after the ARS was built. Rebuild from the current shell.",
  "the cell holds no text to replace" =
    "The mapped cell is empty in the workbook: the placeholder moved or was deleted. Rebuild from the current shell.",
  "reserved for manual derivation" =
    "Intentional: the method is beyond the deterministic engine, and the cell is reserved. See ars_manual_worklist() for the full list.",
  "the placeholder asks for a statistic the analysis does not produce" =
    "The shell shows more numbers than the row's method computes -- most often 'xx (xx.x)' on a count-only row. Annotate the row so it reads as a count with percentage, or simplify the placeholder.",
  "the analysis produced no value for this cell" =
    "The analysis ran and has no result for this column/level: usually the column label does not match any value of the column variable in the data.",
  "the row stands for a repeated block, which needs row expansion" =
    "The row's analysis draws a whole distribution into one cell. Author the block as a template (header annotated with the bare variable over mock rows), or update to a version with categorical row expansion.",
  "no result in the ARD for this cell" =
    "The ARD has no row for this cell's analysis at all: the analysis was skipped or failed at execution. Check the execution diagnostics for its id.",
  "the placeholder text could not be located in the cell" =
    "The cell's text changed after the ARS was built, so the placeholder pattern is gone. Rebuild from the current shell.",
  "no result in this column is shown as a percentage" =
    "The (N=xx) header fills from the denominator of the column's percentage results, and this column shows none. Expected for count-only columns.",
  "no analysis in this column reports a denominator" =
    "No percentage in this column carried a denominator, so there is no N to write. Check the column's analyses declare a population.",
  "the (N=xx) placeholder could not be located in the cell" =
    "The header cell's text changed after the ARS was built. Rebuild from the current shell.",
  "the nested block's analyses produced no levels" =
    "The nested pair ran but the data has no rows in scope -- check the section's record filter and the population.",
  "the categorical block's analysis produced no levels" =
    "The block's analysis ran but produced no category levels -- check the variable is populated in scope, and the spec codelist if one exists.",
  "the block has fewer observed levels than authored template rows; the leftover row was cleared" =
    "Nothing to fix: the shell authored more mock rows than the data has levels, and the spare row was cleared on purpose.",
  "the shell has no template row to expand" =
    "A listing needs one body row of placeholders to use as its template. Add one row of column placeholders under the header.",
  "the listing's rows could not be read from the data" =
    "The listing's source dataset could not be read -- check the ADaM directory holds it and the file opens.",
  "the listing selected no rows" =
    "The listing's filter matched nothing. Check the record filter against the data.",
  "the template row is not in the workbook" =
    "The listing's template row is outside the sheet -- the shell changed after the ARS was built. Rebuild from the current shell.",
  "the shell states no annotation block to write the series into" =
    "A figure sheet needs the annotation block that declares its series. Re-annotate the figure sheet.",
  "no ADaM directory was given, so the series could not be computed" =
    "Figures compute their series straight from the data: pass the ADaM directory to the build.",
  "the shell does not name both an x axis and a y axis variable" =
    "The figure's annotation must name both axes (x and y variables). Complete the annotation.",
  "the figure's filter could not be parsed" =
    "The figure's record filter is not in a supported form -- see the WARN in the diagnostics for the supported grammar.",
  "the figure's filter selected no records" =
    "The figure's filter matched nothing. Check it against the data.",
  "no cell map recorded for this output" =
    "The output carries no cell map -- it was built from a Word shell or an older ARS file. Rebuild from the Excel shell."
)

## Parameterised reason families: the leading text is stable, the tail names
## the offender. Matched by prefix or pattern after the exact lookup misses.
.FILL_REASON_HINT_FAMILIES <- list(
  list(match = function(reason) {
    startsWith(reason, "the column's analyses report different denominators")
  }, hint = "Two analyses in one column disagree about the population N, so the header cannot pick one. Align the analyses' populations, or split the columns."),
  list(match = function(reason) {
    startsWith(reason, "the sheet carries")
  }, hint = "The sheet holds features (formulas, filters, validations) that cannot survive row insertion, so the block declined to expand. Remove them from the shell, or fill this table by hand."),
  list(match = function(reason) {
    startsWith(reason, "the output names sheet")
  }, hint = "The ARS was built from a different workbook than the one being filled. Rebuild from the current shell."),
  list(match = function(reason) {
    startsWith(reason, "dataset ") && grepl("ADaM directory", reason)
  }, hint = "The figure's source dataset is not in the ADaM directory. Add the file, or correct the annotation's dataset."),
  list(match = function(reason) {
    grepl(" does not have ", reason, fixed = TRUE)
  }, hint = "The annotation names a variable its dataset does not carry. Correct the annotation, or the ADaM spec.")
)

#' The author-facing hint for one pending reason, or NA when the reason is
#' not one the fill writes.
#' @noRd
.fill_reason_hint <- function(reason) {
  reason <- as.character(reason %||% NA_character_)
  if (is.na(reason) || !nzchar(reason)) return(NA_character_)
  exact <- .FILL_REASON_HINTS[[reason]]
  if (!is.null(exact)) return(exact)
  for (family in .FILL_REASON_HINT_FAMILIES) {
    if (isTRUE(family$match(reason))) return(family$hint)
  }
  NA_character_
}
