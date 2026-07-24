## arsbridge -- write_validation_report.R
## ---------------------------------------------------------------------------
## Writes the spec-validation report to a styled Excel workbook (openxlsx2).
## Sheet 1 "Validation": annotation-vs-spec cross-reference, rows tinted by
## status (PASS green, WARN amber, FAIL red).
## Sheet 2 "Diagnostics" (when records exist): every pipeline fallback,
## parsing miss, LLM failure, and dropped condition collected by the
## diagnostics collector (R/diagnostics.R), tinted by severity.
## A final "Legend" sheet documents what each tint means.

## Row-tint fill colours, keyed by status / severity. Single source of truth
## for the tinting AND the Legend sheet, so a colour change can never make the
## two disagree.
.REPORT_STATUS_FILL <- c(PASS = "E2EFDA",   # light green
                         WARN = "FFF2CC",   # light amber
                         FAIL = "FCE4D6",   # light red
                         INFO = "DDEBF7")   # light blue

#' Write a validation report data frame to a styled Excel file.
#'
#' @param report_df Data frame from `validate_annotations_spec()`.
#' @param output_path Path to the `.xlsx` to write.
#' @param diagnostics Optional data frame from `diag_records()` -- written to
#'   a "Diagnostics" worksheet when it has rows.
#' @param blockers Optional data frame from `ars_blockers()` -- written as the
#'   FIRST worksheet ("What to fix first") when it has rows, so the user sees
#'   the show-stoppers before anything else.
#'
#' @return Invisibly returns `output_path`.
#'
#' @keywords internal
#' @noRd
write_validation_report <- function(report_df, output_path, diagnostics = NULL,
                                    blockers = NULL) {
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    cli::cli_abort("openxlsx2 is required to write the validation report.")
  }

  wb <- openxlsx2::wb_workbook(creator = "arsbridge")

  ## Show-stoppers first, so the reader fixes the blocking inputs before
  ## wading through the full validation / diagnostics detail.
  if (!is.null(blockers) && nrow(blockers) > 0) {
    wb$add_worksheet("What to fix first")
    .write_styled_sheet(wb, "What to fix first", blockers, tint_col = "severity")
  }

  wb$add_worksheet("Validation")

  if (nrow(report_df) == 0) {
    placeholder <- data.frame(
      message = "No validation findings to report.",
      stringsAsFactors = FALSE
    )
    wb$add_data(sheet = "Validation", x = placeholder, start_row = 1L)
  } else {
    .write_styled_sheet(wb, "Validation", report_df, tint_col = "status")
  }

  if (!is.null(diagnostics) && nrow(diagnostics) > 0) {
    wb$add_worksheet("Diagnostics")
    .write_styled_sheet(wb, "Diagnostics", diagnostics, tint_col = "severity")
  }

  ## Always last: explain what the row tints on every sheet mean.
  .write_legend_sheet(wb)

  openxlsx2::wb_save(wb, file = output_path, overwrite = TRUE)
  invisible(output_path)
}

#' Write the "Legend" worksheet: one tinted row per status/severity, so the
#' reader sees the colour and reads its meaning + exact hex code. Uses the same
#' `.REPORT_STATUS_FILL` palette and the same styled-sheet renderer as the
#' tinting itself, so the key can never drift from the report.
#' @noRd
.write_legend_sheet <- function(wb) {
  legend <- data.frame(
    Status = c("PASS", "WARN", "FAIL", "INFO"),
    Meaning = c(
      "Annotation matched a dataset + variable in the ADaM spec. No action needed.",
      "Needs review (e.g. an uncertain mapping). The ARS JSON is still generated.",
      "Could not be validated (invalid dataset/variable, or a blocking gap). Fix before use.",
      "Informational note (mainly the Diagnostics sheet). Not a validation failure."),
    `Fill (hex)` = unname(.REPORT_STATUS_FILL[c("PASS", "WARN", "FAIL", "INFO")]),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  wb$add_worksheet("Legend")
  .write_styled_sheet(wb, "Legend", legend, tint_col = "Status")
  ## A cell with no tint simply carries no status (e.g. a header).
  wb$add_data(
    sheet = "Legend", start_row = nrow(legend) + 3L,
    x = data.frame(
      Note = "An untinted cell carries no status (headers, or a row with no finding).",
      check.names = FALSE, stringsAsFactors = FALSE)
  )
  invisible(wb)
}

#' Write one data frame to a worksheet with the shared header style,
#' row tinting keyed on `tint_col`, clamped auto widths, and frozen header.
#' @noRd
.write_styled_sheet <- function(wb, sheet, df, tint_col) {
  wb$add_data(sheet = sheet, x = df, start_row = 1L)

  n_cols <- ncol(df)

  ## Header style: bold white text on dark blue.
  wb$add_cell_style(
    sheet      = sheet,
    dims       = openxlsx2::wb_dims(rows = 1L, cols = seq_len(n_cols)),
    horizontal = "left"
  )
  wb$add_font(
    sheet = sheet,
    dims  = openxlsx2::wb_dims(rows = 1L, cols = seq_len(n_cols)),
    bold  = "true", color = openxlsx2::wb_color(hex = "FFFFFF")
  )
  wb$add_fill(
    sheet = sheet,
    dims  = openxlsx2::wb_dims(rows = 1L, cols = seq_len(n_cols)),
    color = openxlsx2::wb_color(hex = "1F4E78")
  )

  ## Tint rows by status / severity. Not every sheet has a status to tint by
  ## (an edit log, for instance), in which case tint_col is NULL.
  if (!is.null(tint_col)) {
    status_col <- match(tint_col, names(df))
    if (!is.na(status_col)) {
      .tint_status_rows(wb, df, status_col, sheet = sheet)
    }
  }

  ## Auto widths (clamped 12..60).
  for (c in seq_len(n_cols)) {
    lens <- nchar(as.character(df[[c]]) %||% "")
    width <- max(12L, min(60L, max(lens, na.rm = TRUE) + 2L))
    wb$set_col_widths(sheet = sheet, cols = c, widths = width)
  }
  wb$freeze_pane(sheet = sheet, first_active_row = 2L)
  invisible(wb)
}

.tint_status_rows <- function(wb, df, status_col, sheet = "Validation") {
  status_vals <- df[[status_col]]
  for (i in seq_along(status_vals)) {
    colr <- unname(.REPORT_STATUS_FILL[status_vals[i]])
    if (is.na(colr) || !nzchar(colr)) next
    wb$add_fill(
      sheet = sheet,
      dims  = openxlsx2::wb_dims(rows = i + 1L, cols = seq_len(ncol(df))),
      color = openxlsx2::wb_color(hex = colr)
    )
  }
}
