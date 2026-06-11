## arsbridge -- write_validation_report.R
## ---------------------------------------------------------------------------
## Writes the spec-validation report to a styled Excel workbook (openxlsx2).
## Sheet 1 "Validation": annotation-vs-spec cross-reference, rows tinted by
## status (PASS green, WARN amber, FAIL red).
## Sheet 2 "Diagnostics" (when records exist): every pipeline fallback,
## parsing miss, LLM failure, and dropped condition collected by the
## diagnostics collector (R/diagnostics.R), tinted by severity.

#' Write a validation report data frame to a styled Excel file.
#'
#' @param report_df Data frame from [validate_annotations_spec()].
#' @param output_path Path to the `.xlsx` to write.
#' @param diagnostics Optional data frame from [diag_records()] -- written to
#'   a second "Diagnostics" worksheet when it has rows.
#'
#' @return Invisibly returns `output_path`.
#'
#' @keywords internal
#' @noRd
write_validation_report <- function(report_df, output_path, diagnostics = NULL) {
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    cli::cli_abort("openxlsx2 is required to write the validation report.")
  }

  wb <- openxlsx2::wb_workbook(creator = "arsbridge")
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

  openxlsx2::wb_save(wb, file = output_path, overwrite = TRUE)
  invisible(output_path)
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

  ## Tint rows by status / severity.
  status_col <- match(tint_col, names(df))
  if (!is.na(status_col)) {
    .tint_status_rows(wb, df, status_col, sheet = sheet)
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
  pal <- c(PASS = "E2EFDA", WARN = "FFF2CC", FAIL = "FCE4D6",
           INFO = "DDEBF7")
  for (i in seq_along(status_vals)) {
    colr <- unname(pal[status_vals[i]])
    if (is.na(colr) || !nzchar(colr)) next
    wb$add_fill(
      sheet = sheet,
      dims  = openxlsx2::wb_dims(rows = i + 1L, cols = seq_len(ncol(df))),
      color = openxlsx2::wb_color(hex = colr)
    )
  }
}
