## arsbridge -- write_validation_report.R
## ---------------------------------------------------------------------------
## Writes the spec-validation report to a styled Excel workbook (openxlsx2).
## One worksheet ("Validation"), header row coloured, rows tinted by status
## (PASS green, WARN amber, FAIL red), auto column widths, frozen header.

#' Write a validation report data frame to a styled Excel file.
#'
#' @param report_df Data frame from [validate_annotations_spec()].
#' @param output_path Path to the `.xlsx` to write.
#'
#' @return Invisibly returns `output_path`.
#'
#' @keywords internal
#' @noRd
write_validation_report <- function(report_df, output_path) {
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
    wb$add_data(sheet = "Validation", x = report_df, start_row = 1L)

    n_rows <- nrow(report_df) + 1L   ## +1 for header
    n_cols <- ncol(report_df)

    ## Header style: bold white text on dark blue.
    wb$add_cell_style(
      sheet      = "Validation",
      dims       = openxlsx2::wb_dims(rows = 1L, cols = seq_len(n_cols)),
      horizontal = "left"
    )
    wb$add_font(
      sheet = "Validation",
      dims  = openxlsx2::wb_dims(rows = 1L, cols = seq_len(n_cols)),
      bold  = "true", color = openxlsx2::wb_color(hex = "FFFFFF")
    )
    wb$add_fill(
      sheet = "Validation",
      dims  = openxlsx2::wb_dims(rows = 1L, cols = seq_len(n_cols)),
      color = openxlsx2::wb_color(hex = "1F4E78")
    )

    ## Tint rows by status.
    status_col <- match("status", names(report_df))
    if (!is.na(status_col)) {
      .tint_status_rows(wb, report_df, status_col)
    }

    ## Auto widths (clamped 12..60).
    for (c in seq_len(n_cols)) {
      lens <- nchar(as.character(report_df[[c]]) %||% "")
      width <- max(12L, min(60L, max(lens, na.rm = TRUE) + 2L))
      wb$set_col_widths(sheet = "Validation",
                        cols = c, widths = width)
    }
    wb$freeze_pane(sheet = "Validation", first_active_row = 2L)
  }

  openxlsx2::wb_save(wb, file = output_path, overwrite = TRUE)
  invisible(output_path)
}

.tint_status_rows <- function(wb, df, status_col) {
  status_vals <- df[[status_col]]
  pal <- c(PASS = "E2EFDA", WARN = "FFF2CC", FAIL = "FCE4D6")
  for (i in seq_along(status_vals)) {
    colr <- pal[[status_vals[i]]]
    if (is.null(colr) || !nzchar(colr)) next
    wb$add_fill(
      sheet = "Validation",
      dims  = openxlsx2::wb_dims(rows = i + 1L, cols = seq_len(ncol(df))),
      color = openxlsx2::wb_color(hex = colr)
    )
  }
}
