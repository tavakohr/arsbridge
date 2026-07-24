## arsbridge -- export_edit_log.R
## ---------------------------------------------------------------------------
## The review session as a QC record.
##
## A corrected reporting event is a deliverable, and in a regulated setting a
## deliverable that a human changed needs to say what was changed, by whom and
## when. The sidecar .edits.json already holds that, but JSON is not what a QC
## reviewer or an auditor reads -- so this turns it into the same styled
## workbook the rest of the pipeline reports through.

#' Export a review session's changes as a QC workbook
#'
#' Turns the edit log written beside a corrected reporting event
#' (`<name>.edits.json`) into a styled Excel workbook: who changed what, when,
#' and what the value was before and after.
#'
#' The ARS JSON itself carries no provenance fields, which is deliberate --
#' the deliverable stays CDISC-conformant. This is where the provenance lives
#' instead.
#'
#' @param edits Either the path to a `<name>.edits.json` sidecar, the path to
#'   the reporting event it sits beside, or the data frame of edits itself.
#' @param output_path Path to the `.xlsx` to write. Defaults to the sidecar's
#'   name with an `.xlsx` extension.
#'
#' @return Invisibly, the path written.
#'
#' @section Sheets:
#' \describe{
#'   \item{Summary}{One row per field that ended up different, with its
#'     before and after value -- repeated edits to the same field collapse
#'     into one row, and a field edited back to its original value does not
#'     appear at all.}
#'   \item{All changes}{Every recorded edit in order, including the ones the
#'     summary collapses.}
#'   \item{Session}{Who saved it, when, and with which version of arsbridge.}
#' }
#'
#' @seealso [edit_ars()], which writes the sidecar this reads.
#'
#' @examples
#' \dontrun{
#' corrected <- edit_ars("reporting_event.json")
#' export_edit_log(corrected, "review_record.xlsx")
#' }
#' @export
export_edit_log <- function(edits, output_path = NULL) {
  rlang::check_installed("openxlsx2", reason = "to write the QC workbook")

  session <- NULL

  if (is.character(edits) && length(edits) == 1) {
    sidecar_path <- .edit_log_path(edits)
    .require_file(sidecar_path, "edits", "the edit log")

    session <- jsonlite::read_json(sidecar_path, simplifyVector = TRUE)
    edits <- session$edits

    if (is.null(output_path)) {
      output_path <- sub("\\.json$", ".xlsx", sidecar_path)
    }
  }

  if (is.null(output_path)) {
    cli::cli_abort(
      "{.arg output_path} is required when {.arg edits} is a data frame."
    )
  }

  edits <- .as_edit_log(edits)

  workbook <- openxlsx2::wb_workbook(creator = "arsbridge")

  workbook$add_worksheet("Summary")
  summary <- .diff_summary(edits)
  if (nrow(summary) == 0) {
    workbook$add_data(
      sheet = "Summary",
      x = data.frame(message = "No changes were recorded.",
                     stringsAsFactors = FALSE)
    )
  } else {
    names(summary) <- c("Entity type", "Entity", "Field", "Before", "After")
    .write_styled_sheet(workbook, "Summary", summary, tint_col = NULL)
  }

  workbook$add_worksheet("All changes")
  if (nrow(edits) == 0) {
    workbook$add_data(
      sheet = "All changes",
      x = data.frame(message = "No changes were recorded.",
                     stringsAsFactors = FALSE)
    )
  } else {
    .write_styled_sheet(workbook, "All changes", edits, tint_col = NULL)
  }

  workbook$add_worksheet("Session")
  workbook$add_data(sheet = "Session", x = .session_sheet(session, edits))

  openxlsx2::wb_save(workbook, file = output_path, overwrite = TRUE)
  cli::cli_alert_success("Wrote the review record to {.path {output_path}}")
  invisible(output_path)
}

## Accept either the sidecar itself or the reporting event it sits beside,
## since which one a user has to hand depends on where they came from.
#' @noRd
.edit_log_path <- function(path) {
  if (grepl("\\.edits\\.json$", path)) return(path)
  paste0(sub("\\.json$", "", path), ".edits.json")
}

#' @noRd
.as_edit_log <- function(edits) {
  if (is.null(edits) || length(edits) == 0) return(.new_edit_log())

  edits <- as.data.frame(edits, stringsAsFactors = FALSE)
  required <- c("time", "pool", "id", "field", "old", "new")
  if (!all(required %in% names(edits))) {
    cli::cli_abort(c(
      "That does not look like an arsbridge edit log.",
      "i" = "Expected the columns {.val {required}}."
    ))
  }
  edits[, required, drop = FALSE]
}

#' @noRd
.session_sheet <- function(session, edits) {
  value_or <- function(value, fallback) {
    if (is.null(value) || length(value) == 0) fallback else as.character(value)
  }

  data.frame(
    Item = c("Reporting event", "Saved at (UTC)", "Saved by",
             "arsbridge version", "Changes recorded"),
    Value = c(
      value_or(session$source, "(not recorded)"),
      value_or(session$saved_at_utc, "(not recorded)"),
      value_or(session$user, "(not recorded)"),
      value_or(session$arsbridge_version, "(not recorded)"),
      as.character(nrow(edits))
    ),
    stringsAsFactors = FALSE
  )
}
