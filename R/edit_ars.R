## arsbridge -- edit_ars.R
## ---------------------------------------------------------------------------
## The editing entry point, and the save pipeline behind it.
##
## Saving is treated as the dangerous part, because it overwrites a file the
## reviewer may have spent an hour correcting:
##
##   * the original is backed up before the first overwrite,
##   * the new file is written to a temporary name in the SAME directory and
##     renamed into place, so an interrupted write cannot leave a half-written
##     reporting event where the good one was,
##   * an edit log is written beside it as a sidecar, keeping the ARS JSON
##     itself free of non-standard provenance fields,
##   * and the result is re-validated so the reviewer is told what they saved.
##
## .edit_ars_finish() does all of that with no Shiny involved, so the whole
## save path is testable without a browser.

#' Review and correct an ARS reporting event interactively
#'
#' Opens the reporting event in the same structured viewer as [view_ars()],
#' with the detail panels editable: methods, populations, data subsets,
#' groupings and analysis variables are chosen from what actually exists --
#' the entities in the file, the methods the engine can execute, and (when the
#' ADaM spec is supplied) the variables the study really has.
#'
#' Nothing is written until you choose to save, and saving shows what changed
#' first.
#'
#' @param ars What to edit. Either a path to an ARS JSON file, an already
#'   parsed reporting event, or the whole result of [spec_to_ars()] -- which
#'   carries the event, its validation table and the paths it wrote.
#' @param adam_spec_path Optional path to the ADaM spec (`define.xml` or
#'   Excel). When supplied, variables are chosen from the spec rather than
#'   typed, and datasets and variables are checked against it.
#' @param report_path Optional path to the validation report [spec_to_ars()]
#'   wrote. When supplied, annotated shell lines that no analysis covers are
#'   reported as gaps.
#' @param output_path Where to write. Defaults to the file `ars` was read
#'   from; required when `ars` is an in-memory reporting event, since there is
#'   no file to write back to.
#'
#' @return Invisibly, the path written -- or `NULL` if the session was closed
#'   without saving.
#'
#' @section What saving does:
#' The previous file is copied to `<name>.json.bak-<timestamp>` before the
#' first overwrite. The new content is written to a temporary file in the same
#' directory and renamed into place, so an interrupted save cannot destroy the
#' file it was replacing. An edit log is written to `<name>.edits.json`
#' alongside it: the ARS JSON itself stays free of non-standard fields, so the
#' deliverable remains CDISC-clean.
#'
#' @seealso [view_ars()] to review without editing, [validate_ars_model()]
#'   for the findings on the command line.
#'
#' @examples
#' \dontrun{
#' # Correct what spec_to_ars() just generated, then execute it.
#' result <- spec_to_ars(shell_path = "shells.docx",
#'                       adam_spec_path = "adam_spec.xlsx")
#' corrected <- edit_ars(result)
#' ard <- ars_to_ard(corrected, adam_dir = "adam")
#' }
#' @export
edit_ars <- function(ars, adam_spec_path = NULL, report_path = NULL,
                     output_path = NULL) {
  rlang::check_installed(
    c("shiny", "bslib", "DT"),
    reason = "to open the ARS editor"
  )

  input <- .normalize_ars_input(ars, adam_spec_path, report_path)

  if (is.null(output_path)) output_path <- input$source_path
  ## Fail now rather than after an hour of corrections.
  if (is.null(output_path)) {
    cli::cli_abort(c(
      "{.arg output_path} is required when {.arg ars} is not a file.",
      "i" = "An in-memory reporting event has no file to write back to.",
      "i" = "Pass {.code output_path = \"reporting_event.json\"}."
    ))
  }

  app <- .ars_editor_app(
    model       = input$model,
    spec        = input$spec,
    report      = input$report,
    source_path = input$source_path,
    mode        = "edit"
  )

  result <- shiny::runApp(app)

  if (is.null(result)) {
    cli::cli_alert_info("Closed without saving -- nothing was written.")
    return(invisible(NULL))
  }

  .edit_ars_finish(result, output_path, input$spec, input$report)
}

#' @rdname edit_ars
#' @details
#' `review_ars()` is an alias for [edit_ars()]. Both open the same tool; the
#' name is a matter of which framing fits -- "review" is what a clinical QC
#' process calls this step, "edit" is what the tool does.
#' @export
review_ars <- edit_ars


## Serialize, back up, write atomically, log, re-validate. Returns the path.
#' @noRd
.edit_ars_finish <- function(result, output_path, spec = NULL, report = NULL) {
  ars <- model_to_ars(result$model)

  ## Back up whatever is there before replacing it.
  if (file.exists(output_path)) {
    backup_path <- paste0(
      output_path, ".bak-", format(Sys.time(), "%Y%m%d-%H%M%S")
    )
    if (!file.copy(output_path, backup_path, overwrite = FALSE)) {
      cli::cli_warn(
        "Could not back up {.path {output_path}} -- saving anyway."
      )
    } else {
      cli::cli_alert_info("Backed up the previous file to {.path {backup_path}}")
    }
  }

  ## Write to a temporary name in the same directory, then rename. A rename
  ## within one directory is atomic; writing straight to output_path would
  ## leave a truncated file if anything failed part-way.
  target_dir <- dirname(output_path)
  temp_path  <- tempfile(tmpdir = target_dir, fileext = ".json.tmp")

  json_text <- jsonlite::toJSON(ars, auto_unbox = TRUE, pretty = TRUE,
                                null = "null")
  .write_text(json_text, temp_path, "the ARS JSON", useBytes = TRUE)

  if (!file.rename(temp_path, output_path)) {
    unlink(temp_path)
    cli::cli_abort(c(
      "Could not replace {.path {output_path}}.",
      "i" = "The previous file is untouched; check the folder is writable."
    ))
  }

  .write_edit_log(result$edit_log, output_path)

  ## The work is on disk now, so the crash-recovery copy has nothing left to
  ## protect -- leaving it would offer stale changes on the next open.
  .clear_autosave(result$source_path %||% output_path)

  findings <- validate_ars_model(result$model, spec, report)
  .report_save(output_path, result$edit_log, findings)

  invisible(output_path)
}

## The edit log lives beside the JSON rather than inside it: the ARS file
## stays conformant, and the log doubles as the QC record of what a human
## changed and when.
#' @noRd
.write_edit_log <- function(edit_log, output_path) {
  sidecar_path <- paste0(sub("\\.json$", "", output_path), ".edits.json")

  existing <- if (file.exists(sidecar_path)) {
    tryCatch(jsonlite::read_json(sidecar_path), error = function(e) NULL)
  }

  sidecar <- list(
    source            = basename(output_path),
    saved_at_utc      = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    arsbridge_version = as.character(utils::packageVersion("arsbridge")),
    user              = unname(Sys.info()[["user"]]),
    n_edits           = nrow(edit_log),
    edits             = edit_log,
    ## Keep earlier sessions rather than overwriting the record of them.
    previous_sessions = existing[["sessions"]] %||% list()
  )

  json_text <- jsonlite::toJSON(sidecar, auto_unbox = TRUE, pretty = TRUE,
                                null = "null")
  .write_text(json_text, sidecar_path, "the edit log", useBytes = TRUE)
  invisible(sidecar_path)
}

#' @noRd
.report_save <- function(output_path, edit_log, findings) {
  n_edits <- nrow(edit_log)
  cli::cli_alert_success(
    "Saved {n_edits} edit{?s} to {.path {output_path}}"
  )

  n_fail <- sum(findings$severity == "FAIL")
  n_warn <- sum(findings$severity == "WARN")

  if (n_fail > 0) {
    cli::cli_alert_danger(
      "{n_fail} blocking problem{?s} remain{?s/} -- run {.code validate_ars_model()} to list {?it/them}."
    )
  } else if (n_warn > 0) {
    cli::cli_alert_info("No blocking problems; {n_warn} thing{?s} to review.")
  } else {
    cli::cli_alert_success("Nothing left to fix.")
  }
  invisible(NULL)
}
