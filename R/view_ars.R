## arsbridge -- view_ars.R
## ---------------------------------------------------------------------------
## The read-only entry point to the review stage: render a reporting event as
## the structure a programmer already recognises -- the shell's outputs, each
## with its analysis lines -- instead of raw JSON.
##
## view_ars() never writes anything. edit_ars() (which can) shares the same
## app; the only difference is the mode it launches in.

## Input normalization is deliberately separate from anything Shiny, so the
## contract "you may pass a path, a parsed event, or the spec_to_ars() result"
## is unit-testable without a browser.
#' @noRd
.normalize_ars_input <- function(ars, adam_spec_path = NULL,
                                 report_path = NULL) {
  report <- NULL

  ## A spec_to_ars() result carries the event, the validation table and the
  ## paths it wrote -- so handing the whole result straight to the reviewer
  ## wires up dropdowns and gap detection with no further arguments.
  if (is.list(ars) && !is.null(ars[["reporting_event"]])) {
    result <- ars
    ars    <- result[["reporting_event"]]
    report <- result[["validation"]]

    if (is.null(report_path)) report_path <- result[["report_path"]]
    if (is.null(adam_spec_path)) adam_spec_path <- result[["adam_spec_path"]]

    model <- ars_to_model(ars)
    model$source_path <- result[["ars_path"]]
  } else {
    model <- ars_to_model(ars)
  }

  if (is.null(report) && !is.null(report_path)) {
    report <- .read_validation_report(report_path)
  }

  spec <- NULL
  if (!is.null(adam_spec_path)) {
    .require_file(adam_spec_path, "adam_spec_path", INPUT_SPEC)
    spec <- parse_adam_spec(adam_spec_path)
  }

  list(
    model       = model,
    spec        = spec,
    report      = report,
    source_path = model$source_path
  )
}

## Read the annotation validation report written by spec_to_ars().
##
## The sheet is looked up by NAME rather than position: a run with blockers
## puts "What to fix first" ahead of it. A clean run writes a single-column
## placeholder instead of the real table, which comes back as zero rows.
#' @noRd
.read_validation_report <- function(path) {
  .require_file(path, "report_path", "the validation report")

  if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    return(utils::read.csv(path, stringsAsFactors = FALSE))
  }

  rlang::check_installed("readxl", reason = "to read the validation report")

  sheets <- readxl::excel_sheets(path)
  if (!"Validation" %in% sheets) {
    cli::cli_warn(c(
      "No {.val Validation} sheet in {.path {path}}.",
      "i" = "Gap detection needs the report {.fn spec_to_ars} writes."
    ))
    return(NULL)
  }

  report <- as.data.frame(
    readxl::read_excel(path, sheet = "Validation"),
    stringsAsFactors = FALSE
  )

  ## The placeholder a clean run writes carries only a `message` column.
  if (!"tlf_number" %in% names(report)) {
    return(report[0, , drop = FALSE])
  }
  report
}


#' Review an ARS reporting event in a structured, clickable viewer
#'
#' Opens the reporting event as the structure a clinical programmer already
#' recognises -- each output with its analysis lines beneath it -- with
#' validation findings overlaid, so problems are visible without reading JSON.
#'
#' This viewer never writes: use [edit_ars()] to correct what it surfaces.
#'
#' @param ars What to review. Either a path to an ARS JSON file, an already
#'   parsed reporting event, or the whole result of [spec_to_ars()] -- which
#'   carries the event, its validation table and the paths it wrote, so gap
#'   detection and spec-aware display are wired up with no further arguments.
#' @param adam_spec_path Optional path to the ADaM spec (`define.xml` or
#'   Excel). When supplied, datasets and variables are checked against it.
#' @param report_path Optional path to the validation report
#'   [spec_to_ars()] wrote. When supplied, annotated shell lines that no
#'   analysis covers are reported as gaps.
#'
#' @return Invisibly `NULL`. Called for the viewer it opens.
#'
#' @seealso [ars_to_model()] for the same content as data frames,
#'   [validate_ars_model()] for the findings without a browser.
#'
#' @examples
#' \dontrun{
#' # Review what spec_to_ars() just generated.
#' result <- spec_to_ars(shell_path = "shells.docx",
#'                       adam_spec_path = "adam_spec.xlsx")
#' view_ars(result)
#'
#' # Or review a JSON file directly.
#' view_ars("reporting_event.json", adam_spec_path = "adam_spec.xlsx")
#' }
#' @export
view_ars <- function(ars, adam_spec_path = NULL, report_path = NULL) {
  rlang::check_installed(
    c("shiny", "bslib", "DT"),
    reason = "to open the ARS viewer"
  )

  input <- .normalize_ars_input(ars, adam_spec_path, report_path)

  app <- .ars_editor_app(
    model       = input$model,
    spec        = input$spec,
    report      = input$report,
    source_path = input$source_path,
    mode        = "view"
  )

  shiny::runApp(app)
  invisible(NULL)
}
