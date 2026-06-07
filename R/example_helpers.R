## arsbridge -- example_helpers.R
## ---------------------------------------------------------------------------
## Tiny user-facing helpers that expose the bundled training data and offer a
## zero-argument entry point so new users can see the package do its thing
## without owning a study.
##
## Pattern (intentional): mirrors `readr::readr_example()` and
## `palmerpenguins::path_to_file()` -- when called with no argument, list
## available files; when called with a name, return the absolute path.

#' Bundled training files shipped with arsbridge
#'
#' Returns the absolute path to a bundled example file, or -- when called
#' with no argument -- the names of all files available in the bundle.
#' The bundle is a small, anonymised slice of the APX-DRM-301 atopic
#' dermatitis study used throughout the documentation and tests.
#'
#' @param file Character. Name of a bundled file (e.g.
#'   `"annotated_shell.docx"`). If `NULL` (default), returns a character
#'   vector of every file in the bundle.
#'
#' @return A character path (absolute) when `file` is named; a character
#'   vector of file names when `file` is `NULL`.
#'
#' @details
#' Files currently in the bundle:
#' \describe{
#'   \item{`annotated_shell.docx`}{Lead-programmer annotated TLF shells
#'     for 40 TLFs (24 tables + 10 listings + 6 figures). Uses the
#'     standard red \code{C00000} run convention for ADaM variable
#'     references. Roughly 80 KB.}
#'   \item{`adam_spec.xlsx`}{ADaM specification workbook covering 8
#'     domains (ADSL, ADAE, ADCM, ADEFF, ADEX, ADLB, ADMH, ADVS).
#'     Roughly 95 KB.}
#'   \item{`ADaM.zip`}{Simulated 60-subject ADaM data as XPT files
#'     (the eight domains above). Stratified by treatment arm
#'     (13 / 23 / 24 across Placebo / UPADALIMIB 15 mg / UPADALIMIB 30 mg).
#'     Roughly 680 KB compressed, 12 MB extracted. Consumed by the
#'     downstream `siera_workflow/` runner, not by arsbridge itself.}
#' }
#'
#' @examples
#' arsbridge_example()                       # list bundle contents
#' arsbridge_example("annotated_shell.docx") # path to the shell
#' arsbridge_example("adam_spec.xlsx")       # path to the ADaM spec
#'
#' @export
arsbridge_example <- function(file = NULL) {
  bundle <- system.file("extdata", "example_apx_drm_301",
                        package = "arsbridge")
  if (!nzchar(bundle) || !dir.exists(bundle)) {
    cli::cli_abort(c(
      "arsbridge example bundle not found.",
      "i" = "Reinstall the package or run {.code devtools::load_all()} from the package source."
    ))
  }

  files <- list.files(bundle)
  if (is.null(file)) return(files)

  if (!file %in% files) {
    cli::cli_abort(c(
      "{.val {file}} is not in the bundle.",
      "i" = "Available: {.val {files}}",
      " " = "Call {.code arsbridge_example()} with no argument to list bundle contents."
    ))
  }

  file.path(bundle, file)
}


#' Run spec_to_ars() against the bundled example inputs
#'
#' Zero-argument entry point that runs the full `spec_to_ars()` pipeline
#' against `arsbridge_example("annotated_shell.docx")` +
#' `arsbridge_example("adam_spec.xlsx")`. Useful as a first call after
#' installation -- you get a real ARS JSON and validation report from the
#' APX-DRM-301 training shell without owning a study.
#'
#' @param output_path Where to write the ARS JSON. Default:
#'   `"reporting_event.json"` in `tempdir()`.
#' @param report_path Where to write the spec validation report. Default:
#'   `"spec_validation_report.xlsx"` in `tempdir()`.
#' @param ... Additional arguments forwarded to [spec_to_ars()].
#'
#' @return Invisibly returns the [spec_to_ars()] result list.
#'
#' @examples
#' \dontrun{
#' # One-call demo. Takes ~6 minutes (40 LLM calls).
#' res <- spec_to_ars_example()
#' res$n_tlfs       # 40
#' res$n_analyses   # ~226
#' res$n_warnings   # ~29
#' str(res$reporting_event, max.level = 1)
#' }
#' @export
spec_to_ars_example <- function(
  output_path = file.path(tempdir(), "reporting_event.json"),
  report_path = file.path(tempdir(), "spec_validation_report.xlsx"),
  ...
) {
  spec_to_ars(
    shell_path     = arsbridge_example("annotated_shell.docx"),
    adam_spec_path = arsbridge_example("adam_spec.xlsx"),
    output_path    = output_path,
    report_path    = report_path,
    study_id       = "APX-DRM-301",
    study_name     = "PROSVALIN Phase 3 in Atopic Dermatitis (training example)",
    ...
  )
}
