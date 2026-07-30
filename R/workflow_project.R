## arsbridge -- workflow_project.R
## ---------------------------------------------------------------------------
## The project model behind ars_workflow(): one folder per study run, with a
## fixed layout, a small state file recording the inputs, and step statuses
## derived from FILE EXISTENCE -- so closing the app loses nothing and
## reopening it resumes exactly where the files say the work stands.
##
## Everything in this file is plain R (no Shiny), so it is unit-testable and
## usable from a script.
##
## Layout under a project directory:
##   arsbridge_project.json    shell/spec paths, study id, timestamps
##   copilot/                  the two-phase instruction files + JSON schema,
##                             plus everything the Copilot round-trips return:
##                             tlf_extraction_blueprints.json, supplement.json,
##                             extraction_validation_report.json
##   ars/                      reporting_event.json, validation xlsx, code/

.WORKFLOW_STATE_FILE <- "arsbridge_project.json"
.WORKFLOW_BLUEPRINT_FILE <- "tlf_extraction_blueprints.json"
.WORKFLOW_SUPPLEMENT_FILE <- "supplement.json"
.WORKFLOW_EXTRACTION_REPORT_FILE <- "extraction_validation_report.json"

#' The project's subdirectories (paths only; nothing is created).
#' @noRd
.workflow_dirs <- function(project_dir) {
  list(
    copilot = file.path(project_dir, "copilot"),
    ars     = file.path(project_dir, "ars")
  )
}

#' Every canonical file path of a project, in one place.
#' @noRd
.workflow_paths <- function(project_dir) {
  dirs <- .workflow_dirs(project_dir)
  list(
    state             = file.path(project_dir, .WORKFLOW_STATE_FILE),
    phase1_md         = file.path(dirs$copilot, .COPILOT_PHASE1_FILE),
    phase2_md         = file.path(dirs$copilot, .COPILOT_PHASE2_FILE),
    schema            = file.path(dirs$copilot, .SUPPLEMENT_SCHEMA_FILE),
    blueprint         = file.path(dirs$copilot, .WORKFLOW_BLUEPRINT_FILE),
    supplement        = file.path(dirs$copilot, .WORKFLOW_SUPPLEMENT_FILE),
    extraction_report = file.path(dirs$copilot, .WORKFLOW_EXTRACTION_REPORT_FILE),
    ars               = file.path(dirs$ars, "reporting_event.json"),
    report            = file.path(dirs$ars, "spec_validation_report.xlsx"),
    code_dir          = file.path(dirs$ars, "code")
  )
}

#' Create the project: the four subdirectories and the state file.
#' @noRd
.workflow_init <- function(project_dir, shell_path, adam_spec_path,
                           study_id = "STUDY-001") {
  shell_path     <- trimws(shell_path %||% "")
  adam_spec_path <- trimws(adam_spec_path %||% "")
  study_id       <- trimws(study_id %||% "")
  if (!nzchar(study_id)) study_id <- "STUDY-001"

  if (!.is_shell_path(shell_path) || !file.exists(shell_path)) {
    cli::cli_abort(
      "The shell must be an existing {.file .docx} or {.file .xlsx} file (got {.path {shell_path}}).")
  }
  if (!grepl("\\.(xlsx|xml)$", adam_spec_path, ignore.case = TRUE) ||
      !file.exists(adam_spec_path)) {
    cli::cli_abort("The ADaM spec must be an existing {.file .xlsx} or {.file .xml} file (got {.path {adam_spec_path}}).")
  }
  ## An Excel shell and an Excel spec have the same extension, so the two
  ## inputs can no longer be told apart by their names alone. One path given
  ## for both would parse the spec as a shell and produce an empty project
  ## with no useful explanation.
  if (identical(normalizePath(shell_path, mustWork = FALSE),
                normalizePath(adam_spec_path, mustWork = FALSE))) {
    cli::cli_abort(c(
      "The shell and the ADaM spec are the same file.",
      "x" = "Got {.path {shell_path}} for both.",
      "i" = "The shell is the annotated TLF document; the spec lists the ADaM variables."
    ))
  }

  if (!dir.exists(project_dir)) {
    ok <- dir.create(project_dir, recursive = TRUE, showWarnings = FALSE)
    if (!isTRUE(ok) && !dir.exists(project_dir)) {
      cli::cli_abort("Could not create the project directory {.path {project_dir}}.")
    }
  }
  for (d in .workflow_dirs(project_dir)) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }

  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  state <- list(
    arsbridge_version = as.character(utils::packageVersion("arsbridge")),
    created           = now,
    updated           = now,
    shell_path        = normalizePath(shell_path),
    adam_spec_path    = normalizePath(adam_spec_path),
    study_id          = study_id
  )
  .workflow_write_state(project_dir, state)
  invisible(state)
}

#' @noRd
.workflow_write_state <- function(project_dir, state) {
  json_text <- jsonlite::toJSON(state, auto_unbox = TRUE, pretty = TRUE,
                                null = "null")
  .write_text(json_text, .workflow_paths(project_dir)$state,
              "the project state", useBytes = TRUE)
  invisible(state)
}

#' Read the project state; NULL when there is no (readable) project here.
#' @noRd
.workflow_read_state <- function(project_dir) {
  path <- .workflow_paths(project_dir)$state
  if (is.null(project_dir) || !nzchar(project_dir %||% "") ||
      !file.exists(path)) {
    return(NULL)
  }
  tryCatch(
    jsonlite::fromJSON(path, simplifyVector = TRUE),
    error = function(e) {
      cli::cli_warn("Could not read {.path {path}}: {conditionMessage(e)} -- treating this folder as a fresh project.")
      NULL
    }
  )
}

#' Merge fields into the state file and refresh its timestamp.
#' @noRd
.workflow_touch_state <- function(project_dir, ...) {
  state <- .workflow_read_state(project_dir)
  if (is.null(state)) return(invisible(NULL))
  updates <- list(...)
  for (field in names(updates)) state[[field]] <- updates[[field]]
  state$updated <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  .workflow_write_state(project_dir, state)
}

## The step ids, in journey order. Steps 3 and 4 (the manual Copilot
## round-trips) are skippable: the build runs deterministically without them.
.WORKFLOW_STEPS <- c("project", "instructions", "phase1", "phase2",
                     "build", "review")

#' Step statuses, derived entirely from what is on disk.
#'
#' Returns one row per step: `step`, `done`, `available` (all prerequisites
#' met), and a human `detail`. The skip rule: phase1/phase2 become available
#' once the instruction files exist, but the BUILD step needs only the
#' instructions too -- a project can go straight to a deterministic build.
#' @noRd
.workflow_status <- function(project_dir) {
  paths <- .workflow_paths(project_dir)
  state <- .workflow_read_state(project_dir)

  inputs_ok <- !is.null(state) &&
    file.exists(state$shell_path %||% "") &&
    file.exists(state$adam_spec_path %||% "")
  project_done <- !is.null(state)
  project_detail <- if (is.null(state)) {
    "Choose the project folder, shell, and ADaM spec."
  } else if (!inputs_ok) {
    "The recorded shell or spec no longer exists -- fix the paths."
  } else {
    sprintf("%s -- shell and spec recorded.", state$study_id %||% "")
  }

  instructions_done <- file.exists(paths$phase1_md) &&
    file.exists(paths$phase2_md) && file.exists(paths$schema)
  blueprint_done  <- file.exists(paths$blueprint)
  supplement_done <- file.exists(paths$supplement)
  ars_done        <- file.exists(paths$ars)

  data.frame(
    step = .WORKFLOW_STEPS,
    done = c(project_done && inputs_ok, instructions_done, blueprint_done,
             supplement_done, ars_done, FALSE),
    available = c(
      TRUE,
      project_done && inputs_ok,
      instructions_done,
      instructions_done,
      project_done && inputs_ok && instructions_done,
      ars_done
    ),
    detail = c(
      project_detail,
      if (instructions_done) "Instruction files are in copilot/." else
        "Write the Phase 1 / Phase 2 instruction files.",
      if (blueprint_done) "Blueprint received." else
        "Run Phase 1 in your chat assistant, then drop the blueprint here.",
      if (supplement_done) "supplement.json received." else
        "Run Phase 2 in your chat assistant, then drop supplement.json here.",
      if (ars_done) "Reporting event built." else
        "Build the reporting event (with the supplement, or deterministically).",
      if (ars_done) "Open the reporting event in the review editor." else
        "Available after the build."
    ),
    stringsAsFactors = FALSE
  )
}

#' Receive a JSON document (pasted text or an uploaded file's content) and
#' write the canonical copy into the project.
#'
#' Chat assistants return JSON inside a fenced code block as often as not, so
#' a leading/trailing fence is stripped before parsing. The text must parse
#' as JSON to be accepted -- a truncated paste is caught here, not three
#' steps later.
#' @noRd
.workflow_receive_json <- function(text, dest, label = "the file") {
  text <- paste(text %||% "", collapse = "\n")
  text <- trimws(text)
  ## Strip a markdown code fence, with or without the "json" tag.
  text <- sub("^```[Jj]?[Ss]?[Oo]?[Nn]?\\s*\\n", "", text)
  text <- sub("\\n```\\s*$", "", text)
  text <- trimws(text)

  if (!nzchar(text)) {
    return(list(ok = FALSE, path = NULL,
                message = paste0("Nothing to save -- paste or upload ", label, ".")))
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(text, simplifyVector = FALSE),
    error = function(e) e
  )
  if (inherits(parsed, "error")) {
    return(list(ok = FALSE, path = NULL,
                message = paste0("Not valid JSON (", conditionMessage(parsed),
                                 ") -- copy the complete file and try again.")))
  }

  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  .write_text(text, dest, label, useBytes = TRUE)
  list(ok = TRUE, path = dest, message = paste0("Saved ", basename(dest), "."))
}

#' Run the build: spec_to_ars() with the project's inputs, in supplement mode
#' when supplement.json exists and deterministically otherwise. Kept out of
#' the Shiny observer so tests exercise it directly.
#' @noRd
.workflow_run_build <- function(state, paths) {
  supplement <- if (file.exists(paths$supplement)) paths$supplement else NULL
  spec_to_ars(
    shell_path     = state$shell_path,
    adam_spec_path = state$adam_spec_path,
    output_path    = paths$ars,
    study_id       = state$study_id %||% "STUDY-001",
    supplement     = supplement,
    report_path    = paths$report,
    code_dir       = paths$code_dir,
    use_llm        = FALSE,
    verbose        = FALSE
  )
}

#' The stopApp payload that hands the built event to the editor.
#' @noRd
.workflow_handoff_payload <- function(project_dir) {
  paths <- .workflow_paths(project_dir)
  state <- .workflow_read_state(project_dir)
  list(
    action         = "edit",
    project_dir    = project_dir,
    ars_path       = paths$ars,
    adam_spec_path = state$adam_spec_path %||% NULL,
    report_path    = if (file.exists(paths$report)) paths$report else NULL
  )
}
