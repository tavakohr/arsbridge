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
##   supplement/               the instruction file + JSON schema, the draft
##                             supplement the project can generate, and the
##                             reviewed supplement.json the next build uses
##   ars/                      reporting_event.json, validation xlsx, code/,
##                             ard.rds, filled_shells.xlsx, run.log, and the
##                             payload of the last build

.WORKFLOW_STATE_FILE <- "arsbridge_project.json"
.WORKFLOW_DRAFT_FILE <- "supplement_draft.json"
.WORKFLOW_SUPPLEMENT_FILE <- "supplement.json"
.WORKFLOW_PAYLOAD_FILE <- "last_run.rds"

#' The project's subdirectories (paths only; nothing is created).
#' @noRd
.workflow_dirs <- function(project_dir) {
  list(
    supplement = file.path(project_dir, "supplement"),
    ars        = file.path(project_dir, "ars")
  )
}

#' Every canonical file path of a project, in one place.
#' @noRd
.workflow_paths <- function(project_dir) {
  dirs <- .workflow_dirs(project_dir)
  list(
    state            = file.path(project_dir, .WORKFLOW_STATE_FILE),
    instructions     = file.path(dirs$supplement, .COPILOT_INSTRUCTIONS_FILE),
    schema           = file.path(dirs$supplement, .SUPPLEMENT_SCHEMA_FILE),
    draft            = file.path(dirs$supplement, .WORKFLOW_DRAFT_FILE),
    supplement       = file.path(dirs$supplement, .WORKFLOW_SUPPLEMENT_FILE),
    ars              = file.path(dirs$ars, "reporting_event.json"),
    report           = file.path(dirs$ars, "validation_report.xlsx"),
    code_dir         = file.path(dirs$ars, "code"),
    ard              = file.path(dirs$ars, "ard.rds"),
    filled_workbook  = file.path(dirs$ars, "filled_shells.xlsx"),
    run_log          = file.path(dirs$ars, "run.log"),
    payload          = file.path(dirs$ars, .WORKFLOW_PAYLOAD_FILE)
  )
}

#' Create the project: the four subdirectories and the state file.
#' @noRd
.workflow_init <- function(project_dir, shell_path, adam_spec_path,
                           study_id = "STUDY-001", adam_dir = NULL) {
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
    ## Optional: without it the reporting event is still built, and the
    ## results and filled workbook are reported as not produced rather than
    ## treated as a failure.
    adam_dir          = if (is.null(adam_dir) || !nzchar(trimws(adam_dir)))
                          NULL else normalizePath(trimws(adam_dir),
                                                  mustWork = FALSE),
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

## --- remembering which projects exist --------------------------------------
##
## Everything above derives a project's state from its own folder, which is
## right: the folder IS the state. What it cannot do is tell you WHICH folder,
## and that gap was the whole of a usability complaint -- `ars_workflow()` with
## no argument opened on five empty boxes every time, so a study you had set up
## the day before had to be typed out again from memory.
##
## So one small thing is remembered outside the project: the list of project
## folders that have been opened. It lives in the user's config directory,
## alongside the editor's autosaves in `tools::R_user_dir("arsbridge", ...)`,
## and it holds paths and nothing else -- no study data leaves the project.

.WORKFLOW_RECENT_FILE <- "recent_projects.json"
.WORKFLOW_RECENT_MAX  <- 8L

#' Where the recent-projects list lives.
#'
#' Behind an option so a test suite never writes into the real user
#' directory; `tests/testthat/setup.R` points it at a temporary file.
#' @noRd
.workflow_recent_path <- function() {
  getOption("arsbridge.recent_projects",
            file.path(tools::R_user_dir("arsbridge", "config"),
                      .WORKFLOW_RECENT_FILE))
}

#' The list exactly as stored, folders that are gone included.
#' @noRd
.workflow_recent_stored <- function(path = .workflow_recent_path()) {
  if (!file.exists(path)) return(character())
  dirs <- tryCatch(jsonlite::fromJSON(path, simplifyVector = TRUE),
                   error = function(e) NULL)
  if (!is.character(dirs)) return(character())
  unique(dirs[nzchar(dirs)])
}

#' Projects opened before, most recent first, ones that are no longer there
#' left out.
#'
#' The filtering happens on the way OUT rather than on the way in: a project
#' on a network share or an unplugged drive is not gone, it is unreachable,
#' and it comes back on the list when the drive does.
#' @noRd
.workflow_recent_projects <- function(path = .workflow_recent_path()) {
  dirs <- .workflow_recent_stored(path)
  dirs[file.exists(file.path(dirs, .WORKFLOW_STATE_FILE))]
}

#' Record a project as the most recently opened one.
#' @noRd
.workflow_remember_project <- function(project_dir,
                                       path = .workflow_recent_path()) {
  if (is.null(project_dir) || !nzchar(project_dir %||% "")) {
    return(invisible(character()))
  }
  full <- normalizePath(project_dir, mustWork = FALSE)
  dirs <- utils::head(c(full, setdiff(.workflow_recent_stored(path), full)),
                      .WORKFLOW_RECENT_MAX)
  ## Remembering is a convenience, so it fails quietly and completely: a
  ## read-only config directory must not be the reason a project cannot be
  ## opened, nor the source of a warning about one.
  written <- suppressWarnings(tryCatch({
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    .write_text(jsonlite::toJSON(dirs, pretty = TRUE), path,
                "the recent-projects list", useBytes = TRUE)
    TRUE
  }, error = function(e) FALSE))
  if (!isTRUE(written)) return(invisible(character()))
  invisible(dirs)
}

#' The project to open when the caller named none.
#'
#' In order: what the caller asked for; the working directory, if you are
#' standing in a project (the strongest signal there is); then the last
#' project opened. `NULL` only when none of those holds, which is the genuine
#' first-run case the empty setup form is for.
#' @noRd
.workflow_resolve_project <- function(project_dir = NULL, wd = getwd(),
                                      recent = .workflow_recent_projects()) {
  if (!is.null(project_dir) && nzchar(project_dir)) return(project_dir)
  if (file.exists(file.path(wd, .WORKFLOW_STATE_FILE))) return(wd)
  if (length(recent) > 0) return(recent[[1]])
  NULL
}

## The step ids, in journey order.
##
## One phase, not two. The old flow made a user carry a blueprint out to a
## chat assistant and a supplement back before anything could be built, which
## put two manual round-trips in front of the first result. A deterministic
## build needs neither, so the build comes SECOND -- you see outputs before
## you decide whether anything needs helping -- and the supplement became an
## optional loop after it: generate a draft from what the parser already
## found, edit the handful of judgements that are wrong, rebuild.
.WORKFLOW_STEPS <- c("project", "build", "supplement", "review", "results")

#' TRUE when two path strings name the same file.
#'
#' Windows spells one file two ways: `file.path()` keeps the separators it was
#' handed, while anything that has passed through `dirname()` comes back fully
#' forward-slashed. Comparing the strings would call one file two.
#' @noRd
.workflow_same_file <- function(a, b) {
  if (length(a) != 1L || length(b) != 1L) return(FALSE)
  if (is.na(a) || is.na(b)) return(FALSE)
  identical(
    normalizePath(a, winslash = "/", mustWork = FALSE),
    normalizePath(b, winslash = "/", mustWork = FALSE)
  )
}

#' Resolve the reporting event that the Review step may open.
#'
#' Current payloads explicitly identify artifacts from their own run. An
#' explicit missing `ars_json` therefore blocks Review even when an older file
#' remains at the canonical path. Payloads archived before artifact receipts
#' existed intentionally fall back to that canonical file.
#' @noRd
.workflow_review_ars_path <- function(project_dir,
                                      paths = .workflow_paths(project_dir)) {
  ars_path <- paths$ars

  if (file.exists(paths$payload)) {
    payload <- tryCatch(readRDS(paths$payload), error = function(e) e)
    if (inherits(payload, "condition")) return(NULL)

    artifacts <- payload$artifacts
    if (is.list(artifacts) && "ars_json" %in% names(artifacts)) {
      recorded <- artifacts$ars_json
      ## The receipt names this project's own reporting event, so keep the
      ## project's spelling of it and let callers compare paths as strings.
      ars_path <- if (.workflow_same_file(recorded, paths$ars)) {
        paths$ars
      } else {
        recorded
      }
    }
  }

  if (length(ars_path) != 1L || is.na(ars_path) || !nzchar(ars_path) ||
      !file.exists(ars_path)) {
    return(NULL)
  }
  ars_path
}

#' Step statuses, derived entirely from what is on disk.
#'
#' Returns one row per step: `step`, `done`, `available` (all prerequisites
#' met), and a human `detail`. Nothing is remembered in the session, so
#' closing the app loses nothing and reopening it resumes where the files say
#' the work stands.
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

  ars_done         <- file.exists(paths$ars)
  review_ars       <- .workflow_review_ars_path(project_dir, paths)
  review_available <- !is.null(review_ars)
  supplement_done  <- file.exists(paths$supplement)
  draft_done      <- file.exists(paths$draft)
  results_done    <- file.exists(paths$payload)

  build_detail <- if (!ars_done) {
    "Build the reporting event, the results, and the filled workbook."
  } else if (supplement_done) {
    "Built, using the reviewed supplement."
  } else {
    "Built deterministically."
  }

  supplement_detail <- if (supplement_done) {
    "A reviewed supplement is in place; the next build will use it."
  } else if (draft_done) {
    "Draft generated -- edit the judgements that are wrong and save it as supplement.json."
  } else {
    "Optional. Generate a draft from what the parser already found."
  }

  data.frame(
    step = .WORKFLOW_STEPS,
    done = c(project_done && inputs_ok, ars_done, supplement_done, FALSE,
             results_done),
    available = c(
      TRUE,
      project_done && inputs_ok,
      ## The supplement describes what the parser found, so it needs a parse
      ## to have happened -- which is why it follows the build rather than
      ## blocking it.
      ars_done,
      review_available,
      results_done
    ),
    detail = c(
      project_detail,
      build_detail,
      supplement_detail,
      if (review_available) "Open the reporting event in the review editor." else
        "Available after the build.",
      if (results_done) "Everything the last run produced, and what it could not."
        else "Available after the build."
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

#' Run the whole build for a project and keep its payload.
#'
#' Everything the run produced -- and everything it could not -- comes back in
#' one value (`ars_workflow_run()`), which is written to `ars/last_run.rds` so
#' the Results step can render it after the app has been closed and reopened.
#' Nothing about this is Shiny-aware, so it runs identically from a script and
#' inside a background worker.
#'
#' The supplement is used when a reviewed one exists and ignored otherwise: a
#' deterministic build is a first-class mode, not a degraded one.
#'
#' @param adam_dir Optional; without it the ARS is still built and the missing
#'   results are reported rather than treated as a failure.
#' @return The payload, invisibly.
#' @noRd
.workflow_run_build <- function(state, paths, adam_dir = NULL,
                                use_llm = FALSE, api_key = NULL,
                                log_path = NULL, on_progress = NULL) {
  supplement <- if (file.exists(paths$supplement)) paths$supplement else NULL
  payload <- ars_workflow_run(
    shell_path     = state$shell_path,
    adam_spec_path = state$adam_spec_path,
    output_dir     = dirname(paths$ars),
    adam_dir       = adam_dir %||% state$adam_dir %||% NULL,
    study_id       = state$study_id %||% "STUDY-001",
    supplement     = supplement,
    use_llm        = use_llm,
    api_key        = api_key,
    log_path       = log_path %||% NULL,
    on_progress    = on_progress
  )
  saveRDS(payload, paths$payload)
  invisible(payload)
}

#' The in-process fallback's explanation of a version skew, as one sentence.
#'
#' Pure so the wording is testable: the interesting case is `NA` (arsbridge
#' not installed at all), which `%||%` cannot catch -- `NA_character_` is
#' neither NULL nor empty, so the old message printed "the installed
#' arsbridge is NA".
#' @noRd
.workflow_skew_message <- function(installed, running) {
  installed_label <- if (is.null(installed) || is.na(installed)) {
    "absent (not installed in .libPaths())"
  } else {
    installed
  }
  sprintf(paste("Running the build in this process: the installed arsbridge",
                "is %s but this session is %s."),
          installed_label, running)
}

#' The stopApp payload that hands the built event to the editor.
#' @noRd
.workflow_handoff_payload <- function(project_dir) {
  paths <- .workflow_paths(project_dir)
  ars_path <- .workflow_review_ars_path(project_dir, paths)
  if (is.null(ars_path)) return(NULL)

  state <- .workflow_read_state(project_dir)
  list(
    action         = "edit",
    project_dir    = project_dir,
    ars_path       = ars_path,
    adam_spec_path = state$adam_spec_path %||% NULL,
    report_path    = if (file.exists(paths$report)) paths$report else NULL
  )
}
