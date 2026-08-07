## arsbridge -- app_ars_workflow.R
## ---------------------------------------------------------------------------
## The guided workflow app: one place to run the whole journey from an
## annotated shell to a reviewed reporting event.
##
##   1. Project setup        folders + shell/spec paths + study id
##   2. Instruction files    the two-phase Copilot documents + JSON schema
##   3. Phase 1 (manual)     assistant returns tlf_extraction_blueprints.json
##   4. Phase 2 (manual)     assistant returns supplement.json (validated here)
##   5. Build                spec_to_ars() -> ars/reporting_event.json
##   6. Review & edit        hand off to the existing editor
##
## The editor is a blocking runApp whose save flow completes after it exits,
## so this app CHAINS to it: "Open in the editor" stops the workflow with a
## payload, ars_workflow() launches edit_ars(), and when the editor closes
## the workflow relaunches with every status re-derived from disk. Closing
## the app never loses anything -- the project folder is the state.

#' Run the guided arsbridge workflow
#'
#' Opens a step-by-step app that walks one study project from an annotated
#' TLF shell to a reviewed CDISC ARS reporting event:
#'
#' 1. **Project setup** -- pick a project folder and name the annotated shell
#'    (`.docx` or `.xlsx`), the ADaM spec (`.xlsx`/`.xml`), and a study id. The
#'    folder gets a fixed layout (`copilot/`, `ars/`) and a
#'    small `arsbridge_project.json` state file. With `shinyFiles` installed
#'    each path comes with a Browse button; without it they are text fields,
#'    as before.
#' 2. **Instruction files** -- writes the two-phase Copilot instructions and
#'    the supplement JSON schema into `copilot/` (see
#'    [ars_copilot_instructions()]).
#' 3. **Phase 1 (manual)** -- upload the Phase 1 instructions, the shell, and
#'    the spec to your chat assistant; paste or upload its
#'    `tlf_extraction_blueprints.json` reply back here. A pre-flight check
#'    catches truncated or wrong-version files before they cost a Phase 2
#'    session.
#' 4. **Phase 2 (manual)** -- upload the Phase 2 instructions, the schema,
#'    the shell, and the blueprint; paste or upload `supplement.json` back.
#'    [ars_validate_supplement()] runs immediately, and any FAILs come with
#'    a paste-ready repair prompt for the assistant.
#' 5. **Build** -- runs [spec_to_ars()] with the supplement (or
#'    deterministically when you skip the Copilot phases) and reports the
#'    result: outputs, analyses, warnings, blockers, and the written files.
#' 6. **Review & edit** -- hands the built reporting event to [edit_ars()].
#'    When the editor closes, the workflow reopens with fresh statuses.
#'
#' Every step's status is derived from the files in the project folder, so
#' closing the app loses nothing: reopening the same `project_dir` resumes
#' exactly where the files stand. Steps 3 and 4 are skippable -- without a
#' supplement the build runs in deterministic mode.
#'
#' @param project_dir Path to the project folder. `NULL` (default) works out
#'   which project you meant: the working directory if you are standing in a
#'   project, otherwise the last project opened. Only a genuine first run
#'   starts on an empty setup form. Projects opened before are also offered in
#'   a dropdown, so switching between studies does not mean retyping four
#'   paths; that list is kept in `tools::R_user_dir("arsbridge", "config")`
#'   and holds folder paths and nothing else.
#'
#' @return Invisibly, the project directory.
#'
#' @seealso [spec_to_ars()], [ars_copilot_instructions()],
#'   [ars_validate_supplement()], [edit_ars()].
#'
#' @examplesIf interactive()
#' ars_workflow()                 # start fresh
#' ars_workflow("~/my_study")     # resume an existing project
#' @export
ars_workflow <- function(project_dir = NULL) {
  rlang::check_installed(c("shiny", "bslib", "DT"),
                         reason = "to open the arsbridge workflow")
  ## No argument does not mean "start from nothing": if you are standing in a
  ## project, or opened one last time, that is the one you meant.
  project_dir <- .workflow_resolve_project(project_dir)
  if (!is.null(project_dir) && nzchar(project_dir) && dir.exists(project_dir)) {
    project_dir <- normalizePath(project_dir)
    .workflow_remember_project(project_dir)
  }

  repeat {
    result <- shiny::runApp(.ars_workflow_app(project_dir))
    if (is.null(result) || !identical(result$action, "edit")) {
      if (!is.null(result$project_dir)) project_dir <- result$project_dir
      break
    }
    project_dir <- result$project_dir
    edit_ars(
      result$ars_path,
      adam_spec_path = result$adam_spec_path,
      report_path    = result$report_path,
      output_path    = result$ars_path
    )
    ## The editor has closed (saved or not); relaunch the workflow so the
    ## user lands back on the journey with statuses re-read from disk.
  }
  invisible(project_dir)
}

#' @noRd
## ---------------------------------------------------------------------------
## Running the build off the UI's process
## ---------------------------------------------------------------------------

#' Start the build in a background process, or NULL when that is not possible.
#'
#' callr is an Imports since 0.1.0.9061, so its gate below is near-unreachable
#' (a broken library only); the app still degrades to synchronous rather than
#' refusing if it ever fires.
#' Returning NULL rather than aborting is deliberate -- a frozen UI is worse
#' than a responsive one, and much better than not being able to build.
#'
#' The worker is a FRESH R process, so it inherits no options and no
#' environment. Everything the run needs is passed explicitly -- which is why
#' `ars_workflow_run()` takes them as arguments rather than reading them.
#' @noRd
.workflow_start_build <- function(state, paths, use_llm = FALSE) {
  ## Every road to the synchronous fallback says so. Three of the four used
  ## to be silent, and a user staring at a frozen UI could not learn why --
  ## or that installing one package would fix it.
  if (!isTRUE(getOption("arsbridge.workflow_background", TRUE))) {
    shiny::showNotification(
      paste("Background builds are disabled (arsbridge.workflow_background",
            "= FALSE); building in this process."),
      type = "message", duration = 4)
    return(NULL)
  }
  if (!requireNamespace("callr", quietly = TRUE)) {
    shiny::showNotification(
      paste("callr is not installed, so the build runs in this process and",
            "the UI will pause until it finishes. install.packages(\"callr\")",
            "and restart for a responsive build with a live log."),
      type = "warning", duration = 10)
    return(NULL)
  }

  ## The worker loads the INSTALLED arsbridge, not the one running this app.
  ## In production they are the same package; in development they are not, and
  ## a silent version skew means the app builds with code you are not looking
  ## at. Fall back to running in-process, which at least uses the right code.
  running   <- as.character(utils::packageVersion("arsbridge"))
  installed <- tryCatch(
    as.character(utils::packageVersion("arsbridge", lib.loc = .libPaths())),
    error = function(e) NA_character_)
  if (is.na(installed) || !identical(installed, running)) {
    shiny::showNotification(.workflow_skew_message(installed, running),
                            type = "warning", duration = 8)
    return(NULL)
  }
  supplement <- if (file.exists(paths$supplement)) paths$supplement else NULL
  tryCatch(
    callr::r_bg(
      ## payload_path is the worker's own business, not an argument of
      ## ars_workflow_run() -- it is where the worker leaves the payload so
      ## the Results step can find it after a restart.
      ##
      ## The progress emitter is resolved inside the worker, from ITS
      ## namespace: passing the function object would serialize it, and the
      ## version-skew gate above already guarantees the two namespaces match.
      ## Its lines go to stdout, which the redirect below turns into run.log.
      func = function(args, payload_path) {
        args$on_progress <- asNamespace("arsbridge")$.progress_emit_line
        payload <- do.call(arsbridge::ars_workflow_run, args)
        saveRDS(payload, payload_path)
        payload
      },
      args = list(payload_path = paths$payload, args = list(
        shell_path     = state$shell_path,
        adam_spec_path = state$adam_spec_path,
        output_dir     = dirname(paths$ars),
        adam_dir       = state$adam_dir %||% NULL,
        study_id       = state$study_id %||% "STUDY-001",
        supplement     = supplement,
        use_llm        = use_llm,
        log_path       = paths$run_log
      )),
      stdout = paths$run_log, stderr = "2>&1",
      supervise = TRUE),
    error = function(e) {
      shiny::showNotification(
        paste("Could not start the background build:", conditionMessage(e),
              "-- building in this process."),
        type = "warning", duration = 8)
      NULL
    })
}

#' The current progress of a background build, read back from its run log.
#'
#' The worker's only channel to the app is the log callr builds from its
#' stdout, so the state of the build IS the last [progress] line in it.
#'
#' `last_seen` is the log's modification time, and it is the only thing on
#' screen that separates a stage taking its time from a worker that has died.
#' The raw tail rides along for the expander -- it used to be shown under the
#' bar as the last three NON-progress lines, which with `verbose = FALSE`
#' meant it was pinned forever on the package-attach message from the
#' worker's first second, and read as evidence of a hang.
#' @noRd
.workflow_read_progress <- function(log_path) {
  if (is.null(log_path) || !file.exists(log_path)) return(NULL)
  lines <- readLines(log_path, warn = FALSE)
  list(ev        = .parse_progress_line(lines),
       last_seen = file.mtime(log_path),
       tail      = utils::tail(lines, 20))
}

#' A duration a person can read at a glance.
#' @noRd
.workflow_elapsed_label <- function(seconds) {
  seconds <- max(0, round(as.numeric(seconds)))
  if (seconds < 60) return(sprintf("%ds", seconds))
  minutes <- seconds %/% 60
  if (minutes < 60) return(sprintf("%dm %02ds", minutes, seconds %% 60))
  sprintf("%dh %02dm", minutes %/% 60, minutes %% 60)
}

## How long a build may go without saying anything before the panel says so.
## Long enough that a slow stage does not trip it (an LLM pass over a big
## shell, a workbook save) and short enough that a dead worker is not mistaken
## for a working one for the rest of the afternoon.
.WORKFLOW_STALL_SECONDS <- 180

#' What the panel says beneath the bar: how long this build has been going,
#' when it last reported anything, and -- past the stall threshold -- that it
#' may be stuck.
#'
#' Pure, so the wording is testable without a session or a clock.
#' @noRd
.workflow_liveness_ui <- function(info, started, now = Sys.time()) {
  if (is.null(started)) return(NULL)
  elapsed <- .workflow_elapsed_label(difftime(now, started, units = "secs"))
  quiet <- if (!is.null(info$last_seen) && !is.na(info$last_seen)) {
    as.numeric(difftime(now, info$last_seen, units = "secs"))
  } else {
    NA_real_
  }
  stalled <- !is.na(quiet) && quiet > .WORKFLOW_STALL_SECONDS
  shiny::div(
    class = if (stalled) "mt-1 text-warning-emphasis" else "mt-1 text-muted",
    sprintf("Running for %s.", elapsed),
    if (!is.na(quiet)) {
      sprintf(" Last reported %s ago.", .workflow_elapsed_label(quiet))
    },
    if (stalled) {
      shiny::span(" This build may be stuck -- cancel it and try again.")
    })
}

#' What one progress event says it is doing, as a phrase.
#'
#' The label names what is happening NOW; the count is of what is already
#' finished. Saying "done" is what keeps "T_14_1_5 (3 of 4 done)" from reading
#' as a contradiction. A named sub-step ("saving the workbook") carries no
#' count, and used to be dropped entirely by the `n > 0` gate -- which is why
#' the slowest part of a fill had nothing to show for itself.
#' @noRd
.workflow_progress_detail <- function(ev) {
  if (is.na(ev$label %||% NA_character_)) return("")
  ## A stage of one has nothing to count, and "(1 of 1 done)" beside a phrase
  ## like "writing the results" is noise.
  if ((ev$n %||% 0) > 1) {
    sprintf("%s (%d of %d done)", ev$label, ev$i, ev$n)
  } else {
    ev$label
  }
}

#' The build panel's progress block, as a tag: bar when an event is known,
#' spinner while waiting for the first one.
#' @noRd
.workflow_progress_ui <- function(info, started = NULL, now = Sys.time()) {
  ev <- info$ev
  live <- .workflow_liveness_ui(info, started, now)
  log_details <- if (length(info$tail %||% character()) > 0) {
    shiny::tags$details(
      class = "mt-1",
      shiny::tags$summary(class = "text-muted", "Run log"),
      shiny::pre(class = "mb-0 mt-1 small text-muted",
                 paste(info$tail, collapse = "\n")))
  }
  if (is.null(ev)) {
    return(shiny::div(
      class = "border rounded p-2 small bg-light",
      shiny::div(class = "spinner-border spinner-border-sm me-2"),
      shiny::span("Running in a background process..."),
      live, log_details))
  }
  pct <- round(100 * .progress_fraction(ev))
  detail <- .workflow_progress_detail(ev)
  shiny::div(
    class = "border rounded p-2 small bg-light",
    shiny::div(sprintf("%s%s", .progress_stage_label(ev$stage),
                       if (nzchar(detail)) paste0(" -- ", detail) else "")),
    shiny::div(
      class = "progress mt-1", style = "height: 6px;",
      shiny::div(class = "progress-bar", role = "progressbar",
                 style = sprintf("width: %d%%;", pct))),
    live, log_details)
}

#' What to announce for a finished build's payload.
#'
#' Pure, so the choice of words is testable without a Shiny session. The
#' unfilled case outranks "partial": a workbook that looks identical to the
#' shell is the one outcome a user will misread as success.
#' @noRd
.workflow_build_notice <- function(payload) {
  status <- payload$status %||% "success"
  if (identical(status, "error")) {
    return(list(
      text = paste("Build failed at", payload$failed_stage %||% "an early stage."),
      type = "warning"))
  }
  n_unfilled <- if (is.null(payload$unfilled_cells)) 0L
                else nrow(payload$unfilled_cells)
  fill <- payload$fill
  if (!is.null(fill) && (fill$filled %||% 1) == 0 && n_unfilled > 0) {
    return(list(
      text = "Build complete, but the workbook was left unfilled -- see Results for why.",
      type = "warning"))
  }
  ## The fill never RAN, on a run that should have produced it: an Excel
  ## shell with data, but the ARD came back empty (a fully clean shell does
  ## this -- zero analyses execute, ars_to_ard() returns NULL, and stage 3
  ## is skipped). Without this branch that run announces plain success.
  if (is.null(fill) && .workflow_fill_expected(payload)) {
    return(list(
      text = "Build complete, but no results were computed, so the workbook was not filled -- see Results for why.",
      type = "warning"))
  }
  if (identical(status, "partial")) {
    return(list(text = "Build complete, with findings -- see Results.",
                type = "warning"))
  }
  list(text = "Build complete.", type = "message")
}

#' Should this run have produced a filled workbook? From the payload alone.
#' @noRd
.workflow_fill_expected <- function(payload) {
  meta <- payload$metadata
  grepl("\\.xlsx$", meta$shell_path %||% "", ignore.case = TRUE) &&
    !is.na(meta$adam_dir %||% NA_character_)
}

#' Why a background build ended with no result, said with somewhere to look.
#'
#' A worker that dies before it can return leaves its reason in the run log
#' and nowhere else, so a message that does not name the log leaves the user
#' with "Build failed: callr subprocess failed" and no next step.
#' @noRd
.workflow_worker_message <- function(err, log_path) {
  text <- conditionMessage(err)
  if (is.null(log_path) || !file.exists(log_path %||% "")) return(text)
  paste0(text, " The worker's own output is in ", log_path, ".")
}

#' Common handling for a finished build, however it ran.
#' @noRd
.workflow_finish_build <- function(res, state, bump) {
  state$started(NULL)
  if (inherits(res, "error") || inherits(res, "condition")) {
    shiny::showNotification(paste("Build failed:", conditionMessage(res)),
                            type = "error", duration = 10)
    bump()
    return(invisible(NULL))
  }
  state$last_result(res)
  bump()
  notice <- .workflow_build_notice(res)
  shiny::showNotification(notice$text, type = notice$type, duration = 8)
  bslib::accordion_panel_open("wizard", "panel_results")
  invisible(res)
}

#' The payload of the last run: this session's if there was one, otherwise
#' whatever the project has on disk -- so Results survives a restart.
#' @noRd
.workflow_last_payload <- function(paths, state) {
  live <- state$last_result()
  if (!is.null(live)) return(live)
  if (is.null(paths) || !file.exists(paths$payload)) return(NULL)
  tryCatch(readRDS(paths$payload), error = function(e) NULL)
}

#' The one-line verdict on a run.
#' @noRd
.workflow_summary_ui <- function(payload) {
  status <- payload$status %||% "success"
  tone <- switch(status, success = "alert-success", partial = "alert-warning",
                 "alert-danger")
  fails <- sum((payload$diagnostics$severity %||% character()) %in% "FAIL")
  seconds <- payload$timings$total %||% NA_real_
  shiny::div(
    class = paste("alert py-1 px-2 small mb-0 mt-2", tone),
    shiny::strong(toupper(status)), " -- ",
    if (!is.na(seconds)) sprintf("%.1fs. ", seconds) else "",
    if (fails > 0) sprintf("%d blocking finding(s). ", fails) else "",
    if (!is.null(payload$error)) payload$error else "",
    if (identical(status, "success") && fails == 0) "Nothing was left undone." else ""
  )
}

.workflow_app_state <- function(project_dir) {
  list(
    project_dir         = shiny::reactiveVal(project_dir),
    nonce               = shiny::reactiveVal(0L),
    last_result         = shiny::reactiveVal(NULL),
    supplement_findings = shiny::reactiveVal(NULL),
    ## The background build: whether one is in flight, its handle, and when
    ## it started -- elapsed time is the one signal that keeps moving no
    ## matter how long a stage stays silent.
    running             = shiny::reactiveVal(FALSE),
    job                 = shiny::reactiveVal(NULL),
    started             = shiny::reactiveVal(NULL),
    ## The last progress event seen, plus the raw log tail behind it. In
    ## background mode the poller refreshes it; in-process it is set by the
    ## callback (for tests and the final state -- live repainting there goes
    ## through shiny::setProgress, because no reactive flush happens while
    ## the observer blocks).
    progress            = shiny::reactiveVal(NULL)
  )
}

#' One status badge.
#' @noRd
.workflow_badge <- function(done, available) {
  if (isTRUE(done)) {
    shiny::span(class = "badge text-bg-success", "Done")
  } else if (isTRUE(available)) {
    shiny::span(class = "badge text-bg-primary", "Ready")
  } else {
    shiny::span(class = "badge text-bg-secondary", "Waiting")
  }
}

#' Can this session offer a file browser?
#'
#' shinyFiles is a Suggests. Without it every path field is what it always
#' was, a text box you type into -- the app must not need a package it does
#' not depend on in order to open.
#' @noRd
.workflow_pickers_available <- function() {
  requireNamespace("shinyFiles", quietly = TRUE)
}

#' The roots a picker may browse from.
#'
#' Home first, because it is where a study folder almost always is, then
#' whatever the platform calls its volumes -- drive letters on Windows,
#' mounted volumes on macOS. `getVolumes()` returns a FUNCTION, hence the
#' second pair of parentheses.
#' @noRd
.workflow_picker_roots <- function() {
  volumes <- tryCatch(shinyFiles::getVolumes()(), error = function(e) NULL)
  roots <- c(Home = path.expand("~"), volumes %||% character())
  roots[!duplicated(names(roots))]
}

#' A path field, with a Browse button beside it when there is one.
#' @noRd
.workflow_path_field <- function(field, button = NULL) {
  if (is.null(button)) return(field)
  shiny::div(
    class = "d-flex gap-2 align-items-end",
    shiny::div(class = "flex-grow-1", field),
    ## The margin matches the text input's, so the button sits on its
    ## baseline rather than below it.
    shiny::div(class = "mb-3", button))
}

#' Step 1, as a tag.
#'
#' Its own function because it is the one panel whose CONTENT is the thing
#' being got right -- every field arrives carrying the project's recorded
#' value, and each path field carries a Browse button when there is one to
#' give it. A tag is testable; the panel inside a `shinyApp()` is not.
#' @noRd
.workflow_project_panel <- function(project_dir, initial_state, pickers) {
  browse_dir <- function(id, title) {
    if (!pickers) return(NULL)
    shinyFiles::shinyDirButton(id, "Browse", title,
                               class = "btn-outline-secondary btn-sm")
  }
  browse_file <- function(id, title) {
    if (!pickers) return(NULL)
    shinyFiles::shinyFilesButton(id, "Browse", title, multiple = FALSE,
                                 class = "btn-outline-secondary btn-sm")
  }

  bslib::accordion_panel(
    title = "1. Project setup", value = "panel_project",
    shiny::uiOutput("badge_project"),
    shiny::uiOutput("project_resumed"),
    shiny::uiOutput("recent_projects"),
    .workflow_path_field(
      shiny::textInput("project_dir", "Project folder",
                       value = project_dir %||% "", width = "100%"),
      browse_dir("pick_project_dir", "Choose the project folder")),
    .workflow_path_field(
      shiny::textInput("shell_path", "Annotated TLF shell (.docx or .xlsx)",
                       value = initial_state$shell_path %||% "",
                       width = "100%"),
      browse_file("pick_shell", "Choose the annotated TLF shell")),
    .workflow_path_field(
      shiny::textInput("spec_path", "ADaM specification (.xlsx / .xml)",
                       value = initial_state$adam_spec_path %||% "",
                       width = "100%"),
      browse_file("pick_spec", "Choose the ADaM specification")),
    .workflow_path_field(
      shiny::textInput("adam_dir", "ADaM dataset folder (optional)",
                       value = initial_state$adam_dir %||% "",
                       width = "100%"),
      browse_dir("pick_adam_dir", "Choose the ADaM dataset folder")),
    shiny::textInput("study_id", "Study id",
                     value = initial_state$study_id %||% "STUDY-001"),
    shiny::actionButton("init_project", "Create / update project",
                        class = "btn-primary btn-sm"),
    shiny::div(class = "text-muted small mt-2",
               "The folder gets supplement/ and ars/ subfolders plus ",
               "arsbridge_project.json. Reopening the same folder ",
               "resumes where the files say the work stands. Without an ",
               "ADaM folder the reporting event is still built; the ",
               "results and the filled workbook are not."),
    if (!pickers) {
      shiny::div(class = "text-muted small mt-1",
                 "Install ", shiny::tags$code("shinyFiles"),
                 " to browse for these paths instead of typing them.")
    }
  )
}

#' A copyable checklist of files to upload to the chat assistant.
#' @noRd
.workflow_upload_list <- function(title, paths) {
  paths <- Filter(function(p) !is.null(p) && nzchar(p), paths)
  shiny::div(
    class = "border rounded p-2 mb-3 small",
    shiny::div(class = "fw-bold mb-1", title),
    shiny::tags$ol(
      class = "mb-0",
      lapply(paths, function(p) shiny::tags$li(shiny::tags$code(p)))
    )
  )
}

#' @noRd
.ars_workflow_app <- function(project_dir = NULL) {
  initial_state <- if (!is.null(project_dir)) {
    .workflow_read_state(project_dir)
  }
  initial_status <- if (!is.null(initial_state)) {
    .workflow_status(project_dir)
  }
  ## Decided once, at build time: the UI and the server must agree on whether
  ## the picker inputs exist at all.
  pickers <- .workflow_pickers_available()
  ## What the app can offer to reopen, read before the session starts so the
  ## list does not shift under a select input mid-use.
  recent <- .workflow_recent_projects()
  first_open <- if (is.null(initial_status)) {
    "project"
  } else {
    pending <- initial_status$step[initial_status$available &
                                     !initial_status$done]
    if (length(pending) > 0) {
      pending[1]
    } else if (isTRUE(initial_status$done[initial_status$step == "build"])) {
      "review"
    } else {
      "project"
    }
  }

  ui <- bslib::page_fillable(
    title = "arsbridge workflow",
    theme = bslib::bs_theme(version = 5),

    shiny::div(
      class = "d-flex justify-content-between align-items-center my-2",
      shiny::h4(class = "mb-0", "arsbridge workflow"),
      shiny::div(
        class = "d-flex gap-2",
        shiny::uiOutput("project_label", inline = TRUE),
        shiny::actionButton("quit_workflow", "Close", class = "btn-sm")
      )
    ),

    bslib::accordion(
      id = "wizard",
      open = paste0("panel_", first_open),
      multiple = FALSE,

      .workflow_project_panel(project_dir, initial_state, pickers),

      bslib::accordion_panel(
        title = "2. Build", value = "panel_build",
        shiny::uiOutput("badge_build"),
        shiny::uiOutput("build_mode"),
        shiny::uiOutput("build_actions"),
        shiny::uiOutput("build_progress"),
        shiny::uiOutput("build_summary")
      ),

      bslib::accordion_panel(
        title = "3. Supplement (optional)", value = "panel_supplement",
        shiny::uiOutput("badge_supplement"),
        shiny::p(class = "small",
                 "When the build got something wrong -- or when the shell ",
                 "carries no annotations at all: a reviewed supplement can ",
                 "declare the bindings for a clean shell. Generate a draft ",
                 "from what the parser already found, correct or add the ",
                 "judgements, save it as ",
                 shiny::tags$code("supplement.json"), " and rebuild."),
        shiny::uiOutput("supplement_actions"),
        shiny::uiOutput("supplement_paths"),
        DT::DTOutput("supplement_findings")
      ),

      bslib::accordion_panel(
        title = "4. Review & edit", value = "panel_review",
        shiny::uiOutput("badge_review"),
        shiny::uiOutput("review_paths"),
        shiny::uiOutput("review_actions"),
        shiny::div(class = "text-muted small mt-2",
                   "The editor opens in this window's place; when you close ",
                   "it, the workflow returns with fresh statuses.")
      ),

      bslib::accordion_panel(
        title = "5. Results", value = "panel_results",
        shiny::uiOutput("badge_results"),
        shiny::uiOutput("results_artifacts"),
        shiny::uiOutput("results_fill_rollup"),
        shiny::p(class = "small mt-2 mb-1",
                 shiny::strong("What the run could not do."),
                 " Everything arsbridge declined to guess at, with the ",
                 "reason and the fix."),
        DT::DTOutput("results_diagnostics"),
        shiny::uiOutput("results_unfilled_intro"),
        DT::DTOutput("results_unfilled"),
        shiny::uiOutput("results_reasons_intro"),
        DT::DTOutput("results_reasons"),
        shiny::uiOutput("results_pending")
      )
    )
  )

  server <- function(input, output, session) {
    state <- .workflow_app_state(project_dir)
    bump <- function() state$nonce(state$nonce() + 1L)

    current_dir <- shiny::reactive({
      dir <- state$project_dir()
      if (is.null(dir) || !nzchar(dir)) NULL else dir
    })
    meta <- shiny::reactive({
      state$nonce()
      dir <- current_dir()
      if (is.null(dir)) NULL else .workflow_read_state(dir)
    })
    paths_r <- shiny::reactive({
      dir <- current_dir()
      if (is.null(dir)) NULL else .workflow_paths(dir)
    })
    status <- shiny::reactive({
      state$nonce()
      dir <- current_dir()
      if (is.null(dir)) NULL else .workflow_status(dir)
    })
    step_row <- function(step) {
      st <- status()
      if (is.null(st)) return(list(done = FALSE, available = step == "project"))
      as.list(st[st$step == step, , drop = FALSE])
    }

    output$project_label <- shiny::renderUI({
      dir <- current_dir()
      if (is.null(dir)) return(NULL)
      shiny::span(class = "text-muted small align-self-center",
                  shiny::tags$code(dir))
    })

    ## --- badges -------------------------------------------------------
    for (step in .WORKFLOW_STEPS) {
      local({
        this_step <- step
        output[[paste0("badge_", this_step)]] <- shiny::renderUI({
          row <- step_row(this_step)
          done <- isTRUE(row$done)
          ## Phase 2 counts as done only when the received supplement has no
          ## FAILs -- the file existing is not enough to move on.
          if (identical(this_step, "phase2") && done) {
            findings <- state$supplement_findings()
            if (!is.null(findings) && any(findings$severity == "FAIL")) {
              done <- FALSE
            }
          }
          shiny::div(class = "mb-2",
                     .workflow_badge(done, isTRUE(row$available)),
                     shiny::span(class = "text-muted small ms-2",
                                 row$detail %||% ""))
        })
      })
    }

    ## --- step 1: project ------------------------------------------------
    ##
    ## The boxes below arrive filled whenever the app could work out which
    ## project you meant. Saying so matters: a form that fills itself in and
    ## does not explain why is a form you re-read every field of.
    output$project_resumed <- shiny::renderUI({
      if (is.null(initial_state)) return(NULL)
      shiny::div(
        class = "alert alert-info py-1 px-2 small",
        "Resuming ", shiny::tags$code(project_dir),
        " -- the paths below were read from its ",
        shiny::tags$code(.WORKFLOW_STATE_FILE),
        ". Change any of them and press Create / update project.")
    })

    output$recent_projects <- shiny::renderUI({
      others <- setdiff(recent, project_dir %||% "")
      if (length(others) == 0) return(NULL)
      shiny::selectInput(
        "recent_project", "Switch to a project you opened before",
        choices = c("Choose a project" = "", stats::setNames(others, others)),
        width = "100%")
    })

    shiny::observeEvent(input$recent_project, {
      dir <- trimws(input$recent_project %||% "")
      if (!nzchar(dir)) return()
      chosen <- .workflow_read_state(dir)
      if (is.null(chosen)) {
        shiny::showNotification(
          paste0("No readable project in ", dir, " any more."),
          type = "warning", duration = 6)
        return()
      }
      shiny::updateTextInput(session, "project_dir", value = dir)
      shiny::updateTextInput(session, "shell_path",
                             value = chosen$shell_path %||% "")
      shiny::updateTextInput(session, "spec_path",
                             value = chosen$adam_spec_path %||% "")
      shiny::updateTextInput(session, "adam_dir",
                             value = chosen$adam_dir %||% "")
      shiny::updateTextInput(session, "study_id",
                             value = chosen$study_id %||% "STUDY-001")
      state$project_dir(normalizePath(dir))
      .workflow_remember_project(dir)
      bump()
      shiny::showNotification(paste("Switched to", dir), type = "message",
                              duration = 5)
    })

    ## --- the file pickers -------------------------------------------------
    ##
    ## Only wired when shinyFiles is installed; without it the fields above
    ## are plain text inputs and everything below still works. The chosen
    ## path is written back into the text input rather than kept beside it,
    ## so the text field stays the one place a path lives and nothing
    ## downstream has to know a picker exists.
    if (pickers) {
      roots <- .workflow_picker_roots()
      shinyFiles::shinyDirChoose(input, "pick_project_dir", roots = roots)
      shinyFiles::shinyDirChoose(input, "pick_adam_dir", roots = roots)
      shinyFiles::shinyFileChoose(input, "pick_shell", roots = roots,
                                  filetypes = c("docx", "xlsx"))
      shinyFiles::shinyFileChoose(input, "pick_spec", roots = roots,
                                  filetypes = c("xlsx", "xml"))

      observe_dir_pick <- function(pick_id, field_id) {
        shiny::observeEvent(input[[pick_id]], {
          picked <- shinyFiles::parseDirPath(roots, input[[pick_id]])
          if (length(picked) == 1 && nzchar(picked)) {
            shiny::updateTextInput(session, field_id,
                                   value = as.character(picked))
          }
        })
      }
      observe_file_pick <- function(pick_id, field_id) {
        shiny::observeEvent(input[[pick_id]], {
          picked <- shinyFiles::parseFilePaths(roots, input[[pick_id]])
          if (nrow(picked) == 1) {
            shiny::updateTextInput(session, field_id,
                                   value = as.character(picked$datapath[[1]]))
          }
        })
      }
      observe_dir_pick("pick_project_dir", "project_dir")
      observe_dir_pick("pick_adam_dir", "adam_dir")
      observe_file_pick("pick_shell", "shell_path")
      observe_file_pick("pick_spec", "spec_path")
    }

    shiny::observeEvent(input$init_project, {
      dir <- trimws(input$project_dir %||% "")
      if (!nzchar(dir)) {
        shiny::showNotification("Name a project folder.", type = "warning")
        return()
      }
      init <- tryCatch(
        .workflow_init(dir, input$shell_path, input$spec_path,
                       input$study_id, adam_dir = input$adam_dir),
        error = function(e) e
      )
      if (inherits(init, "error")) {
        shiny::showNotification(conditionMessage(init), type = "error",
                                duration = 8)
        return()
      }
      state$project_dir(normalizePath(dir))
      .workflow_remember_project(dir)
      bump()
      shiny::showNotification("Project ready.", type = "message", duration = 4)
      bslib::accordion_panel_open("wizard", "panel_build")
    })

    ## --- step 3: supplement (optional, AFTER the build) -------------------
    ##
    ## The old flow put two manual chat round-trips in front of the first
    ## build. This one runs the build first and offers a draft supplement
    ## generated from what the parser already found -- so the user corrects
    ## specific judgements rather than authoring a document from scratch.
    output$supplement_actions <- shiny::renderUI({
      state$nonce()
      if (!isTRUE(step_row("supplement")$available)) {
        return(shiny::div(class = "text-muted small",
                          "Available once there is a build to correct."))
      }
      shiny::div(
        shiny::actionButton("write_instructions", "Write assistant instructions",
                            class = "btn-outline-secondary btn-sm me-1"),
        shiny::actionButton("generate_draft", "Generate draft supplement",
                            class = "btn-primary btn-sm me-1"),
        shiny::actionButton("validate_supplement", "Validate supplement.json",
                            class = "btn-outline-secondary btn-sm")
      )
    })

    output$supplement_paths <- shiny::renderUI({
      state$nonce()
      paths <- paths_r()
      if (is.null(paths)) return(NULL)
      line <- function(label, path) {
        if (!file.exists(path)) return(NULL)
        shiny::div(class = "small", label, ": ", shiny::tags$code(path))
      }
      shiny::div(
        class = "mt-2",
        line("Instructions", paths$instructions),
        line("Schema", paths$schema),
        line("Draft", paths$draft),
        line("Reviewed supplement", paths$supplement),
        if (file.exists(paths$draft) && !file.exists(paths$supplement)) {
          shiny::div(class = "alert alert-info py-1 px-2 mt-2 mb-0 small",
                     "Edit the draft, then save it as ",
                     shiny::tags$code(basename(paths$supplement)),
                     " in the same folder and rebuild.")
        }
      )
    })

    shiny::observeEvent(input$write_instructions, {
      paths <- paths_r()
      if (is.null(paths)) return()
      done <- tryCatch({
        suppressMessages(ars_copilot_instructions(
          dir = dirname(paths$instructions), workflow = "single",
          open = FALSE, overwrite = TRUE))
        TRUE
      }, error = function(e) e)
      if (inherits(done, "error")) {
        shiny::showNotification(conditionMessage(done), type = "error",
                                duration = 8)
        return()
      }
      bump()
      shiny::showNotification("Instructions and schema written.",
                              type = "message", duration = 4)
    })

    shiny::observeEvent(input$generate_draft, {
      paths <- paths_r()
      m <- meta()
      if (is.null(paths) || is.null(m)) return()
      done <- tryCatch(
        suppressMessages(suppressWarnings(write_supplement_draft(
          shell_path     = m$shell_path,
          adam_spec_path = m$adam_spec_path,
          output_path    = paths$draft))),
        error = function(e) e
      )
      if (inherits(done, "error")) {
        shiny::showNotification(paste("Could not draft:",
                                      conditionMessage(done)),
                                type = "error", duration = 10)
        return()
      }
      bump()
      shiny::showNotification("Draft written -- edit it, save it as supplement.json, rebuild.",
                              type = "message", duration = 8)
    })

    shiny::observeEvent(input$validate_supplement, {
      paths <- paths_r()
      m <- meta()
      if (is.null(paths) || is.null(m)) return()
      if (!file.exists(paths$supplement)) {
        shiny::showNotification("No supplement.json in the project yet.",
                                type = "warning", duration = 6)
        return()
      }
      findings <- tryCatch(
        suppressMessages(ars_validate_supplement(
          paths$supplement, adam_spec_path = m$adam_spec_path)),
        error = function(e) {
          data.frame(severity = "FAIL", tlf = NA_character_, where = "file",
                     problem = conditionMessage(e), stringsAsFactors = FALSE)
        })
      state$supplement_findings(findings)
      bump()
      n_fail <- sum(findings$severity == "FAIL")
      shiny::showNotification(
        if (n_fail > 0) paste(n_fail, "FAIL finding(s) -- fix them before rebuilding.")
        else "Supplement validates; rebuild to use it.",
        type = if (n_fail > 0) "error" else "message", duration = 6)
    })

    output$supplement_findings <- DT::renderDT({
      state$nonce()
      findings <- state$supplement_findings()
      if (is.null(findings) || nrow(findings) == 0) return(NULL)
      DT::datatable(findings, rownames = FALSE, options = list(
        pageLength = 5, dom = "tp", scrollX = TRUE))
    })

    ## --- step 2: build ----------------------------------------------------
    ##
    ## The build runs in a BACKGROUND PROCESS. It takes seconds on a small
    ## study and minutes on a real one with an LLM pass, and a Shiny app that
    ## runs that on its own process is frozen for the duration -- the user
    ## cannot tell a slow build from a hung one, and cannot cancel either.
    ##
    ## callr rather than a persistent worker pool: a build is one long job,
    ## not many small ones, so start-up cost is noise against it, and a fresh
    ## process each time means no state survives from the previous run --
    ## which is worth having when the output is a regulatory deliverable.
    ## `ars_workflow_run()` was written for this: paths in, one value out,
    ## never throws. Swapping the engine touches only this block.
    output$build_mode <- shiny::renderUI({
      state$nonce()
      paths <- paths_r()
      m <- meta()
      if (is.null(paths)) return(NULL)
      has_data <- !is.null(m$adam_dir) && nzchar(m$adam_dir %||% "") &&
        dir.exists(m$adam_dir %||% "")
      shiny::div(
        class = "small",
        shiny::p(class = "mb-1",
                 "Mode: ",
                 shiny::strong(if (file.exists(paths$supplement)) "supplement"
                               else "deterministic"),
                 if (file.exists(paths$supplement))
                   " -- the reviewed supplement will drive the extraction."
                 else
                   " -- the annotation pass runs alone. Add a supplement later and rebuild."),
        if (!has_data) {
          shiny::div(class = "alert alert-warning py-1 px-2 mb-0",
                     "No ADaM folder recorded: the reporting event is built, ",
                     "but not the results or the filled workbook.")
        }
      )
    })

    output$build_actions <- shiny::renderUI({
      state$nonce()
      if (!isTRUE(step_row("build")$available)) {
        return(shiny::div(class = "text-muted small",
                          "Record the shell and the ADaM spec first."))
      }
      ## A running build always comes with a way out. Without one, `running`
      ## had a single exit -- the poller watching the process die -- and any
      ## run that outlived the user's patience, or any poll that threw, left
      ## the panel showing a disabled "Building..." for the rest of the
      ## session. A supplement dropped in at that point could never be built
      ## with, which is the whole point of the supplement loop.
      if (isTRUE(state$running())) {
        return(shiny::div(
          class = "d-flex gap-2",
          shiny::actionButton("run_build", "Building...", class = "btn-sm",
                              disabled = TRUE),
          shiny::actionButton("cancel_build", "Cancel",
                              class = "btn-outline-danger btn-sm")))
      }
      shiny::actionButton(
        "run_build",
        if (isTRUE(step_row("build")$done)) "Rebuild" else "Build",
        class = "btn-primary btn-sm")
    })

    ## Progress renders from state$progress, which the background poller
    ## refreshes from run.log. (In-process, no reactive flush happens while
    ## the build blocks its observer, so this never paints mid-run -- the
    ## live channel there is shiny::setProgress, which writes the websocket
    ## immediately. This block is the background mode's display.)
    output$build_progress <- shiny::renderUI({
      if (!isTRUE(state$running())) return(NULL)
      .workflow_progress_ui(state$progress(), state$started())
    })

    shiny::observeEvent(input$run_build, {
      paths <- paths_r()
      m <- meta()
      if (is.null(paths) || is.null(m) || isTRUE(state$running())) return()

      dir.create(dirname(paths$run_log), recursive = TRUE, showWarnings = FALSE)
      state$progress(NULL)   # last run's bar must not front-run this one
      state$started(Sys.time())
      state$running(TRUE)
      handle <- .workflow_start_build(m, paths)
      if (is.null(handle)) {
        ## No background engine available: run in-process rather than refuse.
        ## A frozen UI is worse than a responsive one, but it is much better
        ## than not being able to build at all. Progress flows through
        ## setProgress: it reaches the browser immediately even from inside
        ## this blocking observer, which a reactiveVal cannot -- do not
        ## "simplify" it away in favour of state$progress.
        res <- tryCatch({
          progress_cb <- function(ev) {
            state$progress(list(ev = ev, last_seen = Sys.time(),
                                tail = character()))
            detail <- .workflow_progress_detail(ev)
            tryCatch(shiny::setProgress(
              value = .progress_fraction(ev),
              message = .progress_stage_label(ev$stage),
              detail = if (nzchar(detail)) detail), error = function(e) NULL)
          }
          shiny::withProgress(
            message = "Building (in this process)...", value = 0,
            suppressMessages(suppressWarnings(
              .workflow_run_build(m, paths, on_progress = progress_cb))))
        }, error = function(e) e)
        state$running(FALSE)
        .workflow_finish_build(res, state, bump)
        return()
      }
      state$job(handle)
    })

    ## Stop a build that is taking longer than the user is willing to wait,
    ## or one whose worker has gone quiet for good. The state it leaves is
    ## the ordinary "not running" one, so Build is immediately available
    ## again -- including with a supplement dropped in meanwhile.
    shiny::observeEvent(input$cancel_build, {
      handle <- state$job()
      if (!is.null(handle)) {
        ## The tree, not just the worker: a build can have children (a curl
        ## for an LLM call, a sourced cards script), and one left behind
        ## keeps a handle on the workbook it was writing.
        tryCatch(handle$kill_tree(),
                 error = function(e) tryCatch(handle$kill(),
                                              error = function(e) NULL))
      }
      state$running(FALSE)
      state$job(NULL)
      state$started(NULL)
      state$progress(NULL)
      bump()
      shiny::showNotification(
        paste("Build cancelled. Whatever it had already written is still on",
              "disk -- rebuild when you are ready."),
        type = "message", duration = 6)
    })

    ## Poll the background process. Nothing about the payload is recomputed
    ## here -- the worker already assembled it, including its diagnostics.
    ## Each tick also refreshes the progress display from the run log, where
    ## the worker's [progress] lines land via the stdout redirect.
    shiny::observe({
      if (!isTRUE(state$running())) return()
      handle <- state$job()
      ## Should be unreachable: run_build sets `running` and `job` in one
      ## flush. But if it ever happens, the panel shows a disabled
      ## "Building..." with no process to wait for and nothing that will ever
      ## clear it. Self-heal rather than sit there.
      if (is.null(handle)) {
        state$running(FALSE)
        state$started(NULL)
        return()
      }
      ## Registered before anything that can fail, so the next tick is
      ## already scheduled whatever the rest of this observer does.
      shiny::invalidateLater(700)

      ## A poll must never be the thing that ends polling. readLines() on a
      ## file the worker is writing can fail (Windows, most likely), and a
      ## Shiny observer that throws is DESTROYED -- after which `running`
      ## stays TRUE for the life of the session and the build panel is dead
      ## with no way back. Swallow it and try again in 700ms.
      alive <- tryCatch({
        paths <- paths_r()
        if (!is.null(paths)) {
          state$progress(.workflow_read_progress(paths$run_log))
        }
        handle$is_alive()
      }, error = function(e) NA)
      if (is.na(alive) || isTRUE(alive)) return()

      log_path <- paths_r()$run_log
      res <- tryCatch(
        handle$get_result(),
        error = function(e) simpleError(.workflow_worker_message(e, log_path)))
      state$running(FALSE)
      state$job(NULL)
      .workflow_finish_build(res, state, bump)
    })

    output$build_summary <- shiny::renderUI({
      state$nonce()
      payload <- .workflow_last_payload(paths_r(), state)
      if (is.null(payload)) return(NULL)
      .workflow_summary_ui(payload)
    })

    ## --- step 4: review hand-off -------------------------------------------
    output$review_paths <- shiny::renderUI({
      state$nonce()
      paths <- paths_r()
      if (is.null(paths) || !file.exists(paths$ars)) return(NULL)
      .workflow_upload_list("Built artifacts:", list(
        paths$ars,
        if (file.exists(paths$report)) paths$report,
        if (dir.exists(paths$code_dir)) paths$code_dir
      ))
    })

    output$review_actions <- shiny::renderUI({
      if (!isTRUE(step_row("review")$available)) {
        return(shiny::div(class = "text-muted small",
                          "Build the reporting event first."))
      }
      shiny::actionButton("open_editor", "Open in the review editor",
                          class = "btn-primary btn-sm")
    })

    shiny::observeEvent(input$open_editor, {
      dir <- current_dir()
      if (is.null(dir)) return()
      shiny::stopApp(.workflow_handoff_payload(dir))
    })

    shiny::observeEvent(input$quit_workflow, {
      shiny::stopApp(list(action = "quit", project_dir = current_dir()))
    })

    ## --- step 5: results ---------------------------------------------------
    ##
    ## Everything the run produced, and -- the part that matters -- everything
    ## it declined to produce, with the reason and the fix. The whole writer
    ## is built to report rather than guess; this is where the reporting
    ## becomes visible to someone who never reads a console.
    output$results_artifacts <- shiny::renderUI({
      state$nonce()
      payload <- .workflow_last_payload(paths_r(), state)
      if (is.null(payload)) {
        return(shiny::div(class = "text-muted small",
                          "Available after the first build."))
      }
      labels <- c(ars_json = "Reporting event", validation_report = "Validation report",
                  code_dir = "cards scripts", ard_rds = "ARD",
                  filled_workbook = "Filled workbook",
                  fill_debrief = "Fill debrief",
                  run_log = "Run log (may contain console output -- review before sharing)")
      rows <- lapply(names(labels), function(key) {
        path <- payload$artifacts[[key]] %||% NA_character_
        if (is.na(path)) {
          return(shiny::div(class = "small text-muted",
                            labels[[key]], ": not produced"))
        }
        shiny::div(class = "small", labels[[key]], ": ", shiny::tags$code(path))
      })
      shiny::div(.workflow_summary_ui(payload),
                 shiny::div(class = "mt-2", rows))
    })

    output$results_fill_rollup <- shiny::renderUI({
      state$nonce()
      payload <- .workflow_last_payload(paths_r(), state)
      if (is.null(payload)) return(NULL)
      panels <- .fill_debrief_panels(payload)
      if (is.null(panels)) {
        ## An archived run from before the census existed: say so instead
        ## of erroring or showing an empty box.
        if (!is.null(payload$fill)) {
          return(shiny::div(class = "text-muted small mt-2",
                            "Fill rollup: available after the next build."))
        }
        return(NULL)
      }
      shiny::div(
        class = "mt-2 small",
        shiny::strong("How much of each sheet filled."),
        shiny::tags$ul(lapply(panels$sheet_lines, shiny::tags$li)),
        if (length(panels$column_callouts) > 0) {
          shiny::div(
            class = "alert alert-warning small mb-2",
            shiny::strong("Columns that filled nothing:"),
            shiny::tags$ul(lapply(panels$column_callouts, shiny::tags$li)))
        }
      )
    })

    output$results_reasons_intro <- shiny::renderUI({
      state$nonce()
      payload <- .workflow_last_payload(paths_r(), state)
      panels <- .fill_debrief_panels(payload %||% list())
      if (is.null(panels) || nrow(panels$reasons) == 0) return(NULL)
      shiny::p(class = "small mt-3 mb-1",
               shiny::strong("Why, grouped."),
               " Every distinct reason across the run, how many cells it ",
               "explains, and what an author can do about it.")
    })

    output$results_reasons <- DT::renderDT({
      state$nonce()
      payload <- .workflow_last_payload(paths_r(), state)
      panels <- .fill_debrief_panels(payload %||% list())
      if (is.null(panels) || nrow(panels$reasons) == 0) return(NULL)
      DT::datatable(
        panels$reasons,
        rownames = FALSE,
        options = list(pageLength = 8, scrollX = TRUE, dom = "tp"))
    })

    output$results_diagnostics <- DT::renderDT({
      state$nonce()
      payload <- .workflow_last_payload(paths_r(), state)
      diagnostics <- payload$diagnostics
      if (is.null(diagnostics) || nrow(diagnostics) == 0) return(NULL)
      ## FAIL first: they are what stopped something happening. INFO is kept
      ## rather than filtered out -- it is where arsbridge reports the
      ## inferences it made on the user's behalf, and those are worth reading.
      order_by <- match(diagnostics$severity, c("FAIL", "WARN", "INFO"))
      diagnostics <- diagnostics[order(order_by, diagnostics$stage), , drop = FALSE]
      DT::datatable(
        diagnostics[, c("severity", "stage", "tlf_number", "location",
                        "problem", "action")],
        rownames = FALSE, filter = "top",
        options = list(pageLength = 8, scrollX = TRUE, dom = "ftp"))
    })

    output$results_unfilled_intro <- shiny::renderUI({
      state$nonce()
      payload <- .workflow_last_payload(paths_r(), state)
      unfilled <- payload$unfilled_cells
      if (is.null(unfilled) || nrow(unfilled) == 0) return(NULL)
      shiny::p(class = "small mt-3 mb-1",
               shiny::strong("Cells still showing their placeholder."),
               " Per cell, with the reason. An empty cell in a clinical ",
               "table reads as a zero, so nothing was blanked or guessed.")
    })

    output$results_unfilled <- DT::renderDT({
      state$nonce()
      payload <- .workflow_last_payload(paths_r(), state)
      unfilled <- payload$unfilled_cells
      if (is.null(unfilled) || nrow(unfilled) == 0) return(NULL)
      ## The reason is the payload's most useful column: on a run that
      ## filled nothing, its value distribution IS the diagnosis. The hint
      ## beside it is what the author does about it.
      unfilled$hint <- vapply(unfilled$reason, .fill_reason_hint,
                              character(1), USE.NAMES = FALSE)
      DT::datatable(
        unfilled[, c("sheet", "ref", "status", "reason", "hint")],
        rownames = FALSE, filter = "top",
        options = list(pageLength = 8, scrollX = TRUE, dom = "ftp"))
    })

    output$results_pending <- shiny::renderUI({
      state$nonce()
      payload <- .workflow_last_payload(paths_r(), state)
      if (is.null(payload)) return(NULL)
      unfilled <- payload$unfilled_cells
      pending  <- payload$pending
      n_unfilled <- if (is.null(unfilled)) 0L else nrow(unfilled)
      n_pending  <- if (is.null(pending)) 0L else nrow(pending)

      ## The clean-shell case, said in one box instead of scattered WARNs:
      ## the parse found stub rows but not one annotation, and the fill
      ## consequently placed nothing. Without this, the first sign a user
      ## gets is a "filled" workbook identical to the shell.
      diags <- payload$diagnostics
      no_annots <- !is.null(diags) && nrow(diags) > 0 &&
        any(grepl("no annotations were detected", diags$problem %||% ""))
      ## "Nothing filled" has two shapes: the fill ran and placed nothing,
      ## or a fully clean shell executed zero analyses so the fill never ran
      ## at all (payload$fill is NULL on a run that should have had one).
      filled_zero <-
        (!is.null(payload$fill) && (payload$fill$filled %||% 1) == 0) ||
        (is.null(payload$fill) && .workflow_fill_expected(payload))
      callout <- if (no_annots && filled_zero) {
        shiny::div(
          class = "alert alert-warning small mt-2 mb-2",
          shiny::strong("The shell carries no annotations, so nothing could be filled. "),
          "That is by design -- an unannotated shell gives the pipeline ",
          "nothing to bind, and it does not guess. Annotate the shell ",
          "(red ", shiny::tags$code("DATASET.VARIABLE"),
          " in the stub cells), or open Step 3: a reviewed supplement can ",
          "declare the bindings for a clean shell. Then rebuild.")
      }

      if (is.null(callout) && n_unfilled == 0 && n_pending == 0) return(NULL)
      shiny::div(
        class = "mt-2 small",
        callout,
        if (n_pending > 0) {
          shiny::div(sprintf(
            "%d result(s) reserved for a manual derivation (ADR 0002).",
            n_pending))
        }
      )
    })
  }

  shiny::shinyApp(ui, server)
}
