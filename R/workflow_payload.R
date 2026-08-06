## arsbridge -- workflow_payload.R
## ---------------------------------------------------------------------------
## The headless build: shell + spec + data in, one structured payload out.
##
## `ars_workflow()` is a Shiny app, and a Shiny app must not run a six-minute
## build on its own process -- the UI freezes and the user cannot tell a slow
## run from a hung one. So the build lives here instead, as a plain function
## that takes PATHS and returns a LIST, and knows nothing about Shiny. The app
## sends it to a background process (`callr`) and renders what comes back.
##
## Being callable from a fresh R process is the whole design constraint, and
## it has three consequences that are easy to get wrong:
##
##   1. NOTHING CROSSES THE PROCESS BOUNDARY BY ITSELF. Diagnostics live in a
##      package-level environment (`.diag_env`), so in a background process
##      they accumulate in the WORKER and `ars_diagnostics()` in the calling
##      session returns nothing at all. Every FAIL and WARN would vanish
##      silently. They are harvested here and returned in the payload.
##
##   2. THE STAGES RESET EACH OTHER. `spec_to_ars()` and `ars_to_ard()` each
##      call `diag_reset()` on entry, which is right when they are called on
##      their own and wrong in a pipeline: running them in sequence and
##      reading the diagnostics at the end returns only what happened after
##      the LAST reset. So diagnostics are taken after EACH stage and
##      accumulated. This is not a detail -- it is the difference between a
##      complete diagnostics table and one silently missing every parse and
##      build finding.
##
##   3. OPTIONS AND ENVIRONMENT DO NOT TRAVEL. A fresh process has no
##      `arsbridge.derived_dt` (so every run stamps a different time, and the
##      ARD stops being reproducible), no LLM provider option, and no API key.
##      They are parameters here, set inside the run, and echoed back in
##      `metadata` so an archived payload says what it was built with.
##
## The payload never throws. A run that dies still returns, with
## `status = "error"` and the path to its log -- which is exactly when the log
## is worth having.

## The columns every diagnostics table has, whether or not anything was
## recorded. A zero-row frame with the right shape is what lets the caller
## bind, filter and render without a special case for "nothing went wrong".
.EMPTY_DIAGNOSTICS <- function() {
  data.frame(stage = character(), severity = character(), input = character(),
             tlf_number = character(), location = character(),
             problem = character(), action = character(),
             stringsAsFactors = FALSE)
}

#' Take everything recorded so far, in the payload's shape.
#'
#' Called after each stage rather than once at the end, because the stages
#' reset the record between them (see the file header).
#' @noRd
.diag_take <- function() {
  records <- tryCatch(diag_records(), error = function(e) NULL)
  if (is.null(records) || nrow(records) == 0) return(.EMPTY_DIAGNOSTICS())
  keep <- names(.EMPTY_DIAGNOSTICS())
  for (column in setdiff(keep, names(records))) records[[column]] <- NA_character_
  records[, keep, drop = FALSE]
}

#' Run one stage: time it, keep its diagnostics, and catch its failure.
#'
#' A stage that throws does not take the run down. The pipeline stops -- later
#' stages have nothing to work from -- but the payload is still assembled, so
#' the caller learns which stage failed, with what message, and can read the
#' diagnostics that led up to it.
#' @noRd
.workflow_stage <- function(name, expr) {
  started <- Sys.time()
  failure <- NULL
  value <- withCallingHandlers(
    tryCatch(force(expr), error = function(e) {
      failure <<- conditionMessage(e)
      NULL
    }),
    warning = function(w) invokeRestart("muffleWarning"))
  list(
    name        = name,
    value       = value,
    error       = failure,
    seconds     = round(as.numeric(difftime(Sys.time(), started, units = "secs")), 3),
    diagnostics = .diag_take()
  )
}

#' Decide the run's status from what it produced.
#'
#' Deliberately not "any WARN makes it partial" -- there are dozens of WARN
#' sites and a healthy run trips several, so that rule would make every run
#' partial and the distinction useless. A run is partial when something the
#' user asked for did not happen: a FAIL was recorded, or a sheet was skipped.
#' @noRd
.payload_status <- function(diagnostics, skipped, error) {
  if (!is.null(error)) return("error")
  fails <- sum(diagnostics$severity %in% "FAIL")
  if (fails > 0 || (skipped %||% 0) > 0) return("partial")
  "success"
}

#' Build every output for a study, in one call
#'
#' Runs the whole pipeline -- `spec_to_ars()`, then `ars_to_ard()`, then
#' `ars_fill_shell()` for an Excel shell -- and returns one structured list
#' describing what it produced. Takes paths rather than data, holds no state,
#' and never throws, so it can be sent to a background process and its result
#' rendered by a UI.
#'
#' This is what `ars_workflow()`'s build step calls. It is exported because a
#' background worker has to be able to reach it (`arsbridge::ars_workflow_run`),
#' and because running the whole pipeline headlessly is useful on its own --
#' in a script, in a scheduled job, or in a validation run.
#'
#' @param shell_path Annotated TLF shell, `.docx` or `.xlsx`.
#' @param adam_spec_path ADaM spec (`.xlsx` or define.xml).
#' @param output_dir Directory to write the outputs into; created if absent.
#' @param adam_dir Directory of ADaM datasets. Without it the ARD and the
#'   filled workbook are skipped and reported, and only the ARS is built.
#' @param study_id Study identifier stamped into the reporting event.
#' @param supplement Optional Copilot supplement JSON.
#' @param use_llm Whether to call an LLM for the annotations the regex cannot
#'   resolve. `FALSE` runs deterministically.
#' @param llm_provider,api_key LLM settings, passed explicitly because a
#'   background process inherits neither the option nor the environment.
#' @param derived_dt ISO-8601 timestamp to stamp on computed ARD rows. Pass a
#'   fixed value to make a run byte-reproducible; `NULL` uses the clock.
#' @param log_path Where the run's console output was captured, if the caller
#'   redirected it. Recorded in `artifacts$run_log` so the payload can point
#'   at its own log.
#' @param on_progress Optional function called with one progress event at a
#'   time: a list with `stage`, `stage_idx`, `n_stages`, `i`, `n`, `label`.
#'   Each stage announces itself with `i = 0`, then ticks once per TLF,
#'   analysis, or sheet -- `i` counting the items already FINISHED and `label`
#'   naming the one now in flight. The work on either side of those loops
#'   (reading the inputs, writing the reporting event, saving the workbook)
#'   ticks as a named step with no count, so no stage runs out silent. Errors
#'   it raises are swallowed -- a progress bar must never take a build down.
#'   `NULL` (the default) reports nothing and changes nothing.
#'
#' @return A list with `status` (`"success"`, `"partial"` or `"error"`),
#'   `timings`, `artifacts`, `metadata`, `diagnostics` (one row per finding,
#'   with `severity` -- never split, so `INFO` findings survive),
#'   `pending` (cells reserved for manual derivation, ADR 0002),
#'   `fill` (the fill stage's headline counts -- `filled`, `pending`,
#'   `skipped` -- or `NULL` when no fill ran),
#'   `unfilled_cells` (workbook cells left showing a placeholder, and why),
#'   and `error`.
#'
#' @seealso [ars_workflow()] for the app, [spec_to_ars()] for the build step.
#' @export
#' @examples
#' \dontrun{
#'   payload <- ars_workflow_run(
#'     shell_path     = "inputs/shells.xlsx",
#'     adam_spec_path = "inputs/adam_spec.xlsx",
#'     adam_dir       = "inputs/ADaM",
#'     output_dir     = "outputs")
#'   payload$status
#'   subset(payload$diagnostics, severity == "FAIL")
#' }
ars_workflow_run <- function(shell_path, adam_spec_path, output_dir,
                             adam_dir = NULL, study_id = "STUDY-001",
                             supplement = NULL, use_llm = FALSE,
                             llm_provider = NULL, api_key = NULL,
                             derived_dt = NULL, log_path = NULL,
                             on_progress = NULL) {
  started <- Sys.time()

  ## The stage plan is known before anything runs, so the progress events can
  ## say "stage 2 of 3" truthfully. A stage that will be skipped (no data, or
  ## a Word shell) is simply not counted.
  has_data <- !is.null(adam_dir) && nzchar(adam_dir) && dir.exists(adam_dir)
  will_fill <- has_data && grepl("\\.xlsx$", shell_path, ignore.case = TRUE)
  n_stages <- 1L + as.integer(has_data) + as.integer(will_fill)

  ## Wrap one stage with the progress hook: announce entry (i = 0), then let
  ## the stage's own per-item loop tick through `.progress_tick()`, which
  ## reads the option installed here. Installed per stage and restored even
  ## when the stage dies, so an error can never leak the hook into the
  ## caller's session.
  stage_no <- 0L
  with_stage_progress <- function(stage_name, expr) {
    if (is.null(on_progress)) return(force(expr))
    stage_no <<- stage_no + 1L
    idx <- stage_no
    emit <- function(i, n, label = NA_character_) {
      tryCatch(on_progress(list(stage = stage_name, stage_idx = idx,
                                n_stages = n_stages, i = i, n = n,
                                label = label)),
               error = function(e) NULL)
    }
    emit(0L, 0L)
    old <- options(arsbridge.progress = emit)
    on.exit(options(old), add = TRUE)
    force(expr)
  }

  ## Settings a fresh process does not inherit, applied here and restored on
  ## the way out so a caller running this in-process is left as it was found.
  if (!is.null(derived_dt)) {
    old <- options(arsbridge.derived_dt = derived_dt)
    on.exit(options(old), add = TRUE)
  }
  if (!is.null(llm_provider)) {
    old_provider <- options(ars.llm.provider = llm_provider)
    on.exit(options(old_provider), add = TRUE)
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  paths <- list(
    ars_json          = file.path(output_dir, "reporting_event.json"),
    validation_report = file.path(output_dir, "validation_report.xlsx"),
    code_dir          = file.path(output_dir, "code"),
    ard_rds           = file.path(output_dir, "ard.rds"),
    filled_workbook   = file.path(output_dir, "filled_shells.xlsx")
  )

  diagnostics <- .EMPTY_DIAGNOSTICS()
  timings <- list()
  failure <- NULL
  stage_failed <- NA_character_
  ard <- NULL
  fill <- NULL

  record <- function(stage) {
    diagnostics <<- rbind(diagnostics, stage$diagnostics)
    timings[[stage$name]] <<- stage$seconds
    if (!is.null(stage$error) && is.null(failure)) {
      failure <<- stage$error
      stage_failed <<- stage$name
    }
    stage
  }

  ## 1. The reporting event.
  build <- with_stage_progress("spec_to_ars",
    record(.workflow_stage("spec_to_ars", spec_to_ars(
      shell_path     = shell_path,
      adam_spec_path = adam_spec_path,
      output_path    = paths$ars_json,
      study_id       = study_id,
      supplement     = supplement,
      report_path    = paths$validation_report,
      code_dir       = paths$code_dir,
      use_llm        = use_llm,
      api_key        = api_key,
      verbose        = FALSE))))

  ## 2. The results. Skipped, and said so, when there is no data to run
  ##    against -- which is a normal way to use the build, not an error.
  if (is.null(failure)) {
    if (is.null(adam_dir) || !nzchar(adam_dir) || !dir.exists(adam_dir)) {
      diagnostics <- rbind(diagnostics, data.frame(
        stage = "execute_ard", severity = "INFO", input = INPUT_DATA,
        tlf_number = NA_character_, location = NA_character_,
        problem = "No ADaM directory was given, so no results were computed.",
        action = "Pass adam_dir to produce the ARD and the filled workbook.",
        stringsAsFactors = FALSE))
    } else {
      ## Writing the ARD used to sit BETWEEN the stages, where neither one's
      ## progress hook was installed, so it reported nothing at all -- 6.6
      ## seconds of silence on the bundled example, the second-longest of the
      ## run after the fill itself. It belongs to the stage that produced the
      ## thing being written, so it happens inside it: the hook is live, and
      ## the stage's recorded time now includes the write it is responsible
      ## for. A write that fails is a failure OF that stage, which is also
      ## what .workflow_stage() will now record.
      run <- with_stage_progress("ars_to_ard",
        record(.workflow_stage("ars_to_ard", {
          computed <- ars_to_ard(paths$ars_json, adam_dir)
          if (!is.null(computed)) {
            .progress_tick(1L, 1L, "writing the results")
            saveRDS(computed, paths$ard_rds)
          }
          computed
        })))
      ard <- run$value
      if (is.null(ard) && is.null(failure)) {
        ## ars_to_ard() returns NULL when not one analysis executed -- on a
        ## fully clean shell, for instance. The fill guard below then skips
        ## stage 3 entirely, so no per-cell census ever exists to warn from:
        ## without these rows the payload reads "Build complete." while the
        ## deliverable the user asked for (data was given!) quietly never
        ## happened. This is the one gap the field run's own artifacts
        ## cannot explain, so the explanation must be written here.
        diagnostics <- rbind(diagnostics, data.frame(
          stage = "execute_ard", severity = "WARN", input = INPUT_ARS,
          tlf_number = NA_character_, location = NA_character_,
          problem = "The reporting event carries no executable analyses, so no results were computed.",
          action = "Usually a shell with no (or unreadable) annotations: annotate it, or declare the bindings in a reviewed supplement, then rebuild.",
          stringsAsFactors = FALSE))
        if (will_fill) {
          diagnostics <- rbind(diagnostics, data.frame(
            stage = "fill_shell", severity = "WARN", input = INPUT_SHELL,
            tlf_number = NA_character_, location = NA_character_,
            problem = "Without results, the filled workbook was not produced.",
            action = "Fix the analyses above; the fill will run on the next build.",
            stringsAsFactors = FALSE))
        }
      }
    }
  }

  ## 3. The deliverable, for an Excel shell.
  if (is.null(failure) && !is.null(ard) &&
      grepl("\\.xlsx$", shell_path, ignore.case = TRUE)) {
    filled <- with_stage_progress("ars_fill_shell",
      record(.workflow_stage("ars_fill_shell", ars_fill_shell(
        shell_path  = shell_path,
        ars         = paths$ars_json,
        ard         = ard,
        output_path = paths$filled_workbook,
        adam_dir    = adam_dir,
        overwrite   = TRUE))))
    fill <- filled$value
  }

  produced <- function(path) if (file.exists(path)) path else NA_character_
  unfilled <- if (!is.null(fill)) fill$diagnostics else
    data.frame(output_id = character(), sheet = character(), ref = character(),
               status = character(), reason = character(),
               stringsAsFactors = FALSE)

  pending <- if (!is.null(ard)) {
    tryCatch(ars_manual_worklist(ard), error = function(e) NULL)
  } else NULL

  list(
    status = .payload_status(diagnostics,
                              skipped = sum(unfilled$status %in% "skipped"),
                              error = failure),
    timings = c(timings, list(total = round(
      as.numeric(difftime(Sys.time(), started, units = "secs")), 3))),
    artifacts = list(
      ars_json          = produced(paths$ars_json),
      validation_report = produced(paths$validation_report),
      code_dir          = if (dir.exists(paths$code_dir)) paths$code_dir else NA_character_,
      ard_rds           = produced(paths$ard_rds),
      filled_workbook   = produced(paths$filled_workbook),
      run_log           = log_path %||% NA_character_
    ),
    metadata = list(
      arsbridge_version = as.character(utils::packageVersion("arsbridge")),
      study_id          = study_id,
      shell_path        = shell_path,
      adam_spec_path    = adam_spec_path,
      adam_dir          = adam_dir %||% NA_character_,
      supplement        = supplement %||% NA_character_,
      derived_dt        = derived_dt %||% NA_character_,
      llm_provider      = llm_provider %||% NA_character_,
      llm_mode_enabled  = isTRUE(use_llm)
    ),
    diagnostics    = diagnostics,
    pending        = pending,
    ## The fill's own headline numbers. A UI deciding "did this build
    ## actually produce a filled workbook?" should not have to re-derive
    ## that from the per-cell census.
    fill           = if (!is.null(fill)) {
      list(filled = fill$filled, pending = fill$pending,
           skipped = fill$skipped)
    } else NULL,
    unfilled_cells = unfilled,
    error          = failure,
    failed_stage   = stage_failed
  )
}
