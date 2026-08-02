## arsbridge -- progress.R
## ---------------------------------------------------------------------------
## One progress channel for the whole pipeline.
##
## The problem this solves: a real study takes minutes, and the person
## watching the build had no way to tell a slow run from a hung one -- in the
## app's in-process mode there was literally no signal at all.
##
## The design is a single hook, not a family of callback arguments threaded
## through every function: the three long stages call `.progress_tick()` from
## their per-item loops, and the tick reads one option. When nobody set the
## option, a tick costs one `getOption()` and nothing else -- which is what
## keeps default behaviour (and the byte-identity baseline) untouched.
##
## Two consumers exist, one per mode:
##  * in-process (Shiny): a callback that forwards to `shiny::setProgress()`.
##  * background (callr): `.progress_emit_line()` writes one parseable line
##    per event to stdout, which callr redirects into the project's run.log;
##    the app's poller reads the log back with `.parse_progress_line()`.

#' Report one unit of progress, if anyone is listening.
#'
#' `i` of `n` within the current stage, with a human label (a TLF number, an
#' analysis id). The callback is read from `getOption("arsbridge.progress")`
#' and its errors are swallowed: a progress bar must never take a build down.
#' @noRd
.progress_tick <- function(i, n, label = NA_character_) {
  callback <- getOption("arsbridge.progress")
  if (is.null(callback)) return(invisible(NULL))
  tryCatch(callback(i = i, n = n, label = label), error = function(e) NULL)
  invisible(NULL)
}

#' Write one progress event as a line on stdout.
#'
#' The background worker's only channel to the app is the run log that callr
#' builds from its stdout, so the event becomes one greppable line. The label
#' comes last because it may contain spaces; the flush matters because the
#' redirected file would otherwise buffer past the poller.
#' @noRd
.progress_emit_line <- function(ev) {
  cat(sprintf("[progress] stage=%s stage_idx=%d n_stages=%d i=%d n=%d label=%s\n",
              ev$stage %||% "?", as.integer(ev$stage_idx %||% 1L),
              as.integer(ev$n_stages %||% 1L), as.integer(ev$i %||% 0L),
              as.integer(ev$n %||% 0L),
              if (is.na(ev$label %||% NA_character_)) "" else ev$label))
  flush(stdout())
  invisible(NULL)
}

#' The last progress event in a character vector of log lines, or NULL.
#' @noRd
.parse_progress_line <- function(lines) {
  hits <- grep("^\\[progress\\] ", lines, value = TRUE)
  if (length(hits) == 0) return(NULL)
  line <- hits[[length(hits)]]
  m <- regmatches(line, regexec(
    "^\\[progress\\] stage=(\\S+) stage_idx=(\\d+) n_stages=(\\d+) i=(\\d+) n=(\\d+) label=(.*)$",
    line))[[1]]
  if (length(m) != 7) return(NULL)
  list(stage = m[[2]], stage_idx = as.integer(m[[3]]),
       n_stages = as.integer(m[[4]]), i = as.integer(m[[5]]),
       n = as.integer(m[[6]]),
       label = if (nzchar(m[[7]])) m[[7]] else NA_character_)
}

#' Overall completion of the run, in [0, 1], from one event.
#'
#' Stages weigh equally: precise weighting would need to know each stage's
#' cost in advance, and a bar that moves through every stage beats one that
#' is accurate and confusing.
#' @noRd
.progress_fraction <- function(ev) {
  n_stages <- max(1L, as.integer(ev$n_stages %||% 1L))
  stage_idx <- min(max(1L, as.integer(ev$stage_idx %||% 1L)), n_stages)
  within <- if ((ev$n %||% 0) > 0) (ev$i %||% 0) / ev$n else 0
  value <- ((stage_idx - 1) + min(max(within, 0), 1)) / n_stages
  min(max(value, 0), 1)
}

#' A stage's name as the person watching the bar should read it.
#' @noRd
.progress_stage_label <- function(stage) {
  switch(as.character(stage %||% ""),
         spec_to_ars    = "Building the reporting event",
         ars_to_ard     = "Computing results",
         ars_fill_shell = "Filling the workbook",
         "Working")
}
