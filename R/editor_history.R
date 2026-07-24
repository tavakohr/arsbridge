## arsbridge -- editor_history.R
## ---------------------------------------------------------------------------
## Undo/redo, and surviving a crash.
##
## Both exist for the same reason: a review session is an hour of careful
## judgement that nothing should be able to take away. A mis-click must be
## reversible, and a browser that dies must not cost the whole session.
##
## History is kept as snapshots of the model rather than as a log to replay.
## Replay sounds tidier but is only as correct as the log, and a log that
## drifts from the model silently produces a WRONG reporting event -- which is
## much worse than using a few megabytes of memory.

## How many steps back a reviewer can go. Deep enough that undo is a reflex
## rather than a rationed resource, bounded so a long session cannot grow
## without limit.
.HISTORY_LIMIT <- 50L

#' @noRd
.new_history <- function() {
  list(past = list(), future = list())
}

#' @noRd
.snapshot <- function(state) {
  list(model = state$model(), edit_log = state$edit_log())
}

## Called before every mutation. Taking the snapshot BEFORE the change is what
## makes undo mean "put it back how it was".
#' @noRd
.push_history <- function(state) {
  history <- state$history()
  history$past <- c(history$past, list(.snapshot(state)))

  if (length(history$past) > .HISTORY_LIMIT) {
    history$past <- utils::tail(history$past, .HISTORY_LIMIT)
  }
  ## A new edit after undoing abandons the redo branch, which is what every
  ## editor does and what people expect.
  history$future <- list()

  state$history(history)
  invisible(TRUE)
}

#' @noRd
.can_undo <- function(state) length(state$history()$past) > 0

#' @noRd
.can_redo <- function(state) length(state$history()$future) > 0

#' @noRd
.restore_snapshot <- function(state, snapshot) {
  state$model(snapshot$model)
  state$edit_log(snapshot$edit_log)
  state$findings(
    validate_ars_model(snapshot$model, state$spec, state$report)
  )
  ## The panels are showing values that just changed underneath them, so tell
  ## them to redraw -- otherwise an undone edit stays visible in its input.
  state$refresh(state$refresh() + 1L)
  invisible(TRUE)
}

#' @noRd
.undo <- function(state) {
  history <- state$history()
  if (length(history$past) == 0) return(invisible(FALSE))

  current <- .snapshot(state)
  restored <- history$past[[length(history$past)]]

  history$past <- history$past[-length(history$past)]
  history$future <- c(list(current), history$future)
  state$history(history)

  .restore_snapshot(state, restored)
  invisible(TRUE)
}

#' @noRd
.redo <- function(state) {
  history <- state$history()
  if (length(history$future) == 0) return(invisible(FALSE))

  current <- .snapshot(state)
  restored <- history$future[[1]]

  history$future <- history$future[-1]
  history$past <- c(history$past, list(current))
  state$history(history)

  .restore_snapshot(state, restored)
  invisible(TRUE)
}


## --- surviving a crash ------------------------------------------------------
##
## The autosave lives in the user's cache directory rather than tempdir(),
## because the case worth protecting against is R itself going away -- and
## tempdir() goes with it.

#' @noRd
.autosave_dir <- function() {
  tools::R_user_dir("arsbridge", "cache")
}

## A short, stable fingerprint of a path. Two different studies whose files
## share a basename must not overwrite each other's recovery data, and the
## same file reopened later must find its own.
#' @noRd
.path_key <- function(path) {
  characters <- as.integer(charToRaw(path))

  ## A plain rolling checksum. This only has to separate paths on one
  ## machine, so it does not need to be a cryptographic hash.
  checksum <- 0
  for (value in characters) {
    checksum <- (checksum * 31 + value) %% 100000000
  }
  sprintf("%08d", checksum)
}

## Keyed on the file being edited, so two studies open in turn do not overwrite
## each other's recovery data.
#' @noRd
.autosave_path <- function(source_path) {
  if (is.null(source_path)) return(NULL)

  full_path <- normalizePath(source_path, mustWork = FALSE)
  name <- paste0(
    "editor-", .path_key(full_path), "-",
    .slug(basename(source_path)), ".rds"
  )
  file.path(.autosave_dir(), name)
}

#' @noRd
.write_autosave <- function(state) {
  path <- .autosave_path(state$source_path)
  if (is.null(path)) return(invisible(NULL))

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  ## Recovery must never be the thing that breaks a session, so a failure to
  ## write it is silent -- the reviewer still has their work on screen.
  tryCatch(
    saveRDS(
      list(
        source_path = state$source_path,
        saved_at    = Sys.time(),
        model       = state$model(),
        edit_log    = state$edit_log()
      ),
      path
    ),
    error = function(e) NULL
  )
  invisible(path)
}

#' @noRd
.read_autosave <- function(source_path) {
  path <- .autosave_path(source_path)
  if (is.null(path) || !file.exists(path)) return(NULL)

  recovered <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(recovered) || !inherits(recovered$model, "ars_model")) {
    return(NULL)
  }
  ## Nothing to offer if the session died before anything was changed.
  if (nrow(recovered$edit_log) == 0) return(NULL)

  recovered
}

#' @noRd
.clear_autosave <- function(source_path) {
  path <- .autosave_path(source_path)
  if (!is.null(path) && file.exists(path)) unlink(path)
  invisible(NULL)
}
