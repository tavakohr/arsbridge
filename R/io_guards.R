## arsbridge -- io_guards.R
## ---------------------------------------------------------------------------
## One place that turns every input-reading / output-writing failure into a
## plain-English, input-pointed message. The contract used everywhere:
##
##   [which input document] -- [what is wrong]. [why it blocks a clean
##   deliverable]. To fix: [concrete action on that document].
##
## HARD failures (a deliverable is impossible -- e.g. the shell cannot be
## opened) raise a clean `cli::cli_abort()`, never a raw base-R error such as
## "invalid filename argument". SOFT failures (one dataset of many cannot be
## read) record a FAIL diagnostic and return NULL so the rest of the run still
## produces what it can.

## Plain-English names for the four input documents arsbridge consumes. Used
## as the `input` field of diagnostics and inside abort messages so the user
## always knows which file to open.
INPUT_SHELL <- "annotated shell (.docx)"
INPUT_SPEC  <- "ADaM spec (.xlsx/.xml)"
INPUT_DATA  <- "ADaM dataset"
INPUT_ARS   <- "ARS JSON"
INPUT_LLM   <- "LLM provider / API key"
INPUT_CAPABILITY <- "arsbridge capability (manual table)"
INPUT_SUPPLEMENT <- "Copilot supplement (.json)"

#' Append the file's basename to a document label, e.g.
#' "ARS JSON 'reporting_event.json'". Falls back to the bare label when the
#' path is not a usable single string.
#' @noRd
.doc_label <- function(label, path = NULL) {
  if (!is.null(path) && is.character(path) && length(path) == 1 &&
      !is.na(path) && nzchar(path)) {
    paste0(label, " '", basename(path), "'")
  } else {
    label
  }
}

## ---------------------------------------------------------------------------
## Path guards -- NULL / empty / wrong-type / missing, before any base-R call.
## ---------------------------------------------------------------------------

#' Require `path` to be a single, non-empty character string pointing at an
#' existing file. Aborts cleanly (naming the input document) otherwise.
#' @noRd
.require_file <- function(path, arg, doc) {
  if (is.null(path) || !is.character(path) || length(path) != 1 ||
      is.na(path) || !nzchar(path)) {
    cli::cli_abort(c(
      "x" = "No {doc} was supplied ({.arg {arg}} is empty or missing).",
      "i" = "To fix: pass the path to your {doc}."
    ))
  }
  if (!file.exists(path)) {
    cli::cli_abort(c(
      "x" = "{doc} not found at {.path {path}}.",
      "i" = "To fix: check the path to your {doc} -- the file does not exist there."
    ))
  }
  invisible(path)
}

#' Require `dir` to be a single, non-empty character string pointing at an
#' existing directory. Aborts cleanly (naming the input document) otherwise.
#' @noRd
.require_dir <- function(dir, arg, doc) {
  if (is.null(dir) || !is.character(dir) || length(dir) != 1 ||
      is.na(dir) || !nzchar(dir)) {
    cli::cli_abort(c(
      "x" = "No {doc} folder was supplied ({.arg {arg}} is empty or missing).",
      "i" = "To fix: pass the folder that holds your {doc} files (.xpt or .csv)."
    ))
  }
  if (!dir.exists(dir)) {
    cli::cli_abort(c(
      "x" = "{doc} folder not found at {.path {dir}}.",
      "i" = "To fix: point {.arg {arg}} at the folder that holds your {doc} files."
    ))
  }
  invisible(dir)
}

## ---------------------------------------------------------------------------
## Readers / writer -- each converts a raw base-R failure into the contract.
## ---------------------------------------------------------------------------

#' Read + parse an ARS JSON file. HARD: a missing/unreadable ARS means no ARD
#' and no TLFs, so this aborts cleanly rather than degrade.
#' @noRd
.read_json <- function(path, arg = "ars_path", doc = INPUT_ARS) {
  .require_file(path, arg, doc)
  tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) {
      msg <- conditionMessage(e)
      cli::cli_abort(c(
        "x" = "The {doc} {.path {basename(path)}} could not be read as valid JSON ({msg}).",
        "i" = "To fix: regenerate it with {.fn spec_to_ars} -- a hand-edited file is often truncated or has a stray comma."
      ))
    }
  )
}

#' Read one ADaM dataset (.xpt or .csv) by full path. SOFT: a single bad
#' dataset should not stop the whole run, so this records a FAIL diagnostic
#' and returns NULL; callers already skip analyses whose data is NULL.
#' @noRd
.read_dataset <- function(path, ds_name) {
  is_xpt <- grepl("\\.xpt$", path, ignore.case = TRUE)
  ds_up  <- toupper(ds_name)
  doc    <- paste0(INPUT_DATA, " ", ds_up)
  reader <- if (is_xpt) {
    function(p) haven::read_xpt(p)
  } else {
    function(p) utils::read.csv(p, stringsAsFactors = FALSE, check.names = FALSE)
  }
  tryCatch(
    reader(path),
    error = function(e) {
      .diag_gap(
        stage = "execute_ard", severity = "FAIL", input = doc,
        problem = sprintf("The %s file (%s) could not be read: %s.",
                          ds_up, basename(path), conditionMessage(e)),
        why = "Every analysis that reads this dataset will be skipped.",
        fix = sprintf("Re-export %s as a valid %s and place it in the ADaM folder.",
                      basename(path),
                      if (is_xpt) "SAS transport file (XPT v5 or v8)" else "CSV"),
        location = path
      )
      NULL
    }
  )
}

#' Open an annotated-shell .docx. HARD: an unreadable shell means no ARS.
#' @noRd
.read_docx <- function(path, arg = "docx_path", doc = INPUT_SHELL) {
  .require_file(path, arg, doc)
  tryCatch(
    officer::read_docx(path),
    error = function(e) {
      msg <- conditionMessage(e)
      cli::cli_abort(c(
        "x" = "The {doc} {.path {basename(path)}} could not be opened ({msg}).",
        "i" = "To fix: re-save it as a valid Word .docx -- it may be corrupt, password-protected, or not actually a .docx."
      ))
    }
  )
}

#' Read a UTF-8 text file (e.g. a prompt template). HARD by default.
#' @noRd
.read_lines <- function(path, what) {
  tryCatch(
    readLines(path, warn = FALSE, encoding = "UTF-8"),
    error = function(e) {
      msg <- conditionMessage(e)
      cli::cli_abort(c(
        "x" = "Could not read {what} at {.path {path}} ({msg}).",
        "i" = "To fix: check the file exists and is readable."
      ))
    }
  )
}

#' Write text to a file (e.g. the emitted ARS JSON). HARD: if we cannot write
#' the deliverable, the user must know exactly why.
#' @noRd
.write_text <- function(text, path, what, useBytes = FALSE) {
  tryCatch(
    writeLines(text, path, useBytes = useBytes),
    error = function(e) {
      msg <- conditionMessage(e)
      cli::cli_abort(c(
        "x" = "Could not write {what} to {.path {path}} ({msg}).",
        "i" = "To fix: choose a writable output folder -- the current one may be read-only, missing, or full."
      ))
    }
  )
  invisible(path)
}
