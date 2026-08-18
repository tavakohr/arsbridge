## arsbridge -- unresolved_labels.R
## ---------------------------------------------------------------------------
## The statistic rows nothing could bind, as data.
##
## When a shell row's label names statistics, three things have to line up: the
## label has to be readable as a statistic request, the row's method has to
## declare that statistic, and the ARD has to carry it. The build stage already
## reports a failure of either of the first two -- one WARN per row, naming the
## label, the method, and what that method does declare.
##
## A WARN is prose addressed to a reader. It is the right thing to emit and the
## wrong thing to work from: a study team fixing twenty rows, an app listing
## them for review, or a generator proposing what they mean all need the same
## facts as fields. This file is that -- the identical set of rows the WARNs
## describe, keyed and typed.
##
## Nothing here decides anything. It reads what `.build_table_cells()` recorded
## and reshapes it. Deciding what a refused row MEANS is the next layer's job,
## and it is deliberately not in this file.

## Read one multi-valued field back out of parsed ARS JSON.
##
## `.read_json()` parses with `simplifyVector = FALSE`, so a JSON array arrives
## as an R list and a one-element array arrives as a bare scalar (auto_unbox
## wrote it that way). Both have to end up the same character vector, or a row
## with exactly one unsupported statistic reads differently from a row with
## two -- the kind of arity bug that only ever shows up on real data.
#' @noRd
.unresolved_chr <- function(x) {
  if (is.null(x)) return(character(0))
  out <- as.character(unlist(x, use.names = FALSE))
  out[!is.na(out)]
}

## Assemble the queue frame from the accumulated rows.
##
## Built COLUMN-wise, deliberately. Row-wise assembly (`rbind.data.frame` over
## one list per row) looks tidier and is wrong here: it coerces each row to a
## data frame first, and a row whose token vector is empty -- every
## `"unreadable"` row, which is half the point of this queue -- coerces to a
## frame of ZERO rows and takes the whole bind down with "arguments imply
## differing number of rows".
##
## Assembling the atomic columns first and attaching the list columns
## afterwards also keeps them plain lists, so the empty frame and a populated
## one subset and print identically. A caller must never need to know which
## one it has.
#' @noRd
.unresolved_frame <- function(rows) {
  chr <- function(field) {
    if (length(rows) == 0) return(character(0))
    vapply(rows, function(r) as.character(r[[field]]), character(1))
  }
  out <- data.frame(
    output_id   = chr("output_id"),
    tlf         = chr("tlf"),
    sheet       = chr("sheet"),
    row         = if (length(rows) == 0) integer(0)
                  else vapply(rows, function(r) as.integer(r$row), integer(1)),
    label       = chr("label"),
    analysis_id = chr("analysis_id"),
    method_id   = chr("method_id"),
    reason      = chr("reason"),
    stringsAsFactors = FALSE)
  out$tokens      <- lapply(rows, function(r) r$tokens)
  out$unsupported <- lapply(rows, function(r) r$unsupported)
  out$available   <- lapply(rows, function(r) r$available)
  out
}

## Resolve whatever the caller has into a reporting event.
##
## `spec_to_ars()` returns a RUN RESULT -- paths, counts, diagnostics, and the
## reporting event under `reporting_event` -- not the event itself. Reading
## `$outputs` off that wrapper finds nothing, so a caller who passed the
## obvious thing would get an empty queue and read it as "nothing unresolved".
## That is the worst answer available: silently indistinguishable from the
## real one. So the wrapper is unwrapped, and an object that is neither shape
## aborts.
#' @noRd
.unresolved_reporting_event <- function(ars) {
  if (is.character(ars)) return(.read_json(ars, arg = "ars"))
  if (is.list(ars)) {
    ## A run result: take the event it carries.
    if (!is.null(ars$reporting_event)) return(ars$reporting_event)
    ## A reporting event: `outputs` is where the cell maps live. An event with
    ## no outputs at all is legitimate (nothing was built), so an empty list
    ## counts as long as the element is present.
    if ("outputs" %in% names(ars)) return(ars)
  }
  cli::cli_abort(c(
    "{.arg ars} is not a reporting event.",
    "i" = "Pass the path to an ARS {.file .json}, the result of
           {.fn spec_to_ars}, or its {.field reporting_event}."
  ))
}

#' The statistic rows arsbridge refused to bind
#'
#' A shell row whose label names statistics is filled from the block above it,
#' and which statistics it gets is decided by what the label says. When that
#' cannot be established, arsbridge binds *nothing* on the row rather than
#' binding whichever statistic the method happens to list first -- a
#' part-bound row shifts its remaining placeholders onto the wrong statistics,
#' which writes a real number of the wrong thing. The cells stay on their
#' placeholders and the build stage logs a WARN.
#'
#' This function returns those same rows as a data frame, so they can be
#' worked as a queue instead of read as prose.
#'
#' There are two reasons, and they lead to different work:
#'
#' \describe{
#'   \item{`"unreadable"`}{The label could not be read as a statistic request
#'     at all -- an unfamiliar spelling, or a row that is not a statistic row.
#'     `tokens` is empty. Somebody has to say what the row means.}
#'   \item{`"unsupported"`}{The label was read correctly, and names a
#'     statistic the row's method does not declare -- a standard-error line
#'     over a method that produces no standard error, say. `tokens` holds what
#'     the label asked for and `unsupported` holds the part the method cannot
#'     supply. No synonym fixes this: either the row's method is wrong for it,
#'     or the statistic is beyond the engine.}
#' }
#'
#' @param ars The ARS to read, in any of the three shapes a caller has one in:
#'   the path to an ARS `.json`, the run result [spec_to_ars()] returns (the
#'   reporting event is taken from its `reporting_event` element), or a
#'   reporting event itself. Anything else is an error rather than an empty
#'   queue -- "nothing is unresolved" and "I was handed the wrong object" must
#'   not look the same, because the first is the answer a caller acts on.
#'
#' @return A data frame, one row per refused shell row, with columns:
#'   \describe{
#'     \item{`output_id`}{The ARS output the row belongs to.}
#'     \item{`tlf`}{That output's TLF number, as the shell numbered it.}
#'     \item{`sheet`}{The worksheet the row is on.}
#'     \item{`row`}{The sheet row number.}
#'     \item{`label`}{The row label, exactly as authored.}
#'     \item{`analysis_id`}{The analysis the row would have been filled from --
#'       its block's parent, or its own when it has one.}
#'     \item{`method_id`}{That analysis's ARS method.}
#'     \item{`reason`}{`"unreadable"` or `"unsupported"`, as above.}
#'     \item{`tokens`}{List column: the semantic statistics the label asked
#'       for, in reading order. Empty when `reason` is `"unreadable"`.}
#'     \item{`unsupported`}{List column: the subset of `tokens` the method does
#'       not declare. Empty when `reason` is `"unreadable"`.}
#'     \item{`available`}{List column: the operations the method DOES declare
#'       -- what the row could have asked for instead.}
#'   }
#'   Zero rows means every statistic row bound. The columns are present either
#'   way, so a caller can bind or filter the result without a special case.
#'
#'   Only a shell built from an `.xlsx` carries a cell map, so a reporting
#'   event built from a Word shell always returns zero rows.
#'
#' @seealso [ars_manual_worklist()] for cells reserved because their METHOD is
#'   beyond the engine -- a different queue: those rows are understood and
#'   waiting on a derivation, these are not yet understood.
#'
#' @examples
#' \dontrun{
#' built <- spec_to_ars("shell.xlsx", "adam_spec.xlsx")
#' todo  <- ars_unresolved_labels(built)          # or built$ars_path
#' todo[todo$reason == "unreadable", c("tlf", "row", "label")]
#' }
#' @export
ars_unresolved_labels <- function(ars) {
  spec <- .unresolved_reporting_event(ars)

  rows <- list()
  for (output in spec$outputs %||% list()) {
    fill <- output[["_meta"]][["shell_fill"]]
    unresolved <- fill$unresolved %||% list()
    if (length(unresolved) == 0) next
    for (u in unresolved) {
      rows[[length(rows) + 1L]] <- list(
        output_id   = output$id %||% NA_character_,
        tlf         = output$name %||% NA_character_,
        sheet       = fill$source$sheet %||% NA_character_,
        row         = u$row %||% NA_integer_,
        label       = u$label %||% "",
        analysis_id = u$analysis_id %||% NA_character_,
        method_id   = u$method_id %||% NA_character_,
        reason      = u$reason %||% NA_character_,
        tokens      = .unresolved_chr(u$tokens),
        unsupported = .unresolved_chr(u$unsupported),
        available   = .unresolved_chr(u$available)
      )
    }
  }

  ## One path for both cases: an empty queue is a zero-row frame with the same
  ## eleven columns, so a caller can filter or bind it without a special case.
  .unresolved_frame(rows)
}
