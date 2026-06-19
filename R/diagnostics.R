## arsbridge -- diagnostics.R
## ---------------------------------------------------------------------------
## Pipeline-wide diagnostics collector. Every silent fallback, regex miss,
## skipped sheet, LLM failure, unknown method, or dropped where-clause is
## recorded here instead of (or in addition to) a console warning, so the
## validation report can surface ALL quality findings for a new study in
## one place.
##
## Implemented as a package-level environment rather than a parameter
## threaded through every internal function: collection points are spread
## across parsers, the LLM enricher, the ARS builder, and the ARD executor,
## and the records are only ever consumed by the exported entry points
## (`spec_to_ars()`, `ars_to_ard()`), which reset the collector on entry.

.diag_env <- new.env(parent = emptyenv())
.diag_env$records <- list()
.diag_env$llm_fail <- 0L

#' Reset the diagnostics collector (called at the start of each exported
#' pipeline entry point).
#' @noRd
diag_reset <- function() {
  .diag_env$records <- list()
  .diag_env$llm_fail <- 0L
  invisible(NULL)
}

## A wholesale LLM-enrichment failure is one root cause (usually a bad API key
## or model id) that repeats once per TLF. Count it here and let the caller
## raise a SINGLE summary finding, instead of N identical per-TLF rows.
#' @noRd
.diag_llm_fail_bump  <- function() .diag_env$llm_fail <- (.diag_env$llm_fail %||% 0L) + 1L
#' @noRd
.diag_llm_fail_count <- function() .diag_env$llm_fail %||% 0L

#' Record one diagnostic finding.
#'
#' @param stage    Pipeline stage: "parse_shell" | "parse_spec" |
#'   "enrich_llm" | "where_clause" | "build_ars" | "execute_ard".
#' @param severity "FAIL" (output likely wrong), "WARN" (fallback applied --
#'   needs review), or "INFO" (notable decision, no action expected).
#' @param problem  What happened, in plain language.
#' @param input    Which input document the finding concerns, in plain
#'   English (e.g. one of the `INPUT_*` labels: annotated shell, ADaM spec,
#'   ADaM dataset, ARS JSON). NA when not tied to a specific document.
#' @param tlf_number TLF the finding belongs to (NA when not applicable,
#'   e.g. spec-level findings).
#' @param location Finer-grained context: sheet name, stub label,
#'   annotation text, analysis id, ...
#' @param action   What the pipeline did about it (the fallback taken) and/or
#'   how the user should fix it.
#' @noRd
diag_add <- function(stage,
                     severity,
                     problem,
                     input      = NA_character_,
                     tlf_number = NA_character_,
                     location   = NA_character_,
                     action     = NA_character_) {
  ## Coerce every field to exactly one character value -- NULL and
  ## zero-length inputs (e.g. sprintf() over a NULL) must never be able to
  ## break the pipeline they are reporting on.
  chr1 <- function(x) {
    x <- as.character(x %||% NA_character_)
    if (length(x) == 0) NA_character_ else x[1]
  }
  rec <- data.frame(
    stage      = chr1(stage),
    severity   = chr1(severity),
    input      = chr1(input),
    tlf_number = chr1(tlf_number),
    location   = chr1(location),
    problem    = chr1(problem),
    action     = chr1(action),
    stringsAsFactors = FALSE
  )
  .diag_env$records[[length(.diag_env$records) + 1L]] <- rec
  invisible(NULL)
}

#' Record a gap using the standard plain-English contract.
#'
#' Thin wrapper over [diag_add()] that enforces the house message shape:
#' *what is wrong* (`problem`), *why it blocks a clean deliverable* (`why`,
#' folded into the problem text), and *how to fix it on the named input
#' document* (`fix`, surfaced as the action). Keeps wording consistent across
#' every collection point without changing the stored schema.
#' @noRd
.diag_gap <- function(stage, severity, input, problem, why = NULL, fix = NULL,
                      tlf_number = NA_character_, location = NA_character_) {
  prob <- if (!is.null(why) && nzchar(why)) paste0(problem, " ", why) else problem
  act  <- if (!is.null(fix) && nzchar(fix)) paste0("To fix: ", fix) else NA_character_
  diag_add(stage = stage, severity = severity, input = input,
           problem = prob, action = act,
           tlf_number = tlf_number, location = location)
}

#' All diagnostics recorded since the last `diag_reset()`, as a data frame.
#' Zero-row (with the full column set) when nothing was recorded.
#' @noRd
diag_records <- function() {
  if (length(.diag_env$records) == 0) {
    return(data.frame(
      stage      = character(),
      severity   = character(),
      input      = character(),
      tlf_number = character(),
      location   = character(),
      problem    = character(),
      action     = character(),
      stringsAsFactors = FALSE
    ))
  }
  out <- do.call(rbind, .diag_env$records)
  rownames(out) <- NULL
  out
}

#' Retrieve pipeline diagnostics from the most recent run
#'
#' Returns every fallback, parsing miss, skipped sheet, LLM failure,
#' unknown analysis method, and dropped where-clause condition recorded
#' during the most recent [spec_to_ars()] or [ars_to_ard()] call in this
#' R session. The same records are written to the "Diagnostics" sheet of
#' the validation report and returned in the `diagnostics` element of the
#' [spec_to_ars()] result; this accessor exists for interactive inspection
#' after the fact.
#'
#' @return Data frame with columns `stage`, `severity` (`FAIL` / `WARN` /
#'   `INFO`), `input` (which input document the finding concerns),
#'   `tlf_number`, `location`, `problem`, `action`.
#'
#' @examples
#' \dontrun{
#' spec_to_ars("shells.docx", "adam_spec.xlsx")
#' ars_diagnostics()
#' }
#' @export
ars_diagnostics <- function() {
  diag_records()
}

#' Blocking problems from the most recent run, in plain English
#'
#' The show-stoppers: every `FAIL`-severity finding from the most recent
#' [spec_to_ars()] or [ars_to_ard()] call -- the gaps that mean arsbridge
#' could not produce clean ARS / ARD / ready-to-run R code. Each row names the
#' input document to open (`input`), what is wrong and why (`problem`), and how
#' to fix it (`action`). A zero-row result means there were no blocking gaps.
#'
#' This is the same set surfaced at the top of the validation report
#' ("What to fix first") and returned in the `blockers` element of the
#' [spec_to_ars()] result; this accessor exists for interactive inspection.
#'
#' @param diagnostics Data frame of diagnostics to summarise. Defaults to the
#'   findings from the most recent run ([ars_diagnostics()]).
#'
#' @return Data frame with columns `input`, `problem`, `action`, `stage`,
#'   `tlf_number`, `location` -- one row per blocking (FAIL) finding.
#'
#' @examples
#' \dontrun{
#' spec_to_ars("shells.docx", "adam_spec.xlsx")
#' ars_blockers()   # what must be fixed, in plain English
#' }
#' @export
ars_blockers <- function(diagnostics = ars_diagnostics()) {
  cols <- c("input", "problem", "action", "stage", "tlf_number", "location")
  if (is.null(diagnostics) || nrow(diagnostics) == 0 ||
      !"severity" %in% names(diagnostics)) {
    empty <- stats::setNames(
      lapply(cols, function(.) character()), cols)
    return(data.frame(empty, stringsAsFactors = FALSE))
  }
  fails <- diagnostics[diagnostics$severity == "FAIL", , drop = FALSE]
  keep  <- intersect(cols, names(fails))
  fails <- fails[, keep, drop = FALSE]
  rownames(fails) <- NULL
  fails
}
