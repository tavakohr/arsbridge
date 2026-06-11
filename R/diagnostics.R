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

#' Reset the diagnostics collector (called at the start of each exported
#' pipeline entry point).
#' @noRd
diag_reset <- function() {
  .diag_env$records <- list()
  invisible(NULL)
}

#' Record one diagnostic finding.
#'
#' @param stage    Pipeline stage: "parse_shell" | "parse_spec" |
#'   "enrich_llm" | "where_clause" | "build_ars" | "execute_ard".
#' @param severity "FAIL" (output likely wrong), "WARN" (fallback applied --
#'   needs review), or "INFO" (notable decision, no action expected).
#' @param problem  What happened, in plain language.
#' @param tlf_number TLF the finding belongs to (NA when not applicable,
#'   e.g. spec-level findings).
#' @param location Finer-grained context: sheet name, stub label,
#'   annotation text, analysis id, ...
#' @param action   What the pipeline did about it (the fallback taken).
#' @noRd
diag_add <- function(stage,
                     severity,
                     problem,
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
    tlf_number = chr1(tlf_number),
    location   = chr1(location),
    problem    = chr1(problem),
    action     = chr1(action),
    stringsAsFactors = FALSE
  )
  .diag_env$records[[length(.diag_env$records) + 1L]] <- rec
  invisible(NULL)
}

#' All diagnostics recorded since the last [diag_reset()], as a data frame.
#' Zero-row (with the full column set) when nothing was recorded.
#' @noRd
diag_records <- function() {
  if (length(.diag_env$records) == 0) {
    return(data.frame(
      stage      = character(),
      severity   = character(),
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
#'   `INFO`), `tlf_number`, `location`, `problem`, `action`.
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
