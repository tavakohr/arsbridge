## arsbridge -- capability.R
## ---------------------------------------------------------------------------
## What arsbridge can and cannot build. arsbridge maps TLF analyses onto a
## small set of descriptive {cards} methods (summary statistics, count and
## percentage, AE frequency, subject counts, listings, basic figures).
## Inferential / model-based analyses have no {cards} equivalent -- forcing
## them into a count produces nonsense (e.g. categorising every distinct
## numeric value). Such tables are detected here, flagged as UNSUPPORTED, and
## carried to the final output as a numbered placeholder instead of garbage.
##
## Detection runs in two layers (mirrors the shell reader):
##   * LLM: enrich_with_llm() asks the model for `is_supported` +
##     `unsupported_reason` and routes the answer through .capability_from_llm().
##   * Keyword heuristic: .capability_keyword_scan() reads the section title,
##     footnotes, and stub labels for inferential indicators. Always runs, so
##     a keyless run still catches the common cases.
## A section is unsupported if EITHER layer says so (union, conservative).
## ---------------------------------------------------------------------------

## Inferential / model-based indicators. Each entry: a human label and a
## case-insensitive regex over the section's text (title + footnotes + labels).
## Kept deliberately specific so descriptive tables that merely mention a word
## in passing are not over-flagged.
.UNSUPPORTED_INDICATORS <- list(
  list(label = "Cochran-Mantel-Haenszel test",
       re = "cochran[-\\s]*mantel[-\\s]*haenszel|\\bcmh\\b"),
  list(label = "exact confidence interval (Clopper-Pearson)",
       re = "clopper[-\\s]*pearson"),
  list(label = "Newcombe / Wilson difference interval",
       re = "newcombe|wilson score"),
  list(label = "Fisher exact / chi-square test",
       re = "fisher'?s? exact|chi[-\\s]*square|chi\\b"),
  list(label = "p-value (hypothesis test)",
       re = "p[-\\s]*value"),
  list(label = "odds ratio",            re = "odds ratio"),
  list(label = "hazard ratio / Cox model",
       re = "hazard ratio|\\bcox\\b|proportional hazards"),
  list(label = "logistic regression",   re = "logistic regression"),
  list(label = "ANCOVA / MMRM / LS-means",
       re = "ancova|\\bmmrm\\b|least[-\\s]*squares? mean|ls[-\\s]*mean"),
  list(label = "multiple imputation / NRI",
       re = "multiple imputation|non[-\\s]*responder imputation|\\bnri\\b"),
  list(label = "log-rank test",         re = "log[-\\s]*rank")
)

#' Concatenate the human-readable text of a section that may carry an
#' inferential signal: title, footnotes, and stub-row labels.
#' @noRd
.section_text <- function(section) {
  labels <- vapply(section$stub_rows %||% list(),
                   function(r) as.character(r$label %||% ""), character(1))
  paste(c(section$title %||% "",
          section$footnotes %||% character(),
          labels),
        collapse = " \n ")
}

#' Keyword layer: does the section's text indicate an unsupported analysis?
#' Returns list(supported = TRUE/FALSE, reason = character).
#' @noRd
.capability_keyword_scan <- function(section) {
  txt <- tolower(.section_text(section))
  if (!nzchar(trimws(txt))) return(list(supported = TRUE, reason = ""))
  hits <- character()
  for (ind in .UNSUPPORTED_INDICATORS) {
    if (grepl(ind$re, txt, perl = TRUE, ignore.case = TRUE)) {
      hits <- c(hits, ind$label)
    }
  }
  if (length(hits) == 0) return(list(supported = TRUE, reason = ""))
  list(supported = FALSE,
       reason = paste0("requires ", paste(unique(hits), collapse = "; "),
                       " -- beyond arsbridge's descriptive {cards} methods"))
}

#' LLM layer: normalise the model's is_supported / unsupported_reason answer.
#' A missing / NA `is_supported` is treated as supported (the keyword scan is
#' the safety net).
#' @noRd
.capability_from_llm <- function(enrichment) {
  is_sup <- enrichment$is_supported
  if (is.null(is_sup) || isTRUE(is_sup)) return(list(supported = TRUE, reason = ""))
  reason <- trimws(enrichment$unsupported_reason %||% "")
  if (!nzchar(reason)) reason <- "analysis type not supported by arsbridge"
  list(supported = FALSE, reason = reason)
}

#' Combined capability verdict for one enriched section (union of both layers).
#'
#' @param section    The parsed/enriched section (carries title, footnotes,
#'   stub_rows, and any LLM `is_supported` / `unsupported_reason`).
#' @return list(supported = logical, reason = character). `reason` is empty
#'   when supported.
#'
#' @keywords internal
#' @noRd
assess_capability <- function(section) {
  kw  <- .capability_keyword_scan(section)
  llm <- .capability_from_llm(section)
  if (kw$supported && llm$supported) return(list(supported = TRUE, reason = ""))
  reasons <- unique(c(if (!llm$supported) llm$reason,
                      if (!kw$supported)  kw$reason))
  list(supported = FALSE, reason = paste(reasons, collapse = "; "))
}
