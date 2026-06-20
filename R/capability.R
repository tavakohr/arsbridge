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

## ---------------------------------------------------------------------------
## Method classification (ADR 0001). A gated section is not necessarily a
## wholesale "unsupported" -- some of its statistics now have executable
## descriptors (a Clopper-Pearson CI, a stratified CMH p-value). This layer maps
## the section's text to those specific methods, extracting the operands they
## need, and reports the residual indicators arsbridge still cannot compute.
## Deterministic keyword scan; an LLM enrichment may supersede it later.
## ---------------------------------------------------------------------------

## Pull a stratification variable out of "stratified by REGION" / "stratified by
## region" style text. Returns an upper-cased token (an ADaM-style variable
## name) or NULL when none is named -- a CMH needs it to execute.
#' @noRd
.extract_strata <- function(text) {
  m <- regmatches(text, regexec(
    "strat[a-z]*\\s+by\\s+(?:the\\s+)?([A-Za-z][A-Za-z0-9_]*)", text,
    ignore.case = TRUE))[[1]]
  if (length(m) >= 2 && nzchar(m[2])) toupper(m[2]) else NULL
}

#' Classify the executable methods a gated section needs
#'
#' Deterministic keyword mapping from a section's text to the specific
#' arsbridge-executable methods it calls for (ADR 0001), plus the residual
#' inferential indicators that remain unsupported. Used by `build_ars_json()` to
#' build a partial section: descriptive rows plus one analysis per executable
#' method, reserving only the residual.
#'
#' @param section A parsed/enriched TLF section.
#' @return `list(executable = list(list(method_id, strata)), residual =
#'   character())`. `strata` is `NULL` unless the method needs and names one.
#' @keywords internal
#' @noRd
classify_section_methods <- function(section) {
  raw <- .section_text(section)
  txt <- tolower(raw)
  exec <- list(); residual <- character()
  if (!nzchar(trimws(txt))) return(list(executable = exec, residual = residual))

  handled_cmh <- FALSE
  ## Clopper-Pearson exact CI -- needs no operand beyond response + grouping.
  if (grepl("clopper[-\\s]*pearson", txt, perl = TRUE)) {
    exec[[length(exec) + 1L]] <- list(method_id = "MTH_PROPORTION_CI_EXACT",
                                      strata = NULL)
  }
  ## CMH -- executable only when a stratification variable is named.
  if (grepl("cochran[-\\s]*mantel[-\\s]*haenszel|\\bcmh\\b", txt, perl = TRUE)) {
    strata <- .extract_strata(raw)
    if (!is.null(strata)) {
      exec[[length(exec) + 1L]] <- list(method_id = "MTH_CMH_TEST",
                                        strata = strata)
      handled_cmh <- TRUE
    } else {
      residual <- c(residual,
                    "Cochran-Mantel-Haenszel test (no stratification variable named)")
    }
  }

  ## Inferential indicators we cannot yet compute -> residual. The generic
  ## p-value indicator is dropped (it is implied by any handled test) so a fully
  ## classified table is not needlessly reserved.
  skip <- c("p-value (hypothesis test)",
            "exact confidence interval (Clopper-Pearson)")
  if (handled_cmh) skip <- c(skip, "Cochran-Mantel-Haenszel test")
  for (ind in .UNSUPPORTED_INDICATORS) {
    if (ind$label %in% skip) next
    if (grepl(ind$re, txt, perl = TRUE, ignore.case = TRUE)) {
      residual <- c(residual, ind$label)
    }
  }
  list(executable = exec, residual = unique(residual))
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
