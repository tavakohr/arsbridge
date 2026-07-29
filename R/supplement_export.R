## arsbridge -- supplement_export.R
## ---------------------------------------------------------------------------
## The reviewed-manifest workflow, stage 1: turn a parse into a DRAFT
## supplement a human can read, correct, and keep as the source of truth.
##
## Today the .docx is the contract, so every parser miss costs a code change.
## With a draft supplement the parse is a starting point instead: run
## write_supplement_draft() once, review/edit the JSON, then feed it back
## with spec_to_ars(supplement = ..., supplement_trust =
## "prefer_supplement"). A missed row becomes a 30-second file edit.
##
## The draft is emitted in the same v4 format read_supplement() consumes, so
## the whole correction machinery (spec gate, typed WhereClauses, override
## provenance) already applies to it unchanged.

#' Write a draft supplement from a parsed shell
#'
#' Parses an annotated TLF shell and writes what it found as a v4 supplement
#' JSON: one entry per TLF with the title, population (as a typed
#' analysis-set condition), and one analysis per annotated stub row. The
#' file is a *draft*: review it, correct anything the parser got wrong, and
#' keep the reviewed file under version control as the study's extraction
#' manifest. Feed it back with
#' `spec_to_ars(supplement = ..., supplement_trust = "prefer_supplement")`
#' -- a reviewed value then overrides a wrong parse instead of only filling
#' gaps.
#'
#' Rows the parser could not annotate, references outside the ADaM spec, and
#' statistic sub-rows carrying annotations are not silently dropped: each is
#' listed in the entry's `provenance$reviewItems` so the reviewer sees the
#' full roster of open questions per TLF.
#'
#' @param shell_path Path to the annotated TLF shell `.docx`.
#' @param output_path Where to write the draft JSON.
#' @param adam_spec_path Optional path to the ADaM specification workbook.
#'   When given, a draft analysis whose variable is not in the spec is
#'   flagged in `reviewItems` (the same gate `spec_to_ars()` enforces later).
#' @param heading_patterns Optional custom heading patterns, as in
#'   [parse_shell_docx()].
#'
#' @return Invisibly, the normalized output path. The draft is also
#'   validated with [ars_validate_supplement()] and any findings are shown.
#' @export
write_supplement_draft <- function(shell_path,
                                   output_path = "supplement_draft.json",
                                   adam_spec_path = NULL,
                                   heading_patterns = NULL) {
  spec_lookup <- NULL
  if (!is.null(adam_spec_path)) {
    spec_lookup <- parse_adam_spec(adam_spec_path)$lookup
  }

  sections <- parse_shell_docx(shell_path, spec_lookup = spec_lookup,
                               heading_patterns = heading_patterns)
  if (length(sections) == 0) {
    cli::cli_abort("No TLF sections parsed from {.path {shell_path}} -- nothing to draft.")
  }

  supp <- .sections_to_supplement(sections, spec_lookup)
  json <- jsonlite::toJSON(supp, auto_unbox = TRUE, pretty = TRUE)
  writeLines(json, output_path)

  ## Immediately validate what we just wrote with the same pre-flight the
  ## reviewed file will face later -- a draft that fails its own format is
  ## an arsbridge bug the user should see now, not at spec_to_ars() time.
  findings <- ars_validate_supplement(output_path,
                                      adam_spec_path = adam_spec_path)
  n_fail <- if (is.data.frame(findings)) {
    sum(findings$severity == "FAIL")
  } else {
    0L
  }
  if (n_fail > 0) {
    cli::cli_warn(paste(
      "The draft has {n_fail} validation failure{?s} --",
      "see {.code ars_validate_supplement()} for the findings."))
  }

  cli::cli_inform(c(
    "v" = paste("Draft supplement written to {.path {output_path}}",
                "({length(supp$tlfs)} TLF{?s})."),
    "i" = paste("Review and correct it, keep it under version control,",
                "then run {.code spec_to_ars(..., supplement = <path>,",
                'supplement_trust = "prefer_supplement")}.')
  ))
  invisible(normalizePath(output_path, winslash = "/", mustWork = FALSE))
}

#' Assemble the v4 supplement object for a list of parsed sections.
#' @noRd
.sections_to_supplement <- function(sections, spec_lookup = NULL) {
  tlfs <- list()
  for (sec in sections) {
    ## Key by the full designator-bearing number ("T-14-3-1"), not the bare
    ## normalized one: a study can have BOTH Table 14.3.1 and Figure 14.3.1,
    ## and bare "14.3.1" keys would overwrite each other here and be
    ## ambiguous at match time. .match_supplement_tlf() still matches these
    ## keys to sections by normalized number plus designator.
    key <- trimws(sec$tlf_number %||% "")
    if (!nzchar(key)) next
    tlfs[[key]] <- .section_to_supplement_tlf(sec, spec_lookup)
  }
  list(
    supplement_version = 4L,
    generator = list(
      workflow            = "write_supplement_draft",
      instruction_version = as.character(utils::packageVersion("arsbridge"))
    ),
    tlfs = tlfs
  )
}

#' One section -> one v4 tlfEntry. Everything the parser is unsure about
#' lands in provenance$reviewItems rather than being dropped.
#' @noRd
.section_to_supplement_tlf <- function(sec, spec_lookup = NULL) {
  spec_keys <- toupper(names(spec_lookup %||% list()))
  in_spec <- function(ref) length(spec_keys) == 0 || ref %in% spec_keys

  atype <- .infer_analysis_type(sec)
  entry <- list(
    title         = sec$title %||% "",
    outputType    = sec$tlf_type %||% "TABLE",
    analysis_type = atype,
    is_supported  = TRUE
  )
  method <- switch(atype,
                   CONTINUOUS   = "MTH_SUMMARY_STATISTICS_CONTINUOUS",
                   CATEGORICAL  = "MTH_COUNT_AND_PERCENTAGE",
                   AE_FREQUENCY = "MTH_AE_FREQUENCY_COUNT",
                   SURVIVAL     = "MTH_KAPLAN_MEIER_ESTIMATE",
                   LISTING      = "MTH_LISTING",
                   NULL)
  if (!is.null(method)) entry$methodId <- method

  review <- character(0)

  ## Population -> typed analysisSet. A population the parser saw but could
  ## not turn into a condition is a review item, not a silent omission.
  pop_annot <- trimws(sec$population_annot %||% "")
  if (nzchar(pop_annot)) {
    pop_where <- parse_where_clause(pop_annot)
    if (!is.null(pop_where)) {
      analysis_set <- pop_where
      pop_label <- trimws(sec$population_text %||% "")
      if (nzchar(pop_label)) {
        analysis_set <- c(list(label = pop_label), analysis_set)
      }
      entry$analysisSet <- analysis_set
    } else {
      review <- c(review, sprintf(
        "Population annotation '%s' did not parse into a condition -- add the analysisSet by hand.",
        pop_annot))
    }
  } else if (nzchar(trimws(sec$population_text %||% ""))) {
    review <- c(review, sprintf(
      "Population line '%s' carries no annotation -- add the analysisSet by hand.",
      trimws(sec$population_text)))
  }

  ## Stub rows -> analyses. Bare labels, unresolvable annotations, and
  ## annotated statistic sub-rows go to reviewItems.
  analyses <- list()
  rows <- sec$stub_rows %||% list()
  for (i in seq_along(rows)) {
    row <- rows[[i]]
    label <- trimws(row$label %||% "")
    if (!nzchar(label)) next
    is_statline <- .norm_label(label) %in% .STATLINE_ROW_LABELS

    if (!isTRUE(row$has_annot)) {
      if (!is_statline) {
        review <- c(review, sprintf(
          "Row '%s' has no annotation -- add an analysis entry if it needs one.",
          label))
      }
      next
    }
    if (is_statline) {
      review <- c(review, sprintf(
        "Statistic sub-row '%s' carries annotation '%s' -- annotate the parent row instead.",
        label, row$annotation))
      next
    }

    refs <- extract_annotation_vars(row$annotation)
    if (length(refs) == 0) {
      review <- c(review, sprintf(
        "Row '%s': annotation '%s' yields no DATASET.VARIABLE reference.",
        label, row$annotation))
      next
    }
    ref <- refs[[1]]
    if (!in_spec(toupper(ref))) {
      review <- c(review, sprintf(
        "Row '%s': %s is not in the ADaM spec -- spec_to_ars() will reject it as-is.",
        label, ref))
    }

    parts <- strsplit(ref, ".", fixed = TRUE)[[1]]
    analysis <- list(
      rowLabel = label,
      variable = list(dataset = parts[[1]], variable = parts[[2]]),
      order    = length(analyses) + 1L
    )
    wc <- parse_where_clause(row$annotation)
    if (!is.null(wc)) analysis$whereClause <- wc
    conf <- toupper(row$detection_confidence %||% "")
    if (conf %in% c("HIGH", "MEDIUM", "LOW")) analysis$confidence <- conf
    analyses[[length(analyses) + 1L]] <- analysis
  }
  if (length(analyses) > 0) entry$analyses <- analyses

  ## Anchors let .supplement_anchor_check() confirm a reviewed manifest
  ## still matches the shell it was drafted from.
  labels <- vapply(rows, function(r) trimws(r$label %||% ""), character(1))
  labels <- labels[nzchar(labels)]
  entry$anchors <- list(
    firstRowLabel = if (length(labels) > 0) labels[[1]] else "",
    lastRowLabel  = if (length(labels) > 0) labels[[length(labels)]] else "",
    rowCount      = length(labels)
  )

  provenance <- list(blueprintStatus = "draft_from_parse")
  if (length(review) > 0) provenance$reviewItems <- as.list(review)
  entry$provenance <- provenance

  entry
}
