## arsbridge -- supplement.R
## ---------------------------------------------------------------------------
## Supplement mode: LLM-grade help with no API access.
##
## The user uploads the raw annotated shell + ADaM spec + the static
## instruction file shipped in inst/copilot/ to a chat assistant (Copilot,
## ChatGPT, ...). The assistant returns ONE standard JSON document -- the
## "supplement" -- which spec_to_ars(supplement = ...) consumes in place of
## the live LLM:
##   * bindings fill ONLY the rows the deterministic regex left unannotated
##     (regex wins conflicts -- a disagreement is a WARN, never an override);
##   * per-TLF enrichment fields feed the same path a live LLM answer would.
##
## Safe by construction:
##   * label-keyed, never row-index-keyed -- the assistant's own reading of
##     the docx cannot misalign rows; binding goes through the same fuzzy
##     label matcher as below-table annotations (.match_stub_label);
##   * every proposed variable passes the HARD ADaM-SPEC GATE, exactly like
##     a live LLM proposal (extract_shell_llm.R) -- hallucinations are
##     dropped and logged as blockers, never shipped;
##   * versioned format (supplement_version) so old files fail loudly.

.SUPPLEMENT_VERSION <- 3L

## Filenames of the static instruction documents (in inst/copilot/) and the
## shipped v3 JSON Schema (in inst/schema/). ars_copilot_instructions() copies
## the set for the chosen workflow into the user's directory.
.COPILOT_INSTRUCTIONS_FILE <- "arsbridge_copilot_instructions.md"
.COPILOT_PHASE1_FILE       <- "arsbridge_phase1_blueprint_instructions.md"
.COPILOT_PHASE2_FILE       <- "arsbridge_phase2_build_instructions.md"
.SUPPLEMENT_SCHEMA_FILE    <- "arsbridge_supplement_v3.schema.json"

## Enum accepted for supplement analysis_type -- must stay in sync with
## .enrich_type() in enrich_with_llm.R.
.SUPPLEMENT_ANALYSIS_TYPES <- c("CONTINUOUS", "CATEGORICAL", "SURVIVAL",
                                "AE_FREQUENCY", "FIGURE", "LISTING", "OTHER")

## ---------------------------------------------------------------------------
## The static instruction file
## ---------------------------------------------------------------------------

## Resolve a shipped resource (installed package first, source tree fallback).
#' @noRd
.copilot_resolve <- function(subdir, file) {
  src <- system.file(subdir, file, package = "arsbridge")
  if (!nzchar(src)) {
    dev <- file.path("inst", subdir, file)
    if (file.exists(dev)) src <- dev
  }
  src
}

#' Write the Copilot instruction files for the supplement workflow
#'
#' Environments with no LLM API access can still boost `spec_to_ars()`
#' accuracy with a chat assistant (GitHub Copilot, ChatGPT, an enterprise
#' portal): upload the instruction file(s) this function writes TOGETHER WITH
#' your annotated shell `.docx`, ADaM spec `.xlsx`, and the shipped JSON
#' Schema, and the assistant replies with one strict `supplement.json`
#' (format v3). Pass that file to `spec_to_ars(supplement = "supplement.json")`.
#'
#' Two workflows are offered:
#' * `"single"` (default): one instruction file. The assistant reads the shell
#'   and spec and returns the supplement in one pass. Best for small/medium
#'   shells.
#' * `"two_phase"`: two instruction files. Phase 1 produces an evidence
#'   blueprint (`tlf_extraction_blueprints.json`); Phase 2 turns that blueprint
#'   into the supplement plus a validation report. Best for large or complex
#'   shells where a single pass misses items -- the phases force explicit
#'   evidence discovery then semantic construction with review cycles.
#'
#' The files are static and versioned -- do not edit them; the format they
#' request is what `spec_to_ars()` knows how to validate. The JSON Schema
#' (`arsbridge_supplement_v3.schema.json`) is written alongside so the
#' assistant can self-check its reply, and so can
#' [ars_validate_supplement()] (when `jsonvalidate` is installed).
#'
#' @param dir       Directory to write the files into. Default: the current
#'   working directory.
#' @param workflow  `"single"` (default) or `"two_phase"`. See Details.
#' @param open      Open the first written file for reading. Default: `TRUE`
#'   in interactive sessions.
#' @param overwrite Overwrite existing copies. Default `FALSE` (existing
#'   copies are reported and kept).
#'
#' @return Invisibly, a character vector of the absolute paths written.
#'
#' @section Data note:
#' Uploading the shell and spec to a chat assistant transmits their text
#' (TLF titles, stub labels, variable names -- never patient data, which
#' these documents do not contain) to that provider. Confirm your
#' organisation's policy first.
#'
#' @seealso [ars_validate_supplement()] to pre-flight the reply,
#'   [spec_to_ars()] to consume it (`supplement =`), and [set_llm_key()] if an
#'   API key becomes available. Full walkthrough: `vignette("no-api-access")`.
#'
#' @examples
#' \dontrun{
#' ars_copilot_instructions()                       # single-file workflow
#' ars_copilot_instructions(workflow = "two_phase") # blueprint + build
#' }
#' @export
ars_copilot_instructions <- function(dir = ".",
                                     workflow = c("single", "two_phase"),
                                     open = interactive(),
                                     overwrite = FALSE) {
  workflow <- match.arg(workflow)

  ## The set of (subdir, filename) resources for the chosen workflow. The JSON
  ## Schema ships in every set so the assistant always has something to
  ## self-validate against.
  files <- if (identical(workflow, "single")) {
    list(c("copilot", .COPILOT_INSTRUCTIONS_FILE),
         c("schema",  .SUPPLEMENT_SCHEMA_FILE))
  } else {
    list(c("copilot", .COPILOT_PHASE1_FILE),
         c("copilot", .COPILOT_PHASE2_FILE),
         c("schema",  .SUPPLEMENT_SCHEMA_FILE))
  }

  ## Create the target directory the user named rather than aborting.
  if (!dir.exists(dir)) {
    ok <- dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    if (!isTRUE(ok) && !dir.exists(dir)) {
      cli::cli_abort(c(
        "Could not create the target directory {.path {dir}}.",
        "i" = "Pass an existing, writable folder as {.arg dir}."
      ))
    }
    cli::cli_alert_info("Created {.path {dir}}.")
  }

  written <- character(0)
  for (f in files) {
    src <- .copilot_resolve(f[1], f[2])
    if (!nzchar(src) || !file.exists(src)) {
      cli::cli_abort("Shipped resource not found in the package: {.file {f[2]}}")
    }
    dest <- file.path(dir, f[2])
    if (file.exists(dest) && !isTRUE(overwrite)) {
      cli::cli_alert_info("Using the existing copy at {.path {dest}} (pass {.code overwrite = TRUE} to refresh it).")
    } else {
      ok <- file.copy(src, dest, overwrite = TRUE)
      if (!isTRUE(ok)) cli::cli_abort("Could not write {.path {dest}}.")
      cli::cli_alert_success("Wrote {.path {dest}}")
    }
    written <- c(written, normalizePath(dest))
  }

  if (identical(workflow, "single")) {
    cli::cli_h2("Supplement workflow (no API key needed)")
    cli::cli_ol(c(
      "Upload to your chat assistant: the instruction file + the JSON Schema + your annotated shell {.file .docx} + your ADaM spec {.file .xlsx}.",
      "Save the assistant's JSON reply as {.file supplement.json}.",
      "Optional pre-flight: {.code ars_validate_supplement(\"supplement.json\", \"<adam_spec>.xlsx\")}.",
      "Run {.code spec_to_ars(shell, spec, supplement = \"supplement.json\")}."
    ))
  } else {
    cli::cli_h2("Two-phase supplement workflow (no API key needed)")
    cli::cli_ol(c(
      "Session 1 (Phase 1): upload the Phase-1 file + shell + spec. Save the reply as {.file tlf_extraction_blueprints.json}.",
      "Session 2 (Phase 2): upload the Phase-2 file + the JSON Schema + shell + the blueprint. Save {.file supplement.json} and {.file extraction_validation_report.json}.",
      "Optional pre-flight: {.code ars_validate_supplement(\"supplement.json\", \"<adam_spec>.xlsx\")}.",
      "Run {.code spec_to_ars(shell, spec, supplement = \"supplement.json\")}."
    ))
  }

  if (isTRUE(open) && length(written) > 0) {
    tryCatch(file.show(written[1], title = basename(written[1])),
             error = function(e) invisible(NULL))
  }
  invisible(written)
}

## ---------------------------------------------------------------------------
## Reading + validating a supplement
## ---------------------------------------------------------------------------

#' Strip a markdown code fence (```json ... ```) if the assistant included
#' one and normalise the "smart" double quotes chat UIs substitute into JSON.
#'
#' Format v3 carries conditions as typed objects, not strings, so the v2
#' `="..."` double-quote-value repair is gone: a bare `=` next to a quote can
#' now only appear inside a legitimate title, and rewriting it would corrupt
#' the file. Malformed typed JSON is a clean parse error the assistant fixes
#' by re-emitting one strict-JSON block.
#' @noRd
.clean_supplement_text <- function(txt) {
  txt <- paste(txt, collapse = "\n")
  ## Smart double quotes (U+201C / U+201D) -> ASCII. Written with \u escapes so
  ## the source stays ASCII-portable (R CMD check: "Portable packages must use
  ## only ASCII characters").
  txt <- gsub("[\u201c\u201d]", "\"", txt)

  ## Keep only the fenced block when present (the contract asks for exactly
  ## one); otherwise use the whole text.
  m <- regmatches(txt, regexec("```(?:json)?\\s*(\\{[\\s\\S]*\\})\\s*```",
                               txt, perl = TRUE))[[1]]
  if (length(m) == 2) return(m[2])
  txt
}

#' Read and structurally validate a supplement file.
#'
#' Aborts (with a message the user can act on) when the file is unreadable,
#' not JSON, carries the wrong version, or has no usable `tlfs` object.
#' Field-level problems are NOT fatal here -- they surface when the fields
#' are applied (or via `ars_validate_supplement()` beforehand).
#'
#' @return The parsed supplement as a nested list (jsonlite
#'   `simplifyVector = FALSE` shape -- same as a live structured response).
#' @noRd
read_supplement <- function(path) {
  .require_file(path, "supplement", INPUT_SUPPLEMENT)
  txt <- .clean_supplement_text(.read_lines(path, "the supplement file"))

  supp <- tryCatch(
    jsonlite::fromJSON(txt, simplifyVector = FALSE),
    error = function(e) {
      err_msg <- conditionMessage(e)
      cli::cli_abort(c(
        "Supplement file is not valid JSON: {.path {path}}",
        "x" = "{err_msg}",
        "i" = "In format v3 conditions are typed objects, not strings: a {.code condition} with dataset, variable, comparator, and a value array.",
        "i" = "Ask the assistant to re-emit ONE fenced strict-JSON block (no trailing commas, no comments, no smart quotes)."
      ))
    }
  )

  ver <- suppressWarnings(as.integer(supp$supplement_version %||% NA))
  if (is.na(ver) || ver != .SUPPLEMENT_VERSION) {
    cli::cli_abort(c(
      "Supplement version mismatch: file says {.val {supp$supplement_version %||% 'none'}}, this arsbridge expects {.val {(.SUPPLEMENT_VERSION)}}.",
      "i" = "Format v3 carries conditions as typed WhereClause objects, not strings -- a v2 file cannot be reused.",
      "i" = "Regenerate the supplement with the instruction files from {.code ars_copilot_instructions()} of THIS package version."
    ))
  }

  tlfs <- supp$tlfs
  if (is.null(tlfs) || !is.list(tlfs) || length(tlfs) == 0 ||
      is.null(names(tlfs)) || any(!nzchar(names(tlfs)))) {
    cli::cli_abort(c(
      "Supplement has no usable {.field tlfs} object (a named map of TLF number -> fields).",
      "i" = "Ask the assistant to follow the ANSWER FORMAT section of the instruction file."
    ))
  }

  supp
}

## `ars_validate_supplement()` (the v3 pre-flight validator) lives in
## R/supplement_validate.R.

## ---------------------------------------------------------------------------
## Applying a supplement
## ---------------------------------------------------------------------------

#' Normalise a TLF identifier for matching supplement keys to parsed
#' sections. "14.1.1", "Table 14.1.1", and the parser's own "T-14-1-1" all
#' normalise to "14.1.1": uppercase, unify separators to ".", drop a
#' designator prefix (word or initial).
#' @noRd
.norm_tlf_key <- function(x) {
  x <- toupper(trimws(as.character(x %||% "")))
  x <- gsub("[-_ ]+", ".", x)
  sub("^(TABLE|LISTING|FIGURE|T|L|F)\\.", "", x)
}

#' Find the supplement entry for a section's TLF number (or NULL).
#' @noRd
.match_supplement_tlf <- function(supp, tlf_number) {
  keys <- names(supp$tlfs %||% list())
  hit <- which(vapply(keys, function(k)
    identical(.norm_tlf_key(k), .norm_tlf_key(tlf_number)), logical(1)))
  if (length(hit) == 0) return(NULL)
  supp$tlfs[[hit[1]]]
}

#' Supplement TLF keys that match none of the parsed sections -- almost
#' always a numbering typo in the assistant's output.
#' @noRd
.supplement_unmatched_tlfs <- function(supp, section_ids) {
  keys <- names(supp$tlfs %||% list())
  ids  <- vapply(section_ids, .norm_tlf_key, character(1))
  keys[!vapply(keys, function(k) .norm_tlf_key(k) %in% ids, logical(1))]
}

#' TRUE when a parsed title and a supplement title agree, tolerantly. The
#' parser may keep more of the heading than the assistant (or vice versa) --
#' e.g. the parser drops the population, the assistant drops a subtitle -- so
#' equal-after-normalisation OR one containing the other counts as agreement.
#' A short title needs exact normalised equality (containment on 1-2 words is
#' meaningless).
#' @noRd
.titles_agree <- function(parsed, supplied) {
  a <- .norm_label(parsed)
  b <- .norm_label(supplied)
  if (!nzchar(a) || !nzchar(b)) return(TRUE)     # nothing to disagree about
  if (identical(a, b)) return(TRUE)
  if (min(nchar(a), nchar(b)) < 6L) return(FALSE)
  grepl(a, b, fixed = TRUE) || grepl(b, a, fixed = TRUE)
}

#' Cross-check the supplement's table inventory against the parsed sections.
#'
#' The supplement tier is the only one whose author (a chat assistant) read
#' the whole shell independently, so it is where "is arsbridge using the
#' right set of tables?" can actually be checked. Returns a list of findings
#' `list(severity, tlf_number, problem, action)` -- all non-blocking -- for:
#'   1. a supplement TLF that matches no parsed section (extra / mis-numbered);
#'   2. a parsed section that the supplement never mentions (a table the
#'      supplement author saw but arsbridge may have parsed differently, or
#'      one the author omitted);
#'   3. a matched pair whose titles disagree (possible wrong table).
#' @noRd
.supplement_crosscheck <- function(supp, sections) {
  findings <- list()
  add <- function(sev, tlf, problem, action) {
    findings[[length(findings) + 1L]] <<-
      list(severity = sev, tlf_number = tlf, problem = problem, action = action)
  }

  section_ids <- vapply(sections, function(s) s$tlf_number %||% "", character(1))

  ## 1. supplement entries with no matching parsed table.
  for (key in .supplement_unmatched_tlfs(supp, section_ids)) {
    add("WARN", key,
        sprintf("Supplement entry '%s' matches no TLF parsed from the shell", key),
        "Entry ignored -- key each TLF by the number in its shell heading (e.g. '14.1.1')")
  }

  ## 2 + 3. parsed tables missing from the supplement, and title mismatches.
  supp_keys_norm <- vapply(names(supp$tlfs %||% list()), .norm_tlf_key, character(1))
  for (sec in sections) {
    in_supp <- .norm_tlf_key(sec$tlf_number) %in% supp_keys_norm
    if (!in_supp) {
      add("WARN", sec$tlf_number,
          sprintf("TLF %s (%s) was parsed from the shell but the supplement has no entry for it",
                  sec$tlf_number,
                  if (nzchar(trimws(sec$title %||% ""))) sec$title else "no title"),
          "Confirm the supplement covers every table -- a missing entry means no supplement help for this one")
      next
    }
    supp_tlf   <- .match_supplement_tlf(supp, sec$tlf_number)
    supp_title <- trimws(as.character(supp_tlf$title %||% ""))
    parsed_title <- trimws(sec$title %||% "")
    if (nzchar(supp_title) && nzchar(parsed_title) &&
        !.titles_agree(parsed_title, supp_title)) {
      add("WARN", sec$tlf_number,
          sprintf("TLF %s: shell parsed the title as '%s' but the supplement says '%s'",
                  sec$tlf_number, parsed_title, supp_title),
          "Verify arsbridge is using the right table -- the heading and the supplement disagree on the title")
    }
  }

  findings
}

#' A supplement v3 `{dataset, variable}` object -> "DATASET.VARIABLE", or "".
#' @noRd
.supp_var_ref <- function(x) {
  if (is.null(x)) return("")
  ds <- toupper(trimws(.as_scalar_char(x[["dataset"]]) %||% ""))
  v  <- toupper(trimws(.as_scalar_char(x[["variable"]]) %||% ""))
  if (!nzchar(ds) || !nzchar(v)) return("")
  paste0(ds, ".", v)
}

#' A supplement v3 confidence value -> a detection_confidence. A supplement is
#' advisory, so even HIGH is only "medium" ground truth; anything else is "low".
#' @noRd
.supp_confidence <- function(x) {
  if (identical(toupper(.as_scalar_char(x) %||% ""), "HIGH")) "medium" else "low"
}

#' A supplement v3 methodId -> the id when it is a catalogue id, else NULL.
#' @noRd
.supp_method_id <- function(x) {
  id <- toupper(trimws(.as_scalar_char(x) %||% ""))
  if (nzchar(id) && id %in% .SUPP_METHOD_IDS) id else NULL
}

#' Apply one TLF's supplement (format v3) to a parsed section.
#'
#' Fill-gaps-only policy (decided with the package owner): a supplement
#' analysis lands ONLY on a row the deterministic pass left unannotated.
#' Conflicts (row already annotated with a different variable) are WARNs --
#' the regex result stands. Statistic sub-rows ("Mean (SD)", "Median", ...)
#' are never bound (ADR 0003: they are layout rows of the analysis above).
#' Every variable passes the hard ADaM-spec gate first.
#'
#' v3 conditions are TYPED (`whereClause`, `analysisSet`, grouping `groups`):
#' they are validated by `.supp_where()` and stored as internal WhereClauses
#' (`sec$population_where`, `row$supplement_where`, group `condition`), so the
#' builders consume them without ever re-parsing a string.
#'
#' @param trust `"fill_gaps"` (default): a supplement value lands only where
#'   the regex left a gap; a conflict keeps the shell value (regex wins).
#'   `"prefer_supplement"`: a validated, spec-gated supplement value OVERRIDES
#'   the shell value on a conflict, with a WARN recording both; the gate is
#'   never bypassed, and the overridden shell value is kept as a secondary
#'   analysis so nothing is dropped.
#'
#' @return The section, with bound rows carrying
#'   `detection_method = "supplement"` and a `supplement_where` filter.
#' @noRd
.apply_supplement_bindings <- function(sec, supp_tlf, spec_lookup,
                                       trust = c("fill_gaps", "prefer_supplement")) {
  if (is.null(supp_tlf)) return(sec)
  trust <- match.arg(trust)
  prefer <- identical(trust, "prefer_supplement")
  spec_keys <- toupper(names(spec_lookup %||% list()))
  gate_ok <- function(ref) length(spec_keys) == 0 || ref %in% spec_keys

  ## Fill an empty parsed title from the supplement (the assistant read the
  ## heading directly). Only when the shell gave us nothing -- a parsed title
  ## always wins, and a disagreement is surfaced by .supplement_crosscheck().
  supp_title <- trimws(as.character(supp_tlf$title %||% ""))
  if (nzchar(supp_title) && !nzchar(trimws(sec$title %||% ""))) {
    sec$title <- supp_title
    diag_add(
      stage = "supplement", severity = "INFO", input = INPUT_SUPPLEMENT,
      problem = sprintf("TLF %s: title sourced from the supplement", sec$tlf_number),
      tlf_number = sec$tlf_number, location = supp_title,
      action = "Shell heading had no title; verify the supplement title against the shell"
    )
  }

  labels_norm <- vapply(sec$stub_rows %||% list(),
                        function(r) .norm_label(r$label), character(1))
  n_applied <- 0L; n_conflict <- 0L; n_unmatched <- 0L; n_rejected <- 0L

  ## Listing columns bind exactly like analyses (label -> variable), so fold
  ## them into the same loop; their `order`/`format` are display hints the
  ## renderer takes from the shell, not consumed here.
  bind_items <- supp_tlf$analyses %||% list()
  for (lc in supp_tlf$listingColumns %||% list()) {
    bind_items[[length(bind_items) + 1L]] <- list(
      rowLabel = lc$label, variable = lc$variable)
  }

  for (b in bind_items) {
    label <- trimws(as.character(b$rowLabel %||% ""))
    ref   <- .supp_var_ref(b$variable)
    if (!nzchar(label) || !nzchar(ref)) next

    ## --- HARD SPEC GATE (same policy as the live LLM path) ---------------
    if (!gate_ok(ref)) {
      n_rejected <- n_rejected + 1L
      .diag_gap(
        stage = "supplement", severity = "FAIL", input = INPUT_SUPPLEMENT,
        problem = sprintf("Supplement variable %s for row '%s' is not in the ADaM spec; rejected.",
                          ref, label),
        why = "A proposed variable absent from the ADaM spec is treated as a hallucination, never shipped.",
        fix = sprintf("Fix this analysis in the supplement (regenerate it, or edit the JSON) or add %s to the ADaM spec.", ref),
        tlf_number = sec$tlf_number, location = label)
      next
    }

    ## Typed row filter: validated once here, stored as an internal WhereClause
    ## and never re-parsed from a string. An invalid clause, or one naming a
    ## variable outside the spec, drops the whole analysis (never silently
    ## weaker filtering).
    parsed_where <- NULL
    if (!is.null(b$whereClause)) {
      wr <- .supp_where(b$whereClause,
                        sprintf("tlfs/%s/analyses[%s]/whereClause",
                                sec$tlf_number %||% "?", label))
      if (length(wr$problems) > 0 || is.null(wr$where)) {
        n_rejected <- n_rejected + 1L
        .diag_gap(
          stage = "supplement", severity = "FAIL", input = INPUT_SUPPLEMENT,
          problem = sprintf("Supplement row '%s' has an invalid whereClause: %s",
                            label, paste(wr$problems, collapse = "; ")),
          why = "A row filter that does not validate is dropped rather than shipped as weaker filtering.",
          fix = "Fix the whereClause in the supplement (typed condition/compoundExpression).",
          tlf_number = sec$tlf_number, location = label)
        next
      }
      bad_refs <- Filter(function(r) !gate_ok(r), .where_refs(wr$where))
      if (length(bad_refs) > 0) {
        n_rejected <- n_rejected + 1L
        .diag_gap(
          stage = "supplement", severity = "FAIL", input = INPUT_SUPPLEMENT,
          problem = sprintf("Supplement row '%s' whereClause references %s, not in the ADaM spec; rejected.",
                            label, paste(bad_refs, collapse = ", ")),
          why = "A condition referencing a variable absent from the ADaM spec is treated as a hallucination.",
          fix = "Fix the whereClause, or add the variable to the ADaM spec.",
          tlf_number = sec$tlf_number, location = label)
        next
      }
      parsed_where <- wr$where
    }

    ## Display annotation (round-trips through parse_where_clause) -- drives
    ## primary-variable extraction and method inference downstream; the typed
    ## clause above is what actually filters.
    annotation <- if (!is.null(parsed_where)) {
      paste0(ref, " WHERE ", .where_to_annotation(parsed_where))
    } else {
      ref
    }

    idx <- .match_stub_label(.norm_label(label), labels_norm)
    if (is.na(idx)) {
      n_unmatched <- n_unmatched + 1L
      ## No stub row carries this label, but the variable already passed the
      ## spec gate -- keep the analysis as a free-standing one for the builder
      ## rather than dropping it (the supplement named an analysis the shell
      ## has no row for). build_ars_json() materialises these onto the output.
      sec$supplement_extra_rows[[length(sec$supplement_extra_rows) + 1L]] <-
        list(label = label, annotation = annotation, where = parsed_where)
      diag_add(
        stage = "supplement", severity = "WARN", input = INPUT_SUPPLEMENT,
        problem = sprintf("Supplement analysis label '%s' matched no stub row", label),
        tlf_number = sec$tlf_number,
        action = "Kept as a free-standing analysis on this TLF -- if it was meant for an existing row, match the label to the stub text as it appears in the shell"
      )
      next
    }

    ## Statistic sub-rows belong to the analysis row above them; binding one
    ## would create a duplicate analysis block (same gate as the LLM path).
    if (.norm_label(sec$stub_rows[[idx]]$label %||% "") %in% .STATLINE_ROW_LABELS) {
      diag_add(
        stage = "supplement", severity = "INFO", input = INPUT_SUPPLEMENT,
        problem = sprintf("Analysis for statistic sub-row '%s' skipped", label),
        tlf_number = sec$tlf_number,
        action = "Statistic lines are layout rows of the analysis above them; annotate the parent row instead"
      )
      next
    }

    if (isTRUE(sec$stub_rows[[idx]]$has_annot)) {
      existing <- toupper(trimws(sec$stub_rows[[idx]]$annotation %||% ""))
      if (!startsWith(existing, ref)) {
        n_conflict <- n_conflict + 1L
        shell_orig <- sec$stub_rows[[idx]]$annotation
        if (prefer) {
          ## prefer_supplement: the (validated, spec-gated) supplement value
          ## overrides the shell's. The shell's original is kept as a secondary
          ## analysis so nothing is dropped, and a WARN records both.
          sec$stub_rows[[idx]]$annotation                <- annotation
          sec$stub_rows[[idx]]$detection_method          <- "supplement"
          sec$stub_rows[[idx]]$detection_confidence      <- .supp_confidence(b$confidence)
          sec$stub_rows[[idx]]$supplement_where          <- parsed_where
          sec$stub_rows[[idx]]$supplement_method_id      <- .supp_method_id(b$methodId)
          sec$stub_rows[[idx]]$secondary_annotation      <- shell_orig
          sec$stub_rows[[idx]]$shell_overridden_annotation <- shell_orig
          sec$stub_rows[[idx]]$supplement_conflict       <- TRUE
          diag_add(
            stage = "supplement", severity = "WARN", input = INPUT_SUPPLEMENT,
            problem = sprintf("Row '%s': supplement %s overrides the shell annotation %s (trust = prefer_supplement)",
                              sec$stub_rows[[idx]]$label %||% label, ref, existing),
            tlf_number = sec$tlf_number,
            action = "Supplement value used; shell original kept as a secondary analysis -- review this row"
          )
        } else {
          ## Fill-gaps policy: the shell annotation stands (regex wins). Keep
          ## the supplement's proposal on the row as provenance rather than
          ## discarding it silently, so a later review step (or the validation
          ## report) can surface the disagreement and the proposal is never lost.
          sec$stub_rows[[idx]]$supplement_proposed_annotation <- annotation
          sec$stub_rows[[idx]]$secondary_annotation           <- annotation
          sec$stub_rows[[idx]]$supplement_conflict            <- TRUE
          sec$stub_rows[[idx]]$supplement_conflict_with       <- existing
          diag_add(
            stage = "supplement", severity = "WARN", input = INPUT_SUPPLEMENT,
            problem = sprintf("Row '%s': supplement proposes %s but the shell annotation reads %s",
                              sec$stub_rows[[idx]]$label %||% label, ref, existing),
            tlf_number = sec$tlf_number,
            action = "Shell annotation kept (regex wins); supplement proposal retained for provenance -- review this row if the shell is wrong"
          )
        }
      }
      next   ## the row is resolved either way; the gap-fill path is below
    }

    sec$stub_rows[[idx]]$annotation           <- annotation
    sec$stub_rows[[idx]]$has_annot            <- TRUE
    sec$stub_rows[[idx]]$detection_method     <- "supplement"
    sec$stub_rows[[idx]]$detection_confidence <- .supp_confidence(b$confidence)
    sec$stub_rows[[idx]]$supplement_where     <- parsed_where
    sec$stub_rows[[idx]]$supplement_method_id <- .supp_method_id(b$methodId)
    ## parentRowLabel / denominator are recorded but not yet computed.
    if (nzchar(trimws(as.character(b$parentRowLabel %||% "")))) {
      sec$stub_rows[[idx]]$supplement_parent_label <- trimws(as.character(b$parentRowLabel))
    }
    if (!is.null(b$denominator)) {
      sec$stub_rows[[idx]]$supplement_denominator <- b$denominator
    }
    n_applied <- n_applied + 1L
  }

  ## Population: from the typed analysisSet, when the shell found none, or (in
  ## prefer_supplement mode) overriding the shell's with a WARN.
  had_pop <- nzchar(sec$population_annot %||% "")
  if (!is.null(supp_tlf$analysisSet) && (!had_pop || prefer)) {
    as3 <- supp_tlf$analysisSet
    wr <- .supp_where(as3, sprintf("tlfs/%s/analysisSet", sec$tlf_number %||% "?"))
    bad_refs <- if (is.null(wr$where)) character(0)
                else Filter(function(r) !gate_ok(r), .where_refs(wr$where))
    if (!is.null(wr$where) && length(bad_refs) == 0) {
      new_pop <- .where_to_annotation(wr$where)
      if (prefer && had_pop && !identical(sec$population_annot, new_pop)) {
        diag_add(
          stage = "supplement", severity = "WARN", input = INPUT_SUPPLEMENT,
          problem = sprintf("Population overridden: shell '%s' -> supplement '%s' (trust = prefer_supplement)",
                            sec$population_annot, new_pop),
          tlf_number = sec$tlf_number,
          action = "Supplement population used -- review this TLF"
        )
      }
      sec$population_where <- wr$where
      sec$population_annot <- new_pop
      lab <- trimws(as.character(as3$label %||% ""))
      if (nzchar(lab) && (!nzchar(sec$population_text %||% "") || prefer)) {
        sec$population_text <- lab
      }
    } else {
      diag_add(
        stage = "supplement", severity = "WARN", input = INPUT_SUPPLEMENT,
        problem = sprintf("Supplement analysisSet rejected: %s",
                          if (length(bad_refs) > 0)
                            paste("not in spec:", paste(bad_refs, collapse = ", "))
                          else paste(wr$problems, collapse = "; ")),
        tlf_number = sec$tlf_number,
        action = "Population left as parsed from the shell"
      )
    }
  }

  ## Column axis + per-column groups: from the ordered groupings, only when the
  ## deterministic shell pass found none. The outermost grouping sets the axis
  ## variable; the first grouping carrying explicit condition groups (>= 2, all
  ## in-spec) becomes the column groups -- each group keeps its typed condition
  ## so .build_group_levels() consumes it with no translation.
  ## In prefer_supplement mode the supplement axis/groups override the shell's.
  groupings <- supp_tlf$groupings %||% list()
  if (length(groupings) > 0 && (is.null(sec$column_annotation) || prefer)) {
    axis_ref <- .supp_grouping_ref(groupings[[1]])
    if (nzchar(axis_ref) && gate_ok(axis_ref)) {
      if (prefer && !is.null(sec$column_annotation) &&
          !identical(sec$column_annotation, axis_ref)) {
        diag_add(
          stage = "supplement", severity = "WARN", input = INPUT_SUPPLEMENT,
          problem = sprintf("Column axis overridden: shell '%s' -> supplement '%s' (trust = prefer_supplement)",
                            sec$column_annotation, axis_ref),
          tlf_number = sec$tlf_number,
          action = "Supplement column axis used -- review this TLF"
        )
      }
      sec$column_annotation <- axis_ref
    }
  }
  if (length(groupings) > 0 && (is.null(sec$column_groups) || prefer)) {
    sec <- .apply_supplement_groups(sec, groupings, gate_ok)
  }

  ## Channels arsbridge records but does not yet compute: kept on the section
  ## (and later copied to the output _meta) so a reviewer -- and a future
  ## engine -- can see them, with one INFO naming what was recorded.
  sec <- .record_supplement_extras(sec, supp_tlf)

  ## Anchor crosscheck: a cheap guard that the assistant read the SAME table
  ## arsbridge parsed. A wrong-table read that a title comparison misses often
  ## shows up as a first-row-label or row-count mismatch.
  .supplement_anchor_check(sec, supp_tlf$anchors)

  if (n_applied + n_conflict + n_unmatched + n_rejected > 0) {
    diag_add(
      stage = "supplement", severity = "INFO", input = INPUT_SUPPLEMENT,
      problem = sprintf("Supplement analyses: %d applied, %d conflict%s (regex kept), %d unmatched, %d rejected by the spec gate",
                        n_applied, n_conflict,
                        if (n_conflict == 1) "" else "s",
                        n_unmatched, n_rejected),
      tlf_number = sec$tlf_number,
      action = "See the per-analysis findings above for anything other than 'applied'"
    )
  }
  sec
}

#' Record the supplement channels arsbridge does not yet compute onto the
#' section (`sec$supplement_extras`) and emit one INFO naming them. Consuming
#' these means new engine/renderer behaviour, out of scope for now; keeping
#' them visible means a reviewer never loses what the supplement supplied.
#' @noRd
.record_supplement_extras <- function(sec, supp_tlf) {
  extras <- sec$supplement_extras %||% list()
  recorded <- character(0)

  if (!is.null(supp_tlf$recordFilter)) {
    wr <- .supp_where(supp_tlf$recordFilter,
                      sprintf("tlfs/%s/recordFilter", sec$tlf_number %||% "?"))
    if (!is.null(wr$where)) {
      extras$recordFilter <- wr$where
      recorded <- c(recorded, "recordFilter")
    }
  }
  if (length(supp_tlf$sorting %||% list()) > 0) {
    extras$sorting <- supp_tlf$sorting
    recorded <- c(recorded, "sorting")
  }
  if (!is.null(supp_tlf$provenance)) {
    extras$provenance <- supp_tlf$provenance
    recorded <- c(recorded, "provenance")
  }

  ## Per-row parent/denominator hints stored while binding also count as
  ## recorded-but-not-computed context.
  has_parent <- any(vapply(sec$stub_rows %||% list(),
                           function(r) !is.null(r$supplement_parent_label), logical(1)))
  has_denom  <- any(vapply(sec$stub_rows %||% list(),
                           function(r) !is.null(r$supplement_denominator), logical(1)))
  if (has_parent) recorded <- c(recorded, "parentRowLabel")
  if (has_denom)  recorded <- c(recorded, "denominator")

  if (length(extras) > 0) sec$supplement_extras <- extras
  if (length(recorded) > 0) {
    diag_add(
      stage = "supplement", severity = "INFO", input = INPUT_SUPPLEMENT,
      problem = sprintf("Recorded but not yet computed: %s",
                        paste(unique(recorded), collapse = ", ")),
      tlf_number = sec$tlf_number,
      action = "Carried into the output's _meta.supplement for review; not used in computation yet"
    )
  }
  sec
}

#' Warn when the supplement's row anchors disagree with the parsed stub rows --
#' a cheap "did the assistant read the same table?" guard.
#' @noRd
.supplement_anchor_check <- function(sec, anchors) {
  if (is.null(anchors)) return(invisible(NULL))
  labels <- vapply(sec$stub_rows %||% list(),
                   function(r) trimws(as.character(r$label %||% "")), character(1))
  first <- trimws(as.character(anchors$firstRowLabel %||% ""))
  if (nzchar(first) && length(labels) > 0 &&
      !identical(.norm_label(first), .norm_label(labels[1]))) {
    diag_add(
      stage = "supplement", severity = "WARN", input = INPUT_SUPPLEMENT,
      problem = sprintf("Anchor mismatch: supplement's first row is '%s' but the shell's first stub is '%s'",
                        first, labels[1]),
      tlf_number = sec$tlf_number,
      action = "Confirm the supplement describes the same table arsbridge parsed"
    )
  }
  rc <- suppressWarnings(as.integer(anchors$rowCount %||% NA))
  if (!is.na(rc) && length(labels) > 0 && rc != length(labels)) {
    diag_add(
      stage = "supplement", severity = "WARN", input = INPUT_SUPPLEMENT,
      problem = sprintf("Anchor mismatch: supplement says %d rows but the shell parsed %d stub rows",
                        rc, length(labels)),
      tlf_number = sec$tlf_number,
      action = "Confirm the supplement describes the same table arsbridge parsed"
    )
  }
  invisible(NULL)
}

#' One grouping factor's `{groupingDataset, groupingVariable}` -> "DS.VAR", "".
#' @noRd
.supp_grouping_ref <- function(g) {
  ds <- toupper(trimws(.as_scalar_char(g[["groupingDataset"]]) %||% ""))
  v  <- toupper(trimws(.as_scalar_char(g[["groupingVariable"]]) %||% ""))
  if (!nzchar(ds) || !nzchar(v)) return("")
  paste0(ds, ".", v)
}

#' Build `sec$column_groups` from the first supplement grouping that carries
#' explicit condition groups (>= 2, all in-spec). Each group keeps its typed
#' `condition` so `.build_group_levels()` consumes it directly. Groups
#' referencing an out-of-spec variable are rejected by the hard gate.
#' @noRd
.apply_supplement_groups <- function(sec, groupings, gate_ok) {
  for (g in groupings) {
    raw_groups <- g$groups %||% list()
    if (length(raw_groups) < 2) next
    axis_ref <- .supp_grouping_ref(g)
    if (!nzchar(axis_ref) || !gate_ok(axis_ref)) next
    bare_var <- sub("^.*\\.", "", axis_ref)
    ds       <- sub("\\..*$", "", axis_ref)

    groups <- list(); n_dropped <- 0L
    for (grp in raw_groups) {
      label <- trimws(as.character(grp$label %||% ""))
      if (!nzchar(label)) { n_dropped <- n_dropped + 1L; next }
      wr <- .supp_where(grp, sprintf("tlfs/%s/groupings/%s", sec$tlf_number %||% "?", label))
      bad_refs <- if (is.null(wr$where)) character(0)
                  else Filter(function(r) !gate_ok(r), .where_refs(wr$where))
      if (is.null(wr$where) || length(bad_refs) > 0) {
        n_dropped <- n_dropped + 1L
        detail <- if (length(bad_refs) > 0) paste(bad_refs, collapse = ", ")
                  else paste(wr$problems, collapse = "; ")
        .diag_gap(
          stage = "supplement", severity = "FAIL", input = INPUT_SUPPLEMENT,
          problem = sprintf("Supplement column group '%s' condition rejected (%s).",
                            label, detail),
          why = "A column condition that does not validate, or names a variable absent from the ADaM spec, is treated as a hallucination.",
          fix = "Fix this group's condition in the supplement, or add the variable to the ADaM spec.",
          tlf_number = sec$tlf_number, location = label)
        next
      }
      order <- suppressWarnings(as.integer(grp$order %||% (length(groups) + 1L)))
      groups[[length(groups) + 1L]] <- list(
        label      = label,
        annotation = .where_to_annotation(wr$where),
        condition  = wr$where,
        order      = if (is.na(order)) length(groups) + 1L else order)
    }
    if (length(groups) >= 2) {
      sec$column_groups <- list(variable = bare_var, dataset = ds, groups = groups)
      if (is.null(sec$column_annotation)) sec$column_annotation <- axis_ref
      diag_add(
        stage = "supplement", severity = "INFO", input = INPUT_SUPPLEMENT,
        problem = sprintf("%d column-group condition(s) sourced from the supplement for %s%s",
                          length(groups), axis_ref,
                          if (n_dropped > 0) sprintf(" (%d rejected)", n_dropped) else ""),
        tlf_number = sec$tlf_number,
        action = "Each becomes one display column; verify the labels and conditions in the ARS JSON"
      )
      return(sec)
    }
  }
  sec
}

#' Map one TLF's supplement (v3) fields into the shape `.enrich_structured()`
#' returns, so `enrich_with_llm()` consumes them through the exact same path
#' as a live LLM answer. Returns `list()` when the supplement has nothing
#' for this TLF (the enricher then falls back to heuristics with a WARN).
#'
#' The rich v3 analysis family is folded to an engine family via `.V3_TYPE_MAP`
#' (e.g. MIXED_SUMMARY -> CONTINUOUS); the section-level `methodId` becomes the
#' catalogue method name; groupings become an ordered, dataset-qualified
#' `by_variables` list (`.resolve_grouping_from_spec()` accepts "ADSL.TRT01A").
#' @noRd
.supplement_enrich_answer <- function(supp_tlf) {
  if (is.null(supp_tlf)) return(list())
  out <- list()
  nz_chr <- function(x) {
    x <- trimws(as.character(x %||% ""))
    if (nzchar(x)) x else NULL
  }

  at3 <- toupper(nz_chr(supp_tlf$analysis_type) %||% "")
  if (nzchar(at3) && at3 %in% .SUPPLEMENT_V3_ANALYSIS_TYPES) {
    out$analysis_type <- unname(.V3_TYPE_MAP[[at3]])
  }
  mn <- .method_name_from_id(supp_tlf$methodId)
  if (!is.null(mn)) out$ars_method_name <- mn

  by_vars <- Filter(nzchar, vapply(supp_tlf$groupings %||% list(),
                                   .supp_grouping_ref, character(1)))
  if (length(by_vars) > 0) out$by_variables <- as.list(by_vars)

  if (!is.null(supp_tlf$includeTotal)) {
    out$include_total <- isTRUE(as.logical(supp_tlf$includeTotal))
  }
  if (!is.null(supp_tlf$is_supported)) {
    out$is_supported <- isTRUE(as.logical(supp_tlf$is_supported))
    ur <- nz_chr(supp_tlf$unsupported_reason)
    if (!is.null(ur)) out$unsupported_reason <- ur
  }
  out
}
