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

.SUPPLEMENT_VERSION <- 1L

## Filename of the static instruction document (in inst/copilot/ and as the
## default name ars_copilot_instructions() writes).
.COPILOT_INSTRUCTIONS_FILE <- "arsbridge_copilot_instructions.md"

## Enum accepted for supplement analysis_type -- must stay in sync with
## .enrich_type() in enrich_with_llm.R.
.SUPPLEMENT_ANALYSIS_TYPES <- c("CONTINUOUS", "CATEGORICAL", "SURVIVAL",
                                "AE_FREQUENCY", "FIGURE", "LISTING", "OTHER")

## ---------------------------------------------------------------------------
## The static instruction file
## ---------------------------------------------------------------------------

#' Write the Copilot instruction file for the supplement workflow
#'
#' Environments with no LLM API access can still boost `spec_to_ars()`
#' accuracy with a chat assistant (GitHub Copilot, ChatGPT, an enterprise
#' portal): upload the instruction file this function writes TOGETHER WITH
#' your annotated shell `.docx` and ADaM spec `.xlsx`, and the assistant
#' replies with one standard `supplement.json`. Pass that file to
#' `spec_to_ars(supplement = "supplement.json")`.
#'
#' The instruction file is static and versioned -- do not edit it; the
#' format it requests is what `spec_to_ars()` knows how to validate.
#'
#' @param dir       Directory to write the file into. Default: the current
#'   working directory.
#' @param open      Open the file for reading after writing it (so you can
#'   see what the assistant will be told). Default: `TRUE` in interactive
#'   sessions.
#' @param overwrite Overwrite an existing copy. Default `FALSE` (the
#'   existing copy is reported and kept).
#'
#' @return Invisibly, the absolute path of the instruction file.
#'
#' @section Where the file comes from:
#' The instruction file ships *inside* the installed package at
#' `inst/copilot/arsbridge_copilot_instructions.md`. This function resolves it
#' with `system.file("copilot", ...)` and copies it into `dir`, so you never
#' need to know the internal package path. (Under `devtools::load_all()` it
#' falls back to the source tree's `inst/copilot/`.)
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
#' ars_copilot_instructions()   # writes ./arsbridge_copilot_instructions.md
#' }
#' @export
ars_copilot_instructions <- function(dir = ".",
                                     open = interactive(),
                                     overwrite = FALSE) {
  src <- system.file("copilot", .COPILOT_INSTRUCTIONS_FILE,
                     package = "arsbridge")
  if (!nzchar(src)) {
    dev_path <- file.path("inst", "copilot", .COPILOT_INSTRUCTIONS_FILE)
    if (file.exists(dev_path)) src <- dev_path
  }
  if (!nzchar(src) || !file.exists(src)) {
    cli::cli_abort("Instruction file not found in the installed package: {.file {.COPILOT_INSTRUCTIONS_FILE}}")
  }

  ## Create the target directory the user named rather than aborting -- they
  ## explicitly asked us to write there.
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
  dest <- file.path(dir, .COPILOT_INSTRUCTIONS_FILE)

  if (file.exists(dest) && !isTRUE(overwrite)) {
    cli::cli_alert_info("Using the existing copy at {.path {dest}} (pass {.code overwrite = TRUE} to refresh it).")
  } else {
    ok <- file.copy(src, dest, overwrite = TRUE)
    if (!isTRUE(ok)) {
      cli::cli_abort("Could not write {.path {dest}}.")
    }
    cli::cli_alert_success("Wrote {.path {dest}}")
  }

  cli::cli_h2("Supplement workflow (no API key needed)")
  cli::cli_ol(c(
    "Upload to your chat assistant (Copilot/ChatGPT): this instruction file + your annotated shell {.file .docx} + your ADaM spec {.file .xlsx}.",
    "Save the assistant's JSON reply as {.file supplement.json}.",
    "Optional pre-flight: {.code ars_validate_supplement(\"supplement.json\", \"<adam_spec>.xlsx\")}.",
    "Run {.code spec_to_ars(shell, spec, supplement = \"supplement.json\")}."
  ))

  if (isTRUE(open)) {
    tryCatch(file.show(dest, title = .COPILOT_INSTRUCTIONS_FILE),
             error = function(e) invisible(NULL))
  }
  invisible(normalizePath(dest))
}

## ---------------------------------------------------------------------------
## Reading + validating a supplement
## ---------------------------------------------------------------------------

#' Strip a markdown code fence (```json ... ```) if the assistant included
#' one, and normalise the "smart" quotes chat UIs substitute into JSON.
#' @noRd
.clean_supplement_text <- function(txt) {
  txt <- paste(txt, collapse = "\n")
  ## Smart double quotes (U+201C / U+201D) -> ASCII. Smart single quotes are
  ## left alone: they may legitimately appear INSIDE a where-clause value.
  ## The pattern is written with \u escapes so the source stays ASCII-portable
  ## (R CMD check: "Portable packages must use only ASCII characters").
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
        "i" = "Ask the assistant to re-emit ONE fenced strict-JSON block (double quotes, no trailing commas, no comments)."
      ))
    }
  )

  ver <- suppressWarnings(as.integer(supp$supplement_version %||% NA))
  if (is.na(ver) || ver != .SUPPLEMENT_VERSION) {
    cli::cli_abort(c(
      "Supplement version mismatch: file says {.val {supp$supplement_version %||% 'none'}}, this arsbridge expects {.val {(.SUPPLEMENT_VERSION)}}.",
      "i" = "Regenerate the supplement with the instruction file from {.code ars_copilot_instructions()} of THIS package version."
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

#' Validate a Copilot supplement file before running spec_to_ars()
#'
#' Pre-flight check for the supplement workflow (see
#' [ars_copilot_instructions()]): parses the file, checks the format
#' version, every binding's shape and variable syntax, and the enrichment
#' enums. With `adam_spec_path` it additionally verifies every proposed
#' variable against the ADaM spec -- the same hard gate `spec_to_ars()`
#' applies. Findings are printed and returned; messages are worded so they
#' can be pasted back to the assistant to regenerate the file.
#'
#' @param path           Path to the supplement `.json`.
#' @param adam_spec_path Optional path to the ADaM spec (`.xlsx`/`.xml`);
#'   enables the spec gate check.
#'
#' @return Invisibly, a data frame of findings with columns `severity`
#'   (`FAIL`/`WARN`/`INFO`), `tlf`, `where`, `problem`. Zero rows = clean.
#'
#' @seealso [ars_copilot_instructions()] to produce the upload file,
#'   [spec_to_ars()] to consume the validated supplement (`supplement =`).
#'   Full walkthrough: `vignette("no-api-access")`.
#'
#' @examples
#' \dontrun{
#' ars_validate_supplement("supplement.json", "adam_spec.xlsx")
#' }
#' @export
ars_validate_supplement <- function(path, adam_spec_path = NULL) {
  findings <- list()
  note <- function(severity, tlf, where, problem) {
    findings[[length(findings) + 1L]] <<- data.frame(
      severity = severity, tlf = tlf %||% NA_character_,
      where = where, problem = problem, stringsAsFactors = FALSE)
  }

  supp <- tryCatch(read_supplement(path), error = function(e) {
    note("FAIL", NA, "file", conditionMessage(e))
    NULL
  })

  spec_keys <- NULL
  if (!is.null(supp) && !is.null(adam_spec_path)) {
    spec <- tryCatch(parse_adam_spec(adam_spec_path), error = function(e) {
      note("WARN", NA, "spec",
           sprintf("Could not parse the ADaM spec for the gate check: %s",
                   conditionMessage(e)))
      NULL
    })
    if (!is.null(spec)) spec_keys <- toupper(names(spec$lookup %||% list()))
  }

  var_re <- paste0("^", .ADAM_DS, "\\.", .ADAM_VAR, "$")
  check_var <- function(v, tlf, where) {
    v <- toupper(trimws(as.character(v %||% "")))
    if (!grepl(var_re, v, perl = TRUE)) {
      note("FAIL", tlf, where, sprintf(
        "regenerate: '%s' is not DATASET.VARIABLE syntax (e.g. ADSL.AGE)", v))
      return(invisible(NULL))
    }
    if (!is.null(spec_keys) && !v %in% spec_keys) {
      note("FAIL", tlf, where, sprintf(
        "regenerate: %s is not in the ADaM spec -- use only variables from the uploaded spec workbook", v))
    }
    invisible(NULL)
  }

  known_fields <- c("bindings", "columns", "population", "analysis_type",
                    "ars_method_name", "by_variables", "include_total",
                    "is_supported", "unsupported_reason")

  if (!is.null(supp)) {
    for (tlf in names(supp$tlfs)) {
      entry <- supp$tlfs[[tlf]]
      if (!is.list(entry)) {
        note("FAIL", tlf, "tlfs", "regenerate: this TLF's value must be a JSON object")
        next
      }
      extra <- setdiff(names(entry), known_fields)
      if (length(extra) > 0) {
        note("INFO", tlf, "fields", sprintf(
          "unknown field(s) ignored: %s", paste(extra, collapse = ", ")))
      }
      for (i in seq_along(entry$bindings %||% list())) {
        b <- entry$bindings[[i]]
        where <- sprintf("bindings[%d]", i)
        if (!is.list(b) || !nzchar(trimws(as.character(b$label %||% "")))) {
          note("FAIL", tlf, where, "regenerate: each binding needs a non-empty 'label' (the stub text verbatim)")
          next
        }
        check_var(b$variable, tlf, where)
      }
      if (!is.null(entry$columns))    check_var(entry$columns, tlf, "columns")
      if (!is.null(entry$population)) {
        pop_refs <- extract_annotation_vars(as.character(entry$population))
        if (length(pop_refs) == 0) {
          note("WARN", tlf, "population",
               "population carries no DATASET.VARIABLE reference (e.g. \"ADSL.SAFFL='Y'\")")
        } else {
          for (r in pop_refs) check_var(r, tlf, "population")
        }
      }
      at <- toupper(trimws(as.character(entry$analysis_type %||% "")))
      if (nzchar(at) && !at %in% .SUPPLEMENT_ANALYSIS_TYPES) {
        note("FAIL", tlf, "analysis_type", sprintf(
          "regenerate: analysis_type must be one of %s",
          paste(.SUPPLEMENT_ANALYSIS_TYPES, collapse = "|")))
      }
      for (bv in entry$by_variables %||% list()) {
        if (!is.character(bv) && !is.null(bv)) {
          note("FAIL", tlf, "by_variables",
               "regenerate: by_variables must be an array of bare variable-name strings")
          break
        }
      }
    }
  }

  out <- if (length(findings) == 0) {
    data.frame(severity = character(), tlf = character(),
               where = character(), problem = character(),
               stringsAsFactors = FALSE)
  } else {
    do.call(rbind, findings)
  }

  if (nrow(out) == 0) {
    cli::cli_alert_success("Supplement is clean: {length(supp$tlfs %||% list())} TLF entr{?y/ies}, ready for {.code spec_to_ars(supplement = ...)}.")
  } else {
    n_fail <- sum(out$severity == "FAIL")
    cli::cli_alert_warning("{nrow(out)} finding{?s} ({n_fail} FAIL). FAIL rows must be fixed; paste the {.val regenerate:} messages back to the assistant.")
    for (i in seq_len(nrow(out))) {
      bullet <- switch(out$severity[i], FAIL = "x", WARN = "!", "i")
      cli::cli_bullets(stats::setNames(
        sprintf("[%s] %s: %s",
                out$tlf[i] %||% "-", out$where[i], out$problem[i]),
        bullet))
    }
    if (n_fail == 0) {
      cli::cli_alert_info("No FAILs -- the supplement is usable as-is.")
    }
  }
  invisible(out)
}

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

#' Apply one TLF's supplement bindings to a parsed section.
#'
#' Fill-gaps-only policy (decided with the package owner): a supplement
#' binding lands ONLY on a row the deterministic pass left unannotated.
#' Conflicts (row already annotated with a different variable) are WARNs --
#' the regex result stands. Statistic sub-rows ("Mean (SD)", "Median", ...)
#' are never bound (ADR 0003: they are layout rows of the analysis above).
#' Every variable passes the hard ADaM-spec gate first.
#'
#' @return The section, with bound rows carrying
#'   `detection_method = "supplement"`, `detection_confidence = "medium"`.
#' @noRd
.apply_supplement_bindings <- function(sec, supp_tlf, spec_lookup) {
  if (is.null(supp_tlf)) return(sec)
  spec_keys <- toupper(names(spec_lookup %||% list()))
  gate_ok <- function(ref) length(spec_keys) == 0 || ref %in% spec_keys

  labels_norm <- vapply(sec$stub_rows %||% list(),
                        function(r) .norm_label(r$label), character(1))
  n_applied <- 0L; n_conflict <- 0L; n_unmatched <- 0L; n_rejected <- 0L

  for (b in supp_tlf$bindings %||% list()) {
    label <- trimws(as.character(b$label %||% ""))
    ref   <- toupper(trimws(as.character(b$variable %||% "")))
    if (!nzchar(label) || !nzchar(ref)) next

    ## --- HARD SPEC GATE (same policy as the live LLM path) ---------------
    if (!gate_ok(ref)) {
      n_rejected <- n_rejected + 1L
      .diag_gap(
        stage = "supplement", severity = "FAIL", input = INPUT_SUPPLEMENT,
        problem = sprintf("Supplement variable %s for row '%s' is not in the ADaM spec; rejected.",
                          ref, label),
        why = "A proposed variable absent from the ADaM spec is treated as a hallucination, never shipped.",
        fix = sprintf("Fix this binding in the supplement (regenerate it, or edit the JSON) or add %s to the ADaM spec.", ref),
        tlf_number = sec$tlf_number, location = label)
      next
    }

    idx <- .match_stub_label(.norm_label(label), labels_norm)
    if (is.na(idx)) {
      n_unmatched <- n_unmatched + 1L
      diag_add(
        stage = "supplement", severity = "WARN", input = INPUT_SUPPLEMENT,
        problem = sprintf("Supplement binding label '%s' matched no stub row", label),
        tlf_number = sec$tlf_number,
        action = "Binding skipped -- the label must be the stub text as it appears in the shell"
      )
      next
    }

    ## Statistic sub-rows belong to the analysis row above them; binding one
    ## would create a duplicate analysis block (same gate as the LLM path).
    if (.norm_label(sec$stub_rows[[idx]]$label %||% "") %in% .STATLINE_ROW_LABELS) {
      diag_add(
        stage = "supplement", severity = "INFO", input = INPUT_SUPPLEMENT,
        problem = sprintf("Binding for statistic sub-row '%s' skipped", label),
        tlf_number = sec$tlf_number,
        action = "Statistic lines are layout rows of the analysis above them; annotate the parent row instead"
      )
      next
    }

    where <- trimws(as.character(b$where %||% ""))
    annotation <- if (nzchar(where)) paste0(ref, " WHERE ", where) else ref

    if (isTRUE(sec$stub_rows[[idx]]$has_annot)) {
      existing <- toupper(trimws(sec$stub_rows[[idx]]$annotation %||% ""))
      if (!startsWith(existing, ref)) {
        n_conflict <- n_conflict + 1L
        diag_add(
          stage = "supplement", severity = "WARN", input = INPUT_SUPPLEMENT,
          problem = sprintf("Row '%s': supplement proposes %s but the shell annotation reads %s",
                            sec$stub_rows[[idx]]$label %||% label, ref, existing),
          tlf_number = sec$tlf_number,
          action = "Shell annotation kept (regex wins) -- review this row if the shell is wrong"
        )
      }
      next   ## fill gaps only: annotated rows are never touched
    }

    sec$stub_rows[[idx]]$annotation           <- annotation
    sec$stub_rows[[idx]]$has_annot            <- TRUE
    sec$stub_rows[[idx]]$detection_method     <- "supplement"
    sec$stub_rows[[idx]]$detection_confidence <- "medium"
    n_applied <- n_applied + 1L
  }

  ## Column-axis + population: only when the deterministic pass found none.
  cols <- toupper(trimws(as.character(supp_tlf$columns %||% "")))
  if (nzchar(cols) && is.null(sec$column_annotation)) {
    if (gate_ok(cols)) {
      sec$column_annotation <- cols
    } else {
      .diag_gap(
        stage = "supplement", severity = "FAIL", input = INPUT_SUPPLEMENT,
        problem = sprintf("Supplement column variable %s is not in the ADaM spec; rejected.", cols),
        fix = "Fix the supplement's 'columns' field or add the variable to the spec.",
        tlf_number = sec$tlf_number)
    }
  }
  pop <- trimws(as.character(supp_tlf$population %||% ""))
  if (nzchar(pop) && !nzchar(sec$population_annot %||% "")) {
    pop_refs <- toupper(extract_annotation_vars(pop))
    if (length(pop_refs) > 0 && all(vapply(pop_refs, gate_ok, logical(1)))) {
      sec$population_annot <- pop
    } else {
      diag_add(
        stage = "supplement", severity = "WARN", input = INPUT_SUPPLEMENT,
        problem = sprintf("Supplement population '%s' rejected (no valid in-spec DATASET.VARIABLE reference)", pop),
        tlf_number = sec$tlf_number,
        action = "Population left as parsed from the shell"
      )
    }
  }

  if (n_applied + n_conflict + n_unmatched + n_rejected > 0) {
    diag_add(
      stage = "supplement", severity = "INFO", input = INPUT_SUPPLEMENT,
      problem = sprintf("Supplement bindings: %d applied, %d conflict%s (regex kept), %d unmatched, %d rejected by the spec gate",
                        n_applied, n_conflict,
                        if (n_conflict == 1) "" else "s",
                        n_unmatched, n_rejected),
      tlf_number = sec$tlf_number,
      action = "See the per-binding findings above for anything other than 'applied'"
    )
  }
  sec
}

#' Map one TLF's supplement fields into the shape `.enrich_structured()`
#' returns, so `enrich_with_llm()` consumes them through the exact same path
#' as a live LLM answer. Returns `list()` when the supplement has nothing
#' for this TLF (the enricher then falls back to heuristics with a WARN).
#' @noRd
.supplement_enrich_answer <- function(supp_tlf) {
  if (is.null(supp_tlf)) return(list())
  out <- list()
  nz_chr <- function(x) {
    x <- trimws(as.character(x %||% ""))
    if (nzchar(x)) x else NULL
  }
  at <- toupper(nz_chr(supp_tlf$analysis_type) %||% "")
  if (nzchar(at) && at %in% .SUPPLEMENT_ANALYSIS_TYPES) out$analysis_type <- at
  mn <- nz_chr(supp_tlf$ars_method_name)
  if (!is.null(mn)) out$ars_method_name <- mn
  bv <- supp_tlf$by_variables %||% list()
  if (length(bv) > 0) out$by_variables <- bv
  if (!is.null(supp_tlf$include_total)) {
    out$include_total <- isTRUE(as.logical(supp_tlf$include_total))
  }
  if (!is.null(supp_tlf$is_supported)) {
    out$is_supported <- isTRUE(as.logical(supp_tlf$is_supported))
    ur <- nz_chr(supp_tlf$unsupported_reason)
    if (!is.null(ur)) out$unsupported_reason <- ur
  }
  out
}
