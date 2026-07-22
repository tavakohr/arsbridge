## arsbridge -- supplement_validate.R
## ---------------------------------------------------------------------------
## Pre-flight validation of a format-v3 supplement. The pure-R checks here are
## the in-package authority: they run everywhere (no V8 / jsonvalidate needed),
## produce FAIL/WARN/INFO findings worded so a `regenerate:` line can be pasted
## straight back to the chat assistant, and -- when there are FAILs -- attach a
## ready-to-paste repair prompt for the Phase-2B repair loop.
##
## When the Suggests-only {jsonvalidate} is installed, a JSON Schema pass is
## added as extra WARN rows (a structural second opinion), but it is never
## required.

## Per-TLF fields the v3 format defines. Unknown fields are an INFO, not fatal.
.SUPPLEMENT_V3_FIELDS <- c(
  "title", "outputType", "analysis_type", "methodId", "is_supported",
  "unsupported_reason", "analysisSet", "recordFilter", "groupings",
  "includeTotal", "analyses", "listingColumns", "sorting", "anchors",
  "provenance"
)

#' Validate a Copilot supplement file (format v3) before running spec_to_ars()
#'
#' Pre-flight check for the supplement workflow (see
#' [ars_copilot_instructions()]): parses the file, checks the format version,
#' every typed condition, analysis, grouping, and enum. With `adam_spec_path`
#' it additionally verifies every referenced variable against the ADaM spec --
#' the same hard gate `spec_to_ars()` applies. Findings are printed and
#' returned; `regenerate:` messages can be pasted back to the assistant, and a
#' `repair_prompt` attribute bundles all FAILs into one paste-ready block.
#'
#' @param path           Path to the supplement `.json`.
#' @param adam_spec_path Optional path to the ADaM spec (`.xlsx`/`.xml`);
#'   enables the spec gate check.
#'
#' @return Invisibly, a data frame of findings with columns `severity`
#'   (`FAIL`/`WARN`/`INFO`), `tlf`, `where`, `problem`. Zero rows = clean.
#'   When any FAIL is present the data frame carries a `repair_prompt`
#'   attribute (a single string to paste back to the assistant).
#'
#' @seealso [ars_copilot_instructions()] to produce the upload files,
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

  gate_ok <- function(ref) is.null(spec_keys) || ref %in% spec_keys
  var_re  <- paste0("^", .ADAM_DS, "\\.", .ADAM_VAR, "$")

  ## Check one "DATASET.VARIABLE" reference for syntax then spec membership.
  check_ref <- function(ref, tlf, where) {
    ref <- toupper(trimws(as.character(ref %||% "")))
    if (!grepl(var_re, ref, perl = TRUE)) {
      note("FAIL", tlf, where, sprintf(
        "regenerate: '%s' is not DATASET.VARIABLE (e.g. ADSL.AGE)", ref))
      return(invisible(NULL))
    }
    if (!gate_ok(ref)) {
      note("FAIL", tlf, where, sprintf(
        "regenerate: %s is not in the ADaM spec -- use only variables from the uploaded spec workbook", ref))
    }
    invisible(NULL)
  }

  ## Validate a typed where-clause: structure (via .supp_where), spec gate on
  ## every referenced variable, and single-value arity for ordered comparators.
  check_where <- function(x, tlf, where) {
    wr <- .supp_where(x, where)
    if (length(wr$problems) > 0 || is.null(wr$where)) {
      note("FAIL", tlf, where, sprintf(
        "regenerate: %s", paste(wr$problems, collapse = "; ")))
      return(invisible(NULL))
    }
    for (r in .where_refs(wr$where)) check_ref(r, tlf, where)
    for (msg in .arity_warnings(wr$where, where)) note("WARN", tlf, where, msg)
    invisible(NULL)
  }

  if (!is.null(supp)) {
    for (tlf in names(supp$tlfs)) {
      entry <- supp$tlfs[[tlf]]
      if (!is.list(entry)) {
        note("FAIL", tlf, "tlfs", "regenerate: this TLF's value must be a JSON object")
        next
      }

      extra <- setdiff(names(entry), .SUPPLEMENT_V3_FIELDS)
      for (f in extra) {
        hit <- .SUPPLEMENT_V3_FIELDS[tolower(.SUPPLEMENT_V3_FIELDS) == tolower(f)]
        msg <- if (length(hit) > 0) sprintf("unknown field '%s' ignored (did you mean '%s'? field names are case-sensitive)", f, hit[1])
               else sprintf("unknown field '%s' ignored", f)
        note("INFO", tlf, "fields", msg)
      }

      if (!nzchar(trimws(as.character(entry$title %||% "")))) {
        note("INFO", tlf, "title",
             "add a 'title' (the shell heading text) so arsbridge can confirm it parsed this same table")
      }

      ot <- toupper(trimws(as.character(entry$outputType %||% "")))
      if (nzchar(ot) && !ot %in% c("TABLE", "LISTING", "FIGURE")) {
        note("WARN", tlf, "outputType", "outputType should be TABLE, LISTING, or FIGURE")
      }

      at <- toupper(trimws(as.character(entry$analysis_type %||% "")))
      if (!nzchar(at)) {
        note("FAIL", tlf, "analysis_type", sprintf(
          "regenerate: analysis_type is required -- one of %s",
          paste(.SUPPLEMENT_V3_ANALYSIS_TYPES, collapse = "|")))
      } else if (!at %in% .SUPPLEMENT_V3_ANALYSIS_TYPES) {
        note("FAIL", tlf, "analysis_type", sprintf(
          "regenerate: analysis_type must be one of %s",
          paste(.SUPPLEMENT_V3_ANALYSIS_TYPES, collapse = "|")))
      }

      if (is.null(entry$is_supported)) {
        note("FAIL", tlf, "is_supported", "regenerate: is_supported (true/false) is required")
      } else if (!isTRUE(as.logical(entry$is_supported)) &&
                 !nzchar(trimws(as.character(entry$unsupported_reason %||% "")))) {
        note("WARN", tlf, "unsupported_reason",
             "is_supported is false but no unsupported_reason was given")
      }

      mid <- trimws(as.character(entry$methodId %||% ""))
      if (nzchar(mid) && !mid %in% .SUPP_METHOD_IDS) {
        note("WARN", tlf, "methodId", sprintf(
          "methodId '%s' is not a catalogue id -- a placeholder method will be used (one of %s)",
          mid, paste(.SUPP_METHOD_IDS, collapse = ", ")))
      }

      if (!is.null(entry$analysisSet)) check_where(entry$analysisSet, tlf, "analysisSet")
      if (!is.null(entry$recordFilter)) check_where(entry$recordFilter, tlf, "recordFilter")

      .validate_groupings(entry$groupings, tlf, note, check_ref, check_where)
      .validate_analyses(entry, tlf, note, check_ref, check_where)

      for (i in seq_along(entry$listingColumns %||% list())) {
        lc <- entry$listingColumns[[i]]
        where <- sprintf("listingColumns[%d]", i)
        if (!nzchar(trimws(as.character(lc$label %||% "")))) {
          note("FAIL", tlf, where, "regenerate: each listing column needs a non-empty 'label'")
        }
        ref <- .supp_var_ref(lc$variable)
        if (!nzchar(ref)) {
          note("FAIL", tlf, where, "regenerate: each listing column needs a 'variable' with 'dataset' and 'variable'")
        } else {
          check_ref(ref, tlf, where)
        }
      }

      for (i in seq_along(entry$sorting %||% list())) {
        s <- entry$sorting[[i]]
        where <- sprintf("sorting[%d]", i)
        ref <- paste0(toupper(trimws(as.character(s$dataset %||% ""))), ".",
                      toupper(trimws(as.character(s$variable %||% ""))))
        if (ref == ".") {
          note("FAIL", tlf, where, "regenerate: each sort key needs a 'dataset' and 'variable'")
        } else {
          check_ref(ref, tlf, where)
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
      ## Escape braces so cli does not treat a message's literal { } as glue.
      msg <- sprintf("[%s] %s: %s",
                     out$tlf[i] %||% "-", out$where[i], out$problem[i])
      msg <- gsub("}", "}}", gsub("{", "{{", msg, fixed = TRUE), fixed = TRUE)
      cli::cli_bullets(stats::setNames(msg, bullet))
    }
    if (n_fail == 0) {
      cli::cli_alert_info("No FAILs -- the supplement is usable as-is.")
    } else {
      attr(out, "repair_prompt") <- .supplement_repair_prompt(out)
    }
  }
  invisible(out)
}

#' Single-value arity WARNings for ordered comparators (GT/GE/LT/LE need
#' exactly one value). Walks a normalised where-clause recursively.
#' @noRd
.arity_warnings <- function(where, loc) {
  if (is.null(where)) return(character(0))
  if (!is.null(where[["compoundExpression"]])) {
    cls <- where[["compoundExpression"]][["whereClauses"]]
    return(unlist(lapply(cls, .arity_warnings, loc = loc)) %||% character(0))
  }
  cond <- where[["condition"]]
  if (is.null(cond)) return(character(0))
  comp <- toupper(.as_scalar_char(cond[["comparator"]]) %||% "")
  if (comp %in% c("GT", "GE", "LT", "LE") &&
      length(cond[["value"]] %||% list()) != 1L) {
    return(sprintf("comparator %s expects exactly one value", comp))
  }
  character(0)
}

#' Validate the `groupings` array of one TLF entry.
#' @noRd
.validate_groupings <- function(groupings, tlf, note, check_ref, check_where) {
  for (i in seq_along(groupings %||% list())) {
    g <- groupings[[i]]
    where <- sprintf("groupings[%d]", i)
    ref <- .supp_grouping_ref(g)
    if (!nzchar(ref)) {
      note("FAIL", tlf, where, "regenerate: each grouping needs 'groupingDataset' and 'groupingVariable'")
      next
    }
    check_ref(ref, tlf, where)
    data_driven <- isTRUE(as.logical(g$dataDriven))
    grps <- g$groups %||% list()
    if (!data_driven && length(grps) < 2) {
      note("WARN", tlf, where,
           "a condition-defined column axis needs 2+ groups; set dataDriven:true or add the other columns")
    }
    seen_order <- integer(0)
    for (j in seq_along(grps)) {
      grp <- grps[[j]]
      gwhere <- sprintf("groupings[%d].groups[%d]", i, j)
      if (!nzchar(trimws(as.character(grp$label %||% "")))) {
        note("FAIL", tlf, gwhere, "regenerate: each group needs a non-empty 'label'")
      }
      if (grepl("total", tolower(as.character(grp$label %||% "")))) {
        note("WARN", tlf, gwhere, "a group labelled like 'Total' -- use includeTotal:true instead of a group")
      }
      if (is.null(grp$condition) && is.null(grp$compoundExpression)) {
        note("FAIL", tlf, gwhere, "regenerate: each group needs a typed 'condition' or 'compoundExpression'")
      } else {
        check_where(grp, tlf, gwhere)
      }
      ord <- suppressWarnings(as.integer(grp$order %||% NA))
      if (!is.na(ord)) {
        if (ord %in% seen_order) note("WARN", tlf, gwhere, sprintf("duplicate order %d", ord))
        seen_order <- c(seen_order, ord)
      }
    }
  }
}

#' Validate the `analyses` array of one TLF entry.
#' @noRd
.validate_analyses <- function(entry, tlf, note, check_ref, check_where) {
  labels <- vapply(entry$analyses %||% list(),
                   function(a) trimws(as.character(a$rowLabel %||% "")),
                   character(1))
  seen_order <- integer(0)
  for (i in seq_along(entry$analyses %||% list())) {
    a <- entry$analyses[[i]]
    where <- sprintf("analyses[%d]", i)
    if (!nzchar(trimws(as.character(a$rowLabel %||% "")))) {
      note("FAIL", tlf, where, "regenerate: each analysis needs a non-empty 'rowLabel' (the stub text verbatim)")
    }
    ref <- .supp_var_ref(a$variable)
    if (!nzchar(ref)) {
      note("FAIL", tlf, where, "regenerate: each analysis needs a 'variable' with 'dataset' and 'variable'")
    } else {
      check_ref(ref, tlf, where)
    }
    if (!is.null(a$whereClause)) check_where(a$whereClause, tlf, sprintf("%s/whereClause", where))
    mid <- trimws(as.character(a$methodId %||% ""))
    if (nzchar(mid) && !mid %in% .SUPP_METHOD_IDS) {
      note("WARN", tlf, where, sprintf(
        "methodId '%s' is not a catalogue id -- a placeholder method will be used", mid))
    }
    par <- trimws(as.character(a$parentRowLabel %||% ""))
    if (nzchar(par) && !par %in% labels) {
      note("FAIL", tlf, where, sprintf(
        "regenerate: parentRowLabel '%s' names no other analysis rowLabel in this TLF", par))
    }
    conf <- toupper(trimws(as.character(a$confidence %||% "")))
    if (nzchar(conf) && !conf %in% c("HIGH", "MEDIUM", "LOW")) {
      note("INFO", tlf, where, "confidence should be HIGH, MEDIUM, or LOW")
    }
    ord <- suppressWarnings(as.integer(a$order %||% NA))
    if (!is.na(ord)) {
      if (ord %in% seen_order) note("WARN", tlf, where, sprintf("duplicate order %d", ord))
      seen_order <- c(seen_order, ord)
    }
  }
}

#' Bundle every FAIL into one paste-ready repair prompt for the assistant --
#' the Phase-2B repair loop pastes this back verbatim.
#' @noRd
.supplement_repair_prompt <- function(out) {
  fails <- out[out$severity == "FAIL", , drop = FALSE]
  if (nrow(fails) == 0) return(NULL)
  bullets <- sprintf("- [TLF %s] %s: %s",
                     fails$tlf %||% "-", fails$where, fails$problem)
  paste(
    "The supplement failed validation. Fix ONLY the following, keep every",
    "other TLF and field byte-for-byte identical, and re-emit the COMPLETE",
    "supplement as one fenced strict-JSON block:",
    "",
    paste(bullets, collapse = "\n"),
    sep = "\n")
}
