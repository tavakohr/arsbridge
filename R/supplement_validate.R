## arsbridge -- supplement_validate.R
## ---------------------------------------------------------------------------
## Pre-flight validation of a format-v4 supplement. The pure-R checks here are
## the in-package authority: they run everywhere (no V8 / jsonvalidate needed),
## produce FAIL/WARN/INFO findings worded so a `regenerate:` line can be pasted
## straight back to the chat assistant, and -- when there are FAILs -- attach a
## ready-to-paste repair prompt for the Phase-2B repair loop.
##
## When the Suggests-only {jsonvalidate} is installed, a JSON Schema pass is
## added as extra WARN rows (a structural second opinion), but it is never
## required.

## Per-TLF fields the v4 format defines. Unknown fields are an INFO, not fatal.
.SUPPLEMENT_TLF_FIELDS <- c(
  "title", "outputType", "analysis_type", "methodId", "is_supported",
  "unsupported_reason", "analysisSet", "recordFilter", "groupings",
  "includeTotal", "columnHierarchy", "analyses", "listingColumns", "sorting",
  "anchors", "provenance"
)

#' Validate a Copilot supplement file (format v4) before running spec_to_ars()
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
    ## conditionMessage() on a cli::cli_abort() condition can carry raw
    ## ANSI/OSC-8 escape codes when the session that raised it had colour
    ## detection on (RStudio, a real terminal). This finding's `problem` is
    ## surfaced verbatim in the Shiny app's plain-text repair-prompt panel
    ## (no ANSI rendering there), so strip it here.
    note("FAIL", NA, "file", cli::ansi_strip(conditionMessage(e)))
    NULL
  })

  spec_keys <- NULL
  if (!is.null(supp) && !is.null(adam_spec_path)) {
    spec <- tryCatch(parse_adam_spec(adam_spec_path), error = function(e) {
      note("WARN", NA, "spec",
           sprintf("Could not parse the ADaM spec for the gate check: %s",
                   cli::ansi_strip(conditionMessage(e))))
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

      extra <- setdiff(names(entry), .SUPPLEMENT_TLF_FIELDS)
      for (f in extra) {
        hit <- .SUPPLEMENT_TLF_FIELDS[tolower(.SUPPLEMENT_TLF_FIELDS) == tolower(f)]
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
          paste(.SUPP_ANALYSIS_TYPES, collapse = "|")))
      } else if (!at %in% .SUPP_ANALYSIS_TYPES) {
        note("FAIL", tlf, "analysis_type", sprintf(
          "regenerate: analysis_type must be one of %s",
          paste(.SUPP_ANALYSIS_TYPES, collapse = "|")))
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
      .validate_column_hierarchy(entry, tlf, note, check_ref, check_where)
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

#' Validate the `columnHierarchy` of one TLF entry (v4 hierarchical column
#' tree): the includeTotal conflict, node completeness, parent references,
#' duplicate ids, and each node condition through the shared where/ref
#' checks.
#' @noRd
.validate_column_hierarchy <- function(entry, tlf, note, check_ref, check_where) {
  hierarchy <- entry$columnHierarchy
  if (is.null(hierarchy)) return(invisible(NULL))

  if (isTRUE(as.logical(entry$includeTotal %||% FALSE))) {
    note("FAIL", tlf, "columnHierarchy",
         "regenerate: columnHierarchy and includeTotal:true cannot be declared together -- declare the overall Total as a GRAND_TOTAL node instead")
  }

  mode <- toupper(trimws(as.character(hierarchy$mode %||% "")))
  if (!mode %in% c("NESTED", "ASYMMETRIC_NESTED")) {
    note("FAIL", tlf, "columnHierarchy",
         "regenerate: columnHierarchy.mode must be NESTED or ASYMMETRIC_NESTED")
  }

  nodes <- hierarchy$nodes %||% list()
  if (length(nodes) < 2) {
    note("FAIL", tlf, "columnHierarchy",
         "regenerate: columnHierarchy needs at least two nodes (one per display column, plus spanning parents)")
    return(invisible(NULL))
  }

  ids <- vapply(nodes, function(n) trimws(as.character(n$id %||% "")), character(1))
  if (anyDuplicated(ids[nzchar(ids)])) {
    note("FAIL", tlf, "columnHierarchy", "regenerate: node ids must be unique")
  }

  for (i in seq_along(nodes)) {
    n <- nodes[[i]]
    where <- sprintf("columnHierarchy.nodes[%d]", i)
    ntype <- toupper(trimws(as.character(n$nodeType %||% "")))

    if (!nzchar(ids[i])) {
      note("FAIL", tlf, where, "regenerate: each node needs a non-empty 'id'")
    }
    if (!nzchar(trimws(as.character(n$label %||% "")))) {
      note("FAIL", tlf, where, "regenerate: each node needs a non-empty 'label'")
    }
    if (!ntype %in% c("GROUP", "LEAF", "SUBTOTAL", "GRAND_TOTAL")) {
      note("FAIL", tlf, where,
           "regenerate: nodeType must be GROUP, LEAF, SUBTOTAL, or GRAND_TOTAL")
    }

    par <- trimws(as.character(n$parentId %||% ""))
    if (nzchar(par) && !par %in% ids) {
      note("FAIL", tlf, where, sprintf(
        "regenerate: parentId '%s' does not match any node id", par))
    }

    has_cond <- !is.null(n$condition) || !is.null(n$compoundExpression)
    if (has_cond) {
      check_where(n, tlf, where)
    } else if (ntype %in% c("GROUP", "LEAF")) {
      note("WARN", tlf, where, sprintf(
        "a %s node usually needs a condition (which subjects fall in this column?)",
        ntype))
    }
    if (ntype == "GRAND_TOTAL" && has_cond) {
      note("WARN", tlf, where,
           "a GRAND_TOTAL node is scoped by the analysis set; its own condition is ignored")
    }
    if (ntype == "SUBTOTAL" && !nzchar(par)) {
      note("FAIL", tlf, where,
           "regenerate: a SUBTOTAL node needs a parentId -- it totals its parent's column group")
    }
  }
  invisible(NULL)
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
    ## A suppression entry removes a wrongly-parsed row; it names the row
    ## and nothing else, so the variable requirement does not apply to it.
    if (isTRUE(a$suppress)) next
    ref <- .supp_var_ref(a$variable)
    if (!nzchar(ref)) {
      note("FAIL", tlf, where, "regenerate: each analysis needs a 'variable' with 'dataset' and 'variable' (or 'suppress': true to remove the row)")
    } else {
      check_ref(ref, tlf, where)
    }
    if (!is.null(a$whereClause)) check_where(a$whereClause, tlf, sprintf("%s/whereClause", where))
    mid <- trimws(as.character(a$methodId %||% ""))
    if (nzchar(mid) && !mid %in% .SUPP_METHOD_IDS) {
      note("WARN", tlf, where, sprintf(
        "methodId '%s' is not a catalogue id -- a placeholder method will be used", mid))
    }
    ## parentRowLabel may point to a displayed stub row that is not itself a
    ## supplement analysis (a category header like "Sex, n (%)"), which the
    ## validator cannot see -- so a non-matching parent is only advisory. A
    ## self-reference, though, is always an error.
    par <- trimws(as.character(a$parentRowLabel %||% ""))
    self <- trimws(as.character(a$rowLabel %||% ""))
    if (nzchar(par) && identical(par, self)) {
      note("FAIL", tlf, where, sprintf(
        "regenerate: parentRowLabel '%s' points at its own row", par))
    } else if (nzchar(par) && !par %in% labels) {
      note("INFO", tlf, where, sprintf(
        "parentRowLabel '%s' is not another analysis in this TLF; confirm it matches a displayed stub row", par))
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

#' Pre-flight a Phase-1 blueprint file (tlf_extraction_blueprints.json).
#'
#' Advisory by design: arsbridge never consumes the blueprint (only the chat
#' assistant does, in Phase 2), so this exists to catch a truncated paste or
#' a wrong-version file BEFORE the user burns a Phase-2 session on it. The
#' Phase-1 contract pins the version (2) and the one-entry-per-TLF rule but
#' not exact key names, so the checks are tolerant: FAIL only for unusable
#' files, WARN/INFO for things worth a look.
#'
#' Returns the same findings shape as [ars_validate_supplement()]:
#' `severity` / `tlf` / `where` / `problem`, zero rows = clean.
#' @noRd
.validate_blueprint <- function(path) {
  findings <- list()
  note <- function(severity, tlf, where, problem) {
    findings[[length(findings) + 1L]] <<- data.frame(
      severity = severity, tlf = tlf %||% NA_character_,
      where = where, problem = problem, stringsAsFactors = FALSE)
  }
  done <- function() {
    if (length(findings) == 0) {
      data.frame(severity = character(), tlf = character(),
                 where = character(), problem = character(),
                 stringsAsFactors = FALSE)
    } else {
      do.call(rbind, findings)
    }
  }

  bp <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) e
  )
  if (inherits(bp, "error")) {
    note("FAIL", NA, "file", sprintf(
      "Not valid JSON (%s) -- re-copy the complete blueprint from the assistant.",
      conditionMessage(bp)))
    return(done())
  }
  if (!is.list(bp)) {
    note("FAIL", NA, "file", "The blueprint must be a JSON object.")
    return(done())
  }

  ## Version: the contract says blueprint version 2; the exact key name is
  ## not pinned, so probe the likely spellings.
  version <- bp[["blueprint_version"]] %||% bp[["blueprintVersion"]] %||%
    bp[["version"]]
  if (is.null(version)) {
    note("WARN", NA, "version",
         "No blueprint version field found -- expected blueprint version 2.")
  } else if (!identical(as.integer(version[[1]]), 2L)) {
    note("FAIL", NA, "version", sprintf(
      "Blueprint version is %s but this arsbridge expects 2 -- regenerate Phase 1 with the current instructions.",
      as.character(version[[1]])))
  }

  ## The TLF collection: a named object under a likely key, else the largest
  ## named-list member (tolerant), else nothing usable.
  collection <- bp[["tlfs"]] %||% bp[["blueprints"]] %||%
    bp[["tlf_blueprints"]]
  if (is.null(collection)) {
    named_lists <- Filter(function(x) {
      is.list(x) && length(x) > 0 && !is.null(names(x)) && all(nzchar(names(x)))
    }, bp)
    if (length(named_lists) > 0) {
      sizes <- vapply(named_lists, length, integer(1))
      collection <- named_lists[[which.max(sizes)]]
    }
  }
  if (is.null(collection) || length(collection) == 0) {
    note("FAIL", NA, "tlfs",
         "No per-TLF blueprint collection found -- the file must carry one entry per output.")
    return(done())
  }

  keys <- names(collection) %||% character(0)
  dups <- unique(keys[duplicated(keys)])
  for (d in dups) {
    note("FAIL", d, "tlfs", "Duplicate TLF key -- every TLF must appear exactly once.")
  }

  ## Per-TLF status and placeholders, where the entry exposes them.
  status_of <- function(entry) {
    toupper(trimws(as.character(
      entry[["blueprint_status"]] %||% entry[["blueprintStatus"]] %||%
        entry[["status"]] %||% ""
    )))
  }
  for (key in keys) {
    entry <- collection[[key]]
    if (!is.list(entry)) next
    status <- status_of(entry)
    if (status %in% c("BLUEPRINT_INCOMPLETE", "FAILED")) {
      note("WARN", key, "status", sprintf(
        "Blueprint status is %s -- Phase 2 will struggle with this TLF; consider another Phase 1 pass.",
        status))
    } else if (identical(status, "READY_WITH_REVIEW")) {
      note("INFO", key, "status",
           "Blueprint status is READY_WITH_REVIEW -- check its review items during Phase 2.")
    }
    flat <- paste(unlist(entry), collapse = " ")
    if (grepl("__REQUIRED_VALUE__|__RESOLVE_OR_REVIEW__", flat)) {
      note("INFO", key, "placeholders",
           "Carries unresolved placeholders -- legitimate in a blueprint; Phase 2 must resolve them.")
    }
  }

  done()
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
