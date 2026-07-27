## arsbridge -- validate_annotations_spec.R
## ---------------------------------------------------------------------------
## For each extracted annotation, look up every DATASET.VARIABLE reference
## against the parsed ADaM spec. Returns a tidy validation report.

#' Pull the literal assigned to `ref` out of an annotation string, e.g.
#' `"ADAE.AESER='Y'"` -> `"Y"`. NA when the annotation is a bare variable
#' reference (no `=`) or a form this doesn't recognise (comparators other
#' than `=`, `IN (...)` lists) -- those are outside this check's scope, not
#' failures of it.
#' @noRd
.extract_annotation_value <- function(annotation, ref) {
  pat <- paste0(gsub("\\.", "\\\\.", ref, perl = TRUE),
               "\\s*=\\s*['\"]([^'\"]*)['\"]")
  m <- regexpr(pat, annotation, perl = TRUE)
  if (m == -1) return(NA_character_)
  trimws(sub(pat, "\\1", regmatches(annotation, m), perl = TRUE))
}

#' Cross-check one variable=value literal against the spec's declared
#' Length and Codelist for that variable -- `ref %in% names(spec_lookup)`
#' alone misses a variable that EXISTS but is bound to a value it cannot
#' hold. The spec's `codelist` field is a codelist NAME, not inline terms,
#' so the actual terms are resolved via `.codelist_for()` against
#' `spec_codelists`. Returns NULL when nothing is checkable or nothing is
#' wrong; otherwise list(status, message).
#' @noRd
.check_annotation_value <- function(value, spec_entry, spec_codelists) {
  if (is.na(value) || !nzchar(value)) return(NULL)

  len <- suppressWarnings(as.numeric(spec_entry$length %||% NA))
  if (!is.na(len) && nchar(value) > len) {
    return(list(
      status = "FAIL",
      message = sprintf(
        paste("Value '%s' is %d character%s but the spec declares length %d --",
             "check this is the right variable and the right literal (a value",
             "that cannot fit is never correct)"),
        value, nchar(value), if (nchar(value) == 1) "" else "s", len
      )
    ))
  }

  cl <- .codelist_for(spec_codelists, spec_entry$dataset, spec_entry$variable, spec_entry)
  if (!is.null(cl) && !is.null(cl$terms) && nrow(cl$terms) > 0) {
    terms <- cl$terms$term
    if (!toupper(value) %in% toupper(terms)) {
      return(list(
        status = "WARN",
        message = sprintf(
          paste("Value '%s' is not in the spec's declared codelist for this",
               "variable (%s) -- check for a typo or a different variable"),
          value, paste(terms, collapse = ", ")
        )
      ))
    }
  }
  NULL
}

#' Cross-reference extracted annotations against the ADaM spec.
#'
#' @param tlf_sections List of TLF sections (output of `parse_shell_docx()`).
#' @param spec_lookup  Named list keyed by `"DATASET.VARIABLE"` (the `lookup`
#'   element of `parse_adam_spec()`).
#' @param spec_codelists Named list of codelists (the `codelists` element of
#'   `parse_adam_spec()`), used to check a bound value against its variable's
#'   declared codelist. Optional -- when `NULL`, the codelist half of the
#'   value check is skipped (existence and length checks still run).
#'
#' @return Data frame with columns: tlf_number, stub_label, annotation,
#'   variable_ref, status (PASS | WARN | FAIL), message.
#'
#' @keywords internal
#' @noRd
validate_annotations_spec <- function(tlf_sections, spec_lookup, spec_codelists = NULL) {
  out_rows <- list()

  ## Helper: validate one annotation, append rows.
  add_rows <- function(tlf_number, stub_label, annotation) {
    refs <- extract_annotation_vars(annotation)
    if (length(refs) == 0) {
      out_rows[[length(out_rows) + 1L]] <<- data.frame(
        tlf_number = tlf_number, stub_label = stub_label,
        annotation = annotation, variable_ref = NA_character_,
        status = "WARN",
        message = "No DATASET.VARIABLE reference parsed from annotation",
        stringsAsFactors = FALSE
      )
      return(invisible())
    }
    for (ref in refs) {
      pieces <- strsplit(ref, "\\.", fixed = FALSE)[[1]]
      ds  <- pieces[1]
      var <- pieces[2]
      status  <- "PASS"
      message <- "Variable found in ADaM spec"
      if (!ref %in% names(spec_lookup)) {
        same_ds <- any(startsWith(names(spec_lookup), paste0(ds, ".")))
        if (same_ds) {
          status  <- "WARN"
          message <- sprintf(
            "Variable %s not in spec but dataset %s exists -- check for typo or pending derivation",
            var, ds
          )
        } else {
          status  <- "FAIL"
          message <- sprintf("Dataset %s not found in ADaM spec", ds)
        }
      } else {
        ## The variable exists -- now check whether the LITERAL bound to it
        ## could ever be right. `ref %in% names(spec_lookup)` alone would
        ## have called `ADAE.AESER='Severe'` clean, because AESER is a real
        ## ADAE variable; it just cannot hold a 6-character value at
        ## declared length 1.
        val <- .extract_annotation_value(annotation, ref)
        finding <- .check_annotation_value(val, spec_lookup[[ref]], spec_codelists)
        if (!is.null(finding)) {
          status  <- finding$status
          message <- finding$message
        }
      }
      out_rows[[length(out_rows) + 1L]] <<- data.frame(
        tlf_number = tlf_number, stub_label = stub_label,
        annotation = annotation, variable_ref = ref,
        status = status, message = message,
        stringsAsFactors = FALSE
      )
    }
  }

  for (sec in tlf_sections) {
    ## Population annotation row.
    if (nzchar(sec$population_annot %||% "")) {
      add_rows(sec$tlf_number, "<population>", sec$population_annot)
    }
    ## Stub-row annotations.
    for (row in sec$stub_rows) {
      if (isTRUE(row$has_annot)) {
        add_rows(sec$tlf_number, row$label, row$annotation)
      }
    }
  }

  if (length(out_rows) == 0) {
    return(data.frame(
      tlf_number = character(), stub_label = character(),
      annotation = character(), variable_ref = character(),
      status = character(), message = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, out_rows)
}
