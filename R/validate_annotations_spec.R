## arsbridge -- validate_annotations_spec.R
## ---------------------------------------------------------------------------
## For each extracted annotation, look up every DATASET.VARIABLE reference
## against the parsed ADaM spec. Returns a tidy validation report.

#' Cross-reference extracted annotations against the ADaM spec.
#'
#' @param tlf_sections List of TLF sections (output of `parse_shell_docx()`).
#' @param spec_lookup  Named list keyed by `"DATASET.VARIABLE"` (the `lookup`
#'   element of `parse_adam_spec()`).
#'
#' @return Data frame with columns: tlf_number, stub_label, annotation,
#'   variable_ref, status (PASS | WARN | FAIL), message.
#'
#' @keywords internal
#' @noRd
validate_annotations_spec <- function(tlf_sections, spec_lookup) {
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
