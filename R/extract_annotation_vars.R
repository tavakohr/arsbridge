## arsbridge -- extract_annotation_vars.R
## ---------------------------------------------------------------------------
## Parses an annotation string and returns every "DATASET.VARIABLE" reference
## it contains, including dataset.variable references implied by count
## expressions ("unique USUBJID in ADCM" implies "ADCM.USUBJID").

#' Extract all ADaM variable references from an annotation string
#'
#' @param annotation Character scalar -- the annotation text from a stub
#'   cell (e.g. "ADSL.AGE", "ADSL.SCRFFL='Y'",
#'   "unique USUBJID in ADCM where ADCM.CONTRTFL='Y'").
#'
#' @return Character vector of unique "DATASET.VARIABLE" references in the
#'   order encountered. Empty vector if no references found.
#'
#' @keywords internal
#' @noRd
extract_annotation_vars <- function(annotation) {
  if (is.null(annotation) || !nzchar(trimws(annotation))) return(character())

  txt <- as.character(annotation)
  refs <- character()

  ## Direct DATASET.VARIABLE occurrences.
  m <- regmatches(
    txt,
    gregexpr(paste0("\\b", .ADAM_DS, "\\.", .ADAM_VAR, "\\b"),
             txt, perl = TRUE)
  )[[1]]
  if (length(m) > 0) refs <- c(refs, m)

  ## "unique USUBJID in DATASET" -> add DATASET.USUBJID
  count_m <- regmatches(
    txt,
    gregexpr(paste0("(?i)unique\\s+USUBJID\\s+in\\s+(", .ADAM_DS, ")"),
             txt, perl = TRUE)
  )[[1]]
  if (length(count_m) > 0) {
    ds <- sub("(?i).*\\bin\\s+", "", count_m, perl = TRUE)
    refs <- c(refs, paste0(toupper(ds), ".USUBJID"))
  }

  ## "DATASET.VARIABLE where OTHERVAR='X'"  -> also add DATASET.OTHERVAR
  ## Only fires when a bare variable (no dataset prefix) appears after "where"
  ## and a preceding DATASET.VARIABLE has set the dataset context.
  where_m <- regmatches(
    txt,
    gregexpr(
      paste0("\\b(", .ADAM_DS, ")\\.", .ADAM_VAR,
             "\\s+(?i:where)\\s+(", .ADAM_VAR, ")\\s*="),
      txt, perl = TRUE
    )
  )[[1]]
  for (chunk in where_m) {
    pieces <- regmatches(chunk, regexec(
      paste0("\\b(", .ADAM_DS, ")\\.", .ADAM_VAR,
             "\\s+(?i:where)\\s+(", .ADAM_VAR, ")\\s*="),
      chunk, perl = TRUE
    ))[[1]]
    if (length(pieces) == 3) {
      refs <- c(refs, paste0(pieces[2], ".", pieces[3]))
    }
  }

  unique(refs)
}
