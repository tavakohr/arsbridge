## arsbridge -- utils_where_clause.R
## ---------------------------------------------------------------------------
## Converts an annotation expression (e.g. "ADSL.SAFFL='Y'",
## "ADSL.SAFFL='Y' and ADCM.CONTRTFL='Y'") into a CDISC ARS WhereClause /
## WhereClauseCondition / WhereClauseCompoundExpression object.
##
## Regex tokens .ADAM_DS, .ADAM_VAR and the `%||%` operator come from
## R/aaa_constants.R, which sources first.

## Single condition: "ADSL.SAFFL='Y'" (also matches ARS-style "EQ 'Y'")
.RE_CONDITION_EQ <- paste0(
  "(", .ADAM_DS, ")\\.(", .ADAM_VAR, ")",
  "\\s*=\\s*'([^']*)'"
)
.RE_CONDITION_ARS <- paste0(
  "(", .ADAM_DS, ")\\.(", .ADAM_VAR, ")",
  "\\s+(EQ|NE|IN|NOTIN|GT|GE|LT|LE)\\s+'([^']*)'"
)
.RE_NULL_CHECK <- paste0(
  "(", .ADAM_DS, ")\\.(", .ADAM_VAR, ")",
  "\\s+not\\s+(?:null|missing)"
)

#' Build an ARS WhereClause object from an annotation expression.
#'
#' Returns NULL if no parseable condition is found (caller should treat as
#' "no condition / all subjects"). Returns a single Condition object for a
#' simple expression, or a CompoundExpression wrapping multiple conditions
#' joined by AND / OR / NOT.
#'
#' Supported inputs:
#'   "ADSL.SAFFL='Y'"
#'   "ADSL.SAFFL='Y' and ADCM.CONTRTFL='Y'"
#'   "ADSL.SFENRLFL='Y' or ADSL.WTHTYP='Withdrawal Prior to Treatment'"
#'   "ADSL.PARAMCD EQ 'OS'"
#'   "ADSL.DCSREAS not missing"
#'
#' @noRd
parse_where_clause <- function(expr) {
  expr <- trimws(expr %||% "")
  if (!nzchar(expr)) return(NULL)

  ## Strip leading/trailing "unique USUBJID in DATASET where" boilerplate so
  ## what remains is just the conditional payload.
  expr <- sub("^(?i)\\s*unique\\s+USUBJID\\s+in\\s+[A-Z0-9]+\\s+where\\s+",
              "", expr, perl = TRUE)

  ## Detect logical joiner -- "and"/"&"/"AND" produce AND; "or"/"|"/"OR" → OR.
  joiner <- NULL
  if (grepl("\\s+(?i:and|&&|and)\\s+|\\s&\\s", expr, perl = TRUE)) joiner <- "AND"
  if (is.null(joiner) &&
      grepl("\\s+(?i:or|\\|\\|)\\s+|\\s\\|\\s", expr, perl = TRUE))  joiner <- "OR"

  ## Split into atomic clauses on the joiner, if any.
  parts <- if (!is.null(joiner)) {
    strsplit(expr, "\\s+(?i:and|or)\\s+", perl = TRUE)[[1]]
  } else {
    expr
  }
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]

  conditions <- lapply(parts, .one_condition)
  conditions <- Filter(Negate(is.null), conditions)
  if (length(conditions) == 0) return(NULL)
  if (length(conditions) == 1) return(conditions[[1]])

  list(
    compoundExpression = list(
      logicalOperator = joiner %||% "AND",
      whereClauses    = conditions
    )
  )
}

#' Parse one atomic clause into an ARS WhereClauseCondition object.
#' @noRd
.one_condition <- function(part) {
  ## ARS-style: DATASET.VARIABLE EQ 'value'
  m <- regmatches(part, regexec(.RE_CONDITION_ARS, part, perl = TRUE))[[1]]
  if (length(m) == 5) {
    return(.cond(m[2], m[3], m[4], m[5]))
  }
  ## Equality: DATASET.VARIABLE='value'
  m <- regmatches(part, regexec(.RE_CONDITION_EQ, part, perl = TRUE))[[1]]
  if (length(m) == 4) {
    return(.cond(m[2], m[3], "EQ", m[4]))
  }
  ## Null check: DATASET.VARIABLE not null / not missing
  m <- regmatches(part, regexec(.RE_NULL_CHECK, part, ignore.case = TRUE, perl = TRUE))[[1]]
  if (length(m) == 3) {
    return(.cond(m[2], m[3], "NE", NA_character_))
  }
  NULL
}

.cond <- function(dataset, variable, comparator, value) {
  list(
    condition = list(
      dataset    = dataset,
      variable   = variable,
      comparator = comparator,
      value      = if (is.na(value)) list() else list(value)
    )
  )
}
