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
## Unquoted numeric comparison: "ADSL.AGE GE 65".
.RE_CONDITION_NUM <- paste0(
  "(", .ADAM_DS, ")\\.(", .ADAM_VAR, ")",
  "\\s+(EQ|NE|GT|GE|LT|LE)\\s+([-+]?\\d+(?:\\.\\d+)?)\\b"
)
## Multi-value list: "ADSL.RACE IN ('WHITE','ASIAN')" / "NOT IN (...)".
.RE_CONDITION_IN_LIST <- paste0(
  "(", .ADAM_DS, ")\\.(", .ADAM_VAR, ")",
  "\\s+(?i:(NOT\\s*IN|NOTIN|IN))\\s*\\(\\s*('[^']*'(?:\\s*,\\s*'[^']*')*)\\s*\\)"
)
## Range: "ADSL.AGE between 18 ~AND~ 65" (the inner "and" is replaced with
## the ~AND~ marker by parse_where_clause BEFORE joiner splitting so the
## range is not torn apart). Values quoted or numeric.
.RE_BETWEEN <- paste0(
  "(", .ADAM_DS, ")\\.(", .ADAM_VAR, ")",
  "\\s+(?i:between)\\s+('[^']*'|[-+]?\\d+(?:\\.\\d+)?)",
  "\\s+~AND~\\s+('[^']*'|[-+]?\\d+(?:\\.\\d+)?)"
)
## Substring: "ADAE.AETERM contains 'rash'". CONTAINS is an arsbridge
## extension -- not in the ARS v1.0 ConditionComparatorEnum.
.RE_CONTAINS <- paste0(
  "(", .ADAM_DS, ")\\.(", .ADAM_VAR, ")",
  "\\s+(?i:contains)\\s+'([^']*)'"
)
.RE_NULL_CHECK <- paste0(
  "(", .ADAM_DS, ")\\.(", .ADAM_VAR, ")",
  "\\s+(?:is\\s+)?not\\s+(?:null|missing)"
)
## Positive null check: "ADSL.DTHDT is null" / "ADSL.DTHDT missing".
.RE_IS_NULL <- paste0(
  "(", .ADAM_DS, ")\\.(", .ADAM_VAR, ")",
  "\\s+(?:is\\s+)?(?:null|missing)\\b"
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
#'   "ADSL.AGE GE 65"                       (unquoted numeric)
#'   "ADSL.RACE IN ('WHITE','ASIAN')"        (multi-value list)
#'   "ADSL.AGE between 18 and 65"            (-> GE/LE compound)
#'   "ADAE.AETERM contains 'rash'"           (CONTAINS extension)
#'   "ADSL.DCSREAS not missing" / "ADSL.DTHDT is null"
#'
#' @noRd
parse_where_clause <- function(expr) {
  expr <- trimws(expr %||% "")
  if (!nzchar(expr)) return(NULL)

  ## Strip leading/trailing "unique USUBJID in DATASET where" boilerplate so
  ## what remains is just the conditional payload.
  expr <- sub("^(?i)\\s*unique\\s+USUBJID\\s+in\\s+[A-Z0-9]+\\s+where\\s+",
              "", expr, perl = TRUE)

  ## Protect BETWEEN's inner "and" with a marker BEFORE joiner splitting
  ## ("AGE between 18 and 65" must not be torn into two clauses).
  expr <- gsub("(?i)(between\\s+(?:'[^']*'|[-+]?\\d+(?:\\.\\d+)?))\\s+and\\s+",
               "\\1 ~AND~ ", expr, perl = TRUE)

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
  unparsed   <- parts[vapply(conditions, is.null, logical(1))]
  conditions <- Filter(Negate(is.null), conditions)

  ## Anything that survived boilerplate-stripping but didn't parse into a
  ## condition is silently weaker filtering downstream -- record it. Skip
  ## parts with no DATASET.VARIABLE shape at all (plain prose like
  ## "Safety Population" is not a condition attempt).
  for (u in unparsed) {
    if (grepl(paste0(.ADAM_DS, "\\.", .ADAM_VAR), u, perl = TRUE)) {
      diag_add(
        stage = "where_clause", severity = "WARN",
        problem = "Condition could not be parsed into an ARS WhereClause",
        location = u,
        action = "Condition dropped -- filtering will be weaker than the annotation intends (supported: =, EQ/NE/IN/NOTIN/GT/GE/LT/LE incl. unquoted numerics, IN ('a','b') lists, BETWEEN x AND y, CONTAINS 'text', is/not null/missing)"
      )
    }
  }

  if (length(conditions) == 0) return(NULL)
  if (length(conditions) == 1) return(conditions[[1]])

  list(
    compoundExpression = list(
      logicalOperator = joiner %||% "AND",
      whereClauses    = conditions
    )
  )
}

#' Parse one atomic clause into an ARS WhereClauseCondition object (or, for
#' BETWEEN, a compoundExpression of GE + LE). Branch order matters: more
#' specific forms before less specific ones.
#' @noRd
.one_condition <- function(part) {
  strip_q <- function(x) sub("^'(.*)'$", "\\1", x)

  ## Range: DATASET.VARIABLE between lo ~AND~ hi -> (GE lo) AND (LE hi).
  ## ARS v1.0 has no BETWEEN comparator, so emit the conformant compound.
  m <- regmatches(part, regexec(.RE_BETWEEN, part, perl = TRUE))[[1]]
  if (length(m) == 5) {
    return(list(
      compoundExpression = list(
        logicalOperator = "AND",
        whereClauses    = list(
          .cond(m[2], m[3], "GE", strip_q(m[4])),
          .cond(m[2], m[3], "LE", strip_q(m[5]))
        )
      )
    ))
  }
  ## Multi-value list: DATASET.VARIABLE IN ('a','b') / NOT IN ('a','b')
  m <- regmatches(part, regexec(.RE_CONDITION_IN_LIST, part, perl = TRUE))[[1]]
  if (length(m) == 5) {
    comp <- if (grepl("NOT", toupper(m[4]))) "NOTIN" else "IN"
    vals <- regmatches(m[5], gregexpr("'[^']*'", m[5]))[[1]]
    vals <- vapply(vals, strip_q, character(1), USE.NAMES = FALSE)
    return(.cond_multi(m[2], m[3], comp, vals))
  }
  ## ARS-style: DATASET.VARIABLE EQ 'value'
  m <- regmatches(part, regexec(.RE_CONDITION_ARS, part, perl = TRUE))[[1]]
  if (length(m) == 5) {
    return(.cond(m[2], m[3], m[4], m[5]))
  }
  ## Unquoted numeric: DATASET.VARIABLE GE 65
  m <- regmatches(part, regexec(.RE_CONDITION_NUM, part, perl = TRUE))[[1]]
  if (length(m) == 5) {
    return(.cond(m[2], m[3], m[4], m[5]))
  }
  ## Equality: DATASET.VARIABLE='value'
  m <- regmatches(part, regexec(.RE_CONDITION_EQ, part, perl = TRUE))[[1]]
  if (length(m) == 4) {
    return(.cond(m[2], m[3], "EQ", m[4]))
  }
  ## Substring: DATASET.VARIABLE contains 'text' (arsbridge extension).
  m <- regmatches(part, regexec(.RE_CONTAINS, part, perl = TRUE))[[1]]
  if (length(m) == 4) {
    diag_add(
      stage = "where_clause", severity = "INFO",
      problem = "CONTAINS comparator emitted (arsbridge extension, not in the ARS v1.0 ConditionComparatorEnum)",
      location = part,
      action = "ars_to_ard() executes it as a case-insensitive substring match; external ARS consumers may reject it"
    )
    return(.cond(m[2], m[3], "CONTAINS", m[4]))
  }
  ## Null checks: "not null/missing" BEFORE the positive form.
  m <- regmatches(part, regexec(.RE_NULL_CHECK, part, ignore.case = TRUE, perl = TRUE))[[1]]
  if (length(m) == 3) {
    return(.cond(m[2], m[3], "NE", NA_character_))
  }
  m <- regmatches(part, regexec(.RE_IS_NULL, part, ignore.case = TRUE, perl = TRUE))[[1]]
  if (length(m) == 3) {
    return(.cond(m[2], m[3], "EQ", NA_character_))
  }
  NULL
}

#' Flatten a single annotation WHERE clause into the
#' `{dataset, variable, comparator, value}` shape that
#' `.build_data_subset()` consumes. Returns NULL when the annotation has no
#' parseable condition, or when it parses to a compound expression (which the
#' single-condition DataSubset builder cannot represent yet).
#' @noRd
flat_data_subset <- function(annotation) {
  wc <- parse_where_clause(annotation)
  if (is.null(wc) || is.null(wc$condition)) return(NULL)
  cond <- wc$condition
  list(
    dataset    = cond$dataset,
    variable   = cond$variable,
    comparator = cond$comparator %||% "EQ",
    value      = cond$value      %||% list()
  )
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

.cond_multi <- function(dataset, variable, comparator, values) {
  list(
    condition = list(
      dataset    = dataset,
      variable   = variable,
      comparator = comparator,
      value      = as.list(values)
    )
  )
}
