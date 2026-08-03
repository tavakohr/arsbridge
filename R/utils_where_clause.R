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
## Unquoted numeric equality: "ADSL.COHORTN=1" (the usual column-header
## annotation form). The quoted form must win when both could apply, so
## .one_condition() tries this only after .RE_CONDITION_EQ.
.RE_CONDITION_EQ_NUM <- paste0(
  "(", .ADAM_DS, ")\\.(", .ADAM_VAR, ")",
  "\\s*=\\s*([-+]?\\d+(?:\\.\\d+)?)\\b"
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
## Call-form missing checks as annotated shells actually write them:
## R's "is.na(ADSL.COHORTN)" and SAS's "missing(COHORTN)", plus the negations
## "!is.na(...)" / "not missing(...)". The negated form embeds the positive
## one, so .one_condition() must test .RE_ISNA_NEG first.
.RE_ISNA_NEG <- paste0(
  "(?:!|\\bnot\\b)\\s*(?:is\\.na|missing)\\s*\\(\\s*",
  "(", .ADAM_DS, ")\\.(", .ADAM_VAR, ")\\s*\\)"
)
.RE_ISNA_POS <- paste0(
  "(?:is\\.na|missing)\\s*\\(\\s*",
  "(", .ADAM_DS, ")\\.(", .ADAM_VAR, ")\\s*\\)"
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
#'   "ADSL.COHORTN=1"                        (unquoted numeric equality)
#'   "ADSL.RACE IN ('WHITE','ASIAN')"        (multi-value list)
#'   "ADSL.AGE between 18 and 65"            (-> GE/LE compound)
#'   "ADAE.AETERM contains 'rash'"           (CONTAINS extension)
#'   "ADSL.DCSREAS not missing" / "ADSL.DTHDT is null"
#'   "is.na(ADSL.COHORTN)" / "missing(ADSL.COHORTN)"  (call-form missing)
#'   "!is.na(ADSL.COHORTN)" / "not missing(ADSL.COHORTN)"  (call-form present)
#'
#' @noRd
parse_where_clause <- function(expr) {
  expr <- trimws(expr %||% "")
  if (!nzchar(expr)) return(NULL)

  ## Normalise the R/Python double-equals equality operator to the single "="
  ## the grammar below expects (shells and supplements write both
  ## "ADSL.COHORTN=99" and "ADSL.COHORTN==99"). "!=", ">=" and "<=" never
  ## contain the "==" substring, so they are left untouched. Comparison values
  ## in clinical filters do not contain "==", so this is safe on the whole
  ## expression.
  expr <- gsub("==", "=", expr, fixed = TRUE)

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
  ## "Safety Population" is not a condition attempt), AND skip a BARE
  ## DATASET.VARIABLE reference with no operator (e.g. a stub's analysis-
  ## variable annotation "ADSL.AGEGR1") -- that is a variable pointer, not a
  ## filter, so "no condition" is correct, not a parse failure.
  ## A part is an ATTEMPTED condition (worth warning) only when something
  ## remains after its DATASET.VARIABLE token -- an operator, value, or stray
  ## comparator like "like". A token alone is a bare variable pointer.
  is_attempt <- function(s) {
    rest <- sub(paste0(.ADAM_DS, "\\.", .ADAM_VAR), "", s, perl = TRUE)
    nzchar(trimws(rest))
  }
  ## A clause that is a DIRECTIVE, not a condition. "once/subject ADAE.AOCCIFL"
  ## names a variable and carries text around it, so it looks like an attempted
  ## filter -- but it has its own consumer (.once_per_subject_var(), which
  ## routes the row to the distinct-subject method) and nothing was dropped.
  ## Warning about it told the author to fix an annotation the package
  ## understood perfectly.
  is_directive <- function(s) !is.null(.once_per_subject_var(s))
  for (u in unparsed) {
    if (is_directive(u)) next
    if (grepl(paste0(.ADAM_DS, "\\.", .ADAM_VAR), u, perl = TRUE) && is_attempt(u)) {
      diag_add(
        stage = "where_clause", severity = "WARN",
        problem = "Condition could not be parsed into an ARS WhereClause",
        location = u,
        action = "Condition dropped -- filtering will be weaker than the annotation intends (supported: =, EQ/NE/IN/NOTIN/GT/GE/LT/LE incl. unquoted numerics, IN ('a','b') lists, BETWEEN x AND y, CONTAINS 'text', is/not null/missing, is.na()/missing() incl. negation)"
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
  ## Unquoted numeric equality: DATASET.VARIABLE=1
  m <- regmatches(part, regexec(.RE_CONDITION_EQ_NUM, part, perl = TRUE))[[1]]
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
  ## Call-form missing checks: "!is.na(...)" / "not missing(...)" BEFORE the
  ## positive "is.na(...)" / "missing(...)" (the negated form embeds it).
  m <- regmatches(part, regexec(.RE_ISNA_NEG, part, ignore.case = TRUE, perl = TRUE))[[1]]
  if (length(m) == 3) {
    return(.cond(m[2], m[3], "NE", NA_character_))
  }
  m <- regmatches(part, regexec(.RE_ISNA_POS, part, ignore.case = TRUE, perl = TRUE))[[1]]
  if (length(m) == 3) {
    return(.cond(m[2], m[3], "EQ", NA_character_))
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

## --- WhereClause -> R predicate source text -------------------------------
##
## where_to_filter_expr() turns a WhereClause into a dplyr/base predicate STRING
## that, evaluated against a dataset, reproduces the logical mask of
## eval_where_clause()/eval_condition() in ars_to_ard.R EXACTLY. The cards
## emitter (R/ars_to_code.R) drops these strings into `dplyr::filter(...)`, so
## the code arsbridge emits filters identically to how arsbridge executes --
## the deterministic-equivalence guarantee of Plan B. Keep the two in lock-step:
## any change to eval_condition() must be mirrored here (see test-where_to_filter_expr).

#' Datasets referenced anywhere in a WhereClause (mirrors the
#' get_referenced_datasets() closure in ars_to_ard.R). Used by the emitter to
#' decide direct-filter vs cross-dataset subject restriction.
#' @noRd
.where_datasets <- function(where) {
  if (is.null(where)) return(character(0))
  if (!is.null(where[["condition"]])) {
    return(.as_scalar_char(where[["condition"]][["dataset"]]) %||% character(0))
  }
  if (!is.null(where[["compoundExpression"]])) {
    cls <- where[["compoundExpression"]][["whereClauses"]]
    return(unique(unlist(lapply(cls, .where_datasets))))
  }
  if (!is.null(where[["dataset"]])) {
    return(.as_scalar_char(where[["dataset"]]) %||% character(0))
  }
  character(0)
}

#' Render a character vector as an escaped R `c("a", "b")` literal.
#' @noRd
.r_chr_vec <- function(vals) {
  paste0("c(", paste(encodeString(as.character(vals), quote = "\""),
                     collapse = ", "), ")")
}

#' One WhereClauseCondition -> predicate string (mirrors eval_condition()).
#' @noRd
.condition_to_expr <- function(cond) {
  var  <- .as_scalar_char(cond[["variable"]])
  comp <- .as_scalar_char(cond[["comparator"]]) %||% "EQ"
  vals <- unlist(cond[["value"]])
  vals <- vals[!is.na(vals)]

  if (is.null(var) || !nzchar(var)) return("TRUE")

  if (comp %in% c("EQ", "IN")) {
    if (length(vals) == 0) sprintf("(is.na(%s) | %s == \"\")", var, var)
    else sprintf("%s %%in%% %s", var, .r_chr_vec(vals))
  } else if (comp %in% c("NE", "NOTIN")) {
    if (length(vals) == 0) sprintf("(!is.na(%s) & %s != \"\")", var, var)
    else sprintf("!(%s %%in%% %s)", var, .r_chr_vec(vals))
  } else if (comp %in% c("LT", "LE", "GT", "GE")) {
    op <- c(LT = "<", LE = "<=", GT = ">", GE = ">=")[[comp]]
    sprintf("%s %s as.numeric(%s)", var, op,
            encodeString(as.character(vals[1]), quote = "\""))
  } else if (comp == "CONTAINS") {
    if (length(vals) == 0) return("FALSE")
    atoms <- vapply(vals, function(v) sprintf(
      "grepl(tolower(%s), tolower(as.character(%s)), fixed = TRUE)",
      encodeString(v, quote = "\""), var), character(1))
    if (length(atoms) == 1) atoms
    else paste0("(", paste(atoms, collapse = " | "), ")")
  } else {
    "TRUE"
  }
}

#' Convert an ARS WhereClause into a predicate string.
#'
#' Mirrors `eval_where_clause()` in ars_to_ard.R: `NULL` -> "TRUE" (no filter);
#' a `condition` -> the comparator predicate; a `compoundExpression` -> the
#' parenthesised atoms joined by ` & ` (AND) or ` | ` (OR); an unrecognised
#' operator -> "TRUE".
#'
#' @param where A WhereClause-bearing object (analysisSet / dataSubset /
#'   WhereClause), or `NULL`.
#' @return A single character string suitable for `eval(parse(text = .))` or
#'   `dplyr::filter()`.
#' @noRd
where_to_filter_expr <- function(where) {
  if (is.null(where)) return("TRUE")
  if (!is.null(where[["condition"]])) {
    return(.condition_to_expr(where[["condition"]]))
  }
  if (!is.null(where[["compoundExpression"]])) {
    ce      <- where[["compoundExpression"]]
    op      <- .as_scalar_char(ce[["logicalOperator"]])
    clauses <- ce[["whereClauses"]]
    if (length(clauses) == 0) return("TRUE")
    sep <- if (identical(op, "AND")) " & " else if (identical(op, "OR")) " | " else NULL
    if (is.null(sep)) return("TRUE")
    atoms <- vapply(clauses, function(cl) sprintf("(%s)", where_to_filter_expr(cl)),
                    character(1))
    return(paste(atoms, collapse = sep))
  }
  if (!is.null(where[["variable"]])) {
    return(.condition_to_expr(where))
  }
  "TRUE"
}

## --- WhereClause algebra ---------------------------------------------------
##
## Small structural helpers for composing and comparing WhereClause objects.
## They exist so hierarchical column trees can build a leaf column's condition
## as AND(parent condition, own condition), assert that a subtotal's condition
## equals the parent condition, and let validation detect duplicate result
## paths. They operate on the same shapes parse_where_clause() produces:
## NULL, a `condition` object, or a `compoundExpression`.

#' Combine WhereClause objects into one clause.
#'
#' NULL inputs are dropped (NULL means "no condition", the AND identity).
#' Nested compoundExpressions that use the same operator are flattened, so
#' AND(AND(a, b), c) becomes AND(a, b, c). Returns NULL when nothing remains,
#' the single clause unchanged when only one remains, or a compoundExpression.
#'
#' @noRd
combine_conditions <- function(..., operator = "AND") {
  stopifnot(operator %in% c("AND", "OR"))
  clauses <- Filter(Negate(is.null), list(...))
  if (length(clauses) == 0) return(NULL)

  flattened <- list()
  for (clause in clauses) {
    ce <- clause[["compoundExpression"]]
    same_op <- !is.null(ce) && identical(.as_scalar_char(ce[["logicalOperator"]]), operator)
    if (same_op) {
      flattened <- c(flattened, ce[["whereClauses"]])
    } else {
      flattened <- c(flattened, list(clause))
    }
  }

  if (length(flattened) == 1) return(flattened[[1]])
  list(
    compoundExpression = list(
      logicalOperator = operator,
      whereClauses    = flattened
    )
  )
}

#' Canonical form of a WhereClause, for order-insensitive comparison.
#'
#' A single condition becomes a condition with uppercased dataset, variable,
#' and comparator, and its values as a sorted character vector. A
#' compoundExpression gets each member canonicalized, same-operator nesting
#' flattened, and the members sorted by their serialized form. Two clauses
#' that state the same thing in a different order therefore canonicalize to
#' identical objects.
#'
#' @noRd
canonicalize_condition <- function(x) {
  if (is.null(x)) return(NULL)

  if (!is.null(x[["condition"]])) {
    cond <- x[["condition"]]
    vals <- as.character(unlist(cond[["value"]]))
    return(list(
      condition = list(
        dataset    = toupper(.as_scalar_char(cond[["dataset"]]) %||% ""),
        variable   = toupper(.as_scalar_char(cond[["variable"]]) %||% ""),
        comparator = toupper(.as_scalar_char(cond[["comparator"]]) %||% "EQ"),
        value      = as.list(sort(vals))
      )
    ))
  }

  if (!is.null(x[["compoundExpression"]])) {
    ce <- x[["compoundExpression"]]
    op <- toupper(.as_scalar_char(ce[["logicalOperator"]]) %||% "AND")
    members <- lapply(ce[["whereClauses"]], canonicalize_condition)
    members <- Filter(Negate(is.null), members)

    ## Flatten same-operator nesting after canonicalizing the members.
    flat <- list()
    for (m in members) {
      mce <- m[["compoundExpression"]]
      if (!is.null(mce) && identical(mce[["logicalOperator"]], op)) {
        flat <- c(flat, mce[["whereClauses"]])
      } else {
        flat <- c(flat, list(m))
      }
    }

    if (length(flat) == 0) return(NULL)
    if (length(flat) == 1) return(flat[[1]])

    keys <- vapply(flat, function(m) {
      paste(deparse(m), collapse = "")
    }, character(1))
    flat <- flat[order(keys)]

    return(list(
      compoundExpression = list(
        logicalOperator = op,
        whereClauses    = flat
      )
    ))
  }

  ## A bare condition body (dataset/variable at the top level, as some ARS
  ## nodes carry) is wrapped so both spellings canonicalize the same way.
  if (!is.null(x[["variable"]])) {
    return(canonicalize_condition(list(condition = x)))
  }

  NULL
}

#' Are two WhereClauses structurally equivalent?
#' @noRd
conditions_equal <- function(a, b) {
  identical(canonicalize_condition(a), canonicalize_condition(b))
}

#' The canonical atomic conditions of a pure-AND clause.
#'
#' Returns a list of canonical single-condition objects when `x` is NULL (an
#' empty set), a single condition, or an all-AND compound. Returns NA when the
#' clause contains an OR anywhere, because it then has no simple atom-set
#' reading.
#'
#' @noRd
.and_atoms <- function(x) {
  canon <- canonicalize_condition(x)
  if (is.null(canon)) return(list())
  if (!is.null(canon[["condition"]])) return(list(canon))

  ce <- canon[["compoundExpression"]]
  if (!identical(ce[["logicalOperator"]], "AND")) return(NA)

  atoms <- list()
  for (m in ce[["whereClauses"]]) {
    inner <- .and_atoms(m)
    if (!is.list(inner)) return(NA)
    atoms <- c(atoms, inner)
  }
  atoms
}

#' Does the child condition imply the parent condition?
#'
#' TRUE when every atomic condition of the parent also appears in the child,
#' so the child's subject set is a subset of the parent's. This is the check
#' that "COHORT = 1 AND SUBGROUP = 2" is a valid child of "COHORT = 1". A NULL
#' parent (no condition) is implied by anything. Clauses containing OR are
#' answered conservatively: TRUE only when the two clauses are structurally
#' equal, FALSE otherwise.
#'
#' @noRd
condition_implies <- function(child, parent) {
  if (is.null(parent)) return(TRUE)
  if (conditions_equal(child, parent)) return(TRUE)

  child_atoms  <- .and_atoms(child)
  parent_atoms <- .and_atoms(parent)
  if (!is.list(child_atoms) || !is.list(parent_atoms)) return(FALSE)

  child_keys  <- vapply(child_atoms,  function(a) paste(deparse(a), collapse = ""), character(1))
  parent_keys <- vapply(parent_atoms, function(a) paste(deparse(a), collapse = ""), character(1))
  all(parent_keys %in% child_keys)
}

#' The WhereClause of an ARS Group node, in the wrapped shape the executor
#' and emitter consume.
#'
#' The official ARS v1.0 Group IS-A WhereClause: it carries `level`, `order`,
#' and either a bare `condition` (a WhereClauseCondition: dataset, variable,
#' comparator, value) or a `compoundExpression` directly. arsbridge's
#' internal shape wraps conditions one level deeper. This accessor reads
#' both and always returns the wrapped shape (or NULL when the group has no
#' condition at all -- a data-driven level).
#' @noRd
.group_where <- function(g) {
  cond <- g[["condition"]]
  if (!is.null(cond)) {
    ## Already-wrapped legacy shape: condition/compoundExpression inside.
    if (!is.null(cond[["condition"]]) || !is.null(cond[["compoundExpression"]])) {
      return(cond)
    }
    return(list(condition = cond))
  }
  if (!is.null(g[["compoundExpression"]])) {
    return(list(compoundExpression = g[["compoundExpression"]]))
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
