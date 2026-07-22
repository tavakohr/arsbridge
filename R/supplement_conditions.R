## arsbridge -- supplement_conditions.R
## ---------------------------------------------------------------------------
## Supplement format v3 ships conditions as TYPED objects, not strings. A v3
## where-clause is already the CDISC ARS WhereClause shape:
##
##   {"condition": {"dataset": "ADSL", "variable": "SAFFL",
##                  "comparator": "EQ", "value": ["Y"]}}
##   {"compoundExpression": {"logicalOperator": "AND",
##                           "whereClauses": [ <WhereClause>, ... ]}}
##
## and this is byte-for-byte the internal list shape parse_where_clause()
## returns for a string annotation (R/utils_where_clause.R). So a v3 clause
## needs NO string parsing -- only validation and light normalisation. That is
## the whole point of v3: the ==/OR/smart-quote string-repair fragility that
## bit real studies simply cannot occur when the assistant hands us the parsed
## tree directly.
##
## Everything here operates on that tree. `.supp_where()` validates and
## normalises one clause; the small readers (`.where_refs`, `.where_flat`,
## `.where_to_annotation`) pull what the downstream builders and diagnostics
## need out of it. `.where_to_annotation()` produces a DISPLAY string only --
## it is never re-parsed as a filter; the typed tree stays authoritative.

## Comparators from the ARS v1.0 ConditionComparatorEnum. CONTAINS is an
## arsbridge extension handled alongside them (see .supp_condition).
.SUPP_COMPARATORS <- c("EQ", "NE", "GT", "GE", "LT", "LE", "IN", "NOTIN")

## Comparator flip used when a NOT negates a single condition. A NOT over one
## condition is rewritten to the negated comparator so the engine (which treats
## a bare NOT as a no-op) never silently drops the negation.
.SUPP_NEGATE <- c(
  EQ = "NE", NE = "EQ",
  IN = "NOTIN", NOTIN = "IN",
  GT = "LE", LE = "GT",
  GE = "LT", LT = "GE"
)

## The six method ids the supplement may reference. Kept in step with the
## catalogue in .STANDARD_METHODS (R/build_ars_json.R); hard-coded here so this
## file does not depend on another file's top-level object being sourced first.
.SUPP_METHOD_IDS <- c(
  "MTH_SUMMARY_STATISTICS_CONTINUOUS",
  "MTH_COUNT_AND_PERCENTAGE",
  "MTH_SUBJECT_COUNT",
  "MTH_KAPLAN_MEIER_ESTIMATE",
  "MTH_AE_FREQUENCY_COUNT",
  "MTH_LISTING"
)

## Analysis families the v3 supplement may declare. Richer than the internal
## enum (.SUPPLEMENT_ANALYSIS_TYPES in supplement.R) so the assistant can name
## what it actually sees; .V3_TYPE_MAP folds each down to an engine family.
.SUPPLEMENT_V3_ANALYSIS_TYPES <- c(
  "CONTINUOUS", "CATEGORICAL", "CATEGORICAL_HIERARCHICAL", "MIXED_SUMMARY",
  "SUBJECT_COUNT", "SURVIVAL", "AE_FREQUENCY", "SHIFT_TABLE", "LISTING",
  "FIGURE", "MODEL_BASED", "OTHER"
)

## v3 analysis family -> the internal family the enricher/engine understands.
## MIXED_SUMMARY maps to CONTINUOUS: the engine's per-row spec-verdict
## correction (build_ars_json.R) then routes the categorical rows to counts,
## which is exactly mixed-summary behaviour. SHIFT_TABLE and MODEL_BASED map to
## OTHER, which trips the existing "needs review" path in enrich_with_llm.R.
.V3_TYPE_MAP <- c(
  CONTINUOUS               = "CONTINUOUS",
  CATEGORICAL              = "CATEGORICAL",
  CATEGORICAL_HIERARCHICAL = "CATEGORICAL",
  MIXED_SUMMARY            = "CONTINUOUS",
  SUBJECT_COUNT            = "CATEGORICAL",
  SURVIVAL                 = "SURVIVAL",
  AE_FREQUENCY             = "AE_FREQUENCY",
  SHIFT_TABLE              = "OTHER",
  LISTING                  = "LISTING",
  FIGURE                   = "FIGURE",
  MODEL_BASED              = "OTHER",
  OTHER                    = "OTHER"
)

#' Validate and normalise one typed v3 where-clause.
#'
#' Accepts the ARS `{condition: ...}` / `{compoundExpression: ...}` shape (and
#' tolerates a bare condition object without the wrapper). Uppercases dataset
#' and variable names, checks the comparator, coerces every value to a string,
#' and rewrites a NOT over a single condition to the negated comparator.
#'
#' @param x       The parsed clause (a jsonlite `simplifyVector = FALSE` list).
#' @param context A JSON-path-ish label used in problem messages, e.g.
#'   `"tlfs/14.1.1/analyses[3]/whereClause"`.
#' @return A list `list(where, problems, infos)`. `where` is the internal
#'   WhereClause (identical to parse_where_clause()'s output) or `NULL` when a
#'   `problems` entry made the clause unusable. `problems` are FAIL-worthy;
#'   `infos` note tolerated repairs.
#' @noRd
.supp_where <- function(x, context = "whereClause") {
  problems <- character(0)
  infos    <- character(0)

  ## Tolerate a bare condition object (no {"condition": ...} wrapper): a very
  ## common assistant slip. Wrap it and note the repair.
  if (is.list(x) && is.null(x[["condition"]]) &&
      is.null(x[["compoundExpression"]]) && !is.null(x[["variable"]])) {
    infos <- c(infos, sprintf(
      "%s: bare condition object accepted -- wrap it as {\"condition\": {...}}",
      context))
    x <- list(condition = x)
  }

  if (!is.list(x) ||
      (is.null(x[["condition"]]) && is.null(x[["compoundExpression"]]))) {
    return(list(
      where = NULL,
      problems = sprintf(
        "%s: not a where-clause -- need a 'condition' or 'compoundExpression' key",
        context),
      infos = infos))
  }
  if (!is.null(x[["condition"]]) && !is.null(x[["compoundExpression"]])) {
    return(list(
      where = NULL,
      problems = sprintf(
        "%s: has both 'condition' and 'compoundExpression' -- use exactly one",
        context),
      infos = infos))
  }

  ## Simple condition.
  if (!is.null(x[["condition"]])) {
    res <- .supp_condition(x[["condition"]], context)
    return(list(where = res$where,
                problems = c(problems, res$problems),
                infos = c(infos, res$infos)))
  }

  ## Compound expression.
  ce <- x[["compoundExpression"]]
  op <- toupper(.as_scalar_char(ce[["logicalOperator"]]) %||% "")
  clauses <- ce[["whereClauses"]] %||% list()

  if (op == "NOT") {
    if (length(clauses) != 1) {
      problems <- c(problems, sprintf(
        "%s: NOT must negate exactly one sub-clause (got %d)",
        context, length(clauses)))
      return(list(where = NULL, problems = problems, infos = infos))
    }
    child <- .supp_where(clauses[[1]], sprintf("%s/whereClauses[1]", context))
    problems <- c(problems, child$problems)
    infos    <- c(infos, child$infos)
    if (is.null(child$where)) {
      return(list(where = NULL, problems = problems, infos = infos))
    }
    if (!is.null(child$where[["compoundExpression"]])) {
      problems <- c(problems, sprintf(
        "%s: NOT over a compound expression is not supported -- express it as NE/NOTIN or an OR of negations",
        context))
      return(list(where = NULL, problems = problems, infos = infos))
    }
    cond <- child$where[["condition"]]
    comp <- cond[["comparator"]]
    neg  <- .SUPP_NEGATE[[comp]]
    if (is.null(neg) || is.na(neg)) {
      problems <- c(problems, sprintf(
        "%s: comparator %s cannot be negated with NOT", context, comp))
      return(list(where = NULL, problems = problems, infos = infos))
    }
    cond[["comparator"]] <- neg
    infos <- c(infos, sprintf(
      "%s: NOT rewritten to comparator %s", context, neg))
    return(list(where = list(condition = cond),
                problems = problems, infos = infos))
  }

  if (!op %in% c("AND", "OR")) {
    problems <- c(problems, sprintf(
      "%s: logicalOperator must be AND, OR, or NOT (got '%s')", context, op))
    return(list(where = NULL, problems = problems, infos = infos))
  }
  if (length(clauses) < 2) {
    problems <- c(problems, sprintf(
      "%s: %s needs at least two sub-clauses (got %d)",
      context, op, length(clauses)))
    return(list(where = NULL, problems = problems, infos = infos))
  }

  kids <- lapply(seq_along(clauses), function(i) {
    .supp_where(clauses[[i]], sprintf("%s/whereClauses[%d]", context, i))
  })
  problems <- c(problems, unlist(lapply(kids, `[[`, "problems")))
  infos    <- c(infos, unlist(lapply(kids, `[[`, "infos")))
  wheres   <- lapply(kids, `[[`, "where")
  if (any(vapply(wheres, is.null, logical(1)))) {
    return(list(where = NULL, problems = problems, infos = infos))
  }
  list(
    where = list(compoundExpression = list(
      logicalOperator = op, whereClauses = wheres)),
    problems = problems, infos = infos)
}

#' Validate and normalise one typed condition into the internal shape.
#' @noRd
.supp_condition <- function(cond, context) {
  problems <- character(0)
  infos    <- character(0)

  ds  <- toupper(.as_scalar_char(cond[["dataset"]]) %||% "")
  var <- toupper(.as_scalar_char(cond[["variable"]]) %||% "")
  if (!nzchar(ds) || !nzchar(var)) {
    problems <- c(problems, sprintf(
      "%s: condition needs a non-empty 'dataset' and 'variable'", context))
    return(list(where = NULL, problems = problems, infos = infos))
  }

  comp <- toupper(.as_scalar_char(cond[["comparator"]]) %||% "EQ")
  if (comp == "CONTAINS") {
    infos <- c(infos, sprintf(
      "%s: CONTAINS is an arsbridge extension, not in the ARS v1.0 comparator enum",
      context))
  } else if (!comp %in% .SUPP_COMPARATORS) {
    problems <- c(problems, sprintf(
      "%s: comparator '%s' is not one of %s (or CONTAINS)",
      context, comp, paste(.SUPP_COMPARATORS, collapse = "/")))
    return(list(where = NULL, problems = problems, infos = infos))
  }

  ## Coerce values to a list of character scalars. An empty list is the
  ## missing-value test (EQ + [] -> is.na), matching parse_where_clause().
  raw <- cond[["value"]]
  if (is.null(raw)) {
    value <- list()
  } else if (is.list(raw)) {
    value <- Filter(Negate(is.null), lapply(raw, .as_scalar_char))
  } else {
    ## A JSON scalar came through un-wrapped -- accept and note it.
    infos <- c(infos, sprintf(
      "%s: scalar 'value' wrapped into a one-element array", context))
    value <- as.list(as.character(raw))
  }

  list(
    where = list(condition = list(
      dataset    = ds,
      variable   = var,
      comparator = comp,
      value      = value)),
    problems = problems, infos = infos)
}

#' Every DATASET.VARIABLE referenced anywhere in a where-clause. Feeds the hard
#' ADaM-spec gate (a compound clause may name several variables). Mirrors
#' `.where_datasets()` in utils_where_clause.R but returns "DS.VAR" strings.
#' @noRd
.where_refs <- function(where) {
  if (is.null(where)) return(character(0))
  if (!is.null(where[["condition"]])) {
    cond <- where[["condition"]]
    ds <- .as_scalar_char(cond[["dataset"]])
    v  <- .as_scalar_char(cond[["variable"]])
    if (is.null(ds) || is.null(v)) return(character(0))
    return(paste0(ds, ".", v))
  }
  if (!is.null(where[["compoundExpression"]])) {
    cls <- where[["compoundExpression"]][["whereClauses"]]
    return(unique(unlist(lapply(cls, .where_refs))) %||% character(0))
  }
  if (!is.null(where[["variable"]])) {
    ds <- .as_scalar_char(where[["dataset"]])
    v  <- .as_scalar_char(where[["variable"]])
    if (is.null(ds) || is.null(v)) return(character(0))
    return(paste0(ds, ".", v))
  }
  character(0)
}

#' The single-condition flat `{dataset, variable, comparator, value}` shape
#' that `.build_data_subset()` consumes, or `NULL` for a compound clause (which
#' the caller emits as a `compoundExpression` DataSubset instead). Identical to
#' `flat_data_subset()`'s output, so the builders need no new code path.
#' @noRd
.where_flat <- function(where) {
  if (is.null(where) || is.null(where[["condition"]])) return(NULL)
  cond <- where[["condition"]]
  list(
    dataset    = cond[["dataset"]],
    variable   = cond[["variable"]],
    comparator = cond[["comparator"]] %||% "EQ",
    value      = cond[["value"]] %||% list())
}

#' Canonical DISPLAY string for a where-clause (diagnostics, provenance, the
#' row `annotation` field). NEVER re-parsed as a filter -- the typed clause is
#' authoritative. The grammar it emits is a subset of what parse_where_clause()
#' accepts, so a round-trip is lossless for the canonical arity of each
#' comparator (locked by a property test).
#' @noRd
.where_to_annotation <- function(where) {
  if (is.null(where)) return("")
  if (!is.null(where[["compoundExpression"]])) {
    ce  <- where[["compoundExpression"]]
    op  <- toupper(.as_scalar_char(ce[["logicalOperator"]]) %||% "AND")
    sep <- if (op == "OR") " or " else " and "
    parts <- vapply(ce[["whereClauses"]], .where_to_annotation, character(1))
    parts <- parts[nzchar(parts)]
    return(paste(parts, collapse = sep))
  }
  cond <- where[["condition"]] %||% where
  ds <- .as_scalar_char(cond[["dataset"]])
  v  <- .as_scalar_char(cond[["variable"]])
  if (is.null(ds) || is.null(v)) return("")
  ref  <- paste0(ds, ".", v)
  comp <- toupper(.as_scalar_char(cond[["comparator"]]) %||% "EQ")
  vals <- as.character(unlist(cond[["value"]]))
  vals <- vals[!is.na(vals)]
  q <- function(s) paste0("'", s, "'")

  if (length(vals) == 0) {
    if (comp == "NE") return(paste0("!is.na(", ref, ")"))
    return(paste0("is.na(", ref, ")"))
  }
  if (comp == "IN" || (comp == "EQ" && length(vals) > 1)) {
    return(paste0(ref, " IN (",
                  paste(vapply(vals, q, character(1)), collapse = ","), ")"))
  }
  if (comp == "NOTIN" || (comp == "NE" && length(vals) > 1)) {
    return(paste0(ref, " NOT IN (",
                  paste(vapply(vals, q, character(1)), collapse = ","), ")"))
  }
  if (comp == "EQ")       return(paste0(ref, "=", q(vals[1])))
  if (comp == "CONTAINS") return(paste0(ref, " contains ", q(vals[1])))
  ## NE, GT, GE, LT, LE with a single value.
  paste0(ref, " ", comp, " ", q(vals[1]))
}

#' Catalogue method id -> its display name (reverse of .STANDARD_METHODS).
#' Returns `NULL` for an unknown id so the caller can fall back.
#' @noRd
.method_name_from_id <- function(id) {
  id <- .as_scalar_char(id)
  if (is.null(id) || !nzchar(id)) return(NULL)
  for (nm in names(.STANDARD_METHODS)) {
    if (identical(.STANDARD_METHODS[[nm]][["id"]], id)) {
      return(.STANDARD_METHODS[[nm]][["name"]])
    }
  }
  NULL
}
