## The Boolean structure of an annotation, and what the parsed tree MEANS.
##
## THE DEFECT CLASS. A flat clause splitter -- find one joiner, cut on it --
## does not fail on an expression whose structure it cannot hold. It answers
## with a DIFFERENT expression that is valid, executable and restricts other
## records. `A AND (B OR C)` became `AND(A, B, C)`: a strictly narrower
## restriction, and when the two disjuncts are mutually exclusive, one no
## record satisfies at all. A population written that way emitted an
## unsatisfiable analysis set and every percentage under it divided by the
## wrong N.
##
## THE INVARIANT. Structure is parsed, with the precedence every SQL and SAS
## author already writes to -- brackets, then NOT, then AND, then OR -- and an
## accepted expression consumes the WHOLE token stream. A tree built from part
## of the input is the same class of wrong answer as a flat split: it looks
## finished. Anything left over refuses, and the expression reserves.
##
## HOW THIS FILE PROVES IT, rather than restating it:
##
##   exact shape     the precedence cases are asserted as trees, not as
##                   "parsed without error".
##   truth table     three atoms, all eight combinations, executed. A tree of
##                   the right shape that MEANS something else fails here --
##                   and the table is pinned against the flat reading it
##                   replaces, so re-flattening turns these red.
##   metamorphic     every case runs in two invented vocabularies, and the
##                   trees are asserted structurally identical. The algorithm
##                   keys on relationships, not on familiar names.
##
## Identifiers here are invented and belong to no study.

.bst_v1 <- list(ds = "ADQX", a = "QXAFL", b = "QXBFL", c = "QXCFL",
                txt = "QXTERM", num = "QXVAL", cat = "QXCAT")
.bst_v2 <- list(ds = "ADZZ", a = "ZZONE", b = "ZZTWO", c = "ZZTHREE",
                txt = "ZZNAME", num = "ZZAMT", cat = "ZZGROUP")

.bst_parse <- function(expr) suppressWarnings(parse_where_clause(expr))

.bst_kind <- function(expr) {
  wc <- .bst_parse(expr)
  if (.is_unresolved_condition(wc)) return("unresolved")
  if (is.null(wc)) return("absent")
  "clause"
}

## The tree written out. Identifiers included, so an exact-shape assertion can
## name the operands; `.bst_skeleton()` below drops them for the rename test.
.bst_shape <- function(wc) {
  if (.is_unresolved_condition(wc)) return("UNRESOLVED")
  if (is.null(wc)) return("ABSENT")
  if (!is.null(wc$condition)) {
    cond <- wc$condition
    return(sprintf("%s.%s %s %s", cond$dataset, cond$variable,
                   cond$comparator,
                   paste(unlist(cond$value), collapse = "|")))
  }
  ce <- wc$compoundExpression
  sprintf("%s(%s)", ce$logicalOperator,
          paste(vapply(ce$whereClauses, .bst_shape, character(1)),
                collapse = ", "))
}

## The same tree with every identifier and value replaced by its position, so
## two vocabularies can be compared directly.
.bst_skeleton <- function(wc) {
  if (.is_unresolved_condition(wc)) return("UNRESOLVED")
  if (is.null(wc)) return("ABSENT")
  if (!is.null(wc$condition)) {
    return(sprintf("cond[%s,%d]", wc$condition$comparator,
                   length(wc$condition$value %||% list())))
  }
  ce <- wc$compoundExpression
  sprintf("%s(%s)", ce$logicalOperator,
          paste(vapply(ce$whereClauses, .bst_skeleton, character(1)),
                collapse = ", "))
}

## An atomic condition, put back into the wrapped shape a WhereClause carries.
## `.where_atoms()` hands back the bare condition bodies.
.bst_wrap <- function(cond) list(condition = cond)

## Eight subjects, one per combination of three flags.
.bst_frame <- function(v) {
  grid <- expand.grid(a = c("N", "Y"), b = c("N", "Y"), c = c("N", "Y"),
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  df <- data.frame(USUBJID = sprintf("S%02d", seq_len(nrow(grid))),
                   stringsAsFactors = FALSE)
  df[[v$a]] <- grid$a
  df[[v$b]] <- grid$b
  df[[v$c]] <- grid$c
  df
}

.bst_atoms <- function(v) {
  list(a = sprintf("%s.%s='Y'", v$ds, v$a),
       b = sprintf("%s.%s='Y'", v$ds, v$b),
       c = sprintf("%s.%s='Y'", v$ds, v$c))
}

## A minimal shell section, for the tests that follow the tree all the way
## into a built ARS.
.bst_lookup <- function(v) {
  rec <- function(var, type) list(dataset = v$ds, variable = var, type = type,
                                  codelist = "")
  out <- list()
  out[[paste0(v$ds, ".", v$txt)]] <- rec(v$txt, "Char")
  out[[paste0(v$ds, ".", v$cat)]] <- rec(v$cat, "Char")
  out[[paste0(v$ds, ".", v$a)]]   <- rec(v$a,   "Char")
  out[[paste0(v$ds, ".", v$b)]]   <- rec(v$b,   "Char")
  out[[paste0(v$ds, ".", v$c)]]   <- rec(v$c,   "Char")
  out[[paste0(v$ds, ".", v$num)]] <- rec(v$num, "Num")
  out[["ADSL.SAFFL"]]  <- list(dataset = "ADSL", variable = "SAFFL",
                               type = "Char", codelist = "NY")
  out[["ADSL.TRT01A"]] <- list(dataset = "ADSL", variable = "TRT01A",
                               type = "Char", codelist = "")
  out
}

.bst_section <- function(v, population) {
  list(
    tlf_number = "T-98-1-1", tlf_type = "TABLE", title = "Boolean probe",
    population_text = "Safety Population", population_annot = population,
    source_datasets = v$ds, col_headers = c("", "A", "B"), n_data_cols = 2L,
    stub_rows = list(list(label = "Assessment",
                          annotation = sprintf("%s.%s", v$ds, v$txt),
                          has_annot = TRUE, raw_text = "Assessment",
                          n_slots = 2L)),
    analysis_type = "CATEGORICAL",
    ars_method_name = "Count and Percentage",
    by_variable = "TRT01A", by_variable_dataset = "ADSL",
    enriched_rows = list())
}


## ---------------------------------------------------------------------------
## A. Precedence, asserted as exact trees
## ---------------------------------------------------------------------------

test_that("brackets, AND and OR produce exactly the tree they describe", {
  for (v in list(.bst_v1, .bst_v2)) {
    at <- .bst_atoms(v)
    q <- function(var) sprintf("%s.%s EQ Y", v$ds, var)

    expect_identical(
      .bst_shape(.bst_parse(sprintf("%s AND (%s OR %s)", at$a, at$b, at$c))),
      sprintf("AND(%s, OR(%s, %s))", q(v$a), q(v$b), q(v$c)),
      info = v$ds)

    expect_identical(
      .bst_shape(.bst_parse(sprintf("(%s OR %s) AND %s", at$a, at$b, at$c))),
      sprintf("AND(OR(%s, %s), %s)", q(v$a), q(v$b), q(v$c)),
      info = v$ds)

    ## Without brackets, AND binds tighter than OR.
    expect_identical(
      .bst_shape(.bst_parse(sprintf("%s OR %s AND %s", at$a, at$b, at$c))),
      sprintf("OR(%s, AND(%s, %s))", q(v$a), q(v$b), q(v$c)),
      info = v$ds)

    ## And the mirror, so "AND binds tighter" is not satisfied by an
    ## implementation that simply always nests the tail.
    expect_identical(
      .bst_shape(.bst_parse(sprintf("%s AND %s OR %s", at$a, at$b, at$c))),
      sprintf("OR(AND(%s, %s), %s)", q(v$a), q(v$b), q(v$c)),
      info = v$ds)

    ## One operator repeated is one n-ary node, not a nest -- the shape ARS
    ## writes and the shape the flat splitter produced for the expressions it
    ## could handle, so nothing downstream sees a new arrangement.
    expect_identical(
      .bst_shape(.bst_parse(sprintf("%s AND %s AND %s", at$a, at$b, at$c))),
      sprintf("AND(%s, %s, %s)", q(v$a), q(v$b), q(v$c)),
      info = v$ds)
  }
})


test_that("the tree is the same in either vocabulary", {
  ## The rename test. Every dataset and variable changes; nothing about the
  ## structure may.
  exprs <- function(v) {
    at <- .bst_atoms(v)
    c(sprintf("%s AND (%s OR %s)", at$a, at$b, at$c),
      sprintf("(%s OR %s) AND %s", at$a, at$b, at$c),
      sprintf("%s OR %s AND %s", at$a, at$b, at$c),
      sprintf("((%s OR %s) AND %s) OR %s", at$a, at$b, at$c, at$a),
      sprintf("%s AND %s AND %s", at$a, at$b, at$c))
  }
  one <- vapply(exprs(.bst_v1), function(e) .bst_skeleton(.bst_parse(e)),
                character(1), USE.NAMES = FALSE)
  two <- vapply(exprs(.bst_v2), function(e) .bst_skeleton(.bst_parse(e)),
                character(1), USE.NAMES = FALSE)
  ## Scope assertion: five expressions were actually read as trees, so a
  ## grammar change that stopped recognising them turns this red rather than
  ## leaving two matching lists of "UNRESOLVED".
  expect_equal(length(one), 5L)
  expect_false(any(one %in% c("UNRESOLVED", "ABSENT")))
  expect_identical(one, two)
})


test_that("the population defect that motivated the refusal now parses", {
  ## The acceptance case: a safety flag AND a choice of two cohorts. Read
  ## flat it became AND(SAFFL, COHORT=A, COHORT=B) -- an analysis set no
  ## subject satisfies, emitted with nothing said.
  for (v in list(.bst_v1, .bst_v2)) {
    pop <- sprintf("ADSL.SAFFL='Y' AND (%s.%s='A' OR %s.%s='B')",
                   v$ds, v$cat, v$ds, v$cat)
    wc <- .bst_parse(pop)
    expect_false(.is_unresolved_condition(wc), info = v$ds)
    expect_identical(
      .bst_shape(wc),
      sprintf("AND(ADSL.SAFFL EQ Y, OR(%s.%s EQ A, %s.%s EQ B))",
              v$ds, v$cat, v$ds, v$cat),
      info = v$ds)

    ## The two cohort values are mutually exclusive, so the flat reading is
    ## not merely different -- it is empty. Proved on data rather than
    ## asserted, because "the tree is right" and "the tree means the right
    ## thing" are separate claims.
    df <- data.frame(USUBJID = sprintf("S%02d", 1:4),
                     SAFFL = c("Y", "Y", "N", "Y"),
                     stringsAsFactors = FALSE)
    df[[v$cat]] <- c("A", "B", "A", "C")
    expect_identical(.eval_where_clause(df, wc), c(TRUE, TRUE, FALSE, FALSE),
                     info = v$ds)

    flat <- list(compoundExpression = list(
      logicalOperator = "AND",
      whereClauses = lapply(.where_atoms(wc), .bst_wrap)))
    expect_true(all(!.eval_where_clause(df, flat)), info = v$ds)
  }
})


## ---------------------------------------------------------------------------
## B. Truth tables -- what the tree MEANS
## ---------------------------------------------------------------------------

test_that("every parsed tree agrees with the expression on all 8 rows", {
  for (v in list(.bst_v1, .bst_v2)) {
    at <- .bst_atoms(v)
    df <- .bst_frame(v)
    a <- df[[v$a]] == "Y"
    b <- df[[v$b]] == "Y"
    c_ <- df[[v$c]] == "Y"

    cases <- list(
      list(expr = sprintf("%s AND (%s OR %s)", at$a, at$b, at$c),
           want = a & (b | c_)),
      list(expr = sprintf("(%s OR %s) AND %s", at$a, at$b, at$c),
           want = (a | b) & c_),
      list(expr = sprintf("%s OR %s AND %s", at$a, at$b, at$c),
           want = a | (b & c_)),
      list(expr = sprintf("%s AND %s OR %s", at$a, at$b, at$c),
           want = (a & b) | c_),
      list(expr = sprintf("(%s AND %s) OR %s", at$a, at$b, at$c),
           want = (a & b) | c_),
      list(expr = sprintf("%s AND %s AND %s", at$a, at$b, at$c),
           want = a & b & c_),
      list(expr = sprintf("%s OR %s OR %s", at$a, at$b, at$c),
           want = a | b | c_),
      list(expr = sprintf("(%s OR %s) AND (%s OR %s)", at$a, at$b, at$a, at$c),
           want = (a | b) & (a | c_)),
      list(expr = sprintf("((%s AND %s) OR %s) AND %s", at$a, at$b, at$c, at$a),
           want = ((a & b) | c_) & a)
    )

    ## The flat reading every one of these used to get: all atoms under one
    ## AND. Pinned below, so an implementation that quietly re-flattens fails
    ## here even if the shape assertions above were relaxed.
    flat_want <- a & b & c_

    for (case in cases) {
      wc <- .bst_parse(case$expr)
      expect_false(.is_unresolved_condition(wc), info = case$expr)

      ## What arsbridge EXECUTES.
      expect_identical(.eval_where_clause(df, wc), case$want, info = case$expr)

      ## What arsbridge EMITS -- the dplyr predicate the generated program
      ## carries. The two halves must agree, or the emitted program computes
      ## something other than the ARD.
      emitted <- eval(parse(text = where_to_filter_expr(wc)), envir = df)
      expect_identical(emitted, case$want, info = case$expr)
    }

    ## Scope assertion, and the anti-flattening pin: most of the table must
    ## actually disagree with the flat reading, or it would be satisfied by
    ## the very defect it exists to catch.
    disagrees <- vapply(cases, function(case) !identical(case$want, flat_want),
                        logical(1))
    expect_equal(length(cases), 9L, info = v$ds)
    expect_gte(sum(disagrees), 7L)
  }
})


## ---------------------------------------------------------------------------
## C. The lexer must not read an atom's insides as structure
## ---------------------------------------------------------------------------

test_that("a quoted value carrying operators is not read as structure", {
  ## Values real studies contain. Each one carries something the structural
  ## lexer would read as grammar if literal masking were not in front of it.
  hostile <- c("BLACK OR AFRICAN AMERICAN",
               "NOT DONE",
               "AND/OR",
               "(SEE APPENDIX)",
               "A AND (B OR C)",
               "NOT APPLICABLE (OR UNKNOWN)")
  for (v in list(.bst_v1, .bst_v2)) {
    for (value in hostile) {
      expr <- sprintf("%s.%s='%s' AND %s.%s='Y'",
                      v$ds, v$txt, value, v$ds, v$a)
      wc <- .bst_parse(expr)
      expect_false(.is_unresolved_condition(wc), info = expr)
      atoms <- .where_atoms(wc)
      ## Two atoms, and the first still carries the value BYTE FOR BYTE. A
      ## value torn on an operator inside it would arrive truncated, and a
      ## truncated value filters on records nobody asked for.
      ## Guarded so a lost atom FAILS rather than erroring on the index: an
      ## erroring test proves the mutant broke something, not that this
      ## assertion caught it.
      expect_length(atoms, 2L)
      nth <- function(i) if (length(atoms) >= i) unlist(atoms[[i]]$value) else NA
      expect_identical(nth(1L), value, info = expr)
      expect_identical(nth(2L), "Y", info = expr)
    }
    ## And in double quotes, which annotated shells write just as often.
    expr <- sprintf("%s.%s=\"NOT DONE OR MISSING\"", v$ds, v$txt)
    expect_identical(unlist(.bst_parse(expr)$condition$value),
                     "NOT DONE OR MISSING", info = v$ds)
  }
})


test_that("an atomic form keeps its own brackets and its own NOT", {
  ## Each of these owns a parenthesis or the word NOT that belongs to the
  ## ATOM, not to the expression. Reading either as structure would refuse a
  ## condition this grammar can represent perfectly.
  for (v in list(.bst_v1, .bst_v2)) {
    joined <- function(atom) sprintf("%s AND %s.%s='Y'", atom, v$ds, v$a)
    cases <- list(
      list(atom = sprintf("%s.%s IN ('A','B')", v$ds, v$cat),
           comparator = "IN", n = 2L),
      list(atom = sprintf("%s.%s NOT IN ('A','B')", v$ds, v$cat),
           comparator = "NOTIN", n = 2L),
      list(atom = sprintf("is.na(%s.%s)", v$ds, v$cat),
           comparator = "EQ", n = 0L),
      list(atom = sprintf("not missing(%s.%s)", v$ds, v$cat),
           comparator = "NE", n = 0L),
      list(atom = sprintf("%s.%s not missing", v$ds, v$cat),
           comparator = "NE", n = 0L)
    )
    for (case in cases) {
      wc <- .bst_parse(joined(case$atom))
      expect_false(.is_unresolved_condition(wc), info = case$atom)
      atoms <- .where_atoms(wc)
      ## Guarded for the same reason as above.
      expect_length(atoms, 2L)
      head_atom <- if (length(atoms) >= 1L) atoms[[1]] else list()
      expect_identical(head_atom$comparator %||% NA_character_,
                       case$comparator, info = case$atom)
      expect_equal(length(unlist(head_atom$value)), case$n, info = case$atom)
    }

    ## BETWEEN's inner "and" is part of the range, not a conjunction. Read as
    ## one, the expression becomes three clauses and the range is destroyed.
    wc <- .bst_parse(sprintf("%s.%s between 1 and 5 AND %s.%s='Y'",
                             v$ds, v$num, v$ds, v$a))
    expect_false(.is_unresolved_condition(wc), info = v$ds)
    expect_identical(
      .bst_shape(wc),
      sprintf("AND(AND(%s.%s GE 1, %s.%s LE 5), %s.%s EQ Y)",
              v$ds, v$num, v$ds, v$num, v$ds, v$a),
      info = v$ds)
  }
})


test_that("a bracketed note groups nothing", {
  ## A unit, a planned N, an aside. None encloses an operand, and refusing an
  ## expression over a punctuation habit withholds a correct result.
  for (v in list(.bst_v1, .bst_v2)) {
    kept <- c(
      sprintf("%s.%s GT 0 (per protocol) AND %s.%s='Y'",
              v$ds, v$num, v$ds, v$a),
      sprintf("%s.%s='Y' (N=XX) AND %s.%s='Y'", v$ds, v$a, v$ds, v$b),
      sprintf("(%s.%s='Y') AND (%s.%s='Y')", v$ds, v$a, v$ds, v$b)
    )
    kinds <- vapply(kept, .bst_kind, character(1), USE.NAMES = FALSE)
    expect_equal(length(kinds), 3L, info = v$ds)
    expect_true(all(kinds == "clause"), info = v$ds)
    for (expr in kept) {
      expect_length(.where_atoms(.bst_parse(expr)), 2L)
    }
  }
})


## ---------------------------------------------------------------------------
## D. An accepted expression consumes the whole token stream
## ---------------------------------------------------------------------------

test_that("a malformed expression reserves rather than parsing a prefix", {
  for (v in list(.bst_v1, .bst_v2)) {
    at <- .bst_atoms(v)
    malformed <- c(
      sprintf("%s AND", at$a),                                 # dangling AND
      sprintf("OR %s", at$a),                                  # leading OR
      sprintf("%s AND (%s OR %s", at$a, at$b, at$c),           # unclosed
      sprintf("%s OR %s)", at$a, at$b),                        # unopened
      sprintf("%s NOT %s", at$a, at$b),                        # no joiner
      sprintf("%s AND (%s OR %s) %s", at$a, at$b, at$c, at$a), # trailing text
      sprintf("(%s AND %s))", at$a, at$b),                     # extra close
      sprintf("%s AND OR %s", at$a, at$b)                      # two operators
    )
    kinds <- vapply(malformed, .bst_kind, character(1), USE.NAMES = FALSE)
    ## Scope assertion: eight forms were actually put to the parser.
    expect_equal(length(kinds), 8L, info = v$ds)
    expect_true(all(kinds == "unresolved"), info = v$ds)

    ## Position decides what a bracket is. After a joiner the author has said
    ## the next thing is an OPERAND, so brackets with nothing between them are
    ## a missing term, not an aside -- reducing `A AND ()` to `A` removes a
    ## term the author wrote and leaves the expression looking complete.
    expect_identical(.parse_boolean(.lex_boolean(
      sprintf("%s.%s GT 1 AND ()", v$ds, v$num)))$reason,
      "an empty operand", info = v$ds)
  }
})


test_that("an operand that leaves a condition unread reserves", {
  ## Consuming every STRUCTURAL token is not consuming every CONDITION. The
  ## leaf battery is unanchored and stops at the first form it recognises, so
  ## `A='Y' B='N'` was read as `A='Y'` and the second condition simply
  ## disappeared -- the same silent-loss class one level below the tree.
  for (v in list(.bst_v1, .bst_v2)) {
    a <- sprintf("%s.%s='Y'", v$ds, v$a)
    b <- sprintf("%s.%s='N'", v$ds, v$b)
    c_ <- sprintf("%s.%s='N'", v$ds, v$c)

    lost <- c(
      sprintf("%s %s", a, b),                       # juxtaposed
      sprintf("%s ; %s", a, b),                     # punctuation between
      sprintf("%s unexpected %s", a, b),            # prose between
      sprintf("%s AND %s %s", a, sprintf("%s.%s='Y'", v$ds, v$b), c_),
      sprintf("%s AND (%s %s)", a, sprintf("%s.%s='Y'", v$ds, v$b), c_)
    )
    kinds <- vapply(lost, .bst_kind, character(1), USE.NAMES = FALSE)
    ## Scope assertion: five forms were actually read.
    expect_equal(length(kinds), 5L, info = v$ds)
    expect_true(all(kinds == "unresolved"), info = v$ds)

    ## The over-reservation counterpart, and it is what keeps this from
    ## becoming "reserve anything with text after a condition". A descriptive
    ## suffix follows a condition all the time and states none.
    kept <- c(
      sprintf("%s (per protocol)", a),
      sprintf("%s.%s GT 0 (per protocol)", v$ds, v$num),
      sprintf("%s (N=XX)", a),
      sprintf("%s AND %s", a, b)
    )
    kept_kinds <- vapply(kept, .bst_kind, character(1), USE.NAMES = FALSE)
    expect_equal(length(kept_kinds), 4L, info = v$ds)
    expect_true(all(kept_kinds == "clause"), info = v$ds)

    ## And the value that survived is the one that was written -- "accepted"
    ## must not mean "accepted after quietly reading something else".
    expect_identical(.bst_shape(.bst_parse(sprintf("%s (per protocol)", a))),
                     sprintf("%s.%s EQ Y", v$ds, v$a), info = v$ds)
  }
})


test_that("a bracket in operand position is an operand, not an aside", {
  ## `A='Y' (N=XX)` puts the bracket after a condition, where an aside is
  ## exactly what it is. `A='Y' AND (N=XX)` puts it after a joiner, where the
  ## author has said the next thing is a term of the restriction -- and a term
  ## that quietly evaluates to nothing removes itself from the filter.
  for (v in list(.bst_v1, .bst_v2)) {
    a <- sprintf("%s.%s='Y'", v$ds, v$a)

    reserved <- c(
      sprintf("%s AND ()", a),
      sprintf("%s AND (per protocol)", a),
      sprintf("%s OR (not recorded)", a),
      sprintf("(per protocol) AND %s", a)
    )
    kinds <- vapply(reserved, .bst_kind, character(1), USE.NAMES = FALSE)
    expect_equal(length(kinds), 4L, info = v$ds)
    expect_true(all(kinds == "unresolved"), info = v$ds)

    ## WHY it reserved, not just that it did. An empty operand is diagnosed as
    ## an empty operand: the author is told a term is missing, rather than
    ## being sent to look for a condition inside brackets that hold nothing.
    ## Reading this through the real entry point is what makes the difference
    ## observable -- the lexer never sees the brackets unless the masker has
    ## left them alone.
    diag_reset()
    suppressWarnings(parse_where_clause(sprintf("%s AND ()", a)))
    empty <- ars_diagnostics()
    expect_true(any(grepl("an empty operand", empty$problem, fixed = TRUE)),
                info = v$ds)

    diag_reset()
    suppressWarnings(parse_where_clause(sprintf("%s AND (per protocol)", a)))
    prose <- ars_diagnostics()
    expect_true(any(grepl("states no condition", prose$problem, fixed = TRUE)),
                info = v$ds)
    diag_reset()

    ## Same text, suffix position: still an aside, still accepted.
    expect_identical(.bst_kind(sprintf("%s (per protocol)", a)), "clause",
                     info = v$ds)
    expect_identical(.bst_kind(sprintf("%s (per protocol) AND %s.%s='Y'",
                                       a, v$ds, v$b)), "clause", info = v$ds)

    ## And prose that joins nothing to a condition is still a sentence. This
    ## is what separates "an operand vanished" from "these words are not an
    ## expression at all" -- reserving the latter withholds results that were
    ## never at risk.
    expect_identical(.bst_kind("safety population or better"), "absent",
                     info = v$ds)
    expect_identical(.bst_kind("(not collected) or (not evaluable)"), "absent",
                     info = v$ds)
  }
})


test_that("a refusal names what stopped the parse", {
  ## Read on the parser directly, with unquoted numeric operands so no
  ## masking stands between the text and the token stream. The reason is what
  ## the author is shown, so it has to name the construct rather than
  ## announce a generic failure.
  v <- .bst_v1
  reason <- function(expr) .parse_boolean(.lex_boolean(expr))$reason
  atom <- sprintf("%s.%s GT 1", v$ds, v$num)

  expect_identical(reason(sprintf("%s AND", atom)),
                   "an operator with nothing after it")
  expect_identical(reason(sprintf("OR %s", atom)),
                   "an operator with no condition before it")
  expect_identical(reason(sprintf("%s AND (%s OR %s", atom, atom, atom)),
                   "an unclosed parenthesis")
  expect_identical(reason(sprintf("%s AND )", atom)),
                   "a parenthesis that closes nothing")
  expect_identical(reason(sprintf("%s AND (%s) %s", atom, atom, atom)),
                   "text this grammar cannot place")

  ## A well-formed expression has no reason at all -- so these assertions
  ## cannot be satisfied by a parser that refuses everything.
  expect_null(reason(sprintf("%s AND (%s OR %s)", atom, atom, atom)))
})


test_that("prose is not refused for words that look like operators", {
  ## The over-reservation hazard, pinned. English carries "and", "or" and
  ## "not", and a row whose annotation states no filter must not be withheld.
  v <- .bst_v1
  expect_identical(.bst_kind("not applicable"), "absent")
  expect_identical(.bst_kind("safety population or better"), "absent")
  expect_identical(.bst_kind(sprintf("%s.%s (not collected)", v$ds, v$txt)),
                   "absent")
  expect_identical(.bst_kind(sprintf("%s.%s (unit %s.%s)",
                                     v$ds, v$num, v$ds, v$cat)), "absent")
  ## "!=" is a comparator; its "!" negates no sub-expression, so the parser
  ## must not report a negation the author did not write.
  expect_null(
    .parse_boolean(.lex_boolean(sprintf("%s.%s != 5", v$ds, v$num)))$reason)
})


## ---------------------------------------------------------------------------
## E. Negation is recognised, and refused for a stated reason
## ---------------------------------------------------------------------------

test_that("a NOT over a real condition reserves, at the right precedence", {
  ## ARS has a NOT, but nothing downstream executes one: the evaluator and the
  ## predicate emitter both answer an unrecognised operator with "keep every
  ## row", so an emitted NOT would be a filter that silently does nothing.
  ## Until that changes, a negation reserves -- and the parser still has to
  ## place it correctly, or it would report the wrong construct.
  for (v in list(.bst_v1, .bst_v2)) {
    at <- .bst_atoms(v)
    negated <- c(
      sprintf("NOT %s", at$a),
      sprintf("NOT (%s OR %s)", at$a, at$b),
      sprintf("%s AND NOT %s", at$a, at$b),
      sprintf("NOT %s AND %s", at$a, at$b),
      sprintf("!%s", at$a)
    )
    kinds <- vapply(negated, .bst_kind, character(1), USE.NAMES = FALSE)
    expect_equal(length(kinds), 5L, info = v$ds)
    expect_true(all(kinds == "unresolved"), info = v$ds)

    ## NOT binds tighter than AND: `NOT A AND B` is `(NOT A) AND B`, so the
    ## parse succeeds structurally and the refusal comes from the tree
    ## builder, not from a malformed stream.
    tree <- .parse_boolean(.lex_boolean(
      sprintf("NOT %s.%s GT 1 AND %s.%s GT 2", v$ds, v$num, v$ds, v$num)))
    expect_identical(tree$kind, "and", info = v$ds)
    expect_identical(tree$children[[1]]$kind, "not", info = v$ds)
    expect_identical(tree$children[[2]]$kind, "atom", info = v$ds)
  }
})


test_that("a NOT that governs no condition negates nothing", {
  ## The counterpart, and it is what keeps the negation refusal from becoming
  ## "reserve every row whose annotation contains the word not".
  expect_identical(.bst_kind("not applicable"), "absent")
  expect_identical(.bst_kind("not evaluable or not done"), "absent")
})


## ---------------------------------------------------------------------------
## F. The tree reaches the entities that carry a condition
## ---------------------------------------------------------------------------

test_that("a grouped population becomes a nested analysis-set condition", {
  for (v in list(.bst_v1, .bst_v2)) {
    pop <- sprintf("ADSL.SAFFL='Y' AND (%s.%s='A' OR %s.%s='B')",
                   v$ds, v$cat, v$ds, v$cat)
    re <- suppressWarnings(build_ars_json(list(.bst_section(v, pop)),
                                          spec_lookup = .bst_lookup(v)))
    as_node <- re$analysisSets[[1]]

    ## No reservation, and a real condition -- the nested one, not a flat
    ## three-way AND that no subject satisfies.
    expect_false(nzchar(as_node$unresolvedCondition %||% ""), info = v$ds)
    expect_identical(
      .bst_shape(.group_where(as_node)),
      sprintf("AND(ADSL.SAFFL EQ Y, OR(%s.%s EQ A, %s.%s EQ B))",
              v$ds, v$cat, v$ds, v$cat),
      info = v$ds)

    ## And it validates clean, so a nested tree is not merely emitted but
    ## accepted by the model checks that decide what gets withheld.
    model <- ars_to_model(re)
    findings <- validate_ars_model(model)
    expect_equal(sum(findings$ref == "ANALYSIS_SET_CONDITION_UNRESOLVED"), 0L,
                 info = v$ds)
    expect_length(.reservations_from_findings(model, findings)$by_analysis, 0L)
  }
})


test_that("a grouped column level becomes a nested group condition", {
  v <- .bst_v1
  sec <- list(
    tlf_number = "T-98-2-3", tlf_type = "TABLE", title = "Header structure",
    .pending_column_annotations = list(
      labels = c("Group A (N=XX)", "Group B (N=XX)", "Either (N=XX)"),
      annotations = c(
        sprintf("%s.%s='A'", v$ds, v$cat),
        sprintf("%s.%s='B'", v$ds, v$cat),
        sprintf("%s.%s='A' OR %s.%s='B'", v$ds, v$cat, v$ds, v$cat))))
  out <- suppressWarnings(.resolve_table_column_groups(sec))
  out$by_variable <- v$cat
  out$by_variable_dataset <- v$ds

  gf <- suppressWarnings(.build_grouping(out))
  expect_length(gf$groups, 3L)
  ## Nothing reserved, and the third level really carries the disjunction --
  ## a column silently narrowed to one cohort would report the wrong N.
  marked <- vapply(gf$groups, function(g) nzchar(g$unresolvedCondition %||% ""),
                   logical(1))
  expect_equal(sum(marked), 0L)
  expect_identical(
    .bst_shape(.group_where(gf$groups[[3]])),
    sprintf("OR(%s.%s EQ A, %s.%s EQ B)", v$ds, v$cat, v$ds, v$cat))
})


test_that("a grouped restriction reaches the restriction plan intact", {
  ## The plan is what both halves of the equivalence guarantee consume: the
  ## executor interprets it, the emitter renders it. A nested tree the plan
  ## could not hold would fail here rather than downstream.
  for (v in list(.bst_v1, .bst_v2)) {
    at <- .bst_atoms(v)
    wc <- .bst_parse(sprintf("%s AND (%s OR %s)", at$a, at$b, at$c))
    plan <- .where_restriction_plan(wc, target_ds = v$ds)
    expect_true(plan$ok, info = v$ds)
    ## One dataset throughout, so the whole tree is a single row-wise leaf --
    ## no subject projection, and therefore no question about which record
    ## the clauses had to agree on.
    expect_identical(plan$node$kind, "row", info = v$ds)
    expect_identical(.bst_shape(plan$node$where), .bst_shape(wc), info = v$ds)

    ## Across datasets the OR branch must survive as its own node rather than
    ## being folded into the conjunction.
    mixed <- .bst_parse(sprintf("ADSL.SAFFL='Y' AND (%s OR %s)", at$b, at$c))
    plan2 <- .where_restriction_plan(mixed, target_ds = "ADSL")
    expect_true(plan2$ok, info = v$ds)
    expect_identical(plan2$node$kind, "op", info = v$ds)
    expect_identical(plan2$node$op, "AND", info = v$ds)
    kinds <- vapply(plan2$node$children, function(ch) ch$kind, character(1))
    expect_true("row" %in% kinds && "subject" %in% kinds, info = v$ds)
  }
})
