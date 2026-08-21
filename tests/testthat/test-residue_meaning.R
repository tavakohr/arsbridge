## Leftover text that decides what a condition MEANS.
##
## THE DEFECT CLASS. The leaf grammar finds a condition inside an operand and
## then asks one question about whatever text is left over: does it state a
## condition of its own? If not, the leftover was treated as prose and
## discarded. But text can decide what a condition means without stating one,
## and two kinds do:
##
##   * a negating word inverts it. "<DS>.<SUBJ> with no <DS2> record where
##     FLAG='Y'" was read as FLAG='Y' -- the complement of the population the
##     author asked for. A row labelled "did not receive X" selected exactly
##     the subjects who did;
##   * a qualified reference in front of the condition SCOPES it. "<DS>.<SUBJ>
##     with a <DS2> record where A, and with a <DS2> record where B" is a
##     per-subject existential over two records; read as a row-wise AND it
##     asks for one record satisfying both, which is a different set (the
##     counterexample `.where_restriction_plan()` is built around).
##
## Neither reading was executed while a compound could not be carried -- the
## row reserved for an unrelated reason and the misreading stayed latent. It
## stops being latent the moment compounds are carried, which is why this is
## fixed alongside that change rather than after it.
##
## THE INVARIANT. An operand is accepted only when the text around the
## recognised leaf leaves its meaning intact. Negation anywhere, or a
## qualified reference scoping it from the front, means the operand was not
## fully read, and an operand that was not fully read reserves.
##
## POSITION is what separates a scoping prefix from an aside, and a semicolon
## ends the head -- the forms this grammar has always read (`HEAD; <filter>`,
## a trailing note, a descriptive suffix) must go on reading. Those are the
## cases below that assert something is STILL accepted; they are the reason
## the rule is two narrow rules rather than "leftover text reserves".
##
## Identifiers here are invented and belong to no study.

.rm_v1 <- list(ds = "ADQX", subj = "USUBJID", flag = "QXFL", cat = "QXCAT",
               txt = "QXTRT", num = "QXVAL", foreign = "ADZZ",
               fflag = "ZZFL", fcat = "ZZCAT")
.rm_v2 <- list(ds = "ADWW", subj = "USUBJID", flag = "WWSTAT", cat = "WWGROUP",
               txt = "WWNAME", num = "WWAMT", foreign = "ADYY",
               fflag = "YYSTAT", fcat = "YYGROUP")
.rm_vocabs <- list(.rm_v1, .rm_v2)

.rm_shape <- function(w) {
  if (.is_unresolved_condition(w)) return("UNRESOLVED")
  if (is.null(w)) return("ABSENT")
  cond <- w[["condition"]]
  if (is.null(cond) && !is.null(w[["variable"]])) cond <- w
  if (!is.null(cond)) {
    return(sprintf("%s.%s %s %s", cond$dataset, cond$variable, cond$comparator,
                   paste(unlist(cond$value), collapse = "|")))
  }
  ce <- w[["compoundExpression"]]
  if (is.null(ce)) return("OTHER")
  sprintf("%s(%s)", ce$logicalOperator,
          paste(vapply(ce$whereClauses, .rm_shape, character(1)),
                collapse = ", "))
}

.rm_atom <- function(ds, var, cmp, val) {
  sprintf("%s.%s %s %s", ds, var, cmp, val)
}

# ---- A: a negation must never be dropped -----------------------------------

test_that("A1: a negating word alone reserves, with nothing else to catch it", {
  ## Deliberately no qualified reference in front of the condition, so the
  ## scoping rule in section B cannot be what reserves these. The negation is
  ## the only thing standing between this and the COMPLEMENT of the population
  ## the author asked for -- the worst answer available, because it computes
  ## something and looks right.
  for (v in .rm_vocabs) {
    for (word in c("no", "not", "without", "never", "excluding", "except")) {
      ann <- sprintf("with %s %s record where %s.%s='Y'",
                     word, v$foreign, v$foreign, v$fflag)
      expect_equal(.rm_shape(parse_where_clause(ann)), "UNRESOLVED")
    }
  }
})

test_that("A1b: the authored shape -- head, negation, foreign condition -- reserves", {
  for (v in .rm_vocabs) {
    ann <- sprintf("%s.%s with no %s record where %s.%s='Y'",
                   v$ds, v$subj, v$foreign, v$foreign, v$fflag)
    expect_equal(.rm_shape(parse_where_clause(ann)), "UNRESOLVED")
  }
})

test_that("A2: one negated operand reserves the whole expression", {
  ## Not just the negated half. Carrying the readable side alone restricts by
  ## less than was written and says nothing about the difference.
  for (v in .rm_vocabs) {
    ann <- sprintf("%s.%s='Y' AND with no %s record where %s.%s='Y'",
                   v$ds, v$flag, v$foreign, v$foreign, v$fflag)
    expect_equal(.rm_shape(parse_where_clause(ann)), "UNRESOLVED")
  }
})

# ---- B: a scoping prefix is not prose --------------------------------------

test_that("B1: two per-subject existentials do not collapse into a row-wise AND", {
  ## No negation anywhere, so A2's rule cannot catch this one. "a record where
  ## A, and a record where B" may be satisfied by two DIFFERENT records; a
  ## row-wise AND asks for one record satisfying both. Different sets, and the
  ## expression does not say the grammar may pick either.
  for (v in .rm_vocabs) {
    ann <- sprintf(paste0("%s.%s with a %s record where %s.%s='Y'",
                          " AND with a %s record where %s.%s='C1'"),
                   v$ds, v$subj, v$foreign, v$foreign, v$fflag,
                   v$foreign, v$foreign, v$fcat)
    expect_equal(.rm_shape(parse_where_clause(ann)), "UNRESOLVED")
  }
})

test_that("B2: a qualified reference scoping a single condition reserves", {
  for (v in .rm_vocabs) {
    ann <- sprintf("%s.%s vs reference %s.%s='C1'", v$ds, v$txt, v$ds, v$cat)
    expect_equal(.rm_shape(parse_where_clause(ann)), "UNRESOLVED")
  }
})

# ---- C: the forms this grammar has always read still read ------------------

test_that("C1: `HEAD; <filter>` still reads -- a semicolon ends the head", {
  ## The reference before the semicolon is the row's own subject, not a phrase
  ## scoping what follows. Reserving these would withhold results from a form
  ## the package has read since before any of this.
  for (v in .rm_vocabs) {
    ann <- sprintf("%s.%s; %s.%s='Y'", v$ds, v$txt, v$ds, v$flag)
    expect_equal(.rm_shape(parse_where_clause(ann)),
                 .rm_atom(v$ds, v$flag, "EQ", "Y"))
  }
})

test_that("C2: a trailing note naming a variable still reads", {
  ## An aside AFTER a fully-read condition says something about the row --
  ## which variable supplies the label, which flag makes the count
  ## once-per-subject -- and has never unmade the condition.
  for (v in .rm_vocabs) {
    ann <- sprintf("%s.%s='Y'; label %s.%s", v$ds, v$flag, v$ds, v$cat)
    expect_equal(.rm_shape(parse_where_clause(ann)),
                 .rm_atom(v$ds, v$flag, "EQ", "Y"))
  }
})

test_that("C3: a descriptive suffix still reads", {
  for (v in .rm_vocabs) {
    ann <- sprintf("%s.%s GT 0 (per protocol)", v$ds, v$num)
    expect_equal(.rm_shape(parse_where_clause(ann)),
                 .rm_atom(v$ds, v$num, "GT", "0"))
  }
})

# ---- D: the rule reads structure, never the inside of a value --------------

test_that("D1: a negating word inside a quoted value is part of the value", {
  ## Literals are masked before either rule is applied. A category legitimately
  ## called "NOT REPORTED" is a value, and reserving on it would refuse a row
  ## for containing an ordinary word.
  for (v in .rm_vocabs) {
    ann <- sprintf("%s.%s='NOT REPORTED'", v$ds, v$cat)
    expect_equal(.rm_shape(parse_where_clause(ann)),
                 .rm_atom(v$ds, v$cat, "EQ", "NOT REPORTED"))

    joined <- sprintf("%s.%s='Y' AND %s.%s='NOT REPORTED'",
                      v$ds, v$flag, v$ds, v$cat)
    expect_equal(.rm_shape(parse_where_clause(joined)),
                 sprintf("AND(%s, %s)",
                         .rm_atom(v$ds, v$flag, "EQ", "Y"),
                         .rm_atom(v$ds, v$cat, "EQ", "NOT REPORTED")))
  }
})

test_that("D2: a value that SPELLS a qualified reference is still a value", {
  for (v in .rm_vocabs) {
    ann <- sprintf("%s.%s='%s.%s'", v$ds, v$cat, v$foreign, v$fflag)
    expect_equal(.rm_shape(parse_where_clause(ann)),
                 .rm_atom(v$ds, v$cat, "EQ",
                          sprintf("%s.%s", v$foreign, v$fflag)))
  }
})

test_that("D: scope -- the accepted forms above really are read, not skipped", {
  ## Without this, a grammar change that reserved everything would leave
  ## sections A and B green and sections C and D silently unexercised.
  reads <- unlist(lapply(.rm_vocabs, function(v) {
    c(sprintf("%s.%s; %s.%s='Y'", v$ds, v$txt, v$ds, v$flag),
      sprintf("%s.%s='Y'; label %s.%s", v$ds, v$flag, v$ds, v$cat),
      sprintf("%s.%s GT 0 (per protocol)", v$ds, v$num),
      sprintf("%s.%s='NOT REPORTED'", v$ds, v$cat),
      sprintf("%s.%s='%s.%s'", v$ds, v$cat, v$foreign, v$fflag))
  }), use.names = FALSE)
  got <- vapply(reads, function(a) .rm_shape(parse_where_clause(a)),
                character(1))
  expect_equal(sum(got != "UNRESOLVED" & got != "ABSENT"), 10L)
})
