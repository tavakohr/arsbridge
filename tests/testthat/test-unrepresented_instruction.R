## An authored instruction the emitted restriction cannot carry out.
##
## THE DEFECT CLASS. An annotation can say two things at once: a filter, and a
## rule about how to treat records the filter does not select --
##
##     <DS>.FLAG='Y' and <DS>.VAL=1 (a subject with no <visit> record is a
##     non-responder)
##
## The bracket is masked as an aside, correctly as far as the FILTER goes,
## since it states no condition. The filter is then read, and it is right about
## which records qualify and silent about the rule. Computing it alone produces
## a plausible number under a DIFFERENT definition than the one asked for --
## here a responder rate over a smaller denominator -- and nothing on the
## analysis says so.
##
## Why it becomes urgent rather than staying latent: while a compound could not
## be carried, rows of this shape reserved anyway, for an unrelated reason. The
## moment the carrier works, the same incomplete interpretation starts
## executing. A known-incomplete reading must not become executable merely
## because transport improved.
##
## THE INVARIANT: a filter says which records SURVIVE, so an aside that speaks
## about the records it EXCLUDES is stating something no filter can implement
## -- those records are not there to be acted on.
##
## Three clauses establish that, and clause one is a CONSTRUCTION rather than a
## stray negative, because a negative attaches to whatever follows it: "(records
## are not shown separately)" negates the showing. Only `with no`, `without`,
## `missing` and their kin establish that an OBSERVATION is absent (B2d).
##
## What must go on computing: a unit, a planned N, a protocol reference, a
## variable that supplies a label. Those describe the row; they do not instruct
## a computation. And literals are masked first, so a value containing a
## negating word -- or a bracketed range inside quotes -- is a value.
##
## Identifiers here are invented and belong to no study.

.ui_v1 <- list(ds = "ADQX", flag = "QXFL", cat = "QXCAT", num = "QXVAL",
               unit = "QXVALU")
.ui_v2 <- list(ds = "ADWW", flag = "WWSTAT", cat = "WWGROUP", num = "WWAMT",
               unit = "WWAMTU")
.ui_vocabs <- list(.ui_v1, .ui_v2)

## RESERVED or not, as the row builder would decide it.
.ui_state <- function(ann) {
  got <- .subset_from_annotation(ann)
  if (.is_unresolved_condition(got)) return("RESERVED")
  if (is.null(got) || length(got) == 0) return("none")
  "subset"
}

# ---- A: an instruction about excluded records reserves ---------------------

test_that("A1: a negating aside reserves a row whose flat filter reads fine", {
  for (v in .ui_vocabs) {
    ann <- sprintf("%s.%s='Y' (a subject with no follow-up record is a failure)",
                   v$ds, v$flag)
    expect_equal(.unrepresented_instruction(ann),
                 "(a subject with no follow-up record is a failure)")
    expect_equal(.ui_state(ann), "RESERVED")

    ## The filter itself is not in doubt -- that is the whole point. It reads,
    ## and reading it is not enough.
    expect_false(.is_unresolved_condition(parse_where_clause(
      sprintf("%s.%s='Y'", v$ds, v$flag))))
  }
})

test_that("A2: it reserves a compound filter too, not just a flat one", {
  for (v in .ui_vocabs) {
    ann <- sprintf(
      "%s.%s='Y' and %s.%s=1 (a subject with no record is counted as a failure)",
      v$ds, v$flag, v$ds, v$num)
    expect_equal(.ui_state(ann), "RESERVED")
  }
})

test_that("A3: it reserves an envelope form as well", {
  ## The wording avoids the word "not" deliberately. `NOT` is a Boolean
  ## OPERATOR, so a bracket containing it is kept as structure and never
  ## becomes an aside -- such a row reserves, but for an unrelated reason, and
  ## a test written that way would pass while asserting nothing about this
  ## rule. Asserting the instruction is what the state came from keeps it
  ## honest.
  for (v in .ui_vocabs) {
    for (envelope in c(sprintf("%s.%s WHERE %s.%s='Y'", v$ds, v$num, v$ds, v$flag),
                       sprintf("%s.%s [where %s.%s='Y']", v$ds, v$num, v$ds, v$flag))) {
      ann <- paste(envelope, "(a subject with no record is a failure)")
      expect_equal(.unrepresented_instruction(ann),
                   "(a subject with no record is a failure)")
      expect_equal(.ui_state(ann), "RESERVED")
    }
  }
})

test_that("A4: a square-bracketed instruction reserves as well", {
  ## Reached by a different masking path -- the residue rule rather than the
  ## note masker -- and pinned here so both stay closed.
  for (v in .ui_vocabs) {
    ann <- sprintf("%s.%s='Y' [subjects with no follow-up count as failures]",
                   v$ds, v$flag)
    expect_equal(.ui_state(ann), "RESERVED")
  }
})

test_that("A5: the reservation names the instruction, not just the filter", {
  ## A reservation nobody can act on is only half an answer: the text that
  ## caused it has to travel with it.
  for (v in .ui_vocabs) {
    ann <- sprintf("%s.%s='Y' (a subject with no record is a failure)",
                   v$ds, v$flag)
    got <- .subset_from_annotation(ann)
    expect_true(.is_unresolved_condition(got))
    expect_match(paste(unlist(got$dropped), collapse = " "), "no record",
                 fixed = TRUE)
  }
})

# ---- B: a description of the row is not an instruction ---------------------

test_that("B1: descriptive asides still compute", {
  for (v in .ui_vocabs) {
    anns <- c(
      sprintf("%s.%s='Y' (per protocol)", v$ds, v$flag),
      sprintf("%s.%s='Y' (N=XX)", v$ds, v$flag),
      sprintf("%s.%s='Y' (WEEKS)", v$ds, v$flag),
      sprintf("%s.%s='Y' (unit %s.%s)", v$ds, v$flag, v$ds, v$unit),
      sprintf("%s.%s='Y' [per protocol]", v$ds, v$flag))
    for (ann in anns) {
      expect_equal(.ui_state(ann), "subset")
    }
  }
})

test_that("B2: a negating word inside a quoted value is part of the value", {
  ## Literals are masked before any aside is examined. Reserving a row because
  ## its category is called 'Not Reported' would refuse an ordinary answer.
  for (v in .ui_vocabs) {
    ## The third and fourth are the ones that make masking load-bearing: a
    ## BRACKETED negating word inside a value looks exactly like an aside to
    ## anything reading the raw string.
    ## The last two are what make masking load-bearing: a bracketed span that
    ## is BOTH negated and about records, sitting inside a quoted value. To
    ## anything reading the raw string it is indistinguishable from a real
    ## instruction; masked first, it is a value and nothing else.
    for (val in c("NOT REPORTED", "No Evidence of Disease", "Adult (18-65)",
                  "Cohort (subjects with no visit are excluded)",
                  "Group (records with no baseline are counted separately)")) {
      ann <- sprintf("%s.%s='%s'", v$ds, v$cat, val)
      expect_equal(.ui_state(ann), "subset")
      expect_equal(.unrepresented_instruction(ann), "")
    }
  }
})

test_that("B2b: a negated aside about the DISPLAY still computes", {
  ## Negation alone is not the rule, and this is the half that keeps it narrow.
  ## Each aside below is asserted to BE negated, so the test cannot quietly
  ## stop exercising the intended path -- if a word left the negation
  ## vocabulary, the first expectation goes red rather than the second passing
  ## for the wrong reason. What they lack is any claim about records, so
  ## reserving them would withhold ordinary rows for an ordinary word.
  for (v in .ui_vocabs) {
    ## The last two also ASSIGN something -- "is applied", "are shown" -- so
    ## the only clause they fail is the unit of observation. Without that
    ## clause they would reserve, which is what makes it load-bearing rather
    ## than decorative.
    for (aside in c("(no units)", "(not reported separately)",
                    "(except per protocol)", "(never re-derived)",
                    "(no imputation)", "(no adjustment is applied)",
                    "(percentages are not shown)")) {
      expect_false(grepl(.RE_ABSENT_OBSERVATION, aside, perl = TRUE))
      ann <- sprintf("%s.%s='Y' %s", v$ds, v$flag, aside)
      expect_equal(.unrepresented_instruction(ann), "")
    }

    ## Negated AND about records, but assigning them nothing -- so there is no
    ## computation to be missing. These are the cases the third clause exists
    ## for; without it they would reserve rows that compute correctly.
    for (aside in c("(except visit 1)", "(no record-level adjustment)")) {
      expect_true(grepl(.RE_OBSERVATION_UNIT, aside, perl = TRUE))
      expect_false(.aside_assigns_state(aside))
      ann <- sprintf("%s.%s='Y' %s", v$ds, v$flag, aside)
      expect_equal(.unrepresented_instruction(ann), "")
      expect_equal(.ui_state(ann), "subset")
    }
  }
})

test_that("B2b2: absence of something that is not an observation still computes", {
  ## Absence AND assignment, but absent of WHAT? Nothing the analysis observes.
  ## The unit clause is the only thing standing between this and a needless
  ## reservation, which is what makes it load-bearing rather than decorative.
  for (v in .ui_vocabs) {
    for (aside in c("(without adjustment the totals are unchanged)",
                    "(missing footnotes are added at QC)")) {
      expect_true(grepl(.RE_ABSENT_OBSERVATION, aside, perl = TRUE))
      expect_true(.aside_assigns_state(aside))
      expect_false(grepl(.RE_OBSERVATION_UNIT, aside, perl = TRUE))

      ann <- sprintf("%s.%s='Y' %s", v$ds, v$flag, aside)
      expect_equal(.unrepresented_instruction(ann), "")
      expect_equal(.ui_state(ann), "subset")
    }
  }
})

test_that("B2c: naming records is not enough -- the aside must assign them something", {
  ## The minimal pair that isolates the third clause. Both are negated and both
  ## name records; only the second says what becomes of them, and only the
  ## second is therefore a computation this version cannot carry out.
  for (v in .ui_vocabs) {
    ## Two quiet cases: one names records without absence, one states absence
    ## without assigning anything. Both lack a computation to be missing.
    for (q in c("(except visit 1)", "(subjects with no visit)")) {
      expect_equal(.unrepresented_instruction(
        sprintf("%s.%s='Y' %s", v$ds, v$flag, q)), "")
    }
    quiet <- sprintf("%s.%s='Y' (except visit 1)", v$ds, v$flag)
    loud  <- sprintf("%s.%s='Y' (records with no visit 1 are counted as failures)",
                     v$ds, v$flag)
    expect_equal(.unrepresented_instruction(quiet), "")
    expect_equal(.unrepresented_instruction(loud),
                 "(records with no visit 1 are counted as failures)")
    expect_equal(.ui_state(quiet), "subset")
    expect_equal(.ui_state(loud), "RESERVED")
  }
})

test_that("B2d: a negative that attaches to the COMPUTATION, not to the data", {
  ## The control that co-occurrence alone cannot pass. Each of these is
  ## negated, names the unit of observation, AND carries an assignment word --
  ## all three of the loose tests -- yet none says a record is ABSENT. The
  ## negative attaches to the showing, the displaying, the adjusting.
  ##
  ## This is why clause one is a construction (`with no`, `without`, `missing`)
  ## rather than a stray negative: only a construction establishes that the
  ## thing not there is the OBSERVATION.
  for (v in .ui_vocabs) {
    for (aside in c("(no record-level adjustment is applied)",
                    "(records are not shown separately)",
                    "(subject counts are not displayed)")) {
      expect_true(grepl(.RE_RESIDUE_NEGATION, aside, perl = TRUE))
      expect_true(grepl(.RE_OBSERVATION_UNIT, aside, perl = TRUE))
      ## ... and yet no absence of an observation is stated:
      expect_false(grepl(.RE_ABSENT_OBSERVATION, aside, perl = TRUE))

      ann <- sprintf("%s.%s='Y' %s", v$ds, v$flag, aside)
      expect_equal(.unrepresented_instruction(ann), "")
    }

    ## Non-vacuity: at least one of them DOES satisfy the assignment clause,
    ## so the absence clause is demonstrably what turns it away. (The other
    ## two are now also caught by the presentation rule in B2f -- belt and
    ## braces, and worth knowing which belt is holding.)
    expect_true(.aside_assigns_state("(no record-level adjustment is applied)"))
    expect_false(.aside_assigns_state("(records are not shown separately)"))
  }
})

test_that("B2d-row: and the row computes, where nothing else stands in the way", {
  ## Splitting the row-level claim from the rule-level one above, because two
  ## of those asides contain the word `not` -- which is a Boolean OPERATOR
  ## here, so the masker keeps the bracket as structure and the annotation
  ## reserves as unreadable. That is pre-existing behaviour on `main`, nothing
  ## to do with this rule, and asserting "computes" for them would be
  ## asserting something false.
  ##
  ## So the row-level claim is made on asides whose negating word is not an
  ## operator, where a reservation could only have come from this rule.
  for (v in .ui_vocabs) {
    for (aside in c("(no record-level adjustment is applied)",
                    "(no subject-level rounding is applied)",
                    "(excluding visit 1, counts are shown)")) {
      expect_true(grepl(.RE_RESIDUE_NEGATION, aside, perl = TRUE))
      expect_true(grepl(.RE_OBSERVATION_UNIT, aside, perl = TRUE))
      expect_true(.aside_assigns_state(aside))
      expect_false(grepl(.RE_ABSENT_OBSERVATION, aside, perl = TRUE))

      ann <- sprintf("%s.%s='Y' %s", v$ds, v$flag, aside)
      expect_equal(.unrepresented_instruction(ann), "")
      expect_equal(.ui_state(ann), "subset")
    }
  }
})

test_that("B2f: display language is not an assignment, however copular", {
  ## The control a bare copula cannot pass. Each of these states an absence, of
  ## an observation, in a sentence containing `are` -- so every clause except
  ## the assignment one is satisfied -- and each says only WHERE the records
  ## appear. Nothing is computed differently because of them.
  for (v in .ui_vocabs) {
    for (aside in c("(missing visits are displayed separately)",
                    "(missing records are listed separately)",
                    "(missing visits are displayed as separate rows)",
                    "(records with no baseline are shown in a footnote)")) {
      expect_true(grepl(.RE_ABSENT_OBSERVATION, aside, perl = TRUE))
      expect_true(grepl(.RE_OBSERVATION_UNIT, aside, perl = TRUE))
      expect_true(grepl(.RE_COPULA, aside, perl = TRUE))
      ## ... and yet no state is assigned:
      expect_false(.aside_assigns_state(aside))

      ann <- sprintf("%s.%s='Y' %s", v$ds, v$flag, aside)
      expect_equal(.unrepresented_instruction(ann), "")
      expect_equal(.ui_state(ann), "subset")
    }
  }
})

test_that("B2g: `as <state>` assigns, even carried by a presentation verb", {
  ## The other side of B2f, and the reason the `as` test runs BEFORE the
  ## presentation test. "reported as non-responders" values the records; the
  ## verb that carries it is beside the point.
  for (v in .ui_vocabs) {
    ann <- sprintf(
      "%s.%s='Y' (subjects with missing data are reported as non-responders)",
      v$ds, v$flag)
    expect_true(.aside_assigns_state(
      "(subjects with missing data are reported as non-responders)"))
    expect_equal(.unrepresented_instruction(ann),
                 "(subjects with missing data are reported as non-responders)")
    expect_equal(.ui_state(ann), "RESERVED")
  }
})

test_that("B2e: absence stated as `missing` reserves, like `with no`", {
  ## The construction is what matters, not the word `no`.
  for (v in .ui_vocabs) {
    ann <- sprintf("%s.%s='Y' (subjects with missing data are non-responders)",
                   v$ds, v$flag)
    expect_equal(.unrepresented_instruction(ann),
                 "(subjects with missing data are non-responders)")
    expect_equal(.ui_state(ann), "RESERVED")
  }
})

test_that("B3: an aside holding a real condition is structure, not an aside", {
  ## The note masker leaves a bracket alone when it could hold an operand, so
  ## this rule never sees one. Pinned because the reverse -- treating a
  ## structural group as an aside -- would silently drop a clause.
  for (v in .ui_vocabs) {
    ann <- sprintf("%s.%s='Y' AND (%s.%s=1 OR %s.%s=2)",
                   v$ds, v$flag, v$ds, v$num, v$ds, v$num)
    expect_equal(.unrepresented_instruction(ann), "")
    ## A compound: readable, and not a flat subset, so it does not compute
    ## through this path today.
    expect_false(.is_unresolved_condition(parse_where_clause(ann)))
  }
})

test_that("B: scope -- the descriptive forms really do compute", {
  ## Guards section B against a change that reserves everything, which would
  ## leave section A green and section B asserting nothing.
  computed <- unlist(lapply(.ui_vocabs, function(v) {
    vapply(c(sprintf("%s.%s='Y' (per protocol)", v$ds, v$flag),
             sprintf("%s.%s='Y' (N=XX)", v$ds, v$flag),
             sprintf("%s.%s='Y' (WEEKS)", v$ds, v$flag),
             sprintf("%s.%s='Y' (unit %s.%s)", v$ds, v$flag, v$ds, v$unit),
             sprintf("%s.%s='NOT REPORTED'", v$ds, v$cat),
             sprintf("%s.%s='Cohort (subjects with no visit are excluded)'",
                     v$ds, v$cat)),
           .ui_state, character(1))
  }), use.names = FALSE)
  expect_equal(sum(computed == "subset"), 12L)
})
