## The annotation envelope, and what a bare variable inside it is allowed to
## inherit.
##
## THE DEFECT CLASS. A shell names what a row reports and then says which
## records it reports on -- `HEAD (when <conditions>)`, `HEAD WHERE
## <conditions>`. Those are two languages in one string, and arsbridge read
## only one of them: `WHERE` was recognised, `(when ...)` was not, so the head
## became an unplaceable token and a perfectly clear row reserved.
##
## Where the form WAS recognised, the context it supplies was applied as a
## text prefix -- the head dataset pasted onto the front of the tail. That is
## wrong twice over. It qualified only the FIRST clause, because the prefix
## lands once on a string; and it ASSUMED the variable lives on that dataset
## instead of confirming it.
##
## THE INVARIANT. The envelope is split from the head, and a bare name inside
## it inherits the head's dataset only where the ADaM spec confirms that exact
## DATASET.VARIABLE. Inheritance is ALL-OR-NONE: if one bare name cannot be
## confirmed, the whole filter is unresolved rather than honoured in part,
## because a filter honoured in part restricts by less than was written and
## says nothing about it.
##
## The five boundaries this file pins, each of which is a way the rule could
## be got wrong while still looking right:
##
##   1. a name carried by BOTH the head dataset and a foreign one resolves to
##      the head -- the context is the head's, not a search;
##   2. a name carried ONLY by a foreign dataset is unresolved, never borrowed;
##   3. an explicitly qualified foreign clause does not move the context for
##      the clauses after it;
##   4. qualification never touches a quoted literal or comparator syntax;
##   5. one unconfirmable name reserves the whole filter.
##
## Identifiers here are invented and belong to no study.

.ae_v1 <- list(head = "ADQX", cat = "QXCAT", flag = "QXPRESP",
               occur = "QXOCCUR", txt = "QXTRT", num = "QXVAL",
               foreign = "ADZZ", shared = "QXSHARE", only = "ZZONLY")
.ae_v2 <- list(head = "ADWW", cat = "WWGROUP", flag = "WWSTAT",
               occur = "WWDONE", txt = "WWNAME", num = "WWAMT",
               foreign = "ADYY", shared = "WWBOTH", only = "YYONLY")

## The spec these tests validate against: every head variable, the shared name
## on BOTH datasets, and one name the foreign dataset alone carries.
.ae_resolves <- function(v) {
  keys <- toupper(c(
    paste0(v$head, ".", c(v$cat, v$flag, v$occur, v$txt, v$num, v$shared)),
    paste0(v$foreign, ".", c(v$shared, v$only))))
  function(dataset, variable) {
    toupper(paste0(dataset, ".", variable)) %in% keys
  }
}

.ae_shape <- function(w) {
  if (.is_unresolved_condition(w)) return("UNRESOLVED")
  if (is.null(w)) return("ABSENT")
  if (!is.null(w$condition)) {
    return(sprintf("%s.%s %s %s", w$condition$dataset, w$condition$variable,
                   w$condition$comparator,
                   paste(unlist(w$condition$value), collapse = "|")))
  }
  ce <- w$compoundExpression
  sprintf("%s(%s)", ce$logicalOperator,
          paste(vapply(ce$whereClauses, .ae_shape, character(1)),
                collapse = ", "))
}

.ae_condition <- function(ann, v) {
  suppressWarnings(.annotation_condition(ann, .ae_resolves(v)))
}


## ---------------------------------------------------------------------------
## A. The envelope is split from the head
## ---------------------------------------------------------------------------

test_that("every envelope spelling yields the same head and filter", {
  for (v in list(.ae_v1, .ae_v2)) {
    filter <- sprintf("%s='A' AND %s='Y'", v$cat, v$flag)
    ## Both keywords in all three styles -- the advertised contract, pinned in
    ## full rather than sampled.
    spellings <- c(
      sprintf("%s.%s (when %s)", v$head, v$txt, filter),
      sprintf("%s.%s (where %s)", v$head, v$txt, filter),
      sprintf("%s.%s [when %s]", v$head, v$txt, filter),
      sprintf("%s.%s [where %s]", v$head, v$txt, filter),
      sprintf("%s.%s when %s", v$head, v$txt, filter),
      sprintf("%s.%s where %s", v$head, v$txt, filter),
      ## Case is not part of the contract either way round.
      sprintf("%s.%s WHERE %s", v$head, v$txt, filter),
      sprintf("%s.%s (WHEN %s)", v$head, v$txt, filter)
    )
    read <- Filter(Negate(is.null), lapply(spellings, .annotation_envelope))
    ## Scope assertion on what was READ, not on what was offered: counting the
    ## inputs would be satisfied by eight NULLs.
    expect_equal(length(read), 8L, info = v$head)
    expect_true(all(vapply(read, function(s) identical(s$dataset, v$head),
                           logical(1))), info = v$head)
    expect_true(all(vapply(read, function(s) identical(s$variable, v$txt),
                           logical(1))), info = v$head)
    expect_true(all(vapply(read, function(s) identical(s$filter, filter),
                           logical(1))), info = v$head)
  }
})


test_that("text that is not an envelope is not read as one", {
  ## The over-reservation guard. A note, a unit and a bare pointer follow a
  ## head reference constantly and state no filter; reading one as an envelope
  ## would invent a restriction and then reserve the row for failing to
  ## confirm it.
  for (v in list(.ae_v1, .ae_v2)) {
    not_envelopes <- c(
      sprintf("%s.%s", v$head, v$txt),
      sprintf("%s.%s (not collected)", v$head, v$txt),
      sprintf("%s.%s (unit mg)", v$head, v$num),
      sprintf("%s.%s (%s.%s)", v$head, v$num, v$head, v$cat),
      sprintf("count of %s.USUBJID", v$head)
    )
    got <- lapply(not_envelopes, .annotation_envelope)
    expect_equal(length(got), 5L, info = v$head)
    expect_true(all(vapply(got, is.null, logical(1))), info = v$head)
  }
})


test_that("an envelope's every clause is qualified, not just the first", {
  ## The defect that motivated this change. A text prefix lands once, so
  ## `HEAD.VAR WHERE A='1' AND B='2' AND C='3'` qualified A and left B and C
  ## bare -- and the clauses that stayed bare were then dropped or reserved,
  ## with the row filtering on a third of what the author wrote.
  for (v in list(.ae_v1, .ae_v2)) {
    ann <- sprintf("%s.%s (when %s='A' AND %s='Y' AND %s='Y')",
                   v$head, v$txt, v$cat, v$flag, v$occur)
    expect_identical(
      .ae_shape(.ae_condition(ann, v)),
      sprintf("AND(%s.%s EQ A, %s.%s EQ Y, %s.%s EQ Y)",
              v$head, v$cat, v$head, v$flag, v$head, v$occur),
      info = v$head)
  }
})


test_that("a delimiter inside a value does not close the envelope", {
  ## Where the envelope CLOSES is structure, and structure is read on masked
  ## text. A bracket inside a quoted value is part of the value: counted as a
  ## closer it ends the envelope mid-literal, and the filter that survives is
  ## whichever fragment happened to come first -- a restriction the author
  ## never wrote, on a value that no longer exists in the data.
  for (v in list(.ae_v1, .ae_v2)) {
    cases <- list(
      list(ann = sprintf("%s.%s (when %s='A)B' AND %s='Y')",
                         v$head, v$txt, v$cat, v$flag),
           value = "A)B"),
      list(ann = sprintf("%s.%s [where %s='A]B' AND %s='Y']",
                         v$head, v$txt, v$cat, v$flag),
           value = "A]B"),
      ## Openers too -- a value may contain any of them, and each is a
      ## different way to end the scan in the wrong place.
      list(ann = sprintf("%s.%s (when %s='(A)[B]' AND %s='Y')",
                         v$head, v$txt, v$cat, v$flag),
           value = "(A)[B]")
    )
    got <- lapply(cases, function(case) .ae_condition(case$ann, v))
    ## Scope assertion on what was READ: three values carrying delimiters
    ## produced conditions, not reservations.
    expect_equal(sum(!vapply(got, .is_unresolved_condition, logical(1))), 3L,
                 info = v$head)
    for (i in seq_along(cases)) {
      ## Both clauses survive, and the value byte for byte.
      expect_identical(
        .ae_shape(got[[i]]),
        sprintf("AND(%s.%s EQ %s, %s.%s EQ Y)",
                v$head, v$cat, cases[[i]]$value, v$head, v$flag),
        info = cases[[i]]$ann)
    }

    ## The controls, and they are what keep the masking from hiding real
    ## structure: a comparator's own bracketed list, and a nested group, both
    ## still counted when the envelope's closer is found.
    expect_identical(
      .ae_shape(.ae_condition(
        sprintf("%s.%s (when %s IN ('A','B'))", v$head, v$txt, v$cat), v)),
      sprintf("%s.%s IN A|B", v$head, v$cat), info = v$head)
    expect_identical(
      .ae_shape(.ae_condition(
        sprintf("%s.%s (when %s='A' AND (%s='Y' OR %s='Y'))",
                v$head, v$txt, v$cat, v$flag, v$occur), v)),
      sprintf("AND(%s.%s EQ A, OR(%s.%s EQ Y, %s.%s EQ Y))",
              v$head, v$cat, v$head, v$flag, v$head, v$occur),
      info = v$head)

    ## And a bracket that really does close early -- outside any literal --
    ## still means this is not one envelope.
    expect_null(.annotation_envelope(
      sprintf("%s.%s (when %s='A') AND (%s='Y')",
              v$head, v$txt, v$cat, v$flag)), info = v$head)
  }
})


## ---------------------------------------------------------------------------
## B. The five scope boundaries
## ---------------------------------------------------------------------------

test_that("(1) a name on both datasets resolves to the head", {
  ## The context is the head's dataset, not a search for somewhere the name
  ## exists. A name both datasets carry is exactly where a search and a
  ## context give different answers.
  for (v in list(.ae_v1, .ae_v2)) {
    ann <- sprintf("%s.%s (when %s='A')", v$head, v$txt, v$shared)
    expect_identical(.ae_shape(.ae_condition(ann, v)),
                     sprintf("%s.%s EQ A", v$head, v$shared), info = v$head)

    ## And the control that gives the assertion its force: the same name IS on
    ## the foreign dataset, so "resolved to the head" is a choice rather than
    ## the only possibility.
    expect_true(.ae_resolves(v)(v$foreign, v$shared), info = v$head)
  }
})


test_that("(2) a name only another dataset carries is never borrowed", {
  for (v in list(.ae_v1, .ae_v2)) {
    ann <- sprintf("%s.%s (when %s='A')", v$head, v$txt, v$only)
    expect_identical(.ae_shape(.ae_condition(ann, v)), "UNRESOLVED",
                     info = v$head)

    ## The name really does exist -- somewhere else. That is what makes this
    ## a refusal to borrow rather than a lookup failure.
    expect_true(.ae_resolves(v)(v$foreign, v$only), info = v$head)
    expect_false(.ae_resolves(v)(v$head, v$only), info = v$head)
  }
})


test_that("(3) a qualified foreign clause does not move the context", {
  ## `ADXX.FOO='1' AND BAR='2'` must read BAR against the HEAD, not against
  ## ADXX. Letting the last-seen dataset carry forward would silently
  ## re-point a condition at a dataset the author never wrote beside it.
  for (v in list(.ae_v1, .ae_v2)) {
    ann <- sprintf("%s.%s (when %s.%s='A' AND %s='Y')",
                   v$head, v$txt, v$foreign, v$shared, v$flag)
    expect_identical(
      .ae_shape(.ae_condition(ann, v)),
      sprintf("AND(%s.%s EQ A, %s.%s EQ Y)",
              v$foreign, v$shared, v$head, v$flag),
      info = v$head)

    ## The proof that the context did not move: a following bare name that
    ## exists ONLY on the foreign dataset is still unresolved, even though the
    ## clause before it named that dataset explicitly.
    drifted <- sprintf("%s.%s (when %s.%s='A' AND %s='Y')",
                       v$head, v$txt, v$foreign, v$shared, v$only)
    expect_identical(.ae_shape(.ae_condition(drifted, v)), "UNRESOLVED",
                     info = v$head)
  }
})


test_that("(4) qualification touches no literal and no comparator", {
  for (v in list(.ae_v1, .ae_v2)) {
    ## Values that spell an OPERAND -- a variable name with a comparator right
    ## behind it, which is exactly the shape the qualifier hunts for. Read on
    ## raw text these are rewritten inside the quotes, and a filter whose
    ## value has been edited selects records nobody asked for. Asserted byte
    ## for byte, because a value that arrives altered is the whole failure.
    hostile <- c(sprintf("%s=Y", v$flag),
                 sprintf("%s='A' AND %s='B'", v$cat, v$flag),
                 sprintf("is.na(%s)", v$cat),
                 sprintf("%s GE 1", v$num))
    for (value in hostile) {
      ann <- sprintf("%s.%s (when %s=\"%s\")", v$head, v$txt, v$cat, value)
      got <- .ae_condition(ann, v)
      expect_false(.is_unresolved_condition(got), info = value)
      expect_identical(unlist(got$condition$value), value, info = value)
      expect_identical(got$condition$dataset, v$head, info = value)
      expect_identical(got$condition$variable, v$cat, info = value)
    }
    ## Scope assertion: four hostile values were actually read.
    expect_equal(length(hostile), 4L, info = v$head)

    ## Comparator syntax is grammar, not operands: word comparators, a value
    ## list, a range and the presence tests all survive with their own shape.
    keep <- list(
      list(filter = sprintf("%s EQ 'A'", v$cat),
           want = sprintf("%s.%s EQ A", v$head, v$cat)),
      list(filter = sprintf("%s IN ('A','B')", v$cat),
           want = sprintf("%s.%s IN A|B", v$head, v$cat)),
      list(filter = sprintf("%s NOT IN ('A','B')", v$cat),
           want = sprintf("%s.%s NOTIN A|B", v$head, v$cat)),
      list(filter = sprintf("%s GE 1", v$num),
           want = sprintf("%s.%s GE 1", v$head, v$num)),
      list(filter = sprintf("%s between 1 and 5", v$num),
           want = sprintf("AND(%s.%s GE 1, %s.%s LE 5)",
                          v$head, v$num, v$head, v$num)),
      list(filter = sprintf("%s not missing", v$cat),
           want = sprintf("%s.%s NE ", v$head, v$cat)),
      list(filter = sprintf("is.na(%s)", v$cat),
           want = sprintf("%s.%s EQ ", v$head, v$cat))
    )
    for (case in keep) {
      ann <- sprintf("%s.%s (when %s)", v$head, v$txt, case$filter)
      expect_identical(.ae_shape(.ae_condition(ann, v)), case$want,
                       info = paste(v$head, case$filter))
    }
    ## Scope assertion: seven comparator forms were actually exercised.
    expect_equal(length(keep), 7L, info = v$head)
  }
})


test_that("(5) one unconfirmable name reserves the whole filter", {
  ## All-or-none. Honouring the clauses that happen to be provable filters on
  ## less than the author wrote, and produces a number that looks finished.
  for (v in list(.ae_v1, .ae_v2)) {
    ## Two provable clauses and one that is not, in each position -- so this
    ## cannot pass because the unprovable name happened to come first.
    positions <- c(
      sprintf("%s='A' AND %s='Y' AND %s='Y'", v$only, v$cat, v$flag),
      sprintf("%s='A' AND %s='Y' AND %s='Y'", v$cat, v$only, v$flag),
      sprintf("%s='A' AND %s='Y' AND %s='Y'", v$cat, v$flag, v$only)
    )
    for (filter in positions) {
      ann <- sprintf("%s.%s (when %s)", v$head, v$txt, filter)
      expect_identical(.ae_shape(.ae_condition(ann, v)), "UNRESOLVED",
                       info = filter)
    }

    ## The control: with every name provable the same shape computes, so
    ## "unresolved" above is the unconfirmable name and not the form.
    ok <- sprintf("%s.%s (when %s='A' AND %s='Y' AND %s='Y')",
                  v$head, v$txt, v$cat, v$flag, v$occur)
    expect_false(.is_unresolved_condition(.ae_condition(ok, v)), info = v$head)

    ## And the reservation quotes the AUTHOR's text, never the rewritten form
    ## -- a finding the author cannot recognise is a finding they cannot act
    ## on.
    ann <- sprintf("%s.%s (when %s='A' AND %s='Y')", v$head, v$txt,
                   v$cat, v$only)
    expect_identical(.unresolved_condition_text(.ae_condition(ann, v)), ann,
                     info = v$head)
  }
})


test_that("without a spec in reach a bare name is not provable", {
  ## The safe direction, and a real one: two of this package's own callers
  ## reach the annotation reader, and both carry the spec. Anything else
  ## cannot confirm, and "cannot confirm" is the same answer as "not there".
  for (v in list(.ae_v1, .ae_v2)) {
    ann <- sprintf("%s.%s (when %s='A')", v$head, v$txt, v$cat)
    expect_identical(
      .ae_shape(suppressWarnings(.annotation_condition(ann, NULL))),
      "UNRESOLVED", info = v$head)

    ## But an envelope whose clauses all name their own dataset asks nothing
    ## of the spec -- there is no inheritance to confirm -- so it reads
    ## without one.
    qualified <- sprintf("%s.%s (when %s.%s='A')", v$head, v$txt, v$head, v$cat)
    expect_identical(
      .ae_shape(suppressWarnings(.annotation_condition(qualified, NULL))),
      sprintf("%s.%s EQ A", v$head, v$cat), info = v$head)
  }
})


## ---------------------------------------------------------------------------
## C. What the row then does with it
## ---------------------------------------------------------------------------

test_that("a single-clause envelope computes, a compound one reserves", {
  ## PR3 produces the typed condition; carrying a compound into a row
  ## DataSubset is the next change. Both outcomes are asserted so the
  ## boundary is visible rather than assumed -- and so the day the carrier
  ## lands, this test is where it shows.
  for (v in list(.ae_v1, .ae_v2)) {
    one <- sprintf("%s.%s (when %s='A')", v$head, v$txt, v$cat)
    subset <- suppressWarnings(.subset_from_annotation(one, .ae_resolves(v)))
    expect_false(.is_unresolved_condition(subset), info = v$head)
    expect_identical(subset$dataset, v$head, info = v$head)
    expect_identical(subset$variable, v$cat, info = v$head)
    expect_identical(unlist(subset$value), "A", info = v$head)

    many <- sprintf("%s.%s (when %s='A' AND %s='Y')", v$head, v$txt,
                    v$cat, v$flag)
    reserved <- suppressWarnings(.subset_from_annotation(many, .ae_resolves(v)))
    expect_true(.is_unresolved_condition(reserved), info = v$head)

    ## Reserved for the RIGHT reason: the condition was read, and it is the
    ## carrier that cannot hold it. Reading it is what this change delivers.
    expect_false(.is_unresolved_condition(.ae_condition(many, v)),
                 info = v$head)
    expect_identical(
      .ae_shape(.ae_condition(many, v)),
      sprintf("AND(%s.%s EQ A, %s.%s EQ Y)", v$head, v$cat, v$head, v$flag),
      info = v$head)
  }
})
