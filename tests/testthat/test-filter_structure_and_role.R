## What a filter RESTRICTS, and what happens when its structure cannot be
## carried.
##
## THE DEFECT CLASS. Two failures with one shape: arsbridge accepted a filter
## it could not represent, and then acted on the acceptance.
##
##   Boolean structure. The clause splitter is flat -- it finds one joiner and
##   splits on it -- so an expression carrying grouping, negation, or both
##   joiners was not REFUSED. It was answered with a different expression that
##   is valid, executable and restricts other records. `A AND (B OR C)` became
##   `AND(A, B, C)`; a population written that way emitted an analysis set no
##   subject satisfies, and every percentage under it divided by the wrong N.
##
##   Filter role. "The filter is on the row's own variable" and "I could not
##   read the filter" both presented as an empty filter variable, and both
##   selected the subject-count family. So an unreadable annotation was typed
##   as a count of something nobody could evaluate, under a line displaying
##   two statistics.
##
## THE INVARIANT. A restriction this grammar cannot represent is refused, not
## approximated: it reserves, exactly like one that cannot be read at all,
## because to the reader of the number the two are the same failure. And the
## role a filter plays describes the RESTRICTION only -- it never decides which
## statistic the line reports.
##
## THE OVER-RESERVATION HAZARD, pinned in both directions throughout. Refusing
## is not free: a reserved row withholds a result that may have been perfectly
## correct. So the refusal is narrow -- parentheses that group nothing, a
## comparator whose spelling contains "!", prose carrying an English "not",
## and a whole-expression bracket are all left alone.
##
## Identifiers here are invented and belong to no study. The second vocabulary
## exists to prove the rules key on structure rather than on familiar names.

.fsr_v1 <- list(ds = "ADQX", cat = "QXCAT", flag = "QXPRESP",
                occur = "QXOCCUR", txt = "QXTRT", num = "QXVAL",
                parm = "QXPARM", sev = "QXSEV")
.fsr_v2 <- list(ds = "ADZZ", cat = "ZZGROUP", flag = "ZZSTAT",
                occur = "ZZDONE", txt = "ZZNAME", num = "ZZAMT",
                parm = "ZZCODE", sev = "ZZGRADE")

.fsr_kind <- function(expr) {
  wc <- suppressWarnings(parse_where_clause(expr))
  if (.is_unresolved_condition(wc)) return("unresolved")
  if (is.null(wc)) return("absent")
  "clause"
}

.fsr_lookup <- function(v) {
  rec <- function(var, type, codelist = "") {
    list(dataset = v$ds, variable = var, type = type, codelist = codelist)
  }
  out <- list()
  out[[paste0(v$ds, ".", v$txt)]]   <- rec(v$txt,   "Char")
  out[[paste0(v$ds, ".", v$cat)]]   <- rec(v$cat,   "Char")
  out[[paste0(v$ds, ".", v$flag)]]  <- rec(v$flag,  "Char", "NY")
  out[[paste0(v$ds, ".", v$occur)]] <- rec(v$occur, "Char", "NY")
  out[[paste0(v$ds, ".", v$sev)]]   <- rec(v$sev,   "Char")
  out[[paste0(v$ds, ".", v$parm)]]  <- rec(v$parm,  "Char")
  out[[paste0(v$ds, ".", v$num)]]   <- rec(v$num,   "Num")
  out[["ADSL.SAFFL"]]  <- list(dataset = "ADSL", variable = "SAFFL",
                               type = "Char", codelist = "NY")
  out[["ADSL.TRT01A"]] <- list(dataset = "ADSL", variable = "TRT01A",
                               type = "Char", codelist = "")
  out
}

.fsr_section <- function(v, rows, population = "ADSL.SAFFL='Y'") {
  list(
    tlf_number = "T-99-1-1", tlf_type = "TABLE", title = "Structure probe",
    population_text = "Safety Population", population_annot = population,
    source_datasets = v$ds, col_headers = c("", "A", "B"), n_data_cols = 2L,
    stub_rows = rows, analysis_type = "CATEGORICAL",
    ars_method_name = "Count and Percentage",
    by_variable = "TRT01A", by_variable_dataset = "ADSL",
    enriched_rows = list())
}

.fsr_row <- function(label, annotation, slots = 2L, where = NULL) {
  row <- list(label = label, annotation = annotation, has_annot = TRUE,
              raw_text = label, n_slots = slots)
  if (!is.null(where)) {
    row$supplement_where <- where
    row$detection_method <- "supplement"
  }
  row
}

.fsr_cond <- function(ds, variable, value, comparator = "EQ") {
  list(condition = list(dataset = ds, variable = variable,
                        comparator = comparator, value = list(value)))
}
.fsr_and <- function(...) {
  list(compoundExpression = list(logicalOperator = "AND",
                                 whereClauses = list(...)))
}
.fsr_or <- function(...) {
  list(compoundExpression = list(logicalOperator = "OR",
                                 whereClauses = list(...)))
}


## ---------------------------------------------------------------------------
## A. Structures that must be refused
## ---------------------------------------------------------------------------

test_that("an expression whose Boolean structure cannot be carried reserves", {
  for (v in list(.fsr_v1, .fsr_v2)) {
    a <- sprintf("%s.%s='A'", v$ds, v$cat)
    b <- sprintf("%s.%s='Y'", v$ds, v$flag)
    c_ <- sprintf("%s.%s='Y'", v$ds, v$occur)

    ## Each of these has a MEANING this grammar has no shape for. The flat
    ## split answers every one of them with `AND(a, b, c)`, which is a
    ## different restriction -- and for the third, one no record satisfies.
    unrepresentable <- c(
      sprintf("%s AND (%s OR %s)", a, b, c_),
      sprintf("(%s OR %s) AND %s", a, b, c_),
      sprintf("(%s AND %s) OR %s", a, b, c_),
      sprintf("%s OR %s AND %s", a, b, c_),
      sprintf("NOT (%s OR %s)", a, b),
      sprintf("%s AND NOT %s", a, b)
    )

    kinds <- vapply(unrepresentable, .fsr_kind, character(1), USE.NAMES = FALSE)
    ## Scope assertion: six forms were actually read, so a grammar change that
    ## stopped recognising them turns this red instead of vacuously green.
    expect_equal(length(kinds), 6L, info = v$ds)
    expect_true(all(kinds == "unresolved"), info = v$ds)
  }
})


test_that("a refused structure names the construct, not a parse failure", {
  v <- .fsr_v1
  expr <- sprintf("%s.%s='A' AND (%s.%s='Y' OR %s.%s='Y')",
                  v$ds, v$cat, v$ds, v$flag, v$ds, v$occur)
  expect_identical(.unsupported_structure(expr), "grouped sub-expressions")
  expect_identical(
    .unsupported_structure(sprintf("NOT %s.%s='A'", v$ds, v$cat)), "negation")
  expect_identical(
    .unsupported_structure(sprintf("%s.%s='A' OR %s.%s='Y' AND %s.%s='Y'",
                                   v$ds, v$cat, v$ds, v$flag, v$ds, v$occur)),
    "mixed AND/OR without explicit grouping")
})


## ---------------------------------------------------------------------------
## B. Structures that must NOT be refused
## ---------------------------------------------------------------------------

test_that("brackets and words that group nothing are left alone", {
  for (v in list(.fsr_v1, .fsr_v2)) {
    ## Every one of these is either representable as written, or states no
    ## condition at all. Refusing any of them withholds a correct result.
    safe <- c(
      sprintf("(%s.%s='Y')", v$ds, v$flag),                   # whole-wrap
      sprintf("((%s.%s='Y'))", v$ds, v$flag),                 # wrapped twice
      sprintf("(%s.%s='Y' AND %s.%s='Y')",                    # wrap over one
              v$ds, v$flag, v$ds, v$occur),                   #   AND: flat
      sprintf("%s.%s='Y' AND %s.%s='Y'", v$ds, v$flag, v$ds, v$occur),
      sprintf("%s.%s='A' OR %s.%s='B'", v$ds, v$cat, v$ds, v$cat),
      sprintf("%s.%s IN ('A','B')", v$ds, v$cat),             # call-shaped ()
      sprintf("is.na(%s.%s)", v$ds, v$cat),
      sprintf("not missing(%s.%s)", v$ds, v$cat),
      sprintf("%s.%s not missing", v$ds, v$cat),
      sprintf("%s.%s between 1 and 5", v$ds, v$num),          # inner "and"
      ## A bracket around ONE condition inside a joined expression. It groups
      ## nothing -- there is no second operand for it to bind -- so the
      ## reading is the flat one either way, and refusing it would withhold a
      ## correct result over a punctuation habit.
      sprintf("%s.%s='Y' AND (%s.%s='Y')", v$ds, v$flag, v$ds, v$occur),
      sprintf("(%s.%s='Y') AND (%s.%s='Y')", v$ds, v$flag, v$ds, v$occur),
      sprintf("(%s.%s='A') OR (%s.%s='B')", v$ds, v$cat, v$ds, v$cat)
    )
    kinds <- vapply(safe, .fsr_kind, character(1), USE.NAMES = FALSE)
    expect_equal(length(kinds), 13L, info = v$ds)
    expect_false(any(kinds == "unresolved"), info = v$ds)

    ## And the readable ones really did produce a clause, so "not unresolved"
    ## cannot be satisfied by everything collapsing to "absent".
    expect_true(sum(kinds == "clause") >= 11L, info = v$ds)

    ## The bracketed-atom forms keep every condition, which is what proves
    ## they were read flat rather than merely "not refused".
    both <- suppressWarnings(parse_where_clause(
      sprintf("(%s.%s='Y') AND (%s.%s='Y')", v$ds, v$flag, v$ds, v$occur)))
    expect_length(.where_atoms(both), 2L)
  }
})


test_that("a flat conjunction keeps every one of its conditions", {
  ## The counterpart to the refusals above: an AND of three is representable,
  ## and all three must survive. A refusal that swept this up would withhold
  ## most of the filters real shells are written with.
  for (v in list(.fsr_v1, .fsr_v2)) {
    wc <- suppressWarnings(parse_where_clause(sprintf(
      "%s.%s='DIAGNOSTIC' AND %s.%s='Y' AND %s.%s='Y'",
      v$ds, v$cat, v$ds, v$flag, v$ds, v$occur)))
    expect_false(.is_unresolved_condition(wc), info = v$ds)
    expect_identical(wc$compoundExpression$logicalOperator, "AND", info = v$ds)
    expect_length(wc$compoundExpression$whereClauses, 3L)
    expect_identical(
      vapply(.where_atoms(wc), function(a) a$variable, character(1)),
      c(v$cat, v$flag, v$occur), info = v$ds)
  }
})


test_that("prose and comparators are not read as structure", {
  v <- .fsr_v1
  ## An English "not" in text that states no condition. Reserving here would
  ## withhold every row whose annotation carries an ordinary sentence.
  expect_identical(.fsr_kind("not applicable"), "absent")
  expect_identical(.fsr_kind(sprintf("%s.%s (not collected)", v$ds, v$txt)),
                   "absent")

  ## "!=" is a comparator; its "!" negates no sub-expression. The structure
  ## check must not claim otherwise -- whether or not the clause parser can
  ## read the comparator is a separate question with its own answer.
  expect_null(.unsupported_structure(sprintf("%s.%s != 5", v$ds, v$num)))
})


## ---------------------------------------------------------------------------
## C. The refusal reaches every entity that can carry a condition
## ---------------------------------------------------------------------------

test_that("an unrepresentable population reserves the analyses that use it", {
  for (v in list(.fsr_v1, .fsr_v2)) {
    pop <- sprintf("ADSL.SAFFL='Y' AND (%s.%s='A' OR %s.%s='B')",
                   v$ds, v$cat, v$ds, v$cat)
    sec <- .fsr_section(
      v, list(.fsr_row("Assessment", sprintf("%s.%s", v$ds, v$txt))),
      population = pop)
    re <- suppressWarnings(build_ars_json(list(sec),
                                          spec_lookup = .fsr_lookup(v)))

    as_node <- re$analysisSets[[1]]
    ## The set is MARKED, and carries no condition of its own: an unreadable
    ## population that emitted a condition anyway would restrict by something
    ## the author did not write.
    expect_true(nzchar(as_node$unresolvedCondition %||% ""), info = v$ds)
    expect_null(as_node$condition, info = v$ds)
    expect_null(as_node$compoundExpression, info = v$ds)

    ## And it withholds: GAP, then the reservation map takes the analyses.
    model <- ars_to_model(re)
    findings <- validate_ars_model(model)
    hit <- findings[findings$ref == "ANALYSIS_SET_CONDITION_UNRESOLVED", ,
                    drop = FALSE]
    expect_equal(nrow(hit), 1L, info = v$ds)
    ## Guarded so a missing finding FAILS rather than erroring on the index:
    ## an erroring test proves the mutant broke something, not that this
    ## assertion caught it.
    expect_identical(if (nrow(hit) == 1L) hit$severity[[1]] else NA_character_,
                     "GAP", info = v$ds)

    reserved <- names(.reservations_from_findings(model, findings)$by_analysis)
    expect_gt(length(reserved), 0L)
    expect_true(all(model$analyses$id %in% reserved), info = v$ds)
  }
})


test_that("an unrepresentable column level reserves through its grouping", {
  v <- .fsr_v1
  sec <- list(
    tlf_number = "T-99-2-2", tlf_type = "TABLE", title = "Header structure",
    .pending_column_annotations = list(
      labels = c("Group A (N=XX)", "Group B (N=XX)", "Either (N=XX)"),
      annotations = c(
        sprintf("%s.%s='A'", v$ds, v$cat),
        sprintf("%s.%s='B'", v$ds, v$cat),
        ## Grouping inside a column header: the level cannot be represented,
        ## so the column must not silently become an unrestricted one.
        sprintf("%s.%s='A' AND (%s.%s='Y' OR %s.%s='Y')",
                v$ds, v$cat, v$ds, v$flag, v$ds, v$occur))))
  out <- suppressWarnings(.resolve_table_column_groups(sec))
  out$by_variable <- v$cat
  out$by_variable_dataset <- v$ds

  gf <- suppressWarnings(.build_grouping(out))
  ## The axis keeps its shape -- three headers in, three levels out -- and the
  ## unreadable one is marked rather than dropped, so the remaining levels
  ## cannot close the gap and re-label a column.
  expect_length(gf$groups, 3L)
  marked <- vapply(gf$groups,
                   function(g) nzchar(g$unresolvedCondition %||% ""),
                   logical(1))
  expect_equal(sum(marked), 1L)
})


test_that("a row's stated restriction is carried or reserved, never dropped", {
  ## The shape of the real-world annotation: a qualified conjunction on a row.
  ## The GRAMMAR reads it -- asserted first, so this test can never be read as
  ## "a flat AND is unrepresentable" -- and the question is what the built
  ## analysis then does with it.
  ##
  ## Exactly two outcomes are acceptable, and the test is written to accept
  ## either, because which one applies changes as the subset builder grows:
  ##
  ##   carried   the analysis has a DataSubset expressing the restriction, or
  ##   reserved  the analysis has none and says so.
  ##
  ## The third outcome is the defect: no subset, no marker, and an analysis
  ## that computes over every record in the dataset while looking finished.
  for (v in list(.fsr_v1, .fsr_v2)) {
    tail <- sprintf("%s.%s='DIAGNOSTIC' AND %s.%s='Y'",
                    v$ds, v$cat, v$ds, v$flag)
    expect_false(.is_unresolved_condition(
      suppressWarnings(parse_where_clause(tail))), info = v$ds)

    ann <- sprintf("%s.%s WHERE %s", v$ds, v$txt, tail)
    ## Whatever comes back, it is never the answer that means "no filter was
    ## ever stated" -- that is what let the restriction disappear.
    expect_false(is.null(suppressWarnings(.subset_from_annotation(ann))),
                 info = v$ds)

    sec <- .fsr_section(v, list(.fsr_row("Type of assessment", ann)))
    re <- suppressWarnings(build_ars_json(list(sec),
                                          spec_lookup = .fsr_lookup(v)))
    an <- re$analyses[[1]]
    carried  <- nzchar(an$dataSubsetId %||% "")
    reserved <- nzchar(an$unresolvedCondition %||% "")
    expect_true(carried || reserved, info = v$ds)

    ## When it is the reservation, it must withhold rather than merely warn.
    if (reserved) {
      model <- ars_to_model(re)
      findings <- validate_ars_model(model)
      hit <- findings[findings$ref == "ANALYSIS_CONDITION_UNRESOLVED", ,
                      drop = FALSE]
      expect_equal(nrow(hit), 1L, info = v$ds)
      expect_identical(hit$severity[[1]], "GAP", info = v$ds)
    }
  }
})


test_that("a clause of a joined restriction is never dropped in silence", {
  ## How shells write a filter after the head reference: the first clause
  ## carries the dataset and the rest do not. The unqualified clauses matched
  ## no pattern AND carried no DATASET.VARIABLE, so they were not even counted
  ## as dropped -- the row filtered on one clause of three, with a WARN that
  ## named only the clause that DID parse.
  for (v in list(.fsr_v1, .fsr_v2)) {
    ann <- sprintf("%s.%s WHERE %s='DIAGNOSTIC' AND %s='Y' AND %s='Y'",
                   v$ds, v$txt, v$cat, v$flag, v$occur)
    sec <- .fsr_section(v, list(.fsr_row("Type of assessment", ann)))
    re <- suppressWarnings(build_ars_json(list(sec),
                                          spec_lookup = .fsr_lookup(v)))
    an <- re$analyses[[1]]
    ## Reserved, and specifically NOT filtered on the one clause that parsed.
    expect_true(nzchar(an$unresolvedCondition %||% ""), info = v$ds)
    expect_false(nzchar(an$dataSubsetId %||% ""), info = v$ds)

    ## The control, and it is what keeps this from being "reserve everything":
    ## the same form with a single clause is unambiguous and still computes.
    one <- sprintf("%s.%s WHERE %s='DIAGNOSTIC'", v$ds, v$txt, v$cat)
    sec1 <- .fsr_section(v, list(.fsr_row("Type of assessment", one)))
    re1 <- suppressWarnings(build_ars_json(list(sec1),
                                           spec_lookup = .fsr_lookup(v)))
    an1 <- re1$analyses[[1]]
    expect_true(nzchar(an1$dataSubsetId %||% ""), info = v$ds)
    expect_false(nzchar(an1$unresolvedCondition %||% ""), info = v$ds)
  }
})


## ---------------------------------------------------------------------------
## D. What the filter restricts
## ---------------------------------------------------------------------------

test_that("filter role reports what the restriction speaks about", {
  for (v in list(.fsr_v1, .fsr_v2)) {
    role <- function(where) .filter_role(where, v$ds, v$txt)

    expect_identical(role(NULL), "none")
    expect_identical(role(.fsr_cond(v$ds, v$txt, "X")), "on_primary")
    expect_identical(role(.fsr_cond(v$ds, v$cat, "X")), "scoping_other")
    expect_identical(
      role(.fsr_and(.fsr_cond(v$ds, v$cat, "X"), .fsr_cond(v$ds, v$flag, "Y"))),
      "scoping_other")
    expect_identical(
      role(.fsr_and(.fsr_cond(v$ds, v$txt, "X"), .fsr_cond(v$ds, v$cat, "Y"))),
      "mixed_conjunctive")
    expect_identical(
      role(.unresolved_condition("something unreadable")), "unknown")

    ## Mixed under an OR is NOT conjunctive: the branch that was taken may not
    ## have restricted the row's variable at all, so the atoms cannot be
    ## separated into "about me" and "scope". No separation, no answer.
    expect_identical(
      role(.fsr_or(.fsr_cond(v$ds, v$txt, "X"), .fsr_cond(v$ds, v$cat, "Y"))),
      "unknown")
  }
})


test_that("filter role keeps dataset identity", {
  ## A multi-domain table is the case this protects. Two ADaM datasets may
  ## carry a variable of the same name, and matching on the name alone reads a
  ## filter on ANOTHER domain's variable as a filter on this row's own.
  v <- .fsr_v1
  same_name_elsewhere <- .fsr_cond("ADZZ", v$txt, "X")
  expect_identical(.filter_role(same_name_elsewhere, v$ds, v$txt),
                   "scoping_other")
  expect_identical(.filter_role(.fsr_cond(v$ds, v$txt, "X"), v$ds, v$txt),
                   "on_primary")
})


test_that("pinning is equality, not any mention of the variable", {
  for (v in list(.fsr_v1, .fsr_v2)) {
    pins <- function(where) .filter_pins_primary(where, v$ds, v$num)

    ## One value survives: the line can only report how many.
    expect_true(pins(.fsr_and(.fsr_cond(v$ds, v$parm, "P1"),
                              .fsr_cond(v$ds, v$num, "5"))))
    ## Bounded, not pinned: the variable still varies inside the subset.
    expect_false(pins(.fsr_and(.fsr_cond(v$ds, v$parm, "P1"),
                               .fsr_cond(v$ds, v$num, "0", "GT"))))
    expect_false(pins(.fsr_cond(v$ds, v$parm, "P1")))
    ## Under an OR a "pinning" atom may sit on the branch not taken.
    expect_false(pins(.fsr_or(.fsr_cond(v$ds, v$num, "5"),
                              .fsr_cond(v$ds, v$parm, "P1"))))
  }
})


## ---------------------------------------------------------------------------
## E. The role does not decide the statistic
## ---------------------------------------------------------------------------

test_that("a restriction touching the row's variable does not force a count", {
  ## The regression that matters most here, and the one the old heuristic got
  ## wrong in both directions.
  for (v in list(.fsr_v1, .fsr_v2)) {
    ## Continuous: the filter scopes to one parameter and bounds the value.
    ## The variable still varies, the shell is asking for a summary of it, and
    ## a subject count would answer a question nobody asked.
    bounded <- .fsr_and(.fsr_cond(v$ds, v$parm, "P1"),
                        .fsr_cond(v$ds, v$num, "0", "GT"))
    got <- .infer_row_method(
      list(annotation = sprintf("%s.%s WHERE %s.%s='P1' AND %s.%s GT 0",
                                v$ds, v$num, v$ds, v$parm, v$ds, v$num),
           n_slots = 2L),
      var_is_categorical = FALSE, filter = bounded, filter_known = TRUE)
    expect_identical(got$method, "Summary Statistics - Continuous", info = v$ds)

    ## Counted: the same SHAPE of restriction, but it pins the variable to one
    ## value, so there is nothing left to distribute over.
    pinned <- .fsr_and(.fsr_cond(v$ds, v$flag, "Y"),
                       .fsr_cond(v$ds, v$sev, "SEVERE"))
    got <- .infer_row_method(
      list(annotation = sprintf("%s.%s WHERE %s.%s='Y' AND %s.%s='SEVERE'",
                                v$ds, v$sev, v$ds, v$flag, v$ds, v$sev),
           n_slots = 2L),
      var_is_categorical = TRUE, filter = pinned, filter_known = TRUE)
    expect_identical(got$method, "Subject Count and Percentage", info = v$ds)

    ## Scoping only: the row still reports the distribution of its variable.
    scoping <- .fsr_cond(v$ds, v$cat, "DIAGNOSTIC")
    got <- .infer_row_method(
      list(annotation = sprintf("%s.%s WHERE %s.%s='DIAGNOSTIC'",
                                v$ds, v$txt, v$ds, v$cat), n_slots = 2L),
      var_is_categorical = TRUE, filter = scoping, filter_known = TRUE)
    expect_identical(got$method, "Count and Percentage", info = v$ds)
  }
})


test_that("an unreadable filter does not select the count family by default", {
  for (v in list(.fsr_v1, .fsr_v2)) {
    ## Restricts OTHER variables: the row still reports its own variable's
    ## distribution, and the reservation -- not the method -- is what stops a
    ## number being produced.
    ann <- sprintf("%s.%s (when %s='DIAGNOSTIC' AND %s='Y')",
                   v$ds, v$txt, v$cat, v$flag)
    got <- .infer_row_method(list(annotation = ann, n_slots = 2L),
                             var_is_categorical = TRUE,
                             filter = .unresolved_condition(ann),
                             filter_known = TRUE)
    expect_identical(got$method, "Count and Percentage", info = v$ds)

    ## A threshold this grammar cannot yet read is still a threshold ON the
    ## row's own variable, and still a count. Method intent follows from a
    ## condition being STATED, not from the parser succeeding on it.
    thresh <- sprintf("%s.%s >= 16", v$ds, v$num)
    got <- .infer_row_method(list(annotation = thresh, n_slots = 2L),
                             var_is_categorical = FALSE,
                             filter = .unresolved_condition(thresh),
                             filter_known = TRUE)
    expect_identical(got$method, "Subject Count and Percentage", info = v$ds)

    ## An unreadable restriction naming another variable through a form with
    ## no comparison operator at all must NOT be mistaken for one about the
    ## row's own variable -- absence of a recognised operand is not evidence.
    call_form <- sprintf("%s.%s WHERE is.na(%s)", v$ds, v$txt, v$cat)
    expect_false(.unreadable_restricts_only_primary(call_form, v$ds, v$txt),
                 info = v$ds)
  }
})


## ---------------------------------------------------------------------------
## F. Metadata is answered for the dataset that was asked about
## ---------------------------------------------------------------------------

test_that("a variable's type is not borrowed from another dataset", {
  ## A table mixing domains: the row names one dataset's variable, and only
  ## ANOTHER dataset carries a variable of that name. Answering from the other
  ## dataset decides this row's METHOD on metadata belonging to something else.
  ##
  ## The section is deliberately continuous while the same-named variable
  ## elsewhere is character, so the two answers cannot coincide -- with the
  ## borrowed record the row would be corrected to a count, and without it the
  ## row keeps the section's method. A fixture whose section method matched
  ## the borrowed verdict would pass either way and prove nothing.
  v <- .fsr_v1
  sec <- .fsr_section(v, list(.fsr_row("Assessment",
                                       sprintf("%s.%s", v$ds, v$txt))))
  sec$analysis_type <- "CONTINUOUS"
  sec$ars_method_name <- "Summary Statistics - Continuous"

  elsewhere <- list()
  elsewhere[[paste0("ADZZ.", v$txt)]] <- list(dataset = "ADZZ",
                                              variable = v$txt,
                                              type = "Char", codelist = "")
  elsewhere[["ADSL.SAFFL"]] <- list(dataset = "ADSL", variable = "SAFFL",
                                    type = "Char", codelist = "NY")
  re <- suppressWarnings(build_ars_json(list(sec), spec_lookup = elsewhere))
  expect_length(re$analyses, 1L)
  expect_identical(re$analyses[[1]]$methodId,
                   "MTH_SUMMARY_STATISTICS_CONTINUOUS")

  ## The control that gives it meaning: describe the row's OWN
  ## DATASET.VARIABLE and the verdict becomes available, so the row is
  ## corrected to the categorical method. Without this the assertion above
  ## would pass on a build that never consults the spec at all.
  own <- elsewhere
  own[[paste0(v$ds, ".", v$txt)]] <- list(dataset = v$ds, variable = v$txt,
                                          type = "Char", codelist = "")
  re2 <- suppressWarnings(build_ars_json(list(sec), spec_lookup = own))
  expect_identical(re2$analyses[[1]]$methodId, "MTH_COUNT_AND_PERCENTAGE")
})
