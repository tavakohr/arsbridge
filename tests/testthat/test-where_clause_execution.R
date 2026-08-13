## Cross-dataset where-clause execution, characterised before it is refactored.
##
## `where_keep_mask()` and `apply_where_clause()` are closures inside
## ars_to_ard(), so nothing can call them directly. They are reached only by
## running a conversion -- which is exactly why these tests are integration
## level. `apply_where_clause()` builds `df_population`, the frame every
## percentage is computed against, and it does so on BOTH execution paths, so a
## change here moves denominators rather than merely rearranging code.
##
## Written BEFORE the de-closuring refactor and expected to pass unchanged
## after it. Two of them are labelled reproductions of defects rather than
## statements of desired behaviour; they are marked KNOWN DEFECT and say what
## the right answer would be.
##
## TRTA/TRT01A denominator mapping is deliberately NOT re-tested here -- it is
## already pinned numerically, percentages included, in test-ars_to_ard.R.

skip_if_not_installed("cards")
skip_if_not_installed("withr")

## Eight subjects, two arms. ADAE carries the SAME arm variable as ADSL on
## purpose: the occurrence-domain case, where the domain says TRTA and ADSL
## says TRT01A, is a different mechanism (.denominator_by_subject(), which
## refuses the join when a population subject has no record in the domain) and
## is already pinned numerically in test-ars_to_ard.R. Mixing it in here would
## move the denominator for reasons that have nothing to do with where-clauses.
## ADCM exists so a clause can reference a SECOND foreign dataset. ADNOKEY has
## no subject key at all.
.wc_adam <- function(envir = parent.frame()) {
  td <- withr::local_tempdir(.local_envir = envir)

  utils::write.csv(data.frame(
    USUBJID = sprintf("S%02d", 1:8),
    TRT01A  = rep(c("Drug A", "Placebo"), each = 4),
    SAFFL   = c("Y", "Y", "Y", "Y", "Y", "Y", "Y", "N"),
    ## Subject level only: an ADAE analysis cannot answer a COHORTN clause
    ## from its own rows, which is what makes it the cross-dataset case.
    COHORTN = c(1, 1, 2, 2, 1, 1, 2, 2),
    stringsAsFactors = FALSE
  ), file.path(td, "adsl.csv"), row.names = FALSE)

  utils::write.csv(data.frame(
    USUBJID = c("S01", "S01", "S02", "S03", "S05", "S06", "S07"),
    TRT01A  = c("Drug A", "Drug A", "Drug A", "Drug A",
                "Placebo", "Placebo", "Placebo"),
    AEDECOD = c("Headache", "Nausea", "Headache", "Headache",
                "Headache", "Nausea", "Nausea"),
    TRTEMFL = "Y",
    stringsAsFactors = FALSE
  ), file.path(td, "adae.csv"), row.names = FALSE)

  ## Conmeds for a subset of subjects, so an ADCM clause selects fewer
  ## subjects than the safety population.
  utils::write.csv(data.frame(
    USUBJID  = c("S01", "S02", "S05"),
    CONTRTFL = "Y",
    stringsAsFactors = FALSE
  ), file.path(td, "adcm.csv"), row.names = FALSE)

  ## No USUBJID: a cross-dataset clause cannot be carried back from here.
  utils::write.csv(data.frame(SITEID = c("01", "02"), FLAG = "Y",
                              stringsAsFactors = FALSE),
                   file.path(td, "adnokey.csv"), row.names = FALSE)
  td
}

## One analysis: subjects with each AE term, by arm, out of a population the
## `pop` where-clause defines. `pop` is the whole point -- it is what reaches
## apply_where_clause().
.wc_spec <- function(pop) {
  list(
    id = "WC", name = "WC", version = "1",
    analysisSets = list(c(list(id = "AS_POP", name = "Population"), pop)),
    dataSubsets = list(),
    analysisGroupings = list(list(
      ## Declared on ADSL, where the denominator frame carries it natively.
      ## Declaring the same variable on ADAE instead silently makes N the whole
      ## population for every column -- a separate mechanism
      ## (.denominator_by_subject) that would move the number under test here.
      id = "GF_TRT", name = "TRT01A",
      groupingVariable = list(dataset = "ADSL", variable = "TRT01A"),
      dataDriven = TRUE)),
    methods = list(list(id = "MTH_SUBJECT_COUNT_PCT",
                        name = "Subject Count and Percentage")),
    analyses = list(list(
      id = "AN_AE", methodId = "MTH_SUBJECT_COUNT_PCT",
      analysisSetId = "AS_POP",
      analysisVariable = list(dataset = "ADAE", variable = "AEDECOD"),
      orderedGroupings = list(list(order = 1, groupingId = "GF_TRT",
                                   resultsByGroup = TRUE)))),
    outputs = list(list(id = "T_AE", name = "T-AE",
                        referencedAnalysisIds = list("AN_AE"))))
}

.wc_write <- function(spec, dir, name = "wc.json") {
  path <- file.path(dir, name)
  jsonlite::write_json(spec, path, auto_unbox = TRUE, null = "null")
  path
}

.wc_run <- function(spec, dir, name = "wc.json", ...) {
  suppressMessages(suppressWarnings(
    ars_to_ard(.wc_write(spec, dir, name), dir, ...)))
}

## One statistic, by arm and AE term.
.wc_stat <- function(ard, arm, term, stat_name) {
  if (is.null(ard) || nrow(ard) == 0) return(NA_real_)
  flat <- function(column) vapply(ard[[column]], function(x) {
    if (length(x)) as.character(x[[1]]) else NA_character_
  }, character(1))
  rows <- which(flat("group1_level") == arm &
                  flat("variable_level") == term &
                  ard$stat_name == stat_name)
  if (length(rows) != 1L) return(NA_real_)
  as.numeric(ard$stat[[rows]])
}

## A BARE WhereClauseCondition, as an analysisSet node carries it. Wrapping it
## in list(condition = ) would make .eval_condition() look for `variable` on a
## WhereClause and find none, so the clause would silently select every row --
## which is exactly how the first draft of these tests passed while measuring
## nothing.
.wc_cond <- function(dataset, variable, value) {
  list(dataset = dataset, variable = variable,
       comparator = "EQ", value = list(value))
}

## What goes into the analysisSet node for a simple clause.
.wc_condition <- function(dataset, variable, value) {
  list(condition = .wc_cond(dataset, variable, value))
}

# ---- supported behaviour: one foreign dataset -------------------------------

test_that("a cross-dataset population filter restricts the denominator", {
  ## The clause names ADSL; the analysis runs on ADAE, which carries neither
  ## SAFFL nor COHORTN. Row-wise evaluation on the AE frame would read every
  ## row as FALSE and report nothing, and dropping the clause would count the
  ## whole study -- so N is the assertion that separates right from both wrongs.
  td   <- .wc_adam()
  ards <- list(
    default = .wc_run(.wc_spec(.wc_condition("ADSL", "COHORTN", 1)), td),
    legacy  = .wc_run(.wc_spec(.wc_condition("ADSL", "COHORTN", 1)), td,
                      legacy = TRUE))

  adsl <- utils::read.csv(file.path(td, "adsl.csv"), stringsAsFactors = FALSE)
  adae <- utils::read.csv(file.path(td, "adae.csv"), stringsAsFactors = FALSE)
  in_pop <- adsl$USUBJID[adsl$COHORTN == 1]

  for (nm in names(ards)) {
    ard <- ards[[nm]]
    for (arm in c("Drug A", "Placebo")) {
      arm_subjects <- intersect(in_pop, adsl$USUBJID[adsl$TRT01A == arm])
      expected_n <- length(unique(adae$USUBJID[
        adae$AEDECOD == "Headache" & adae$TRT01A == arm &
          adae$USUBJID %in% arm_subjects]))

      expect_equal(.wc_stat(ard, arm, "Headache", "N"),
                   length(arm_subjects), info = paste(nm, arm, "N"))
      expect_equal(.wc_stat(ard, arm, "Headache", "n"),
                   expected_n, info = paste(nm, arm, "n"))
      expect_equal(.wc_stat(ard, arm, "Headache", "p"),
                   expected_n / length(arm_subjects),
                   info = paste(nm, arm, "p"))
    }
  }

  ## Both failure modes named above are counts, so pin that neither happened:
  ## the cohort-1 denominator is smaller than the whole study and not zero.
  expect_lt(.wc_stat(ards$default, "Drug A", "Headache", "N"),
            sum(adsl$TRT01A == "Drug A"))
  expect_gt(.wc_stat(ards$default, "Drug A", "Headache", "N"), 0)
})

test_that("a same-dataset population filter is evaluated row by row", {
  ## The other branch of the same rule: when the clause names the analysis's
  ## own dataset there is nothing to carry back by subject.
  td  <- .wc_adam()
  ard <- .wc_run(.wc_spec(.wc_condition("ADAE", "TRTEMFL", "Y")), td)
  adae <- utils::read.csv(file.path(td, "adae.csv"), stringsAsFactors = FALSE)

  expect_equal(.wc_stat(ard, "Drug A", "Headache", "n"),
               length(unique(adae$USUBJID[adae$AEDECOD == "Headache" &
                                            adae$TRT01A == "Drug A"])))
})

# ---- diagnostics the refactor must not disturb ------------------------------

test_that("a referenced dataset that is not there FAILs by name", {
  ## get_df() owns this diagnostic today. Severity, wording and control flow
  ## are pinned here so promoting it to a package-level store cannot change
  ## them as a side effect.
  td  <- .wc_adam()
  ard <- .wc_run(.wc_spec(.wc_condition("ADXX", "FLAG", "Y")), td)

  diagnostics <- ars_diagnostics()
  hit <- diagnostics[grepl("ADXX", diagnostics$problem), , drop = FALSE]

  expect_gt(nrow(hit), 0)
  expect_true(all(hit$severity == "FAIL"))
  expect_match(hit$problem[[1]], "Dataset ADXX not found in ADaM directory",
               fixed = TRUE)
  expect_match(hit$action[[1]], "skipped")
  ## And the run completes rather than erroring.
  expect_true(is.null(ard) || is.data.frame(ard))
})

test_that("KNOWN DEFECT: a referenced dataset with no subject key", {
  ## The executor half behaves: it sees that nothing can be carried back
  ## without a key, declines to apply the filter, and says so -- a silently
  ## unapplied population filter would be a wrong denominator.
  ##
  ## The emitted half has no such check. .apply_where_expr() writes
  ## `USUBJID %in% (ADNOKEY |> filter(...) |> pull(USUBJID))` against a frame
  ## with no USUBJID, dplyr::pull() selects no column, and the analysis dies.
  ##
  ## So the WARN promises the run continues unfiltered while the analysis is
  ## actually dropped. CORRECT behaviour is what the WARN describes: continue
  ## with the filter unapplied, giving Drug A N = 4. Recorded, not endorsed;
  ## explicitly NOT in scope for the de-closuring refactor.
  td  <- .wc_adam()
  ard <- .wc_run(.wc_spec(.wc_condition("ADNOKEY", "FLAG", "Y")), td)

  diagnostics <- ars_diagnostics()
  hit <- diagnostics[grepl("ADNOKEY", diagnostics$problem), , drop = FALSE]
  expect_gt(nrow(hit), 0)
  expect_true(all(hit$severity == "WARN"))
  expect_match(hit$action[[1]], "NOT applied")

  ## The defect: nothing survives, despite the WARN saying the run goes on.
  expect_true(any(diagnostics$severity == "FAIL" &
                    grepl("cards calculation error", diagnostics$problem)))
  expect_null(ard)
})

# ---- KNOWN DEFECTS: reproduced, not endorsed --------------------------------

test_that("a valid compound clause across two foreign datasets is correct", {
  ## This WAS a KNOWN DEFECT. `ADSL.SAFFL='Y' AND ADCM.CONTRTFL='Y'` against
  ## an ADAE analysis is valid -- the shape utils_where_clause.R documents --
  ## and both halves were wrong about it: the executor evaluated the WHOLE
  ## compound against each referenced dataset in turn, so CONTRTFL read FALSE
  ## for every ADSL row, the subject intersection emptied and the population
  ## became nobody; the emitter filtered only refs[1] by a predicate naming a
  ## column that dataset does not have.
  ##
  ## The restriction plan evaluates each maximal single-dataset subtree on its
  ## own dataset and combines the resulting target-length masks, so AND across
  ## two foreign datasets is now the intersection of their subjects.
  td <- .wc_adam()
  compound <- list(compoundExpression = list(
    logicalOperator = "AND",
    whereClauses = list(
      list(condition = .wc_cond("ADSL", "SAFFL", "Y")),
      list(condition = .wc_cond("ADCM", "CONTRTFL", "Y")))))
  ard <- .wc_run(.wc_spec(compound), td, name = "compound.json")

  adsl <- utils::read.csv(file.path(td, "adsl.csv"), stringsAsFactors = FALSE)
  adcm <- utils::read.csv(file.path(td, "adcm.csv"), stringsAsFactors = FALSE)
  adae <- utils::read.csv(file.path(td, "adae.csv"), stringsAsFactors = FALSE)

  ## Computed independently: safety subjects who also have a flagged conmed.
  correct_pop <- intersect(adsl$USUBJID[adsl$SAFFL == "Y"],
                           adcm$USUBJID[adcm$CONTRTFL == "Y"])
  expect_gt(length(correct_pop), 0)

  for (arm in c("Drug A", "Placebo")) {
    in_arm <- intersect(correct_pop, adsl$USUBJID[adsl$TRT01A == arm])
    correct_n <- length(unique(adae$USUBJID[adae$AEDECOD == "Headache" &
                                              adae$USUBJID %in% in_arm]))
    expect_equal(.wc_stat(ard, arm, "Headache", "n"), correct_n,
                 info = paste(arm, "n"))
    expect_equal(.wc_stat(ard, arm, "Headache", "N"), length(in_arm),
                 info = paste(arm, "N"))
  }
})

test_that("a dataset field carrying two values is read once, and reported", {
  ## This WAS a KNOWN DEFECT: `.where_datasets()` coerced through
  ## `.as_scalar_char()` and kept only the first dataset, while the executor's
  ## own `get_referenced_datasets()` returned the raw field and intersected
  ## every referenced dataset's subjects. One spec, two filters.
  ##
  ## The duplicate is gone and `.where_datasets()` is the single source of
  ## truth, so the scalar reading now holds on both sides -- the ARS schema has
  ## `dataset` as a scalar. The deliberate behaviour change is that narrowing
  ## is no longer silent.
  multi <- list(condition = .wc_cond(list("ADSL", "ADCM"), "SAFFL", "Y"))

  ## Both halves agree.
  expect_identical(.where_datasets(multi), "ADSL")

  td       <- .wc_adam()
  as_multi <- .wc_run(.wc_spec(multi), td, name = "multi.json")
  diagnostics <- ars_diagnostics()
  as_adsl  <- .wc_run(.wc_spec(.wc_condition("ADSL", "SAFFL", "Y")), td,
                      name = "adsl.json")

  expect_equal(.wc_stat(as_multi, "Drug A", "Headache", "n"),
               .wc_stat(as_adsl,  "Drug A", "Headache", "n"))
  expect_equal(.wc_stat(as_multi, "Drug A", "Headache", "N"),
               .wc_stat(as_adsl,  "Drug A", "Headache", "N"))

  ## And the malformed cardinality is reported rather than swallowed.
  hit <- diagnostics[grepl("names 2 datasets", diagnostics$problem), ,
                     drop = FALSE]
  expect_gt(nrow(hit), 0)
  expect_true(all(hit$severity == "WARN"))
  expect_match(hit$problem[[1]], "ADSL, ADCM", fixed = TRUE)
  expect_match(hit$action[[1]], "Only ADSL is used", fixed = TRUE)
})

# ---- the promoted helpers, called directly ---------------------------------
#
# None of this was reachable before: .where_keep_mask() and its store were
# closures inside ars_to_ard(). Everything above drives them through a whole
# conversion, which is the honest way to pin behaviour but a slow and indirect
# way to pin edges. These call them.

.wc_store <- function(envir = parent.frame()) {
  .adam_store(.wc_adam(envir = envir))
}

test_that("the store reads a dataset by name, case-insensitively", {
  store <- .wc_store()
  expect_s3_class(store$get("ADSL"), "data.frame")
  expect_equal(nrow(store$get("adsl")), 8L)
  expect_null(store$get(NULL))
  expect_null(store$get(""))
})

test_that("the store FAILs by name for a dataset that is not there", {
  ## Same severity and wording the closure emitted; pinned again here because
  ## this is now the only place it lives.
  diag_reset()
  store <- .wc_store()
  expect_null(suppressWarnings(store$get("ADXX")))

  hit <- ars_diagnostics()
  hit <- hit[grepl("ADXX", hit$problem), , drop = FALSE]
  expect_equal(nrow(hit), 1L)
  expect_equal(hit$severity[[1]], "FAIL")
  expect_match(hit$problem[[1]], "Dataset ADXX not found in ADaM directory",
               fixed = TRUE)
})

test_that("a same-dataset clause is a row-wise mask", {
  store <- .wc_store()
  adae  <- store$get("ADAE")
  mask  <- .where_keep_mask(adae, "ADAE",
                            list(condition = .wc_cond("ADAE", "AEDECOD",
                                                      "Headache")),
                            store, "USUBJID")
  expect_equal(mask, adae$AEDECOD == "Headache")
})

test_that("a foreign clause is answered there and carried back by subject", {
  store <- .wc_store()
  adae  <- store$get("ADAE")
  adsl  <- store$get("ADSL")
  mask  <- .where_keep_mask(adae, "ADAE",
                            list(condition = .wc_cond("ADSL", "COHORTN", 1)),
                            store, "USUBJID")

  ## Not row-wise: ADAE has no COHORTN, so evaluating it here would keep
  ## nothing at all.
  expect_equal(mask, adae$USUBJID %in% adsl$USUBJID[adsl$COHORTN == 1])
  expect_true(any(mask))
  expect_false(all(mask))
})

test_that("a NULL clause and an empty frame are handled without special cases", {
  store <- .wc_store()
  adae  <- store$get("ADAE")
  expect_equal(.where_keep_mask(adae, "ADAE", NULL, store, "USUBJID"),
               rep(TRUE, nrow(adae)))
  expect_equal(.where_keep_mask(NULL, "ADAE", NULL, store, "USUBJID"),
               logical(0))
})

test_that("a dataset the store cannot read leaves the mask unfiltered", {
  ## Preserved behaviour, not endorsed: the clause is dropped rather than
  ## failing closed. Recorded with the deferred defects.
  diag_reset()
  store <- .wc_store()
  adae  <- store$get("ADAE")
  mask  <- suppressWarnings(.where_keep_mask(
    adae, "ADAE", list(condition = .wc_cond("ADXX", "FLAG", "Y")),
    store, "USUBJID"))
  expect_equal(mask, rep(TRUE, nrow(adae)))
})

test_that("the raw dataset reader sees cardinality the coerced one hides", {
  simple   <- list(condition = .wc_cond("ADSL", "SAFFL", "Y"))
  multi    <- list(condition = .wc_cond(list("ADSL", "ADCM"), "SAFFL", "Y"))
  compound <- list(compoundExpression = list(
    logicalOperator = "AND",
    whereClauses = list(list(condition = .wc_cond("ADSL", "SAFFL", "Y")),
                        list(condition = .wc_cond("ADCM", "CONTRTFL", "Y")))))

  expect_equal(.where_datasets_raw(simple), list("ADSL"))
  expect_equal(.where_datasets_raw(multi), list(list("ADSL", "ADCM")))
  expect_equal(.where_datasets_raw(compound), list("ADSL", "ADCM"))
  expect_equal(.where_datasets_raw(NULL), list())

  ## A well-formed compound naming two datasets is NOT malformed cardinality:
  ## two fields of one value each, so nothing is reported.
  diag_reset()
  expect_equal(.where_datasets_checked(compound), c("ADSL", "ADCM"))
  expect_equal(nrow(ars_diagnostics()), 0L)
})

test_that("apply_where_clause returns the frame the mask selects", {
  store <- .wc_store()
  kept  <- .apply_where_clause("ADAE",
                               list(condition = .wc_cond("ADAE", "AEDECOD",
                                                         "Headache")),
                               store, "USUBJID")
  expect_equal(nrow(kept), sum(store$get("ADAE")$AEDECOD == "Headache"))
  ## No clause is the whole frame; an unreadable dataset is NULL, not an error.
  expect_equal(nrow(.apply_where_clause("ADAE", NULL, store, "USUBJID")),
               nrow(store$get("ADAE")))
  expect_null(suppressWarnings(
    .apply_where_clause("ADXX", NULL, store, "USUBJID")))
})
