## The restriction plan: one structure, two consumers.
##
## The rule it encodes is not per-atom, and that distinction is the whole
## point. A maximal subtree naming ONE dataset is evaluated ROW-WISE on that
## dataset; only then, if the dataset is foreign, are qualifying rows projected
## to subject ids. Decomposing a same-dataset AND into independent existential
## subject tests would keep a subject whose predicates are satisfied by two
## DIFFERENT records -- see T1, which is the case that motivated all of this.
##
## Regrouping under AND uses associativity and commutativity only. Regrouping
## across an OR would need distribution, which changes the answer, so an
## expression whose row coherence cannot be recovered that way is unsupported
## and both halves must refuse it rather than pick a reading (T5).

skip_if_not_installed("withr")

.rp_cond <- function(ds, var, val) {
  list(condition = list(dataset = ds, variable = var,
                        comparator = "EQ", value = list(val)))
}
.rp_and <- function(...) list(compoundExpression = list(
  logicalOperator = "AND", whereClauses = list(...)))
.rp_or <- function(...) list(compoundExpression = list(
  logicalOperator = "OR", whereClauses = list(...)))

## The conmed frame from the counterexample: S01's two predicates are satisfied
## by two different records; S02 satisfies both on one record.
.rp_adam <- function(envir = parent.frame()) {
  td <- withr::local_tempdir(.local_envir = envir)
  utils::write.csv(data.frame(
    USUBJID = c("S01", "S02", "S03", "S04"),
    TRT01A  = c("A", "A", "B", "B"),
    SAFFL   = c("Y", "Y", "Y", "N"),
    stringsAsFactors = FALSE
  ), file.path(td, "adsl.csv"), row.names = FALSE)

  utils::write.csv(data.frame(
    USUBJID  = c("S01", "S01", "S02", "S03"),
    CMDECOD  = c("ASPIRIN", "IBUPROFEN", "ASPIRIN", "IBUPROFEN"),
    CONTRTFL = c("N", "Y", "Y", "Y"),
    stringsAsFactors = FALSE
  ), file.path(td, "adcm.csv"), row.names = FALSE)

  utils::write.csv(data.frame(
    USUBJID = c("S01", "S02", "S03", "S04"),
    AEDECOD = "Headache",
    stringsAsFactors = FALSE
  ), file.path(td, "adae.csv"), row.names = FALSE)
  td
}

.rp_mask <- function(where, envir = parent.frame()) {
  store <- .adam_store(.rp_adam(envir = envir))
  adae  <- store$get("ADAE")
  list(mask = .where_keep_mask(adae, "ADAE", where, store, "USUBJID"),
       subjects = adae$USUBJID)
}

.rp_kept <- function(where, envir = parent.frame()) {
  got <- .rp_mask(where, envir = envir)
  if (is.null(got$mask)) return(NULL)
  got$subjects[got$mask]
}

# ---- T1 / T2: same-row semantics -------------------------------------------

test_that("T1: same-dataset AND keeps only subjects with ONE matching record", {
  ## S01 has ASPIRIN on a record flagged N and IBUPROFEN on a record flagged Y.
  ## No conmed record is both, so S01 must not qualify. An atom-wise plan --
  ## subjects with ASPIRIN, intersected with subjects flagged Y -- would keep
  ## S01, which is the bug this design exists to avoid.
  kept <- .rp_kept(.rp_and(.rp_cond("ADCM", "CMDECOD", "ASPIRIN"),
                           .rp_cond("ADCM", "CONTRTFL", "Y")))
  expect_false("S01" %in% kept)
})

test_that("T2: positive control -- one record satisfying both qualifies", {
  ## S02's single ADCM record is ASPIRIN and flagged Y.
  kept <- .rp_kept(.rp_and(.rp_cond("ADCM", "CMDECOD", "ASPIRIN"),
                           .rp_cond("ADCM", "CONTRTFL", "Y")))
  expect_equal(kept, "S02")
})

# ---- T3 / T4: coherence inside a larger expression --------------------------

test_that("T3: same-dataset coherence survives inside a cross-dataset OR", {
  ## (ADCM.A AND ADCM.B) OR ADSL.SAFFL='Y'. The ADCM half must stay row-wise
  ## (so S01 does not qualify through it), while the ADSL half admits every
  ## safety subject.
  kept <- .rp_kept(.rp_or(
    .rp_and(.rp_cond("ADCM", "CMDECOD", "ASPIRIN"),
            .rp_cond("ADCM", "CONTRTFL", "Y")),
    .rp_cond("ADSL", "SAFFL", "Y")))

  ## S01-S03 are SAFFL='Y' so they enter through the OR's right side; S04 is
  ## not, and has no qualifying conmed record.
  expect_setequal(kept, c("S01", "S02", "S03"))
  expect_false("S04" %in% kept)
})

test_that("T4: AND regrouping reunites same-dataset siblings", {
  ## (ADCM.A AND ADSL.SAFFL) AND ADCM.B. The two ADCM predicates start in
  ## different subtrees; associativity and commutativity put them back
  ## together, so S01 is still judged record by record.
  kept <- .rp_kept(.rp_and(
    .rp_and(.rp_cond("ADCM", "CMDECOD", "ASPIRIN"),
            .rp_cond("ADSL", "SAFFL", "Y")),
    .rp_cond("ADCM", "CONTRTFL", "Y")))
  expect_equal(kept, "S02")
})

test_that("regrouping never crosses an OR boundary", {
  ## Directly on the planner: the ADCM predicates on either side of an OR must
  ## remain two separate subject projections. If regrouping ever reached
  ## across the OR they would collapse into one row-wise subtree, silently
  ## changing the question.
  plan <- .where_restriction_plan(
    .rp_or(.rp_cond("ADCM", "CMDECOD", "ASPIRIN"),
           .rp_cond("ADCM", "CONTRTFL", "Y")),
    "ADAE", "USUBJID")

  ## Both sides name ADCM only, so the whole OR is ONE maximal single-dataset
  ## subtree -- row-wise on ADCM, which for OR is equivalent either way.
  expect_true(plan$ok)
  expect_equal(plan$node$kind, "subject")
  expect_equal(plan$node$dataset, "ADCM")

  ## But mixed with a foreign sibling the OR must NOT be regrouped into the
  ## AND's ADCM group.
  mixed <- .where_restriction_plan(
    .rp_and(.rp_or(.rp_cond("ADCM", "CMDECOD", "ASPIRIN"),
                   .rp_cond("ADSL", "SAFFL", "Y")),
            .rp_cond("ADCM", "CONTRTFL", "Y")),
    "ADAE", "USUBJID")
  expect_false(mixed$ok)
})

# ---- T5: unsupported rather than guessed ------------------------------------

test_that("T5: an expression whose row coherence cannot be recovered fails closed", {
  ## (ADCM.A OR ADSL.S) AND ADCM.B. Recovering coherence between the two ADCM
  ## predicates would need distribution, and distribution changes the answer:
  ## a subject with A on one record and B on another qualifies under one
  ## reading and not the other. The expression does not say which is meant.
  where <- .rp_and(
    .rp_or(.rp_cond("ADCM", "CMDECOD", "ASPIRIN"),
           .rp_cond("ADSL", "SAFFL", "Y")),
    .rp_cond("ADCM", "CONTRTFL", "Y"))

  plan <- .where_restriction_plan(where, "ADAE", "USUBJID")
  expect_false(plan$ok)
  expect_equal(plan$reason, "ambiguous_row_coherence")
  expect_match(plan$detail, "ADCM", fixed = TRUE)

  ## The executor computes no mask ...
  diag_reset()
  expect_null(.rp_kept(where))
  fails <- ars_diagnostics()
  fails <- fails[fails$severity == "FAIL", , drop = FALSE]
  expect_gt(nrow(fails), 0)
  expect_match(fails$problem[[1]], "cannot be planned", fixed = TRUE)

  ## ... and the emitter writes no code that could produce one by some other
  ## reading.
  expect_null(.apply_where_expr("ADAE", "ADAE", where, "USUBJID"))
})

# ---- T6 / T7: the operator is honoured across datasets ----------------------

test_that("T6: AND across two foreign datasets intersects subjects", {
  kept <- .rp_kept(.rp_and(.rp_cond("ADSL", "SAFFL", "Y"),
                           .rp_cond("ADCM", "CONTRTFL", "Y")))
  ## SAFFL='Y' is S01-S03; a Y-flagged conmed record belongs to S01, S02, S03.
  expect_setequal(kept, c("S01", "S02", "S03"))
})

test_that("T7: OR across two foreign datasets unions subjects", {
  ## The previous implementation intersected regardless of the operator, so an
  ## OR silently behaved as an AND.
  kept <- .rp_kept(.rp_or(.rp_cond("ADSL", "SAFFL", "N"),
                          .rp_cond("ADCM", "CONTRTFL", "Y")))
  ## SAFFL='N' is S04; Y-flagged conmeds belong to S01, S02, S03.
  expect_setequal(kept, c("S01", "S02", "S03", "S04"))
})

# ---- T8: the two halves agree ----------------------------------------------

test_that("T8: emitted filtering reproduces executed filtering", {
  ## The guarantee, asserted rather than trusted: build the mask through the
  ## executor, and through the predicate the emitter writes, and compare.
  td    <- .rp_adam()
  store <- .adam_store(td)
  frames <- list(ADAE = store$get("ADAE"), ADSL = store$get("ADSL"),
                 ADCM = store$get("ADCM"))

  cases <- list(
    same_row_and = .rp_and(.rp_cond("ADCM", "CMDECOD", "ASPIRIN"),
                           .rp_cond("ADCM", "CONTRTFL", "Y")),
    coherent_or  = .rp_or(.rp_and(.rp_cond("ADCM", "CMDECOD", "ASPIRIN"),
                                  .rp_cond("ADCM", "CONTRTFL", "Y")),
                          .rp_cond("ADSL", "SAFFL", "Y")),
    regrouped    = .rp_and(.rp_and(.rp_cond("ADCM", "CMDECOD", "ASPIRIN"),
                                   .rp_cond("ADSL", "SAFFL", "Y")),
                           .rp_cond("ADCM", "CONTRTFL", "Y")),
    two_foreign_and = .rp_and(.rp_cond("ADSL", "SAFFL", "Y"),
                              .rp_cond("ADCM", "CONTRTFL", "Y")),
    two_foreign_or  = .rp_or(.rp_cond("ADSL", "SAFFL", "N"),
                             .rp_cond("ADCM", "CONTRTFL", "Y")),
    target_row      = .rp_cond("ADAE", "AEDECOD", "Headache")
  )

  for (name in names(cases)) {
    where <- cases[[name]]
    executed <- .where_keep_mask(frames$ADAE, "ADAE", where, store, "USUBJID")

    plan <- .where_restriction_plan(where, "ADAE", "USUBJID")
    expect_true(plan$ok, info = name)
    predicate <- .plan_pred_expr(plan$node, "ADAE", "USUBJID")
    emitted <- eval(parse(text = predicate),
                    envir = c(as.list(frames$ADAE), frames))

    expect_equal(executed, emitted, info = name)
  }
})

# ---- normalisation is idempotent -------------------------------------------

test_that("planning an already-planned shape returns the same plan", {
  ## The planner normalises (flatten nested ANDs, regroup same-dataset
  ## siblings). Running it over an expression that is already in that shape
  ## must not keep rewriting -- otherwise the plan a reader inspects and the
  ## plan that runs could differ by a round.
  nested <- .rp_and(.rp_and(.rp_cond("ADCM", "CMDECOD", "ASPIRIN"),
                            .rp_cond("ADSL", "SAFFL", "Y")),
                    .rp_cond("ADCM", "CONTRTFL", "Y"))
  flat <- .rp_and(.rp_and(.rp_cond("ADCM", "CMDECOD", "ASPIRIN"),
                          .rp_cond("ADCM", "CONTRTFL", "Y")),
                  .rp_cond("ADSL", "SAFFL", "Y"))

  first  <- .where_restriction_plan(nested, "ADAE", "USUBJID")
  again  <- .where_restriction_plan(flat,   "ADAE", "USUBJID")

  ## Same datasets projected, same shape, same order.
  expect_equal(.plan_subject_datasets(first$node),
               .plan_subject_datasets(again$node))
  expect_equal(first$node$kind, again$node$kind)
  expect_equal(length(first$node$children), length(again$node$children))
})
