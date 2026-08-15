## Where a grouping's variable lives, and what that means for the denominator.
##
## Two rules, and everything here is one of them:
##
##   POPULATION-FIRST. If the denominator frame already carries the grouping
##   variable it is authoritative, whatever the grouping metadata names as its
##   dataset. A treatment variable copied onto an event domain must not decide
##   the denominator, because subjects with no event would drop out of it --
##   and the denominator is the POPULATION, not the subjects some domain
##   happens to know about.
##
##   FAIL CLOSED. Once the population frame CANNOT supply the variable, the
##   foreign dataset is the only source of group membership. A subject with two
##   values, or a population subject the domain does not know, cannot be placed
##   in any group, so any per-group N would be partly invented. Refused, not
##   approximated.
##
## The bug that made this urgent: the converter writes the FLAT
## `groupingDataset`, while the execution side read only the NESTED
## `groupingVariable$dataset`. Everything arsbridge produced therefore resolved
## to "no dataset", the denominator join never fired, and an AE table whose
## columns were annotated ADAE.TRTA reported every percentage out of the whole
## study on the emitted path while the executor got it right.

skip_if_not_installed("cards")
skip_if_not_installed("withr")

# ---- the canonical resolver -------------------------------------------------

.gd_gf <- function(flat = NULL, nested = NULL) {
  gf <- list(id = "GF", name = "TRT01A")
  if (!is.null(flat)) gf$groupingDataset <- flat
  gf$groupingVariable <- if (is.null(nested)) "TRT01A" else
    list(dataset = nested, variable = "TRT01A")
  gf
}

test_that("flat-only, nested-only and both-equal resolve identically", {
  ## The converter and the editor write flat; a spec-correct ARS from
  ## elsewhere carries nested. Neither may be invisible.
  for (gf in list(.gd_gf(flat = "ADAE"),
                  .gd_gf(nested = "ADAE"),
                  .gd_gf(flat = "ADAE", nested = "ADAE"),
                  .gd_gf(flat = "adae", nested = "ADAE"))) {
    got <- .grouping_dataset(gf)
    expect_equal(got$dataset, "ADAE")
    expect_false(got$conflict)
  }
  ## Neither form present is simply "no dataset", not a conflict.
  none <- .grouping_dataset(list(id = "GF", groupingVariable = "TRT01A"))
  expect_true(is.na(none$dataset))
  expect_false(none$conflict)
})

test_that("flat and nested disagreement is rejected, not resolved", {
  ## Picking either could move a denominator, which is the whole failure this
  ## area exists to prevent -- so neither wins.
  got <- .grouping_dataset(.gd_gf(flat = "ADAE", nested = "ADSL"))
  expect_true(got$conflict)
  expect_true(is.na(got$dataset))
  expect_equal(got$flat, "ADAE")
  expect_equal(got$nested, "ADSL")
})

# ---- the fixture ------------------------------------------------------------

## `same_name`  TRUE  -> the domain carries TRT01A, as ADSL does
##              FALSE -> the domain carries TRTA, which ADSL lacks
## `disagree`   one subject's domain value contradicts ADSL
## `covered`    every population subject has a domain record
## `one_each`   at most one distinct domain value per subject
.gd_adam <- function(same_name = TRUE, disagree = FALSE, covered = TRUE,
                     one_each = TRUE, envir = parent.frame()) {
  td <- withr::local_tempdir(.local_envir = envir)
  utils::write.csv(data.frame(
    USUBJID = sprintf("S%02d", 1:6),
    TRT01A  = rep(c("Drug A", "Placebo"), each = 3),
    SAFFL   = "Y", stringsAsFactors = FALSE
  ), file.path(td, "adsl.csv"), row.names = FALSE)

  subj <- sprintf("S%02d", 1:6)
  arm  <- rep(c("Drug A", "Placebo"), each = 3)
  if (!covered) { keep <- 1:4; subj <- subj[keep]; arm <- arm[keep] }
  if (disagree) arm[[2]] <- "Placebo"          # S02 is Drug A in ADSL
  dom <- data.frame(USUBJID = subj, ARMCOL = arm,
                    AEDECOD = c("Headache", "Headache", "Nausea",
                                "Headache", "Nausea", "Nausea")[seq_along(subj)],
                    stringsAsFactors = FALSE)
  if (!one_each) {
    dom <- rbind(dom, data.frame(USUBJID = "S01", ARMCOL = "Placebo",
                                 AEDECOD = "Rash", stringsAsFactors = FALSE))
  }
  names(dom)[names(dom) == "ARMCOL"] <- if (same_name) "TRT01A" else "TRTA"
  utils::write.csv(dom, file.path(td, "adae.csv"), row.names = FALSE)
  td
}

## `flat`/`nested` choose how the grouping declares its dataset.
.gd_spec <- function(var, flat = "ADAE", nested = NULL) {
  gf <- list(id = "GF_G", name = var, dataDriven = TRUE)
  if (!is.null(flat)) gf$groupingDataset <- flat
  gf$groupingVariable <- if (is.null(nested)) var else
    list(dataset = nested, variable = var)
  list(
    id = "GD", name = "GD", version = "1",
    analysisSets = list(list(id = "AS", name = "Safety",
      condition = list(dataset = "ADSL", variable = "SAFFL",
                       comparator = "EQ", value = list("Y")))),
    dataSubsets = list(),
    analysisGroupings = list(gf),
    methods = list(list(id = "MTH_SUBJECT_COUNT_PCT", name = "n (%)")),
    analyses = list(list(id = "AN_G", methodId = "MTH_SUBJECT_COUNT_PCT",
      analysisSetId = "AS",
      analysisVariable = list(dataset = "ADAE", variable = "AEDECOD"),
      orderedGroupings = list(list(order = 1, groupingId = "GF_G",
                                   resultsByGroup = TRUE)))),
    outputs = list(list(id = "T_G", name = "T-G",
                        referencedAnalysisIds = list("AN_G"))))
}

.gd_run <- function(spec, dir, name, ...) {
  path <- file.path(dir, name)
  jsonlite::write_json(spec, path, auto_unbox = TRUE, null = "null")
  suppressMessages(suppressWarnings(ars_to_ard(path, dir, ...)))
}

.gd_stat <- function(ard, arm, sn, term = "Headache") {
  if (is.null(ard) || nrow(ard) == 0) return(NA_real_)
  flat <- function(col) vapply(ard[[col]], function(x)
    if (length(x)) as.character(x[[1]]) else NA_character_, character(1))
  i <- which(flat("group1_level") == arm & flat("variable_level") == term &
               ard$stat_name == sn)
  if (length(i) != 1L) return(NA_real_)
  as.numeric(ard$stat[[i]])
}

.gd_status <- function(ard, status) {
  if (is.null(ard) || !"result_status" %in% names(ard)) return(0L)
  st <- as.character(ard[["result_status"]])
  sum(!is.na(st) & st == status)
}

# ---- population-first -------------------------------------------------------

test_that("the population's own variable wins over the declared dataset", {
  ## ADSL and ADAE agree, both carry TRT01A. Declaring the grouping on ADAE
  ## must not change the answer: joining would collide the two columns into
  ## TRT01A.x / TRT01A.y and every N would become the whole study.
  td <- .gd_adam(same_name = TRUE)
  for (decl in list(list(flat = "ADSL"), list(flat = "ADAE"),
                    list(flat = NULL, nested = "ADAE"))) {
    ard <- .gd_run(do.call(.gd_spec, c(list(var = "TRT01A"), decl)), td,
                   paste0("pf_", decl$flat %||% "nested", ".json"))
    expect_equal(.gd_stat(ard, "Drug A", "N"), 3,
                 info = paste("declared:", decl$flat %||% decl$nested))
    expect_equal(.gd_stat(ard, "Placebo", "N"), 3)
    expect_equal(.gd_status(ard, "blocked"), 0L)
  }
})

test_that("a subject absent from the domain stays in the denominator", {
  ## The population answers for them. This is the case that MUST NOT block --
  ## it is what population-first is for.
  td  <- .gd_adam(same_name = TRUE, covered = FALSE)
  ard <- .gd_run(.gd_spec("TRT01A", flat = "ADAE"), td, "absent.json")

  expect_equal(.gd_status(ard, "blocked"), 0L)
  ## S05 and S06 have no ADAE record and are still counted in Placebo's N.
  expect_equal(.gd_stat(ard, "Placebo", "N"), 3)
})

# ---- carrying a variable the population lacks -------------------------------

test_that("a foreign grouping variable is carried back, and is correct", {
  ## ADSL has no TRTA, so the domain is the only source -- supported, when
  ## every subject resolves to exactly one value.
  td  <- .gd_adam(same_name = FALSE, covered = TRUE, one_each = TRUE)
  ard <- .gd_run(.gd_spec("TRTA", flat = "ADAE"), td, "foreign.json")

  expect_equal(.gd_status(ard, "blocked"), 0L)
  expect_equal(.gd_stat(ard, "Drug A", "N"), 3)
  expect_equal(.gd_stat(ard, "Placebo", "N"), 3)
  ## Not the whole study, which is what the flat-field bug produced.
  expect_false(isTRUE(all.equal(.gd_stat(ard, "Drug A", "N"), 6)))
})

test_that("multiple foreign values for one subject block", {
  td  <- .gd_adam(same_name = FALSE, one_each = FALSE)
  ard <- .gd_run(.gd_spec("TRTA", flat = "ADAE"), td, "multi.json")

  expect_equal(.gd_status(ard, "computed"), 0L)
  expect_equal(unique(as.character(ard[["block_reason"]])),
               "denominator_grouping_ambiguous")
})

test_that("a population subject the domain does not know blocks", {
  ## Distinct from the same-name case above: there the population answered.
  ## Here it cannot, so group membership is unknown.
  td  <- .gd_adam(same_name = FALSE, covered = FALSE)
  ard <- .gd_run(.gd_spec("TRTA", flat = "ADAE"), td, "uncovered.json")

  expect_equal(.gd_status(ard, "computed"), 0L)
  expect_equal(unique(as.character(ard[["block_reason"]])),
               "denominator_grouping_unresolved")
  expect_match(ars_blockers()$problem[[1]], "no resolvable TRTA value",
               fixed = TRUE)
})

# ---- same-name disagreement -------------------------------------------------

test_that("a population/domain value disagreement blocks, never n > N", {
  ## The numerator is grouped by the domain's copy and the denominator by the
  ## population's, so a disagreement counts a subject under one group and
  ## measures it against another -- the demonstrated n = 3 against N = 2.
  td <- .gd_adam(same_name = TRUE, disagree = TRUE)
  for (lg in c(FALSE, TRUE)) {
    ard <- .gd_run(.gd_spec("TRT01A", flat = "ADAE"), td,
                   paste0("disagree_", lg, ".json"), legacy = lg)
    expect_equal(.gd_status(ard, "computed"), 0L, info = paste("legacy", lg))
    expect_equal(unique(as.character(ard[["block_reason"]])),
                 "grouping_value_disagreement")
  }
})

# ---- executor / emitter equivalence ----------------------------------------

test_that("emitted and direct execution agree on n, N and p", {
  for (case in list(list(var = "TRT01A", same = TRUE),
                    list(var = "TRTA",   same = FALSE))) {
    td <- .gd_adam(same_name = case$same)
    spec <- .gd_spec(case$var, flat = "ADAE")
    a <- .gd_run(spec, td, paste0("eq_d_", case$var, ".json"), legacy = FALSE)
    b <- .gd_run(spec, td, paste0("eq_l_", case$var, ".json"), legacy = TRUE)
    for (arm in c("Drug A", "Placebo")) {
      for (sn in c("n", "N", "p")) {
        expect_equal(.gd_stat(a, arm, sn), .gd_stat(b, arm, sn),
                     info = paste(case$var, arm, sn))
      }
    }
  }
})

# ---- the backstop -----------------------------------------------------------

## Not the fix -- the net under it. Whatever the grouping metadata says, a
## computed percentage must be a percentage.
.gd_expect_coherent <- function(ard, info = "") {
  if (is.null(ard) || !"result_status" %in% names(ard)) return(invisible())
  flat <- function(col) vapply(ard[[col]], function(x)
    if (length(x)) as.character(x[[1]]) else NA_character_, character(1))
  st <- as.character(ard[["result_status"]])
  keep <- !is.na(st) & st == "computed"
  if (!any(keep)) return(invisible())

  key <- paste(flat("analysis_id"), flat("group1_level"),
               flat("variable_level"))[keep]
  sn  <- as.character(ard$stat_name)[keep]
  val <- suppressWarnings(as.numeric(vapply(ard$stat[keep], function(x)
    if (length(x)) as.character(x[[1]]) else NA_character_, character(1))))

  for (k in unique(key)) {
    at <- key == k
    n <- val[at & sn == "n"]; N <- val[at & sn == "N"]; p <- val[at & sn == "p"]
    if (length(n) == 1 && length(N) == 1 && !is.na(n) && !is.na(N)) {
      expect_lte(n, N, label = paste("n <= N", info, k))
    }
    if (length(p) == 1 && !is.na(p)) {
      expect_gte(p, 0); expect_lte(p, 1)
    }
  }
  invisible()
}

test_that("no computed percentage exceeds its denominator", {
  for (same in c(TRUE, FALSE)) {
    td <- .gd_adam(same_name = same)
    var <- if (same) "TRT01A" else "TRTA"
    for (decl in c("ADSL", "ADAE")) {
      ard <- .gd_run(.gd_spec(var, flat = decl), td,
                     paste0("bs_", var, "_", decl, ".json"))
      .gd_expect_coherent(ard, info = paste(var, decl))
    }
  }
})

# ---- flat / nested conflict -------------------------------------------------
#
# Detecting the conflict is not enough: if the run continued it would have
# silently chosen one representation, and which one decides whether the
# denominator is joined from a domain. Both surfaces refuse it.

.gd_conflict_spec <- function() {
  spec <- .gd_spec("TRT01A", flat = "ADAE")
  ## Same grouping, two datasets: flat says ADAE, nested says ADSL.
  spec$analysisGroupings[[1]]$groupingVariable <-
    list(dataset = "ADSL", variable = "TRT01A")
  spec
}

test_that("validate_ars_model() raises a GAP on the conflict", {
  td <- .gd_adam(same_name = TRUE)
  path <- file.path(td, "conflict_v.json")
  jsonlite::write_json(.gd_conflict_spec(), path, auto_unbox = TRUE,
                       null = "null")

  model <- ars_to_model(path)
  findings <- validate_ars_model(model)
  hit <- findings[findings$field == "groupingDataset", , drop = FALSE]

  expect_equal(nrow(hit), 1L)
  expect_equal(hit$severity[[1]], "GAP")
  expect_match(hit$problem[[1]], "groupingDataset says ADAE", fixed = TRUE)
  expect_match(hit$problem[[1]], "groupingVariable.dataset says ADSL",
               fixed = TRUE)
  ## FAIL is what the execution gate refuses on.
  expect_true(arsbridge:::.model_validation_gate(model)$blocked)
})

test_that("a conflicted grouping refuses the event on BOTH paths", {
  ## Not "the default path picks flat and legacy picks nested" -- that would be
  ## two engines answering a question the spec does not settle.
  ##
  ## The refusal is the STRUCTURAL gate, not a per-analysis blocked row: a
  ## grouping that contradicts itself is a blocking validation FAIL, and
  ## .assert_runnable_ars() refuses to execute an event with one. That is the
  ## existing contract for structural findings and is strictly more
  ## fail-closed than reserving cells -- nothing is computed at all.
  td <- .gd_adam(same_name = TRUE)
  for (lg in c(FALSE, TRUE)) {
    path <- file.path(td, paste0("conflict_", lg, ".json"))
    jsonlite::write_json(.gd_conflict_spec(), path, auto_unbox = TRUE,
                         null = "null")
    expect_error(
      suppressMessages(suppressWarnings(ars_to_ard(path, td, legacy = lg))),
      "structural validation failed", info = paste("legacy", lg))
  }
})

test_that("the conflict names the grouping and both datasets", {
  ## So the spec can be repaired without guessing which field to trust.
  td <- .gd_adam(same_name = TRUE)
  path <- file.path(td, "conflict_map.json")
  jsonlite::write_json(.gd_conflict_spec(), path, auto_unbox = TRUE,
                       null = "null")

  gate <- arsbridge:::.model_validation_gate(ars_to_model(path))
  hit <- gate$blocking_findings
  hit <- hit[hit$field == "groupingDataset", , drop = FALSE]

  expect_equal(nrow(hit), 1L)
  expect_equal(hit$id[[1]], "GF_G")
  expect_match(hit$problem[[1]], "ADAE", fixed = TRUE)
  expect_match(hit$problem[[1]], "ADSL", fixed = TRUE)
})

test_that("agreeing metadata still resolves and computes normally", {
  ## The other side of the conflict rule: agreement in either form, or in both
  ## differing only by case, is not a conflict and must not block.
  td <- .gd_adam(same_name = TRUE)
  spec <- .gd_spec("TRT01A", flat = "adae")
  spec$analysisGroupings[[1]]$groupingVariable <-
    list(dataset = "ADAE", variable = "TRT01A")

  ard <- .gd_run(spec, td, "agree_case.json")
  expect_equal(.gd_status(ard, "blocked"), 0L)
  expect_equal(.gd_stat(ard, "Drug A", "N"), 3)
})
