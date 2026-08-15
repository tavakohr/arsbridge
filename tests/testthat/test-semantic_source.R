# An analysis may only compute from data its own annotation named.
#
# The failure this detects has no visible symptom. A row inherits another row's
# variable, computes a well-formed number from the wrong column, and is marked
# filled. Where the two columns happen to agree in a data cut -- STRAT2 and
# AGEGR1 do, in the study below -- the output is numerically correct and the
# analysis is still wrong. So none of these tests use numeric agreement as
# evidence: they assert the semantic source directly.
#
# Scope: this file tests the DETECTOR. It does not fix anything, and the
# violations it pins are expected to be present until the build-stage repair
# lands.
#
# The tests are layered, and the order matters:
#
#   1. the rule in isolation      -- structural, no study vocabulary
#   2. the general contract       -- invented identifiers, plus a renaming
#                                    that must not change any verdict
#   3. one real study             -- acceptance: a clean event stays clean
#   4. a second, different study  -- regression: known defects stay pinned
#
# Layers 3 and 4 are evidence that the rule survives contact with real shells.
# They are not the definition of correctness -- layers 1 and 2 are. A study's
# dataset and variable names appear only below this line, never in the package.

## ---- the rule, in isolation -------------------------------------------------

test_that("annotation references survive the conditions the parser cannot read", {
  ## The detector must not reuse the where-clause parser, or it goes blind in
  ## exactly the cases it polices. These two annotations are the known A-06 and
  ## A-07 breakages; both must still DECLARE their source.
  expect_equal(.annotation_source_refs("ADSL.RACE='BLACK OR AFRICAN AMERICAN'"),
               "ADSL.RACE")
  expect_equal(.annotation_source_refs("ADSL.STRAT2='Adolescent (<18)'"),
               "ADSL.STRAT2")
  expect_equal(.annotation_source_refs("ADEX.TRTDUR >= 16"), "ADEX.TRTDUR")

  ## Confirm the premise: the condition parser genuinely fails on these.
  expect_null(parse_where_clause("ADSL.RACE='BLACK OR AFRICAN AMERICAN'"))
  expect_null(parse_where_clause("ADEX.TRTDUR >= 16"))

  ## Unqualified text declares nothing -- it is not checkable, not clean.
  expect_length(.annotation_source_refs("no qualified reference here"), 0L)
  expect_length(.annotation_source_refs(""), 0L)
  expect_length(.annotation_source_refs(NULL), 0L)
})

test_that("a where clause declares every source it restricts on, to the leaves", {
  leaf <- list(condition = list(dataset = "ADAE", variable = "TRTEMFL",
                                comparator = "EQ", value = list("Y")))
  expect_equal(.where_source_pairs(leaf), "ADAE.TRTEMFL")

  compound <- list(compoundExpression = list(logicalOperator = "AND",
    whereClauses = list(
      leaf,
      list(condition = list(dataset = "ADAE", variable = "AESEV",
                            comparator = "EQ", value = list("SEVERE"))),
      list(compoundExpression = list(logicalOperator = "OR", whereClauses = list(
        list(condition = list(dataset = "ADSL", variable = "SAFFL",
                              comparator = "EQ", value = list("Y")))))))))
  expect_setequal(.where_source_pairs(compound),
                  c("ADAE.TRTEMFL", "ADAE.AESEV", "ADSL.SAFFL"))
  expect_length(.where_source_pairs(NULL), 0L)
})

test_that("the comparison is on qualified pairs, not bare variable names", {
  ## ADSL.AGE and ADAE.AGE are different sources. A bare-name check would let
  ## that substitution through.
  expect_false(.source_is_declared("ADAE.AGE", "ADSL.AGE"))
  expect_true(.source_is_declared("ADSL.AGE", c("ADSL.AGE", "ADSL.AGEU")))
  ## A resolved pair carrying no dataset degrades to the variable alone rather
  ## than manufacturing a mismatch from an absent field.
  expect_true(.source_is_declared("AGE", "ADSL.AGE"))
  expect_false(.source_is_declared("SEX", "ADSL.AGE"))
})

test_that("containment is one-way: the annotation may name more than is used", {
  ## `ADSL.AGE; unit ADSL.AGEU='YEARS'` analyses AGE and never uses AGEU. That
  ## is not a finding -- only the opposite direction is.
  model <- .ssrc_model(list(
    .ssrc_analysis("AN_1", "ADSL", "AGE", "ADSL.AGE; unit ADSL.AGEU='YEARS'")
  ))
  expect_equal(nrow(.ssrc_hits(model)), 0L)
})

test_that("analysing one variable while restricting on another is not a finding", {
  ## The legitimate cross-variable form. 70 analyses across the two studies are
  ## of this shape; an equality rule would condemn every one.
  model <- .ssrc_model(
    analyses = list(.ssrc_analysis("AN_1", "ADAE", "AESOC",
                                   "ADAE.AESOC WHERE ADAE.TRTEMFL='Y'", "DS_1")),
    subsets = list(.ssrc_subset("DS_1", "ADAE", "TRTEMFL", "Y"))
  )
  expect_equal(nrow(.ssrc_hits(model)), 0L)
})

test_that("a source the annotation never named is a GAP", {
  ## A-11 in miniature: the annotation says ETHNICN, the analysis resolves RACEN.
  model <- .ssrc_model(
    analyses = list(.ssrc_analysis("AN_BAD", "ADSL", "RACEN",
                                   "ADSL.ETHNICN=3", "DS_BAD")),
    subsets = list(.ssrc_subset("DS_BAD", "ADSL", "RACEN", "9"))
  )
  hits <- .ssrc_hits(model)
  expect_equal(nrow(hits), 1L)
  expect_equal(hits$severity, "GAP")
  expect_equal(hits$id, "AN_BAD")
  ## The message has to be actionable without looking at any number.
  expect_match(hits$problem, "ADSL.RACEN", fixed = TRUE)
  expect_match(hits$problem, "ADSL.ETHNICN", fixed = TRUE)
  expect_match(hits$problem, "ADSL.ETHNICN=3", fixed = TRUE)
})

test_that("absent and uninterpretable annotations are counted apart", {
  ## The two must never collapse. "No annotation" means this check does not
  ## apply to the row; "an annotation I cannot read" means the row made a
  ## source claim that could not be settled. Reporting the second as the first
  ## would quietly shrink the unexamined surface -- so this asserts both
  ## directions at once, on models identical but for the annotation.
  absent <- ssrc_analysis("AN_1", "ADQX", "MEASURE", NULL)
  absent$annotation <- NULL
  absent_scope <- .semantic_source_scope(ssrc_model(list(absent)))

  ## Present, non-empty, and carrying no qualified reference the grammar can
  ## read -- prose, and a bare variable with no dataset to qualify it.
  for (text in c("summarised per the SAP", "MEASURE", "see footnote 2")) {
    present_scope <- .semantic_source_scope(
      ssrc_model(list(ssrc_analysis("AN_1", "ADQX", "MEASURE", text))))
    expect_equal(present_scope$not_checkable, 1L, info = text)
    expect_equal(present_scope$no_claim, 0L, info = text)
    expect_equal(present_scope$checkable, 0L, info = text)
  }

  expect_equal(absent_scope$no_claim, 1L)
  expect_equal(absent_scope$not_checkable, 0L)
  expect_equal(absent_scope$checkable, 0L)

  ## Whitespace is absence, not an unreadable claim.
  blank_scope <- .semantic_source_scope(
    ssrc_model(list(ssrc_analysis("AN_1", "ADQX", "MEASURE", "   "))))
  expect_equal(blank_scope$no_claim, 1L)
  expect_equal(blank_scope$not_checkable, 0L)
})

test_that("an ARS with no annotation extension validates, and claims nothing", {
  ## `annotation` is an arsbridge extension. An ARS written by another tool has
  ## no such field and those analyses pool as NA, so this is the ordinary shape
  ## of a foreign file rather than an edge case. It must neither error nor be
  ## judged -- and it must not be tallied as an unsettled claim either, since
  ## the row never made one.
  bare <- ssrc_analysis("AN_1", "ADQX", "MEASURE", NULL)
  bare$annotation <- NULL
  model <- ssrc_model(list(bare))

  expect_true(is.na(model$analyses$annotation[[1]]))
  expect_length(.annotation_source_refs(model$analyses$annotation[[1]]), 0L)

  expect_equal(nrow(ssrc_hits(model)), 0L)
  scope <- .semantic_source_scope(model)
  expect_equal(scope$checkable, 0L)
  expect_equal(scope$not_checkable, 0L)
  expect_equal(scope$no_claim, 1L)
})

test_that("an unprovable annotation is counted, never judged", {
  model <- .ssrc_model(list(
    .ssrc_analysis("AN_1", "ADSL", "AGE", "age in years, per the SAP")
  ))
  expect_equal(nrow(.ssrc_hits(model)), 0L)
  scope <- .semantic_source_scope(model)
  expect_equal(scope$checkable, 0L)
  expect_equal(scope$not_checkable, 1L)
  expect_equal(scope$no_claim, 0L)
})

## ---- the general contract, on identifiers from no study ---------------------
##
## The rule relates two sets of qualified references, so it has to hold for
## names this package has never seen. Everything below is invented.
##
## Each case asserts `checkable` as well as the verdict, and that guard is not
## decoration: the annotation reader recognises references by an identifier
## grammar, and a name outside that grammar declares nothing. Such an analysis
## becomes unprovable rather than clean -- so without the guard, a grammar
## change would silently turn all four of these into tests of nothing, passing
## while proving no rule at all.

## Two vocabularies with nothing in common. Every case runs under both.
##   ds/var          -- the variable the row summarises
##   flag_ds/flag    -- the filter the annotation DECLARES
##   other_flag      -- a filter the analysis USES but never declared
##   other_ds/other  -- a variable the analysis USES but never declared
.SSRC_NAMES_A <- list(ds = "ADQX", var = "MEASURE",
                      flag_ds = "ADZZ", flag = "KEEPFL",
                      other_ds = "ADWK", other = "WRONGVAR",
                      other_flag = "OTHERFL")
.SSRC_NAMES_B <- list(ds = "ADVN", var = "TOPIC",
                      flag_ds = "ADRP", flag = "PICKFL",
                      other_ds = "ADHJ", other = "BADVAR",
                      other_flag = "SKIPFL")

.ssrc_qualify <- function(dataset, variable) paste0(dataset, ".", variable)

## The four shapes of the contract, each built from an identifier set alone.
.ssrc_case <- function(n, shape) {
  ## Declares the summarised variable and one filter.
  restricted <- sprintf("%s WHERE %s='Y'",
                        .ssrc_qualify(n$ds, n$var),
                        .ssrc_qualify(n$flag_ds, n$flag))
  switch(
    shape,
    ## Resolves exactly what it declared.
    plain_ok = list(
      analyses = list(ssrc_analysis("AN_1", n$ds, n$var,
                                    .ssrc_qualify(n$ds, n$var))),
      subsets = list()),
    ## Summarises one variable, restricts on another, declares both. The
    ## subset restricts on `flag`, which is exactly what was declared.
    restricted_ok = list(
      analyses = list(ssrc_analysis("AN_1", n$ds, n$var, restricted, "DS_1")),
      subsets = list(ssrc_subset("DS_1", n$flag_ds, n$flag, "Y"))),
    ## Computes `other`, having declared only `var`. Undeclared: other.
    wrong_variable = list(
      analyses = list(ssrc_analysis("AN_1", n$other_ds, n$other,
                                    .ssrc_qualify(n$ds, n$var))),
      subsets = list()),
    ## Declares filter `flag` but the subset restricts on `other_flag`
    ## instead. Undeclared: other_flag. The analysis variable is fine here;
    ## only the filter drifted.
    wrong_subset = list(
      analyses = list(ssrc_analysis("AN_1", n$ds, n$var, restricted, "DS_1")),
      subsets = list(ssrc_subset("DS_1", n$flag_ds, n$other_flag, "Y")))
  )
}

## The source each broken shape is expected to report as undeclared.
.ssrc_undeclared <- function(n, shape) {
  if (shape == "wrong_variable") {
    .ssrc_qualify(n$other_ds, n$other)
  } else {
    .ssrc_qualify(n$flag_ds, n$other_flag)
  }
}

.ssrc_verdict <- function(n, shape) {
  case <- .ssrc_case(n, shape)
  model <- ssrc_model(case$analyses, case$subsets)
  hits <- ssrc_hits(model)
  scope <- .semantic_source_scope(model)
  list(findings = nrow(hits),
       checkable = scope$checkable,
       not_checkable = scope$not_checkable,
       no_claim = scope$no_claim,
       problem = if (nrow(hits) > 0) hits$problem[[1]] else "")
}

.SSRC_CLEAN_SHAPES  <- c("plain_ok", "restricted_ok")
.SSRC_BROKEN_SHAPES <- c("wrong_variable", "wrong_subset")

test_that("the contract holds for identifiers from no study", {
  for (shape in .SSRC_CLEAN_SHAPES) {
    v <- .ssrc_verdict(.SSRC_NAMES_A, shape)
    expect_equal(v$findings, 0L, info = shape)
    ## Non-vacuous: the rule actually reached this analysis.
    expect_equal(v$checkable, 1L, info = shape)
    expect_equal(v$not_checkable, 0L, info = shape)
    expect_equal(v$no_claim, 0L, info = shape)
  }
  for (shape in .SSRC_BROKEN_SHAPES) {
    v <- .ssrc_verdict(.SSRC_NAMES_A, shape)
    expect_equal(v$findings, 1L, info = shape)
    expect_equal(v$checkable, 1L, info = shape)
    expect_equal(v$not_checkable, 0L, info = shape)
    expect_equal(v$no_claim, 0L, info = shape)
  }
})

test_that("a synthetic finding names the source that was not declared", {
  n <- .SSRC_NAMES_A
  for (shape in .SSRC_BROKEN_SHAPES) {
    v <- .ssrc_verdict(n, shape)
    ## The source that was used without being declared.
    expect_match(v$problem, .ssrc_undeclared(n, shape), fixed = TRUE,
                 info = shape)
    ## And what the annotation did declare, so the message is actionable.
    expect_match(v$problem, .ssrc_qualify(n$ds, n$var), fixed = TRUE,
                 info = shape)
  }
})

test_that("renaming every identifier changes no verdict", {
  ## Metamorphic: same structure, disjoint vocabulary, identical outcome. If
  ## any verdict moved, the rule would be reading names rather than relations.
  names_a <- unlist(.SSRC_NAMES_A, use.names = FALSE)
  names_b <- unlist(.SSRC_NAMES_B, use.names = FALSE)
  expect_length(intersect(names_a, names_b), 0L)

  for (shape in c(.SSRC_CLEAN_SHAPES, .SSRC_BROKEN_SHAPES)) {
    before <- .ssrc_verdict(.SSRC_NAMES_A, shape)
    after  <- .ssrc_verdict(.SSRC_NAMES_B, shape)
    expect_equal(after$findings, before$findings, info = shape)
    expect_equal(after$checkable, before$checkable, info = shape)
    expect_equal(after$not_checkable, before$not_checkable, info = shape)
    expect_equal(after$no_claim, before$no_claim, info = shape)
  }
})

test_that("a renamed failing case fails for the same structural reason", {
  ## Equal counts would still allow the rename to fail for a different cause,
  ## so this pins the roles: the undeclared source and the declared one, both
  ## renamed, and no trace of the vocabulary the case was written in.
  for (shape in .SSRC_BROKEN_SHAPES) {
    after <- .ssrc_verdict(.SSRC_NAMES_B, shape)
    expect_equal(after$findings, 1L, info = shape)
    expect_match(after$problem, .ssrc_undeclared(.SSRC_NAMES_B, shape),
                 fixed = TRUE, info = shape)
    expect_match(after$problem,
                 .ssrc_qualify(.SSRC_NAMES_B$ds, .SSRC_NAMES_B$var),
                 fixed = TRUE, info = shape)

    for (stale in unlist(.SSRC_NAMES_A, use.names = FALSE)) {
      expect_false(grepl(stale, after$problem, fixed = TRUE),
                   info = paste(shape, stale))
    }
  }
})

## ---- one real study, then a very different one ------------------------------
##
## Acceptance and regression. These carry study vocabulary; the rule does not.

test_that("CDSC-ALZ-201 declares every source it uses", {
  ## Study 1. Zero findings, and a non-zero checkable count so that zero means
  ## "checked and clean" rather than "nothing was looked at".
  skip_on_cran()
  skip_if_not_installed("openxlsx2")

  model <- .ssrc_study_model(
    shell = arsbridge_example("annotated_shell.xlsx"),
    spec  = arsbridge_example("adam_spec.xlsx"),
    tag   = "ALZ")
  scope <- .semantic_source_scope(model)
  expect_gt(scope$checkable, 0L)
  expect_equal(scope$not_checkable, 0L)
  expect_equal(nrow(.ssrc_hits(model)), 0L)
})

## The five rows this study exposed, and the source each one's OWN annotation
## declares. Keyed by sheet row, because that is an input: it survives a repair
## that renumbers analyses, and it cannot be produced by the code under test.
## (The generated ids did in fact renumber here -- removing one spurious
## analysis shifted every id after it -- so pinning ids would have re-created
## the very circularity these regressions exist to avoid.)
.SSRC_DRM_T1412_ROWS <- c(
  "35"  = "ADSL.ETHNICN",   # level of Ethnicity; had taken Race's variable
  "36"  = "ADSL.ETHNICN",   # the second victim in the same block
  "121" = "ADSL.STRAT2",    # randomisation stratum; had taken an age grouping
  "122" = "ADSL.STRAT2",
  "123" = "ADSL.STRAT2"
)
## The gated table has no layout to key on, so its row is anchored on the
## annotation text the shell wrote, which is equally an input.
.SSRC_DRM_T1421_ANNOTATION <- "ADEFF.PCHG75FL='Y'"
.SSRC_DRM_T1421_SOURCE     <- "ADEFF.PCHG75FL"

#' Assert every formerly-affected row now resolves the source it declared.
#' Shared by the deterministic and supplement-assisted runs so the two cannot
#' drift apart.
.ssrc_expect_drm_repaired <- function(art, label) {
  by_row <- ssrc_resolved_by_sheet_row(art$json, "14_1_2")
  ## Non-vacuous: the rows must actually be present to be judged.
  expect_true(all(names(.SSRC_DRM_T1412_ROWS) %in% names(by_row)),
              info = paste(label, "-- every affected sheet row is in the layout"))
  for (srow in names(.SSRC_DRM_T1412_ROWS)) {
    expect_equal(by_row[[srow]], .SSRC_DRM_T1412_ROWS[[srow]],
                 info = sprintf("%s -- sheet row %s", label, srow))
  }

  resolved <- ssrc_resolved_for_annotation(art$json, .SSRC_DRM_T1421_ANNOTATION)
  expect_length(resolved, 1L)
  expect_equal(unname(resolved), .SSRC_DRM_T1421_SOURCE, info = label)
}

test_that("APX-DRM-301 resolves the source every shell row declared", {
  ## Study 2, deterministic. This study exposed five rows whose ARS analysis
  ## computed from a variable the row's own annotation never named. The
  ## regression asserts the CORRECTED source of each one by name -- "no
  ## findings" alone would also be true of an event that had lost the rows
  ## altogether, or that stopped being checkable.
  skip_on_cran()
  skip_if_not_installed("openxlsx2")
  drm <- .ssrc_drm_inputs()
  skip_if(is.null(drm), "APX-DRM-301 study material is not available")

  art <- ssrc_study_artifacts(shell = drm$shell, spec = drm$spec, tag = "DRMdet")

  ## 1. Each formerly-affected row now carries its declared source.
  .ssrc_expect_drm_repaired(art, "deterministic")

  ## 2. And the detector, over the whole event, is clean -- with a non-zero
  ##    checkable count, so zero means "checked and clean".
  scope <- .semantic_source_scope(art$model)
  expect_gt(scope$checkable, 0L)
  expect_equal(scope$not_checkable, 0L)
  expect_equal(nrow(.ssrc_hits(art$model)), 0L)
})

test_that("the reviewed supplement keeps the sources the shell rows declared", {
  ## The supplement carries a typed whereClause for each of these rows and
  ## binds it by display label. It used to land on the same wrong rows; the
  ## repair has to hold on this path too, not only on the deterministic one.
  skip_on_cran()
  skip_if_not_installed("openxlsx2")
  drm <- .ssrc_drm_inputs()
  skip_if(is.null(drm), "APX-DRM-301 study material is not available")
  skip_if(!file.exists(drm$supplement), "supplement.json is not available")

  art <- ssrc_study_artifacts(shell = drm$shell, spec = drm$spec,
                              tag = "DRMsup", supplement = drm$supplement)

  .ssrc_expect_drm_repaired(art, "supplement")

  scope <- .semantic_source_scope(art$model)
  expect_gt(scope$checkable, 0L)
  expect_equal(scope$not_checkable, 0L)
  expect_equal(nrow(.ssrc_hits(art$model)), 0L)
})

test_that("A-15 stays invisible to numbers and is fixed in the ARS", {
  ## The whole reason this detector exists, and the regression that must
  ## outlive the repair. Two variables agree exactly in this data cut, so the
  ## printed table was correct while the analysis was wrong. The numeric
  ## coincidence is still there -- that is the point -- so nothing about the
  ## values can tell you the ARS is now right. Only the source can.
  skip_on_cran()
  skip_if_not_installed("openxlsx2")
  skip_if_not_installed("haven")
  drm <- .ssrc_drm_inputs()
  skip_if(is.null(drm), "APX-DRM-301 study material is not available")

  adsl <- haven::read_xpt(file.path(drm$adam, "adsl.xpt"))
  lvl <- "Adult (18-65)"
  by_strat <- sum(!is.na(adsl$STRAT2) & as.character(adsl$STRAT2) == lvl)
  by_age   <- sum(!is.na(adsl$AGEGR1) & as.character(adsl$AGEGR1) == lvl)

  ## The premise, unchanged by the repair: a numeric oracle cannot tell the
  ## intended variable from the substituted one here.
  expect_equal(by_strat, by_age)
  expect_gt(by_strat, 0L)

  ## The ARS now binds the variable the row declared, not the one that happens
  ## to agree with it. Sheet row 122 is the "Adult (18-65)" stratum row.
  art <- ssrc_study_artifacts(shell = drm$shell, spec = drm$spec, tag = "DRMa15")
  by_row <- ssrc_resolved_by_sheet_row(art$json, "14_1_2")
  expect_equal(by_row[["122"]], "ADSL.STRAT2")

  ## And the detector that first made this visible is satisfied.
  expect_equal(nrow(.ssrc_hits(art$model)), 0L)
})
