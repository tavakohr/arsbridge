## Grouping IDENTITY: what makes two GroupingFactors the same axis.
##
## A grouping id is minted from the variable name alone, so two outputs that
## group the SAME variable in DIFFERENT ways used to collide: the first
## definition won and the second output's analyses were silently re-pointed at
## it. The field symptom was a subgroup table whose Low/Medium/High columns
## stayed as placeholders while Total filled -- the ARD carried the earlier
## table's cohort levels, which no column header matched.
##
## The rule these tests pin: groupings are deduplicated by DEFINITION. Same
## definition -> one shared factor. Same variable, different definition -> both
## survive, the first under the bare id.

## One analysable section with a condition-defined column axis. `groups` is the
## shell's own shape (label + annotation string), so the parse path runs.
.gi_section <- function(tlf, variable, groups, dataset = "ADSL") {
  list(
    tlf_number       = tlf,
    tlf_type         = "TABLE",
    title            = paste("Table", tlf),
    population_text  = "Safety Population",
    population_annot = "ADSL.SAFFL='Y'",
    analysis_type    = "CATEGORICAL",
    ars_method_name  = "Count and Percentage",
    by_variable      = variable,
    by_variable_dataset = dataset,
    column_groups    = list(
      variable = variable,
      dataset  = dataset,
      groups   = groups
    ),
    stub_rows     = list(list(label = "Sex", annotation = "ADSL.SEX",
                              has_annot = TRUE)),
    enriched_rows = list(list(label = "Sex", primary_dataset = "ADSL",
                              primary_variable = "SEX", data_subset = NULL,
                              variable_role = "ANALYSIS"))
  )
}

.gi_group <- function(label, annotation, order) {
  list(label = label, annotation = annotation, order = order)
}

## The cohort axis of a demographics table.
.gi_cohort_groups <- function() {
  list(
    .gi_group("Cohort A", "ADSL.COHORTN=1", 1L),
    .gi_group("Cohort B", "ADSL.COHORTN=2", 2L)
  )
}

## A subgroup table's axis: every column shares the cohort restriction and
## varies a second variable. The FIRST variable referenced is COHORTN, so this
## axis is named after COHORTN too -- which is exactly how it used to collide
## with the demographics table above.
.gi_subgroup_groups <- function() {
  list(
    .gi_group("Low",    "ADSL.COHORTN=1 AND ADSL.CGHGR1N=1", 1L),
    .gi_group("Medium", "ADSL.COHORTN=1 AND ADSL.CGHGR1N=2", 2L),
    .gi_group("High",   "ADSL.COHORTN=1 AND ADSL.CGHGR1N=3", 3L)
  )
}

## The grouping each output's analyses actually reference.
.gi_referenced_id <- function(re, tlf) {
  analysis <- Filter(function(a) startsWith(a$id, paste0("AN_", gsub("-", "_", tlf))),
                     re$analyses)[[1]]
  analysis$orderedGroupings[[1]]$groupingId
}

.gi_grouping <- function(re, id) {
  Filter(function(g) identical(g$id, id), re$analysisGroupings)[[1]]
}

.gi_labels <- function(gf) {
  vapply(gf$groups, function(g) g$label, character(1))
}


test_that("two outputs grouping one variable differently each keep their own definition", {
  re <- build_ars_json(list(
    .gi_section("T-14-1-1", "COHORTN", .gi_cohort_groups()),
    .gi_section("T-14-3-2", "COHORTN", .gi_subgroup_groups())
  ), study_id = "S1")

  expect_length(re$analysisGroupings, 2)

  ## The first definition keeps the bare id, so a single-definition study is
  ## byte-identical to what the builder emitted before.
  first_id  <- .gi_referenced_id(re, "T-14-1-1")
  second_id <- .gi_referenced_id(re, "T-14-3-2")
  expect_equal(first_id, "GF_COHORTN")
  expect_false(identical(first_id, second_id))

  ## Each output's analyses resolve to the columns its own shell declared.
  expect_equal(.gi_labels(.gi_grouping(re, first_id)),
               c("Cohort A", "Cohort B"))
  expect_equal(.gi_labels(.gi_grouping(re, second_id)),
               c("Low", "Medium", "High"))
})

test_that("two outputs grouping one variable identically share one factor", {
  re <- build_ars_json(list(
    .gi_section("T-14-1-1", "COHORTN", .gi_cohort_groups()),
    .gi_section("T-14-1-2", "COHORTN", .gi_cohort_groups())
  ), study_id = "S1")

  expect_length(re$analysisGroupings, 1)
  expect_equal(.gi_referenced_id(re, "T-14-1-1"), "GF_COHORTN")
  expect_equal(.gi_referenced_id(re, "T-14-1-2"), "GF_COHORTN")
})

test_that("a reordered IN list is the same definition, not a new one", {
  re <- build_ars_json(list(
    .gi_section("T-14-1-1", "COHORTN",
                list(.gi_group("Either", "ADSL.COHORTN IN (1,2)", 1L))),
    .gi_section("T-14-1-2", "COHORTN",
                list(.gi_group("Either", "ADSL.COHORTN IN (2,1)", 1L)))
  ), study_id = "S1")

  expect_length(re$analysisGroupings, 1)
})

test_that("group ids stay unique when two definitions share a column label", {
  ## Both tables call a column "High", but they mean different subjects. The
  ## per-level group id is variable + label, so without re-minting, the two
  ## factors would carry the SAME group id -- and resolve_analysis() keeps one
  ## global group index, so one output's result path would resolve the other
  ## output's condition.
  re <- build_ars_json(list(
    .gi_section("T-14-1-1", "COHORTN",
                list(.gi_group("High", "ADSL.COHORTN=1", 1L))),
    .gi_section("T-14-3-2", "COHORTN",
                list(.gi_group("High", "ADSL.COHORTN=1 AND ADSL.CGHGR1N=3", 1L)))
  ), study_id = "S1")

  expect_length(re$analysisGroupings, 2)
  group_ids <- unlist(lapply(re$analysisGroupings, function(gf) {
    vapply(gf$groups, function(g) g$id, character(1))
  }))
  expect_equal(anyDuplicated(group_ids), 0L)
})

test_that("a data-driven row grouping does not absorb a column axis on the same variable", {
  ## A nested block's parent level is data-driven with no enumerated groups.
  ## It shares nothing with a condition-defined column axis beyond the variable
  ## name, so both must survive.
  column_axis <- .gi_section("T-14-1-1", "AESOC",
                             list(.gi_group("Any", "ADAE.AESOC not missing", 1L)))
  row_axis <- arsbridge:::.build_row_grouping("AESOC", "ADAE")

  re <- build_ars_json(list(column_axis), study_id = "S1")
  built <- re$analysisGroupings[[1]]

  expect_false(identical(
    arsbridge:::.grouping_signature(built),
    arsbridge:::.grouping_signature(row_axis)
  ))
})
