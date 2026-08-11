## Entity IDENTITY: what makes two AnalysisSets, or two DataSubsets, the same.
##
## Both ids are minted from a name or a tag rather than from the definition,
## the way grouping ids used to be (see test-grouping_identity.R). An analysis
## set is named after the population TEXT while its condition comes from the
## annotation, so two tables that both say "Safety Population" and filter
## differently collided: the first definition won and every later analysis ran
## the wrong population -- invisibly, because the output still fills.
##
## The rule these tests pin: entities are deduplicated by DEFINITION. Same
## definition -> one shared entity. Same name, different definition -> both
## survive, the first under the bare id.

.ei_section <- function(tlf, population_text = "Safety Population",
                        population_annot = "ADSL.SAFFL='Y'",
                        row_annotation = "ADSL.SEX") {
  list(
    tlf_number       = tlf,
    tlf_type         = "TABLE",
    title            = paste("Table", tlf),
    population_text  = population_text,
    population_annot = population_annot,
    analysis_type    = "CATEGORICAL",
    ars_method_name  = "Count and Percentage",
    by_variable      = "TRT01A",
    by_variable_dataset = "ADSL",
    source_format    = "docx",
    col_headers      = c("Characteristic", "Drug", "Placebo"),
    stub_rows        = list(list(label = "Sex", annotation = row_annotation,
                                 has_annot = TRUE)),
    enriched_rows    = list(list(label = "Sex", primary_dataset = "ADSL",
                                 primary_variable = "SEX",
                                 data_subset = NULL,
                                 variable_role = "ANALYSIS"))
  )
}

## A section whose single row is restricted by a data subset.
.ei_subset_section <- function(tlf, comparator, values) {
  section <- .ei_section(tlf)
  section$enriched_rows <- list(list(
    label = "Any AE", primary_dataset = "ADAE", primary_variable = "AEDECOD",
    variable_role = "ANALYSIS",
    data_subset = list(dataset = "ADAE", variable = "AESEV",
                       comparator = comparator, value = values)
  ))
  section$stub_rows <- list(list(label = "Any AE", annotation = "ADAE.AEDECOD",
                                 has_annot = TRUE))
  section
}

.ei_set <- function(re, id) {
  Filter(function(s) identical(s$id, id), re$analysisSets)[[1]]
}

## The analysis set each output's analyses actually reference.
.ei_referenced_set <- function(re, tlf) {
  prefix <- paste0("AN_", gsub("-", "_", tlf))
  analysis <- Filter(function(a) startsWith(a$id, prefix), re$analyses)[[1]]
  analysis$analysisSetId
}

.ei_referenced_subset <- function(re, tlf) {
  prefix <- paste0("AN_", gsub("-", "_", tlf))
  analysis <- Filter(function(a) startsWith(a$id, prefix), re$analyses)[[1]]
  analysis$dataSubsetId
}

.ei_values <- function(node) {
  as.character(unlist(node$condition$value %||% list()))
}


test_that("one population name over two definitions keeps both", {
  re <- build_ars_json(list(
    .ei_section("T-14-1-1", population_annot = "ADSL.SAFFL='Y'"),
    .ei_section("T-14-3-1",
                population_annot = "ADSL.SAFFL='Y' AND ADSL.ITTFL='Y'")
  ), study_id = "S1")

  expect_length(re$analysisSets, 2)

  first  <- .ei_referenced_set(re, "T-14-1-1")
  second <- .ei_referenced_set(re, "T-14-3-1")
  ## The first definition keeps the bare id, so a single-definition study is
  ## byte-identical to what the builder emitted before.
  expect_equal(first, "AS_SAFETY_POPULATION")
  expect_false(identical(first, second))

  ## Each output runs the population its own shell declared: the narrower one
  ## is a compound expression, the first is not.
  expect_null(.ei_set(re, first)$compoundExpression)
  expect_false(is.null(.ei_set(re, second)$compoundExpression))
})

test_that("one population definition written twice is a single set", {
  re <- build_ars_json(list(
    .ei_section("T-14-1-1"),
    .ei_section("T-14-1-2")
  ), study_id = "S1")

  expect_length(re$analysisSets, 1)
  expect_equal(.ei_referenced_set(re, "T-14-1-1"), "AS_SAFETY_POPULATION")
  expect_equal(.ei_referenced_set(re, "T-14-1-2"), "AS_SAFETY_POPULATION")
})

test_that("one population condition spelled two ways is a single set", {
  ## Canonicalized, so value order is not part of the definition.
  re <- build_ars_json(list(
    .ei_section("T-14-1-1", population_annot = "ADSL.COHORTN IN (1,2)"),
    .ei_section("T-14-1-2", population_annot = "ADSL.COHORTN IN (2,1)")
  ), study_id = "S1")

  expect_length(re$analysisSets, 1)
  expect_equal(.ei_referenced_set(re, "T-14-1-1"),
               .ei_referenced_set(re, "T-14-1-2"))
})

test_that("an annotated population upgrades an unannotated namesake", {
  ## The common shell shape: one table spells the filter out, another names the
  ## same population and leaves it implied. Splitting them would be wrong, and
  ## so would letting the unfiltered one win -- every later analysis would then
  ## run on the full population, with no sign of it in the output.
  re <- build_ars_json(list(
    .ei_section("T-14-1-1", population_annot = ""),
    .ei_section("T-14-1-2", population_annot = "ADSL.SAFFL='Y'")
  ), study_id = "S1")

  expect_length(re$analysisSets, 1)
  set <- .ei_set(re, .ei_referenced_set(re, "T-14-1-1"))
  expect_equal(set$condition$variable, "SAFFL")
  expect_equal(.ei_referenced_set(re, "T-14-1-1"),
               .ei_referenced_set(re, "T-14-1-2"))
})

test_that("two subsets on one variable and value keep their own comparators", {
  ## The tag carries the first value and no comparator, so these two collide
  ## on id: DS_ADAE_AESEV_SEVERE for both.
  re <- build_ars_json(list(
    .ei_subset_section("T-14-3-1", "EQ", list("SEVERE")),
    .ei_subset_section("T-14-3-2", "NE", list("SEVERE"))
  ), study_id = "S1")

  expect_length(re$dataSubsets, 2)

  first  <- .ei_referenced_subset(re, "T-14-3-1")
  second <- .ei_referenced_subset(re, "T-14-3-2")
  expect_equal(first, "DS_ADAE_AESEV_SEVERE")
  expect_false(identical(first, second))

  comparators <- vapply(re$dataSubsets, function(d) d$condition$comparator,
                        character(1))
  expect_setequal(comparators, c("EQ", "NE"))
})

test_that("two subsets sharing a first value keep their own value lists", {
  re <- build_ars_json(list(
    .ei_subset_section("T-14-3-1", "IN", list("SEVERE")),
    .ei_subset_section("T-14-3-2", "IN", list("SEVERE", "MODERATE"))
  ), study_id = "S1")

  expect_length(re$dataSubsets, 2)
  values <- lapply(re$dataSubsets, .ei_values)
  expect_true(any(vapply(values, function(v) identical(v, "SEVERE"),
                         logical(1))))
  expect_true(any(vapply(values,
                         function(v) identical(v, c("SEVERE", "MODERATE")),
                         logical(1))))
})

test_that("one subset definition used by two outputs is a single subset", {
  re <- build_ars_json(list(
    .ei_subset_section("T-14-3-1", "EQ", list("SEVERE")),
    .ei_subset_section("T-14-3-2", "EQ", list("SEVERE"))
  ), study_id = "S1")

  expect_length(re$dataSubsets, 1)
  expect_equal(.ei_referenced_subset(re, "T-14-3-1"),
               .ei_referenced_subset(re, "T-14-3-2"))
})

test_that("nothing over-splits: one plain section keeps one of each", {
  ## The guard against a registrar that is too eager. A single ordinary
  ## section keeps exactly one analysis set and the one standing "no filter"
  ## subset the builder always emits, each stamped with its order and level.
  re <- build_ars_json(list(.ei_section("T-14-1-1")), study_id = "S1")

  expect_length(re$analysisSets, 1)
  expect_equal(re$analysisSets[[1]]$id, "AS_SAFETY_POPULATION")
  expect_equal(re$analysisSets[[1]]$order, 1L)
  expect_equal(re$analysisSets[[1]]$level, 1L)

  expect_length(re$dataSubsets, 1)
  expect_equal(re$dataSubsets[[1]]$id, "DS_ALL")
  expect_equal(re$dataSubsets[[1]]$order, 1L)
  expect_equal(re$dataSubsets[[1]]$level, 1L)
})
