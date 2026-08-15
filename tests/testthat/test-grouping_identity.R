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
.gi_section <- function(tlf, variable, groups, dataset = "ADSL",
                        source_format = "docx") {
  group_labels <- vapply(groups, function(group) group$label, character(1))
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
    source_format    = source_format,
    col_headers      = c("Characteristic", group_labels),
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

test_that("grouping identity follows explicit order, not serialized list order", {
  grouping <- build_ars_json(list(
    .gi_section("T-14-1-1", "COHORTN", .gi_cohort_groups())
  ), study_id = "S1")$analysisGroupings[[1]]
  reordered <- grouping
  reordered$groups <- rev(reordered$groups)

  expect_identical(
    .grouping_signature(grouping),
    .grouping_signature(reordered)
  )
})

test_that("grouping identity preserves genuinely different display orders", {
  grouping <- build_ars_json(list(
    .gi_section("T-14-1-1", "COHORTN", .gi_cohort_groups())
  ), study_id = "S1")$analysisGroupings[[1]]
  changed_order <- grouping
  changed_order$groups[[1]]$order <- 2L
  changed_order$groups[[2]]$order <- 1L

  expect_false(identical(
    .grouping_signature(grouping),
    .grouping_signature(changed_order)
  ))
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

test_that("generated groupings with no fixed levels are data-driven", {
  empty_axis <- .gi_section("T-14-1-1", "TRT01A", list())
  empty_axis$col_headers <- c("Characteristic", "Observed treatment")

  generated <- build_ars_json(list(empty_axis), study_id = "S1")

  no_axis <- empty_axis
  no_axis$by_variable <- ""
  no_axis$column_groups <- NULL
  fallback <- build_ars_json(list(no_axis), study_id = "S1")

  expect_true(generated$analysisGroupings[[1]]$dataDriven)
  expect_length(generated$analysisGroupings[[1]]$groups, 0L)
  expect_true(fallback$analysisGroupings[[1]]$dataDriven)
  expect_length(fallback$analysisGroupings[[1]]$groups, 0L)
})

.gi_flat_model <- function() {
  ars_to_model(build_ars_json(list(
    .gi_section("T-14-1-1", "COHORTN", .gi_cohort_groups(),
                source_format = "docx"),
    .gi_section("T-14-3-2", "COHORTN", .gi_subgroup_groups(),
                source_format = "xlsx")
  ), study_id = "S1"))
}

test_that("flat-axis validation follows the grouping each output references", {
  findings <- validate_ars_model(.gi_flat_model())

  expect_false(any(findings$ref %in% "FLAT_AXIS_COLUMN_COUNT_MISMATCH"))
  expect_false(any(findings$ref %in% "FLAT_AXIS_COLUMN_LABEL_MISMATCH"))
})

test_that("layout metadata distinguishes physical stub from compact result columns", {
  model <- .gi_flat_model()
  output_index <- match("T_14_1_1", model$outputs$id)
  physical_output <- model$outputs$raw[[output_index]]
  physical_output$displays[[1]]$display$columns[[1]] <- list()
  result_labels <- c("Cohort A", "Cohort B")

  compact_output <- physical_output
  compact_output[["_meta"]][["shell_layout"]] <- NULL
  compact_output$displays[[1]]$display$columns <-
    compact_output$displays[[1]]$display$columns[-1]

  expect_equal(.flat_display_labels(physical_output), result_labels)
  expect_equal(.flat_display_labels(compact_output), result_labels)

  model$outputs$raw[[output_index]] <- compact_output
  findings <- validate_ars_model(model)

  expect_false(any(
    startsWith(findings$ref, "FLAT_AXIS_") &
      findings$id == "T_14_1_1"
  ))
})

test_that("a stub label may equal the first result-column label", {
  model <- .gi_flat_model()
  output_index <- match("T_14_3_2", model$outputs$id)
  columns <- model$outputs$raw[[output_index]]$displays[[1]]$display$columns
  first_group_label <- columns[[2]]$label
  model$outputs$raw[[output_index]]$displays[[1]]$display$columns[[1]]$label <-
    first_group_label

  findings <- validate_ars_model(model)

  expect_false(any(
    startsWith(findings$ref, "FLAT_AXIS_") &
      findings$id == "T_14_3_2"
  ))
})

test_that("empty result-group paths do not bypass flat-axis validation", {
  model <- .gi_flat_model()
  output_index <- match("T_14_3_2", model$outputs$id)
  columns <- model$outputs$raw[[output_index]]$displays[[1]]$display$columns
  model$outputs$raw[[output_index]]$displays[[1]]$display$columns <-
    columns[-length(columns)]
  model$outputs$raw[[output_index]]$resultGroupPaths <- list(paths = list())

  findings <- validate_ars_model(model)
  mismatch <- findings[
    findings$ref %in% "FLAT_AXIS_COLUMN_COUNT_MISMATCH" &
      findings$id == "T_14_3_2",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(mismatch), 1L)
  expect_true(.validation_gate(findings)$blocked)
})

test_that("a flat output must display one column per referenced group", {
  model <- .gi_flat_model()
  output_index <- match("T_14_3_2", model$outputs$id)
  columns <- model$outputs$raw[[output_index]]$displays[[1]]$display$columns
  model$outputs$raw[[output_index]]$displays[[1]]$display$columns <- columns[-4]

  findings <- validate_ars_model(model)
  mismatch <- findings[
    findings$ref %in% "FLAT_AXIS_COLUMN_COUNT_MISMATCH" &
      findings$id == "T_14_3_2",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(mismatch), 1L)
  expect_equal(mismatch$severity, "GAP")
})

test_that("a flat output cannot combine different fixed grouping definitions", {
  model <- .gi_flat_model()
  output_index <- match("T_14_3_2", model$outputs$id)
  other_output <- match("T_14_1_1", model$outputs$id)
  analysis_ids <- c(
    .split_values(model$outputs$referenced_analysis_ids[output_index]),
    .split_values(model$outputs$referenced_analysis_ids[other_output])[[1]]
  )
  model$outputs$referenced_analysis_ids[output_index] <- .join_values(analysis_ids)

  findings <- validate_ars_model(model)
  mismatch <- findings[
    findings$ref %in% "FLAT_AXIS_COLUMN_COUNT_MISMATCH" &
      findings$id == "T_14_3_2",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(mismatch), 1L)
  expect_match(mismatch$problem, "different fixed grouping definitions")
})


test_that("a flat output with no result columns is a count mismatch", {
  model <- .gi_flat_model()
  output_index <- match("T_14_3_2", model$outputs$id)
  columns <- model$outputs$raw[[output_index]]$displays[[1]]$display$columns
  model$outputs$raw[[output_index]]$displays[[1]]$display$columns <- columns[1]

  findings <- validate_ars_model(model)
  mismatch <- findings[
    findings$ref %in% "FLAT_AXIS_COLUMN_COUNT_MISMATCH" &
      findings$id == "T_14_3_2",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(mismatch), 1L)
  expect_equal(mismatch$severity, "GAP")
})


test_that("flat output labels must match the referenced grouping", {
  model <- .gi_flat_model()
  output_index <- match("T_14_3_2", model$outputs$id)
  model$outputs$raw[[output_index]]$displays[[1]]$display$columns[[4]]$label <-
    "Unexpected"

  findings <- validate_ars_model(model)
  mismatch <- findings[
    findings$ref %in% "FLAT_AXIS_COLUMN_LABEL_MISMATCH" &
      findings$id == "T_14_3_2",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(mismatch), 1L)
  expect_equal(mismatch$severity, "GAP")
})

.gi_total_model <- function(include_total = c(TRUE, TRUE),
                            total_labels = c("Overall", "Overall")) {
  model <- .gi_flat_model()
  output_index <- match("T_14_3_2", model$outputs$id)
  analysis_index <- match(
    .split_values(model$outputs$referenced_analysis_ids[output_index]),
    model$analyses$id
  )[[1]]

  first_id <- model$analyses$id[analysis_index]
  second_id <- paste0(first_id, "_SECOND")
  second <- model$analyses[analysis_index, , drop = FALSE]
  second$id <- second_id
  second$includeTotal <- include_total[[2]]
  second_raw <- second$raw[[1]]
  second_raw$id <- second_id
  second_raw$includeTotal <- include_total[[2]]
  second_raw$totalLabel <- total_labels[[2]]
  second$raw <- list(second_raw)

  model$analyses$includeTotal[analysis_index] <- include_total[[1]]
  model$analyses$raw[[analysis_index]]$includeTotal <- include_total[[1]]
  model$analyses$raw[[analysis_index]]$totalLabel <- total_labels[[1]]
  model$analyses <- rbind(model$analyses, second)
  model$outputs$referenced_analysis_ids[output_index] <- .join_values(
    c(first_id, second_id)
  )

  columns <- model$outputs$raw[[output_index]]$displays[[1]]$display$columns
  columns[[length(columns) + 1L]] <- list(label = "Overall")
  model$outputs$raw[[output_index]]$displays[[1]]$display$columns <- columns
  model
}


test_that("every analysis on a flat output must agree on Total inclusion", {
  findings <- validate_ars_model(.gi_total_model(
    include_total = c(TRUE, FALSE)
  ))
  mismatch <- findings[
    findings$ref %in% "FLAT_AXIS_COLUMN_COUNT_MISMATCH" &
      findings$id == "T_14_3_2",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(mismatch), 1L)
  expect_match(mismatch$problem, "disagree")
})


test_that("every analysis on a flat output must use the displayed Total label", {
  findings <- validate_ars_model(.gi_total_model(
    total_labels = c("Overall", "All")
  ))
  mismatch <- findings[
    findings$ref %in% "FLAT_AXIS_COLUMN_LABEL_MISMATCH" &
      findings$id == "T_14_3_2",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(mismatch), 1L)
  expect_match(mismatch$problem, "Total labels")
})
