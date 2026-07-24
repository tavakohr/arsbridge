## Adding, removing, reordering and detaching -- the edits that change the
## shape of a reporting event rather than the content of one field.
##
## The contract these pin: a hand-added line is indistinguishable from a
## generated one, referential integrity survives every operation, and the
## tables of contents follow along without being touched directly.

.add_model <- function() {
  ars_to_model(test_path("fixtures", "ars_apx_drm_301_deterministic.json"))
}

.add_a_line <- function(model, output_id = "T_14_1_2", ...) {
  defaults <- list(
    output_id       = output_id,
    label           = "Smoking status, n (%)",
    dataset         = "ADSL",
    variable        = "SMOKFL",
    method_id       = "MTH_COUNT_AND_PERCENTAGE",
    analysis_set_id = model$analysis_sets$id[1],
    grouping_ids    = "GF_TRT01A",
    annotation      = "ADSL.SMOKFL"
  )
  do.call(model_add_analysis, c(list(model), utils::modifyList(defaults,
                                                              list(...))))
}


test_that("an added analysis lands in the pool and in its output", {
  model <- .add_model()
  before <- nrow(model$analyses)

  updated <- .add_a_line(model)
  added_id <- attr(updated, "last_added")

  expect_equal(nrow(updated$analyses), before + 1)
  expect_true(added_id %in% updated$analyses$id)

  references <- .split_values(
    updated$outputs$referenced_analysis_ids[updated$outputs$id == "T_14_1_2"]
  )
  expect_true(added_id %in% references)
  expect_equal(updated$analyses$output_id[updated$analyses$id == added_id],
               "T_14_1_2")
})

test_that("the minted id follows the generator's convention and is free", {
  model <- .add_model()
  updated <- .add_a_line(model)
  added_id <- attr(updated, "last_added")

  expect_match(added_id, "^AN_T_14_1_2_[0-9]{3}$")
  expect_false(added_id %in% model$analyses$id)
  expect_equal(sum(updated$analyses$id == added_id), 1)

  ## Adding again mints a different id rather than colliding.
  twice <- .add_a_line(updated)
  expect_false(attr(twice, "last_added") == added_id)
  expect_equal(anyDuplicated(twice$analyses$id), 0)
})

test_that("a minted id skips one that is already taken", {
  model <- .add_model()
  ## Occupy the id the next add would naturally choose.
  taken <- .next_analysis_id(model, "T_14_1_2")
  model$analyses$id[1] <- taken

  expect_false(.next_analysis_id(model, "T_14_1_2") == taken)
})

test_that("an added node has exactly the shape the generator emits", {
  model <- .add_model()
  updated <- .add_a_line(model)
  added_id <- attr(updated, "last_added")

  ars <- model_to_ars(updated)
  nodes <- ars$analyses
  added <- nodes[[which(vapply(nodes, function(a) a$id == added_id,
                               logical(1)))]]
  generated <- nodes[[1]]

  expect_setequal(names(added), names(generated))

  ## The pieces that are easy to get wrong: the flat/nested variable pair,
  ## the empty-string "no subset" sentinel, and the self-referential
  ## operation placeholders siera needs.
  expect_equal(added$dataset, "ADSL")
  expect_equal(added$analysisVariable$variable, "SMOKFL")
  expect_identical(added$dataSubsetId, "")
  expect_equal(added$referencedAnalysisOperations[[1]]$analysisId, added_id)
  expect_equal(added$referencedAnalysisOperations[[2]]$analysisId, added_id)
  expect_equal(added$orderedGroupings[[1]]$groupingId, "GF_TRT01A")
  expect_equal(added$orderedGroupings[[1]]$order, 1)
})

test_that("a line can be inserted at a chosen position", {
  model <- .add_model()
  references <- .split_values(
    model$outputs$referenced_analysis_ids[model$outputs$id == "T_14_1_2"]
  )

  updated <- .add_a_line(model, after = references[2])
  added_id <- attr(updated, "last_added")

  after_refs <- .split_values(
    updated$outputs$referenced_analysis_ids[updated$outputs$id == "T_14_1_2"]
  )
  expect_equal(which(after_refs == added_id), 3)
  ## The lines around it keep their order.
  expect_equal(after_refs[1:2], references[1:2])
  expect_equal(after_refs[4], references[3])
})

test_that("with no position given the line goes last", {
  model <- .add_model()
  updated <- .add_a_line(model)
  added_id <- attr(updated, "last_added")

  references <- .split_values(
    updated$outputs$referenced_analysis_ids[updated$outputs$id == "T_14_1_2"]
  )
  expect_equal(utils::tail(references, 1), added_id)
})

test_that("adding a line rebuilds the tables of contents", {
  model <- .add_model()
  updated <- .add_a_line(model)
  added_id <- attr(updated, "last_added")

  ars <- model_to_ars(updated)
  items <- ars$mainListOfContents$contentsList$listItems
  index <- which(vapply(items, function(x) x$outputId == "T_14_1_2",
                        logical(1)))
  listed <- vapply(items[[index]]$sublist$listItems,
                   function(x) x$analysisId, character(1))

  expect_true(added_id %in% listed)

  ## Untouched outputs keep their original entries.
  original <- .read_json(
    test_path("fixtures", "ars_apx_drm_301_deterministic.json")
  )
  expect_identical(items[[1]], original$mainListOfContents$contentsList$listItems[[1]])
})

test_that("an added line leaves the event referentially intact", {
  model <- .add_model()
  updated <- .add_a_line(model)
  findings <- validate_ars_model(updated)

  expect_equal(sum(findings$severity == "FAIL"), 0)
})

test_that("adding to an output that does not exist is refused", {
  model <- .add_model()
  expect_error(.add_a_line(model, output_id = "T_NOPE"), "No output")
})

test_that("removing a line removes every reference to it", {
  model <- .add_model()
  target <- model$analyses$id[model$analyses$output_id == "T_14_1_2"][1]

  updated <- model_remove_analysis(model, target)

  expect_false(target %in% updated$analyses$id)
  references <- unlist(lapply(updated$outputs$referenced_analysis_ids,
                              .split_values))
  expect_false(target %in% references)
  expect_equal(sum(validate_ars_model(updated)$severity == "FAIL"), 0)
})

test_that("adding then removing a line restores the original event", {
  path <- test_path("fixtures", "ars_apx_drm_301_deterministic.json")
  original <- .read_json(path)

  model <- ars_to_model(path)
  added <- .add_a_line(model)
  restored <- model_remove_analysis(added, attr(added, "last_added"))

  expect_equal(model_to_ars(restored), original)
})

test_that("removing the last line of an output leaves it empty, not broken", {
  model <- .add_model()
  output_id <- model$outputs$id[model$outputs$n_analyses > 0][1]
  references <- .split_values(
    model$outputs$referenced_analysis_ids[model$outputs$id == output_id]
  )

  for (analysis_id in references) {
    model <- model_remove_analysis(model, analysis_id)
  }

  expect_true(is.na(
    model$outputs$referenced_analysis_ids[model$outputs$id == output_id]
  ))
  expect_equal(sum(validate_ars_model(model)$severity == "FAIL"), 0)

  ## The now-empty output is reported, since it would render blank.
  findings <- validate_ars_model(model)
  empty <- findings[findings$id == output_id &
                      findings$field == "referenced_analysis_ids", ]
  expect_gt(nrow(empty), 0)
})

test_that("removing an analysis that is not there is refused", {
  expect_error(model_remove_analysis(.add_model(), "AN_GONE"), "No analysis")
})

test_that("a line can be moved within its output", {
  model <- .add_model()
  references <- .split_values(
    model$outputs$referenced_analysis_ids[model$outputs$id == "T_14_1_2"]
  )

  moved <- model_move_analysis(model, "T_14_1_2", references[3], -1)
  after <- .split_values(
    moved$outputs$referenced_analysis_ids[moved$outputs$id == "T_14_1_2"]
  )

  expect_equal(after[2], references[3])
  expect_equal(after[3], references[2])
  expect_setequal(after, references)
})

test_that("moving past either end does nothing", {
  model <- .add_model()
  references <- .split_values(
    model$outputs$referenced_analysis_ids[model$outputs$id == "T_14_1_2"]
  )

  unchanged_up <- model_move_analysis(model, "T_14_1_2", references[1], -1)
  unchanged_down <- model_move_analysis(
    model, "T_14_1_2", utils::tail(references, 1), 1
  )

  expect_equal(
    .split_values(
      unchanged_up$outputs$referenced_analysis_ids[
        unchanged_up$outputs$id == "T_14_1_2"
      ]
    ),
    references
  )
  expect_equal(
    .split_values(
      unchanged_down$outputs$referenced_analysis_ids[
        unchanged_down$outputs$id == "T_14_1_2"
      ]
    ),
    references
  )
})

test_that("reordering changes the contents list and nothing else", {
  path <- test_path("fixtures", "ars_apx_drm_301_deterministic.json")
  original <- .read_json(path)
  model <- ars_to_model(path)

  references <- .split_values(
    model$outputs$referenced_analysis_ids[model$outputs$id == "T_14_1_2"]
  )
  moved <- model_move_analysis(model, "T_14_1_2", references[3], -1)
  ars <- model_to_ars(moved)

  ## The analyses themselves are untouched; only the order they are shown in
  ## changed.
  expect_equal(ars$analyses, original$analyses)

  index <- which(vapply(ars$outputs, function(o) o$id == "T_14_1_2",
                        logical(1)))
  expect_equal(unlist(ars$outputs[[index]]$referencedAnalysisIds)[2],
               references[3])
})


test_that("detaching gives one analysis its own copy of a shared population", {
  model <- .add_model()
  shared_id <- model$analyses$analysisSetId[1]
  before <- .usage_count(.entity_usage(model)$analysis_sets, shared_id)
  expect_gt(before, 1)

  target <- model$analyses$id[model$analyses$analysisSetId == shared_id][1]
  detached <- model_detach_entity(model, "analysis_sets", shared_id, target)
  variant_id <- attr(detached, "last_added")

  expect_true(variant_id %in% detached$analysis_sets$id)
  expect_equal(detached$analyses$analysisSetId[detached$analyses$id == target],
               variant_id)

  usage <- .entity_usage(detached)$analysis_sets
  expect_equal(.usage_count(usage, shared_id), before - 1)
  expect_equal(.usage_count(usage, variant_id), 1)
  expect_equal(sum(validate_ars_model(detached)$severity == "FAIL"), 0)
})

test_that("editing a detached copy leaves the original alone", {
  model <- .add_model()
  shared_id <- model$analyses$analysisSetId[1]
  target <- model$analyses$id[model$analyses$analysisSetId == shared_id][1]

  detached <- model_detach_entity(model, "analysis_sets", shared_id, target)
  variant_id <- attr(detached, "last_added")
  edited <- model_set_field(detached, "analysis_sets", variant_id,
                            "condition_variable", "ITTFL")

  ars <- model_to_ars(edited)
  find_set <- function(id) {
    ars$analysisSets[[which(vapply(ars$analysisSets, function(s) s$id == id,
                                   logical(1)))]]
  }

  expect_equal(find_set(variant_id)$condition$variable, "ITTFL")
  expect_equal(
    find_set(shared_id)$condition$variable,
    model$analysis_sets$condition_variable[
      model$analysis_sets$id == shared_id
    ]
  )
})

test_that("a detached copy is marked as a variant", {
  model <- .add_model()
  shared_id <- model$analyses$analysisSetId[1]
  target <- model$analyses$id[model$analyses$analysisSetId == shared_id][1]

  detached <- model_detach_entity(model, "analysis_sets", shared_id, target)
  variant_id <- attr(detached, "last_added")
  label <- detached$analysis_sets$label[
    detached$analysis_sets$id == variant_id
  ]

  expect_match(label, "variant")
})

test_that("detaching twice mints distinct variants", {
  model <- .add_model()
  shared_id <- model$analyses$analysisSetId[1]
  targets <- model$analyses$id[model$analyses$analysisSetId == shared_id][1:2]

  once <- model_detach_entity(model, "analysis_sets", shared_id, targets[1])
  twice <- model_detach_entity(once, "analysis_sets", shared_id, targets[2])

  expect_false(attr(once, "last_added") == attr(twice, "last_added"))
  expect_equal(anyDuplicated(twice$analysis_sets$id), 0)
})

test_that("detaching a grouping repoints only that one entry", {
  model <- .add_model()
  target <- model$analyses$id[!is.na(model$analyses$grouping_ids)][1]
  before <- .split_values(
    model$analyses$grouping_ids[model$analyses$id == target]
  )

  detached <- model_detach_entity(model, "groupings", before[1], target)
  variant_id <- attr(detached, "last_added")
  after <- .split_values(
    detached$analyses$grouping_ids[detached$analyses$id == target]
  )

  expect_equal(length(after), length(before))
  expect_equal(after[1], variant_id)
  expect_equal(sum(validate_ars_model(detached)$severity == "FAIL"), 0)
})

test_that("a method cannot be detached, because the engine dispatches on its id", {
  ## A per-analysis copy of a method would carry a new id, which no executor
  ## matches -- the line would silently degrade to a generic summary. The
  ## method dropdown is the supported way to change one line.
  model <- .add_model()
  target <- model$analyses$id[
    model$analyses$methodId == "MTH_COUNT_AND_PERCENTAGE"
  ][1]

  expect_error(
    model_detach_entity(model, "methods", "MTH_COUNT_AND_PERCENTAGE", target),
    "cannot be detached"
  )
})

test_that("a detached population keeps the line executable", {
  ## Conditions are consumed by content, so a variant behaves exactly like the
  ## original until its condition is edited. This is what makes detaching
  ## populations safe where detaching methods is not.
  model <- .add_model()
  shared_id <- model$analyses$analysisSetId[1]
  target <- model$analyses$id[model$analyses$analysisSetId == shared_id][1]

  detached <- model_detach_entity(model, "analysis_sets", shared_id, target)
  before <- validate_ars_model(model)
  after <- validate_ars_model(detached)

  expect_equal(nrow(after), nrow(before))
})

test_that("only shared-entity pools can be detached from", {
  model <- .add_model()
  expect_error(
    model_detach_entity(model, "outputs", "T_14_1_2", model$analyses$id[1]),
    "Cannot detach"
  )
})


test_that("a gap finding carries the variable it is about", {
  model <- .add_model()
  report <- utils::read.csv(
    test_path("fixtures", "ars_apx_drm_301_validation.csv"),
    stringsAsFactors = FALSE
  )

  target <- model$analyses$id[model$analyses$output_id == "T_14_1_2" &
                                model$analyses$variable == "SEX"][1]
  skip_if(length(target) == 0, "fixture has no ADSL.SEX analysis")

  model <- model_remove_analysis(model, target)
  findings <- validate_ars_model(model, report = report)
  gap <- findings[findings$field == "analyses", ]

  expect_gt(nrow(gap), 0)
  expect_equal(gap$ref[1], "ADSL.SEX")
  expect_true(.is_gap_finding(gap[1, , drop = FALSE]))

  ## Which is what lets the wizard open pre-filled.
  parts <- .split_variable_ref(gap$ref[1])
  expect_equal(parts$dataset, "ADSL")
  expect_equal(parts$variable, "SEX")
})

test_that("a variable reference without a dataset still yields a variable", {
  expect_equal(.split_variable_ref("SEX")$variable, "SEX")
  expect_true(is.na(.split_variable_ref("SEX")$dataset))
  expect_true(is.na(.split_variable_ref(NA_character_)$variable))
  expect_true(is.na(.split_variable_ref("")$variable))
})

test_that("the wizard offers every existing line as a position", {
  model <- .add_model()
  choices <- .position_choices(model, "T_14_1_2")
  references <- .split_values(
    model$outputs$referenced_analysis_ids[model$outputs$id == "T_14_1_2"]
  )

  expect_equal(length(choices), length(references) + 1)
  expect_equal(choices[[1]], "")
  expect_match(names(choices)[1], "end")
  expect_true(all(references %in% unlist(choices)))
})

test_that("an output with no lines offers only the first position", {
  model <- .add_model()
  empty <- model$outputs$id[model$outputs$n_analyses == 0][1]
  choices <- .position_choices(model, empty)

  expect_equal(length(choices), 1)
  expect_match(names(choices)[1], "first")
})

test_that("the wizard defaults to the shape of the output's other lines", {
  model <- .add_model()
  defaults <- .sibling_defaults(model, "T_14_1_2")

  siblings <- model$analyses[model$analyses$output_id == "T_14_1_2", ]
  commonest <- names(sort(table(siblings$methodId), decreasing = TRUE))[1]

  expect_equal(defaults$method_id, commonest)
  expect_equal(defaults$data_subset_id, "")
  expect_true(defaults$analysis_set_id %in% model$analysis_sets$id)
  expect_true(all(defaults$grouping_ids %in% model$groupings$id))
})

test_that("an empty output still gets usable defaults", {
  model <- .add_model()
  empty <- model$outputs$id[model$outputs$n_analyses == 0][1]
  defaults <- .sibling_defaults(model, empty)

  ## Falls back to the event as a whole rather than returning nothing.
  expect_true(defaults$method_id %in% model$methods$id)
  expect_true(defaults$analysis_set_id %in% model$analysis_sets$id)
})

test_that("an added line executes into an ARD", {
  ## The point of adding a line: it produces numbers like any other.
  skip_on_cran()

  dir <- withr::local_tempdir()
  path <- file.path(dir, "reporting_event.json")
  file.copy(
    test_path("fixtures", "ars_apx_drm_301_deterministic.json"), path
  )

  model <- ars_to_model(path)
  updated <- .add_a_line(
    model,
    label = "Sex, n (%) added by hand",
    variable = "SEX",
    analysis_set_id = model$analyses$analysisSetId[
      model$analyses$output_id == "T_14_1_2"
    ][1]
  )
  added_id <- attr(updated, "last_added")

  json <- jsonlite::toJSON(model_to_ars(updated), auto_unbox = TRUE,
                           pretty = TRUE, null = "null")
  writeLines(as.character(json), path, useBytes = TRUE)

  adam_dir <- withr::local_tempdir()
  utils::unzip(arsbridge_example("ADaM.zip"), exdir = adam_dir)

  ard <- suppressWarnings(suppressMessages(
    ars_to_ard(path, adam_dir = adam_dir, output_ids = "T_14_1_2")
  ))

  expect_true(added_id %in% ard$analysis_id)
  expect_gt(sum(ard$analysis_id == added_id), 0)
})
