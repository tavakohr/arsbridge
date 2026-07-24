## The tree and detail modules.
##
## Most of the logic is in pure helpers that build the tree's content and
## resolve an analysis's ids into words, so they are tested directly; the
## reactive wiring is covered with shiny::testServer().

.tree_model <- function() {
  ars_to_model(test_path("fixtures", "ars_apx_drm_301_deterministic.json"))
}

test_that("the tree lists every analysis under its output", {
  model <- .tree_model()
  tree  <- .tree_data(model)

  expect_setequal(unique(tree$output_id), model$outputs$id)
  expect_true(all(stats::na.omit(tree$analysis_id) %in% model$analyses$id))

  ## Every analysis reference becomes a line; an output with none still gets
  ## a row of its own so it stays visible.
  n_references <- sum(vapply(
    model$outputs$referenced_analysis_ids,
    function(ids) length(.split_values(ids)), integer(1)
  ))
  n_empty_outputs <- sum(model$outputs$n_analyses == 0)

  expect_equal(nrow(tree), n_references + n_empty_outputs)
  expect_equal(sum(is.na(tree$analysis_id)), n_empty_outputs)
})

test_that("an analysis with no label falls back to its id", {
  model <- .tree_model()
  model$analyses$label[1] <- NA_character_

  tree <- .tree_data(model)
  row <- tree[tree$analysis_id == model$analyses$id[1], ]

  expect_equal(row$analysis_label[1], model$analyses$id[1])
})

test_that("an output with no analyses still appears", {
  model <- .tree_model()
  model$outputs$referenced_analysis_ids[1] <- NA_character_

  tree <- .tree_data(model)
  row <- tree[tree$output_id == model$outputs$id[1], ]

  expect_equal(nrow(row), 1)
  expect_true(is.na(row$analysis_id))
})

test_that("an empty event produces an empty tree rather than an error", {
  model <- ars_to_model(test_path("fixtures", "tfrmt_reporting_event.json"))
  model$outputs <- model$outputs[0, ]

  tree <- .tree_data(model)
  expect_equal(nrow(tree), 0)
  expect_true("output_id" %in% names(tree))
})

test_that("findings badge the analysis they are about", {
  model <- .tree_model()
  target <- model$analyses$id[1]
  findings <- data.frame(
    severity = "FAIL", entity = "analyses", id = target,
    field = "methodId", problem = "x", action = "y",
    stringsAsFactors = FALSE
  )

  tree <- .tree_data(model, findings)
  expect_equal(tree$badge[which(tree$analysis_id == target)], "FAIL")
  expect_true(all(is.na(tree$badge[which(tree$analysis_id != target)])))
})

test_that("an output's badge is the worst finding anywhere beneath it", {
  model <- .tree_model()
  output_id <- model$outputs$id[1]
  analysis_ids <- .split_values(model$outputs$referenced_analysis_ids[1])

  findings <- data.frame(
    severity = c("INFO", "FAIL"),
    entity   = "analyses",
    id       = analysis_ids[1:2],
    field    = "methodId", problem = "x", action = "y",
    stringsAsFactors = FALSE
  )

  tree <- .tree_data(model, findings)
  expect_equal(.output_badge(tree, output_id, findings), "FAIL")

  expect_equal(.worst_severity(c("INFO", "WARN")), "WARN")
  expect_true(is.na(.worst_severity(character(0))))
})

test_that("the filter matches outputs and analyses, and is case-insensitive", {
  model <- .tree_model()
  tree  <- .tree_data(model)

  expect_equal(nrow(.tree_filter(tree, "")), nrow(tree))
  expect_equal(nrow(.tree_filter(tree, NULL)), nrow(tree))

  ## Filtering by an output keeps that output's whole line-up.
  by_output <- .tree_filter(tree, "T_14_1_2")
  expect_equal(unique(by_output$output_id), "T_14_1_2")
  expect_gt(nrow(by_output), 1)

  ## Filtering by an analysis label narrows to matching lines.
  by_label <- .tree_filter(tree, "sex")
  expect_gt(nrow(by_label), 0)
  expect_lt(nrow(by_label), nrow(tree))

  expect_equal(nrow(.tree_filter(tree, "SEX")), nrow(by_label))
  expect_equal(nrow(.tree_filter(tree, "zzz-no-match")), 0)
})

test_that("clicking a node selects it", {
  skip_if_not_installed("shiny")
  model <- .tree_model()
  target <- model$analyses$id[2]

  shiny::testServer(
    mod_tree_server,
    args = list(state = .editor_state(model, NULL, NULL, NULL, "view")),
    {
      expect_null(state$selected())

      session$setInputs(selected = list(pool = "analyses", id = target))
      expect_equal(state$selected()$id, target)
      expect_equal(state$selected()$pool, "analyses")

      session$setInputs(selected = list(pool = "outputs",
                                        id = model$outputs$id[1]))
      expect_equal(state$selected()$pool, "outputs")
    }
  )
})

test_that("an analysis's ids are resolved into words", {
  model <- .tree_model()
  row <- model$analyses[1, , drop = FALSE]
  fields <- .analysis_summary_fields(row, model)

  expect_type(fields, "character")
  expect_equal(fields[["Analysis id"]], row$id)
  expect_equal(fields[["Variable"]], paste0(row$dataset, ".", row$variable))

  ## The method is named, not just referenced, and the reviewer is told what
  ## the engine will do with it.
  expect_match(fields[["Method"]], row$methodId, fixed = TRUE)
  expect_match(fields[["Method"]], "Subject Count")
  expect_equal(fields[["Executed as"]], "Computed by the engine")

  ## The population and subset show their conditions, not bare ids.
  expect_match(fields[["Population"]], "ADSL.ITTFL", fixed = TRUE)
  expect_match(fields[["Data subset"]], "RANDFL", fixed = TRUE)
  expect_match(fields[["Grouped by"]], "GF_TRT01A", fixed = TRUE)
})

test_that("an analysis with no data subset says so", {
  model <- .tree_model()
  none <- which(model$analyses$dataSubsetId == "")[1]
  fields <- .analysis_summary_fields(model$analyses[none, , drop = FALSE],
                                     model)

  expect_equal(fields[["Data subset"]], "None (all records)")
})

test_that("a dangling reference is shown rather than silently blank", {
  model <- .tree_model()
  model$analyses$methodId[1] <- "MTH_GONE"
  fields <- .analysis_summary_fields(model$analyses[1, , drop = FALSE], model)

  expect_match(fields[["Method"]], "not in this reporting event")
  expect_equal(fields[["Executed as"]],
               "No executor -- the generic summarizer runs instead")
})

test_that("empty fields render as a dash rather than NA", {
  model <- .tree_model()
  model$analyses$sapDescription[1] <- NA_character_
  fields <- .analysis_summary_fields(model$analyses[1, , drop = FALSE], model)

  expect_equal(fields[["SAP description"]], "--")
  expect_false(any(is.na(fields)))
})

test_that("the execution note describes each method class in plain words", {
  expect_equal(.execution_note("MTH_COUNT_AND_PERCENTAGE"),
               "Computed by the engine")
  expect_match(.execution_note("MTH_UNSUPPORTED_ANALYSIS"), "manual")
  expect_match(.execution_note("MTH_CMH_TEST"), "stratification")
  expect_match(.execution_note("MTH_CMH_TEST", "BASELINE"), "prerequisites")
  expect_match(.execution_note(NA_character_), "No method")
})

test_that("selecting an entity renders its detail panel", {
  skip_if_not_installed("shiny")
  model <- .tree_model()

  ## renderUI returns a tag structure, so flatten it before matching.
  rendered_text <- function(x) paste(as.character(x), collapse = " ")

  shiny::testServer(
    mod_detail_server,
    args = list(state = .editor_state(model, NULL, NULL, NULL, "view")),
    {
      ## Nothing selected yet.
      expect_match(rendered_text(output$detail), "Select an output")

      state$selected(list(pool = "analyses", id = model$analyses$id[1]))
      session$flushReact()
      expect_match(rendered_text(output$detail), model$analyses$id[1],
                   fixed = TRUE)

      state$selected(list(pool = "outputs", id = model$outputs$id[1]))
      session$flushReact()
      expect_match(rendered_text(output$detail), model$outputs$id[1],
                   fixed = TRUE)

      ## An entity that has since been removed is reported, not crashed on.
      state$selected(list(pool = "analyses", id = "AN_GONE"))
      session$flushReact()
      expect_match(rendered_text(output$detail), "no longer")
    }
  )
})

test_that("the entity library counts how many analyses share each entity", {
  model <- .tree_model()
  table <- .library_table(model, "methods")

  expect_true("Used by" %in% names(table))
  expect_equal(nrow(table), nrow(model$methods))
  expect_equal(
    sum(table[["Used by"]]),
    sum(!is.na(model$analyses$methodId))
  )

  ## A method nothing references reads zero rather than erroring.
  unused <- table[table[["Used by"]] == 0, ]
  expect_gt(nrow(unused), 0)
})

test_that("the entity library handles pools that are empty", {
  model <- ars_to_model(test_path("fixtures", "tfrmt_reporting_event.json"))

  for (pool in c("methods", "analysis_sets", "data_subsets", "groupings")) {
    table <- .library_table(model, pool)
    expect_s3_class(table, "data.frame")
  }
  expect_equal(nrow(.library_table(model, "methods")), 0)
  expect_equal(nrow(.library_table(model, "groupings")), 4)
})

test_that("selecting a finding navigates to the entity it is about", {
  skip_if_not_installed("shiny")
  model <- .tree_model()
  model$analyses$methodId[1] <- "MTH_GONE"
  state <- .editor_state(model, NULL, NULL, NULL, "view")

  shiny::testServer(
    mod_validation_server,
    args = list(state = state),
    {
      findings <- state$findings()
      dangling <- which(findings$severity == "FAIL")[1]
      expect_false(is.na(dangling))

      session$setInputs(findings_rows_selected = dangling)
      expect_equal(state$selected()$pool, findings$entity[dangling])
      expect_equal(state$selected()$id, findings$id[dangling])
    }
  )
})
