## Editing shared entities from the library.
##
## This view exists because ARS entities are shared by reference: changing a
## population here changes every analysis that uses it, which is the point when
## the population itself is wrong and a trap otherwise. The panel says which
## situation you are in.

.library_model <- function() {
  ars_to_model(test_path("fixtures", "ars_apx_drm_301_deterministic.json"))
}

.library_state <- function(mode = "edit") {
  .editor_state(.library_model(), NULL, NULL, NULL, mode)
}

.set_input <- function(session, name, value) {
  args <- list(value)
  names(args) <- name
  do.call(session$setInputs, args)
}


test_that("selecting an entity shows its editable definition", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")

  state <- .library_state()

  shiny::testServer(mod_entity_library_server, args = list(state = state), {
    session$setInputs(table_data_subsets_rows_selected = 1)
    rendered <- paste(as.character(output$detail_data_subsets), collapse = " ")

    expect_match(rendered, "condition_variable")
    expect_match(rendered, "used by")
    expect_match(rendered, "Apply JSON")
  })
})

test_that("the read-only viewer shows the definition without inputs", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")

  state <- .library_state(mode = "view")

  shiny::testServer(mod_entity_library_server, args = list(state = state), {
    session$setInputs(table_data_subsets_rows_selected = 1)
    rendered <- paste(as.character(output$detail_data_subsets), collapse = " ")

    expect_false(grepl("Apply JSON", rendered, fixed = TRUE))
    expect_match(rendered, "Condition")
  })
})

test_that("editing a condition updates the model and the JSON", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")

  state <- .library_state()
  subset_id <- .library_model()$data_subsets$id[1]
  input_name <- .entity_input_id("data_subsets", subset_id,
                                 "condition_variable")

  shiny::testServer(mod_entity_library_server, args = list(state = state), {
    session$setInputs(table_data_subsets_rows_selected = 1)
    .set_input(session, input_name, "ZZVAR")

    expect_equal(state$model()$data_subsets$condition_variable[1], "ZZVAR")
    expect_equal(nrow(state$edit_log()), 1)

    ars <- model_to_ars(state$model())
    expect_equal(ars$dataSubsets[[1]]$condition$variable, "ZZVAR")
  })
})

test_that("editing a method from the library reaches the reporting event", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")

  state <- .library_state()
  method_id <- .library_model()$methods$id[1]
  input_name <- .entity_input_id("methods", method_id, "description")

  shiny::testServer(mod_entity_library_server, args = list(state = state), {
    session$setInputs(table_methods_rows_selected = 1)
    .set_input(session, input_name, "Edited from the library")

    expect_equal(state$model()$methods$description[1],
                 "Edited from the library")
    ars <- model_to_ars(state$model())
    expect_equal(ars$methods[[1]]$description, "Edited from the library")
  })
})

test_that("a library edit can be undone", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")

  state <- .library_state()
  model <- .library_model()
  subset_id <- model$data_subsets$id[1]
  original <- model$data_subsets$condition_variable[1]
  input_name <- .entity_input_id("data_subsets", subset_id,
                                 "condition_variable")

  shiny::testServer(mod_entity_library_server, args = list(state = state), {
    session$setInputs(table_data_subsets_rows_selected = 1)
    .set_input(session, input_name, "ZZVAR")
    expect_true(.can_undo(state))

    .undo(state)
    expect_equal(state$model()$data_subsets$condition_variable[1], original)
  })
})

test_that("input ids are unique per entity, so values cannot leak across rows", {
  model <- .library_model()
  ids <- model$data_subsets$id[1:3]
  input_names <- vapply(
    ids,
    function(id) .entity_input_id("data_subsets", id, "condition_variable"),
    character(1)
  )

  expect_equal(length(unique(input_names)), 3)
  ## And the same pool/field on a different pool cannot collide either.
  expect_false(
    .entity_input_id("data_subsets", ids[1], "name") ==
      .entity_input_id("analysis_sets", ids[1], "name")
  )
})

test_that("the raw-JSON escape hatch applies a valid replacement", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")

  state <- .library_state()
  subset_id <- .library_model()$data_subsets$id[1]
  json_input <- .entity_input_id("data_subsets", subset_id, "json")

  replacement <- as.character(jsonlite::toJSON(list(
    id = subset_id, name = "Rewritten", label = "Rewritten",
    condition = list(dataset = "ADSL", variable = "AGE",
                     comparator = "GE", value = list("18")),
    level = 1L, order = 1L
  ), auto_unbox = TRUE))

  shiny::testServer(mod_entity_library_server, args = list(state = state), {
    session$setInputs(table_data_subsets_rows_selected = 1)
    .set_input(session, json_input, replacement)
    session$setInputs(apply_json = list(pool = "data_subsets", id = subset_id))

    expect_equal(state$model()$data_subsets$condition_variable[1], "AGE")
    expect_equal(state$model()$data_subsets$label[1], "Rewritten")
    expect_equal(nrow(state$edit_log()), 1)
  })
})

test_that("invalid JSON is refused and changes nothing", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")

  state <- .library_state()
  model <- .library_model()
  subset_id <- model$data_subsets$id[1]
  json_input <- .entity_input_id("data_subsets", subset_id, "json")
  original <- model$data_subsets$condition_variable[1]

  shiny::testServer(mod_entity_library_server, args = list(state = state), {
    session$setInputs(table_data_subsets_rows_selected = 1)
    .set_input(session, json_input, "{not json")
    session$setInputs(apply_json = list(pool = "data_subsets", id = subset_id))

    expect_equal(state$model()$data_subsets$condition_variable[1], original)
    expect_equal(nrow(state$edit_log()), 0)
  })
})

test_that("a JSON replacement that renames the entity is refused", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")

  state <- .library_state()
  subset_id <- .library_model()$data_subsets$id[1]
  json_input <- .entity_input_id("data_subsets", subset_id, "json")

  shiny::testServer(mod_entity_library_server, args = list(state = state), {
    session$setInputs(table_data_subsets_rows_selected = 1)
    .set_input(session, json_input, '{"id": "DS_SOMETHING_ELSE"}')
    session$setInputs(apply_json = list(pool = "data_subsets", id = subset_id))

    expect_equal(nrow(state$edit_log()), 0)
    expect_true(subset_id %in% state$model()$data_subsets$id)
  })
})
