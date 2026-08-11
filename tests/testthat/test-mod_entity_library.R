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


test_that("a grouping added after startup remains editable", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  state <- .library_state()

  shiny::testServer(mod_entity_library_server, args = list(state = state), {
    session$setInputs(
      new_grouping_dataset = "ADSL",
      new_grouping_variable = "AGEGR1",
      new_grouping_label = "Age groups",
      confirm_grouping_add = 1
    )
    session$flushReact()

    added_id <- attr(state$model(), "last_added")
    expect_identical(added_id, "GF_AGEGR1")

    input_name <- .entity_input_id("groupings", added_id, "label")
    .set_input(session, input_name, "Edited after add")

    model <- state$model()
    row <- model$groupings[model$groupings$id == added_id, , drop = FALSE]
    expect_identical(row$label, "Edited after add")

    log <- state$edit_log()
    expect_true(any(
      log$pool == "groupings" &
        log$id == added_id &
        log$field == "label" &
        log$new == "Edited after add"
    ))

    ars <- model_to_ars(model)
    grouping_ids <- vapply(
      ars$analysisGroupings,
      function(grouping) grouping$id,
      character(1)
    )
    saved <- ars$analysisGroupings[[match(added_id, grouping_ids)]]
    expect_identical(saved$label, "Edited after add")
  })
})


test_that("a grouping cloned after startup can be edited independently", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  state <- .library_state()
  fixture <- .library_model()
  base_id <- fixture$groupings$id[1]
  base_label <- fixture$groupings$label[1]

  shiny::testServer(mod_entity_library_server, args = list(state = state), {
    session$setInputs(table_groupings_rows_selected = 1, grouping_clone = 1)
    session$flushReact()

    clone_id <- setdiff(state$model()$groupings$id, base_id)
    expect_length(clone_id, 1)
    expect_match(clone_id, paste0("^", base_id, "_VARIANT"))

    input_name <- .entity_input_id("groupings", clone_id, "label")
    .set_input(session, input_name, "Edited clone")

    model <- state$model()
    expect_identical(
      model$groupings$label[model$groupings$id == clone_id],
      "Edited clone"
    )
    expect_identical(
      model$groupings$label[model$groupings$id == base_id],
      base_label
    )

    log <- state$edit_log()
    expect_true(any(
      log$pool == "groupings" &
        log$id == clone_id &
        log$field == "label" &
        log$new == "Edited clone"
    ))
  })
})


test_that("a deleted entity id receives a fresh observer when recreated", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  state <- .library_state()

  shiny::testServer(mod_entity_library_server, args = list(state = state), {
    added <- model_add_grouping(
      state$model(), "ADSL", "AGEGR1", "First grouping"
    )
    entity_id <- attr(added, "last_added")
    state$model(added)
    session$flushReact()

    input_name <- .entity_input_id("groupings", entity_id, "label")
    .set_input(session, input_name, "First edit")
    expect_identical(
      state$model()$groupings$label[
        state$model()$groupings$id == entity_id
      ],
      "First edit"
    )

    state$model(model_remove_grouping(state$model(), entity_id))
    session$flushReact()
    expect_false(entity_id %in% state$model()$groupings$id)

    recreated <- model_add_grouping(
      state$model(), "ADSL", "AGEGR1", "Recreated grouping"
    )
    expect_identical(attr(recreated, "last_added"), entity_id)
    state$model(recreated)
    session$flushReact()

    expect_identical(
      state$model()$groupings$label[
        state$model()$groupings$id == entity_id
      ],
      "Recreated grouping"
    )

    .set_input(session, input_name, "Edited after re-add")
    expect_identical(
      state$model()$groupings$label[
        state$model()$groupings$id == entity_id
      ],
      "Edited after re-add"
    )
  })
})


test_that("a catalogue method added after startup remains editable", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  state <- .library_state()
  method_id <- "MTH_KAPLAN_MEIER_ESTIMATE"
  expect_false(method_id %in% .library_model()$methods$id)

  shiny::testServer(mod_entity_library_server, args = list(state = state), {
    state$model(model_add_method_from_catalogue(state$model(), method_id))
    session$flushReact()

    input_name <- .entity_input_id("methods", method_id, "description")
    .set_input(session, input_name, "Edited after catalogue insertion")

    model <- state$model()
    expect_identical(
      model$methods$description[model$methods$id == method_id],
      "Edited after catalogue insertion"
    )

    ars <- model_to_ars(model)
    method_ids <- vapply(ars$methods, function(method) method$id, character(1))
    saved <- ars$methods[[match(method_id, method_ids)]]
    expect_identical(saved$description, "Edited after catalogue insertion")
  })
})


test_that("a blank grouping label receives its generated default", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  state <- .library_state()

  shiny::testServer(mod_entity_library_server, args = list(state = state), {
    session$setInputs(
      new_grouping_dataset = "ADSL",
      new_grouping_variable = "AGEGR1",
      new_grouping_label = "",
      confirm_grouping_add = 1
    )
    session$flushReact()

    added_id <- attr(state$model(), "last_added")
    row <- state$model()$groupings[
      state$model()$groupings$id == added_id,
      ,
      drop = FALSE
    ]
    expect_identical(row$label, "Grouping by AGEGR1")

    ars <- model_to_ars(state$model())
    grouping_ids <- vapply(
      ars$analysisGroupings,
      function(grouping) grouping$id,
      character(1)
    )
    saved <- ars$analysisGroupings[[match(added_id, grouping_ids)]]
    expect_identical(saved$label, "Grouping by AGEGR1")
  })
})


test_that("entity observer reconciliation tracks membership without duplicates", {
  registry <- new.env(parent = emptyenv())
  counts <- new.env(parent = emptyenv())
  counts$created <- character()
  counts$destroyed <- character()

  make_observer <- function(spec) {
    key <- spec$key
    counts$created <- c(counts$created, key)
    list(destroy = function() {
      counts$destroyed <- c(counts$destroyed, key)
    })
  }

  desired <- list(field_a = list(key = "field_a"))
  .reconcile_entity_observers(registry, desired, make_observer)
  expect_identical(counts$created, "field_a")
  expect_identical(ls(registry), "field_a")

  .reconcile_entity_observers(registry, desired, make_observer)
  expect_identical(counts$created, "field_a")

  .reconcile_entity_observers(registry, list(), make_observer)
  expect_identical(counts$destroyed, "field_a")
  expect_length(ls(registry), 0)

  .reconcile_entity_observers(registry, desired, make_observer)
  expect_identical(counts$created, c("field_a", "field_a"))
  expect_identical(ls(registry), "field_a")
})
