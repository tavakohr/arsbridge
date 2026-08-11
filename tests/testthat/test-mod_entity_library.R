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

## The frozen fixture's only grouping is data-driven and childless, which is
## precisely the case the nested view has nothing to show for. This gives it
## the two shapes a child group arrives in -- a simple condition and a
## compound expression -- plus one that carries neither.
.library_model_nested <- function() {
  ars <- .read_json(test_path("fixtures", "ars_apx_drm_301_deterministic.json"))
  index <- which(vapply(
    ars$analysisGroupings,
    function(node) identical(node$id, "GF_TRT01A"),
    logical(1)
  ))

  drug_a <- list(dataset = "ADSL", variable = "TRT01A",
                 comparator = "EQ", value = list("Drug A"))
  drug_b <- list(dataset = "ADSL", variable = "TRT01A",
                 comparator = "EQ", value = list("Drug B"))

  ars$analysisGroupings[[index]]$dataDriven <- FALSE
  ars$analysisGroupings[[index]]$groups <- list(
    list(id = "GRP_DRUG_A", name = "Drug A", label = "Drug A",
         level = 1L, order = 1L, condition = drug_a),
    list(id = "GRP_EITHER", name = "Either drug", label = "Either drug",
         level = 1L, order = 2L,
         compoundExpression = list(
           logicalOperator = "OR",
           whereClauses = list(list(condition = drug_a),
                               list(condition = drug_b))
         )),
    list(id = "GRP_UNDEFINED", name = "Undefined", label = "Undefined",
         level = 1L, order = 3L)
  )
  ars_to_model(ars)
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

test_that("a grouping's child groups are listed with their conditions", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")

  state <- .editor_state(.library_model_nested(), NULL, NULL, NULL, "edit")

  shiny::testServer(mod_entity_library_server, args = list(state = state), {
    session$setInputs(table_groupings_rows_selected = 1)
    rendered <- paste(as.character(output$detail_groupings), collapse = " ")

    ## The raw-JSON hatch already carried the labels and ids, so only a
    ## readable filter expression proves the child groups are shown as
    ## themselves rather than buried in the node.
    expect_match(rendered, "Groups", fixed = TRUE)
    expect_match(rendered, "TRT01A %in%", fixed = TRUE)

    ## A compound child reads as the whole expression, not as "compound".
    expect_match(rendered, ") | (", fixed = TRUE)
  })
})

test_that("a child group with no condition of its own shows none", {
  model <- .library_model_nested()
  index <- match("GF_TRT01A", model$groupings$id)
  groups <- .groups_table(model$groupings$raw[[index]]$groups)

  expect_identical(nrow(groups), 3L)
  expect_identical(groups$id, c("GRP_DRUG_A", "GRP_EITHER", "GRP_UNDEFINED"))
  expect_identical(groups$order, c(1L, 2L, 3L))

  ## An absent where-clause is no condition at all. Passing it to
  ## where_to_filter_expr() would call it TRUE, which reads as "every row".
  expect_identical(groups$condition[[3]], NA_character_)
})

test_that("a data-driven grouping says where its levels come from", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")

  ## View mode, so the read-only panel is exercised alongside the editable
  ## one the test above drives.
  state <- .editor_state(.valid_fixture_model(), NULL, NULL, NULL, "view")

  shiny::testServer(mod_entity_library_server, args = list(state = state), {
    session$setInputs(table_groupings_rows_selected = 1)
    rendered <- paste(as.character(output$detail_groupings), collapse = " ")

    expect_match(rendered, "from the data", fixed = TRUE)
    expect_false(grepl("Apply JSON", rendered, fixed = TRUE))
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


## --- Editing child groups in place -----------------------------------------
##
## Child inputs are rendered but NOT observed: they are read by name inside
## one isolated handler when a Save button fires. Per-keystroke observers
## would not survive here -- a child write is a node write, so it goes through
## .record_structural_edit(), which bumps state$refresh(), which re-renders
## the panel and destroys the very input being typed into.

.cge_state <- function(mode = "edit") {
  .editor_state(.library_model_nested(), NULL, NULL, NULL, mode)
}

.cge_groups <- function(state, grouping_id = "GF_TRT01A") {
  shiny::isolate({
    model <- state$model()
    model$groupings$raw[[match(grouping_id, model$groupings$id)]]$groups
  })
}

test_that("a simple child gets condition inputs and a compound one does not", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  state <- .cge_state()
  shiny::testServer(mod_entity_library_server, args = list(state = state), {
    session$setInputs(table_groupings_rows_selected = 1)
    rendered <- paste(as.character(output$detail_groupings), collapse = " ")

    expect_match(
      rendered,
      .entity_input_id("groupings", "GF_TRT01A", "condition_variable",
                       child = "GRP_DRUG_A"),
      fixed = TRUE)
    ## A compound child stays read-only: replacing it with a simple condition
    ## would drop the other clauses.
    expect_false(grepl(
      .entity_input_id("groupings", "GF_TRT01A", "condition_variable",
                       child = "GRP_EITHER"),
      rendered, fixed = TRUE))
  })
})

test_that("saving one child leaves its siblings byte-identical", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  state <- .cge_state()
  before <- .cge_groups(state)

  shiny::testServer(mod_entity_library_server, args = list(state = state), {
    session$setInputs(table_groupings_rows_selected = 1)
    field <- function(name) {
      .entity_input_id("groupings", "GF_TRT01A", name, child = "GRP_DRUG_A")
    }
    .set_input(session, field("label"), "Drug A only")
    .set_input(session, field("condition_dataset"), "ADSL")
    .set_input(session, field("condition_variable"), "TRT01A")
    .set_input(session, field("condition_comparator"), "IN")
    .set_input(session, field("condition_value"), "Drug A;Drug A 10 mg")
    session$setInputs(group_apply = list(grouping = "GF_TRT01A",
                                         group = "GRP_DRUG_A"))
    session$flushReact()
  })

  after <- .cge_groups(state)
  expect_equal(after[[1]]$label, "Drug A only")
  expect_equal(after[[1]]$condition$comparator, "IN")
  expect_equal(after[[1]]$condition$value, list("Drug A", "Drug A 10 mg"))

  ## The H3 guarantee, through the UI this time.
  expect_equal(after[[2]], before[[2]])
  expect_equal(after[[3]], before[[3]])
  expect_length(after, 3L)

  ## One action, one log row -- and the child id is in `field`, because
  ## .diff_summary() collapses on pool|id|field.
  log <- shiny::isolate(state$edit_log())
  expect_equal(nrow(log), 1L)
  expect_true(grepl("GRP_DRUG_A", log$field[1], fixed = TRUE))

  ## And it round-trips.
  ars <- model_to_ars(shiny::isolate(state$model()))
  node <- Filter(function(g) identical(g$id, "GF_TRT01A"),
                 ars$analysisGroupings)[[1]]
  expect_equal(node$groups[[1]]$label, "Drug A only")
})

test_that("two child edits stay two lines in the save summary", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  state <- .cge_state()
  shiny::testServer(mod_entity_library_server, args = list(state = state), {
    session$setInputs(table_groupings_rows_selected = 1)
    for (child in c("GRP_DRUG_A", "GRP_UNDEFINED")) {
      .set_input(session,
                 .entity_input_id("groupings", "GF_TRT01A", "label",
                                  child = child),
                 paste("Renamed", child))
      session$setInputs(group_apply = list(grouping = "GF_TRT01A",
                                           group = child))
      session$flushReact()
    }
  })

  summary <- .diff_summary(shiny::isolate(state$edit_log()))
  expect_equal(nrow(summary), 2L)
})

test_that("a child edit can be undone", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  state <- .cge_state()
  before <- .cge_groups(state)[[1]]$label

  shiny::testServer(mod_entity_library_server, args = list(state = state), {
    session$setInputs(table_groupings_rows_selected = 1)
    .set_input(session,
               .entity_input_id("groupings", "GF_TRT01A", "label",
                                child = "GRP_DRUG_A"),
               "Changed")
    session$setInputs(group_apply = list(grouping = "GF_TRT01A",
                                         group = "GRP_DRUG_A"))
    session$flushReact()
  })
  expect_equal(.cge_groups(state)[[1]]$label, "Changed")

  shiny::isolate(.undo(state))
  expect_equal(.cge_groups(state)[[1]]$label, before)
  expect_equal(nrow(shiny::isolate(state$edit_log())), 0L)
})

test_that("a grouping left fixed and childless says so, and offers the flip", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  ## The frozen fixture's grouping is data-driven with no children. Turning
  ## the mode off makes it a fixed grouping that defines no result columns --
  ## allowed, logged, undoable, and unsaveable until it is resolved.
  state <- .editor_state(.valid_fixture_model(), NULL, NULL, NULL, "edit")

  shiny::testServer(mod_entity_library_server, args = list(state = state), {
    session$setInputs(table_groupings_rows_selected = 1)
    .set_input(session,
               .entity_input_id("groupings", "GF_TRT01A", "dataDriven"),
               FALSE)
    session$flushReact()

    findings <- state$findings()
    expect_true(any(findings$ref %in% "FIXED_GROUPING_EMPTY"))

    rendered <- paste(as.character(output$detail_groupings), collapse = " ")
    expect_match(rendered, "defines no result columns", fixed = TRUE)
    expect_match(rendered, "Mark data-driven", fixed = TRUE)

    ## The offered fix is an explicit, logged action -- never automatic.
    session$setInputs(group_make_data_driven = list(grouping = "GF_TRT01A"))
    session$flushReact()
    expect_false(any(state$findings()$ref %in% "FIXED_GROUPING_EMPTY"))
  })
})

test_that("a fixed childless grouping cannot be saved", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  state <- .editor_state(.valid_fixture_model(), NULL, NULL, NULL, "edit")
  gate <- shiny::isolate({
    state$model(model_set_field(state$model(), "groupings", "GF_TRT01A",
                                "dataDriven", FALSE))
    .model_validation_gate(state$model(), state$spec, state$report)
  })
  expect_true(gate$blocked)
  expect_true("FIXED_GROUPING_EMPTY" %in% gate$blocking_refs)
})


## --- Child-group CRUD ------------------------------------------------------

test_that("a child group can be added from the panel", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  state <- .cge_state()
  shiny::testServer(mod_entity_library_server, args = list(state = state), {
    session$setInputs(table_groupings_rows_selected = 1)
    ## Two steps, as in the app: the modal opens, and only then is it
    ## confirmed. Firing both in one flush would let the confirm run before
    ## the modal recorded which grouping it belongs to.
    session$setInputs(group_add = list(grouping = "GF_TRT01A"))
    session$flushReact()
    session$setInputs(
      new_group_label = "Drug C",
      new_group_dataset = "ADSL",
      new_group_variable = "TRT01A",
      new_group_comparator = "EQ",
      new_group_value = "Drug C",
      confirm_group_add = 1
    )
    session$flushReact()
  })

  groups <- .cge_groups(state)
  expect_length(groups, 4L)
  expect_equal(groups[[4]]$label, "Drug C")
  expect_equal(groups[[4]]$condition$value, list("Drug C"))
  expect_equal(groups[[4]]$order, 4L)

  log <- shiny::isolate(state$edit_log())
  expect_equal(nrow(log), 1L)
  expect_true(grepl("GRP_TRT01A_DRUG_C", log$field[1], fixed = TRUE))
  expect_equal(log$new[1], "(added)")
})

test_that("a child group can be cloned and the original left alone", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  state <- .cge_state()
  before <- .cge_groups(state)[[1]]

  shiny::testServer(mod_entity_library_server, args = list(state = state), {
    session$setInputs(table_groupings_rows_selected = 1)
    session$setInputs(group_action = list(grouping = "GF_TRT01A",
                                          group = "GRP_DRUG_A",
                                          action = "clone"))
    session$flushReact()
  })

  groups <- .cge_groups(state)
  expect_length(groups, 4L)
  expect_equal(groups[[1]], before)
  ## The copy sits next to what it was copied from, with its own id.
  expect_match(groups[[2]]$label, "copy")
  expect_false(identical(groups[[2]]$id, before$id))
  expect_equal(groups[[2]]$condition, before$condition)
})

test_that("child groups reorder from the panel", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  state <- .cge_state()
  shiny::testServer(mod_entity_library_server, args = list(state = state), {
    session$setInputs(table_groupings_rows_selected = 1)
    session$setInputs(group_action = list(grouping = "GF_TRT01A",
                                          group = "GRP_UNDEFINED",
                                          action = "up"))
    session$flushReact()
  })

  groups <- .cge_groups(state)
  expect_equal(vapply(groups, function(g) g$id, ""),
               c("GRP_DRUG_A", "GRP_UNDEFINED", "GRP_EITHER"))
  orders <- vapply(groups, function(g) as.integer(g$order), integer(1))
  expect_equal(orders, 1:3)
})

test_that("deleting the last child of a fixed grouping is allowed and blocked", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  ## Allowed, because "clear these and define new ones" is a real sequence.
  ## Blocked at the save, because the model it produces defines no columns.
  state <- .cge_state()
  shiny::testServer(mod_entity_library_server, args = list(state = state), {
    session$setInputs(table_groupings_rows_selected = 1)
    for (child in c("GRP_DRUG_A", "GRP_EITHER", "GRP_UNDEFINED")) {
      session$setInputs(group_action = list(grouping = "GF_TRT01A",
                                            group = child,
                                            action = "delete"))
      session$flushReact()
    }

    expect_length(.cge_groups(state), 0L)
    expect_true(any(state$findings()$ref %in% "FIXED_GROUPING_EMPTY"))

    rendered <- paste(as.character(output$detail_groupings), collapse = " ")
    expect_match(rendered, "Mark data-driven", fixed = TRUE)
  })
})

test_that("a child named by a result path is refused, with the output named", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  model <- .library_model_nested()
  node <- model$outputs$raw[[1]]
  node$resultGroupPaths <- list(paths = list(list(
    pathId = "P1", groupIds = list("GRP_DRUG_A")
  )))
  model$outputs$raw[[1]] <- node
  state <- .editor_state(model, NULL, NULL, NULL, "edit")

  shiny::testServer(mod_entity_library_server, args = list(state = state), {
    session$setInputs(table_groupings_rows_selected = 1)
    session$setInputs(group_action = list(grouping = "GF_TRT01A",
                                          group = "GRP_DRUG_A",
                                          action = "delete"))
    session$flushReact()
  })

  ## Nothing removed, and nothing logged as if it had been.
  expect_length(.cge_groups(state), 3L)
  expect_equal(nrow(shiny::isolate(state$edit_log())), 0L)
})

test_that("every child action is undoable, one step each", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  state <- .cge_state()
  shiny::testServer(mod_entity_library_server, args = list(state = state), {
    session$setInputs(table_groupings_rows_selected = 1)
    session$setInputs(group_action = list(grouping = "GF_TRT01A",
                                          group = "GRP_UNDEFINED",
                                          action = "delete"))
    session$flushReact()
    expect_length(.cge_groups(state), 2L)

    .undo(state)
    expect_length(.cge_groups(state), 3L)
    expect_equal(nrow(state$edit_log()), 0L)
  })
})
