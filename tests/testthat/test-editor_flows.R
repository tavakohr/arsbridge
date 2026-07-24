## The editor's server flows, driven end to end under shiny::testServer().
##
## Until now these paths -- the editable panel's inputs, the structural
## observers, the save/undo/recovery module, the add-analysis wizard -- were
## verified in a real browser, which coverage instrumentation cannot see.
## These tests drive the same flows through the mock session, so a regression
## fails here first and the coverage report reflects what is actually
## exercised.

.flows_model <- function() {
  ars_to_model(test_path("fixtures", "ars_apx_drm_301_deterministic.json"))
}

.flows_state <- function(mode = "edit", model = NULL, spec = NULL,
                         report = NULL, source_path = NULL) {
  if (is.null(model)) model <- .flows_model()
  .editor_state(model, spec, report, source_path, mode)
}

.flows_spec <- function() {
  parse_adam_spec(arsbridge_example("adam_spec.xlsx"))
}


## --- the editable analysis panel --------------------------------------------

test_that("the edit panel renders and every field observer writes through", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  state <- .flows_state(spec = .flows_spec())

  shiny::testServer(mod_detail_server, args = list(state = state), {
    target <- shiny::isolate(state$model())$analyses$id[1]
    state$selected(list(pool = "analyses", id = target))
    session$flushReact()

    ## The editable panel builds (detach controls, method choices, dropdowns).
    rendered <- paste(as.character(output$detail), collapse = " ")
    expect_match(rendered, "Remove line")
    expect_match(rendered, "shared by")

    ## One field at a time, the way the inputs fire them.
    session$setInputs(label = "Driven label")
    session$setInputs(description = "Driven description")
    session$setInputs(dataset = "ADSL")
    session$setInputs(variable = "AGE")
    session$setInputs(analysisSetId = "AS_SAFETY_POPULATION_ADSL_SAFFL_Y")
    session$setInputs(dataSubsetId = "")
    session$setInputs(reason = "SPECIFIED IN PROTOCOL")
    session$setInputs(purpose = "PRIMARY OUTCOME MEASURE")
    session$setInputs(strata = "SEX")
    session$setInputs(includeTotal = TRUE)
    session$setInputs(grouping_ids = "GF_TRT01A")

    row <- state$model()$analyses
    row <- row[row$id == target, ]
    expect_equal(row$label, "Driven label")
    expect_equal(row$variable, "AGE")
    expect_equal(row$reason, "SPECIFIED IN PROTOCOL")
    expect_equal(row$purpose, "PRIMARY OUTCOME MEASURE")
    expect_equal(row$strata, "SEX")
    expect_true(row$includeTotal)

    ## Every genuine change was logged; the echoes were not.
    expect_gt(nrow(state$edit_log()), 5)
  })
})

test_that("choosing a catalogue method inserts it before pointing at it", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  state <- .flows_state()

  shiny::testServer(mod_detail_server, args = list(state = state), {
    target <- shiny::isolate(state$model())$analyses$id[1]
    state$selected(list(pool = "analyses", id = target))
    session$flushReact()

    expect_false(
      "MTH_KAPLAN_MEIER_ESTIMATE" %in% shiny::isolate(state$model())$methods$id
    )
    session$setInputs(methodId = "MTH_KAPLAN_MEIER_ESTIMATE")

    model <- state$model()
    expect_true("MTH_KAPLAN_MEIER_ESTIMATE" %in% model$methods$id)
    expect_equal(model$analyses$methodId[model$analyses$id == target],
                 "MTH_KAPLAN_MEIER_ESTIMATE")
    expect_equal(sum(validate_ars_model(model)$severity == "FAIL"), 0)
  })
})

test_that("the output panel renders its ordered lines with the edit controls", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  state <- .flows_state()

  shiny::testServer(mod_detail_server, args = list(state = state), {
    state$selected(list(pool = "outputs", id = "T_14_1_2"))
    session$flushReact()

    rendered <- paste(as.character(output$detail), collapse = " ")
    expect_match(rendered, "Add analysis")
    expect_match(rendered, "list-group-item")
  })
})

test_that("the structural observers move, remove and detach", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  state <- .flows_state()

  shiny::testServer(mod_detail_server, args = list(state = state), {
    model <- shiny::isolate(state$model())
    refs <- .split_values(
      model$outputs$referenced_analysis_ids[model$outputs$id == "T_14_1_2"]
    )

    ## Add: the panel's button files a request for the wizard.
    session$setInputs(add_to_output = list(pool = "outputs", id = "T_14_1_2"))
    expect_equal(state$add_request()$output_id, "T_14_1_2")

    ## Move: second line up, visible in the model's reference order.
    session$setInputs(move = list(output = "T_14_1_2", id = refs[2],
                                  offset = -1))
    moved <- .split_values(
      state$model()$outputs$referenced_analysis_ids[
        state$model()$outputs$id == "T_14_1_2"
      ]
    )
    expect_equal(moved[1], refs[2])

    ## Remove: confirmation first, then the line and its references go.
    session$setInputs(remove_analysis = list(pool = "analyses", id = refs[3]))
    session$setInputs(confirm_remove = list(pool = "analyses", id = refs[3]))
    expect_false(refs[3] %in% state$model()$analyses$id)
    expect_null(state$selected())

    ## Detach: this line gets its own copy of the shared population.
    target <- state$model()$analyses$id[1]
    shared <- state$model()$analyses$analysisSetId[1]
    session$setInputs(detach = list(pool = "analysis_sets", entity = shared,
                                    id = target))
    repointed <- state$model()$analyses
    expect_match(repointed$analysisSetId[repointed$id == target], "_VARIANT")

    ## Selecting from inside the panel routes like the tree.
    session$setInputs(selected = list(pool = "analyses", id = target))
    expect_equal(state$selected()$id, target)

    ## Four structural edits, all logged and all undoable.
    expect_gte(nrow(state$edit_log()), 3)
    expect_true(.can_undo(state))
  })
})


## --- save, discard, undo, recovery -------------------------------------------

test_that("the save module tracks dirtiness, undoes, redoes and confirms", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  state <- .flows_state()

  shiny::testServer(mod_save_server, args = list(state = state), {
    ## Clean: no badge, both history buttons disabled.
    expect_match(paste(as.character(output$dirty), collapse = " "),
                 "No changes yet")
    expect_match(paste(as.character(output$history), collapse = " "),
                 "disabled")

    ## One edit: badge counts it, undo becomes available.
    target <- shiny::isolate(state$model())$analyses$id[1]
    apply_edit(state, "analyses", target, "label", "Dirty now")
    session$flushReact()
    expect_match(paste(as.character(output$dirty), collapse = " "),
                 "1 unsaved change")

    session$setInputs(undo = 1)
    expect_equal(nrow(state$edit_log()), 0)
    session$setInputs(redo = 1)
    expect_equal(nrow(state$edit_log()), 1)

    ## Save opens the confirmation (diff table built from the log), and
    ## confirming stops the app with the model and log as its value.
    session$setInputs(save = 1)
    session$setInputs(confirm_save = 1)

    ## Discard with a dirty log asks first; confirming returns nothing.
    session$setInputs(discard = 1)
    session$setInputs(confirm_discard = 1)
  })
})

test_that("a crashed session's work is offered back and restorable", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  dir <- withr::local_tempdir()
  path <- file.path(dir, "reporting_event.json")
  file.copy(test_path("fixtures", "ars_apx_drm_301_deterministic.json"), path)
  withr::defer(.clear_autosave(path))

  ## A previous session that died with one unsaved change.
  shiny::reactiveConsole(TRUE)
  earlier <- .flows_state(source_path = path)
  apply_edit(earlier, "analyses",
             shiny::isolate(earlier$model())$analyses$id[1],
             "label", "Recovered work")
  shiny::reactiveConsole(FALSE)
  expect_false(is.null(.read_autosave(path)))

  ## A fresh session on the same file offers the work back.
  state <- .flows_state(source_path = path)
  shiny::testServer(mod_save_server, args = list(state = state), {
    session$setInputs(accept_recovery = 1)
    model <- state$model()
    expect_equal(model$analyses$label[1], "Recovered work")
    expect_equal(nrow(state$edit_log()), 1)
  })

  ## Declining instead clears the recovery copy.
  shiny::reactiveConsole(TRUE)
  again <- .flows_state(source_path = path)
  apply_edit(again, "analyses",
             shiny::isolate(again$model())$analyses$id[2],
             "label", "Second crash")
  shiny::reactiveConsole(FALSE)

  fresh <- .flows_state(source_path = path)
  shiny::testServer(mod_save_server, args = list(state = fresh), {
    session$setInputs(discard_recovery = 1)
  })
  expect_null(.read_autosave(path))
})


## --- the add-analysis wizard --------------------------------------------------

test_that("the wizard refuses a line with no variable, then adds a real one", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  state <- .flows_state(spec = .flows_spec())

  shiny::testServer(mod_add_analysis_server, args = list(state = state), {
    before <- nrow(shiny::isolate(state$model())$analyses)

    state$add_request(.add_request("T_14_1_2"))
    session$flushReact()

    ## No variable chosen: refused, with the reason rendered in the dialog.
    session$setInputs(variable = "", label = "", confirm_add = 1)
    expect_match(paste(as.character(output$variable_problem), collapse = " "),
                 "Choose the variable")
    expect_equal(nrow(state$model()$analyses), before)

    ## The dataset picker rescopes the variable choices.
    session$setInputs(dataset = "ADAE")

    ## A real line, at a chosen position, with a label falling back sensibly.
    session$setInputs(
      dataset = "ADSL", variable = "SMOKFL", label = "",
      methodId = "MTH_COUNT_AND_PERCENTAGE",
      analysisSetId = shiny::isolate(state$model())$analysis_sets$id[1],
      dataSubsetId = "", grouping_ids = "GF_TRT01A",
      includeTotal = FALSE, after = ""
    )
    session$setInputs(confirm_add = 2)

    model <- state$model()
    expect_equal(nrow(model$analyses), before + 1)
    added <- state$selected()
    expect_equal(added$pool, "analyses")
    expect_equal(model$analyses$label[model$analyses$id == added$id], "SMOKFL")
    expect_null(state$add_request())
    expect_equal(sum(validate_ars_model(model)$severity == "FAIL"), 0)
  })
})

test_that("the wizard can pull a method in from the standard catalogue", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  state <- .flows_state()

  shiny::testServer(mod_add_analysis_server, args = list(state = state), {
    state$add_request(.add_request("T_14_1_1"))
    session$flushReact()

    session$setInputs(
      dataset = "ADSL", variable = "AGE", label = "KM line",
      methodId = "MTH_KAPLAN_MEIER_ESTIMATE",
      analysisSetId = shiny::isolate(state$model())$analysis_sets$id[1],
      dataSubsetId = "", grouping_ids = "GF_TRT01A",
      includeTotal = FALSE, after = ""
    )
    session$setInputs(confirm_add = 1)

    expect_true("MTH_KAPLAN_MEIER_ESTIMATE" %in% state$model()$methods$id)
  })
})

test_that("cancelling the wizard files nothing", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  state <- .flows_state()

  shiny::testServer(mod_add_analysis_server, args = list(state = state), {
    before <- nrow(shiny::isolate(state$model())$analyses)
    state$add_request(.add_request("T_14_1_1"))
    session$flushReact()

    session$setInputs(cancel_add = 1)
    expect_null(state$add_request())
    expect_equal(nrow(state$model()$analyses), before)
  })
})


## --- the surrounding panels ---------------------------------------------------

test_that("the validation summary reads clean or counts, as findings dictate", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  state <- .flows_state()

  shiny::testServer(mod_validation_server, args = list(state = state), {
    rendered <- paste(as.character(output$summary), collapse = " ")
    expect_match(rendered, "To review|Blocking")

    state$findings(.new_findings())
    session$flushReact()
    expect_match(paste(as.character(output$summary), collapse = " "),
                 "Nothing to fix")
  })
})

test_that("the JSON tab renders only when asked", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  state <- .flows_state()

  shiny::testServer(mod_json_server, args = list(state = state), {
    session$setInputs(render = 0)
    expect_null(output$json)

    session$setInputs(render = 1)
    expect_match(paste(as.character(output$json), collapse = " "),
                 "mainListOfContents")
  })
})

test_that("the status header summarizes safety, with save controls in edit mode", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  viewer <- .flows_state(mode = "view")
  shiny::testServer(mod_status_server, args = list(state = viewer), {
    rendered <- paste(as.character(output$status), collapse = " ")
    expect_match(rendered, "No blocking problems")
    expect_false(grepl("Save and close", rendered))
  })

  editor <- .flows_state(mode = "edit")
  editor$findings(rbind(
    shiny::isolate(editor$findings()),
    data.frame(severity = "FAIL", entity = "analyses", id = "AN_X",
               field = "methodId", problem = "x", action = "y",
               ref = NA_character_, stringsAsFactors = FALSE)
  ))
  shiny::testServer(mod_status_server, args = list(state = editor), {
    rendered <- paste(as.character(output$status), collapse = " ")
    expect_match(rendered, "1 blocking problem")
    expect_match(rendered, "Save and close")
  })
})

test_that("the entity library's editable panels render for every pool", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  ## A compound subset so that branch of the panel renders too.
  ars <- .read_json(test_path("fixtures",
                              "ars_apx_drm_301_deterministic.json"))
  ars$dataSubsets[[1]] <- list(
    id = ars$dataSubsets[[1]]$id, name = "Compound", label = "Compound",
    compoundExpression = list(
      logicalOperator = "AND",
      whereClauses = list(
        list(condition = list(dataset = "ADSL", variable = "AGE",
                              comparator = "GE", value = list("18"))),
        list(condition = list(dataset = "ADSL", variable = "AGE",
                              comparator = "LE", value = list("65")))
      )
    ),
    level = 1L, order = 1L
  )
  state <- .flows_state(model = ars_to_model(ars))

  shiny::testServer(mod_entity_library_server, args = list(state = state), {
    session$setInputs(table_methods_rows_selected = 1)
    expect_match(paste(as.character(output$detail_methods), collapse = " "),
                 "Operations")

    session$setInputs(table_groupings_rows_selected = 1)
    expect_match(paste(as.character(output$detail_groupings), collapse = " "),
                 "groupingVariable")

    session$setInputs(table_analysis_sets_rows_selected = 1)
    expect_match(
      paste(as.character(output$detail_analysis_sets), collapse = " "),
      "condition"
    )

    ## The compound subset shows its rendered condition, not editable fields.
    session$setInputs(table_data_subsets_rows_selected = 1)
    expect_match(
      paste(as.character(output$detail_data_subsets), collapse = " "),
      "compound"
    )
  })
})

test_that("the read-only library panels render for methods and groupings too", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  state <- .flows_state(mode = "view")

  shiny::testServer(mod_entity_library_server, args = list(state = state), {
    session$setInputs(table_methods_rows_selected = 1)
    expect_match(paste(as.character(output$detail_methods), collapse = " "),
                 "Executed as")

    session$setInputs(table_groupings_rows_selected = 1)
    expect_match(paste(as.character(output$detail_groupings), collapse = " "),
                 "Data driven")
  })
})


## --- the launchers, with the app run mocked out -------------------------------

test_that("view_ars() runs its full body and returns nothing", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  launched <- FALSE
  result <- testthat::with_mocked_bindings(
    runApp = function(...) { launched <<- TRUE; NULL },
    .package = "shiny",
    view_ars(
      test_path("fixtures", "ars_apx_drm_301_deterministic.json"),
      adam_spec_path = arsbridge_example("adam_spec.xlsx"),
      report_path = test_path("fixtures", "ars_apx_drm_301_validation.csv")
    )
  )

  expect_true(launched)
  expect_null(result)
})

test_that("edit_ars() closed without saving writes nothing", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  dir <- withr::local_tempdir()
  path <- file.path(dir, "reporting_event.json")
  file.copy(test_path("fixtures", "ars_apx_drm_301_deterministic.json"), path)
  before <- readLines(path, warn = FALSE)

  result <- suppressMessages(testthat::with_mocked_bindings(
    runApp = function(...) NULL,
    .package = "shiny",
    edit_ars(path)
  ))

  expect_null(result)
  expect_identical(readLines(path, warn = FALSE), before)
  expect_equal(length(list.files(dir)), 1)
})

test_that("edit_ars() on a spec_to_ars() result saves through the full path", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  dir <- withr::local_tempdir()
  out_path <- file.path(dir, "corrected.json")

  fixture <- test_path("fixtures", "ars_apx_drm_301_deterministic.json")
  fake_result <- list(
    reporting_event = .read_json(fixture),
    validation      = utils::read.csv(
      test_path("fixtures", "ars_apx_drm_301_validation.csv"),
      stringsAsFactors = FALSE
    ),
    ars_path        = fixture,
    report_path     = NULL,
    adam_spec_path  = arsbridge_example("adam_spec.xlsx")
  )

  ## The mocked session returns one edit, as Save and close would.
  written <- suppressMessages(testthat::with_mocked_bindings(
    runApp = function(app) {
      model <- ars_to_model(fixture)
      target <- model$analyses$id[1]
      list(
        model    = model_set_field(model, "analyses", target, "label",
                                   "Edited in mock"),
        edit_log = data.frame(
          time = "2026-07-23T00:00:00Z", pool = "analyses", id = target,
          field = "label", old = "Randomized", new = "Edited in mock",
          stringsAsFactors = FALSE
        ),
        source_path = NULL
      )
    },
    .package = "shiny",
    edit_ars(fake_result, output_path = out_path)
  ))

  expect_equal(written, out_path)
  saved <- .read_json(out_path)
  expect_equal(saved$analyses[[1]]$label, "Edited in mock")
  expect_true(file.exists(file.path(dir, "corrected.edits.json")))
})

test_that("a report workbook without a Validation sheet is declined politely", {
  skip_if_not_installed("openxlsx2")

  path <- withr::local_tempfile(fileext = ".xlsx")
  wb <- openxlsx2::wb_workbook()
  wb$add_worksheet("Something else")
  wb$add_data(sheet = "Something else", x = data.frame(x = 1))
  openxlsx2::wb_save(wb, file = path)

  expect_warning(report <- .read_validation_report(path), "No .* sheet")
  expect_null(report)
})


## --- small builders that only a browser used to touch --------------------------

test_that("the ui builders and js helpers produce what the app mounts", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  expect_s3_class(mod_tree_ui("t"), "shiny.tag.list")
  expect_s3_class(mod_detail_ui("d"), "shiny.tag")
  expect_s3_class(mod_entity_library_ui("l"), "shiny.tag")
  expect_s3_class(mod_validation_ui("v"), "shiny.tag.list")
  expect_s3_class(mod_json_ui("j"), "shiny.tag.list")
  expect_s3_class(mod_status_ui("s"), "shiny.tag")
  expect_s3_class(mod_save_ui("sv"), "shiny.tag.list")

  expect_match(.select_js("id", "analyses", "AN_1"), "priority: 'event'")
  expect_match(.event_js("id"), "Date.now")
  expect_match(.detach_js("id", "methods", "M", "A"), "entity: 'M'")
  expect_match(.move_js("id", "T", "A", -1), "offset: -1")

  expect_null(.severity_badge(NA_character_))
  expect_match(as.character(.severity_badge("FAIL")), "danger")
  expect_match(as.character(.detail_row("Label", NULL)), "--")
  expect_match(as.character(.json_block(list(a = 1))), "pre")
})
