## The editing wiring: apply_edit(), the dropdown choices, and the
## diff-before-save summary.
##
## apply_edit() carries the rule that makes editing behave under Shiny: an
## input echoing back its own value is not an edit. Without that, every
## selection change would log a phantom change.
##
## The state is a set of reactive values, so these tests turn on shiny's
## console reactivity rather than standing up a session.

.edit_state <- function(model = NULL, spec = NULL, report = NULL) {
  if (is.null(model)) {
    model <- ars_to_model(
      test_path("fixtures", "ars_apx_drm_301_deterministic.json")
    )
  }
  .editor_state(model, spec, report, NULL, "edit")
}

.edit_log_row <- function(id, field = "label", old = "before", new = "after") {
  data.frame(
    time = "2026-07-23T00:00:00Z", pool = "analyses", id = id,
    field = field, old = old, new = new,
    stringsAsFactors = FALSE
  )
}


test_that("an edit updates the model and records one log row", {
  skip_if_not_installed("shiny")
  withr::local_options(shiny.suppressMissingContextError = TRUE)
  shiny::reactiveConsole(TRUE)
  withr::defer(shiny::reactiveConsole(FALSE))

  state <- .edit_state()
  target <- state$model()$analyses$id[1]

  expect_true(apply_edit(state, "analyses", target, "label", "Renamed"))
  expect_equal(state$model()$analyses$label[1], "Renamed")

  log <- state$edit_log()
  expect_equal(nrow(log), 1)
  expect_equal(log$id, target)
  expect_equal(log$field, "label")
  expect_equal(log$new, "Renamed")
})

test_that("writing the same value again is not an edit", {
  skip_if_not_installed("shiny")
  shiny::reactiveConsole(TRUE)
  withr::defer(shiny::reactiveConsole(FALSE))

  state <- .edit_state()
  target <- state$model()$analyses$id[1]
  current <- state$model()$analyses$label[1]

  expect_false(apply_edit(state, "analyses", target, "label", current))
  expect_equal(nrow(state$edit_log()), 0)

  ## A genuine change still registers afterwards.
  expect_true(apply_edit(state, "analyses", target, "label", "Different"))
  expect_equal(nrow(state$edit_log()), 1)
})

test_that("an edit re-runs validation", {
  skip_if_not_installed("shiny")
  shiny::reactiveConsole(TRUE)
  withr::defer(shiny::reactiveConsole(FALSE))

  state <- .edit_state()
  target <- state$model()$analyses$id[1]
  expect_equal(sum(state$findings()$severity == "FAIL"), 0)

  apply_edit(state, "analyses", target, "methodId", "MTH_GONE")

  findings <- state$findings()
  expect_gt(sum(findings$severity == "FAIL"), 0)
  expect_true(any(grepl("MTH_GONE", findings$problem)))
})

test_that("clearing an optional field removes the key on save", {
  skip_if_not_installed("shiny")
  shiny::reactiveConsole(TRUE)
  withr::defer(shiny::reactiveConsole(FALSE))

  state <- .edit_state()
  model <- state$model()
  target <- model$analyses$id[!is.na(model$analyses$strata)][1]
  index <- which(model$analyses$id == target)

  apply_edit(state, "analyses", target, "strata", NA_character_)

  ars <- model_to_ars(state$model())
  expect_false("strata" %in% names(ars$analyses[[index]]))
  expect_equal(state$edit_log()$new, "(not set)")
})

test_that("an edit to an entity that is gone is ignored", {
  skip_if_not_installed("shiny")
  shiny::reactiveConsole(TRUE)
  withr::defer(shiny::reactiveConsole(FALSE))

  state <- .edit_state()
  expect_false(apply_edit(state, "analyses", "AN_GONE", "label", "x"))
  expect_equal(nrow(state$edit_log()), 0)
})

test_that("choosing a catalogue method adds it to the file first", {
  skip_if_not_installed("shiny")
  shiny::reactiveConsole(TRUE)
  withr::defer(shiny::reactiveConsole(FALSE))

  ## A hand-built event with only one method, so the catalogue is non-empty.
  ars <- list(
    id = "S", name = "S", version = "1",
    methods = list(list(id = "MTH_COUNT_AND_PERCENTAGE",
                        name = "Count and Percentage",
                        operations = list())),
    analyses = list(list(id = "AN_1", label = "Line",
                         methodId = "MTH_COUNT_AND_PERCENTAGE",
                         dataset = "ADSL", variable = "SEX")),
    outputs = list(list(id = "T_1", name = "T-1",
                        referencedAnalysisIds = list("AN_1")))
  )

  state <- .edit_state(ars_to_model(ars))
  expect_false("MTH_SUBJECT_COUNT" %in% state$model()$methods$id)

  ## What the methodId observer does when a catalogue entry is picked.
  state$model(
    model_add_method_from_catalogue(state$model(), "MTH_SUBJECT_COUNT")
  )
  apply_edit(state, "analyses", "AN_1", "methodId", "MTH_SUBJECT_COUNT")

  model <- state$model()
  expect_true("MTH_SUBJECT_COUNT" %in% model$methods$id)
  expect_equal(model$analyses$methodId[1], "MTH_SUBJECT_COUNT")

  ## The reference resolves, so no dangling-method finding.
  expect_equal(sum(state$findings()$severity == "FAIL"), 0)
})

test_that("entity dropdowns say how many analyses share each entity", {
  model <- ars_to_model(
    test_path("fixtures", "ars_apx_drm_301_deterministic.json")
  )

  choices <- .entity_choices(model, "analysis_sets")
  expect_equal(length(choices), nrow(model$analysis_sets))
  expect_true(any(grepl("shared by", names(choices))))

  ## Data subsets offer an explicit "none", which is the empty-string
  ## sentinel the model uses.
  subsets <- .entity_choices(model, "data_subsets", include_none = TRUE)
  expect_equal(subsets[[1]], "")
  expect_match(names(subsets)[1], "None")
})

test_that("method choices separate what is in the file from the catalogue", {
  model <- ars_to_model(
    test_path("fixtures", "ars_apx_drm_301_deterministic.json")
  )
  choices <- .method_choices(model)

  expect_named(choices, c("In this reporting event", "Standard methods"))
  expect_setequal(unlist(choices[[1]]), model$methods$id)
  expect_false(any(unlist(choices[[2]]) %in% model$methods$id))

  ## Each choice says what the engine will do with it.
  expect_true(any(grepl("computed", names(choices[[1]]))))
  expect_true(any(grepl("manual computation", names(choices[[1]]))))
})

test_that("variable choices come from the ADaM spec, scoped to the dataset", {
  spec <- parse_adam_spec(arsbridge_example("adam_spec.xlsx"))

  all_variables <- .variable_choices(spec)
  adsl <- .variable_choices(spec, "ADSL")

  expect_lt(length(adsl), length(all_variables))
  expect_true("AGE" %in% unlist(adsl))
  expect_true(any(grepl(" -- ", names(adsl))))

  expect_null(.variable_choices(NULL))
  expect_null(.variable_choices(spec, "ADNOPE"))
})

test_that("the save summary collapses repeated edits to one row per field", {
  log <- rbind(
    .edit_log_row("AN_1", "label", "a", "b"),
    .edit_log_row("AN_1", "label", "b", "c"),
    .edit_log_row("AN_2", "methodId", "MTH_X", "MTH_Y")
  )

  summary <- .diff_summary(log)

  expect_equal(nrow(summary), 2)
  label_row <- summary[summary$id == "AN_1", ]
  expect_equal(label_row$old, "a")
  expect_equal(label_row$new, "c")
})

test_that("a field edited back to its original value is not a change", {
  log <- rbind(
    .edit_log_row("AN_1", "label", "a", "b"),
    .edit_log_row("AN_1", "label", "b", "a")
  )

  expect_equal(nrow(.diff_summary(log)), 0)
  expect_match(
    paste(as.character(.diff_table_ui(log)), collapse = " "),
    "Nothing has changed"
  )
})

test_that("the save summary lists what changed", {
  log <- .edit_log_row("AN_1", "methodId", "MTH_X", "MTH_Y")
  rendered <- paste(as.character(.diff_table_ui(log)), collapse = " ")

  expect_match(rendered, "1 field")
  expect_match(rendered, "AN_1")
  expect_match(rendered, "MTH_Y")
})

test_that("an empty input becomes 'not set' rather than an empty string", {
  expect_true(is.na(.input_to_value("")))
  expect_true(is.na(.input_to_value(NULL)))
  expect_equal(.input_to_value("SEX"), "SEX")

  expect_equal(.blank_na(NA_character_), "")
  expect_equal(.blank_na("x"), "x")
})

test_that("the editor app builds in edit mode", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  model <- ars_to_model(
    test_path("fixtures", "ars_apx_drm_301_deterministic.json")
  )
  expect_s3_class(.ars_editor_app(model, mode = "edit"), "shiny.appobj")
})
