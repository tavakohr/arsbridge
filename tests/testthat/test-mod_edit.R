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
    model <- .valid_fixture_model()
  }
  .editor_state(model, spec, report, NULL, "edit")
}

.total_label_repair_model <- function() {
  groups <- list(
    list(label = "Cohort A", annotation = "ADSL.COHORTN=1", order = 1L),
    list(label = "Cohort B", annotation = "ADSL.COHORTN=2", order = 2L)
  )
  section <- list(
    tlf_number = "T-1",
    tlf_type = "TABLE",
    title = "Table 1",
    population_text = "Safety Population",
    population_annot = "ADSL.SAFFL='Y'",
    analysis_type = "CATEGORICAL",
    ars_method_name = "Count and Percentage",
    by_variable = "COHORTN",
    by_variable_dataset = "ADSL",
    source_format = "docx",
    col_headers = c("Characteristic", "Cohort A", "Cohort B"),
    column_groups = list(variable = "COHORTN", dataset = "ADSL",
                         groups = groups),
    stub_rows = list(list(label = "Sex", annotation = "ADSL.SEX",
                          has_annot = TRUE)),
    enriched_rows = list(list(label = "Sex", primary_dataset = "ADSL",
                              primary_variable = "SEX", data_subset = NULL,
                              variable_role = "ANALYSIS"))
  )
  model <- ars_to_model(build_ars_json(list(section), study_id = "S1"))

  first <- model$analyses[1, , drop = FALSE]
  first$id <- "AN_T_1_001"
  first$includeTotal <- TRUE
  first$raw[[1]]$id <- first$id
  first$raw[[1]]$includeTotal <- TRUE
  first$raw[[1]]$totalLabel <- "All"

  second <- first
  second$id <- "AN_T_1_002"
  second$raw[[1]]$id <- second$id
  second$raw[[1]]$totalLabel <- "Overall"
  model$analyses <- rbind(first, second)
  model$outputs$referenced_analysis_ids[1] <- .join_values(c(first$id, second$id))
  model$outputs$raw[[1]]$referencedAnalysisIds <- list(first$id, second$id)

  columns <- model$outputs$raw[[1]]$displays[[1]]$display$columns
  columns[[length(columns) + 1L]] <- list(label = "Overall")
  model$outputs$raw[[1]]$displays[[1]]$display$columns <- columns
  model
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
  expect_equal(sum(state$findings()$severity == "GAP"), 0)

  apply_edit(state, "analyses", target, "methodId", "MTH_GONE")

  findings <- state$findings()
  expect_gt(sum(findings$severity == "GAP"), 0)
  expect_true(any(grepl("MTH_GONE", findings$problem)))
})

test_that("the structured editor can repair and save a Total-label blocker", {
  skip_if_not_installed("shiny")
  shiny::reactiveConsole(TRUE)
  withr::defer(shiny::reactiveConsole(FALSE))

  state <- .edit_state(.total_label_repair_model())
  target <- "AN_T_1_001"
  mismatch <- state$findings()$ref %in% "FLAT_AXIS_COLUMN_LABEL_MISMATCH"
  expect_equal(sum(mismatch), 1L)

  row <- state$model()$analyses[state$model()$analyses$id == target, ]
  html <- paste(as.character(.analysis_edit_ui(
    row, state$model(), state, identity
  )), collapse = "\n")
  expect_match(html, "Total column label", fixed = TRUE)
  expect_match(html, "All", fixed = TRUE)

  expect_true(apply_edit(state, "analyses", target,
                         "totalLabel", "Overall"))
  expect_equal(state$model()$analyses$totalLabel[1], "Overall")
  expect_equal(state$model()$analyses$raw[[1]]$totalLabel, "Overall")
  expect_false(any(state$findings()$ref %in%
                     "FLAT_AXIS_COLUMN_LABEL_MISMATCH"))

  output_path <- tempfile(fileext = ".json")
  result <- list(
    model = state$model(),
    edit_log = state$edit_log(),
    source_path = output_path
  )
  suppressMessages(.edit_ars_finish(result, output_path))
  saved <- jsonlite::read_json(output_path, simplifyVector = FALSE)
  saved_analysis <- Filter(function(x) identical(x$id, target),
                           saved$analyses)[[1]]
  expect_equal(saved_analysis$totalLabel, "Overall")
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
  expect_equal(sum(state$findings()$severity == "GAP"), 0)
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
