## The shell table's reactive wiring, driven through shiny::testServer() in
## the house style of test-editor_flows.R: a shell-row click lands in the
## delegated input, moves the selection to the owning analysis, and keeps the
## table on screen with the analysis form beside it.

.shell_flows_model <- function() {
  ars_to_model(test_path("fixtures", "ars_apx_drm_301_deterministic.json"))
}

.shell_flows_state <- function(mode = "edit", model = NULL) {
  if (is.null(model)) model <- .shell_flows_model()
  .editor_state(model, NULL, NULL, NULL, mode)
}


test_that("the output panel opens as the shell-shaped table", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  state <- .shell_flows_state()

  shiny::testServer(mod_detail_server, args = list(state = state), {
    state$selected(list(pool = "outputs", id = "T_14_1_1"))
    session$flushReact()

    rendered <- paste(as.character(output$detail), collapse = " ")
    expect_match(rendered, "shell-table")
    ## Authored rows with no analysis of their own are finally visible.
    expect_match(rendered, "Adverse event")
    expect_match(rendered, "Lack of efficacy")
    ## The old metadata block is demoted, not gone.
    expect_match(rendered, "Output properties")
    ## The reorder list is untouched this phase.
    expect_match(rendered, "list-group-item")
  })
})

test_that("clicking an owned row selects its analysis and keeps the table", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  state <- .shell_flows_state()

  shiny::testServer(mod_detail_server, args = list(state = state), {
    state$selected(list(pool = "outputs", id = "T_14_1_2"))
    session$flushReact()

    ## Order 3 is the authored "Mean (SD)" stat line -- reachable in no
    ## previous UI -- owned by the Age (years) continuous analysis.
    session$setInputs(shell_row = list(output = "T_14_1_2", order = 3))

    expect_equal(state$shell_row(), list(output = "T_14_1_2", order = 3L))
    expect_equal(state$selected(),
                 list(pool = "analyses", id = "AN_T_14_1_2_001"))

    session$flushReact()
    rendered <- paste(as.character(output$detail), collapse = " ")
    expect_match(rendered, "shell-table")
    expect_match(rendered, "table-active")
    ## The side panel carries the full edit form.
    expect_match(rendered, "Remove line")

    ## Typing in the side panel writes through the existing apply_edit path.
    session$setInputs(label = "Edited beside the table")
    model <- state$model()
    expect_equal(
      model$analyses$label[model$analyses$id == "AN_T_14_1_2_001"],
      "Edited beside the table"
    )
    expect_equal(nrow(state$edit_log()), 1L)
  })
})

test_that("clicking a section-header row sets shell_row but not selected", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  ## The fixture authors no unowned rows, so plant a section header ahead of
  ## the disposition counts.
  model <- .shell_flows_model()
  index <- match("T_14_1_1", model$outputs$id)
  model$outputs$raw[[index]][["_meta"]][["shell_layout"]] <- c(
    list(list(order = 0L, label = "Disposition", indent = 0L,
              analysis_id = NULL, kind = "label_row")),
    model$outputs$raw[[index]][["_meta"]][["shell_layout"]]
  )
  state <- .shell_flows_state(model = model)

  shiny::testServer(mod_detail_server, args = list(state = state), {
    state$selected(list(pool = "outputs", id = "T_14_1_1"))
    session$flushReact()

    session$setInputs(shell_row = list(output = "T_14_1_1", order = 0))

    expect_equal(state$shell_row(), list(output = "T_14_1_1", order = 0L))
    expect_equal(state$selected(), list(pool = "outputs", id = "T_14_1_1"))

    session$flushReact()
    rendered <- paste(as.character(output$detail), collapse = " ")
    expect_match(rendered, "table-active")
    expect_match(rendered, "Authored shell text")
  })
})

test_that("in view mode the click opens the read-only detail panel", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  state <- .shell_flows_state(mode = "view")

  shiny::testServer(mod_detail_server, args = list(state = state), {
    state$selected(list(pool = "outputs", id = "T_14_1_2"))
    session$flushReact()

    session$setInputs(shell_row = list(output = "T_14_1_2", order = 6))
    expect_equal(state$selected(),
                 list(pool = "analyses", id = "AN_T_14_1_2_002"))

    session$flushReact()
    rendered <- paste(as.character(output$detail), collapse = " ")
    expect_match(rendered, "shell-table")
    expect_match(rendered, "Analysis id")
    expect_false(grepl("Remove line", rendered, fixed = TRUE))
  })
})

test_that("clicking the header explains the columns without moving selection", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  state <- .shell_flows_state()

  shiny::testServer(mod_detail_server, args = list(state = state), {
    state$selected(list(pool = "outputs", id = "T_14_1_2"))
    session$flushReact()

    session$setInputs(shell_header = list(output = "T_14_1_2"))
    expect_equal(state$shell_header(), list(output = "T_14_1_2"))
    expect_equal(state$selected(), list(pool = "outputs", id = "T_14_1_2"))

    session$flushReact()
    rendered <- paste(as.character(output$detail), collapse = " ")
    ## Panel-specific text, not the header cells' tooltip.
    expect_match(rendered, "produced by the grouping above")
    expect_match(rendered, "GF_TRT01A")
    expect_match(rendered, "Entities tab")

    ## A row click takes the panel slot back and clears the header state.
    session$setInputs(shell_row = list(output = "T_14_1_2", order = 3))
    expect_null(state$shell_header())
    session$flushReact()
    rendered <- paste(as.character(output$detail), collapse = " ")
    expect_match(rendered, "Remove line")
    expect_false(grepl("produced by the grouping above", rendered,
                       fixed = TRUE))
  })
})

test_that("Edit in Entities files an entity request and the library obeys", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  state <- .shell_flows_state()

  shiny::testServer(mod_detail_server, args = list(state = state), {
    state$selected(list(pool = "outputs", id = "T_14_1_2"))
    session$flushReact()
    session$setInputs(shell_header = list(output = "T_14_1_2"))
    session$flushReact()

    rendered <- paste(as.character(output$detail), collapse = " ")
    expect_match(rendered, "Edit in Entities")

    session$setInputs(open_entity = list(pool = "groupings",
                                         id = "GF_TRT01A"))
    expect_equal(state$entity_request(),
                 list(pool = "groupings", id = "GF_TRT01A"))
  })

  ## The library reacts to the request without error: pool tab + row.
  shiny::testServer(mod_entity_library_server, args = list(state = state), {
    state$entity_request(list(pool = "groupings", id = "GF_TRT01A"))
    expect_silent(session$flushReact())
  })
})

test_that("the analysis panel links back to its output table", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  state <- .shell_flows_state()

  shiny::testServer(mod_detail_server, args = list(state = state), {
    ## A tree-style selection lands on the full-width analysis form.
    state$selected(list(pool = "analyses", id = "AN_T_14_1_2_001"))
    session$flushReact()
    rendered <- paste(as.character(output$detail), collapse = " ")
    expect_match(rendered, "Back to the output table")

    ## The link fires the same delegated input the tree uses, and the shell
    ## view comes back.
    session$setInputs(selected = list(pool = "outputs", id = "T_14_1_2"))
    session$flushReact()
    rendered <- paste(as.character(output$detail), collapse = " ")
    expect_match(rendered, "shell-table")
  })
})

test_that("selecting elsewhere makes the shell click stale, not sticky", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  state <- .shell_flows_state()

  shiny::testServer(mod_detail_server, args = list(state = state), {
    state$selected(list(pool = "outputs", id = "T_14_1_2"))
    session$flushReact()
    session$setInputs(shell_row = list(output = "T_14_1_2", order = 3))
    session$flushReact()

    ## The reviewer clicks a different analysis in the tree: the full-width
    ## analysis panel comes back, the old row click does not pin the table.
    state$selected(list(pool = "analyses", id = "AN_T_14_1_2_007"))
    session$flushReact()

    rendered <- paste(as.character(output$detail), collapse = " ")
    expect_match(rendered, "Remove line")
    expect_false(grepl("shell-table", rendered, fixed = TRUE))
  })
})
