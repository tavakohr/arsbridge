# The Columns review panel: renders the parsed header tree and declared
# paths, edits a path's role/totalStrategy through the model (history +
# edit log + re-validation), and surfaces the structural warning codes.

skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("DT")

.ct_model <- function(td) {
  event <- jsonlite::fromJSON(.asym_build(td)$ars_path, simplifyVector = FALSE)
  ars_to_model(event)
}

.ct_state <- function(model, mode = "edit") {
  arsbridge:::.editor_state(model, spec = NULL, report = NULL,
                            source_path = NULL, mode = mode)
}

test_that("the panel lists the path-mode output and its declared paths", {
  td <- withr::local_tempdir()
  state <- .ct_state(.ct_model(td), mode = "view")

  shiny::testServer(arsbridge:::mod_column_tree_server, args = list(state = state), {
    df <- tree_outputs()
    expect_identical(nrow(df), 1L)

    paths <- paths_df()
    expect_identical(nrow(paths), 6L)
    expect_identical(paths$column[4], "Cohort A > Total")
    expect_identical(paths$role[4], "SUBTOTAL")
    expect_identical(paths$total_strategy[4], "condition_based")
  })
})

test_that("editing a path's totalStrategy round-trips through model_to_ars", {
  td <- withr::local_tempdir()
  state <- .ct_state(.ct_model(td), mode = "edit")

  shiny::testServer(arsbridge:::mod_column_tree_server, args = list(state = state), {
    session$setInputs(output = tree_outputs()$id[1])
    session$setInputs(paths_rows_selected = 4L)
    session$setInputs(path_total_strategy = "analysis_set")

    # The model was updated and the edit logged.
    log <- state$edit_log()
    expect_true(any(grepl("totalStrategy", log$field)))

    back <- model_to_ars(state$model())
    out <- NULL
    for (o in back$outputs) if (!is.null(o$resultGroupPaths)) out <- o
    expect_identical(out$resultGroupPaths$paths[[4]]$totalStrategy,
                     "analysis_set")

    # No-op echoes do not add phantom edits.
    n_edits <- nrow(state$edit_log())
    session$setInputs(path_total_strategy = "analysis_set")
    expect_identical(nrow(state$edit_log()), n_edits)
  })
})

test_that("structural damage surfaces as warning codes in the panel", {
  td <- withr::local_tempdir()
  event <- jsonlite::fromJSON(.asym_build(td)$ars_path, simplifyVector = FALSE)
  for (i in seq_along(event$outputs)) {
    if (!is.null(event$outputs[[i]]$resultGroupPaths)) {
      event$outputs[[i]]$resultGroupPaths$paths <-
        event$outputs[[i]]$resultGroupPaths$paths[1:3]
    }
  }
  model <- ars_to_model(event)
  state <- .ct_state(model, mode = "view")

  shiny::testServer(arsbridge:::mod_column_tree_server, args = list(state = state), {
    findings <- state$findings()
    expect_true(any(findings$ref == "DISPLAY_COLUMN_COUNT_MISMATCH"))

    expect_identical(nrow(paths_df()), 3L)
    html <- as.character(output$warnings$html)
    expect_match(html, "DISPLAY_COLUMN_COUNT_MISMATCH")
  })
})
