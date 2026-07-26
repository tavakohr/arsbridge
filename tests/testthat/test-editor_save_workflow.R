# The split save workflow (Save in place / Save As / Save and close), the
# bulk grouping assignment, and the grouping entity lifecycle.

skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("DT")

.swf_model <- function() {
  ars_to_model(test_path("fixtures", "ars_apx_drm_301_deterministic.json"))
}

.swf_state <- function(source_path = NULL, model = .swf_model()) {
  arsbridge:::.editor_state(model, spec = NULL, report = NULL,
                            source_path = source_path, mode = "edit")
}

test_that("Save in place writes the file, backs up, and resets the dirty state", {
  td <- withr::local_tempdir()
  source_path <- file.path(td, "re.json")
  model <- .swf_model()
  writeLines(jsonlite::toJSON(model_to_ars(model), auto_unbox = TRUE,
                              null = "null"), source_path)

  state <- .swf_state(source_path, model)

  shiny::testServer(arsbridge:::mod_save_server, args = list(state = state), {
    target <- state$model()$analyses$id[1]
    apply_edit(state, "analyses", target, "label", "Renamed in place")
    expect_identical(nrow(state$edit_log()), 1L)

    suppressMessages(session$setInputs(confirm_save_stay = 1))

    # Written, backed up, dirty state cleared.
    saved <- jsonlite::fromJSON(source_path, simplifyVector = FALSE)
    labels <- vapply(saved$analyses, function(a) a$label %||% "", character(1))
    expect_true("Renamed in place" %in% labels)
    expect_length(list.files(td, pattern = "^re\\.json\\.bak-"), 1L)
    expect_identical(nrow(state$edit_log()), 0L)
  })
})

test_that("Save As writes a copy and keeps the session dirty", {
  td <- withr::local_tempdir()
  source_path <- file.path(td, "re.json")
  model <- .swf_model()
  writeLines(jsonlite::toJSON(model_to_ars(model), auto_unbox = TRUE,
                              null = "null"), source_path)
  state <- .swf_state(source_path, model)

  shiny::testServer(arsbridge:::mod_save_server, args = list(state = state), {
    target <- state$model()$analyses$id[1]
    apply_edit(state, "analyses", target, "label", "Renamed for the copy")

    copy_path <- file.path(td, "re_copy.json")
    suppressMessages(session$setInputs(save_as_path = copy_path,
                                       confirm_save_as = 1))

    expect_true(file.exists(copy_path))
    saved <- jsonlite::fromJSON(copy_path, simplifyVector = FALSE)
    labels <- vapply(saved$analyses, function(a) a$label %||% "", character(1))
    expect_true("Renamed for the copy" %in% labels)

    # The original file is untouched and the session is still dirty.
    original <- jsonlite::fromJSON(source_path, simplifyVector = FALSE)
    labels0 <- vapply(original$analyses, function(a) a$label %||% "", character(1))
    expect_false("Renamed for the copy" %in% labels0)
    expect_identical(nrow(state$edit_log()), 1L)
  })
})

test_that("bulk grouping assignment copies one line's groupings across the output", {
  state <- .swf_state()

  shiny::testServer(arsbridge:::mod_detail_server, args = list(state = state), {
    model <- state$model()
    out_id <- model$outputs$id[1]
    in_output <- model$analyses[!is.na(model$analyses$output_id) &
                                  model$analyses$output_id == out_id, ]
    skip_if(nrow(in_output) < 2, "fixture output has fewer than two analyses")

    source_id <- in_output$id[1]
    state$selected(list(pool = "analyses", id = source_id))

    # Give the source line a grouping set no other line has, then apply it
    # to the whole output.
    state$model(arsbridge:::model_add_grouping(state$model(), "ADSL", "AGEGR1"))
    new_gf <- attr(state$model(), "last_added")
    apply_edit(state, "analyses", source_id, "grouping_ids", new_gf)
    session$setInputs(apply_groupings_all = 1)
    session$setInputs(confirm_apply_groupings_all = 1)

    updated <- state$model()$analyses
    updated <- updated[!is.na(updated$output_id) & updated$output_id == out_id, ]
    expect_true(all(updated$grouping_ids == new_gf))

    # One undo reverses the whole bulk action (the source line keeps its own
    # earlier edit).
    arsbridge:::.undo(state)
    reverted <- state$model()$analyses
    reverted <- reverted[!is.na(reverted$output_id) & reverted$output_id == out_id, ]
    expect_false(all(reverted$grouping_ids == new_gf))
    expect_identical(reverted$grouping_ids[reverted$id == source_id], new_gf)
  })
})

test_that("grouping add, clone, and delete behave and respect dependencies", {
  model <- .swf_model()

  # Add: fresh data-driven grouping with a collision-safe id.
  added <- arsbridge:::model_add_grouping(model, "ADSL", "AGEGR1")
  new_id <- attr(added, "last_added")
  expect_true(new_id %in% added$groupings$id)
  row <- added$groupings[added$groupings$id == new_id, ]
  expect_identical(row$groupingVariable, "AGEGR1")
  expect_true(row$dataDriven)

  # Clone: copy exists under a variant id; original untouched.
  base_id <- model$groupings$id[1]
  cloned <- arsbridge:::model_clone_grouping(model, base_id)
  clone_id <- attr(cloned, "last_added")
  expect_true(clone_id %in% cloned$groupings$id)
  expect_true(base_id %in% cloned$groupings$id)

  # Delete: refused while referenced, allowed once unreferenced.
  used_id <- arsbridge:::.split_values(
    model$analyses$grouping_ids[!is.na(model$analyses$grouping_ids)][1]
  )[1]
  expect_error(arsbridge:::model_remove_grouping(model, used_id),
               "still used")
  expect_gt(length(arsbridge:::.grouping_dependents(model, used_id)), 0)

  removed <- arsbridge:::model_remove_grouping(cloned, clone_id)
  expect_false(clone_id %in% removed$groupings$id)

  # The added grouping survives the round trip to ARS JSON.
  back <- model_to_ars(added)
  gf_ids <- vapply(back$analysisGroupings, function(g) g$id, character(1))
  expect_true(new_id %in% gf_ids)
})
