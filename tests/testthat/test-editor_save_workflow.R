# The split save workflow (Save in place / Save As / Save and close), the
# bulk grouping assignment, and the grouping entity lifecycle.

skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("DT")

.swf_model <- function() {
  .valid_fixture_model()
}

.swf_state <- function(source_path = NULL, model = .swf_model()) {
  arsbridge:::.editor_state(model, spec = NULL, report = NULL,
                            source_path = source_path, mode = "edit")
}

.swf_one_edit_log <- function(id, field = "label") {
  data.frame(
    time = "2026-08-10T00:00:00Z", pool = "analyses", id = id,
    field = field, old = "before", new = "after",
    stringsAsFactors = FALSE
  )
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

test_that("every explicit save confirmation revalidates the current model", {
  td <- withr::local_tempdir()
  source_path <- file.path(td, "re.json")
  model <- .swf_model()
  json <- jsonlite::toJSON(model_to_ars(model), auto_unbox = TRUE,
                           pretty = TRUE, null = "null")
  writeLines(json, source_path)
  before <- readBin(source_path, what = "raw", n = file.info(source_path)$size)
  state <- .swf_state(source_path, model)

  # Simulate a model update landing after a clean modal was opened. The cached
  # findings deliberately remain clean: confirmation must validate the model,
  # not trust that stale snapshot.
  invalid <- shiny::isolate(state$model())
  invalid$analyses$methodId[1] <- NA_character_
  state$model(invalid)
  state$findings(.new_findings())
  state$edit_log(.swf_one_edit_log(invalid$analyses$id[1], "methodId"))
  shiny::isolate(.write_autosave(state))
  withr::defer(.clear_autosave(source_path))
  expect_false(is.null(.read_autosave(source_path)))

  stop_called <- FALSE
  testthat::local_mocked_bindings(
    stopApp = function(...) stop_called <<- TRUE,
    .package = "shiny"
  )

  ## What "revalidates" means changed with the reservation work. It used to
  ## mean the save was refused on the strength of the CURRENT model rather than
  ## the stale clean snapshot. Saves are no longer refused -- the editor is how
  ## an author repairs an event, and refusing to persist a partial repair was
  ## the worst place for a refusal to sit -- so the observable proof is that
  ## the confirmation replaced the stale findings with the current model's.
  ## Trusting the snapshot would leave the panel clean.
  shiny::testServer(arsbridge:::mod_save_server, args = list(state = state), {
    suppressMessages(session$setInputs(confirm_save_stay = 1))

    ## Revalidated: the gap the stale snapshot did not have is now reported.
    refreshed <- state$findings()
    expect_true("METHOD_NOT_ASSIGNED" %in% refreshed$ref)
    expect_true(any(refreshed$severity %in% "GAP"))

    ## And the save really happened, rather than being silently skipped.
    expect_false(identical(
      readBin(source_path, what = "raw", n = file.info(source_path)$size),
      before
    ))

    copy_path <- file.path(td, "gapped_copy.json")
    suppressMessages(session$setInputs(save_as_path = copy_path,
                                       confirm_save_as = 1))
    expect_true(file.exists(copy_path))

    ## Save-and-close now closes, because the save succeeded. It used to be
    ## held open by the refusal.
    suppressMessages(session$setInputs(confirm_save = 1))
    expect_true(stop_called)

    ## The three facts the old refusal used to establish, inverted rather than
    ## dropped. Under the refusal the crash-recovery copy survived and no audit
    ## sidecar was written, because nothing had been saved; now the work is on
    ## disk, so the recovery copy has nothing left to protect and the edits are
    ## recorded beside the file.
    expect_null(.read_autosave(source_path))
    expect_true(file.exists(
      paste0(sub("\\.json$", "", source_path), ".edits.json")
    ))
    expect_true(file.exists(
      paste0(sub("\\.json$", "", copy_path), ".edits.json")
    ))
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

# A source line whose groupings differ from every sibling in its output, so
# there is always something for the plan to report.
.swf_regrouped <- function(model = .swf_model()) {
  counts <- table(model$analyses$output_id[!is.na(model$analyses$output_id)])
  out_id <- names(counts)[counts >= 2][1]
  if (is.na(out_id)) return(NULL)

  source_id <- model$analyses$id[!is.na(model$analyses$output_id) &
                                   model$analyses$output_id == out_id][1]
  model <- arsbridge:::model_add_grouping(model, "ADSL", "AGEGR1")
  grouping_id <- attr(model, "last_added")
  model <- model_set_field(model, "analyses", source_id,
                           "grouping_ids", grouping_id)

  list(model = model, output_id = out_id, source_id = source_id,
       grouping_id = grouping_id)
}

test_that("the bulk grouping plan stays inside one output", {
  fixture <- .swf_regrouped()
  skip_if(is.null(fixture), "fixture has no output with two analyses")

  plan <- arsbridge:::.bulk_grouping_plan(fixture$model, fixture$source_id)
  analyses <- fixture$model$analyses

  expect_true(nrow(plan$changes) > 0)
  expect_identical(plan$output_id, fixture$output_id)
  expect_identical(plan$new_value, fixture$grouping_id)

  # Every target belongs to the source line's output, and the source line is
  # not among them -- it already has the groupings being copied.
  target_outputs <- analyses$output_id[match(plan$changes$id, analyses$id)]
  expect_true(all(target_outputs == fixture$output_id))
  expect_false(fixture$source_id %in% plan$changes$id)

  # And no line from any other output is planned, which is the guarantee
  # section 11 asks for.
  elsewhere <- analyses$id[is.na(analyses$output_id) |
                             analyses$output_id != fixture$output_id]
  expect_length(intersect(plan$changes$id, elsewhere), 0L)
})

test_that("the bulk grouping plan reports only lines that would change", {
  fixture <- .swf_regrouped()
  skip_if(is.null(fixture), "fixture has no output with two analyses")

  # Bring one sibling into line by hand; the plan should stop listing it.
  before <- arsbridge:::.bulk_grouping_plan(fixture$model, fixture$source_id)
  settled <- before$changes$id[1]
  model <- model_set_field(fixture$model, "analyses", settled,
                           "grouping_ids", fixture$grouping_id)

  after <- arsbridge:::.bulk_grouping_plan(model, fixture$source_id)
  expect_false(settled %in% after$changes$id)
  expect_identical(nrow(after$changes), nrow(before$changes) - 1L)

  # The preview shows one row per changed line, plus the header row.
  html <- as.character(arsbridge:::.bulk_grouping_preview_table(after))
  expect_identical(
    lengths(regmatches(html, gregexpr("<tr>", html, fixed = TRUE))),
    nrow(after$changes) + 1L
  )
})

test_that("a line outside any output has no bulk grouping plan", {
  model <- .swf_model()

  expect_null(arsbridge:::.bulk_grouping_plan(model, "NO_SUCH_ANALYSIS"))

  detached <- model
  detached$analyses$output_id[1] <- NA_character_
  expect_null(
    arsbridge:::.bulk_grouping_plan(detached, detached$analyses$id[1])
  )
})

test_that("grouping references read as names, and a dangling one says so", {
  model <- .swf_model()
  grouping_id <- model$groupings$id[1]

  phrase <- arsbridge:::.grouping_phrase(model, grouping_id)
  expect_match(phrase, grouping_id, fixed = TRUE)

  expect_identical(arsbridge:::.grouping_phrase(model, NA_character_),
                   "no groupings")
  expect_match(arsbridge:::.grouping_phrase(model, "GF_GONE"),
               "not in this reporting event")

  # Two groupings read as a list, in the order the analysis holds them.
  pair <- paste(model$groupings$id[1:2], collapse = ";")
  expect_match(arsbridge:::.grouping_phrase(model, pair), ", ", fixed = TRUE)
})

test_that("bulk assignment re-previews rather than applying a stale plan", {
  fixture <- .swf_regrouped()
  skip_if(is.null(fixture), "fixture has no output with two analyses")

  state <- .swf_state(model = fixture$model)

  shiny::testServer(arsbridge:::mod_detail_server, args = list(state = state), {
    state$selected(list(pool = "analyses", id = fixture$source_id))
    session$setInputs(apply_groupings_all = 1)

    # The reviewer is looking at the preview when the source line's own
    # groupings change underneath it. Confirming now must not write the
    # groupings that were previewed.
    plan <- arsbridge:::.bulk_grouping_plan(state$model(), fixture$source_id)
    apply_edit(state, "analyses", fixture$source_id, "grouping_ids",
               NA_character_)
    before <- state$model()$analyses

    session$setInputs(confirm_apply_groupings_all = 1)

    after <- state$model()$analyses
    expect_identical(after$grouping_ids, before$grouping_ids)
    expect_false(any(after$grouping_ids[match(plan$changes$id, after$id)] ==
                       fixture$grouping_id, na.rm = TRUE))

    # Confirming the refreshed preview does go through.
    session$setInputs(confirm_apply_groupings_all = 2)
    applied <- state$model()$analyses
    targets <- !is.na(applied$output_id) &
      applied$output_id == fixture$output_id
    expect_true(all(is.na(applied$grouping_ids[targets])))
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
