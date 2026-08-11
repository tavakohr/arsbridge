# The workflow app's server flows under testServer.
#
# One phase, five panels: set up the project, build, optionally correct with a
# supplement, review, read the results. The tests below follow that journey,
# and pay particular attention to the two things the redesign changed --
# the build comes before the supplement, and the results survive a restart.

skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("DT")

.wfa_inputs <- function() {
  list(
    shell = arsbridge_example("annotated_shell.docx"),
    spec  = arsbridge_example("adam_spec.xlsx")
  )
}

.wfa_server <- function(project_dir = NULL) {
  arsbridge:::.ars_workflow_app(project_dir)
}

## Step 1's markup. A shinyApp() keeps no handle on its UI, so the panel is
## built directly -- which is why it is its own function.
.wfa_panel_html <- function(project_dir = NULL,
                            pickers = arsbridge:::.workflow_pickers_available()) {
  panel <- arsbridge:::.workflow_project_panel(
    project_dir, arsbridge:::.workflow_read_state(project_dir %||% ""), pickers)
  paste(as.character(panel), collapse = "\n")
}

## Builds run in-process here. testServer drives a virtual clock, so the
## background poller would never fire and the test would assert on a run that
## has not finished -- it would be testing the harness, not the app.
.wfa_no_keys <- function(code) {
  withr::with_options(
    list(arsbridge.workflow_background = FALSE),
    withr::with_envvar(
      c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
        GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
      suppressMessages(suppressWarnings(code))))
}

test_that("the app scaffolds a project and unlocks the build immediately", {
  ## The point of the one-phase flow: nothing stands between recording the
  ## inputs and seeing what the engine can do on its own.
  td <- withr::local_tempdir()
  project <- file.path(td, "study")
  inputs <- .wfa_inputs()

  shiny::testServer(.wfa_server(NULL), {
    session$setInputs(project_dir = project,
                      shell_path  = inputs$shell,
                      spec_path   = inputs$spec,
                      adam_dir    = "",
                      study_id    = "CDSC-ALZ-201")
    session$setInputs(init_project = 1)

    expect_true(file.exists(arsbridge:::.workflow_paths(project)$state))
    st <- status()
    expect_true(st$done[st$step == "project"])
    expect_true(st$available[st$step == "build"])
    ## The supplement waits for a parse to have happened.
    expect_false(st$available[st$step == "supplement"])
  })
})

test_that("the project records an ADaM folder when given one", {
  td <- withr::local_tempdir()
  project <- file.path(td, "study")
  inputs <- .wfa_inputs()
  adam <- test_path("fixtures", "adam_apx_drm_301")

  shiny::testServer(.wfa_server(NULL), {
    session$setInputs(project_dir = project, shell_path = inputs$shell,
                      spec_path = inputs$spec, adam_dir = adam,
                      study_id = "S1")
    session$setInputs(init_project = 1)
    recorded <- arsbridge:::.workflow_read_state(project)
    expect_true(dir.exists(recorded$adam_dir))
  })
})

test_that("a build runs, and its payload drives the results panel", {
  td <- withr::local_tempdir()
  project <- file.path(td, "study")
  inputs <- .wfa_inputs()
  arsbridge:::.workflow_init(project, inputs$shell, inputs$spec)

  .wfa_no_keys(
    shiny::testServer(.wfa_server(project), {
      session$setInputs(run_build = 1)

      payload <- state$last_result()
      expect_false(is.null(payload))
      expect_true(payload$status %in% c("success", "partial"))
      ## Every stage's diagnostics, not just the last stage's.
      expect_gt(nrow(payload$diagnostics), 0)
      expect_true("parse_shell" %in% payload$diagnostics$stage)

      st <- status()
      expect_true(st$done[st$step == "build"])
      expect_true(st$available[st$step == "supplement"])
      expect_true(st$available[st$step == "results"])
    })
  )
})

test_that("a worker crash replaces the previous successful payload", {
  td <- withr::local_tempdir()
  project <- file.path(td, "study")
  inputs <- .wfa_inputs()
  arsbridge:::.workflow_init(project, inputs$shell, inputs$spec)
  paths <- arsbridge:::.workflow_paths(project)
  previous <- list(
    status = "success",
    artifacts = list(code_paths = file.path(paths$code_dir, "OLD_OUTPUT.R"))
  )
  saveRDS(previous, paths$payload)

  shiny::testServer(.wfa_server(project), {
    state$last_result(previous)
    arsbridge:::.workflow_finish_build(
      simpleError("worker crashed"), state, bump
    )

    current <- state$last_result()
    expect_equal(current$status, "error")
    expect_match(current$error, "worker crashed", fixed = TRUE)
    expect_length(current$artifacts$code_paths, 0L)
    expect_true(all(is.na(unlist(current$artifacts[c(
      "ars_json", "validation_report", "code_dir", "ard_rds",
      "filled_workbook", "fill_debrief"
    )]))))

    persisted <- readRDS(paths$payload)
    expect_equal(persisted$status, "error")
    expect_match(persisted$error, "worker crashed", fixed = TRUE)
  })
})

test_that("the results panel reads the payload from disk after a restart", {
  ## Nothing is remembered in the session. A user who closes the app and comes
  ## back must still see what the last run produced and what it declined to.
  td <- withr::local_tempdir()
  project <- file.path(td, "study")
  inputs <- .wfa_inputs()
  arsbridge:::.workflow_init(project, inputs$shell, inputs$spec)
  paths <- arsbridge:::.workflow_paths(project)
  state0 <- arsbridge:::.workflow_read_state(project)
  .wfa_no_keys(arsbridge:::.workflow_run_build(state0, paths))
  expect_true(file.exists(paths$payload))

  ## A brand-new session: last_result() is empty, so the panel must fall back
  ## to the payload the previous run left behind.
  shiny::testServer(.wfa_server(project), {
    expect_null(state$last_result())
    recovered <- arsbridge:::.workflow_last_payload(paths, state)
    expect_false(is.null(recovered))
    expect_gt(nrow(recovered$diagnostics), 0)
    st <- status()
    expect_true(st$done[st$step == "results"])
  })
})

test_that("the supplement step drafts, and validates what comes back", {
  td <- withr::local_tempdir()
  project <- file.path(td, "study")
  inputs <- .wfa_inputs()
  arsbridge:::.workflow_init(project, inputs$shell, inputs$spec)
  paths <- arsbridge:::.workflow_paths(project)
  state0 <- arsbridge:::.workflow_read_state(project)
  .wfa_no_keys(arsbridge:::.workflow_run_build(state0, paths))

  .wfa_no_keys(
    shiny::testServer(.wfa_server(project), {
      session$setInputs(write_instructions = 1)
      expect_true(file.exists(paths$instructions))
      expect_true(file.exists(paths$schema))

      session$setInputs(generate_draft = 1)
      expect_true(file.exists(paths$draft))

      ## A reviewed supplement, validated on request rather than on arrival:
      ## the user edits it outside the app now, so there is no paste to catch.
      good <- paste(readLines(test_path("fixtures", "supplement_v4_example.json"),
                              warn = FALSE), collapse = "\n")
      writeLines(good, paths$supplement)
      session$setInputs(validate_supplement = 1)
      findings <- state$supplement_findings()
      expect_false(is.null(findings))

      st <- status()
      expect_true(st$done[st$step == "supplement"])
      expect_match(st$detail[st$step == "build"], "reviewed supplement")
    })
  )
})

test_that("validating a supplement that is not there says so", {
  td <- withr::local_tempdir()
  project <- file.path(td, "study")
  inputs <- .wfa_inputs()
  arsbridge:::.workflow_init(project, inputs$shell, inputs$spec)
  paths <- arsbridge:::.workflow_paths(project)
  state0 <- arsbridge:::.workflow_read_state(project)
  .wfa_no_keys(arsbridge:::.workflow_run_build(state0, paths))

  shiny::testServer(.wfa_server(project), {
    session$setInputs(validate_supplement = 1)
    expect_null(state$supplement_findings())
  })
})

test_that("a resumed project derives statuses and exposes the hand-off", {
  td <- withr::local_tempdir()
  project <- file.path(td, "study")
  inputs <- .wfa_inputs()
  arsbridge:::.workflow_init(project, inputs$shell, inputs$spec, "APX-DRM-301")
  paths0 <- arsbridge:::.workflow_paths(project)
  state0 <- arsbridge:::.workflow_read_state(project)
  .wfa_no_keys(arsbridge:::.workflow_run_build(state0, paths0))

  shiny::testServer(.wfa_server(project), {
    st <- status()
    expect_true(st$done[st$step == "build"])
    expect_true(st$available[st$step == "review"])
  })

  payload <- arsbridge:::.workflow_handoff_payload(project)
  expect_identical(payload$action, "edit")
  expect_identical(payload$ars_path, paths0$ars)
})

test_that("the completion notice calls an unfilled workbook what it is", {
  ## The one outcome a user will misread as success is a workbook that looks
  ## identical to the shell -- it outranks the generic "partial" wording.
  filled <- list(status = "success",
                 fill = list(filled = 12L, pending = 0L, skipped = 0L),
                 unfilled_cells = data.frame())
  expect_equal(arsbridge:::.workflow_build_notice(filled)$type, "message")

  unfilled <- list(status = "partial",
                   fill = list(filled = 0L, pending = 20L, skipped = 0L),
                   unfilled_cells = data.frame(reason = rep("x", 20)))
  notice <- arsbridge:::.workflow_build_notice(unfilled)
  expect_equal(notice$type, "warning")
  expect_match(notice$text, "left unfilled")

  partial <- list(status = "partial", fill = list(filled = 5L),
                  unfilled_cells = data.frame(reason = "x"))
  expect_match(arsbridge:::.workflow_build_notice(partial)$text, "findings")

  failed <- list(status = "error", failed_stage = "spec_to_ars")
  expect_match(arsbridge:::.workflow_build_notice(failed)$text, "spec_to_ars")

  ## No fill stage at all (Word shell, or no data): never the unfilled text.
  no_fill <- list(status = "success", fill = NULL,
                  unfilled_cells = data.frame())
  expect_equal(arsbridge:::.workflow_build_notice(no_fill)$text,
               "Build complete.")
})

test_that("a blocked build is rendered as needs fixes with retained and skipped work", {
  td <- withr::local_tempdir()
  project <- file.path(td, "study")
  inputs <- .wfa_inputs()
  arsbridge:::.workflow_init(project, inputs$shell, inputs$spec)
  paths <- arsbridge:::.workflow_paths(project)
  writeLines("{}", paths$ars)
  writeLines("report", paths$report)
  dir.create(paths$code_dir, recursive = TRUE)
  writeLines("# stale", file.path(paths$code_dir, "OLD_OUTPUT.R"))

  finding <- data.frame(
    severity = "FAIL",
    ref = "FLAT_AXIS_COLUMN_LABEL_MISMATCH",
    entity = "outputs",
    id = "T_01",
    field = "columns",
    problem = "The displayed labels do not match the grouping.",
    action = "Align the display labels with the grouping.",
    stringsAsFactors = FALSE
  )
  payload <- list(
    status = "partial",
    timings = list(total = 0.2),
    artifacts = list(
      ars_json = paths$ars,
      validation_report = paths$report,
      code_dir = NA_character_
    ),
    diagnostics = data.frame(),
    validation_gate = list(
      blocked = TRUE,
      status = "needs-fixes",
      blocking_findings = finding,
      blocking_refs = finding$ref,
      summary = "One blocking model finding."
    ),
    needs_fixes = TRUE
  )

  notice <- arsbridge:::.workflow_build_notice(payload)
  expect_match(notice$text, "needs fixes", ignore.case = TRUE)

  shiny::testServer(.wfa_server(project), {
    state$last_result(payload)
    session$flushReact()

    html <- as.character(output$results_artifacts$html)
    expect_match(html, "NEEDS FIXES", fixed = TRUE)
    expect_match(html, "ARS JSON and validation report were retained", fixed = TRUE)
    expect_match(html, "runnable code, ARD, filled workbook, and fill debrief were skipped",
                 fixed = TRUE)
    expect_match(html, "FLAT_AXIS_COLUMN_LABEL_MISMATCH", fixed = TRUE)
    expect_match(html, "The displayed labels do not match", fixed = TRUE)
    expect_match(html, "Align the display labels", fixed = TRUE)

    review_html <- as.character(output$review_paths$html)
    expect_match(review_html, paths$ars, fixed = TRUE)
    expect_match(review_html, paths$report, fixed = TRUE)
    expect_no_match(review_html, "OLD_OUTPUT.R", fixed = TRUE)
    expect_no_match(review_html, paths$code_dir, fixed = TRUE)
  })
})

test_that("successful builds render only current-run scripts", {
  td <- withr::local_tempdir()
  project <- file.path(td, "study")
  inputs <- .wfa_inputs()
  arsbridge:::.workflow_init(project, inputs$shell, inputs$spec)
  paths <- arsbridge:::.workflow_paths(project)
  dir.create(paths$code_dir, recursive = TRUE)
  stale_path <- file.path(paths$code_dir, "OLD_OUTPUT.R")
  current_path <- file.path(paths$code_dir, "T_01.R")
  writeLines("# stale", stale_path)
  writeLines("# current", current_path)

  payload <- list(
    status = "success",
    timings = list(total = 0.2),
    artifacts = list(
      ars_json = paths$ars,
      validation_report = paths$report,
      code_dir = NA_character_,
      code_paths = current_path,
      ard_rds = NA_character_,
      filled_workbook = NA_character_,
      fill_debrief = NA_character_,
      run_log = paths$run_log
    ),
    diagnostics = .EMPTY_DIAGNOSTICS(),
    validation_gate = .validation_gate(.new_findings()),
    needs_fixes = FALSE
  )

  shiny::testServer(.wfa_server(project), {
    state$last_result(payload)
    session$flushReact()

    review_html <- as.character(output$review_paths$html)
    expect_match(review_html, current_path, fixed = TRUE)
    expect_no_match(review_html, stale_path, fixed = TRUE)

    results_html <- as.character(output$results_artifacts$html)
    expect_match(results_html, current_path, fixed = TRUE)
    expect_no_match(results_html, stale_path, fixed = TRUE)
  })
})

test_that("the unfilled table shows census provenance for current and archived payloads", {
  td <- withr::local_tempdir()
  project <- file.path(td, "study")
  inputs <- .wfa_inputs()
  arsbridge:::.workflow_init(project, inputs$shell, inputs$spec)

  current <- data.frame(
    sheet = "Table 1",
    ref = "C7",
    status = "pending",
    reason = "no matching result",
    row_label = "Category A",
    method_id = "MTH_COUNT_AND_PERCENTAGE",
    placeholder = "xx (xx.x)",
    ars_grouping_id = "GRP_ARM",
    ars_group_label = "Treatment arm",
    variable_level = "A",
    parent_level = "Parent category",
    ard_lookup_key = "analysis=AN_01 | group=A | parent=Parent category",
    stringsAsFactors = FALSE
  )
  provenance <- c(
    "output_id", "row", "col", "col_label", "analysis_id",
    "row_label", "method_id", "placeholder", "ars_grouping_id",
    "ars_group_label", "variable_level", "parent_level", "ard_lookup_key"
  )

  shiny::testServer(.wfa_server(project), {
    state$last_result(list(status = "partial", unfilled_cells = current))
    session$flushReact()
    rendered <- as.character(output$results_unfilled)
    for (column in provenance) {
      expect_match(rendered, column, fixed = TRUE)
    }

    archived <- current[, c("sheet", "ref", "status", "reason"), drop = FALSE]
    state$last_result(list(status = "partial", unfilled_cells = archived))
    session$flushReact()
    archived_rendered <- as.character(output$results_unfilled)
    for (column in provenance) {
      expect_match(archived_rendered, column, fixed = TRUE)
    }
  })
})

test_that("the skew message names an absent install instead of printing NA", {
  ## NA_character_ is neither NULL nor empty, so %||% could not catch it and
  ## the old message read "the installed arsbridge is NA".
  msg <- arsbridge:::.workflow_skew_message(NA_character_, "0.1.0.9060")
  expect_match(msg, "absent")
  expect_false(grepl("\\bNA\\b", msg))
  expect_match(arsbridge:::.workflow_skew_message("0.1.0.9059", "0.1.0.9060"),
               "0\\.1\\.0\\.9059")
})

test_that("progress read back from a run log matches what the worker wrote", {
  log <- withr::local_tempfile(fileext = ".log")
  writeLines("Parsed 9 TLF sections from shells.xlsx", log)
  ## Append the way the worker does: through the emitter.
  lines <- utils::capture.output({
    arsbridge:::.progress_emit_line(list(
      stage = "ars_to_ard", stage_idx = 2L, n_stages = 3L,
      i = 5L, n = 32L, label = "AN_T_14_1_1_001"))
  })
  cat(lines, file = log, sep = "\n", append = TRUE)

  info <- arsbridge:::.workflow_read_progress(log)
  expect_equal(info$ev$stage, "ars_to_ard")
  expect_equal(info$ev$i, 5L)
  ## The tail is now the RAW last lines, shown behind a "Run log" expander
  ## rather than under the bar. Filtering the [progress] lines out of it was
  ## what left it frozen on the worker's start-up chatter: with verbose off,
  ## every line the worker writes after that IS a progress line.
  expect_true(any(grepl("Parsed 9 TLF", info$tail)))
  expect_true(any(grepl("^\\[progress\\]", info$tail)))
  ## The liveness signal: when the worker last said anything at all.
  expect_false(is.na(info$last_seen))

  expect_null(arsbridge:::.workflow_read_progress(NULL))
  expect_null(arsbridge:::.workflow_read_progress(tempfile("gone_")))
})

test_that("the progress block is a bar with an event and a spinner without", {
  with_ev <- arsbridge:::.workflow_progress_ui(list(
    ev = list(stage = "ars_fill_shell", stage_idx = 3L, n_stages = 3L,
              i = 4L, n = 9L, label = "T_14_2_1"),
    tail = character()))
  html <- as.character(with_ev)
  expect_match(html, "progress-bar")
  expect_match(html, "Filling the workbook")
  ## The count is of outputs FINISHED, and it says so -- the label beside it
  ## names the one still in flight.
  expect_match(html, "T_14_2_1 \\(4 of 9 done\\)")

  waiting <- arsbridge:::.workflow_progress_ui(list(ev = NULL,
                                                    tail = character()))
  expect_match(as.character(waiting), "spinner-border")
})

test_that("a named sub-step with no count still shows what it is doing", {
  ## The regression this guards: the detail was gated on n > 0, so
  ## "saving the workbook" -- the slowest step of a real fill -- rendered as
  ## a bare stage name and the panel looked frozen at 100%.
  saving <- arsbridge:::.workflow_progress_ui(list(
    ev = list(stage = "ars_fill_shell", stage_idx = 3L, n_stages = 3L,
              i = 0L, n = 0L, label = "saving the workbook"),
    tail = character()))
  expect_match(as.character(saving), "saving the workbook")
})

test_that("the panel says how long the build has been going, and when it went quiet", {
  now <- as.POSIXct("2026-08-05 12:00:00", tz = "UTC")
  info <- list(ev = NULL, last_seen = now - 30, tail = character())

  live <- arsbridge:::.workflow_liveness_ui(info, started = now - 95, now = now)
  expect_match(as.character(live), "Running for 1m 35s")
  expect_match(as.character(live), "Last reported 30s ago")
  expect_no_match(as.character(live), "may be stuck")

  ## Past the stall threshold the panel says so rather than leaving the user
  ## to guess whether a silent worker is working or dead.
  quiet <- list(ev = NULL, tail = character(),
                last_seen = now - arsbridge:::.WORKFLOW_STALL_SECONDS - 1)
  stalled <- arsbridge:::.workflow_liveness_ui(quiet, started = now - 600,
                                               now = now)
  expect_match(as.character(stalled), "may be stuck")

  ## No build in flight, nothing to say.
  expect_null(arsbridge:::.workflow_liveness_ui(info, started = NULL, now = now))
})

## ---------------------------------------------------------------------------
## Step 1 arrives filled in
## ---------------------------------------------------------------------------

test_that("reopening a project brings its paths back, and says that it did", {
  ## The complaint: ars_workflow() asked for everything again, with every box
  ## empty, even when the project it had just been used on was right there.
  td <- withr::local_tempdir()
  project <- file.path(td, "study")
  inputs <- .wfa_inputs()
  arsbridge:::.workflow_init(project, inputs$shell, inputs$spec,
                             study_id = "APX-DRM-301")

  shiny::testServer(.wfa_server(project), {
    session$flushReact()
    html <- as.character(output$project_resumed$html)
    expect_match(html, "Resuming")
    expect_match(html, arsbridge:::.WORKFLOW_STATE_FILE, fixed = TRUE)
  })

  ## The values themselves come from the panel, which is built from the
  ## state file rather than from anything the session remembers.
  panel <- .wfa_panel_html(project)
  expect_match(panel, basename(inputs$shell), fixed = TRUE)
  expect_match(panel, basename(inputs$spec), fixed = TRUE)
  expect_match(panel, "APX-DRM-301", fixed = TRUE)
})

test_that("a first run has nothing to resume and says nothing", {
  shiny::testServer(.wfa_server(NULL), {
    session$flushReact()
    expect_null(output$project_resumed)
  })
})

test_that("switching to a remembered project repopulates every field", {
  td <- withr::local_tempdir()
  store <- withr::local_tempfile(fileext = ".json")
  withr::local_options(list(arsbridge.recent_projects = store))

  inputs <- .wfa_inputs()
  other <- file.path(td, "other_study")
  arsbridge:::.workflow_init(other, inputs$shell, inputs$spec,
                             study_id = "OTHER-001")
  arsbridge:::.workflow_remember_project(other)

  current <- file.path(td, "current")
  arsbridge:::.workflow_init(current, inputs$shell, inputs$spec)

  shiny::testServer(.wfa_server(current), {
    session$flushReact()
    ## The other project is on offer; the one already open is not.
    picker <- as.character(output$recent_projects$html)
    expect_match(picker, normalizePath(other), fixed = TRUE)

    session$setInputs(recent_project = normalizePath(other))
    expect_equal(state$project_dir(), normalizePath(other))
    expect_equal(meta()$study_id, "OTHER-001")
  })
})

test_that("a remembered project that has since been emptied is refused, not opened", {
  td <- withr::local_tempdir()
  store <- withr::local_tempfile(fileext = ".json")
  withr::local_options(list(arsbridge.recent_projects = store))
  inputs <- .wfa_inputs()
  project <- file.path(td, "study")
  arsbridge:::.workflow_init(project, inputs$shell, inputs$spec)

  shiny::testServer(.wfa_server(project), {
    gone <- file.path(td, "vanished")
    dir.create(gone)
    session$setInputs(recent_project = gone)
    ## The app stays where it was rather than switching to nothing.
    expect_equal(state$project_dir(), project)
  })
})

test_that("the setup panel offers a browser when it can", {
  skip_if_not_installed("shinyFiles")
  panel <- .wfa_panel_html(NULL, pickers = TRUE)
  for (id in c("pick_project_dir", "pick_shell", "pick_spec",
               "pick_adam_dir")) {
    expect_match(panel, id, fixed = TRUE)
  }
})

test_that("without shinyFiles the panel still opens, and says what would help", {
  ## The app must never need a Suggests in order to open: the fields are what
  ## they always were, a text box you type into.
  panel <- .wfa_panel_html(NULL, pickers = FALSE)
  expect_match(panel, "shinyFiles", fixed = TRUE)
  expect_no_match(panel, "pick_shell", fixed = TRUE)
  ## And either way the text inputs remain the one place a path lives.
  expect_match(panel, "shell_path", fixed = TRUE)
})

## ---------------------------------------------------------------------------
## Never trapped in "Building..."
##
## `running` used to have exactly one exit -- the poller watching the worker
## die -- so a build that outlived the user's patience, or a poll that threw,
## left the panel showing a disabled "Building..." for the rest of the
## session. The field report was the sharp end of that: a supplement was
## dropped in, the mode line flipped to "supplement", and there was no way to
## build with it.
## ---------------------------------------------------------------------------

test_that("a running build offers a way out", {
  td <- withr::local_tempdir()
  project <- file.path(td, "study")
  inputs <- .wfa_inputs()
  arsbridge:::.workflow_init(project, inputs$shell, inputs$spec)

  shiny::testServer(.wfa_server(project), {
    ## A job as well as the flag: `running` on its own is the state the
    ## poller self-heals out of, tested separately below.
    state$job(list(is_alive = function() TRUE))
    state$running(TRUE)
    session$flushReact()
    html <- as.character(output$build_actions$html)
    expect_match(html, "Cancel")
    expect_match(html, "Building")
  })
})

test_that("cancelling kills the worker and hands the panel back", {
  td <- withr::local_tempdir()
  project <- file.path(td, "study")
  inputs <- .wfa_inputs()
  arsbridge:::.workflow_init(project, inputs$shell, inputs$spec)

  killed <- FALSE
  shiny::testServer(.wfa_server(project), {
    state$job(list(kill_tree = function() killed <<- TRUE))
    state$running(TRUE)
    state$started(Sys.time())
    session$setInputs(cancel_build = 1)

    expect_true(killed)
    expect_false(state$running())
    expect_null(state$job())
    expect_null(state$started())
    ## And the button is a build button again, not a disabled "Building...".
    expect_no_match(as.character(output$build_actions$html), "Building")
  })
})

test_that("a worker that cannot be killed still releases the panel", {
  ## kill_tree() is not in every processx, and a process that has already
  ## gone can throw on either call. Neither is a reason to stay stuck.
  td <- withr::local_tempdir()
  project <- file.path(td, "study")
  inputs <- .wfa_inputs()
  arsbridge:::.workflow_init(project, inputs$shell, inputs$spec)

  shiny::testServer(.wfa_server(project), {
    state$job(list(kill_tree = function() stop("no such method"),
                   kill = function() stop("already gone")))
    state$running(TRUE)
    session$setInputs(cancel_build = 1)
    expect_false(state$running())
    expect_null(state$job())
  })
})

test_that("running with no job to wait for self-heals", {
  ## Unreachable by design -- run_build sets both in one flush -- but the
  ## failure mode if it ever happens is a panel that is stuck forever, so the
  ## poller resets rather than returning into the same state next tick.
  td <- withr::local_tempdir()
  project <- file.path(td, "study")
  inputs <- .wfa_inputs()
  arsbridge:::.workflow_init(project, inputs$shell, inputs$spec)

  shiny::testServer(.wfa_server(project), {
    state$running(TRUE)
    state$job(NULL)
    session$flushReact()
    expect_false(state$running())
  })
})

test_that("a poll that throws does not end the polling", {
  ## The defect: readLines() on a file the worker is writing can fail, and a
  ## Shiny observer that throws is DESTROYED -- taking the only thing that
  ## ever clears `running` with it.
  td <- withr::local_tempdir()
  project <- file.path(td, "study")
  inputs <- .wfa_inputs()
  arsbridge:::.workflow_init(project, inputs$shell, inputs$spec)

  shiny::testServer(.wfa_server(project), {
    state$job(list(is_alive = function() stop("the log is locked")))
    state$running(TRUE)
    expect_no_error(session$flushReact())
    expect_true(state$running())

    ## Still polling: the next tick, on a worker that has finished, finishes
    ## the build as usual.
    state$job(list(is_alive = function() FALSE,
                   get_result = function() stop("the worker died")))
    session$elapse(700)
    expect_false(state$running())
  })
})

test_that("a worker that died with no result says where to look", {
  log <- withr::local_tempfile(fileext = ".log")
  writeLines("Error: cannot open the connection", log)
  message <- arsbridge:::.workflow_worker_message(
    simpleError("callr subprocess failed"), log)
  expect_match(message, "callr subprocess failed")
  expect_match(message, log, fixed = TRUE)

  ## No log, no promise of one.
  expect_equal(
    arsbridge:::.workflow_worker_message(simpleError("boom"), NULL), "boom")
  expect_equal(
    arsbridge:::.workflow_worker_message(simpleError("boom"),
                                         tempfile("gone_")), "boom")
})

test_that("an in-process build reports progress and leaves the last event", {
  td <- withr::local_tempdir()
  project <- file.path(td, "study")
  inputs <- .wfa_inputs()
  arsbridge:::.workflow_init(project, inputs$shell, inputs$spec)

  .wfa_no_keys(
    shiny::testServer(.wfa_server(project), {
      session$setInputs(run_build = 1)
      ## The callback fed state$progress throughout; what remains is the
      ## final event of the last stage that ran (a Word shell without data:
      ## the build stage alone).
      info <- state$progress()
      expect_false(is.null(info))
      expect_equal(info$ev$stage, "spec_to_ars")
      expect_equal(info$ev$i, info$ev$n)
    })
  )
})

test_that("the notice catches the fill that never ran at all", {
  ## Variant B of the field failure: an Excel shell with data, but zero
  ## analyses executed, so payload$fill is NULL rather than filled = 0.
  meta_x <- list(shell_path = "shells.xlsx", adam_dir = "/data/adam")
  never_ran <- list(status = "success", fill = NULL,
                    unfilled_cells = data.frame(), metadata = meta_x)
  notice <- arsbridge:::.workflow_build_notice(never_ran)
  expect_equal(notice$type, "warning")
  expect_match(notice$text, "not filled")

  ## A Word shell, or a run without data, legitimately has no fill: silence.
  meta_docx <- list(shell_path = "shells.docx", adam_dir = "/data/adam")
  expect_equal(arsbridge:::.workflow_build_notice(
    list(status = "success", fill = NULL, unfilled_cells = data.frame(),
         metadata = meta_docx))$text, "Build complete.")
  meta_nodata <- list(shell_path = "shells.xlsx", adam_dir = NA_character_)
  expect_equal(arsbridge:::.workflow_build_notice(
    list(status = "success", fill = NULL, unfilled_cells = data.frame(),
         metadata = meta_nodata))$text, "Build complete.")
})
