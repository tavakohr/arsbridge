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
  ## The tail excludes the [progress] lines -- they are the bar's food, not
  ## reading material -- and keeps the human output.
  expect_true(any(grepl("Parsed 9 TLF", info$tail)))
  expect_false(any(grepl("^\\[progress\\]", info$tail)))

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
  expect_match(html, "T_14_2_1 \\(4/9\\)")

  waiting <- arsbridge:::.workflow_progress_ui(list(ev = NULL,
                                                    tail = character()))
  expect_match(as.character(waiting), "spinner-border")
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
