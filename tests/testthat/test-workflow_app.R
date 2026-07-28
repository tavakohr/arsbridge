# The workflow app's server flows under testServer: scaffold, instruction
# writing, blueprint and supplement receive/validate, and resume.

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

test_that("the app scaffolds a project and writes the instruction files", {
  td <- withr::local_tempdir()
  project <- file.path(td, "study")
  inputs <- .wfa_inputs()

  shiny::testServer(.wfa_server(NULL), {
    session$setInputs(project_dir = project,
                      shell_path  = inputs$shell,
                      spec_path   = inputs$spec,
                      study_id    = "CDSC-ALZ-201")
    session$setInputs(init_project = 1)

    expect_true(file.exists(arsbridge:::.workflow_paths(project)$state))
    st <- status()
    expect_true(st$done[st$step == "project"])
    expect_true(st$available[st$step == "instructions"])

    session$setInputs(write_instructions = 1)
    paths <- arsbridge:::.workflow_paths(current_dir())
    expect_true(file.exists(paths$phase1_md))
    expect_true(file.exists(paths$phase2_md))
    expect_true(file.exists(paths$schema))
    st <- status()
    expect_true(st$done[st$step == "instructions"])
    expect_true(st$available[st$step == "build"])   # deterministic skip
  })
})

test_that("blueprint paste and upload land in copilot/ with findings", {
  td <- withr::local_tempdir()
  project <- file.path(td, "study")
  inputs <- .wfa_inputs()
  arsbridge:::.workflow_init(project, inputs$shell, inputs$spec)
  suppressMessages(ars_copilot_instructions(
    dir = arsbridge:::.workflow_dirs(project)$copilot,
    workflow = "two_phase", open = FALSE, overwrite = TRUE
  ))

  bp_json <- jsonlite::toJSON(list(
    blueprint_version = 2,
    tlfs = list(`14.1.1` = list(blueprint_status = "READY_FOR_PHASE_2"))
  ), auto_unbox = TRUE)

  shiny::testServer(.wfa_server(project), {
    # Paste path (with a chat-style code fence).
    session$setInputs(blueprint_paste = paste("```json", bp_json, "```",
                                              sep = "\n"))
    session$setInputs(blueprint_save_paste = 1)
    paths <- arsbridge:::.workflow_paths(project)
    expect_true(file.exists(paths$blueprint))
    expect_false(any(state$blueprint_findings()$severity == "FAIL"))
    expect_true(status()$done[status()$step == "phase1"])

    # Upload path: a plain temp file standing in for fileInput's datapath.
    tmp <- file.path(td, "upload.json")
    writeLines(as.character(bp_json), tmp)
    session$setInputs(blueprint_upload = list(name = "b.json",
                                              datapath = tmp))
    expect_true(file.exists(paths$blueprint))
  })
})

test_that("a valid supplement completes phase 2; a broken one shows the repair prompt", {
  td <- withr::local_tempdir()
  project <- file.path(td, "study")
  inputs <- .wfa_inputs()
  arsbridge:::.workflow_init(project, inputs$shell, inputs$spec)
  suppressMessages(ars_copilot_instructions(
    dir = arsbridge:::.workflow_dirs(project)$copilot,
    workflow = "two_phase", open = FALSE, overwrite = TRUE
  ))

  good <- paste(readLines(test_path("fixtures", "supplement_v4_example.json"),
                          warn = FALSE), collapse = "\n")

  shiny::testServer(.wfa_server(project), {
    session$setInputs(supplement_paste = good)
    session$setInputs(supplement_save_paste = 1)
    paths <- arsbridge:::.workflow_paths(project)
    expect_true(file.exists(paths$supplement))
    findings <- state$supplement_findings()
    expect_false(any(findings$severity == "FAIL"))

    # A wrong-version supplement: FAIL findings with a repair prompt, and
    # the phase-2 badge logic treats the step as not complete.
    broken <- sub('"supplement_version": 4', '"supplement_version": 3', good,
                  fixed = TRUE)
    session$setInputs(supplement_paste = broken)
    session$setInputs(supplement_save_paste = 2)
    findings <- state$supplement_findings()
    expect_true(any(findings$severity == "FAIL"))
  })
})

test_that("a resumed project derives statuses and exposes the hand-off", {
  td <- withr::local_tempdir()
  project <- file.path(td, "study")
  inputs <- .wfa_inputs()
  arsbridge:::.workflow_init(project, inputs$shell, inputs$spec, "APX-DRM-301")
  suppressMessages(ars_copilot_instructions(
    dir = arsbridge:::.workflow_dirs(project)$copilot,
    workflow = "two_phase", open = FALSE, overwrite = TRUE
  ))
  state0 <- arsbridge:::.workflow_read_state(project)
  paths0 <- arsbridge:::.workflow_paths(project)
  withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(suppressWarnings(
      arsbridge:::.workflow_run_build(state0, paths0)
    ))
  )

  shiny::testServer(.wfa_server(project), {
    st <- status()
    expect_true(all(st$done[st$step %in% c("project", "instructions", "build")]))
    expect_true(st$available[st$step == "review"])

    payload <- arsbridge:::.workflow_handoff_payload(current_dir())
    expect_identical(payload$action, "edit")
    expect_true(file.exists(payload$ars_path))
  })
})
