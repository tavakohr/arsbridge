# The workflow project model: folder scaffold, disk-derived step statuses,
# JSON receive (paste/upload), the blueprint pre-flight, and the build helper.

.wfp_inputs <- function() {
  list(
    shell = arsbridge_example("annotated_shell.docx"),
    spec  = arsbridge_example("adam_spec.xlsx")
  )
}

test_that("init scaffolds the project and round-trips its state", {
  td <- withr::local_tempdir()
  project <- file.path(td, "study")
  inputs <- .wfp_inputs()

  state <- arsbridge:::.workflow_init(project, inputs$shell, inputs$spec,
                                      "CDSC-ALZ-201")
  for (d in arsbridge:::.workflow_dirs(project)) expect_true(dir.exists(d))
  expect_true(file.exists(arsbridge:::.workflow_paths(project)$state))

  back <- arsbridge:::.workflow_read_state(project)
  expect_identical(back$study_id, "CDSC-ALZ-201")
  expect_true(file.exists(back$shell_path))
  expect_true(file.exists(back$adam_spec_path))

  # Touch merges fields and refreshes the timestamp.
  arsbridge:::.workflow_touch_state(project, study_id = "CDSC-ALZ-202")
  expect_identical(arsbridge:::.workflow_read_state(project)$study_id,
                   "CDSC-ALZ-202")
})

test_that("init rejects wrong or missing inputs", {
  td <- withr::local_tempdir()
  inputs <- .wfp_inputs()

  expect_error(
    arsbridge:::.workflow_init(file.path(td, "p"), "not_there.docx", inputs$spec),
    "docx"
  )
  expect_error(
    arsbridge:::.workflow_init(file.path(td, "p"), inputs$shell, "spec.csv"),
    "xlsx"
  )
})

test_that("step statuses are derived purely from files, with the skip rule", {
  td <- withr::local_tempdir()
  project <- file.path(td, "study")
  inputs <- .wfp_inputs()

  # Nothing yet: only the project step is available.
  st0 <- arsbridge:::.workflow_status(project)
  expect_false(any(st0$done))
  expect_identical(st0$available, c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))

  arsbridge:::.workflow_init(project, inputs$shell, inputs$spec)
  st1 <- arsbridge:::.workflow_status(project)
  expect_true(st1$done[st1$step == "project"])
  expect_true(st1$available[st1$step == "instructions"])
  expect_false(st1$available[st1$step == "build"])

  # Instruction files unlock BOTH phases and the build (deterministic skip).
  suppressMessages(ars_copilot_instructions(
    dir = arsbridge:::.workflow_dirs(project)$copilot,
    workflow = "two_phase", open = FALSE, overwrite = TRUE
  ))
  st2 <- arsbridge:::.workflow_status(project)
  expect_true(st2$done[st2$step == "instructions"])
  expect_true(all(st2$available[st2$step %in% c("phase1", "phase2", "build")]))
  expect_false(st2$done[st2$step == "phase1"])

  # Dropping the built event marks build done and review available.
  paths <- arsbridge:::.workflow_paths(project)
  dir.create(dirname(paths$ars), recursive = TRUE, showWarnings = FALSE)
  writeLines("{}", paths$ars)
  st3 <- arsbridge:::.workflow_status(project)
  expect_true(st3$done[st3$step == "build"])
  expect_true(st3$available[st3$step == "review"])

  # A vanished input flags the project as broken.
  arsbridge:::.workflow_touch_state(project,
                                    shell_path = file.path(td, "gone.docx"))
  st4 <- arsbridge:::.workflow_status(project)
  expect_false(st4$done[st4$step == "project"])
  expect_match(st4$detail[st4$step == "project"], "no longer exists")
})

test_that("receive_json takes clean and fenced pastes and rejects garbage", {
  td <- withr::local_tempdir()
  dest <- file.path(td, "phase1", "blueprint.json")

  ok <- arsbridge:::.workflow_receive_json('{"a": 1}', dest, "the blueprint")
  expect_true(ok$ok)
  expect_true(file.exists(dest))

  fenced <- paste("```json", '{"b": 2}', "```", sep = "\n")
  ok2 <- arsbridge:::.workflow_receive_json(fenced, dest, "the blueprint")
  expect_true(ok2$ok)
  expect_identical(jsonlite::fromJSON(dest)$b, 2L)

  bad <- arsbridge:::.workflow_receive_json('{"broken": ', dest, "the blueprint")
  expect_false(bad$ok)
  expect_match(bad$message, "Not valid JSON")

  empty <- arsbridge:::.workflow_receive_json("", dest, "the blueprint")
  expect_false(empty$ok)
})

test_that(".validate_blueprint is tolerant but catches unusable files", {
  td <- withr::local_tempdir()
  write_bp <- function(x) {
    p <- file.path(td, "bp.json")
    writeLines(jsonlite::toJSON(x, auto_unbox = TRUE), p)
    p
  }

  clean <- write_bp(list(
    blueprint_version = 2,
    tlfs = list(
      `14.1.1` = list(blueprint_status = "READY_FOR_PHASE_2"),
      `14.2.1` = list(blueprint_status = "READY_WITH_REVIEW")
    )
  ))
  f <- arsbridge:::.validate_blueprint(clean)
  expect_false(any(f$severity == "FAIL"))
  expect_true(any(f$severity == "INFO" & grepl("READY_WITH_REVIEW", f$problem)))

  wrong_version <- write_bp(list(blueprint_version = 1,
                                 tlfs = list(`14.1.1` = list())))
  f2 <- arsbridge:::.validate_blueprint(wrong_version)
  expect_true(any(f2$severity == "FAIL" & grepl("version", f2$problem)))

  no_tlfs <- write_bp(list(blueprint_version = 2))
  f3 <- arsbridge:::.validate_blueprint(no_tlfs)
  expect_true(any(f3$severity == "FAIL" & f3$where == "tlfs"))

  incomplete <- write_bp(list(
    blueprint_version = 2,
    tlfs = list(`14.1.1` = list(blueprint_status = "BLUEPRINT_INCOMPLETE",
                                note = "__REQUIRED_VALUE__"))
  ))
  f4 <- arsbridge:::.validate_blueprint(incomplete)
  expect_true(any(f4$severity == "WARN" & grepl("INCOMPLETE", f4$problem)))
  expect_true(any(f4$severity == "INFO" & f4$where == "placeholders"))

  malformed <- file.path(td, "broken.json")
  writeLines('{"nope', malformed)
  f5 <- arsbridge:::.validate_blueprint(malformed)
  expect_identical(nrow(f5), 1L)
  expect_identical(f5$severity, "FAIL")
})

test_that("the build helper produces the reporting event deterministically", {
  td <- withr::local_tempdir()
  project <- file.path(td, "study")
  inputs <- .wfp_inputs()
  arsbridge:::.workflow_init(project, inputs$shell, inputs$spec, "APX-DRM-301")

  state <- arsbridge:::.workflow_read_state(project)
  paths <- arsbridge:::.workflow_paths(project)
  res <- withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(suppressWarnings(
      arsbridge:::.workflow_run_build(state, paths)
    ))
  )

  expect_true(file.exists(paths$ars))
  expect_true(file.exists(paths$report))
  expect_identical(res$extraction_mode, "deterministic")
  expect_gt(res$n_tlfs, 0)

  # Hand-off payload points the editor at the built artifacts.
  payload <- arsbridge:::.workflow_handoff_payload(project)
  expect_identical(payload$action, "edit")
  expect_identical(payload$ars_path, paths$ars)
  expect_true(file.exists(payload$adam_spec_path))
  expect_identical(payload$report_path, paths$report)

  # And the built event opens in the editor's model layer.
  model <- ars_to_model(paths$ars)
  expect_s3_class(model, "ars_model")
})

test_that("the shipped phase instruction files carry no stale version-3 strings", {
  for (f in c("arsbridge_phase1_blueprint_instructions.md",
              "arsbridge_phase2_build_instructions.md",
              "arsbridge_copilot_instructions.md")) {
    path <- system.file("copilot", f, package = "arsbridge")
    if (!nzchar(path)) path <- file.path("../../inst/copilot", f)
    skip_if(!file.exists(path))
    text <- readLines(path, warn = FALSE)
    expect_false(any(grepl("version 3", text)), label = f)
  }
})
