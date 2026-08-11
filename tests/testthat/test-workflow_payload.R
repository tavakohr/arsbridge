## The payload contract.
##
## This is the seam between the engine and any UI: a Shiny app sends
## ars_workflow_run() to a background process and renders what comes back. So
## the tests here are about the SHAPE and COMPLETENESS of what comes back --
## the things a caller on the far side of a process boundary cannot check for
## itself, and cannot recover if they are missing.

SHELL_X <- test_path("fixtures", "shells_apx_drm_301.xlsx")
SPEC_X  <- test_path("fixtures", "adam_spec_apx_drm_301.xlsx")
ADAM_X  <- test_path("fixtures", "adam_apx_drm_301")

no_keys <- function(code) {
  withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(suppressWarnings(code)))
}

payload_run <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    cache <<- no_keys(ars_workflow_run(
      shell_path = SHELL_X, adam_spec_path = SPEC_X, adam_dir = ADAM_X,
      output_dir = tempfile("wf_"), study_id = "APX-DRM-301",
      derived_dt = "2026-07-30T00:00:00Z"))
    cache
  }
})

## ---------------------------------------------------------------------------
## The diagnostics hand-back -- the reason this module exists
## ---------------------------------------------------------------------------

test_that("diagnostics from every stage survive, not just the last one", {
  ## spec_to_ars() and ars_to_ard() each call diag_reset() on entry. Reading
  ## the record once at the end therefore returns only what happened after the
  ## LAST reset -- on this fixture, 1 finding out of 41. Harvesting after each
  ## stage is what makes the table complete, and this is the test that fails
  ## if someone "simplifies" it back to a single read.
  p <- payload_run()
  expect_gt(nrow(p$diagnostics), 20)
  stages <- unique(p$diagnostics$stage)
  expect_true("parse_shell" %in% stages)
  expect_true("build_ars" %in% stages)
})

test_that("the diagnostics table keeps every field a reader needs", {
  ## Dropping any of these is a silent loss on the far side of the boundary.
  ## `action` in particular: a UI that reports what broke without the "To fix"
  ## step is half a report.
  p <- payload_run()
  expect_named(p$diagnostics,
               c("stage", "severity", "input", "tlf_number", "location",
                 "problem", "action"))
  expect_true(any(!is.na(p$diagnostics$action)))
})

test_that("INFO findings survive -- they are the inferences worth reviewing", {
  ## The fill writer reports the shell-label-to-data-code pairings it made
  ## ("Female -> F") as INFO so a human can check them. Splitting the payload
  ## into fails and warnings would discard exactly those.
  p <- payload_run()
  expect_true("INFO" %in% p$diagnostics$severity)
})

test_that("an empty diagnostics table still has the full shape", {
  empty <- .EMPTY_DIAGNOSTICS()
  expect_equal(nrow(empty), 0L)
  expect_named(empty, c("stage", "severity", "input", "tlf_number",
                        "location", "problem", "action"))
})

## ---------------------------------------------------------------------------
## The rest of the contract
## ---------------------------------------------------------------------------

test_that("the payload names every artifact it produced", {
  p <- payload_run()
  expect_named(p$artifacts,
               c("ars_json", "validation_report", "code_dir", "code_paths",
                 "ard_rds", "filled_workbook", "fill_debrief", "run_log"))
  for (name in c("ars_json", "ard_rds", "filled_workbook")) {
    expect_true(file.exists(p$artifacts[[name]]), info = name)
  }
})

test_that("metadata records what the run was built with", {
  ## An archived payload has to answer "which version, from which inputs" on
  ## its own -- nobody will have the calling session any more.
  p <- payload_run()
  expect_equal(p$metadata$arsbridge_version,
               as.character(utils::packageVersion("arsbridge")))
  expect_equal(p$metadata$shell_path, SHELL_X)
  expect_equal(p$metadata$derived_dt, "2026-07-30T00:00:00Z")
  expect_false(p$metadata$llm_mode_enabled)
})

test_that("derived_dt is honoured, so a run can be reproducible", {
  ## A background process inherits no options. If this stopped being passed
  ## through, every run would stamp a different time and two identical runs
  ## would stop matching -- with nothing to show for it.
  p <- payload_run()
  ard <- readRDS(p$artifacts$ard_rds)
  stamped <- unique(ard$derived_dt[!is.na(ard$derived_dt)])
  expect_equal(stamped, "2026-07-30T00:00:00Z")
})

test_that("every stage is timed", {
  p <- payload_run()
  expect_true(all(c("spec_to_ars", "ars_to_ard", "ars_fill_shell", "total")
                  %in% names(p$timings)))
  expect_gt(p$timings$total, 0)
})

test_that("a clean run is a success", {
  p <- payload_run()
  expect_equal(p$status, "success")
  expect_null(p$error)
})

## ---------------------------------------------------------------------------
## Status, and the cases a UI branches on
## ---------------------------------------------------------------------------

test_that("status distinguishes what a user can act on", {
  diags <- function(sev) data.frame(severity = sev, stringsAsFactors = FALSE)
  expect_equal(.payload_status(diags(character()), 0, NULL), "success")
  ## WARN alone is not partial -- there are dozens of WARN sites and a healthy
  ## run trips several; that rule would make every run partial.
  expect_equal(.payload_status(diags(c("WARN", "INFO")), 0, NULL), "success")
  expect_equal(.payload_status(diags("FAIL"), 0, NULL), "partial")
  expect_equal(.payload_status(diags(character()), 1, NULL), "partial")
  expect_equal(.payload_status(diags("FAIL"), 1, "boom"), "error")
})

test_that("a run without data builds the ARS and says why it stopped there", {
  p <- no_keys(ars_workflow_run(
    shell_path = SHELL_X, adam_spec_path = SPEC_X,
    output_dir = tempfile("wf_"), adam_dir = NULL))
  expect_true(file.exists(p$artifacts$ars_json))
  expect_true(is.na(p$artifacts$ard_rds))
  expect_true(any(grepl("No ADaM directory", p$diagnostics$problem)))
  ## Not an error: building only the ARS is a normal way to use this.
  expect_equal(p$status, "success")
})

test_that("a blocked build needs fixes and reports only this run's artifacts", {
  output_dir <- withr::local_tempdir()
  paths <- list(
    code_dir = file.path(output_dir, "code"),
    ard = file.path(output_dir, "ard.rds"),
    filled = file.path(output_dir, "filled_shells.xlsx"),
    debrief = file.path(output_dir, "fill_debrief.xlsx")
  )
  dir.create(paths$code_dir)
  writeLines("stale code", file.path(paths$code_dir, "stale.R"))
  saveRDS(data.frame(stale = TRUE), paths$ard)
  writeLines("stale fill", paths$filled)
  writeLines("stale debrief", paths$debrief)

  findings <- .add_finding(
    .new_findings(), "FAIL", "groupings", "GF_EMPTY", "groups",
    "This fixed grouping has no groups.", "Add groups or make it data-driven.",
    ref = "FIXED_GROUPING_EMPTY"
  )
  gate <- list(
    blocked = TRUE,
    status = "needs-fixes",
    findings = findings,
    blocking_findings = findings,
    blocking_refs = "FIXED_GROUPING_EMPTY",
    summary = "1 blocking finding. Add groups or make it data-driven."
  )

  testthat::local_mocked_bindings(
    spec_to_ars = function(shell_path, adam_spec_path, output_path, report_path,
                           code_dir, ...) {
      writeLines("{}", output_path)
      writeLines("validation", report_path)
      list(
        ars_path = output_path,
        report_path = report_path,
        code_dir = code_dir,
        code_paths = character(0),
        validation_gate = gate,
        ars_validation = findings
      )
    },
    ars_to_ard = function(...) stop("blocked builds must not execute"),
    ars_fill_shell = function(...) stop("blocked builds must not fill"),
    write_fill_debrief = function(...) stop("blocked builds need no debrief"),
    .package = "arsbridge"
  )

  p <- ars_workflow_run(
    shell_path = SHELL_X,
    adam_spec_path = SPEC_X,
    adam_dir = ADAM_X,
    output_dir = output_dir
  )

  expect_equal(p$status, "partial")
  expect_true(p$needs_fixes)
  expect_true(p$validation_gate$blocked)
  expect_null(p$error)
  expect_true(file.exists(p$artifacts$ars_json))
  expect_true(file.exists(p$artifacts$validation_report))
  expect_true(all(is.na(unlist(p$artifacts[c(
    "code_dir", "ard_rds", "filled_workbook", "fill_debrief"
  )]))))
  expect_true(any(p$diagnostics$stage == "validate_ars" &
                    p$diagnostics$severity == "FAIL"))
})

test_that("a successful rebuild reports only scripts written by this run", {
  output_dir <- withr::local_tempdir()
  code_dir <- file.path(output_dir, "code")
  dir.create(code_dir)
  stale_path <- file.path(code_dir, "OLD_OUTPUT.R")
  current_path <- file.path(code_dir, "T_01.R")
  writeLines("# stale", stale_path)

  gate <- .validation_gate(.new_findings())
  testthat::local_mocked_bindings(
    spec_to_ars = function(shell_path, adam_spec_path, output_path, report_path,
                           code_dir, ...) {
      writeLines("{}", output_path)
      writeLines("validation", report_path)
      writeLines("# current", current_path)
      list(
        ars_path = output_path,
        report_path = report_path,
        code_dir = code_dir,
        code_paths = c(T_01 = current_path),
        validation_gate = gate,
        ars_validation = gate$findings
      )
    },
    .package = "arsbridge"
  )

  payload <- ars_workflow_run(
    shell_path = SHELL_X,
    adam_spec_path = SPEC_X,
    output_dir = output_dir,
    adam_dir = NULL
  )

  expect_true(is.na(payload$artifacts$code_dir))
  expect_identical(unname(payload$artifacts$code_paths), current_path)
  expect_false(stale_path %in% payload$artifacts$code_paths)
})


test_that("a late build failure retains only its current repair JSON", {
  output_dir <- withr::local_tempdir()
  ars_path <- file.path(output_dir, "reporting_event.json")
  stale_paths <- c(
    file.path(output_dir, "validation_report.xlsx"),
    file.path(output_dir, "code", "OLD_OUTPUT.R"),
    file.path(output_dir, "ard.rds"),
    file.path(output_dir, "filled_shells.xlsx"),
    file.path(output_dir, "fill_debrief.xlsx")
  )
  for (path in stale_paths) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines("stale", path)
  }
  writeLines('{"run":"prior"}', ars_path)

  write_repair <- TRUE
  testthat::local_mocked_bindings(
    spec_to_ars = function(..., output_path, .on_artifact_written = NULL) {
      if (write_repair) {
        writeLines('{"run":"current"}', output_path)
        if (is.function(.on_artifact_written)) {
          .on_artifact_written(list(ars_json = output_path))
        }
      }
      stop("forced failure after writing repair JSON")
    },
    ars_to_ard = function(...) stop("a failed build must not execute"),
    .package = "arsbridge"
  )

  current <- ars_workflow_run(
    shell_path = SHELL_X,
    adam_spec_path = SPEC_X,
    adam_dir = ADAM_X,
    output_dir = output_dir
  )

  expect_equal(current$status, "error")
  expect_equal(current$failed_stage, "spec_to_ars")
  expect_identical(current$artifacts$ars_json, ars_path)
  expect_identical(readLines(ars_path), '{"run":"current"}')
  expect_length(current$artifacts$code_paths, 0L)
  expect_true(all(is.na(unlist(current$artifacts[c(
    "validation_report", "code_dir", "ard_rds",
    "filled_workbook", "fill_debrief"
  )]))))

  write_repair <- FALSE
  stale_only <- ars_workflow_run(
    shell_path = SHELL_X,
    adam_spec_path = SPEC_X,
    adam_dir = ADAM_X,
    output_dir = output_dir
  )

  expect_true(is.na(stale_only$artifacts$ars_json))
})


test_that("a late report failure retains exact current-run code paths", {
  output_dir <- withr::local_tempdir()
  code_dir <- file.path(output_dir, "code")
  dir.create(code_dir)
  stale_path <- file.path(code_dir, "OLD_OUTPUT.R")
  writeLines("# from a prior run", stale_path)

  testthat::local_mocked_bindings(
    write_validation_report = function(...) {
      stop("forced validation report failure")
    },
    .package = "arsbridge"
  )

  payload <- no_keys(ars_workflow_run(
    shell_path = test_path("fixtures", "annotated_shell_2tlf_minimal.docx"),
    adam_spec_path = test_path("fixtures", "adam_spec_minimal.xlsx"),
    output_dir = output_dir
  ))

  expected <- stats::setNames(
    file.path(code_dir, c("T_14_1_1.R", "T_14_3_1.R")),
    c("T_14_1_1", "T_14_3_1")
  )
  expect_equal(payload$status, "error")
  expect_equal(payload$failed_stage, "spec_to_ars")
  expect_match(payload$error, "forced validation report failure")
  expect_identical(payload$artifacts$code_paths, expected)
  expect_true(all(file.exists(expected)))
  expect_false(stale_path %in% payload$artifacts$code_paths)
})


test_that("a failing stage returns a payload instead of throwing", {
  ## The moment the caller most needs the payload is when the run died: it
  ## carries which stage, the message, and the log to read.
  p <- no_keys(ars_workflow_run(
    shell_path = test_path("fixtures", "does_not_exist.xlsx"),
    adam_spec_path = SPEC_X, output_dir = tempfile("wf_"),
    log_path = "/tmp/run.log"))
  expect_equal(p$status, "error")
  expect_equal(p$failed_stage, "spec_to_ars")
  expect_true(nzchar(p$error))
  expect_equal(p$artifacts$run_log, "/tmp/run.log")
  expect_named(p$diagnostics,
               c("stage", "severity", "input", "tlf_number", "location",
                 "problem", "action"))
})

test_that("unfilled workbook cells come back with their reasons", {
  ## The columns are the contract: a caller on the far side of a process
  ## boundary renders this frame and cannot recover a missing field. They must
  ## be there even when nothing is unfilled -- which is this fixture's case
  ## now that nested blocks expand, so the frame is legitimately empty and the
  ## test asserts the shape rather than a count it no longer controls.
  p <- payload_run()
  provenance <- c(
    "row", "col", "col_label", "analysis_id", "row_label", "method_id",
    "placeholder", "ars_grouping_id", "ars_group_label", "variable_level",
    "parent_level", "ard_lookup_key"
  )
  expect_true(all(c("sheet", "ref", "status", "reason", provenance) %in%
                    names(p$unfilled_cells)))
  expect_identical(names(p$unfilled_cells), names(p$fill_census))
  expect_equal(p$fill$pending, 0)
  if (nrow(p$unfilled_cells) > 0) {
    expect_true(all(nzchar(p$unfilled_cells$reason)))
  }

  ## The reason text itself is still exercised, on a run that HAS a gap: an
  ## ARD missing one analysis leaves its cells with nothing to fetch.
  ard <- readRDS(p$artifacts$ard_rds)
  gap <- ard[!(as.character(ard$analysis_id) %in% "AN_T_14_1_2_001"), ]
  res <- suppressMessages(suppressWarnings(ars_fill_shell(
    shell_path = SHELL_X, ars = p$artifacts$ars_json, ard = gap,
    output_path = tempfile(fileext = ".xlsx"), adam_dir = ADAM_X,
    overwrite = TRUE)))
  unresolved <- res$census[res$census$status != "filled", ]
  expect_gt(nrow(unresolved), 0)
  expect_true(all(nzchar(unresolved$reason)))
})

test_that("the payload survives serialization, which is how it travels", {
  ## callr returns the value over a serialized connection. Anything that does
  ## not survive readRDS(saveRDS()) does not reach the app.
  p <- payload_run()
  path <- tempfile(fileext = ".rds")
  saveRDS(p, path)
  back <- readRDS(path)
  expect_identical(back$status, p$status)
  expect_identical(back$diagnostics, p$diagnostics)
  expect_identical(back$metadata, p$metadata)
})

test_that("the fill's headline counts ride the payload", {
  ## A UI deciding "did this build actually produce a filled workbook?"
  ## reads three numbers, not the per-cell census.
  p <- payload_run()
  expect_named(p$fill, c("filled", "pending", "skipped"))
  expect_gt(p$fill$filled, 0)
})

test_that("a run with no fill stage carries a NULL fill, not a fake zero", {
  p <- no_keys(ars_workflow_run(
    shell_path = SHELL_X, adam_spec_path = SPEC_X,
    output_dir = tempfile("wf_"), adam_dir = NULL))
  expect_null(p$fill)
})

test_that("a clean shell's run says the fill never happened, and why", {
  ## The fully clean shell executes zero analyses; ars_to_ard() returns NULL
  ## and the fill stage is skipped by its guard -- so there is no per-cell
  ## census to warn from. Found live during acceptance: that run announced
  ## plain success. The payload must carry the explanation itself.
  skip_if_not_installed("openxlsx2")
  clean <- tempfile(fileext = ".xlsx")
  wb <- openxlsx2::wb_workbook()
  wb$add_worksheet("Table 14.1.1")
  cells <- list(c(1, 1, "Table 14.1.1"), c(2, 1, "Summary of Disposition"),
                c(4, 1, "Item"), c(4, 2, "Placebo"), c(4, 3, "Drug 10 mg"),
                c(5, 1, "Subjects treated"), c(5, 2, "xx"), c(5, 3, "xx"))
  for (cell in cells) {
    wb$add_data(sheet = "Table 14.1.1", x = cell[[3]],
                start_row = as.integer(cell[[1]]),
                start_col = as.integer(cell[[2]]), col_names = FALSE)
  }
  wb$save(clean)

  p <- no_keys(ars_workflow_run(
    shell_path = clean, adam_spec_path = SPEC_X, adam_dir = ADAM_X,
    output_dir = tempfile("wf_clean_")))

  expect_null(p$fill)
  expect_true(any(p$diagnostics$stage == "execute_ard" &
                    grepl("no executable analyses", p$diagnostics$problem)))
  expect_true(any(p$diagnostics$stage == "fill_shell" &
                    grepl("was not produced", p$diagnostics$problem)))
  expect_true(is.na(p$artifacts$filled_workbook))
})

## ---------------------------------------------------------------------------
## The fill debrief artifact (PR C2)
## ---------------------------------------------------------------------------

test_that("a filling run writes the fill debrief and hands back the census", {
  p <- payload_run()
  expect_false(is.na(p$artifacts$fill_debrief))
  expect_true(file.exists(p$artifacts$fill_debrief))

  ## The census travels in the payload, filled cells included, so the app
  ## can roll it up without re-reading any file.
  expect_s3_class(p$fill_census, "data.frame")
  expect_true("filled" %in% p$fill_census$status)
  provenance <- c(
    "row", "col", "col_label", "analysis_id", "row_label", "method_id",
    "placeholder", "ars_grouping_id", "ars_group_label", "variable_level",
    "parent_level", "ard_lookup_key"
  )
  expect_true(all(provenance %in% names(p$fill_census)))

  ## And the workbook itself exposes the same provenance in its census sheet.
  wb <- openxlsx2::wb_load(p$artifacts$fill_debrief)
  expect_true(all(c("Fill census", "Columns", "Reasons", "Legend") %in%
                    unname(openxlsx2::wb_get_sheet_names(wb))))
  workbook_census <- openxlsx2::wb_to_df(wb, sheet = "Fill census")
  expect_true(all(provenance %in% names(workbook_census)))
})

test_that("a run that never fills produces no debrief, and no error", {
  p <- no_keys(ars_workflow_run(
    shell_path = SHELL_X, adam_spec_path = SPEC_X,
    output_dir = tempfile("wf_"), study_id = "APX-DRM-301"))
  expect_true(is.na(p$artifacts$fill_debrief))
  expect_null(p$fill_census)
  expect_false(identical(p$status, "error"))
})
