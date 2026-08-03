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
               c("ars_json", "validation_report", "code_dir", "ard_rds",
                 "filled_workbook", "run_log"))
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
  expect_true(all(c("sheet", "ref", "status", "reason") %in%
                    names(p$unfilled_cells)))
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
  expect_gt(nrow(res$diagnostics), 0)
  expect_true(all(nzchar(res$diagnostics$reason)))
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
