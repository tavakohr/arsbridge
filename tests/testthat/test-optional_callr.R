# callr is a Suggests: the workflow must build without it.
#
# The background process is a responsiveness optimisation, never a
# requirement -- everything the worker does, ars_workflow_run() also does in
# this process. When callr is missing the app is required to SAY so and build
# anyway, never to refuse.
#
# Absence has to be simulated here rather than arranged, because the ordinary
# suite cannot arrange it: testthat itself hard-depends on callr, so callr is
# always installed while these tests run. The core-minimal CI job is the other
# half of this proof -- it installs no Suggests at all, has no testthat, and
# therefore no callr.

skip_if_not_installed("shiny")

.oc_no_callr <- function() {
  ## Only callr disappears; every other package answers honestly.
  function(pkg) if (identical(pkg, "callr")) FALSE else
    requireNamespace(pkg, quietly = TRUE)
}

## Collect what the app puts on screen, without a session to put it in.
.oc_capture_notices <- function(env = parent.frame()) {
  notes <- new.env(parent = emptyenv())
  notes$seen <- character()
  testthat::local_mocked_bindings(
    showNotification = function(ui, ...) {
      notes$seen <- c(notes$seen, paste(as.character(ui), collapse = " "))
      invisible(NULL)
    },
    .package = "shiny", .env = env)
  notes
}

test_that("callr is optional, not a hard dependency", {
  ## The whole point of the PR, asserted where a future edit would trip on it.
  dcf <- read.dcf(system.file("DESCRIPTION", package = "arsbridge"),
                  fields = c("Imports", "Suggests"))
  expect_false(grepl("\\bcallr\\b", dcf[1, "Imports"]))
  expect_true(grepl("\\bcallr\\b", dcf[1, "Suggests"]))
})

test_that("a missing callr degrades to an in-process build, and says why", {
  local_mocked_bindings(.pkg_available = .oc_no_callr(), .package = "arsbridge")
  notes <- .oc_capture_notices()

  handle <- withr::with_options(
    list(arsbridge.workflow_background = TRUE),
    arsbridge:::.workflow_start_build(
      state = list(shell_path = "shell.docx", adam_spec_path = "spec.xlsx"),
      paths = list(supplement = tempfile())))

  ## NULL is the app's signal to build synchronously -- see the caller.
  expect_null(handle)
  ## And the reason is on screen. A silent fallback leaves a user staring at
  ## a frozen UI with no way to learn that one install would fix it.
  expect_length(notes$seen, 1L)
  expect_match(notes$seen, "callr is not installed", fixed = TRUE)
  expect_match(notes$seen, "install.packages", fixed = TRUE)
})

test_that("with callr present, the missing-callr fallback is not taken", {
  ## A guard that fires unconditionally is how a working optimisation gets
  ## silently switched off; this fails if the gate stops consulting
  ## .pkg_available(). It does NOT claim the launch is reached -- the
  ## version-skew gate legitimately returns first under load_all(), where the
  ## running package is not the installed one.
  skip_if_not_installed("callr")
  local_mocked_bindings(.pkg_available = function(pkg) TRUE, .package = "arsbridge")
  ## Mocked purely so no real worker can ever be spawned on these fictional
  ## paths if the skew gate does let execution through. Nothing to clean up.
  local_mocked_bindings(
    r_bg = function(...) structure(list(), class = "oc_fake_handle"),
    .package = "callr")
  notes <- .oc_capture_notices()

  withr::with_options(
    list(arsbridge.workflow_background = TRUE),
    arsbridge:::.workflow_start_build(
      state = list(shell_path = "shell.docx", adam_spec_path = "spec.xlsx"),
      paths = list(supplement = tempfile())))

  expect_false(any(grepl("callr is not installed", notes$seen, fixed = TRUE)))
})

test_that("the app completes a real build with callr unavailable", {
  ## The contract end to end: not merely "returns NULL", but "the build still
  ## happens". Background stays ENABLED here -- disabling it would take the
  ## option branch and never reach the callr gate at all.
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  skip_if_not_installed("cards")

  td <- withr::local_tempdir()
  project <- file.path(td, "study")
  arsbridge:::.workflow_init(project,
                             arsbridge_example("annotated_shell.docx"),
                             arsbridge_example("adam_spec.xlsx"))

  local_mocked_bindings(.pkg_available = .oc_no_callr(), .package = "arsbridge")

  withr::with_options(
    list(arsbridge.workflow_background = TRUE),
    withr::with_envvar(
      c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
        GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
      suppressMessages(suppressWarnings(
        shiny::testServer(arsbridge:::.ars_workflow_app(project), {
          session$setInputs(run_build = 1)
          payload <- state$last_result()
          expect_false(is.null(payload))
          expect_true(payload$status %in% c("success", "partial"))
          expect_true(file.exists(payload$artifacts$ars_json))
        })))))
})
