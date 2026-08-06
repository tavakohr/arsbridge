## The progress channel.
##
## One hook, two consumers: the in-process app forwards events to a Shiny
## progress bar; the background worker prints one line per event to stdout
## and the app parses the run log back. The tests here are about the parts a
## bug would silently break: the emit/parse round trip, the arithmetic, and
## above all the guarantee that a build with nobody listening behaves exactly
## as before -- that guarantee is what protects the byte-identity baseline.

SHELL_P <- test_path("fixtures", "shells_apx_drm_301.xlsx")
SPEC_P  <- test_path("fixtures", "adam_spec_apx_drm_301.xlsx")
ADAM_P  <- test_path("fixtures", "adam_apx_drm_301")

no_keys_p <- function(code) {
  withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(suppressWarnings(code)))
}

## ---------------------------------------------------------------------------
## The pure pieces
## ---------------------------------------------------------------------------

test_that("an emitted line parses back to the event, label spaces included", {
  ev <- list(stage = "ars_fill_shell", stage_idx = 3L, n_stages = 3L,
             i = 2L, n = 9L, label = "Table 14.1.5")
  line <- capture.output(.progress_emit_line(ev))
  parsed <- .parse_progress_line(line)
  expect_equal(parsed$stage, "ars_fill_shell")
  expect_equal(parsed$stage_idx, 3L)
  expect_equal(parsed$n_stages, 3L)
  expect_equal(parsed$i, 2L)
  expect_equal(parsed$n, 9L)
  expect_equal(parsed$label, "Table 14.1.5")
})

test_that("the last progress line wins, and garbage yields NULL", {
  lines <- c("Parsed 9 TLF sections from shells.xlsx",
             "[progress] stage=spec_to_ars stage_idx=1 n_stages=3 i=1 n=9 label=T-14-1-1",
             "some other output",
             "[progress] stage=spec_to_ars stage_idx=1 n_stages=3 i=4 n=9 label=T-14-2-1")
  parsed <- .parse_progress_line(lines)
  expect_equal(parsed$i, 4L)
  expect_equal(parsed$label, "T-14-2-1")

  expect_null(.parse_progress_line(character()))
  expect_null(.parse_progress_line(c("nothing", "to", "see")))
  expect_null(.parse_progress_line("[progress] mangled beyond=recognition"))
})

test_that("a stage-enter event has no label and still round-trips", {
  ev <- list(stage = "ars_to_ard", stage_idx = 2L, n_stages = 3L,
             i = 0L, n = 0L, label = NA_character_)
  parsed <- .parse_progress_line(capture.output(.progress_emit_line(ev)))
  expect_true(is.na(parsed$label))
  expect_equal(parsed$i, 0L)
})

test_that("the overall fraction walks the stages in order", {
  frac <- function(idx, i, n) .progress_fraction(
    list(stage_idx = idx, n_stages = 3L, i = i, n = n))
  expect_equal(frac(1L, 0L, 0L), 0)
  expect_equal(frac(1L, 9L, 9L), 1 / 3)
  expect_equal(frac(2L, 16L, 32L), 1 / 3 + 1 / 6)
  expect_equal(frac(3L, 9L, 9L), 1)
  ## Malformed events clamp instead of escaping [0, 1].
  expect_equal(frac(5L, 2L, 1L), 1)
  expect_gte(.progress_fraction(list()), 0)
})

test_that("stage labels read as actions, with a fallback", {
  expect_equal(.progress_stage_label("spec_to_ars"),
               "Building the reporting event")
  expect_equal(.progress_stage_label("ars_fill_shell"),
               "Filling the workbook")
  expect_equal(.progress_stage_label("something_new"), "Working")
})

test_that("a tick with no listener is silent, and a throwing one is contained", {
  withr::with_options(list(arsbridge.progress = NULL), {
    expect_no_error(.progress_tick(1L, 10L, "T-1"))
  })
  withr::with_options(
    list(arsbridge.progress = function(...) stop("a broken progress bar")), {
      ## A progress callback must never take a build down.
      expect_no_error(.progress_tick(1L, 10L, "T-1"))
    })
})

## ---------------------------------------------------------------------------
## Through the engine
## ---------------------------------------------------------------------------

test_that("a run reports every stage, in order, to completion", {
  events <- list()
  no_keys_p(ars_workflow_run(
    shell_path = SHELL_P, adam_spec_path = SPEC_P, adam_dir = ADAM_P,
    output_dir = tempfile("wfp_"), study_id = "APX-DRM-301",
    on_progress = function(ev) events[[length(events) + 1L]] <<- ev))

  expect_gt(length(events), 3)
  stages <- unique(vapply(events, function(e) e$stage, character(1)))
  expect_equal(stages, c("spec_to_ars", "ars_to_ard", "ars_fill_shell"))
  expect_true(all(vapply(events, function(e) e$n_stages, integer(1)) == 3L))

  ## Each stage's ticks reach their own n: the bar arrives, never stalls.
  for (stage in stages) {
    of_stage <- Filter(function(e) identical(e$stage, stage), events)
    last <- of_stage[[length(of_stage)]]
    expect_equal(last$i, last$n, info = stage)
    expect_gt(last$n, 0, label = paste(stage, "final n"))
  }

  ## The hook must not leak into the session once the run is over.
  expect_null(getOption("arsbridge.progress"))
})

test_that("every stage ends on a named step, so none of them runs out silent", {
  ## The defect this pins: the ticks only ever covered the per-item LOOPS, so
  ## the work after the last item -- writing the reporting event, binding the
  ## ARD, and above all openxlsx2::wb_save() -- ran with the bar already at
  ## 100% and nothing changing on screen. On a real study that is the longest
  ## silence of the run, and it read as a hang.
  events <- list()
  no_keys_p(ars_workflow_run(
    shell_path = SHELL_P, adam_spec_path = SPEC_P, adam_dir = ADAM_P,
    output_dir = tempfile("wfp_"), study_id = "APX-DRM-301",
    on_progress = function(ev) events[[length(events) + 1L]] <<- ev))

  final <- c(spec_to_ars = "writing the reporting event",
             ars_to_ard = "assembling the results",
             ars_fill_shell = "saving the workbook")
  for (stage in names(final)) {
    of_stage <- Filter(function(e) identical(e$stage, stage), events)
    expect_equal(of_stage[[length(of_stage)]]$label, final[[stage]],
                 info = stage)
  }

  ## And the counts are of work FINISHED: no per-item tick claims the whole
  ## stage before the named closing step does.
  for (stage in names(final)) {
    items <- Filter(function(e) identical(e$stage, stage) && !is.na(e$label) &&
                      !identical(e$label, final[[stage]]) && e$n > 0, events)
    if (length(items) == 0) next
    expect_lt(max(vapply(items, function(e) e$i, integer(1))),
              items[[1]]$n)
  }
})

test_that("the stage plan counts only the stages that will run", {
  events <- list()
  no_keys_p(ars_workflow_run(
    shell_path = SHELL_P, adam_spec_path = SPEC_P, adam_dir = NULL,
    output_dir = tempfile("wfp_"),
    on_progress = function(ev) events[[length(events) + 1L]] <<- ev))
  expect_true(all(vapply(events, function(e) e$n_stages, integer(1)) == 1L))
  stages <- unique(vapply(events, function(e) e$stage, character(1)))
  expect_equal(stages, "spec_to_ars")
})

test_that("the tick sites are live: a listener installed by hand hears them", {
  ## The loops call .progress_tick() unconditionally and the tick reads the
  ## option -- so a sentinel option hears the build even without going
  ## through ars_workflow_run(). If someone gates a tick site behind
  ## verbose, this is the test that notices.
  ticks <- 0L
  withr::with_options(
    list(arsbridge.progress = function(...) ticks <<- ticks + 1L), {
      no_keys_p(ars_workflow_run(
        shell_path = SHELL_P, adam_spec_path = SPEC_P, adam_dir = NULL,
        output_dir = tempfile("wfp_")))
    })
  expect_gt(ticks, 0)
})

test_that("with nobody listening, on_progress = NULL is the default no-op", {
  ## The opt-in contract that protects the byte-identity baseline: a default
  ## session has no option set, so every tick returns after one getOption().
  ## (That the outputs themselves are unchanged is pinned by the golden-diff
  ## ritual; here we pin that no hook is installed or left behind.)
  expect_null(getOption("arsbridge.progress"))
  no_keys_p(ars_workflow_run(
    shell_path = SHELL_P, adam_spec_path = SPEC_P, adam_dir = NULL,
    output_dir = tempfile("wfp_")))
  expect_null(getOption("arsbridge.progress"))
})
