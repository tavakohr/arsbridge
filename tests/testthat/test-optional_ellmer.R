# ellmer is a Suggests: it is asked for only once a run has resolved that it
# will actually call an LLM.
#
# Both readers -- extract_shell_llm() and enrich_with_llm() -- reach the model
# through .enrich_structured() and nothing else (the extractor takes it as
# `call_fn`), so intercepting that one function answers "did this run invoke an
# LLM?" behaviourally rather than by reading the source. Every case below runs
# that interception, so a guard moved above the mode resolution, or a
# deterministic path that grew a live call, fails here.
#
# The other half -- what happens when ellmer is genuinely absent -- is proven
# in the core-minimal CI job, the only place absence is real.

.oe_inputs <- function() {
  list(shell = arsbridge_example("annotated_shell.xlsx"),
       spec  = arsbridge_example("adam_spec.xlsx"))
}

.oe_blank_env <- function(extra = character()) {
  e <- c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
         GLM_API_KEY = "", ARS_LLM_PROVIDER = "")
  e[names(extra)] <- extra
  e
}

## Run the pipeline while counting LLM invocations. The mock returns NULL,
## which is the "terminal failure" the callers already handle by falling back
## to heuristics -- so a run reaches the end either way and the COUNT is the
## evidence, not whether it errored.
.oe_run <- function(use_llm, extra_env = character(), supplement = NULL,
                    keep_warnings = FALSE, env = parent.frame()) {
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  testthat::local_mocked_bindings(
    .enrich_structured = function(...) {
      calls$n <- calls$n + 1L
      NULL
    },
    .package = "arsbridge", .env = env)

  inp <- .oe_inputs()
  out <- withr::local_tempdir(.local_envir = env)
  path <- file.path(out, "ars.json")
  ## keep_warnings lets a caller assert on the missing-key warning, which is
  ## part of the contract and would otherwise be swallowed here.
  hush <- if (keep_warnings) identity else suppressWarnings
  withr::with_envvar(
    .oe_blank_env(extra_env),
    suppressMessages(hush(spec_to_ars(
      shell_path     = inp$shell,
      adam_spec_path = inp$spec,
      output_path    = path,
      study_id       = "OE-1",
      supplement     = supplement,
      use_llm        = use_llm))))
  spec <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  list(mode = spec[["_meta"]][["extraction_mode"]], llm_calls = calls$n,
       ars_path = path)
}

.OE_KEY <- "sk-ant-fake-key-for-tests-0001"

## A supplement carrying one field the deterministic parser does NOT produce.
## The bundled shell asks for no Total column on Table 14.1.1, so a run that
## reports includeTotal = TRUE for its analyses can only have got it from the
## supplement. The baseline is asserted below so this stays non-vacuous.
.OE_MARKED_TLF <- "AN_T_14_1_1"

.oe_marked_supplement <- function(env = parent.frame()) {
  inp <- .oe_inputs()
  draft <- withr::local_tempfile(fileext = ".json", .local_envir = env)
  suppressMessages(suppressWarnings(write_supplement_draft(
    shell_path = inp$shell, adam_spec_path = inp$spec, output_path = draft)))
  d <- jsonlite::fromJSON(draft, simplifyVector = FALSE)
  d$tlfs[["T-14-1-1"]]$includeTotal <- TRUE
  path <- withr::local_tempfile(fileext = ".json", .local_envir = env)
  jsonlite::write_json(d, path, auto_unbox = TRUE, null = "null")
  path
}

## Did the supplement's includeTotal land on the marked table's analyses?
.oe_marker_landed <- function(ars_path) {
  spec <- jsonlite::fromJSON(ars_path, simplifyVector = FALSE)
  hit <- Filter(function(a) startsWith(as.character(a$id), .OE_MARKED_TLF),
                spec$analyses)
  length(hit) > 0 &&
    all(vapply(hit, function(a)
      isTRUE(as.logical(unlist(a$includeTotal)[1])), logical(1)))
}

test_that("use_llm = FALSE never calls an LLM, key or no key", {
  ## The override that matters most: a configured key must not drag ellmer
  ## into a run that was never going to call it.
  bare <- .oe_run(FALSE)
  expect_equal(bare$mode, "deterministic")
  expect_equal(bare$llm_calls, 0L)

  keyed <- .oe_run(FALSE, c(ANTHROPIC_API_KEY = .OE_KEY))
  expect_equal(keyed$mode, "deterministic")
  expect_equal(keyed$llm_calls, 0L)

  both <- .oe_run(FALSE, c(ANTHROPIC_API_KEY = .OE_KEY,
                           ARS_LLM_PROVIDER  = "anthropic"))
  expect_equal(both$mode, "deterministic")
  expect_equal(both$llm_calls, 0L)
})

test_that("use_llm = TRUE with no usable key falls back without calling", {
  ## Unchanged contract: opting in is not the same as being able to.
  res <- .oe_run(TRUE)
  expect_equal(res$mode, "deterministic")
  expect_equal(res$llm_calls, 0L)
})

test_that("a preferred provider with no key still warns, then falls back", {
  ## The existing missing-key warning is preserved and the run continues
  ## deterministically -- so a missing ellmer is beside the point here: the
  ## provider is unusable whether or not it is installed.
  expect_warning(
    res <- .oe_run(TRUE, c(ARS_LLM_PROVIDER = "anthropic"),
                   keep_warnings = TRUE),
    "API key is not set")
  expect_equal(res$mode, "deterministic")
  expect_equal(res$llm_calls, 0L)
})

test_that("a supplement enriches the ARS without calling an LLM", {
  ## The production path: deterministic parse, then a supplement produced
  ## outside this process. Asserting "zero LLM calls" alone would pass for a
  ## run that ignored the supplement entirely, so the marked field has to
  ## actually arrive.
  supp <- .oe_marked_supplement()
  res <- .oe_run(FALSE, supplement = supp)

  expect_equal(res$mode, "supplement")
  expect_equal(res$llm_calls, 0L)
  expect_true(.oe_marker_landed(res$ars_path))
})

test_that("the supplement marker is not something the parser produces", {
  ## What makes the assertion above non-vacuous: the same shell, read
  ## deterministically, does NOT set includeTotal on that table.
  det <- .oe_run(FALSE)
  expect_equal(det$mode, "deterministic")
  expect_false(.oe_marker_landed(det$ars_path))
})

test_that("a supplement beats the live LLM, key and opt-in notwithstanding", {
  ## Precedence, asserted rather than assumed: `supplement` is resolved before
  ## `use_llm` is even consulted, so an opted-in run with a usable key still
  ## makes no live call -- and would not need ellmer to do it.
  supp <- .oe_marked_supplement()
  res <- .oe_run(TRUE, c(ANTHROPIC_API_KEY = .OE_KEY,
                         ARS_LLM_PROVIDER  = "anthropic"),
                 supplement = supp)

  expect_equal(res$mode, "supplement")
  expect_equal(res$llm_calls, 0L)
  expect_true(.oe_marker_landed(res$ars_path))
})

test_that("use_llm does not change what a supplement produces", {
  ## The same supplement, with and without the opt-in, must give the same
  ## answer -- opting in is not a way to alter supplement handling.
  supp <- .oe_marked_supplement()
  off <- .oe_run(FALSE, supplement = supp)
  on  <- .oe_run(TRUE, c(ANTHROPIC_API_KEY = .OE_KEY), supplement = supp)

  expect_equal(off$mode, on$mode)
  expect_equal(off$llm_calls, 0L)
  expect_equal(on$llm_calls, 0L)

  strip <- function(p) {
    s <- jsonlite::fromJSON(p, simplifyVector = FALSE)
    s[["_meta"]] <- NULL   # carries the study id and timestamps
    s$id <- NULL
    s
  }
  expect_equal(strip(off$ars_path), strip(on$ars_path))
})

test_that("use_llm = TRUE with a usable key DOES call the LLM", {
  ## The positive control. Without this the tests above would pass for a
  ## pipeline that had simply stopped calling LLMs at all.
  ##
  ## This is the one case in the file that needs ellmer for real: it is the
  ## only one that reaches the live branch, where the guard fires and the
  ## request schema is built from ellmer's type constructors.
  skip_if_not_installed("ellmer")
  res <- .oe_run(TRUE, c(ANTHROPIC_API_KEY = .OE_KEY,
                         ARS_LLM_PROVIDER  = "anthropic"))
  expect_equal(res$mode, "llm")
  expect_gt(res$llm_calls, 0L)
})

test_that("ellmer is optional, not a hard dependency", {
  dcf <- read.dcf(system.file("DESCRIPTION", package = "arsbridge"),
                  fields = c("Imports", "Suggests"))
  expect_false(grepl("\\bellmer\\b", dcf[1, "Imports"]))
  expect_true(grepl("\\bellmer\\b", dcf[1, "Suggests"]))
})
