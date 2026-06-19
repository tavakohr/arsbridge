## LLM hardening (P3): ellmer schema, retry/backoff, glue-delimiter fix,
## OTHER analysis_type routing.

## --- .enrich_type schema ----------------------------------------------------

test_that(".enrich_type builds an ellmer type without error", {
  skip_if_not_installed("ellmer")
  expect_no_error(ty <- .enrich_type())
  expect_false(is.null(ty))
})

## --- .is_retryable ----------------------------------------------------------

test_that(".is_retryable is TRUE for transient errors", {
  for (m in c("HTTP 429 rate limit", "overloaded_error", "HTTP 503",
              "HTTP 500 Internal", "Timeout was reached",
              "connection reset", "service temporarily unavailable")) {
    expect_true(.is_retryable(simpleError(m)), info = m)
  }
})

test_that(".is_retryable is FALSE for auth / client errors", {
  for (m in c("HTTP 401 Unauthorized", "HTTP 400 Bad Request",
              "invalid x-api-key", "HTTP 404 Not Found")) {
    expect_false(.is_retryable(simpleError(m)), info = m)
  }
})

## --- .with_retry ------------------------------------------------------------

test_that(".with_retry returns immediately on first success (no sleep)", {
  slept <- 0
  res <- .with_retry(function() "ok", max_tries = 3L,
                     sleep = function(s) slept <<- slept + 1L)
  expect_equal(res, "ok")
  expect_equal(slept, 0L)
})

test_that(".with_retry retries transient errors then succeeds with backoff", {
  attempts <- 0L; delays <- numeric()
  fn <- function() {
    attempts <<- attempts + 1L
    if (attempts < 3L) stop("HTTP 429 rate limit")
    "ok"
  }
  res <- .with_retry(fn, max_tries = 3L, base_delay = 1,
                     sleep = function(s) delays <<- c(delays, s))
  expect_equal(res, "ok")
  expect_equal(attempts, 3L)
  ## exponential: 1, 2
  expect_equal(delays, c(1, 2))
})

test_that(".with_retry fails fast on non-retryable error (no retries)", {
  attempts <- 0L; slept <- 0L
  fn <- function() { attempts <<- attempts + 1L; stop("HTTP 401 Unauthorized") }
  expect_error(
    .with_retry(fn, max_tries = 4L, sleep = function(s) slept <<- slept + 1L),
    "401"
  )
  expect_equal(attempts, 1L)
  expect_equal(slept, 0L)
})

test_that(".with_retry gives up after max_tries on persistent transient", {
  attempts <- 0L
  fn <- function() { attempts <<- attempts + 1L; stop("HTTP 503 unavailable") }
  expect_error(.with_retry(fn, max_tries = 2L, sleep = function(s) NULL), "503")
  expect_equal(attempts, 2L)
})

## --- glue delimiter fix (regression) ----------------------------------------

test_that(".render_enrich_prompt tolerates literal braces in annotations", {
  ## Before the <<>> delimiter switch, a literal { in the payload made glue
  ## try to evaluate the brace content and the render errored.
  payload <- list(
    tlf_number = "T-1",
    annotated_rows = list(list(
      label = "x",
      annotation = "ADSL.AVAL where GRP='Cohort {A}' or Y='{z}'"
    ))
  )
  expect_no_error(out <- .render_enrich_prompt(payload))
  expect_true(grepl("Cohort {A}", out, fixed = TRUE))
  expect_true(grepl("{z}", out, fixed = TRUE))
})

test_that(".render_enrich_prompt still injects the payload", {
  out <- .render_enrich_prompt(list(tlf_number = "T-42-1-1"))
  expect_true(grepl("T-42-1-1", out, fixed = TRUE))
})

## --- OTHER analysis_type routing --------------------------------------------

test_that("OTHER analysis_type flags needs_review and infers a concrete type", {
  testthat::local_mocked_bindings(
    .enrich_structured = function(...) list(
      analysis_type   = "OTHER",
      ars_method_name = NULL,
      by_variable     = "",
      row_enrichments = list(list(label = "Cmax", primary_dataset = "ADPP",
                                  primary_variable = "AVAL",
                                  variable_role = "ANALYSIS"))
    )
  )
  diag_reset()
  sec <- list(
    tlf_number = "T-16-2-1", tlf_type = "TABLE",
    title = "Pharmacokinetic Parameter Summary",
    population_text = "PK", population_annot = "",
    col_headers = character(),
    stub_rows = list(list(label = "Cmax", annotation = "ADPP.AVAL",
                          has_annot = TRUE))
  )
  out <- enrich_with_llm(sec, spec_lookup = list(ADPP.AVAL = list()),
                         provider = "anthropic", model = "m", api_key = "k")
  expect_true(isTRUE(out$needs_review))
  expect_false(identical(out$analysis_type, "OTHER"))
  recs <- diag_records()
  expect_true(any(recs$stage == "enrich_llm" & grepl("OTHER", recs$problem)))
})

test_that("structured-call failure (NULL) falls back to heuristics + FAIL diag", {
  testthat::local_mocked_bindings(.enrich_structured = function(...) NULL)
  diag_reset()
  sec <- list(
    tlf_number = "T-14-3-1", tlf_type = "TABLE",
    title = "Summary of Treatment-Emergent Adverse Events",
    population_text = "Safety", population_annot = "ADSL.SAFFL='Y'",
    col_headers = character(),
    stub_rows = list(list(label = "Any TEAE", annotation = "ADAE.AEDECOD",
                          has_annot = TRUE))
  )
  out <- enrich_with_llm(sec, spec_lookup = list(ADSL.TRT01A = list(),
                                                 ADAE.AEDECOD = list()),
                         provider = "anthropic", model = "m", api_key = "k")
  ## Keyword heuristic should still classify this AE table.
  expect_equal(out$analysis_type, "AE_FREQUENCY")
  ## A wholesale LLM failure is now counted (spec_to_ars raises one summary
  ## finding for it) rather than logged once per TLF.
  expect_gt(.diag_llm_fail_count(), 0)
})
