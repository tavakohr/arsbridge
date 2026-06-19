## LLM-primary extraction: spec gate, cross-check, degraded mode.

.mk_section <- function() {
  list(
    tlf_number = "T-14-1-1", tlf_type = "TABLE", title = "Demographics",
    stub_rows = list(
      list(label = "Age (years)", annotation = "", has_annot = FALSE,
           detection_method = NA_character_, detection_confidence = NA_character_,
           raw_text = "Age (years) <ADSL.AGE>"),
      list(label = "Sex", annotation = "", has_annot = FALSE,
           detection_method = NA_character_, detection_confidence = NA_character_,
           raw_text = "Sex <ADSL.SEX>")
    )
  )
}

.spec <- list(ADSL.AGE = list(), ADSL.SEX = list())

with_key <- function(code) {
  withr::with_envvar(
    c(ANTHROPIC_API_KEY = "sk-ant-test", ARS_LLM_PROVIDER = "anthropic"),
    code
  )
}

test_that("in-spec proposals update rows and mark method = llm", {
  diag_reset()
  call_fn <- function(...) list(rows = list(
    list(row_index = 1L, display_label = "Age (years)",
         dataset = "ADSL", variable = "AGE", confidence = "high"),
    list(row_index = 2L, display_label = "Sex",
         dataset = "ADSL", variable = "SEX", confidence = "high")
  ))
  out <- with_key(extract_shell_llm(.mk_section(), spec_lookup = .spec,
                                    call_fn = call_fn))
  expect_equal(out$stub_rows[[1]]$annotation, "ADSL.AGE")
  expect_true(out$stub_rows[[1]]$has_annot)
  expect_equal(out$stub_rows[[1]]$detection_method, "llm")
  expect_equal(out$stub_rows[[2]]$annotation, "ADSL.SEX")
})

test_that("where_clause is appended to the annotation", {
  diag_reset()
  call_fn <- function(...) list(rows = list(
    list(row_index = 1L, display_label = "Age", dataset = "ADSL",
         variable = "AGE", where_clause = "SAFFL='Y'")
  ))
  out <- with_key(extract_shell_llm(.mk_section(), spec_lookup = .spec,
                                    call_fn = call_fn))
  expect_equal(out$stub_rows[[1]]$annotation, "ADSL.AGE WHERE SAFFL='Y'")
})

test_that("HARD GATE rejects an out-of-spec variable and logs a blocker", {
  diag_reset()
  call_fn <- function(...) list(rows = list(
    list(row_index = 1L, display_label = "Age", dataset = "ADSL",
         variable = "AAGE")   ## hallucinated -- not in spec
  ))
  out <- with_key(extract_shell_llm(.mk_section(), spec_lookup = .spec,
                                    call_fn = call_fn))
  ## Row 1 deterministic result untouched (still no annotation).
  expect_false(out$stub_rows[[1]]$has_annot)
  expect_identical(out$stub_rows[[1]]$annotation, "")
  ## A FAIL blocker names the rejected variable.
  recs <- diag_records()
  rej <- recs[recs$stage == "extract_llm" & recs$severity == "FAIL", ]
  expect_true(nrow(rej) >= 1)
  expect_true(any(grepl("ADSL.AAGE", rej$problem)))
})

test_that("cross-check warns when LLM and regex disagree", {
  diag_reset()
  sec <- .mk_section()
  sec$stub_rows[[1]]$annotation <- "ADSL.AGE"   ## regex said AGE
  sec$stub_rows[[1]]$has_annot  <- TRUE
  call_fn <- function(...) list(rows = list(
    list(row_index = 1L, display_label = "Age", dataset = "ADSL",
         variable = "SEX")     ## LLM says SEX -> disagreement
  ))
  out <- with_key(extract_shell_llm(sec, spec_lookup = .spec, call_fn = call_fn))
  recs <- diag_records()
  expect_true(any(recs$stage == "extract_llm" & recs$severity == "WARN" &
                    grepl("disagree|matched", recs$action %||% recs$problem,
                          ignore.case = TRUE) |
                    grepl("ADSL.SEX", recs$problem)))
})

test_that("DEGRADED mode: no key keeps regex result and warns, no LLM call", {
  diag_reset()
  called <- FALSE
  call_fn <- function(...) { called <<- TRUE; list(rows = list()) }
  out <- withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    extract_shell_llm(.mk_section(), spec_lookup = .spec, call_fn = call_fn)
  )
  expect_false(called)                          ## never hit the model
  expect_false(out$stub_rows[[1]]$has_annot)    ## deterministic result kept
  recs <- diag_records()
  expect_true(any(recs$stage == "extract_llm" & recs$severity == "WARN"))
})

test_that("empty proposal bumps the llm-fail counter", {
  diag_reset()
  call_fn <- function(...) NULL
  with_key(extract_shell_llm(.mk_section(), spec_lookup = .spec, call_fn = call_fn))
  expect_gt(.diag_llm_fail_count(), 0)
})
