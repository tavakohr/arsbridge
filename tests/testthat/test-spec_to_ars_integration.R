## Integration tests for the top-level spec_to_ars() orchestrator.
## Live LLM tests are skipped automatically when ANTHROPIC_API_KEY is not set.

test_that("spec_to_ars errors on missing shell file", {
  expect_error(
    spec_to_ars(
      shell_path     = "nonexistent.docx",
      adam_spec_path = test_path("fixtures/adam_spec_minimal.xlsx"),
      api_key        = "sk-test"
    ),
    "not found"
  )
})

test_that("spec_to_ars errors on missing spec file", {
  expect_error(
    spec_to_ars(
      shell_path     = test_path("fixtures/annotated_shell_2tlf_minimal.docx"),
      adam_spec_path = "nonexistent.xlsx",
      api_key        = "sk-test"
    ),
    "not found"
  )
})

test_that("spec_to_ars without any API key runs in deterministic mode", {
  ## A missing key must never stop the run: the pipeline degrades to
  ## regex + keyword heuristics and says so once (see also test-supplement.R).
  res <- withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(spec_to_ars(
      shell_path     = test_path("fixtures/annotated_shell_2tlf_minimal.docx"),
      adam_spec_path = test_path("fixtures/adam_spec_minimal.xlsx"),
      api_key        = "",
      output_path    = tempfile(fileext = ".json"),
      report_path    = tempfile(fileext = ".xlsx"),
      verbose        = FALSE
    ))
  )
  expect_equal(res$extraction_mode, "deterministic")
  expect_true(file.exists(res$ars_path))
  expect_gt(res$n_analyses, 0)
})

test_that("LLM is opt-in: default (use_llm = FALSE) ignores a configured key", {
  ## A (fake) key is present. The LLM is OPT-IN, so the default run must NOT
  ## touch it: deterministic mode, no LLM call -> no 401 outage FAIL and no
  ## key-related warning. This proves a configured key alone no longer selects
  ## the llm tier.
  res <- withr::with_envvar(
    c(ANTHROPIC_API_KEY = "sk-ant-should-not-be-used",
      OPENAI_API_KEY = "", GEMINI_API_KEY = "", GLM_API_KEY = "",
      ARS_LLM_PROVIDER = "anthropic"),
    suppressMessages(spec_to_ars(
      shell_path     = test_path("fixtures/annotated_shell_2tlf_minimal.docx"),
      adam_spec_path = test_path("fixtures/adam_spec_minimal.xlsx"),
      ## no use_llm -> default FALSE
      output_path    = tempfile(fileext = ".json"),
      report_path    = tempfile(fileext = ".xlsx"),
      verbose        = FALSE
    ))
  )
  expect_equal(res$extraction_mode, "deterministic")
  expect_true(file.exists(res$ars_path))
  d <- res$diagnostics
  ## No LLM call was attempted -> no enrichment outage FAIL...
  expect_equal(sum(d$stage == "enrich_llm" & d$severity == "FAIL"), 0)
  ## ...and no key-related error/warning.
  expect_equal(sum(d$severity %in% c("WARN", "FAIL") &
                     grepl("API key|LLM key|No LLM key", d$problem,
                           ignore.case = TRUE)), 0)
})

test_that("spec_to_ars rejects wrong shell extension", {
  bad <- tempfile(fileext = ".txt")
  writeLines("not a docx", bad)
  expect_error(
    spec_to_ars(
      shell_path     = bad,
      adam_spec_path = test_path("fixtures/adam_spec_minimal.xlsx"),
      api_key        = "sk-test"
    ),
    "\\.docx"
  )
})

test_that("spec_to_ars emits per-TLF {cards} deliverables (deterministic)", {
  skip_if_not_installed("cards")
  ## Default run is deterministic (LLM opt-in): the whole pipeline -- including
  ## {cards} emission -- runs on regex + heuristics with no LLM call at all.
  td       <- withr::local_tempdir()
  json_out <- file.path(td, "re.json")
  res <- withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    spec_to_ars(
      shell_path     = test_path("fixtures/annotated_shell_2tlf_minimal.docx"),
      adam_spec_path = test_path("fixtures/adam_spec_minimal.xlsx"),
      output_path    = json_out,
      report_path    = file.path(td, "rep.xlsx"),
      verbose        = FALSE
    )
  )
  expect_equal(res$extraction_mode, "deterministic")

  expect_gte(length(res$code_paths), 1)
  expect_true(all(file.exists(res$code_paths)))
  expect_equal(normalizePath(res$code_dir),
               normalizePath(file.path(dirname(json_out), "code")))
  ## Every emitted deliverable is valid R and free of internal symbols.
  for (p in res$code_paths) {
    txt <- paste(readLines(p), collapse = "\n")
    expect_silent(parse(text = txt))
    expect_false(grepl("arsbridge|MTH_|load_adam", txt))
  }
})

test_that("spec_to_ars end-to-end on minimal synthetic fixture (requires API key)", {
  skip_if(Sys.getenv("ANTHROPIC_API_KEY") == "",
          "ANTHROPIC_API_KEY not set -- skipping live LLM integration test")

  json_out   <- tempfile(fileext = ".json")
  report_out <- tempfile(fileext = ".xlsx")
  res <- spec_to_ars(
    shell_path     = test_path("fixtures/annotated_shell_2tlf_minimal.docx"),
    adam_spec_path = test_path("fixtures/adam_spec_minimal.xlsx"),
    output_path    = json_out,
    report_path    = report_out,
    use_llm        = TRUE,
    verbose        = FALSE
  )
  expect_true(file.exists(json_out))
  expect_true(file.exists(report_out))
  expect_equal(res$n_tlfs, 2)
  expect_gte(res$n_analyses, 3)

  ars <- jsonlite::fromJSON(json_out, simplifyVector = FALSE)
  expect_length(ars$outputs, 2)
})

test_that("spec_to_ars end-to-end on real APX-DRM-301 fixture (requires API key + real fixture)", {
  skip_if(Sys.getenv("ANTHROPIC_API_KEY") == "",
          "ANTHROPIC_API_KEY not set")
  real_shell <- normalizePath(
    file.path(test_path("fixtures"), "..", "..", "..", "inputs",
              "APX-DRM-301_TLF_Shells_v1.0_sample_annotated.docx"),
    mustWork = FALSE
  )
  skip_if(!file.exists(real_shell),
          "Real APX-DRM-301 fixture not present in inputs/")

  json_out   <- tempfile(fileext = ".json")
  report_out <- tempfile(fileext = ".xlsx")
  res <- spec_to_ars(
    shell_path     = real_shell,
    adam_spec_path = test_path("fixtures/adam_spec_minimal.xlsx"),
    output_path    = json_out,
    report_path    = report_out,
    use_llm        = TRUE,
    verbose        = FALSE
  )
  expect_true(file.exists(json_out))
  expect_gte(res$n_tlfs, 1)
})

test_that("spec_to_ars runs deterministically on an RWE-style one-line-heading shell", {
  res <- withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(spec_to_ars(
      shell_path     = test_path("fixtures/annotated_shell_rwe_style.docx"),
      adam_spec_path = test_path("fixtures/adam_spec_rwe.xlsx"),
      output_path    = tempfile(fileext = ".json"),
      report_path    = tempfile(fileext = ".xlsx"),
      verbose        = FALSE
    ))
  )
  expect_equal(res$extraction_mode, "deterministic")
  expect_equal(res$n_tlfs, 2)
  expect_true(file.exists(res$ars_path))
})

test_that("spec_to_ars threads heading_patterns through to the parser", {
  res <- withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(spec_to_ars(
      shell_path     = test_path("fixtures/annotated_shell_custom_heading.docx"),
      adam_spec_path = test_path("fixtures/adam_spec_minimal.xlsx"),
      heading_patterns =
        "^(?i)Output\\s+(?<number>\\d+(?:\\.\\d+)*)\\s*:\\s*(?<title>.*)$",
      output_path    = tempfile(fileext = ".json"),
      report_path    = tempfile(fileext = ".xlsx"),
      verbose        = FALSE
    ))
  )
  expect_equal(res$n_tlfs, 1)
})

test_that("the zero-section abort lists near-candidates, gives heading guidance, and points at heading_patterns", {
  err <- tryCatch(
    suppressWarnings(spec_to_ars(
      shell_path     = test_path("fixtures/annotated_shell_near_miss.docx"),
      adam_spec_path = test_path("fixtures/adam_spec_minimal.xlsx"),
      output_path    = tempfile(fileext = ".json"),
      report_path    = tempfile(fileext = ".xlsx"),
      verbose        = FALSE
    )),
    error = function(e) e
  )
  msg <- conditionMessage(err)
  expect_match(msg, "begins with Table")     # recommended heading guidance
  expect_match(msg, "heading_patterns")      # the escape hatch
})


test_that("reason and purpose arguments are gated on the CDISC vocabularies", {
  ## The vocabularies are closed, and the abort happens before any parsing --
  ## a typo must not cost six minutes of pipeline first.
  expect_error(
    spec_to_ars("shell.docx", "spec.xlsx",
                analysis_reason = "BECAUSE I SAID SO"),
    "controlled terms"
  )
  expect_error(
    spec_to_ars("shell.docx", "spec.xlsx",
                analysis_purpose = "SAFETY"),
    "controlled terms"
  )
})
