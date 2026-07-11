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

test_that("spec_to_ars emits per-TLF {cards} deliverables (offline heuristic)", {
  skip_if_not_installed("cards")
  ## A bogus key makes .enrich_structured() return NULL, so the heuristic
  ## fallback runs the whole pipeline with no real LLM call.
  td       <- withr::local_tempdir()
  json_out <- file.path(td, "re.json")
  res <- spec_to_ars(
    shell_path     = test_path("fixtures/annotated_shell_2tlf_minimal.docx"),
    adam_spec_path = test_path("fixtures/adam_spec_minimal.xlsx"),
    output_path    = json_out,
    report_path    = file.path(td, "rep.xlsx"),
    api_key        = "sk-ant-offline", provider = "anthropic",
    model          = "claude-haiku-4-5",
    verbose        = FALSE
  )

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
    verbose        = FALSE
  )
  expect_true(file.exists(json_out))
  expect_gte(res$n_tlfs, 1)
})
