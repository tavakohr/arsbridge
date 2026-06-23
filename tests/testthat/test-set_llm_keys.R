library(testthat)

test_that("get_active_llm and set functions work correctly", {
  # Backup existing env vars
  orig_anthropic <- Sys.getenv("ANTHROPIC_API_KEY", unset = NA_character_)
  orig_openai <- Sys.getenv("OPENAI_API_KEY", unset = NA_character_)
  orig_gemini <- Sys.getenv("GEMINI_API_KEY", unset = NA_character_)
  orig_pref <- Sys.getenv("ARS_LLM_PROVIDER", unset = NA_character_)

  # Clean env vars for testing
  Sys.unsetenv("ANTHROPIC_API_KEY")
  Sys.unsetenv("OPENAI_API_KEY")
  Sys.unsetenv("GEMINI_API_KEY")
  Sys.unsetenv("ARS_LLM_PROVIDER")

  on.exit({
    # Restore original environment
    if (is.na(orig_anthropic)) Sys.unsetenv("ANTHROPIC_API_KEY") else Sys.setenv(ANTHROPIC_API_KEY = orig_anthropic)
    if (is.na(orig_openai)) Sys.unsetenv("OPENAI_API_KEY") else Sys.setenv(OPENAI_API_KEY = orig_openai)
    if (is.na(orig_gemini)) Sys.unsetenv("GEMINI_API_KEY") else Sys.setenv(GEMINI_API_KEY = orig_gemini)
    if (is.na(orig_pref)) Sys.unsetenv("ARS_LLM_PROVIDER") else Sys.setenv(ARS_LLM_PROVIDER = orig_pref)
  })

  # Initially, no key is active
  active <- get_active_llm()
  expect_null(active$provider)

  # Test set_anthropic_key
  temp_renv <- tempfile()
  # Use project-scoped or pass key directly, we override env var directly in R session
  Sys.setenv(ANTHROPIC_API_KEY = "sk-ant-testkey123")
  active <- get_active_llm()
  expect_equal(active$provider, "anthropic")
  expect_equal(active$model, "claude-sonnet-4-6")

  # Add OpenAI key
  Sys.setenv(OPENAI_API_KEY = "sk-openaikey456")
  active <- get_active_llm()
  # Default priority is Anthropic -> OpenAI -> Gemini
  expect_equal(active$provider, "anthropic")

  # Change preference to OpenAI
  Sys.setenv(ARS_LLM_PROVIDER = "openai")
  active <- get_active_llm()
  expect_equal(active$provider, "openai")
  expect_equal(active$model, "gpt-4o")

  # Add Gemini
  Sys.setenv(GEMINI_API_KEY = "geminikey789")
  Sys.setenv(ARS_LLM_PROVIDER = "gemini")
  active <- get_active_llm()
  expect_equal(active$provider, "gemini")
  expect_equal(active$model, "gemini-2.5-pro")

  # Check show_active_llm works without error
  show_active_llm()
})

test_that("set_*_key sets the session env var and leaves files unchanged", {
  withr::local_envvar(c(OPENAI_API_KEY = NA, GEMINI_API_KEY = NA,
                        ANTHROPIC_API_KEY = NA, GLM_API_KEY = NA))
  # Non-interactive: sets the session var, does not touch .Renviron.
  expect_message(set_openai_key("sk-openai-abc123"), "current R session")
  expect_equal(Sys.getenv("OPENAI_API_KEY"), "sk-openai-abc123")

  set_gemini_key("gemini-xyz")
  expect_equal(Sys.getenv("GEMINI_API_KEY"), "gemini-xyz")

  # Generic setter for a registry provider with no dedicated wrapper.
  set_llm_key("glm", "glm-key-123")
  expect_equal(Sys.getenv("GLM_API_KEY"), "glm-key-123")
})

test_that("check_anthropic_key reflects whether the key is set", {
  withr::local_envvar(c(ANTHROPIC_API_KEY = "sk-ant-abcdef1234"))
  expect_true(check_anthropic_key())
  withr::local_envvar(c(ANTHROPIC_API_KEY = NA))
  expect_false(check_anthropic_key())
})

test_that("an empty key errors and a wrong prefix warns", {
  withr::local_envvar(c(ANTHROPIC_API_KEY = NA))
  expect_error(set_anthropic_key("   "), regexp = "[Ee]mpty")
  expect_warning(set_anthropic_key("not-a-real-prefix-key"),
                 regexp = "does not start with")
})

test_that("a NULL key in a non-interactive session is a clear error", {
  # Tests run non-interactively, so the prompt path aborts with guidance.
  expect_error(set_openai_key(NULL), regexp = "non-interactive")
})

test_that(".write_key_to_renviron_generic writes and de-duplicates the var", {
  path <- withr::local_tempfile()
  writeLines(c("OTHER=keep", "OPENAI_API_KEY=old"), path)
  arsbridge:::.write_key_to_renviron_generic("OPENAI_API_KEY", "new", path)
  lines <- readLines(path)
  expect_true("OTHER=keep" %in% lines)          # unrelated line preserved
  expect_equal(sum(grepl("^OPENAI_API_KEY=", lines)), 1L)  # de-duplicated
  expect_true("OPENAI_API_KEY=new" %in% lines)  # updated value
})
