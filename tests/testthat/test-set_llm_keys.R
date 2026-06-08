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
  expect_equal(active$model, "claude-3-5-sonnet-latest")

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
  expect_equal(active$model, "gemini-1.5-pro")

  # Check show_active_llm works without error
  show_active_llm()
})
