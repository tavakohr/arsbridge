## Provider registry: adding a provider is one entry, no switch() edits.

test_that("registry exposes the four built-in providers in priority order", {
  expect_identical(.llm_provider_names(),
                   c("anthropic", "openai", "gemini", "glm"))
})

test_that(".llm_env_var / .llm_default_model resolve from the registry", {
  expect_equal(.llm_env_var("anthropic"), "ANTHROPIC_API_KEY")
  expect_equal(.llm_env_var("glm"), "GLM_API_KEY")
  expect_equal(.llm_default_model("anthropic"), "claude-sonnet-4-6")
  expect_equal(.llm_default_model("gemini"), "gemini-2.5-pro")
})

test_that("unknown provider aborts with the supported list", {
  expect_error(.llm_provider("doesnotexist"), "Unsupported LLM provider")
  expect_error(.llm_env_var("doesnotexist"), "Unsupported LLM provider")
})

test_that("get_active_llm honours registry priority and ARS_LLM_PROVIDER", {
  withr::local_envvar(
    ANTHROPIC_API_KEY = "sk-ant-aaaaaaaaaaaa",
    OPENAI_API_KEY    = "sk-oooooooooooo",
    GEMINI_API_KEY    = "",
    GLM_API_KEY       = "glm-kkkkkkkkkkkk",
    ARS_LLM_PROVIDER  = ""
  )
  withr::local_options(ars.llm.provider = NULL)
  ## Anthropic first in priority order.
  expect_equal(get_active_llm()$provider, "anthropic")

  ## Explicit preference selects a later (registry) provider when its key set.
  withr::local_envvar(ARS_LLM_PROVIDER = "glm")
  active <- get_active_llm()
  expect_equal(active$provider, "glm")
  expect_equal(active$model, "glm-4-plus")
})

test_that("a base_url provider passes endpoint + key to chat_openai", {
  skip_if_not_installed("ellmer")
  captured <- NULL
  testthat::local_mocked_bindings(
    chat_openai = function(...) { captured <<- list(...); structure(list(), class = "Chat") },
    .package = "ellmer"
  )
  .llm_build_chat("glm", model = "glm-4-plus",
                  system_prompt = "x", max_tokens = 8192L,
                  api_key = "glm-secret")
  expect_equal(captured$base_url, "https://open.bigmodel.cn/api/paas/v4")
  expect_equal(captured$api_key, "glm-secret")
  expect_equal(captured$model, "glm-4-plus")
})
