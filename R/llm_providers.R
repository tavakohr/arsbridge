## arsbridge -- llm_providers.R
## ---------------------------------------------------------------------------
## Single source of truth for every supported LLM provider. Adding a new
## provider (GLM, DeepSeek, OpenRouter, a future model, ...) is ONE entry in
## `.LLM_PROVIDERS` -- no `switch()` edits anywhere else.
##
## Each entry:
##   label    Human-readable provider name (for messages / key prompts).
##   env      Environment variable holding the API key.
##   model    Default model id when the caller passes none.
##   prefix   Expected API-key prefix (""/NA = no check). Used only to warn.
##   chat     Name of the `ellmer::chat_*()` constructor to use.
##   base_url Optional OpenAI-compatible endpoint. When set, `chat` is
##            `chat_openai` and the api key is passed explicitly (the
##            endpoint reads it as a bearer token, NOT from OPENAI_API_KEY).
##   url      Where the user obtains a key (shown in setter messages).
##
## Provider priority (when several keys are set and ARS_LLM_PROVIDER is unset)
## follows the order of this list, top to bottom.
## ---------------------------------------------------------------------------

.LLM_PROVIDERS <- list(
  anthropic = list(
    label   = "Anthropic",
    env     = "ANTHROPIC_API_KEY",
    model   = "claude-sonnet-4-6",
    prefix  = "sk-ant-",
    chat    = "chat_anthropic",
    base_url = NULL,
    url     = "https://console.anthropic.com/settings/keys"
  ),
  openai = list(
    label   = "OpenAI",
    env     = "OPENAI_API_KEY",
    model   = "gpt-4o",
    prefix  = "sk-",
    chat    = "chat_openai",
    base_url = NULL,
    url     = "https://platform.openai.com/api-keys"
  ),
  gemini = list(
    label   = "Gemini",
    env     = "GEMINI_API_KEY",
    model   = "gemini-2.5-pro",
    prefix  = "",
    chat    = "chat_google_gemini",
    base_url = NULL,
    url     = "https://aistudio.google.com/app/apikey"
  ),
  glm = list(
    ## Zhipu / z.ai GLM. OpenAI-compatible endpoint, so it reuses chat_openai
    ## with a base_url. Set the exact model id you want via the `model` arg of
    ## spec_to_ars() (e.g. "glm-4-plus", "glm-4.6", or a newer GLM release);
    ## the default below is a safe fallback, not a hard pin.
    label   = "GLM (Zhipu)",
    env     = "GLM_API_KEY",
    model   = "glm-4-plus",
    prefix  = "",
    chat    = "chat_openai",
    base_url = "https://open.bigmodel.cn/api/paas/v4",
    url     = "https://open.bigmodel.cn/"
  )
)

#' Names of all supported providers, in priority order.
#' @noRd
.llm_provider_names <- function() names(.LLM_PROVIDERS)

#' Return one provider's config, or abort with the list of supported names.
#' @noRd
.llm_provider <- function(provider) {
  p <- .LLM_PROVIDERS[[provider %||% ""]]
  if (is.null(p)) {
    supported <- .llm_provider_names()
    cli::cli_abort(c(
      "Unsupported LLM provider: {.val {provider}}.",
      "i" = "Supported: {.val {supported}}."
    ))
  }
  p
}

#' The API-key environment variable for a provider.
#' @noRd
.llm_env_var <- function(provider) .llm_provider(provider)$env

#' The default model id for a provider.
#' @noRd
.llm_default_model <- function(provider) .llm_provider(provider)$model

#' Build an ellmer chat object for `provider`.
#'
#' Resolves the constructor from `ellmer` by name so a new provider is just a
#' registry entry. For OpenAI-compatible providers (non-NULL `base_url`) the
#' endpoint and api key are passed explicitly, because the default
#' `chat_openai()` would otherwise read `OPENAI_API_KEY`.
#' @noRd
.llm_build_chat <- function(provider, model, system_prompt, max_tokens,
                            api_key = NULL) {
  p <- .llm_provider(provider)
  chat_fn <- getExportedValue("ellmer", p$chat)
  args <- list(
    system_prompt = system_prompt,
    model         = model,
    params        = ellmer::params(max_tokens = max_tokens)
  )
  if (!is.null(p$base_url)) {
    args$base_url <- p$base_url
    if (nzchar(api_key %||% "")) args$api_key <- api_key
  }
  do.call(chat_fn, args)
}
