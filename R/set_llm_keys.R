## arsbridge -- set_llm_keys.R
## ---------------------------------------------------------------------------
## Helpers that make it obvious where API keys live.
## Provides functions to set Anthropic, OpenAI, and Gemini API keys and inspect
## which provider is currently active.

#' Get the active LLM provider and configurations
#'
#' Inspects the environment variables and system options to determine the
#' active LLM provider (Anthropic, OpenAI, or Gemini) and their corresponding keys.
#'
#' If multiple API keys are set, it prioritizes the provider specified in the
#' `ARS_LLM_PROVIDER` environment variable or global option. If that is not set,
#' it defaults to the first available key in the order of: Anthropic, OpenAI, Gemini.
#'
#' @return A list containing:
#'   \describe{
#'     \item{`provider`}{The name of the active provider (`"anthropic"`, `"openai"`, or `"gemini"`), or `NULL` if none are active.}
#'     \item{`model`}{The default model name for the active provider.}
#'     \item{`keys_set`}{A character vector listing the providers that currently have keys set.}
#'     \item{`active_key_masked`}{A masked version of the active API key (showing only first 7 and last 3 characters), or `NULL`.}
#'   }
#' @export
#' @examples
#' get_active_llm()
get_active_llm <- function() {
  keys <- list(
    anthropic = Sys.getenv("ANTHROPIC_API_KEY", unset = ""),
    openai    = Sys.getenv("OPENAI_API_KEY", unset = ""),
    gemini    = Sys.getenv("GEMINI_API_KEY", unset = "")
  )

  keys_set <- names(keys)[sapply(keys, nzchar)]

  # Check for explicit user preference
  pref <- Sys.getenv("ARS_LLM_PROVIDER", unset = getOption("ars.llm.provider", ""))
  pref <- tolower(trimws(pref))

  provider <- NULL
  if (nzchar(pref) && pref %in% c("anthropic", "openai", "gemini")) {
    if (pref %in% keys_set) {
      provider <- pref
    } else {
      cli::cli_warn("Preferred LLM provider {.val {pref}} was specified, but its API key is not set.")
    }
  }

  if (is.null(provider)) {
    if ("anthropic" %in% keys_set) {
      provider <- "anthropic"
    } else if ("openai" %in% keys_set) {
      provider <- "openai"
    } else if ("gemini" %in% keys_set) {
      provider <- "gemini"
    }
  }

  model <- NULL
  active_key_masked <- NULL
  if (!is.null(provider)) {
    model <- switch(provider,
      anthropic = "claude-3-5-sonnet-latest",
      openai    = "gpt-4o",
      gemini    = "gemini-1.5-pro"
    )
    raw_key <- keys[[provider]]
    n <- nchar(raw_key)
    if (n > 10) {
      active_key_masked <- paste0(
        substr(raw_key, 1, 7),
        strrep("*", max(0, n - 10)),
        substr(raw_key, max(1, n - 2), n)
      )
    } else {
      active_key_masked <- strrep("*", n)
    }
  }

  list(
    provider = provider,
    model = model,
    keys_set = keys_set,
    active_key_masked = active_key_masked
  )
}

#' Show the active LLM provider and API key status
#'
#' Prints a summary of the active LLM provider, the model that will be used,
#' and the set/missing status of API keys for Anthropic, OpenAI, and Gemini.
#'
#' @return Invisibly returns the active provider name (character), or `NULL`.
#' @export
#' @examples
#' show_active_llm()
show_active_llm <- function() {
  active <- get_active_llm()
  
  cli::cli_h2("LLM Configuration Status")
  
  keys <- list(
    anthropic = Sys.getenv("ANTHROPIC_API_KEY", unset = ""),
    openai    = Sys.getenv("OPENAI_API_KEY", unset = ""),
    gemini    = Sys.getenv("GEMINI_API_KEY", unset = "")
  )
  
  for (prov in names(keys)) {
    status <- if (nzchar(keys[[prov]])) {
      k <- keys[[prov]]
      n <- nchar(k)
      masked <- if (n > 10) {
        paste0(substr(k, 1, 7), strrep("*", max(0, n - 10)), substr(k, max(1, n - 2), n))
      } else {
        strrep("*", n)
      }
      paste0("{.val ", masked, "} (", n, " chars)")
    } else {
      "{.danger NOT SET}"
    }
    
    active_marker <- if (identical(active$provider, prov)) {
      " {.strong [ACTIVE]}"
    } else {
      ""
    }
    
    cli::cli_alert_info("{.strong {toupper(prov)}}: {status}{active_marker}")
  }
  
  if (!is.null(active$provider)) {
    cli::cli_alert_success("Active LLM Provider: {.val {toupper(active$provider)}}")
    cli::cli_alert_success("Default Model: {.val {active$model}}")
  } else {
    cli::cli_alert_danger("No active LLM provider found. Set an API key to get started.")
  }
  
  invisible(active$provider)
}

#' Set your Anthropic API key for arsbridge
#'
#' Writes `ANTHROPIC_API_KEY=...` to your user `.Renviron` file so it loads
#' automatically every time you start R, AND sets it in the current session
#' so you can call [spec_to_ars()] immediately.
#'
#' @param key Character. Your Anthropic API key (starts with `"sk-ant-"`).
#'   If `NULL` (default) and R is running interactively, prompts you.
#' @param scope `"user"` (default) writes to your home `.Renviron`.
#'   `"project"` writes to `.Renviron` in the current working directory.
#'
#' @return Invisibly returns the path to the `.Renviron` file that was updated.
#' @export
set_anthropic_key <- function(key = NULL, scope = c("user", "project")) {
  .set_key_generic(
    provider_name = "Anthropic",
    env_var       = "ANTHROPIC_API_KEY",
    prefix        = "sk-ant-",
    key           = key,
    scope         = scope,
    url           = "https://console.anthropic.com/settings/keys"
  )
}

#' Set your OpenAI API key for arsbridge
#'
#' Writes `OPENAI_API_KEY=...` to your user `.Renviron` file so it loads
#' automatically every time you start R, AND sets it in the current session.
#'
#' @param key Character. Your OpenAI API key (starts with `"sk-"`).
#'   If `NULL` (default) and R is running interactively, prompts you.
#' @param scope `"user"` (default) writes to your home `.Renviron`.
#'   `"project"` writes to `.Renviron` in the current working directory.
#'
#' @return Invisibly returns the path to the `.Renviron` file that was updated.
#' @export
set_openai_key <- function(key = NULL, scope = c("user", "project")) {
  .set_key_generic(
    provider_name = "OpenAI",
    env_var       = "OPENAI_API_KEY",
    prefix        = "sk-",
    key           = key,
    scope         = scope,
    url           = "https://platform.openai.com/api-keys"
  )
}

#' Set your Gemini API key for arsbridge
#'
#' Writes `GEMINI_API_KEY=...` to your user `.Renviron` file so it loads
#' automatically every time you start R, AND sets it in the current session.
#'
#' @param key Character. Your Gemini API key.
#'   If `NULL` (default) and R is running interactively, prompts you.
#' @param scope `"user"` (default) writes to your home `.Renviron`.
#'   `"project"` writes to `.Renviron` in the current working directory.
#'
#' @return Invisibly returns the path to the `.Renviron` file that was updated.
#' @export
set_gemini_key <- function(key = NULL, scope = c("user", "project")) {
  .set_key_generic(
    provider_name = "Gemini",
    env_var       = "GEMINI_API_KEY",
    prefix        = "",
    key           = key,
    scope         = scope,
    url           = "https://aistudio.google.com/app/apikey"
  )
}

#' Check whether the Anthropic API key is set
#'
#' Reports whether `ANTHROPIC_API_KEY` is visible to the current R session,
#' without printing the key itself.
#'
#' @return Invisibly returns `TRUE` if the key is set, `FALSE` otherwise.
#' @export
check_anthropic_key <- function() {
  key <- Sys.getenv("ANTHROPIC_API_KEY", unset = "")
  if (nzchar(key)) {
    n <- nchar(key)
    masked <- paste0(substr(key, 1, 7), strrep("*", max(0, n - 11)),
                     substr(key, n - 3, n))
    cli::cli_alert_success("ANTHROPIC_API_KEY is set ({n} chars): {.val {masked}}")
    return(invisible(TRUE))
  }
  cli::cli_alert_danger("ANTHROPIC_API_KEY is not set.")
  invisible(FALSE)
}


## --- Internal helpers ------------------------------------------------------

.set_key_generic <- function(provider_name, env_var, prefix, key, scope, url) {
  scope <- match.arg(scope)

  if (is.null(key)) {
    if (!interactive()) {
      cli::cli_abort(c(
        "Cannot prompt for the key in a non-interactive R session.",
        "i" = "Pass it directly: {.code set_{tolower(provider_name)}_key('key_here')}"
      ))
    }
    key <- .prompt_for_key_generic(provider_name)
  }

  key <- trimws(as.character(key))
  if (!nzchar(key)) {
    cli::cli_abort(c(
      "Empty key.",
      "i" = "Get one at {.url {url}}."
    ))
  }
  if (nzchar(prefix) && !startsWith(key, prefix)) {
    cli::cli_warn(c(
      "Key does not start with {.val {prefix}}.",
      "i" = "Continuing anyway -- double-check this is a valid {provider_name} key."
    ))
  }

  renv_path <- switch(scope,
    user    = path.expand("~/.Renviron"),
    project = file.path(getwd(), ".Renviron")
  )

  .write_key_to_renviron_generic(env_var, key, renv_path)
  
  args <- list(key)
  names(args) <- env_var
  do.call(Sys.setenv, args)

  cli::cli_alert_success("Saved {provider_name} key to {.path {renv_path}}.")
  cli::cli_alert_info("Also set in the current R session -- no restart needed.")

  invisible(renv_path)
}

.prompt_for_key_generic <- function(provider_name) {
  prompt_str <- paste0("Paste your ", provider_name, " API key: ")
  if (requireNamespace("askpass", quietly = TRUE)) {
    return(askpass::askpass(prompt_str))
  }
  cli::cli_alert_warning(c(
    "Your key will be visible on screen as you paste it.",
    "i" = "Install {.pkg askpass} ({.code install.packages('askpass')}) for hidden entry next time."
  ))
  readline(prompt_str)
}

.write_key_to_renviron_generic <- function(var_name, key, path) {
  existing <- if (file.exists(path)) {
    readLines(path, warn = FALSE, encoding = "UTF-8")
  } else {
    character()
  }
  existing <- existing[!grepl(paste0("^\\s*", var_name, "\\s*="), existing)]
  new_lines <- c(existing, paste0(var_name, "=", key))
  writeLines(new_lines, path, useBytes = TRUE)
  invisible(path)
}
