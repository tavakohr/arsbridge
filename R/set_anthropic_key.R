## arsbridge -- set_anthropic_key.R
## ---------------------------------------------------------------------------
## Two small exported helpers that make it obvious where the Anthropic API
## key lives. `set_anthropic_key()` walks the user through writing it into
## their .Renviron (the conventional R-startup file) AND sets it in the
## current session so spec_to_ars() works immediately. `check_anthropic_key()`
## reports whether the key is currently visible to R without exposing it.

#' Set (or update) your Anthropic API key for arsbridge
#'
#' Writes `ANTHROPIC_API_KEY=...` to your user `.Renviron` file so it loads
#' automatically every time you start R, AND sets it in the current session
#' so you can call [spec_to_ars()] immediately -- no restart required.
#'
#' Get a key at <https://console.anthropic.com/settings/keys>.
#'
#' @param key Character. Your Anthropic API key (starts with `"sk-ant-"`).
#'   If `NULL` (default) and R is running interactively, prompts you to
#'   paste the key.
#' @param scope `"user"` (default) writes to your home `.Renviron`
#'   (recommended -- one key shared across all your R projects).
#'   `"project"` writes to `.Renviron` in the current working directory
#'   (useful when collaborating on a shared project where each contributor
#'   has their own key).
#'
#' @return Invisibly returns the path to the `.Renviron` file that was
#'   updated.
#'
#' @examples
#' \dontrun{
#' # Interactive prompt (recommended -- key is not echoed to the screen
#' # when the 'askpass' package is installed)
#' set_anthropic_key()
#'
#' # Or paste it directly
#' set_anthropic_key("sk-ant-api03-...")
#'
#' # Project-scoped key (writes to ./.Renviron, not your home one)
#' set_anthropic_key("sk-ant-api03-...", scope = "project")
#' }
#' @export
set_anthropic_key <- function(key = NULL, scope = c("user", "project")) {
  scope <- match.arg(scope)

  if (is.null(key)) {
    if (!interactive()) {
      cli::cli_abort(c(
        "Cannot prompt for the key in a non-interactive R session.",
        "i" = "Pass it directly: {.code set_anthropic_key('sk-ant-...')}"
      ))
    }
    key <- .prompt_for_key()
  }

  key <- trimws(as.character(key))
  if (!nzchar(key)) {
    cli::cli_abort(c(
      "Empty key.",
      "i" = "Get one at {.url https://console.anthropic.com/settings/keys}."
    ))
  }
  if (!startsWith(key, "sk-ant-")) {
    cli::cli_warn(c(
      "Key does not start with {.val sk-ant-}.",
      "i" = "Continuing anyway -- double-check this is an Anthropic key."
    ))
  }

  renv_path <- switch(scope,
    user    = path.expand("~/.Renviron"),
    project = file.path(getwd(), ".Renviron")
  )

  .write_key_to_renviron(key, renv_path)
  Sys.setenv(ANTHROPIC_API_KEY = key)

  cli::cli_alert_success("Saved key to {.path {renv_path}}.")
  cli::cli_alert_info("Also set in the current R session -- no restart needed.")
  cli::cli_inform(c(
    " " = "Next time you start R, the key will load automatically from this file."
  ))

  invisible(renv_path)
}


#' Check whether the Anthropic API key is set
#'
#' Reports whether `ANTHROPIC_API_KEY` is visible to the current R session,
#' without printing the key itself. Useful as a quick "am I set up?" check
#' before calling [spec_to_ars()].
#'
#' @return Invisibly returns `TRUE` if the key is set, `FALSE` otherwise.
#'   Prints a status message either way.
#'
#' @examples
#' \dontrun{
#' check_anthropic_key()
#' }
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
  cli::cli_inform(c(
    "i" = "Easiest fix: run {.code arsbridge::set_anthropic_key()} for an interactive prompt.",
    " " = "Or paste it once: {.code arsbridge::set_anthropic_key('sk-ant-...')}",
    " " = "Get a key at {.url https://console.anthropic.com/settings/keys}."
  ))
  invisible(FALSE)
}


## --- Internal helpers ------------------------------------------------------

.prompt_for_key <- function() {
  if (requireNamespace("askpass", quietly = TRUE)) {
    return(askpass::askpass("Paste your Anthropic API key (sk-ant-...): "))
  }
  cli::cli_alert_warning(c(
    "Your key will be visible on screen as you paste it.",
    "i" = "Install {.pkg askpass} ({.code install.packages('askpass')}) for hidden entry next time."
  ))
  readline("Paste your Anthropic API key (sk-ant-...): ")
}

.write_key_to_renviron <- function(key, path) {
  existing <- if (file.exists(path)) {
    readLines(path, warn = FALSE, encoding = "UTF-8")
  } else {
    character()
  }
  ## Strip any prior ANTHROPIC_API_KEY line so we don't end up with two.
  existing <- existing[!grepl("^\\s*ANTHROPIC_API_KEY\\s*=", existing)]
  new_lines <- c(existing, paste0("ANTHROPIC_API_KEY=", key))
  writeLines(new_lines, path, useBytes = TRUE)
  invisible(path)
}
