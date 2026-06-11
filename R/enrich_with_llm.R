## arsbridge -- enrich_with_llm.R
## ---------------------------------------------------------------------------
## ONE LLM call per TLF section -- never one per row. The annotations are
## already authoritative; the LLM only fills in semantic metadata that the
## shell cannot directly express: analysis_type, method name, per-row role,
## and optional DataSubset conditions.
##
## Uses `ellmer::chat_anthropic()` (the Posit-endorsed Anthropic wrapper).
## Do NOT switch to raw httr2 -- it adds code with no functional gain.

#' Enrich one TLF section with LLM-derived semantic metadata
#'
#' @param section     A single TLF section list (output of [parse_shell_docx()]).
#' @param spec_lookup The `lookup` element of [parse_adam_spec()] (or NULL).
#' @param model       Anthropic model id (default `"claude-sonnet-4-6"`).
#' @param api_key     API key (default reads `ANTHROPIC_API_KEY`).
#'
#' @return The input section with these fields added:
#'   `analysis_type`, `ars_method_name`, `by_variable`,
#'   and an `enriched_rows` list (one per annotated row) with fields
#'   `label`, `primary_dataset`, `primary_variable`, `data_subset`,
#'   `variable_role`.
#'
#' @keywords internal
#' @noRd
enrich_with_llm <- function(section,
                            spec_lookup = NULL,
                            model       = NULL,
                            api_key     = NULL,
                            provider    = NULL) {
  active <- get_active_llm()
  if (is.null(provider)) {
    provider <- active$provider
  }
  if (is.null(provider)) {
    cli::cli_abort(c(
      "No active LLM API key found.",
      "i" = "Please set up an API key for Anthropic, OpenAI, or Gemini."
    ))
  }

  if (is.null(model)) {
    model <- active$model
  }
  if (is.null(api_key)) {
    env_var <- switch(provider,
      anthropic = "ANTHROPIC_API_KEY",
      openai    = "OPENAI_API_KEY",
      gemini    = "GEMINI_API_KEY"
    )
    api_key <- Sys.getenv(env_var)
  }

  if (is.null(api_key) || !nzchar(api_key)) {
    cli::cli_abort(c(
      "API key for {.val {provider}} is not set.",
      "i" = "Please configure your API key."
    ))
  }

  annotated_rows <- Filter(function(r) isTRUE(r$has_annot), section$stub_rows)

  tlf_payload <- list(
    tlf_number            = section$tlf_number,
    tlf_type              = section$tlf_type,
    title                 = section$title,
    population            = section$population_text,
    population_annotation = section$population_annot,
    col_headers           = section$col_headers,
    annotated_rows        = lapply(annotated_rows, function(r) list(
      label      = r$label,
      annotation = r$annotation
    )),
    available_variables   = if (!is.null(spec_lookup)) names(spec_lookup) else character()
  )

  prompt <- .render_enrich_prompt(tlf_payload)
  raw    <- .invoke_llm(prompt, provider = provider, model = model, api_key = api_key)
  parsed <- .parse_llm_json(raw)

  ## Record degraded-enrichment diagnostics: whole-response failures first,
  ## then per-field fallbacks. These feed the validation report so a study
  ## team can see exactly which TLFs ran on heuristics instead of the LLM.
  if (!nzchar(trimws(raw %||% ""))) {
    diag_add(
      stage = "enrich_llm", severity = "FAIL",
      problem = sprintf("LLM call failed or returned empty response (provider %s, model %s)",
                        provider %||% "?", model %||% "default"),
      tlf_number = section$tlf_number,
      location = section$title %||% "",
      action = "All enrichment fields fell back to keyword heuristics -- review this TLF's analysis type, method, and grouping"
    )
  } else if (length(parsed) == 0) {
    diag_add(
      stage = "enrich_llm", severity = "FAIL",
      problem = "LLM response could not be parsed as JSON",
      tlf_number = section$tlf_number,
      location = section$title %||% "",
      action = "All enrichment fields fell back to keyword heuristics -- review this TLF"
    )
  } else {
    if (is.null(parsed$analysis_type)) {
      diag_add(
        stage = "enrich_llm", severity = "WARN",
        problem = "LLM response missing 'analysis_type'",
        tlf_number = section$tlf_number,
        action = "Inferred from title/stub keywords"
      )
    }
    if (is.null(parsed$by_variable)) {
      diag_add(
        stage = "enrich_llm", severity = "WARN",
        problem = "LLM response missing 'by_variable'",
        tlf_number = section$tlf_number,
        action = "Defaulted grouping to TRT01A -- verify against the shell's column headers"
      )
    }
    if (is.null(parsed$row_enrichments)) {
      diag_add(
        stage = "enrich_llm", severity = "WARN",
        problem = "LLM response missing 'row_enrichments'",
        tlf_number = section$tlf_number,
        action = "Per-row metadata regex-derived from annotations only (no roles/subsets)"
      )
    }
  }

  section$analysis_type   <- parsed$analysis_type   %||% .infer_analysis_type(section)
  section$ars_method_name <- parsed$ars_method_name %||% .infer_method_name(section$analysis_type)
  section$by_variable     <- parsed$by_variable     %||% "TRT01A"
  section$enriched_rows   <- parsed$row_enrichments %||% .fallback_enrichments(annotated_rows)

  ## Ground the LLM-chosen grouping variable against the spec: hallucinated
  ## or out-of-spec variables are the main silent-wrong-output risk here.
  if (!is.null(spec_lookup) && length(spec_lookup) > 0 &&
      nzchar(section$by_variable %||% "")) {
    bare <- sub("^[A-Z0-9]+\\.", "", toupper(section$by_variable))
    in_spec <- any(grepl(paste0("\\.", bare, "$"),
                         toupper(names(spec_lookup))))
    if (!in_spec) {
      diag_add(
        stage = "enrich_llm", severity = "WARN",
        problem = sprintf("Grouping variable '%s' not found in the ADaM spec", section$by_variable),
        tlf_number = section$tlf_number,
        action = "Kept as-is -- verify the treatment/grouping variable for this TLF"
      )
    }
  }

  section
}


## --- Internal helpers ------------------------------------------------------

#' Read the prompt template from inst/prompts/ and substitute one placeholder.
#' Uses glue with .open="{" / .close="}"; the template already escapes literal
#' JSON braces as {{ ... }} for glue's benefit.
#' @noRd
.render_enrich_prompt <- function(tlf_payload) {
  path <- system.file("prompts", "enrich_tlf_prompt.txt", package = "arsbridge")
  if (!nzchar(path)) {
    dev_path <- file.path("inst", "prompts", "enrich_tlf_prompt.txt")
    if (file.exists(dev_path)) path <- dev_path
  }
  if (!nzchar(path) || !file.exists(path)) {
    cli::cli_abort("Prompt template not found: enrich_tlf_prompt.txt")
  }
  tmpl <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  tlf_json <- jsonlite::toJSON(tlf_payload, auto_unbox = TRUE, pretty = TRUE)
  glue::glue(tmpl, tlf_json = tlf_json, .open = "{", .close = "}")
}

#' Call LLM via ellmer and return the raw response text.
#' @noRd
.invoke_llm <- function(prompt, provider, model, api_key,
                        system_prompt = paste(
                          "You are an expert CDISC clinical statistical programmer.",
                          "Reply with valid JSON only, no surrounding prose.")) {
  env_var <- switch(provider,
    anthropic = "ANTHROPIC_API_KEY",
    openai    = "OPENAI_API_KEY",
    gemini    = "GEMINI_API_KEY"
  )

  if (nzchar(api_key) && !identical(Sys.getenv(env_var), api_key)) {
    prior <- Sys.getenv(env_var, unset = NA_character_)
    args <- list(api_key)
    names(args) <- env_var
    do.call(Sys.setenv, args)
    on.exit({
      if (is.na(prior)) {
        Sys.unsetenv(env_var)
      } else {
        restore_args <- list(prior)
        names(restore_args) <- env_var
        do.call(Sys.setenv, restore_args)
      }
    }, add = TRUE)
  }

  chat <- switch(provider,
    anthropic = ellmer::chat_anthropic(
      system_prompt = system_prompt,
      model         = model,
      params        = ellmer::params(max_tokens = 8192L)
    ),
    openai = ellmer::chat_openai(
      system_prompt = system_prompt,
      model         = model,
      params        = ellmer::params(max_tokens = 4096L)
    ),
    gemini = ellmer::chat_google_gemini(
      system_prompt = system_prompt,
      model         = model,
      params        = ellmer::params(max_tokens = 8192L)
    ),
    cli::cli_abort("Unsupported LLM provider: {.val {provider}}")
  )

  tryCatch(
    chat$chat(prompt, echo = "none"),
    error = function(e) {
      cli::cli_warn(c("LLM API call failed:", "x" = conditionMessage(e)))
      ""
    }
  )
}

#' Parse JSON response. Tolerates markdown fences and stray prose around
#' the JSON object. Returns an empty list on parse failure.
#' @noRd
.parse_llm_json <- function(text) {
  text <- trimws(text %||% "")
  if (!nzchar(text)) return(list())
  if (startsWith(text, "```")) {
    lines <- strsplit(text, "\n", fixed = TRUE)[[1]][-1]
    if (length(lines) > 0 && trimws(lines[length(lines)]) == "```") {
      lines <- lines[-length(lines)]
    }
    text <- paste(lines, collapse = "\n")
  }
  if (!startsWith(trimws(text), "{") && !startsWith(trimws(text), "[")) {
    first <- regexpr("[{\\[]", text)
    last  <- max(gregexpr("[}\\]]", text)[[1]])
    if (first > 0 && last > first) text <- substr(text, first, last)
  }
  tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE),
           error = function(e) {
             cli::cli_warn(c("Failed to parse LLM JSON response:",
                             "x" = conditionMessage(e)))
             list()
           })
}

#' Deterministic fallback when the LLM is unavailable / parsing fails.
#' @noRd
.infer_analysis_type <- function(section) {
  title <- tolower(section$title %||% "")
  if (grepl("adverse|teae|safety event|ae ", title)) return("AE_FREQUENCY")
  if (grepl("survival|kaplan|time to event", title)) return("SURVIVAL")
  if (section$tlf_type == "LISTING") return("LISTING")
  if (section$tlf_type == "FIGURE")  return("FIGURE")
  if (any(grepl("^(n|mean|sd|median|min|max|q1|q3)$",
                tolower(vapply(section$stub_rows, function(r) r$label %||% "",
                               character(1)))))) return("CONTINUOUS")
  "CATEGORICAL"
}

.infer_method_name <- function(atype) {
  switch(toupper(atype %||% "CATEGORICAL"),
         CONTINUOUS   = "Summary Statistics - Continuous",
         CATEGORICAL  = "Count and Percentage",
         SURVIVAL     = "Kaplan-Meier Estimate",
         AE_FREQUENCY = "AE Frequency Count",
         LISTING      = "Listing",
         FIGURE       = "Listing",
         "Count and Percentage")
}

.fallback_enrichments <- function(annotated_rows) {
  lapply(annotated_rows, function(r) {
    refs <- extract_annotation_vars(r$annotation)
    primary <- if (length(refs) > 0) refs[1] else ""
    pieces  <- if (nzchar(primary)) strsplit(primary, "\\.", fixed = FALSE)[[1]] else c("", "")
    list(
      label            = r$label,
      primary_dataset  = if (length(pieces) >= 1) pieces[1] else "",
      primary_variable = if (length(pieces) >= 2) pieces[2] else "",
      data_subset      = NULL,
      variable_role    = "ANALYSIS"
    )
  })
}
