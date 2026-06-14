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
#' @param section     A single TLF section list (output of `parse_shell_docx()`).
#' @param spec_lookup The `lookup` element of `parse_adam_spec()` (or NULL).
#' @param model       Anthropic model id (default `"claude-sonnet-4-6"`).
#' @param api_key     API key (default reads `ANTHROPIC_API_KEY`).
#'
#' @return The input section with these fields added:
#'   `analysis_type`, `ars_method_name`, `by_variable`,
#'   `by_variable_dataset` (the spec-resolved dataset the grouping variable
#'   lives in), `needs_review` (TRUE when no grouping could be resolved for
#'   a grouped output type), and an `enriched_rows` list (one per annotated
#'   row) with fields `label`, `primary_dataset`, `primary_variable`,
#'   `data_subset`, `variable_role`.
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
  parsed <- .enrich_structured(prompt, .enrich_type(), provider = provider,
                               model = model, api_key = api_key) %||% list()

  ## Record degraded-enrichment diagnostics: whole-response failure first,
  ## then per-field fallbacks. These feed the validation report so a study
  ## team can see exactly which TLFs ran on heuristics instead of the LLM.
  if (length(parsed) == 0) {
    diag_add(
      stage = "enrich_llm", severity = "FAIL",
      problem = sprintf("LLM call failed (provider %s, model %s)",
                        provider %||% "?", model %||% "default"),
      tlf_number = section$tlf_number,
      location = section$title %||% "",
      action = "All enrichment fields fell back to keyword heuristics -- review this TLF's analysis type, method, and grouping"
    )
  } else {
    if (is.null(parsed$analysis_type) || !nzchar(parsed$analysis_type %||% "")) {
      diag_add(
        stage = "enrich_llm", severity = "WARN",
        problem = "LLM response missing 'analysis_type'",
        tlf_number = section$tlf_number,
        action = "Inferred from title/stub keywords"
      )
    }
    if (is.null(parsed$row_enrichments) || length(parsed$row_enrichments) == 0) {
      diag_add(
        stage = "enrich_llm", severity = "WARN",
        problem = "LLM response missing 'row_enrichments'",
        tlf_number = section$tlf_number,
        action = "Per-row metadata regex-derived from annotations only (no roles/subsets)"
      )
    }
  }

  ## Normalise empty strings (schema may return "" for optional fields) to
  ## NULL so the %||% fallbacks fire.
  nz <- function(x) if (is.null(x) || !nzchar(as.character(x)[1])) NULL else x
  atype <- nz(parsed$analysis_type)

  ## OTHER = the model could not map this output to a known analysis type.
  ## Flag for human review and infer a concrete type for method/grouping.
  if (identical(atype, "OTHER")) {
    section$needs_review <- TRUE
    inferred <- .infer_analysis_type(section)
    diag_add(
      stage = "enrich_llm", severity = "WARN",
      problem = "LLM classified analysis_type as OTHER (novel / unmapped table type)",
      tlf_number = section$tlf_number,
      location = section$title %||% "",
      action = sprintf("Flagged in _meta.sections_needing_review; treated as %s for method selection", inferred)
    )
    atype <- inferred
  }

  section$analysis_type   <- atype %||% .infer_analysis_type(section)
  section$ars_method_name <- nz(parsed$ars_method_name) %||% .infer_method_name(section$analysis_type)
  section$enriched_rows   <- nz(parsed$row_enrichments) %||% .fallback_enrichments(annotated_rows)
  ## A flag annotation like "ADSL.RANDFL='Y'" is a SUBSET FILTER, not an
  ## analysis variable. The LLM (and the heuristic fallback) frequently omit
  ## the DataSubset, which makes ars_to_ard() count the flag's levels ("Y", "")
  ## -- collapsing e.g. a 15-row disposition table to 2 rows with a stray "Y"
  ## column. Backfill the where-clause for any row that lacks a data_subset.
  section$enriched_rows   <- .backfill_data_subsets(section$enriched_rows,
                                                    annotated_rows)

  ## --- Grouping variable resolution -----------------------------------
  ## Multi-level: the LLM returns an ORDERED by_variables array (outermost
  ## first) plus an include_total flag. Each variable is grounded against
  ## the spec (resolving the dataset it lives in -- groupings are NOT
  ## always ADSL). Fallback chain when none resolve:
  ## 1. Spec-detected treatment variable (TRTxxA/TRTxxP, then ACTARM/ARM).
  ## 2. Ungrouped + needs_review -- never silently inject a guess.
  ## Listings and figures legitimately have no grouping; skip the fallback
  ## chain for them.
  raw_by <- parsed$by_variables %||%
    (if (!is.null(nz(parsed$by_variable))) list(parsed$by_variable) else list())
  groupings <- Filter(
    Negate(is.null),
    lapply(raw_by, .resolve_grouping_from_spec, spec_lookup = spec_lookup)
  )
  for (g in groupings) {
    if (isFALSE(g$in_spec)) {
      diag_add(
        stage = "enrich_llm", severity = "WARN",
        problem = sprintf("Grouping variable '%s' not found in the ADaM spec",
                          g$variable),
        tlf_number = section$tlf_number,
        action = "Kept as-is -- verify the grouping variable for this TLF"
      )
    }
  }
  if (length(groupings) == 0 &&
      !section$analysis_type %in% c("LISTING", "FIGURE")) {
    fallback_var <- .default_treatment_var(spec_lookup)
    if (!is.null(fallback_var)) {
      groupings <- list(list(variable = fallback_var, dataset = "ADSL",
                             in_spec = TRUE))
      diag_add(
        stage = "enrich_llm", severity = "WARN",
        problem = "No grouping variable identified for this TLF",
        tlf_number = section$tlf_number,
        action = sprintf("Defaulted to spec-detected treatment variable %s -- verify against the shell's column headers", fallback_var)
      )
    } else {
      section$needs_review <- TRUE
      diag_add(
        stage = "enrich_llm", severity = "WARN",
        problem = "No grouping variable identified and no treatment variable found in the ADaM spec",
        tlf_number = section$tlf_number,
        action = "Section built UNGROUPED and flagged in _meta.sections_needing_review"
      )
    }
  }
  section$groupings     <- groupings
  section$include_total <- isTRUE(as.logical(parsed$include_total %||% FALSE))
  ## Back-compat single-grouping fields = outermost grouping.
  first <- if (length(groupings) > 0) groupings[[1]] else NULL
  section$by_variable         <- first$variable %||% ""
  section$by_variable_dataset <- first$dataset  %||% "ADSL"

  section
}


## --- Internal helpers ------------------------------------------------------

#' Read the prompt template from inst/prompts/ and substitute the payload.
#' Uses `<<` / `>>` glue delimiters so literal `{` / `}` characters in the
#' serialised payload (e.g. inside an annotation string) are never parsed
#' as interpolation -- those braces previously corrupted the prompt.
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
  glue::glue(tmpl, tlf_json = tlf_json, .open = "<<", .close = ">>")
}

#' ellmer type schema for the enrichment response.
#'
#' Using a schema forces the model (via tool calling) to return exactly
#' this shape -- eliminating the markdown-fence / stray-prose parsing that
#' a free-text JSON response required. All fields except the row label are
#' optional so a partial answer still validates; missing fields fall back
#' to heuristics in `enrich_with_llm()`.
#' @noRd
.enrich_type <- function() {
  ellmer::type_object(
    .description = "Semantic metadata for one TLF shell section.",
    analysis_type = ellmer::type_enum(
      values = c("CONTINUOUS", "CATEGORICAL", "SURVIVAL", "AE_FREQUENCY",
                 "FIGURE", "LISTING", "OTHER"),
      description = paste(
        "Analysis kind. Use OTHER only when none of the named types fits",
        "(e.g. PK parameter summary, shift table, logistic regression)."
      )
    ),
    ars_method_name = ellmer::type_string(
      "Closest standard ARS method name, or a short descriptive name.",
      required = FALSE
    ),
    by_variables = ellmer::type_array(
      items = ellmer::type_string(),
      description = paste(
        "Ordered grouping variables (outermost first) whose values form",
        "the result columns -- e.g. [\"TRT01A\"] for treatment columns, or",
        "[\"TRT01A\",\"SEX\"] when sex is nested within treatment. Bare",
        "ADaM variable names (no dataset prefix) present in",
        "available_variables. Empty array when the output has no grouping",
        "columns. Do NOT include a pseudo-variable for a Total column --",
        "use include_total instead."),
      required = FALSE
    ),
    include_total = ellmer::type_boolean(
      paste("TRUE when the output carries an overall/Total column in",
            "addition to the per-group columns."),
      required = FALSE
    ),
    row_enrichments = ellmer::type_array(
      items = ellmer::type_object(
        label = ellmer::type_string("Stub row label exactly as given."),
        primary_dataset = ellmer::type_string(
          "Dataset portion of the annotation (e.g. ADSL).", required = FALSE),
        primary_variable = ellmer::type_string(
          "Variable portion of the annotation (e.g. AGE).", required = FALSE),
        variable_role = ellmer::type_enum(
          values = c("ANALYSIS", "GROUPING", "COUNT", "FLAG"),
          description = "Role this row's variable plays.", required = FALSE),
        data_subset = ellmer::type_object(
          .description = paste(
            "Row-specific WHERE condition beyond the population flag;",
            "omit when none."),
          .required = FALSE,
          dataset = ellmer::type_string(required = FALSE),
          variable = ellmer::type_string(required = FALSE),
          comparator = ellmer::type_enum(
            values = c("EQ", "NE", "IN", "NOTIN", "GT", "GE", "LT", "LE"),
            required = FALSE),
          value = ellmer::type_array(items = ellmer::type_string(),
                                     required = FALSE)
        )
      ),
      description = "One entry per annotated row.",
      required = FALSE
    )
  )
}

#' TRUE if an error looks transient (rate limit, overload, 5xx, timeout,
#' connection reset) and therefore worth retrying. Auth/validation errors
#' (e.g. HTTP 401/400) are NOT retryable -- fail fast.
#' @noRd
.is_retryable <- function(e) {
  msg <- tolower(conditionMessage(e))
  grepl(paste0("429|rate.?limit|overloaded|529|503|502|500|",
               "timeout|timed out|connection|temporarily|unavailable"),
        msg)
}

#' Run `fn` with exponential backoff on retryable errors. Re-raises the
#' last error if all attempts fail or the error is non-retryable. `sleep`
#' is injectable so the backoff is unit-testable without real waiting.
#' @noRd
.with_retry <- function(fn, max_tries = 3L, base_delay = 1,
                        sleep = base::Sys.sleep) {
  last_err <- NULL
  for (attempt in seq_len(max_tries)) {
    res <- tryCatch(list(ok = TRUE, value = fn()),
                    error = function(e) list(ok = FALSE, error = e))
    if (isTRUE(res$ok)) return(res$value)
    last_err <- res$error
    if (attempt < max_tries && .is_retryable(last_err)) {
      sleep(base_delay * (2^(attempt - 1L)))
    } else {
      break
    }
  }
  stop(last_err)
}

#' Build an ellmer chat for `provider` while `api_key` is in scope, then
#' call `chat_structured()` against `schema` with retry. Returns the parsed
#' named list, or NULL on terminal failure (caller falls back to
#' heuristics).
#' @noRd
.enrich_structured <- function(prompt, schema, provider, model, api_key,
                               max_tries = 3L, sleep = base::Sys.sleep,
                               system_prompt = paste(
                                 "You are an expert CDISC clinical",
                                 "statistical programmer.")) {
  env_var <- switch(provider,
    anthropic = "ANTHROPIC_API_KEY",
    openai    = "OPENAI_API_KEY",
    gemini    = "GEMINI_API_KEY",
    cli::cli_abort("Unsupported LLM provider: {.val {provider}}")
  )

  ## Keep the key in the environment for the WHOLE call -- the Anthropic /
  ## OpenAI / Gemini providers read it at request time, not construction.
  if (nzchar(api_key %||% "") && !identical(Sys.getenv(env_var), api_key)) {
    prior <- Sys.getenv(env_var, unset = NA_character_)
    args <- list(api_key); names(args) <- env_var
    do.call(Sys.setenv, args)
    on.exit({
      if (is.na(prior)) {
        Sys.unsetenv(env_var)
      } else {
        restore_args <- list(prior); names(restore_args) <- env_var
        do.call(Sys.setenv, restore_args)
      }
    }, add = TRUE)
  }

  max_tokens <- if (identical(provider, "openai")) 4096L else 8192L
  chat <- switch(provider,
    anthropic = ellmer::chat_anthropic(system_prompt = system_prompt,
      model = model, params = ellmer::params(max_tokens = max_tokens)),
    openai = ellmer::chat_openai(system_prompt = system_prompt,
      model = model, params = ellmer::params(max_tokens = max_tokens)),
    gemini = ellmer::chat_google_gemini(system_prompt = system_prompt,
      model = model, params = ellmer::params(max_tokens = max_tokens))
  )

  ## convert = FALSE keeps the raw list-of-lists shape (row_enrichments as
  ## a list, not a data.frame) that build_ars_json() iterates over.
  tryCatch(
    .with_retry(
      function() chat$chat_structured(prompt, type = schema, echo = "none",
                                      convert = FALSE),
      max_tries = max_tries, sleep = sleep
    ),
    error = function(e) {
      cli::cli_warn(c("LLM structured call failed:", "x" = conditionMessage(e)))
      NULL
    }
  )
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

#' Resolve a grouping variable name against the ADaM spec.
#'
#' Accepts a bare variable ("TRT01A", "SEX") or a dataset-qualified form
#' ("ADSL.TRT01A"); returns `list(variable, dataset, in_spec)` or NULL when
#' no variable was provided. Dataset resolution order: explicit qualifier
#' (when confirmed by the spec), then ADSL, then the first spec dataset
#' carrying the variable. With no spec available, the qualifier or ADSL is
#' used and `in_spec` is NA.
#' @noRd
.resolve_grouping_from_spec <- function(by_var, spec_lookup) {
  by_var <- toupper(trimws(as.character(by_var %||% "")))
  if (length(by_var) == 0 || !nzchar(by_var)) return(NULL)

  ds_hint <- NULL
  if (grepl(".", by_var, fixed = TRUE)) {
    pieces  <- strsplit(by_var, ".", fixed = TRUE)[[1]]
    ds_hint <- pieces[1]
    by_var  <- pieces[length(pieces)]
  }

  if (is.null(spec_lookup) || length(spec_lookup) == 0) {
    return(list(variable = by_var, dataset = ds_hint %||% "ADSL",
                in_spec = NA))
  }

  keys   <- toupper(names(spec_lookup))
  ds_all <- sub("\\..*$", "", keys)
  vars   <- sub("^.*\\.", "", keys)
  hits   <- unique(ds_all[vars == by_var])

  if (length(hits) == 0) {
    return(list(variable = by_var, dataset = ds_hint %||% "ADSL",
                in_spec = FALSE))
  }
  dataset <- if (!is.null(ds_hint) && ds_hint %in% hits) {
    ds_hint
  } else if ("ADSL" %in% hits) {
    "ADSL"
  } else {
    hits[1]
  }
  list(variable = by_var, dataset = dataset, in_spec = TRUE)
}

#' Detect the study's treatment variable from the ADaM spec (ADSL only).
#' Preference: TRT01A, TRT01P, any TRTxxA/TRTxxP, ACTARM, ARM.
#' Returns NULL when the spec is absent or carries none of these.
#' @noRd
.default_treatment_var <- function(spec_lookup) {
  if (is.null(spec_lookup) || length(spec_lookup) == 0) return(NULL)
  keys      <- toupper(names(spec_lookup))
  adsl_vars <- sub("^ADSL\\.", "", keys[startsWith(keys, "ADSL.")])
  for (cand in c("TRT01A", "TRT01P")) {
    if (cand %in% adsl_vars) return(cand)
  }
  trt <- adsl_vars[grepl("^TRT\\d{2}[AP]$", adsl_vars)]
  if (length(trt) > 0) return(sort(trt)[1])
  for (cand in c("ACTARM", "ARM")) {
    if (cand %in% adsl_vars) return(cand)
  }
  NULL
}

#' Backfill a DataSubset onto every enriched row that lacks one, derived from
#' the row's original annotation WHERE clause. Idempotent: rows that already
#' carry a non-empty `data_subset` are left untouched (LLM output wins).
#' @noRd
.backfill_data_subsets <- function(enriched_rows, annotated_rows) {
  ann_by_label <- stats::setNames(
    lapply(annotated_rows, function(r) r$annotation %||% ""),
    vapply(annotated_rows, function(r) r$label %||% "", character(1)))
  lapply(enriched_rows, function(er) {
    ds <- er$data_subset
    if (!is.null(ds) && length(ds) > 0) return(er)        # keep existing
    ann <- ann_by_label[[er$label %||% ""]]
    if (is.null(ann) || !nzchar(ann)) return(er)
    fs <- flat_data_subset(ann)
    if (is.null(fs)) return(er)
    er$data_subset <- fs
    diag_add(
      stage = "enrich_llm", severity = "INFO",
      problem = sprintf("Backfilled DataSubset for row '%s' from its annotation",
                        er$label %||% "?"),
      location = ann,
      action = sprintf("Applied %s.%s %s '%s' as a subset filter (not an analysis variable)",
                       fs$dataset, fs$variable, fs$comparator,
                       if (length(fs$value)) fs$value[[1]] else ""))
    er
  })
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
