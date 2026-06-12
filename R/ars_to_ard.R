## arsbridge -- ars_to_ard.R
## ---------------------------------------------------------------------------
## Executes CDISC ARS JSON specifications directly using the {cards} package.
## Dynamically loads ADaM datasets and binds results into a single tidy ARD.

## Method-id -> native {cards} executor registry. Keys MUST be ids from
## .STANDARD_METHODS (build_ars_json.R) -- a consistency test enforces
## this. Methods absent here (e.g. MTH_KAPLAN_MEIER_ESTIMATE) run through
## the generic fallback summarizer, which is recorded in the ARD's
## method_actual column and as a diagnostic.
## Each executor: function(df, var, by, denom, subject_key) -> card.
.ARD_EXECUTORS <- list(
  MTH_SUMMARY_STATISTICS_CONTINUOUS = function(df, var, by, denom, subject_key) {
    if (!is.numeric(df[[var]])) {
      df[[var]] <- as.numeric(df[[var]])
    }
    cards::ard_continuous(
      data = df,
      variables = all_of(var),
      by = all_of(by)
    )
  },
  MTH_COUNT_AND_PERCENTAGE = function(df, var, by, denom, subject_key) {
    cards::ard_categorical(
      data = df,
      variables = all_of(var),
      by = all_of(by),
      denominator = denom
    )
  },
  MTH_AE_FREQUENCY_COUNT = function(df, var, by, denom, subject_key) {
    df_unique <- df |>
      dplyr::distinct(!!rlang::sym(subject_key), !!rlang::sym(var),
                      .keep_all = TRUE)
    cards::ard_categorical(
      data = df_unique,
      variables = all_of(var),
      by = all_of(by),
      denominator = denom
    )
  },
  MTH_SUBJECT_COUNT = function(df, var, by, denom, subject_key) {
    df_unique <- df |>
      dplyr::distinct(!!rlang::sym(subject_key), .keep_all = TRUE)
    if (var == subject_key && !is.null(by)) {
      cards::ard_categorical(
        data = df_unique,
        variables = all_of(by)
      )
    } else if (var == subject_key) {
      cards::ard_total_n(df_unique)
    } else {
      cards::ard_categorical(
        data = df_unique,
        variables = all_of(var),
        by = all_of(by),
        denominator = denom
      )
    }
  }
)

#' Execute ARS JSON and return an ARD object using {cards}
#'
#' Reads a CDISC ARS JSON specification and executes the analyses defined within
#' it directly using the `{cards}` package, dynamically loading the ADaM datasets
#' (.csv or .xpt) and combining individual ARD tables into a single tidy ARD object.
#'
#' @param ars_path   Path to the CDISC ARS JSON file.
#' @param adam_dir   Directory containing the ADaM datasets (.csv or .xpt).
#' @param output_ids Optional character vector of Output IDs to run only analyses
#'   referenced by those outputs. Matching is case-insensitive and checks both
#'   Output ID and Output Name (e.g. "T-14-1-1" or "T_14_1_1").
#' @param analysis_ids Optional character vector of Analysis IDs to run only
#'   those specific analyses.
#' @param subject_key Subject-level identifier variable used for
#'   distinct-subject counting and cross-dataset population joins.
#'   Default `"USUBJID"`; set e.g. `"SUBJID"` or `"PATID"` for studies with
#'   a non-standard subject key.
#'
#' @return A tidy ARD data frame of class `"card"`, with traceability
#'   columns `analysis_id`, `method_id`, `output_id`, `method_intended`,
#'   and `method_actual` (differs from `method_intended` when the generic
#'   fallback summarizer was used).
#' @importFrom tidyselect all_of
#' @export
#' @examples
#' \dontrun{
#'   ard <- ars_to_ard("outputs/reporting_event.json", "inputs/ADaM")
#' }
ars_to_ard <- function(ars_path, adam_dir, output_ids = NULL,
                       analysis_ids = NULL, subject_key = "USUBJID") {
  if (!file.exists(ars_path)) {
    cli::cli_abort("ARS JSON file not found: {.path {ars_path}}")
  }
  if (!dir.exists(adam_dir)) {
    cli::cli_abort("ADaM directory not found: {.path {adam_dir}}")
  }

  ## Fresh diagnostics for this execution run (inspect via ars_diagnostics()).
  diag_reset()

  spec <- jsonlite::fromJSON(ars_path, simplifyVector = FALSE)

  # Cache list for loaded datasets
  dfs <- list()
  get_df <- function(name) {
    if (is.null(name) || !nzchar(name)) return(NULL)
    name_upper <- toupper(name)
    if (!name_upper %in% names(dfs)) {
      files <- list.files(adam_dir, full.names = TRUE)
      basenames <- tolower(basename(files))
      csv_file <- files[basenames == tolower(paste0(name_upper, ".csv"))]
      xpt_file <- files[basenames == tolower(paste0(name_upper, ".xpt"))]

      if (length(xpt_file) > 0) {
        if (requireNamespace("haven", quietly = TRUE)) {
          dfs[[name_upper]] <<- haven::read_xpt(xpt_file[1])
        } else {
          cli::cli_abort("Package {.pkg haven} is required to read .xpt files.")
        }
      } else if (length(csv_file) > 0) {
        dfs[[name_upper]] <<- utils::read.csv(csv_file[1], stringsAsFactors = FALSE, check.names = FALSE)
      } else {
        cli::cli_warn("Dataset {.val {name_upper}} not found in {.path {adam_dir}}.")
        diag_add(
          stage = "execute_ard", severity = "FAIL",
          problem = sprintf("Dataset %s not found in ADaM directory", name_upper),
          location = adam_dir,
          action = "All analyses against this dataset were skipped"
        )
        dfs[[name_upper]] <<- NULL
      }
    }
    dfs[[name_upper]]
  }

  # Scalar character helper to prevent list-column issues when simplifyVector = FALSE
  as_scalar_char <- function(x) {
    if (is.null(x)) return(NULL)
    val <- unlist(x)
    if (length(val) == 0) return(NULL)
    as.character(val[1])
  }

  # Build mapping from analysisId to outputId
  analysis_to_output <- list()
  for (out in spec[["outputs"]]) {
    out_id <- as_scalar_char(out[["id"]])
    if (is.null(out_id)) next
    for (an_id in unlist(out[["referencedAnalysisIds"]])) {
      an_id_str <- as_scalar_char(an_id)
      if (!is.null(an_id_str)) {
        analysis_to_output[[an_id_str]] <- out_id
      }
    }
  }

  # Build lookup map for groupings
  grouping_map <- list()
  for (gf in spec[["analysisGroupings"]]) {
    gf_id <- as_scalar_char(gf[["id"]])
    if (is.null(gf_id)) next
    gf_var <- NULL
    if (is.list(gf[["groupingVariable"]])) {
      gf_var <- gf[["groupingVariable"]][["variable"]]
    } else {
      gf_var <- gf[["groupingVariable"]]
    }
    if (is.null(gf_var) || !nzchar(gf_var)) {
      gf_var <- gf[["name"]]
    }
    gf_var_str <- as_scalar_char(gf_var)
    if (!is.null(gf_var_str)) {
      grouping_map[[gf_id]] <- gf_var_str
    }
  }

  # Helper functions to clean and evaluate filters
  clean_var_name <- function(var_name, df_names) {
    if (is.null(var_name) || !nzchar(var_name)) return(var_name)
    if (var_name %in% df_names) return(var_name)
    if (grepl(".", var_name, fixed = TRUE)) {
      parts <- strsplit(var_name, ".", fixed = TRUE)[[1]]
      short_var <- parts[length(parts)]
      if (short_var %in% df_names) return(short_var)
    }
    var_name
  }

  eval_condition <- function(df, cond_obj) {
    var_name <- cond_obj[["variable"]]
    comp <- cond_obj[["comparator"]]
    val_list <- cond_obj[["value"]]

    if (is.null(var_name) || !nzchar(var_name)) {
      return(rep(TRUE, nrow(df)))
    }

    var_name <- clean_var_name(var_name, names(df))

    if (!var_name %in% names(df)) {
      return(rep(FALSE, nrow(df)))
    }

    col_val <- df[[var_name]]
    val <- unlist(val_list)

    if (comp %in% c("EQ", "IN")) {
      if (length(val) == 0) {
        is.na(col_val) | col_val == ""
      } else {
        col_val %in% val
      }
    } else if (comp %in% c("NE", "NOTIN")) {
      if (length(val) == 0) {
        !is.na(col_val) & col_val != ""
      } else {
        !(col_val %in% val)
      }
    } else if (comp == "LT") {
      col_val < as.numeric(val)
    } else if (comp == "LE") {
      col_val <= as.numeric(val)
    } else if (comp == "GT") {
      col_val > as.numeric(val)
    } else if (comp == "GE") {
      col_val >= as.numeric(val)
    } else if (comp == "CONTAINS") {
      ## arsbridge extension comparator: case-insensitive substring match
      ## against any of the supplied values.
      if (length(val) == 0) {
        rep(FALSE, nrow(df))
      } else {
        Reduce(`|`, lapply(val, function(v) {
          grepl(tolower(v), tolower(as.character(col_val)), fixed = TRUE)
        }))
      }
    } else {
      rep(TRUE, nrow(df))
    }
  }

  eval_where_clause <- function(df, where_clause) {
    if (is.null(where_clause)) {
      return(rep(TRUE, nrow(df)))
    }
    if (!is.null(where_clause[["condition"]])) {
      return(eval_condition(df, where_clause[["condition"]]))
    }
    if (!is.null(where_clause[["compoundExpression"]])) {
      comp_expr <- where_clause[["compoundExpression"]]
      op <- comp_expr[["logicalOperator"]]
      clauses <- comp_expr[["whereClauses"]]

      if (length(clauses) == 0) {
        return(rep(TRUE, nrow(df)))
      }

      results <- lapply(clauses, function(clause) eval_where_clause(df, clause))

      if (identical(op, "AND")) {
        Reduce(`&`, results)
      } else if (identical(op, "OR")) {
        Reduce(`|`, results)
      } else {
        rep(TRUE, nrow(df))
      }
    } else {
      if (!is.null(where_clause[["variable"]])) {
        return(eval_condition(df, where_clause))
      }
      rep(TRUE, nrow(df))
    }
  }

  get_referenced_datasets <- function(where_clause) {
    if (is.null(where_clause)) {
      return(character(0))
    }
    if (!is.null(where_clause[["condition"]])) {
      cond <- where_clause[["condition"]]
      return(cond[["dataset"]] %||% character(0))
    }
    if (!is.null(where_clause[["compoundExpression"]])) {
      comp_expr <- where_clause[["compoundExpression"]]
      clauses <- comp_expr[["whereClauses"]]
      return(unique(unlist(lapply(clauses, get_referenced_datasets))))
    }
    if (!is.null(where_clause[["dataset"]])) {
      return(where_clause[["dataset"]])
    }
    character(0)
  }

  apply_where_clause <- function(target_ds_name, where_clause) {
    df <- get_df(target_ds_name)
    if (is.null(df) || is.null(where_clause)) {
      return(df)
    }

    ref_datasets <- get_referenced_datasets(where_clause)
    if (length(ref_datasets) == 0) {
      return(df)
    }

    if (length(ref_datasets) == 1 && ref_datasets == target_ds_name) {
      keep <- eval_where_clause(df, where_clause)
      return(df[keep, , drop = FALSE])
    }

    valid_subjects <- NULL
    for (ref_ds in ref_datasets) {
      ref_df <- get_df(ref_ds)
      if (is.null(ref_df)) next
      keep <- eval_where_clause(ref_df, where_clause)
      ref_df_filtered <- ref_df[keep, , drop = FALSE]

      if (subject_key %in% names(ref_df_filtered)) {
        ds_subjs <- unique(ref_df_filtered[[subject_key]])
        if (is.null(valid_subjects)) {
          valid_subjects <- ds_subjs
        } else {
          valid_subjects <- intersect(valid_subjects, ds_subjs)
        }
      } else {
        diag_add(
          stage = "execute_ard", severity = "WARN",
          problem = sprintf("Subject key %s not present in dataset %s referenced by a where-clause",
                            subject_key, ref_ds),
          location = target_ds_name,
          action = "Cross-dataset filter from this dataset NOT applied"
        )
      }
    }

    if (!is.null(valid_subjects) && subject_key %in% names(df)) {
      df <- df[df[[subject_key]] %in% valid_subjects, , drop = FALSE]
    }

    return(df)
  }

  # Walk analyses and execute
  ard_list <- list()
  for (ana in spec[["analyses"]]) {
    analysis_id <- as_scalar_char(ana[["id"]])
    if (is.null(analysis_id)) next
    out_id <- if (!is.null(analysis_id)) analysis_to_output[[analysis_id]] else NULL

    # Filter by user-selected output_ids and analysis_ids
    if (!is.null(output_ids) || !is.null(analysis_ids)) {
      matched <- FALSE
      if (!is.null(analysis_ids) && tolower(analysis_id) %in% tolower(analysis_ids)) {
        matched <- TRUE
      }
      if (!is.null(output_ids) && !is.null(out_id)) {
        # Match output ID or output name
        out_obj <- Filter(function(o) identical(as_scalar_char(o[["id"]]), out_id), spec[["outputs"]])
        out_name <- if (length(out_obj) > 0) as_scalar_char(out_obj[[1]][["name"]]) else NULL
        if (tolower(out_id) %in% tolower(output_ids) || (!is.null(out_name) && tolower(out_name) %in% tolower(output_ids))) {
          matched <- TRUE
        }
      }
      if (!matched) next
    }

    method_id <- as_scalar_char(ana[["methodId"]])
    analysis_var <- as_scalar_char(ana[["analysisVariable"]][["variable"]] %||% ana[["variable"]])
    analysis_ds <- as_scalar_char(ana[["analysisVariable"]][["dataset"]] %||% ana[["dataset"]])
    pop_id <- as_scalar_char(ana[["analysisSetId"]])
    subset_id <- as_scalar_char(ana[["dataSubsetId"]])

    if (is.null(analysis_ds) || !nzchar(analysis_ds)) {
      cli::cli_warn("Skipping analysis {.val {analysis_id}}: primary dataset not specified.")
      next
    }
    if (is.null(analysis_var) || !nzchar(analysis_var)) {
      cli::cli_warn("Skipping analysis {.val {analysis_id}}: analysis variable not specified.")
      next
    }

    df_base <- get_df(analysis_ds)
    if (is.null(df_base)) {
      cli::cli_warn("Skipping analysis {.val {analysis_id}}: dataset {.val {analysis_ds}} not loaded.")
      next
    }

    pop_where <- NULL
    if (!is.null(pop_id) && nzchar(pop_id)) {
      for (aset in spec[["analysisSets"]]) {
        if (identical(as_scalar_char(aset[["id"]]), pop_id)) {
          pop_where <- aset
          break
        }
      }
    }

    subset_where <- NULL
    if (!is.null(subset_id) && nzchar(subset_id)) {
      for (dsub in spec[["dataSubsets"]]) {
        if (identical(as_scalar_char(dsub[["id"]]), subset_id)) {
          subset_where <- dsub
          break
        }
      }
    }

    # Apply filters
    df_filtered <- df_base
    if (!is.null(pop_where)) {
      df_filtered <- apply_where_clause(analysis_ds, pop_where)
    }

    df_population <- NULL
    adsl_df <- get_df("ADSL")
    if (!is.null(adsl_df)) {
      df_population <- apply_where_clause("ADSL", pop_where)
    }
    if (is.null(df_population)) {
      df_population <- df_filtered
    }

    if (!is.null(subset_where)) {
      df_filtered <- apply_where_clause(analysis_ds, subset_where)
    }

    analysis_var <- unname(clean_var_name(analysis_var, names(df_filtered)))
    if (!analysis_var %in% names(df_filtered)) {
      cli::cli_warn("Skipping analysis {.val {analysis_id}}: variable {.val {analysis_var}} not in dataset {.val {analysis_ds}}.")
      diag_add(
        stage = "execute_ard", severity = "FAIL",
        problem = sprintf("Variable %s not found in dataset %s", analysis_var, analysis_ds),
        location = analysis_id,
        action = "Analysis skipped -- no results in ARD"
      )
      next
    }

    # Resolve groupings
    grouping_vars <- character(0)
    if (!is.null(ana[["orderedGroupings"]])) {
      for (grp in ana[["orderedGroupings"]]) {
        gf_id <- as_scalar_char(grp[["groupingId"]])
        if (!is.null(gf_id)) {
          gf_var <- grouping_map[[gf_id]]
          if (!is.null(gf_var) && nzchar(gf_var)) {
            grouping_vars <- c(grouping_vars, gf_var)
          }
        }
      }
    }

    grouping_vars <- unname(sapply(grouping_vars, clean_var_name, df_names = names(df_filtered)))
    dropped_groupings <- grouping_vars[!grouping_vars %in% names(df_filtered)]
    if (length(dropped_groupings) > 0) {
      diag_add(
        stage = "execute_ard", severity = "WARN",
        problem = sprintf("Grouping variable(s) %s not present in dataset %s",
                          paste(dropped_groupings, collapse = ", "), analysis_ds),
        location = analysis_id,
        action = "Analysis ran UNGROUPED -- results are totals, not by-group"
      )
    }
    grouping_vars <- grouping_vars[grouping_vars %in% names(df_filtered)]
    by_arg <- if (length(grouping_vars) > 0) grouping_vars else NULL

    # Handle listings
    if (identical(method_id, "MTH_LISTING")) {
      cli::cli_inform("Skipping listing analysis {.val {analysis_id}} (listings are not summarized in ARD).")
      next
    }

    # Execute {cards} via the method registry; unknown methods use the
    # generic fallback summarizer and record the substitution.
    executor      <- .ARD_EXECUTORS[[method_id]]
    method_actual <- method_id
    if (is.null(executor)) {
      is_num <- is.numeric(df_filtered[[analysis_var]])
      method_actual <- if (is_num) "FALLBACK_CONTINUOUS" else "FALLBACK_CATEGORICAL"
      cli::cli_warn("Method {.val {method_id}} for analysis {.val {analysis_id}} not natively supported. Using fallback summarizer.")
      diag_add(
        stage = "execute_ard", severity = "WARN",
        problem = sprintf("Method %s not natively supported by the executor", method_id),
        location = analysis_id,
        action = sprintf("Generic %s summary used instead (method_actual = %s) -- verify the statistics match the shell",
                         if (is_num) "continuous" else "categorical", method_actual)
      )
      executor <- if (is_num) {
        function(df, var, by, denom, subject_key) {
          cards::ard_continuous(data = df, variables = all_of(var),
                                by = all_of(by))
        }
      } else {
        function(df, var, by, denom, subject_key) {
          cards::ard_categorical(data = df, variables = all_of(var),
                                 by = all_of(by), denominator = denom)
        }
      }
    }

    if (!subject_key %in% names(df_filtered) &&
        method_id %in% c("MTH_AE_FREQUENCY_COUNT", "MTH_SUBJECT_COUNT")) {
      cli::cli_warn("Skipping analysis {.val {analysis_id}}: subject key {.val {subject_key}} not in dataset {.val {analysis_ds}}.")
      diag_add(
        stage = "execute_ard", severity = "FAIL",
        problem = sprintf("Subject key %s required by %s but not present in dataset %s",
                          subject_key, method_id, analysis_ds),
        location = analysis_id,
        action = "Analysis skipped -- pass subject_key= to ars_to_ard() if this study uses a different identifier"
      )
      next
    }

    ard <- tryCatch(
      executor(df_filtered, analysis_var, by_arg, df_population, subject_key),
      error = function(e) {
        cli::cli_warn("Analysis {.val {analysis_id}} failed during {.pkg cards} calculation: {e$message}")
        diag_add(
          stage = "execute_ard", severity = "FAIL",
          problem = paste0("cards calculation error: ", conditionMessage(e)),
          location = analysis_id,
          action = "Analysis skipped -- no results in ARD"
        )
        NULL
      }
    )

    ## Shell carries an overall/Total column: add an ungrouped pass so the
    ## ARD holds both the by-group and the total statistics.
    include_total <- isTRUE(as.logical(unlist(ana[["includeTotal"]])[1] %||% FALSE))
    if (!is.null(ard) && include_total && !is.null(by_arg)) {
      ard_total <- tryCatch(
        executor(df_filtered, analysis_var, NULL, df_population, subject_key),
        error = function(e) {
          diag_add(
            stage = "execute_ard", severity = "WARN",
            problem = paste0("Total-column pass failed: ", conditionMessage(e)),
            location = analysis_id,
            action = "ARD contains by-group results only"
          )
          NULL
        }
      )
      if (!is.null(ard_total)) {
        ard <- cards::bind_ard(ard, ard_total)
      }
    }

    if (!is.null(ard)) {
      # Add traceability metadata
      ard[["analysis_id"]]     <- analysis_id
      ard[["method_id"]]       <- method_id
      ard[["output_id"]]       <- out_id %||% NA_character_
      ard[["method_intended"]] <- method_id
      ard[["method_actual"]]   <- method_actual

      ard_list[[length(ard_list) + 1L]] <- ard
    }
  }

  ## Surface a one-line execution-quality summary; full records via
  ## ars_diagnostics().
  diags <- diag_records()
  if (nrow(diags) > 0) {
    n_fail <- sum(diags$severity == "FAIL")
    n_warn <- sum(diags$severity == "WARN")
    cli::cli_alert_warning(
      "{nrow(diags)} execution diagnostic{?s} ({n_fail} FAIL, {n_warn} WARN) -- inspect with {.code ars_diagnostics()}"
    )
  }

  if (length(ard_list) == 0) {
    return(NULL)
  }

  # Combine all analyses
  final_ard <- cards::bind_ard(!!!ard_list)
  return(final_ard)
}
