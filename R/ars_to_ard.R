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

## ---------------------------------------------------------------------------
## Declared-but-unexecutable methods (ADR 0001 descriptor seeds / ADR 0002).
## These statistics are describable in the ARS but have no {cards}/{cardx}
## executor yet, so the engine cannot compute them. Rather than skip the
## analysis (which orphans the table cell) or coerce it into a meaningless
## count, ars_to_ard() emits a reserved, fully-keyed stub ARD row per declared
## statistic with result_status = "manual_pending" and stat = NA. A validated
## manual computation later fills that keyed slot (ADR 0002 phases 3-5), keeping
## the Output -> Analysis -> Method -> result chain intact.
##
## Each entry: stats = the stat_names the method declares; by_group = whether
## each statistic is reported per group level (e.g. an exact CI per arm) or once
## for the analysis (e.g. a single CMH p-value, or a between-group difference).
.UNEXECUTABLE_METHODS <- list(
  ## Generic declarative method the spec generator assigns to a
  ## capability-gated section (ADR 0002 phase 3) when the specific statistic is
  ## not yet classified. Reserves one manual_pending cell per analysis row.
  MTH_UNSUPPORTED_ANALYSIS     = list(stats = "result", by_group = FALSE),
  ## Specific declarative methods (ADR 0001 descriptor seeds) -- assigned once
  ## the shell reader classifies the exact statistic.
  MTH_CMH_TEST                 = list(stats = "p.value", by_group = FALSE),
  MTH_PROPORTION_CI_EXACT      = list(stats = c("conf.low", "conf.high"),
                                      by_group = TRUE),
  MTH_PROPORTION_DIFF_NEWCOMBE = list(stats = c("estimate", "conf.low",
                                                "conf.high"), by_group = FALSE)
)

## Methods that DO have a {cardx} executor (emitted by .emit_block) and so are
## computed -- not reserved -- whenever {cardx} is installed. They remain listed
## in .UNEXECUTABLE_METHODS above so that, when {cardx} is absent, the engine
## degrades gracefully and reserves a manual_pending stub instead of erroring.
## Seeded with the exact (Clopper-Pearson) CI, which needs no operand beyond the
## response variable and the treatment grouping. CMH and Newcombe stay
## reserve-only until their stratification / reference-group operands are
## carried through the spec.
.CARDX_METHODS <- c("MTH_PROPORTION_CI_EXACT")

## A one-row {cards} card used as a schema prototype, so stub rows carry exactly
## the columns of the installed {cards} version (list-cols and all) instead of a
## hand-coded guess that could drift across versions. Built with a `by` so the
## group1 / group1_level columns are present.
#' @noRd
.ard_schema_proto <- function() {
  cards::ard_categorical(
    data.frame(.v = factor("a"), .g = factor("x")),
    variables = ".v", by = ".g")[1, , drop = FALSE]
}

## Build one keyed, value-less stub ARD row from a prototype row.
#' @noRd
.stub_ard_row <- function(proto, variable, stat_name, by_var, by_level) {
  r <- proto
  r$group1         <- if (is.na(by_var)) NA_character_ else by_var
  r$group1_level   <- list(if (is.na(by_level)) NA_character_ else by_level)
  r$variable       <- variable %||% NA_character_
  r$variable_level <- list(NA_character_)
  if ("context" %in% names(r))    r$context    <- "manual_pending"
  r$stat_name      <- stat_name
  r$stat_label     <- stat_name
  r$stat           <- list(NA_real_)
  if ("fmt_fun" %in% names(r))    r$fmt_fun    <- list(NULL)
  if ("warning" %in% names(r))    r$warning    <- list(NULL)
  if ("error"   %in% names(r))    r$error      <- list(NULL)
  r
}

## Assemble all stub rows for one unexecutable method: one row per declared
## statistic, times the group levels present in the data when the statistic is
## reported by group. Returns a `card`, or NULL if nothing to reserve.
#' @noRd
.stub_ard_for_method <- function(res, method_id, df, by, var) {
  desc  <- .UNEXECUTABLE_METHODS[[method_id]]
  proto <- .ard_schema_proto()
  by1   <- if (!is.null(by) && length(by) && by[1] %in% names(df))
    by[1] else NA_character_
  levels <- if (isTRUE(desc$by_group) && !is.na(by1)) {
    unique(as.character(df[[by1]]))
  } else {
    NA_character_
  }
  rows <- list()
  for (lv in levels) {
    for (st in desc$stats) {
      rows[[length(rows) + 1L]] <- .stub_ard_row(
        proto, var, st,
        by_var   = if (isTRUE(desc$by_group)) by1 else NA_character_,
        by_level = lv)
    }
  }
  if (length(rows) == 0) return(NULL)
  cards::bind_ard(!!!rows)
}

#' Execute ARS JSON and return an ARD object using 'cards'
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
#' @param legacy Deprecated execution path. When `FALSE` (default) each analysis
#'   is computed by sourcing the pure-`{cards}` block arsbridge emits, so the
#'   ARD is produced by the same code shipped as the deliverable. When `TRUE`
#'   the retired `.ARD_EXECUTORS` registry is used instead (kept only for the
#'   engine-equivalence test and as a transitional escape hatch).
#'
#' @return A tidy ARD data frame of class `"card"`, with traceability
#'   columns `analysis_id`, `method_id`, `output_id`, `method_intended`,
#'   and `method_actual` (differs from `method_intended` when the generic
#'   fallback summarizer was used), plus provenance columns (ADR 0002):
#'   `result_status` (`"computed"` for engine output), `value_source`
#'   (`"cards"`), `derivation_ref` (the emitted block, `arsbridge:emitted:<id>`),
#'   `derived_by` (`"arsbridge"`), and `derived_dt` (run timestamp, ISO-8601;
#'   pin with `options(arsbridge.derived_dt=)`). These let a later partial /
#'   manual fill be distinguished from engine output without breaking
#'   traceability.
#' @importFrom tidyselect all_of
#' @export
#' @examples
#' \dontrun{
#'   ard <- ars_to_ard("outputs/reporting_event.json", "inputs/ADaM")
#' }
ars_to_ard <- function(ars_path, adam_dir, output_ids = NULL,
                       analysis_ids = NULL, subject_key = "USUBJID",
                       legacy = FALSE) {
  .require_file(ars_path, "ars_path", INPUT_ARS)
  .require_dir(adam_dir,  "adam_dir", INPUT_DATA)

  ## Fresh diagnostics for this execution run (inspect via ars_diagnostics()).
  diag_reset()

  spec <- .read_json(ars_path)

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
        ## .read_dataset reads .xpt/.csv and, on a read failure, records a FAIL
        ## diagnostic naming the dataset and returns NULL (this analysis is then
        ## skipped) rather than throwing a cryptic base-R error.
        dfs[[name_upper]] <<- .read_dataset(xpt_file[1], name_upper)
      } else if (length(csv_file) > 0) {
        dfs[[name_upper]] <<- .read_dataset(csv_file[1], name_upper)
      } else {
        cli::cli_warn("Dataset {.val {name_upper}} not found in {.path {adam_dir}}.")
        diag_add(
          stage = "execute_ard", severity = "FAIL", input = INPUT_DATA,
          problem = sprintf("Dataset %s not found in ADaM directory", name_upper),
          location = adam_dir,
          action = "All analyses against this dataset were skipped"
        )
        dfs[[name_upper]] <<- NULL
      }
    }
    dfs[[name_upper]]
  }

  # Scalar character helper (shared file-level implementation in resolve_analysis.R).
  as_scalar_char <- .as_scalar_char

  # Lookup maps built once from the spec and shared with resolve_analysis().
  analysis_to_output <- .build_analysis_to_output(spec)
  grouping_map       <- .build_grouping_map(spec)

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
          stage = "execute_ard", severity = "WARN", input = INPUT_DATA,
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
  user_filtered <- !is.null(output_ids) || !is.null(analysis_ids)
  n_selected    <- 0L   # how many analyses passed the user's id selection
  ard_list <- list()
  for (ana in spec[["analyses"]]) {
    res <- resolve_analysis(ana, spec, subject_key, grouping_map, analysis_to_output)
    analysis_id <- res$analysis_id
    if (is.null(analysis_id)) {
      .diag_gap(
        stage = "execute_ard", severity = "WARN", input = INPUT_ARS,
        problem = "An analysis in the ARS spec has no analysis id and was skipped.",
        why = "Without an id the analysis cannot be tied to an output, so it yields no ARD row.",
        fix = "Regenerate the ARS with spec_to_ars() -- a hand-edited spec may be missing an analysis 'id'."
      )
      next
    }
    out_id <- res$output_id

    # Filter by user-selected output_ids and analysis_ids
    if (user_filtered) {
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
    n_selected <- n_selected + 1L

    method_id    <- res$method_id
    analysis_var <- res$variable
    analysis_ds  <- res$dataset

    if (is.null(analysis_ds) || !nzchar(analysis_ds)) {
      cli::cli_warn("Skipping analysis {.val {analysis_id}}: primary dataset not specified.")
      .diag_gap(
        stage = "execute_ard", severity = "FAIL", input = INPUT_ARS,
        problem = sprintf("Analysis %s does not name a source dataset and was skipped.", analysis_id),
        why = "Without a dataset the result cannot be computed, so this output stays empty.",
        fix = "Check the ADaM spec annotation for this TLF names a dataset, then regenerate the ARS.",
        location = analysis_id
      )
      next
    }
    if (is.null(analysis_var) || !nzchar(analysis_var)) {
      cli::cli_warn("Skipping analysis {.val {analysis_id}}: analysis variable not specified.")
      .diag_gap(
        stage = "execute_ard", severity = "FAIL", input = INPUT_ARS,
        problem = sprintf("Analysis %s does not name an analysis variable and was skipped.", analysis_id),
        why = "Without a variable the result cannot be computed, so this output stays empty.",
        fix = "Check the shell/spec annotation for this row names a variable, then regenerate the ARS.",
        location = analysis_id
      )
      next
    }

    df_base <- get_df(analysis_ds)
    if (is.null(df_base)) {
      cli::cli_warn("Skipping analysis {.val {analysis_id}}: dataset {.val {analysis_ds}} not loaded.")
      next
    }

    pop_where    <- res$pop_where
    subset_where <- res$subset_where

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
        stage = "execute_ard", severity = "FAIL", input = INPUT_DATA,
        problem = sprintf("Variable %s not found in dataset %s", analysis_var, analysis_ds),
        location = analysis_id,
        action = paste0("Analysis skipped -- the shell references ", analysis_var,
                        " but the supplied ", analysis_ds,
                        " does not contain it. Provide an ADaM cut that includes this variable.")
      )
      next
    }

    # Resolve groupings (raw names from the shared resolver; cleaned below)
    grouping_vars <- res$by

    grouping_vars <- unname(sapply(grouping_vars, clean_var_name, df_names = names(df_filtered)))
    dropped_groupings <- grouping_vars[!grouping_vars %in% names(df_filtered)]
    if (length(dropped_groupings) > 0) {
      diag_add(
        stage = "execute_ard", severity = "WARN", input = INPUT_DATA,
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

    # Resolve the method to a cards idiom. Unknown methods fall back to a
    # continuous/categorical summary chosen by the data type, recorded in
    # method_actual. eff_method_id drives the emitter; the legacy registry is
    # only consulted when legacy = TRUE.
    method_actual <- method_id
    eff_method_id <- method_id
    ## A {cardx}-backed method computes via its emitted block when {cardx} is
    ## installed; otherwise it degrades to a reserved stub. A method with no
    ## executor at all always reserves a stub (ADR 0002). A stub skips fallback
    ## coercion and the all-missing data check -- it needs no tabulable values.
    is_cardx_exec <- method_id %in% .CARDX_METHODS &&
      requireNamespace("cardx", quietly = TRUE)
    is_stub <- (method_id %in% names(.UNEXECUTABLE_METHODS)) && !is_cardx_exec
    if (!is_stub && !is_cardx_exec && is.null(.ARD_EXECUTORS[[method_id]])) {
      is_num <- is.numeric(df_filtered[[analysis_var]])
      method_actual <- if (is_num) "FALLBACK_CONTINUOUS" else "FALLBACK_CATEGORICAL"
      eff_method_id <- if (is_num) "MTH_SUMMARY_STATISTICS_CONTINUOUS" else
        "MTH_COUNT_AND_PERCENTAGE"
      cli::cli_warn("Method {.val {method_id}} for analysis {.val {analysis_id}} not natively supported. Using fallback summarizer.")
      diag_add(
        stage = "execute_ard", severity = "WARN", input = INPUT_ARS,
        problem = sprintf("Method %s not natively supported by the executor", method_id),
        location = analysis_id,
        action = sprintf("Generic %s summary used instead (method_actual = %s) -- verify the statistics match the shell",
                         if (is_num) "continuous" else "categorical", method_actual)
      )
    }

    if (!subject_key %in% names(df_filtered) &&
        method_id %in% c("MTH_AE_FREQUENCY_COUNT", "MTH_SUBJECT_COUNT")) {
      cli::cli_warn("Skipping analysis {.val {analysis_id}}: subject key {.val {subject_key}} not in dataset {.val {analysis_ds}}.")
      diag_add(
        stage = "execute_ard", severity = "FAIL", input = INPUT_DATA,
        problem = sprintf("Subject key %s required by %s but not present in dataset %s",
                          subject_key, method_id, analysis_ds),
        location = analysis_id,
        action = "Analysis skipped -- pass subject_key= to ars_to_ard() if this study uses a different identifier"
      )
      next
    }

    ## A study variable that exists but is entirely missing in this data cut
    ## (e.g. a special-interest AE flag with no events) has nothing to
    ## tabulate. {cards} errors hard on an all-NA non-factor column, so detect
    ## this first and skip with an explanatory WARN rather than a cryptic
    ## calculation failure. The output stays empty until a populated cut is
    ## supplied.
    is_continuous_method <- method_id %in% c("MTH_SUMMARY_STATISTICS_CONTINUOUS") ||
      identical(method_actual, "FALLBACK_CONTINUOUS")
    col_vals      <- df_filtered[[analysis_var]]
    all_missing   <- if (is_continuous_method) {
      all(is.na(suppressWarnings(as.numeric(col_vals))))
    } else {
      !is.factor(col_vals) && all(is.na(col_vals))
    }
    if (!is_stub && analysis_var %in% names(df_filtered) && all_missing) {
      reason <- if (is_continuous_method)
        "has no numeric values to summarise" else "is all-missing"
      cli::cli_warn("Skipping analysis {.val {analysis_id}}: variable {.val {analysis_var}} {reason} in this data cut.")
      diag_add(
        stage = "execute_ard", severity = "WARN", input = INPUT_DATA,
        problem = sprintf("Variable %s %s in dataset %s", analysis_var, reason, analysis_ds),
        location = analysis_id,
        action = paste0("Nothing to tabulate in this cut -- supply an ADaM ",
                        "dataset where ", analysis_var, " is populated to render this row.")
      )
      next
    }

    ard <- if (is_stub) {
      ## Reserve keyed manual_pending rows; no calculation attempted.
      .stub_ard_for_method(res, method_id, df_filtered, by_arg, analysis_var)
    } else tryCatch({
      if (legacy) {
        ## Retired registry path -- kept only for the engine-equivalence test.
        executor <- .ARD_EXECUTORS[[method_id]]
        if (is.null(executor)) {
          executor <- if (identical(eff_method_id,
                                    "MTH_SUMMARY_STATISTICS_CONTINUOUS")) {
            function(df, var, by, denom, subject_key)
              cards::ard_continuous(data = df, variables = all_of(var),
                                    by = all_of(by))
          } else {
            function(df, var, by, denom, subject_key)
              cards::ard_categorical(data = df, variables = all_of(var),
                                     by = all_of(by), denominator = denom)
          }
        }
        a <- executor(df_filtered, analysis_var, by_arg, df_population,
                      subject_key)
        if (isTRUE(res$include_total) && !is.null(by_arg)) {
          a <- cards::bind_ard(a, executor(df_filtered, analysis_var, NULL,
                                           df_population, subject_key))
        }
        a
      } else {
        ## Default: execute by sourcing the emitted cards block, so the ARD is
        ## computed by the same code arsbridge ships as the deliverable. Feed
        ## the resolver the validated variable/grouping names and the
        ## data-driven fallback method so emitted == executed.
        res$variable      <- analysis_var
        res$by            <- by_arg
        res$method_id     <- eff_method_id
        res$include_total <- isTRUE(res$include_total)
        .run_emitted_block(res, adam_dir)
      }
    }, error = function(e) {
      cli::cli_warn("Analysis {.val {analysis_id}} failed during {.pkg cards} calculation: {e$message}")
      diag_add(
        stage = "execute_ard", severity = "FAIL", input = INPUT_DATA,
        problem = paste0("cards calculation error: ", conditionMessage(e)),
        location = analysis_id,
        action = "Analysis skipped -- no results in ARD"
      )
      NULL
    })

    if (!is.null(ard)) {
      # Add traceability metadata. analysis_descr carries the analysis's
      # human label (e.g. "EASI75 at Week 16") so the rendering layer can
      # disambiguate rows when several analyses summarise the same variable
      # under different data subsets within one output.
      analysis_descr <- res$description
      ard[["analysis_id"]]     <- analysis_id
      ard[["analysis_descr"]]  <- analysis_descr
      ard[["method_id"]]       <- method_id
      ard[["output_id"]]       <- out_id %||% NA_character_
      ard[["method_intended"]] <- method_id
      ard[["method_actual"]]   <- method_actual

      ## Provenance (ADR 0002). A computed row self-describes as {cards} output;
      ## a stub row (declared-but-unexecutable method) is flagged
      ## manual_pending with no value source, so a later validated manual fill
      ## can be told apart from engine output and an auditor can trace where a
      ## value came from. derived_dt is stamped once after assembly (below) for
      ## computed rows only; a manual_pending row stays NA until it is filled.
      ard[["result_status"]]  <- if (is_stub) "manual_pending" else "computed"
      ard[["value_source"]]   <- if (is_stub) NA_character_ else
        if (is_cardx_exec) "cardx" else "cards"
      ard[["derivation_ref"]] <- if (is_stub) NA_character_ else
        paste0("arsbridge:emitted:", analysis_id)
      ard[["derived_by"]]     <- if (is_stub) NA_character_ else "arsbridge"
      ard[["derived_dt"]]     <- NA_character_

      if (is_stub) {
        diag_add(
          stage = "execute_ard", severity = "WARN", input = INPUT_ARS,
          problem = sprintf(
            "Method %s has no executor; reserved %d manual_pending cell(s)",
            method_id, nrow(ard)),
          location = analysis_id,
          action = paste0("Compute these cells with a validated analysis ",
                          "script and fill the reserved ARD rows -- see ",
                          "ars_manual_worklist()"))
      }

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

  ## An explicit id selection that matched nothing is almost always a typo --
  ## tell the user plainly and list the ids that ARE available, instead of
  ## silently handing back an empty/NULL ARD.
  if (user_filtered && n_selected == 0L) {
    valid_ids <- unique(Filter(nzchar, vapply(
      spec[["outputs"]], function(o) as_scalar_char(o[["id"]]) %||% "", character(1)
    )))
    requested <- paste(c(output_ids, analysis_ids), collapse = ", ")
    .diag_gap(
      stage = "execute_ard", severity = "FAIL", input = INPUT_ARS,
      problem = sprintf("None of the requested ids matched any analysis: %s.", requested),
      why = "There was nothing to compute, so no ARD was produced.",
      fix = sprintf("Use one of the available output ids: %s.",
                    paste(valid_ids, collapse = ", "))
    )
    cli::cli_abort(c(
      "x" = "None of the requested ids matched any analysis in the {INPUT_ARS}: {.val {c(output_ids, analysis_ids)}}.",
      "i" = "Available output ids: {.val {valid_ids}}."
    ))
  }

  if (length(ard_list) == 0) {
    return(NULL)
  }

  # Combine all analyses
  final_ard <- cards::bind_ard(!!!ard_list)

  ## Stamp the run timestamp once (ADR 0002), not inside the per-analysis loop:
  ## keeps resolve/emit pure and gives every row of one run an identical value.
  ## ISO-8601 character (not POSIXct) so a stored/round-tripped ARD is
  ## timezone-stable. The arsbridge.derived_dt option lets tests pin it. Only
  ## computed rows are stamped; a manual_pending stub has no value yet, so its
  ## derived_dt stays NA until a manual fill sets it.
  ts <- getOption("arsbridge.derived_dt",
                  format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))
  computed <- !is.na(final_ard[["result_status"]]) &
    final_ard[["result_status"]] == "computed"
  final_ard[["derived_dt"]][computed] <- ts
  return(final_ard)
}

#' Manual-derivation worklist from an ARD
#'
#' Lists every reserved `manual_pending` cell in an ARD produced by
#' [ars_to_ard()] -- statistics arsbridge could not compute (a
#' declared-but-unexecutable method, e.g. a Cochran-Mantel-Haenszel p-value)
#' and reserved as keyed stub rows. This is the analyst's checklist: each row
#' must be computed with a validated analysis script and written back into the
#' ARD (set `stat`, `result_status = "manual_filled"`, `value_source`,
#' `derivation_ref`) before the table is final. See `vignette("getting-started")`
#' and the ADRs under `docs/adr/` for the round-trip.
#'
#' @param ard An ARD data frame of class `"card"` from [ars_to_ard()].
#' @return A data frame with one row per pending cell: `output_id`,
#'   `analysis_id`, `method_id`, `group1`, `group1_level`, `variable`,
#'   `stat_name`. Zero rows (with those columns) when nothing is pending.
#' @export
#' @examples
#' \dontrun{
#'   ard <- ars_to_ard("outputs/reporting_event.json", "inputs/ADaM")
#'   ars_manual_worklist(ard)
#' }
ars_manual_worklist <- function(ard) {
  cols <- c("output_id", "analysis_id", "method_id", "group1", "group1_level",
            "variable", "stat_name")
  empty <- stats::setNames(
    as.data.frame(rep(list(character(0)), length(cols)),
                  stringsAsFactors = FALSE), cols)
  if (is.null(ard) || !"result_status" %in% names(ard)) return(empty)
  pending <- ard[!is.na(ard[["result_status"]]) &
                   ard[["result_status"]] == "manual_pending", , drop = FALSE]
  if (nrow(pending) == 0) return(empty)
  cols_data <- lapply(cols, function(cn) {
    if (!cn %in% names(pending)) return(rep(NA_character_, nrow(pending)))
    col <- pending[[cn]]
    if (is.list(col)) vapply(col, function(x)
      if (length(x)) as.character(x[[1]]) else NA_character_, character(1))
    else as.character(col)
  })
  names(cols_data) <- cols
  flat <- as.data.frame(cols_data, stringsAsFactors = FALSE)
  rownames(flat) <- NULL
  flat
}

#' Validate manually-filled ARD cells
#'
#' Checks the manual fills in an ARD (ADR 0002 phase 5). A cell whose
#' `result_status` was set to `"manual_filled"` must carry both a value
#' (`stat`) and a `derivation_ref` -- the path/id of the validated program that
#' produced it. A manual value with no derivation reference is untraceable and
#' must never ship; [ars_render_all()] surfaces any offending row as a blocker
#' diagnostic before rendering. Run it yourself on a filled ARD to clear the
#' worklist.
#'
#' @param ard An ARD data frame (class `"card"`), typically one whose
#'   `manual_pending` cells (see [ars_manual_worklist()]) have been filled.
#' @return A data frame, one row per offending cell: `output_id`,
#'   `analysis_id`, `method_id`, `stat_name`, and `problem`. Zero rows (with
#'   those columns) when every manual fill is traceable.
#' @seealso [ars_manual_worklist()]
#' @export
#' @examples
#' \dontrun{
#'   bad <- ars_validate_manual_fills(filled_ard)
#'   if (nrow(bad)) stop("untraceable manual values present")
#' }
ars_validate_manual_fills <- function(ard) {
  cols  <- c("output_id", "analysis_id", "method_id", "stat_name", "problem")
  empty <- stats::setNames(
    as.data.frame(rep(list(character(0)), length(cols)),
                  stringsAsFactors = FALSE), cols)
  if (is.null(ard) || !"result_status" %in% names(ard)) return(empty)

  chr <- function(col) if (is.list(col)) vapply(col, function(x)
    if (length(x)) as.character(x[[1]]) else NA_character_, character(1)) else
      as.character(col)

  status <- chr(ard[["result_status"]])
  filled <- !is.na(status) & status == "manual_filled"
  if (!any(filled)) return(empty)

  dref <- if ("derivation_ref" %in% names(ard)) chr(ard[["derivation_ref"]]) else
    rep(NA_character_, length(status))
  sval <- if ("stat" %in% names(ard)) {
    vapply(ard[["stat"]], function(x)
      if (length(x)) suppressWarnings(as.numeric(x[[1]])) else NA_real_,
      numeric(1))
  } else rep(NA_real_, length(status))

  no_ref   <- filled & (is.na(dref) | !nzchar(trimws(dref)))
  no_value <- filled & is.na(sval)
  bad      <- no_ref | no_value
  if (!any(bad)) return(empty)

  problem <- ifelse(no_ref[bad],
                    "manual_filled without derivation_ref (untraceable value)",
                    "manual_filled but value (stat) is still NA")
  rows <- which(bad)
  get  <- function(cn) if (cn %in% names(ard)) chr(ard[[cn]])[rows] else
    rep(NA_character_, length(rows))
  out <- data.frame(
    output_id   = get("output_id"),
    analysis_id = get("analysis_id"),
    method_id   = get("method_id"),
    stat_name   = get("stat_name"),
    problem     = problem,
    stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out
}
