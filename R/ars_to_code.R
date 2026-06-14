## arsbridge -- ars_to_code.R
## ---------------------------------------------------------------------------
## Deterministic emitter: turns each ARS output (TLF) into a self-contained,
## pharmaverse-style {cards} R script. The emitted script is BOTH the
## human-readable deliverable AND the execution engine -- ars_to_ard() sources
## it to build the ARD (Plan B). The script contains NO arsbridge symbols: only
## library(cards) / library(dplyr) / library(haven) and base R.
##
## Code is generated from resolve_analysis() args (R/resolve_analysis.R) and the
## WhereClause -> predicate strings of where_to_filter_expr()
## (R/utils_where_clause.R), so what is emitted is exactly what would execute.
## All helpers internal (@noRd); no new user-facing functions.

## ---- small string helpers -------------------------------------------------

## Strip a leading DATASET. qualifier from a variable reference (mirrors the
## prefix-stripping half of clean_var_name() in ars_to_ard.R).
#' @noRd
.clean_emit_name <- function(v) {
  if (is.null(v) || !length(v)) return(v)
  vapply(v, function(x) {
    if (is.null(x) || !nzchar(x)) return(x)
    if (grepl(".", x, fixed = TRUE)) {
      parts <- strsplit(x, ".", fixed = TRUE)[[1]]
      return(parts[length(parts)])
    }
    x
  }, character(1), USE.NAMES = FALSE)
}

## Backtick a name when it is not a syntactic R name (spaces, symbols, ...).
#' @noRd
.bt <- function(x) if (identical(make.names(x), x)) x else paste0("`", x, "`")

## R object name for a block, derived from the analysis id.
#' @noRd
.blk_name <- function(analysis_id) paste0("blk_", make.names(analysis_id))

## Robust inline loader: case-insensitive filename match (ADaM cuts ship as
## ADSL.xpt / adsl.csv / etc., and Linux file systems are case-sensitive), with
## an .xpt-or-.csv reader. Pure base/haven, self-contained per dataset.
#' @noRd
.loader_line <- function(ds) {
  sprintf(paste0(
    "%s <- local({\n",
    "  f <- list.files(adam_dir, pattern = \"(?i)^%s\\\\.(xpt|csv)$\",\n",
    "                  full.names = TRUE)[1]\n",
    "  if (is.na(f)) stop(\"%s not found in \", adam_dir)\n",
    "  if (grepl(\"(?i)\\\\.xpt$\", f)) {\n",
    "    haven::read_xpt(f)\n",
    "  } else {\n",
    "    utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)\n",
    "  }\n",
    "})"),
    ds, tolower(ds), ds)
}

## ---- data / denominator expressions ---------------------------------------

## Apply ONE WhereClause to a dataset expression, mirroring
## apply_where_clause() in ars_to_ard.R:
##  - no referenced datasets -> no filter
##  - all refs == target     -> direct dplyr::filter(<predicate>)
##  - single foreign ref      -> restrict to subjects passing the predicate in
##                               that reference dataset (cross-dataset join)
#' @noRd
.apply_where_expr <- function(expr, ds, where, subject_key) {
  if (is.null(where)) return(expr)
  refs <- .where_datasets(where)
  if (length(refs) == 0) return(expr)
  pred <- where_to_filter_expr(where)
  if (identical(pred, "TRUE")) return(expr)

  if (all(toupper(refs) == toupper(ds))) {
    return(paste0(expr, " |>\n    dplyr::filter(", pred, ")"))
  }
  ## Cross-dataset: keep subjects who satisfy the clause in the foreign data.
  ref <- refs[1]
  sk  <- subject_key
  paste0(
    expr, " |>\n    dplyr::filter(", sk, " %in% (",
    ref, " |> dplyr::filter(", pred, ") |> dplyr::pull(", sk, ")))"
  )
}

## The analysis data frame expression: dataset, pop filter, subset filter.
#' @noRd
.data_expr <- function(res) {
  e <- res$dataset
  e <- .apply_where_expr(e, res$dataset, res$pop_where, res$subject_key)
  e <- .apply_where_expr(e, res$dataset, res$subset_where, res$subject_key)
  e
}

## The denominator (population) frame expression -- always ADSL-based, mirroring
## df_population = apply_where_clause("ADSL", pop_where) in ars_to_ard.R.
#' @noRd
.denom_expr <- function(res) {
  .apply_where_expr("ADSL", "ADSL", res$pop_where, res$subject_key)
}

## ---- per-method block emission --------------------------------------------

## Human-readable method label (keeps internal method ids out of the
## deliverable -- the emitted code must read as plain pharmaverse cards).
#' @noRd
.method_label <- function(method_id) {
  switch(method_id %||% "",
    MTH_SUMMARY_STATISTICS_CONTINUOUS = "Summary statistics",
    MTH_COUNT_AND_PERCENTAGE          = "Count (%)",
    MTH_AE_FREQUENCY_COUNT            = "AE frequency, n (%)",
    MTH_SUBJECT_COUNT                 = "Subject count",
    "Analysis")
}

## Comment header for a block.
#' @noRd
.block_comment <- function(res) {
  lab  <- res$label %||% res$description %||% res$analysis_id
  head <- sprintf("# %s", lab)
  if (!is.null(res$description) && !identical(res$description, lab)) {
    head <- sprintf("# %s -- %s", lab, res$description)
  }
  lines <- head
  if (!is.null(res$sap_description) && nzchar(res$sap_description)) {
    lines <- c(lines, sprintf("# SAP: %s", res$sap_description))
  }
  lines <- c(lines, sprintf("# %s  (analysis %s)",
                            .method_label(res$method_id), res$analysis_id))
  paste(lines, collapse = "\n")
}

## TRUE when the analysis variable is itself the single flag used as its data
## subset (annotation purely XXFL='Y') -- the disposition / bare-flag case that
## must render as a distinct-subject count labelled by the stub, NOT a
## flag-level breakdown (fixes the RANDFL/Y/100% bug, Plan B 7.3).
#' @noRd
.is_bare_flag <- function(res) {
  sw <- res$subset_where
  if (is.null(sw) || is.null(sw[["condition"]])) return(FALSE)
  svar <- .as_scalar_char(sw[["condition"]][["variable"]])
  var  <- .clean_emit_name(res$variable)
  !is.null(svar) && nzchar(svar) && identical(.clean_emit_name(svar), var)
}

## Emit the cards block(s) for one resolved analysis. Returns a list with
## `code` (character vector of lines) and `objs` (block object names to bind).
#' @noRd
.emit_block <- function(res) {
  var    <- .clean_emit_name(res$variable)
  by     <- .clean_emit_name(res$by)
  sk     <- res$subject_key
  obj    <- .blk_name(res$analysis_id)
  data_e <- .data_expr(res)
  denom  <- .denom_expr(res)
  method <- res$method_id %||% ""
  qvar   <- encodeString(var, quote = "\"")

  ## Leading comma so a missing `by` never leaves a trailing comma before ")".
  by_line <- function(b) if (length(b)) {
    sprintf(",\n  by = all_of(%s)", .r_chr_vec(b))
  } else ""

  ## The cards CALL (no object assignment) for a given `by` vector. Selecting
  ## the idiom by method here -- once -- lets the include_total total pass reuse
  ## the exact same idiom with `by = character(0)`.
  mk_call <- function(b) {
    if (.is_bare_flag(res) &&
        method %in% c("MTH_SUBJECT_COUNT", "MTH_COUNT_AND_PERCENTAGE")) {
      lab <- res$label %||% res$description %||% var
      sprintf(paste0(
        "cards::ard_categorical(\n",
        "  data = %s |>\n    dplyr::distinct(%s, .keep_all = TRUE) |>\n",
        "    dplyr::mutate(%s = %s),\n",
        "  variables = all_of(%s)%s,\n  denominator = %s\n)"),
        data_e, sk, .bt(lab), encodeString(lab, quote = "\""),
        encodeString(lab, quote = "\""), by_line(b), denom)
    } else if (identical(method, "MTH_SUMMARY_STATISTICS_CONTINUOUS")) {
      sprintf(paste0(
        "cards::ard_continuous(\n",
        "  data = %s |>\n    dplyr::mutate(%s = as.numeric(%s)),\n",
        "  variables = all_of(%s)%s\n)"),
        data_e, .bt(var), .bt(var), qvar, by_line(b))
    } else if (identical(method, "MTH_SUBJECT_COUNT")) {
      distinct_e <- sprintf("%s |>\n    dplyr::distinct(%s, .keep_all = TRUE)",
                            data_e, sk)
      if (identical(var, sk) && length(b)) {
        sprintf(paste0("cards::ard_categorical(\n",
                       "  data = %s,\n  variables = all_of(%s)\n)"),
                distinct_e, .r_chr_vec(b))
      } else if (identical(var, sk)) {
        sprintf("cards::ard_total_n(\n  %s\n)", distinct_e)
      } else {
        sprintf(paste0("cards::ard_categorical(\n",
                       "  data = %s,\n  variables = all_of(%s)%s,\n",
                       "  denominator = %s\n)"),
                distinct_e, qvar, by_line(b), denom)
      }
    } else if (identical(method, "MTH_AE_FREQUENCY_COUNT")) {
      sprintf(paste0(
        "cards::ard_categorical(\n",
        "  data = %s |>\n    dplyr::distinct(%s, %s, .keep_all = TRUE),\n",
        "  variables = all_of(%s)%s,\n  denominator = %s\n)"),
        data_e, sk, .bt(var), qvar, by_line(b), denom)
    } else if (identical(method, "MTH_COUNT_AND_PERCENTAGE")) {
      sprintf(paste0(
        "cards::ard_categorical(\n",
        "  data = %s,\n  variables = all_of(%s)%s,\n  denominator = %s\n)"),
        data_e, qvar, by_line(b), denom)
    } else {
      sprintf(paste0(
        "# Fallback: no dedicated idiom for this method; categorical n(%%) used.\n",
        "cards::ard_categorical(\n",
        "  data = %s,\n  variables = all_of(%s)%s,\n  denominator = %s\n)"),
        data_e, qvar, by_line(b), denom)
    }
  }

  comment <- .block_comment(res)
  code <- sprintf("%s\n%s <- %s", comment, obj, mk_call(by))
  objs <- obj

  ## include_total: an extra ungrouped pass (shell carries an overall column).
  if (isTRUE(res$include_total) && length(by)) {
    obj_t <- paste0(obj, "_total")
    code  <- paste0(code, "\n", sprintf("%s <- %s", obj_t, mk_call(character(0))))
    objs  <- c(objs, obj_t)
  }
  list(code = code, objs = objs)
}

## ---- whole-TLF script -----------------------------------------------------

## Datasets a set of resolved analyses touches (analysis ds + where refs),
## always including ADSL (denominator). Upper-cased, unique, ADSL first.
#' @noRd
.tlf_datasets <- function(reslist) {
  ds <- character(0)
  for (res in reslist) {
    ds <- c(ds, res$dataset,
            .where_datasets(res$pop_where),
            .where_datasets(res$subset_where))
  }
  ds <- toupper(unique(ds[!is.na(ds) & nzchar(ds)]))
  ds <- c("ADSL", setdiff(ds, "ADSL"))
  ds[nzchar(ds)]
}

#' Emit one self-contained {cards} script for a single output (TLF).
#'
#' @param output_id The ARS output id to emit.
#' @param spec Parsed ARS spec.
#' @param subject_key Subject identifier (default `"USUBJID"`).
#' @param adam_dir Default ADaM directory baked into the script header (the
#'   reader can edit it; the engine overrides it when sourcing).
#' @param grouping_map,analysis_to_output Optional pre-built lookup maps.
#' @return A single character string: the full script for this TLF.
#' @noRd
.emit_tlf_script <- function(output_id, spec, subject_key = "USUBJID",
                             adam_dir = ".", grouping_map = NULL,
                             analysis_to_output = NULL) {
  if (is.null(grouping_map))       grouping_map       <- .build_grouping_map(spec)
  if (is.null(analysis_to_output)) analysis_to_output <- .build_analysis_to_output(spec)

  ## Resolve the analyses referenced by this output, in spec (display) order.
  reslist <- list()
  for (ana in spec[["analyses"]]) {
    res <- resolve_analysis(ana, spec, subject_key, grouping_map,
                            analysis_to_output)
    if (identical(res$output_id, output_id) &&
        !identical(res$method_id, "MTH_LISTING")) {
      reslist[[length(reslist) + 1L]] <- res
    }
  }

  ard_obj <- paste0("ard_", make.names(output_id))
  header <- c(
    sprintf("## %s -- generated {cards} analysis script (edit freely).", output_id),
    "## Self-contained: this script computes the ARD for this output.",
    "",
    "library(cards)",
    "library(dplyr)",
    "library(haven)",
    "",
    sprintf("adam_dir <- %s  # <- point this at your ADaM folder",
            encodeString(adam_dir, quote = "\""))
  )

  if (length(reslist) == 0) {
    body <- c("", paste0("# No summarisable analyses for this output."),
              paste0(ard_obj, " <- NULL"))
    return(paste(c(header, body), collapse = "\n"))
  }

  ## Load datasets.
  loaders <- vapply(.tlf_datasets(reslist), .loader_line, character(1))

  ## Population (denominator) frames -- one per distinct ADSL pop predicate.
  denom_exprs <- vapply(reslist, .denom_expr, character(1))
  uniq_pop    <- unique(denom_exprs[denom_exprs != "ADSL"])
  pop_names   <- stats::setNames(paste0("pop_", seq_along(uniq_pop)), uniq_pop)
  pop_defs    <- if (length(uniq_pop)) {
    mapply(function(nm, ex) sprintf("%s <- %s", nm, ex),
           pop_names, names(pop_names), USE.NAMES = FALSE)
  } else character(0)

  ## Emit blocks, swapping inline ADSL-pop expressions for the named frames.
  blocks <- character(0)
  objs   <- character(0)
  for (res in reslist) {
    b  <- .emit_block(res)
    de <- .denom_expr(res)
    if (de != "ADSL" && de %in% names(pop_names)) {
      ## Replace the (multi-line) inline denom with its frame name.
      b$code <- gsub(de, pop_names[[de]], b$code, fixed = TRUE)
    }
    blocks <- c(blocks, "", b$code)
    objs   <- c(objs, b$objs)
  }

  bind_line <- sprintf("\n%s <- cards::bind_ard(\n  %s\n)",
                       ard_obj, paste(objs, collapse = ",\n  "))

  paste(c(header, "", loaders,
          if (length(pop_defs)) c("", pop_defs),
          blocks, bind_line),
        collapse = "\n")
}

## ---- single-analysis execution (the emitter IS the engine) ----------------

## Emit a one-analysis script (header + loaders + pop frame + block + bind).
## ars_to_ard() sources this so it executes through the very idioms it ships --
## the emitted deliverable and the computed ARD are the same code.
#' @noRd
.emit_analysis_script <- function(res, adam_dir = ".") {
  datasets <- .tlf_datasets(list(res))
  header <- c("library(cards)", "library(dplyr)", "library(haven)",
              sprintf("adam_dir <- %s", encodeString(adam_dir, quote = "\"")))
  loaders <- vapply(datasets, .loader_line, character(1))
  de <- .denom_expr(res)
  b  <- .emit_block(res)
  code <- b$code
  pop_def <- NULL
  if (de != "ADSL") {
    pop_def <- sprintf("pop_1 <- %s", de)
    code <- gsub(de, "pop_1", code, fixed = TRUE)
  }
  bind <- sprintf("ard_block <- cards::bind_ard(%s)",
                  paste(b$objs, collapse = ", "))
  paste(c(header, "", loaders, if (!is.null(pop_def)) c("", pop_def), "",
          code, "", bind), collapse = "\n")
}

#' Execute one resolved analysis by sourcing its emitted cards block.
#' Returns the block's `card` ARD, or stops on parse/eval error (caller traps).
#' @noRd
.run_emitted_block <- function(res, adam_dir = ".") {
  script <- .emit_analysis_script(res, adam_dir)
  parsed <- tryCatch(parse(text = script), error = function(e) e)
  if (inherits(parsed, "error")) {
    stop(sprintf("emitted block failed to parse: %s",
                 conditionMessage(parsed)))
  }
  env <- new.env(parent = globalenv())
  eval(parsed, envir = env)
  get("ard_block", envir = env)
}

#' Write per-TLF {cards} deliverable scripts for an ARS spec
#'
#' Deterministically emits one self-contained, pharmaverse-style `{cards}` `.R`
#' per output (TLF). Called by `spec_to_ars()`; not part of the user-facing API.
#'
#' @param spec_or_path Parsed ARS spec, or a path to an ARS JSON file.
#' @param code_dir Directory to write `<output_id>.R` files into (created if
#'   needed).
#' @param output_ids Optional character vector restricting which outputs to
#'   emit (matched case-insensitively against output id and name).
#' @param subject_key Subject identifier (default `"USUBJID"`).
#' @param adam_dir Default ADaM directory baked into each script header.
#' @param log Optional `function(msg)` progress callback.
#' @return A named character vector of written file paths (names = output ids).
#' @noRd
write_tlf_code <- function(spec_or_path, code_dir, output_ids = NULL,
                           subject_key = "USUBJID", adam_dir = ".",
                           log = NULL) {
  spec <- if (is.character(spec_or_path)) {
    jsonlite::fromJSON(spec_or_path, simplifyVector = FALSE)
  } else spec_or_path

  if (!dir.exists(code_dir)) {
    dir.create(code_dir, recursive = TRUE, showWarnings = FALSE)
  }

  grouping_map       <- .build_grouping_map(spec)
  analysis_to_output <- .build_analysis_to_output(spec)

  ## All output ids, optionally filtered (by id or name, case-insensitive).
  all_outputs <- spec[["outputs"]] %||% list()
  keep <- vapply(all_outputs, function(o) {
    oid  <- .as_scalar_char(o[["id"]])
    onm  <- .as_scalar_char(o[["name"]])
    if (is.null(output_ids)) return(!is.null(oid))
    lc <- tolower(output_ids)
    (!is.null(oid) && tolower(oid) %in% lc) ||
      (!is.null(onm) && tolower(onm) %in% lc)
  }, logical(1))
  outs <- all_outputs[keep]

  paths <- character(0)
  for (o in outs) {
    oid <- .as_scalar_char(o[["id"]])
    script <- .emit_tlf_script(oid, spec, subject_key, adam_dir,
                               grouping_map, analysis_to_output)
    fp <- file.path(code_dir, paste0(make.names(oid), ".R"))
    writeLines(script, fp)
    paths[[oid]] <- fp
    if (!is.null(log)) log(sprintf("Emitted cards script: %s", fp))
  }
  paths
}
