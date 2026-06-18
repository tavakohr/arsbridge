## arsbridge -- ars_to_tfrmt.R
## ---------------------------------------------------------------------------
## Closes the last gap in the pipeline:
##   spec_to_ars()  -> ARS JSON
##   ars_to_ard()   -> tidy ARD ({cards} format)
##   ars_to_tfrmt() -> tfrmt spec + formatted GT clinical table   [THIS FILE]
##
## The ARD produced by ars_to_ard() is a {cards} object: list-columns for
## `stat`, `stat_label`, `variable_level`, etc., percentage stats stored as
## proportions in [0, 1]. tfrmt needs a flat numeric `value` column and
## percentages on a 0-100 scale, so the rendering path flattens and rescales
## the ARD via the internal .tfrmt_prep_ard() before handing it to tfrmt.

## ---------------------------------------------------------------------------
## Private helpers -- ARS JSON navigation
## ---------------------------------------------------------------------------

## Scalar-character coercion robust to the list columns produced by
## jsonlite::fromJSON(simplifyVector = FALSE).
.sc <- function(x) {
  if (is.null(x)) return(NA_character_)
  v <- unlist(x)
  if (length(v) == 0) return(NA_character_)
  as.character(v[1])
}

## Locate an output object by id OR name (case-insensitive). Aborts if absent.
find_output <- function(spec, output_id) {
  outputs <- spec[["outputs"]]
  if (is.null(outputs) || length(outputs) == 0) {
    cli::cli_abort("ARS spec contains no {.field outputs}.")
  }
  target <- tolower(trimws(output_id))
  for (o in outputs) {
    oid  <- tolower(trimws(.sc(o[["id"]])))
    onm  <- tolower(trimws(.sc(o[["name"]])))
    if (!is.na(oid) && identical(oid, target)) return(o)
    if (!is.na(onm) && identical(onm, target)) return(o)
  }
  cli::cli_abort(c(
    "Output {.val {output_id}} not found in the ARS spec.",
    "i" = "Available output ids: {.val {vapply(outputs, function(o) .sc(o[['id']]), character(1))}}"
  ))
}

## First display object of an output (ARS: output$displays[[1]]).
.first_display <- function(out_obj) {
  disp <- out_obj[["displays"]]
  if (is.null(disp) || length(disp) == 0) return(NULL)
  disp[[1]]
}

## Title: output$label, then display$displayTitle, then output$name.
extract_title <- function(out_obj) {
  lab <- .sc(out_obj[["label"]])
  if (!is.na(lab) && nzchar(lab)) return(lab)
  d <- .first_display(out_obj)
  if (!is.null(d)) {
    dt <- .sc(d[["displayTitle"]])
    if (!is.na(dt) && nzchar(dt)) return(dt)
  }
  nm <- .sc(out_obj[["name"]])
  if (!is.na(nm) && nzchar(nm)) return(nm)
  character(0)
}

## Footnotes: walk display$displaySections for sectionType == "Footnote",
## collect each subSection$text. Returns a character vector (possibly empty).
extract_footnotes <- function(out_obj) {
  d <- .first_display(out_obj)
  if (is.null(d)) return(character(0))
  secs <- d[["displaySections"]]
  if (is.null(secs) || length(secs) == 0) return(character(0))
  notes <- character(0)
  for (s in secs) {
    if (!identical(tolower(.sc(s[["sectionType"]])), "footnote")) next
    for (ss in s[["subSections"]]) {
      txt <- .sc(ss[["text"]])
      if (!is.na(txt) && nzchar(txt)) notes <- c(notes, txt)
    }
  }
  notes
}

## Names of fixed (dataDriven = FALSE) grouping variables in the spec. These
## are treatment / analysis-set style columns -- the column-variable candidates.
.fixed_grouping_vars <- function(spec) {
  gv <- character(0)
  for (g in spec[["analysisGroupings"]]) {
    dd <- .sc(g[["dataDriven"]])
    is_fixed <- is.na(dd) || tolower(dd) %in% c("false", "0", "no")
    if (!is_fixed) next
    var <- g[["groupingVariable"]]
    if (is.list(var)) var <- var[["variable"]]
    var <- .sc(var)
    if (is.na(var) || !nzchar(var)) var <- .sc(g[["name"]])
    if (!is.na(var) && nzchar(var)) {
      ## ADCM.TRTP -> TRTP : keep the bare variable token cards would use.
      var <- sub("^.*\\.", "", var)
      gv <- c(gv, var)
    }
  }
  unique(gv)
}

## ---------------------------------------------------------------------------
## Private helpers -- ARD flattening / column-role detection
## ---------------------------------------------------------------------------

.flat_chr <- function(col) {
  vapply(col, function(x) {
    if (is.null(x) || length(x) == 0) return(NA_character_)
    as.character(x[[1]])
  }, character(1))
}

.flat_num <- function(col) {
  vapply(col, function(x) {
    if (is.null(x) || length(x) == 0) return(NA_real_)
    suppressWarnings(as.numeric(x[[1]]))
  }, numeric(1))
}

## group_* level columns present in the ARD, ordered by numeric suffix.
.group_level_cols <- function(nms) {
  cand <- grep("^group[0-9]+_level$", nms, value = TRUE)
  cand[order(as.integer(sub("^group([0-9]+)_level$", "\\1", cand)))]
}

## Detect the column variable (treatment groups). Returns one ARD column name.
detect_col_var <- function(ard, spec) {
  nms      <- names(ard)
  lvl_cols <- .group_level_cols(nms)
  if (length(lvl_cols) == 0) {
    cli::cli_abort("ARD has no {.field group*_level} columns to use as the column variable.")
  }
  fixed <- .fixed_grouping_vars(spec)

  ## Prefer a group whose *name* column matches a fixed (treatment) grouping.
  for (lc in lvl_cols) {
    name_col <- sub("_level$", "", lc)              # group1_level -> group1
    if (!name_col %in% nms) next
    gnames <- unique(stats::na.omit(.flat_chr(ard[[name_col]])))
    gnames <- sub("^.*\\.", "", gnames)
    n_lvl  <- length(unique(stats::na.omit(.flat_chr(ard[[lc]]))))
    if (any(gnames %in% fixed) && n_lvl <= 12) return(lc)
  }
  ## Fallback: first group level column with a small number of distinct values.
  for (lc in lvl_cols) {
    n_lvl <- length(unique(stats::na.omit(.flat_chr(ard[[lc]]))))
    if (n_lvl <= 6) return(lc)
  }
  lvl_cols[1]
}

## Synthetic ".arsbridge_row" label used for every table: categorical rows are
## labelled by their category (variable_level), continuous rows by the summary
## statistic line ("Mean (SD)", "Median", "(Min, Max)", ...). This lets one
## tfrmt render mixed categorical + continuous outputs with correct row layout.
.ARS_ROW_LABEL <- ".arsbridge_row"

## Map a continuous stat_name to the row line it belongs to.
.statline_for <- function(stat_name) {
  sn <- tolower(stat_name)
  if (sn %in% c("mean", "sd"))     return("Mean (SD)")
  if (sn == "median")              return("Median")
  if (sn %in% c("min", "max"))     return("(Min, Max)")
  if (sn %in% c("p25", "q1"))      return("(Q1, Q3)")
  if (sn %in% c("p75", "q3"))      return("(Q1, Q3)")
  ## Title-case any other continuous stat as its own line.
  paste0(toupper(substr(stat_name, 1, 1)), substr(stat_name, 2, nchar(stat_name)))
}

## Detect row grouping columns given the chosen col_var. The label is always
## the synthetic .ARS_ROW_LABEL built during data prep.
detect_row_roles <- function(ard, col_var) {
  nms      <- names(ard)
  lvl_cols <- setdiff(.group_level_cols(nms), col_var)

  ## group hierarchy: remaining group level cols, then `variable` (each
  ## analysis variable becomes its own row block / header).
  group_vars <- lvl_cols
  if ("variable" %in% nms) group_vars <- c(group_vars, "variable")

  ## Disambiguate collapsed rows: when several analyses summarise the SAME
  ## variable for the same column under different data subsets (e.g. a change
  ## from baseline at multiple visits/parameters), they share one
  ## (column, variable, stat) identity and tfrmt cannot combine them. If that
  ## happens, promote the analysis descriptor to the outermost row group so
  ## each subset becomes its own labelled block.
  if ("analysis_descr" %in% nms) {
    key_cols <- intersect(c(col_var, "variable", "variable_level",
                            "stat_name", lvl_cols), nms)
    key   <- do.call(paste, c(lapply(key_cols, function(k) .flat_chr(ard[[k]])),
                              sep = ""))
    descr <- .flat_chr(ard[["analysis_descr"]])
    collide <- tapply(descr, key, function(d) length(unique(stats::na.omit(d))) > 1)
    if (any(collide, na.rm = TRUE)) group_vars <- c("analysis_descr", group_vars)
  }
  list(label_var = .ARS_ROW_LABEL, group_vars = group_vars)
}

## Continuous-style methods produce stat lines; everything else is categorical.
.is_continuous_method <- function(method_id) {
  method_id %in% c("MTH_SUMMARY_STATISTICS_CONTINUOUS", "FALLBACK_CONTINUOUS")
}

## ---------------------------------------------------------------------------
## Private helpers -- body plan
## ---------------------------------------------------------------------------

## Map a method_id + the stat_names available for it to tfrmt body-plan
## entries. Categorical entries are scoped to label_val ".default"; continuous
## entries are scoped to the stat-line label they format (so they land on
## separate rows). Returns list(structures = <frmt_structure list>,
## params = <stat_names consumed>).
.method_body_entries <- function(method_id, stat_names) {
  has <- function(...) all(c(...) %in% stat_names)
  fs  <- function(label_val, frmt) tfrmt::frmt_structure(
    group_val = ".default", label_val = label_val, frmt)
  structs <- list()
  params  <- character(0)

  count_like <- c("MTH_COUNT_AND_PERCENTAGE", "MTH_AE_FREQUENCY_COUNT",
                  "FALLBACK_CATEGORICAL")

  if (method_id %in% count_like && has("n", "p")) {
    structs <- c(structs, list(fs(".default", tfrmt::frmt_combine(
      "{n} ({p}%)", n = tfrmt::frmt("xx"), p = tfrmt::frmt("xx.x")))))
    params <- c(params, "n", "p")
  } else if (method_id %in% count_like && has("n")) {
    structs <- c(structs, list(fs(".default", tfrmt::frmt_combine(
      "{n}", n = tfrmt::frmt("xx")))))
    params <- c(params, "n")
  } else if (method_id == "MTH_SUBJECT_COUNT") {
    structs <- c(structs, list(fs(".default", tfrmt::frmt_combine(
      "{n}", n = tfrmt::frmt("xxx")))))
    params <- c(params, "n")
  } else if (.is_continuous_method(method_id)) {
    if (has("mean", "sd")) {
      structs <- c(structs, list(fs("Mean (SD)", tfrmt::frmt_combine(
        "{mean} ({sd})", mean = tfrmt::frmt("xx.x"), sd = tfrmt::frmt("x.xx")))))
      params <- c(params, "mean", "sd")
    }
    if (has("median")) {
      structs <- c(structs, list(fs("Median", tfrmt::frmt_combine(
        "{median}", median = tfrmt::frmt("xx.x")))))
      params <- c(params, "median")
    }
    if (has("min", "max")) {
      structs <- c(structs, list(fs("(Min, Max)", tfrmt::frmt_combine(
        "({min}, {max})", min = tfrmt::frmt("xx.x"), max = tfrmt::frmt("xx.x")))))
      params <- c(params, "min", "max")
    }
  }

  ## Fallback for any method / stat combo not handled above: format every
  ## remaining numeric stat (except the N denominator) on its own line.
  if (length(structs) == 0) {
    leftover <- setdiff(stat_names, "N")
    structs <- lapply(leftover, function(sn) {
      cargs <- list(paste0("{", sn, "}"))
      cargs[[sn]] <- tfrmt::frmt("xx.x")
      fs(.statline_for(sn), do.call(tfrmt::frmt_combine, cargs))
    })
    params <- leftover
  }

  list(structures = structs, params = unique(params))
}

## Build the full body_plan + the set of params it consumes.
build_body_plan <- function(ard_out) {
  method_col <- .flat_chr(ard_out[["method_id"]])
  stat_col   <- .flat_chr(ard_out[["stat_name"]])
  methods    <- unique(stats::na.omit(method_col))

  all_structs <- list()
  all_params  <- character(0)
  for (m in methods) {
    sn  <- unique(stats::na.omit(stat_col[method_col == m]))
    ent <- .method_body_entries(m, sn)
    all_structs <- c(all_structs, ent$structures)
    all_params  <- c(all_params, ent$params)
  }

  list(
    body_plan = do.call(tfrmt::body_plan, all_structs),
    params    = unique(all_params)
  )
}

## ---------------------------------------------------------------------------
## Private helpers -- column plan + data preparation
## ---------------------------------------------------------------------------

## Ordered character vector of column (treatment) values. Tries the ARS display
## column definitions; falls back to ARD appearance order with a warning.
build_col_levels <- function(out_obj, ard_out, col_var) {
  ard_levels <- unique(stats::na.omit(.flat_chr(ard_out[[col_var]])))

  ## ARS v1.0 outputs in this pipeline carry no explicit column definitions
  ## (displaySections hold only footnotes), so we order columns from the ARD.
  d <- .first_display(out_obj)
  col_defs <- if (!is.null(d)) d[["columns"]] else NULL
  if (is.null(col_defs) || length(col_defs) == 0) {
    cli::cli_warn(c(
      "No column definitions in the ARS display for this output.",
      "i" = "Column order taken from the ARD values of {.field {col_var}}."
    ), .frequency = "once", .frequency_id = "arsbridge_no_col_defs")
    return(ard_levels)
  }

  labels <- vapply(col_defs, function(c) .sc(c[["label"]]), character(1))
  labels <- labels[!is.na(labels) & nzchar(labels)]
  norm   <- function(x) tolower(gsub("\\s+", "", x))
  ordered <- character(0)
  for (lb in labels) {
    ## Tolerant match: a shell column header carries extra text the ARD arm
    ## value does not (e.g. "UPADALIMIB 15 mg\n(N=200) n (%)" vs the level
    ## "UPADALIMIB 15 mg"), so treat the arm value as a substring of the header.
    nlb <- norm(lb)
    hit <- ard_levels[!ard_levels %in% ordered &
                        vapply(ard_levels,
                               function(a) grepl(norm(a), nlb, fixed = TRUE),
                               logical(1))]
    if (length(hit)) ordered <- c(ordered, hit[1])
  }
  ordered <- c(ordered, setdiff(ard_levels, ordered))
  if (length(ordered) == 0) ordered <- ard_levels
  ordered
}

## Flatten + rescale the ARD for one output into a tfrmt-ready data frame.
## Builds the synthetic .ARS_ROW_LABEL column used as the tfrmt label.
.tfrmt_prep_ard <- function(ard, output_id, col_var, label_var, group_vars,
                            keep_params) {
  oid <- .flat_chr(ard[["output_id"]])
  ard_out <- ard[!is.na(oid) & oid == output_id, , drop = FALSE]
  if (nrow(ard_out) == 0) {
    cli::cli_abort("ARD has zero rows for output {.val {output_id}}.")
  }

  carry <- unique(c(group_vars, col_var))
  carry <- carry[carry %in% names(ard_out)]
  out   <- as.data.frame(lapply(ard_out[carry], .flat_chr), stringsAsFactors = FALSE)
  out[["stat_name"]] <- .flat_chr(ard_out[["stat_name"]])
  out[["stat"]]      <- .flat_num(ard_out[["stat"]])
  method   <- .flat_chr(ard_out[["method_id"]])

  if (identical(label_var, .ARS_ROW_LABEL)) {
    ## Synthetic row label: stat line for continuous rows, category otherwise.
    var_lvl <- if ("variable_level" %in% names(ard_out)) {
      .flat_chr(ard_out[["variable_level"]])
    } else rep(NA_character_, nrow(ard_out))
    row_lbl <- ifelse(.is_continuous_method(method),
                      vapply(out[["stat_name"]], .statline_for, character(1)),
                      var_lvl)
  } else {
    ## User-supplied label column carried verbatim.
    row_lbl <- .flat_chr(ard_out[[label_var]])
  }
  row_lbl[is.na(row_lbl) | !nzchar(row_lbl)] <- "Total"
  out[[label_var]] <- row_lbl

  ## Treatment values: relabel an ungrouped (Total) pass -> "Total".
  cv <- out[[col_var]]
  cv[is.na(cv) | !nzchar(cv)] <- "Total"
  out[[col_var]] <- cv

  ## Rescale proportion stats (cards stores p in [0, 1]) to percentages.
  pct_rows <- out[["stat_name"]] %in% c("p", "pct", "percent")
  if (any(pct_rows)) {
    pv <- out[["stat"]][pct_rows]
    if (all(is.na(pv) | (pv >= 0 & pv <= 1.0000001))) {
      out[["stat"]][pct_rows] <- pv * 100
    }
  }

  ## Keep only stat_names the body plan formats (drops the N denominator etc.).
  out[out[["stat_name"]] %in% keep_params, , drop = FALSE]
}

## ---------------------------------------------------------------------------
## Exported: ars_to_tfrmt()
## ---------------------------------------------------------------------------

#' Build a tfrmt specification for one ARS output
#'
#' Translates one output of a CDISC ARS v1.0 reporting event, together with the
#' tidy ARD produced by [ars_to_ard()], into a [tfrmt::tfrmt()] specification.
#' The returned object can be rendered with [tfrmt::print_to_gt()] or
#' [tfrmt::print_mock_gt()] -- but the ARD must be flattened and rescaled first
#' (see [ars_render_tlf()], which does this for you).
#'
#' Column roles are auto-detected from the `{cards}` ARD column names unless
#' supplied explicitly:
#' * `col_var` -- the `group*_level` column whose grouping variable is a fixed
#'   (treatment) grouping in the ARS spec.
#' * `label_var` -- `variable_level` when it carries text, else `variable`.
#' * `group_vars` -- remaining `group*_level` columns plus `variable` when more
#'   than one analysis variable is present.
#'
#' @param ars_path Path to the CDISC ARS v1.0 JSON (output of [spec_to_ars()]).
#' @param ard Tidy ARD data frame (output of [ars_to_ard()]).
#' @param output_id `character(1)` ARS output id or name to render
#'   (case-insensitive).
#' @param col_var,label_var,group_vars Optional overrides for the auto-detected
#'   column roles described above.
#'
#' @return A [tfrmt::tfrmt()] object. Extracted footnotes are attached as the
#'   attribute `"arsbridge_footnotes"`; [ars_render_tlf()] applies them as GT
#'   source notes.
#' @seealso [ars_render_tlf()], [ars_to_tfrmt_list()]
#' @export
#' @examples
#' \dontrun{
#'   ars  <- arsbridge_example("reporting_event.json")
#'   ard  <- ars_to_ard(ars, "inputs/ADaM")
#'   spec <- ars_to_tfrmt(ars, ard, "T_14_1_1")
#' }
ars_to_tfrmt <- function(ars_path, ard, output_id,
                         col_var = NULL, label_var = NULL, group_vars = NULL) {
  spec <- .read_json(ars_path)
  if (is.null(ard)) {
    cli::cli_abort(c(
      "x" = "No ARD was supplied ({.arg ard} is NULL) -- there are no results to render.",
      "i" = "To fix: run {.fn ars_to_ard} first and pass its result as {.arg ard}."
    ))
  }
  if (is.null(output_id) || !is.character(output_id) || length(output_id) != 1 ||
      !nzchar(output_id)) {
    cli::cli_abort(c(
      "x" = "No {.arg output_id} was supplied.",
      "i" = "To fix: pass the id of the output to render, e.g. {.val T_14_1_1}."
    ))
  }
  out_obj <- find_output(spec, output_id)
  if (is.null(out_obj)) {
    valid <- vapply(spec[["outputs"]], function(o) .sc(o[["id"]]) %||% "",
                    character(1))
    cli::cli_abort(c(
      "x" = "Output id {.val {output_id}} is not in the {INPUT_ARS}.",
      "i" = "Available output ids: {.val {valid[nzchar(valid)]}}."
    ))
  }
  ## Resolve the canonical output id (the ARD keys on id, not name).
  output_id <- .sc(out_obj[["id"]])

  oid     <- .flat_chr(ard[["output_id"]])
  ard_out <- ard[!is.na(oid) & oid == output_id, , drop = FALSE]
  if (nrow(ard_out) == 0) {
    cli::cli_abort("ARD has zero rows for output {.val {output_id}}.")
  }

  if (is.null(col_var))   col_var   <- detect_col_var(ard_out, spec)
  roles <- detect_row_roles(ard_out, col_var)
  if (is.null(label_var))  label_var  <- roles[["label_var"]]
  if (is.null(group_vars)) group_vars <- roles[["group_vars"]]
  group_vars <- setdiff(group_vars, c(col_var, label_var))

  bp        <- build_body_plan(ard_out)
  col_lvls  <- build_col_levels(out_obj, ard_out, col_var)
  title     <- extract_title(out_obj)
  footnotes <- extract_footnotes(out_obj)

  ## Assemble the tfrmt call. group/column take quosure lists (vars()); label/
  ## param/value take single quosures -- build them programmatically.
  group_q  <- do.call(dplyr::vars, lapply(group_vars, as.name))
  column_q <- do.call(dplyr::vars, lapply(col_var, as.name))

  tf_args <- list(
    group     = group_q,
    label     = rlang::new_quosure(as.name(label_var)),
    column    = column_q,
    param     = rlang::new_quosure(as.name("stat_name")),
    value     = rlang::new_quosure(as.name("stat")),
    body_plan = bp[["body_plan"]]
  )
  if (length(title) == 1 && nzchar(title)) tf_args[["title"]] <- title

  ## Lock column order when we have it.
  if (length(col_lvls) > 0) {
    cp_args <- c(lapply(col_lvls, as.name), list(.drop = FALSE))
    tf_args[["col_plan"]] <- tryCatch(
      do.call(tfrmt::col_plan, cp_args),
      error = function(e) NULL
    )
    if (is.null(tf_args[["col_plan"]])) tf_args[["col_plan"]] <- NULL
  }

  tf <- do.call(tfrmt::tfrmt, tf_args)

  attr(tf, "arsbridge_footnotes")  <- footnotes
  attr(tf, "arsbridge_keep_params") <- bp[["params"]]
  attr(tf, "arsbridge_col_var")     <- col_var
  attr(tf, "arsbridge_label_var")   <- label_var
  attr(tf, "arsbridge_group_vars")  <- group_vars
  tf
}

## ---------------------------------------------------------------------------
## Exported: ars_render_tlf()
## ---------------------------------------------------------------------------

#' Render an ARS output to a formatted clinical table
#'
#' Convenience wrapper: builds the [tfrmt::tfrmt()] spec with [ars_to_tfrmt()],
#' flattens and rescales the ARD, renders to a GT table, and attaches any ARS
#' footnotes as GT source notes.
#'
#' @inheritParams ars_to_tfrmt
#' @param format Output format. `"gt"` (default) returns a `gt_tbl`; `"docx"`
#'   and `"rtf"` write a regulatory-style Word / RTF file (via
#'   `{flextable}` + `{officer}`) and return the path invisibly.
#' @param file Output path for `format = "docx"` / `"rtf"`. Defaults to
#'   `<output_id>.<format>` in [tempdir()].
#' @param rtf_path Deprecated alias for `file`.
#' @param ... Passed to [ars_to_tfrmt()] (e.g. `col_var`, `label_var`).
#'
#' @return A `gt_tbl` when `format = "gt"`; otherwise the written file path,
#'   invisibly.
#' @seealso [ars_to_tfrmt()], [ars_render_all()]
#' @export
#' @examples
#' \dontrun{
#'   gt_tbl <- ars_render_tlf(ars_path, ard, "T_14_1_1")
#'   ars_render_tlf(ars_path, ard, "T_14_1_1", format = "docx", file = "t1.docx")
#' }
ars_render_tlf <- function(ars_path, ard, output_id,
                           format = c("gt", "docx", "rtf"),
                           file = NULL, rtf_path = NULL, ...) {
  format  <- match.arg(format)
  tf      <- ars_to_tfrmt(ars_path, ard, output_id, ...)

  col_var     <- attr(tf, "arsbridge_col_var")
  label_var   <- attr(tf, "arsbridge_label_var")
  group_vars  <- attr(tf, "arsbridge_group_vars")
  keep_params <- attr(tf, "arsbridge_keep_params")
  footnotes   <- attr(tf, "arsbridge_footnotes")

  ## Use the canonical output id resolved during ars_to_tfrmt().
  spec    <- .read_json(ars_path)
  out_obj <- find_output(spec, output_id)
  out_id  <- .sc(out_obj[["id"]])

  data_prepped <- .tfrmt_prep_ard(ard, out_id, col_var, label_var,
                                  group_vars, keep_params)
  stopifnot(label_var %in% names(data_prepped))

  gt_tbl <- tfrmt::print_to_gt(tf, .data = data_prepped)
  ## Blank the synthetic row-label column header.
  if (identical(label_var, .ARS_ROW_LABEL) &&
      label_var %in% names(gt_tbl[["_data"]])) {
    gt_tbl <- gt::cols_label(gt_tbl, .list = stats::setNames(list(""), label_var))
  }
  if (length(footnotes) > 0) {
    for (fn in footnotes) {
      gt_tbl <- gt::tab_source_note(gt_tbl, source_note = fn)
    }
  }

  ## Carry the stub identity so the flextable converter can keep the row-label
  ## (and any group) column on the LEFT regardless of tfrmt's column order.
  attr(gt_tbl, "arsbridge_label_var")  <- label_var
  attr(gt_tbl, "arsbridge_group_vars") <- group_vars

  if (format == "gt") return(gt_tbl)

  ## Word / RTF via flextable.
  file <- file %||% rtf_path %||% file.path(tempdir(), paste0(out_id, ".", format))
  ft <- .gt_to_flextable(gt_tbl, .sc(out_obj[["name"]]) %||% out_id,
                         extract_title(out_obj), footnotes)
  .write_flextable(ft, file, format)
}

## ---------------------------------------------------------------------------
## Exported: ars_to_tfrmt_list()
## ---------------------------------------------------------------------------

#' Build tfrmt specs for every renderable ARS output
#'
#' Returns a named list of [tfrmt::tfrmt()] specs, one per output id that is
#' present in both the ARS spec and the ARD. Outputs that fail to build (e.g.
#' listings with no summarised statistics) are skipped with a warning and
#' returned as `NULL`.
#'
#' @inheritParams ars_to_tfrmt
#' @return A named list of [tfrmt::tfrmt()] objects (or `NULL` per skipped
#'   output), keyed by output id.
#' @seealso [ars_to_tfrmt()], [ars_render_tlf()]
#' @export
ars_to_tfrmt_list <- function(ars_path, ard) {
  spec       <- .read_json(ars_path)
  output_ids <- vapply(spec[["outputs"]], function(o) .sc(o[["id"]]), character(1))
  ard_ids    <- unique(stats::na.omit(.flat_chr(ard[["output_id"]])))
  output_ids <- output_ids[output_ids %in% ard_ids]

  result <- lapply(output_ids, function(oid) {
    tryCatch(
      ars_to_tfrmt(ars_path, ard, oid),
      error = function(e) {
        cli::cli_warn("Skipping output {.val {oid}}: {e$message}")
        NULL
      }
    )
  })
  stats::setNames(result, output_ids)
}
