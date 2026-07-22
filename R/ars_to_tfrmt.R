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

## Authored shell layout persisted by build_ars_json (ADR 0003 Layer C).
## Returns a data frame (order, label, indent, analysis_id, kind) or NULL
## when the output carries no layout (older ARS files / listings / figures)
## -- the caller then falls back to the pre-layout rendering path.
.shell_layout <- function(out_obj) {
  sl <- out_obj[["_meta"]][["shell_layout"]]
  if (is.null(sl) || length(sl) == 0) return(NULL)
  df <- data.frame(
    order       = vapply(sl, function(e) as.integer(e[["order"]] %||% NA_integer_), integer(1)),
    label       = vapply(sl, function(e) as.character(e[["label"]] %||% ""), character(1)),
    indent      = vapply(sl, function(e) as.integer(e[["indent"]] %||% 0L), integer(1)),
    analysis_id = vapply(sl, function(e) {
      v <- e[["analysis_id"]]
      if (is.null(v) || length(v) == 0 || is.na(v[[1]])) NA_character_ else as.character(v[[1]])
    }, character(1)),
    kind        = vapply(sl, function(e) as.character(e[["kind"]] %||% "row"), character(1)),
    level       = vapply(sl, function(e) {
      v <- e[["level"]]
      if (is.null(v) || length(v) == 0) NA_character_ else as.character(v[[1]])
    }, character(1)),
    stringsAsFactors = FALSE
  )
  df[order(df$order), , drop = FALSE]
}

## Column names of the layout-driven display frame. Real data columns (not
## synthetic tfrmt magic) so the flextable converter and tests can key on them.
.ARS_SHELL_GRP <- ".arsbridge_shell_grp"
.ARS_SHELL_LBL <- ".arsbridge_shell_lbl"
.ARS_SHELL_ORD <- ".arsbridge_shell_ord"

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

## Loud, unmistakable filler for a reserved manual_pending cell (ADR 0002
## phase 4). Never blank, never "NA", never a number -- a reviewer must not
## read a reserved cell as a value or a zero. U+2021 (double dagger) keyed to a
## table footnote. Defined once; also used by the placeholder enrichment in
## ars_render_docx.R.
.MANUAL_MARKER <- paste0("[", intToUtf8(0x2021), " manual]")

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
## `subject_n` renames the subject-count "n" param (layout path). Without the
## rename, a table mixing MTH_SUBJECT_COUNT ("{n}") and
## MTH_COUNT_AND_PERCENTAGE ("{n} ({p}%)") shares the "n" param across both
## structures, and tfrmt's combine step errors on the n-only cells
## ("Can't combine `stat` <double> and <character>").
.method_body_entries <- function(method_id, stat_names, subject_n = "n") {
  has <- function(...) all(c(...) %in% stat_names)
  fs  <- function(label_val, frmt) tfrmt::frmt_structure(
    group_val = ".default", label_val = label_val, frmt)
  ## Single-param structure bound to a NAMED param. tfrmt warns on a
  ## one-parameter frmt_combine ("Unable to apply frmt_combine due to
  ## uniqueness of column/row identifiers") -- the named plain frmt form
  ## renders identically without the warning.
  fs1 <- function(label_val, param, f) {
    a <- list(group_val = ".default", label_val = label_val, f)
    names(a)[3] <- param
    do.call(tfrmt::frmt_structure, a)
  }
  structs <- list()
  params  <- character(0)

  ## Declared-but-unexecutable method (ADR 0002 phase 4): the stub rows carry
  ## stat = NA. Render each as the loud manual-derivation marker so a reserved
  ## cell can never be read as a value or a zero. frmt(missing=) prints the
  ## marker because a manual_pending stat is always NA.
  if (method_id %in% names(.UNEXECUTABLE_METHODS)) {
    ## NA stat (still pending) -> the loud marker; a filled value (manual_filled,
    ## ADR 0002 phase 5) -> a generic 3-dp number. The analyst can refine the
    ## display per study, but the value is never lost or shown as the marker.
    structs <- lapply(stat_names, function(sn)
      fs(.statline_for(sn), tfrmt::frmt("xx.xxx", missing = .MANUAL_MARKER)))
    return(list(structures = structs, params = unique(stat_names)))
  }

  count_like <- c("MTH_COUNT_AND_PERCENTAGE", "MTH_AE_FREQUENCY_COUNT",
                  "FALLBACK_CATEGORICAL")

  if (method_id %in% count_like && has("n", "p")) {
    structs <- c(structs, list(fs(".default", tfrmt::frmt_combine(
      "{n} ({p}%)", n = tfrmt::frmt("xx"), p = tfrmt::frmt("xx.x")))))
    params <- c(params, "n", "p")
  } else if (method_id %in% count_like && has("n")) {
    structs <- c(structs, list(fs1(".default", "n", tfrmt::frmt("xx"))))
    params <- c(params, "n")
  } else if (method_id == "MTH_SUBJECT_COUNT") {
    structs <- c(structs, list(fs1(".default", subject_n, tfrmt::frmt("xxx"))))
    params <- c(params, subject_n)
  } else if (.is_continuous_method(method_id)) {
    if (has("mean", "sd")) {
      structs <- c(structs, list(fs("Mean (SD)", tfrmt::frmt_combine(
        "{mean} ({sd})", mean = tfrmt::frmt("xx.x"), sd = tfrmt::frmt("x.xx")))))
      params <- c(params, "mean", "sd")
    }
    if (has("median")) {
      structs <- c(structs, list(fs1("Median", "median", tfrmt::frmt("xx.x"))))
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
      fs1(.statline_for(sn), sn, tfrmt::frmt("xx.x"))
    })
    params <- leftover
  }

  list(structures = structs, params = unique(params))
}

## Parameter name of the invisible filler cell behind an authored label-only
## row (section header / spacer) on the layout-driven path.
.ARS_SPACER_PARAM <- ".arsbridge_spacer"

## Layout-path rename of the subject-count "n" param, so its body-plan
## structure can never collide with a count-and-percentage "{n} ({p}%)"
## structure in the same table (see .method_body_entries).
.ARS_SUBJ_N_PARAM <- ".arsbridge_n_subj"

## Build the full body_plan + the set of params it consumes.
## include_spacer = TRUE (layout-driven path) appends a structure that renders
## the spacer param as a blank, so authored label-only rows print empty cells
## rather than "NA".
build_body_plan <- function(ard_out, include_spacer = FALSE) {
  method_col <- .flat_chr(ard_out[["method_id"]])
  stat_col   <- .flat_chr(ard_out[["stat_name"]])
  methods    <- unique(stats::na.omit(method_col))
  subj_n     <- if (isTRUE(include_spacer)) .ARS_SUBJ_N_PARAM else "n"

  all_structs <- list()
  all_params  <- character(0)
  for (m in methods) {
    sn  <- unique(stats::na.omit(stat_col[method_col == m]))
    ent <- .method_body_entries(m, sn, subject_n = subj_n)
    all_structs <- c(all_structs, ent$structures)
    all_params  <- c(all_params, ent$params)
  }

  if (isTRUE(include_spacer)) {
    spacer_args <- list(group_val = ".default", label_val = ".default",
                        tfrmt::frmt("x", missing = " "))
    names(spacer_args)[3] <- .ARS_SPACER_PARAM
    all_structs <- c(all_structs,
                     list(do.call(tfrmt::frmt_structure, spacer_args)))
    all_params  <- c(all_params, .ARS_SPACER_PARAM)
  }

  list(
    body_plan = do.call(tfrmt::body_plan, all_structs),
    params    = unique(all_params)
  )
}

## Per-method formatted-parameter sets (the same decisions .method_body_entries
## makes for the body plan). The layout prep uses this so e.g. a subject-count
## analysis keeps only its "n" while a count-and-percentage analysis in the
## same table keeps "n" and "p".
.method_params_map <- function(ard_out, subject_n = "n") {
  method_col <- .flat_chr(ard_out[["method_id"]])
  stat_col   <- .flat_chr(ard_out[["stat_name"]])
  methods    <- unique(stats::na.omit(method_col))
  stats::setNames(lapply(methods, function(m) {
    .method_body_entries(m, unique(stats::na.omit(stat_col[method_col == m])),
                         subject_n = subject_n)$params
  }), methods)
}

## ---------------------------------------------------------------------------
## Private helpers -- column plan + data preparation
## ---------------------------------------------------------------------------

## Ordered character vector of column (treatment) values. Tries the ARS display
## column definitions; falls back to ARD appearance order with a warning.
## `restrict = TRUE` (layout-driven path, ADR 0003 Layer D): ARD levels that
## match no shell column header are EXCLUDED instead of appended -- a
## population level like "Screen Failure" in TRT01A must not become a
## treatment column. Falls back to append-all when nothing matches.
build_col_levels <- function(out_obj, ard_out, col_var, restrict = FALSE,
                             ard_levels = NULL) {
  if (is.null(ard_levels)) {
    ard_levels <- unique(stats::na.omit(.flat_chr(ard_out[[col_var]])))
  }

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
  ## Header "core": text before the first parenthesis (drops "(N=200)",
  ## "n (%)") with any trailing stray "n" removed -- so a shortened header
  ## like "Xanomeline Low" can match the fuller arm value
  ## "Xanomeline Low Dose".
  core <- function(lb) {
    x <- sub("\\(.*$", "", lb)
    x <- sub("(?i)\\bn\\s*%?\\s*$", "", x, perl = TRUE)
    norm(x)
  }
  ordered <- character(0)
  for (lb in labels) {
    ## Tolerant match, both directions: a shell column header carries extra
    ## text the ARD arm value does not (e.g. "UPADALIMIB 15 mg\n(N=200) n (%)"
    ## vs the level "UPADALIMIB 15 mg") -- OR the header abbreviates the arm
    ## value (e.g. "Xanomeline Low" vs "Xanomeline Low Dose").
    nlb <- norm(lb)
    crb <- core(lb)
    hit <- ard_levels[!ard_levels %in% ordered &
                        vapply(ard_levels, function(a) {
                          na_ <- norm(a)
                          grepl(na_, nlb, fixed = TRUE) ||
                            (nzchar(crb) && grepl(crb, na_, fixed = TRUE))
                        }, logical(1))]
    if (length(hit)) ordered <- c(ordered, hit[1])
  }
  dropped <- setdiff(ard_levels, ordered)
  if (isTRUE(restrict) && length(ordered) > 0) {
    if (length(dropped) > 0) {
      diag_add(
        stage = "render", severity = "INFO",
        problem = sprintf("ARD level(s) not in the shell column headers: %s",
                          paste(dropped, collapse = ", ")),
        action = "Excluded from the treatment columns (shell layout is authoritative); counted in stub rows where annotated"
      )
    }
    return(ordered)
  }
  ordered <- c(ordered, dropped)
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
    ## Continuous methods AND declared-but-unexecutable methods label each row
    ## by its stat line (Mean (SD), Median, or for a reserved cell the stat name
    ## like "Conf.low"), so the body-plan structure that formats it -- including
    ## the [‡ manual] marker branch -- matches on label_val.
    statline <- .is_continuous_method(method) |
      method %in% names(.UNEXECUTABLE_METHODS)
    row_lbl <- ifelse(statline,
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

## Effective column (treatment) values of an ARD under the layout path. A
## subject-count analysis tabulates the treatment variable itself
## (ard_categorical(variables = by)), so its column value sits in
## variable_level with no group column -- include those.
.layout_col_values <- function(ard_out, col_var, fixed_vars) {
  n    <- nrow(ard_out)
  vals <- if (col_var %in% names(ard_out)) .flat_chr(ard_out[[col_var]]) else
    rep(NA_character_, n)
  varc <- if ("variable" %in% names(ard_out)) .flat_chr(ard_out[["variable"]]) else
    rep(NA_character_, n)
  lvlc <- if ("variable_level" %in% names(ard_out)) .flat_chr(ard_out[["variable_level"]]) else
    rep(NA_character_, n)
  swing <- is.na(vals) & !is.na(varc) & toupper(varc) %in% toupper(fixed_vars)
  vals[swing] <- lvlc[swing]
  unique(stats::na.omit(vals))
}

## Match an authored level-slot value to one of the parent analysis's data
## levels. Tolerant: exact (case-insensitive), then prefix either way (the
## LLM writes 'FEMALE' where the data codes 'F'), then substring.
.match_level <- function(slot, pool) {
  if (is.na(slot) || !nzchar(trimws(slot)) || length(pool) == 0) {
    return(NA_character_)
  }
  s <- toupper(trimws(slot))
  p <- toupper(trimws(pool))
  hit <- which(p == s)
  if (length(hit) == 0) hit <- which(startsWith(s, p))
  if (length(hit) == 0) hit <- which(startsWith(p, s))
  if (length(hit) == 0) {
    hit <- which(vapply(p, function(x) {
      grepl(x, s, fixed = TRUE) || grepl(s, x, fixed = TRUE)
    }, logical(1)))
  }
  if (length(hit) == 0) return(NA_character_)
  pool[hit[1]]
}

## Layout-driven data preparation (ADR 0003 Layer E). Left-joins the ordered
## authored layout to the ARD stats by analysis_id, so every authored row
## appears in order with its authored label. A row whose analysis produced
## nothing renderable becomes a blank line (spacer param) -- never dropped,
## never silently reordered. Categorical / continuous analyses expand to
## level / stat-line rows under their authored label.
.tfrmt_prep_ard_layout <- function(ard, output_id, layout, col_var,
                                   keep_params, col_levels, fixed_vars,
                                   params_map = list()) {
  oid <- .flat_chr(ard[["output_id"]])
  ard_out <- ard[!is.na(oid) & oid == output_id, , drop = FALSE]
  if (nrow(ard_out) == 0) {
    cli::cli_abort("ARD has zero rows for output {.val {output_id}}.")
  }

  n <- nrow(ard_out)
  fc <- function(cn) if (cn %in% names(ard_out)) .flat_chr(ard_out[[cn]]) else
    rep(NA_character_, n)
  flat <- data.frame(
    analysis_id    = fc("analysis_id"),
    method         = fc("method_id"),
    variable       = fc("variable"),
    variable_level = fc("variable_level"),
    stat_name      = .flat_chr(ard_out[["stat_name"]]),
    stat           = .flat_num(ard_out[["stat"]]),
    colv           = fc(col_var),
    stringsAsFactors = FALSE
  )

  ## Subject-count rows: treatment value arrives in variable_level (see
  ## .layout_col_values) -- move it into the column slot.
  swing <- is.na(flat$colv) & !is.na(flat$variable) &
    toupper(flat$variable) %in% toupper(fixed_vars) &
    !is.na(flat$variable_level)
  flat$colv[swing]           <- flat$variable_level[swing]
  flat$variable_level[swing] <- NA_character_
  flat$colv[is.na(flat$colv) | !nzchar(flat$colv)] <- "Total"

  ## Subject-count "n" renamed so its structure never collides with a
  ## count-and-percentage "{n} ({p}%)" structure (see .method_body_entries).
  subj <- !is.na(flat$method) & flat$method == "MTH_SUBJECT_COUNT" &
    flat$stat_name == "n"
  flat$stat_name[subj] <- .ARS_SUBJ_N_PARAM

  ## Rescale proportion stats (cards stores p in [0, 1]) to percentages.
  pct <- flat$stat_name %in% c("p", "pct", "percent")
  if (any(pct)) {
    pv <- flat$stat[pct]
    if (all(is.na(pv) | (pv >= 0 & pv <= 1.0000001))) flat$stat[pct] <- pv * 100
  }

  ## Column restriction: only shell columns (plus the ungrouped Total pass).
  ## Param restriction: each analysis keeps its own method's formatted params.
  keep_cols <- unique(c(col_levels, "Total"))
  p_ok <- vapply(seq_len(nrow(flat)), function(j) {
    ps <- if (!is.na(flat$method[j])) params_map[[flat$method[j]]] else NULL
    if (is.null(ps)) flat$stat_name[j] %in% keep_params else flat$stat_name[j] %in% ps
  }, logical(1))
  flat <- flat[p_ok & flat$colv %in% keep_cols, , drop = FALSE]

  first_col <- if (length(col_levels) > 0) col_levels[1] else "Total"
  blank_row <- function(le, ordv) data.frame(
    grp = le$label_grp %||% le$label, lbl = le$label, colv = first_col,
    stat_name = .ARS_SPACER_PARAM, stat = NA_real_, ordv = ordv,
    stringsAsFactors = FALSE)

  rows <- list()
  consumed <- rep(FALSE, nrow(layout))
  for (i in seq_len(nrow(layout))) {
    if (consumed[i]) next
    le  <- layout[i, , drop = FALSE]
    ord <- le$order * 1000L
    ## The group column is row identity, never printed (noprint plan). An
    ## authored row with an EMPTY stub label (annotation-only rows in nested
    ## AE shells) must still be unique, or several analyses' expanded rows
    ## collide into tfrmt pivot list-cols.
    if (!nzchar(le$label)) {
      le$label_grp <- sprintf(".arsbridge_row_%03d", le$order)
    } else {
      le$label_grp <- le$label
    }
    dat <- if (!is.na(le$analysis_id)) {
      flat[!is.na(flat$analysis_id) & flat$analysis_id == le$analysis_id, ,
           drop = FALSE]
    } else flat[0, , drop = FALSE]

    if (nrow(dat) == 0) {
      ## Authored label-only row, or an analysis with nothing renderable in
      ## the shell columns: keep the authored line, blank (never dropped).
      rows[[length(rows) + 1L]] <- blank_row(le, ord)
      next
    }

    if (!le$kind %in% c("categorical", "continuous", "manual")) {
      ## Scalar row (subject / filtered count): one line, authored label.
      rows[[length(rows) + 1L]] <- data.frame(
        grp = le$label_grp, lbl = le$label, colv = dat$colv,
        stat_name = dat$stat_name, stat = dat$stat, ordv = ord,
        stringsAsFactors = FALSE)
      next
    }

    ## Grouped row: authored label as a header line, then one line per
    ## category level (categorical) / stat line (continuous, manual).
    rows[[length(rows) + 1L]] <- blank_row(le, ord)

    ## Authored LEVEL slots (kind "level" entries following this categorical
    ## parent, option A): each takes its authored label and position, filled
    ## from the parent's computed levels; matched levels leave the parent's
    ## own expansion so nothing renders twice.
    if (identical(le$kind, "categorical")) {
      slots <- integer(0)
      j <- i + 1L
      while (j <= nrow(layout) && identical(layout$kind[j], "level") &&
               identical(layout$analysis_id[j], le$analysis_id)) {
        slots <- c(slots, j)
        j <- j + 1L
      }
      if (length(slots) > 0) {
        lvl_all <- ifelse(is.na(dat$variable_level) | !nzchar(dat$variable_level),
                          "Total", dat$variable_level)
        pool <- unique(lvl_all)
        for (jj in slots) {
          consumed[jj] <- TRUE
          slot_lbl <- layout$label[jj]
          slot_grp <- if (nzchar(slot_lbl)) slot_lbl else
            sprintf(".arsbridge_row_%03d", layout$order[jj])
          slot_ord <- layout$order[jj] * 1000L
          hit <- .match_level(layout$level[jj], pool)
          if (is.na(hit)) {
            rows[[length(rows) + 1L]] <- data.frame(
              grp = slot_grp, lbl = slot_lbl, colv = first_col,
              stat_name = .ARS_SPACER_PARAM, stat = NA_real_,
              ordv = slot_ord, stringsAsFactors = FALSE)
          } else {
            sel <- lvl_all == hit
            rows[[length(rows) + 1L]] <- data.frame(
              grp = slot_grp, lbl = slot_lbl, colv = dat$colv[sel],
              stat_name = dat$stat_name[sel], stat = dat$stat[sel],
              ordv = slot_ord, stringsAsFactors = FALSE)
            pool    <- setdiff(pool, hit)
            dat     <- dat[!sel, , drop = FALSE]
            lvl_all <- lvl_all[!sel]
          }
        }
        ## Every computed level claimed by an authored slot -> no leftover
        ## expansion under the parent header.
        if (nrow(dat) == 0) next
      }
    }

    sub_lbl <- if (identical(le$kind, "categorical")) {
      ifelse(is.na(dat$variable_level) | !nzchar(dat$variable_level),
             "Total", dat$variable_level)
    } else {
      vapply(dat$stat_name, .statline_for, character(1), USE.NAMES = FALSE)
    }

    ## When the shell authors its own sub-rows right after the analysis row
    ## ("Mean (SD)" / "Median" / "Min, Max" as label-only lines, or the
    ## category levels of a categorical block), fill THOSE authored rows
    ## instead of appending a duplicate block: matched sub-lines take the
    ## authored row's position and the authored spacer is consumed.
    trail <- integer(0)
    j <- i + 1L
    while (j <= nrow(layout) &&
             identical(layout$kind[j], "label") && !consumed[j]) {
      trail <- c(trail, j)
      j <- j + 1L
    }
    trail_norm <- vapply(layout$label[trail], .norm_label, character(1),
                         USE.NAMES = FALSE)
    uniq <- unique(sub_lbl)
    sub_ord_map <- stats::setNames(ord + seq_along(uniq), uniq)
    sub_lbl_map <- stats::setNames(uniq, uniq)
    for (u in uniq) {
      un  <- .norm_label(u)
      hit <- which(!is.na(trail_norm) & trail_norm == un)
      if (length(hit) == 0 && nzchar(un)) {
        ## Tolerant fallback: a data level may abbreviate the authored
        ## sub-row or vice versa ("F" vs "Female", "WHITE" vs "White").
        hit <- which(!is.na(trail_norm) &
                       (startsWith(trail_norm, un) |
                          startsWith(un, trail_norm)))
      }
      if (length(hit) > 0) {
        jj <- trail[hit[1]]
        sub_ord_map[[u]]     <- layout$order[jj] * 1000L
        consumed[jj]         <- TRUE
        trail_norm[hit[1]]   <- NA_character_
        ## Category levels display the AUTHORED text ("Female", "White");
        ## continuous stat lines keep their exact statline label, which the
        ## body-plan structures key on ("Mean (SD)").
        if (identical(le$kind, "categorical")) {
          sub_lbl_map[[u]] <- layout$label[jj]
        }
      }
    }
    sub_ord <- unname(sub_ord_map[sub_lbl])
    sub_lbl <- unname(sub_lbl_map[sub_lbl])

    rows[[length(rows) + 1L]] <- data.frame(
      grp = le$label_grp, lbl = sub_lbl, colv = dat$colv,
      stat_name = dat$stat_name, stat = dat$stat, ordv = sub_ord,
      stringsAsFactors = FALSE)
  }

  out <- do.call(rbind, rows)
  names(out) <- c(.ARS_SHELL_GRP, .ARS_SHELL_LBL, col_var,
                  "stat_name", "stat", .ARS_SHELL_ORD)
  rownames(out) <- NULL
  out[order(out[[.ARS_SHELL_ORD]]), , drop = FALSE]
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

  ## Authored shell layout (ADR 0003): when present it drives row labels,
  ## row order, and the column restriction; otherwise the pre-layout
  ## ARD-derived path below is used unchanged.
  layout     <- .shell_layout(out_obj)
  fixed_vars <- .fixed_grouping_vars(spec)

  if (is.null(col_var)) {
    col_var <- if (!is.null(layout)) {
      ## An all-scalar table (every row a subject count) can produce an ARD
      ## with no group*_level columns at all; the layout prep still builds
      ## the column from variable_level, under a synthetic name.
      tryCatch(detect_col_var(ard_out, spec),
               error = function(e) ".arsbridge_col")
    } else {
      detect_col_var(ard_out, spec)
    }
  }
  roles <- if (is.null(layout)) detect_row_roles(ard_out, col_var) else
    list(label_var = .ARS_SHELL_LBL, group_vars = .ARS_SHELL_GRP)
  if (is.null(label_var))  label_var  <- roles[["label_var"]]
  if (is.null(group_vars)) group_vars <- roles[["group_vars"]]
  group_vars <- setdiff(group_vars, c(col_var, label_var))

  bp        <- build_body_plan(ard_out, include_spacer = !is.null(layout))
  col_lvls  <- if (!is.null(layout)) {
    build_col_levels(out_obj, ard_out, col_var, restrict = TRUE,
                     ard_levels = .layout_col_values(ard_out, col_var, fixed_vars))
  } else {
    build_col_levels(out_obj, ard_out, col_var)
  }
  title     <- extract_title(out_obj)
  footnotes <- extract_footnotes(out_obj)

  ## When any reserved manual_pending cell is rendered, key the marker to a
  ## footnote so a reviewer knows the cell awaits a validated manual derivation
  ## (ADR 0002 phase 4).
  if ("result_status" %in% names(ard_out) &&
      any(.flat_chr(ard_out[["result_status"]]) == "manual_pending",
          na.rm = TRUE)) {
    footnotes <- c(footnotes, paste0(
      .MANUAL_MARKER,
      " = statistic requires manual derivation; not computed by arsbridge. ",
      "See ars_manual_worklist()."))
  }

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
  if (!is.null(layout)) {
    ## Pin row order to the authored layout and keep the (identity-only)
    ## group column out of the printed stub.
    tf_args[["sorting_cols"]] <- do.call(dplyr::vars,
                                         list(as.name(.ARS_SHELL_ORD)))
    tf_args[["row_grp_plan"]] <- tfrmt::row_grp_plan(
      label_loc = tfrmt::element_row_grp_loc(location = "noprint"))
  }

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
  attr(tf, "arsbridge_layout")      <- layout
  attr(tf, "arsbridge_col_levels")  <- col_lvls
  attr(tf, "arsbridge_fixed_vars")  <- fixed_vars
  attr(tf, "arsbridge_params_map")  <- .method_params_map(
    ard_out, subject_n = if (!is.null(layout)) .ARS_SUBJ_N_PARAM else "n")
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

  layout <- attr(tf, "arsbridge_layout")
  data_prepped <- if (!is.null(layout)) {
    .tfrmt_prep_ard_layout(
      ard, out_id, layout, col_var, keep_params,
      col_levels = attr(tf, "arsbridge_col_levels"),
      fixed_vars = attr(tf, "arsbridge_fixed_vars"),
      params_map = attr(tf, "arsbridge_params_map") %||% list())
  } else {
    .tfrmt_prep_ard(ard, out_id, col_var, label_var,
                    group_vars, keep_params)
  }
  stopifnot(label_var %in% names(data_prepped))

  gt_tbl <- tryCatch(
    tfrmt::print_to_gt(tf, .data = data_prepped),
    error = function(e) {
      ## A fixed column order (col_plan) that names a column absent from the
      ## prepared data -- e.g. a column-axis grouping that shipped raw
      ## data-driven levels, so no display-labelled columns exist -- makes
      ## tfrmt's create_col_order abort ("Unable to create dataset subset
      ## vars"). Failing here stops the whole pipeline before any RTF/Word file
      ## is written. Instead drop the fixed order and retry: the table still
      ## renders, just in tfrmt's default column order.
      if (is.null(tf[["col_plan"]])) stop(e)
      diag_add(
        stage = "render", severity = "WARN", input = INPUT_ARS,
        problem = sprintf(
          "Output %s: could not apply the shell's fixed column order (%s); rendered in default column order.",
          out_id, conditionMessage(e)),
        location = out_id,
        action = "Usually the column-axis grouping shipped raw data-driven levels -- define its column groups (annotate the shell header conditions, or use the supplement's column_groups) so the display columns are named."
      )
      tf[["col_plan"]] <- NULL
      tfrmt::print_to_gt(tf, .data = data_prepped)
    })
  ## Blank the synthetic row-label column header.
  if (label_var %in% c(.ARS_ROW_LABEL, .ARS_SHELL_LBL) &&
      label_var %in% names(gt_tbl[["_data"]])) {
    gt_tbl <- gt::cols_label(gt_tbl, .list = stats::setNames(list(""), label_var))
  }
  ## The layout ordering column is identity-only -- never displayed.
  if (.ARS_SHELL_ORD %in% names(gt_tbl[["_data"]])) {
    gt_tbl <- tryCatch(gt::cols_hide(gt_tbl, columns = dplyr::all_of(.ARS_SHELL_ORD)),
                       error = function(e) gt_tbl)
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
