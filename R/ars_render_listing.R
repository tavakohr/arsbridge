## arsbridge -- ars_render_listing.R
## ---------------------------------------------------------------------------
## Renders a CDISC ARS *listing* output (subject-level data display) to a GT
## table. Listings are not summarised into the ARD by ars_to_ard(); instead
## each MTH_LISTING analysis defines one column (variable + source dataset +
## label), and this renderer assembles those columns -- merging across datasets
## by subject -- directly from the ADaM data.

## Minimal ADaM loader (xpt/csv), case-insensitive on dataset name.
.listing_load <- function(adam_dir, name) {
  if (is.null(name) || !nzchar(name)) return(NULL)
  files <- list.files(adam_dir, full.names = TRUE)
  base  <- tolower(basename(files))
  xpt <- files[base == tolower(paste0(name, ".xpt"))]
  csv <- files[base == tolower(paste0(name, ".csv"))]
  if (length(xpt)) return(as.data.frame(haven::read_xpt(xpt[1])))
  if (length(csv)) return(utils::read.csv(csv[1], stringsAsFactors = FALSE, check.names = FALSE))
  NULL
}

## Compact where-clause evaluator for listing population/subset filters.
## Supports a single condition (EQ/NE/IN/NOTIN/CONTAINS) and AND/OR compounds.
.listing_eval_where <- function(df, wc) {
  if (is.null(wc)) return(rep(TRUE, nrow(df)))
  if (!is.null(wc[["compoundExpression"]])) {
    ce <- wc[["compoundExpression"]]
    parts <- lapply(ce[["whereClauses"]], function(c) .listing_eval_where(df, c))
    if (!length(parts)) return(rep(TRUE, nrow(df)))
    op <- .sc(ce[["logicalOperator"]])
    return(Reduce(if (identical(op, "OR")) `|` else `&`, parts))
  }
  cond <- wc[["condition"]] %||% wc
  var  <- .sc(cond[["variable"]]); var <- sub("^.*\\.", "", var %||% "")
  if (is.na(var) || !nzchar(var) || !var %in% names(df)) return(rep(TRUE, nrow(df)))
  comp <- .sc(cond[["comparator"]]); val <- unlist(cond[["value"]])
  col  <- df[[var]]
  switch(comp %||% "EQ",
    EQ = col %in% val, IN = col %in% val,
    NE = !(col %in% val), NOTIN = !(col %in% val),
    CONTAINS = Reduce(`|`, lapply(val, function(v)
      grepl(tolower(v), tolower(as.character(col)), fixed = TRUE))),
    rep(TRUE, nrow(df)))
}

#' Render an ARS listing output to a GT table
#'
#' Assembles the columns of a listing output (one per `MTH_LISTING` analysis),
#' merging variables from auxiliary datasets onto the primary dataset by
#' subject, applies the listing's population filter, and returns a `gt_tbl`.
#'
#' @param ars_path Path to the CDISC ARS JSON.
#' @param adam_dir Directory containing the ADaM datasets (.xpt/.csv).
#' @param output_id Listing output id or name (case-insensitive).
#' @param subject_key Subject identifier for cross-dataset merges. Default
#'   `"USUBJID"`.
#' @param max_rows Cap on listed rows (default 500). Set `Inf` for all rows; a
#'   note is added when rows are truncated.
#' @return A `gt_tbl`.
#' @seealso [ars_render_tlf()], [ars_to_ard()]
#' @export
ars_render_listing <- function(ars_path, adam_dir, output_id,
                               subject_key = "USUBJID", max_rows = 500) {
  spec    <- jsonlite::fromJSON(ars_path, simplifyVector = FALSE)
  out_obj <- find_output(spec, output_id)
  output_id <- .sc(out_obj[["id"]])

  aids <- vapply(out_obj[["referencedAnalysisIds"]], .sc, character(1))
  analyses <- Filter(function(a) .sc(a[["id"]]) %in% aids, spec[["analyses"]])
  ## Column specs in referenced order; keep only listing-style analyses.
  cols <- list()
  for (aid in aids) {
    a <- NULL
    for (an in analyses) if (identical(.sc(an[["id"]]), aid)) { a <- an; break }
    if (is.null(a)) next
    if (!identical(.sc(a[["methodId"]]), "MTH_LISTING")) next
    var <- .sc(a[["analysisVariable"]][["variable"]] %||% a[["variable"]])
    ds  <- .sc(a[["analysisVariable"]][["dataset"]] %||% a[["dataset"]])
    lab <- .sc(a[["description"]]) %||% var
    if (is.na(var) || is.na(ds)) next
    cols[[length(cols) + 1L]] <- list(var = var, ds = toupper(ds), label = lab,
                                      set = .sc(a[["analysisSetId"]]),
                                      subset = .sc(a[["dataSubsetId"]]))
  }
  if (!length(cols)) {
    cli::cli_abort("Output {.val {output_id}} has no listing ({.val MTH_LISTING}) columns.")
  }

  ## Primary dataset = the one supplying the most columns.
  ds_counts <- table(vapply(cols, `[[`, character(1), "ds"))
  primary   <- names(ds_counts)[which.max(ds_counts)]
  base_df   <- .listing_load(adam_dir, primary)
  if (is.null(base_df)) {
    cli::cli_abort("Primary listing dataset {.val {primary}} not found in {.path {adam_dir}}.")
  }

  ## Apply the population/subset filter of the primary-dataset columns.
  set_id    <- cols[[which(vapply(cols, `[[`, character(1), "ds") == primary)[1]]][["set"]]
  subset_id <- cols[[which(vapply(cols, `[[`, character(1), "ds") == primary)[1]]][["subset"]]
  find_wc <- function(coll, id) {
    if (is.na(id) || !nzchar(id)) return(NULL)
    for (w in spec[[coll]]) if (identical(.sc(w[["id"]]), id)) return(w)
    NULL
  }
  for (wc in list(find_wc("analysisSets", set_id), find_wc("dataSubsets", subset_id))) {
    if (!is.null(wc)) base_df <- base_df[.listing_eval_where(base_df, wc), , drop = FALSE]
  }

  ## Assemble the requested columns, merging auxiliary datasets by subject.
  aux_cache <- list()
  out <- data.frame(.row = seq_len(nrow(base_df)))
  labels <- character(0)
  for (i in seq_along(cols)) {
    cc <- cols[[i]]; nm <- paste0("c", i); labels[nm] <- cc$label
    if (cc$ds == primary) {
      out[[nm]] <- if (cc$var %in% names(base_df)) base_df[[cc$var]] else NA
    } else {
      if (is.null(aux_cache[[cc$ds]])) aux_cache[[cc$ds]] <- .listing_load(adam_dir, cc$ds)
      aux <- aux_cache[[cc$ds]]
      if (!is.null(aux) && subject_key %in% names(aux) && subject_key %in% names(base_df) &&
          cc$var %in% names(aux)) {
        lk <- aux[!duplicated(aux[[subject_key]]), c(subject_key, cc$var), drop = FALSE]
        out[[nm]] <- lk[[cc$var]][match(base_df[[subject_key]], lk[[subject_key]])]
      } else {
        out[[nm]] <- NA
      }
    }
  }
  out[[".row"]] <- NULL

  n_total <- nrow(out)
  truncated <- is.finite(max_rows) && n_total > max_rows
  if (truncated) out <- out[seq_len(max_rows), , drop = FALSE]

  title     <- extract_title(out_obj)
  footnotes <- extract_footnotes(out_obj)
  if (truncated) {
    footnotes <- c(footnotes, sprintf(
      "Listing truncated to %d of %d rows; pass max_rows = Inf for all.",
      max_rows, n_total))
  }

  gt_tbl <- gt::gt(out)
  gt_tbl <- gt::cols_label(gt_tbl, .list = as.list(labels))
  if (length(title) == 1 && nzchar(title)) gt_tbl <- gt::tab_header(gt_tbl, title = title)
  for (fn in footnotes) gt_tbl <- gt::tab_source_note(gt_tbl, source_note = fn)
  gt_tbl
}
