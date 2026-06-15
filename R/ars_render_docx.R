## arsbridge -- ars_render_docx.R
## ---------------------------------------------------------------------------
## Word/RTF output for rendered TLFs, plus ars_render_all() which renders every
## output of a reporting event (tables + listings + figures) into a single
## landscape Word document and returns a coverage manifest. Built on
## flextable + officer; reuses the GT tables produced by ars_render_tlf() /
## ars_render_listing() and the ggplots from ars_render_figure().

## Convert a GT table (from ars_render_tlf / ars_render_listing) to a
## regulatory-style flextable: title + id as header lines, footnotes in the
## footer, group-header rows bolded, body indented.
.gt_to_flextable <- function(gt_tbl, oid_name, title, footnotes) {
  d <- as.data.frame(gt_tbl[["_data"]], stringsAsFactors = FALSE, check.names = FALSE)
  grp_col  <- intersect(c("..tfrmt_row_grp_lbl", ".tfrmt_row_grp_lbl"), names(d))
  grp_flag <- if (length(grp_col)) as.logical(d[[grp_col[1]]]) else rep(FALSE, nrow(d))
  grp_flag[is.na(grp_flag)] <- FALSE
  d <- d[, !names(d) %in% c("..tfrmt_row_grp_lbl", ".tfrmt_row_grp_lbl"), drop = FALSE]

  ## Restore display labels from the GT column labels when present.
  labs <- tryCatch(gt_tbl[["_boxhead"]][["column_label"]], error = function(e) NULL)
  vars <- tryCatch(gt_tbl[["_boxhead"]][["var"]], error = function(e) NULL)
  header_vals <- names(d)
  if (!is.null(labs) && !is.null(vars)) {
    header_vals <- vapply(names(d), function(v) {
      lab <- labs[match(v, vars)]
      lab <- if (length(lab) && !is.na(lab)) as.character(lab[[1]]) else v
      if (identical(lab, v) && grepl("^\\.|tfrmt", v)) "" else lab
    }, character(1))
  }

  label_col <- names(d)[1]
  body_cols <- names(d)[-1]
  lbl <- as.character(d[[label_col]])
  lbl[!grp_flag] <- paste0("    ", lbl[!grp_flag])
  d[[label_col]] <- lbl
  for (cc in body_cols) { v <- as.character(d[[cc]]); v[is.na(v)] <- ""; d[[cc]] <- v }

  thin <- officer::fp_border(width = 1)
  ft <- flextable::flextable(d)
  ft <- flextable::set_header_labels(ft, values = stats::setNames(as.list(header_vals), names(d)))
  hdr <- c(oid_name, if (length(title)) title[1] else NULL)
  hdr <- hdr[nzchar(hdr)]
  ft <- flextable::add_header_lines(ft, values = hdr)
  if (length(footnotes)) {
    ft <- flextable::add_footer_lines(ft, values = footnotes)
    ft <- flextable::fontsize(ft, size = 8, part = "footer")
    ft <- flextable::italic(ft, part = "footer")
  }
  if (any(grp_flag)) ft <- flextable::bold(ft, i = which(grp_flag), j = 1, part = "body")
  ## Match the annotated shell's look: Arial, with the table id + title block
  ## centred above the columns (regulatory house style).
  ft <- flextable::font(ft, fontname = "Arial", part = "all")
  ft <- flextable::fontsize(ft, size = 9, part = "body")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::align(ft, j = 1, align = "left", part = "all")
  if (length(body_cols)) ft <- flextable::align(ft, j = body_cols, align = "center", part = "all")
  if (length(hdr)) ft <- flextable::align(ft, i = seq_along(hdr), align = "center", part = "header")
  ft <- flextable::border_remove(ft)
  ft <- flextable::hline_top(ft, part = "header", border = thin)
  ft <- flextable::hline_bottom(ft, part = "header", border = thin)
  ft <- flextable::hline_bottom(ft, part = "body", border = thin)
  flextable::autofit(ft)
}

## Write a single flextable to a Word or RTF file (landscape).
.write_flextable <- function(ft, file, format) {
  if (format == "rtf") {
    flextable::save_as_rtf(ft, path = file)
  } else {
    sect <- officer::prop_section(
      page_size = officer::page_size(orient = "landscape"), type = "continuous",
      page_margins = officer::page_mar(top = 0.5, bottom = 0.5,
                                       left = 0.5, right = 0.5))
    flextable::save_as_docx(ft, path = file, pr_section = sect)
  }
  invisible(file)
}

#' Render every output of a reporting event into one Word document
#'
#' Walks all outputs of an ARS reporting event, rendering tables with
#' [ars_render_tlf()], listings with [ars_render_listing()], and figures with
#' [ars_render_figure()], and assembles them into a single landscape `.docx`
#' (one output per page). Returns a manifest recording, for every output,
#' whether it rendered and -- if not -- why.
#'
#' @param ars_path Path to the ARS JSON.
#' @param ard Tidy ARD from [ars_to_ard()] (drives the tables).
#' @param adam_dir Directory of ADaM datasets, required to render listings and
#'   figures. If `NULL`, those are skipped (recorded in the manifest).
#' @param file Output `.docx` path. Default: `reporting_event_tlfs.docx` in
#'   [tempdir()].
#' @param output_ids Optional character vector of output ids or names
#'   (case-insensitive) to render -- any mix of tables, listings, and figures.
#'   `NULL` (default) renders every output. Ids absent from the spec are
#'   reported in the manifest as skipped.
#' @param types Which output kinds to render. Default all three. Applied in
#'   addition to `output_ids`.
#' @param max_rows Row cap for listings (see [ars_render_listing()]).
#' @param progress Optional `function(i, n, output_id)` for progress reporting.
#' @return A data frame manifest (`output_id`, `type`, `status`, `reason`),
#'   invisibly carrying the written file path as attribute `"file"`.
#' @seealso [ars_render_tlf()], [ars_render_listing()], [ars_render_figure()]
#' @export
#' @examples
#' \dontrun{
#'   # Just three specific outputs into one Word document:
#'   ars_render_all(ars, ard, adam_dir,
#'                  output_ids = c("T_14_1_1", "L_16_2_4_1", "F_14_2_1"))
#' }
ars_render_all <- function(ars_path, ard, adam_dir = NULL, file = NULL,
                           output_ids = NULL,
                           types = c("table", "listing", "figure"),
                           max_rows = 500, progress = NULL) {
  spec <- jsonlite::fromJSON(ars_path, simplifyVector = FALSE)
  file <- file %||% file.path(tempdir(), "reporting_event_tlfs.docx")

  ## Optional explicit selection (match id OR name, case-insensitive). Warn on
  ## any requested id that is not in the spec.
  want <- NULL
  if (!is.null(output_ids)) {
    want <- tolower(trimws(output_ids))
    spec_keys <- unlist(lapply(spec[["outputs"]], function(o)
      tolower(c(.sc(o[["id"]]), .sc(o[["name"]])))))
    missing_ids <- output_ids[!want %in% spec_keys]
    if (length(missing_ids)) {
      cli::cli_warn("Output id{?s} not found in the spec: {.val {missing_ids}}")
    }
  }
  sect <- officer::prop_section(
    page_size = officer::page_size(orient = "landscape"), type = "continuous",
    page_margins = officer::page_mar(top = 0.5, bottom = 0.5,
                                     left = 0.5, right = 0.5))
  doc  <- officer::read_docx()
  doc  <- officer::body_set_default_section(doc, sect)

  classify <- function(o) {
    ot <- toupper(.sc(o[["outputType"]]) %||% "")
    if (ot == "TABLE"   || grepl("^T", .sc(o[["id"]]))) return("table")
    if (ot == "LISTING" || grepl("^L", .sc(o[["id"]]))) return("listing")
    if (ot == "FIGURE"  || grepl("^F", .sc(o[["id"]]))) return("figure")
    "table"
  }

  rows <- list(); n <- length(spec[["outputs"]]); first <- TRUE
  for (i in seq_len(n)) {
    o    <- spec[["outputs"]][[i]]
    oid  <- .sc(o[["id"]])
    kind <- classify(o)
    if (!is.null(progress)) progress(i, n, oid)

    ## When an explicit selection is given, silently pass over the rest so the
    ## manifest lists only what was requested.
    if (!is.null(want) &&
        !any(tolower(c(oid, .sc(o[["name"]]))) %in% want)) next

    rec <- list(output_id = oid, type = kind, status = "skipped", reason = "")

    if (!kind %in% types) {
      rec$reason <- "type not requested"
      rows[[length(rows) + 1L]] <- rec; next
    }
    res <- tryCatch({
      if (kind == "table") {
        ft <- .gt_to_flextable(ars_render_tlf(ars_path, ard, oid),
                               .sc(o[["name"]]) %||% oid, extract_title(o),
                               extract_footnotes(o))
        if (!first) doc <- officer::body_add_break(doc)
        doc <- flextable::body_add_flextable(doc, ft, align = "left"); first <- FALSE
        "ok"
      } else if (kind == "listing") {
        if (is.null(adam_dir)) stop("adam_dir required for listings")
        ft <- .gt_to_flextable(ars_render_listing(ars_path, adam_dir, oid, max_rows = max_rows),
                               .sc(o[["name"]]) %||% oid, extract_title(o),
                               extract_footnotes(o))
        if (!first) doc <- officer::body_add_break(doc)
        doc <- flextable::body_add_flextable(doc, ft, align = "left"); first <- FALSE
        "ok"
      } else {
        if (is.null(adam_dir)) stop("adam_dir required for figures")
        p <- ars_render_figure(ars_path, adam_dir, oid)
        if (!first) doc <- officer::body_add_break(doc)
        doc <- officer::body_add_gg(doc, p, width = 9, height = 5.5); first <- FALSE
        "ok"
      }
    }, error = function(e) conditionMessage(e))

    if (identical(res, "ok")) { rec$status <- "rendered" } else { rec$reason <- res }
    rows[[length(rows) + 1L]] <- rec
  }

  print(doc, target = file)
  manifest <- if (length(rows)) {
    do.call(rbind, lapply(rows, function(r)
      data.frame(output_id = r$output_id, type = r$type, status = r$status,
                 reason = r$reason, stringsAsFactors = FALSE)))
  } else {
    data.frame(output_id = character(0), type = character(0),
               status = character(0), reason = character(0))
  }
  attr(manifest, "file") <- file
  manifest
}
