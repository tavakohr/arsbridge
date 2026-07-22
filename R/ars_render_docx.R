## arsbridge -- ars_render_docx.R
## ---------------------------------------------------------------------------
## Word/RTF output for rendered TLFs, plus ars_render_all() which renders every
## output of a reporting event (tables + listings + figures) into a single
## landscape Word document and returns a coverage manifest. Built on
## flextable + officer; reuses the GT tables produced by ars_render_tlf() /
## ars_render_listing() and the ggplots from ars_render_figure().

## Reconstruct the shell's exact TLF heading from an output id, e.g.
## "T_14_2_1" -> "Table 14.2.1", "L_16_2_4_1" -> "Listing 16.2.4.1". The id
## was derived from the shell heading, so this reproduces the shell numbering.
.tlf_heading <- function(oid, kind) {
  num  <- gsub("[_-]", ".", sub("^[A-Za-z]+[_-]", "", oid %||% ""))
  word <- c(table = "Table", listing = "Listing", figure = "Figure")[[kind]] %||% "Table"
  if (nzchar(num)) paste(word, num) else word
}

## Classify one output as "table", "listing", or "figure" from its outputType
## (falling back to the id prefix T/L/F). Shared by every batch renderer so
## they agree on how each output is dispatched.
.classify_output <- function(o) {
  ot <- toupper(.sc(o[["outputType"]]) %||% "")
  id <- .sc(o[["id"]]) %||% ""
  if (ot == "TABLE"   || grepl("^T", id)) return("table")
  if (ot == "LISTING" || grepl("^L", id)) return("listing")
  if (ot == "FIGURE"  || grepl("^F", id)) return("figure")
  "table"
}

## Render one table/listing output to a regulatory flextable, reusing the same
## GT -> flextable path as ars_render_all(). Aborts (caller catches) for figures
## -- they are images, not table cells -- and when a listing lacks adam_dir.
.output_to_flextable <- function(ars_path, spec, ard, o, adam_dir, max_rows) {
  oid  <- .sc(o[["id"]])
  kind <- .classify_output(o)
  if (kind == "figure") {
    stop("figures are not part of a table RTF; render them with ars_render_figure()")
  }
  gt_tbl <- if (kind == "listing") {
    if (is.null(adam_dir))
      stop("adam_dir is required to render listing ", oid)
    ars_render_listing(ars_path, adam_dir, oid, max_rows = max_rows)
  } else {
    ars_render_tlf(ars_path, ard, oid)
  }
  .gt_to_flextable(gt_tbl, .sc(o[["name"]]) %||% oid,
                   extract_title(o), extract_footnotes(o))
}

## The subset of a big ARD belonging to one output id (keeps every column and
## the cards list-columns intact). Empty frame when the output has no rows.
.ard_for_output <- function(ard, oid) {
  if (is.null(ard) || !"output_id" %in% names(ard)) return(ard)
  keep <- .flat_chr(ard[["output_id"]]) == oid
  keep[is.na(keep)] <- FALSE
  ard[keep, , drop = FALSE]
}

## Add a numbered placeholder page for an output arsbridge did not render.
## Keeps the final document complete and the table numbering aligned to the
## shell. `gate = TRUE` (the default) is the deliberate capability gate -- the
## analysis needs statistics beyond arsbridge's descriptive {cards} scope, and
## the placeholder must read as an intentional boundary, not a bug. `gate =
## FALSE` marks an actual render failure, where the reason is an error message.
## Make a string safe for OOXML text nodes: strip ANSI escape sequences
## (cli/rlang error messages carry them in interactive sessions -- ESC is an
## invalid XML character and officer's read_xml aborts on it: "PCDATA invalid
## Char value 27") and any other control characters except tab/newline.
.xml_safe <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("\033\\[[0-9;]*[A-Za-z]", "", x)
  gsub("[\001-\010\013\014\016-\037\177]", "", x, perl = TRUE)
}

.add_placeholder <- function(doc, heading, title, reason, first, gate = TRUE,
                             detail = NULL) {
  heading <- .xml_safe(heading)
  title   <- .xml_safe(title)
  reason  <- .xml_safe(reason)
  detail  <- if (is.null(detail)) NULL else .xml_safe(detail)
  if (!first) doc <- officer::body_add_break(doc)
  doc <- officer::body_add_par(doc, heading)
  if (nzchar(title %||% "")) doc <- officer::body_add_par(doc, title)
  msg <- if (isTRUE(gate)) {
    sprintf(paste0(
      "[ Placeholder -- arsbridge did not generate this table: %s. This is an ",
      "intentional capability gate, not a render error: the analysis needs ",
      "statistical methods beyond arsbridge's descriptive {cards} scope. ",
      "Produce it from a separate validated analysis script. See ",
      "adr/0001-statistical-method-extensibility.md for how this boundary ",
      "is drawn and extended. ]"),
      reason %||% "unsupported analysis")
  } else {
    sprintf(paste0(
      "[ Placeholder -- arsbridge could not render this table: %s. This is a ",
      "render error, not a capability gate; inspect ars_diagnostics() and the ",
      "ARS spec for this output. ]"),
      reason %||% "render error")
  }
  doc <- officer::body_add_par(doc, msg)
  if (!is.null(detail) && nzchar(detail)) doc <- officer::body_add_par(doc, detail)
  doc
}

## TRUE when the ARD holds at least one computed (non-stub) result for an
## output. Drives the render-vs-placeholder choice (ADR 0002 phase 4): an
## output with some computable cells is rendered with its manual_pending cells
## marked; one with no computable cells stays a whole-table placeholder.
.output_has_computed <- function(ard, oid) {
  if (is.null(ard) || !all(c("output_id", "result_status") %in% names(ard)))
    return(FALSE)
  chr <- function(col) if (is.list(col)) vapply(col, function(x)
    if (length(x)) as.character(x[[1]]) else NA_character_, character(1)) else
      as.character(col)
  o <- chr(ard[["output_id"]]); s <- chr(ard[["result_status"]])
  any(!is.na(o) & o == oid & !is.na(s) & s == "computed")
}

## One-line summary of the reserved manual_pending cells for an output, listed
## on its placeholder page so the document names exactly which keyed cells need
## a manual derivation (they exist as stub rows in the ARD).
.reserved_cells_detail <- function(ard, oid) {
  wl <- ars_manual_worklist(ard)
  wl <- wl[!is.na(wl$output_id) & wl$output_id == oid, , drop = FALSE]
  if (nrow(wl) == 0) return(NULL)
  paste0("Reserved cell(s) needing manual derivation (see ars_manual_worklist()): ",
         paste(sprintf("%s [%s]", wl$stat_name, wl$method_id), collapse = "; "))
}

## Convert a GT table (from ars_render_tlf / ars_render_listing) to a
## regulatory-style flextable: title + id as header lines, footnotes in the
## footer, group-header rows bolded, body indented.
.gt_to_flextable <- function(gt_tbl, oid_name, title, footnotes) {
  d <- as.data.frame(gt_tbl[["_data"]], stringsAsFactors = FALSE, check.names = FALSE)
  grp_col  <- intersect(c("..tfrmt_row_grp_lbl", ".tfrmt_row_grp_lbl"), names(d))
  grp_flag <- if (length(grp_col)) as.logical(d[[grp_col[1]]]) else rep(FALSE, nrow(d))
  grp_flag[is.na(grp_flag)] <- FALSE
  d <- d[, !names(d) %in% c("..tfrmt_row_grp_lbl", ".tfrmt_row_grp_lbl",
                            ".arsbridge_shell_ord", ".arsbridge_shell_grp"),
         drop = FALSE]

  ## Keep the row-label (and group) column on the LEFT. tfrmt's col_plan only
  ## names the treatment columns, so with .drop=FALSE the label column can be
  ## appended on the right; move the known stub columns back to the front so the
  ## table is not mirrored. Names come from ars_render_tlf() as attributes.
  label_var  <- attr(gt_tbl, "arsbridge_label_var")
  group_vars <- attr(gt_tbl, "arsbridge_group_vars") %||% character()
  stub_cols  <- intersect(c(group_vars, label_var), names(d))
  if (length(stub_cols)) {
    d <- d[, c(stub_cols, setdiff(names(d), stub_cols)), drop = FALSE]
  }

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

  label_col <- if (!is.null(label_var) && label_var %in% names(d)) label_var else names(d)[1]
  body_cols <- names(d)[-1]
  lbl <- as.character(d[[label_col]])
  lbl[!grp_flag] <- paste0("    ", lbl[!grp_flag])
  d[[label_col]] <- lbl
  for (cc in body_cols) { v <- as.character(d[[cc]]); v[is.na(v)] <- ""; d[[cc]] <- v }

  thin <- officer::fp_border(width = 1)
  ft <- flextable::flextable(d)
  ft <- flextable::set_header_labels(ft, values = stats::setNames(as.list(header_vals), names(d)))
  hdr <- c(oid_name, if (length(title)) title[1] else NULL)
  hdr <- .xml_safe(hdr[nzchar(hdr)])
  ft <- flextable::add_header_lines(ft, values = hdr)
  if (length(footnotes)) {
    ft <- flextable::add_footer_lines(ft, values = .xml_safe(footnotes))
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
  spec <- .read_json(ars_path)
  if (is.null(ard)) {
    cli::cli_abort(c(
      "x" = "No ARD was supplied ({.arg ard} is NULL) -- there are no results to render.",
      "i" = "To fix: run {.fn ars_to_ard} first and pass its result as {.arg ard}."
    ))
  }
  if (!is.null(adam_dir)) .require_dir(adam_dir, "adam_dir", INPUT_DATA)
  file <- file %||% file.path(tempdir(), "reporting_event_tlfs.docx")

  ## Guard the manual fills before rendering (ADR 0002 phase 5): a value typed
  ## into a manual_filled cell without a derivation_ref is untraceable and must
  ## never ship. Surface each as a blocker rather than silently rendering it.
  bad_fills <- ars_validate_manual_fills(ard)
  if (nrow(bad_fills) > 0) {
    for (i in seq_len(nrow(bad_fills))) {
      diag_add(
        stage = "render", severity = "FAIL", input = INPUT_ARS,
        problem = sprintf("Untraceable manual fill in %s (%s): %s",
                          bad_fills$output_id[i] %||% "?",
                          bad_fills$stat_name[i] %||% "?",
                          bad_fills$problem[i]),
        location = bad_fills$analysis_id[i] %||% "",
        action = paste0("Set derivation_ref (the validated program that ",
                        "produced the value) and a non-NA stat before ",
                        "rendering -- see ars_validate_manual_fills()."))
    }
    cli::cli_warn(paste0(
      "{nrow(bad_fills)} manual fill{?s} {?is/are} untraceable (no ",
      "derivation_ref or no value) -- inspect with {.fn ars_validate_manual_fills}; ",
      "rendered cells stay marked until fixed."))
  }

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

  classify <- .classify_output

  ## Outputs arsbridge cannot generate (from build-time capability gate) ->
  ## rendered as numbered placeholders instead of being skipped/coerced.
  unsupported_map <- list()
  for (u in spec[["_meta"]][["unsupported_outputs"]] %||% list()) {
    id <- .sc(u[["id"]]); if (nzchar(id %||% ""))
      unsupported_map[[id]] <- .sc(u[["reason"]]) %||% "not supported by arsbridge"
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

    ## Capability-gated analysis. If the ARD has NO computable cell for this
    ## output -> whole-table numbered placeholder (no execution), now naming the
    ## reserved manual cells. If it has SOME computed cells, fall through and
    ## render the partial table: computed cells are filled, reserved
    ## manual_pending cells render as the loud [‡ manual] marker (ADR 0002 ph4).
    unsup <- unsupported_map[[oid]]
    if (!is.null(unsup) && !.output_has_computed(ard, oid)) {
      doc <- .add_placeholder(doc, .tlf_heading(oid, kind),
                              extract_title(o) %||% .sc(o[["name"]]) %||% oid,
                              unsup, first, gate = TRUE,
                              detail = .reserved_cells_detail(ard, oid))
      first <- FALSE
      rec$status <- "placeholder"; rec$reason <- unsup
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

    if (identical(res, "ok")) {
      rec$status <- "rendered"
      ## A gated output that still rendered did so partially: some cells are
      ## reserved manual_pending markers. Flag it in the manifest.
      if (!is.null(unsup)) rec$reason <- "partial -- manual cells reserved"
    } else {
      ## Render failed -> emit a numbered placeholder so the table still
      ## appears in the document and shell numbering stays intact.
      doc <- .add_placeholder(doc, .tlf_heading(oid, kind),
                              extract_title(o) %||% .sc(o[["name"]]) %||% oid,
                              res, first, gate = FALSE); first <- FALSE
      rec$status <- "placeholder"; rec$reason <- res
    }
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


#' Render each output to its own ARD and table file
#'
#' The "one file per program" companion to [ars_render_all()]: instead of a
#' single combined document, this writes a separate table file (and, by
#' default, a separate ARD `.rds`) for every output in the reporting event --
#' the layout most clinical repositories expect, one deliverable per TLF
#' program.
#'
#' Tables and listings are written with the same regulatory flextable styling
#' as [ars_render_all()]; figures are written as `.png`. Each output's ARD
#' slice (its rows of the big ARD, cards list-columns intact) is saved as
#' `<dir>/<output_id>.rds` unless `write_ard = FALSE`.
#'
#' @param ars_path Path to the ARS reporting-event JSON.
#' @param dir Output directory. Created (recursively) if it does not exist.
#' @param adam_dir Directory of ADaM datasets. Required to compute the ARD when
#'   `ard` is `NULL`, and to render listings/figures.
#' @param ard Optional precomputed ARD (from [ars_to_ard()]). When `NULL` it is
#'   computed from `ars_path` + `adam_dir`.
#' @param output_ids Optional character vector restricting which outputs to
#'   render (matched against output id or name, case-insensitively).
#' @param format Table file format: `"rtf"` (default) or `"docx"`.
#' @param write_ard Also write each output's ARD slice as
#'   `<dir>/<output_id>.rds`. Default `TRUE`.
#' @param max_rows Row cap for listings. Default 500.
#'
#' @return Invisibly, a manifest data frame: `output_id`, `type`, `status`
#'   (`"rendered"`/`"error"`/`"skipped"`), `ard_file`, `doc_file`, `reason`.
#'   `attr(., "dir")` is the output directory.
#'
#' @seealso [ars_render_combined()] for one big ARD + one combined RTF,
#'   [ars_render_all()] for a single combined Word document.
#' @export
ars_render_split <- function(ars_path, dir, adam_dir = NULL, ard = NULL,
                             output_ids = NULL, format = c("rtf", "docx"),
                             write_ard = TRUE, max_rows = 500) {
  format <- match.arg(format)
  spec   <- .read_json(ars_path)
  if (is.null(ard)) {
    if (is.null(adam_dir)) {
      cli::cli_abort(c(
        "x" = "No {.arg ard} supplied and no {.arg adam_dir} to compute one.",
        "i" = "Pass {.arg adam_dir} (the ADaM folder) or a precomputed {.arg ard} from {.fn ars_to_ard}."))
    }
    ard <- ars_to_ard(ars_path, adam_dir = adam_dir)
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  want <- if (!is.null(output_ids)) tolower(trimws(output_ids)) else NULL
  rows <- list()
  for (o in spec[["outputs"]]) {
    oid  <- .sc(o[["id"]])
    kind <- .classify_output(o)
    if (!is.null(want) &&
        !any(tolower(c(oid, .sc(o[["name"]]))) %in% want)) next

    ard_file <- NA_character_
    if (isTRUE(write_ard)) {
      ard_file <- file.path(dir, paste0(oid, ".rds"))
      saveRDS(.ard_for_output(ard, oid), ard_file)
    }

    doc_file <- NA_character_; status <- "error"; reason <- ""
    res <- tryCatch({
      if (kind == "figure") {
        if (is.null(adam_dir)) stop("adam_dir is required to render figures")
        p <- ars_render_figure(ars_path, adam_dir, oid)
        doc_file <- file.path(dir, paste0(oid, ".png"))
        ggplot2::ggsave(doc_file, plot = p, width = 9, height = 5.5, dpi = 300)
      } else {
        ft <- .output_to_flextable(ars_path, spec, ard, o, adam_dir, max_rows)
        doc_file <- file.path(dir, paste0(oid, ".", format))
        .write_flextable(ft, doc_file, format)
      }
      "ok"
    }, error = function(e) conditionMessage(e))

    if (identical(res, "ok")) {
      status <- "rendered"
    } else {
      doc_file <- NA_character_; reason <- res
    }
    rows[[length(rows) + 1L]] <- data.frame(
      output_id = oid %||% NA_character_, type = kind, status = status,
      ard_file = ard_file, doc_file = doc_file, reason = reason,
      stringsAsFactors = FALSE)
  }

  manifest <- if (length(rows)) do.call(rbind, rows) else data.frame(
    output_id = character(), type = character(), status = character(),
    ard_file = character(), doc_file = character(), reason = character(),
    stringsAsFactors = FALSE)
  attr(manifest, "dir") <- dir
  n_ok <- sum(manifest$status == "rendered")
  cli::cli_alert_success(
    "Wrote {n_ok} of {nrow(manifest)} output{?s} to {.path {dir}} (one file per program).")
  invisible(manifest)
}

#' Render every output into one combined ARD and one combined RTF
#'
#' The "run all" companion to [ars_render_split()]: computes (or accepts) the
#' single big ARD covering every analysis, optionally saves it, and writes all
#' tables and listings into ONE combined RTF file (each table carries its own
#' id + title header). Figures are not included in an RTF and are reported as
#' skipped -- use [ars_render_split()] or [ars_render_all()] for those.
#'
#' @param ars_path Path to the ARS reporting-event JSON.
#' @param file Path of the combined `.rtf` to write.
#' @param adam_dir Directory of ADaM datasets. Required to compute the ARD when
#'   `ard` is `NULL`, and to render listings.
#' @param ard Optional precomputed ARD (from [ars_to_ard()]). When `NULL` it is
#'   computed from `ars_path` + `adam_dir`.
#' @param ard_file Optional path to also save the big ARD as an `.rds`.
#' @param output_ids Optional character vector restricting which outputs to
#'   include (matched against output id or name, case-insensitively).
#' @param max_rows Row cap for listings. Default 500.
#'
#' @return Invisibly, a manifest data frame: `output_id`, `type`, `status`
#'   (`"rendered"`/`"error"`/`"skipped"`), `reason`. `attr(., "file")` is the
#'   RTF path and `attr(., "ard_file")` the saved ARD path (or `NA`).
#'
#' @seealso [ars_render_split()] for one file per program,
#'   [ars_render_all()] for a single combined Word document.
#' @export
ars_render_combined <- function(ars_path, file, adam_dir = NULL, ard = NULL,
                                ard_file = NULL, output_ids = NULL,
                                max_rows = 500) {
  spec <- .read_json(ars_path)
  if (is.null(ard)) {
    if (is.null(adam_dir)) {
      cli::cli_abort(c(
        "x" = "No {.arg ard} supplied and no {.arg adam_dir} to compute one.",
        "i" = "Pass {.arg adam_dir} (the ADaM folder) or a precomputed {.arg ard} from {.fn ars_to_ard}."))
    }
    ard <- ars_to_ard(ars_path, adam_dir = adam_dir)
  }
  saved_ard <- NA_character_
  if (!is.null(ard_file)) {
    dir.create(dirname(ard_file), recursive = TRUE, showWarnings = FALSE)
    saveRDS(ard, ard_file); saved_ard <- ard_file
  }
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)

  want <- if (!is.null(output_ids)) tolower(trimws(output_ids)) else NULL
  fts <- list(); rows <- list()
  for (o in spec[["outputs"]]) {
    oid  <- .sc(o[["id"]])
    kind <- .classify_output(o)
    if (!is.null(want) &&
        !any(tolower(c(oid, .sc(o[["name"]]))) %in% want)) next

    if (kind == "figure") {
      rows[[length(rows) + 1L]] <- data.frame(
        output_id = oid, type = kind, status = "skipped",
        reason = "figures are not included in a combined RTF -- use ars_render_split()",
        stringsAsFactors = FALSE)
      next
    }
    res <- tryCatch({
      fts[[oid]] <- .output_to_flextable(ars_path, spec, ard, o, adam_dir, max_rows)
      "ok"
    }, error = function(e) conditionMessage(e))
    rows[[length(rows) + 1L]] <- data.frame(
      output_id = oid, type = kind,
      status = if (identical(res, "ok")) "rendered" else "error",
      reason = if (identical(res, "ok")) "" else res,
      stringsAsFactors = FALSE)
  }

  if (length(fts) == 0) {
    cli::cli_abort(c(
      "x" = "No table or listing output could be rendered into {.path {file}}.",
      "i" = "Inspect {.fn ars_diagnostics} and the manifest for per-output reasons."))
  }
  do.call(flextable::save_as_rtf, c(fts, list(path = file)))

  manifest <- do.call(rbind, rows)
  attr(manifest, "file")     <- file
  attr(manifest, "ard_file") <- saved_ard
  n_ok <- sum(manifest$status == "rendered")
  cli::cli_alert_success(
    "Wrote {n_ok} table/listing output{?s} into {.path {file}}{if (!is.na(saved_ard)) paste0(' (ARD: ', saved_ard, ')') else ''}.")
  invisible(manifest)
}
