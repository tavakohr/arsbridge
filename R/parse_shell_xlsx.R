## arsbridge -- parse_shell_xlsx.R
## ---------------------------------------------------------------------------
## Reads an annotated TLF shell WORKBOOK and returns the same list of section
## objects `parse_shell_docx()` returns. One worksheet per output; the
## workbook's own legend sheet is skipped.
##
## This file is deliberately thin. The cells come from xlsx_cells.R, what the
## rows mean comes from xlsx_grid.R, and every decision about ANNOTATIONS --
## the grammar, the detection layers, the column-group resolution, the row
## binding, the section finalization -- comes from parse_shell_core.R,
## unchanged and shared with the Word reader. What is left here is the
## assembly: walking the sheets and filling in a section object.
##
## SECTION-OBJECT COMPATIBILITY CONTRACT
##
## Two readers producing "a section object" is worth nothing unless the two
## objects mean the same thing. Every field falls in exactly one class, and
## adr/0004-xlsx-shell-input.md is the long form of this:
##
##   Class 1 -- IDENTICAL SEMANTICS. The same shell content must give the same
##     value from either reader: tlf_number, tlf_type, title, population_text,
##     population_annot, footnotes, source_datasets, col_headers, n_data_cols,
##     column_annotation, column_groups, column_tree, include_total_hint, and
##     every stub row's label / annotation / has_annot. These are what
##     tools/parity_check_shell.R compares, and a difference is a bug in one
##     reader until justified.
##
##   Class 2 -- FORMAT-SPECIFIC, WHITELISTED. Fields that legitimately differ
##     because the formats differ: `detection_method` (namespaced per format --
##     see below), `raw_heading` (a Word heading paragraph has no Excel
##     equivalent; the sheet name is used), `header_rows_flagged` (always 0 in
##     Excel: there is no <w:tblHeader/> to flag), `header_rows_inferred`,
##     `n_header_rows`, `n_physical_cols`.
##
##   Class 3 -- ADDITIVE, EXCEL-ONLY. `sheet_name`, `source_format`,
##     `figure_spec`, `template_row`, `cell_grid`, `layout`. NO existing
##     consumer may require these. They exist so the fill writer can find its
##     way back to the cell a number belongs in; anything that reads a section
##     must still work when they are absent.
##
## Detection methods are namespaced ("xlsx_colour" rather than "colour") for
## one reason: the digest and the validation report both surface the method,
## and a reviewer looking at a finding needs to know whether a run's colour or
## a whole cell's font was the evidence. The has_annot / annotation values
## they accompany are class 1 and must match.

## A "source ADSL" clause inside a header annotation. The Word convention is a
## separate "Source: ..." paragraph under the table, which .SOURCE_LINE_RE
## already reads; an Excel shell has nowhere to put a loose line, so it states
## the source inside the stub-header annotation instead, usually without the
## colon.
.XLSX_SOURCE_CLAUSE_RE <-
  "(?i)^\\s*(?:data\\s+)?sources?(?:\\s+datasets?)?\\s*[:=]?\\s+(.+?)\\.?\\s*$"

## A doubled dot between dataset and variable ("ADSL..SAFFL") -- an authoring
## slip of the same family as the line-broken "ADSL. SAFFL" that
## .collapse_ds_dot_space() already repairs. Only ever applied as a
## last-resort, explicitly-reported retry (see .xlsx_population()).
.XLSX_DOUBLED_DOT_RE <- "\\b(AD[A-Z]{1,6})\\.{2,}([A-Z])"

#' Parse an annotated TLF shells Excel workbook.
#'
#' Mirrors `parse_shell_docx()`: same arguments, same return value, same
#' downstream contract. One TLF section per worksheet whose name (or, failing
#' that, whose first row) names an output.
#'
#' @param xlsx_path Path to the annotated TLF shells `.xlsx`.
#' @param spec_lookup Optional `"DATASET.VARIABLE"`-keyed lookup (the
#'   `lookup` element of `parse_adam_spec()`), used to validate listing
#'   column-header variable candidates.
#' @param heading_patterns Optional character vector of PCRE patterns tried
#'   BEFORE the built-in heading grammars, applied to the sheet name and to
#'   row 1 (see `.match_tlf_heading()`).
#' @param progress If `TRUE`, show a cli progress bar while walking the
#'   sheets.
#'
#' @return List of TLF section objects (see the compatibility contract at the
#'   top of this file).
#'
#' @keywords internal
#' @noRd
parse_shell_xlsx <- function(xlsx_path, spec_lookup = NULL,
                             heading_patterns = NULL, progress = FALSE) {
  .validate_heading_patterns(heading_patterns)
  wb <- xlsx_read_shell_cells(xlsx_path)

  sections <- list()
  skipped  <- character()
  if (isTRUE(progress)) {
    cli::cli_progress_bar("Reading sheets", total = length(wb$sheets))
  }
  for (sheet in wb$sheets) {
    if (isTRUE(progress)) cli::cli_progress_update()
    cls <- .classify_sheet(sheet, heading_patterns)

    ## The workbook's own legend is part of the workbook, not a sheet that
    ## failed to parse -- skipped without a complaint.
    if (identical(cls$role, "notes")) next

    if (identical(cls$role, "unknown")) {
      skipped <- c(skipped, sheet$name)
      .diag_gap(
        stage = "parse_shell", severity = "WARN", input = INPUT_SHELL,
        problem = sprintf("Worksheet %s was skipped: neither its name nor its first row names an output.",
                          dQuote(sheet$name, q = FALSE)),
        why = "Only a sheet arsbridge can identify becomes an output; an unnamed one would ship with no number or title.",
        fix = paste("Name the tab for its output ('Table 14.1.1'), or put the",
                    "output number in row 1."),
        location = sheet$name %||% "")
      next
    }

    sec <- .xlsx_section(sheet, cls, spec_lookup, heading_patterns)
    sections[[length(sections) + 1L]] <- .finalize_section(sec, spec_lookup)
  }
  if (isTRUE(progress)) cli::cli_progress_done()

  if (length(sections) == 0) {
    cli::cli_warn(c(
      "!" = "No TLF outputs found in {.path {basename(xlsx_path)}}.",
      "i" = paste("Each output needs its own worksheet, named for the output",
                  "(e.g. 'Table 14.1.1')."),
      "i" = if (length(skipped) > 0) {
        paste0("Sheets seen but not recognised: ",
               paste(skipped, collapse = ", "), ".")
      } else {
        "The workbook has no worksheets arsbridge could read."
      }
    ))
    diag_add(
      stage = "parse_shell", severity = "FAIL", input = INPUT_SHELL,
      problem = "No TLF outputs found in shell workbook",
      location = basename(xlsx_path),
      action = paste("Name each worksheet for its output",
                     "('Table 14.1.1' / 'Figure 14.3.1' / 'Listing 16.2.1'),",
                     "or put the output number in row 1 of the sheet"))
  } else {
    cli::cli_inform(
      "Parsed {length(sections)} TLF section{?s} from {.path {basename(xlsx_path)}}")
  }

  .diagnose_sections(sections)
  sections
}

#' Build one section object from one output worksheet.
#' @noRd
.xlsx_section <- function(sheet, cls, spec_lookup = NULL,
                          heading_patterns = NULL) {
  layout <- .locate_banner_rows(sheet, cls$tlf_type, cls$tlf_number,
                                heading_patterns)

  sec <- .new_section(tlf_number = cls$tlf_number, tlf_type = cls$tlf_type)
  ## Class 3, additive: where this section came from, so the fill writer can
  ## find its way back to the sheet and the cell.
  sec$source_format <- "xlsx"
  sec$sheet_name    <- sheet$name
  sec$layout        <- layout
  ## Which body cells are placeholders waiting for a number, and how many
  ## decimals each wants. Carried on the section because the workbook itself
  ## is long gone by the time the ARS is assembled.
  sec$cell_grid     <- .cell_placeholder_grid(sheet, layout)
  ## Class 2: an Excel header row cannot be flagged the way <w:tblHeader/>
  ## flags a Word one, so the count is always inferred.
  n_header <- length(layout$header_rows %||% integer(0))
  sec$header_rows_flagged  <- 0L
  sec$header_rows_inferred <- n_header
  sec$n_header_rows        <- n_header
  sec$n_physical_cols      <- sheet$n_cols

  ## Class 2: the Word reader keeps the heading paragraph verbatim. The
  ## nearest Excel equivalent is the sheet's own number row, or its name.
  sec$raw_heading <- if (!is.na(layout$number_row)) {
    .sheet_cell_text(sheet, layout$number_row, 1L)
  } else {
    sheet$name %||% ""
  }
  .xlsx_check_number_agreement(sheet, layout, cls)

  sec$title <- if (!is.na(layout$title_row)) {
    trimws(.sheet_cell_text(sheet, layout$title_row, 1L))
  } else ""

  pop <- .xlsx_population(sheet, layout, cls$tlf_number)
  sec$population_text  <- pop$text
  sec$population_annot <- pop$annotation

  sec <- .xlsx_headers(sec, sheet, layout)
  sec <- .xlsx_body(sec, sheet, layout)
  sec
}

#' Report a sheet whose name and whose row 1 name different outputs. The sheet
#' NAME wins: it is the one part of the convention that survives an inserted
#' row, and it is what the reviewer sees on the tab.
#' @noRd
.xlsx_check_number_agreement <- function(sheet, layout, cls) {
  if (is.na(layout$number_row) || identical(cls$source, "row1")) {
    return(invisible(NULL))
  }
  row1 <- .sheet_cell_text(sheet, layout$number_row, 1L)
  hit  <- .match_tlf_heading(row1)$hit
  if (is.null(hit)) return(invisible(NULL))
  if (identical(.tlf_identity(hit)$tlf_number, cls$tlf_number)) {
    return(invisible(NULL))
  }
  .diag_gap(
    stage = "parse_shell", severity = "WARN", input = INPUT_SHELL,
    problem = sprintf(
      "Worksheet %s starts with %s; the sheet name was used as the output number.",
      dQuote(sheet$name, q = FALSE), dQuote(row1, q = FALSE)),
    why = "The tab name and the sheet's own first row disagree about which output this is.",
    fix = "Rename the tab, or correct row 1, so the two agree.",
    tlf_number = cls$tlf_number, location = sheet$name %||% "")
  invisible(NULL)
}

#' The population line and its annotation.
#'
#' The annotation is read by the shared `.population_annot_from_runs()`, so a
#' shell that states its population the same way in Word and in Excel gives
#' the same answer from both. The one Excel-only addition is the last-resort
#' retry: when the line carries NO red at all and the plain-text scan also
#' finds nothing, a doubled dot is repaired and the scan retried, because
#' "ADSL..SAFFL='Y'" is a typo whose cost is an entire output's population
#' filter going unread. It is reported as low confidence every time.
#' @noRd
.xlsx_population <- function(sheet, layout, tlf_number = NA_character_) {
  out <- list(text = "", annotation = "")
  if (is.na(layout$population_row)) return(out)

  cells <- .sheet_row_cells(sheet, layout$population_row)
  if (nrow(cells) == 0) return(out)
  out$text <- trimws(cells$text[[1]])
  runs <- cells$runs[[1]]

  out$annotation <- .population_annot_from_runs(runs, out$text)
  if (nzchar(out$annotation)) return(out)

  ## Nothing found. Only a line with no red at all gets the typo retry -- a
  ## red line that failed to parse is a different problem, and guessing at it
  ## would hide it.
  if (any(vapply(runs, .is_annotation_styled_run, logical(1)))) {
    .diag_gap(
      stage = "parse_shell", severity = "WARN", input = INPUT_SHELL,
      problem = sprintf(
        "The population line of %s is annotated but no ADaM reference could be read from it.",
        tlf_number %||% "?"),
      why = "The output's population filter is unknown, so every count in it may be taken over the wrong subjects.",
      fix = "Write the population annotation as a plain reference, e.g. (ADSL.SAFFL='Y').",
      tlf_number = tlf_number, location = substr(out$text, 1, 80))
    return(out)
  }

  repaired <- gsub(.XLSX_DOUBLED_DOT_RE, "\\1.\\2", out$text, perl = TRUE)
  if (!identical(repaired, out$text)) {
    harvest <- .population_annot_from_runs(list(), repaired)
    if (nzchar(harvest)) {
      out$annotation <- harvest
      .diag_gap(
        stage = "parse_shell", severity = "WARN", input = INPUT_SHELL,
        problem = sprintf(
          "The population line of %s has a doubled dot; it was read as %s.",
          tlf_number %||% "?", dQuote(harvest, q = FALSE)),
        why = "The reference is a typo, so this reading is a repair rather than what the shell says.",
        fix = "Correct the shell to a single dot (ADSL.SAFFL), then re-run.",
        tlf_number = tlf_number, location = substr(out$text, 1, 80))
      return(out)
    }
  }

  diag_add(
    stage = "parse_shell", severity = "INFO", input = INPUT_SHELL,
    problem = "Population line carries no readable ADaM reference",
    tlf_number = tlf_number, location = substr(out$text, 1, 80),
    action = paste("Population text kept for the report; the filter will come",
                   "from the spec or the supplement instead"))
  out
}

#' Column headers.
#'
#' Tables hand the header row to the shared column-group machinery exactly as
#' the Word reader does -- the flat labels plus their annotations, and the
#' header grid for the tree. Listings DEFER detection to
#' `.finalize_section()`, which runs `.detect_listing_header_annotation()`
#' once the source dataset is known, again exactly as the Word reader does.
#'
#' The stub header of these shells carries the column-axis declaration
#' ("[columns -> ADSL.TRT01A; source ADSL]"). That is the arrow convention
#' `bind_annotations()` already understands, so it is routed to
#' `programmer_annotations` rather than being treated as a filter on the stub
#' column -- which is what makes the treatment axis resolve.
#' @noRd
.xlsx_headers <- function(sec, sheet, layout) {
  if (is.na(layout$header_row)) return(sec)
  cells <- .sheet_row_cells(sheet, layout$header_row)
  if (nrow(cells) == 0) return(sec)

  if (identical(sec$tlf_type, "LISTING")) {
    ## .finalize_section() runs the listing-header detector on these cells
    ## once the source dataset is known. The output-level instructions are
    ## lifted out first -- a "[row: ADAE.TRTEMFL='Y']" group left in the cell
    ## would be read as a second DISPLAY VARIABLE for that column, putting a
    ## row filter into the listing's output.
    directives <- .xlsx_header_directives(cells$text[[1]])
    sec$programmer_annotations <- c(sec$programmer_annotations,
                                    directives$annotations)
    sec$source_datasets <- unique(c(sec$source_datasets,
                                    directives$source_datasets))
    sec$.pending_header_cells <- lapply(seq_len(nrow(cells)), function(i) {
      cell <- list(text = cells$text[[i]], runs = cells$runs[[i]])
      if (i == 1L && length(directives$annotations) > 0) {
        cell <- .xlsx_column_only_cell(cell, directives$column_refs)
      }
      cell
    })
    sec$col_headers <- trimws(cells$text)
    sec$n_data_cols <- max(0L, length(sec$col_headers) - 1L)
    return(sec)
  }

  grid <- .grid_header_records(sheet, layout$header_rows %||% layout$header_row)

  ## The stub header's annotation declares the column axis, the source, and
  ## sometimes the denominator -- instructions about the OUTPUT, not a filter
  ## on the stub column. Hand them to the same binder the Word reader's
  ## below-table arrow lines go through.
  stub <- which(vapply(grid, function(g) {
    g$row == 1L && g$col_start == 1L
  }, logical(1)))
  if (length(stub) > 0 && nzchar(grid[[stub[[1]]]]$annotation)) {
    i <- stub[[1]]
    directives <- .xlsx_header_directives(cells$text[[1]])
    sec$programmer_annotations <- c(sec$programmer_annotations,
                                    directives$annotations)
    sec$source_datasets <- unique(c(sec$source_datasets,
                                    directives$source_datasets))
    ## A bare reference in the stub header is the stub column's own variable
    ## and stays with the column; everything else has been lifted out.
    grid[[i]]$annotation <- if (length(directives$column_refs) > 0) {
      directives$column_refs[[1]]
    } else ""
  }

  ## One label and annotation per PHYSICAL column, so a consumer that only
  ## understands a flat axis still gets one; the hierarchy travels on the grid.
  flat <- .xlsx_combine_header_grid(grid)
  keep <- nzchar(flat$labels)
  sec$col_headers <- flat$labels[keep]
  sec$.pending_column_annotations <- list(
    labels = flat$labels[keep], annotations = flat$annotations[keep])
  sec$.pending_header_grid <- grid
  sec$n_data_cols <- max(0L, length(sec$col_headers) - 1L)
  sec
}

#' Read the instructions carried by a header cell.
#'
#' A header cell can hold SEVERAL bracket groups, and they do not mean the
#' same thing:
#'
#'   Subject [ADAE.USUBJID]  [row: ADAE.TRTEMFL='Y'; source ADAE]
#'           `------------'   `-------------------------------'
#'            the column's     instructions about the OUTPUT
#'            own variable
#'
#' Detection joins the styled runs of a cell into ONE annotation, which is
#' right for a stub row and wrong here -- it would weld the column variable to
#' the row filter. So the groups are separated first (`.bracket_spans()`, the
#' same tokenizer the core grammar uses), then each group's ";"-separated
#' clauses are sorted by what they are.
#'
#' @return list(annotations, source_datasets, column_refs). `annotations` are
#'   instructions for `bind_annotations()` and the validation report --
#'   anything arsbridge cannot act on must still be visible to the reviewer.
#'   `column_refs` are bare `DATASET.VARIABLE` groups: the column's own
#'   variable, which belongs to the column, not to the output.
#' @noRd
.xlsx_header_directives <- function(cell_text) {
  text <- as.character(cell_text %||% "")
  out <- list(annotations = character(), source_datasets = character(),
              column_refs = character())

  spans <- .bracket_spans(text)
  groups <- if (length(spans) == 0) {
    trimws(text)
  } else {
    vapply(spans, function(sp) trimws(substr(text, sp[1] + 1L, sp[2] - 1L)),
           character(1))
  }
  groups <- groups[nzchar(groups)]

  for (group in groups) {
    ## A group that is nothing but a reference names this column's variable.
    if (.is_bare_variable_reference(group)) {
      out$column_refs <- c(out$column_refs, trimws(group))
      next
    }
    clauses <- trimws(strsplit(group, ";", fixed = TRUE)[[1]])
    for (clause in clauses[nzchar(clauses)]) {
      m <- regmatches(clause, regexec(.XLSX_SOURCE_CLAUSE_RE, clause,
                                      perl = TRUE))[[1]]
      if (length(m) == 2 && !grepl("->|\u2192", clause)) {
        out$source_datasets <- c(out$source_datasets, .split_source_list(m[2]))
        next
      }
      if (.is_bare_variable_reference(clause)) {
        out$column_refs <- c(out$column_refs, clause)
        next
      }
      out$annotations <- c(out$annotations, clause)
    }
  }
  out$source_datasets <- unique(out$source_datasets)
  out
}

#' Rewrite a listing header cell so its annotation names ONLY the column's own
#' variable, with the output-level instructions removed.
#'
#' The listing-header detector reads a cell's styled runs as the variables the
#' column displays. A stub header that also carries "[row: ADAE.TRTEMFL='Y']"
#' would therefore contribute TRTEMFL as a second display variable -- a row
#' filter shipped as a listing column. The instructions have already been
#' lifted to `programmer_annotations`; this replaces the styled runs with the
#' column references alone, so the detector sees the column and nothing else.
#' @noRd
.xlsx_column_only_cell <- function(cell, column_refs) {
  label <- trimws(strsplit(cell$text, "\n", fixed = TRUE)[[1]][[1]] %||% "")
  if (length(column_refs) == 0) {
    return(list(text = label,
                runs = list(list(text = label, raw_text = label,
                                 color_hex = NA_character_,
                                 highlight = NA_character_, bold = FALSE,
                                 italic = FALSE, underline = FALSE,
                                 strike = FALSE))))
  }
  annot <- paste0("[", paste(column_refs, collapse = ", "), "]")
  plain <- Filter(function(r) !.is_annotation_styled_run(r), cell$runs)
  styled_template <- Filter(.is_annotation_styled_run, cell$runs)
  styled <- if (length(styled_template) > 0) styled_template[[1]] else
    list(color_hex = "C00000", highlight = NA_character_, bold = FALSE,
         italic = TRUE, underline = FALSE, strike = FALSE)
  styled$text     <- paste0("\n", annot)
  styled$raw_text <- styled$text

  list(text = paste0(label, "\n", annot), runs = c(plain, list(styled)))
}

#' TRUE when the text is a bare `DATASET.VARIABLE` (or a small list of them)
#' and nothing else -- no operator, no instruction wording.
#' @noRd
.is_bare_variable_reference <- function(text) {
  text <- trimws(as.character(text %||% ""))
  if (!nzchar(text)) return(FALSE)
  stripped <- gsub(paste0("\\b", .ADAM_DS, "\\.", .ADAM_VAR, "\\b"), "",
                   text, perl = TRUE)
  ## Only separators may remain: "ADAE.ASTDT / AENDT" is still just columns.
  !grepl("[[:alnum:]]", gsub("[[:space:],/&+]|and\\b", "", stripped)) &&
    grepl(paste0("\\b", .ADAM_DS, "\\.", .ADAM_VAR, "\\b"), text, perl = TRUE)
}

#' The body: stub rows for a table, the template row for a listing, the
#' specification block for a figure, and the footnotes for any of them.
#' @noRd
.xlsx_body <- function(sec, sheet, layout) {
  body <- .collect_body_rows(sheet, layout)
  if (nrow(body) == 0) return(sec)

  sec$footnotes <- c(
    sec$footnotes,
    trimws(body$stub_text[body$kind == "footnote" &
                            nzchar(trimws(body$stub_text))]))

  if (identical(sec$tlf_type, "FIGURE")) {
    return(.xlsx_figure_body(sec, sheet, layout))
  }

  ## A red full-width line inside a table body is a programmer instruction
  ## written as its own row -- the same thing a below-table arrow line is in
  ## Word, and it goes to the same place.
  sec$programmer_annotations <- c(
    sec$programmer_annotations,
    trimws(body$stub_text[body$kind == "annotation" &
                            nzchar(trimws(body$stub_text))]))

  if (identical(sec$tlf_type, "LISTING")) {
    ## A listing's body is one template row standing for every data row; its
    ## columns are described by the headers, not by the row. Class 3.
    data_rows <- body$row[body$kind == "data"]
    sec$template_row <- if (length(data_rows) > 0) data_rows[[1]] else NA_integer_
    return(sec)
  }

  sec$stub_rows <- .xlsx_stub_rows(sheet, body, sec$tlf_number)
  sec
}

#' Stub rows of a table sheet.
#'
#' Every row that carries a stub label becomes a row -- both "data" rows and
#' "group" parent rows, because a parent ("Age group, n (%)" over its levels)
#' carries the annotation the levels are grouped by, and dropping it would
#' lose the grouping. Detection is the shared `.detect_annotation()`; only the
#' method name is namespaced, to record whether a run's colour or the whole
#' cell's font was the evidence.
#' @noRd
.xlsx_stub_rows <- function(sheet, body, tlf_number = NA_character_) {
  rows <- list()
  for (i in seq_len(nrow(body))) {
    if (!body$kind[[i]] %in% c("data", "group")) next
    cells <- .sheet_row_cells(sheet, body$row[[i]])
    if (nrow(cells) == 0 || cells$col[[1]] != 1L) next

    raw_text  <- cells$text[[1]]
    runs_meta <- cells$runs[[1]]
    if (!nzchar(trimws(raw_text))) next

    ## A struck-through row is one the author removed from scope; parsing it
    ## as live would re-add a dropped analysis. Same rule as the Word reader.
    if (.all_text_struck(runs_meta)) {
      .diag_gap(
        stage = "parse_shell", severity = "INFO", input = INPUT_SHELL,
        problem = sprintf("Row '%s' in %s is struck through in the shell.",
                          trimws(raw_text), tlf_number %||% "?"),
        why = "Strikethrough marks a row the author removed from scope.",
        fix = "Delete the row outright if it should not be programmed, or clear the strikethrough if it should.",
        tlf_number = tlf_number, location = trimws(raw_text))
      next
    }

    detection <- .detect_annotation(raw_text, runs_meta)
    rows[[length(rows) + 1L]] <- list(
      label                = detection$label,
      annotation           = detection$annotation,
      has_annot            = nzchar(detection$annotation),
      detection_method     = .xlsx_method_name(detection$method, runs_meta),
      detection_confidence = detection$confidence,
      raw_text             = raw_text,
      ## Class 3: which sheet row this came from, so the fill writer can put
      ## the number back where it belongs.
      sheet_row            = body$row[[i]],
      row_kind             = body$kind[[i]],
      is_template          = isTRUE(body$is_template[[i]])
    )
  }
  rows
}

#' Namespace a detection method so a reviewer can tell WHICH Excel convention
#' supplied the evidence: a red run inside a mixed cell, or a whole cell whose
#' font is red. Both reach detection as "colour"; only the run count
#' distinguishes them.
#' @noRd
.xlsx_method_name <- function(method, runs_meta) {
  if (is.null(method) || is.na(method)) return(NA_character_)
  if (!identical(method, "colour")) return(paste0("xlsx_", method))
  if (length(runs_meta) > 1L) "xlsx_rich_run" else "xlsx_cell_font"
}

#' A figure's specification block, attached as the class-3 `figure_spec`.
#'
#' A figure has no rows to annotate, so nothing here becomes a stub row. The
#' directives are what the ARD executor will need (what is on each axis, what
#' the series is, what the filter is); the unreadable lines are kept as
#' programmer annotations so they still reach the validation report.
#' @noRd
.xlsx_figure_body <- function(sec, sheet, layout) {
  fig <- .parse_figure_block(sheet, layout, sec$tlf_number)
  sec$figure_spec <- list(
    directives  = fig$directives,
    anchor_row  = fig$anchor_row,
    unmatched   = fig$unmatched
  )
  sec$source_datasets <- unique(c(sec$source_datasets, fig$source_datasets))
  sec$programmer_annotations <- c(sec$programmer_annotations, fig$unmatched)
  sec
}
