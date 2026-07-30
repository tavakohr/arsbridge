## arsbridge -- parse_shell_docx.R
## ---------------------------------------------------------------------------
## Reads an annotated TLF shell Word document and returns a list of TLF
## section objects. Annotations are detected with a 4-layer hierarchy:
##   Layer 1: coloured runs (C00000 by default) confirmed by ADaM regex
##   Layer 2: bold / italic / underline runs confirmed by ADaM regex
##   Layer 3: plain-text ADaM regex match (works on unformatted shells)
##   Layer 4: LLM fallback -- NOT done here, handled by enrich_with_llm
##
## Walks the OOXML directly via xml2 (officer's high-level API doesn't
## expose font colour at the run level). All XPath uses local-name() to
## tolerate any namespace prefix binding.
##
## This file is the OOXML half only: the body walker, the table/header grid
## readers, and the run/cell readers that turn <w:r> nodes into the run list
## of seam 1. The annotation grammar, the detection layers, and section
## assembly live in R/parse_shell_core.R, shared with the xlsx reader.

## ---------------------------------------------------------------------------
## Constants (OOXML-only -- the shared ones live in parse_shell_core.R)
## ---------------------------------------------------------------------------

.TOC_FIRST_CELL_HINTS <- c("number", "table number", "tlf number", "tlf #")

#' Collect heading-shaped paragraphs that did NOT become sections, with the
#' reason each was rejected. Only called when the walk produced zero
#' sections, to turn the bare "No TLF sections found" into an actionable
#' message: the usual causes are a heading format the grammars do not
#' recognise, or headings living somewhere the body walker cannot see.
#' @noRd
.heading_near_misses <- function(children, heading_patterns = NULL,
                                 max_misses = 8L) {
  out <- list()
  for (ch in children) {
    if (length(out) >= max_misses) break
    if (.local_name(ch) != "p") next
    txt <- trimws(.paragraph_text(ch))
    if (!nzchar(txt)) next

    has_designator <- grepl("(?i)\\b(table|figure|listing)\\b", txt, perl = TRUE) &&
      grepl("\\d+\\.\\d+", txt)
    number_first <- grepl("^\\d{1,3}(?:\\.\\d+)+\\s+\\S", txt)
    if (!has_designator && !number_first) next

    res <- .match_tlf_heading(txt, heading_patterns)
    if (!is.null(res$hit)) next

    starts_with_designator <-
      grepl("(?i)^(table|figure|listing)\\b", txt, perl = TRUE)
    reason <- if (!is.na(res$reject_reason)) {
      res$reject_reason
    } else if (!starts_with_designator && number_first) {
      "number-first line with no Table/Figure/Listing designator (a section heading?)"
    } else if (!starts_with_designator) {
      "the Table/Figure/Listing designator is not at the start of the paragraph"
    } else {
      "the line does not fit any supported heading shape"
    }

    quoted <- if (nchar(txt) > 80) paste0(substr(txt, 1, 77), "...") else txt
    out[[length(out) + 1L]] <- list(text = quoted, reason = reason)
  }
  out
}

## ---------------------------------------------------------------------------
## Public entry point
## ---------------------------------------------------------------------------

#' Parse an annotated TLF shells Word document.
#'
#' Walks the document body in element order, splitting it into TLF sections.
#' For each section, extracts: tlf_number, tlf_type, title, population text,
#' population annotation, footnotes, source datasets, column headers, and
#' stub rows. Each stub row carries `has_annot`, `detection_method`, and
#' `detection_confidence` so downstream code can decide which rows to send
#' to the LLM enrichment step.
#'
#' @param docx_path Path to the annotated TLF shells `.docx`.
#' @param spec_lookup Optional `"DATASET.VARIABLE"`-keyed lookup (the
#'   `lookup` element of `parse_adam_spec()`). When supplied, listing
#'   column-header variable candidates are validated against the spec
#'   (tolerating mixed-case names) instead of relying on the ALL-CAPS
#'   token heuristic and its blocklist.
#' @param heading_patterns Optional character vector of PCRE patterns tried
#'   BEFORE the built-in heading grammars (see `.match_tlf_heading()`).
#' @param progress If `TRUE`, show a cli progress bar while walking the body
#'   (the slow step for a large shell). Default `FALSE`; `spec_to_ars()`
#'   turns it on together with `verbose`.
#'
#' @return List of TLF section objects (see top of file for full schema).
#'
#' @keywords internal
#' @noRd
parse_shell_docx <- function(docx_path, spec_lookup = NULL,
                             heading_patterns = NULL, progress = FALSE) {
  .validate_heading_patterns(heading_patterns)
  doc      <- .read_docx(docx_path)
  root_xml <- doc$doc_obj$get()
  body_xml <- xml2::xml_find_first(root_xml, ".//*[local-name()='body']")
  if (inherits(body_xml, "xml_missing")) {
    cli::cli_abort("Could not locate <w:body> in {.path {docx_path}}.")
  }

  ## A .docx is a zip archive. officer's parsed tree already gives us
  ## word/document.xml, but the comment text and the page-header parts live
  ## in sibling entries officer doesn't expose. Unzip the archive ONCE here
  ## and hand the directory to both readers below, rather than each reader
  ## unzipping the whole file again.
  docx_dir <- .unzip_docx(docx_path)
  on.exit(unlink(docx_dir, recursive = TRUE), add = TRUE)

  ## Word comments are a common annotation convention (the label stays
  ## plain text, the annotation lives in a comment anchored to the row) --
  ## read them once so .populate_table() can bind them to their stub cells.
  comments <- .read_docx_comments(docx_dir)

  ## Some sponsors put the TLF number, title, and population in the Word
  ## page header instead of the body (F2). Real page headers commonly
  ## repeat the same content across several header parts (first page /
  ## odd / even), so dedupe by content before deciding whether there is
  ## exactly one usable heading to draw from.
  header_headings <- .dedupe_by_signature(
    .read_header_headings(docx_dir, heading_patterns),
    function(h) paste(h$tlf_number, h$title, h$population_text, sep = "|")
  )

  ## One heads-up, not per paragraph, if the document has unaccepted
  ## tracked changes -- deleted text is already excluded from parsing (see
  ## .paragraph_text()/.runs_metadata()), but a shell mid-review can still
  ## carry stray edits worth flagging.
  if (!inherits(xml2::xml_find_first(body_xml, ".//*[local-name()='del' or local-name()='ins']"),
               "xml_missing")) {
    diag_add(
      stage = "parse_shell", severity = "INFO", input = INPUT_SHELL,
      problem = "Document contains tracked changes (unaccepted insertions/deletions)",
      location = basename(docx_path),
      action = "Deleted text is excluded from parsing; accept all revisions before running for a cleaner read"
    )
  }
  ## Text-box / callout text is excluded from whatever paragraph it is
  ## anchored inside (see .EXCLUDED_TEXT_ANCESTORS_XPATH) rather than
  ## misattributed to it; say so once so nothing looks silently dropped.
  if (!inherits(xml2::xml_find_first(body_xml, ".//*[local-name()='txbxContent']"),
               "xml_missing")) {
    diag_add(
      stage = "parse_shell", severity = "INFO", input = INPUT_SHELL,
      problem = "Document contains text boxes; their text is not read as part of the surrounding paragraph",
      location = basename(docx_path),
      action = "If a text box carries an annotation, move it into the cell/paragraph text directly"
    )
  }

  children   <- xml2::xml_children(body_xml)
  toc_skip   <- FALSE
  toc_table  <- .detect_toc_table(children)

  ## Table accounting: every top-level <w:tbl> in the body must be handled
  ## somewhere below -- attached to a section, recognised as the TOC, or
  ## skipped with a diagnostic. Counted here, checked after the walk, so a
  ## future code path that quietly `next`s past a table fails loudly.
  n_tbl_total   <- sum(vapply(children,
                              function(ch) identical(.local_name(ch), "tbl"),
                              logical(1)))
  n_tbl_handled <- 0L

  sections   <- list()
  current    <- NULL
  state      <- "BEFORE_HEADING"   ## BEFORE_HEADING / NEED_TITLE / NEED_POP / IN_BODY
  seen_table <- FALSE

  ## F2 (part 1): the body has no heading paragraph of its own anywhere,
  ## but the page header has exactly one usable heading and the body does
  ## have a table -- seed the section from the header before the walk
  ## starts. The loop below then runs completely unmodified: since no body
  ## paragraph will match the heading regex (that is this branch's own
  ## precondition), `current` just accumulates the table/footnotes/Source
  ## line normally, exactly like any other single-section document.
  body_has_heading <- any(vapply(children, function(ch) {
    if (.local_name(ch) != "p") return(FALSE)
    txt <- trimws(.paragraph_text(ch))
    nzchar(txt) && !is.null(.match_tlf_heading(txt, heading_patterns)$hit)
  }, logical(1)))
  body_has_table <- !inherits(
    xml2::xml_find_first(body_xml, ".//*[local-name()='tbl']"), "xml_missing")

  if (!body_has_heading && length(header_headings) == 1 && body_has_table) {
    h <- header_headings[[1]]
    current <- .new_section(
      tlf_number       = h$tlf_number,
      tlf_type         = h$tlf_type,
      title            = h$title,
      population_text  = h$population_text,
      population_annot = h$population_annot
    )
    state <- "IN_BODY"
    diag_add(
      stage = "parse_shell", severity = "INFO", input = INPUT_SHELL,
      problem = sprintf(
        "TLF %s: title/population sourced from the page header (no heading paragraph found in the document body)",
        h$tlf_number),
      tlf_number = h$tlf_number,
      action = "Verify the header-sourced title/population against the shell"
    )
  }

  ## Walking the body is the slow part of a large shell -- every table cell
  ## is read run-by-run. Show a determinate bar over the body elements so a
  ## long parse looks alive rather than hung. Off by default; spec_to_ars()
  ## turns it on with `verbose`.
  show_progress <- isTRUE(progress) && length(children) > 0L
  if (show_progress) {
    cli::cli_progress_bar("Reading shell layout", total = length(children))
  }

  for (child in children) {
    if (show_progress) cli::cli_progress_update()
    tag <- .local_name(child)

    ## Skip the cover/TOC table entirely.
    if (identical(child, toc_table)) {
      if (identical(tag, "tbl")) n_tbl_handled <- n_tbl_handled + 1L
      next
    }

    if (tag == "p") {
      text <- .paragraph_text(child)
      stripped <- trimws(text)
      if (!nzchar(stripped)) next

      ## TLF heading begins a new section.
      hit <- .match_tlf_heading(stripped, heading_patterns)$hit
      if (!is.null(hit)) {
        if (!is.null(current)) {
          sections[[length(sections) + 1]] <- .finalize_section(current, spec_lookup)
        }
        id <- .tlf_identity(hit)
        ## Whatever follows the number on the heading line -- an inline
        ## title, and possibly a dash-separated population, an annotation,
        ## and a [PROGRAMMING DATASETS USED: ...] suffix -- is split into
        ## its parts here.
        parts <- .decompose_heading_tail(hit$tail)
        current <- .new_section(
          tlf_number = id$tlf_number,
          tlf_type   = id$tlf_type,
          title      = parts$title
        )
        current$raw_heading <- stripped
        if (length(parts$source_datasets) > 0) {
          current$source_datasets <- parts$source_datasets
        }
        if (nzchar(parts$extra_annot)) {
          current$programmer_annotations <- c(current$programmer_annotations,
                                              parts$extra_annot)
        }
        ## A heading that already carries its population skips straight to
        ## the body; one with only a title waits for the population line;
        ## a bare heading waits for the title.
        if (nzchar(parts$population_text) || nzchar(parts$population_annot)) {
          current$population_text  <- parts$population_text
          current$population_annot <- parts$population_annot
          state <- "IN_BODY"
        } else if (nzchar(parts$title)) {
          state <- "NEED_POP"
        } else {
          state <- "NEED_TITLE"
        }
        seen_table <- FALSE
        next
      }

      if (is.null(current)) next

      if (state == "NEED_TITLE") {
        current$title <- stripped
        state <- "NEED_POP"
        next
      }

      if (state == "NEED_POP") {
        if (.looks_like_population(stripped)) {
          current$population_text  <- stripped
          current$population_annot <- .extract_population_annot(child, stripped)
          state <- "IN_BODY"
          next
        }
        ## Not the population line after all. A plain, unmarked paragraph
        ## here (no Source shape, no annotation, not a footnote) is almost
        ## always a second title line -- real population statements always
        ## match the lexicon above. Join it to the title and keep waiting
        ## for the real population line.
        is_source   <- grepl(.SOURCE_LINE_RE, stripped, ignore.case = TRUE, perl = TRUE)
        is_footnote <- .looks_like_footnote_lead(stripped)
        is_annot    <- .is_programmer_annotation_paragraph(child, stripped)
        if (!is_source && !is_footnote && !is_annot) {
          current$title <- trimws(paste(current$title, stripped))
          next
        }
        ## This shell has no population line at all (common for listings)
        ## -- stop waiting for one and let this same paragraph fall through
        ## to ordinary body handling below.
        state <- "IN_BODY"
      }

      current <- .triage_body_paragraph(current, child, stripped)

    } else if (tag == "tbl") {
      if (is.null(current)) {
        ## A table with no open section: its heading did not match (or it is
        ## cover-page furniture). Either way it is not parsed -- say so
        ## rather than dropping it silently, because "heading regex missed"
        ## looks identical to "nothing there" in the output.
        n_tbl_handled <- n_tbl_handled + 1L
        n_tbl_rows <- length(xml2::xml_find_all(child,
                                                "./*[local-name()='tr']"))
        diag_add(
          stage = "parse_shell", severity = "WARN", input = INPUT_SHELL,
          problem = sprintf(
            "A table with %d row(s) appears before any recognised TLF heading and was not parsed.",
            n_tbl_rows),
          location = basename(docx_path),
          action = "If this is a real display, its heading was not recognised -- check the heading text or pass heading_patterns; a cover/layout table can be ignored"
        )
        next
      }
      n_tbl_handled <- n_tbl_handled + 1L
      if (seen_table) {
        ## A further table under the SAME heading is a continuation: shells
        ## split one logical display across several Word tables when it runs
        ## over a page. Its stub rows belong to the display that is already
        ## open, so they are appended rather than dropped. The column header
        ## is re-stated on a continuation and must NOT overwrite the one
        ## already captured.
        current <- .append_continuation_table(current, child, comments)
        next
      }
      current    <- .populate_table(current, child, comments)
      seen_table <- TRUE
      ## Whatever we were waiting for (title, population), a table means
      ## the section is unambiguously in its body now -- a paragraph after
      ## the table must never be read as title/population text.
      state <- "IN_BODY"
    }
  }
  if (show_progress) cli::cli_progress_done()

  ## Close the table accounting opened before the walk. A mismatch means a
  ## code path above skipped a table without counting it -- a parser bug of
  ## the silently-dropped-tables kind, so it FAILs rather than warns.
  if (n_tbl_handled != n_tbl_total) {
    diag_add(
      stage = "parse_shell", severity = "FAIL", input = INPUT_SHELL,
      problem = sprintf(
        "Table accounting failed: the document body has %d table(s) but only %d were handled.",
        n_tbl_total, n_tbl_handled),
      location = basename(docx_path),
      action = "This is an arsbridge parsing bug: a table was skipped without a diagnostic. Do not trust this output; report the shell (or its structure digest)."
    )
  }

  if (!is.null(current)) {
    sections[[length(sections) + 1]] <- .finalize_section(current, spec_lookup)
  }

  ## F2 (part 2): the body DOES have its own heading paragraph (so
  ## tlf_number/tlf_type are already correct), but the title came out
  ## empty because it lives in the page header instead. Only attempted for
  ## a single-section document with exactly one usable header heading --
  ## with more than one section, which header belongs to which section is
  ## ambiguous, so nothing is guessed.
  if (length(sections) == 1 && !nzchar(sections[[1]]$title %||% "") &&
      length(header_headings) == 1) {
    h            <- header_headings[[1]]
    body_number  <- sections[[1]]$tlf_number
    numbers_match <- identical(h$tlf_number, body_number)

    if (numbers_match) {
      sections[[1]]$title <- h$title
      if (!nzchar(sections[[1]]$population_text %||% "")) {
        sections[[1]]$population_text  <- h$population_text
        sections[[1]]$population_annot <- h$population_annot
      }
      diag_add(
        stage = "parse_shell", severity = "INFO", input = INPUT_SHELL,
        problem = sprintf("TLF %s: title sourced from the page header",
                          body_number),
        tlf_number = body_number,
        action = "Verify the header-sourced title against the shell"
      )
    } else {
      ## The body heading and the page-header heading disagree on the TLF
      ## number, so the header almost certainly belongs to a different TLF
      ## (a stale template header, or a multi-TLF document). Adopting its
      ## title/population would silently mislabel this section -- and a wrong
      ## analysis-set attribution is expensive -- so we refuse and flag it.
      .diag_gap(
        stage = "parse_shell", severity = "WARN", input = INPUT_SHELL,
        problem = sprintf(
          "Body heading %s has no title, but the only page-header heading is %s -- numbers differ, so the header title was NOT adopted.",
          body_number, h$tlf_number),
        why = "A page header naming a different TLF is usually a stale template or belongs to another section; adopting it would mislabel this one.",
        fix = "Give this section its own title in the body, or make the page header's TLF number match.",
        tlf_number = body_number, location = h$title %||% ""
      )
    }
  }

  if (length(sections) == 0) {
    near_misses <- .heading_near_misses(children, heading_patterns)
    miss_bullets <- vapply(near_misses, function(nm) {
      sprintf("%s -- %s", dQuote(nm$text, q = FALSE), nm$reason)
    }, character(1))
    ## Document text goes through cli's glue interpolation -- escape any
    ## braces it happens to contain.
    cli_bullets <- gsub("}", "}}", gsub("{", "{{", miss_bullets, fixed = TRUE),
                        fixed = TRUE)
    if (length(cli_bullets) > 0) {
      names(cli_bullets) <- rep("x", length(cli_bullets))
    }
    cli::cli_warn(c(
      "No TLF sections found in {.path {docx_path}}.",
      "i" = "Expected paragraphs matching {.val Table X.X.X}, {.val Figure X.X.X}, or {.val Listing X.X.X} (a title, dash-separated population, and annotation may follow on the same line).",
      cli_bullets,
      "i" = .RECOMMENDED_HEADING_HINT
    ))
    diag_add(
      stage = "parse_shell", severity = "FAIL", input = INPUT_SHELL,
      problem = "No TLF sections found in shell document",
      location = basename(docx_path),
      action = if (length(miss_bullets) > 0) {
        paste0("Nothing parsed. Heading-shaped lines were seen but rejected: ",
               paste(miss_bullets, collapse = " | "))
      } else {
        "Nothing parsed -- check heading format (expected 'Table X.X.X' / 'Figure X.X.X' / 'Listing X.X.X')"
      }
    )
    attr(sections, "near_misses") <- near_misses
  } else {
    cli::cli_inform("Parsed {length(sections)} TLF section{?s} from {.path {basename(docx_path)}}")
  }

  ## Per-section parse-quality diagnostics: a section with stub rows but
  ## zero detected annotations is the classic symptom of an annotation
  ## convention the 4-layer detector does not recognise.
  for (sec in sections) {
    n_rows  <- length(sec$stub_rows)
    n_annot <- sum(vapply(sec$stub_rows, function(r) isTRUE(r$has_annot), logical(1)))
    ## A heading number was found but no title text -- the section will be
    ## labelled only by its number downstream. Say how to make the title
    ## identifiable (same guidance as the no-heading error and ?spec_to_ars).
    if (!nzchar(trimws(sec$title %||% ""))) {
      diag_add(
        stage = "parse_shell", severity = "WARN", input = INPUT_SHELL,
        problem = sprintf(
          "TLF %s: heading number found but no title text was identified",
          sec$tlf_number),
        tlf_number = sec$tlf_number,
        location = sec$tlf_number,
        action = .RECOMMENDED_HEADING_HINT
      )
    }
    if (n_rows > 0 && n_annot == 0) {
      diag_add(
        stage = "parse_shell", severity = "WARN", input = INPUT_SHELL,
        problem = sprintf("Section has %d stub row(s) but no annotations were detected", n_rows),
        tlf_number = sec$tlf_number,
        location = sec$title %||% "",
        action = "Section will rely entirely on LLM/fallback inference -- review annotation convention"
      )
    }
    if (n_rows == 0 && !identical(sec$tlf_type, "FIGURE")) {
      diag_add(
        stage = "parse_shell", severity = "WARN", input = INPUT_SHELL,
        problem = "No table rows captured for this section",
        tlf_number = sec$tlf_number,
        location = sec$title %||% "",
        action = "Check that the shell table directly follows the TLF heading"
      )
    }
    if (length(sec$source_datasets) == 0) {
      diag_add(
        stage = "parse_shell", severity = "INFO", input = INPUT_SHELL,
        problem = "No 'Source: ...' line found for this section",
        tlf_number = sec$tlf_number,
        location = sec$title %||% "",
        action = "Source datasets unknown; listing header dataset resolution falls back to ADSL"
      )
    }
    if (length(sec$programmer_annotations) > 0) {
      diag_add(
        stage = "parse_shell", severity = "INFO", input = INPUT_SHELL,
        problem = sprintf("%d programmer annotation line(s) captured outside the table stub",
                          length(sec$programmer_annotations)),
        tlf_number = sec$tlf_number,
        location = sec$title %||% "",
        action = "Kept for row binding and the validation report -- never shipped as footnotes"
      )
    }
  }

  sections
}

## ---------------------------------------------------------------------------
## Body walker -- table handler. Returns the updated `current` section.
## ---------------------------------------------------------------------------

#' Append a CONTINUATION table's rows to the display already open.
#'
#' A shell splits one logical display across several Word tables when it runs
#' over a page break. Those extra tables were dropped, losing every row they
#' held. Their stub rows belong to the open display, so they are appended.
#'
#' The column header is re-stated at the top of a continuation and must not
#' overwrite the geometry already captured (that is what carries the column
#' tree), so a leading header row is skipped -- but only when it really is
#' one: flagged with `<w:tblHeader/>`, or repeating the header text already
#' captured. A continuation that starts straight into data keeps every row.
#' @noRd
.append_continuation_table <- function(current, tbl_node, comments = list()) {
  rows <- xml2::xml_find_all(tbl_node, "./*[local-name()='tr']")
  if (length(rows) == 0) return(current)

  ## A continuation is the SAME display resuming after a page break, so it
  ## must have the same physical column count. A different count means this
  ## is a different table that happens to share the heading -- welding its
  ## rows on would misalign every one of them against the captured header,
  ## so it is refused loudly instead of merged silently.
  n_cols_here <- .grid_n_cols(tbl_node)
  n_cols_open <- current$.table_n_cols %||% n_cols_here
  if (n_cols_here != n_cols_open) {
    diag_add(
      stage = "parse_shell", severity = "FAIL", input = INPUT_SHELL,
      problem = sprintf(
        "A further table under %s has %d column(s) but the open display has %d -- its %d row(s) were NOT appended.",
        current$tlf_number %||% "?", n_cols_here, n_cols_open, length(rows)),
      tlf_number = current$tlf_number,
      location   = current$title %||% "",
      action = "If this is a separate display, give it its own TLF heading; if it is a true continuation, make its column layout match the first table"
    )
    return(current)
  }

  header_norm <- .norm_label(paste(current$col_headers %||% character(0),
                                   collapse = " "))
  first_cells <- xml2::xml_find_all(rows[[1]], "./*[local-name()='tc']")
  first_norm  <- .norm_label(paste(vapply(first_cells, .cell_text, character(1)),
                                   collapse = " "))
  repeats_header <- nzchar(header_norm) && nzchar(first_norm) &&
    identical(header_norm, first_norm)

  drop_first <- .is_flagged_header_row(rows[[1]]) || repeats_header
  data_rows <- if (drop_first) {
    if (length(rows) == 1L) list() else rows[seq.int(2L, length(rows))]
  } else {
    rows
  }

  collected <- .collect_stub_rows(data_rows, current, comments)
  .check_table_row_accounting(tbl_node,
                              n_header_rows = if (drop_first) 1L else 0L,
                              collected     = collected,
                              tlf_number    = current$tlf_number,
                              location      = current$title %||% "")
  added <- collected$rows
  if (length(added) == 0) return(current)

  current$stub_rows <- c(current$stub_rows %||% list(), added)
  diag_add(
    stage = "parse_shell", severity = "INFO", input = INPUT_SHELL,
    problem = sprintf(
      "A continuation table under %s contributed %d more row(s).",
      current$tlf_number %||% "?", length(added)),
    tlf_number = current$tlf_number,
    location = current$title %||% "",
    action = "Its rows were appended to this display, in document order"
  )
  current
}

#' Build stub-row records from a table's data rows. Extracted from
#' `.populate_table()` so a continuation table under the same heading
#' produces rows by exactly the same rules (annotation detection,
#' strikethrough scope, vMerge ghosts, comment and data-cell fallbacks).
#'
#' Returns `list(rows, n_empty, n_vmerge, n_struck)`: the stub-row records
#' plus a count for every row that was deliberately skipped. The counts feed
#' `.check_table_row_accounting()`, which requires every `<w:tr>` handed in
#' here to end up either as a row or in one of the skip counters -- a row
#' must never just vanish.
#' @noRd
.collect_stub_rows <- function(data_rows, current, comments = list()) {
  stub_rows <- list()
  n_empty  <- 0L
  n_vmerge <- 0L
  n_struck <- 0L
  for (row in data_rows) {
    cells <- xml2::xml_find_all(row, "./*[local-name()='tc']")
    if (length(cells) == 0) {
      ## A <w:tr> with no <w:tc> at all -- malformed or purely decorative.
      ## Rare enough to be worth a note; counted so it stays accounted for.
      n_empty <- n_empty + 1L
      diag_add(
        stage = "parse_shell", severity = "INFO", input = INPUT_SHELL,
        problem = sprintf("A table row in %s has no cells and was skipped.",
                          current$tlf_number %||% "?"),
        tlf_number = current$tlf_number,
        action = "Check the shell for an empty or malformed table row"
      )
      next
    }

    stub_cell <- cells[[1]]
    ## A vMerge-continuation stub cell is not a real row -- it is the
    ## bottom half of a vertically merged cell above it. Skip it so it
    ## doesn't show up as a blank ghost row.
    if (.is_vmerge_continuation(stub_cell)) {
      n_vmerge <- n_vmerge + 1L
      next
    }

    raw_text  <- .cell_text(stub_cell)
    runs_meta <- .runs_metadata(stub_cell)

    ## A fully struck-through stub is a row the shell author DELETED and
    ## left visible for review. Parsing it as live re-adds a dropped
    ## analysis to the deliverable, so it is skipped -- loudly, because
    ## the reviewer must be able to see that arsbridge dropped it.
    if (.all_text_struck(runs_meta)) {
      n_struck <- n_struck + 1L
      .diag_gap(
        stage = "parse_shell", severity = "INFO", input = INPUT_SHELL,
        problem = sprintf(
          "Row '%s' in %s is struck through in the shell.",
          trimws(raw_text), current$tlf_number),
        why = "Strikethrough marks a row the author removed from scope.",
        fix = "Delete the row outright if it should not be programmed, or clear the strikethrough if it should.",
        tlf_number = current$tlf_number, location = trimws(raw_text)
      )
      next
    }

    detection <- .detect_annotation(raw_text, runs_meta)
    ## Nothing found in the joined text: retry with the bare-join view for
    ## an annotation that wrapped across paragraphs without leaving the
    ## heuristic a signal (see .detect_annotation_wrapped()).
    if (!nzchar(detection$annotation)) {
      wrapped <- .detect_annotation_wrapped(.cell_paragraphs(stub_cell))
      if (!is.null(wrapped)) detection <- wrapped
    }

    row_entry <- list(
      label                = detection$label,
      annotation           = detection$annotation,
      has_annot            = nzchar(detection$annotation),
      detection_method     = detection$method,
      detection_confidence = detection$confidence,
      raw_text             = raw_text   ## unsplit cell, for the LLM extractor
    )

    ## A Word comment anchored to the stub cell is a deliberate,
    ## explicit annotation -- the label stays plain text and the
    ## annotation lives in the comment instead. Checked before the
    ## data-cell fallback below.
    if (!row_entry$has_annot) {
      comment_annot <- .cell_comment_annotation(stub_cell, comments)
      if (nzchar(comment_annot)) {
        row_entry$annotation           <- comment_annot
        row_entry$has_annot            <- TRUE
        row_entry$detection_method     <- "comment"
        row_entry$detection_confidence <- "high"
      }
    }

    ## A shell sometimes puts the annotation in a data cell (a red
    ## DATASET.VAR under the first treatment column) instead of the stub.
    ## Only look there when the stub cell itself carried nothing, so an
    ## in-cell stub annotation always wins. Column indices are physical
    ## grid columns (gridSpan expanded), not raw cell positions -- a
    ## spanned cell earlier in the row must not shift the reported column.
    if (!row_entry$has_annot && length(cells) > 1) {
      anchor_cols <- .cell_anchor_cols(cells)
      data_hit <- .detect_annotation_in_data_cells(cells[-1], comments,
                                                   anchor_cols[-1])
      if (!is.null(data_hit)) {
        row_entry$annotation           <- data_hit$annotation
        row_entry$has_annot            <- TRUE
        row_entry$detection_method     <- data_hit$method
        row_entry$detection_confidence <- data_hit$confidence
        diag_add(
          stage = "parse_shell", severity = "INFO", input = INPUT_SHELL,
          problem = sprintf(
            "Row '%s': annotation found in data column %d instead of the stub column",
            row_entry$label %||% "", data_hit$column_index),
          tlf_number = current$tlf_number,
          location   = row_entry$label %||% "",
          action     = "Bound to this row -- verify the shell's annotation placement"
        )
      }
    }

    stub_rows[[length(stub_rows) + 1L]] <- row_entry
  }
  list(rows = stub_rows, n_empty = n_empty, n_vmerge = n_vmerge,
       n_struck = n_struck)
}

#' Row-accounting invariant for one table: every `<w:tr>` must be classified
#' as a header row, a data row, or a skip with a known reason. Anything else
#' means the parser dropped a row with no trace -- exactly the failure mode
#' that once dropped whole tables from a deliverable without a message -- so
#' a mismatch is a FAIL, not a warning.
#' @noRd
.check_table_row_accounting <- function(tbl_node, n_header_rows, collected,
                                        tlf_number, location = "") {
  n_total <- length(xml2::xml_find_all(tbl_node, "./*[local-name()='tr']"))
  n_accounted <- n_header_rows + length(collected$rows) +
    collected$n_empty + collected$n_vmerge + collected$n_struck
  if (n_accounted == n_total) return(invisible(TRUE))
  diag_add(
    stage = "parse_shell", severity = "FAIL", input = INPUT_SHELL,
    problem = sprintf(
      "Row accounting failed for a table under %s: %d row(s) in the table but %d accounted for (%d header, %d data, %d cell-less, %d vMerge ghost, %d struck through).",
      tlf_number %||% "?", n_total, n_accounted, n_header_rows,
      length(collected$rows), collected$n_empty, collected$n_vmerge,
      collected$n_struck),
    tlf_number = tlf_number,
    location   = location,
    action = "This is an arsbridge parsing bug: a table row was dropped without a diagnostic. Do not trust this output; report the shell (or its structure digest)."
  )
  invisible(FALSE)
}

.populate_table <- function(current, tbl_node, comments = list()) {
  rows <- xml2::xml_find_all(tbl_node, "./*[local-name()='tr']")
  if (length(rows) == 0) return(current)

  ## Physical column count of this table, kept so a later table under the
  ## same heading is only accepted as a continuation when its geometry
  ## matches (see .append_continuation_table()).
  current$.table_n_cols <- .grid_n_cols(tbl_node)
  ## Also kept on the finished section (parse_decision_digest() reports it
  ## when diagnosing a header/column mismatch against the raw geometry).
  current$n_physical_cols <- current$.table_n_cols

  ## Header rows: normally just row 1, but a nested header (treatment arms
  ## spanning "n (%)" subcolumns) can use several leading rows. Word marks
  ## those with <w:trPr><w:tblHeader/>, but many shell authors never set
  ## "repeat header row", so we also infer an unflagged multi-row header
  ## from a spanned first row.
  flagged_header_rows <- .flagged_header_row_count(rows)
  if (flagged_header_rows > 0L) {
    n_header_rows <- flagged_header_rows
  } else {
    n_header_rows <- .inferred_header_row_count(rows)
    if (n_header_rows > 1L) {
      ## An inferred header is a guess the reviewer should confirm: if it is
      ## wrong it pulls a real data row up into the header (or vice versa).
      .diag_gap(
        stage = "parse_shell", severity = "WARN", input = INPUT_SHELL,
        problem = sprintf(
          "Table under %s appears to have a %d-row header but no row is flagged <w:tblHeader/>; inferred it from the spanned first row.",
          current$tlf_number, n_header_rows),
        why = "Without the repeat-header flag, a multi-row header is a heuristic guess -- a wrong guess mislabels columns or drops a data row.",
        fix = "Set 'Repeat as header row' on the header rows in Word, or verify the parsed column headers.",
        tlf_number = current$tlf_number, location = current$title %||% ""
      )
    }
  }
  header_rows <- rows[seq_len(n_header_rows)]
  ## The header decision, kept on the section so parse_decision_digest()
  ## can report what was DECIDED alongside what the raw geometry SHOWS.
  current$header_rows_flagged  <- flagged_header_rows
  current$header_rows_inferred <- if (flagged_header_rows > 0L) 0L else
    n_header_rows
  current$n_header_rows        <- n_header_rows

  if (identical(current$tlf_type %||% "TABLE", "TABLE")) {
    ## For tables, run the annotation detector on each header cell so a
    ## per-column filter ("Cohort 1 (N=XX) ADSL.COHORTN=1") separates into
    ## a clean display label and a condition. The conditions are resolved
    ## into column groups later, in .finalize_section().
    combined <- .combine_header_rows_detected(header_rows)
    keep     <- nzchar(combined$labels)
    current$col_headers <- combined$labels[keep]
    current$.pending_column_annotations <- list(
      labels      = combined$labels[keep],
      annotations = combined$annotations[keep]
    )
    ## Also keep the raw header-cell geometry (spans, vertical merges) so
    ## .finalize_section() can build the column tree. The flat labels above
    ## lose the parent-child structure of a multi-row header; the grid does
    ## not.
    current$.pending_header_grid <- .header_grid(header_rows)
  } else {
    headers <- .combine_header_rows(header_rows)
    headers <- headers[nzchar(headers)]
    current$col_headers <- headers
  }
  current$n_data_cols <- max(0L, length(current$col_headers) - 1L)

  ## For LISTING outputs, each column header carries the annotation rather
  ## than the stub column. We capture the raw cell content here but DEFER
  ## actual detection to `.finalize_section()` -- the "Source: ..." line
  ## that supplies the dataset prefix typically appears AFTER the table in
  ## document order, so source_datasets is still empty at this point.
  if (identical(current$tlf_type, "LISTING")) {
    header_cells <- xml2::xml_find_all(header_rows[[1]], "./*[local-name()='tc']")
    current$.pending_header_cells <- lapply(header_cells, function(c) {
      list(text = .cell_text(c), runs = .runs_metadata(c))
    })
  }

  ## Stub rows: everything after the header rows. Shared with the
  ## continuation-table path so both build rows identically.
  collected <- .collect_stub_rows(
    if (length(rows) > n_header_rows)
      rows[seq.int(n_header_rows + 1L, length(rows))] else list(),
    current, comments)
  .check_table_row_accounting(tbl_node,
                              n_header_rows = n_header_rows,
                              collected     = collected,
                              tlf_number    = current$tlf_number,
                              location      = current$title %||% "")
  current$stub_rows <- collected$rows

  current
}

#' TRUE if a row is explicitly flagged as a header row
#' (`<w:trPr><w:tblHeader/></w:trPr>` -- Word's "repeat as header row").
#' @noRd
.is_flagged_header_row <- function(row_node) {
  tr_pr <- xml2::xml_find_first(row_node, "./*[local-name()='trPr']")
  if (inherits(tr_pr, "xml_missing")) return(FALSE)
  header_flag <- xml2::xml_find_first(tr_pr, "./*[local-name()='tblHeader']")
  !inherits(header_flag, "xml_missing")
}

#' Number of leading rows explicitly flagged `<w:tblHeader/>`. This is how a
#' nested/spanned header with more than one row is marked in OOXML. Returns
#' 0 when no row carries the flag.
#' @noRd
.flagged_header_row_count <- function(rows) {
  n <- 0L
  for (row_node in rows) {
    if (!.is_flagged_header_row(row_node)) break
    n <- n + 1L
  }
  n
}

#' TRUE if any cell in the row spans more than one grid column (a spanned
#' header label like "Treatment A" sitting over its "n"/"(%)" subcolumns).
#' @noRd
.row_has_spanned_cell <- function(row_node) {
  cells <- xml2::xml_find_all(row_node, "./*[local-name()='tc']")
  for (cell in cells) {
    if (.grid_span(cell) > 1L) return(TRUE)
  }
  FALSE
}

#' TRUE if the row's first cell has no text -- the tell-tale shape of a
#' sub-header row (its stub column is empty while the "n"/"(%)" subcolumns
#' carry short tokens), as opposed to a data row whose first cell is a stub
#' label.
#' @noRd
.row_first_cell_blank <- function(row_node) {
  first_cell <- xml2::xml_find_first(row_node, "./*[local-name()='tc']")
  if (inherits(first_cell, "xml_missing")) return(FALSE)
  !nzchar(trimws(.cell_text(first_cell)))
}

#' Number of header rows to assume when NO row is flagged `<w:tblHeader/>`.
#' Row 1 is always a header. If row 1 spans columns, then each immediately
#' following row whose first cell is blank is taken to be a continuation
#' sub-header row too. Otherwise the header is the usual single row.
#' @noRd
.inferred_header_row_count <- function(rows) {
  if (length(rows) <= 1L) return(length(rows))
  if (!.row_has_spanned_cell(rows[[1]])) return(1L)

  n <- 1L
  for (i in 2:length(rows)) {
    if (!.row_first_cell_blank(rows[[i]])) break
    n <- n + 1L
  }
  n
}

#' Number of physical grid columns a cell covers, from `<w:gridSpan
#' w:val="N"/>`. 1 when the cell doesn't span multiple columns.
#' @noRd
.grid_span <- function(cell_node) {
  span_node <- xml2::xml_find_first(
    cell_node, "./*[local-name()='tcPr']/*[local-name()='gridSpan']")
  if (inherits(span_node, "xml_missing")) return(1L)
  span <- suppressWarnings(as.integer(xml2::xml_attr(span_node, "val")))
  if (is.na(span) || span < 1L) 1L else span
}

#' Expand one header row into one label per physical grid column, repeating
#' a spanned cell's label across every column it covers (a "Treatment A"
#' cell spanning its "n" / "(%)" subcolumns, for example).
#' @noRd
.expand_header_row <- function(row_node) {
  cells  <- xml2::xml_find_all(row_node, "./*[local-name()='tc']")
  labels <- character(0)
  for (cell in cells) {
    label <- trimws(.cell_text(cell))
    span  <- .grid_span(cell)
    labels <- c(labels, rep(label, span))
  }
  labels
}

#' Combine one or more header rows into a single label per grid column, so a
#' nested header ("Treatment A" spanning "n" / "(%)") produces one column
#' count and one label per real column rather than one per raw cell. Rows
#' are combined by column position; a shorter row is padded on the right.
#' @noRd
.combine_header_rows <- function(header_rows) {
  expanded <- lapply(header_rows, .expand_header_row)
  width    <- max(vapply(expanded, length, integer(1)), 0L)
  if (width == 0L) return(character(0))

  expanded <- lapply(expanded, function(row_labels) {
    length(row_labels) <- width   ## pads with NA if the row is shorter
    row_labels[is.na(row_labels)] <- ""
    row_labels
  })

  vapply(seq_len(width), function(col) {
    parts <- vapply(expanded, `[[`, character(1), col)
    parts <- unique(parts[nzchar(parts)])
    trimws(paste(parts, collapse = " "))
  }, character(1))
}

#' Expand one header row like `.expand_header_row()`, but run the 4-layer
#' annotation detector on each cell so the display label and any in-cell
#' annotation ("Cohort 1 (N=XX) ADSL.COHORTN=1") come back separated.
#' Returns list(labels, annotations), one element per physical grid column
#' (a spanned cell repeats over every column it covers).
#' @noRd
.expand_header_row_detected <- function(row_node) {
  cells       <- xml2::xml_find_all(row_node, "./*[local-name()='tc']")
  labels      <- character(0)
  annotations <- character(0)
  for (cell in cells) {
    ## Cell-level detection includes the bare-join retry: a narrow header
    ## cell is exactly where an annotation wraps mid-token.
    det  <- .detect_annotation_cell(cell)
    span <- .grid_span(cell)
    labels      <- c(labels, rep(trimws(det$label %||% .cell_text(cell)), span))
    annotations <- c(annotations, rep(det$annotation %||% "", span))
  }
  list(labels = labels, annotations = annotations)
}

#' Combine one or more header rows like `.combine_header_rows()`, but keep
#' the per-grid-column annotation alongside the annotation-stripped label.
#' The label combines down the rows with the same unique/paste logic; the
#' annotation is the FIRST non-empty one seen down a grid column (covers a
#' split header where row 1 holds "Cohort A" and row 2 holds
#' "(N=XX) ADSL.COHORTN=1").
#' @noRd
.combine_header_rows_detected <- function(header_rows) {
  expanded <- lapply(header_rows, .expand_header_row_detected)
  width    <- max(vapply(expanded, function(e) length(e$labels), integer(1)), 0L)
  if (width == 0L) {
    return(list(labels = character(0), annotations = character(0)))
  }

  pad <- function(x, fill) {
    length(x) <- width
    x[is.na(x)] <- fill
    x
  }
  expanded <- lapply(expanded, function(e) {
    list(labels = pad(e$labels, ""), annotations = pad(e$annotations, ""))
  })

  labels <- vapply(seq_len(width), function(col) {
    parts <- vapply(expanded, function(e) e$labels[[col]], character(1))
    parts <- unique(parts[nzchar(parts)])
    trimws(paste(parts, collapse = " "))
  }, character(1))

  annotations <- vapply(seq_len(width), function(col) {
    down <- vapply(expanded, function(e) e$annotations[[col]], character(1))
    down <- down[nzchar(down)]
    if (length(down) > 0) down[[1]] else ""
  }, character(1))

  list(labels = labels, annotations = annotations)
}

#' Capture the raw geometry of the header rows: one record per physical
#' header cell with its row, the grid columns it covers, its
#' annotation-stripped label, any in-cell annotation, and whether it is a
#' vertical-merge continuation. This is the un-flattened counterpart of
#' `.combine_header_rows_detected()` -- the input the column tree is built
#' from, where "Cohort A spans four child columns" is still visible.
#' @noRd
.header_grid <- function(header_rows) {
  grid <- list()
  for (r in seq_along(header_rows)) {
    cells <- xml2::xml_find_all(header_rows[[r]], "./*[local-name()='tc']")
    col <- 1L
    for (cell in cells) {
      span <- .grid_span(cell)
      raw  <- .cell_text(cell)
      det  <- .detect_annotation_cell(cell)
      grid[[length(grid) + 1L]] <- list(
        row             = r,
        col_start       = col,
        col_end         = col + span - 1L,
        text            = trimws(det$label %||% raw),
        annotation      = det$annotation %||% "",
        vmerge_continue = .is_vmerge_continuation(cell)
      )
      col <- col + span
    }
  }
  grid
}

#' TRUE when a cell is a `vMerge` continuation of the cell above it, not a
#' genuine new row. Per OOXML, `<w:vMerge/>` with no `val` attribute, or
#' `val="continue"`, means "continue the merge from above"; only
#' `val="restart"` starts a new merged cell.
#' @noRd
.is_vmerge_continuation <- function(cell_node) {
  vmerge <- xml2::xml_find_first(
    cell_node, "./*[local-name()='tcPr']/*[local-name()='vMerge']")
  if (inherits(vmerge, "xml_missing")) return(FALSE)
  val <- xml2::xml_attr(vmerge, "val")
  is.na(val) || !identical(tolower(val), "restart")
}

#' Scan the non-stub cells of a row for an annotation the stub cell itself
#' didn't carry -- either in the cell's own text/formatting, or in a Word
#' comment anchored to the cell. Returns the first hit as `list(annotation,
#' confidence, column_index, method)`, or `NULL` when nothing matches.
#' `anchor_cols`, when given, holds each cell's physical grid column (from
#' `.cell_anchor_cols()`), so a gridSpan cell earlier in the row does not
#' shift the reported column; without it the index falls back to the raw
#' cell position (1-based over the whole row including the stub column).
#' @noRd
.detect_annotation_in_data_cells <- function(data_cells, comments = list(),
                                             anchor_cols = NULL) {
  for (i in seq_along(data_cells)) {
    cell         <- data_cells[[i]]
    column_index <- if (!is.null(anchor_cols)) anchor_cols[[i]] else i + 1L

    text <- .cell_text(cell)
    hit  <- .detect_annotation(text, .runs_metadata(cell))
    if (nzchar(hit$annotation)) {
      return(list(annotation   = hit$annotation,
                  confidence   = hit$confidence,
                  column_index = column_index,
                  method       = "data_cell"))
    }

    comment_annot <- .cell_comment_annotation(cell, comments)
    if (nzchar(comment_annot)) {
      return(list(annotation   = comment_annot,
                  confidence   = "high",
                  column_index = column_index,
                  method       = "data_cell_comment"))
    }
  }
  NULL
}

## ---------------------------------------------------------------------------
## Word comments as an annotation channel (F4)
##
## A docx is a zip archive. The main document body (word/document.xml) is
## already available through officer's parsed tree, but a comment's TEXT
## lives in a sibling part, word/comments.xml, which officer does not
## expose. We read that one small file directly by unzipping the docx to a
## temp directory -- the same technique the fixture builder
## (tests/testthat/fixtures/build_fixtures.R) already uses to write test
## docx files.
## ---------------------------------------------------------------------------

#' Unzip a `.docx` to a fresh temp directory and return its path. The caller
#' owns cleanup (`unlink(dir, recursive = TRUE)`).
#' @noRd
.unzip_docx <- function(docx_path) {
  dir <- tempfile()
  dir.create(dir)
  utils::unzip(docx_path, exdir = dir)
  dir
}

#' Read `word/comments.xml` (if the docx has one) into a named list keyed
#' by comment id, each holding that comment's plain text. Returns an empty
#' list when the document has no comments part at all. `docx_dir` is an
#' already-unzipped docx directory (see `.unzip_docx()`).
#' @noRd
.read_docx_comments <- function(docx_dir) {
  comments_path <- file.path(docx_dir, "word", "comments.xml")
  if (!file.exists(comments_path)) return(list())

  comments_xml <- xml2::read_xml(comments_path)
  comment_nodes <- xml2::xml_find_all(comments_xml, ".//*[local-name()='comment']")

  out <- list()
  for (node in comment_nodes) {
    id   <- xml2::xml_attr(node, "id")
    text <- .paragraph_text(node)
    if (!is.na(id) && nzchar(trimws(text))) {
      out[[id]] <- trimws(text)
    }
  }
  out
}

#' The ADaM annotation carried by a comment anchored anywhere inside
#' `cell_node` (via `<w:commentReference w:id="...">`), or `""` when the
#' cell has no comment, the referenced comment has no text, or that text
#' doesn't contain a recognisable ADaM reference. Only the matching
#' portion of the comment is used, the same way Layer 3 reads a plain-text
#' cell -- a comment can be a full sentence ("Use ADSL.AGE for this row")
#' rather than a bare annotation.
#' @noRd
.cell_comment_annotation <- function(cell_node, comments) {
  if (length(comments) == 0) return("")

  ref <- xml2::xml_find_first(cell_node, ".//*[local-name()='commentReference']")
  if (inherits(ref, "xml_missing")) return("")

  id <- xml2::xml_attr(ref, "id")
  comment_text <- comments[[id]] %||% ""
  if (!nzchar(comment_text)) return("")

  matches <- regmatches(comment_text,
                        gregexpr(.ANNOTATION_PATTERN, comment_text, perl = TRUE))[[1]]
  if (length(matches) == 0) return("")
  .canon_annotation(paste(matches, collapse = " and "))
}

## ---------------------------------------------------------------------------
## Page-header titles and populations (F2)
##
## Some sponsors put the TLF number, title, and population in the Word
## section header (repeated on every page) instead of the document body,
## leaving the body as table + footnotes only. word/header*.xml parts are
## separate zip entries that officer's parsed tree does not expose, so they
## are read the same way word/comments.xml is above.
## ---------------------------------------------------------------------------

#' Drop consecutive-or-not duplicate list elements, keeping the first
#' occurrence of each distinct `sig_fn(item)` signature.
#' @noRd
.dedupe_by_signature <- function(items, sig_fn) {
  seen <- character(0)
  out  <- list()
  for (item in items) {
    sig <- sig_fn(item)
    if (sig %in% seen) next
    seen <- c(seen, sig)
    out[[length(out) + 1L]] <- item
  }
  out
}

#' Non-empty `<w:p>` nodes of one header part, in document order.
#' @noRd
.header_paragraphs <- function(header_xml_root) {
  paras <- xml2::xml_find_all(header_xml_root, ".//*[local-name()='p']")
  Filter(function(p) nzchar(trimws(.paragraph_text(p))), paras)
}

#' Read every `word/headerN.xml` part of a docx and, for each, look for a
#' TLF heading among its paragraphs using the same reading convention as
#' the body (heading paragraph, then title, then population). Returns a
#' list of heading records -- `list(tlf_number, tlf_type, title,
#' population_text, population_annot)` -- one per header part that
#' contains a recognisable heading. Header parts with no heading, or that
#' don't exist at all, contribute nothing (empty result is normal and safe
#' -- most page headers carry no TLF-shaped text). `docx_dir` is an
#' already-unzipped docx directory (see `.unzip_docx()`).
#' @noRd
.read_header_headings <- function(docx_dir, heading_patterns = NULL) {
  header_paths <- list.files(file.path(docx_dir, "word"),
                             pattern = "^header\\d*\\.xml$",
                             full.names = TRUE)
  headings <- list()

  for (path in header_paths) {
    paras <- .header_paragraphs(xml2::read_xml(path))
    if (length(paras) == 0) next

    ## Find the first paragraph that looks like a TLF heading.
    heading_at <- NULL
    hit <- NULL
    for (i in seq_along(paras)) {
      txt <- trimws(.paragraph_text(paras[[i]]))
      h   <- .match_tlf_heading(txt, heading_patterns)$hit
      if (!is.null(h)) {
        heading_at <- i
        hit        <- h
        break
      }
    }
    if (is.null(heading_at)) next

    id    <- .tlf_identity(hit)
    parts <- .decompose_heading_tail(hit$tail)

    ## Title: inline on the heading line itself, else the next paragraph.
    next_at <- heading_at + 1L
    title <- parts$title
    if (!nzchar(title) && next_at <= length(paras)) {
      title   <- trimws(.paragraph_text(paras[[next_at]]))
      next_at <- next_at + 1L
    }

    ## Population: already on the heading line, else the paragraph after
    ## the title -- only if it actually looks like one (reusing the same
    ## check the body walker uses).
    population_text  <- parts$population_text
    population_annot <- parts$population_annot
    if (!nzchar(population_text) && !nzchar(population_annot) &&
        next_at <= length(paras)) {
      pop_node <- paras[[next_at]]
      pop_text <- trimws(.paragraph_text(pop_node))
      if (.looks_like_population(pop_text)) {
        population_text  <- pop_text
        population_annot <- .extract_population_annot(pop_node, pop_text)
      }
    }

    headings[[length(headings) + 1L]] <- list(
      tlf_number       = id$tlf_number,
      tlf_type         = id$tlf_type,
      title            = title,
      population_text  = population_text,
      population_annot = population_annot
    )
  }

  headings
}

## ---------------------------------------------------------------------------
## Cell run extraction
## ---------------------------------------------------------------------------

## A tracked-change deletion (`<w:del>`) or a text box (`<w:txbxContent>`)
## should not contribute to a paragraph's or cell's plain text: deleted text
## is not really there any more, and a text box's text belongs to its own
## callout, not to whatever paragraph it happens to be anchored inside.
## Inserted text (`<w:ins>`) is NOT excluded -- it reads as normal text.
.EXCLUDED_TEXT_ANCESTORS_XPATH <-
  "not(ancestor::*[local-name()='del']) and not(ancestor::*[local-name()='txbxContent'])"

#' Concatenate all text under a paragraph/cell node, preserving order.
#' @noRd
.paragraph_text <- function(p_node) {
  t_nodes <- xml2::xml_find_all(
    p_node, paste0(".//*[local-name()='t'][", .EXCLUDED_TEXT_ANCESTORS_XPATH, "]"))
  .normalize_shell_text(paste(xml2::xml_text(t_nodes), collapse = ""))
}

#' The paragraph list of one table cell: trimmed, normalized, empties
#' dropped, order preserved. This is the cell's lossless view -- how the
#' paragraphs JOIN depends on who is asking (a label joins with a space, an
#' annotation joins bare), so the list itself is what both consumers build
#' from. A cell with no `<w:p>` children falls back to its own direct text.
#' @noRd
.cell_paragraphs <- function(cell_node) {
  paras <- xml2::xml_find_all(cell_node, "./*[local-name()='p']")
  if (length(paras) == 0) {
    txt <- .paragraph_text(cell_node)
    return(if (nzchar(txt)) txt else character(0))
  }
  parts <- trimws(vapply(paras, .paragraph_text, character(1)))
  parts <- parts[nzchar(parts)]
  vapply(parts, .normalize_shell_text, character(1), USE.NAMES = FALSE)
}

#' Text of one table cell. A stub label that wraps in Word is authored as
#' SEVERAL paragraphs in one cell ("Ongoing subjects at the time of the data"
#' / "extraction, n (%)"), and two categorical levels are sometimes authored
#' as two paragraphs of the same cell. Runs inside a paragraph join with
#' nothing (they are one word broken by formatting), but paragraphs join
#' with a SPACE -- concatenating them bare fuses words ("dataextraction") and
#' welds separate levels into one label.
#' @noRd
.cell_text <- function(cell_node) {
  parts <- .cell_paragraphs(cell_node)
  if (length(parts) == 0) return("")
  if (length(parts) == 1) return(parts[[1]])

  ## Join with a space -- EXCEPT where the break falls inside an annotation,
  ## which Word wraps mid-token in a narrow header cell:
  ##
  ##   "[ADSL.C" / "GHGR1N=1]"   ->  "[ADSL.CGHGR1N=1]"   (one reference)
  ##   "...of the data" / "extraction, n (%)"
  ##                             ->  "...of the data extraction, n (%)"
  ##
  ## A space in the first case truncates the variable to ADSL.C and loses the
  ## condition; no space in the second fuses two words. The break is inside an
  ## annotation when a bracket is still open, or when the text so far ends
  ## mid-reference (a dataset dot with an incomplete variable after it).
  out <- parts[1]
  for (i in seq_along(parts)[-1]) {
    open_bracket <- .unclosed_bracket(out)
    mid_ref <- grepl(paste0("\\b", .ADAM_DS, "\\.[A-Z0-9]*$"), out, perl = TRUE) &&
      grepl("^[A-Z0-9]", parts[i], perl = TRUE)
    out <- paste0(out, if (open_bracket || mid_ref) "" else " ", parts[i])
  }
  .normalize_shell_text(trimws(out))
}

#' TRUE when a run is struck through (`<w:strike/>` or `<w:dstrike/>`).
#' Word writes the property with no attribute when ON; `w:val="false"`/`"0"`
#' explicitly turns it OFF, which a style-inheriting run uses to opt out of
#' a struck paragraph style.
#' @noRd
.run_is_struck <- function(run_node) {
  for (tag in c("strike", "dstrike")) {
    node <- xml2::xml_find_first(
      run_node, sprintf("./*[local-name()='rPr']/*[local-name()='%s']", tag))
    if (inherits(node, "xml_missing")) next
    val <- xml2::xml_attr(node, "val")
    if (is.na(val) || !tolower(val) %in% c("false", "0", "off")) return(TRUE)
  }
  FALSE
}

#' The seam-1 run list for every run inside the node (see the contract at the
#' top of parse_shell_core.R). Each entry: list(text, raw_text, color_hex,
#' highlight, bold, italic, underline, strike). `raw_text` is the
#' pre-normalization text (curly quotes and other authoring artifacts intact)
#' -- kept alongside the normalized `text` so a run-integrity lint can see
#' what the shell author actually typed.
#' @noRd
.runs_metadata <- function(node) {
  runs <- xml2::xml_find_all(
    node, paste0(".//*[local-name()='r'][", .EXCLUDED_TEXT_ANCESTORS_XPATH, "]"))
  out  <- vector("list", length(runs))
  for (i in seq_along(runs)) {
    r <- runs[[i]]
    t_nodes <- xml2::xml_find_all(r, ".//*[local-name()='t']")
    raw <- paste(xml2::xml_text(t_nodes), collapse = "")
    txt <- .normalize_shell_text(raw)

    color_node <- xml2::xml_find_first(r, "./*[local-name()='rPr']/*[local-name()='color']")
    color <- if (inherits(color_node, "xml_missing")) NA_character_
             else toupper(xml2::xml_attr(color_node, "val") %||% NA_character_)

    highlight_node <- xml2::xml_find_first(r, "./*[local-name()='rPr']/*[local-name()='highlight']")
    highlight <- if (inherits(highlight_node, "xml_missing")) NA_character_
                 else tolower(xml2::xml_attr(highlight_node, "val") %||% NA_character_)

    bold      <- !inherits(xml2::xml_find_first(r, "./*[local-name()='rPr']/*[local-name()='b']"),  "xml_missing")
    italic    <- !inherits(xml2::xml_find_first(r, "./*[local-name()='rPr']/*[local-name()='i']"),  "xml_missing")
    underline <- !inherits(xml2::xml_find_first(r, "./*[local-name()='rPr']/*[local-name()='u']"),  "xml_missing")
    strike    <- .run_is_struck(r)

    out[[i]] <- list(text = txt, raw_text = raw, color_hex = color,
                     highlight = highlight, bold = bold, italic = italic,
                     underline = underline, strike = strike)
  }
  out
}

#' TRUE if any non-empty run of a paragraph carries an annotation colour or
#' highlight.
#' @noRd
.has_annotation_colour <- function(p_node) {
  meta <- .runs_metadata(p_node)
  any(vapply(meta, .is_annotation_styled_run, logical(1)))
}

#' TRUE if a paragraph has every non-empty run coloured grey 808080.
#' @noRd
.is_grey_paragraph <- function(p_node) {
  meta <- .runs_metadata(p_node)
  meta <- Filter(function(m) nzchar(m$text), meta)
  if (length(meta) == 0) return(FALSE)
  cols <- vapply(meta, function(m) m$color_hex %||% NA_character_, character(1))
  all(!is.na(cols) & cols == .GREY_HEX)
}

.TYPOGRAPHIC_QUOTES_RE <- "[\u2018\u2019\u201C\u201D]"

#' Lint one programmer-annotation paragraph for authoring corruption: raw
#' typographic quotes (the visual sign a line was hand-edited/pasted over
#' rather than typed fresh -- this package straightens them in memory, but a
#' chat assistant reading the raw file sees them as authored and can copy
#' them verbatim into a JSON string, breaking its syntax), and colour drift
#' mid-annotation (a line typed fresh is one run in one colour).
#' @return Character vector of human-readable issues (possibly empty).
#' @noRd
.lint_annotation_run_integrity <- function(p_node) {
  meta <- Filter(.is_annotation_styled_run, .runs_metadata(p_node))
  if (length(meta) == 0) return(character())
  issues <- character()

  raw_joined <- paste(vapply(meta, function(m) m$raw_text %||% "", character(1)),
                      collapse = "")
  if (grepl(.TYPOGRAPHIC_QUOTES_RE, raw_joined, perl = TRUE)) {
    issues <- c(issues, paste0(
      "uses typographic quotes instead of straight quotes -- this package ",
      "straightens them in memory, but a chat assistant reading the raw file ",
      "sees them as authored and can copy them into a JSON string verbatim, ",
      "breaking the JSON syntax of anything it writes back"
    ))
  }

  colours <- unique(vapply(meta, function(m) m$color_hex %||% NA_character_, character(1)))
  colours <- colours[!is.na(colours)]
  if (length(colours) > 1) {
    issues <- c(issues, sprintf(
      paste("mixes %d colours within one annotation (%s) -- an annotation",
           "typed fresh is one run in one colour; a colour change partway",
           "through usually means the line was pasted or hand-edited over"),
      length(colours), paste(colours, collapse = " then ")
    ))
  }
  issues
}

## ---------------------------------------------------------------------------
## Body-paragraph state-machine helpers (F1: flexible heading/title/
## population recognition)
## ---------------------------------------------------------------------------

#' TRUE if a body paragraph is a programmer mapping instruction (ADR 0003
#' Layer B): a coloured run, an ADaM DATASET.VARIABLE pattern, or a
#' "Label -> DATASET.VAR" arrow line. Shared by the population check below
#' and by `.triage_body_paragraph()`, so the two agree on what counts as an
#' annotation.
#' @noRd
.is_programmer_annotation_paragraph <- function(p_node, stripped) {
  if (.has_annotation_colour(p_node)) return(TRUE)
  if (grepl(.ANNOTATION_PATTERN, stripped, perl = TRUE)) return(TRUE)
  if (grepl(.ARROW_ANNOT_RE, stripped, perl = TRUE)) return(TRUE)
  FALSE
}

#' Route one ordinary body paragraph (title and population are already
#' settled) to Source datasets, programmer annotations, or footnotes.
#' Returns the updated section.
#' @noRd
.triage_body_paragraph <- function(current, p_node, stripped) {
  src <- regmatches(stripped, regexec(.SOURCE_LINE_RE, stripped,
                                      ignore.case = TRUE, perl = TRUE))[[1]]
  if (length(src) == 2 && .is_grey_paragraph(p_node)) {
    current$source_datasets <- .split_source_list(src[2])
    return(current)
  }
  ## Source line without grey -- still accept based on text pattern, as
  ## long as we haven't already captured a source list some other way.
  if (length(src) == 2 && !nzchar(paste(current$source_datasets, collapse = ""))) {
    current$source_datasets <- .split_source_list(src[2])
    return(current)
  }

  ## Programmer annotation (ADR 0003 Layer B) -- a mapping instruction for
  ## the programmer, routed to programmer_annotations and never shipped as
  ## a footnote.
  if (.is_programmer_annotation_paragraph(p_node, stripped)) {
    current$programmer_annotations <- c(current$programmer_annotations, stripped)
    issues <- .lint_annotation_run_integrity(p_node)
    for (issue in issues) {
      diag_add(
        stage = "parse_shell", severity = "WARN", input = INPUT_SHELL,
        problem = sprintf("Annotation line %s: \"%s\"", issue, stripped),
        tlf_number = current$tlf_number,
        location = current$title %||% "",
        action = paste("In the shell, delete this line and retype it fresh as",
                       "one run, one colour, straight quotes -- do not edit",
                       "the existing run in place")
      )
    }
    return(current)
  }

  ## Footnote markers or longer prose -> footnote bucket.
  if (grepl("^[\\[\\(]?[a-z\\d\\*]", stripped, perl = TRUE) ||
      nchar(stripped) > 10) {
    current$footnotes <- c(current$footnotes, stripped)
  }
  current
}

## ---------------------------------------------------------------------------
## Population paragraph annotation extraction
## ---------------------------------------------------------------------------

#' Concatenate all non-grey coloured runs of a paragraph; if none, fall back
#' to Layer 3 plain-text detection on the full paragraph text.
#' @noRd
.extract_population_annot <- function(p_node, full_text) {
  full_text <- .unwrap_bracket_instructions(full_text)$text
  meta <- .runs_metadata(p_node)
  coloured <- Filter(.is_annotation_styled_run, meta)
  if (length(coloured) > 0) {
    out <- paste(vapply(coloured, function(m) m$text, character(1)), collapse = "")
    out <- .unwrap_bracket_instructions(trimws(out))$text
    out <- sub("^\\[(.*)\\]$", "\\1", out, perl = TRUE)
    if (grepl(.ANNOTATION_PATTERN, out, perl = TRUE)) {
      return(.canon_annotation(out))
    }
  }
  ## Layer 3 on the population paragraph text.
  if (grepl(.ANNOTATION_PATTERN, full_text, perl = TRUE)) {
    ## Extract just the matching ADaM segment(s).
    m <- regmatches(full_text,
                    gregexpr(.ANNOTATION_PATTERN, full_text, perl = TRUE))[[1]]
    if (length(m) > 0) return(.canon_annotation(paste(m, collapse = " and ")))
  }
  ""
}

## ---------------------------------------------------------------------------
## Annotation detection, cell entry point
##
## The layers themselves are in parse_shell_core.R; this is the docx adapter
## that reads a `<w:tc>` node into the text and run list they expect.
## ---------------------------------------------------------------------------

#' Full annotation detection for one CELL: the normal path over the joined
#' cell text first, then -- only when that finds nothing and the cell has
#' several paragraphs -- the bare-join retry (`.detect_annotation_wrapped()`
#' in parse_shell_core.R).
#' @noRd
.detect_annotation_cell <- function(cell_node, runs_meta = NULL) {
  runs_meta <- runs_meta %||% .runs_metadata(cell_node)
  det <- .detect_annotation(.cell_text(cell_node), runs_meta)
  if (!nzchar(det$annotation)) {
    wrapped <- .detect_annotation_wrapped(.cell_paragraphs(cell_node))
    if (!is.null(wrapped)) det <- wrapped
  }
  det
}

## ---------------------------------------------------------------------------
## TOC table detection
## ---------------------------------------------------------------------------

#' Returns the xml node of the TOC table (if any) so the walker can skip it.
#' Heuristic: the first table whose first-row first-cell text contains
#' "number", "table number", "tlf number", or "tlf #".
#' @noRd
.detect_toc_table <- function(children) {
  for (child in children) {
    if (.local_name(child) != "tbl") next
    first_row <- xml2::xml_find_first(child, "./*[local-name()='tr']")
    if (inherits(first_row, "xml_missing")) next
    first_cell <- xml2::xml_find_first(first_row, "./*[local-name()='tc']")
    if (inherits(first_cell, "xml_missing")) return(NULL)
    txt <- tolower(trimws(.cell_text(first_cell)))
    if (any(vapply(.TOC_FIRST_CELL_HINTS,
                   function(h) grepl(h, txt, fixed = TRUE),
                   logical(1)))) {
      return(child)
    }
    return(NULL)   ## first table examined, not a TOC -- no further skip
  }
  NULL
}

.local_name <- function(node) sub("^.*:", "", xml2::xml_name(node))
