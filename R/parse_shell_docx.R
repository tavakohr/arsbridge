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

## ---------------------------------------------------------------------------
## Constants
## ---------------------------------------------------------------------------

.GREY_HEX <- "808080"   ## ignored as annotation -- source / disclaimer
.BLACK_HEXES <- c("000000", "FFFFFF", "AUTO", "NONE")

## Case-insensitive ("TABLE 14.1.1"), tolerates a trailing suffix letter
## ("Listing 16.2.1a"), single-number designators ("Figure 3"), and an
## optional inline title after a colon ("Table 14.1.1: Summary of
## Demographics") -- the third group is that inline title, empty when the
## heading is alone on its own paragraph. The colon is what tells a real
## inline title apart from body prose that just happens to mention a table
## number ("Table 14.1.1 shows the demographic summary" must NOT match): a
## trailing title is only ever recognised when it follows a literal ":".
.TLF_HEADING_RE <- "^(?i)(Table|Figure|Listing)\\s+(\\d{1,3}(?:\\.\\d+)*[a-z]?)\\s*(?::\\s*(.*))?\\s*$"
## Accepts "Source:", "Sources:", "Data Source:", "Source datasets:", and
## "=" in place of ":".
.SOURCE_LINE_RE <- "^\\s*(?:Data\\s+)?Sources?(?:\\s+datasets?)?\\s*[:=]\\s*(.+?)\\.?\\s*$"
## Words a genuine population / analysis-set statement almost always
## contains. Used to recognise the population line even when a shell has
## no line for it right after the title (or has an extra title line first).
.POPULATION_LEXICON_RE <- "(?i)population|analysis\\s+set|subjects|patients|safety|\\bITT\\b|\\bFAS\\b|\\bPP\\b"
## A CDISC population/analysis-set flag variable ("SAFFL", "ITTFL", "FASFL",
## "RANDFL", ...) -- always ends in "FL". Used to recognise a population line
## that has no population WORDING but whose own annotation is a flag
## reference ("(ADSL.SAFFL='Y')"). This must stay narrow: a treatment-column
## mapping line ("Treatment columns -> ADSL.TRT01A") also carries an
## annotation, and if it were read as the population it would never reach
## bind_annotations() as the column-axis grouping.
.POPULATION_FLAG_RE <- "\\b[A-Z][A-Z0-9]{1,7}FL\\b"
## Below-table annotation convention: "Label -> DATASET.VAR ..." (ASCII arrow
## or U+2192). The left side names one or more stub rows / the column axis;
## the right side is the annotation. Read by bind_annotations().
.ARROW_ANNOT_RE <- "^\\s*.+?\\s*(?:->|\u2192)\\s*.+$"
.TOC_FIRST_CELL_HINTS <- c("number", "table number", "tlf number", "tlf #")

## Multi-pattern union for annotation detection (Layer 3 + validation gate
## for Layers 1 and 2). Order matters: PCRE alternation returns the leftmost
## match of the leftmost branch that fits, so MORE-SPECIFIC patterns must
## come BEFORE the bare DATASET.VARIABLE form -- otherwise a string like
## "ADSL.SAFFL='Y'" matches the simple branch first and drops the ='Y' tail.
## Shared regex tokens .ADAM_DS / .ADAM_VAR come from R/aaa_constants.R.
.ANNOTATION_PATTERN <- paste(
  ## DATASET.VAR WHERE OTHERVAR='val'  (longest -- match first)
  paste0("\\b", .ADAM_DS, "\\.", .ADAM_VAR,
         "\\s+(?i:where)\\s+", .ADAM_VAR, "\\s*=\\s*'[^']*'"),
  ## DATASET.VAR EQ 'val' / NE 'val' / ... (ARS comparator form)
  paste0("\\b", .ADAM_DS, "\\.", .ADAM_VAR,
         "\\s+(?:EQ|NE|IN|NOTIN|GT|GE|LT|LE)\\s+'[^']*'"),
  ## DATASET.VAR='val'  (most common shell-annotation form)
  paste0("\\b", .ADAM_DS, "\\.", .ADAM_VAR, "\\s*=\\s*'[^']*'"),
  ## DATASET.VAR not null / not missing
  paste0("\\b", .ADAM_DS, "\\.", .ADAM_VAR,
         "\\s+(?i:not\\s+(?:null|missing))"),
  ## unique USUBJID in DATASET (count expression)
  paste0("(?i)unique\\s+USUBJID\\s+in\\s+", .ADAM_DS),
  ## Bare DATASET.VARIABLE  (least specific -- match last)
  paste0("\\b", .ADAM_DS, "\\.", .ADAM_VAR, "\\b"),
  sep = "|"
)

## ---------------------------------------------------------------------------
## Section object constructor
## ---------------------------------------------------------------------------

#' Build an empty TLF section object with all fields present. Used by both
#' the body walker (when a heading paragraph starts a section) and the F2
#' page-header seeding block, so the two can never drift apart on the field
#' set. `title`, `population_text`, and `population_annot` are filled in
#' later as the walk sees them.
#' @noRd
.new_section <- function(tlf_number, tlf_type, title = "",
                         population_text = "", population_annot = "") {
  list(
    tlf_number             = tlf_number,
    tlf_type               = tlf_type,
    title                  = title,
    population_text        = population_text,
    population_annot       = population_annot,
    footnotes              = character(),
    programmer_annotations = character(),
    source_datasets        = character(),
    col_headers            = character(),
    n_data_cols            = 0L,
    stub_rows              = list()
  )
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
#'
#' @return List of TLF section objects (see top of file for full schema).
#'
#' @keywords internal
#' @noRd
parse_shell_docx <- function(docx_path, spec_lookup = NULL) {
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
    .read_header_headings(docx_dir),
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
    nzchar(txt) &&
      length(regmatches(txt, regexec(.TLF_HEADING_RE, txt, perl = TRUE))[[1]]) == 4
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

  for (child in children) {
    tag <- .local_name(child)

    ## Skip the cover/TOC table entirely.
    if (identical(child, toc_table)) next

    if (tag == "p") {
      text <- .paragraph_text(child)
      stripped <- trimws(text)
      if (!nzchar(stripped)) next

      ## TLF heading begins a new section.
      m <- regmatches(stripped, regexec(.TLF_HEADING_RE, stripped, perl = TRUE))[[1]]
      if (length(m) == 4) {
        if (!is.null(current)) {
          sections[[length(sections) + 1]] <- .finalize_section(current, spec_lookup)
        }
        word         <- tools::toTitleCase(tolower(m[2]))
        number       <- m[3]
        prefix       <- substr(toupper(word), 1, 1)
        inline_title <- trimws(m[4])
        tlf_type     <- switch(tolower(word),
                               table   = "TABLE",
                               figure  = "FIGURE",
                               listing = "LISTING",
                               "TABLE")
        current <- .new_section(
          tlf_number = paste0(prefix, "-", gsub("\\.", "-", number)),
          tlf_type   = tlf_type,
          title      = inline_title
        )
        ## A heading with its title already on the same line ("Table
        ## 14.1.1: Summary...") skips straight to waiting for population.
        if (nzchar(inline_title)) {
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
      if (is.null(current)) next
      if (seen_table) {
        ## arsbridge models one table per TLF section; a second table here is
        ## dropped -- say so rather than lose it silently.
        .diag_gap(
          stage = "parse_shell", severity = "WARN", input = INPUT_SHELL,
          problem = sprintf("A second table was found under %s and was ignored.",
                            current$tlf_number),
          why = "arsbridge builds one table per TLF heading, so any extra table is dropped.",
          fix = "Give the extra table its own Table/Listing heading in the shell, or merge it into the first.",
          tlf_number = current$tlf_number, location = current$title %||% ""
        )
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
    cli::cli_warn(c(
      "No TLF sections found in {.path {docx_path}}.",
      "i" = "Expected paragraphs matching {.val Table X.X.X}, {.val Figure X.X.X}, or {.val Listing X.X.X}."
    ))
    diag_add(
      stage = "parse_shell", severity = "FAIL", input = INPUT_SHELL,
      problem = "No TLF sections found in shell document",
      location = basename(docx_path),
      action = "Nothing parsed -- check heading format (expected 'Table X.X.X' / 'Figure X.X.X' / 'Listing X.X.X')"
    )
  } else {
    cli::cli_inform("Parsed {length(sections)} TLF section{?s} from {.path {basename(docx_path)}}")
  }

  ## Per-section parse-quality diagnostics: a section with stub rows but
  ## zero detected annotations is the classic symptom of an annotation
  ## convention the 4-layer detector does not recognise.
  for (sec in sections) {
    n_rows  <- length(sec$stub_rows)
    n_annot <- sum(vapply(sec$stub_rows, function(r) isTRUE(r$has_annot), logical(1)))
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

.populate_table <- function(current, tbl_node, comments = list()) {
  rows <- xml2::xml_find_all(tbl_node, "./*[local-name()='tr']")
  if (length(rows) == 0) return(current)

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

  headers <- .combine_header_rows(header_rows)
  headers <- headers[nzchar(headers)]
  current$col_headers <- headers
  current$n_data_cols <- max(0L, length(headers) - 1L)

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

  ## Stub rows: everything after the header rows.
  stub_rows <- list()
  if (length(rows) > n_header_rows) {
    data_rows <- rows[seq.int(n_header_rows + 1L, length(rows))]
    for (row in data_rows) {
      cells <- xml2::xml_find_all(row, "./*[local-name()='tc']")
      if (length(cells) == 0) next

      stub_cell <- cells[[1]]
      ## A vMerge-continuation stub cell is not a real row -- it is the
      ## bottom half of a vertically merged cell above it. Skip it so it
      ## doesn't show up as a blank ghost row.
      if (.is_vmerge_continuation(stub_cell)) next

      raw_text  <- .cell_text(stub_cell)
      runs_meta <- .runs_metadata(stub_cell)
      detection <- .detect_annotation(raw_text, runs_meta)

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
      ## in-cell stub annotation always wins.
      if (!row_entry$has_annot && length(cells) > 1) {
        data_hit <- .detect_annotation_in_data_cells(cells[-1], comments)
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
  }
  current$stub_rows <- stub_rows

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
#' confidence, column_index, method)` (column_index is 1-based over the whole
#' row including the stub column), or `NULL` when nothing matches.
#' @noRd
.detect_annotation_in_data_cells <- function(data_cells, comments = list()) {
  for (i in seq_along(data_cells)) {
    cell         <- data_cells[[i]]
    column_index <- i + 1L   ## +1: data_cells excludes the stub column

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
  paste(matches, collapse = " and ")
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
.read_header_headings <- function(docx_dir) {
  header_paths <- list.files(file.path(docx_dir, "word"),
                             pattern = "^header\\d*\\.xml$",
                             full.names = TRUE)
  headings <- list()

  for (path in header_paths) {
    paras <- .header_paragraphs(xml2::read_xml(path))
    if (length(paras) == 0) next

    ## Find the first paragraph that looks like a TLF heading.
    heading_at <- NULL
    m <- NULL
    for (i in seq_along(paras)) {
      txt <- trimws(.paragraph_text(paras[[i]]))
      mm  <- regmatches(txt, regexec(.TLF_HEADING_RE, txt, perl = TRUE))[[1]]
      if (length(mm) == 4) {
        heading_at <- i
        m          <- mm
        break
      }
    }
    if (is.null(heading_at)) next

    word         <- tools::toTitleCase(tolower(m[2]))
    prefix       <- substr(toupper(word), 1, 1)
    tlf_type     <- switch(tolower(word), table = "TABLE", figure = "FIGURE",
                           listing = "LISTING", "TABLE")
    inline_title <- trimws(m[4])

    ## Title: inline on the heading line itself, else the next paragraph.
    next_at <- heading_at + 1L
    title <- inline_title
    if (!nzchar(title) && next_at <= length(paras)) {
      title   <- trimws(.paragraph_text(paras[[next_at]]))
      next_at <- next_at + 1L
    }

    ## Population: the paragraph after the title, only if it actually
    ## looks like one (reusing the same check the body walker uses).
    population_text  <- ""
    population_annot <- ""
    if (next_at <= length(paras)) {
      pop_node <- paras[[next_at]]
      pop_text <- trimws(.paragraph_text(pop_node))
      if (.looks_like_population(pop_text)) {
        population_text  <- pop_text
        population_annot <- .extract_population_annot(pop_node, pop_text)
      }
    }

    headings[[length(headings) + 1L]] <- list(
      tlf_number       = paste0(prefix, "-", gsub("\\.", "-", m[3])),
      tlf_type         = tlf_type,
      title            = title,
      population_text  = population_text,
      population_annot = population_annot
    )
  }

  headings
}

## ---------------------------------------------------------------------------
## Convention-agnostic annotation binding (ADR 0003 Layer A)
## ---------------------------------------------------------------------------

#' Normalise a label for fuzzy matching: lowercase, punctuation and
#' indentation stripped, whitespace collapsed.
#' @noRd
.norm_label <- function(x) {
  x <- tolower(trimws(as.character(x %||% "")))
  x <- gsub("[[:punct:]]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}

## Substring containment only kicks in once the shorter string is at least
## this long. Otherwise a one- or two-character stub label like "n" or "%"
## substring-matches almost any longer phrase (e.g. "n" is inside "treatment
## columns"), binding an annotation to the wrong row.
.MIN_SUBSTRING_MATCH_CHARS <- 3L

#' Index of the stub row whose label matches `lhs_norm`: exact normalised
#' match first, then prefix (either direction), then substring containment.
#' Returns NA when nothing matches.
#' @noRd
.match_stub_label <- function(lhs_norm, labels_norm) {
  if (!nzchar(lhs_norm)) return(NA_integer_)

  hit <- which(labels_norm == lhs_norm)
  if (length(hit)) return(hit[1])

  hit <- which(nzchar(labels_norm) &
                 (startsWith(labels_norm, lhs_norm) |
                    startsWith(lhs_norm, labels_norm)))
  if (length(hit)) return(hit[1])

  contains_either <- function(l) {
    if (!nzchar(l)) return(FALSE)
    shorter <- min(nchar(l), nchar(lhs_norm))
    if (shorter < .MIN_SUBSTRING_MATCH_CHARS) return(FALSE)
    grepl(lhs_norm, l, fixed = TRUE) || grepl(l, lhs_norm, fixed = TRUE)
  }
  hit <- which(vapply(labels_norm, contains_either, logical(1)))
  if (length(hit)) return(hit[1])

  NA_integer_
}

## Left-side phrases that name the column (treatment) axis rather than a row.
.COLUMN_AXIS_PHRASES <- c(
  "treatment column", "treatment columns", "column", "columns",
  "treatment group", "treatment groups", "treatment arm", "treatment arms",
  "treatment"
)

#' Bind programmer annotations to their stub rows regardless of placement.
#'
#' Reads the `Label -> annotation` lines collected in
#' `sec$programmer_annotations` and binds each to the stub row whose label
#' fuzzy-matches the left side; sets that row's `annotation`,
#' `has_annot = TRUE`, `detection_method = "below_table_arrow"`. A left side
#' naming the column axis is stored as `sec$column_annotation`
#' ("DATASET.VARIABLE"); one matching the population line fills
#' `population_annot` when empty. In-cell detections always win: only rows
#' still `has_annot = FALSE` are bound. Unmatched lines stay in
#' `programmer_annotations` untouched (they still reach the validation
#' report). A multi-label left side ("Completed / Discontinued") splits on
#' "/" and, when the right side is `DS.VAR (v1 / v2)` with matching
#' cardinality, each row binds to its own `DS.VAR='v_i'`.
#'
#' @noRd
bind_annotations <- function(sec) {
  anns <- as.character(sec$programmer_annotations %||% character())
  if (length(anns) == 0) return(sec)

  labels_norm <- vapply(sec$stub_rows %||% list(),
                        function(r) .norm_label(r$label), character(1))
  pop_norm    <- .norm_label(sec$population_text)
  var_ref_re  <- paste0("\\b", .ADAM_DS, "\\.", .ADAM_VAR, "\\b")

  ## Compound lines carry several clauses separated by ";":
  ## "Subject -> ADAE.USUBJID ; Treatment -> ADAE.TRT01A".
  clauses <- unlist(lapply(anns, function(line)
    trimws(strsplit(line, ";", fixed = TRUE)[[1]])))
  clauses <- clauses[nzchar(clauses)]

  for (clause in clauses) {
    ## Split on the first arrow; fall back to the first colon ONLY when the
    ## left side matches something (a stub row / the column axis / the
    ## population line) -- a plain "Note: ..." must not bind.
    pos <- regexpr("->|\u2192", clause)
    if (pos > 0) {
      lhs <- substr(clause, 1, pos - 1)
      rhs <- substr(clause, pos + attr(pos, "match.length"), nchar(clause))
    } else {
      cpos <- regexpr(":", clause, fixed = TRUE)
      if (cpos <= 1) next
      lhs <- substr(clause, 1, cpos - 1)
      rhs <- substr(clause, cpos + 1, nchar(clause))
    }
    rhs <- trimws(rhs)
    if (!nzchar(rhs)) next
    lhs_full_norm <- .norm_label(lhs)
    if (!nzchar(lhs_full_norm)) next

    ## Left-side row candidates: the full label first (so "Start/Stop"
    ## matches its own row), then the "/"-split pieces (so
    ## "Completed / Discontinued" binds two rows).
    lhs_labels <- trimws(strsplit(lhs, "/", fixed = TRUE)[[1]])
    lhs_labels <- lhs_labels[nzchar(lhs_labels)]
    full_hit <- .match_stub_label(lhs_full_norm, labels_norm)
    if (!is.na(full_hit) || length(lhs_labels) == 0) {
      lhs_labels <- lhs
    }

    ## Multi-label left side + "DS.VAR (v1 / v2 ...)" right side with the
    ## same cardinality -> one value-filter annotation per label.
    per_values <- NULL
    pm <- regmatches(rhs, regexec(
      paste0("^\\s*(", .ADAM_DS, "\\.", .ADAM_VAR, ")\\s*\\(([^)]+)\\)\\s*$"),
      rhs, perl = TRUE))[[1]]
    if (length(pm) == 3 && length(lhs_labels) > 1) {
      vals <- trimws(strsplit(pm[3], "[/,]")[[1]])
      vals <- vals[nzchar(vals)]
      if (length(vals) == length(lhs_labels)) {
        per_values <- list(var = pm[2], vals = vals)
      }
    }

    ## 1. Stub rows claim the left side first.
    matched_any <- FALSE
    for (k in seq_along(lhs_labels)) {
      idx <- .match_stub_label(.norm_label(lhs_labels[k]), labels_norm)
      if (is.na(idx)) next
      matched_any <- TRUE
      if (isTRUE(sec$stub_rows[[idx]]$has_annot)) next   ## in-cell wins
      ann_k <- if (!is.null(per_values)) {
        paste0(per_values$var, "='", per_values$vals[k], "'")
      } else rhs
      sec$stub_rows[[idx]]$annotation           <- ann_k
      sec$stub_rows[[idx]]$has_annot            <- TRUE
      sec$stub_rows[[idx]]$detection_method     <- "below_table_arrow"
      sec$stub_rows[[idx]]$detection_confidence <- "high"
    }
    if (matched_any) next

    ## 2. Column-axis annotation: "Treatment columns -> ADSL.TRT01A",
    ## "Column N and treatment -> ...", or an exact column-header match.
    is_col_lhs <- lhs_full_norm %in% .COLUMN_AXIS_PHRASES ||
      grepl("\\b(column|columns|treatment)\\b", lhs_full_norm) ||
      any(vapply(sec$col_headers %||% character(), function(h) {
        hn <- .norm_label(h)
        nzchar(hn) && identical(hn, lhs_full_norm)
      }, logical(1)))
    if (is_col_lhs) {
      ref <- regmatches(rhs, regexpr(var_ref_re, rhs, perl = TRUE))
      if (length(ref) == 1 && is.null(sec$column_annotation)) {
        sec$column_annotation <- toupper(ref)
      }
      next
    }

    ## 3. Population annotation.
    if (nzchar(pop_norm) &&
        !is.na(.match_stub_label(lhs_full_norm, pop_norm)) &&
        !nzchar(sec$population_annot %||% "")) {
      sec$population_annot <- rhs
    }
  }

  sec
}

#' Resolve deferred listing-header detection now that the section is complete.
#'
#' By the time a section is pushed to `sections`, the entire body of the TLF
#' has been seen -- including the "Source: ..." line that provides the
#' primary source dataset. This is where we run `.detect_listing_header_annotation()`
#' on the saved header cells, attach `header_rows`, and append annotated
#' headers to `stub_rows` for uniform downstream processing.
#'
#' @noRd
.finalize_section <- function(sec, spec_lookup = NULL) {
  ## Bind below-table / arrow-form annotations to their rows first, so the
  ## per-section "no annotations detected" diagnostic and everything
  ## downstream see the bound rows (ADR 0003 Layer A).
  sec <- bind_annotations(sec)
  pending <- sec$.pending_header_cells
  if (length(pending %||% list()) == 0) return(sec)
  if (!identical(sec$tlf_type, "LISTING")) {
    sec$.pending_header_cells <- NULL
    return(sec)
  }

  ## Pull the first source dataset and strip any "(CONDITION)" suffix --
  ## e.g. "ADAE (TRTEMFL='Y')" -> "ADAE".
  source_ds <- if (length(sec$source_datasets) > 0) sec$source_datasets[1] else ""
  source_ds <- trimws(sub("\\s*\\(.*$", "", source_ds))
  if (!nzchar(source_ds)) {
    diag_add(
      stage = "parse_shell", severity = "WARN", input = INPUT_SHELL,
      problem = "Listing has no source dataset; header variables defaulted to ADSL",
      tlf_number = sec$tlf_number,
      location = sec$title %||% "",
      action = "Defaulted dataset prefix to ADSL -- verify each listing variable's dataset"
    )
    source_ds <- "ADSL"
  }

  hdr_rows <- vector("list", length(pending))
  for (j in seq_along(pending)) {
    p <- pending[[j]]
    d <- .detect_listing_header_annotation(p$text, p$runs, source_ds,
                                           spec_lookup = spec_lookup)
    hdr_rows[[j]] <- list(
      label                = d$label,
      annotation           = d$annotation,
      has_annot            = nzchar(d$annotation),
      detection_method     = d$method,
      detection_confidence = d$confidence,
      raw_text             = as.character(p$text %||% "")
    )
    ## A multi-line header cell that yielded no variable token usually means
    ## the variable-name convention differs from "ALL-CAPS on line 2+".
    if (!nzchar(d$annotation) &&
        length(strsplit(as.character(p$text %||% ""), "\n", fixed = TRUE)[[1]]) >= 2) {
      diag_add(
        stage = "parse_shell", severity = "INFO", input = INPUT_SHELL,
        problem = "Multi-line listing header cell yielded no variable annotation",
        tlf_number = sec$tlf_number,
        location = substr(gsub("\n", " | ", p$text), 1, 80),
        action = "Header skipped -- variable may use a convention the token extractor does not recognise"
      )
    }
  }
  ## Listing shells may annotate their columns as below-table arrow lines
  ## ("Subject -> ADAE.USUBJID ; Treatment -> ADAE.TRT01A") instead of inside
  ## the header cells -- bind those against the header rows (ADR 0003
  ## Layer A) before deciding which headers carry annotations.
  if (length(sec$programmer_annotations %||% character()) > 0) {
    tmp <- sec
    tmp$stub_rows <- hdr_rows
    tmp <- bind_annotations(tmp)
    hdr_rows <- tmp$stub_rows
    if (is.null(sec$column_annotation)) {
      sec$column_annotation <- tmp$column_annotation
    }
  }
  sec$header_rows <- hdr_rows

  ## Append annotated headers to stub_rows so validate/enrich/build see
  ## them with no special-case branch.
  annotated_headers <- Filter(function(r) isTRUE(r$has_annot), hdr_rows)
  sec$stub_rows <- c(sec$stub_rows %||% list(), annotated_headers)

  sec$.pending_header_cells <- NULL
  sec
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
  paste(xml2::xml_text(t_nodes), collapse = "")
}

.cell_text <- function(cell_node) .paragraph_text(cell_node)

#' Returns a list of per-run metadata for every run inside the node.
#' Each entry: list(text, color_hex, highlight, bold, italic, underline).
#' @noRd
.runs_metadata <- function(node) {
  runs <- xml2::xml_find_all(
    node, paste0(".//*[local-name()='r'][", .EXCLUDED_TEXT_ANCESTORS_XPATH, "]"))
  out  <- vector("list", length(runs))
  for (i in seq_along(runs)) {
    r <- runs[[i]]
    t_nodes <- xml2::xml_find_all(r, ".//*[local-name()='t']")
    txt <- paste(xml2::xml_text(t_nodes), collapse = "")

    color_node <- xml2::xml_find_first(r, "./*[local-name()='rPr']/*[local-name()='color']")
    color <- if (inherits(color_node, "xml_missing")) NA_character_
             else toupper(xml2::xml_attr(color_node, "val") %||% NA_character_)

    highlight_node <- xml2::xml_find_first(r, "./*[local-name()='rPr']/*[local-name()='highlight']")
    highlight <- if (inherits(highlight_node, "xml_missing")) NA_character_
                 else tolower(xml2::xml_attr(highlight_node, "val") %||% NA_character_)

    bold      <- !inherits(xml2::xml_find_first(r, "./*[local-name()='rPr']/*[local-name()='b']"),  "xml_missing")
    italic    <- !inherits(xml2::xml_find_first(r, "./*[local-name()='rPr']/*[local-name()='i']"),  "xml_missing")
    underline <- !inherits(xml2::xml_find_first(r, "./*[local-name()='rPr']/*[local-name()='u']"),  "xml_missing")

    out[[i]] <- list(text = txt, color_hex = color, highlight = highlight,
                     bold = bold, italic = italic, underline = underline)
  }
  out
}

#' TRUE for a run that carries an annotation-style visual marker: a font
#' colour that isn't grey/black/white/automatic, or a highlight that isn't
#' "none"/"black". Shared by every place that decides whether a run counts
#' as "annotated" -- stub cells, the population line, and listing headers.
#' @noRd
.is_annotation_styled_run <- function(m) {
  if (!nzchar(m$text %||% "")) return(FALSE)
  has_colour    <- !is.na(m$color_hex) && !m$color_hex %in% c(.GREY_HEX, .BLACK_HEXES)
  has_highlight <- !is.na(m$highlight) && !m$highlight %in% c("none", "black")
  has_colour || has_highlight
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

#' TRUE if a paragraph looks like the population / analysis-set statement.
#'
#' Two ways to qualify:
#'   1. The text mentions the population lexicon ("Safety Population",
#'      "Analysis Set", "ITT", ...).
#'   2. The text has no population wording but carries a population-FLAG
#'      annotation ("(ADSL.SAFFL='Y')"): some shells annotate the population
#'      line and nothing else about it reads as prose.
#'
#' Case 2 is deliberately restricted to flag variables. A general "has any
#' annotation" test used to live here, but it also matched a treatment-column
#' mapping ("Treatment columns -> ADSL.TRT01A") placed right after the title;
#' that line would then be misfiled as the population and never reach
#' bind_annotations() as the column-axis grouping.
#' @noRd
.looks_like_population <- function(stripped) {
  if (grepl(.POPULATION_LEXICON_RE, stripped, perl = TRUE)) return(TRUE)
  grepl(.POPULATION_FLAG_RE, stripped, perl = TRUE)
}

## Leading markers that identify a footnote rather than a title continuation:
## a bracket/asterisk/digit at the start, or an opening word like "Note:" /
## "Abbreviations:". A real second title line ("by ATC Class") starts with a
## plain word and does not match.
.FOOTNOTE_LEAD_RE <- "^\\s*(?:[\\[\\(*\u2020\u2021]|\\d|note\\b|footnote\\b|abbreviations?\\b|key\\b)"

#' TRUE if a paragraph begins like a footnote (see .FOOTNOTE_LEAD_RE). Used
#' so the NEED_POP title-join step never glues a pre-table footnote onto the
#' title.
#' @noRd
.looks_like_footnote_lead <- function(stripped) {
  grepl(.FOOTNOTE_LEAD_RE, stripped, ignore.case = TRUE, perl = TRUE)
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
  meta <- .runs_metadata(p_node)
  coloured <- Filter(.is_annotation_styled_run, meta)
  if (length(coloured) > 0) {
    out <- paste(vapply(coloured, function(m) m$text, character(1)), collapse = "")
    out <- trimws(out)
    if (grepl(.ANNOTATION_PATTERN, out, perl = TRUE)) return(out)
  }
  ## Layer 3 on the population paragraph text.
  if (grepl(.ANNOTATION_PATTERN, full_text, perl = TRUE)) {
    ## Extract just the matching ADaM segment(s).
    m <- regmatches(full_text,
                    gregexpr(.ANNOTATION_PATTERN, full_text, perl = TRUE))[[1]]
    if (length(m) > 0) return(paste(m, collapse = " and "))
  }
  ""
}

## ---------------------------------------------------------------------------
## Annotation detection -- 4-layer (Layer 4 deferred to LLM)
## ---------------------------------------------------------------------------

#' Apply the detection hierarchy to one stub cell.
#' Returns list(label, annotation, method, confidence).
#' @noRd
.detect_annotation <- function(cell_text, runs_meta) {
  ## Layer 1: coloured or highlighted runs that additionally match the
  ## ADaM pattern.
  coloured_runs <- Filter(.is_annotation_styled_run, runs_meta)
  if (length(coloured_runs) > 0) {
    candidate <- trimws(paste(vapply(coloured_runs, function(m) m$text,
                                     character(1)),
                              collapse = ""))
    if (grepl(.ANNOTATION_PATTERN, candidate, perl = TRUE)) {
      label <- trimws(.strip_annotation_from_text(cell_text, candidate))
      return(list(label = label, annotation = candidate,
                  method = "colour", confidence = "high"))
    }
  }

  ## Layer 2: formatted runs (bold/italic/underline) matching ADaM pattern.
  formatted_runs <- Filter(function(m) {
    (isTRUE(m$bold) || isTRUE(m$italic) || isTRUE(m$underline)) &&
      nzchar(m$text)
  }, runs_meta)
  if (length(formatted_runs) > 0) {
    candidate <- trimws(paste(vapply(formatted_runs, function(m) m$text,
                                     character(1)),
                              collapse = ""))
    if (grepl(.ANNOTATION_PATTERN, candidate, perl = TRUE)) {
      label <- trimws(.strip_annotation_from_text(cell_text, candidate))
      return(list(label = label, annotation = candidate,
                  method = "format", confidence = "medium"))
    }
  }

  ## Layer 3: plain-text ADaM regex match anywhere in the cell.
  pieces <- split_label_annotation(cell_text)
  if (nzchar(pieces$annotation)) {
    confidence <- if (.is_full_dataset_dot_variable(pieces$annotation)) "high" else "medium"
    return(list(label = pieces$label, annotation = pieces$annotation,
                method = "pattern", confidence = confidence))
  }

  ## Layer 4 fallback would happen later (LLM); here, no annotation.
  list(label = trimws(cell_text), annotation = "",
       method = NA_character_, confidence = NA_character_)
}

#' Remove an annotation substring from a cell text and return the remaining
#' label portion (used when Layer 1 / Layer 2 detected the annotation from
#' formatting alone).
#' @noRd
.strip_annotation_from_text <- function(cell_text, annotation) {
  pos <- regexpr(annotation, cell_text, fixed = TRUE)
  if (pos < 1) return(cell_text)
  before <- substr(cell_text, 1, pos - 1)
  gsub("\\s*[\\[\\(]?\\s*$", "", before, perl = TRUE)
}

#' Layer 3 split helper. Unexported. Returns list(label, annotation).
#'
#' Identifies the first ADaM-pattern match in `cell_text`, splits text into
#' a "label" (everything before) and "annotation" (the match and any
#' compound continuation). Handles bracket-enclosed forms by stripping the
#' opening bracket from the label and the closing bracket from the
#' annotation tail.
#'
#' @noRd
split_label_annotation <- function(cell_text) {
  cell_text <- as.character(cell_text %||% "")
  if (!nzchar(trimws(cell_text))) {
    return(list(label = "", annotation = ""))
  }

  m <- regexpr(.ANNOTATION_PATTERN, cell_text, perl = TRUE)
  if (m == -1) {
    return(list(label = trimws(cell_text), annotation = ""))
  }

  start <- as.integer(m)
  before <- substr(cell_text, 1, start - 1L)
  label <- trimws(sub("\\s*[\\[\\(]?\\s*$", "", before, perl = TRUE))

  annotation <- substr(cell_text, start, nchar(cell_text))
  annotation <- sub("\\s*[\\]\\)]\\s*$", "", annotation, perl = TRUE)
  annotation <- trimws(annotation)

  list(label = label, annotation = annotation)
}

#' TRUE if the annotation is exactly a DATASET.VARIABLE form (Layer 3a -- HIGH
#' confidence). FALSE for partial / abbreviated forms (Layer 3b -- MEDIUM).
#' @noRd
.is_full_dataset_dot_variable <- function(annotation) {
  pat <- paste0("^(", .ADAM_DS, "\\.", .ADAM_VAR, ")(\\b|=|\\s|$)")
  grepl(pat, annotation, perl = TRUE)
}

## ---------------------------------------------------------------------------
## Listing column-header annotation detection
##
## Listing shells annotate variables in the COLUMN HEADERS rather than
## the stub column, with a different convention than table stubs:
##
##   Cell text:     "Subject ID\nUSUBJID"
##                  display label on line 1, variable name on line 2+
##
##   Multi-var:     "AE PT (Verbatim)\nAEDECOD (AETERM)"
##                  -> primary AEDECOD, supplementary AETERM
##
## The variable lacks a DATASET prefix. We resolve the dataset from:
##   1. .UNIVERSAL_ADSL_VARS lookup (USUBJID, ARM, AGE, ... -> ADSL)
##   2. The TLF's first source dataset from "Source: ..." (ADAE, ADLB, etc.)
##
## Detection runs Layer 1 (coloured runs in the cell) first, falling back
## to a Layer 3 scan of the post-display-line text. Validation against the
## ADaM spec catches any wrong-dataset guess as a WARN finding, so the
## heuristic dataset assignment is a "draft" the human reviewer confirms.
## ---------------------------------------------------------------------------

#' Detect a listing column-header annotation.
#'
#' @param cell_text    The full concatenated text of the header cell.
#' @param runs_meta    Per-run metadata for the cell (from `.runs_metadata`).
#' @param source_ds    The TLF's primary source dataset (used as the default
#'   prefix for variables not in `.UNIVERSAL_ADSL_VARS`).
#' @param spec_lookup  Optional `"DATASET.VARIABLE"`-keyed spec lookup. When
#'   present, candidate tokens are matched case-insensitively against the
#'   spec's variable names (catching mixed-case headers like "AeDecod") and
#'   the dataset is resolved from the spec rather than guessed.
#'
#' @return list(label, annotation, method, confidence).
#'
#' @noRd
.detect_listing_header_annotation <- function(cell_text, runs_meta, source_ds,
                                              spec_lookup = NULL) {
  cell_text <- as.character(cell_text %||% "")
  no_match <- list(label = trimws(cell_text), annotation = "",
                   method = NA_character_, confidence = NA_character_)
  if (!nzchar(trimws(cell_text))) return(no_match)

  ## Layer 1: coloured (red) or highlighted runs in the header carry the
  ## annotation directly.
  coloured <- Filter(.is_annotation_styled_run, runs_meta)
  candidate_text <- NULL
  source_method  <- NULL
  if (length(coloured) > 0) {
    candidate_text <- paste(vapply(coloured, function(m) m$text, character(1)),
                            collapse = "")
    source_method <- "listing_header_colour"
  }

  ## Layer 3: line-2 fallback. Split on \n and treat first line as display,
  ## remainder as the variable-name region.
  lines <- strsplit(cell_text, "\n", fixed = TRUE)[[1]]
  lines <- trimws(lines)
  if (is.null(candidate_text)) {
    if (length(lines) < 2) return(no_match)
    candidate_text <- paste(lines[-1], collapse = " ")
    source_method <- "listing_header_pattern"
  }
  if (!nzchar(trimws(candidate_text))) return(no_match)

  ## Candidate variable tokens. With a spec available, scan
  ## case-insensitively and keep only tokens that are real spec variables
  ## (catches mixed-case conventions like "AeDecod"; no blocklist needed --
  ## English noise simply fails the spec membership test). Without a spec,
  ## fall back to the ALL-CAPS heuristic + blocklist.
  spec_keys <- if (!is.null(spec_lookup)) toupper(names(spec_lookup)) else character()
  spec_vars <- unique(sub("^.*\\.", "", spec_keys))
  if (length(spec_vars) > 0) {
    tokens <- regmatches(
      candidate_text,
      gregexpr("\\b[A-Za-z][A-Za-z0-9]{2,7}\\b", candidate_text, perl = TRUE)
    )[[1]]
    tokens <- unique(toupper(tokens))
    tokens <- tokens[tokens %in% spec_vars]
  } else {
    tokens <- regmatches(
      candidate_text,
      gregexpr("\\b[A-Z][A-Z0-9]{2,7}\\b", candidate_text, perl = TRUE)
    )[[1]]
    tokens <- unique(tokens[!tokens %in% .HEADER_TOKEN_BLOCKLIST])
  }
  if (length(tokens) == 0) return(no_match)

  resolve_ds <- function(v) {
    if (length(spec_keys) > 0) {
      ## Spec-grounded resolution: source dataset first, ADSL second, then
      ## whichever spec dataset carries the variable.
      ds_hits <- unique(sub("\\..*$", "", spec_keys[sub("^.*\\.", "", spec_keys) == v]))
      if (toupper(source_ds) %in% ds_hits) return(toupper(source_ds))
      if ("ADSL" %in% ds_hits) return("ADSL")
      if (length(ds_hits) > 0) return(ds_hits[1])
    }
    if (v %in% .UNIVERSAL_ADSL_VARS) "ADSL" else source_ds
  }
  qualified <- vapply(tokens, function(v) paste0(resolve_ds(v), ".", v),
                      character(1), USE.NAMES = FALSE)
  annotation <- if (length(qualified) == 1) {
    qualified
  } else {
    sprintf("%s (%s)", qualified[1], paste(qualified[-1], collapse = ", "))
  }

  label <- if (length(lines) > 0) lines[1] else trimws(cell_text)

  list(label = label, annotation = annotation,
       method = source_method,
       confidence = if (identical(source_method, "listing_header_colour"))
                      "high" else "medium")
}

## Tokens that look like ADaM variable names but are common English in
## listing headers -- never treat them as variables.
.HEADER_TOKEN_BLOCKLIST <- c(
  "ID", "PT", "SOC", "AE", "NA",
  "WP", "NRS", "EASI", "DLQI", "POEM",
  "CI", "SD", "SE", "PY"
)

## ---------------------------------------------------------------------------
## Source line / TOC helpers
## ---------------------------------------------------------------------------

.split_source_list <- function(raw) {
  raw   <- trimws(sub("\\.\\s*$", "", raw))
  parts <- strsplit(raw, "[,;]+")[[1]]
  parts <- toupper(trimws(parts))
  parts[nzchar(parts)]
}

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
