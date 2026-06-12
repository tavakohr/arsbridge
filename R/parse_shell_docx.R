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
## optional trailing colon.
.TLF_HEADING_RE <- "^(?i)(Table|Figure|Listing)\\s+(\\d{1,3}(?:\\.\\d+)*[a-z]?)\\s*:?\\s*$"
## Accepts "Source:", "Sources:", "Data Source:", "Source datasets:", and
## "=" in place of ":".
.SOURCE_LINE_RE <- "^\\s*(?:Data\\s+)?Sources?(?:\\s+datasets?)?\\s*[:=]\\s*(.+?)\\.?\\s*$"
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
#'   `lookup` element of [parse_adam_spec()]). When supplied, listing
#'   column-header variable candidates are validated against the spec
#'   (tolerating mixed-case names) instead of relying on the ALL-CAPS
#'   token heuristic and its blocklist.
#'
#' @return List of TLF section objects (see top of file for full schema).
#'
#' @keywords internal
#' @noRd
parse_shell_docx <- function(docx_path, spec_lookup = NULL) {
  if (!file.exists(docx_path)) {
    cli::cli_abort("Shell file not found: {.path {docx_path}}")
  }

  doc      <- officer::read_docx(docx_path)
  root_xml <- doc$doc_obj$get()
  body_xml <- xml2::xml_find_first(root_xml, ".//*[local-name()='body']")
  if (inherits(body_xml, "xml_missing")) {
    cli::cli_abort("Could not locate <w:body> in {.path {docx_path}}.")
  }

  children   <- xml2::xml_children(body_xml)
  toc_skip   <- FALSE
  toc_table  <- .detect_toc_table(children)

  sections   <- list()
  current    <- NULL
  state      <- "BEFORE_HEADING"   ## BEFORE_HEADING / NEED_TITLE / NEED_POP / IN_BODY
  seen_table <- FALSE

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
      if (length(m) == 3) {
        if (!is.null(current)) {
          sections[[length(sections) + 1]] <- .finalize_section(current, spec_lookup)
        }
        word   <- tools::toTitleCase(tolower(m[2]))
        number <- m[3]
        prefix <- substr(toupper(word), 1, 1)
        current <- list(
          tlf_number       = paste0(prefix, "-", gsub("\\.", "-", number)),
          tlf_type         = switch(tolower(word),
                                    table   = "TABLE",
                                    figure  = "FIGURE",
                                    listing = "LISTING",
                                    "TABLE"),
          title            = "",
          population_text  = "",
          population_annot = "",
          footnotes        = character(),
          source_datasets  = character(),
          col_headers      = character(),
          n_data_cols      = 0L,
          stub_rows        = list()
        )
        state      <- "NEED_TITLE"
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
        current$population_text  <- stripped
        current$population_annot <- .extract_population_annot(child, stripped)
        state <- "IN_BODY"
        next
      }

      ## Source line?
      src <- regmatches(stripped, regexec(.SOURCE_LINE_RE, stripped,
                                          ignore.case = TRUE, perl = TRUE))[[1]]
      if (length(src) == 2 && .is_grey_paragraph(child)) {
        current$source_datasets <- .split_source_list(src[2])
        next
      }
      ## Source line without grey -- still accept based on text pattern.
      if (length(src) == 2 && !nzchar(paste(current$source_datasets, collapse = ""))) {
        current$source_datasets <- .split_source_list(src[2])
        next
      }
      ## Footnote markers or longer prose -> footnote bucket.
      if (grepl("^[\\[\\(]?[a-z\\d\\*]", stripped, perl = TRUE) ||
          nchar(stripped) > 10) {
        current$footnotes <- c(current$footnotes, stripped)
      }

    } else if (tag == "tbl") {
      if (is.null(current) || seen_table) next
      current    <- .populate_table(current, child)
      seen_table <- TRUE
    }
  }

  if (!is.null(current)) {
    sections[[length(sections) + 1]] <- .finalize_section(current, spec_lookup)
  }

  if (length(sections) == 0) {
    cli::cli_warn(c(
      "No TLF sections found in {.path {docx_path}}.",
      "i" = "Expected paragraphs matching {.val Table X.X.X}, {.val Figure X.X.X}, or {.val Listing X.X.X}."
    ))
    diag_add(
      stage = "parse_shell", severity = "FAIL",
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
        stage = "parse_shell", severity = "WARN",
        problem = sprintf("Section has %d stub row(s) but no annotations were detected", n_rows),
        tlf_number = sec$tlf_number,
        location = sec$title %||% "",
        action = "Section will rely entirely on LLM/fallback inference -- review annotation convention"
      )
    }
    if (n_rows == 0 && !identical(sec$tlf_type, "FIGURE")) {
      diag_add(
        stage = "parse_shell", severity = "WARN",
        problem = "No table rows captured for this section",
        tlf_number = sec$tlf_number,
        location = sec$title %||% "",
        action = "Check that the shell table directly follows the TLF heading"
      )
    }
    if (length(sec$source_datasets) == 0) {
      diag_add(
        stage = "parse_shell", severity = "INFO",
        problem = "No 'Source: ...' line found for this section",
        tlf_number = sec$tlf_number,
        location = sec$title %||% "",
        action = "Source datasets unknown; listing header dataset resolution falls back to ADSL"
      )
    }
  }

  sections
}

## ---------------------------------------------------------------------------
## Body walker -- table handler. Returns the updated `current` section.
## ---------------------------------------------------------------------------

.populate_table <- function(current, tbl_node) {
  rows <- xml2::xml_find_all(tbl_node, "./*[local-name()='tr']")
  if (length(rows) == 0) return(current)

  ## Column headers from the first row.
  header_cells <- xml2::xml_find_all(rows[[1]], "./*[local-name()='tc']")
  headers <- vapply(header_cells, function(c) trimws(.cell_text(c)), character(1))
  headers <- headers[nzchar(headers)]
  current$col_headers <- headers
  current$n_data_cols <- max(0L, length(headers) - 1L)

  ## For LISTING outputs, each column header carries the annotation rather
  ## than the stub column. We capture the raw cell content here but DEFER
  ## actual detection to `.finalize_section()` -- the "Source: ..." line
  ## that supplies the dataset prefix typically appears AFTER the table in
  ## document order, so source_datasets is still empty at this point.
  if (identical(current$tlf_type, "LISTING")) {
    current$.pending_header_cells <- lapply(header_cells, function(c) {
      list(text = .cell_text(c), runs = .runs_metadata(c))
    })
  }

  ## Stub rows from row 2..N (column 0 only).
  if (length(rows) >= 2) {
    stub_rows <- vector("list", length(rows) - 1L)
    for (i in seq.int(2, length(rows))) {
      cells <- xml2::xml_find_all(rows[[i]], "./*[local-name()='tc']")
      if (length(cells) == 0) {
        stub_rows[[i - 1L]] <- NULL
        next
      }
      stub_cell <- cells[[1]]
      raw_text  <- .cell_text(stub_cell)
      runs_meta <- .runs_metadata(stub_cell)
      detection <- .detect_annotation(raw_text, runs_meta)
      stub_rows[[i - 1L]] <- list(
        label                = detection$label,
        annotation           = detection$annotation,
        has_annot            = nzchar(detection$annotation),
        detection_method     = detection$method,
        detection_confidence = detection$confidence
      )
    }
    current$stub_rows <- Filter(Negate(is.null), stub_rows)
  }

  current
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
      stage = "parse_shell", severity = "WARN",
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
      detection_confidence = d$confidence
    )
    ## A multi-line header cell that yielded no variable token usually means
    ## the variable-name convention differs from "ALL-CAPS on line 2+".
    if (!nzchar(d$annotation) &&
        length(strsplit(as.character(p$text %||% ""), "\n", fixed = TRUE)[[1]]) >= 2) {
      diag_add(
        stage = "parse_shell", severity = "INFO",
        problem = "Multi-line listing header cell yielded no variable annotation",
        tlf_number = sec$tlf_number,
        location = substr(gsub("\n", " | ", p$text), 1, 80),
        action = "Header skipped -- variable may use a convention the token extractor does not recognise"
      )
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

#' Concatenate all text under a paragraph/cell node, preserving order.
#' @noRd
.paragraph_text <- function(p_node) {
  t_nodes <- xml2::xml_find_all(p_node, ".//*[local-name()='t']")
  paste(xml2::xml_text(t_nodes), collapse = "")
}

.cell_text <- function(cell_node) .paragraph_text(cell_node)

#' Returns a list of per-run metadata for every run inside the node.
#' Each entry: list(text, color_hex, bold, italic, underline).
#' @noRd
.runs_metadata <- function(node) {
  runs <- xml2::xml_find_all(node, ".//*[local-name()='r']")
  out  <- vector("list", length(runs))
  for (i in seq_along(runs)) {
    r <- runs[[i]]
    t_nodes <- xml2::xml_find_all(r, ".//*[local-name()='t']")
    txt <- paste(xml2::xml_text(t_nodes), collapse = "")

    color_node <- xml2::xml_find_first(r, "./*[local-name()='rPr']/*[local-name()='color']")
    color <- if (inherits(color_node, "xml_missing")) NA_character_
             else toupper(xml2::xml_attr(color_node, "val") %||% NA_character_)

    bold      <- !inherits(xml2::xml_find_first(r, "./*[local-name()='rPr']/*[local-name()='b']"),  "xml_missing")
    italic    <- !inherits(xml2::xml_find_first(r, "./*[local-name()='rPr']/*[local-name()='i']"),  "xml_missing")
    underline <- !inherits(xml2::xml_find_first(r, "./*[local-name()='rPr']/*[local-name()='u']"),  "xml_missing")

    out[[i]] <- list(text = txt, color_hex = color, bold = bold,
                     italic = italic, underline = underline)
  }
  out
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
## Population paragraph annotation extraction
## ---------------------------------------------------------------------------

#' Concatenate all non-grey coloured runs of a paragraph; if none, fall back
#' to Layer 3 plain-text detection on the full paragraph text.
#' @noRd
.extract_population_annot <- function(p_node, full_text) {
  meta <- .runs_metadata(p_node)
  coloured <- Filter(function(m) {
    !is.na(m$color_hex) &&
      !m$color_hex %in% c(.GREY_HEX, .BLACK_HEXES) &&
      nzchar(m$text)
  }, meta)
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
  ## Layer 1: coloured runs (red C00000 or any non-grey/non-black) that
  ## additionally match the ADaM pattern.
  coloured_runs <- Filter(function(m) {
    !is.na(m$color_hex) &&
      !m$color_hex %in% c(.GREY_HEX, .BLACK_HEXES) &&
      nzchar(m$text)
  }, runs_meta)
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

  ## Layer 1: coloured (red) runs in the header carry the annotation directly.
  coloured <- Filter(function(m) {
    !is.na(m$color_hex) &&
      !m$color_hex %in% c(.GREY_HEX, .BLACK_HEXES) &&
      nzchar(m$text)
  }, runs_meta)
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
