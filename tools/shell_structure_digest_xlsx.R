## tools/shell_structure_digest_xlsx.R
## ---------------------------------------------------------------------------
## PRIVACY-SAFE STRUCTURE EXTRACTOR for annotated TLF shell WORKBOOKS.
##
## The Excel twin of shell_structure_digest.R. Same purpose, same promise: let
## someone debug arsbridge's reading of a shell they are NOT allowed to share.
## It reads an .xlsx and writes a JSON digest describing only the SHAPE of the
## workbook -- how many sheets, where the banner rows are, which cells carry a
## red run, what the merges are, and character-class silhouettes of the text.
## No label text, no annotation values, no titles, no study identifiers ever
## reach the output.
##
## Dependencies: xml2 and jsonlite only. arsbridge does NOT need to be
## installed, so this runs on a locked-down machine.
##
## Usage (from any directory):
##
##   source("shell_structure_digest_xlsx.R")
##   digest_shell_xlsx("MyStudy_Shells.xlsx", "digest.json")
##
## Then OPEN digest.json, read it yourself, and only send it on if you are
## satisfied. It is plain text and short enough to skim.
##
## What is deliberately NOT captured: any literal character of the workbook's
## text. Letters become "a"/"A", digits become "9". "Cohort 1 (N=XX)" is
## recorded as "Aaaaaa 9 (A=AA)", "xx (xx.x)" as "aa (aa.a)". That is enough
## to tell a placeholder from a label, and a cohort-shaped header from a
## statistic one, and nothing about the study.

suppressPackageStartupMessages({
  library(xml2)
  library(jsonlite)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

## Same rules as tools/shell_structure_digest.R, so the two digests of the
## same study can be compared line for line.
silhouette <- function(x, max_chars = 60) {
  x <- substr(as.character(x %||% ""), 1, max_chars)
  x <- gsub("[A-Z]", "A", x)
  x <- gsub("[a-z]", "a", x)
  gsub("[0-9]", "9", x)
}

## --- SpreadsheetML walking ---------------------------------------------------
##
## Deliberately a SECOND, standalone implementation of what R/xlsx_cells.R
## does. It has to run where arsbridge is not installed, and being independent
## is also the point: when this digest and the parser's decision digest
## disagree, the disagreement is evidence rather than a shared bug.

.part <- function(dir, path) {
  full <- file.path(dir, path)
  if (!file.exists(full)) return(NULL)
  read_xml(full)
}

.col_num <- function(letters_part) {
  chars <- strsplit(toupper(letters_part), "", fixed = TRUE)[[1]]
  n <- 0L
  for (ch in chars) n <- n * 26L + (utf8ToInt(ch) - utf8ToInt("A") + 1L)
  n
}

.ref_rc <- function(ref) {
  m <- regmatches(ref, regexec("^([A-Za-z]+)([0-9]+)$", ref))[[1]]
  if (length(m) != 3) return(c(NA_integer_, NA_integer_))
  c(as.integer(m[3]), .col_num(m[2]))
}

## 8-digit ARGB -> 6-hex, matching what the parser normalizes to.
.colour <- function(node) {
  if (is.null(node) || inherits(node, "xml_missing")) return(NA_character_)
  c_node <- xml_find_first(node, "./*[local-name()='color']")
  if (inherits(c_node, "xml_missing")) return(NA_character_)
  rgb <- xml_attr(c_node, "rgb")
  if (is.na(rgb) || !nzchar(rgb)) return(NA_character_)
  rgb <- toupper(rgb)
  if (nchar(rgb) == 8L) substr(rgb, 3L, 8L) else rgb
}

.font_table <- function(dir) {
  doc <- .part(dir, file.path("xl", "styles.xml"))
  if (is.null(doc)) return(list(colours = character(), xf = integer()))
  fonts <- xml_find_all(
    doc, "/*[local-name()='styleSheet']/*[local-name()='fonts']/*[local-name()='font']")
  colours <- vapply(fonts, .colour, character(1))
  xfs <- xml_find_all(
    doc, "/*[local-name()='styleSheet']/*[local-name()='cellXfs']/*[local-name()='xf']")
  xf <- vapply(xfs, function(x) {
    id <- xml_attr(x, "fontId")
    if (is.na(id)) 0L else as.integer(id)
  }, integer(1)) + 1L
  list(colours = colours, xf = xf)
}

.shared_strings <- function(dir) {
  doc <- .part(dir, file.path("xl", "sharedStrings.xml"))
  if (is.null(doc)) return(list())
  lapply(xml_find_all(doc, "/*[local-name()='sst']/*[local-name()='si']"),
         function(si) .container_runs(si))
}

## Runs of an <is> or <si>: text plus colour, which is all the digest needs.
.container_runs <- function(node) {
  rs <- xml_find_all(node, "./*[local-name()='r']")
  if (length(rs) > 0) {
    return(lapply(rs, function(r) {
      rpr <- xml_find_first(r, "./*[local-name()='rPr']")
      list(text = paste(xml_text(xml_find_all(r, "./*[local-name()='t']")),
                        collapse = ""),
           colour = .colour(rpr))
    }))
  }
  ts <- xml_find_all(node, "./*[local-name()='t']")
  if (length(ts) == 0) return(list())
  list(list(text = paste(xml_text(ts), collapse = ""), colour = NA_character_))
}

.sheet_parts <- function(dir) {
  wb <- .part(dir, file.path("xl", "workbook.xml"))
  if (is.null(wb)) stop("Not a readable .xlsx: xl/workbook.xml is missing.")
  sheets <- xml_find_all(
    wb, "/*[local-name()='workbook']/*[local-name()='sheets']/*[local-name()='sheet']")
  rels <- .part(dir, file.path("xl", "_rels", "workbook.xml.rels"))
  targets <- character()
  if (!is.null(rels)) {
    nodes <- xml_find_all(rels, "/*/*[local-name()='Relationship']")
    targets <- stats::setNames(
      vapply(nodes, function(n) xml_attr(n, "Target") %||% "", character(1)),
      vapply(nodes, function(n) xml_attr(n, "Id") %||% "", character(1)))
  }
  lapply(seq_along(sheets), function(i) {
    rid <- xml_attr(sheets[[i]], "id")
    target <- if (!is.na(rid) && rid %in% names(targets)) targets[[rid]] else ""
    part <- if (!nzchar(target)) {
      file.path("xl", "worksheets", sprintf("sheet%d.xml", i))
    } else if (startsWith(target, "/")) {
      sub("^/", "", target)
    } else {
      file.path("xl", target)
    }
    list(name = xml_attr(sheets[[i]], "name") %||% "", index = i, part = part)
  })
}

.sheet_digest <- function(dir, sheet, shared, fonts) {
  doc <- .part(dir, sheet$part)
  if (is.null(doc)) {
    return(list(name = silhouette(sheet$name), index = sheet$index,
                readable = FALSE))
  }

  cells <- xml_find_all(
    doc, paste0("/*[local-name()='worksheet']/*[local-name()='sheetData']",
                "/*[local-name()='row']/*[local-name()='c']"))

  rows <- integer(0); cols <- integer(0)
  records <- list()
  for (cell in cells) {
    rc <- .ref_rc(xml_attr(cell, "r") %||% "")
    if (anyNA(rc)) next
    type <- xml_attr(cell, "t")
    type <- if (is.na(type)) "n" else type

    runs <- if (identical(type, "inlineStr")) {
      .container_runs(xml_find_first(cell, "./*[local-name()='is']"))
    } else if (identical(type, "s")) {
      v <- xml_find_first(cell, "./*[local-name()='v']")
      idx <- if (inherits(v, "xml_missing")) NA_integer_ else
        suppressWarnings(as.integer(xml_text(v))) + 1L
      if (!is.na(idx) && idx >= 1L && idx <= length(shared)) shared[[idx]] else
        list()
    } else {
      v <- xml_find_first(cell, "./*[local-name()='v']")
      if (inherits(v, "xml_missing")) list() else
        list(list(text = xml_text(v), colour = NA_character_))
    }

    ## The cell's own font, which is how a whole-cell-red annotation is
    ## stated -- and the reason a digest that only looked at runs would
    ## report a figure sheet as unannotated.
    s <- xml_attr(cell, "s")
    cell_colour <- NA_character_
    if (!is.na(s)) {
      xf <- as.integer(s) + 1L
      if (!is.na(xf) && xf >= 1L && xf <= length(fonts$xf)) {
        fi <- fonts$xf[[xf]]
        if (fi >= 1L && fi <= length(fonts$colours)) {
          cell_colour <- fonts$colours[[fi]]
        }
      }
    }

    colours <- unique(c(vapply(runs, function(r) r$colour %||% NA_character_,
                               character(1)),
                        cell_colour))
    colours <- colours[!is.na(colours)]
    text <- paste(vapply(runs, function(r) r$text, character(1)),
                  collapse = "")

    rows <- c(rows, rc[[1]]); cols <- c(cols, rc[[2]])
    records[[length(records) + 1L]] <- list(
      row      = rc[[1]],
      col      = rc[[2]],
      kind     = type,
      n_runs   = length(runs),
      colours  = colours,
      silhouette = silhouette(gsub("\n", "\\\\n", text))
    )
  }

  merges <- vapply(
    xml_find_all(doc, paste0("/*[local-name()='worksheet']",
                             "/*[local-name()='mergeCells']",
                             "/*[local-name()='mergeCell']")),
    function(n) xml_attr(n, "ref") %||% "", character(1))

  list(
    name       = silhouette(sheet$name),
    index      = sheet$index,
    readable   = TRUE,
    n_cells    = length(records),
    n_rows     = if (length(rows)) max(rows) else 0L,
    n_cols     = if (length(cols)) max(cols) else 0L,
    ## The gaps ARE the layout: a blank spacer row is absent from the file.
    populated_rows = sort(unique(rows)),
    merges     = merges,
    colours    = sort(unique(unlist(lapply(records, function(r) r$colours)))),
    n_coloured_cells = sum(vapply(records, function(r) {
      any(!r$colours %in% c("000000", "FFFFFF", "808080"))
    }, logical(1))),
    cells = records
  )
}

## --- entry point -------------------------------------------------------------

#' Write a privacy-safe structure digest of a shell workbook.
digest_shell_xlsx <- function(xlsx_path, out_json = "shell_digest_xlsx.json") {
  stopifnot(file.exists(xlsx_path))
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  utils::unzip(xlsx_path, exdir = dir)

  fonts  <- .font_table(dir)
  shared <- .shared_strings(dir)
  sheets <- .sheet_parts(dir)

  digest <- list(
    format = "arsbridge shell structure digest (xlsx) v1",
    note = paste("All text is character-class silhouettes (A/a/9);",
                 "no literal workbook text is present."),
    n_sheets = length(sheets),
    has_shared_strings = length(shared) > 0,
    sheets = lapply(sheets, function(s) .sheet_digest(dir, s, shared, fonts))
  )

  writeLines(toJSON(digest, auto_unbox = TRUE, pretty = TRUE), out_json)
  message(sprintf("Wrote %s (%d sheet%s).", out_json, length(sheets),
                  if (length(sheets) == 1L) "" else "s"))
  message("OPEN IT AND READ IT before sending it anywhere.")
  invisible(digest)
}

## --- compact summary ---------------------------------------------------------

#' One line per sheet: where the banner appears to be, how much is coloured,
#' and how wide it is. Enough to compare two machines' workbooks by eye.
digest_xlsx_summary <- function(digest) {
  cat(sprintf("%d sheet%s, shared strings: %s\n\n", digest$n_sheets,
              if (digest$n_sheets == 1L) "" else "s",
              if (isTRUE(digest$has_shared_strings)) "yes" else "no (inline)"))
  cat(sprintf("%-22s %5s %5s %6s %8s %s\n",
              "sheet (silhouette)", "rows", "cols", "cells", "coloured",
              "merges"))
  for (s in digest$sheets) {
    if (!isTRUE(s$readable)) {
      cat(sprintf("%-22s  UNREADABLE\n", substr(s$name, 1, 22)))
      next
    }
    cat(sprintf("%-22s %5d %5d %6d %8d %d\n", substr(s$name, 1, 22),
                s$n_rows, s$n_cols, s$n_cells, s$n_coloured_cells,
                length(s$merges)))
  }
  invisible(NULL)
}
