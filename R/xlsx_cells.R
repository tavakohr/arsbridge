## arsbridge -- xlsx_cells.R
## ---------------------------------------------------------------------------
## The SpreadsheetML cell reader: the xlsx counterpart of the run/cell readers
## in parse_shell_docx.R. Its whole job is to produce seam 1 of
## parse_shell_core.R -- the per-run metadata list -- from a workbook, so the
## shared annotation grammar and detection layers work on an Excel shell
## without a single format branch.
##
## Why read the XML directly instead of readxl / openxlsx2::wb_to_df(): an
## annotated Excel shell carries the annotation as a SECOND FORMATTING RUN
## inside the cell --
##
##   <c r="A5" t="inlineStr"><is>
##     <r><rPr><color rgb="FF000000"/></rPr><t>Completed study</t></r>
##     <r><rPr><i val="1"/><color rgb="FFC00000"/></rPr><t>
## [ADSL.EOSSTT = 'COMPLETED']</t></r>
##   </is></c>
##
## -- and every high-level reader returns that cell as the single flattened
## string "Completed study\n[ADSL.EOSSTT = 'COMPLETED']", silently discarding
## the fact that the second half was red. Red font is the primary annotation
## signal in these shells, so per-run reading is not an optimization here, it
## is the requirement. All XPath uses local-name() so the default
## SpreadsheetML namespace never has to be bound.
##
## Two facts about real files this reader has to absorb:
##
##   * Strings live in EITHER place. Excel and openxlsx2 intern them in
##     xl/sharedStrings.xml (`<c t="s"><v>7</v>` -> `<si>` #7); openpyxl,
##     which generated the exemplar shell, writes them inline instead
##     (`<c t="inlineStr"><is>`). Both carry runs the same way, so both are
##     read into the same run list.
##   * A run's <rPr> may be partial or absent, in which case the run inherits
##     the CELL's font (s= -> cellXfs -> fonts). A cell with no runs at all --
##     a plain string, a number -- is therefore presented as ONE synthesized
##     run carrying the cell font, which is what makes an entirely-red cell
##     (how the exemplar's figure sheet annotates) look to the detection
##     layers exactly like a red run inside a mixed cell. One interface,
##     either convention.
##
## Colours are normalized to seam 1's 6-hex form: SpreadsheetML writes 8-digit
## ARGB ("FFC00000"), so the leading alpha byte is dropped. A theme or indexed
## colour resolves to NA (no colour stated) rather than being looked up --
## theme resolution is deliberately out of scope, and the shells in question
## always state annotation colours as explicit rgb.

## ---------------------------------------------------------------------------
## Zip members and A1 references
## ---------------------------------------------------------------------------

#' Unzip a workbook once and return the directory. An .xlsx is a zip archive
#' whose parts (workbook, styles, one XML per sheet) all have to be read, so
#' the archive is expanded a single time and every reader below works off the
#' resulting directory.
#' @noRd
.xlsx_unzip <- function(xlsx_path) {
  dir <- tempfile()
  dir.create(dir)
  utils::unzip(xlsx_path, exdir = dir)
  dir
}

#' Parse one part of an unzipped workbook, or return NULL when the part does
#' not exist (sharedStrings.xml is absent from a workbook whose strings are
#' all inline; styles.xml from a workbook that styles nothing).
#' @noRd
.xlsx_part_xml <- function(dir, part) {
  path <- file.path(dir, part)
  if (!file.exists(path)) return(NULL)
  xml2::read_xml(path)
}

#' Column letters to a 1-based column number ("A" -> 1, "AA" -> 27).
#' @noRd
.xlsx_col_to_num <- function(col_letters) {
  chars <- strsplit(toupper(col_letters), "", fixed = TRUE)[[1]]
  num <- 0L
  for (ch in chars) {
    num <- num * 26L + (utf8ToInt(ch) - utf8ToInt("A") + 1L)
  }
  num
}

#' A 1-based column number back to letters (1 -> "A", 27 -> "AA"). Needed by
#' the fill writer, which addresses cells by reference.
#' @noRd
.xlsx_num_to_col <- function(col_num) {
  out <- character()
  n <- as.integer(col_num)
  while (n > 0L) {
    rem <- (n - 1L) %% 26L
    out <- c(intToUtf8(utf8ToInt("A") + rem), out)
    n <- (n - 1L) %/% 26L
  }
  paste(out, collapse = "")
}

#' An A1 reference to `c(row, col)`. Returns NA for anything that is not a
#' plain cell reference.
#' @noRd
.xlsx_a1_to_rc <- function(ref) {
  m <- regmatches(ref, regexec("^\\$?([A-Za-z]+)\\$?([0-9]+)$", ref))[[1]]
  if (length(m) != 3) return(c(row = NA_integer_, col = NA_integer_))
  c(row = as.integer(m[3]), col = .xlsx_col_to_num(m[2]))
}

#' `c(row, col)` to an A1 reference.
#' @noRd
.xlsx_rc_to_a1 <- function(row, col) {
  paste0(.xlsx_num_to_col(col), as.integer(row))
}

## ---------------------------------------------------------------------------
## Font properties
## ---------------------------------------------------------------------------

#' Read a boolean run/font property the way SpreadsheetML writes it. The
#' element's presence means ON; `val="0"` / `"false"` / `"off"` is how a
#' property explicitly opts OUT (openpyxl writes the redundant `val="1"` for
#' ON, Excel writes the bare element -- both mean the same thing).
#' @noRd
.xlsx_flag <- function(props_node, tag) {
  if (is.null(props_node)) return(FALSE)
  node <- xml2::xml_find_first(
    props_node, sprintf("./*[local-name()='%s']", tag))
  if (inherits(node, "xml_missing")) return(FALSE)
  val <- xml2::xml_attr(node, "val")
  is.na(val) || !tolower(val) %in% c("false", "0", "off")
}

#' The seam-1 colour of a `<rPr>` / `<font>` node: 6 upper-case hex digits, or
#' NA when the node states no explicit rgb. SpreadsheetML's 8-digit ARGB
#' ("FFC00000") loses its leading alpha byte; a theme or indexed colour is
#' reported as NA rather than resolved (see the file header).
#' @noRd
.xlsx_color_hex <- function(props_node) {
  if (is.null(props_node)) return(NA_character_)
  node <- xml2::xml_find_first(props_node, "./*[local-name()='color']")
  if (inherits(node, "xml_missing")) return(NA_character_)
  rgb <- xml2::xml_attr(node, "rgb")
  if (is.na(rgb) || !nzchar(rgb)) return(NA_character_)
  rgb <- toupper(rgb)
  if (nchar(rgb) == 8L) rgb <- substr(rgb, 3L, 8L)
  rgb
}

#' The formatting properties of one `<rPr>` / `<font>` node, in seam-1 terms.
#' @noRd
.xlsx_font_props <- function(props_node) {
  list(
    color_hex = .xlsx_color_hex(props_node),
    bold      = .xlsx_flag(props_node, "b"),
    italic    = .xlsx_flag(props_node, "i"),
    underline = .xlsx_flag(props_node, "u"),
    strike    = .xlsx_flag(props_node, "strike")
  )
}

#' The default font properties, used for a cell whose style names no font and
#' as the fallback shape when styles.xml is absent entirely.
#' @noRd
.xlsx_default_font <- function() {
  list(color_hex = NA_character_, bold = FALSE, italic = FALSE,
       underline = FALSE, strike = FALSE)
}

#' Read xl/styles.xml into what a cell needs: the font table, and the
#' cellXfs mapping that turns a cell's `s=` style index into a font index.
#'
#' @return list(fonts, xf_font). `fonts` is a list of font-property lists in
#'   declaration order (1-based); `xf_font` maps 1-based xf index to 1-based
#'   font index. Both empty when the workbook has no styles part.
#' @noRd
.xlsx_styles <- function(dir) {
  doc <- .xlsx_part_xml(dir, file.path("xl", "styles.xml"))
  if (is.null(doc)) return(list(fonts = list(), xf_font = integer()))

  font_nodes <- xml2::xml_find_all(
    doc, "/*[local-name()='styleSheet']/*[local-name()='fonts']/*[local-name()='font']")
  fonts <- lapply(font_nodes, .xlsx_font_props)

  xf_nodes <- xml2::xml_find_all(
    doc, "/*[local-name()='styleSheet']/*[local-name()='cellXfs']/*[local-name()='xf']")
  xf_font <- vapply(xf_nodes, function(xf) {
    id <- xml2::xml_attr(xf, "fontId")
    if (is.na(id)) 0L else as.integer(id)
  }, integer(1)) + 1L   ## fontId is 0-based

  list(fonts = fonts, xf_font = xf_font)
}

#' The font properties in force for a cell, from its `s=` style index.
#' Anything unresolvable (no style, index past the table) falls back to the
#' default font, which states no colour and no emphasis -- so an unreadable
#' style can never invent an annotation.
#' @noRd
.xlsx_cell_font <- function(styles, s_attr) {
  if (is.na(s_attr) || !nzchar(s_attr)) return(.xlsx_default_font())
  xf <- as.integer(s_attr) + 1L   ## s= is a 0-based cellXfs index
  if (is.na(xf) || xf < 1L || xf > length(styles$xf_font)) {
    return(.xlsx_default_font())
  }
  font_i <- styles$xf_font[[xf]]
  if (font_i < 1L || font_i > length(styles$fonts)) {
    return(.xlsx_default_font())
  }
  styles$fonts[[font_i]]
}

## ---------------------------------------------------------------------------
## Runs: seam 1
## ---------------------------------------------------------------------------

#' Build one seam-1 run. `props` are the run's own formatting properties;
#' NA / FALSE fields mean "not stated by this run", and the caller supplies
#' the cell font to inherit from.
#' @noRd
.xlsx_run <- function(raw_text, props) {
  list(
    text      = .normalize_shell_text(raw_text),
    raw_text  = raw_text,
    color_hex = props$color_hex,
    highlight = NA_character_,   ## no SpreadsheetML equivalent -- see seam 1
    bold      = isTRUE(props$bold),
    italic    = isTRUE(props$italic),
    underline = isTRUE(props$underline),
    strike    = isTRUE(props$strike)
  )
}

#' The run list of a rich-text container -- an `<is>` (inline string) or an
#' `<si>` (shared string). A container with `<r>` children yields one run per
#' `<r>`; one that is a bare `<t>` yields a single unformatted run, so the
#' caller can give it the cell's own font.
#' @noRd
.xlsx_text_runs <- function(container) {
  if (is.null(container) || inherits(container, "xml_missing")) return(list())

  r_nodes <- xml2::xml_find_all(container, "./*[local-name()='r']")
  if (length(r_nodes) > 0) {
    return(lapply(r_nodes, function(r) {
      props_node <- xml2::xml_find_first(r, "./*[local-name()='rPr']")
      if (inherits(props_node, "xml_missing")) props_node <- NULL
      t_nodes <- xml2::xml_find_all(r, "./*[local-name()='t']")
      raw <- paste(xml2::xml_text(t_nodes), collapse = "")
      .xlsx_run(raw, .xlsx_font_props(props_node))
    }))
  }

  t_nodes <- xml2::xml_find_all(container, "./*[local-name()='t']")
  if (length(t_nodes) == 0) return(list())
  list(.xlsx_run(paste(xml2::xml_text(t_nodes), collapse = ""),
                 .xlsx_default_font()))
}

#' Fill every formatting field a run did not state from the cell's font.
#' A run inside a rich cell usually carries a full `<rPr>` and inherits
#' nothing; a synthesized whole-cell run inherits everything, which is how an
#' entirely-red cell reaches the detection layers as a red run.
#' @noRd
.xlsx_inherit_font <- function(runs, cell_font) {
  lapply(runs, function(run) {
    if (is.na(run$color_hex)) run$color_hex <- cell_font$color_hex
    if (!isTRUE(run$bold))      run$bold      <- isTRUE(cell_font$bold)
    if (!isTRUE(run$italic))    run$italic    <- isTRUE(cell_font$italic)
    if (!isTRUE(run$underline)) run$underline <- isTRUE(cell_font$underline)
    if (!isTRUE(run$strike))    run$strike    <- isTRUE(cell_font$strike)
    run
  })
}

#' Read xl/sharedStrings.xml into a list of run lists, indexed by the 1-based
#' shared-string index (`<c t="s"><v>7</v>` refers to element 8). Returns an
#' empty list when the workbook interns no strings.
#' @noRd
.xlsx_shared_strings <- function(dir) {
  doc <- .xlsx_part_xml(dir, file.path("xl", "sharedStrings.xml"))
  if (is.null(doc)) return(list())
  si <- xml2::xml_find_all(
    doc, "/*[local-name()='sst']/*[local-name()='si']")
  lapply(si, .xlsx_text_runs)
}

## ---------------------------------------------------------------------------
## Sheets
## ---------------------------------------------------------------------------

#' The workbook's sheets in tab order, each resolved to the zip part holding
#' it.
#'
#' The sheet's `r:id` is resolved through xl/_rels/workbook.xml.rels, whose
#' Target may be workbook-relative ("worksheets/sheet1.xml", how Excel and
#' openxlsx2 write it) or package-absolute ("/xl/worksheets/sheet1.xml", how
#' openpyxl writes it). A sheet whose relationship cannot be resolved falls
#' back to position ("xl/worksheets/sheet<n>.xml"), which is the layout every
#' generator in practice uses anyway.
#'
#' @return data.frame(name, sheet_id, order, part), one row per sheet.
#' @noRd
.xlsx_sheet_index <- function(dir) {
  doc <- .xlsx_part_xml(dir, file.path("xl", "workbook.xml"))
  if (is.null(doc)) {
    cli::cli_abort("Not a readable .xlsx: xl/workbook.xml is missing.")
  }
  sheet_nodes <- xml2::xml_find_all(
    doc, "/*[local-name()='workbook']/*[local-name()='sheets']/*[local-name()='sheet']")
  if (length(sheet_nodes) == 0) {
    cli::cli_abort("Workbook declares no sheets.")
  }

  rels <- .xlsx_part_xml(dir, file.path("xl", "_rels", "workbook.xml.rels"))
  targets <- character()
  if (!is.null(rels)) {
    rel_nodes <- xml2::xml_find_all(rels, "/*/*[local-name()='Relationship']")
    ids  <- vapply(rel_nodes, function(n) xml2::xml_attr(n, "Id") %||% "",
                   character(1))
    tgts <- vapply(rel_nodes, function(n) xml2::xml_attr(n, "Target") %||% "",
                   character(1))
    targets <- stats::setNames(tgts, ids)
  }

  name     <- character(length(sheet_nodes))
  sheet_id <- integer(length(sheet_nodes))
  part     <- character(length(sheet_nodes))
  for (i in seq_along(sheet_nodes)) {
    node <- sheet_nodes[[i]]
    name[[i]] <- xml2::xml_attr(node, "name") %||% ""
    id <- xml2::xml_attr(node, "sheetId")
    sheet_id[[i]] <- if (is.na(id)) i else as.integer(id)

    rid <- xml2::xml_attr(node, "id")   ## r:id, namespace prefix stripped
    target <- if (!is.na(rid) && rid %in% names(targets)) targets[[rid]] else ""
    part[[i]] <- .xlsx_resolve_part(target, i)
  }

  data.frame(name = name, sheet_id = sheet_id, order = seq_along(sheet_nodes),
             part = part, stringsAsFactors = FALSE)
}

#' A relationship Target to a zip-member path, tolerating both the
#' workbook-relative and package-absolute forms; empty target falls back to
#' the positional layout.
#' @noRd
.xlsx_resolve_part <- function(target, position) {
  target <- as.character(target %||% "")
  if (!nzchar(target)) {
    return(file.path("xl", "worksheets", sprintf("sheet%d.xml", position)))
  }
  if (startsWith(target, "/")) return(sub("^/", "", target))
  file.path("xl", target)
}

#' The merged ranges of a sheet.
#'
#' @return data.frame(ref, row_start, row_end, col_start, col_end); zero rows
#'   when the sheet merges nothing.
#' @noRd
.xlsx_merged_ranges <- function(sheet_doc) {
  nodes <- xml2::xml_find_all(
    sheet_doc,
    "/*[local-name()='worksheet']/*[local-name()='mergeCells']/*[local-name()='mergeCell']")
  empty <- data.frame(ref = character(), row_start = integer(),
                      row_end = integer(), col_start = integer(),
                      col_end = integer(), stringsAsFactors = FALSE)
  if (length(nodes) == 0) return(empty)

  out <- empty
  for (node in nodes) {
    ref <- xml2::xml_attr(node, "ref") %||% ""
    halves <- strsplit(ref, ":", fixed = TRUE)[[1]]
    if (length(halves) != 2) next
    from <- .xlsx_a1_to_rc(halves[[1]])
    to   <- .xlsx_a1_to_rc(halves[[2]])
    if (anyNA(c(from, to))) next
    out <- rbind(out, data.frame(
      ref = ref, row_start = from[["row"]], row_end = to[["row"]],
      col_start = from[["col"]], col_end = to[["col"]],
      stringsAsFactors = FALSE))
  }
  out[order(out$row_start, out$col_start), , drop = FALSE]
}

#' Every populated cell of one sheet, with its run list.
#'
#' Only cells the file actually writes appear -- a shell's sheet is sparse
#' (blank spacer rows are simply absent), and inventing rows for the gaps
#' would misrepresent the geometry the layout classifier in xlsx_grid.R has
#' to read. `row`/`col` are the physical 1-based coordinates.
#'
#' @return data.frame(row, col, ref, kind, text, cell_color, cell_bold,
#'   cell_italic) plus the list-column `runs`. `kind` is one of "string",
#'   "number", "bool", or "blank" (a cell present only to carry formatting).
#' @noRd
.xlsx_sheet_cells <- function(sheet_doc, shared, styles) {
  cell_nodes <- xml2::xml_find_all(
    sheet_doc,
    paste0("/*[local-name()='worksheet']/*[local-name()='sheetData']",
           "/*[local-name()='row']/*[local-name()='c']"))

  n <- length(cell_nodes)
  row <- integer(n); col <- integer(n); ref <- character(n)
  kind <- character(n); text <- character(n)
  cell_color <- character(n); cell_bold <- logical(n); cell_italic <- logical(n)
  runs <- vector("list", n)

  for (i in seq_len(n)) {
    node <- cell_nodes[[i]]
    ref[[i]] <- xml2::xml_attr(node, "r") %||% ""
    rc <- .xlsx_a1_to_rc(ref[[i]])
    row[[i]] <- rc[["row"]]
    col[[i]] <- rc[["col"]]

    font <- .xlsx_cell_font(styles, xml2::xml_attr(node, "s"))
    cell_color[[i]]  <- font$color_hex %||% NA_character_
    cell_bold[[i]]   <- isTRUE(font$bold)
    cell_italic[[i]] <- isTRUE(font$italic)

    cell <- .xlsx_cell_content(node, shared)
    kind[[i]] <- cell$kind
    cell_runs <- .xlsx_inherit_font(cell$runs, font)
    runs[[i]]  <- cell_runs
    text[[i]]  <- .normalize_shell_text(paste(
      vapply(cell_runs, function(r) r$text, character(1)), collapse = ""))
  }

  out <- data.frame(
    row = row, col = col, ref = ref, kind = kind, text = text,
    cell_color = cell_color, cell_bold = cell_bold, cell_italic = cell_italic,
    stringsAsFactors = FALSE)
  out$runs <- runs
  out[order(out$row, out$col), , drop = FALSE]
}

#' The content of one `<c>` node: its kind, and its runs.
#'
#' `t=` decides where the text is. "s" indexes the shared-string table,
#' "inlineStr" carries an `<is>` container, "str" is a formula's cached string
#' result, "b" a boolean, and no `t` at all means a number in `<v>`. A
#' formula's `<f>` is never evaluated -- only its cached `<v>` is read.
#' @noRd
.xlsx_cell_content <- function(node, shared) {
  type <- xml2::xml_attr(node, "t")
  type <- if (is.na(type)) "n" else type

  if (identical(type, "inlineStr")) {
    container <- xml2::xml_find_first(node, "./*[local-name()='is']")
    return(list(kind = "string", runs = .xlsx_text_runs(container)))
  }

  if (identical(type, "s")) {
    v <- xml2::xml_find_first(node, "./*[local-name()='v']")
    if (inherits(v, "xml_missing")) return(list(kind = "blank", runs = list()))
    idx <- suppressWarnings(as.integer(xml2::xml_text(v))) + 1L
    if (is.na(idx) || idx < 1L || idx > length(shared)) {
      return(list(kind = "blank", runs = list()))
    }
    return(list(kind = "string", runs = shared[[idx]]))
  }

  v <- xml2::xml_find_first(node, "./*[local-name()='v']")
  if (inherits(v, "xml_missing")) return(list(kind = "blank", runs = list()))
  raw <- xml2::xml_text(v)
  kind <- switch(type, str = "string", b = "bool", "number")
  list(kind = kind,
       runs = list(.xlsx_run(raw, .xlsx_default_font())))
}

## ---------------------------------------------------------------------------
## Entry point
## ---------------------------------------------------------------------------

#' Read every sheet of a shell workbook down to seam-1 runs.
#'
#' The cell-layer entry point: one pass over the archive, returning what the
#' layout classifier and the section parser need and nothing more. No shell
#' semantics are applied here -- which row is a banner, which cell is a
#' placeholder, and which sheet is a TLF are all decided downstream.
#'
#' @param xlsx_path Path to the shell workbook.
#'
#' @return list(path, sheets). `sheets` is a list in tab order, each entry
#'   list(name, sheet_id, order, part, dimension, n_rows, n_cols, cells,
#'   merges) -- `cells` as returned by `.xlsx_sheet_cells()`, `merges` by
#'   `.xlsx_merged_ranges()`. `n_rows`/`n_cols` are the extent of the
#'   POPULATED cells, not the declared `<dimension>`, which a generator is
#'   free to overstate.
#'
#' @keywords internal
#' @noRd
xlsx_read_shell_cells <- function(xlsx_path) {
  if (!file.exists(xlsx_path)) {
    cli::cli_abort("Shell workbook not found: {.path {xlsx_path}}.")
  }
  dir <- .xlsx_unzip(xlsx_path)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  styles <- .xlsx_styles(dir)
  shared <- .xlsx_shared_strings(dir)
  index  <- .xlsx_sheet_index(dir)

  sheets <- vector("list", nrow(index))
  for (i in seq_len(nrow(index))) {
    doc <- .xlsx_part_xml(dir, index$part[[i]])
    if (is.null(doc)) {
      cli::cli_abort(c(
        "Sheet {.val {index$name[[i]]}} names a part that is not in the workbook.",
        "x" = "Expected {.path {index$part[[i]]}}."
      ))
    }
    cells  <- .xlsx_sheet_cells(doc, shared, styles)
    dim_node <- xml2::xml_find_first(
      doc, "/*[local-name()='worksheet']/*[local-name()='dimension']")
    sheets[[i]] <- list(
      name      = index$name[[i]],
      sheet_id  = index$sheet_id[[i]],
      order     = index$order[[i]],
      part      = index$part[[i]],
      dimension = if (inherits(dim_node, "xml_missing")) NA_character_
                  else xml2::xml_attr(dim_node, "ref"),
      n_rows    = if (nrow(cells) == 0) 0L else max(cells$row),
      n_cols    = if (nrow(cells) == 0) 0L else max(cells$col),
      cells     = cells,
      merges    = .xlsx_merged_ranges(doc)
    )
  }
  names(sheets) <- index$name

  list(path = xlsx_path, sheets = sheets)
}
