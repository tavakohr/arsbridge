## helper-rowgen.R -------------------------------------------------------------
## Generates small annotated-shell .docx files for the row-accounting tests.
##
## The hand-authored fixtures each reproduce ONE bug that was already found;
## the combinations are where new bugs live. This helper builds a shell from
## raw OOXML with independently toggled table features (continuation table,
## unflagged multi-row header, vMerge stub, multi-paragraph cell, struck-
## through row, gridSpan data row) so a test can sweep the cross-product and
## assert the row-accounting invariant on every combination.
##
## An officer skeleton supplies the docx package parts ([Content_Types].xml,
## _rels, styles, ...); only word/document.xml is replaced with generated
## content. The skeleton is built once per test run and cached.

.rowgen_env <- new.env(parent = emptyenv())

.rowgen_skeleton_docx <- function() {
  if (!is.null(.rowgen_env$skeleton)) return(.rowgen_env$skeleton)
  skel_docx <- file.path(tempdir(), "rowgen_skeleton.docx")
  print(officer::read_docx(), target = skel_docx)
  .rowgen_env$skeleton <- skel_docx
  skel_docx
}

.rowgen_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

## One run; struck = TRUE adds <w:strike/> so .run_is_struck() sees it.
rowgen_run <- function(text, struck = FALSE) {
  rpr <- if (struck) "<w:rPr><w:strike/></w:rPr>" else ""
  sprintf('<w:r>%s<w:t xml:space="preserve">%s</w:t></w:r>',
          rpr, .rowgen_escape(text))
}

rowgen_par <- function(text, struck = FALSE) {
  sprintf("<w:p>%s</w:p>", rowgen_run(text, struck = struck))
}

## One table cell. `paras` is a character vector -- more than one element
## makes a multi-paragraph cell (a wrapped label). `vmerge` is NULL,
## "restart" (anchor of a vertical merge) or "continue" (the ghost half).
rowgen_cell <- function(paras, gridspan = 1L, vmerge = NULL, struck = FALSE) {
  props <- character(0)
  if (gridspan > 1L) {
    props <- c(props, sprintf('<w:gridSpan w:val="%d"/>', gridspan))
  }
  if (!is.null(vmerge)) {
    props <- c(props,
               if (identical(vmerge, "restart")) '<w:vMerge w:val="restart"/>'
               else "<w:vMerge/>")
  }
  tc_pr <- if (length(props) > 0) {
    sprintf("<w:tcPr>%s</w:tcPr>", paste(props, collapse = ""))
  } else {
    ""
  }
  body <- paste(vapply(paras, rowgen_par, character(1), struck = struck),
                collapse = "")
  ## A cell must contain at least one paragraph to be valid OOXML.
  if (length(paras) == 0) body <- "<w:p/>"
  sprintf("<w:tc>%s%s</w:tc>", tc_pr, body)
}

## One table row from already-built cells; header = TRUE sets Word's
## "repeat as header row" flag (<w:tblHeader/>).
rowgen_row <- function(cells, header = FALSE) {
  tr_pr <- if (header) "<w:trPr><w:tblHeader/></w:trPr>" else ""
  sprintf("<w:tr>%s%s</w:tr>", tr_pr, paste(cells, collapse = ""))
}

rowgen_table <- function(rows, n_cols = 3L) {
  grid <- paste(rep('<w:gridCol w:w="2400"/>', n_cols), collapse = "")
  sprintf("<w:tbl><w:tblPr/><w:tblGrid>%s</w:tblGrid>%s</w:tbl>",
          grid, paste(rows, collapse = ""))
}

## Assemble a .docx whose body is exactly `blocks` (paragraph/table OOXML
## strings, in order) followed by the skeleton's sectPr.
rowgen_docx <- function(path, blocks) {
  td <- tempfile("rowgen_doc_")
  dir.create(td)
  utils::unzip(.rowgen_skeleton_docx(), exdir = td)

  doc_xml_path <- file.path(td, "word", "document.xml")
  d <- xml2::read_xml(doc_xml_path)
  body <- xml2::xml_find_first(d, ".//*[local-name()='body']")

  ## Drop everything except the section properties, then insert our blocks
  ## before it. Blocks are parsed inside a namespaced wrapper so the w:
  ## prefix resolves.
  for (child in xml2::xml_children(body)) {
    if (!identical(sub("^.*:", "", xml2::xml_name(child)), "sectPr")) {
      xml2::xml_remove(child)
    }
  }
  w_ns <- "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
  wrapper <- xml2::read_xml(sprintf(
    '<w:body xmlns:w="%s">%s</w:body>',
    w_ns, paste(blocks, collapse = "")))
  sect_pr <- xml2::xml_find_first(body, "./*[local-name()='sectPr']")
  for (blk in xml2::xml_children(wrapper)) {
    if (inherits(sect_pr, "xml_missing")) {
      xml2::xml_add_child(body, blk)
    } else {
      xml2::xml_add_sibling(sect_pr, blk, .where = "before")
    }
  }
  xml2::write_xml(d, doc_xml_path)

  ## Re-zip. all.files = TRUE keeps `_rels/.rels`, which list.files()
  ## otherwise drops as hidden -- without it the docx is unreadable.
  abs_out <- normalizePath(path, winslash = "/", mustWork = FALSE)
  cwd <- getwd()
  on.exit(setwd(cwd), add = TRUE)
  setwd(td)
  files <- list.files(".", recursive = TRUE, all.files = TRUE,
                      full.names = FALSE)
  files <- sub("^\\./", "", files)
  unlink(abs_out)
  utils::zip(zipfile = abs_out, files = files, flags = "-r9Xq")
  setwd(cwd)
  unlink(td, recursive = TRUE)
  invisible(path)
}

## Build one shell .docx for a feature combination. Returns the path.
rowgen_shell <- function(path,
                         continuation     = FALSE,
                         unflagged_header = FALSE,
                         vmerge           = FALSE,
                         multi_para       = FALSE,
                         struck           = FALSE,
                         gridspan         = FALSE) {
  header_rows <- if (unflagged_header) {
    ## Two-row nested header with NO <w:tblHeader/> flag anywhere: a blank
    ## stub cell over a spanned arm cell, then the subcolumn labels. This is
    ## the shape .inferred_header_row_count() has to guess about.
    list(
      rowgen_row(c(rowgen_cell(""),
                   rowgen_cell("Treatment A", gridspan = 2L))),
      rowgen_row(c(rowgen_cell("Characteristic"),
                   rowgen_cell("n"),
                   rowgen_cell("(%)")))
    )
  } else {
    list(
      rowgen_row(c(rowgen_cell("Characteristic"),
                   rowgen_cell("Treatment A"),
                   rowgen_cell("Placebo")),
                 header = TRUE)
    )
  }

  data_rows <- list(
    rowgen_row(c(rowgen_cell("Age (years)  ADSL.AGE"),
                 rowgen_cell(""), rowgen_cell("")))
  )
  if (multi_para) {
    ## A stub label wrapped across two paragraphs of one cell.
    data_rows <- c(data_rows, list(
      rowgen_row(c(rowgen_cell(c("Ongoing subjects at data",
                                 "extraction, n (%)  ADSL.ONGOFL='Y'")),
                   rowgen_cell(""), rowgen_cell("")))
    ))
  }
  if (vmerge) {
    ## An anchor row plus its vMerge-continuation ghost row below it.
    data_rows <- c(data_rows, list(
      rowgen_row(c(rowgen_cell("Race, n (%)  ADSL.RACE", vmerge = "restart"),
                   rowgen_cell(""), rowgen_cell(""))),
      rowgen_row(c(rowgen_cell(character(0), vmerge = "continue"),
                   rowgen_cell(""), rowgen_cell("")))
    ))
  }
  if (struck) {
    data_rows <- c(data_rows, list(
      rowgen_row(c(rowgen_cell("Weight (kg)  ADSL.WEIGHT", struck = TRUE),
                   rowgen_cell("", struck = TRUE),
                   rowgen_cell("", struck = TRUE)))
    ))
  }
  if (gridspan) {
    ## A data row whose stub cell spans two grid columns.
    data_rows <- c(data_rows, list(
      rowgen_row(c(rowgen_cell("All subjects  ADSL.SAFFL='Y'",
                               gridspan = 2L),
                   rowgen_cell("")))
    ))
  }

  blocks <- list(
    rowgen_par("Table 14.1.1"),
    rowgen_par("Combination Fixture Summary"),
    rowgen_par("Safety Population (ADSL.SAFFL='Y')"),
    rowgen_table(c(header_rows, data_rows))
  )
  if (continuation) {
    blocks <- c(blocks, list(
      rowgen_table(list(
        ## Continuation restates the header (flagged this time) and adds one
        ## more data row.
        rowgen_row(c(rowgen_cell("Characteristic"),
                     rowgen_cell("Treatment A"),
                     rowgen_cell("Placebo")),
                   header = TRUE),
        rowgen_row(c(rowgen_cell("Height (cm)  ADSL.HEIGHT"),
                     rowgen_cell(""), rowgen_cell("")))
      ))
    ))
  }
  blocks <- c(blocks, list(rowgen_par("Source: ADSL")))

  rowgen_docx(path, blocks)
}
