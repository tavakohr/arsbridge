## .table_grid(): the R x C occupancy model of a Word table ------------------
## Pure-XML tests: a <w:tbl> string parses straight into a grid, no .docx
## fixture needed.

.tbl_xml <- function(inner, n_cols) {
  grid_cols <- paste(rep('<w:gridCol w:w="2400"/>', n_cols), collapse = "")
  xml2::read_xml(sprintf(
    paste0('<w:tbl xmlns:w="http://schemas.openxmlformats.org/',
           'wordprocessingml/2006/main">',
           "<w:tblPr/><w:tblGrid>%s</w:tblGrid>%s</w:tbl>"),
    grid_cols, inner))
}

.tc <- function(text, span = 1L, vmerge = NULL) {
  props <- character(0)
  if (span > 1L) props <- c(props, sprintf('<w:gridSpan w:val="%d"/>', span))
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
  sprintf('<w:tc>%s<w:p><w:r><w:t>%s</w:t></w:r></w:p></w:tc>', tc_pr, text)
}

.tr <- function(...) sprintf("<w:tr>%s</w:tr>", paste(c(...), collapse = ""))

test_that("a plain table expands one cell per position", {
  tbl <- .tbl_xml(.tr(.tc("A"), .tc("B"), .tc("C")), n_cols = 3L)
  g <- .table_grid(tbl)
  expect_equal(g$n_rows, 1L)
  expect_equal(g$n_cols, 3L)
  expect_equal(vapply(g$rows[[1]], `[[`, character(1), "kind"),
               rep("cell", 3))
  expect_equal(.grid_text(g, 1, 2), "B")
})

test_that("gridSpan repeats a cell across the columns it covers", {
  tbl <- .tbl_xml(paste0(
    .tr(.tc(""), .tc("Treatment A", span = 2L)),
    .tr(.tc("Char"), .tc("n"), .tc("(%)"))
  ), n_cols = 3L)
  g <- .table_grid(tbl)
  kinds <- vapply(g$rows[[1]], `[[`, character(1), "kind")
  expect_equal(kinds, c("cell", "cell", "span"))
  ## The span tail points back at its anchor and shows its text.
  expect_equal(g$rows[[1]][[3]]$anchor_col, 2L)
  expect_equal(.grid_text(g, 1, 3), "Treatment A")
  ## The second row is unaffected.
  expect_equal(.grid_text(g, 2, 3), "(%)")
})

test_that("vMerge continuations inherit their anchor and mark ghost rows", {
  tbl <- .tbl_xml(paste0(
    .tr(.tc("Race", vmerge = "restart"), .tc("x"), .tc("y")),
    .tr(.tc("", vmerge = "continue"), .tc("x2"), .tc("y2")),
    .tr(.tc("", vmerge = "continue"), .tc("x3"), .tc("y3"))
  ), n_cols = 3L)
  g <- .table_grid(tbl)
  expect_equal(g$rows[[2]][[1]]$kind, "vmerge")
  ## The chain resolves to the TOP cell in one hop, even two rows down.
  expect_equal(g$rows[[3]][[1]]$anchor_row, 1L)
  expect_equal(.grid_text(g, 3, 1), "Race")
  expect_false(.grid_row_is_ghost(g, 1))
  expect_true(.grid_row_is_ghost(g, 2))
  expect_true(.grid_row_is_ghost(g, 3))
})

test_that("a ragged row pads with missing positions", {
  tbl <- .tbl_xml(paste0(
    .tr(.tc("A"), .tc("B"), .tc("C")),
    .tr(.tc("only"))
  ), n_cols = 3L)
  g <- .table_grid(tbl)
  kinds <- vapply(g$rows[[2]], `[[`, character(1), "kind")
  expect_equal(kinds, c("cell", "missing", "missing"))
  expect_equal(.grid_text(g, 2, 3), "")
})

test_that(".grid_n_cols falls back to the widest row without a tblGrid", {
  tbl <- xml2::read_xml(paste0(
    '<w:tbl xmlns:w="http://schemas.openxmlformats.org/',
    'wordprocessingml/2006/main"><w:tblPr/>',
    .tr(.tc("A", span = 2L), .tc("B")),
    .tr(.tc("C")),
    "</w:tbl>"))
  expect_equal(.grid_n_cols(tbl), 3L)
})

test_that(".cell_anchor_cols accounts for spans", {
  tbl <- .tbl_xml(.tr(.tc("wide", span = 2L), .tc("B"), .tc("C")),
                  n_cols = 4L)
  row <- xml2::xml_find_first(tbl, "./*[local-name()='tr']")
  cells <- xml2::xml_find_all(row, "./*[local-name()='tc']")
  expect_equal(.cell_anchor_cols(cells), c(1L, 3L, 4L))
})

test_that("a continuation table with a different column count is refused", {
  skip_if_not_installed("officer")

  ## Same heading, second table has 4 columns instead of 3: a different
  ## display, not a continuation -- its rows must NOT be welded on.
  docx <- tempfile(fileext = ".docx")
  rowgen_docx(docx, list(
    rowgen_par("Table 14.1.1"),
    rowgen_par("Continuation Mismatch Fixture"),
    rowgen_par("Safety Population (ADSL.SAFFL='Y')"),
    rowgen_table(list(
      rowgen_row(c(rowgen_cell("Characteristic"),
                   rowgen_cell("Treatment A"),
                   rowgen_cell("Placebo")),
                 header = TRUE),
      rowgen_row(c(rowgen_cell("Age (years)  ADSL.AGE"),
                   rowgen_cell(""), rowgen_cell("")))
    ), n_cols = 3L),
    rowgen_table(list(
      rowgen_row(c(rowgen_cell("Parameter"),
                   rowgen_cell("Visit"),
                   rowgen_cell("Treatment A"),
                   rowgen_cell("Placebo")),
                 header = TRUE),
      rowgen_row(c(rowgen_cell("Weight (kg)  ADSL.WEIGHT"),
                   rowgen_cell(""), rowgen_cell(""), rowgen_cell("")))
    ), n_cols = 4L),
    rowgen_par("Source: ADSL")
  ))

  diag_reset()
  sections <- suppressMessages(parse_shell_docx(docx))
  d <- diag_records()
  unlink(docx)

  labels <- vapply(sections[[1]]$stub_rows,
                   function(r) r$label %||% "", character(1))
  expect_true(any(grepl("^Age", labels)))
  expect_false(any(grepl("^Weight", labels)))
  refusal <- d[grepl("were NOT appended", d$problem), , drop = FALSE]
  expect_equal(nrow(refusal), 1L)
  expect_equal(refusal$severity, "FAIL")
})

test_that("a matching continuation table still appends (guard lets it through)", {
  skip_if_not_installed("officer")

  docx <- tempfile(fileext = ".docx")
  rowgen_shell(docx, continuation = TRUE)

  diag_reset()
  sections <- suppressMessages(parse_shell_docx(docx))
  d <- diag_records()
  unlink(docx)

  labels <- vapply(sections[[1]]$stub_rows,
                   function(r) r$label %||% "", character(1))
  expect_true(any(grepl("^Height", labels)))
  expect_equal(sum(grepl("were NOT appended", d$problem)), 0L)
})
