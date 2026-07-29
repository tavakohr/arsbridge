## The cell paragraph list and the dual-join annotation retry ----------------
## A cell's paragraph list is its lossless view; how the paragraphs JOIN
## depends on the consumer (label: space, annotation: bare). These tests
## drive the semantic layer with plain character vectors -- no .docx needed.

test_that("wrapped retry recovers an annotation the space join splits", {
  ## "Completed study (ADSL" / ".EOSSTT='COMPLETED')": no open square
  ## bracket and no mid-reference tail, so .cell_text() joins with a space
  ## and the plain-text layer misses the reference entirely.
  paras <- c("Completed study (ADSL", ".EOSSTT='COMPLETED')")
  joined <- paste(paras, collapse = " ")
  expect_equal(.detect_annotation(joined, list())$annotation, "")

  w <- .detect_annotation_wrapped(paras)
  expect_equal(w$annotation, "ADSL.EOSSTT='COMPLETED'")
  expect_equal(w$label, "Completed study")
  expect_equal(w$method, "pattern_wrapped")
  expect_equal(w$confidence, "medium")
})

test_that("wrapped retry keeps the label spaced when only the label wraps", {
  ## A wrapped LABEL with a trailing annotation: the label pieces join with
  ## spaces, never fused ("dataextraction").
  paras <- c("Ongoing subjects at the time of the data",
             "extraction, n (%) ADSL.ONGOFL='Y'")
  w <- .detect_annotation_wrapped(paras)
  expect_equal(w$annotation, "ADSL.ONGOFL='Y'")
  expect_equal(w$label,
               "Ongoing subjects at the time of the data extraction, n (%)")
})

test_that("wrapped retry finds a mid-token bracket wrap", {
  w <- .detect_annotation_wrapped(c("Cohort 1 (N=XX) [ADSL.C", "GHGR1N=1]"))
  expect_equal(w$annotation, "ADSL.CGHGR1N=1")
  expect_equal(w$label, "Cohort 1 (N=XX)")
})

test_that("wrapped retry returns NULL when there is nothing to find", {
  expect_null(.detect_annotation_wrapped(
    c("Ongoing subjects at the time of the data", "extraction, n (%)")))
  expect_null(.detect_annotation_wrapped("single paragraph ADSL.AGE"))
  expect_null(.detect_annotation_wrapped(character(0)))
})

test_that(".cell_paragraphs keeps the paragraph list intact", {
  cell <- xml2::read_xml(paste0(
    '<w:tc xmlns:w="http://schemas.openxmlformats.org/',
    'wordprocessingml/2006/main">',
    "<w:p><w:r><w:t>first line</w:t></w:r></w:p>",
    "<w:p><w:r><w:t/></w:r></w:p>",
    "<w:p><w:r><w:t>second line</w:t></w:r></w:p>",
    "</w:tc>"))
  expect_equal(.cell_paragraphs(cell), c("first line", "second line"))
})

test_that("end-to-end: a wrapped stub annotation reaches the parsed row", {
  skip_if_not_installed("officer")

  docx <- tempfile(fileext = ".docx")
  rowgen_docx(docx, list(
    rowgen_par("Table 14.1.1"),
    rowgen_par("Wrapped Annotation Fixture"),
    rowgen_par("Safety Population (ADSL.SAFFL='Y')"),
    rowgen_table(list(
      rowgen_row(c(rowgen_cell("Characteristic"),
                   rowgen_cell("Treatment A"),
                   rowgen_cell("Placebo")),
                 header = TRUE),
      rowgen_row(c(rowgen_cell(c("Completed study (ADSL",
                                 ".EOSSTT='COMPLETED')")),
                   rowgen_cell(""), rowgen_cell("")))
    )),
    rowgen_par("Source: ADSL")
  ))

  diag_reset()
  sections <- suppressMessages(parse_shell_docx(docx))
  unlink(docx)

  rows <- sections[[1]]$stub_rows
  hit <- Filter(function(r) grepl("^Completed study", r$label %||% ""), rows)
  expect_length(hit, 1L)
  expect_equal(hit[[1]]$annotation, "ADSL.EOSSTT='COMPLETED'")
  expect_equal(hit[[1]]$detection_method, "pattern_wrapped")
  expect_true(hit[[1]]$has_annot)
})
