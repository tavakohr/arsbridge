## Row-accounting invariant ---------------------------------------------------
## Every <w:tr> in a parsed table must end up classified: header row, data
## row, or skipped with a known reason (no cells / vMerge ghost / struck
## through). Every top-level <w:tbl> must be handled: attached to a section,
## recognised as the TOC, or flagged as unparsed. A miss on either count is
## a FAIL diagnostic -- the tripwire for the silently-dropped-rows bug class.

## The combinatorial sweep: all 2^6 combinations of the table features that
## have each individually caused a parsing bug. The hand-authored fixtures
## cover single features; the bugs live in the combinations.
test_that("row accounting holds across the feature cross-product", {
  skip_if_not_installed("officer")

  features <- expand.grid(
    continuation     = c(FALSE, TRUE),
    unflagged_header = c(FALSE, TRUE),
    vmerge           = c(FALSE, TRUE),
    multi_para       = c(FALSE, TRUE),
    struck           = c(FALSE, TRUE),
    gridspan         = c(FALSE, TRUE)
  )

  for (i in seq_len(nrow(features))) {
    f <- features[i, ]
    combo <- paste(names(f)[unlist(f)], collapse = "+")
    if (!nzchar(combo)) combo <- "plain"

    docx <- tempfile(fileext = ".docx")
    rowgen_shell(docx,
                 continuation     = f$continuation,
                 unflagged_header = f$unflagged_header,
                 vmerge           = f$vmerge,
                 multi_para       = f$multi_para,
                 struck           = f$struck,
                 gridspan         = f$gridspan)

    diag_reset()
    sections <- suppressMessages(parse_shell_docx(docx))
    d <- diag_records()
    unlink(docx)

    ## The invariant: no combination may trip the accounting FAIL.
    fails <- d[d$severity == "FAIL", , drop = FALSE]
    expect_equal(nrow(fails), 0L,
                 info = sprintf("combo '%s' tripped: %s", combo,
                                paste(fails$problem, collapse = " | ")))

    ## And the document still parses to exactly one section with rows.
    expect_length(sections, 1L)
    expect_gt(length(sections[[1]]$stub_rows), 0L)

    ## Feature-specific accounting: a struck row must leave its trace.
    if (f$struck) {
      expect_true(any(grepl("struck through", d$problem, fixed = TRUE)),
                  info = sprintf("combo '%s': no strike diagnostic", combo))
    }
  }
})

test_that("a struck row and a vMerge ghost are skipped but counted", {
  skip_if_not_installed("officer")

  docx <- tempfile(fileext = ".docx")
  rowgen_shell(docx, vmerge = TRUE, struck = TRUE)

  diag_reset()
  sections <- suppressMessages(parse_shell_docx(docx))
  d <- diag_records()
  unlink(docx)

  expect_equal(sum(d$severity == "FAIL"), 0L)
  labels <- vapply(sections[[1]]$stub_rows,
                   function(r) r$label %||% "", character(1))
  ## The struck row and the ghost row must NOT surface as stub rows.
  expect_false(any(grepl("Weight", labels)))
  ## The vMerge anchor row itself IS a real row.
  expect_true(any(grepl("Race", labels)))
})

test_that(".check_table_row_accounting FAILs on an unaccounted row", {
  ## Fabricate an undercount directly: a 3-row table where only 2 rows are
  ## accounted for. This proves the tripwire itself fires.
  tbl <- xml2::read_xml(paste0(
    '<w:tbl xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/',
    '2006/main">',
    "<w:tr><w:tc><w:p/></w:tc></w:tr>",
    "<w:tr><w:tc><w:p/></w:tc></w:tr>",
    "<w:tr><w:tc><w:p/></w:tc></w:tr>",
    "</w:tbl>"))
  collected <- list(rows = list(list(label = "x")),
                    n_empty = 0L, n_vmerge = 0L, n_struck = 0L)

  diag_reset()
  ok <- .check_table_row_accounting(tbl, n_header_rows = 1L,
                                    collected = collected,
                                    tlf_number = "T-14-1-1")
  d <- diag_records()

  expect_false(ok)
  expect_equal(sum(d$severity == "FAIL"), 1L)
  expect_match(d$problem[d$severity == "FAIL"], "Row accounting failed")
})

test_that("a table before any recognised heading is flagged, not dropped", {
  skip_if_not_installed("officer")

  ## No TLF heading at all: the table cannot be attached to a section.
  docx <- tempfile(fileext = ".docx")
  rowgen_docx(docx, list(
    rowgen_par("Just an ordinary paragraph"),
    rowgen_table(list(
      rowgen_row(c(rowgen_cell("Characteristic"),
                   rowgen_cell("Treatment A"),
                   rowgen_cell("Placebo")),
                 header = TRUE),
      rowgen_row(c(rowgen_cell("Age (years)  ADSL.AGE"),
                   rowgen_cell(""), rowgen_cell("")))
    ))
  ))

  diag_reset()
  sections <- suppressMessages(suppressWarnings(parse_shell_docx(docx)))
  d <- diag_records()
  unlink(docx)

  expect_length(sections, 0L)
  ## The table is accounted for: no ACCOUNTING failure. (The pre-existing
  ## "No TLF sections found" FAIL still fires -- that one is correct here.)
  expect_equal(sum(grepl("accounting failed", d$problem)), 0L)
  orphan <- d[d$severity == "WARN" &
                grepl("before any recognised TLF heading", d$problem), ,
              drop = FALSE]
  expect_equal(nrow(orphan), 1L)
  expect_match(orphan$problem, "2 row\\(s\\)")
})

test_that("an empty <w:tr> is skipped with a diagnostic, not silently", {
  skip_if_not_installed("officer")

  docx <- tempfile(fileext = ".docx")
  rowgen_docx(docx, list(
    rowgen_par("Table 14.1.1"),
    rowgen_par("Empty Row Fixture"),
    rowgen_par("Safety Population (ADSL.SAFFL='Y')"),
    rowgen_table(list(
      rowgen_row(c(rowgen_cell("Characteristic"),
                   rowgen_cell("Treatment A"),
                   rowgen_cell("Placebo")),
                 header = TRUE),
      rowgen_row(c(rowgen_cell("Age (years)  ADSL.AGE"),
                   rowgen_cell(""), rowgen_cell(""))),
      "<w:tr></w:tr>"
    )),
    rowgen_par("Source: ADSL")
  ))

  diag_reset()
  sections <- suppressMessages(parse_shell_docx(docx))
  d <- diag_records()
  unlink(docx)

  expect_length(sections, 1L)
  expect_equal(sum(d$severity == "FAIL"), 0L)
  expect_true(any(grepl("has no cells", d$problem, fixed = TRUE)))
})
