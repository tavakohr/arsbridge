## parse_decision_digest(): privacy-safe record of parser decisions ----------
## The hard requirement is the negative one: no literal document text may
## reach the output. The positive ones: the header decision, physical vs
## flattened column counts, and row classifications are all present, so the
## digest can be diffed against the raw-geometry digest from
## tools/shell_structure_digest.R.

test_that("the digest reports decisions and leaks no document text", {
  out <- tempfile(fileext = ".json")
  dg <- suppressMessages(parse_decision_digest(
    test_path("fixtures", "CDSC-ALZ-201_TLF_Shells_v1.0_annotated.docx"),
    out))

  expect_equal(dg$n_sections, 8L)
  t1 <- dg$tlfs[["T-14-1-1"]]
  expect_equal(t1$header_rows_flagged + t1$header_rows_inferred,
               t1$n_header_rows)
  expect_equal(t1$n_physical_cols, 4L)
  expect_equal(t1$n_col_headers, 4L)
  expect_gt(t1$n_stub_rows, 0L)
  ## Every stub row records its classification facts.
  expect_true(all(vapply(t1$stub_rows, function(r)
    is.logical(r$has_annot), logical(1))))

  ## THE requirement: nothing readable leaves. Known words from the fixture
  ## must not appear anywhere in the JSON, and every silhouetted field is
  ## A/a/9-only.
  txt <- paste(readLines(out), collapse = " ")
  for (word in c("Disposition", "Subject", "Demographic", "Adverse",
                 "SAFFL", "ADSL", "Cohort")) {
    expect_false(grepl(word, txt, ignore.case = TRUE),
                 info = sprintf("leaked '%s'", word))
  }
  for (lbl in c(t1$col_headers, vapply(t1$stub_rows, `[[`, character(1),
                                       "label"))) {
    expect_false(grepl("[B-Zb-z]", gsub("[Aa9]", "", lbl)),
                 info = sprintf("non-silhouette text: '%s'", lbl))
  }
  unlink(out)
})

test_that("a spanning-header stat-pair table shows its shape in the digest", {
  skip_if_not_installed("officer")

  ## The client-shell shape: 2 cohorts spanning an "xx" / "(xx.x)" stat
  ## pair each, placeholder cells in the data grid -- 5 physical columns
  ## behind 3 visual ones.
  docx <- tempfile(fileext = ".docx")
  rowgen_docx(docx, list(
    rowgen_par("Table 14.1.1"),
    rowgen_par("Spanned Stat Pair Fixture"),
    rowgen_par("Safety Population (ADSL.SAFFL='Y')"),
    rowgen_table(list(
      rowgen_row(c(rowgen_cell(""),
                   rowgen_cell("Cohort A (N=XX) ADSL.COHORTN=1",
                               gridspan = 2L),
                   rowgen_cell("Cohort B (N=XX) ADSL.COHORTN=2",
                               gridspan = 2L))),
      rowgen_row(c(rowgen_cell("Completed, n (%)  ADSL.EOSSTT='COMPLETED'"),
                   rowgen_cell("xx"), rowgen_cell("(xx.x)"),
                   rowgen_cell("xx"), rowgen_cell("(xx.x)")))
    ), n_cols = 5L),
    rowgen_par("Source: ADSL")
  ))

  out <- tempfile(fileext = ".json")
  dg <- suppressMessages(parse_decision_digest(docx, out))
  unlink(docx)

  t1 <- dg$tlfs[["T-14-1-1"]]
  ## The digest must expose exactly the facts that diagnose a column
  ## mismatch: the physical width, the header decision, and what the
  ## flattening produced.
  expect_equal(t1$n_physical_cols, 5L)
  expect_equal(t1$n_header_rows,
               t1$header_rows_flagged + t1$header_rows_inferred)
  expect_true(t1$n_col_headers >= 1L)
  ## A placeholder-shaped column label is visible AS a placeholder shape.
  txt <- paste(readLines(out), collapse = " ")
  expect_false(grepl("Cohort", txt))
  unlink(out)
})
