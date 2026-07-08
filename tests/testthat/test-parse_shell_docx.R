test_that("parse_shell_docx finds the 2 TLF sections in the synthetic fixture", {
  secs <- parse_shell_docx(test_path("fixtures/annotated_shell_2tlf_minimal.docx"))
  expect_length(secs, 2)
  expect_equal(vapply(secs, `[[`, character(1), "tlf_number"),
               c("T-14-1-1", "T-14-3-1"))
  expect_equal(vapply(secs, `[[`, character(1), "tlf_type"),
               c("TABLE", "TABLE"))
})

test_that("title and population text captured", {
  secs <- parse_shell_docx(test_path("fixtures/annotated_shell_2tlf_minimal.docx"))
  expect_match(secs[[1]]$title, "Demographic")
  expect_match(secs[[1]]$population_text, "Safety Population")
})

test_that("population annotation extracted with full equality suffix", {
  secs <- parse_shell_docx(test_path("fixtures/annotated_shell_2tlf_minimal.docx"))
  expect_equal(secs[[1]]$population_annot, "ADSL.SAFFL='Y'")
  expect_equal(secs[[2]]$population_annot, "ADSL.SAFFL='Y'")
})

test_that("annotated rows are flagged has_annot = TRUE", {
  secs <- parse_shell_docx(test_path("fixtures/annotated_shell_2tlf_minimal.docx"))
  annotated <- Filter(function(r) isTRUE(r$has_annot), secs[[1]]$stub_rows)
  expect_gte(length(annotated), 3)
  expect_true(any(vapply(annotated, function(r) r$annotation == "ADSL.AGE", logical(1))))
})

test_that("child sub-rows (n, Mean (SD), Median, Male, Female) have has_annot = FALSE", {
  secs <- parse_shell_docx(test_path("fixtures/annotated_shell_2tlf_minimal.docx"))
  child_labels <- c("n", "Mean (SD)", "Median", "Male", "Female",
                    "White", "Black or African American")
  for (sec in secs) {
    children <- Filter(function(r) r$label %in% child_labels, sec$stub_rows)
    expect_true(all(vapply(children, function(r) !isTRUE(r$has_annot), logical(1))),
                info = sec$tlf_number)
  }
})

test_that("source datasets extracted from the Source: line", {
  secs <- parse_shell_docx(test_path("fixtures/annotated_shell_2tlf_minimal.docx"))
  expect_equal(secs[[1]]$source_datasets, "ADSL")
  expect_equal(secs[[2]]$source_datasets, c("ADSL", "ADAE"))
})

test_that("Layer 1 colour detection sets confidence=high for full DATASET.VAR", {
  ## The fixture's stub-cell annotations carry a genuine red C00000 run
  ## (build_fixtures.R's repaint_red()), so Layer 1 (colour) fires here, not
  ## Layer 3 (plain text) -- colour is checked first in .detect_annotation().
  secs <- parse_shell_docx(test_path("fixtures/annotated_shell_2tlf_minimal.docx"))
  annotated <- Filter(function(r) isTRUE(r$has_annot), secs[[1]]$stub_rows)
  expect_true(all(vapply(annotated, function(r) r$detection_method == "colour", logical(1))))
  expect_true(all(vapply(annotated, function(r) r$detection_confidence == "high", logical(1))))
})

test_that("parse_shell_docx aborts on missing file", {
  expect_error(parse_shell_docx("nonexistent.docx"), "not found")
})

test_that("parse_shell_docx walks the bundled annotated shell end to end", {
  shell <- tryCatch(arsbridge_example("annotated_shell.docx"),
                    error = function(e) "")
  skip_if(!nzchar(shell) || !file.exists(shell), "bundled shell not available")

  secs <- arsbridge:::parse_shell_docx(shell)
  expect_type(secs, "list")
  expect_gt(length(secs), 0)
  # Every section carries a TLF number.
  expect_true(all(vapply(secs, function(s) !is.null(s$tlf_number), logical(1))))
  # Some sections have stub rows, and the deterministic detector annotated some.
  expect_true(any(vapply(secs,
    function(s) length(s$stub_rows) > 0, logical(1))))
  any_annot <- any(vapply(secs, function(s)
    any(vapply(s$stub_rows, function(r) isTRUE(r$has_annot), logical(1))),
    logical(1)))
  expect_true(any_annot)
})

test_that("parse_shell_docx validates listing headers against a spec lookup", {
  shell <- tryCatch(arsbridge_example("annotated_shell.docx"),
                    error = function(e) "")
  skip_if(!nzchar(shell) || !file.exists(shell), "bundled shell not available")
  # Exercise the spec_lookup branch of the column-header detector.
  lookup <- list(ADSL.USUBJID = list(), ADAE.AEDECOD = list())
  secs <- arsbridge:::parse_shell_docx(shell, spec_lookup = lookup)
  expect_type(secs, "list")
  expect_gt(length(secs), 0)
})

## --- F1: flexible heading / title / population state machine --------------

test_that("an inline heading ('Table X: Title') captures the title on the same line", {
  secs <- parse_shell_docx(test_path("fixtures/annotated_shell_inline_heading.docx"))
  expect_length(secs, 1)
  expect_equal(secs[[1]]$tlf_number, "T-14-1-1")
  expect_equal(secs[[1]]$title, "Summary of Demographic Characteristics")
  expect_equal(secs[[1]]$population_annot, "ADSL.SAFFL='Y'")
})

test_that("the heading regex requires a colon for an inline title -- prose mentioning a table number is not a heading", {
  ## Guards the F1 heading-regex fix: allowing an inline title must not
  ## start matching ordinary prose that merely references a table number.
  txt <- "Table 14.1.1 shows the demographic summary"
  m <- regmatches(txt, regexec(.TLF_HEADING_RE, txt, perl = TRUE))[[1]]
  expect_length(m, 0)
})

test_that("a listing with no population line leaves population_text empty, not eaten", {
  secs <- parse_shell_docx(test_path("fixtures/annotated_shell_no_population.docx"))
  expect_length(secs, 1)
  expect_equal(secs[[1]]$tlf_type, "LISTING")
  expect_equal(secs[[1]]$title, "Subject-Level Listing of Deaths")
  expect_equal(secs[[1]]$population_text, "")
  # The Source line after the table must still be captured, not swallowed
  # as population text.
  expect_equal(secs[[1]]$source_datasets, "ADSL")
})

test_that("a two-paragraph title is joined before the population line is read", {
  secs <- parse_shell_docx(test_path("fixtures/annotated_shell_two_line_title.docx"))
  expect_length(secs, 1)
  expect_equal(secs[[1]]$title,
              "Summary of Concomitant Medications by ATC Class")
  expect_equal(secs[[1]]$population_annot, "ADSL.SAFFL='Y'")
})

## --- F3: merged cells, multi-row headers, non-stub annotations -------------

test_that("a spanned (gridSpan) two-row header counts real data columns, not raw cells", {
  secs <- parse_shell_docx(test_path("fixtures/annotated_shell_merged_headers.docx"))
  sec <- secs[[1]]
  # Row 1 has 3 raw cells (stub + 2 spanned arm labels); the true grid is
  # 5 columns (stub + 2 arms x 2 subcolumns each).
  expect_equal(sec$col_headers,
              c("Category", "Treatment A n", "Treatment A (%)",
                "Placebo n", "Placebo (%)"))
  expect_equal(sec$n_data_cols, 4L)
})

test_that("a vMerge-continuation stub cell is dropped, not read as its own row", {
  secs <- parse_shell_docx(test_path("fixtures/annotated_shell_merged_headers.docx"))
  labels <- vapply(secs[[1]]$stub_rows, function(r) r$label, character(1))
  expect_equal(labels, c("Any AE", "Headache"))
})

test_that("an annotation in a data cell is bound to its row and logged", {
  diag_reset()
  secs <- parse_shell_docx(test_path("fixtures/annotated_shell_merged_headers.docx"))
  row <- Filter(function(r) r$label == "Any AE", secs[[1]]$stub_rows)[[1]]
  expect_true(row$has_annot)
  expect_equal(row$annotation, "ADAE.TRTEMFL='Y'")
  expect_equal(row$detection_method, "data_cell")

  diags <- diag_records()
  expect_true(any(grepl("data column", diags$problem)))
})

## --- F4: comments, highlights, tracked changes, text boxes -----------------

test_that("a Word comment anchored to a stub cell supplies its annotation", {
  secs <- parse_shell_docx(test_path("fixtures/annotated_shell_comments_highlights.docx"))
  row <- Filter(function(r) r$label == "Weight (kg)", secs[[1]]$stub_rows)[[1]]
  expect_true(row$has_annot)
  expect_equal(row$annotation, "ADSL.WEIGHT")
  expect_equal(row$detection_method, "comment")
  expect_equal(row$detection_confidence, "high")
})

test_that("a highlighted run (no font colour) is detected like a coloured one", {
  secs <- parse_shell_docx(test_path("fixtures/annotated_shell_comments_highlights.docx"))
  row <- Filter(function(r) r$label == "Height (cm)", secs[[1]]$stub_rows)[[1]]
  expect_true(row$has_annot)
  expect_equal(row$annotation, "ADSL.HEIGHT")
  expect_equal(row$detection_method, "colour")
})

test_that("deleted (tracked-change) text is excluded from the parsed cell text", {
  diag_reset()
  secs <- parse_shell_docx(test_path("fixtures/annotated_shell_comments_highlights.docx"))
  row <- Filter(function(r) r$label == "Age (years)", secs[[1]]$stub_rows)[[1]]
  expect_equal(row$annotation, "ADSL.AGE")
  expect_false(grepl("OLDVAR", row$raw_text))

  diags <- diag_records()
  expect_true(any(grepl("tracked changes", diags$problem)))
})

test_that("text box content does not leak into the surrounding stub label", {
  diag_reset()
  secs <- parse_shell_docx(test_path("fixtures/annotated_shell_comments_highlights.docx"))
  row <- Filter(function(r) r$label == "Notes", secs[[1]]$stub_rows)[[1]]
  expect_false(grepl("IGNORE ME", row$raw_text))
  expect_false(row$has_annot)

  diags <- diag_records()
  expect_true(any(grepl("text box", diags$problem)))
})

## --- F2: titles/populations living in the page header ----------------------

test_that("TLF number, title, and population are sourced from the page header when the body has none", {
  diag_reset()
  secs <- parse_shell_docx(test_path("fixtures/annotated_shell_page_header_title.docx"))
  expect_length(secs, 1)
  sec <- secs[[1]]
  expect_equal(sec$tlf_number, "T-14-5-1")
  expect_equal(sec$tlf_type, "TABLE")
  expect_equal(sec$title, "Bone Mineral Density Change from Baseline")
  expect_equal(sec$population_annot, "ADSL.SAFFL='Y'")
  # The body's own Source line is still read normally.
  expect_equal(sec$source_datasets, "ADSL")
  # The body table's own annotations still parse normally too.
  row <- Filter(function(r) grepl("^BMD", r$label), sec$stub_rows)[[1]]
  expect_equal(row$annotation, "ADSL.WEIGHT")

  diags <- diag_records()
  expect_true(any(grepl("page header", diags$problem)))
})

test_that("a shell with no page-header content is unaffected by the header reader", {
  secs <- parse_shell_docx(test_path("fixtures/annotated_shell_2tlf_minimal.docx"))
  expect_length(secs, 2)
  expect_true(all(nzchar(vapply(secs, function(s) s$title, character(1)))))
})

## --- Re-evaluation regression tests (REEVALUATION_p0-parsing_premerge.md) ---

## §2.1 -- a page header naming a DIFFERENT TLF must not have its title or
## population silently adopted by the body section.
test_that("a page-header heading with a mismatched TLF number is not adopted", {
  diag_reset()
  secs <- parse_shell_docx(test_path("fixtures/annotated_shell_header_mismatch.docx"))
  expect_length(secs, 1)
  sec <- secs[[1]]
  expect_equal(sec$tlf_number, "T-1-1")
  # The stale "Table 9.9.9" header title/population must NOT be attached.
  expect_equal(sec$title, "")
  expect_false(grepl("Stale Template", sec$title))
  expect_equal(sec$population_text, "")

  diags <- diag_records()
  mism <- diags[diags$severity == "WARN" & grepl("numbers differ", diags$problem), ]
  expect_gt(nrow(mism), 0)
})

## §2.2 -- a two-row nested header with no <w:tblHeader/> flag must be
## inferred: no ghost stub row, subcolumn labels preserved, and a WARN.
test_that("an unflagged two-row nested header is inferred, not read as a data row", {
  diag_reset()
  secs <- parse_shell_docx(test_path("fixtures/annotated_shell_unflagged_headers.docx"))
  sec <- secs[[1]]
  expect_equal(sec$col_headers,
              c("Category", "Treatment A n", "Treatment A (%)",
                "Placebo n", "Placebo (%)"))
  expect_equal(sec$n_data_cols, 4L)
  # Only the real data row survives -- the second header row is not a ghost.
  labels <- vapply(sec$stub_rows, function(r) r$label, character(1))
  expect_equal(labels, "Any AE")

  diags <- diag_records()
  inferred <- diags[diags$severity == "WARN" &
                      grepl("no row is flagged", diags$problem), ]
  expect_gt(nrow(inferred), 0)
})

## §3a -- a treatment-column mapping arrow line right after the title is not
## the population; it must become the column-axis grouping.
test_that("an arrow line after the title binds as column annotation, not population", {
  secs <- parse_shell_docx(test_path("fixtures/annotated_shell_arrow_after_title.docx"))
  sec <- secs[[1]]
  expect_equal(sec$title, "Summary of Exposure")
  expect_equal(sec$population_text, "")
  expect_equal(sec$population_annot, "")
  expect_equal(sec$column_annotation, "ADSL.TRT01A")
})

## Probe 3 -- a pre-table "Note:" footnote must not be glued onto the title.
test_that("a pre-table footnote is not swallowed into the title", {
  secs <- parse_shell_docx(test_path("fixtures/annotated_shell_pretable_footnote.docx"))
  sec <- secs[[1]]
  expect_equal(sec$title, "Summary of Vital Signs")
  expect_true(any(grepl("^Note:", sec$footnotes)))
})

## Probe 5 -- a comment anchored to a DATA cell (not the stub) still binds.
test_that("a comment anchored to a data cell supplies the row annotation", {
  secs <- parse_shell_docx(test_path("fixtures/annotated_shell_datacell_comment.docx"))
  row <- Filter(function(r) r$label == "Weight (kg)", secs[[1]]$stub_rows)[[1]]
  expect_true(row$has_annot)
  expect_equal(row$annotation, "ADSL.WEIGHT")
  expect_equal(row$detection_method, "data_cell_comment")
})
