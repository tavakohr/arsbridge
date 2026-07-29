## The SpreadsheetML cell layer: does it produce seam 1 of parse_shell_core.R
## faithfully, and is it indifferent to which of the two legal string
## conventions the workbook uses?
##
## Fixtures (built by fixtures/build_fixtures_xlsx.R, invented APX-DRM-301
## content):
##   shell_cells_inline_apx.xlsx  inline strings, bare boolean flags,
##                                relative rel Target  (openxlsx2 / openpyxl)
##   shell_cells_shared_apx.xlsx  shared strings, val="1" flags,
##                                absolute rel Target  (Excel)

inline_wb <- function() {
  xlsx_read_shell_cells(test_path("fixtures", "shell_cells_inline_apx.xlsx"))
}
shared_wb <- function() {
  xlsx_read_shell_cells(test_path("fixtures", "shell_cells_shared_apx.xlsx"))
}

## The cell of `sheet` at an A1 reference, as a one-row data frame.
cell_at <- function(sheet, ref) {
  hit <- sheet$cells[sheet$cells$ref == ref, , drop = FALSE]
  expect_equal(nrow(hit), 1L, info = paste("expected exactly one cell at", ref))
  hit
}

rpr <- function(xml) xml2::xml_root(xml2::read_xml(xml))

## ---------------------------------------------------------------------------
## A1 references
## ---------------------------------------------------------------------------

test_that("column letters and numbers convert both ways", {
  expect_equal(.xlsx_col_to_num("A"), 1L)
  expect_equal(.xlsx_col_to_num("Z"), 26L)
  expect_equal(.xlsx_col_to_num("AA"), 27L)
  expect_equal(.xlsx_col_to_num("AB"), 28L)
  expect_equal(.xlsx_col_to_num("BA"), 53L)

  ## Round-trip: the fill writer addresses cells by reference, so the inverse
  ## has to be exact past the single-letter range.
  for (n in c(1L, 26L, 27L, 52L, 53L, 702L, 703L)) {
    expect_equal(.xlsx_col_to_num(.xlsx_num_to_col(n)), n)
  }
})

test_that("an A1 reference parses to row and column, absolute form included", {
  expect_equal(.xlsx_a1_to_rc("A1"), c(row = 1L, col = 1L))
  expect_equal(.xlsx_a1_to_rc("D8"), c(row = 8L, col = 4L))
  expect_equal(.xlsx_a1_to_rc("AA12"), c(row = 12L, col = 27L))
  expect_equal(.xlsx_a1_to_rc("$B$4"), c(row = 4L, col = 2L))
  expect_equal(.xlsx_rc_to_a1(12, 27), "AA12")
})

test_that("a reference that is not a plain cell yields NA rather than a guess", {
  expect_true(all(is.na(.xlsx_a1_to_rc("A1:D8"))))
  expect_true(all(is.na(.xlsx_a1_to_rc(""))))
})

## ---------------------------------------------------------------------------
## Font properties
## ---------------------------------------------------------------------------

test_that("an 8-digit ARGB colour loses its alpha byte to match seam 1", {
  ## Seam 1 states colours as 6 hex digits so that the .GREY_HEX /
  ## .BLACK_HEXES comparisons in parse_shell_core.R need no format branch.
  expect_equal(.xlsx_color_hex(rpr('<rPr><color rgb="FFC00000"/></rPr>')),
               "C00000")
  expect_equal(.xlsx_color_hex(rpr('<rPr><color rgb="FF000000"/></rPr>')),
               "000000")
  expect_equal(.xlsx_color_hex(rpr('<rPr><color rgb="c00000"/></rPr>')),
               "C00000")
})

test_that("a theme or indexed colour is NA, not a resolved guess", {
  ## Theme resolution is out of scope; reporting NA means "no colour stated",
  ## which can never invent an annotation.
  expect_true(is.na(.xlsx_color_hex(rpr('<rPr><color theme="1"/></rPr>'))))
  expect_true(is.na(.xlsx_color_hex(rpr('<rPr><color indexed="8"/></rPr>'))))
  expect_true(is.na(.xlsx_color_hex(rpr("<rPr/>"))))
  expect_true(is.na(.xlsx_color_hex(NULL)))
})

test_that("both spellings of a boolean run property are read the same", {
  ## Excel writes the bare element; openpyxl writes the redundant val="1".
  expect_true(.xlsx_flag(rpr("<rPr><i/></rPr>"), "i"))
  expect_true(.xlsx_flag(rpr('<rPr><i val="1"/></rPr>'), "i"))
  expect_true(.xlsx_flag(rpr('<rPr><i val="true"/></rPr>'), "i"))

  ## ... and an explicit opt-out is honoured, so a run that turns italics OFF
  ## is not read as emphasised.
  expect_false(.xlsx_flag(rpr('<rPr><i val="0"/></rPr>'), "i"))
  expect_false(.xlsx_flag(rpr('<rPr><i val="false"/></rPr>'), "i"))
  expect_false(.xlsx_flag(rpr("<rPr/>"), "i"))
  expect_false(.xlsx_flag(NULL, "i"))
})

## ---------------------------------------------------------------------------
## Workbook and sheet structure
## ---------------------------------------------------------------------------

test_that("sheets come back in tab order, named", {
  wb <- inline_wb()
  expect_equal(names(wb$sheets), c("Table 14.1.1", "Figure 14.3.1"))
  expect_equal(vapply(wb$sheets, function(s) s$order, integer(1),
                      USE.NAMES = FALSE), 1:2)
})

test_that("a sheet reports the extent of its populated cells", {
  s <- inline_wb()$sheets[["Table 14.1.1"]]
  expect_equal(s$n_rows, 9L)   ## the numeric cell on row 9
  expect_equal(s$n_cols, 4L)
})

test_that("merged ranges parse to row and column bounds", {
  s <- inline_wb()$sheets[["Table 14.1.1"]]
  expect_equal(nrow(s$merges), 4L)
  expect_equal(s$merges$ref, c("A1:D1", "A2:D2", "A3:D3", "A8:D8"))
  expect_equal(unique(s$merges$col_start), 1L)
  expect_equal(unique(s$merges$col_end), 4L)
  expect_equal(s$merges$row_start, s$merges$row_end)

  ## A sheet that merges nothing gets zero rows, not NULL.
  fig <- inline_wb()$sheets[["Figure 14.3.1"]]
  expect_equal(nrow(fig$merges), 0L)
})

test_that("an empty spacer row is absent rather than invented", {
  ## Row 7 of the fixture is a blank spacer. Fabricating a row for it would
  ## misreport the geometry the layout classifier reads.
  s <- inline_wb()$sheets[["Table 14.1.1"]]
  expect_false(7L %in% s$cells$row)
  expect_true(all(c(6L, 8L) %in% s$cells$row))
})

test_that("cells arrive in row-major order", {
  s <- inline_wb()$sheets[["Table 14.1.1"]]
  expect_false(is.unsorted(s$cells$row))
  first_row <- s$cells[s$cells$row == 4L, , drop = FALSE]
  expect_equal(first_row$col, 1:4)
})

test_that("a numeric cell is reported as a number, not a string", {
  s <- inline_wb()$sheets[["Table 14.1.1"]]
  expect_equal(cell_at(s, "B9")$kind, "number")
  expect_equal(cell_at(s, "B9")$text, "42")
  expect_equal(cell_at(s, "B5")$kind, "string")
})

## ---------------------------------------------------------------------------
## Seam 1: the run list
## ---------------------------------------------------------------------------

test_that("an annotated stub cell comes back as two runs, one of them styled", {
  s <- inline_wb()$sheets[["Table 14.1.1"]]
  runs <- cell_at(s, "A5")$runs[[1]]
  expect_length(runs, 2L)

  expect_equal(runs[[1]]$text, "Subjects treated")
  expect_equal(runs[[1]]$color_hex, "000000")
  expect_false(.is_annotation_styled_run(runs[[1]]))

  expect_equal(trimws(runs[[2]]$text), "[ADSL.SAFFL = 'Y']")
  expect_equal(runs[[2]]$color_hex, "C00000")
  expect_true(runs[[2]]$italic)
  expect_true(.is_annotation_styled_run(runs[[2]]))
})

test_that("every run carries the full seam-1 field set", {
  runs <- cell_at(inline_wb()$sheets[["Table 14.1.1"]], "A5")$runs[[1]]
  expect_named(runs[[1]],
               c("text", "raw_text", "color_hex", "highlight", "bold",
                 "italic", "underline", "strike"))
  ## Highlight has no SpreadsheetML equivalent, so it is always absent -- the
  ## highlight half of .is_annotation_styled_run() simply never fires.
  expect_true(all(vapply(runs, function(r) is.na(r$highlight), logical(1))))
})

test_that("run text is normalized while raw_text keeps what was authored", {
  ## .normalize_shell_text() straightens the smart quotes an author's Word or
  ## Excel autocorrect introduces; raw_text is what the run-integrity lint
  ## needs to see.
  runs <- cell_at(inline_wb()$sheets[["Table 14.1.1"]], "A5")$runs[[1]]
  expect_equal(runs[[1]]$text, .normalize_shell_text(runs[[1]]$raw_text))
})

test_that("cell text is the runs joined bare, preserving the newline", {
  ## The runs of a cell are one string broken by formatting, so they join with
  ## NOTHING; the line break between label and annotation is already inside
  ## the annotation run's own text.
  cell <- cell_at(inline_wb()$sheets[["Table 14.1.1"]], "A6")
  expect_equal(cell$text, "Completed study\n[ADSL.EOSSTT = 'COMPLETED']")
})

test_that("a whole-cell colour is presented as one styled run", {
  ## The figure sheet annotates by colouring the ENTIRE cell rather than a run
  ## inside it. Synthesizing a single run carrying the cell font is what lets
  ## the detection layers see one interface for both conventions.
  fig <- inline_wb()$sheets[["Figure 14.3.1"]]
  cell <- cell_at(fig, "A4")
  expect_equal(cell$cell_color, "C00000")
  runs <- cell$runs[[1]]
  expect_length(runs, 1L)
  expect_equal(runs[[1]]$color_hex, "C00000")
  expect_true(.is_annotation_styled_run(runs[[1]]))
  expect_equal(runs[[1]]$text, "X axis -> ADVS.AVISITN (label ADVS.AVISIT)")
})

test_that("an unannotated cell yields one run that is not styled", {
  fig <- inline_wb()$sheets[["Figure 14.3.1"]]
  runs <- cell_at(fig, "A2")$runs[[1]]
  expect_length(runs, 1L)
  expect_false(.is_annotation_styled_run(runs[[1]]))
})

## ---------------------------------------------------------------------------
## Both string conventions
## ---------------------------------------------------------------------------

test_that("interned strings read exactly like inline ones", {
  ## The same shell saved by Excel interns its strings in sharedStrings.xml
  ## and spells its boolean flags val="1"; none of that may reach seam 1.
  s <- shared_wb()$sheets[["Table 14.2.1"]]

  plain <- cell_at(s, "A1")
  expect_equal(plain$text, "Table 14.2.1")
  expect_length(plain$runs[[1]], 1L)

  rich <- cell_at(s, "A2")$runs[[1]]
  expect_length(rich, 2L)
  expect_equal(rich[[1]]$text, "Any adverse event")
  expect_false(.is_annotation_styled_run(rich[[1]]))
  expect_equal(rich[[2]]$color_hex, "C00000")
  expect_true(rich[[2]]$italic)
  expect_true(.is_annotation_styled_run(rich[[2]]))
})

test_that("a package-absolute relationship Target resolves to its sheet", {
  ## openpyxl writes Target="/xl/worksheets/sheet1.xml"; Excel and openxlsx2
  ## write it workbook-relative. Both have to land on the same part.
  wb <- shared_wb()
  expect_equal(wb$sheets[[1]]$part, "xl/worksheets/sheet1.xml")
  expect_equal(nrow(wb$sheets[[1]]$cells), 5L)
})

test_that("a whole-cell font from the style table is read in either convention", {
  s <- shared_wb()$sheets[["Table 14.2.1"]]
  runs <- cell_at(s, "A4")$runs[[1]]
  expect_length(runs, 1L)
  expect_true(.is_annotation_styled_run(runs[[1]]))
  expect_equal(runs[[1]]$text, "Series -> ADAE.TRTA")
})

## ---------------------------------------------------------------------------
## The point of all of it: the shared core reads these cells unchanged
## ---------------------------------------------------------------------------

test_that("core annotation detection works on xlsx cells with no format branch", {
  ## The whole reason the cell layer produces seam 1: .detect_annotation()
  ## from parse_shell_core.R is the docx detector, unmodified.
  s <- inline_wb()$sheets[["Table 14.1.1"]]
  cell <- cell_at(s, "A6")

  det <- .detect_annotation(cell$text, cell$runs[[1]])
  expect_equal(det$label, "Completed study")
  expect_equal(det$annotation, "ADSL.EOSSTT = 'COMPLETED'")
  expect_equal(det$method, "colour")
  expect_equal(det$confidence, "high")
})

test_that("core detection reads an interned annotated cell the same way", {
  s <- shared_wb()$sheets[["Table 14.2.1"]]
  cell <- cell_at(s, "A2")

  det <- .detect_annotation(cell$text, cell$runs[[1]])
  expect_equal(det$label, "Any adverse event")
  expect_equal(det$annotation, "ADAE.TRTEMFL = 'Y'")
  expect_equal(det$method, "colour")
})

test_that("an unannotated cell yields no annotation", {
  s <- inline_wb()$sheets[["Table 14.1.1"]]
  cell <- cell_at(s, "A8")
  det <- .detect_annotation(cell$text, cell$runs[[1]])
  expect_equal(det$annotation, "")
  expect_true(is.na(det$method))
})

## ---------------------------------------------------------------------------
## Failure modes
## ---------------------------------------------------------------------------

test_that("a missing workbook aborts with the path", {
  expect_error(xlsx_read_shell_cells("no_such_shell.xlsx"), "not found")
})

test_that("a zip that is not a workbook says so instead of reading nothing", {
  ## A readable archive with no xl/workbook.xml is a wrong file, not an empty
  ## shell -- returning zero sheets would send it silently down the pipeline.
  dir <- withr::local_tempdir()
  writeLines("not a workbook", file.path(dir, "notes.txt"))
  bogus <- file.path(withr::local_tempdir(), "bogus.xlsx")
  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)
  utils::zip(bogus, files = "notes.txt", flags = "-q")
  setwd(old)

  expect_error(xlsx_read_shell_cells(bogus), "workbook.xml is missing")
})
