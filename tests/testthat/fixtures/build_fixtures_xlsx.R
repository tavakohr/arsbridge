## tests/testthat/fixtures/build_fixtures_xlsx.R
## ---------------------------------------------------------------------------
## Re-generates the Excel SHELL fixtures for the xlsx cell layer. Run from the
## package root:
##
##   Rscript tests/testthat/fixtures/build_fixtures_xlsx.R
##
## (The ADaM spec .xlsx fixtures and the .docx shells are built by
## build_fixtures.R -- this file only builds annotated Excel SHELLS.)
##
## All content is invented APX-DRM-301 material. No sponsor file is ever a
## fixture, and no sponsor wording is copied into one: what is reproduced here
## is the FORMAT (where the runs are, which are red, what is merged), never
## anyone's text.
##
## Two fixtures, because a shell workbook reaches arsbridge through two
## mutually exclusive string conventions and the cell layer has to be
## indifferent to which one it gets:
##
##   shell_cells_inline_apx.xlsx   openxlsx2-written. Strings are INLINE
##     (`<c t="inlineStr"><is>`), boolean run properties are the bare element
##     (`<i/>`), and the sheet relationship Target is workbook-relative. This
##     is also what openpyxl produces, i.e. how an authored shell arrives.
##
##   shell_cells_shared_apx.xlsx   hand-built SpreadsheetML. Strings are
##     INTERNED in xl/sharedStrings.xml (`<c t="s"><v>3</v>`), boolean run
##     properties carry the redundant `val="1"`, and the Target is
##     package-absolute. This is what EXCEL writes -- so it is the shape the
##     shell comes back in the moment a reviewer opens it and saves, which
##     the review workflow expects them to do. openxlsx2 cannot generate a
##     shared-string workbook at all (it writes everything inline), so this
##     one is assembled part by part.
##
## Between them, every branch of the cell layer's format tolerance is
## exercised: both string homes, both boolean spellings, both Target forms,
## rich runs and whole-cell fonts, merges, sparse rows, and a numeric cell.

suppressPackageStartupMessages({
  library(openxlsx2)
})

here <- if (dir.exists("tests/testthat/fixtures")) {
  "tests/testthat/fixtures"
} else if (dir.exists("fixtures")) {
  "fixtures"
} else {
  "."
}

BLACK <- "FF000000"
RED   <- "FFC00000"   ## the annotation colour, as authored shells state it

## ---------------------------------------------------------------------------
## Fixture 1 -- inline strings, openxlsx2 (how an authored shell arrives)
## ---------------------------------------------------------------------------

## A stub cell as these shells author it: the display label in black, then a
## newline, then the annotation in small red italics, all in ONE cell.
annotated_cell <- function(label, annotation) {
  fmt_txt(label, color = wb_color(hex = BLACK), size = 10) +
    fmt_txt(paste0("\n", annotation), color = wb_color(hex = RED),
            size = 8, italic = TRUE)
}

put <- function(wb, x, row, col) {
  wb$add_data(x = x, start_row = row, start_col = col, col_names = FALSE)
}

wb <- wb_workbook()

## --- Sheet 1: a table, the ordinary case -----------------------------------
wb$add_worksheet("Table 14.1.1")

put(wb, "Table 14.1.1", 1, 1)
put(wb, "Summary of Subject Disposition", 2, 1)
## Population line: the annotation is in red PARENTHESES, not brackets.
put(wb, fmt_txt("Safety Population ", color = wb_color(hex = BLACK), size = 10) +
      fmt_txt("(ADSL.SAFFL='Y')", color = wb_color(hex = RED), size = 8,
              italic = TRUE), 3, 1)

## Header row: the stub header carries the column-axis annotation.
put(wb, annotated_cell("Category", "[columns -> ADSL.TRT01A]"), 4, 1)
put(wb, "Placebo",        4, 2)
put(wb, "Drug 10 mg",     4, 3)
put(wb, "Drug 20 mg",     4, 4)

## Body rows, with the two placeholder shapes the fill writer will read.
put(wb, annotated_cell("Subjects treated", "[ADSL.SAFFL = 'Y']"), 5, 1)
for (j in 2:4) put(wb, "xx", 5, j)

put(wb, annotated_cell("Completed study", "[ADSL.EOSSTT = 'COMPLETED']"), 6, 1)
for (j in 2:4) put(wb, "xx (xx.x)", 6, j)

## Row 7 is deliberately left EMPTY -- a spacer row. The file will contain no
## <row r="7">, so the reader has to tolerate a gap in the row sequence.

put(wb, "Percentages are based on the number of treated subjects.", 8, 1)

## A numeric cell, so `kind` has something other than "string" to report.
put(wb, 42, 9, 2)

wb$merge_cells(dims = "A1:D1")
wb$merge_cells(dims = "A2:D2")
wb$merge_cells(dims = "A3:D3")
wb$merge_cells(dims = "A8:D8")

## --- Sheet 2: a figure, annotated by WHOLE-CELL colour ---------------------
## No rich runs here: the entire cell is red. The reader must present such a
## cell as a single styled run so the detection layers see one interface.
wb$add_worksheet("Figure 14.3.1")
put(wb, "Figure 14.3.1", 1, 1)
put(wb, "Mean Pulse Rate Over Time by Treatment", 2, 1)
put(wb, "X axis -> ADVS.AVISITN (label ADVS.AVISIT)", 4, 1)
put(wb, "Y axis -> mean of ADVS.AVAL", 5, 1)
put(wb, "Series -> ADVS.TRTA", 6, 1)
wb$add_font(dims = "A4:A6", color = wb_color(hex = RED), size = 9,
            italic = TRUE)

inline_path <- file.path(here, "shell_cells_inline_apx.xlsx")
wb$save(inline_path)
cat("wrote", inline_path, "\n")

## ---------------------------------------------------------------------------
## Fixture 2 -- shared strings, hand-built (how Excel writes it back)
## ---------------------------------------------------------------------------

## Interned strings. si 0 and 2 are plain; si 1 is a rich pair (black label +
## red annotation) exactly as a shared-string workbook carries an annotated
## stub; note the `val="1"` spelling of the italic flag.
shared_strings <- paste0(
  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  '<sst xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"',
  ' count="4" uniqueCount="4">',
  '<si><t>Table 14.2.1</t></si>',
  '<si>',
  '<r><rPr><rFont val="Arial"/><color rgb="', BLACK, '"/><sz val="10"/></rPr>',
  '<t>Any adverse event</t></r>',
  '<r><rPr><rFont val="Arial"/><i val="1"/><color rgb="', RED, '"/>',
  '<sz val="8"/></rPr>',
  '<t xml:space="preserve">\n[ADAE.TRTEMFL = \'Y\']</t></r>',
  '</si>',
  '<si><t>xx</t></si>',
  '<si><t>Series -&gt; ADAE.TRTA</t></si>',
  '</sst>')

## Two fonts: black body text, and the red annotation font used as a
## WHOLE-CELL font on A4 (style index 2 -> font 1).
styles <- paste0(
  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  '<styleSheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">',
  '<fonts count="2">',
  '<font><name val="Arial"/><color rgb="', BLACK, '"/><sz val="10"/></font>',
  '<font><name val="Arial"/><i val="1"/><color rgb="', RED, '"/>',
  '<sz val="9"/></font>',
  '</fonts>',
  '<cellXfs count="3">',
  '<xf numFmtId="0" fontId="0" xfId="0"/>',
  '<xf numFmtId="0" fontId="0" applyFont="1" xfId="0"/>',
  '<xf numFmtId="0" fontId="1" applyFont="1" xfId="0"/>',
  '</cellXfs>',
  '</styleSheet>')

## Row 3 is absent (spacer). C2 is numeric. A4 is whole-cell red.
sheet1 <- paste0(
  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  '<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">',
  '<dimension ref="A1:C4"/>',
  '<sheetData>',
  '<row r="1"><c r="A1" s="1" t="s"><v>0</v></c></row>',
  '<row r="2">',
  '<c r="A2" s="1" t="s"><v>1</v></c>',
  '<c r="B2" s="1" t="s"><v>2</v></c>',
  '<c r="C2" s="1"><v>17</v></c>',
  '</row>',
  '<row r="4"><c r="A4" s="2" t="s"><v>3</v></c></row>',
  '</sheetData>',
  '<mergeCells count="1"><mergeCell ref="A1:C1"/></mergeCells>',
  '</worksheet>')

workbook <- paste0(
  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  '<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"',
  ' xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">',
  '<sheets><sheet name="Table 14.2.1" sheetId="1" r:id="rId1"/></sheets>',
  '</workbook>')

## Package-ABSOLUTE Target, the form openpyxl and some Excel builds write.
workbook_rels <- paste0(
  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
  '<Relationship Id="rId1" Target="/xl/worksheets/sheet1.xml"',
  ' Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet"/>',
  '<Relationship Id="rId2" Target="/xl/styles.xml"',
  ' Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles"/>',
  '<Relationship Id="rId3" Target="/xl/sharedStrings.xml"',
  ' Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/sharedStrings"/>',
  '</Relationships>')

root_rels <- paste0(
  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
  '<Relationship Id="rId1" Target="xl/workbook.xml"',
  ' Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument"/>',
  '</Relationships>')

content_types <- paste0(
  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
  '<Default Extension="rels"',
  ' ContentType="application/vnd.openxmlformats-package.relationships+xml"/>',
  '<Default Extension="xml" ContentType="application/xml"/>',
  '<Override PartName="/xl/workbook.xml"',
  ' ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>',
  '<Override PartName="/xl/worksheets/sheet1.xml"',
  ' ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>',
  '<Override PartName="/xl/styles.xml"',
  ' ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"/>',
  '<Override PartName="/xl/sharedStrings.xml"',
  ' ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sharedStrings+xml"/>',
  '</Types>')

parts <- list(
  "[Content_Types].xml"          = content_types,
  "_rels/.rels"                  = root_rels,
  "xl/workbook.xml"              = workbook,
  "xl/_rels/workbook.xml.rels"   = workbook_rels,
  "xl/styles.xml"                = styles,
  "xl/sharedStrings.xml"         = shared_strings,
  "xl/worksheets/sheet1.xml"     = sheet1
)

## Zip the parts up as an .xlsx. The archive is built from INSIDE the staging
## directory, over "." rather than a file list, so member names stay relative
## and the "[Content_Types].xml" member -- whose brackets a shell would read
## as a glob pattern -- never has to survive being passed as an argument.
staging <- file.path(tempdir(), "xlsx_shared_fixture")
unlink(staging, recursive = TRUE)
for (part in names(parts)) {
  target <- file.path(staging, part)
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  writeLines(parts[[part]], target, useBytes = TRUE)
}

shared_path <- file.path(normalizePath(here), "shell_cells_shared_apx.xlsx")
unlink(shared_path)
old <- setwd(staging)
on.exit(setwd(old), add = TRUE)
utils::zip(shared_path, files = ".", flags = "-q -r -X")
setwd(old)
cat("wrote", shared_path, "\n")
