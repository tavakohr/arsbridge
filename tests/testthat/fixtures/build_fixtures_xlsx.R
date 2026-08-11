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

## ---------------------------------------------------------------------------
## Fixture 3 -- the full study shell, shells_apx_drm_301.xlsx
## ---------------------------------------------------------------------------
##
## Nine sheets in the shape a real annotated shell workbook has: five tables,
## two listings, one figure, and the workbook's own legend. Invented
## APX-DRM-301 content throughout -- an atopic dermatitis study with three
## arms. What is reproduced from the exemplar is the CONVENTION, never the
## text: banner rows 1-4, red italic bracket annotations in-cell, red
## parentheses on the population line, unbracketed red arrow prose on the
## figure sheet, spacer rows between groups, merged titles and footnotes.
##
## Deliberate deviations, each exercising a tolerance the parser claims:
##   Table 14.1.3  no population line, and its number row says 14.1.3 while
##                 the tab is named Table 14.1.4 -- the mismatch report.
##   Table 14.2.1  a two-row merged column header (cohort over sub-columns),
##                 which the flat exemplar never exercises.
##   Figure 14.3.1 one arrow line naming an aspect arsbridge does not know.

arm <- c("Placebo", "Drug 10 mg", "Drug 20 mg")

wb3 <- wb_workbook()

sheet_banner <- function(wb, sheet, number, title, population = NULL,
                         n_cols = 4L) {
  wb$add_data(sheet = sheet, x = number, start_row = 1, start_col = 1,
              col_names = FALSE)
  wb$add_data(sheet = sheet, x = title, start_row = 2, start_col = 1,
              col_names = FALSE)
  wb$merge_cells(sheet = sheet, dims = paste0("A1:", int2col(n_cols), "1"))
  wb$merge_cells(sheet = sheet, dims = paste0("A2:", int2col(n_cols), "2"))
  if (!is.null(population)) {
    wb$add_data(
      sheet = sheet,
      x = fmt_txt(paste0(population, " "), color = wb_color(hex = BLACK),
                  size = 10) +
        fmt_txt("(ADSL.SAFFL='Y')", color = wb_color(hex = RED), size = 8,
                italic = TRUE),
      start_row = 3, start_col = 1, col_names = FALSE)
    wb$merge_cells(sheet = sheet, dims = paste0("A3:", int2col(n_cols), "3"))
  }
  invisible(wb)
}

sheet_footnote <- function(wb, sheet, row, text, n_cols = 4L) {
  wb$add_data(sheet = sheet, x = text, start_row = row, start_col = 1,
              col_names = FALSE)
  wb$merge_cells(sheet = sheet,
                 dims = sprintf("A%d:%s%d", row, int2col(n_cols), row))
  invisible(wb)
}

## Header row: stub header carrying the output-level directives, then one
## plain cell per treatment column.
sheet_header <- function(wb, sheet, row, stub, directives, arms = arm) {
  wb$add_data(sheet = sheet, x = annotated_cell(stub, directives),
              start_row = row, start_col = 1, col_names = FALSE)
  for (j in seq_along(arms)) {
    wb$add_data(sheet = sheet, x = arms[[j]], start_row = row,
                start_col = j + 1L, col_names = FALSE)
  }
  invisible(wb)
}

data_row <- function(wb, sheet, row, label, annot = NULL,
                     value = "xx (xx.x)", arms = arm) {
  x <- if (is.null(annot)) label else annotated_cell(label, annot)
  wb$add_data(sheet = sheet, x = x, start_row = row, start_col = 1,
              col_names = FALSE)
  if (!is.null(value)) {
    for (j in seq_along(arms)) {
      wb$add_data(sheet = sheet, x = value, start_row = row,
                  start_col = j + 1L, col_names = FALSE)
    }
  }
  invisible(wb)
}

## --- Table 14.1.1: disposition, the plain case -----------------------------
wb3$add_worksheet("Table 14.1.1")
sheet_banner(wb3, "Table 14.1.1", "Table 14.1.1",
             "Summary of Subject Disposition", "Safety Population")
sheet_header(wb3, "Table 14.1.1", 4, "Category",
             "[columns -> ADSL.TRT01A; source ADSL]")
data_row(wb3, "Table 14.1.1", 5, "Subjects treated", "[ADSL.SAFFL = 'Y']",
         value = "xx")
data_row(wb3, "Table 14.1.1", 6, "Completed study",
         "[ADSL.EOSSTT = 'COMPLETED']")
data_row(wb3, "Table 14.1.1", 7, "Discontinued study",
         "[ADSL.EOSSTT = 'DISCONTINUED']")
sheet_footnote(wb3, "Table 14.1.1", 8,
               "Percentages are based on the number of treated subjects.")

## --- Table 14.1.2: demographics, group parents and spacer rows -------------
wb3$add_worksheet("Table 14.1.2")
sheet_banner(wb3, "Table 14.1.2", "Table 14.1.2",
             "Demographics and Baseline Characteristics", "Safety Population")
sheet_header(wb3, "Table 14.1.2", 4, "Characteristic",
             "[columns -> ADSL.TRT01A; source ADSL]")
data_row(wb3, "Table 14.1.2", 5, "Age (years)", "[ADSL.AGE]", value = NULL)
data_row(wb3, "Table 14.1.2", 6, "Mean (SD)", value = "xx.x (xx.xx)")
data_row(wb3, "Table 14.1.2", 7, "Median", value = "xx.x")
data_row(wb3, "Table 14.1.2", 8, "Q1, Q3", value = "xx, xx")
## row 9 blank -- spacer
data_row(wb3, "Table 14.1.2", 10, "Sex, n (%)", "[ADSL.SEX]", value = NULL)
data_row(wb3, "Table 14.1.2", 11, "Female")
data_row(wb3, "Table 14.1.2", 12, "Male")

## --- Table 14.1.3: no population line, and a number/tab mismatch -----------
## The TAB is named 14.1.4 while row 1 says 14.1.3: the sheet name wins and
## the disagreement is reported.
wb3$add_worksheet("Table 14.1.4")
sheet_banner(wb3, "Table 14.1.4", "Table 14.1.3",
             "Protocol Deviations", population = NULL)
sheet_header(wb3, "Table 14.1.4", 3, "Deviation category",
             "[columns -> ADSL.TRT01A; source ADDV]")
data_row(wb3, "Table 14.1.4", 4, "Any major deviation",
         "[ADDV.DVCAT = 'MAJOR']")
data_row(wb3, "Table 14.1.4", 5, "Inclusion criteria",
         "[ADDV.DVDECOD = 'INCLUSION']")

## --- Table 14.2.1: a two-row merged column header --------------------------
## Cohort spanning two statistic sub-columns each -- the nested header shape
## the exemplar never exercises, so the column tree has a fixture.
wb3$add_worksheet("Table 14.2.1")
sheet_banner(wb3, "Table 14.2.1", "Table 14.2.1", "Study Drug Exposure",
             "Safety Population", n_cols = 5L)
wb3$add_data(sheet = "Table 14.2.1",
             x = annotated_cell("Statistic",
                                "[columns -> ADEX.TRT01A; source ADEX]"),
             start_row = 4, start_col = 1, col_names = FALSE)
wb3$add_data(sheet = "Table 14.2.1",
             x = annotated_cell("Drug 10 mg", "[ADEX.TRT01AN = 1]"),
             start_row = 4, start_col = 2, col_names = FALSE)
wb3$add_data(sheet = "Table 14.2.1",
             x = annotated_cell("Drug 20 mg", "[ADEX.TRT01AN = 2]"),
             start_row = 4, start_col = 4, col_names = FALSE)
wb3$merge_cells(sheet = "Table 14.2.1", dims = "B4:C4")
wb3$merge_cells(sheet = "Table 14.2.1", dims = "D4:E4")
## The sub-columns carry their OWN conditions -- without them the header is a
## display split, not a grouping hierarchy, and no column tree can be built.
sub_visits <- c("[ADEX.AVISITN = 12]", "[ADEX.AVISITN = 24]")
for (j in 2:5) {
  k <- ((j - 2L) %% 2L) + 1L
  wb3$add_data(sheet = "Table 14.2.1",
               x = annotated_cell(c("Week 12", "Week 24")[[k]], sub_visits[[k]]),
               start_row = 5, start_col = j, col_names = FALSE)
}
## Row 5 column 1 is left empty: the stub header spans both header rows.
data_row(wb3, "Table 14.2.1", 6, "Duration (days)", "[ADEX.TRTDURD]",
         value = NULL)
data_row(wb3, "Table 14.2.1", 7, "Mean (SD)", value = "xx.x (xx.xx)",
         arms = 1:4)
data_row(wb3, "Table 14.2.1", 8, "Median", value = "xx.x", arms = 1:4)

## --- Table 14.3.1: adverse events, template rows ---------------------------
wb3$add_worksheet("Table 14.3.1")
sheet_banner(wb3, "Table 14.3.1", "Table 14.3.1",
             "Treatment-Emergent Adverse Events by System Organ Class",
             "Safety Population")
sheet_header(wb3, "Table 14.3.1", 4, "System Organ Class / Preferred Term",
             "[columns -> ADAE.TRT01A; source ADAE]")
data_row(wb3, "Table 14.3.1", 5, "Subjects with any TEAE",
         "[row incl. ADAE.TRTEMFL='Y'; once/subject]")
## row 6 blank -- spacer
data_row(wb3, "Table 14.3.1", 7, "<System Organ Class>", "[ADAE.AESOC]")
data_row(wb3, "Table 14.3.1", 8, "<Preferred Term>", "[ADAE.AEDECOD]")
sheet_footnote(wb3, "Table 14.3.1", 10,
               "A subject is counted once per system organ class.")

## --- Listing 16.2.1: the row filter lives in a second bracket group --------
wb3$add_worksheet("Listing 16.2.1")
sheet_banner(wb3, "Listing 16.2.1", "Listing 16.2.1",
             "Listing of Adverse Events", "Safety Population", n_cols = 5L)
listing_header <- function(wb, sheet, cols) {
  for (j in seq_along(cols)) {
    wb$add_data(sheet = sheet,
                x = annotated_cell(names(cols)[[j]], cols[[j]]),
                start_row = 4, start_col = j, col_names = FALSE)
  }
}
listing_header(wb3, "Listing 16.2.1", c(
  "Subject"   = "[ADAE.USUBJID]  [row: ADAE.TRTEMFL='Y'; source ADAE]",
  "Treatment" = "[ADAE.TRT01A]",
  "AE term"   = "[ADAE.AEDECOD]",
  "Severity"  = "[ADAE.ASEV]",
  "Outcome"   = "[ADAE.AEOUT]"))
for (j in 1:5) {
  wb3$add_data(sheet = "Listing 16.2.1",
               x = c("xxx-xxx", "xxxx", "xxxx", "x", "xxxx")[[j]],
               start_row = 5, start_col = j, col_names = FALSE)
}

## --- Listing 16.2.2: no output-level directives at all ---------------------
wb3$add_worksheet("Listing 16.2.2")
sheet_banner(wb3, "Listing 16.2.2", "Listing 16.2.2",
             "Listing of Concomitant Medications", "Safety Population",
             n_cols = 3L)
for (j in 1:3) {
  wb3$add_data(sheet = "Listing 16.2.2",
               x = annotated_cell(c("Subject", "Reported term", "Start/Stop")[[j]],
                                  c("[ADCM.USUBJID]", "[ADCM.CMTRT]",
                                    "[ADCM.ASTDT / AENDT]")[[j]]),
               start_row = 4, start_col = j, col_names = FALSE)
  wb3$add_data(sheet = "Listing 16.2.2",
               x = c("xxx-xxx", "xxxx", "dd-mmm / dd-mmm")[[j]],
               start_row = 5, start_col = j, col_names = FALSE)
}

## --- Figure 14.3.1: whole-cell red arrow prose, one unknown aspect ---------
wb3$add_worksheet("Figure 14.3.1")
sheet_banner(wb3, "Figure 14.3.1", "Figure 14.3.1",
             "Mean (+/- SE) Pulse Rate Over Time by Treatment",
             "Safety Population", n_cols = 2L)
wb3$add_data(sheet = "Figure 14.3.1", x = "Chart type", start_row = 5,
             start_col = 1, col_names = FALSE)
wb3$add_data(sheet = "Figure 14.3.1", x = "Line plot with error bars",
             start_row = 5, start_col = 2, col_names = FALSE)
fig_lines <- c("Programming annotations",
               "Source: ADVS.",
               "X axis -> ADVS.AVISITN (label ADVS.AVISIT)",
               "Y axis -> mean of ADVS.AVAL",
               "Series (colour) -> ADVS.TRTA",
               "Filter -> ADVS.PARAMCD='PULSE'",
               "Error bars -> SE = sd(ADVS.AVAL) / sqrt(n)",
               "Reference line -> 0")
for (i in seq_along(fig_lines)) {
  row <- 6L + i
  wb3$add_data(sheet = "Figure 14.3.1", x = fig_lines[[i]], start_row = row,
               start_col = 1, col_names = FALSE)
  wb3$merge_cells(sheet = "Figure 14.3.1", dims = sprintf("A%d:B%d", row, row))
}
wb3$add_font(sheet = "Figure 14.3.1", dims = "A7:A14",
             color = wb_color(hex = RED), size = 9, italic = TRUE)

## --- Formatting Notes: the workbook's own legend ---------------------------
wb3$add_worksheet("Formatting Notes")
wb3$add_data(sheet = "Formatting Notes", x = "Formatting Notes",
             start_row = 1, start_col = 1, col_names = FALSE)
notes <- c(
  "Workbook structure" = "One worksheet per output, named for the output.",
  "Annotations"        = "Red italic text in square brackets is a programming annotation.",
  "Placeholders"       = "xx marks a count; xx.x a one-decimal value.")
for (i in seq_along(notes)) {
  wb3$add_data(sheet = "Formatting Notes", x = names(notes)[[i]],
               start_row = 2L + i, start_col = 1, col_names = FALSE)
  wb3$add_data(sheet = "Formatting Notes", x = notes[[i]],
               start_row = 2L + i, start_col = 2, col_names = FALSE)
}

full_path <- file.path(here, "shells_apx_drm_301.xlsx")
wb3$save(full_path)
cat("wrote", full_path, "\n")

## ---------------------------------------------------------------------------
## Fixture 4 -- a second header dialect, shells_apx_nheaders.xlsx
## ---------------------------------------------------------------------------
##
## The conventions that broke the first field workbook, recreated with the
## same invented APX-DRM-301 content. Two sheets so the two fixes regress
## independently by test name:
##
##   Table 14.1.5  the combined case: the stub header cell holds ONLY the red
##                 column directive (no visible label -- its column label is
##                 blank once the annotation is stripped), the arm headers
##                 carry an "(N=XX)" decoration, the placeholders are
##                 UPPERCASE, and one stub label ends in a "[a]" footnote
##                 marker.
##   Table 14.1.6  the decoration alone: a labelled stub, "(N=XX)" arm
##                 headers, an Age block with statistic sub-rows.

wb4 <- wb_workbook()
arm_n <- paste0(arm, " (N=XX)")

## A header cell that is nothing but the directive -- red italic, no label.
directive_only <- function(text) {
  fmt_txt(text, color = wb_color(hex = RED), size = 8, italic = TRUE)
}

## --- Table 14.1.5: blank stub header + (N=XX) + uppercase + [a] ------------
wb4$add_worksheet("Table 14.1.5")
sheet_banner(wb4, "Table 14.1.5", "Table 14.1.5",
             "Summary of Study Completion", "Safety Population")
wb4$add_data(sheet = "Table 14.1.5",
             x = directive_only("[columns -> ADSL.TRT01A; source ADSL]"),
             start_row = 4, start_col = 1, col_names = FALSE)
for (j in seq_along(arm_n)) {
  wb4$add_data(sheet = "Table 14.1.5", x = arm_n[[j]], start_row = 4,
               start_col = j + 1L, col_names = FALSE)
}
data_row(wb4, "Table 14.1.5", 5, "Subjects treated", "[ADSL.SAFFL = 'Y']",
         value = "XX")
data_row(wb4, "Table 14.1.5", 6, "Completed study",
         "[ADSL.EOSSTT = 'COMPLETED']", value = "XX (XX.X)")
data_row(wb4, "Table 14.1.5", 7, "Discontinued study [a]",
         "[ADSL.EOSSTT = 'DISCONTINUED']", value = "XX (XX.X)")
sheet_footnote(wb4, "Table 14.1.5", 9, "[a] End-of-study status.")

## --- Table 14.1.6: (N=XX) headers over an Age block ------------------------
wb4$add_worksheet("Table 14.1.6")
sheet_banner(wb4, "Table 14.1.6", "Table 14.1.6",
             "Summary of Age", "Safety Population")
sheet_header(wb4, "Table 14.1.6", 4, "Characteristic",
             "[columns -> ADSL.TRT01A; source ADSL]", arms = arm_n)
data_row(wb4, "Table 14.1.6", 5, "Age (years)", "[ADSL.AGE]", value = NULL)
data_row(wb4, "Table 14.1.6", 6, "Mean (SD)", value = "XX.X (XX.XX)")
data_row(wb4, "Table 14.1.6", 7, "Median", value = "XX.X")

path4 <- file.path(here, "shells_apx_nheaders.xlsx")
wb4$save(path4)
cat("wrote", path4, "\n")

## ---------------------------------------------------------------------------
## Fixture 5 -- the acceptance shapes, shells_apx_acceptance.xlsx
## ---------------------------------------------------------------------------
##
## The two shell shapes the field failures of 2026-08 arrived as, in one
## workbook so a single parse produces both and their axes have to coexist.
##
## Sheet 1 groups TRT01A the ordinary way: three plain arm headers, so the
## axis is data-driven and mints GF_TRT01A.
##
## Sheet 2 is a SUBGROUP table. Every column carries a compound condition, and
## the FIRST variable each one references is TRT01A again -- which is how the
## real failure happened: the axis is named after the variable it references
## first, so this table's Male/Female columns minted GF_TRT01A too, collided
## with sheet 1's definition, and were silently dropped. The ARD then carried
## sheet 1's arm levels, no subgroup header matched them, and every subgroup
## column stayed a placeholder while Total filled.
##
## Both sheets read ADSL only, so tests run them against the existing
## adam_apx_drm_301 fixture data with no new datasets.

wb5 <- wb_workbook()

## --- Table 14.1.1: the plain treatment axis --------------------------------
wb5$add_worksheet("Table 14.1.1")
sheet_banner(wb5, "Table 14.1.1", "Table 14.1.1",
             "Summary of Subject Disposition", "Safety Population")
sheet_header(wb5, "Table 14.1.1", 4, "Category",
             "[columns -> ADSL.TRT01A; source ADSL]")
data_row(wb5, "Table 14.1.1", 5, "Subjects treated", "[ADSL.SAFFL = 'Y']",
         value = "xx")
data_row(wb5, "Table 14.1.1", 6, "Discontinued study",
         "[ADSL.EOSSTT = 'DISCONTINUED']")

## --- Table 14.4.1: the subgroup axis, compound conditions ------------------
subgroups <- list(
  list(label = "Drug 10 mg, Male",
       annot = "[ADSL.TRT01A = 'Drug 10 mg' AND ADSL.SEX = 'M']"),
  list(label = "Drug 10 mg, Female",
       annot = "[ADSL.TRT01A = 'Drug 10 mg' AND ADSL.SEX = 'F']"),
  list(label = "Drug 20 mg, Male",
       annot = "[ADSL.TRT01A = 'Drug 20 mg' AND ADSL.SEX = 'M']")
)

wb5$add_worksheet("Table 14.4.1")
sheet_banner(wb5, "Table 14.4.1", "Table 14.4.1",
             "Summary of Subject Disposition by Subgroup", "Safety Population")
wb5$add_data(sheet = "Table 14.4.1",
             x = annotated_cell("Category",
                                "[columns -> ADSL.TRT01A; source ADSL]"),
             start_row = 4, start_col = 1, col_names = FALSE)
for (j in seq_along(subgroups)) {
  wb5$add_data(
    sheet = "Table 14.4.1",
    x = annotated_cell(subgroups[[j]]$label, subgroups[[j]]$annot),
    start_row = 4, start_col = j + 1L, col_names = FALSE)
}
data_row(wb5, "Table 14.4.1", 5, "Subjects treated", "[ADSL.SAFFL = 'Y']",
         value = "xx", arms = subgroups)
data_row(wb5, "Table 14.4.1", 6, "Discontinued study",
         "[ADSL.EOSSTT = 'DISCONTINUED']", arms = subgroups)

path5 <- file.path(here, "shells_apx_acceptance.xlsx")
wb5$save(path5)
cat("wrote", path5, "\n")
