## Sheet-layout semantics: which sheet is an output, which rows are its
## banner, what each body row is, which cells are placeholders, and what a
## figure sheet's arrow prose declares.
##
## Most tests build their sheet with helper-gridgen.R so the geometry is
## exact and the combinations can be swept. The tests at the bottom run
## against the real committed fixture, which is what keeps the generator
## honest.

## ---------------------------------------------------------------------------
## Sheet classification
## ---------------------------------------------------------------------------

test_that("a sheet named for its output is classified from the name", {
  expect_equal(.classify_sheet(gridgen_tlf("Table 14.1.1"))$tlf_number,
               "T-14-1-1")
  expect_equal(.classify_sheet(gridgen_tlf("Table 14.1.1"))$tlf_type, "TABLE")
  expect_equal(.classify_sheet(gridgen_tlf("Listing 16.2.7.1"))$tlf_number,
               "L-16-2-7-1")
  expect_equal(.classify_sheet(gridgen_tlf("Listing 16.2.7.1"))$tlf_type,
               "LISTING")
  expect_equal(.classify_sheet(gridgen_figure("Figure 14.3.1"))$tlf_type,
               "FIGURE")

  cls <- .classify_sheet(gridgen_tlf("Table 14.1.1"))
  expect_equal(cls$role, "tlf")
  expect_equal(cls$source, "sheet_name")
})

test_that("the sheet name beats row 1, because a row can be inserted above", {
  ## The name is the one part of the convention an edit cannot push down.
  sheet <- gridgen_tlf("Table 14.1.1")
  sheet$cells$text[sheet$cells$ref == "A1"] <- "Table 99.9.9"
  expect_equal(.classify_sheet(sheet)$tlf_number, "T-14-1-1")
})

test_that("a sheet whose tab was renamed is still found from row 1", {
  sheet <- gridgen_tlf("Table 14.1.1")
  sheet$name <- "Sheet1"
  cls <- .classify_sheet(sheet)
  expect_equal(cls$role, "tlf")
  expect_equal(cls$tlf_number, "T-14-1-1")
  expect_equal(cls$source, "row1")
})

test_that("a documentation sheet is recognised, not treated as a failed output", {
  ## These workbooks ship a legend sheet. It is a legitimate part of the
  ## workbook, so it is skipped quietly rather than warned about.
  for (nm in c("Formatting Notes", "Notes", "Legend", "Read me", "README",
               "Instructions", "Contents", "Table of Contents", "Index",
               "Key", "Cover", "Conventions")) {
    sheet <- gridgen_sheet(nm, list(gridgen_cell(1, 1, nm)))
    expect_equal(.classify_sheet(sheet)$role, "notes", info = nm)
  }
})

test_that("a sheet that is neither an output nor documentation is unknown", {
  sheet <- gridgen_sheet("Scratch", list(gridgen_cell(1, 1, "working notes")))
  cls <- .classify_sheet(sheet)
  expect_equal(cls$role, "unknown")
  expect_true(is.na(cls$tlf_number))
})

test_that("a custom heading pattern reaches sheet classification", {
  sheet <- gridgen_sheet("TFL 3.1.2", list(gridgen_cell(1, 1, "x")))
  expect_equal(.classify_sheet(sheet)$role, "unknown")
  cls <- .classify_sheet(
    sheet, heading_patterns = "^TFL\\s+(?<number>\\d+(?:\\.\\d+)*)$")
  expect_equal(cls$role, "tlf")
  expect_equal(cls$tlf_number, "T-3-1-2")
})

## ---------------------------------------------------------------------------
## Banner location
## ---------------------------------------------------------------------------

test_that("the conventional rows 1-4 banner is located", {
  sheet <- gridgen_tlf()
  L <- .locate_banner_rows(sheet, "TABLE", "T-14-1-1")
  expect_equal(L$number_row, 1L)
  expect_equal(L$title_row, 2L)
  expect_equal(L$population_row, 3L)
  expect_equal(L$header_row, 4L)
  expect_equal(L$first_body_row, 5L)
})

test_that("the banner is still found when rows were inserted above it", {
  ## A shell with a logo row pasted on top must not stop parsing.
  for (offset in 0:2) {
    L <- .locate_banner_rows(gridgen_tlf(offset = offset), "TABLE")
    expect_equal(L$number_row, offset + 1L, info = paste("offset", offset))
    expect_equal(L$header_row, offset + 4L, info = paste("offset", offset))
  }
})

test_that("a shell with no population line still finds its header and body", {
  L <- .locate_banner_rows(gridgen_tlf(population = FALSE), "TABLE")
  expect_true(is.na(L$population_row))
  expect_equal(L$number_row, 1L)
  expect_equal(L$title_row, 2L)
  expect_equal(L$header_row, 3L)
  expect_equal(L$first_body_row, 4L)
})

test_that("a figure is not given a header row it does not have", {
  ## A figure's key/value spec block fills two columns and would otherwise be
  ## mistaken for a column header.
  L <- .locate_banner_rows(gridgen_figure(), "FIGURE", "F-14-3-1")
  expect_true(is.na(L$header_row))
  expect_equal(L$number_row, 1L)
  expect_equal(L$population_row, 3L)
})

test_that("a missing header row on a table is reported", {
  diag_reset()
  sheet <- gridgen_sheet("Table 14.9.9", list(
    gridgen_cell(1, 1, "Table 14.9.9"),
    gridgen_cell(2, 1, "A title"),
    gridgen_cell(3, 1, "Safety Population")))
  L <- .locate_banner_rows(sheet, "TABLE", "T-14-9-9")
  expect_true(is.na(L$header_row))
  d <- diag_records()
  expect_true(any(grepl("No column-header row", d$problem)))
  expect_equal(d$severity[grepl("No column-header row", d$problem)], "WARN")
})

test_that("a sheet with no output-number row reports the deviation and continues", {
  diag_reset()
  sheet <- gridgen_tlf()
  sheet$cells <- sheet$cells[sheet$cells$row != 1L, , drop = FALSE]
  L <- .locate_banner_rows(sheet, "TABLE", "T-14-1-1")
  expect_true(is.na(L$number_row))
  expect_false(is.na(L$header_row))   ## the body is still readable
  expect_true(any(grepl("no output-number row", diag_records()$problem)))
})

test_that("the banner survives every combination of its optional parts", {
  ## The toggle sweep: whatever is present must be found, and the first body
  ## row must always land on the first data row.
  for (population in c(TRUE, FALSE)) {
    for (footnote in c(TRUE, FALSE)) {
      for (offset in c(0L, 2L)) {
        sheet <- gridgen_tlf(offset = offset, population = population,
                             footnote = footnote)
        L <- .locate_banner_rows(sheet, "TABLE")
        label <- sprintf("pop=%s foot=%s offset=%d", population, footnote,
                         offset)
        expect_equal(L$number_row, offset + 1L, info = label)
        expect_equal(L$title_row, offset + 2L, info = label)
        expect_equal(is.na(L$population_row), !population, info = label)
        expect_equal(L$header_row, offset + 3L + as.integer(population),
                     info = label)
        expect_equal(L$first_body_row, L$header_row + 1L, info = label)
      }
    }
  }
})

## ---------------------------------------------------------------------------
## Header records (seam 2)
## ---------------------------------------------------------------------------

test_that("header cells become seam-2 records with the annotation split off", {
  sheet <- gridgen_tlf()
  grid <- .grid_header_records(sheet, 4L)
  expect_length(grid, 4L)
  expect_named(grid[[1]], c("row", "col_start", "col_end", "text",
                            "annotation", "vmerge_continue"))

  ## The stub header carries the column-axis annotation; its display text is
  ## the label alone.
  expect_equal(grid[[1]]$text, "Category")
  expect_equal(grid[[1]]$annotation, "columns -> ADSL.TRT01A")
  expect_equal(grid[[2]]$text, "Arm 1")
  expect_equal(grid[[2]]$annotation, "")
})

test_that("header records number rows within the block, not the sheet", {
  ## .header_grid_to_tree() reads row 1 as the outermost header level, so a
  ## header on sheet row 4 must still be record row 1.
  grid <- .grid_header_records(gridgen_tlf(), 4L)
  expect_true(all(vapply(grid, function(g) g$row, integer(1)) == 1L))
})

test_that("a merged header cell spans the columns it covers", {
  ## Forward compatibility with a grouped header: the span is what lets
  ## column_tree.R see a parent over its sub-columns.
  sheet <- gridgen_sheet("Table 14.5.1", list(
    gridgen_cell(4, 1, "Category"),
    gridgen_cell(4, 2, "Cohort A", annot = "[ADSL.COHORTN=1]"),
    gridgen_cell(4, 4, "Cohort B", annot = "[ADSL.COHORTN=2]")),
    merges = c("B4:C4", "D4:E4"))
  grid <- .grid_header_records(sheet, 4L)
  expect_equal(grid[[1]]$col_start, 1L)
  expect_equal(grid[[1]]$col_end, 1L)
  expect_equal(grid[[2]]$col_start, 2L)
  expect_equal(grid[[2]]$col_end, 3L)
  expect_equal(grid[[3]]$col_start, 4L)
  expect_equal(grid[[3]]$col_end, 5L)
})

test_that("the records feed column_tree.R unchanged", {
  ## The whole reason for matching the docx record shape.
  sheet <- gridgen_sheet("Table 14.5.1", list(
    gridgen_cell(4, 1, "Category"),
    gridgen_cell(4, 2, "Cohort A", annot = "[ADSL.COHORTN=1]"),
    gridgen_cell(4, 3, "Cohort B", annot = "[ADSL.COHORTN=2]")))
  tree <- .header_grid_to_tree(.grid_header_records(sheet, 4L))
  expect_length(tree$nodes, 2L)
  expect_equal(vapply(tree$nodes, function(n) n$label, character(1)),
               c("Cohort A", "Cohort B"))
})

test_that("a vertically merged Excel header has no continuation ghost", {
  ## Excel writes a vertically merged cell once, at its anchor -- unlike Word,
  ## which leaves a vMerge continuation cell behind.
  grid <- .grid_header_records(gridgen_tlf(), 4L)
  expect_false(any(vapply(grid, function(g) g$vmerge_continue, logical(1))))
})

## ---------------------------------------------------------------------------
## Body rows
## ---------------------------------------------------------------------------

test_that("body rows are classified by what they carry", {
  sheet <- gridgen_tlf(groups = TRUE, spacers = TRUE)
  L <- .locate_banner_rows(sheet, "TABLE")
  b <- .collect_body_rows(sheet, L)

  expect_equal(b$kind[b$row == 5L], "group")     ## parent row, no results
  expect_equal(b$kind[b$row == 6L], "data")
  expect_equal(b$kind[b$row == 8L], "data")      ## row 7 is the spacer
  expect_false(7L %in% b$row)
  expect_equal(b$kind[b$row == 9L], "footnote")
})

test_that("a group row and a footnote row are told apart by the merge", {
  ## Both fill column 1 alone. Only the footnote spans the sheet width.
  sheet <- gridgen_tlf(groups = TRUE)
  L <- .locate_banner_rows(sheet, "TABLE")
  b <- .collect_body_rows(sheet, L)
  expect_equal(sum(b$kind == "group"), 1L)
  expect_equal(sum(b$kind == "footnote"), 1L)
})

test_that("a red full-width row is an instruction, not a footnote", {
  ## How a figure sheet states its whole specification.
  sheet <- gridgen_figure()
  L <- .locate_banner_rows(sheet, "FIGURE")
  b <- .collect_body_rows(sheet, L)
  expect_true(all(b$kind[b$row >= 8L] == "annotation"))
  expect_true(all(b$has_annotation[b$row >= 8L]))
})

test_that("a template stub is flagged as one", {
  sheet <- gridgen_sheet("Table 14.3.2", list(
    gridgen_cell(5, 1, "<System Organ Class>", annot = "[ADAE.AESOC]"),
    gridgen_cell(5, 2, "xx (xx.x)"),
    gridgen_cell(6, 1, "Subjects with any TEAE"),
    gridgen_cell(6, 2, "xx (xx.x)")))
  b <- .collect_body_rows(sheet, list(first_body_row = 5L))
  expect_true(b$is_template[b$row == 5L])
  expect_false(b$is_template[b$row == 6L])
})

## ---------------------------------------------------------------------------
## Placeholder lexicon
## ---------------------------------------------------------------------------

test_that("the placeholder itself specifies the decimal places", {
  ## This is why a filled shell needs no separate format declaration.
  expect_equal(.parse_placeholder("xx")$slots[[1]]$decimals, 0L)
  expect_equal(.parse_placeholder("xx.x")$slots[[1]]$decimals, 1L)
  expect_equal(.parse_placeholder("xx.xxx")$slots[[1]]$decimals, 3L)
  expect_equal(.parse_placeholder("xx")$slots[[1]]$width, 2L)
})

test_that("a compound placeholder yields one slot per statistic, in order", {
  p <- .parse_placeholder("xx (xx.x)")
  expect_equal(p$kind, "placeholder")
  expect_equal(p$n_slots, 2L)
  expect_equal(vapply(p$slots, function(s) s$token, character(1)),
               c("xx", "xx.x"))
  expect_equal(vapply(p$slots, function(s) s$decimals, integer(1)), c(0L, 1L))

  ## The spans are what let the fill writer substitute in place and keep the
  ## author's punctuation.
  expect_equal(p$slots[[1]]$start, 1L)
  expect_equal(p$slots[[2]]$start, 5L)

  expect_equal(.parse_placeholder("xx.x (xx.xx)")$n_slots, 2L)
  expect_equal(
    vapply(.parse_placeholder("xx.x (xx.xx)")$slots,
           function(s) s$decimals, integer(1)), c(1L, 2L))
  expect_equal(.parse_placeholder("xx, xx")$n_slots, 2L)
})

test_that("date and template placeholders are recognised as their own kinds", {
  p <- .parse_placeholder("dd-mmm / dd-mmm")
  expect_equal(p$kind, "placeholder")
  expect_equal(p$n_slots, 2L)
  expect_true(all(vapply(p$slots, function(s) s$type, character(1)) == "date"))

  t <- .parse_placeholder("<Preferred Term>")
  expect_equal(t$kind, "template")
})

test_that("a label that merely contains an x is not a placeholder", {
  ## The regression this rule exists for: "Max" and "Min, Max" sit in the same
  ## stub column as real placeholders, and overwriting a label with a number
  ## would silently corrupt the output.
  for (label in c("Max", "Min, Max", "Q1, Q3", "Mean (SD)", "Median",
                  "18-64", ">64", "Exposure", "n (%)", "Sex, n (%)",
                  "NONE", "Any TEAE", "Xanomeline Low")) {
    expect_equal(.parse_placeholder(label)$kind, "literal", info = label)
  }
})

test_that("an empty cell is empty rather than literal", {
  expect_equal(.parse_placeholder("")$kind, "empty")
  expect_equal(.parse_placeholder("   ")$kind, "empty")
  expect_equal(.parse_placeholder(NULL)$kind, "empty")
})

test_that("a numeric cell is a value already, not a placeholder for one", {
  sheet <- gridgen_sheet("Table 14.1.1", list(
    gridgen_cell(5, 1, "Subjects"),
    gridgen_cell(5, 2, "xx"),
    gridgen_cell(5, 3, "42", kind = "number")))
  g <- .cell_placeholder_grid(sheet, list(first_body_row = 5L))
  expect_equal(g$kind[g$ref == "B5"], "placeholder")
  expect_equal(g$kind[g$ref == "C5"], "literal")
})

test_that("the placeholder grid covers the body and skips the banner", {
  sheet <- gridgen_tlf()
  L <- .locate_banner_rows(sheet, "TABLE")
  g <- .cell_placeholder_grid(sheet, L)
  expect_true(all(g$row >= L$first_body_row))
  expect_equal(sum(g$kind == "placeholder"), 6L)   ## 2 rows x 3 result cols
})

## ---------------------------------------------------------------------------
## Figure directives
## ---------------------------------------------------------------------------

test_that("an arrow directive splits into aspect and value", {
  d <- .parse_arrow_directive("X axis -> ADVS.AVISITN (label ADVS.AVISIT)")
  expect_equal(d$key, "x_axis")
  expect_equal(d$value, "ADVS.AVISITN (label ADVS.AVISIT)")
  expect_equal(d$label, "X axis")
})

test_that("aspect spelling does not matter", {
  expect_equal(.parse_arrow_directive("Series (colour) -> ADVS.TRTA")$key,
               "series")
  expect_equal(.parse_arrow_directive("series color -> X")$key, "series")
  expect_equal(.parse_arrow_directive("Y-axis -> X")$key, "y_axis")
  expect_equal(.parse_arrow_directive("Error bars -> X")$key, "error_bars")
  expect_equal(.parse_arrow_directive("Filter -> X")$key, "filter")
  expect_equal(.parse_arrow_directive("Chart type -> X")$key, "chart_type")
  ## The unicode arrow a word processor substitutes.
  expect_equal(.parse_arrow_directive("X axis → ADVS.AVISITN")$key,
               "x_axis")
})

test_that("an unknown aspect is kept with no key rather than dropped", {
  d <- .parse_arrow_directive("Reference line -> 0")
  expect_true(is.na(d$key))
  expect_equal(d$value, "0")
})

test_that("a line with no arrow is not a directive", {
  expect_null(.parse_arrow_directive("Programming annotations"))
  expect_null(.parse_arrow_directive(""))
})

test_that("a figure block yields its directives, source and anchor", {
  diag_reset()
  sheet <- gridgen_figure()
  L <- .locate_banner_rows(sheet, "FIGURE", "F-14-3-1")
  f <- .parse_figure_block(sheet, L, "F-14-3-1")

  expect_equal(sort(names(f$directives)), c("series", "x_axis", "y_axis"))
  expect_equal(f$directives$x_axis$value, "ADVS.AVISITN")
  expect_equal(f$source_datasets, "ADVS")
  ## The anchor is where the computed series data will be written.
  expect_equal(f$anchor_row, 8L)
  expect_length(f$unmatched, 0L)
})

test_that("only the red lines of a figure sheet are read as instructions", {
  ## The fixture's black spec block includes "Chart type -> stacked bar" --
  ## the exact arrow form of a real directive. Colour is the only thing that
  ## says it is display material, so reading it would mean the parser is
  ## taking the human-facing caption as programming instructions.
  sheet <- gridgen_figure()
  L <- .locate_banner_rows(sheet, "FIGURE")
  f <- .parse_figure_block(sheet, L, "F-14-3-1")

  expect_false("chart_type" %in% names(f$directives))
  expect_false(any(grepl("stacked bar", f$unmatched)))
})

test_that("an unreadable figure line is reported and kept, never dropped", {
  diag_reset()
  sheet <- gridgen_figure(directives = c("X axis -> ADVS.AVISITN",
                                         "Draw a smoothed trend line"))
  L <- .locate_banner_rows(sheet, "FIGURE", "F-14-3-1")
  f <- .parse_figure_block(sheet, L, "F-14-3-1")
  expect_equal(f$unmatched, "Draw a smoothed trend line")
  expect_true(any(grepl("not recognised as a directive",
                        diag_records()$problem)))
})

test_that("a repeated aspect keeps the first and warns", {
  diag_reset()
  sheet <- gridgen_figure(directives = c("X axis -> ADVS.AVISITN",
                                         "X axis -> ADVS.ADY"))
  L <- .locate_banner_rows(sheet, "FIGURE", "F-14-3-1")
  f <- .parse_figure_block(sheet, L, "F-14-3-1")
  expect_equal(f$directives$x_axis$value, "ADVS.AVISITN")
  d <- diag_records()
  expect_true(any(grepl("more than once", d$problem)))
  expect_equal(d$severity[grepl("more than once", d$problem)], "WARN")
})

## ---------------------------------------------------------------------------
## TLF identity (shared with the docx reader)
## ---------------------------------------------------------------------------

test_that("a heading match names its output the same way for any reader", {
  id <- .tlf_identity(.match_tlf_heading("Table 14.1.1")$hit)
  expect_equal(id$tlf_number, "T-14-1-1")
  expect_equal(id$tlf_type, "TABLE")

  expect_equal(.tlf_identity(.match_tlf_heading("Figure 3")$hit)$tlf_number,
               "F-3")
  expect_equal(
    .tlf_identity(.match_tlf_heading("Listing 16.2.7.1")$hit)$tlf_type,
    "LISTING")
  ## An unrecognised designator defaults to a table rather than failing.
  expect_equal(.tlf_identity(list(type_word = "Panel", number = "1"))$tlf_type,
               "TABLE")
})

## ---------------------------------------------------------------------------
## Against the real reader
## ---------------------------------------------------------------------------

test_that("the generated sheet shape matches what the reader produces", {
  ## Without this, every test above could be passing against a fiction.
  real <- xlsx_read_shell_cells(
    test_path("fixtures", "shell_cells_inline_apx.xlsx"))$sheets[[1]]
  fake <- gridgen_tlf()

  expect_equal(sort(names(fake)), sort(names(real)))
  expect_equal(sort(names(fake$cells)), sort(names(real$cells)))
  expect_equal(sort(names(fake$merges)), sort(names(real$merges)))
  expect_equal(sort(names(fake$cells$runs[[1]][[1]])),
               sort(names(real$cells$runs[[1]][[1]])))
})

test_that("the layout of a real workbook sheet reads end to end", {
  wb <- xlsx_read_shell_cells(
    test_path("fixtures", "shell_cells_inline_apx.xlsx"))

  tbl <- wb$sheets[["Table 14.1.1"]]
  cls <- .classify_sheet(tbl)
  expect_equal(cls$tlf_number, "T-14-1-1")

  L <- .locate_banner_rows(tbl, cls$tlf_type, cls$tlf_number)
  expect_equal(c(L$number_row, L$title_row, L$population_row, L$header_row),
               c(1L, 2L, 3L, 4L))

  grid <- .grid_header_records(tbl, L$header_row)
  expect_equal(grid[[1]]$annotation, "columns -> ADSL.TRT01A")

  b <- .collect_body_rows(tbl, L)
  expect_equal(b$kind[b$row == 5L], "data")
  expect_equal(b$kind[b$row == 8L], "footnote")

  g <- .cell_placeholder_grid(tbl, L)
  expect_equal(g$kind[g$ref == "B6"], "placeholder")
  expect_equal(g$n_slots[g$ref == "B6"], 2L)   ## "xx (xx.x)"
})

test_that("a real figure sheet's whole-cell red block parses", {
  wb <- xlsx_read_shell_cells(
    test_path("fixtures", "shell_cells_inline_apx.xlsx"))
  fig <- wb$sheets[["Figure 14.3.1"]]
  cls <- .classify_sheet(fig)
  expect_equal(cls$tlf_type, "FIGURE")

  L <- .locate_banner_rows(fig, "FIGURE", cls$tlf_number)
  f <- .parse_figure_block(fig, L, cls$tlf_number)
  expect_equal(f$directives$x_axis$value, "ADVS.AVISITN (label ADVS.AVISIT)")
  expect_equal(f$directives$y_axis$value, "mean of ADVS.AVAL")
  expect_equal(f$directives$series$value, "ADVS.TRTA")
})
