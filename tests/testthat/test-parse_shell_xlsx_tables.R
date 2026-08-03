## parse_shell_xlsx() on table sheets: does a worksheet become a section
## object the rest of the package already knows how to consume?
##
## Fixture: shells_apx_drm_301.xlsx (invented APX-DRM-301 content, nine
## sheets, built by fixtures/build_fixtures_xlsx.R).

shell_xlsx <- function() test_path("fixtures", "shells_apx_drm_301.xlsx")

parsed <- function(...) {
  suppressMessages(suppressWarnings(parse_shell_xlsx(shell_xlsx(), ...)))
}

section_of <- function(sections, number) {
  hit <- Filter(function(s) identical(s$tlf_number, number), sections)
  expect_length(hit, 1L)
  hit[[1]]
}

## ---------------------------------------------------------------------------
## The workbook as a whole
## ---------------------------------------------------------------------------

test_that("every output sheet becomes a section and the legend does not", {
  secs <- parsed()
  expect_length(secs, 8L)   ## 9 sheets, minus "Formatting Notes"
  expect_false("Formatting Notes" %in%
                 vapply(secs, function(s) s$sheet_name, character(1)))
})

test_that("sections come back in workbook order", {
  expect_equal(
    vapply(parsed(), function(s) s$tlf_number, character(1)),
    c("T-14-1-1", "T-14-1-2", "T-14-1-4", "T-14-2-1", "T-14-3-1",
      "L-16-2-1", "L-16-2-2", "F-14-3-1"))
})

test_that("each output is typed from its number", {
  types <- vapply(parsed(), function(s) s$tlf_type, character(1))
  expect_equal(sum(types == "TABLE"), 5L)
  expect_equal(sum(types == "LISTING"), 2L)
  expect_equal(sum(types == "FIGURE"), 1L)
})

test_that("a section carries every field .new_section() defines", {
  ## The contract the rest of the package consumes -- a missing field would
  ## surface as a NULL somewhere far downstream.
  sec <- section_of(parsed(), "T-14-1-1")
  expect_true(all(names(.new_section("T-1", "TABLE")) %in% names(sec)))
})

test_that("the workbook is recorded on the section, additively", {
  ## Class 3: the fill writer needs to find its way back to the sheet.
  sec <- section_of(parsed(), "T-14-1-1")
  expect_equal(sec$source_format, "xlsx")
  expect_equal(sec$sheet_name, "Table 14.1.1")
})

## ---------------------------------------------------------------------------
## Banner
## ---------------------------------------------------------------------------

test_that("title and population come off the banner", {
  sec <- section_of(parsed(), "T-14-1-1")
  expect_equal(sec$title, "Summary of Subject Disposition")
  expect_match(sec$population_text, "^Safety Population")
  expect_match(sec$population_annot, "ADSL\\.SAFFL='Y'")
})

test_that("a shell with no population line parses without one", {
  sec <- section_of(parsed(), "T-14-1-4")
  expect_equal(sec$population_text, "")
  expect_equal(sec$population_annot, "")
  expect_equal(sec$title, "Protocol Deviations")
  expect_length(sec$stub_rows, 2L)   ## the body is still read
})

test_that("a tab name that disagrees with row 1 is reported, and the tab wins", {
  ## The tab name is the one part of the convention an inserted row cannot
  ## displace, so it is authoritative -- but silently preferring it would hide
  ## a real authoring mistake.
  diag_reset()
  secs <- suppressMessages(suppressWarnings(parse_shell_xlsx(shell_xlsx())))
  expect_true("T-14-1-4" %in% vapply(secs, function(s) s$tlf_number,
                                     character(1)))
  d <- diag_records()
  hit <- grepl("the sheet name was used as the output number", d$problem)
  expect_true(any(hit))
  expect_equal(unique(d$severity[hit]), "WARN")
})

## ---------------------------------------------------------------------------
## Stub rows
## ---------------------------------------------------------------------------

test_that("annotated stub rows split into label and annotation", {
  sec <- section_of(parsed(), "T-14-1-1")
  expect_length(sec$stub_rows, 3L)
  expect_equal(vapply(sec$stub_rows, function(r) r$label, character(1)),
               c("Subjects treated", "Completed study", "Discontinued study"))
  expect_equal(vapply(sec$stub_rows, function(r) r$annotation, character(1)),
               c("ADSL.SAFFL = 'Y'", "ADSL.EOSSTT = 'COMPLETED'",
                 "ADSL.EOSSTT = 'DISCONTINUED'"))
  expect_true(all(vapply(sec$stub_rows, function(r) r$has_annot, logical(1))))
})

test_that("the detection method names the Excel convention that supplied it", {
  ## Class 2: a reviewer reading a finding needs to know whether a run's
  ## colour or a whole cell's font was the evidence.
  sec <- section_of(parsed(), "T-14-1-1")
  expect_equal(unique(vapply(sec$stub_rows, function(r) r$detection_method,
                             character(1))),
               "xlsx_rich_run")
  expect_equal(unique(vapply(sec$stub_rows, function(r) r$detection_confidence,
                             character(1))),
               "high")
})

test_that("a group parent row is kept, because it carries the grouping", {
  ## "Sex, n (%)" has no result columns of its own but states the variable its
  ## levels are grouped by -- dropping it would lose the grouping entirely.
  sec <- section_of(parsed(), "T-14-1-2")
  labels <- vapply(sec$stub_rows, function(r) r$label, character(1))
  expect_true("Sex, n (%)" %in% labels)

  parent <- sec$stub_rows[[which(labels == "Sex, n (%)")]]
  expect_equal(parent$annotation, "ADSL.SEX")
  expect_equal(parent$row_kind, "group")

  ## ... and its levels follow it, unannotated.
  expect_true(all(c("Female", "Male") %in% labels))
  expect_false(sec$stub_rows[[which(labels == "Female")]]$has_annot)
})

test_that("a spacer row produces no stub row", {
  ## Row 9 of Table 14.1.2 is blank. The rows either side of it are real.
  sec <- section_of(parsed(), "T-14-1-2")
  rows <- vapply(sec$stub_rows, function(r) r$sheet_row, integer(1))
  expect_false(9L %in% rows)
  expect_true(all(c(8L, 10L) %in% rows))
})

test_that("each stub row records the sheet row it came from", {
  ## Class 3, and the basis of the whole fill-writer design.
  sec <- section_of(parsed(), "T-14-1-1")
  expect_equal(vapply(sec$stub_rows, function(r) r$sheet_row, integer(1)),
               5:7)
})

test_that("a template stub row is flagged as repeating", {
  sec <- section_of(parsed(), "T-14-3-1")
  labels <- vapply(sec$stub_rows, function(r) r$label, character(1))
  soc <- sec$stub_rows[[which(labels == "<System Organ Class>")]]
  expect_true(soc$is_template)
  expect_equal(soc$annotation, "ADAE.AESOC")

  fixed <- sec$stub_rows[[which(labels == "Subjects with any TEAE")]]
  expect_false(fixed$is_template)
})

## ---------------------------------------------------------------------------
## Column axis
## ---------------------------------------------------------------------------

test_that("the stub header's directives set the column axis and the source", {
  ## "[columns -> ADSL.TRT01A; source ADSL]" states things about the OUTPUT,
  ## not a filter on the stub column, so it goes through the same binder the
  ## Word reader's below-table arrow lines go through.
  sec <- section_of(parsed(), "T-14-1-1")
  expect_equal(sec$column_annotation, "ADSL.TRT01A")
  expect_equal(sec$source_datasets, "ADSL")
  expect_equal(sec$col_headers,
               c("Category", "Placebo", "Drug 10 mg", "Drug 20 mg"))
  expect_equal(sec$n_data_cols, 3L)
})

test_that("a directive arsbridge cannot act on is still visible", {
  ## The denominator clause has no consumer yet. Losing it would mean the
  ## reviewer never learns the shell stated one.
  sec <- section_of(parsed(), "T-14-1-1")
  expect_true(any(grepl("columns -> ADSL.TRT01A",
                        sec$programmer_annotations)))
})

test_that("the source clause never leaks into the column headers", {
  sec <- section_of(parsed(), "T-14-1-1")
  expect_false(any(grepl("source", sec$col_headers, ignore.case = TRUE)))
  expect_false(any(grepl("->", sec$col_headers, fixed = TRUE)))
})

## ---------------------------------------------------------------------------
## Multi-row headers
## ---------------------------------------------------------------------------

test_that("a merged group header over conditioned sub-columns builds a tree", {
  ## Table 14.2.1 has "Drug 10 mg" merged over "Week 12"/"Week 24", each with
  ## its own condition. This is the seam-2 claim: column_tree.R turns Excel
  ## header records into a hierarchy with no change at all.
  sec <- section_of(parsed(), "T-14-2-1")
  expect_equal(sec$n_header_rows, 2L)
  expect_equal(sec$column_tree$mode, "NESTED")

  levels <- vapply(sec$column_tree$levels,
                   function(l) paste0(l$dataset, ".", l$variable), character(1))
  expect_equal(levels, c("ADEX.TRT01AN", "ADEX.AVISITN"))
  expect_equal(length(column_tree_paths(sec$column_tree)), 4L)
})

test_that("a multi-row header still yields one flat label per column", {
  ## Everything that only understands a flat column axis has to keep working.
  sec <- section_of(parsed(), "T-14-2-1")
  expect_equal(sec$col_headers,
               c("Statistic", "Drug 10 mg Week 12", "Drug 10 mg Week 24",
                 "Drug 20 mg Week 12", "Drug 20 mg Week 24"))
  expect_equal(sec$n_data_cols, 4L)
})

test_that("Excel never reports a flagged header row", {
  ## Class 2: there is no <w:tblHeader/> in a worksheet, so the count is
  ## always inferred.
  for (sec in parsed()) {
    expect_equal(sec$header_rows_flagged, 0L, info = sec$tlf_number)
  }
})

## ---------------------------------------------------------------------------
## Footnotes
## ---------------------------------------------------------------------------

test_that("a merged row below the body is a footnote", {
  sec <- section_of(parsed(), "T-14-1-1")
  expect_length(sec$footnotes, 1L)
  expect_match(sec$footnotes[[1]], "^Percentages are based")
})

test_that("a footnote never becomes a stub row", {
  sec <- section_of(parsed(), "T-14-1-1")
  labels <- vapply(sec$stub_rows, function(r) r$label, character(1))
  expect_false(any(grepl("Percentages", labels)))
})

## ---------------------------------------------------------------------------
## Failure modes
## ---------------------------------------------------------------------------

test_that("a workbook with no recognisable output warns and returns nothing", {
  wb <- openxlsx2::wb_workbook()$add_worksheet("Scratch")
  wb$add_data(x = "working notes", col_names = FALSE)
  path <- withr::local_tempfile(fileext = ".xlsx")
  wb$save(path)

  diag_reset()
  expect_warning(secs <- parse_shell_xlsx(path), "No TLF outputs")
  expect_length(secs, 0L)
  d <- diag_records()
  expect_true(any(d$severity == "FAIL"))
  ## The unrecognised sheet is named, so the user knows where to look.
  expect_true(any(grepl("Scratch", d$problem)))
})

test_that("a missing workbook aborts with the path", {
  expect_error(parse_shell_xlsx("no_such_shell.xlsx"), "not found")
})

test_that("a custom heading pattern reaches the sheet names", {
  wb <- openxlsx2::wb_workbook()$add_worksheet("TFL 3.1")
  wb$add_data(x = "TFL 3.1", col_names = FALSE)
  wb$add_data(x = "A Title", start_row = 2, col_names = FALSE)
  wb$add_data(x = c("Category", "Arm A"), start_row = 3, col_names = FALSE)
  path <- withr::local_tempfile(fileext = ".xlsx")
  wb$save(path)

  secs <- suppressMessages(suppressWarnings(parse_shell_xlsx(
    path, heading_patterns = "^TFL\\s+(?<number>\\d+(?:\\.\\d+)*)$")))
  expect_length(secs, 1L)
  expect_equal(secs[[1]]$tlf_number, "T-3-1")
})

## ---------------------------------------------------------------------------
## The column axis and the denominator it will be divided by
## ---------------------------------------------------------------------------

## Percentages divide by ADSL, and {cards} splits that frame by the column
## variable. A column axis on a domain variable ADSL does not carry -- the
## ordinary shape of a conmeds or medical-history shell, where the domain says
## TRTA and ADSL says TRT01A -- therefore reports every percentage out of the
## whole study. It is a property of the SHELL, so the author hears about it at
## parse time; the engine will not repair it by guessing that TRTA means
## TRT01A (they are not interchangeable in a crossover or multi-period study).

.axis_shell <- function(axis, envir = parent.frame()) {
  wb <- openxlsx2::wb_workbook()$add_worksheet("Table 14.4.1")
  put <- function(x, row, col = 1L) {
    wb$add_data(sheet = "Table 14.4.1", x = x, start_row = row,
                start_col = col, col_names = FALSE)
  }
  black <- openxlsx2::wb_color(hex = "FF000000")
  red   <- openxlsx2::wb_color(hex = "FFC00000")
  annotated <- function(label, annotation) {
    openxlsx2::fmt_txt(label, color = black, size = 10) +
      openxlsx2::fmt_txt(paste0("\n", annotation), color = red, size = 8,
                         italic = TRUE)
  }
  put("Table 14.4.1", 1)
  put("Concomitant Medications by Class", 2)
  put(annotated("Safety Population ", "(ADSL.SAFFL='Y')"), 3)
  for (r in 1:3) {
    wb$merge_cells(sheet = "Table 14.4.1", dims = sprintf("A%d:D%d", r, r))
  }
  put(annotated("Medication Class",
                sprintf("[columns -> %s; source ADCM]", axis)), 4)
  for (j in seq_along(c("Placebo", "Drug A", "Drug B"))) {
    put(c("Placebo", "Drug A", "Drug B")[[j]], 4, j + 1L)
  }
  put(annotated("Any medication", "[ADCM.CMCLAS]"), 5)
  for (j in 1:3) put("xx (xx.x)", 5, j + 1L)
  path <- withr::local_tempfile(fileext = ".xlsx", .local_envir = envir)
  wb$save(path)
  path
}

.axis_lookup <- list(
  "ADSL.SAFFL"  = list(dataset = "ADSL", variable = "SAFFL"),
  "ADSL.TRT01A" = list(dataset = "ADSL", variable = "TRT01A"),
  "ADCM.TRTA"   = list(dataset = "ADCM", variable = "TRTA"),
  "ADCM.CMCLAS" = list(dataset = "ADCM", variable = "CMCLAS")
)

.axis_findings <- function(axis) {
  diag_reset()
  invisible(suppressMessages(suppressWarnings(
    parse_shell_xlsx(.axis_shell(axis), spec_lookup = .axis_lookup))))
  recs <- diag_records()
  recs[grepl("Column axis", recs$problem), , drop = FALSE]
}

test_that("a column axis ADSL cannot answer is reported at parse time", {
  found <- .axis_findings("ADCM.TRTA")
  expect_equal(nrow(found), 1L)
  expect_equal(found$severity, "WARN")
  expect_match(found$problem, "no ADSL.TRTA")
  expect_match(found$problem, "out of the whole study")
  ## The advice is the fix the author can actually make.
  expect_match(found$action, "ADSL.TRT01A", fixed = TRUE)
})

test_that("a domain copy of an ADSL variable is noted, not warned about", {
  ## ADAE.TRT01A and its kin produce correct percentages -- ADSL carries
  ## TRT01A, so the denominator splits. Worth saying, not worth a warning on
  ## an authoring style that is right.
  found <- .axis_findings("ADCM.TRT01A")
  expect_equal(nrow(found), 1L)
  expect_equal(found$severity, "INFO")
  expect_match(found$action, "columns -> ADSL.TRT01A", fixed = TRUE)
})

test_that("an ADSL column axis says nothing at all", {
  expect_equal(nrow(.axis_findings("ADSL.TRT01A")), 0L)
})
