## Tests for the fill writer.
##
## The failure this file exists to catch is a number landing in the wrong
## cell. That failure does not throw, does not look wrong, and survives
## review -- so the end-to-end block below checks values against results
## computed a second way, from the raw datasets, rather than against whatever
## the pipeline happened to produce.

skip_if_not_installed("openxlsx2")

SHELL <- test_path("fixtures", "shells_apx_drm_301.xlsx")
ADAM  <- test_path("fixtures", "adam_apx_drm_301")
SPEC  <- test_path("fixtures", "adam_spec_apx_drm_301.xlsx")

## ---------------------------------------------------------------------------
## Reading the ARD
## ---------------------------------------------------------------------------

test_that("proportions are rescaled per analysis, and only when they can be", {
  ## cards reports p in [0, 1]; a shell's "xx.x" wants 25.0, not 0.3.
  expect_equal(
    .rescale_proportions(c(0.25, 0.75), c("p", "p"), c("A", "A")),
    c(25, 75))

  ## Already a percentage: multiplying again would be the worse guess.
  expect_equal(
    .rescale_proportions(c(25, 75), c("p", "p"), c("A", "A")),
    c(25, 75))

  ## One analysis out of range must not stop the healthy one being rescaled.
  expect_equal(
    .rescale_proportions(c(0.5, 140), c("p", "p"), c("A", "B")),
    c(50, 140))

  ## Non-proportion statistics are never touched.
  expect_equal(
    .rescale_proportions(c(0.25, 0.25), c("p", "mean"), c("A", "A")),
    c(25, 0.25))
})

test_that("a label matches its own punctuation variants and nothing else", {
  ## The crossed column axis: the shell stacks two header rows, the ARD joins
  ## the grouping path with " > ". Same column, two spellings.
  expect_true(.same_label("Drug 10 mg > Week 12", "Drug 10 mg Week 12"))
  expect_true(.same_label("PLACEBO", "Placebo"))

  ## And it must not collapse two genuinely different columns.
  expect_false(.same_label("Drug 10 mg > Week 12", "Drug 20 mg Week 12"))
  expect_false(.same_label("Drug 10 mg", "Drug 100 mg"))
})

test_that("shell labels pair with data codes only when the pairing is certain", {
  index <- data.frame(
    analysis_id    = rep("AN1", 4),
    variable_level = c("F", "F", "M", "M"),
    stringsAsFactors = FALSE)

  ## The ordinary case: "Female"/"Male" over F/M is a one-to-one pairing.
  expect_equal(.level_decode(index, "AN1", c("Female", "Male")),
               c(Female = "F", Male = "M"))

  ## Ambiguity is refused, not resolved by order. Both labels begin with "M".
  ambiguous <- data.frame(analysis_id = rep("AN1", 2),
                          variable_level = c("M", "MO"),
                          stringsAsFactors = FALSE)
  expect_length(.level_decode(ambiguous, "AN1", c("Moderate", "Mild")), 0)

  ## Nothing to pair: the labels already match the data.
  exact <- data.frame(analysis_id = rep("AN1", 2),
                      variable_level = c("Female", "Male"),
                      stringsAsFactors = FALSE)
  expect_length(.level_decode(exact, "AN1", c("Female", "Male")), 0)

  ## No relationship at all: refused rather than guessed positionally.
  coded <- data.frame(analysis_id = rep("AN1", 2),
                      variable_level = c("1", "2"),
                      stringsAsFactors = FALSE)
  expect_length(.level_decode(coded, "AN1", c("Female", "Male")), 0)
})

test_that("a cell answered by many levels is ambiguous, never the first of them", {
  ## "<System Organ Class>" stands for a repeated block. Writing one SOC's
  ## count into it would hide every other SOC behind a real-looking number.
  index <- data.frame(
    analysis_id    = rep("AN1", 3),
    group_level    = rep("Placebo", 3),
    variable_level = c("Cardiac disorders", "Nervous system disorders",
                       "Skin disorders"),
    stat_name      = rep("n", 3),
    value          = c(1, 2, 3),
    status         = rep("computed", 3),
    stringsAsFactors = FALSE)

  hit <- .ard_value(index, "AN1", "Placebo", NA_character_, "n")
  expect_equal(hit$status, "ambiguous")
  expect_true(is.na(hit$value))
})

test_that("a reserved manual cell is reported as such, not as a missing one", {
  index <- data.frame(
    analysis_id = "AN1", group_level = "Placebo", variable_level = NA_character_,
    stat_name = "n", value = NA_real_, status = "manual_pending",
    stringsAsFactors = FALSE)
  expect_equal(.ard_value(index, "AN1", "Placebo", NA_character_, "n")$status,
               "manual_pending")
  ## ADR 0002: the distinction survives to the reader of the workbook.
  expect_equal(.pending_reason(c("manual_pending")),
               "reserved for manual derivation")
})

test_that("the reason names the first failing slot, not the worst one", {
  ## "xx (xx.x)" with no count and a surplus percentage: the missing count is
  ## what the reader needs to hear about.
  expect_equal(.pending_reason(c("no_row", "unbound")),
               "no result in the ARD for this cell")
  expect_equal(.pending_reason(c("computed", "unbound")),
               "the placeholder asks for a statistic the analysis does not produce")
  expect_true(is.na(.pending_reason(c("computed", "computed"))))
})

## ---------------------------------------------------------------------------
## Formatting
## ---------------------------------------------------------------------------

test_that("the placeholder is the format specification", {
  expect_equal(.format_value(66, 1L), "66.0")
  expect_equal(.format_value(5.16397, 2L), "5.16")
  expect_equal(.format_value(4, 0L), "4")
  ## Trailing zeros are the point: a TLF cell reads "12.0", not "12".
  expect_equal(.format_value(12, 1L), "12.0")
  expect_true(is.na(.format_value(NA_real_, 1L)))
})

test_that("substitution keeps the punctuation the author chose", {
  slots <- list(list(start = 1L, stop = 4L), list(start = 7L, stop = 11L))
  expect_equal(.format_placeholder("xx.x (xx.xx)", slots, c("66.0", "5.16")),
               "66.0 (5.16)")

  ## An unresolved slot keeps its placeholder, so the gap stays visible.
  expect_equal(.format_placeholder("xx.x (xx.xx)", slots, c("66.0", NA)),
               "66.0 (xx.xx)")

  ## Nothing resolved: nothing to write.
  expect_null(.format_placeholder("xx.x (xx.xx)", slots, c(NA, NA)))
})

test_that("normalized offsets map back through dropped zero-width characters", {
  ## The parser drops zero-width characters, so position i in the text it
  ## recorded is not position i in the file.
  expect_equal(.raw_index_map("abc"), 1:3)
  expect_equal(.raw_index_map("a​bc"), c(1L, 3L, 4L))
})

## ---------------------------------------------------------------------------
## Cell XML surgery
## ---------------------------------------------------------------------------

test_that("a kept run is byte-identical, including what arsbridge cannot model", {
  xml <- paste0(
    '<is><r><rPr><rFont val="Arial"/><b val="1"/><color rgb="FF000000"/>',
    '<sz val="10"/></rPr><t>Age (years)</t></r>',
    '<r><rPr><rFont val="Arial"/><i val="1"/><color rgb="FFC00000"/>',
    '<sz val="8"/></rPr><t xml:space="preserve">\n[ADSL.AGE]</t></r></is>')
  doc <- xml2::read_xml(xml)
  expect_true(.drop_annotation_runs(doc))

  out <- .is_xml(doc)
  ## rFont and sz are on the run and in neither the run model nor fmt_txt's
  ## reach -- rebuilding the cell would have dropped them.
  expect_match(out, 'rFont val="Arial"', fixed = FALSE)
  expect_match(out, 'sz val="10"', fixed = FALSE)
  expect_false(grepl("C00000", out))
  expect_equal(.is_text(doc), "Age (years)")
})

test_that("a cell with no annotation is left completely alone", {
  xml <- '<is><r><rPr><color rgb="FF000000"/></rPr><t>Mean (SD)</t></r></is>'
  doc <- xml2::read_xml(xml)
  expect_false(.drop_annotation_runs(doc))
})

test_that("a range spanning two runs is written once, not twice", {
  xml <- '<is><r><t>xx</t></r><r><t>.x</t></r></is>'
  doc <- xml2::read_xml(xml)
  expect_true(.replace_ranges(doc, list(list(start = 1L, stop = 4L,
                                             text = "66.0"))))
  expect_equal(.is_text(doc), "66.0")
})

test_that("leading and trailing spaces survive an edit", {
  xml <- '<is><r><t xml:space="preserve">  xx  </t></r></is>'
  doc <- xml2::read_xml(xml)
  .replace_ranges(doc, list(list(start = 3L, stop = 4L, text = "42")))
  expect_equal(.is_text(doc), "  42  ")
})

test_that("editing a shared string detaches the cell instead of the entry", {
  ## Excel interns strings, and in a TLF shell one entry backs many cells --
  ## "xx.x (xx.xx)" is the same string for a whole table. Editing the entry
  ## would write one result into every cell that shares the placeholder.
  path <- test_path("fixtures", "shell_cells_shared_apx.xlsx")
  skip_if_not(file.exists(path))

  wb <- openxlsx2::wb_load(path)
  cc <- wb$worksheets[[1]]$sheet_data$cc
  shared <- which(cc$c_t == "s")
  skip_if(length(shared) == 0, "fixture interns no strings")

  slot <- shared[[1]]
  xml <- .cell_is_xml(wb, cc, slot)
  expect_false(is.na(xml))
  expect_match(xml, "^<is")

  before <- wb$sharedStrings
  cc <- .cell_set_is(cc, slot, "<is><t>edited</t></is>")
  expect_equal(cc$c_t[[slot]], "inlineStr")
  expect_equal(cc$v[[slot]], "")
  ## The shared table itself is untouched, so no other cell moved.
  expect_identical(wb$sharedStrings, before)
})

## ---------------------------------------------------------------------------
## Guards
## ---------------------------------------------------------------------------

test_that("a Word shell is refused with the reason, not filled", {
  expect_error(
    ars_fill_shell(test_path("fixtures", "annotated_shell_2tlf_minimal.docx"),
                   ars = list(outputs = list()), ard = NULL,
                   output_path = withr::local_tempfile(fileext = ".xlsx")),
    "Only an Excel shell")
})

test_that("an ARS with no cell map is refused rather than silently doing nothing", {
  expect_error(
    ars_fill_shell(SHELL, ars = list(outputs = list(list(id = "T1"))),
                   ard = NULL,
                   output_path = withr::local_tempfile(fileext = ".xlsx")),
    "no cell map")
})

test_that("an existing output file is not replaced without being asked", {
  existing <- withr::local_tempfile(fileext = ".xlsx")
  writeLines("not a workbook", existing)
  expect_error(
    ars_fill_shell(SHELL, ars = list(outputs = list()), ard = NULL,
                   output_path = existing),
    "already exists")
})

## ---------------------------------------------------------------------------
## End to end
## ---------------------------------------------------------------------------

## The whole chain, once, shared by the tests below.
filled_run <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    ## Not withr::local_tempfile: these outlive the call, because every test
    ## below reuses this one run rather than rebuilding the chain.
    ars <- tempfile(fileext = ".json")
    withr::with_envvar(
      c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
        GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
      suppressMessages(suppressWarnings(spec_to_ars(
        shell_path = SHELL, adam_spec_path = SPEC, api_key = "",
        output_path = ars, report_path = tempfile(fileext = ".xlsx"),
        verbose = FALSE))))
    ard <- suppressMessages(suppressWarnings(ars_to_ard(ars, ADAM)))
    out <- tempfile(fileext = ".xlsx")
    res <- suppressMessages(suppressWarnings(ars_fill_shell(
      shell_path = SHELL, ars = ars, ard = ard, output_path = out,
      adam_dir = ADAM, overwrite = TRUE)))
    cache <<- list(ars = ars, ard = ard, path = out, res = res,
                   book = xlsx_read_shell_cells(out))
    cache
  }
})

cell_text <- function(book, sheet, ref) {
  cells <- book$sheets[[sheet]]$cells
  hit <- cells$text[cells$ref == ref]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

test_that("continuous statistics match results computed straight from the data", {
  skip_if_not_installed("withr")
  run <- filled_run()
  adsl <- utils::read.csv(file.path(ADAM, "ADSL.csv"), stringsAsFactors = FALSE)

  ## Computed here, from the raw dataset, not read back out of the pipeline.
  for (col in list(c("B", "Placebo"), c("C", "Drug 10 mg"), c("D", "Drug 20 mg"))) {
    ages <- adsl$AGE[adsl$TRT01A == col[[2]]]
    expect_equal(
      cell_text(run$book, "Table 14.1.2", paste0(col[[1]], "6")),
      sprintf("%.1f (%.2f)", mean(ages), stats::sd(ages)),
      info = col[[2]])
    expect_equal(
      cell_text(run$book, "Table 14.1.2", paste0(col[[1]], "7")),
      sprintf("%.1f", stats::median(ages)),
      info = col[[2]])
  }
})

test_that("counts and percentages match, in the right columns", {
  run <- filled_run()
  adsl <- utils::read.csv(file.path(ADAM, "ADSL.csv"), stringsAsFactors = FALSE)

  ## The arms differ on purpose: 2/2, 1/3, 3/1. A column mix-up cannot pass.
  for (col in list(c("B", "Placebo"), c("C", "Drug 10 mg"), c("D", "Drug 20 mg"))) {
    arm <- adsl[adsl$TRT01A == col[[2]], , drop = FALSE]
    n_f <- sum(arm$SEX == "F")
    expect_equal(
      cell_text(run$book, "Table 14.1.2", paste0(col[[1]], "11")),
      sprintf("%d (%.1f)", n_f, 100 * n_f / nrow(arm)),
      info = col[[2]])
  }

  ## "Female"/"Male" against F/M -- the decode pairing, end to end.
  expect_equal(cell_text(run$book, "Table 14.1.2", "B12"), "2 (50.0)")
})

test_that("a crossed column axis lands in the right one of four columns", {
  run <- filled_run()
  adex <- utils::read.csv(file.path(ADAM, "ADEX.csv"), stringsAsFactors = FALSE)

  expected <- function(arm, visit) {
    v <- adex$TRTDURD[adex$TRT01A == arm & adex$AVISITN == visit]
    sprintf("%.1f (%.2f)", mean(v), stats::sd(v))
  }
  ## Two arms x two visits, all four distinct: the header says which is which.
  expect_equal(cell_text(run$book, "Table 14.2.1", "B7"),
               expected("Drug 10 mg", 12))
  expect_equal(cell_text(run$book, "Table 14.2.1", "C7"),
               expected("Drug 10 mg", 24))
  expect_equal(cell_text(run$book, "Table 14.2.1", "D7"),
               expected("Drug 20 mg", 12))
  expect_equal(cell_text(run$book, "Table 14.2.1", "E7"),
               expected("Drug 20 mg", 24))
})

test_that("quartile lines take the statistic the label names, in order", {
  ## The Q1/Q3 line must not receive the median, and the median line must not
  ## receive the method's whole statistic list. The definition of a quartile
  ## is {cards}'s, so it is compared against the ARD rather than base R --
  ## what is being tested is which statistic reached which line.
  run <- filled_run()
  chr <- function(col) vapply(col, function(x) {
    if (length(x) == 0 || is.null(x[[1]]) || is.function(x[[1]])) NA_character_
    else as.character(x[[1]])[[1]]
  }, character(1))
  ard <- run$ard
  pick <- function(stat) {
    rows <- chr(ard$analysis_id) == "AN_T_14_1_2_001" &
      as.character(ard$stat_name) == stat &
      chr(ard$group1_level) == "Placebo"
    as.numeric(chr(ard$stat)[rows][[1]])
  }
  expect_equal(cell_text(run$book, "Table 14.1.2", "B8"),
               sprintf("%.0f, %.0f", pick("p25"), pick("p75")))
})

test_that("a template row is left alone rather than given one level's value", {
  run <- filled_run()
  ## "<System Organ Class>" needs row expansion, which the writer does not do.
  expect_match(cell_text(run$book, "Table 14.3.1", "B7"), "^x+ \\(x+\\.x+\\)$")
  reasons <- run$res$diagnostics$reason[run$res$diagnostics$sheet == "Table 14.3.1"]
  expect_true(any(grepl("repeated block", reasons)))
})

test_that("an unfillable cell keeps its placeholder and is reported", {
  run <- filled_run()
  ## Placebo has no discontinuation in the fixture, so no ARD row exists.
  ## An empty cell would read as a zero; the placeholder says "not computed".
  expect_match(cell_text(run$book, "Table 14.1.1", "B7"), "x")
  expect_true(any(run$res$diagnostics$ref == "B7" &
                    run$res$diagnostics$sheet == "Table 14.1.1"))
  expect_gt(run$res$pending, 0)
  expect_equal(run$res$skipped, 0)
})

test_that("no annotation survives into the deliverable", {
  run <- filled_run()
  left <- 0L
  for (sheet in run$book$sheets) {
    for (i in seq_len(nrow(sheet$cells))) {
      for (r in sheet$cells$runs[[i]]) {
        if (.is_annotation_styled_run(r)) left <- left + 1L
      }
    }
  }
  expect_equal(left, 0L)
})

test_that("the author's layout comes through untouched", {
  run <- filled_run()
  before <- xlsx_read_shell_cells(SHELL)

  unzip_to <- function(p) {
    d <- tempfile(); dir.create(d); utils::unzip(p, exdir = d); d
  }
  db <- unzip_to(SHELL); da <- unzip_to(run$path)
  attrs <- function(dir, part, tag, attr) {
    doc <- xml2::read_xml(file.path(dir, part))
    xml2::xml_attr(xml2::xml_find_all(
      doc, paste0("//*[local-name()='", tag, "']")), attr)
  }

  ## A listing legitimately changes shape -- its template row becomes N rows,
  ## and everything below moves down -- so the strict "nothing moved" check
  ## applies to the sheets that keep their shape. The listing's own invariant
  ## is checked below.
  unchanged <- setdiff(names(before$sheets),
                       grep("^Listing", names(before$sheets), value = TRUE))
  for (name in unchanged) {
    b <- before$sheets[[name]]
    a <- run$book$sheets[[name]]
    expect_identical(sort(b$merges$ref), sort(a$merges$ref), info = name)
    expect_identical(attrs(db, b$part, "col", "width"),
                     attrs(da, a$part, "col", "width"), info = name)
    expect_identical(attrs(db, b$part, "row", "ht"),
                     attrs(da, a$part, "row", "ht"), info = name)
  }

  ## Column widths are a property of the sheet, not of its rows, so they must
  ## survive an expansion too.
  for (name in grep("^Listing", names(before$sheets), value = TRUE)) {
    b <- before$sheets[[name]]
    a <- run$book$sheets[[name]]
    expect_identical(attrs(db, b$part, "col", "width"),
                     attrs(da, a$part, "col", "width"), info = name)
    expect_gte(nrow(a$cells), nrow(b$cells))
  }
})

test_that("labels, titles and footnotes are exactly as authored", {
  run <- filled_run()
  before <- xlsx_read_shell_cells(SHELL)
  ## Column A of a table sheet is labels; only annotation stripping may touch
  ## it, so anything that carried no annotation must be identical.
  sheet <- "Table 14.1.1"
  for (ref in c("A1", "A2", "A5", "A6", "A7", "A8", "B4", "C4", "D4")) {
    b <- cell_text(before, sheet, ref)
    a <- cell_text(run$book, sheet, ref)
    if (grepl("\\[", b %||% "")) next   # carried an annotation
    expect_identical(a, b, info = ref)
  }
})

test_that("the filled workbook still opens", {
  run <- filled_run()
  expect_s3_class(openxlsx2::wb_load(run$path), "wbWorkbook")
})

test_that("pending placeholders can be cleared when the workbook is going out for manual entry", {
  run <- filled_run()
  out <- tempfile(fileext = ".xlsx")
  suppressMessages(suppressWarnings(ars_fill_shell(
    shell_path = SHELL, ars = run$ars, ard = run$ard, output_path = out,
    keep_pending_placeholders = FALSE, overwrite = TRUE)))
  book <- xlsx_read_shell_cells(out)
  expect_equal(cell_text(book, "Table 14.1.1", "B7") %||% "", "")
  ## A filled cell is still filled.
  expect_match(cell_text(book, "Table 14.1.2", "B6"), "^66")
})

test_that("annotations can be kept for review", {
  run <- filled_run()
  out <- tempfile(fileext = ".xlsx")
  suppressMessages(suppressWarnings(ars_fill_shell(
    shell_path = SHELL, ars = run$ars, ard = run$ard, output_path = out,
    strip_annotations = FALSE, overwrite = TRUE)))
  book <- xlsx_read_shell_cells(out)
  expect_match(cell_text(book, "Table 14.1.2", "A5"), "ADSL.AGE", fixed = TRUE)
  ## The numbers are there either way.
  expect_match(cell_text(book, "Table 14.1.2", "B6"), "^66")
})

## There is deliberately no test here that the writer and the renderer agree
## on what a percentage is. They agree by construction: `.rescale_proportions()`
## is one function and `.tfrmt_prep_ard_layout()` calls it. The behaviour is
## covered by the unit tests at the top of this file and, end to end, by
## "counts and percentages match, in the right columns" above, which checks
## the written cell against 100 * n / N computed from the raw dataset.
##
## An earlier version grepped R/ars_to_tfrmt.R for the call. Do not bring that
## back: tests run from the installed package under R CMD check, where there
## is no R/ directory, so it errored on every platform that got that far --
## and it asserted nothing about behaviour even when it passed.

## ---------------------------------------------------------------------------
## Listings: one template row becomes N
## ---------------------------------------------------------------------------

## A minimal listing sheet: header, one template row, and a merged footnote
## under it. Built in-test rather than committed, because what is being tested
## is the mechanism -- what happens to the rows BELOW the template when the
## template expands -- and that needs a footnote the committed fixture's
## listings do not have.
mini_listing <- function(footnote = TRUE) {
  wb <- openxlsx2::wb_workbook()$add_worksheet("L")
  wb$add_data(sheet = "L", x = "Listing 1.1", dims = "A1", col_names = FALSE)
  wb$add_data(sheet = "L", x = data.frame(a = "Subject", b = "Term",
                                          stringsAsFactors = FALSE),
              dims = "A2", col_names = FALSE)
  wb$add_data(sheet = "L", x = data.frame(a = "xxx-xxx", b = "xxxx",
                                          stringsAsFactors = FALSE),
              dims = "A3", col_names = FALSE)
  if (footnote) {
    wb$add_data(sheet = "L", x = "Source: ADAE.", dims = "A4", col_names = FALSE)
    wb$merge_cells(sheet = "L", dims = "A4:B4")
  }
  path <- tempfile(fileext = ".xlsx")
  openxlsx2::wb_save(wb, path, overwrite = TRUE)
  path
}

test_that("a template row expands into one row per record", {
  path <- mini_listing()
  wb <- openxlsx2::wb_load(path)
  rows <- data.frame(a = c("S1", "S2", "S3"), b = c("Headache", "Rash", "Nausea"),
                     stringsAsFactors = FALSE)

  res <- .fill_listing_sheet(wb, 1L, list(template_row = 3L), rows, TRUE)
  expect_equal(res$records[[1]]$status, "filled")
  expect_equal(res$records[[1]]$rows, 3L)

  out <- tempfile(fileext = ".xlsx")
  openxlsx2::wb_save(wb, out, overwrite = TRUE)
  sheet <- xlsx_read_shell_cells(out)$sheets[[1]]
  txt <- function(ref) {
    hit <- sheet$cells$text[sheet$cells$ref == ref]
    if (length(hit) == 0) NA_character_ else hit[[1]]
  }
  expect_equal(txt("A3"), "S1"); expect_equal(txt("B3"), "Headache")
  expect_equal(txt("A4"), "S2"); expect_equal(txt("B4"), "Rash")
  expect_equal(txt("A5"), "S3"); expect_equal(txt("B5"), "Nausea")
})

test_that("the footnote below a template row moves down with it, merge and all", {
  ## The reason .shift_rows_down() exists. A footnote left where it was would
  ## be overwritten by the third data row, and its merge would swallow two of
  ## them.
  path <- mini_listing()
  before <- xlsx_read_shell_cells(path)$sheets[[1]]
  expect_equal(before$merges$ref, "A4:B4")

  wb <- openxlsx2::wb_load(path)
  rows <- data.frame(a = c("S1", "S2", "S3"), b = c("x", "y", "z"),
                     stringsAsFactors = FALSE)
  .fill_listing_sheet(wb, 1L, list(template_row = 3L), rows, TRUE)
  out <- tempfile(fileext = ".xlsx")
  openxlsx2::wb_save(wb, out, overwrite = TRUE)

  after <- xlsx_read_shell_cells(out)$sheets[[1]]
  ftxt <- after$cells$text[after$cells$ref == "A6"]
  expect_equal(ftxt, "Source: ADAE.")
  expect_equal(after$merges$ref, "A6:B6")
  ## And the data rows are intact underneath it.
  expect_equal(after$cells$text[after$cells$ref == "A5"], "S3")
})

test_that("a single-row listing needs no shift and moves nothing", {
  path <- mini_listing()
  wb <- openxlsx2::wb_load(path)
  .fill_listing_sheet(wb, 1L, list(template_row = 3L),
                      data.frame(a = "S1", b = "x", stringsAsFactors = FALSE), TRUE)
  out <- tempfile(fileext = ".xlsx")
  openxlsx2::wb_save(wb, out, overwrite = TRUE)
  after <- xlsx_read_shell_cells(out)$sheets[[1]]
  expect_equal(after$cells$text[after$cells$ref == "A3"], "S1")
  expect_equal(after$merges$ref, "A4:B4")   # unmoved
})

test_that("an empty listing keeps its template and says so", {
  path <- mini_listing()
  wb <- openxlsx2::wb_load(path)
  res <- .fill_listing_sheet(wb, 1L, list(template_row = 3L),
                             data.frame(a = character(), b = character()), TRUE)
  expect_equal(res$records[[1]]$status, "pending")
  expect_match(res$records[[1]]$reason, "selected no rows")

  ## Distinguished from not having the data at all.
  res2 <- .fill_listing_sheet(wb, 1L, list(template_row = 3L), NULL, TRUE)
  expect_match(res2$records[[1]]$reason, "could not be read")
})

test_that("shifting rows moves cells, merges, and the declared extent together", {
  ## Missing any one of these still produces a file that opens, which is what
  ## makes it dangerous: a stale merge silently swallows a data row.
  path <- mini_listing()
  wb <- openxlsx2::wb_load(path)
  wb$worksheets[[1]] <- .shift_rows_down(wb$worksheets[[1]], from_row = 3L,
                                         by = 2L, template_row = 3L)
  out <- tempfile(fileext = ".xlsx")
  openxlsx2::wb_save(wb, out, overwrite = TRUE)
  after <- xlsx_read_shell_cells(out)$sheets[[1]]

  expect_equal(after$cells$text[after$cells$ref == "A1"], "Listing 1.1")  # above
  expect_equal(after$cells$text[after$cells$ref == "A2"], "Subject")      # above
  expect_equal(after$cells$text[after$cells$ref == "A5"], "xxx-xxx")      # moved
  expect_equal(after$cells$text[after$cells$ref == "A6"], "Source: ADAE.")
  expect_equal(after$merges$ref, "A6:B6")
  expect_match(wb$worksheets[[1]]$dimension, "B6")
})

## ---------------------------------------------------------------------------
## Figures: a series computed from the shell's prose
## ---------------------------------------------------------------------------

test_that("a figure directive's variable references are read correctly", {
  expect_equal(.figure_ref("ADVS.AVISITN (label ADVS.AVISIT)"),
               list(dataset = "ADVS", variable = "AVISITN"))
  expect_equal(.figure_label_ref("ADVS.AVISITN (label ADVS.AVISIT)"),
               list(dataset = "ADVS", variable = "AVISIT"))
  expect_equal(.figure_ref("mean of ADVS.AVAL"),
               list(dataset = "ADVS", variable = "AVAL"))
  expect_null(.figure_ref("no reference here"))
  expect_null(.figure_label_ref("ADVS.AVAL"))
})

test_that("the figure series matches means computed straight from the data", {
  run <- filled_run()
  sheet <- run$book$sheets[["Figure 14.3.1"]]
  advs <- utils::read.csv(file.path(ADAM, "ADVS.csv"), stringsAsFactors = FALSE)
  pulse <- advs[advs$PARAMCD == "PULSE", ]

  cell <- function(ref) {
    hit <- sheet$cells$text[sheet$cells$ref == ref]
    if (length(hit) == 0) NA_character_ else hit[[1]]
  }
  ## Header of the written block.
  expect_equal(cell("A7"), "AVISITN")
  expect_equal(cell("F7"), "se")

  ## Every written row, against the same statistic computed here.
  for (r in 8:16) {
    visit <- suppressWarnings(as.numeric(cell(paste0("A", r))))
    arm   <- cell(paste0("C", r))
    if (is.na(visit) || is.na(arm)) next
    v <- pulse$AVAL[pulse$AVISITN == visit & pulse$TRTA == arm]
    expect_equal(as.numeric(cell(paste0("D", r))), length(v), info = paste(arm, visit))
    expect_equal(as.numeric(cell(paste0("E", r))), mean(v), tolerance = 1e-6,
                 info = paste(arm, visit))
    expect_equal(as.numeric(cell(paste0("F", r))),
                 stats::sd(v) / sqrt(length(v)), tolerance = 1e-6,
                 info = paste(arm, visit))
  }
})

test_that("the figure's filter actually filters", {
  ## The fixture carries a second parameter (DIABP) precisely so this can
  ## fail: an unapplied filter averages both together, which looks like a
  ## plausible number rather than an error.
  run <- filled_run()
  sheet <- run$book$sheets[["Figure 14.3.1"]]
  n <- suppressWarnings(as.numeric(
    sheet$cells$text[sheet$cells$ref == "D8"]))
  advs <- utils::read.csv(file.path(ADAM, "ADVS.csv"), stringsAsFactors = FALSE)
  expect_equal(n, 4)                       # one parameter, one arm, one visit
  expect_equal(nrow(advs) / 2, sum(advs$PARAMCD == "PULSE"))
})

test_that("a figure with no ADaM directory is reported, not silently skipped", {
  res <- .fill_figure_sheet(openxlsx2::wb_workbook()$add_worksheet("F"), 1L,
                            list(series_anchor = 5L, directives = list()),
                            NULL, "F")
  expect_equal(res$records[[1]]$status, "pending")
  expect_match(res$records[[1]]$reason, "no ADaM directory")
})

test_that("listings and figures fill end to end, and nothing else regresses", {
  run <- filled_run()
  ## One row per AE record. The shell declares no TRTEMFL filter on this
  ## listing -- only the safety population, which every subject is in -- so
  ## every event is listed, including the one that is not treatment-emergent.
  ## Asserting the filtered count here would be testing a filter the shell
  ## never asked for.
  adae <- utils::read.csv(file.path(ADAM, "ADAE.csv"), stringsAsFactors = FALSE)
  expected <- nrow(adae)
  sheet <- run$book$sheets[["Listing 16.2.1"]]
  body <- sheet$cells[sheet$cells$col == 1 & sheet$cells$row >= 5, , drop = FALSE]
  expect_equal(nrow(body), expected)

  ## Tables are still filled as before.
  t2 <- run$book$sheets[["Table 14.1.2"]]
  expect_equal(t2$cells$text[t2$cells$ref == "B6"], "66.0 (5.16)")
})
