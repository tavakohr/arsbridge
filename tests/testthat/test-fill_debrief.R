## The fill debrief (PR C1): the census keeps every cell -- filled ones
## included -- with its position and analysis, plain rollups answer "which
## column lost everything and why", and every pending reason carries an
## author-facing hint. This is the self-service diagnosis story for a
## machine nothing may leave: the answers must be complete on-screen.

skip_if_not_installed("openxlsx2")


## --- the census ---------------------------------------------------------------

test_that(".fill_census keeps every cell with its position and analysis", {
  records <- list(
    list(output_id = "T1", sheet = "S", ref = "B5", row = 5L, col = 2L,
         col_label = "Drug A", analysis_id = "AN_1", status = "filled",
         text = "3 (60.0)"),
    list(output_id = "T1", sheet = "S", ref = "C5", row = 5L, col = 3L,
         col_label = "Placebo", analysis_id = "AN_1", status = "pending",
         reason = "the analysis produced no value for this cell"),
    ## An output-level skip has no cell address at all.
    list(output_id = "T2", sheet = "S2", ref = NA_character_,
         status = "skipped",
         reason = "no cell map recorded for this output")
  )
  census <- .fill_census(records)
  expect_s3_class(census, "data.frame")
  expect_equal(nrow(census), 3)
  expect_setequal(
    names(census),
    c("output_id", "sheet", "ref", "row", "col", "col_label",
      "analysis_id", "status", "reason"))
  ## The filled cell is IN the census, with its position, and no reason.
  filled <- census[census$status == "filled", ]
  expect_equal(filled$row, 5L)
  expect_equal(filled$col, 2L)
  expect_equal(filled$col_label, "Drug A")
  expect_equal(filled$analysis_id, "AN_1")
  expect_true(is.na(filled$reason))
  ## The addressless skip survives with NA position.
  expect_true(is.na(census$row[census$output_id == "T2"]))
})

test_that(".fill_census of nothing is an empty frame with the full shape", {
  census <- .fill_census(list())
  expect_equal(nrow(census), 0)
  expect_true(all(c("output_id", "sheet", "ref", "row", "col", "col_label",
                    "analysis_id", "status", "reason") %in% names(census)))
})


## --- the rollups ---------------------------------------------------------------

.debrief_census <- function() {
  data.frame(
    output_id   = "T1",
    sheet       = "Table X",
    ref         = c("B5", "C5", "D5", "B6", "C6", "D6"),
    row         = c(5L, 5L, 5L, 6L, 6L, 6L),
    col         = c(2L, 3L, 4L, 2L, 3L, 4L),
    col_label   = rep(c("Drug A", "Placebo", "Total"), 2),
    analysis_id = "AN_1",
    status      = c("filled", "filled", "pending",
                    "filled", "filled", "pending"),
    reason      = c(NA, NA, "the column is not on the output's column axis",
                    NA, NA, "the column is not on the output's column axis"),
    stringsAsFactors = FALSE
  )
}

test_that("ars_fill_summary rolls up sheets, columns and reasons", {
  s <- ars_fill_summary(.debrief_census())

  expect_equal(s$sheets$sheet, "Table X")
  expect_equal(s$sheets$cells, 6L)
  expect_equal(s$sheets$filled, 4L)
  expect_equal(s$sheets$pending, 2L)

  ## The lost column is named, with its modal reason.
  total <- s$columns[s$columns$col_label == "Total", ]
  expect_equal(total$cells, 2L)
  expect_equal(total$filled, 0L)
  expect_match(total$modal_reason, "not on the output's column axis")
  drug <- s$columns[s$columns$col_label == "Drug A", ]
  expect_equal(drug$filled, 2L)

  ## The reason histogram carries the hint alongside the count.
  expect_equal(nrow(s$reasons), 1)
  expect_equal(s$reasons$n_cells, 2L)
  expect_false(is.na(s$reasons$hint))
})

test_that("ars_fill_summary of an empty census does not divide by zero", {
  s <- ars_fill_summary(.fill_census(list()))
  expect_equal(nrow(s$sheets), 0)
  expect_equal(nrow(s$columns), 0)
  expect_equal(nrow(s$reasons), 0)
})


## --- the hints ------------------------------------------------------------------

test_that("every known pending reason carries a hint", {
  reasons <- c(
    ## Build-time (the cell map).
    "a template row of the categorical block above -- awaiting row expansion",
    "the column is not on the output's column axis",
    "no analysis covers this row",
    "the method declares no statistic for this placeholder",
    ## Fill-time, fixed cells.
    "not bound to an analysis",
    "the cell is not in the workbook",
    "the cell holds no text to replace",
    "reserved for manual derivation",
    "the placeholder asks for a statistic the analysis does not produce",
    "the analysis produced no value for this cell",
    "the row stands for a repeated block, which needs row expansion",
    "no result in the ARD for this cell",
    "the placeholder text could not be located in the cell",
    ## Header N.
    "no result in this column is shown as a percentage",
    "no analysis in this column reports a denominator",
    "the column's analyses report different denominators 84 / 86",
    "the (N=xx) placeholder could not be located in the cell",
    ## Expansions.
    "the nested block's analyses produced no levels",
    "the categorical block's analysis produced no levels",
    "the sheet carries formulas, which cannot be shifted",
    paste("the block has fewer observed levels than authored template",
          "rows; the leftover row was cleared"),
    ## Listings.
    "the shell has no template row to expand",
    "the listing's rows could not be read from the data",
    "the listing selected no rows",
    "the template row is not in the workbook",
    ## Figures.
    "the shell states no annotation block to write the series into",
    "no ADaM directory was given, so the series could not be computed",
    "the shell does not name both an x axis and a y axis variable",
    "dataset ADXX is not in the ADaM directory",
    "the figure's filter could not be parsed",
    "ADVS does not have PARAMCDX",
    "the figure's filter selected no records",
    ## Output level.
    "no cell map recorded for this output",
    "the output names sheet 'Table 9', which is not in the workbook"
  )
  for (reason in reasons) {
    hint <- .fill_reason_hint(reason)
    expect_true(!is.na(hint) && nzchar(hint), info = reason)
  }
})

test_that("an unknown reason gets no invented hint", {
  expect_true(is.na(.fill_reason_hint("something never seen before")))
  expect_true(is.na(.fill_reason_hint(NA_character_)))
})


## --- ars_fill_shell returns the census and its own findings --------------------

test_that("the fill returns a full census and the run's findings", {
  td <- withr::local_tempdir()

  vars <- data.frame(
    Dataset   = rep("ADSL", 3),
    Variable  = c("USUBJID", "TRT01A", "SAFFL"),
    Label     = c("Subject", "Treatment", "Safety Flag"),
    Type      = c("Char", "Char", "Char"),
    Origin    = "Derived", Codelist = "", Length = "40", Mandatory = "Req",
    stringsAsFactors = FALSE)
  spec <- file.path(td, "spec.xlsx")
  wb <- openxlsx2::wb_workbook() |>
    openxlsx2::wb_add_worksheet("Variables") |>
    openxlsx2::wb_add_data(sheet = "Variables", x = vars)
  openxlsx2::wb_save(wb, spec, overwrite = TRUE)

  adam <- file.path(td, "adam")
  dir.create(adam)
  utils::write.csv(
    data.frame(USUBJID = sprintf("S%02d", 1:6),
               TRT01A = rep(c("Drug A", "Placebo"), each = 3),
               SAFFL = "Y", stringsAsFactors = FALSE),
    file.path(adam, "ADSL.csv"), row.names = FALSE)

  wb <- openxlsx2::wb_workbook()$add_worksheet("Table 1")
  put <- function(x, row, col = 1L) {
    wb$add_data(sheet = "Table 1", x = x, start_row = row, start_col = col,
                col_names = FALSE)
  }
  black <- openxlsx2::wb_color(hex = "FF000000")
  red   <- openxlsx2::wb_color(hex = "FFC00000")
  ann <- function(label, annotation) {
    openxlsx2::fmt_txt(label, color = black, size = 10) +
      openxlsx2::fmt_txt(paste0("\n", annotation), color = red, size = 8,
                         italic = TRUE)
  }
  put("Table 1", 1)
  put("Disposition", 2)
  put(ann("Safety Population ", "(ADSL.SAFFL='Y')"), 3)
  for (r in 1:3) {
    wb$merge_cells(sheet = "Table 1", dims = sprintf("A%d:D%d", r, r))
  }
  put(ann("Item", "[columns -> ADSL.TRT01A; source ADSL]"), 4)
  put("Drug A", 4, 2L)
  put("Placebo", 4, 3L)
  ## A third displayed column no data answers: its cells stay pending, so
  ## the fill's own column-coverage WARN fires -- the findings must carry it.
  put("Drug X", 4, 4L)
  put(ann("Subjects in population, n", "[ADSL.USUBJID]"), 5)
  for (j in 2:4) put("xx", 5, j)
  shell <- file.path(td, "shell.xlsx")
  wb$save(shell)

  ars <- file.path(td, "re.json")
  withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(suppressWarnings(spec_to_ars(
      shell_path = shell, adam_spec_path = spec, api_key = "",
      output_path = ars, report_path = file.path(td, "rep.xlsx"),
      verbose = FALSE))))
  ard <- suppressMessages(suppressWarnings(ars_to_ard(ars, adam)))
  res <- suppressMessages(suppressWarnings(ars_fill_shell(
    shell_path = shell, ars = ars, ard = ard,
    output_path = file.path(td, "filled.xlsx"), adam_dir = adam,
    overwrite = TRUE)))

  ## The census holds the FILLED cells too, with position and column label.
  expect_null(res$diagnostics)
  filled <- res$census[res$census$status == "filled", , drop = FALSE]
  expect_gte(nrow(filled), 2)
  expect_true(all(!is.na(filled$row)))
  expect_true(all(!is.na(filled$col)))
  expect_true("Drug A" %in% filled$col_label)
  expect_true(all(!is.na(filled$analysis_id)))

  ## The findings frame carries the fill stage's own WARN about the lost
  ## column -- the diagnostics a CLI caller used to lose on exit.
  expect_s3_class(res$findings, "data.frame")
  expect_true(any(res$findings$severity == "WARN" &
                    grepl("kept its placeholder", res$findings$problem)))
})
