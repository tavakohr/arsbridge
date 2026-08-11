## End-to-end regression for a two-slot bare-USUBJID row. The inferred
## Subject Count and Percentage method must calculate and fill within each arm.

.subject_pct_annotated_cell <- function(label, annotation) {
  black <- openxlsx2::wb_color(hex = "FF000000")
  red <- openxlsx2::wb_color(hex = "FFC00000")

  openxlsx2::fmt_txt(label, color = black, size = 10) +
    openxlsx2::fmt_txt(
      paste0("\n", annotation),
      color = red,
      size = 8,
      italic = TRUE
    )
}

.subject_pct_shell <- function(td) {
  sheet <- "Table 14.1.1"
  wb <- openxlsx2::wb_workbook()$add_worksheet(sheet)
  put <- function(value, row, col = 1L) {
    wb$add_data(
      sheet = sheet,
      x = value,
      start_row = row,
      start_col = col,
      col_names = FALSE
    )
  }

  put("Table 14.1.1", 1)
  put("Subjects in the Safety Population", 2)
  put(.subject_pct_annotated_cell(
    "Safety Population ",
    "(ADSL.SAFFL='Y')"
  ), 3)
  for (row in 1:3) {
    wb$merge_cells(sheet = sheet, dims = sprintf("A%d:D%d", row, row))
  }

  put("Disposition", 4)
  put(.subject_pct_annotated_cell("Arm A", "[ADSL.TRT01A='A']"), 4, 2)
  put(.subject_pct_annotated_cell("Arm B", "[ADSL.TRT01A='B']"), 4, 3)
  put(.subject_pct_annotated_cell(
    "Total",
    "[ADSL.TRT01A IN ('A','B')]"
  ), 4, 4)
  put(.subject_pct_annotated_cell(
    "Subjects, n (%)",
    "[ADSL.USUBJID WHERE ADSL.SAFFL='Y'; count of unique USUBJID]"
  ), 5)
  put("xx (xx.x)", 5, 2)
  put("xx (xx.x)", 5, 3)
  put("xx (xx.x)", 5, 4)

  path <- file.path(td, "subject_count_shell.xlsx")
  wb$save(path)
  path
}

.subject_pct_adam_spec <- function(td) {
  variables <- data.frame(
    Dataset = rep("ADSL", 3),
    Variable = c("USUBJID", "SAFFL", "TRT01A"),
    Label = c(
      "Unique Subject Identifier",
      "Safety Population Flag",
      "Actual Treatment"
    ),
    Type = rep("Char", 3),
    Origin = rep("Derived", 3),
    Codelist = rep("", 3),
    Length = c("40", "1", "20"),
    Mandatory = rep("Req", 3),
    stringsAsFactors = FALSE
  )

  path <- file.path(td, "subject_count_adam_spec.xlsx")
  wb <- openxlsx2::wb_workbook() |>
    openxlsx2::wb_add_worksheet("Variables") |>
    openxlsx2::wb_add_data(sheet = "Variables", x = variables)
  openxlsx2::wb_save(wb, file = path, overwrite = TRUE)
  path
}

.subject_pct_data <- function(td) {
  adam_dir <- file.path(td, "adam")
  dir.create(adam_dir)
  utils::write.csv(data.frame(
    USUBJID = sprintf("SUBJ-%02d", 1:10),
    SAFFL = "Y",
    TRT01A = c(rep("A", 6), rep("B", 4)),
    stringsAsFactors = FALSE
  ), file.path(adam_dir, "ADSL.csv"), row.names = FALSE)
  adam_dir
}

test_that("two-slot subject count fills counts and arm percentages end to end", {
  skip_if_not_installed("openxlsx2")
  skip_if_not_installed("cards")

  td <- withr::local_tempdir()
  shell_path <- .subject_pct_shell(td)
  adam_spec_path <- .subject_pct_adam_spec(td)
  adam_dir <- .subject_pct_data(td)
  ars_path <- file.path(td, "reporting_event.json")

  withr::with_envvar(
    c(
      ANTHROPIC_API_KEY = "",
      OPENAI_API_KEY = "",
      GEMINI_API_KEY = "",
      GLM_API_KEY = "",
      ARS_LLM_PROVIDER = ""
    ),
    suppressMessages(suppressWarnings(spec_to_ars(
      shell_path = shell_path,
      adam_spec_path = adam_spec_path,
      api_key = "",
      output_path = ars_path,
      report_path = file.path(td, "validation_report.xlsx"),
      verbose = FALSE
    )))
  )

  ars <- jsonlite::fromJSON(ars_path, simplifyVector = FALSE)
  expect_equal(ars$analyses[[1]]$methodId, "MTH_SUBJECT_COUNT_PCT")

  ard <- suppressWarnings(ars_to_ard(ars_path, adam_dir))
  filled_path <- file.path(td, "filled.xlsx")
  fill <- suppressWarnings(ars_fill_shell(
    shell_path = shell_path,
    ars = ars_path,
    ard = ard,
    output_path = filled_path,
    overwrite = TRUE
  ))

  expect_equal(fill$pending, 0)
  expect_equal(fill$filled, 3)

  filled <- openxlsx2::wb_to_df(
    openxlsx2::wb_load(filled_path),
    sheet = "Table 14.1.1",
    col_names = FALSE
  )
  expect_equal(as.character(filled[[2]][5]), "6 (100.0)")
  expect_equal(as.character(filled[[3]][5]), "4 (100.0)")
  expect_equal(as.character(filled[[4]][5]), "10 (100.0)")
})
