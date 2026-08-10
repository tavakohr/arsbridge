## Regression for the nested SOC/PT Total column: the Total pass must retain
## the parent SOC grouping so every expanded child row has a fillable ARD key.

.nested_total_annotated_cell <- function(label, annotation) {
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

.nested_total_shell <- function(td) {
  sheet <- "Table 14.3.1"
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

  put("Table 14.3.1", 1)
  put("Adverse Events by System Organ Class and Preferred Term", 2)
  put(.nested_total_annotated_cell(
    "Safety Population ",
    "(ADSL.SAFFL='Y')"
  ), 3)
  for (row in 1:3) {
    wb$merge_cells(sheet = sheet, dims = sprintf("A%d:D%d", row, row))
  }

  put("System Organ Class / Preferred Term", 4)
  put(.nested_total_annotated_cell("Cohort A", "[ADSL.COHORTN = 1]"), 4, 2)
  put(.nested_total_annotated_cell("Cohort B", "[ADSL.COHORTN = 2]"), 4, 3)
  put(.nested_total_annotated_cell("Total", "[ADSL.COHORTN IN (1,2)]"), 4, 4)

  put(.nested_total_annotated_cell("<System Organ Class>", "[ADAE.AESOC]"), 5)
  put(.nested_total_annotated_cell("<Preferred Term>", "[ADAE.AEDECOD]"), 6)
  for (row in 5:6) {
    for (col in 2:4) put("xx (xx.x)", row, col)
  }

  path <- file.path(td, "nested_total_shell.xlsx")
  wb$save(path)
  path
}

.nested_total_adam_spec <- function(td) {
  variables <- data.frame(
    Dataset = c(rep("ADSL", 3), rep("ADAE", 4)),
    Variable = c(
      "USUBJID", "SAFFL", "COHORTN",
      "USUBJID", "COHORTN", "AESOC", "AEDECOD"
    ),
    Label = c(
      "Unique Subject Identifier", "Safety Population Flag", "Cohort",
      "Unique Subject Identifier", "Cohort",
      "System Organ Class", "Preferred Term"
    ),
    Type = c("Char", "Char", "Num", "Char", "Num", "Char", "Char"),
    Origin = rep("Derived", 7),
    Codelist = rep("", 7),
    Length = c("40", "1", "8", "40", "8", "200", "200"),
    Mandatory = rep("Req", 7),
    stringsAsFactors = FALSE
  )

  path <- file.path(td, "nested_total_adam_spec.xlsx")
  wb <- openxlsx2::wb_workbook() |>
    openxlsx2::wb_add_worksheet("Variables") |>
    openxlsx2::wb_add_data(sheet = "Variables", x = variables)
  openxlsx2::wb_save(wb, file = path, overwrite = TRUE)
  path
}

.nested_total_data <- function(td) {
  adam_dir <- file.path(td, "adam")
  dir.create(adam_dir)

  adsl <- data.frame(
    USUBJID = sprintf("SUBJ-%02d", 1:8),
    SAFFL = "Y",
    COHORTN = c(1, 1, 1, 1, 1, 2, 2, 2),
    stringsAsFactors = FALSE
  )
  utils::write.csv(adsl, file.path(adam_dir, "ADSL.csv"), row.names = FALSE)

  adae <- data.frame(
    USUBJID = c(
      "SUBJ-01", "SUBJ-02", "SUBJ-06",
      "SUBJ-02", "SUBJ-07",
      "SUBJ-03", "SUBJ-06",
      "SUBJ-04", "SUBJ-05", "SUBJ-08"
    ),
    COHORTN = c(1, 1, 2, 1, 2, 1, 2, 1, 1, 2),
    AESOC = c(
      rep("Respiratory disorders", 5),
      rep("Gastrointestinal disorders", 5)
    ),
    AEDECOD = c(
      "Shared term", "Shared term", "Shared term", "Dyspnoea", "Dyspnoea",
      "Shared term", "Shared term", "Vomiting", "Vomiting", "Vomiting"
    ),
    stringsAsFactors = FALSE
  )
  utils::write.csv(adae, file.path(adam_dir, "ADAE.csv"), row.names = FALSE)
  adam_dir
}

test_that("nested child Total cells fill end to end", {
  skip_if_not_installed("openxlsx2")
  skip_if_not_installed("cards")

  td <- withr::local_tempdir()
  shell_path <- .nested_total_shell(td)
  adam_spec_path <- .nested_total_adam_spec(td)
  adam_dir <- .nested_total_data(td)
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
  expect_equal(fill$filled, 18)

  filled <- openxlsx2::wb_to_df(
    openxlsx2::wb_load(filled_path),
    sheet = "Table 14.3.1",
    col_names = FALSE
  )
  labels <- as.character(filled[[1]])
  totals <- as.character(filled[[4]])
  parent <- NA_character_
  shared_totals <- character(0)
  for (row in seq_along(labels)) {
    if (labels[[row]] %in% c(
      "Respiratory disorders",
      "Gastrointestinal disorders"
    )) {
      parent <- labels[[row]]
    }
    if (identical(labels[[row]], "Shared term")) {
      shared_totals[[parent]] <- totals[[row]]
    }
  }

  expect_equal(shared_totals[["Respiratory disorders"]], "3 (37.5)")
  expect_equal(shared_totals[["Gastrointestinal disorders"]], "2 (25.0)")
})
