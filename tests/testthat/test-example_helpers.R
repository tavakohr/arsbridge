test_that("arsbridge_example() with no args lists the bundle", {
  files <- arsbridge_example()
  expect_true(is.character(files))
  expect_true("annotated_shell.docx" %in% files)
  expect_true("annotated_shell.xlsx" %in% files)
  expect_true("adam_spec.xlsx"       %in% files)
  expect_true("ADaM.zip"             %in% files)
})

test_that("arsbridge_example(name) returns an absolute existing path", {
  p <- arsbridge_example("annotated_shell.docx")
  expect_true(file.exists(p))
  expect_match(p, "annotated_shell\\.docx$")

  p2 <- arsbridge_example("adam_spec.xlsx")
  expect_true(file.exists(p2))

  p3 <- arsbridge_example("ADaM.zip")
  expect_true(file.exists(p3))
  expect_gt(file.info(p3)$size, 100000)   # ~265 KB
})

test_that("arsbridge_example(unknown_file) errors with the available list", {
  expect_error(arsbridge_example("nope.docx"), "not in the bundle")
})

test_that("spec_to_ars_example runs deterministically when no API key is set", {
  ## A missing key no longer aborts: the run degrades to regex + heuristics.
  res <- withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(spec_to_ars_example(
      api_key     = "",
      output_path = tempfile(fileext = ".json"),
      report_path = tempfile(fileext = ".xlsx"),
      verbose     = FALSE
    ))
  )
  expect_equal(res$extraction_mode, "deterministic")
  expect_true(file.exists(res$ars_path))
})

test_that("the bundled Excel shell parses to annotated sections", {
  skip_if_not_installed("openxlsx2")
  spec <- parse_adam_spec(arsbridge_example("adam_spec.xlsx"))
  secs <- suppressMessages(suppressWarnings(parse_shell(
    arsbridge_example("annotated_shell.xlsx"), spec_lookup = spec$lookup)))
  expect_length(secs, 4L)
  expect_true(all(vapply(secs, function(s) nzchar(s$tlf_number %||% ""),
                         logical(1))))
  ## At least the tables carry machine-readable annotations.
  n_annot <- sum(vapply(secs, function(s) {
    sum(vapply(s$stub_rows %||% list(),
               function(r) isTRUE(r$has_annot), logical(1)))
  }, integer(1)))
  expect_gt(n_annot, 3)
})

test_that("the bundle demos the whole chain offline: build, execute, fill", {
  ## The reason the Excel shell exists. spec_to_ars on the bundled xlsx,
  ## ars_to_ard on the bundled data, ars_fill_shell back into the workbook
  ## -- and real numbers land under their own arm headers. The oracle is
  ## pharmaverseadam itself: 86 / 96 / 72 safety subjects per arm.
  skip_on_cran()
  skip_if_not_installed("openxlsx2")

  adam_dir <- withr::local_tempdir()
  utils::unzip(arsbridge_example("ADaM.zip"), exdir = adam_dir)

  payload <- withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(suppressWarnings(ars_workflow_run(
      shell_path     = arsbridge_example("annotated_shell.xlsx"),
      adam_spec_path = arsbridge_example("adam_spec.xlsx"),
      adam_dir       = adam_dir,
      output_dir     = withr::local_tempdir(),
      study_id       = "CDSC-ALZ-201"))))

  expect_true(payload$status %in% c("success", "partial"))
  expect_gt(payload$fill$filled, 0)

  book <- xlsx_read_shell_cells(payload$artifacts$filled_workbook)
  cells <- book$sheets[["Table 14.1.1"]]$cells
  cell <- function(ref) cells$text[cells$ref == ref]
  expect_equal(cell("B5"), "86")   # Placebo
  expect_equal(cell("C5"), "96")   # Xanomeline Low Dose
  expect_equal(cell("D5"), "72")   # Xanomeline High Dose

  ## The bundle is the only place a listing of this size is written -- 1,191
  ## rows, five columns -- and a listing that reads back perfectly in R can
  ## still open in Excel with four of its columns empty. Checked against the
  ## raw XML, where Excel's view is the only view.
  expect_rows_well_formed(payload$artifacts$filled_workbook)
  listing <- book$sheets[["Listing 16.2.7.1"]]
  last <- max(listing$cells$row)
  for (col in c("A", "B", "C", "D", "E")) {
    ref <- paste0(col, last)
    expect_equal(
      xlsx_raw_cell_text(payload$artifacts$filled_workbook,
                         basename(listing$part), ref),
      listing$cells$text[listing$cells$ref == ref], info = ref)
  }
})
