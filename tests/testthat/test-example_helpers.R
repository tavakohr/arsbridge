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
  expect_length(secs, 8L)
  expect_true(all(vapply(secs, function(s) nzchar(s$tlf_number %||% ""),
                         logical(1))))
  ## At least the tables carry machine-readable annotations.
  n_annot <- sum(vapply(secs, function(s) {
    sum(vapply(s$stub_rows %||% list(),
               function(r) isTRUE(r$has_annot), logical(1)))
  }, integer(1)))
  expect_gt(n_annot, 3)
})

test_that("the two bundled shells are the same study, output for output", {
  ## The drift this catches: the Excel shell was hand-authored with four of
  ## the study's eight outputs while the Word one had all eight, and nothing
  ## compared them -- the bundle advertised "one worksheet per output" and
  ## was not.
  ##
  ## Compared at the OUTPUT level, not cell for cell: the Excel shell is a
  ## transcription of the same study, not a facsimile of the Word document
  ## (it leaves out analyses this study's public data cannot support). The
  ## reader-level lockstep -- every class-1 field, every annotated row -- is
  ## test-parity_docx_xlsx.R, on fixtures built to be identical.
  skip_if_not_installed("openxlsx2")
  spec <- parse_adam_spec(arsbridge_example("adam_spec.xlsx"))
  read <- function(file) {
    suppressMessages(suppressWarnings(parse_shell(
      arsbridge_example(file), spec_lookup = spec$lookup)))
  }
  docx <- read("annotated_shell.docx")
  xlsx <- read("annotated_shell.xlsx")

  for (field in c("tlf_number", "tlf_type", "title")) {
    expect_identical(
      vapply(docx, function(s) s[[field]] %||% NA_character_, character(1)),
      vapply(xlsx, function(s) s[[field]] %||% NA_character_, character(1)),
      info = field)
  }
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

  ## The reference bundle is what users copy, so it runs clean: not one
  ## warning from any stage. `ars_diagnostics()` would not catch a regression
  ## here -- it holds only the LAST stage's records -- so the assertion is on
  ## the payload, which harvests every stage.
  expect_equal(
    sum(payload$diagnostics$severity %in% c("WARN", "FAIL")), 0L,
    info = paste(utils::head(
      payload$diagnostics$problem[payload$diagnostics$severity %in%
                                    c("WARN", "FAIL")], 5), collapse = " | "))

  book <- xlsx_read_shell_cells(payload$artifacts$filled_workbook)
  cells <- book$sheets[["Table 14.1.1"]]$cells
  cell <- function(ref) cells$text[cells$ref == ref]
  expect_equal(cell("B5"), "86")   # Placebo
  expect_equal(cell("C5"), "96")   # Xanomeline Low Dose
  expect_equal(cell("D5"), "72")   # Xanomeline High Dose

  ## The one arm header the shell decorates with "(N=XX)" is answered with
  ## the same 86 the column's percentages are computed against.
  expect_equal(cell("B4"), "Placebo (N=86)")

  ## The nested block: two authored token rows become the study's own system
  ## organ classes, each followed by its own preferred terms. The tokens
  ## themselves must not survive, and the block must present the SAME rows in
  ## the SAME order as the Word renderer would print from this very ARD --
  ## one study cannot have two answers to "which SOC comes first".
  nested <- book$sheets[["Table 14.3.2"]]$cells
  stub <- nested[nested$col == 1 & nested$row >= 7, , drop = FALSE]
  filled_lines <- stub$text[order(stub$row)]
  filled_lines <- filled_lines[!grepl("^A subject is counted", filled_lines)]
  expect_gt(length(filled_lines), 20)
  expect_false(any(grepl("^<", filled_lines)))

  ## The block counts TREATMENT-EMERGENT events, as its title says. Nothing
  ## propagates the "Subjects with any TEAE" row's filter down the block, so
  ## each token row carries it; without it the table counts every adverse
  ## event under a treatment-emergent title. Nervous system disorders is the
  ## row that shows the difference: 12/22/25 subjects over all AEs, 8/22/23
  ## over treatment-emergent ones. The oracle is the raw dataset.
  adae <- haven::read_xpt(file.path(adam_dir, "adae.xpt"))
  teae <- adae[adae$TRTEMFL == "Y", ]
  soc_row <- nested$row[nested$col == 1 &
                          nested$text == "NERVOUS SYSTEM DISORDERS"]
  expect_length(soc_row, 1L)
  for (arm in list(c("B", "Placebo"), c("C", "Xanomeline Low Dose"),
                   c("D", "Xanomeline High Dose"))) {
    subjects <- unique(teae$USUBJID[teae$AESOC == "NERVOUS SYSTEM DISORDERS" &
                                      teae$TRT01A == arm[[2]]])
    expect_match(
      nested$text[nested$ref == paste0(arm[[1]], soc_row)],
      paste0("^", length(subjects), " \\("), info = arm[[2]])
  }

  ard <- readRDS(payload$artifacts$ard_rds)
  tf <- suppressMessages(suppressWarnings(
    ars_to_tfrmt(payload$artifacts$ars_json, ard, "T_14_3_2")))
  prepped <- .tfrmt_prep_ard_layout(
    ard, "T_14_3_2", attr(tf, "arsbridge_layout"),
    attr(tf, "arsbridge_col_var"), attr(tf, "arsbridge_keep_params"),
    col_levels = attr(tf, "arsbridge_col_levels"),
    fixed_vars = attr(tf, "arsbridge_fixed_vars"),
    params_map = attr(tf, "arsbridge_params_map") %||% list())
  ords <- sort(unique(prepped[[.ARS_SHELL_ORD]]))
  rendered_lines <- vapply(ords, function(o) {
    unique(prepped[[.ARS_SHELL_LBL]][prepped[[.ARS_SHELL_ORD]] == o])[1]
  }, character(1))
  ## The rendered table opens with the authored "Subjects with any TEAE" row;
  ## the block itself is everything after it.
  expect_equal(as.character(filled_lines), as.character(rendered_lines[-1]))

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
