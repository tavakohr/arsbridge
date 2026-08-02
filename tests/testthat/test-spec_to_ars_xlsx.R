## spec_to_ars() end to end from an EXCEL shell.
##
## The parity test proves the two readers agree; this proves the Excel path
## actually reaches the end of the pipeline -- a conformant ARS reporting
## event, with the analyses the shell asked for, from a workbook.

apx_shell <- function() test_path("fixtures", "shells_apx_drm_301.xlsx")
apx_spec  <- function() test_path("fixtures", "adam_spec_apx_drm_301.xlsx")

build_apx <- function(shell = apx_shell(), ...) {
  out <- withr::local_tempfile(fileext = ".json", .local_envir = parent.frame())
  res <- withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(suppressWarnings(spec_to_ars(
      shell_path     = shell,
      adam_spec_path = apx_spec(),
      api_key        = "",
      output_path    = out,
      report_path    = withr::local_tempfile(fileext = ".xlsx",
                                             .local_envir = parent.frame()),
      verbose        = FALSE,
      ...))))
  list(res = res, path = out, ars = jsonlite::fromJSON(out,
                                                       simplifyVector = FALSE))
}

## ---------------------------------------------------------------------------
## The gate
## ---------------------------------------------------------------------------

test_that("an .xlsx shell is accepted", {
  ## The gate this PR lifted: until now this aborted on the extension.
  expect_no_error(build_apx())
})

test_that("a shell in neither format is still refused, by name", {
  bad <- withr::local_tempfile(fileext = ".pdf")
  writeLines("not a shell", bad)
  expect_error(
    spec_to_ars(shell_path = bad, adam_spec_path = apx_spec(),
                api_key = "sk-test"),
    "\\.docx.*\\.xlsx")
})

test_that("one file given as both shell and spec is refused", {
  ## Both inputs can be .xlsx now, so the extension no longer tells them
  ## apart. Without this guard the spec parses as a shell and the run ends
  ## with an unexplained empty result.
  expect_error(
    spec_to_ars(shell_path = apx_spec(), adam_spec_path = apx_spec(),
                api_key = "sk-test"),
    "same file")
})

## ---------------------------------------------------------------------------
## The reporting event
## ---------------------------------------------------------------------------

test_that("an Excel shell produces a conformant ARS reporting event", {
  built <- build_apx()
  findings <- ars_conformance(built$path)
  expect_equal(nrow(findings[findings$severity == "FAIL", , drop = FALSE]), 0L)
})

test_that("every output in the workbook reaches the reporting event", {
  built <- build_apx()
  ## Eight sheets are outputs; the ninth is the workbook's legend and must
  ## not become one.
  expect_equal(built$res$n_tlfs, 8L)
  expect_length(built$ars$outputs, 8L)
  expect_true(built$res$n_analyses > 0)
})

test_that("the outputs are named and titled from the workbook", {
  built <- build_apx()
  outputs <- built$ars$outputs
  ids <- vapply(outputs, function(o) o$id, character(1))
  expect_true(all(c("T_14_1_1", "L_16_2_1", "F_14_3_1") %in% ids))

  disposition <- outputs[[which(ids == "T_14_1_1")]]
  expect_equal(disposition$name, "T-14-1-1")
  expect_equal(disposition$label, "Summary of Subject Disposition")
  expect_equal(disposition$outputType, "TABLE")

  ## The type came off each sheet's own number.
  types <- vapply(outputs, function(o) o$outputType %||% "", character(1))
  expect_equal(types[[which(ids == "L_16_2_1")]], "LISTING")
  expect_equal(types[[which(ids == "F_14_3_1")]], "FIGURE")
})

test_that("the treatment axis stated in the stub header becomes a grouping", {
  ## "[columns -> ADSL.TRT01A]" is the Excel convention for the column axis.
  ## If it did not resolve, every table would lose its treatment columns.
  built <- build_apx()
  vars <- vapply(built$ars$analysisGroupings %||% list(), function(g) {
    paste0(g$groupingDataset %||% "", ".", g$groupingVariable %||% "")
  }, character(1))
  expect_true("ADSL.TRT01A" %in% vars)
})

test_that("a nested Excel header becomes two grouping axes", {
  ## Table 14.2.1's merged "Drug 10 mg" header over conditioned "Week 12" /
  ## "Week 24" sub-columns -- the hierarchy has to survive all the way into
  ## the reporting event, not just into the section object.
  built <- build_apx()
  vars <- vapply(built$ars$analysisGroupings %||% list(), function(g) {
    paste0(g$groupingDataset %||% "", ".", g$groupingVariable %||% "")
  }, character(1))
  expect_true(all(c("ADEX.TRT01AN", "ADEX.AVISITN") %in% vars))
})

test_that("the analyses reference variables the shell annotated", {
  built <- build_apx()
  txt <- jsonlite::toJSON(built$ars, auto_unbox = TRUE)
  for (v in c("EOSSTT", "SAFFL", "AESOC", "AEDECOD")) {
    expect_match(as.character(txt), v, info = v)
  }
})

test_that("the run reports which shell it read", {
  built <- build_apx()
  expect_match(built$res$ars_path, "\\.json$")
  expect_equal(built$res$extraction_mode, "deterministic")
})

## ---------------------------------------------------------------------------
## Diagnostics survive the format change
## ---------------------------------------------------------------------------

test_that("shell diagnostics name the shell in a format-neutral way", {
  ## INPUT_SHELL used to read "annotated shell (.docx)" -- pointing an Excel
  ## user at a Word file.
  built <- build_apx()
  d <- ars_diagnostics()
  shell_rows <- d[d$stage == "parse_shell", , drop = FALSE]
  if (nrow(shell_rows) > 0) {
    expect_false(any(grepl("^annotated shell \\(\\.docx\\)$",
                           shell_rows$input)))
  }
  expect_match(INPUT_SHELL, "\\.docx.*\\.xlsx")
})

test_that("the deliberate authoring mistakes in the fixture are reported", {
  ## The workbook carries a tab name that disagrees with its own first row.
  ## An end-to-end run must surface that, not absorb it.
  build_apx()
  d <- ars_diagnostics()
  expect_true(any(grepl("sheet name was used as the output number", d$problem)))
})

## ---------------------------------------------------------------------------
## Deliverables
## ---------------------------------------------------------------------------

test_that("per-TLF code is emitted for an Excel shell like any other", {
  built <- build_apx()
  expect_true(length(built$res$code_paths %||% character()) > 0)
  expect_true(all(file.exists(built$res$code_paths)))
})

test_that("the validation report is written", {
  built <- build_apx()
  expect_true(file.exists(built$res$report_path))
})

## ---------------------------------------------------------------------------
## The second header dialect: (N=XX) decorations and a label-less stub header
## ---------------------------------------------------------------------------
## shells_apx_nheaders.xlsx recreates, with invented content, the conventions
## that broke the first field workbook. Table 14.1.5 combines all of them;
## Table 14.1.6 carries only the (N=XX) decoration -- so when one of these
## tests fails, the name says which fix regressed.

test_that("(N=XX) arm headers reach the fill map stripped, at true columns", {
  b <- build_apx(shell = test_path("fixtures", "shells_apx_nheaders.xlsx"))
  hit <- Filter(function(o) identical(o$id, "T_14_1_6"), b$ars$outputs)
  expect_length(hit, 1L)
  cols <- hit[[1]]$`_meta`$shell_fill$columns
  expect_equal(vapply(cols, function(c) c$label, character(1)),
               c("Placebo", "Drug 10 mg", "Drug 20 mg"))
  expect_equal(vapply(cols, function(c) c$col, integer(1)), 2:4)
})

test_that("a stub header holding only the directive does not shift columns", {
  ## The stub header cell is red directive text alone; once the annotation is
  ## lifted, its label is blank. The compacted header vector then starts at
  ## "Placebo", and a map built from it would put Placebo's values under the
  ## stub -- every number one column left of its header.
  b <- build_apx(shell = test_path("fixtures", "shells_apx_nheaders.xlsx"))
  hit <- Filter(function(o) identical(o$id, "T_14_1_5"), b$ars$outputs)
  expect_length(hit, 1L)
  cols <- hit[[1]]$`_meta`$shell_fill$columns
  expect_equal(vapply(cols, function(c) c$col, integer(1)), 2:4)
  expect_equal(vapply(cols, function(c) c$label, character(1)),
               c("Placebo", "Drug 10 mg", "Drug 20 mg"))
})

test_that("a stub label's [a] footnote marker does not block its analysis", {
  b <- build_apx(shell = test_path("fixtures", "shells_apx_nheaders.xlsx"))
  labels <- vapply(b$ars$analyses, function(a) a$label %||% "", character(1))
  expect_true(any(grepl("^Discontinued study$", labels)))
})
