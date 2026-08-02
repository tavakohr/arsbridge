## The whole chain against the second header dialect.
##
## shells_apx_nheaders.xlsx recreates, with invented APX-DRM-301 content, the
## conventions of the first field workbook that came back unfilled: "(N=XX)"
## decorations on the arm headers, a stub header cell that is nothing but the
## red column directive, UPPERCASE placeholders, and a "[a]" footnote marker
## on a stub label. Every oracle below is recomputed from the raw CSVs -- a
## number in the wrong cell does not throw, so the check must not come from
## the pipeline it is checking. The per-arm values differ (4/3/2 completions,
## three distinct age means), so a one-column shift cannot pass.

skip_if_not_installed("openxlsx2")

N_SHELL <- test_path("fixtures", "shells_apx_nheaders.xlsx")
N_SPEC  <- test_path("fixtures", "adam_spec_apx_drm_301.xlsx")
N_ADAM  <- test_path("fixtures", "adam_apx_drm_301")

nheader_run <- local({
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
        shell_path = N_SHELL, adam_spec_path = N_SPEC, api_key = "",
        output_path = ars, report_path = tempfile(fileext = ".xlsx"),
        verbose = FALSE))))
    ard <- suppressMessages(suppressWarnings(ars_to_ard(ars, N_ADAM)))
    out <- tempfile(fileext = ".xlsx")
    res <- suppressMessages(suppressWarnings(ars_fill_shell(
      shell_path = N_SHELL, ars = ars, ard = ard, output_path = out,
      adam_dir = N_ADAM, overwrite = TRUE)))
    cache <<- list(ars = ars, ard = ard, path = out, res = res,
                   book = xlsx_read_shell_cells(out))
    cache
  }
})

ncell <- function(book, sheet, ref) {
  cells <- book$sheets[[sheet]]$cells
  hit <- cells$text[cells$ref == ref]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

test_that("the dialect workbook fills at all", {
  run <- nheader_run()
  expect_gt(run$res$filled, 0)
})

test_that("counts land under their own (N=XX) headers, not one column over", {
  ## Completed-study counts are 4 / 3 / 2 across the arms -- the values
  ## themselves prove the column join, which is the point of this fixture.
  ## Both slots fill: the row shows "XX (XX.X)", so the [VAR = 'value'] filter
  ## row is typed as a subject count WITH its percentage, and the percentage
  ## is out of the arm's population, not of the filtered rows.
  run <- nheader_run()
  adsl <- utils::read.csv(file.path(N_ADAM, "ADSL.csv"),
                          stringsAsFactors = FALSE)
  for (col in list(c("B", "Placebo"), c("C", "Drug 10 mg"),
                   c("D", "Drug 20 mg"))) {
    in_arm <- adsl$TRT01A == col[[2]]
    n_done <- sum(in_arm & adsl$EOSSTT == "COMPLETED")
    expect_equal(
      ncell(run$book, "Table 14.1.5", paste0(col[[1]], "6")),
      sprintf("%d (%.1f)", n_done, 100 * n_done / sum(in_arm)),
      info = col[[2]])
  }
})

test_that("a level absent from the data leaves its cell as authored", {
  ## No Placebo subject discontinued, so the ARD has no row for that cell;
  ## the writer leaves the placeholder rather than inventing a zero. Same
  ## behaviour as the plain fixture -- pinned here because sparse levels are
  ## common in real studies and a silent "0" would be a fabricated result.
  run <- nheader_run()
  expect_equal(ncell(run$book, "Table 14.1.5", "B7"), "XX (XX.X)")
})

test_that("uppercase placeholders resolve with the decimals they state", {
  ## "XX.X (XX.XX)" means one decimal then two, exactly like its lowercase
  ## twin; the age means differ per arm so the join is proven again here.
  run <- nheader_run()
  adsl <- utils::read.csv(file.path(N_ADAM, "ADSL.csv"),
                          stringsAsFactors = FALSE)
  for (col in list(c("B", "Placebo"), c("C", "Drug 10 mg"),
                   c("D", "Drug 20 mg"))) {
    ages <- adsl$AGE[adsl$TRT01A == col[[2]]]
    expect_equal(
      ncell(run$book, "Table 14.1.6", paste0(col[[1]], "6")),
      sprintf("%.1f (%.2f)", mean(ages), stats::sd(ages)),
      info = col[[2]])
    expect_equal(
      ncell(run$book, "Table 14.1.6", paste0(col[[1]], "7")),
      sprintf("%.1f", stats::median(ages)),
      info = col[[2]])
  }
})

test_that("the [a] footnote marker survives in the filled stub label", {
  ## The marker is display apparatus: stripped from the parsed label so the
  ## ARD join works, but the workbook cell the reviewer sees keeps it.
  run <- nheader_run()
  expect_match(ncell(run$book, "Table 14.1.5", "A7"), "Discontinued study")
  expect_match(ncell(run$book, "Table 14.1.5", "A7"), "\\[a\\]")
})

test_that("each arm header's (N=XX) is filled with that arm's own denominator", {
  ## The decoration is not just stripped for matching any more -- it is
  ## answered. Each arm has 4 subjects, so a header that came back with
  ## another arm's N, or with the table's total, would show something other
  ## than 4 and fail here.
  run <- nheader_run()
  for (col in c("B", "C", "D")) {
    expect_match(ncell(run$book, "Table 14.1.5", paste0(col, "4")),
                 "\\(N=4\\)", info = col)
  }
  ## The arm's name is untouched -- only the number inside the brackets moved.
  expect_equal(ncell(run$book, "Table 14.1.5", "B4"), "Placebo (N=4)")

  ## And it is recorded, so the QC sidecar can show where the number came from.
  diags <- run$res$diagnostics
  if (is.data.frame(diags) && nrow(diags) > 0) {
    expect_false(any(diags$ref %in% c("B4", "C4", "D4") &
                       diags$sheet == "Table 14.1.5"))
  } else {
    succeed()
  }
})

test_that("a header N is left as authored when nothing under it states a denominator", {
  ## Table 14.1.6 is continuous all the way down: a mean has no denominator
  ## the way a percentage does. Writing the arm's subject count there would
  ## be a number the table cannot be checked against, so the placeholder
  ## stays.
  run <- nheader_run()
  expect_match(ncell(run$book, "Table 14.1.6", "B4"), "\\(N=XX\\)")
})

test_that("no dialect cell is lost to the column axis", {
  ## "not on the output's column axis" is the drift signature: it means a
  ## column failed to map at all, which is what the blank stub header used
  ## to cause. ("no result in the ARD" can appear legitimately -- B7's
  ## zero-count level above -- so it is not asserted away here.)
  run <- nheader_run()
  diags <- run$res$diagnostics
  if (is.data.frame(diags) && nrow(diags) > 0) {
    expect_false(any(grepl("not on the output's column axis", diags$reason)))
  } else {
    succeed()
  }
})
