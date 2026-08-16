## A-08: a statistic line that names several statistics at once.
##
## General defect class: the map from a shell's statistic-line label to the
## statistics it shows held only lines naming ONE statistic, or a centre and
## its spread in the two spellings arsbridge itself writes. A line combining a
## centre with an interval -- the standard clinical "Median (Q1, Q3)" --
## matched nothing, so the cell was left on its placeholder and the census
## reported "no result in the ARD for this cell". That reason was untrue: the
## results were in the ARD, and nothing could name them.
##
## General invariant: the label map reads what an AUTHOR wrote, so it must
## cover the lines authors write, whether or not arsbridge ever writes them
## itself. A line naming several statistics is filled from the same results as
## the lines naming them separately.
##
## This is deliberately only the lexicon. The adjacent case of a bare "n" line
## on a continuous block, where the map yields "n" and the ARD carries "N", is
## a different mismatch and is left alone here.

.sll_vocabs <- list(
  first  = list(ds = "ADQX", arm = "TRTP", num = "AGE",
                a = "Drug A", b = "Placebo", sheet = "Table 14.1.2"),
  second = list(ds = "ADZZ", arm = "ZZGRP", num = "ZZVAL",
                a = "Kappa", b = "Lambda", sheet = "Table 9.4.4")
)

## A continuous block: a parent row carrying the variable, over statistic
## lines. Four lines the map already knew, plus the combined one.
.sll_LINES <- list(
  list(label = "Mean (SD)",       placeholder = "xx.x (xx.xx)"),
  list(label = "Median",          placeholder = "xx.x"),
  list(label = "Q1, Q3",          placeholder = "xx.x, xx.x"),
  list(label = "Median (Q1, Q3)", placeholder = "xx.x (xx.x, xx.x)"),
  list(label = "Min, Max",        placeholder = "xx, xx")
)

.sll_build <- function(vocab, dir) {
  sw <- openxlsx2::wb_workbook()$add_worksheet("Variables")
  sw$add_data(sheet = "Variables", x = data.frame(
    Dataset  = c("ADSL", "ADSL", rep(vocab$ds, 3)),
    Variable = c("USUBJID", vocab$arm, "USUBJID", vocab$arm, vocab$num),
    Label    = c("Subject", "Treatment", "Subject", "Treatment", "Measure"),
    Type     = c("Char", "Char", "Char", "Char", "Num"),
    Origin = "Derived", Codelist = "", Length = "40", Mandatory = "Req",
    stringsAsFactors = FALSE))
  spec <- file.path(dir, "spec.xlsx")
  sw$save(spec)

  adam <- file.path(dir, "adam")
  dir.create(adam, showWarnings = FALSE)
  ## Twelve subjects, six per arm, deliberately unequal so a median and its
  ## quartiles are distinct numbers rather than coincidentally equal.
  subjects <- data.frame(
    USUBJID = sprintf("S%02d", 1:12),
    ARM = rep(c(vocab$a, vocab$b), each = 6),
    VAL = c(41, 52, 63, 47, 58, 69, 44, 55, 66, 50, 61, 72),
    stringsAsFactors = FALSE)
  names(subjects) <- c("USUBJID", vocab$arm, vocab$num)
  utils::write.csv(subjects, file.path(adam, paste0(vocab$ds, ".csv")),
                   row.names = FALSE)
  utils::write.csv(subjects, file.path(adam, "ADSL.csv"), row.names = FALSE)

  black <- openxlsx2::wb_color(hex = "FF000000")
  red   <- openxlsx2::wb_color(hex = "FFC00000")
  ann <- function(l, a) {
    openxlsx2::fmt_txt(l, color = black, size = 10) +
      openxlsx2::fmt_txt(paste0("\n", a), color = red, size = 8, italic = TRUE)
  }

  wb <- openxlsx2::wb_workbook()$add_worksheet(vocab$sheet)
  put <- function(x, row, col = 1L) {
    wb$add_data(sheet = vocab$sheet, x = x, start_row = row, start_col = col,
                col_names = FALSE)
  }
  put(vocab$sheet, 1)
  put("Continuous summary", 2)
  put("Item", 3)
  put(ann(sprintf("%s (N=XX)", vocab$a),
          sprintf("%s.%s='%s'", vocab$ds, vocab$arm, vocab$a)), 3, 2L)
  put(ann(sprintf("%s (N=XX)", vocab$b),
          sprintf("%s.%s='%s'", vocab$ds, vocab$arm, vocab$b)), 3, 3L)
  put(ann("Measure (units)", sprintf("[%s.%s]", vocab$ds, vocab$num)), 4)
  for (i in seq_along(.sll_LINES)) {
    put(.sll_LINES[[i]]$label, 4L + i)
    for (j in 2:3) put(.sll_LINES[[i]]$placeholder, 4L + i, j)
  }
  shell <- file.path(dir, "shell.xlsx")
  wb$save(shell)
  list(shell = shell, spec = spec, adam = adam, sheet = vocab$sheet)
}

.sll_fill <- function(paths, dir) {
  ars <- file.path(dir, "ars.json")
  withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(suppressWarnings(spec_to_ars(
      shell_path = paths$shell, adam_spec_path = paths$spec, api_key = "",
      use_llm = FALSE, verbose = FALSE, output_path = ars,
      report_path = file.path(dir, "report.xlsx"), emit_code = FALSE))))
  ard <- suppressMessages(suppressWarnings(ars_to_ard(ars, paths$adam)))
  filled <- file.path(dir, "filled.xlsx")
  res <- suppressMessages(suppressWarnings(ars_fill_shell(
    shell_path = paths$shell, ars = ars, ard = ard, output_path = filled,
    adam_dir = paths$adam, overwrite = TRUE)))
  book <- openxlsx2::wb_to_df(openxlsx2::wb_load(filled), sheet = paths$sheet,
                              col_names = FALSE)
  list(census = res$census, ard = ard,
       cell = function(row, col) as.character(book[row, col]))
}

## The numbers a filled cell shows, in the order it shows them.
.sll_nums <- function(text) {
  regmatches(text, gregexpr("[0-9]+\\.?[0-9]*", text))[[1]]
}


test_that("a combined statistic line names all of its statistics", {
  ## The measured case: the line the register recorded as leaving 13 rows
  ## blank across two tables while the ARD held their results.
  expect_equal(.stats_for_line("Median (Q1, Q3)"), c("median", "p25", "p75"))

  ## The other combined forms the map carries. Each is asserted rather than
  ## assumed: an entry no test names is an entry nobody can rely on, and the
  ## reversed orders exist because every other pair in the map carries both.
  expect_equal(.stats_for_line("Median (Q3, Q1)"), c("median", "p75", "p25"))
  expect_equal(.stats_for_line("Median (Min, Max)"), c("median", "min", "max"))
  expect_equal(.stats_for_line("Mean (Min, Max)"), c("mean", "min", "max"))

  ## Every combined line resolves to statistics the separate lines already
  ## resolve to, so none of them invents a statistic the engine cannot produce.
  known <- unlist(lapply(c("Mean (SD)", "Median", "Q1, Q3", "Min, Max"),
                         .stats_for_line))
  for (line in c("Median (Q1, Q3)", "Median (Q3, Q1)",
                 "Median (Min, Max)", "Mean (Min, Max)")) {
    expect_true(all(.stats_for_line(line) %in% known), info = line)
  }

  ## The lines that already worked are untouched, and a label that is not a
  ## statistic line at all is still read as a level of the parent.
  expect_equal(.stats_for_line("Median"), "median")
  expect_equal(.stats_for_line("Q1, Q3"), c("p25", "p75"))
  expect_equal(.stats_for_line("Mean (SD)"), c("mean", "sd"))
  expect_equal(.stats_for_line("Min, Max"), c("min", "max"))
  expect_null(.stats_for_line("Female"))
  expect_null(.stats_for_line("Median of prior therapies"))
})


test_that("statistic lines still round-trip through the renderer's mapping", {
  ## The two maps must still agree wherever BOTH can express a line. They are
  ## not inverses everywhere and need not be: this map reads what an author
  ## wrote, and an author may put several statistics on one line where
  ## the renderer, laying out its own table, gives each its own.
  for (line in c("Mean (SD)", "Median")) {
    stats <- .stats_for_line(line)
    expect_false(is.null(stats), info = line)
    for (sn in stats) expect_equal(.statline_for(sn), line, info = line)
  }
  ## And the combined line's statistics are exactly the union of the separate
  ## lines the renderer does write, so nothing new was invented for it.
  expect_setequal(
    .stats_for_line("Median (Q1, Q3)"),
    c(.stats_for_line("Median"), .stats_for_line("Q1, Q3"))
  )
})


test_that("a combined statistic line fills, and agrees with the separate lines", {
  skip_if_not_installed("openxlsx2")
  checked <- 0L

  for (vname in names(.sll_vocabs)) {
    vocab <- .sll_vocabs[[vname]]
    dir <- withr::local_tempdir()
    run <- .sll_fill(.sll_build(vocab, dir), dir)

    ## Non-vacuity: the ARD really holds the three statistics. Without this the
    ## fill assertions could pass on an engine that computed nothing and a
    ## census that had stopped reporting.
    for (sn in c("median", "p25", "p75")) {
      expect_true(sn %in% as.character(run$ard$stat_name), info = sn)
    }

    ## Every statistic line fills -- the combined one, and the four that
    ## already did. A change that fixed one by breaking another would show up
    ## here rather than in review.
    for (i in seq_along(.sll_LINES)) {
      label <- .sll_LINES[[i]]$label
      rows <- run$census[!is.na(run$census$row_label) &
                           run$census$row_label == label, , drop = FALSE]
      expect_equal(nrow(rows), 2L, info = label)
      expect_true(all(rows$status == "filled"), info = label)
    }

    ## And the numbers are right. Asserted against the values the engine puts
    ## on the separate Median and Q1, Q3 lines rather than against arithmetic
    ## written here: a transcribed expectation can drift from what the code
    ## does, and this pins the combined cell to the engine's own answer, in the
    ## engine's own order.
    for (col in 2:3) {
      median_line   <- run$cell(6L, col)
      quartile_line <- run$cell(7L, col)
      combined_line <- run$cell(8L, col)

      ## Non-vacuity for the comparison itself: three numbers, not an empty
      ## cell compared with another empty cell.
      expect_length(.sll_nums(median_line), 1L)
      expect_length(.sll_nums(quartile_line), 2L)
      expect_length(.sll_nums(combined_line), 3L)

      expect_equal(
        .sll_nums(combined_line),
        c(.sll_nums(median_line), .sll_nums(quartile_line)),
        info = paste(vname, "column", col)
      )
      ## The placeholder is gone: no "xx" survives into a filled cell.
      expect_false(grepl("x", combined_line, ignore.case = TRUE))
    }

    checked <- checked + 1L
  }

  ## Both vocabularies ran. They share no dataset, variable or level name, so
  ## the lexicon cannot be keying on anything a study happened to be called.
  expect_equal(checked, length(.sll_vocabs))
})
