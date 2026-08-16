## B6 / A-01: a capability-gated table is still a table.
##
## General defect class: the flag that says "this section's statistics cannot
## be computed" was also used to decide two things it has no bearing on --
## whether the output has a stub column, and whether its rows can be located on
## the sheet. A gated section therefore lost its row-to-analysis map and was
## read as though it had no stub, with two consequences:
##
##   * its stub column was counted as a result column, so the axis was reported
##     one column wider than the shell draws it
##   * nothing could bind its cells, so every one reported "no analysis covers
##     this row" while the event held reserved results for those very analyses
##
## The second is the damaging one. The table came back blank rather than
## visibly reserved, which reads as "not run yet" -- the opposite of what the
## reservation model exists to communicate.
##
## General invariant: whether a section's statistics are computable decides
## what its analyses COMPUTE. It does not decide the output's shape. A table
## keeps its stub and its row map whether or not anything in it can be
## computed.

.gsl_vocabs <- list(
  first  = list(ds = "ADQX", arm = "TRTP", pop = "QXFL",
                a = "Drug A", b = "Placebo"),
  second = list(ds = "ADZZ", arm = "ZZGRP", pop = "ZZANL",
                a = "Kappa", b = "Lambda")
)

## A workbook with two structurally identical tables. Only the closing note
## differs: one names an inferential model, which the keyword capability layer
## gates without needing an LLM. Everything asserted is a difference between
## the two, so the fixture carries its own control.
.gsl_build <- function(vocab, dir) {
  swb <- openxlsx2::wb_workbook()$add_worksheet("Variables")
  swb$add_data(sheet = "Variables", x = data.frame(
    Dataset  = c("ADSL", "ADSL", rep(vocab$ds, 3)),
    Variable = c("USUBJID", vocab$arm, "USUBJID", vocab$arm, vocab$pop),
    Label    = c("Subject", "Treatment", "Subject", "Treatment", "Flag"),
    Type = "Char", Origin = "Derived", Codelist = "", Length = "40",
    Mandatory = "Req", stringsAsFactors = FALSE))
  spec <- file.path(dir, "spec.xlsx")
  swb$save(spec)

  adam <- file.path(dir, "adam")
  dir.create(adam, showWarnings = FALSE)
  subjects <- data.frame(
    USUBJID = sprintf("S%02d", 1:8), ARM = rep(c(vocab$a, vocab$b), each = 4),
    FLAG = "Y", stringsAsFactors = FALSE)
  names(subjects) <- c("USUBJID", vocab$arm, vocab$pop)
  utils::write.csv(subjects, file.path(adam, paste0(vocab$ds, ".csv")),
                   row.names = FALSE)
  utils::write.csv(subjects, file.path(adam, "ADSL.csv"), row.names = FALSE)

  black <- openxlsx2::wb_color(hex = "FF000000")
  red   <- openxlsx2::wb_color(hex = "FFC00000")
  ann <- function(l, a) {
    openxlsx2::fmt_txt(l, color = black, size = 10) +
      openxlsx2::fmt_txt(paste0("\n", a), color = red, size = 8, italic = TRUE)
  }

  wb <- openxlsx2::wb_workbook()
  sheets <- list(
    list(sheet = "Table 14.1.1", note = "Descriptive summary."),
    list(sheet = "Table 14.2.1",
         note = "Treatment comparison uses an ANCOVA model; LS-means presented.")
  )
  for (s in sheets) {
    wb$add_worksheet(s$sheet)
    put <- function(x, row, col = 1L) {
      wb$add_data(sheet = s$sheet, x = x, start_row = row, start_col = col,
                  col_names = FALSE)
    }
    put(s$sheet, 1)
    put("Endpoint table", 2)
    put(ann("Analysed Population ",
            sprintf("(%s.%s='Y')", vocab$ds, vocab$pop)), 3)
    ## Per-column conditions, so the column axis is a FIXED grouping and the
    ## count check is actually reached. A data-driven axis returns early and
    ## the column-count assertion below would be vacuous.
    put("Item", 4)
    put(ann(sprintf("%s (N=XX)", vocab$a),
            sprintf("%s.%s='%s'", vocab$ds, vocab$arm, vocab$a)), 4, 2L)
    put(ann(sprintf("%s (N=XX)", vocab$b),
            sprintf("%s.%s='%s'", vocab$ds, vocab$arm, vocab$b)), 4, 3L)
    put(ann("Subjects, n", sprintf("[%s.USUBJID]", vocab$ds)), 5)
    for (j in 2:3) put("xx", 5, j)
    put(ann("Subjects with flag, n", sprintf("[%s.USUBJID]", vocab$ds)), 6)
    for (j in 2:3) put("xx", 6, j)
    put(s$note, 8)
  }
  shell <- file.path(dir, "shell.xlsx")
  wb$save(shell)
  list(shell = shell, spec = spec, adam = adam)
}

.gsl_run <- function(paths, dir) {
  res <- withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(suppressWarnings(spec_to_ars(
      shell_path = paths$shell, adam_spec_path = paths$spec, api_key = "",
      use_llm = FALSE, verbose = FALSE,
      output_path = file.path(dir, "ars.json"),
      report_path = file.path(dir, "report.xlsx"), emit_code = FALSE))))
  model <- ars_to_model(res$ars_path)

  facts <- lapply(seq_len(nrow(model$outputs)), function(i) {
    node <- model$outputs$raw[[i]]
    cells <- node[["_meta"]][["shell_fill"]][["cells"]] %||% list()
    list(
      id      = model$outputs$id[i],
      gated   = !is.null(node[["_meta"]][["unsupported_reason"]]) ||
        any(vapply(model$analyses$methodId[
          model$analyses$id %in% unlist(node[["referencedAnalysisIds"]] %||% list())
        ], function(m) identical(m, "MTH_UNSUPPORTED_ANALYSIS"), logical(1))),
      layout  = length(node[["_meta"]][["shell_layout"]] %||% list()),
      labels  = .flat_display_labels(node),
      cells   = length(cells),
      bound   = sum(vapply(cells, function(c) !is.null(c[["analysis_id"]]),
                           logical(1)))
    )
  })
  names(facts) <- vapply(facts, function(f) f$id, character(1))
  list(facts = facts, findings = res$ars_validation,
       verdict = res$validation_gate$verdict)
}


test_that("a gated table keeps its stub, its layout and its row bindings", {
  skip_if_not_installed("openxlsx2")
  checked <- 0L

  for (vname in names(.gsl_vocabs)) {
    vocab <- .gsl_vocabs[[vname]]
    td <- withr::local_tempdir()
    run <- .gsl_run(.gsl_build(vocab, td), td)

    expect_length(run$facts, 2L)
    plain <- run$facts[["T_14_1_1"]]
    gated <- run$facts[["T_14_2_1"]]
    expect_false(is.null(plain))
    expect_false(is.null(gated))

    ## Non-vacuity: the fixture really did gate the second table. Without this
    ## the whole test would pass on a build where nothing was gated at all.
    expect_true(gated$gated, info = vname)
    expect_false(plain$gated, info = vname)

    for (out in list(plain, gated)) {
      ## The stub is not a result column, in either table.
      expect_equal(out$labels, c(vocab$a, vocab$b), info = vname)
      ## Every authored row is located on the sheet ...
      expect_gt(out$layout, 0L)
      ## ... so every cell binds to the analysis that fills it.
      expect_gt(out$cells, 0L)
      expect_equal(out$bound, out$cells, info = vname)
    }

    ## And the spurious column-count finding is gone. The gated table's axis
    ## matches its grouping exactly as the plain one's does.
    expect_length(run$findings$ref[grepl("^FLAT_AXIS", run$findings$ref)], 0L)

    checked <- checked + 1L
  }
  expect_equal(checked, length(.gsl_vocabs))
})


test_that("an output with no stub axis still carries no layout", {
  skip_if_not_installed("openxlsx2")
  ## The behaviour the change must NOT lose. A listing's rows are records and a
  ## figure has none, so neither has a row-to-analysis map to keep -- the test
  ## that drops the layout still has to fire for them, or the fill would try to
  ## bind sheet rows that mean nothing.
  vocab <- .gsl_vocabs$first
  td <- withr::local_tempdir()
  paths <- .gsl_build(vocab, td)

  black <- openxlsx2::wb_color(hex = "FF000000")
  red   <- openxlsx2::wb_color(hex = "FFC00000")
  wb <- openxlsx2::wb_load(paths$shell)
  wb$add_worksheet("Listing 16.2.1")
  put <- function(x, row, col = 1L) {
    wb$add_data(sheet = "Listing 16.2.1", x = x, start_row = row,
                start_col = col, col_names = FALSE)
  }
  put("Listing 16.2.1", 1)
  put("Subject listing", 2)
  put(openxlsx2::fmt_txt("Subject", color = black, size = 10) +
        openxlsx2::fmt_txt(sprintf("\n[%s.USUBJID]", vocab$ds),
                           color = red, size = 8, italic = TRUE), 4)
  put(openxlsx2::fmt_txt("Treatment", color = black, size = 10) +
        openxlsx2::fmt_txt(sprintf("\n[%s.%s]", vocab$ds, vocab$arm),
                           color = red, size = 8, italic = TRUE), 4, 2L)
  put("xx", 5); put("xx", 5, 2L)
  wb$save(paths$shell)

  run <- .gsl_run(paths, td)
  listing <- Filter(function(f) grepl("^L_", f$id), run$facts)
  ## Non-vacuity: the listing was actually built, so the assertion below is
  ## about a listing rather than about an empty list.
  expect_length(listing, 1L)
  expect_equal(listing[[1]]$layout, 0L)

  ## And the tables beside it are unaffected.
  tables <- Filter(function(f) grepl("^T_", f$id), run$facts)
  expect_length(tables, 2L)
  for (t in tables) expect_gt(t$layout, 0L)
})


test_that("the bundled study is unchanged by the stub rule", {
  ## Real-study regression. Neither bundled shell has a gated section, so the
  ## point here is that nothing MOVED: the stub is still identified correctly
  ## for every output of a study that was already right, in both formats --
  ## the docx one being the case with no cell map at all.
  skip_if_not_installed("openxlsx2")
  base <- system.file("extdata", "example_cdsc_alz_201", package = "arsbridge")
  skip_if(!nzchar(base))

  td <- withr::local_tempdir()
  checked <- 0L

  for (fmt in c("xlsx", "docx")) {
    res <- withr::with_envvar(
      c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
        GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
      suppressMessages(suppressWarnings(spec_to_ars(
        shell_path = file.path(base, paste0("annotated_shell.", fmt)),
        adam_spec_path = file.path(base, "adam_spec.xlsx"),
        api_key = "", use_llm = FALSE, verbose = FALSE,
        output_path = file.path(td, paste0(fmt, ".json")),
        report_path = file.path(td, paste0(fmt, "-report.xlsx")),
        emit_code = FALSE))))

    model <- ars_to_model(res$ars_path)
    expect_gt(nrow(model$outputs), 0L)

    ## No output gains or loses a result column, and no flat-axis finding
    ## appears. A stub counted as a result column would show up as both.
    for (i in seq_len(nrow(model$outputs))) {
      if (!identical(model$outputs$outputType[i], "TABLE")) next
      labels <- .flat_display_labels(model$outputs$raw[[i]])
      expect_false(any(grepl("^Item$", labels)))
    }
    axis <- res$ars_validation$ref[grepl("^FLAT_AXIS", res$ars_validation$ref)]
    expect_length(axis, 0L)

    checked <- checked + 1L
  }
  expect_equal(checked, 2L)
})
