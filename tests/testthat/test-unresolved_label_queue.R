## S-1: the statistic rows nothing could bind, as data.
##
## General defect class: when a statistic row's label cannot be resolved,
## arsbridge already refuses to bind it -- correctly -- and reports the refusal
## as one WARN per row. A WARN is prose. Everything that has to ACT on those
## rows (a study team working through them, an app listing them for review, a
## generator proposing what they mean) has to re-derive the facts by reading
## sentences, so in practice nothing acts on them at all.
##
## General invariant: every row the build stage refuses is recoverable as
## structured data carrying the same facts the WARN states -- where the row is,
## what it was called, which analysis and method it belonged to, what the label
## asked for, and what the method could have supplied. The two refusal reasons
## stay distinct, because they lead to different work: an unreadable label
## needs somebody to say what it means, an unsupported one cannot be fixed by
## any synonym.
##
## Nothing here changes what binds. A row that bound before binds now.

.ulq_vocabs <- list(
  first  = list(ds = "ADQX", arm = "TRTP", num = "AGE",
                a = "Drug A", b = "Placebo", sheet = "Table 14.1.2"),
  second = list(ds = "ADZZ", arm = "ZZGRP", num = "ZZVAL",
                a = "Kappa", b = "Lambda", sheet = "Table 9.4.4")
)

## Two lines that bind, and three that cannot -- one of each refusal reason,
## plus a second unsupported one so a test cannot pass by finding "the"
## unsupported row.
##
## `Standard Error` and `Coefficient of variation` are read correctly and ask
## for statistics the continuous summary method does not declare.
## `Interquartile spread` is a real thing an author might write and is not a
## spelling the grammar knows, so it is not read at all. Neither list is
## special-cased anywhere in production -- they are ordinary labels.
.ulq_LINES <- list(
  list(label = "Mean (SD)",                placeholder = "xx.x (xx.xx)",
       binds = TRUE),
  list(label = "Median",                   placeholder = "xx.x",
       binds = TRUE),
  list(label = "Standard Error",           placeholder = "xx.xx",
       binds = FALSE, reason = "unsupported", tokens = "se"),
  list(label = "Interquartile spread",     placeholder = "xx.x, xx.x",
       binds = FALSE, reason = "unreadable", tokens = character()),
  list(label = "Coefficient of variation", placeholder = "xx.x",
       binds = FALSE, reason = "unsupported", tokens = "cv")
)

.ulq_build <- function(vocab, dir, lines = .ulq_LINES) {
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
  for (i in seq_along(lines)) {
    put(lines[[i]]$label, 4L + i)
    for (j in 2:3) put(lines[[i]]$placeholder, 4L + i, j)
  }
  shell <- file.path(dir, "shell.xlsx")
  wb$save(shell)
  list(shell = shell, spec = spec, sheet = vocab$sheet,
       ## Which sheet row each line landed on -- asserted, not assumed.
       rows = stats::setNames(4L + seq_along(lines),
                              vapply(lines, function(l) l$label, character(1))))
}

## Returns the run result AND the path, so a test can exercise either shape
## the caller actually has in hand.
.ulq_run <- function(paths, dir) {
  ars <- file.path(dir, "ars.json")
  built <- withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(suppressWarnings(spec_to_ars(
      shell_path = paths$shell, adam_spec_path = paths$spec, api_key = "",
      use_llm = FALSE, verbose = FALSE, output_path = ars,
      report_path = file.path(dir, "report.xlsx"), emit_code = FALSE))))
  list(built = built, path = ars)
}

.ulq_ars <- function(paths, dir) .ulq_run(paths, dir)$path


test_that("every refused statistic row is reported with its full context", {
  skip_if_not_installed("openxlsx2")
  dir   <- withr::local_tempdir()
  vocab <- .ulq_vocabs$first
  paths <- .ulq_build(vocab, dir)
  ars   <- .ulq_ars(paths, dir)
  q     <- ars_unresolved_labels(ars)

  ## The model the queue points INTO. An analysis id that names nothing is
  ## worse than no id at all -- it reads as a working reference and resolves
  ## to nothing -- so the id is checked against the analyses that exist, not
  ## merely for being non-empty. (`nzchar(NA)` is TRUE, which is exactly how
  ## a missing id passes a naive check.)
  model  <- jsonlite::fromJSON(ars, simplifyVector = FALSE)
  by_id  <- stats::setNames(
    lapply(model$analyses, function(a) a),
    vapply(model$analyses, function(a) as.character(a$id), character(1)))

  expected <- Filter(function(l) !isTRUE(l$binds), .ulq_LINES)
  ## Scope, asserted rather than assumed: if the grammar changed such that
  ## these labels all became readable -- or all became unreadable -- this test
  ## must go red rather than pass on an empty or over-full queue.
  expect_equal(nrow(q), length(expected))
  expect_setequal(q$label, vapply(expected, function(l) l$label, character(1)))

  for (l in expected) {
    r <- q[q$label == l$label, , drop = FALSE]
    expect_equal(nrow(r), 1L, info = l$label)
    ## Everything below indexes that single row. Without this the assertions
    ## become subscript ERRORS rather than clean failures the moment the row
    ## is absent, which hides what actually broke.
    if (nrow(r) != 1L) next
    expect_equal(r$reason, l$reason, info = l$label)
    expect_equal(r$tokens[[1]], l$tokens, info = l$label)
    ## Only a label that was READ can name a statistic the method lacks.
    expect_equal(r$unsupported[[1]],
                 if (identical(l$reason, "unsupported")) l$tokens
                 else character(), info = l$label)
    ## Where it is, in the workbook the author will open.
    expect_equal(r$sheet, vocab$sheet, info = l$label)
    expect_equal(r$row, unname(paths$rows[[l$label]]), info = l$label)
    ## Which analysis it would have been filled from, and by what method --
    ## resolved against the reporting event, not just present as a string.
    resolves <- !is.na(r$analysis_id) && r$analysis_id %in% names(by_id)
    expect_false(is.na(r$analysis_id), info = l$label)
    expect_true(resolves, info = l$label)
    ## Indexed only once the id is known to resolve: `by_id[[NA]]` and
    ## `by_id[["nope"]]` are subscript errors, and an error here would mask
    ## the clean failure the two expectations above already report.
    if (resolves) {
      expect_equal(as.character(by_id[[r$analysis_id]]$methodId), r$method_id,
                   info = l$label)
    }
    expect_equal(r$method_id, "MTH_SUMMARY_STATISTICS_CONTINUOUS",
                 info = l$label)
    ## What the row could have asked for instead.
    expect_setequal(r$available[[1]],
                    c("OP_N", "OP_MEAN", "OP_SD", "OP_MEDIAN",
                      "OP_Q1", "OP_Q3", "OP_MIN", "OP_MAX"))
    expect_true(nzchar(r$output_id), info = l$label)
    expect_true(nzchar(r$tlf), info = l$label)
  }

  ## Every refused row belongs to the SAME analysis -- the block's parent --
  ## which is what makes the queue actionable: the reader knows what the rows
  ## were meant to summarise.
  expect_equal(length(unique(q$analysis_id)), 1L)
  expect_false(anyNA(q$analysis_id))
})


test_that("a row that binds never appears in the queue", {
  skip_if_not_installed("openxlsx2")
  dir   <- withr::local_tempdir()
  paths <- .ulq_build(.ulq_vocabs$first, dir)
  q     <- ars_unresolved_labels(.ulq_ars(paths, dir))

  bound <- Filter(function(l) isTRUE(l$binds), .ulq_LINES)
  ## Non-vacuity: the fixture must actually contain rows that bind, or the
  ## absence below proves nothing.
  expect_gt(length(bound), 0L)
  for (l in bound) expect_false(l$label %in% q$label, info = l$label)
})


test_that("the queue is the same whether the ARS is a path or a parsed list", {
  skip_if_not_installed("openxlsx2")
  dir   <- withr::local_tempdir()
  paths <- .ulq_build(.ulq_vocabs$first, dir)
  ars   <- .ulq_ars(paths, dir)

  from_path <- ars_unresolved_labels(ars)
  from_list <- ars_unresolved_labels(jsonlite::fromJSON(ars,
                                                        simplifyVector = FALSE))
  expect_gt(nrow(from_path), 0L)
  expect_equal(from_path, from_list)
})


test_that("a single unsupported statistic reads as a vector, not a scalar", {
  ## The arity trap this file exists to pin: JSON auto-unboxes a one-element
  ## array, so a row with exactly one unsupported statistic comes back as a
  ## bare string while a row with two comes back as a list. Both must reach
  ## the caller as a character vector, or every downstream length() is wrong
  ## on precisely the commonest case.
  one <- .unresolved_chr("se")
  two <- .unresolved_chr(list("q1", "q3"))
  expect_identical(one, "se")
  expect_identical(two, c("q1", "q3"))
  expect_identical(.unresolved_chr(NULL), character(0))
  expect_identical(.unresolved_chr(list()), character(0))
})


test_that("a shell with nothing unresolved returns an empty frame, not NULL", {
  skip_if_not_installed("openxlsx2")
  dir <- withr::local_tempdir()
  ## Only the two lines that bind.
  clean <- Filter(function(l) isTRUE(l$binds), .ulq_LINES)
  paths <- .ulq_build(.ulq_vocabs$first, dir, lines = clean)
  q     <- ars_unresolved_labels(.ulq_ars(paths, dir))

  expect_s3_class(q, "data.frame")
  expect_equal(nrow(q), 0L)
  ## Same shape as a populated queue, so a caller never special-cases empty.
  full <- ars_unresolved_labels(
    .ulq_ars(.ulq_build(.ulq_vocabs$first, withr::local_tempdir()),
             withr::local_tempdir()))
  expect_gt(nrow(full), 0L)
  expect_equal(names(q), names(full))
  for (nm in names(q)) expect_equal(class(q[[nm]]), class(full[[nm]]), info = nm)
})


test_that("the queue keys on structure, not on familiar names", {
  ## Metamorphic: rename every dataset, variable, arm value and sheet into a
  ## second invented vocabulary. The queue must be structurally identical --
  ## same rows refused, same reasons, same statistics, same positions -- with
  ## only the study's own identifiers differing. This is what shows the
  ## reporting keys on the relationship between label and method rather than
  ## on anything it recognises about the names.
  skip_if_not_installed("openxlsx2")
  qs <- lapply(.ulq_vocabs, function(vocab) {
    dir   <- withr::local_tempdir()
    paths <- .ulq_build(vocab, dir)
    q     <- ars_unresolved_labels(.ulq_ars(paths, dir))
    q[order(q$row), , drop = FALSE]
  })

  expect_gt(nrow(qs$first), 0L)
  expect_equal(nrow(qs$first), nrow(qs$second))
  expect_equal(qs$first$label,  qs$second$label)
  expect_equal(qs$first$reason, qs$second$reason)
  expect_equal(qs$first$row,    qs$second$row)
  expect_equal(qs$first$tokens, qs$second$tokens)
  expect_equal(qs$first$unsupported, qs$second$unsupported)
  expect_equal(qs$first$available,   qs$second$available)
  expect_equal(qs$first$method_id,   qs$second$method_id)

  ## And the vocabularies really were different, or the comparison above is
  ## comparing a study with itself.
  expect_false(identical(qs$first$sheet, qs$second$sheet))
})


test_that("the two refusal reasons are the ones the constants declare", {
  ## A closed vocabulary: the builder writes these and the reader reports
  ## them, so a third code appearing on one side and not the other would be
  ## a silent miss rather than an error.
  expect_setequal(.UNRESOLVED_REASONS,
                  c(.UNRESOLVED_UNREADABLE, .UNRESOLVED_UNSUPPORTED))
  expect_equal(length(unique(.UNRESOLVED_REASONS)), 2L)
})


test_that("the run result spec_to_ars() returns is accepted as-is", {
  ## The shape a caller actually has in hand. spec_to_ars() returns a RUN
  ## RESULT -- paths, counts, diagnostics, and the reporting event under
  ## `reporting_event` -- so reading `$outputs` off it finds nothing. Before
  ## this, passing the obvious object returned an EMPTY queue, which reads as
  ## "nothing unresolved" and is indistinguishable from the real answer.
  skip_if_not_installed("openxlsx2")
  dir   <- withr::local_tempdir()
  paths <- .ulq_build(.ulq_vocabs$first, dir)
  run   <- .ulq_run(paths, dir)
  expect_true("reporting_event" %in% names(run$built))

  ## Caught as a clean FAILURE, not an error: without the unwrap the run
  ## result is not a valid event and aborts, and a test that merely errors
  ## reports "0 failures" -- which reads as "not detected".
  res <- tryCatch(ars_unresolved_labels(run$built), error = function(e) e)
  expect_false(inherits(res, "error"))
  if (inherits(res, "error")) return(invisible(NULL))

  from_result <- res
  from_path   <- ars_unresolved_labels(run$path)
  from_event  <- ars_unresolved_labels(run$built$reporting_event)

  ## Non-vacuity: an empty queue would make all three trivially equal.
  expect_gt(nrow(from_result), 0L)
  expect_equal(from_result, from_path)
  expect_equal(from_result, from_event)
})


test_that("an object that is not a reporting event aborts, not returns empty", {
  ## Silence is the failure mode being prevented: an empty queue means
  ## "nothing is unresolved", and a caller acts on that.
  expect_error(ars_unresolved_labels(list(foo = 1)), "not a reporting event")
  expect_error(ars_unresolved_labels(42), "not a reporting event")
  ## A genuine event with no outputs is a real, empty answer -- not an error.
  expect_equal(nrow(ars_unresolved_labels(list(outputs = list()))), 0L)
})
