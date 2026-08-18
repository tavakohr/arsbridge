## A-09: the count line of a continuous block can never match.
##
## General defect class: the ARD stat_name an ARS operation produces was
## treated as a property of the operation id alone. It is a property of the
## (method, operation) pair -- the same operation is spelled differently
## depending on the idiom the method runs under -- and the ARD join is exact.
## So an operation could ask for a name nothing in the ARD carries, and every
## cell bound to it reported "no result in the ARD for this cell": the wording
## for an analysis that never ran, used for a number sitting in the ARD under
## another name.
##
## General invariant: every operation of every method resolves to a non-empty
## ORDERED SET of candidate stat names, a cell matches if the ARD carries any
## of them, and within one method no two operations may share a candidate --
## otherwise one operation would answer another operation's cell. That last
## clause is why the register's suggested "match case-insensitively" is not
## what was built: it would have made the count and the denominator of every
## counting method indistinguishable.


## ---------------------------------------------------------------------------
## The invariant, over the whole method catalogue
## ---------------------------------------------------------------------------

test_that("every declared operation resolves to at least one stat name", {
  methods <- .STANDARD_METHODS
  expect_gt(length(methods), 0L)

  checked <- 0L
  for (m in methods) {
    for (op in m$operations %||% list()) {
      cand <- .operation_stat_names(m$id, op$id, op$name)
      expect_true(length(cand) > 0L && all(nzchar(cand)) && !anyNA(cand),
                  info = paste(m$id, op$id))
      checked <- checked + 1L
    }
  }
  ## Assert the scope, so a catalogue that stopped declaring operations turns
  ## this test red instead of passing vacuously.
  expect_gt(checked, 15L)
})


test_that("no two operations of one method share a stat name", {
  ## The collision guard. An operation whose candidates overlap another's in
  ## the SAME method would answer that operation's cell -- silently, with a
  ## real number of the wrong statistic.
  checked <- 0L
  for (m in .STANDARD_METHODS) {
    ops <- m$operations %||% list()
    if (length(ops) < 2L) next
    cands <- lapply(ops, function(op) .operation_stat_names(m$id, op$id, op$name))
    for (i in seq_along(cands)) {
      for (j in seq_along(cands)) {
        if (j <= i) next
        expect_length(intersect(cands[[i]], cands[[j]]), 0L)
        checked <- checked + 1L
      }
    }
  }
  expect_gt(checked, 0L)
})


test_that("the count operation is spelled by the method that runs it", {
  cont <- "MTH_SUMMARY_STATISTICS_CONTINUOUS"
  catg <- "MTH_COUNT_AND_PERCENTAGE"

  ## The engine's own spellings, read from the engine rather than transcribed,
  ## so this comparison cannot drift from what {cards} actually emits.
  d <- data.frame(G = c("A", "A", "B", "B"), V = c(1, 2, 3, 4),
                  L = c("x", "y", "x", "y"), stringsAsFactors = FALSE)
  cont_stats <- unique(as.character(
    cards::ard_continuous(data = d, variables = "V", by = "G")$stat_name))
  catg_stats <- unique(as.character(
    cards::ard_categorical(data = d, variables = "L", by = "G")$stat_name))

  ## The count under a continuous method must be findable in a continuous ARD.
  expect_true(any(.operation_stat_names(cont, "OP_N", "n") %in% cont_stats))
  ## And under a counting method, in a categorical ARD -- as the count, never
  ## as the denominator.
  expect_true(any(.operation_stat_names(catg, "OP_N", "Count") %in% catg_stats))
  expect_length(intersect(.operation_stat_names(catg, "OP_N", "Count"),
                          .operation_stat_names(catg, "OP_DENOM", "Denominator")),
                0L)

  ## A method the table says nothing about keeps the global spelling.
  expect_equal(.operation_stat_names("MTH_NOT_IN_THE_TABLE", "OP_N", "n"),
               .operation_stat_name("OP_N", "n"))
  ## And a missing method id is ordinary, not an error.
  expect_equal(.operation_stat_names(NA, "OP_MANUAL", "Manual derivation"),
               .MANUAL_STAT_NAME)
})


## ---------------------------------------------------------------------------
## The lookup itself: the candidate set is what widens the join
## ---------------------------------------------------------------------------

## An ARD-shaped index built from a real {cards} result, so the join is tested
## against the engine's own output rather than against a hand-typed frame.
.osc_index <- function(ard, analysis_id) {
  ard <- as.data.frame(ard)
  ard$analysis_id   <- analysis_id
  ard$result_status <- "computed"
  .ard_index(ard)
}

test_that("a continuous count resolves through its candidates and not without", {
  d <- data.frame(G = rep(c("A", "B"), each = 5),
                  V = c(1, 2, 3, 4, NA, 6, 7, 8, 9, 10))
  idx <- .osc_index(cards::ard_continuous(data = d, variables = "V", by = "G"),
                    "AN_1")

  ## Non-vacuity: the count really is in there, under the engine's spelling.
  expect_true(any(idx$stat_name == "N"))

  cand <- .operation_stat_names("MTH_SUMMARY_STATISTICS_CONTINUOUS", "OP_N", "n")
  hit <- .ard_value(idx, "AN_1", group_level = "A", variable_level = NA,
                    stat_name = cand)
  expect_equal(hit$status, "computed")
  expect_equal(hit$value, 4)   # one of the five values is missing

  ## The old single-spelling key finds nothing -- which is the defect, stated
  ## as an assertion so it cannot come back unnoticed.
  miss <- .ard_value(idx, "AN_1", group_level = "A", variable_level = NA,
                     stat_name = "n")
  expect_equal(miss$status, "no_row")
})


test_that("a counting method's count never resolves to its denominator", {
  d <- data.frame(G = rep(c("A", "B"), each = 4),
                  L = c("x", "x", "x", "y", "x", "y", "y", "y"),
                  stringsAsFactors = FALSE)
  idx <- .osc_index(
    cards::ard_categorical(data = d, variables = "L", by = "G"), "AN_2")

  n_key <- .operation_stat_names("MTH_COUNT_AND_PERCENTAGE", "OP_N", "Count")
  d_key <- .operation_stat_names("MTH_COUNT_AND_PERCENTAGE", "OP_DENOM",
                                 "Denominator")

  n_hit <- .ard_value(idx, "AN_2", "A", "x", n_key)
  d_hit <- .ard_value(idx, "AN_2", "A", "x", d_key)

  expect_equal(n_hit$status, "computed")
  expect_equal(d_hit$status, "computed")
  ## Three of A's four records are "x": the count is 3, the denominator 4. If
  ## the two keys had overlapped, one of these would carry the other's number.
  expect_equal(n_hit$value, 3)
  expect_equal(d_hit$value, 4)
})


test_that("a slot resolves through a secondary spelling of its operation", {
  ## The candidate set is tolerance, not a rename. `ars_fill_shell()` fills
  ## from an ARD the CALLER supplies, which need not be one arsbridge produced,
  ## so the count can arrive under either spelling. Carrying the whole set on
  ## the slot is what lets the cell match either -- and without a test the
  ## machinery is unfalsifiable, which is its own defect.
  lower <- data.frame(
    analysis_id = "AN_3", group1_level = "A", variable_level = NA_character_,
    stat_name = "n", stat = 7, result_status = "computed",
    stringsAsFactors = FALSE)
  idx <- .ard_index(lower)

  slots <- .method_operation_slots(.STANDARD_METHODS,
                                   "MTH_SUMMARY_STATISTICS_CONTINUOUS")
  op_n <- Filter(function(s) identical(s$operation_id, "OP_N"), slots)[[1]]
  ## Non-vacuity: the operation really carries more than one spelling, and its
  ## primary is NOT the one this ARD used -- otherwise the pair below would be
  ## comparing a key against itself.
  expect_gt(length(op_n$stat_names), 1L)
  expect_false(identical(op_n$stat_name, "n"))

  as_cell <- function(slot) {
    list(analysis_id = "AN_3", group = list(label = "A"), slots = list(slot))
  }
  base_slot <- list(order = 1L, token = "xx", type = "value", decimals = 0L,
                    start = 1L, stop = 2L, operation_id = "OP_N",
                    stat_name = op_n$stat_name)

  widened <- .resolve_cell(
    as_cell(c(base_slot, list(stat_names = op_n$stat_names))), idx)
  expect_equal(widened$statuses, "computed")
  expect_false(is.na(widened$values))

  ## Drop the candidates and the same cell finds nothing.
  primary_only <- .resolve_cell(as_cell(base_slot), idx)
  expect_equal(primary_only$statuses, "no_row")
})


## ---------------------------------------------------------------------------
## End to end: a continuous block whose count line fills
## ---------------------------------------------------------------------------

.osc_vocabs <- list(
  first  = list(ds = "ADQX", arm = "TRTP", num = "AGE",
                a = "Drug A", b = "Placebo", sheet = "Table 14.1.2"),
  second = list(ds = "ADZZ", arm = "ZZGRP", num = "ZZVAL",
                a = "Kappa", b = "Lambda", sheet = "Table 9.4.4")
)

## A continuous block: the parent row carries the variable, the lines name the
## statistics. The count line is the one A-09 is about; the others are here so
## a fix that filled the count by breaking them cannot pass.
.osc_LINES <- list(
  list(label = "n",         placeholder = "xx"),
  list(label = "Mean (SD)", placeholder = "xx.x (xx.xx)"),
  list(label = "Median",    placeholder = "xx.x"),
  list(label = "Min, Max",  placeholder = "xx, xx")
)

.osc_build <- function(vocab, dir) {
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
  ## Six subjects per arm, one of them with no measurement, so the count line
  ## must show the number of NON-MISSING values and is distinguishable from
  ## the number of subjects.
  subjects <- data.frame(
    USUBJID = sprintf("S%02d", 1:12),
    ARM = rep(c(vocab$a, vocab$b), each = 6),
    VAL = c(41, 52, 63, 47, 58, NA, 44, 55, 66, 50, 61, 72),
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
  for (i in seq_along(.osc_LINES)) {
    put(.osc_LINES[[i]]$label, 4L + i)
    for (j in 2:3) put(.osc_LINES[[i]]$placeholder, 4L + i, j)
  }
  shell <- file.path(dir, "shell.xlsx")
  wb$save(shell)
  list(shell = shell, spec = spec, adam = adam, sheet = vocab$sheet)
}

.osc_fill <- function(paths, dir, ard_fn = identity) {
  ars <- file.path(dir, "ars.json")
  withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(suppressWarnings(spec_to_ars(
      shell_path = paths$shell, adam_spec_path = paths$spec, api_key = "",
      use_llm = FALSE, verbose = FALSE, output_path = ars,
      report_path = file.path(dir, "report.xlsx"), emit_code = FALSE))))
  ard <- ard_fn(suppressMessages(suppressWarnings(ars_to_ard(ars, paths$adam))))
  filled <- file.path(dir, "filled.xlsx")
  res <- suppressMessages(suppressWarnings(ars_fill_shell(
    shell_path = paths$shell, ars = ars, ard = ard, output_path = filled,
    adam_dir = paths$adam, overwrite = TRUE)))
  book <- openxlsx2::wb_to_df(openxlsx2::wb_load(filled), sheet = paths$sheet,
                              col_names = FALSE)
  list(census = res$census, ard = ard,
       cell = function(row, col) as.character(book[row, col]))
}


test_that("the count line of a continuous block fills", {
  skip_if_not_installed("openxlsx2")
  dir <- withr::local_tempdir()
  paths <- .osc_build(.osc_vocabs$first, dir)
  out <- .osc_fill(paths, dir)

  ## Non-vacuity: the engine really did produce the count, so a filled cell
  ## below cannot be an empty comparison.
  expect_gt(sum(as.character(out$ard$stat_name) %in% "N", na.rm = TRUE), 0L)

  ## Row 5 is the count line (the block's parent row is row 4). Five of six
  ## values are non-missing in the first arm, six of six in the second.
  expect_equal(out$cell(5, 2), "5")
  expect_equal(out$cell(5, 3), "6")

  ## And the placeholder is gone, which is the thing the reader sees.
  expect_false(grepl("x", out$cell(5, 2), ignore.case = TRUE))

  ## The lines that already worked still work: a fix that filled the count by
  ## shifting the other statistics onto the wrong operations fails here.
  expect_true(grepl("^[0-9.]+ \\([0-9.]+\\)$", out$cell(6, 2)))
  expect_false(grepl("x", out$cell(7, 2), ignore.case = TRUE))
  expect_false(grepl("x", out$cell(8, 2), ignore.case = TRUE))

  ## Every cell of the block filled -- all four lines, both columns.
  body <- out$census[out$census$row >= 5L, , drop = FALSE]
  expect_equal(nrow(body), 8L)
  expect_equal(sum(body$status %in% "filled"), 8L)

  ## The only cells still on a placeholder are the header's "(N=XX)", and they
  ## stay there for the documented reason: a table showing no percentage has no
  ## denominator to put in a column header.
  ##
  ## Asserted rather than excluded, because it is the guard on this change:
  ## `.column_denominator()` matches the stat name "N" directly and deliberately
  ## skips continuous analyses. Now that a continuous count also answers to "N",
  ## a leak there would fill these two cells with the count of one arm and pass
  ## silently. This is what notices.
  pending <- out$census[out$census$status %in% "pending", , drop = FALSE]
  expect_equal(nrow(pending), 2L)
  expect_true(all(pending$row == 3L))
  expect_true(all(pending$reason ==
                    "no result in this column is shown as a percentage"))
})


test_that("the same block reads identically in a second vocabulary", {
  ## Rename every dataset, variable and level into an unrelated vocabulary.
  ## The algorithm keys on the method and the engine's spellings, so the
  ## outcome must be structurally identical.
  skip_if_not_installed("openxlsx2")
  dir <- withr::local_tempdir()
  paths <- .osc_build(.osc_vocabs$second, dir)
  out <- .osc_fill(paths, dir)

  expect_equal(out$cell(5, 2), "5")
  expect_equal(out$cell(5, 3), "6")

  body <- out$census[out$census$row >= 5L, , drop = FALSE]
  expect_equal(nrow(body), 8L)
  expect_equal(sum(body$status %in% "filled"), 8L)
})


## ---------------------------------------------------------------------------
## The diagnostic that would have named this defect in one line
## ---------------------------------------------------------------------------

.osc_spec <- function(method) list(methods = list(method))

.osc_ard <- function(stat_names, method_id) {
  data.frame(
    analysis_id     = "AN_1",
    stat_name       = stat_names,
    result_status   = "computed",
    method_intended = method_id,
    method_actual   = method_id,
    stringsAsFactors = FALSE)
}

.osc_method <- function(id) {
  Filter(function(x) identical(x$id, id), .STANDARD_METHODS)[[1]]
}

test_that("an operation with no result in the ARD is named", {
  m <- .osc_method("MTH_SUMMARY_STATISTICS_CONTINUOUS")
  declared <- vapply(m$operations,
                     function(op) .operation_stat_names(m$id, op$id, op$name)[[1]],
                     character(1))
  expect_gt(length(declared), 1L)

  ## Complete: silent.
  diag_reset()
  .check_operation_coverage(.osc_ard(declared, m$id), .osc_spec(m))
  expect_equal(nrow(diag_records()), 0L)

  ## One operation absent: named, with the statistics that WERE emitted, so a
  ## naming disagreement reads differently from an analysis that produced
  ## nothing.
  diag_reset()
  .check_operation_coverage(.osc_ard(declared[-1], m$id), .osc_spec(m))
  rec <- diag_records()
  expect_equal(nrow(rec), 1L)
  expect_equal(rec$severity, "WARN")
  expect_true(grepl(m$operations[[1]]$id, rec$problem, fixed = TRUE))
  expect_true(grepl(declared[[2]], rec$problem, fixed = TRUE))
})


test_that("a reserved or substituted analysis is not reported as a shortfall", {
  m <- .osc_method("MTH_SUMMARY_STATISTICS_CONTINUOUS")

  ## Nothing computed: the analysis was reserved, which is a different thing
  ## and already has its own reason.
  diag_reset()
  ard <- .osc_ard("N", m$id)
  ard$result_status <- "manual_pending"
  .check_operation_coverage(ard, .osc_spec(m))
  expect_equal(nrow(diag_records()), 0L)

  ## A substituted method genuinely produces a different operation set.
  diag_reset()
  ard <- .osc_ard("N", m$id)
  ard$method_intended <- "MTH_KAPLAN_MEIER_ESTIMATE"
  .check_operation_coverage(ard, .osc_spec(m))
  expect_equal(nrow(diag_records()), 0L)
})


test_that("a caller's ARD spelled differently still fills the count line", {
  ## `ars_fill_shell()` fills from whatever ARD it is handed, and that ARD
  ## need not be one arsbridge produced. Here the numbers are the same and
  ## only the count's spelling differs -- which is the case the candidate set
  ## exists for, driven all the way through the cell map rather than around it.
  skip_if_not_installed("openxlsx2")
  dir <- withr::local_tempdir()
  paths <- .osc_build(.osc_vocabs$first, dir)

  seen <- new.env(parent = emptyenv())
  out <- .osc_fill(paths, dir, ard_fn = function(ard) {
    sn <- as.character(ard$stat_name)
    seen$before <- sum(sn %in% "N", na.rm = TRUE)
    ard$stat_name[!is.na(sn) & sn == "N"] <- "n"
    seen$after <- sum(as.character(ard$stat_name) %in% "N", na.rm = TRUE)
    ard
  })

  ## Non-vacuity: the rewrite really happened, and really left no row under the
  ## primary spelling for the count to fall back on.
  expect_gt(seen$before, 0L)
  expect_equal(seen$after, 0L)

  expect_equal(out$cell(5, 2), "5")
  expect_equal(out$cell(5, 3), "6")
})
