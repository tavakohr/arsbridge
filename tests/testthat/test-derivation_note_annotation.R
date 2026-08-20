## A derivation note is not a row filter.
##
## General defect class: a shell annotation may state, after a semicolon, how
## the row's variable is DERIVED. Read as a filter, the clause compares a
## variable to another variable -- which an ARS condition cannot express, an
## ARS condition comparing a variable to VALUES -- so the row reserved as
## "a condition this package could not read". Worse, a filtered row is COUNTED
## rather than summarised, so the block's method became a subject count; and
## under a per-category method every unannotated child row is a level, so the
## block's statistic rows (n, Mean (SD), Median, Q1, Q3, Min, Max) were
## recorded as category levels of a variable. Those cells can never match an
## ARD row, and nothing reported it.
##
## General invariant: a clause with NO left operand whose right operand is a
## qualified ADaM reference states a relationship, not a restriction. It does
## not filter, it does not decide the method, and it is reported rather than
## silently discarded. A clause with a left operand, or one comparing against
## a VALUE, is a row filter exactly as before.

## Two unrelated invented vocabularies. The second exists to prove the rule
## keys on the shape of the clause and not on any familiar name.
V1 <- list(ds = "ADQX", dur = "MEASDUR", end = "ENDDY", arm = "TRTGRP",
           a = "Regimen P", b = "Regimen Q", sheet = "Table 90.1")
V2 <- list(ds = "ADZZ", dur = "SPANDY", end = "STOPDY", arm = "COHORT",
           a = "Cohort M", b = "Cohort N", sheet = "Table 91.1")

.dn_ann <- function(v) sprintf("%s.%s; = %s.%s", v$ds, v$dur, v$ds, v$end)


## --- the parser ------------------------------------------------------------

test_that("a derivation note is not read as a condition", {
  for (v in list(V1, V2)) {
    ann <- .dn_ann(v)
    split <- .split_derivation_note(ann)
    expect_equal(split$head, sprintf("%s.%s", v$ds, v$dur), info = v$ds)
    expect_equal(split$note, sprintf("= %s.%s", v$ds, v$end), info = v$ds)

    ## The defect, at the layer it started: no unreadable condition, and no
    ## data subset invented in its place.
    subset <- .subset_from_annotation(ann)
    expect_false(.is_unresolved_condition(subset), info = v$ds)
    expect_null(subset, info = v$ds)

    ## And the method comes from the variable's type, as it would with no
    ## note at all.
    got <- .infer_row_method(list(annotation = ann, n_slots = 1L),
                             var_is_categorical = FALSE)
    expect_equal(got$method, "Summary Statistics - Continuous", info = v$ds)
    expect_equal(got$kind, "continuous", info = v$ds)
  }
})


test_that("a genuine row filter is still a row filter", {
  ## Each of these keeps its OWN existing behaviour, which is not the same
  ## behaviour for all of them -- that is the point. A condition restricting
  ## the row's own variable counts subjects in that state; one naming a
  ## DIFFERENT variable only scopes the data, and the variable's type still
  ## decides the method. `unres` records whether the where-clause grammar can
  ## READ the condition: two of these it cannot, which is pre-existing and
  ## deliberately untouched here.
  ##
  ## Every value below was MEASURED on the code before this change, so a
  ## drift in either direction turns this red.
  ## The METHOD column changed for three rows with the filter-role work, and
  ## the rule that produced the new values is one sentence: a restriction says
  ## which records survive, not which statistic is reported about them.
  ##
  ## Exactly one thing a filter can say still settles the method, and it is a
  ## TEMPORARY structural dependency rather than a semantic rule: a restriction
  ## pinning the row's variable to a single value keeps the count family,
  ## because block construction downstream reads what a block IS from the
  ## method its first row was given. It is not a claim that `=` requests a
  ## count -- a shell may pin a value and then ask for Mean/SD. Expect these
  ## values to change when block shape is settled before method selection.
  ## Two conditions have to hold for a pinned reading:
  ##
  ##   the clause was READ  -- an unresolved clause is evidence about nothing,
  ##                          so nothing is known about what it pins; and
  ##   it is an equality    -- a threshold or range leaves the variable free
  ##                          to vary among the survivors.
  ##
  ## Which is why the four rows below differ, and each for one reason:
  ##
  ##   "; >= 65"   unresolved -> no evidence -> the variable's type decides
  ##   "; = 30"    unresolved -> no evidence, EVEN THOUGH it is written as an
  ##               equality: the grammar never read it, so "it pins" is not
  ##               something this package knows
  ##   "EVFL='Y'"  read, equality -> one value survives -> counted
  ##   "GE 16"     read, threshold -> the variable still varies -> type decides
  ##
  ## `unres` is unchanged for every row: what the grammar can READ did not
  ## move, and the two rows that reserve still reserve. No number changes --
  ## a reserved row computes nothing either way.
  keeps <- list(
    ## an implied left operand with a VALUE -- "this variable, restricted"
    list(ann = "ADQX.AGEYR; >= 65",        cat = FALSE, unres = TRUE,
         method = "Summary Statistics - Continuous", kind = "continuous"),
    list(ann = "ADQX.MEASDUR; = 30",       cat = FALSE, unres = TRUE,
         method = "Summary Statistics - Continuous", kind = "continuous"),
    ## a condition on the variable itself, no semicolon
    list(ann = "ADQX.EVFL='Y'",            cat = TRUE,  unres = FALSE,
         method = "Subject Count", kind = "filtered_count"),
    list(ann = "ADQX.MEASDUR GE 16",       cat = FALSE, unres = FALSE,
         method = "Summary Statistics - Continuous", kind = "continuous"),
    ## a condition naming ANOTHER variable: scoping, so type decides
    list(ann = "ADQX.TERM; ADQX.EVFL='Y'", cat = TRUE,  unres = FALSE,
         method = "Count and Percentage", kind = "categorical"),
    ## Two variables compared, but WITH a left operand. This is the case the
    ## "no left operand" half of the rule exists for: it is a restriction the
    ## grammar cannot read, so it reserves -- and it must keep reserving.
    ## Treating it as a derivation note would drop a real restriction and
    ## compute the row over every record.
    ##
    ## The METHOD here changed with the filter-role work, and deliberately.
    ## The restriction names STDT and TRTSDT -- neither of them this row's
    ## variable -- so it scopes the records rather than describing a state of
    ## TERM, and TERM's own type decides what the line reports. It used to
    ## count subjects, for no better reason than that the flattener returned
    ## nothing and "nothing" was read as "the filter is on my own variable".
    ##
    ## What has NOT changed is the half that protects the number: `unres`
    ## stays TRUE, so the row is still reserved and still computes nothing.
    list(ann = "ADQX.TERM; ADQX.STDT = ADQX.TRTSDT", cat = FALSE, unres = TRUE,
         method = "Summary Statistics - Continuous", kind = "continuous"))
  for (k in keeps) {
    expect_equal(.split_derivation_note(k$ann)$note, "", info = k$ann)
    got <- .infer_row_method(list(annotation = k$ann, n_slots = 1L),
                             var_is_categorical = k$cat)
    expect_equal(got$method, k$method, info = k$ann)
    expect_equal(got$kind, k$kind, info = k$ann)
    ## The stated condition still reaches the subset reader -- never silently
    ## removed, whether or not the grammar can read it.
    subset <- .subset_from_annotation(k$ann)
    expect_false(is.null(subset), info = k$ann)
    expect_equal(.is_unresolved_condition(subset), k$unres, info = k$ann)
  }

  ## Two clauses, or anything else after the semicolon, is left alone: this
  ## may only ever remove the one form it recognises.
  for (ann in c("ADQX.TERM; ADQX.EVFL='Y'; ADQX.SEQ=1",
                "ADQX.MEASDUR; see the SAP",
                "ADQX.MEASDUR; = ADQX.ENDDY and ADQX.EVFL='Y'",
                ## a left operand present -- a comparison, not a definition
                "ADQX.TERM; ADQX.STDT = ADQX.TRTSDT",
                "ADQX.MEASDUR; ADQX.ENDDY = ADQX.STDY")) {
    expect_equal(.split_derivation_note(ann)$note, "", info = ann)
  }
})


## --- the block the defect broke, end to end --------------------------------

.dn_build <- function(v, dir) {
  sw <- openxlsx2::wb_workbook()$add_worksheet("Variables")
  sw$add_data(sheet = "Variables", x = data.frame(
    Dataset  = c("ADSL", "ADSL", rep(v$ds, 4)),
    Variable = c("USUBJID", v$arm, "USUBJID", v$arm, v$dur, v$end),
    Label    = c("Subject", "Group", "Subject", "Group", "Duration", "End day"),
    Type     = c("Char", "Char", "Char", "Char", "Num", "Num"),
    Origin = "Derived", Codelist = "", Length = "40", Mandatory = "Req",
    stringsAsFactors = FALSE))
  spec <- file.path(dir, "spec.xlsx"); sw$save(spec)

  black <- openxlsx2::wb_color(hex = "FF000000")
  red   <- openxlsx2::wb_color(hex = "FFC00000")
  ann <- function(l, a) {
    openxlsx2::fmt_txt(l, color = black, size = 10) +
      openxlsx2::fmt_txt(paste0("\n", a), color = red, size = 8, italic = TRUE)
  }
  wb <- openxlsx2::wb_workbook()$add_worksheet(v$sheet)
  put <- function(x, row, col = 1L) {
    wb$add_data(sheet = v$sheet, x = x, start_row = row, start_col = col,
                col_names = FALSE)
  }
  put(v$sheet, 1)
  put("Duration of exposure", 2)
  put("Item", 3)
  put(ann(sprintf("%s (N=XX)", v$a), sprintf("%s.%s='%s'", v$ds, v$arm, v$a)), 3, 2L)
  put(ann(sprintf("%s (N=XX)", v$b), sprintf("%s.%s='%s'", v$ds, v$arm, v$b)), 3, 3L)
  ## The annotation at the heart of this: a variable, and how it is derived.
  put(ann("Duration (days)", sprintf("[%s]", .dn_ann(v))), 4)
  lines <- list(list("n", "xx"), list("Mean (SD)", "xx.x (xx.xx)"),
                list("Median", "xx.x"), list("Q1, Q3", "xx.x, xx.x"),
                list("Min, Max", "xx.x, xx.x"))
  for (i in seq_along(lines)) {
    put(lines[[i]][[1]], 4L + i)
    for (j in 2:3) put(lines[[i]][[2]], 4L + i, j)
  }
  shell <- file.path(dir, "shell.xlsx"); wb$save(shell)
  list(shell = shell, spec = spec, sheet = v$sheet,
       stat_rows = 5:9, labels = vapply(lines, function(x) x[[1]], character(1)))
}

.dn_ars <- function(paths, dir) {
  ars <- file.path(dir, "ars.json")
  withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(suppressWarnings(spec_to_ars(
      shell_path = paths$shell, adam_spec_path = paths$spec, api_key = "",
      use_llm = FALSE, verbose = FALSE, output_path = ars,
      report_path = file.path(dir, "report.xlsx"), emit_code = FALSE))))
  jsonlite::fromJSON(ars, simplifyVector = FALSE)
}

test_that("the block stays a continuous summary and its statistic rows stay statistic rows", {
  skip_if_not_installed("openxlsx2")
  for (v in list(V1, V2)) {
    dir   <- withr::local_tempdir()
    paths <- .dn_build(v, dir)
    j     <- .dn_ars(paths, dir)

    expect_gt(length(j$outputs %||% list()), 0L)
    if (length(j$outputs %||% list()) == 0L) next
    o <- j$outputs[[1]]
    sf <- o$`_meta`$shell_fill
    ## Guarded: a mutant that breaks the build leaves this NULL, and indexing
    ## NULL ERRORS. An error reads as "not detected", which is the direction
    ## that hides a real gap -- so it has to fail instead.
    expect_false(is.null(sf), info = v$ds)
    if (is.null(sf)) next

    ## The parent block is summarised, not counted.
    ids <- unique(unlist(lapply(sf$cells, function(c) c$analysis_id)))
    ids <- ids[!is.na(ids)]
    meths <- unique(vapply(ids, function(id) {
      a <- Filter(function(x) identical(x$id, id), j$analyses)
      if (length(a)) as.character(a[[1]]$methodId %||% "?") else "?"
    }, character(1)))
    expect_equal(meths, "MTH_SUMMARY_STATISTICS_CONTINUOUS", info = v$ds)

    ## No analysis carries an unreadable condition -- the marker that reserved
    ## this block before.
    unresolved <- vapply(j$analyses,
                         function(a) length(a$unresolvedCondition %||% list()) > 0,
                         logical(1))
    expect_false(any(unresolved), info = v$ds)

    ## Every statistic row is a STATISTIC row: it carries the label it was
    ## read from, and it is not a category level.
    by_row <- list()
    for (cl in sf$cells) by_row[[as.character(cl$row)]] <- cl
    checked <- 0L
    for (i in seq_along(paths$stat_rows)) {
      cl <- by_row[[as.character(paths$stat_rows[[i]])]]
      if (is.null(cl)) next
      checked <- checked + 1L
      expect_equal(cl$stat_line, paths$labels[[i]], info = paste(v$ds, cl$row))
      expect_null(cl$variable_level, info = paste(v$ds, cl$row))
      expect_gt(length(cl$slots %||% list()), 0L)
    }
    ## Scope: a shell whose rows stopped being read at all would pass every
    ## assertion above by never running one.
    expect_equal(checked, length(paths$stat_rows), info = v$ds)

    ## The statistics themselves, in the order the labels name them.
    ops <- function(r) {
      cl <- by_row[[as.character(r)]]
      vapply(cl$slots %||% list(), function(s) as.character(s$operation_id %||% NA),
             character(1))
    }
    expect_equal(ops(5), "OP_N")
    expect_equal(ops(6), c("OP_MEAN", "OP_SD"))
    expect_equal(ops(7), "OP_MEDIAN")
    expect_equal(ops(8), c("OP_Q1", "OP_Q3"))
    expect_equal(ops(9), c("OP_MIN", "OP_MAX"))
  }
})


test_that("the derivation note is reported, not silently discarded", {
  ## It states a real relationship. A reader who meant it as a restriction has
  ## to be able to see that it was not applied as one.
  skip_if_not_installed("openxlsx2")
  dir   <- withr::local_tempdir()
  paths <- .dn_build(V1, dir)
  diag_reset()
  invisible(.dn_ars(paths, dir))
  recs <- ars_diagnostics()
  hit  <- recs[grepl("derivation note", recs$problem), ]
  expect_equal(nrow(hit), 1L)
  if (nrow(hit) == 1L) {
    expect_equal(hit$severity[[1]], "INFO")
    expect_match(hit$problem[[1]], "ADQX.ENDDY", fixed = TRUE)
    expect_match(hit$action[[1]], "RESTRICT", fixed = TRUE)
  }
  diag_reset()
})
