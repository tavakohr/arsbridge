## An unreadable condition reserves its results, and nothing else.
##
## The five things that must all hold:
##
##   1. the parser returns `unresolved`, not `NULL`
##   2. the builder records it, and validation raises the right finding
##   3. the finding reaches the reservation map
##   4. no unfiltered calculation is produced
##   5. an unaffected analysis beside it still computes
##
## The fifth is load-bearing and easy to lose. A build that reserved everything
## would satisfy the first four while destroying the point of the change: a
## defect withholds its own cells and no others.
##
## Criterion 4 on real data -- reserved analysis yields no computed row while
## its neighbour does, in the ARD, the generated program AND the workbook -- is
## driven by test-reserved_end_to_end.R, which now carries an unreadable row
## filter among its damage kinds. This file covers the parser contract, the
## finding, the map, and the fill-time figure path that never enters a model.

.ucr_model <- function(unresolved_on = c("none", "analysis", "analysis_set",
                                         "grouping")) {
  unresolved_on <- match.arg(unresolved_on)
  model <- .rmap_model()

  ## Written exactly where the builder writes it -- on the raw node -- so this
  ## exercises the same field `.check_unresolved_condition()` reads in a run.
  stamp <- function(pool, row, value) {
    raw <- model[[pool]]$raw
    raw[[row]][["unresolvedCondition"]] <- value
    model[[pool]]$raw <- raw
    model
  }

  switch(
    unresolved_on,
    none         = model,
    analysis     = stamp("analyses", 1L, "ADQX.SCORE >= 16"),
    analysis_set = stamp("analysis_sets", 1L, "ADQX.GROUPFL >= 'Y'"),
    grouping     = stamp("groupings", 1L, "ADQX.MEASURE IN (")
  )
}

.ucr_reserved <- function(model) {
  findings <- validate_ars_model(model)
  list(
    findings = findings,
    reserved = names(.reservations_from_findings(model, findings)$by_analysis)
  )
}


test_that("criterion 1: an unreadable clause returns unresolved, not NULL", {
  wc <- suppressWarnings(parse_where_clause("ADQX.SCORE >= 16"))
  expect_false(is.null(wc))
  expect_true(.is_unresolved_condition(wc))
  ## An absent condition is still NULL, or the distinction buys nothing.
  expect_null(suppressWarnings(parse_where_clause("count of ADQX.MEASURE")))
})


test_that("criteria 2, 3 and 5: each entity type reserves its own analyses", {
  ## The control: with no marker anywhere, nothing is reserved. Without it the
  ## cases below could pass on a build that reserves unconditionally.
  clean <- .ucr_reserved(.ucr_model("none"))
  expect_length(clean$reserved, 0L)
  expect_false(any(grepl("CONDITION_UNRESOLVED", clean$findings$ref)))

  cases <- list(
    list(where = "analysis",     ref = "ANALYSIS_CONDITION_UNRESOLVED"),
    list(where = "analysis_set", ref = "ANALYSIS_SET_CONDITION_UNRESOLVED"),
    list(where = "grouping",     ref = "GROUPING_LEVEL_CONDITION_UNRESOLVED")
  )

  for (case in cases) {
    model <- .ucr_model(case$where)
    result <- .ucr_reserved(model)

    ## Criterion 2: the right finding, at GAP so it withholds.
    hit <- result$findings[result$findings$ref == case$ref, , drop = FALSE]
    expect_equal(nrow(hit), 1L, info = case$where)
    expect_identical(hit$severity[[1]], "GAP", info = case$where)
    ## The author's own text travels with it, for the fix report to quote.
    expect_true(nzchar(hit$detail[[1]]), info = case$where)

    ## Criterion 3: it reaches the reservation map.
    expect_gt(length(result$reserved), 0L)

    ## Criterion 5: and does not take the whole event with it. The fixture has
    ## two analyses sharing nothing; at least one must survive.
    all_ids <- model$analyses$id
    expect_equal(length(all_ids), 2L, info = case$where)
    expect_gt(length(setdiff(all_ids, result$reserved)), 0L)
  }
})


test_that("criterion 4: the map withholds the reserved analysis and no other", {
  ## The engine-level proof runs in test-reserved_end_to_end.R on real data.
  ## What is checked here is the input that path depends on: the reservation
  ## carries the right cause, at a scope that withholds, and reaches exactly
  ## the damaged analysis.
  model <- .ucr_model("analysis")
  result <- .ucr_reserved(model)
  expect_gt(length(result$reserved), 0L)

  map <- .reservations_from_findings(model, result$findings)$by_analysis
  for (id in result$reserved) {
    expect_identical(map[[id]]$ref, "ANALYSIS_CONDITION_UNRESOLVED")
    expect_true(map[[id]]$scope %in% .RESERVING_SCOPES)
  }

  ## An analysis outside the reservation carries no entry at all, so nothing
  ## withholds it.
  untouched <- setdiff(model$analyses$id, result$reserved)
  expect_gt(length(untouched), 0L)
  for (id in untouched) expect_null(map[[id]])
})


test_that("an unreadable column header reserves through the grouping", {
  ## The shell-derived chain, followed all the way: a header states a condition
  ## the grammar cannot read -> it is kept as a LEVEL rather than dropped ->
  ## `.build_grouping()` marks it -> the validator raises a GAP -> the
  ## reservation map withholds the analyses on that grouping, and no others.
  ##
  ## Dropping it instead is the failure being prevented: the remaining levels
  ## close the gap and a column shows a different subgroup than its header
  ## claims, with nothing on the page to say so.
  sec <- list(
    tlf_number = "T-14-9-9", tlf_type = "TABLE", title = "Header chain",
    .pending_column_annotations = list(
      labels = c("Cohort A (N=XX)", "Cohort B (N=XX)", "Odd (N=XX)"),
      annotations = c("ADQX.COHORTN=1", "ADQX.COHORTN=2",
                      "ADQX.COHORTN ~= 3")))
  out <- .resolve_table_column_groups(sec)
  out$by_variable <- "COHORTN"
  out$by_variable_dataset <- "ADQX"

  ## The axis keeps its shape: three columns in, three levels out.
  gf <- .build_grouping(out)
  expect_length(gf$groups, 3L)

  ## Put the built levels on a grouping an analysis actually references, so
  ## the reservation has somewhere to land.
  model <- .rmap_model()
  raw <- model$groupings$raw
  raw[[1]]$groups <- gf$groups
  model$groupings$raw <- raw

  findings <- validate_ars_model(model)
  hit <- findings[findings$ref == "GROUPING_LEVEL_CONDITION_UNRESOLVED", ,
                  drop = FALSE]
  expect_equal(nrow(hit), 1L)
  expect_identical(hit$severity[[1]], "GAP")
  expect_identical(hit$detail[[1]], "ADQX.COHORTN ~= 3")

  reserved <- names(.reservations_from_findings(model, findings)$by_analysis)
  expect_gt(length(reserved), 0L)
  ## And an analysis that does not use this grouping still computes.
  expect_gt(length(setdiff(model$analyses$id, reserved)), 0L)
})


test_that("an authored Total that cannot be read marks its analysis", {
  ## The one unreadable condition with nowhere else to go. Every other
  ## unreadable header is also a LEVEL, so the grouping carries it -- but a
  ## Total is never a level (it overlaps them by construction), so without its
  ## own marker nothing would reserve it.
  ##
  ## The dangerous outcome is specific: `total_condition` becomes `totalWhere`
  ## on the analysis, so an unreadable object would be written into the ARS as
  ## though it selected records.
  sec <- list(
    tlf_number = "T-1", tlf_type = "TABLE", title = "Authored total",
    .pending_column_annotations = list(
      labels = c("Cohort A (N=XX)", "Cohort B (N=XX)", "Total (N=XX)"),
      annotations = c("ADQX.COHORTN=1", "ADQX.COHORTN=2",
                      "ADQX.COHORTN ~= 9")))
  out <- .resolve_table_column_groups(sec)

  ## Not the sentinel, and not a clause either: the Total states no condition
  ## this package can act on.
  expect_null(out$total_condition)
  expect_false(.is_unresolved_condition(out$total_condition))
  ## The author's text survives twice over -- as the annotation a reviewer
  ## reads, and as the marker the validator acts on.
  expect_true(nzchar(out$total_annotation %||% ""))
  expect_identical(out$total_unresolved, "ADQX.COHORTN ~= 9")

  ## A readable Total is unaffected, so the branch keys on readability rather
  ## than on the column being a Total.
  ok <- list(
    tlf_number = "T-2", tlf_type = "TABLE", title = "Readable total",
    .pending_column_annotations = list(
      labels = c("Cohort A (N=XX)", "Cohort B (N=XX)", "Total (N=XX)"),
      annotations = c("ADQX.COHORTN=1", "ADQX.COHORTN=2",
                      "ADQX.COHORTN IN (1,2)")))
  fine <- .resolve_table_column_groups(ok)
  expect_false(is.null(fine$total_condition))
  expect_null(fine$total_unresolved)
})


test_that("a figure filter that cannot be read reserves its cell", {
  ## Figures never enter the reporting event -- the filter is a fill-time
  ## directive on the sheet -- so this reservation happens in the fill path and
  ## is proved by running it.
  ##
  ## Two spellings of failure must both reach it. A directive stating no
  ## readable filter was already handled; a filter this grammar cannot read is
  ## NOT null, so before this change it fell through to the evaluator as an
  ## object that is not a WhereClause, and the series would have been computed
  ## over unfiltered data.
  skip_if_not_installed("openxlsx2")

  adam <- withr::local_tempdir()
  utils::write.csv(
    data.frame(USUBJID = c("S1", "S2", "S3", "S4"),
               VISITNUM = c(1, 1, 2, 2),
               SCORE = c(10, 20, 30, 40),
               stringsAsFactors = FALSE),
    file.path(adam, "ADQX.csv"), row.names = FALSE)

  fill_with <- function(filter_txt) {
    directives <- list(
      list(key = "x_axis", value = "ADQX.VISITNUM"),
      list(key = "y_axis", value = "ADQX.SCORE")
    )
    if (!is.null(filter_txt)) {
      directives <- c(directives,
                      list(list(key = "filter", value = filter_txt)))
    }
    ## The sheet is given real cells: the writer puts the series where the
    ## annotation block was, and an empty worksheet has nowhere to put it.
    wb <- openxlsx2::wb_workbook()$add_worksheet("F")
    wb$add_data(sheet = "F",
                x = as.data.frame(matrix("", nrow = 12, ncol = 6)),
                start_row = 1L, col_names = FALSE)
    suppressWarnings(.fill_figure_sheet(
      wb, 1L, list(series_anchor = 5L, directives = directives), adam, "F"))
  }

  ## Non-vacuity, and it is what makes the assertion below mean anything. The
  ## same figure fills successfully with NO filter and with a READABLE one, so
  ## the fixture loads, the axes resolve and the series computes. Only the
  ## unreadable filter is withheld -- which is the whole claim.
  expect_equal(fill_with(NULL)$records[[1]]$status, "filled")
  expect_equal(fill_with("ADQX.VISITNUM=1")$records[[1]]$status, "filled")

  ## And the filter really is unresolved rather than absent, so the branch
  ## under test is the one this change added.
  unreadable <- suppressWarnings(parse_where_clause("ADQX.SCORE >= 16"))
  expect_true(.is_unresolved_condition(unreadable))

  res <- fill_with("ADQX.SCORE >= 16")
  expect_equal(res$records[[1]]$status, "pending")
  ## The exact reason matters as much as the status: every earlier guard in
  ## this path also returns "pending", so only the specific text proves the
  ## filter branch reserved the cell.
  expect_identical(res$records[[1]]$reason,
                   "the figure's filter could not be parsed")
})
