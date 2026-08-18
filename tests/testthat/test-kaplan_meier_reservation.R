## Kaplan-Meier is reserved, not approximated.
##
## General defect class: a method arsbridge cannot execute was in neither the
## executor registry nor the declared-unexecutable descriptor table, so it fell
## through to the generic fallback summarizer. A row asking for median survival
## and its 95% confidence interval came back as an ordinary mean or count,
## computed under a different method id.
##
## That is the worst available outcome -- a real, correctly formatted number of
## a statistic nobody asked for. `method_actual` recorded the substitution, but
## the filled cell did not, and the post-execution coverage check deliberately
## skips a substituted method because its operation set legitimately differs.
## So nothing anywhere said the number was the wrong statistic.
##
## General invariant: a method with no executor RESERVES. It emits one keyed
## `manual_pending` row per statistic its own operations declare, per requested
## group, and the name it reserves under is the name the cell map derives from
## those same operations -- because the two meet on that string, and a
## disagreement does not fail loudly, it simply never matches.

KM <- "MTH_KAPLAN_MEIER_ESTIMATE"

.km_adam <- function(dir) {
  subjects <- data.frame(
    USUBJID = sprintf("S%02d", 1:8),
    TRT01A  = rep(c("Drug A", "Placebo"), each = 4),
    SAFFL   = rep("Y", 8),
    AVAL    = c(12, 18, 24, 30, 9, 15, 21, 27),
    CNSR    = c(0, 0, 1, 0, 0, 1, 0, 0),
    stringsAsFactors = FALSE)
  utils::write.csv(subjects, file.path(dir, "ADSL.csv"), row.names = FALSE)
  utils::write.csv(subjects, file.path(dir, "ADTTE.csv"), row.names = FALSE)
  dir
}

.km_ars <- function() {
  spec <- list(
    id = "MOCK", name = "Mock", version = "1",
    analysisSets = list(list(id = "AS_ITT", name = "ITT",
      condition = list(dataset = "ADTTE", variable = "SAFFL",
                       comparator = "EQ", value = list("Y")))),
    analysisGroupings = list(list(id = "GF_TRT", name = "TRT01A",
      groupingVariable = list(dataset = "ADTTE", variable = "TRT01A"),
      dataDriven = TRUE)),
    methods = list(list(id = KM, name = "Kaplan-Meier Estimate")),
    analyses = list(list(
      id = "AN_KM", name = "Time to event", analysisSetId = "AS_ITT",
      methodId = KM,
      analysisVariable = list(dataset = "ADTTE", variable = "AVAL"),
      orderedGroupings = list(list(groupingId = "GF_TRT")))),
    outputs = list(list(id = "T_14_3_1", name = "T-14.3.1",
      referencedAnalysisIds = list("AN_KM")))
  )
  path <- tempfile("ars_km_", fileext = ".json")
  writeLines(jsonlite::toJSON(spec, auto_unbox = TRUE, null = "null"), path)
  path
}


test_that("the cell map and the reserved stub name the same statistics", {
  ## The join key. These two are derived by different code from the same
  ## operations, and if they drift the lookup simply misses -- a deliberately
  ## reserved cell then reports having no result at all, which reads as a
  ## failed analysis rather than as somebody's job.
  checked <- 0L
  for (m in .STANDARD_METHODS) {
    desc <- .UNEXECUTABLE_METHODS[[m$id]]
    if (is.null(desc)) next
    cell <- vapply(m$operations,
                   function(o) .operation_stat_names(m$id, o$id, o$name)[[1]],
                   character(1))
    expect_setequal(cell, desc$stats)
    checked <- checked + 1L
  }
  ## Assert the scope: a table that stopped overlapping would pass vacuously.
  expect_gt(checked, 0L)

  ## And specifically for Kaplan-Meier, in order.
  m <- Filter(function(x) identical(x$id, KM), .STANDARD_METHODS)[[1]]
  expect_equal(
    vapply(m$operations,
           function(o) .operation_stat_names(KM, o$id, o$name)[[1]],
           character(1)),
    c("events", "median", "conf.low", "conf.high"))
})


test_that("a confidence limit is not asked for by its display label", {
  ## Before this, `OP_CI_LOW` had no entry and fell through to the lower-cased
  ## operation NAME -- so the ARD was asked for "95% ci lower", a column
  ## heading no engine emits and one that changes the moment somebody relabels
  ## the display.
  expect_equal(.operation_stat_names(KM, "OP_CI_LOW", "95% CI Lower"),
               "conf.low")
  expect_equal(.operation_stat_names(KM, "OP_CI_HIGH", "95% CI Upper"),
               "conf.high")
  expect_false(grepl("95%", .operation_stat_names(KM, "OP_CI_LOW",
                                                  "95% CI Lower")))

  ## One vocabulary for a confidence limit across the package: the proportion
  ## methods already reserve under these names.
  expect_true(all(c("conf.low", "conf.high") %in%
                    .UNEXECUTABLE_METHODS[["MTH_PROPORTION_CI_EXACT"]]$stats))
})


test_that("Kaplan-Meier reserves keyed rows instead of being approximated", {
  skip_if_not_installed("cards")
  adam <- .km_adam(withr::local_tempdir())
  ard  <- suppressMessages(suppressWarnings(ars_to_ard(.km_ars(), adam)))
  km   <- ard[ard$analysis_id == "AN_KM", , drop = FALSE]

  ## Four statistics, both arms: eight reserved cells, nothing computed.
  arms <- unique(unlist(km$group1_level))
  expect_setequal(arms, c("Drug A", "Placebo"))
  expect_equal(nrow(km), 8L)
  expect_equal(unique(km$result_status), "manual_pending")
  expect_setequal(unique(as.character(km$stat_name)),
                  c("events", "median", "conf.low", "conf.high"))
  expect_true(all(is.na(unlist(km$stat))))

  ## The defect, asserted as a regression: the method was NOT substituted.
  ## A fallback would leave method_actual naming a summarizer, and the cells
  ## would carry a real mean where a median survival was asked for.
  expect_equal(unique(as.character(km$method_intended)), KM)
  expect_equal(unique(as.character(km$method_actual)), KM)
  expect_false(any(as.character(km$stat_name) %in% c("mean", "sd", "N")))

  ## And it is somebody's job, on the list of jobs.
  wl <- ars_manual_worklist(ard)
  expect_equal(nrow(wl), 8L)
  expect_equal(unique(wl$method_id), KM)
  expect_setequal(unique(wl$stat_name),
                  c("events", "median", "conf.low", "conf.high"))
})


test_that("a Kaplan-Meier cell reports a reservation, not a missing result", {
  ## The whole point, at the layer the reader sees. A cell bound to one of the
  ## method's operations must resolve to the reservation -- "reserved for
  ## manual derivation" -- and not to "no result in the ARD for this cell",
  ## which is the wording for an analysis that failed.
  skip_if_not_installed("cards")
  adam <- .km_adam(withr::local_tempdir())
  ard  <- suppressMessages(suppressWarnings(ars_to_ard(.km_ars(), adam)))
  idx  <- .ard_index(as.data.frame(ard))

  for (op in c("OP_EVENTS", "OP_MEDIAN", "OP_CI_LOW", "OP_CI_HIGH")) {
    key <- .operation_stat_names(KM, op, op)
    hit <- .ard_value(idx, "AN_KM", group_level = "Drug A",
                      variable_level = NA, stat_name = key)
    expect_equal(hit$status, "manual_pending", info = op)
    expect_equal(.pending_reason(hit$status), "reserved for manual derivation",
                 info = op)
  }

  ## A statistic the method never declared still finds nothing, so the
  ## reservation has not been turned into a catch-all.
  miss <- .ard_value(idx, "AN_KM", "Drug A", NA, "p75")
  expect_equal(miss$status, "no_row")
})
