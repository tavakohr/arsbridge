## ars_to_code.R: the deterministic {cards} emitter. The emitted script is both
## the deliverable AND the engine, so these tests assert (a) it is pure
## pharmaverse cards (parses; no internal arsbridge/MTH_/load_adam symbols) and
## (b) when sourced it produces a correct ARD slice.

.ac_adsl <- function(td) {
  utils::write.csv(data.frame(
    USUBJID = sprintf("%02d", 1:8),
    TRT01A  = rep(c("Drug A", "Placebo"), each = 4),
    AGEGR1  = rep(c("<65", ">=65"), times = 4),
    RANDFL  = c("Y", "Y", "Y", "N", "Y", "Y", "N", "Y"),
    stringsAsFactors = FALSE
  ), file.path(td, "adsl.csv"), row.names = FALSE)
}

.ac_spec <- function(analyses, groupings = TRUE, subsets = list()) {
  list(
    analysisSets = list(),
    dataSubsets  = subsets,
    analysisGroupings = if (groupings) list(
      list(id = "GF_TRT", name = "TRT01A", groupingVariable = "TRT01A")
    ) else list(),
    methods = list(),
    outputs = list(list(
      id = "OUT_1", name = "T-1",
      referencedAnalysisIds = as.list(vapply(analyses, `[[`, character(1), "id"))
    )),
    analyses = analyses
  )
}

.ac_write <- function(spec, td) {
  p <- file.path(td, "ars.json")
  writeLines(jsonlite::toJSON(spec, auto_unbox = TRUE, null = "null"), p)
  p
}

## Source an emitted script and return its ard_<output> object.
.ac_source <- function(script_path) {
  env <- new.env(parent = globalenv())
  sys.source(script_path, envir = env)
  get(ls(env, pattern = "^ard_")[1], envir = env)
}

test_that("emitted script parses and is free of internal symbols", {
  td <- withr::local_tempdir()
  spec <- .ac_spec(list(list(
    id = "AN_1", methodId = "MTH_COUNT_AND_PERCENTAGE",
    label = "Age group", dataset = "ADSL", variable = "AGEGR1",
    analysisVariable = list(dataset = "ADSL", variable = "AGEGR1"),
    analysisSetId = "", dataSubsetId = "",
    orderedGroupings = list(list(order = 1, groupingId = "GF_TRT",
                                 resultsByGroup = TRUE)),
    includeTotal = TRUE)))
  paths <- write_tlf_code(.ac_write(spec, td), file.path(td, "code"),
                          adam_dir = td)

  expect_length(paths, 1)
  txt <- paste(readLines(paths[[1]]), collapse = "\n")
  expect_silent(parse(text = txt))
  expect_false(grepl("arsbridge", txt))
  expect_false(grepl("MTH_", txt))
  expect_false(grepl("load_adam", txt))
})

test_that("sourced categorical script yields a by-group + total ARD", {
  skip_if_not_installed("cards")
  td <- withr::local_tempdir()
  .ac_adsl(td)
  spec <- .ac_spec(list(list(
    id = "AN_1", methodId = "MTH_COUNT_AND_PERCENTAGE",
    label = "Age group", dataset = "ADSL", variable = "AGEGR1",
    analysisVariable = list(dataset = "ADSL", variable = "AGEGR1"),
    analysisSetId = "", dataSubsetId = "",
    orderedGroupings = list(list(order = 1, groupingId = "GF_TRT",
                                 resultsByGroup = TRUE)),
    includeTotal = TRUE)))
  paths <- write_tlf_code(.ac_write(spec, td), file.path(td, "code"),
                          adam_dir = td)
  ard <- .ac_source(paths[[1]])

  expect_true("group1" %in% names(ard))
  expect_true("TRT01A" %in% ard$group1)
  expect_true("AGEGR1" %in% ard$variable)
  ## includeTotal: ungrouped pass leaves group1 NA on some rows.
  expect_true(any(is.na(ard$group1)))
})

test_that("continuous block parses and summarises (no trailing comma)", {
  skip_if_not_installed("cards")
  td <- withr::local_tempdir()
  utils::write.csv(data.frame(
    USUBJID = sprintf("%02d", 1:8),
    TRT01A  = rep(c("Drug A", "Placebo"), each = 4),
    AGE     = c(64, 65, 70, 50, 55, 60, 75, 80),
    stringsAsFactors = FALSE
  ), file.path(td, "adsl.csv"), row.names = FALSE)
  spec <- .ac_spec(list(list(
    id = "AN_AGE", methodId = "MTH_SUMMARY_STATISTICS_CONTINUOUS",
    label = "Age (years)", dataset = "ADSL", variable = "AGE",
    analysisVariable = list(dataset = "ADSL", variable = "AGE"),
    analysisSetId = "", dataSubsetId = "",
    orderedGroupings = list(list(order = 1, groupingId = "GF_TRT",
                                 resultsByGroup = TRUE)),
    includeTotal = TRUE)))
  paths <- write_tlf_code(.ac_write(spec, td), file.path(td, "code"),
                          adam_dir = td)
  txt <- paste(readLines(paths[[1]]), collapse = "\n")
  expect_silent(parse(text = txt))
  ard <- .ac_source(paths[[1]])
  expect_true("mean" %in% ard$stat_name)
  expect_true("TRT01A" %in% ard$group1)
})

test_that("disposition bare-flag renders as a labelled subject count, not Y", {
  skip_if_not_installed("cards")
  td <- withr::local_tempdir()
  .ac_adsl(td)
  spec <- .ac_spec(
    list(list(
      id = "AN_DISP", methodId = "MTH_SUBJECT_COUNT",
      label = "Randomized", description = "Subjects randomized",
      dataset = "ADSL", variable = "RANDFL",
      analysisVariable = list(dataset = "ADSL", variable = "RANDFL"),
      analysisSetId = "", dataSubsetId = "DS_RAND",
      orderedGroupings = list(list(order = 1, groupingId = "GF_TRT",
                                   resultsByGroup = TRUE)),
      includeTotal = FALSE)),
    subsets = list(list(
      id = "DS_RAND",
      condition = list(dataset = "ADSL", variable = "RANDFL",
                       comparator = "EQ", value = list("Y"))))
  )
  paths <- write_tlf_code(.ac_write(spec, td), file.path(td, "code"),
                          adam_dir = td)
  ard <- .ac_source(paths[[1]])

  ## Row is labelled "Randomized" (the stub), NOT the flag level "Y".
  expect_true("Randomized" %in% unlist(ard$variable_level))
  expect_false("Y" %in% unlist(ard$variable_level))
  ## n counts distinct randomized subjects per arm (Drug A: 3, Placebo: 3).
  ns <- ard[ard$stat_name == "n", ]
  expect_true(all(unlist(ns$stat) == 3))
})

test_that("disposition flag with no Y in the cut tabulates n=0, not an error", {
  skip_if_not_installed("cards")
  td <- withr::local_tempdir()
  ## CROSSFL is all "N" -- the subset has zero rows. A bare string column would
  ## error ("all missing"); the emitted single-level factor must give n = 0.
  utils::write.csv(data.frame(
    USUBJID = sprintf("%02d", 1:8),
    TRT01A  = rep(c("Drug A", "Placebo"), each = 4),
    CROSSFL = rep("N", 8),
    stringsAsFactors = FALSE
  ), file.path(td, "adsl.csv"), row.names = FALSE)
  spec <- .ac_spec(
    list(list(
      id = "AN_X", methodId = "MTH_SUBJECT_COUNT",
      label = "Crossed over", dataset = "ADSL", variable = "CROSSFL",
      analysisVariable = list(dataset = "ADSL", variable = "CROSSFL"),
      analysisSetId = "", dataSubsetId = "DS_X",
      orderedGroupings = list(list(order = 1, groupingId = "GF_TRT",
                                   resultsByGroup = TRUE)),
      includeTotal = FALSE)),
    subsets = list(list(
      id = "DS_X",
      condition = list(dataset = "ADSL", variable = "CROSSFL",
                       comparator = "EQ", value = list("Y"))))
  )
  paths <- write_tlf_code(.ac_write(spec, td), file.path(td, "code"),
                          adam_dir = td)
  ard <- .ac_source(paths[[1]])  # must not error
  expect_true("Crossed over" %in% unlist(ard$variable_level))
  ns <- ard[ard$stat_name == "n", ]
  expect_true(all(unlist(ns$stat) == 0))
})
