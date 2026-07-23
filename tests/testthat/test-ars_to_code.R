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

test_that("emitted loader finds and reads .sas7bdat (alongside .xpt / .csv)", {
  code <- arsbridge:::.loader_line("ADSL")
  ## The filename pattern and a dedicated reader branch cover sas7bdat.
  expect_match(code, "(xpt|sas7bdat|csv)", fixed = TRUE)
  expect_match(code, "haven::read_sas(f)", fixed = TRUE)

  ## Functional: a sas7bdat cut is located case-insensitively and read.
  skip_if_not_installed("haven")
  td <- withr::local_tempdir()
  ## write_sas() only builds the fixture; it is deprecated (haven 2.5.2), so
  ## silence its warning and skip if a future haven drops it -- the reader
  ## branch (read_sas) is the behaviour under test.
  built <- tryCatch({
    suppressWarnings(haven::write_sas(
      data.frame(USUBJID = c("01", "02"), AGE = c(40, 50),
                 stringsAsFactors = FALSE),
      file.path(td, "adsl.sas7bdat")))
    TRUE
  }, error = function(e) FALSE)
  skip_if_not(built, "haven::write_sas() unavailable to build the sas7bdat fixture")
  adam_dir <- td
  ADSL <- eval(parse(text = code))
  expect_s3_class(ADSL, "data.frame")
  expect_equal(nrow(ADSL), 2)
  expect_true(all(c("USUBJID", "AGE") %in% names(ADSL)))
})

## --- Codelist decode emission ------------------------------------------------

.ac_decode_meta <- function() {
  list(value_decodes = list(
    "ADSL.DCSREASN" = list(
      list(value = "1", label = "DEATH",             order = 1),
      list(value = "2", label = "LOST TO FOLLOW-UP", order = 2),
      list(value = "3", label = "OTHER",             order = 3)
    )
  ))
}

test_that("a shipped decode emits a factor derivation and decodes the ARD", {
  td <- withr::local_tempdir()
  utils::write.csv(data.frame(
    USUBJID  = sprintf("%02d", 1:8),
    TRT01A   = rep(c("Drug A", "Placebo"), each = 4),
    DCSREASN = c(1, 1, 2, NA, 1, NA, NA, NA),
    stringsAsFactors = FALSE
  ), file.path(td, "adsl.csv"), row.names = FALSE)

  spec <- .ac_spec(list(list(
    id = "AN_1", methodId = "MTH_COUNT_AND_PERCENTAGE",
    label = "Discontinuation reason", dataset = "ADSL", variable = "DCSREASN",
    analysisVariable = list(dataset = "ADSL", variable = "DCSREASN"),
    analysisSetId = "", dataSubsetId = "",
    orderedGroupings = list(list(order = 1, groupingId = "GF_TRT",
                                 resultsByGroup = TRUE)),
    includeTotal = FALSE)))
  spec$`_meta` <- .ac_decode_meta()

  paths <- write_tlf_code(.ac_write(spec, td), file.path(td, "code"),
                          adam_dir = td)
  txt <- paste(readLines(paths[[1]]), collapse = "\n")

  ## The deliverable carries the decode as plain readable cards code.
  expect_true(grepl("factor(", txt, fixed = TRUE))
  expect_true(grepl('as.character(DCSREASN)', txt, fixed = TRUE))
  expect_true(grepl('"DEATH"', txt, fixed = TRUE))

  ## Sourcing it yields decoded levels, including the unobserved one (n = 0).
  ard <- .ac_source(paths[[1]])
  lv <- vapply(ard$variable_level, function(x)
    if (length(x)) as.character(x[[1]]) else NA_character_, character(1))
  expect_true(all(c("DEATH", "LOST TO FOLLOW-UP", "OTHER") %in% lv))
  n_other <- unlist(ard$stat[lv == "OTHER" & ard$stat_name == "n"])
  expect_true(all(n_other == 0))
})

test_that("the decode never touches continuous or bare-flag blocks", {
  td <- withr::local_tempdir()
  .ac_adsl(td)

  ## Continuous method on a variable that (wrongly) carries a decode entry:
  ## the emitted block must keep as.numeric, not factor.
  spec <- .ac_spec(list(list(
    id = "AN_AGE", methodId = "MTH_SUMMARY_STATISTICS_CONTINUOUS",
    label = "Age group", dataset = "ADSL", variable = "AGEGR1",
    analysisVariable = list(dataset = "ADSL", variable = "AGEGR1"),
    analysisSetId = "", dataSubsetId = "",
    orderedGroupings = list(), includeTotal = FALSE)))
  spec$`_meta` <- list(value_decodes = list(
    "ADSL.AGEGR1" = list(list(value = "<65", label = "Under 65", order = 1))
  ))
  paths <- write_tlf_code(.ac_write(spec, td), file.path(td, "code"),
                          adam_dir = td)
  txt <- paste(readLines(paths[[1]]), collapse = "\n")
  expect_false(grepl('"Under 65"', txt, fixed = TRUE))

  ## Bare-flag disposition count: the flag's own value never displays, so a
  ## decode on it must not emit either.
  spec2 <- .ac_spec(list(list(
    id = "AN_RAND", methodId = "MTH_SUBJECT_COUNT",
    label = "Randomized", dataset = "ADSL", variable = "RANDFL",
    analysisVariable = list(dataset = "ADSL", variable = "RANDFL"),
    analysisSetId = "", dataSubsetId = "DS_RAND",
    orderedGroupings = list(), includeTotal = FALSE)),
    subsets = list(list(id = "DS_RAND",
      condition = list(dataset = "ADSL", variable = "RANDFL",
                       comparator = "EQ", value = list("Y")))))
  spec2$`_meta` <- list(value_decodes = list(
    "ADSL.RANDFL" = list(list(value = "Y", label = "Yes", order = 1))
  ))
  paths2 <- write_tlf_code(.ac_write(spec2, td), file.path(td, "code2"),
                           adam_dir = td)
  txt2 <- paste(readLines(paths2[[1]]), collapse = "\n")
  expect_false(grepl('"Yes"', txt2, fixed = TRUE))
})
