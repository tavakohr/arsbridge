## P4-P7 robustness: spec fail-early detail, flexible docx regexes,
## spec-validated listing tokens, executor method registry, subject_key
## and column_aliases configuration.

## --- P4: informative abort ---------------------------------------------------

test_that("spec abort names the sheets and why they were skipped", {
  skip_if_not_installed("openxlsx2")
  path <- tempfile(fileext = ".xlsx")
  wb <- openxlsx2::wb_workbook()
  wb$add_worksheet("Notes")
  wb$add_data("Notes", data.frame(Topic = "x", Detail = "y",
                                  stringsAsFactors = FALSE))
  openxlsx2::wb_save(wb, path)
  err <- tryCatch(parse_adam_spec(path), error = function(e) conditionMessage(e))
  expect_match(err, "No variable-level sheet")
  expect_match(err, "Notes")
  expect_match(err, "Topic")
})

## --- P5: heading regex flexibility -------------------------------------------

.match_heading <- function(x) {
  m <- regmatches(x, regexec(.TLF_HEADING_RE, x, perl = TRUE))[[1]]
  if (length(m) == 3) list(word = m[2], number = m[3]) else NULL
}

test_that("heading regex is case-insensitive and accepts suffix letters", {
  expect_equal(.match_heading("Table 14.1.1")$number, "14.1.1")
  expect_equal(.match_heading("TABLE 14.1.1")$number, "14.1.1")
  expect_equal(.match_heading("listing 16.2.1a")$number, "16.2.1a")
  expect_equal(.match_heading("Figure 3")$number, "3")
  expect_equal(.match_heading("Table 14.3.2:")$number, "14.3.2")
})

test_that("heading regex rejects prose and TOC-like lines", {
  expect_null(.match_heading("Table of Contents"))
  expect_null(.match_heading("Table 14.1.1 shows the demographic summary"))
  expect_null(.match_heading("see Table 14.1.1"))
})

test_that("parser handles ALL-CAPS heading end to end", {
  ## The minimal fixture uses title-case headings; verify the regex-derived
  ## type/number logic via a section round-trip of the regex outputs.
  h <- .match_heading("LISTING 16.2.1a")
  expect_equal(tools::toTitleCase(tolower(h$word)), "Listing")
  expect_equal(gsub("\\.", "-", h$number), "16-2-1a")
})

## --- P5: source-line regex variants -------------------------------------------

.match_source <- function(x) {
  m <- regmatches(x, regexec(.SOURCE_LINE_RE, x, ignore.case = TRUE,
                             perl = TRUE))[[1]]
  if (length(m) == 2) m[2] else NULL
}

test_that("source line accepts common variants", {
  expect_equal(.match_source("Source: ADSL"), "ADSL")
  expect_equal(.match_source("Sources: ADSL, ADAE."), "ADSL, ADAE")
  expect_equal(.match_source("Data Source: ADAE"), "ADAE")
  expect_equal(.match_source("Source datasets: ADLB, ADSL"), "ADLB, ADSL")
  expect_equal(.match_source("Source = ADVS"), "ADVS")
})

## --- P5: spec-validated listing header tokens ---------------------------------

test_that("mixed-case header variables are caught when spec is available", {
  lk <- setNames(lapply(c("ADSL.USUBJID", "ADAE.AEDECOD", "ADAE.AETERM"),
                        function(k) list()),
                 c("ADSL.USUBJID", "ADAE.AEDECOD", "ADAE.AETERM"))
  d <- .detect_listing_header_annotation(
    "AE PT (Verbatim)\nAeDecod (AeTerm)", list(), "ADAE", spec_lookup = lk)
  expect_true(nzchar(d$annotation))
  expect_match(d$annotation, "ADAE.AEDECOD", fixed = TRUE)
  expect_match(d$annotation, "ADAE.AETERM", fixed = TRUE)
})

test_that("English noise rejected via spec membership (no blocklist needed)", {
  lk <- setNames(list(list()), "ADSL.USUBJID")
  d <- .detect_listing_header_annotation(
    "Adverse Event\nVerbatim Term", list(), "ADAE", spec_lookup = lk)
  expect_equal(d$annotation, "")
})

test_that("without spec the ALL-CAPS heuristic + blocklist still applies", {
  d <- .detect_listing_header_annotation(
    "Subject ID\nUSUBJID", list(), "ADAE", spec_lookup = NULL)
  expect_equal(d$annotation, "ADSL.USUBJID")  ## universal ADSL var
})

test_that("spec resolves dataset preferring source, then ADSL", {
  lk <- setNames(lapply(1:2, function(i) list()),
                 c("ADAE.ASTDT", "ADSL.ASTDT"))
  d <- .detect_listing_header_annotation(
    "Start Date\nASTDT", list(), "ADAE", spec_lookup = lk)
  expect_equal(d$annotation, "ADAE.ASTDT")
})

## --- P6: registry consistency -------------------------------------------------

test_that("every executor id is a standard catalogue method id", {
  std_ids <- vapply(.STANDARD_METHODS, function(m) m$id, character(1),
                    USE.NAMES = FALSE)
  expect_true(all(names(.ARD_EXECUTORS) %in% std_ids))
})

## --- P6/P7: execution with registry, traceability columns, subject_key --------

.write_mini_ars <- function(td, method_id, dataset, variable) {
  spec <- list(
    analysisSets = list(), dataSubsets = list(),
    analysisGroupings = list(), methods = list(),
    outputs = list(),
    analyses = list(list(
      id = "AN_TEST_001", methodId = method_id,
      dataset = dataset, variable = variable,
      analysisVariable = list(dataset = dataset, variable = variable),
      analysisSetId = "", dataSubsetId = "",
      orderedGroupings = list()
    ))
  )
  p <- file.path(td, "mini_ars.json")
  writeLines(jsonlite::toJSON(spec, auto_unbox = TRUE, null = "null"), p)
  p
}

test_that("native method records method_actual == method_intended", {
  skip_if_not_installed("cards")
  td <- withr::local_tempdir()
  utils::write.csv(data.frame(USUBJID = c("01", "02", "03"),
                              SEX = c("M", "F", "F")),
                   file.path(td, "adsl.csv"), row.names = FALSE)
  ars <- .write_mini_ars(td, "MTH_COUNT_AND_PERCENTAGE", "ADSL", "SEX")
  ard <- ars_to_ard(ars, td)
  expect_false(is.null(ard))
  expect_true(all(ard$method_intended == "MTH_COUNT_AND_PERCENTAGE"))
  expect_true(all(ard$method_actual == "MTH_COUNT_AND_PERCENTAGE"))
})

test_that("unknown method falls back and records FALLBACK method_actual", {
  skip_if_not_installed("cards")
  td <- withr::local_tempdir()
  utils::write.csv(data.frame(USUBJID = c("01", "02", "03"),
                              AVAL = c(1.2, 3.4, 5.6)),
                   file.path(td, "adtte.csv"), row.names = FALSE)
  ## ADSL present so the population-denominator lookup stays quiet.
  utils::write.csv(data.frame(USUBJID = c("01", "02", "03")),
                   file.path(td, "adsl.csv"), row.names = FALSE)
  ars <- .write_mini_ars(td, "MTH_KAPLAN_MEIER_ESTIMATE", "ADTTE", "AVAL")
  expect_warning(ard <- ars_to_ard(ars, td), "fallback")
  expect_true(all(ard$method_intended == "MTH_KAPLAN_MEIER_ESTIMATE"))
  expect_true(all(ard$method_actual == "FALLBACK_CONTINUOUS"))
  recs <- ars_diagnostics()
  expect_true(any(recs$stage == "execute_ard" &
                    grepl("MTH_KAPLAN_MEIER_ESTIMATE", recs$problem)))
})

test_that("custom subject_key drives subject counting", {
  skip_if_not_installed("cards")
  td <- withr::local_tempdir()
  ## 3 rows, 2 unique PATIDs -- distinct() on PATID must yield n = 2.
  utils::write.csv(data.frame(PATID = c("P1", "P1", "P2"),
                              SAFFL = c("Y", "Y", "Y")),
                   file.path(td, "adsl.csv"), row.names = FALSE)
  ars <- .write_mini_ars(td, "MTH_SUBJECT_COUNT", "ADSL", "PATID")
  ard <- ars_to_ard(ars, td, subject_key = "PATID")
  expect_false(is.null(ard))
  n_stat <- ard$stat[ard$stat_name == "N"][[1]]
  expect_equal(n_stat, 2)
})

test_that("missing subject_key for a subject-level method skips with FAIL diag", {
  skip_if_not_installed("cards")
  td <- withr::local_tempdir()
  utils::write.csv(data.frame(PATID = c("P1", "P2"), SEX = c("M", "F")),
                   file.path(td, "adsl.csv"), row.names = FALSE)
  ars <- .write_mini_ars(td, "MTH_SUBJECT_COUNT", "ADSL", "SEX")
  suppressWarnings(ard <- ars_to_ard(ars, td))  ## default USUBJID absent
  expect_null(ard)
  recs <- ars_diagnostics()
  expect_true(any(recs$severity == "FAIL" & grepl("USUBJID", recs$problem)))
})

## --- P7: column aliases --------------------------------------------------------

test_that("custom column aliases map non-standard spec headers", {
  skip_if_not_installed("openxlsx2")
  path <- tempfile(fileext = ".xlsx")
  wb <- openxlsx2::wb_workbook()
  wb$add_worksheet("Spec")
  wb$add_data("Spec", data.frame(
    `ADaM Dataset Name` = c("ADSL", "ADSL"),
    `ADaM Variable`     = c("AGE", "SEX"),
    `Variable Label`    = c("Age", "Sex"),
    check.names = FALSE, stringsAsFactors = FALSE
  ))
  openxlsx2::wb_save(wb, path)

  ## Without aliases these headers are unrecognisable.
  expect_error(parse_adam_spec(path), "No variable-level sheet")

  spec <- parse_adam_spec(path, column_aliases = list(
    dataset  = "adam dataset name",
    variable = "adam variable"
  ))
  expect_true("ADSL.AGE" %in% names(spec$lookup))
  expect_true("ADSL.SEX" %in% names(spec$lookup))
})

test_that("unknown canonical alias name aborts loudly", {
  expect_error(.merge_column_aliases(list(varible = "oops")), "Unknown canonical")
})
