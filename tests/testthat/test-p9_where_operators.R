## P9: expanded where-clause operators -- BETWEEN, IN lists, unquoted
## numerics, CONTAINS extension, positive null checks.

test_that("BETWEEN becomes a GE/LE compound (ARS-conformant)", {
  diag_reset()
  wc <- parse_where_clause("ADSL.AGE between 18 and 65")
  expect_named(wc, "compoundExpression")
  expect_equal(wc$compoundExpression$logicalOperator, "AND")
  cls <- wc$compoundExpression$whereClauses
  expect_length(cls, 2)
  expect_equal(cls[[1]]$condition$comparator, "GE")
  expect_equal(cls[[1]]$condition$value[[1]], "18")
  expect_equal(cls[[2]]$condition$comparator, "LE")
  expect_equal(cls[[2]]$condition$value[[1]], "65")
  expect_equal(nrow(diag_records()), 0)   ## fully parsed, nothing dropped
})

test_that("BETWEEN with quoted values parses too", {
  wc <- parse_where_clause("ADSL.AGE BETWEEN '18' AND '65'")
  expect_named(wc, "compoundExpression")
  expect_equal(wc$compoundExpression$whereClauses[[1]]$condition$value[[1]], "18")
})

test_that("BETWEEN composes with other clauses without being torn apart", {
  wc <- parse_where_clause("ADSL.SAFFL='Y' and ADSL.AGE between 18 and 65")
  expect_named(wc, "compoundExpression")
  cls <- wc$compoundExpression$whereClauses
  expect_length(cls, 2)
  expect_equal(cls[[1]]$condition$variable, "SAFFL")
  ## Second clause is itself the GE/LE compound.
  expect_named(cls[[2]], "compoundExpression")
})

test_that("IN list captures every value", {
  wc <- parse_where_clause("ADSL.RACE IN ('WHITE', 'ASIAN', 'OTHER')")
  expect_equal(wc$condition$comparator, "IN")
  expect_equal(unlist(wc$condition$value), c("WHITE", "ASIAN", "OTHER"))
})

test_that("NOT IN list maps to NOTIN", {
  wc <- parse_where_clause("ADSL.RACE not in ('UNKNOWN')")
  expect_equal(wc$condition$comparator, "NOTIN")
  expect_equal(unlist(wc$condition$value), "UNKNOWN")
})

test_that("unquoted numeric comparison parses", {
  wc <- parse_where_clause("ADSL.AGE GE 65")
  expect_equal(wc$condition$comparator, "GE")
  expect_equal(wc$condition$value[[1]], "65")
  wc2 <- parse_where_clause("ADLB.AVAL LT 2.5")
  expect_equal(wc2$condition$value[[1]], "2.5")
})

test_that("CONTAINS parses with an INFO diagnostic about the extension", {
  diag_reset()
  wc <- parse_where_clause("ADAE.AETERM contains 'rash'")
  expect_equal(wc$condition$comparator, "CONTAINS")
  expect_equal(wc$condition$value[[1]], "rash")
  recs <- diag_records()
  expect_true(any(recs$severity == "INFO" & grepl("CONTAINS", recs$problem)))
})

test_that("positive and negative null checks both parse", {
  wc <- parse_where_clause("ADSL.DTHDT is null")
  expect_equal(wc$condition$comparator, "EQ")
  expect_length(wc$condition$value, 0)

  wc2 <- parse_where_clause("ADSL.DTHDT missing")
  expect_equal(wc2$condition$comparator, "EQ")

  wc3 <- parse_where_clause("ADSL.DCSREAS is not null")
  expect_equal(wc3$condition$comparator, "NE")
  wc4 <- parse_where_clause("ADSL.DCSREAS not missing")
  expect_equal(wc4$condition$comparator, "NE")
})

test_that("previously supported forms are unchanged", {
  expect_equal(parse_where_clause("ADSL.SAFFL='Y'")$condition$comparator, "EQ")
  expect_equal(parse_where_clause("ADTTE.PARAMCD EQ 'OS'")$condition$value[[1]], "OS")
  wc <- parse_where_clause("ADSL.SAFFL='Y' and ADCM.CONTRTFL='Y'")
  expect_length(wc$compoundExpression$whereClauses, 2)
})

## --- executor evaluation of the new comparators -------------------------------

.p9_exec <- function(method = "MTH_COUNT_AND_PERCENTAGE", where_cond) {
  td <- withr::local_tempdir(.local_envir = parent.frame())
  utils::write.csv(data.frame(
    USUBJID = sprintf("%02d", 1:6),
    AGE     = c(20, 40, 64, 65, 70, 90),
    AETERM  = c("Mild rash", "Headache", "RASH severe", "Nausea",
                "Skin rash on arm", "Fever"),
    SEX     = rep(c("M", "F"), 3)
  ), file.path(td, "adsl.csv"), row.names = FALSE)
  spec <- list(
    analysisSets = list(list(
      id = "AS_TEST", name = "test", label = "test",
      condition = NULL, compoundExpression = NULL
    )),
    dataSubsets = list(list(id = "DS_TEST", name = "t", label = "t",
                            condition = where_cond$condition %||% NULL,
                            compoundExpression = where_cond$compoundExpression %||% NULL)),
    analysisGroupings = list(), methods = list(), outputs = list(),
    analyses = list(list(
      id = "AN_P9", methodId = method,
      dataset = "ADSL", variable = "SEX",
      analysisVariable = list(dataset = "ADSL", variable = "SEX"),
      analysisSetId = "", dataSubsetId = "DS_TEST",
      orderedGroupings = list()
    ))
  )
  p <- file.path(td, "ars.json")
  writeLines(jsonlite::toJSON(spec, auto_unbox = TRUE, null = "null"), p)
  ars_to_ard(p, td)
}

test_that("executor evaluates BETWEEN compound (GE/LE)", {
  skip_if_not_installed("cards")
  wc <- parse_where_clause("ADSL.AGE between 40 and 70")
  ard <- .p9_exec(where_cond = wc)
  expect_false(is.null(ard))
  ## Ages 40, 64, 65, 70 pass -> denominator-free count of SEX over 4 rows.
  n_total <- sum(unlist(ard$stat[ard$stat_name == "n"]))
  expect_equal(n_total, 4)
})

test_that("executor evaluates CONTAINS case-insensitively", {
  skip_if_not_installed("cards")
  wc <- parse_where_clause("ADSL.AETERM contains 'rash'")
  ard <- .p9_exec(where_cond = wc)
  expect_false(is.null(ard))
  ## "Mild rash", "RASH severe", "Skin rash on arm" -> 3 rows.
  n_total <- sum(unlist(ard$stat[ard$stat_name == "n"]))
  expect_equal(n_total, 3)
})

test_that("executor evaluates IN list", {
  skip_if_not_installed("cards")
  wc <- parse_where_clause("ADSL.AGE IN ('20', '90')")
  ard <- .p9_exec(where_cond = wc)
  n_total <- sum(unlist(ard$stat[ard$stat_name == "n"]))
  expect_equal(n_total, 2)
})
