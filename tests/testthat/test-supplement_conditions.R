## Supplement v3 typed where-clauses: validation, normalisation, and the small
## readers the downstream builders and diagnostics use. A v3 clause arrives as
## the parsed JSON tree (jsonlite simplifyVector = FALSE), so these helpers
## build clauses the same way.

## --- .supp_where: single conditions ----------------------------------------

test_that(".supp_where normalises a simple EQ condition", {
  x <- list(condition = list(dataset = "adsl", variable = "saffl",
                             comparator = "eq", value = list("Y")))
  res <- .supp_where(x, "c")
  expect_length(res$problems, 0)
  expect_equal(res$where$condition$dataset, "ADSL")
  expect_equal(res$where$condition$variable, "SAFFL")
  expect_equal(res$where$condition$comparator, "EQ")
  expect_equal(res$where$condition$value, list("Y"))
})

test_that(".supp_where keeps an IN list of values", {
  x <- list(condition = list(dataset = "ADSL", variable = "RACE",
                             comparator = "IN", value = list("WHITE", "ASIAN")))
  res <- .supp_where(x)
  expect_length(res$problems, 0)
  expect_equal(res$where$condition$comparator, "IN")
  expect_equal(res$where$condition$value, list("WHITE", "ASIAN"))
})

test_that(".supp_where treats an empty value array as a missing-value test", {
  x <- list(condition = list(dataset = "ADSL", variable = "DTHDT",
                             comparator = "EQ", value = list()))
  res <- .supp_where(x)
  expect_length(res$problems, 0)
  expect_equal(res$where$condition$value, list())
})

test_that(".supp_where coerces numeric values to strings", {
  x <- list(condition = list(dataset = "ADSL", variable = "AGE",
                             comparator = "GE", value = list(65)))
  res <- .supp_where(x)
  expect_length(res$problems, 0)
  expect_identical(res$where$condition$value, list("65"))
})

test_that(".supp_where tolerates a bare condition object with an INFO", {
  x <- list(dataset = "ADSL", variable = "SAFFL", comparator = "EQ",
            value = list("Y"))
  res <- .supp_where(x, "c")
  expect_length(res$problems, 0)
  expect_true(any(grepl("bare condition", res$infos)))
  expect_equal(res$where$condition$variable, "SAFFL")
})

test_that(".supp_where tolerates a scalar (un-arrayed) value with an INFO", {
  x <- list(condition = list(dataset = "ADSL", variable = "SAFFL",
                             comparator = "EQ", value = "Y"))
  res <- .supp_where(x)
  expect_length(res$problems, 0)
  expect_true(any(grepl("wrapped", res$infos)))
  expect_equal(res$where$condition$value, list("Y"))
})

test_that(".supp_where rejects an unknown comparator", {
  x <- list(condition = list(dataset = "ADSL", variable = "SAFFL",
                             comparator = "LIKE", value = list("Y")))
  res <- .supp_where(x, "c")
  expect_null(res$where)
  expect_true(any(grepl("comparator", res$problems)))
})

test_that(".supp_where rejects a condition with no dataset/variable", {
  x <- list(condition = list(comparator = "EQ", value = list("Y")))
  res <- .supp_where(x)
  expect_null(res$where)
  expect_true(any(grepl("dataset", res$problems)))
})

test_that(".supp_where accepts CONTAINS as an extension with an INFO", {
  x <- list(condition = list(dataset = "ADAE", variable = "AETERM",
                             comparator = "CONTAINS", value = list("rash")))
  res <- .supp_where(x)
  expect_length(res$problems, 0)
  expect_equal(res$where$condition$comparator, "CONTAINS")
  expect_true(any(grepl("extension", res$infos)))
})

## --- .supp_where: compound expressions -------------------------------------

test_that(".supp_where builds an AND compound expression", {
  x <- list(compoundExpression = list(
    logicalOperator = "and",
    whereClauses = list(
      list(condition = list(dataset = "ADSL", variable = "SAFFL",
                            comparator = "EQ", value = list("Y"))),
      list(condition = list(dataset = "ADCM", variable = "CONTRTFL",
                            comparator = "EQ", value = list("Y"))))))
  res <- .supp_where(x)
  expect_length(res$problems, 0)
  expect_equal(res$where$compoundExpression$logicalOperator, "AND")
  expect_length(res$where$compoundExpression$whereClauses, 2)
})

test_that(".supp_where fails an AND with fewer than two sub-clauses", {
  x <- list(compoundExpression = list(
    logicalOperator = "AND",
    whereClauses = list(
      list(condition = list(dataset = "ADSL", variable = "SAFFL",
                            comparator = "EQ", value = list("Y"))))))
  res <- .supp_where(x)
  expect_null(res$where)
  expect_true(any(grepl("at least two", res$problems)))
})

test_that(".supp_where propagates a child problem out of a compound", {
  x <- list(compoundExpression = list(
    logicalOperator = "OR",
    whereClauses = list(
      list(condition = list(dataset = "ADSL", variable = "SAFFL",
                            comparator = "EQ", value = list("Y"))),
      list(condition = list(dataset = "ADSL", variable = "SEX",
                            comparator = "BOGUS", value = list("M"))))))
  res <- .supp_where(x)
  expect_null(res$where)
  expect_true(any(grepl("comparator", res$problems)))
})

## --- .supp_where: NOT handling ---------------------------------------------

test_that(".supp_where rewrites NOT over a single condition to the negated comparator", {
  x <- list(compoundExpression = list(
    logicalOperator = "NOT",
    whereClauses = list(
      list(condition = list(dataset = "ADSL", variable = "SAFFL",
                            comparator = "EQ", value = list("Y"))))))
  res <- .supp_where(x)
  expect_length(res$problems, 0)
  expect_null(res$where$compoundExpression)
  expect_equal(res$where$condition$comparator, "NE")
  expect_true(any(grepl("NOT rewritten", res$infos)))
})

test_that(".supp_where rejects NOT over a compound expression", {
  inner <- list(compoundExpression = list(
    logicalOperator = "AND",
    whereClauses = list(
      list(condition = list(dataset = "ADSL", variable = "SAFFL",
                            comparator = "EQ", value = list("Y"))),
      list(condition = list(dataset = "ADSL", variable = "SEX",
                            comparator = "EQ", value = list("M"))))))
  x <- list(compoundExpression = list(
    logicalOperator = "NOT", whereClauses = list(inner)))
  res <- .supp_where(x)
  expect_null(res$where)
  expect_true(any(grepl("NOT over a compound", res$problems)))
})

## --- .where_refs / .where_flat ---------------------------------------------

test_that(".where_refs collects every DATASET.VARIABLE in a compound clause", {
  res <- .supp_where(list(compoundExpression = list(
    logicalOperator = "AND",
    whereClauses = list(
      list(condition = list(dataset = "ADSL", variable = "SAFFL",
                            comparator = "EQ", value = list("Y"))),
      list(condition = list(dataset = "ADAE", variable = "TRTEMFL",
                            comparator = "EQ", value = list("Y")))))))
  expect_setequal(.where_refs(res$where), c("ADSL.SAFFL", "ADAE.TRTEMFL"))
})

test_that(".where_flat returns the single-condition shape and NULL for compounds", {
  single <- .supp_where(list(condition = list(dataset = "ADSL",
    variable = "EOSSTT", comparator = "EQ", value = list("COMPLETED"))))$where
  flat <- .where_flat(single)
  expect_equal(flat$dataset, "ADSL")
  expect_equal(flat$variable, "EOSSTT")
  expect_equal(flat$comparator, "EQ")
  expect_equal(flat$value, list("COMPLETED"))

  compound <- .supp_where(list(compoundExpression = list(
    logicalOperator = "OR",
    whereClauses = list(
      list(condition = list(dataset = "ADSL", variable = "A",
                            comparator = "EQ", value = list("1"))),
      list(condition = list(dataset = "ADSL", variable = "B",
                            comparator = "EQ", value = list("2")))))))$where
  expect_null(.where_flat(compound))
})

## --- .where_to_annotation round-trip ---------------------------------------

test_that(".where_to_annotation round-trips through parse_where_clause", {
  clauses <- list(
    list(condition = list(dataset = "ADSL", variable = "EOSSTT",
                          comparator = "EQ", value = list("COMPLETED"))),
    list(condition = list(dataset = "ADSL", variable = "DTHFL",
                          comparator = "NE", value = list("Y"))),
    list(condition = list(dataset = "ADSL", variable = "AGE",
                          comparator = "GE", value = list("65"))),
    list(condition = list(dataset = "ADSL", variable = "RACE",
                          comparator = "IN", value = list("WHITE", "ASIAN"))),
    list(condition = list(dataset = "ADSL", variable = "RACE",
                          comparator = "NOTIN", value = list("OTHER", "UNKNOWN"))),
    list(condition = list(dataset = "ADSL", variable = "DTHDT",
                          comparator = "EQ", value = list())),
    list(condition = list(dataset = "ADSL", variable = "DTHDT",
                          comparator = "NE", value = list()))
  )
  for (raw in clauses) {
    norm <- .supp_where(raw)$where
    round <- parse_where_clause(.where_to_annotation(norm))
    expect_equal(round, norm)
  }
})

test_that(".where_to_annotation joins a compound with and/or words", {
  and <- .supp_where(list(compoundExpression = list(
    logicalOperator = "AND",
    whereClauses = list(
      list(condition = list(dataset = "ADSL", variable = "SAFFL",
                            comparator = "EQ", value = list("Y"))),
      list(condition = list(dataset = "ADSL", variable = "ITTFL",
                            comparator = "EQ", value = list("Y")))))))$where
  expect_match(.where_to_annotation(and), " and ")
})

## --- .method_name_from_id --------------------------------------------------

test_that(".method_name_from_id reverses the catalogue and returns NULL otherwise", {
  expect_equal(.method_name_from_id("MTH_COUNT_AND_PERCENTAGE"),
               "Count and Percentage")
  expect_equal(.method_name_from_id("MTH_LISTING"), "Listing")
  expect_null(.method_name_from_id("MTH_NOT_A_METHOD"))
})

## --- .V3_TYPE_MAP sanity ---------------------------------------------------

test_that(".V3_TYPE_MAP folds every v3 family to an engine family", {
  expect_setequal(names(.V3_TYPE_MAP), .SUPPLEMENT_V3_ANALYSIS_TYPES)
  expect_true(all(.V3_TYPE_MAP %in% .SUPPLEMENT_ANALYSIS_TYPES))
  expect_equal(unname(.V3_TYPE_MAP["MIXED_SUMMARY"]), "CONTINUOUS")
  expect_equal(unname(.V3_TYPE_MAP["SHIFT_TABLE"]), "OTHER")
})
