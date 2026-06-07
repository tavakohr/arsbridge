test_that("simple equality builds a single Condition", {
  wc <- parse_where_clause("ADSL.SAFFL='Y'")
  expect_named(wc, "condition")
  expect_equal(wc$condition$dataset,    "ADSL")
  expect_equal(wc$condition$variable,   "SAFFL")
  expect_equal(wc$condition$comparator, "EQ")
  expect_equal(wc$condition$value[[1]], "Y")
})

test_that("ARS comparator form parses comparator correctly", {
  wc <- parse_where_clause("ADTTE.PARAMCD EQ 'OS'")
  expect_equal(wc$condition$variable,   "PARAMCD")
  expect_equal(wc$condition$comparator, "EQ")
  expect_equal(wc$condition$value[[1]], "OS")
})

test_that("AND compound expression wraps two conditions", {
  wc <- parse_where_clause("ADSL.SAFFL='Y' and ADCM.CONTRTFL='Y'")
  expect_named(wc, "compoundExpression")
  expect_equal(wc$compoundExpression$logicalOperator, "AND")
  expect_length(wc$compoundExpression$whereClauses, 2)
})

test_that("OR compound expression detected", {
  wc <- parse_where_clause("ADSL.SFENRLFL='Y' or ADSL.WTHTYP='Withdrawal'")
  expect_equal(wc$compoundExpression$logicalOperator, "OR")
})

test_that("not null produces NE with empty value", {
  wc <- parse_where_clause("ADSL.DCSREAS not null")
  expect_equal(wc$condition$comparator, "NE")
  expect_length(wc$condition$value, 0)
})

test_that("empty / NULL input returns NULL", {
  expect_null(parse_where_clause(""))
  expect_null(parse_where_clause(NULL))
})
