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

test_that("is.na() call form parses to EQ with empty value (numeric missing)", {
  wc <- parse_where_clause("is.na(ADSL.COHORTN)")
  expect_equal(wc$condition$dataset, "ADSL")
  expect_equal(wc$condition$variable, "COHORTN")
  expect_equal(wc$condition$comparator, "EQ")
  expect_length(wc$condition$value, 0)
})

test_that("SAS missing() call form parses to EQ with empty value", {
  wc <- parse_where_clause("missing(ADSL.COHORTN)")
  expect_equal(wc$condition$variable, "COHORTN")
  expect_equal(wc$condition$comparator, "EQ")
  expect_length(wc$condition$value, 0)
})

test_that("!is.na() and 'not missing()' call forms parse to NE (present)", {
  wc1 <- parse_where_clause("!is.na(ADSL.COHORTN)")
  expect_equal(wc1$condition$comparator, "NE")
  expect_length(wc1$condition$value, 0)

  wc2 <- parse_where_clause("not missing(ADSL.COHORTN)")
  expect_equal(wc2$condition$comparator, "NE")
  expect_length(wc2$condition$value, 0)
})

test_that("is.na() combines with another condition into a compound", {
  wc <- parse_where_clause("ADSL.SCRNFL='Y' and is.na(ADSL.COHORTN)")
  expect_equal(wc$compoundExpression$logicalOperator, "AND")
  expect_length(wc$compoundExpression$whereClauses, 2)
  comps <- vapply(wc$compoundExpression$whereClauses,
                  function(c) c$condition$comparator, character(1))
  expect_setequal(comps, c("EQ", "EQ"))
})

test_that("empty / NULL input returns NULL", {
  expect_null(parse_where_clause(""))
  expect_null(parse_where_clause(NULL))
})

test_that("unquoted numeric equality parses (ADSL.COHORTN=1)", {
  wc <- parse_where_clause("ADSL.COHORTN=1")
  expect_equal(wc$condition$dataset, "ADSL")
  expect_equal(wc$condition$variable, "COHORTN")
  expect_equal(wc$condition$comparator, "EQ")
  expect_equal(unlist(wc$condition$value), "1")
})

test_that("a quoted value still wins over the numeric-equality branch", {
  wc <- parse_where_clause("ADSL.SAFFL='Y'")
  expect_equal(wc$condition$comparator, "EQ")
  expect_equal(unlist(wc$condition$value), "Y")
})

test_that("double-equals equality parses like single-equals (numeric)", {
  wc <- parse_where_clause("ADSL.COHORTN==99")
  expect_equal(wc$condition$dataset, "ADSL")
  expect_equal(wc$condition$variable, "COHORTN")
  expect_equal(wc$condition$comparator, "EQ")
  expect_equal(unlist(wc$condition$value), "99")
})

test_that("double-equals equality parses like single-equals (quoted)", {
  wc <- parse_where_clause("ADSL.SCRNFL=='Y'")
  expect_equal(wc$condition$comparator, "EQ")
  expect_equal(unlist(wc$condition$value), "Y")
})

test_that("is.na() OR double-equals is one compound, not just the is.na branch", {
  ## The column-header form the user described: a cohort column defined by
  ## "missing OR the numeric Unknown code". Both branches must survive.
  wc <- parse_where_clause("is.na(ADSL.COHORTN) or ADSL.COHORTN==99")
  expect_equal(wc$compoundExpression$logicalOperator, "OR")
  expect_length(wc$compoundExpression$whereClauses, 2)
  vals <- lapply(wc$compoundExpression$whereClauses,
                 function(c) unlist(c$condition$value))
  ## first branch is the missing check (empty value), second is EQ 99
  expect_length(vals[[1]], 0)
  expect_equal(vals[[2]], "99")
})

test_that("!= is not mangled by the ==-normalisation", {
  wc <- parse_where_clause("ADSL.DCSREAS not missing")
  expect_equal(wc$condition$comparator, "NE")
})
