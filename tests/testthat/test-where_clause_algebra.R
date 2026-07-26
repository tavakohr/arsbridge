# WhereClause algebra: combine_conditions(), canonicalize_condition(),
# conditions_equal(), condition_implies(). These underpin hierarchical column
# trees (leaf condition = AND of ancestors + own; subtotal = parent condition)
# and duplicate-path validation.

.wc <- function(expr) arsbridge:::parse_where_clause(expr)

test_that("combine_conditions drops NULLs and keeps a single clause unchanged", {
  a <- .wc("ADSL.TRTGRPN=1")

  expect_null(arsbridge:::combine_conditions())
  expect_null(arsbridge:::combine_conditions(NULL, NULL))
  expect_identical(arsbridge:::combine_conditions(a), a)
  expect_identical(arsbridge:::combine_conditions(NULL, a, NULL), a)
})

test_that("combine_conditions builds an AND compound and flattens same-operator nesting", {
  a <- .wc("ADSL.TRTGRPN=1")
  b <- .wc("ADSL.SUBGRPN=2")
  c <- .wc("ADSL.COMPFL='Y'")

  ab <- arsbridge:::combine_conditions(a, b)
  expect_identical(ab$compoundExpression$logicalOperator, "AND")
  expect_length(ab$compoundExpression$whereClauses, 2L)

  abc <- arsbridge:::combine_conditions(ab, c)
  expect_length(abc$compoundExpression$whereClauses, 3L)

  # OR members are kept intact inside an AND, not torn apart.
  or_clause <- arsbridge:::combine_conditions(a, b, operator = "OR")
  mixed <- arsbridge:::combine_conditions(or_clause, c)
  expect_length(mixed$compoundExpression$whereClauses, 2L)
  expect_identical(
    mixed$compoundExpression$whereClauses[[1]]$compoundExpression$logicalOperator,
    "OR"
  )
})

test_that("canonicalize_condition is order-insensitive and case-normalizing", {
  ab <- arsbridge:::combine_conditions(.wc("ADSL.TRTGRPN=1"), .wc("ADSL.SUBGRPN=2"))
  ba <- arsbridge:::combine_conditions(.wc("ADSL.SUBGRPN=2"), .wc("ADSL.TRTGRPN=1"))
  expect_identical(
    arsbridge:::canonicalize_condition(ab),
    arsbridge:::canonicalize_condition(ba)
  )

  upper <- .wc("ADSL.SAFFL='Y'")
  lower <- list(condition = list(
    dataset = "adsl", variable = "saffl", comparator = "eq", value = list("Y")
  ))
  expect_true(arsbridge:::conditions_equal(upper, lower))

  # IN value order does not matter.
  in_a <- .wc("ADSL.RACEGR IN ('GROUP A','GROUP B')")
  in_b <- .wc("ADSL.RACEGR IN ('GROUP B','GROUP A')")
  expect_true(arsbridge:::conditions_equal(in_a, in_b))
})

test_that("canonicalize_condition survives a jsonlite round trip", {
  clause <- arsbridge:::combine_conditions(
    .wc("ADSL.TRTGRPN=1"), .wc("ADSL.SUBGRPN=2")
  )
  round_tripped <- jsonlite::fromJSON(
    jsonlite::toJSON(clause, auto_unbox = TRUE),
    simplifyVector = FALSE
  )
  expect_true(arsbridge:::conditions_equal(clause, round_tripped))
})

test_that("conditions_equal distinguishes genuinely different clauses", {
  expect_false(arsbridge:::conditions_equal(.wc("ADSL.TRTGRPN=1"), .wc("ADSL.TRTGRPN=2")))
  expect_false(arsbridge:::conditions_equal(.wc("ADSL.TRTGRPN=1"), NULL))
  expect_true(arsbridge:::conditions_equal(NULL, NULL))
})

test_that("condition_implies recognizes parent-child scope", {
  parent <- .wc("ADSL.TRTGRPN=1")
  child  <- arsbridge:::combine_conditions(parent, .wc("ADSL.SUBGRPN=2"))

  expect_true(arsbridge:::condition_implies(child, parent))
  expect_false(arsbridge:::condition_implies(parent, child))

  # NULL parent (no condition) is implied by anything; a NULL child implies
  # only a NULL parent.
  expect_true(arsbridge:::condition_implies(child, NULL))
  expect_true(arsbridge:::condition_implies(NULL, NULL))
  expect_false(arsbridge:::condition_implies(NULL, parent))

  # A sibling with a different parent value does not imply this parent.
  other <- arsbridge:::combine_conditions(.wc("ADSL.TRTGRPN=2"), .wc("ADSL.SUBGRPN=2"))
  expect_false(arsbridge:::condition_implies(other, parent))
})

test_that("condition_implies is conservative in the presence of OR", {
  parent    <- .wc("ADSL.TRTGRPN=1")
  or_clause <- arsbridge:::combine_conditions(
    .wc("ADSL.TRTGRPN=1"), .wc("ADSL.TRTGRPN=2"),
    operator = "OR"
  )

  # An OR child never implies a plain parent (even though logically the
  # first branch would): conservative FALSE.
  expect_false(arsbridge:::condition_implies(or_clause, parent))
  expect_false(arsbridge:::condition_implies(parent, or_clause))

  # Except structural equality, which is always TRUE.
  expect_true(arsbridge:::condition_implies(or_clause, or_clause))
})
