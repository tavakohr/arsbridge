## where_to_filter_expr(): the WhereClause -> predicate-string emitter that the
## cards emitter uses. These tests pin its output to the SAME logical mask that
## eval_condition()/eval_where_clause() in ars_to_ard.R produce, so emitted
## filtering == executed filtering (Plan B deterministic-equivalence guarantee).

.wtf_df <- data.frame(
  SAFFL  = c("Y", "N", "Y", ""),
  AGE    = c(64, 65, 70, 50),
  RACE   = c("WHITE", "ASIAN", "BLACK", "WHITE"),
  AETERM = c("rash", "headache", "RASH mild", "nausea"),
  DTHDT  = c("2020-01-01", "", NA, "2021-02-02"),
  stringsAsFactors = FALSE
)

## Evaluate the emitted predicate against the toy data frame.
.wtf_mask <- function(annotation) {
  where <- parse_where_clause(annotation)
  eval(parse(text = where_to_filter_expr(where)), envir = .wtf_df)
}

test_that("NULL where -> no filter (TRUE)", {
  expect_identical(where_to_filter_expr(NULL), "TRUE")
})

test_that("EQ / NE reproduce eval_condition masks", {
  expect_equal(.wtf_mask("ADSL.SAFFL='Y'"),   c(TRUE, FALSE, TRUE, FALSE))
  expect_equal(.wtf_mask("ADSL.SAFFL NE 'Y'"), c(FALSE, TRUE, FALSE, TRUE))
})

test_that("IN / NOT IN lists reproduce eval_condition masks", {
  expect_equal(.wtf_mask("ADSL.RACE IN ('WHITE','ASIAN')"),
               c(TRUE, TRUE, FALSE, TRUE))
  expect_equal(.wtf_mask("ADSL.RACE NOT IN ('WHITE')"),
               c(FALSE, TRUE, TRUE, FALSE))
})

test_that("numeric comparators reproduce eval_condition masks", {
  expect_equal(.wtf_mask("ADSL.AGE GE 65"), c(FALSE, TRUE, TRUE, FALSE))
  expect_equal(.wtf_mask("ADSL.AGE LT 65"), c(TRUE, FALSE, FALSE, TRUE))
})

test_that("CONTAINS is case-insensitive substring match", {
  expect_equal(.wtf_mask("ADAE.AETERM contains 'rash'"),
               c(TRUE, FALSE, TRUE, FALSE))
})

test_that("null / not-null checks reproduce eval_condition masks", {
  expect_equal(.wtf_mask("ADSL.DTHDT is null"),     c(FALSE, TRUE, TRUE, FALSE))
  expect_equal(.wtf_mask("ADSL.DTHDT not missing"), c(TRUE, FALSE, FALSE, TRUE))
})

test_that("compound AND / OR join atoms correctly", {
  expect_equal(.wtf_mask("ADSL.SAFFL='Y' and ADSL.AGE GE 65"),
               c(FALSE, FALSE, TRUE, FALSE))
  expect_equal(.wtf_mask("ADSL.SAFFL='Y' or ADSL.AGE GE 65"),
               c(TRUE, TRUE, TRUE, FALSE))
})

test_that("empty compound expression -> TRUE", {
  where <- list(compoundExpression = list(logicalOperator = "AND",
                                          whereClauses = list()))
  expect_identical(where_to_filter_expr(where), "TRUE")
})

test_that("emitted predicates always parse as valid R", {
  for (ann in c("ADSL.SAFFL='Y'", "ADSL.RACE IN ('A','B')", "ADSL.AGE GE 65",
                "ADAE.AETERM contains 'x'", "ADSL.DTHDT is null",
                "ADSL.SAFFL='Y' or ADSL.AGE GE 65")) {
    expr <- where_to_filter_expr(parse_where_clause(ann))
    expect_silent(parse(text = expr))
  }
})
