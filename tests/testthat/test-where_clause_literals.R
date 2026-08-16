## A quoted value is data, not grammar.
##
## THE DEFECT CLASS. `parse_where_clause()` reasoned about the structure of an
## expression -- where clauses join, which operator applies, where a range ends
## -- by matching patterns against raw text, quoted values included. So text a
## author wrote as a VALUE could be read as GRAMMAR. A value containing a
## joiner word split the expression at the wrong place; neither half then
## parsed; and the condition was dropped.
##
## Why that is the worst possible failure here: a dropped condition raises no
## error and produces no empty cell. It produces an UNRESTRICTED count, which
## is indistinguishable from a correct one by looking at it. The same door
## existed four times over -- the `==` normaliser, the boilerplate strip, the
## BETWEEN protection and the joiner split all read raw text.
##
## THE INVARIANT. Structural analysis reads masked text in which every quoted
## literal is one opaque token; literals are restored before any clause is
## interpreted. Whatever a value contains -- a joiner word, a comparison
## operator, a quote of the other kind -- it can never change how the
## expression is divided, and it must survive into the condition unaltered.
##
## The identifiers below are invented and belong to no study in this repo. Two
## disjoint vocabularies are used so the metamorphic test can rename everything
## and prove the behaviour keys on structure rather than on familiar names.

## Vocabulary A and vocabulary B name nothing in common: different datasets,
## different variables, different values.
.WCL_VOCAB_A <- list(
  ds = "ADQX", var = "MEASURE", flag_ds = "ADQX", flag = "GROUPFL",
  joined_or = "AMBER OR TEAL RESPONSE",
  joined_and = "SALT AND PEPPER GRADE",
  plain = "AMBER"
)
.WCL_VOCAB_B <- list(
  ds = "ADZZTMP", var = "OUTCOME", flag_ds = "ADZZTMP", flag = "COHRTFL",
  joined_or = "IRON OR COPPER STATUS",
  joined_and = "NORTH AND SOUTH REGION",
  plain = "IRON"
)

## The parsed shape, reduced to what the assertions care about. Returned as a
## plain list so two vocabularies can be compared structurally.
.wcl_shape <- function(wc) {
  ## Three outcomes, and keeping them distinct is the point of B1b: NULL means
  ## no condition was supplied, `unresolved` means one was supplied and could
  ## not be read, and anything else is a clause.
  if (.is_unresolved_condition(wc)) {
    return(list(kind = "unresolved", text = .unresolved_condition_text(wc)))
  }
  if (is.null(wc)) return(list(kind = "absent"))
  if (!is.null(wc$condition)) {
    return(list(kind = "condition",
                comparator = wc$condition$comparator,
                values = unlist(wc$condition$value) %||% character(0)))
  }
  ce <- wc$compoundExpression
  list(kind = "compound", operator = ce$logicalOperator,
       n = length(ce$whereClauses))
}

.wcl_parse <- function(expr) {
  suppressWarnings(parse_where_clause(expr))
}


test_that("a joiner word inside a value does not split the expression", {
  ## The defect, in both quote styles and for both joiner words. Each of these
  ## used to parse as NULL -- the condition dropped, the count unrestricted.
  for (vocab in list(.WCL_VOCAB_A, .WCL_VOCAB_B)) {
    for (value in c(vocab$joined_or, vocab$joined_and)) {
      for (quote in c("'", "\"")) {
        expr <- sprintf("%s.%s=%s%s%s",
                        vocab$ds, vocab$var, quote, value, quote)
        shape <- .wcl_shape(.wcl_parse(expr))

        expect_identical(shape$kind, "condition", info = expr)
        expect_identical(shape$comparator, "EQ", info = expr)
        ## The value survives whole, including the joiner word inside it.
        expect_identical(shape$values, value, info = expr)
      }
    }
  }
})


test_that("a comparison operator inside a value is data, not an operator", {
  ## The second half of the same defect: operator-shaped text inside a literal
  ## defeated operator-shaped matching, or was rewritten by the `==`
  ## normaliser. Both must leave the value untouched.
  vocab <- .WCL_VOCAB_A
  operator_values <- c(">= WEEK 12", "A==B", "A=B", "<5 PERCENT", "A>B")

  for (value in operator_values) {
    expr <- sprintf("%s.%s='%s'", vocab$ds, vocab$var, value)
    shape <- .wcl_shape(.wcl_parse(expr))

    expect_identical(shape$kind, "condition", info = expr)
    ## Byte-for-byte: `==` inside a value must not be normalised to `=`.
    expect_identical(shape$values, value, info = expr)
  }
})


test_that("a real joiner between two clauses still splits", {
  ## The other side of the invariant, and the reason this cannot be fixed by
  ## simply not splitting: structure outside literals must still be read.
  for (vocab in list(.WCL_VOCAB_A, .WCL_VOCAB_B)) {
    conjunction <- sprintf("%s.%s='%s' and %s.%s='Y'",
                           vocab$ds, vocab$var, vocab$plain,
                           vocab$flag_ds, vocab$flag)
    disjunction <- sprintf("%s.%s='%s' or %s.%s='Y'",
                           vocab$ds, vocab$var, vocab$plain,
                           vocab$flag_ds, vocab$flag)

    and_shape <- .wcl_shape(.wcl_parse(conjunction))
    expect_identical(and_shape$kind, "compound", info = conjunction)
    expect_identical(and_shape$operator, "AND", info = conjunction)
    expect_identical(and_shape$n, 2L, info = conjunction)

    or_shape <- .wcl_shape(.wcl_parse(disjunction))
    expect_identical(or_shape$kind, "compound", info = disjunction)
    expect_identical(or_shape$operator, "OR", info = disjunction)
    expect_identical(or_shape$n, 2L, info = disjunction)
  }
})


test_that("a joiner inside a value and a real joiner coexist in one clause", {
  ## The case that separates masking from any cheaper trick: the expression
  ## contains BOTH a joiner word inside a literal AND a genuine joiner between
  ## clauses. Exactly one split is correct.
  vocab <- .WCL_VOCAB_A
  expr <- sprintf("%s.%s='%s' and %s.%s='Y'",
                  vocab$ds, vocab$var, vocab$joined_or,
                  vocab$flag_ds, vocab$flag)

  wc <- .wcl_parse(expr)
  shape <- .wcl_shape(wc)
  expect_identical(shape$kind, "compound")
  expect_identical(shape$operator, "AND")
  expect_identical(shape$n, 2L)

  ## And the literal that contains " OR " arrived intact in the first clause,
  ## rather than being torn and silently discarded.
  first <- wc$compoundExpression$whereClauses[[1]]
  expect_identical(unlist(first$condition$value), vocab$joined_or)
})


test_that("ranges and value lists survive a literal containing a joiner", {
  ## BETWEEN protects its own inner "and" with a marker, and IN lists are
  ## comma-separated -- both ran on raw text and both are now masked.
  vocab <- .WCL_VOCAB_A

  ranged <- sprintf("%s.%s between '%s' and '%s'",
                    vocab$ds, vocab$var, vocab$joined_and, vocab$joined_or)
  range_shape <- .wcl_shape(.wcl_parse(ranged))
  expect_identical(range_shape$kind, "compound", info = ranged)
  expect_identical(range_shape$operator, "AND", info = ranged)
  expect_identical(range_shape$n, 2L, info = ranged)

  listed <- sprintf("%s.%s IN ('%s','%s')",
                    vocab$ds, vocab$var, vocab$joined_or, vocab$plain)
  list_shape <- .wcl_shape(.wcl_parse(listed))
  expect_identical(list_shape$kind, "condition", info = listed)
  expect_identical(list_shape$comparator, "IN", info = listed)
  expect_identical(list_shape$values,
                   c(vocab$joined_or, vocab$plain), info = listed)
})


test_that("renaming every identifier and value changes nothing structural", {
  ## The metamorphic proof. The same expressions in two disjoint vocabularies
  ## must produce structurally identical results -- which is what shows the
  ## parser keys on the shape of the expression and not on any name it has
  ## seen before.
  build <- function(vocab) {
    c(
      sprintf("%s.%s='%s'", vocab$ds, vocab$var, vocab$joined_or),
      sprintf("%s.%s=\"%s\"", vocab$ds, vocab$var, vocab$joined_and),
      sprintf("%s.%s='%s' and %s.%s='Y'", vocab$ds, vocab$var, vocab$plain,
              vocab$flag_ds, vocab$flag),
      sprintf("%s.%s IN ('%s','%s')", vocab$ds, vocab$var,
              vocab$joined_or, vocab$plain),
      sprintf("%s.%s between '%s' and '%s'", vocab$ds, vocab$var,
              vocab$joined_and, vocab$joined_or)
    )
  }

  a_exprs <- build(.WCL_VOCAB_A)
  b_exprs <- build(.WCL_VOCAB_B)
  expect_length(a_exprs, length(b_exprs))
  ## The two vocabularies really are disjoint, or "identical structure" would
  ## be trivially true because the inputs were the same.
  expect_length(intersect(a_exprs, b_exprs), 0L)

  ## Compare the structure only: kinds, operators and arities, never values,
  ## since the values are deliberately different between vocabularies.
  structural <- function(expr) {
    shape <- .wcl_shape(.wcl_parse(expr))
    shape$values <- NULL
    shape
  }
  for (i in seq_along(a_exprs)) {
    expect_identical(structural(a_exprs[i]), structural(b_exprs[i]),
                     info = paste(a_exprs[i], "vs", b_exprs[i]))
  }
  ## Non-vacuity: nothing in either vocabulary was dropped, so the comparison
  ## above is between real parses rather than between two NULLs.
  kinds <- vapply(c(a_exprs, b_exprs),
                  function(e) .wcl_shape(.wcl_parse(e))$kind, character(1))
  expect_false(any(kinds %in% c("absent", "unresolved")))
  expect_equal(length(kinds), 10L)
})


test_that("a value containing the marker characters is preserved, not edited", {
  ## The masking needs delimiters that do not occur in the expression, and the
  ## tempting shortcut is to delete them from the input. That would trade one
  ## silent corruption for another: instead of misreading the value, the parser
  ## would quietly rewrite it -- and this function's whole promise is that a
  ## value arrives in the condition exactly as it was written.
  ##
  ## So the delimiters are chosen per expression from what is free, and a value
  ## that happens to contain a candidate marker must come through untouched.
  vocab <- .WCL_VOCAB_A
  marker <- intToUtf8(0xE000)
  value <- paste0("AMBER", marker, "TEAL")
  expr <- sprintf("%s.%s='%s'", vocab$ds, vocab$var, value)

  shape <- .wcl_shape(.wcl_parse(expr))
  expect_identical(shape$kind, "condition")
  expect_identical(shape$values, value)
  ## Byte-for-byte: the marker is still in the value, not stripped out of it.
  expect_true(grepl(marker, shape$values, fixed = TRUE))
  expect_identical(nchar(shape$values), nchar(value))
})


test_that("an expression that cannot be masked reserves, rather than running", {
  ## If no delimiter pair is free, the structure cannot be separated from the
  ## values and the expression cannot be read at all.
  ##
  ## That is now an UNRESOLVED condition rather than NULL, so it reserves
  ## instead of executing as "no filter" -- the safety guarantee B1a described
  ## as visibility-only.
  ##
  ## The candidate set is narrowed to one character to make the state
  ## reachable at all.
  original <- get(".MASK_CANDIDATES", envir = asNamespace("arsbridge"))
  withr::defer(.rsv_restore(".MASK_CANDIDATES", original))
  .rsv_install(".MASK_CANDIDATES", intToUtf8(0xE000))

  vocab <- .WCL_VOCAB_A
  expr <- sprintf("%s.%s='%s'", vocab$ds, vocab$var, vocab$plain)

  ## Fewer than two candidates, so masking cannot proceed.
  expect_null(.mask_literals(expr))

  ## The parser returns UNRESOLVED -- not NULL -- so the expression reserves
  ## rather than executing as "no filter", and the run carries a diagnostic
  ## naming the reason.
  diag_reset()
  result <- suppressWarnings(parse_where_clause(expr))
  expect_true(.is_unresolved_condition(result))
  expect_identical(.unresolved_condition_text(result), expr)
  reported <- ars_diagnostics()
  expect_true(any(grepl("could not be separated", reported$problem)))
  expect_true(any(reported$severity == "WARN"))
})


test_that("masking is what fixes it: removing it brings the defect back", {
  ## The mutation. `.mask_literals()` is replaced with an identity that hands
  ## back the raw text, which is precisely the behaviour this change removed.
  ## If the generic tests above could pass without masking, they would not be
  ## testing what they claim to.
  original <- get(".mask_literals", envir = asNamespace("arsbridge"))
  withr::defer(.rsv_restore(".mask_literals", original))

  .rsv_install(".mask_literals", function(expr) {
    list(text = expr, literals = character(0))
  })

  vocab <- .WCL_VOCAB_A
  expr <- sprintf("%s.%s='%s'", vocab$ds, vocab$var, vocab$joined_or)

  ## Unmasked, the joiner inside the value splits the expression and both
  ## halves fail to parse. The clause is then UNRESOLVED rather than a
  ## clause -- which is the defect returning, since the author's filter is
  ## no longer applied. It is not "absent": the annotation did supply a
  ## condition, and that distinction is exactly what B1b preserves.
  expect_identical(.wcl_shape(.wcl_parse(expr))$kind, "unresolved")
})


test_that("every previously supported spelling still parses", {
  ## The regression guard. Masking sits in front of every structural step, so
  ## the forms that already worked are the ones most at risk from it.
  vocab <- .WCL_VOCAB_A
  ds <- vocab$ds
  var <- vocab$var

  supported <- c(
    sprintf("%s.%s='%s'", ds, var, vocab$plain),
    sprintf("%s.%s=\"%s\"", ds, var, vocab$plain),
    sprintf("%s.%s EQ '%s'", ds, var, vocab$plain),
    sprintf("%s.NUMV GE 65", ds),
    sprintf("%s.NUMV=1", ds),
    sprintf("%s.NUMV==1", ds),
    sprintf("%s.NUMV between 18 and 65", ds),
    sprintf("%s.%s IN ('%s','X')", ds, var, vocab$plain),
    sprintf("%s.%s contains '%s'", ds, var, vocab$plain),
    sprintf("%s.DTV is null", ds),
    sprintf("%s.DTV not missing", ds),
    sprintf("is.na(%s.NUMV)", ds),
    sprintf("!is.na(%s.NUMV)", ds),
    sprintf("unique USUBJID in %s where %s.%s='%s'", ds, ds, var, vocab$plain)
  )

  kinds <- vapply(supported,
                  function(e) .wcl_shape(.wcl_parse(e))$kind, character(1))
  ## Non-vacuity alongside the verdict: if the grammar ever stopped reading
  ## these identifiers, every entry would be "dropped" and the count is what
  ## says so rather than an empty pass.
  expect_equal(length(kinds), 14L)
  expect_false(any(kinds %in% c("absent", "unresolved")))
})
