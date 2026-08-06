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

test_that("a directive clause is not reported as a failed condition", {
  ## "once/subject ADAE.AOCCIFL" names a variable and carries text around it,
  ## so it has the shape of an attempted filter -- but it is a directive with
  ## its own consumer (.once_per_subject_var(), which routes the row to the
  ## distinct-subject method). Nothing is dropped, so nothing is reported:
  ## the bundled example raised two of these against an annotation the
  ## package understood perfectly.
  diag_reset()
  parse_where_clause("ADAE.ASEV; once/subject ADAE.AOCCIFL")
  recs <- diag_records()
  expect_false(any(recs$stage == "where_clause"))

  ## A genuinely unparseable condition still is.
  diag_reset()
  parse_where_clause("ADSL.AGE like 'x'")
  recs <- diag_records()
  expect_true(any(recs$stage == "where_clause" & recs$severity == "WARN"))
  diag_reset()
})

## ---------------------------------------------------------------------------
## Quoting dialects
##
## The grammar used to accept only 'single' quotes. A field study annotated
## every table the way its programmers write SAS -- [ADSL.COMPLFL="Y"] -- and
## every one of those conditions was dropped without the run failing: wrong
## populations on every table, and a Total column that never computed. Both
## spellings must mean the same thing, and "the same thing" is stronger than
## "both parse": they have to produce the identical WhereClause.
## ---------------------------------------------------------------------------

test_that("single and double quotes produce the identical where clause", {
  both_ways <- function(template) {
    n <- length(gregexpr("%s", template, fixed = TRUE)[[1]])
    fill <- function(q) do.call(sprintf, c(list(template), as.list(rep(q, n))))
    list(single = fill("'"), double = fill('"'))
  }

  templates <- c(
    "ADSL.SAFFL=%sY%s",
    "ADSL.SAFFL EQ %sY%s",
    "ADSL.SAFFL NE %sY%s",
    "ADSL.RACE IN (%sWHITE%s,%sASIAN%s)",
    "ADSL.RACE NOT IN (%sWHITE%s,%sASIAN%s)",
    "ADAE.AETERM contains %srash%s",
    "ADSL.AGE between %s18%s and %s65%s",
    ## The exact shape the field shell used, compound and with empty strings.
    "ADMH.MHDECOD NE %s%s AND ADMH.MHBODSYS=%s%s"
  )

  for (template in templates) {
    spelling <- both_ways(template)
    single <- suppressMessages(parse_where_clause(paste0("[", spelling$single, "]")))
    double <- suppressMessages(parse_where_clause(paste0("[", spelling$double, "]")))
    expect_false(is.null(single), label = spelling$single)
    expect_identical(single, double, label = spelling$double)
  }
})

test_that("a value keeps the other quote character when it contains one", {
  ## Stripping is anchored to a MATCHING pair, so "O'Brien" survives whole.
  wc <- suppressMessages(parse_where_clause('[ADSL.NAME="O\'Brien"]'))
  expect_equal(unlist(wc$condition$value), "O'Brien")
})

test_that("a value list may be bare numbers", {
  ## "ADSL.COHORTN IN (1,2)" is how a coded column axis is written far more
  ## often than with quotes round the codes -- and requiring the quotes cost
  ## one real shell its entire Total column.
  for (text in c("[ADSL.COHORTN IN (1,2)]", "[ADSL.COHORTN IN (1, 2)]")) {
    wc <- suppressMessages(parse_where_clause(text))
    expect_equal(wc$condition$comparator, "IN", info = text)
    expect_equal(unlist(wc$condition$value), c("1", "2"), info = text)
  }
  negated <- suppressMessages(parse_where_clause("[ADSL.COHORTN NOT IN (1,2)]"))
  expect_equal(negated$condition$comparator, "NOTIN")
  expect_equal(unlist(negated$condition$value), c("1", "2"))
})

test_that("a mixed list of numbers and quoted strings parses", {
  wc <- suppressMessages(parse_where_clause('[ADSL.COHORTN IN (1, "2", \'3\')]'))
  expect_equal(unlist(wc$condition$value), c("1", "2", "3"))
})
