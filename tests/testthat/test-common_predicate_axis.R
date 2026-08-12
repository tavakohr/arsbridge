# The column axis inferred from compound column headers (spec section 25).
#
# The rule under test, deliberately strict: every conditioned level header is
# an AND of exactly two simple clauses, exactly one clause is identical across
# all of them, and what is left over names one single variable. Anything else
# must NOT be guessed at -- it comes back ambiguous, and the caller falls back
# to the existing behaviour rather than inventing an axis.

.cpa_candidate <- function(label, annotation) {
  list(
    label      = label,
    annotation = annotation,
    variable   = toupper(extract_annotation_vars(annotation)[[1]]),
    condition  = parse_where_clause(annotation)
  )
}

.cpa_headers <- function(...) {
  pairs <- list(...)
  lapply(names(pairs), function(label) .cpa_candidate(label, pairs[[label]]))
}

test_that("the varying variable is the axis when one predicate is shared", {
  headers <- .cpa_headers(
    Low    = "ADSL.COHORTN=1 AND ADSL.CGHGR1N=1",
    Medium = "ADSL.COHORTN=1 AND ADSL.CGHGR1N=2",
    High   = "ADSL.COHORTN=1 AND ADSL.CGHGR1N=3"
  )

  result <- .infer_common_predicate_axis(headers)

  expect_false(result$ambiguous)
  # The axis is what VARIES, not the first variable each header names.
  expect_identical(result$axis, "ADSL.CGHGR1N")
  expect_identical(result$common, "ADSL.COHORTN")
  expect_identical(result$n_levels, 3L)
})

test_that("a Total header does not take part in the inference", {
  # Total is scoped to the common predicate alone; it is the overall column,
  # never one of the levels, and it must not make the pattern look broken.
  headers <- .cpa_headers(
    Low     = "ADSL.COHORTN=1 AND ADSL.CGHGR1N=1",
    High    = "ADSL.COHORTN=1 AND ADSL.CGHGR1N=3",
    `Total` = "ADSL.COHORTN=1"
  )

  result <- .infer_common_predicate_axis(headers)

  expect_false(result$ambiguous)
  expect_identical(result$axis, "ADSL.CGHGR1N")
})

test_that("clause order within a header does not change the answer", {
  headers <- .cpa_headers(
    Low  = "ADSL.COHORTN=1 AND ADSL.CGHGR1N=1",
    High = "ADSL.CGHGR1N=3 AND ADSL.COHORTN=1"
  )

  result <- .infer_common_predicate_axis(headers)

  expect_false(result$ambiguous)
  expect_identical(result$axis, "ADSL.CGHGR1N")
})

test_that("ordinary single-variable headers are not this pattern at all", {
  # Not a finding, not ambiguous -- just the normal case, left to the
  # existing first-variable rule.
  headers <- .cpa_headers(
    Placebo = "ADSL.TRT01A=Placebo",
    Active  = "ADSL.TRT01A=Xanomeline Low Dose"
  )

  expect_null(.infer_common_predicate_axis(headers))
})

test_that("one plain header among compound ones is not this pattern", {
  headers <- .cpa_headers(
    Low  = "ADSL.COHORTN=1 AND ADSL.CGHGR1N=1",
    High = "ADSL.CGHGR1N=3"
  )

  expect_null(.infer_common_predicate_axis(headers))
})

test_that("fewer than two level headers is not this pattern", {
  headers <- .cpa_headers(Low = "ADSL.COHORTN=1 AND ADSL.CGHGR1N=1")

  expect_null(.infer_common_predicate_axis(headers))
})

test_that("the excluded shapes are left alone rather than guessed at", {
  # OR-joined headers: out of scope for this first implementation.
  or_joined <- .cpa_headers(
    Low  = "ADSL.COHORTN=1 OR ADSL.CGHGR1N=1",
    High = "ADSL.COHORTN=1 OR ADSL.CGHGR1N=3"
  )
  expect_null(.infer_common_predicate_axis(or_joined))

  # Three clauses: out of scope. The value has to be quoted -- an unquoted
  # character value is not a condition at all, and would drop the clause
  # before this code ever saw it.
  three <- .cpa_headers(
    Low  = "ADSL.COHORTN=1 AND ADSL.CGHGR1N=1 AND ADSL.SAFFL='Y'",
    High = "ADSL.COHORTN=1 AND ADSL.CGHGR1N=3 AND ADSL.SAFFL='Y'"
  )
  expect_length(three[[1]]$condition$compoundExpression$whereClauses, 3L)
  expect_null(.infer_common_predicate_axis(three))
})

test_that("no shared predicate is ambiguous, not a guess", {
  headers <- .cpa_headers(
    Low  = "ADSL.COHORTN=1 AND ADSL.CGHGR1N=1",
    High = "ADSL.COHORTN=2 AND ADSL.CGHGR1N=3"
  )

  result <- .infer_common_predicate_axis(headers)

  expect_true(result$ambiguous)
  expect_match(result$reason, "no predicate is common")
  expect_null(result$axis)
})

test_that("more than one varying variable is ambiguous, not a guess", {
  headers <- .cpa_headers(
    Low  = "ADSL.COHORTN=1 AND ADSL.CGHGR1N=1",
    High = "ADSL.COHORTN=1 AND ADSL.AGEGR1N=2"
  )

  result <- .infer_common_predicate_axis(headers)

  expect_true(result$ambiguous)
  expect_match(result$reason, "more than one variable")
  expect_null(result$axis)
})

test_that("headers that do not differ at all are ambiguous", {
  headers <- .cpa_headers(
    One = "ADSL.COHORTN=1 AND ADSL.CGHGR1N=1",
    Two = "ADSL.CGHGR1N=1 AND ADSL.COHORTN=1"
  )

  result <- .infer_common_predicate_axis(headers)

  expect_true(result$ambiguous)
  expect_match(result$reason, "do not differ")
})

test_that("one variable compared twice is not a factorisation", {
  # The shared clause and the varying one name the same variable, so nothing
  # has actually been factored out.
  headers <- .cpa_headers(
    Low  = "ADSL.COHORTN=1 AND ADSL.COHORTN=2",
    High = "ADSL.COHORTN=1 AND ADSL.COHORTN=3"
  )

  result <- .infer_common_predicate_axis(headers)

  expect_true(result$ambiguous)
  expect_match(result$reason, "same variable")
})

test_that("a header whose two clauses are identical is ambiguous", {
  # Nothing is left over once the shared clause is removed, so there is no
  # varying value for that column at all.
  headers <- .cpa_headers(
    Low  = "ADSL.COHORTN=1 AND ADSL.COHORTN=1",
    High = "ADSL.COHORTN=1 AND ADSL.CGHGR1N=3"
  )

  result <- .infer_common_predicate_axis(headers)

  expect_true(result$ambiguous)
  expect_match(result$reason, "repeats the common predicate")
})

# --- end to end, through the fixtures ----------------------------------------

.cpa_diags <- function(path) {
  diag_reset()
  suppressMessages(invisible(parse_shell_docx(path)))
  diag_records()
}

test_that("a common-predicate shell yields the varying variable as the axis", {
  secs <- suppressMessages(
    parse_shell_docx(test_path("fixtures/annotated_shell_common_predicate.docx"))
  )
  groups <- secs[[1]]$column_groups

  expect_identical(groups$variable, "CGHGR1N")
  expect_identical(groups$dataset, "ADSL")
  expect_identical(secs[[1]]$column_annotation, "ADSL.CGHGR1N")
  expect_length(groups$groups, 3L)

  # Option A: each level keeps its WHOLE condition. The shared predicate is
  # not stripped out, only the axis is named from what varies.
  annotations <- vapply(groups$groups, function(g) g$annotation, character(1))
  expect_true(all(grepl("COHORTN=1", annotations, fixed = TRUE)))
  expect_identical(
    vapply(groups$groups, function(g) g$label, character(1)),
    c("Low", "Medium", "High")
  )
})

test_that("successful inference is INFO, and warns about nothing", {
  records <- .cpa_diags(
    test_path("fixtures/annotated_shell_common_predicate.docx")
  )
  axis <- records[grepl("column axis", records$problem), ]

  expect_identical(nrow(axis), 1L)
  expect_identical(axis$severity, "INFO")
  expect_match(axis$problem, "share the predicate ADSL.COHORTN")
  expect_match(axis$problem, "ADSL.CGHGR1N varies")
})

test_that("an undecidable axis warns and changes nothing", {
  path <- test_path("fixtures/annotated_shell_common_predicate_ambiguous.docx")
  records <- .cpa_diags(path)
  axis <- records[grepl("cannot be inferred", records$problem), ]

  expect_identical(nrow(axis), 1L)
  expect_identical(axis$severity, "WARN")
  expect_match(axis$problem, "more than one variable")

  # No guess: the axis is still whatever the existing first-variable rule
  # picked, not one of the two candidates chosen for us.
  secs <- suppressMessages(parse_shell_docx(path))
  expect_identical(secs[[1]]$column_groups$variable, "COHORTN")
})

test_that("the bundled example shell is untouched by this change", {
  # The pattern is absent there, so nothing should be said about an axis at
  # all -- neither the INFO nor the WARN.
  records <- .cpa_diags(arsbridge_example("annotated_shell.docx"))
  expect_identical(
    nrow(records[grepl("share the predicate|cannot be inferred",
                       records$problem), ]),
    0L
  )
})

test_that("the inferred axis survives into the built ARS", {
  skip_on_cran()
  out <- withr::local_tempfile(fileext = ".json")

  result <- suppressMessages(spec_to_ars(
    shell_path     = test_path("fixtures/annotated_shell_common_predicate.docx"),
    adam_spec_path = test_path("fixtures/adam_spec_minimal.xlsx"),
    output_path    = out,
    study_id = "CDSC-ALZ-201", use_llm = FALSE, extract_with_llm = FALSE,
    report_path = withr::local_tempfile(fileext = ".xlsx"), verbose = FALSE
  ))

  groupings <- result$reporting_event$analysisGroupings
  axis <- Filter(function(g) identical(g$groupingVariable, "CGHGR1N"), groupings)

  expect_length(axis, 1L)
  expect_false(isTRUE(axis[[1]]$dataDriven))
  expect_length(axis[[1]]$groups, 3L)

  # Every level is still the full two-clause AND it was annotated with.
  for (group in axis[[1]]$groups) {
    expect_null(group$condition)
    expect_identical(group$compoundExpression$logicalOperator, "AND")
    expect_length(group$compoundExpression$whereClauses, 2L)
  }
})
