# Column tree: the asymmetric two-level header fixture must parse into an
# explicit tree with six declared result-column paths -- three severity
# children plus a subtotal under Cohort A, the Cohort B sibling, and the
# overall Total -- and the subtotal's condition must be the PARENT condition
# (its N exceeds the sum of the displayed children).

.asym_sections <- function() {
  parse_shell_docx(test_path("fixtures/annotated_shell_asymmetric_tree.docx"))
}

test_that("asymmetric two-level header parses into an ASYMMETRIC_NESTED tree", {
  sec <- .asym_sections()[[1]]

  expect_false(is.null(sec$column_tree))
  expect_identical(sec$column_tree$mode, "ASYMMETRIC_NESTED")
  expect_length(arsbridge:::.validate_column_tree(sec$column_tree), 0L)

  types <- vapply(sec$column_tree$nodes, function(n) n$node_type, character(1))
  expect_identical(sum(types == "group"), 1L)         # Cohort A
  expect_identical(sum(types == "subtotal"), 1L)      # Cohort A > Total
  expect_identical(sum(types == "grand_total"), 1L)   # overall Total
  expect_identical(sum(types == "leaf"), 4L)          # Mild/Moderate/Severe + Cohort B
})

test_that("six declared paths come out in display order with correct roles", {
  sec   <- .asym_sections()[[1]]
  paths <- arsbridge:::column_tree_paths(sec$column_tree)

  expect_length(paths, 6L)
  label_paths <- vapply(paths, function(p) paste(p$label_path, collapse = " > "),
                        character(1))
  expect_identical(label_paths, c(
    "Cohort A > Mild",
    "Cohort A > Moderate",
    "Cohort A > Severe",
    "Cohort A > Total",
    "Cohort B",
    "Total"
  ))
  expect_identical(
    vapply(paths, function(p) p$role, character(1)),
    c("DETAIL", "DETAIL", "DETAIL", "SUBTOTAL", "DETAIL", "GRAND_TOTAL")
  )

  # No path may cross Cohort B with a severity level.
  expect_false(any(grepl("^Cohort B > ", label_paths)))
})

test_that("leaf conditions compose parent AND child; subtotal is parent-only", {
  sec   <- .asym_sections()[[1]]
  paths <- arsbridge:::column_tree_paths(sec$column_tree)

  parent_cond <- arsbridge:::parse_where_clause("ADSL.COHGRPN=1")
  mild_cond   <- arsbridge:::combine_conditions(
    parent_cond, arsbridge:::parse_where_clause("ADSL.SEVGR1N=1")
  )

  expect_true(arsbridge:::conditions_equal(paths[[1]]$condition, mild_cond))

  # Subtotal: exactly the parent's condition, NOT the union of children --
  # this is what lets its N (88) exceed the displayed child sum (80).
  subtotal <- paths[[4]]
  expect_identical(subtotal$total_strategy, "condition_based")
  expect_true(arsbridge:::conditions_equal(subtotal$condition, parent_cond))
  expect_true(arsbridge:::condition_implies(paths[[1]]$condition, subtotal$condition))

  # Grand total: no condition of its own (the analysis set scopes it).
  grand <- paths[[6]]
  expect_null(grand$condition)
  expect_identical(grand$total_strategy, "analysis_set")
})

test_that("N hints and provenance are captured per node", {
  sec   <- .asym_sections()[[1]]
  nodes <- sec$column_tree$nodes

  labels <- vapply(nodes, function(n) n$label, character(1))
  hints  <- vapply(nodes, function(n) n$n_hint, integer(1))
  expect_identical(hints[labels == "Mild"], 40L)
  ## Two "Total" nodes since the footnote marker ("Total[a]") is stripped:
  ## the grand total (150) sits in header row 1, Cohort A's subtotal (88)
  ## in row 2 -- node order follows the rows.
  expect_identical(hints[labels == "Total"], c(150L, 88L))

  # Provenance points back to real header cells.
  for (n in nodes) {
    expect_true(n$source$header_row %in% c(1L, 2L))
    expect_true(n$source$col_start >= 2L)
    expect_true(n$source$col_end >= n$source$col_start)
  }
})

test_that("per-level axes resolve and the classic single-axis fields stay filled", {
  sec <- .asym_sections()[[1]]

  levels <- sec$column_tree$levels
  expect_length(levels, 2L)
  expect_identical(levels[[1]]$variable, "COHGRPN")
  expect_identical(levels[[2]]$variable, "SEVGR1N")

  # Compatibility: the level-1 axis still populates column_groups /
  # column_annotation for consumers that only understand one axis.
  expect_identical(sec$column_annotation, "ADSL.COHGRPN")
  expect_identical(sec$column_groups$variable, "COHGRPN")
  expect_identical(
    vapply(sec$column_groups$groups, function(g) g$label, character(1)),
    c("Cohort A", "Cohort B")
  )
  expect_true(isTRUE(sec$include_total_hint))
})

test_that("flat and stat-subcolumn shells stay FLAT with no column tree", {
  # Spanned header over n/(%) sub-columns: geometry is nested but there are
  # no conditioned child columns -- must remain the flat single-axis path.
  secs <- parse_shell_docx(test_path("fixtures/annotated_shell_merged_headers.docx"))
  expect_null(secs[[1]]$column_tree)

  # Classic one-row conditioned headers: also no tree.
  secs <- parse_shell_docx(test_path("fixtures/annotated_shell_column_groups.docx"))
  expect_null(secs[[1]]$column_tree)
  expect_identical(secs[[1]]$column_groups$variable, "COHORTN")
})
