# LLM column-hierarchy grounding: the model may refine roles and fill
# missing grouping variables on the PARSED tree, but a mismatched answer is
# discarded whole and an out-of-spec variable is ignored -- geometry and the
# spec gate stay authoritative.

.ech_section <- function() {
  secs <- parse_shell_docx(
    test_path("fixtures/annotated_shell_asymmetric_tree.docx"))
  secs[[1]]
}

.ech_lookup <- list(ADSL.AGE = list(), ADSL.SEX = list(),
                    ADSL.COMPFL = list(), ADSL.COHGRPN = list(),
                    ADSL.SEVGR1N = list())

.ech_answer <- function(sec, ...) {
  paths <- arsbridge:::column_tree_paths(sec$column_tree)
  leaves <- lapply(paths, function(p) list(
    label_path = as.list(p$label_path),
    role       = p$role
  ))
  answer <- list(
    analysis_type = "CONTINUOUS",
    column_hierarchy = list(leaf_columns = leaves)
  )
  utils::modifyList(answer, list(...))
}

test_that("a matching answer refines the tree; the payload carries it", {
  sec <- .ech_section()
  payload <- arsbridge:::.enrich_payload(sec, list(), .ech_lookup)
  expect_false(is.null(payload$header_tree))
  expect_true(any(vapply(payload$header_tree, function(n)
    identical(n$label, "Cohort A"), logical(1))))

  out <- enrich_with_llm(sec, spec_lookup = .ech_lookup,
                         courier_answers = .ech_answer(sec))
  expect_identical(out$column_tree$mode, "ASYMMETRIC_NESTED")
  expect_length(arsbridge:::column_tree_paths(out$column_tree), 6L)
})

test_that("an answer with different columns is discarded whole", {
  diag_reset()
  sec <- .ech_section()
  answer <- .ech_answer(sec)
  answer$column_hierarchy$leaf_columns <-
    answer$column_hierarchy$leaf_columns[1:3]

  out <- enrich_with_llm(sec, spec_lookup = .ech_lookup,
                         courier_answers = answer)
  # Tree unchanged: still six paths with parsed roles.
  paths <- arsbridge:::column_tree_paths(out$column_tree)
  expect_length(paths, 6L)
  recs <- diag_records()
  expect_true(any(grepl("does not match the parsed header tree", recs$problem)))
})

test_that("an out-of-spec grouping proposal is ignored with a WARN", {
  diag_reset()
  sec <- .ech_section()
  answer <- .ech_answer(sec)
  answer$column_hierarchy$leaf_columns[[1]]$grouping_variables <-
    list("NOTAVAR")

  out <- enrich_with_llm(sec, spec_lookup = .ech_lookup,
                         courier_answers = answer)
  recs <- diag_records()
  expect_true(any(grepl("NOTAVAR", recs$problem) &
                  grepl("not in the ADaM spec", recs$problem)))
  # The parsed grouping_ref is untouched.
  mild <- Filter(function(n) identical(n$label, "Mild"),
                 out$column_tree$nodes)[[1]]
  expect_identical(mild$grouping_ref, "ADSL.SEVGR1N")
})
