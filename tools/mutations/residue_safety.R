## Mutations for the residue-safety rules. Run with:
##   Rscript tools/mutation_harness.R tools/mutations/residue_safety.R
##
## Each one reintroduces a specific way the rule could be got wrong, and the
## named test that catches it is the evidence that test is doing its job.

mutations <- list(
  ## Drop the negation rule: "with no <DS> record where FLAG='Y'" goes back to
  ## reading as FLAG='Y' -- the complement of what was written.
  list(id = "M-neg", file = "R/utils_where_clause.R",
       from = "  if (grepl(.RE_RESIDUE_NEGATION, text, perl = TRUE)) return(rest)",
       to   = "  if (FALSE) return(rest)"),

  ## Drop the scoping-prefix rule: two per-subject existentials collapse into
  ## one row-wise AND, which is a different set of subjects.
  list(id = "M-prefix", file = "R/utils_where_clause.R",
       from = "  if (grepl(.RE_RESIDUE_SCOPING_PREFIX, head_text, perl = TRUE)) return(rest)",
       to   = "  if (FALSE) return(rest)"),

  ## Drop the semicolon boundary: the scoping rule then fires on the head of
  ## `HEAD; <filter>`, reserving a form this grammar has always read.
  list(id = "M-semi", file = "R/utils_where_clause.R",
       from = '  head_text <- sub("^.*;", "", head_text)',
       to   = "  head_text <- head_text")
)

test_files <- c(
  "tests/testthat/test-residue_meaning.R",
  "tests/testthat/test-derivation_note_annotation.R",
  "tests/testthat/test-boolean_structure.R",
  "tests/testthat/test-annotation_envelope.R"
)
