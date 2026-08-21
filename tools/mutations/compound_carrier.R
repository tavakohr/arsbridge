## Mutations for the compound DataSubset carrier. Run with:
##   Rscript tools/mutation_harness.R tools/mutations/compound_carrier.R
##
## Each reintroduces a specific way the carrier could be got wrong. The last
## one targets the restriction PLANNER rather than the carrier: the carrier
## accepting a mixed-dataset tree is not evidence it can execute, and the
## tests say so, so the mutation proves those tests are what enforce it.

mutations <- list(
  ## The pre-carrier bottleneck, in its two historical forms: a compound
  ## silently states no filter (the silent over-count), or reserves.
  list(id = "M1-drop", file = "R/build_ars_json.R",
       from = "  if (is.null(flat)) return(list(compound = where))",
       to   = "  if (is.null(flat)) return(list())"),
  list(id = "M2-reserve", file = "R/build_ars_json.R",
       from = "  if (is.null(flat)) return(list(compound = where))",
       to   = "  if (is.null(flat)) return(list(unresolved = .stated_filter_unrepresented(stated)))"),

  ## First-match: carry only the first clause of the tree. Restricts by less
  ## than was written, which is the failure the whole series exists to stop.
  list(id = "M3-first", file = "R/build_ars_json.R",
       from = "  if (is.null(flat)) return(list(compound = where))",
       to   = "  if (is.null(flat)) return(list(compound = where$compoundExpression$whereClauses[[1]]))"),

  ## An unreadable restriction stops reserving and the row computes over every
  ## record -- the reservation marker's whole purpose.
  list(id = "M4-unreserve", file = "R/build_ars_json.R",
       from = "  if (.is_unresolved_condition(where)) return(list(unresolved = where))",
       to   = "  if (.is_unresolved_condition(where)) return(list())"),

  ## The row builder ignores the compound the reader handed it.
  list(id = "M5-norouting", file = "R/build_ars_json.R",
       from = "          } else if (!is.null(carry$compound)) {\n            er$data_subset_compound <- carry$compound\n          } else if (!is.null(carry$flat)) {",
       to   = "          } else if (FALSE) {\n            er$data_subset_compound <- carry$compound\n          } else if (!is.null(carry$flat)) {"),

  ## Supplement precedence inverted: an authoritative typed clause is
  ## overwritten by one read from annotation text.
  list(id = "M6-precedence", file = "R/build_ars_json.R",
       from = "        if (!is.null(row$supplement_where)) {\n          flat <- .where_flat(row$supplement_where)",
       to   = "        if (FALSE) {\n          flat <- .where_flat(row$supplement_where)"),

  ## The subset builder drops the carrier, so nothing reaches the ARS.
  list(id = "M7-noemit", file = "R/build_ars_json.R",
       from = "  comp <- enrichment$data_subset_compound\n  if (!is.null(comp) && !is.null(comp$compoundExpression)) {",
       to   = "  comp <- NULL\n  if (!is.null(comp) && !is.null(comp$compoundExpression)) {"),

  ## PLANNER, not carrier: the ambiguous mixed-dataset shape stops blocking
  ## and silently filters by one reading of an expression that does not say
  ## which reading was meant.
  list(id = "M8-ambiguous", file = "R/utils_where_clause.R",
       from = "  repeated <- unique(foreign[duplicated(foreign)])\n  if (length(repeated) > 0) {",
       to   = "  repeated <- unique(foreign[duplicated(foreign)])\n  if (FALSE) {")
)

test_files <- c(
  "tests/testthat/test-compound_data_subset.R",
  "tests/testthat/test-filter_structure_and_role.R",
  "tests/testthat/test-build_ars_json.R"
)
