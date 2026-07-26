# Supplement v4 columnHierarchy: a declared hierarchical column tree applies
# as sec$column_tree through the same all-or-nothing spec gate as every other
# supplement channel, and ars_validate_supplement() pre-flights it.

.tree_spec <- list(ADSL.AGE = list(), ADSL.SAFFL = list(),
                   ADSL.COHGRPN = list(), ADSL.SEVGR1N = list())

.tree_section <- function() {
  list(
    tlf_number = "T-14-3-1", tlf_type = "TABLE",
    title = "Demographics by Cohort and Baseline Severity",
    population_annot = "",
    stub_rows = list(
      list(label = "Age (years)", annotation = "ADSL.AGE", has_annot = TRUE,
           detection_method = "colour", detection_confidence = "high",
           raw_text = "Age (years) ADSL.AGE")
    )
  )
}

.tree_wc <- function(variable, value) {
  list(condition = list(dataset = "ADSL", variable = variable,
                        comparator = "EQ", value = list(value)))
}

.hier_nodes <- function() {
  list(
    c(list(id = "N_A", label = "Cohort A", level = 1L, order = 1L,
           nodeType = "GROUP", groupingDataset = "ADSL",
           groupingVariable = "COHGRPN"), .tree_wc("COHGRPN", "1")),
    c(list(id = "N_A_MILD", label = "Mild", parentId = "N_A", level = 2L,
           order = 1L, nodeType = "LEAF", groupingDataset = "ADSL",
           groupingVariable = "SEVGR1N"), .tree_wc("SEVGR1N", "1")),
    c(list(id = "N_A_MOD", label = "Moderate", parentId = "N_A", level = 2L,
           order = 2L, nodeType = "LEAF", groupingDataset = "ADSL",
           groupingVariable = "SEVGR1N"), .tree_wc("SEVGR1N", "2")),
    c(list(id = "N_A_SEV", label = "Severe", parentId = "N_A", level = 2L,
           order = 3L, nodeType = "LEAF", groupingDataset = "ADSL",
           groupingVariable = "SEVGR1N"), .tree_wc("SEVGR1N", "3")),
    list(id = "N_A_TOT", label = "Total", parentId = "N_A", level = 2L,
         order = 4L, nodeType = "SUBTOTAL", totalStrategy = "condition_based"),
    c(list(id = "N_B", label = "Cohort B", level = 1L, order = 2L,
           nodeType = "LEAF", groupingDataset = "ADSL",
           groupingVariable = "COHGRPN"), .tree_wc("COHGRPN", "2")),
    list(id = "N_TOT", label = "Total", level = 1L, order = 3L,
         nodeType = "GRAND_TOTAL", totalStrategy = "analysis_set")
  )
}

.hier_tlf <- function(nodes = .hier_nodes(), mode = "ASYMMETRIC_NESTED", ...) {
  c(list(title = "Demographics by Cohort and Baseline Severity",
         analysis_type = "MIXED_SUMMARY", is_supported = TRUE,
         columnHierarchy = list(mode = mode, nodes = nodes)),
    list(...))
}

test_that("a declared columnHierarchy becomes the section's column tree", {
  diag_reset()
  sec <- arsbridge:::.apply_supplement_bindings(.tree_section(), .hier_tlf(),
                                                .tree_spec)

  expect_false(is.null(sec$column_tree))
  expect_identical(sec$column_tree$mode, "ASYMMETRIC_NESTED")

  paths <- arsbridge:::column_tree_paths(sec$column_tree)
  expect_length(paths, 6L)
  expect_identical(
    vapply(paths, function(p) paste(p$label_path, collapse = " > "), character(1)),
    c("Cohort A > Mild", "Cohort A > Moderate", "Cohort A > Severe",
      "Cohort A > Total", "Cohort B", "Total")
  )

  # Subtotal composes to the parent's condition.
  parent <- arsbridge:::parse_where_clause("ADSL.COHGRPN=1")
  expect_true(arsbridge:::conditions_equal(paths[[4]]$condition, parent))

  # The level-1 axis fills the classic single-axis fields.
  expect_identical(sec$column_annotation, "ADSL.COHGRPN")
  expect_identical(
    vapply(sec$column_groups$groups, function(g) g$label, character(1)),
    c("Cohort A", "Cohort B")
  )
  expect_length(sec$column_tree$levels, 2L)
  expect_true(isTRUE(sec$include_total_hint))
})

test_that("columnHierarchy plus includeTotal=true is rejected whole", {
  diag_reset()
  sec <- arsbridge:::.apply_supplement_bindings(
    .tree_section(), .hier_tlf(includeTotal = TRUE), .tree_spec)

  expect_null(sec$column_tree)
  recs <- diag_records()
  expect_true(any(recs$severity == "FAIL" &
                  grepl("includeTotal", recs$problem)))
})

test_that("one out-of-spec node condition rejects the whole hierarchy", {
  diag_reset()
  nodes <- .hier_nodes()
  nodes[[2]]$condition$variable <- "NOTINSPEC"
  sec <- arsbridge:::.apply_supplement_bindings(
    .tree_section(), .hier_tlf(nodes = nodes), .tree_spec)

  expect_null(sec$column_tree)
  recs <- diag_records()
  expect_true(any(recs$severity == "FAIL" &
                  grepl("columnHierarchy node", recs$problem)))
})

test_that("a broken parent reference rejects the hierarchy", {
  diag_reset()
  nodes <- .hier_nodes()
  nodes[[5]]$parentId <- "N_MISSING"
  sec <- arsbridge:::.apply_supplement_bindings(
    .tree_section(), .hier_tlf(nodes = nodes), .tree_spec)

  expect_null(sec$column_tree)
})

test_that("ars_validate_supplement pre-flights a columnHierarchy", {
  supp <- list(supplement_version = 4L,
               tlfs = list(`14.3.1` = .hier_tlf()))
  path <- tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(supp, auto_unbox = TRUE, pretty = TRUE), path)
  out <- suppressMessages(ars_validate_supplement(path))
  expect_false(any(out$severity == "FAIL"))

  # Conflict, bad mode, orphan SUBTOTAL, unknown parent: all FAIL loudly.
  bad_nodes <- .hier_nodes()
  bad_nodes[[5]]$parentId <- NULL                       # SUBTOTAL without parent
  bad_nodes[[6]]$parentId <- "N_GHOST"                  # unknown parent
  bad <- list(supplement_version = 4L, tlfs = list(`14.3.1` = .hier_tlf(
    nodes = bad_nodes, mode = "SIDEWAYS", includeTotal = TRUE)))
  path2 <- tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(bad, auto_unbox = TRUE, pretty = TRUE), path2)
  out2 <- suppressMessages(ars_validate_supplement(path2))

  expect_true(any(grepl("includeTotal:true", out2$problem)))
  expect_true(any(grepl("mode must be", out2$problem)))
  expect_true(any(grepl("SUBTOTAL node needs a parentId", out2$problem)))
  expect_true(any(grepl("does not match any node id", out2$problem)))
})

test_that("the v4 schema accepts the hierarchy and enforces the total conflict", {
  skip_if_not_installed("jsonvalidate")
  schema_path <- system.file("schema", "arsbridge_supplement_v4.schema.json",
                             package = "arsbridge")
  skip_if(!nzchar(schema_path))
  validator <- jsonvalidate::json_validator(schema_path, engine = "ajv")

  good <- jsonlite::toJSON(list(supplement_version = 4L,
                                tlfs = list(`14.3.1` = .hier_tlf())),
                           auto_unbox = TRUE)
  expect_true(validator(good))

  conflicted <- jsonlite::toJSON(
    list(supplement_version = 4L,
         tlfs = list(`14.3.1` = .hier_tlf(includeTotal = TRUE))),
    auto_unbox = TRUE)
  expect_false(validator(conflicted))
})
