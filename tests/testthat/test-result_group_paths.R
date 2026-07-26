# Result-group paths: the asymmetric column-tree fixture must build an ARS
# whose Output declares the six display paths (an arsbridge extension the
# conformance check strips), whose analyses reference BOTH level grouping
# factors, and whose validation catches structural damage before execution.

.rgp_output <- function(event) {
  for (out in event$outputs) {
    if (!is.null(out$resultGroupPaths)) return(out)
  }
  NULL
}

test_that("the asymmetric shell builds six declared result paths", {
  td <- withr::local_tempdir()
  event <- jsonlite::fromJSON(.asym_build(td)$ars_path, simplifyVector = FALSE)
  out <- .rgp_output(event)
  expect_false(is.null(out))

  rgp <- out$resultGroupPaths
  expect_identical(rgp$mode, "ASYMMETRIC_NESTED")
  paths <- rgp$paths
  expect_length(paths, 6L)

  labels <- vapply(paths, function(p)
    paste(unlist(p$labelPath), collapse = " > "), character(1))
  expect_identical(labels, c(
    "Cohort A > Mild", "Cohort A > Moderate", "Cohort A > Severe",
    "Cohort A > Total[a]", "Cohort B", "Total"
  ))
  expect_identical(
    vapply(paths, function(p) p$role, character(1)),
    c("DETAIL", "DETAIL", "DETAIL", "SUBTOTAL", "DETAIL", "GRAND_TOTAL")
  )
  expect_false(any(grepl("^Cohort B > ", labels)))

  # Subtotal: parent group only, condition-based; grand total: no groups.
  subtotal <- paths[[4]]
  expect_identical(unlist(subtotal$groupIds), "GRP_COHGRPN_COHORT_A")
  expect_identical(subtotal$totalStrategy, "condition_based")
  grand <- paths[[6]]
  expect_length(unlist(grand$groupIds), 0L)
  expect_identical(grand$totalStrategy, "analysis_set")

  # Detail children reference parent + child group levels, in that order.
  mild <- paths[[1]]
  expect_identical(unlist(mild$groupIds),
                   c("GRP_COHGRPN_COHORT_A", "GRP_SEVGR1N_MILD"))

  # Provenance points each path back to a shell header cell.
  expect_length(rgp$provenance, 6L)
})

test_that("both level grouping factors exist and every analysis links them", {
  td <- withr::local_tempdir()
  event <- jsonlite::fromJSON(.asym_build(td)$ars_path, simplifyVector = FALSE)

  gf_ids <- vapply(event$analysisGroupings, function(g) g$id, character(1))
  expect_true(all(c("GF_COHGRPN", "GF_SEVGR1N") %in% gf_ids))

  out <- .rgp_output(event)
  an_ids <- unlist(out$referencedAnalysisIds)
  for (an in event$analyses) {
    if (!an$id %in% an_ids) next
    linked <- vapply(an$orderedGroupings, function(og) og$groupingId, character(1))
    expect_true(all(c("GF_COHGRPN", "GF_SEVGR1N") %in% linked),
                info = an$id)
    # Outer level first.
    expect_true(match("GF_COHGRPN", linked) < match("GF_SEVGR1N", linked))
    # In path mode the grand total is an explicit path, not the boolean.
    expect_false(isTRUE(an$includeTotal))
  }
})

test_that("the path-mode event passes ARS v1.0 conformance after stripping", {
  skip_if_not_installed("jsonvalidate")
  td <- withr::local_tempdir()
  event <- jsonlite::fromJSON(.asym_build(td)$ars_path, simplifyVector = FALSE)

  findings <- ars_conformance(event)
  expect_identical(nrow(findings), 0L)
  stripped <- attr(findings, "stripped_extensions")
  expect_true(any(grepl("resultGroupPaths", stripped)))
})

test_that("validate_ars_model passes the intact event and catches damage", {
  td <- withr::local_tempdir()
  event <- jsonlite::fromJSON(.asym_build(td)$ars_path, simplifyVector = FALSE)
  model <- ars_to_model(event)

  findings <- validate_ars_model(model)
  expect_false(any(grepl(
    "HEADER_TREE_MISSING|DISPLAY_COLUMN_COUNT_MISMATCH|UNMAPPED_LEAF_COLUMN|INVALID_CARTESIAN_PRODUCT|GROUPING_VARIABLE_NOT_LINKED|SUBTOTAL_SCOPE_UNDEFINED|DUPLICATE_RESULT_PATH",
    findings$ref
  )))

  # Damage 1: drop three paths -> count mismatch + unmapped shell columns.
  broken <- event
  for (i in seq_along(broken$outputs)) {
    if (!is.null(broken$outputs[[i]]$resultGroupPaths)) {
      broken$outputs[[i]]$resultGroupPaths$paths <-
        broken$outputs[[i]]$resultGroupPaths$paths[1:3]
    }
  }
  f2 <- validate_ars_model(ars_to_model(broken))
  expect_true(any(f2$ref == "DISPLAY_COLUMN_COUNT_MISMATCH" & f2$severity == "FAIL"))
  expect_true(any(f2$ref == "UNMAPPED_LEAF_COLUMN" & f2$severity == "FAIL"))

  # Damage 2: invent a Cohort B > Mild cross -> INVALID_CARTESIAN_PRODUCT.
  crossed <- event
  for (i in seq_along(crossed$outputs)) {
    rgp <- crossed$outputs[[i]]$resultGroupPaths
    if (is.null(rgp)) next
    fake <- rgp$paths[[1]]
    fake$pathId    <- "PATH_FAKE"
    fake$nodeId    <- "CT_FAKE"
    fake$labelPath <- list("Cohort B", "Mild")
    fake$groupIds  <- list("GRP_COHGRPN_COHORT_B", "GRP_SEVGR1N_MILD")
    rgp$paths[[5]] <- fake   # replace Cohort B so the count stays 6
    crossed$outputs[[i]]$resultGroupPaths <- rgp
  }
  f3 <- validate_ars_model(ars_to_model(crossed))
  expect_true(any(f3$ref == "INVALID_CARTESIAN_PRODUCT" & f3$severity == "FAIL"))

  # Damage 3: strip a subtotal's scope -> SUBTOTAL_SCOPE_UNDEFINED.
  unscoped <- event
  for (i in seq_along(unscoped$outputs)) {
    rgp <- unscoped$outputs[[i]]$resultGroupPaths
    if (is.null(rgp)) next
    rgp$paths[[4]]$totalStrategy <- NULL
    rgp$paths[[4]]$groupIds <- list()
    unscoped$outputs[[i]]$resultGroupPaths <- rgp
  }
  f4 <- validate_ars_model(ars_to_model(unscoped))
  expect_true(any(f4$ref == "SUBTOTAL_SCOPE_UNDEFINED" & f4$severity == "FAIL"))

  # Damage 4: unlink a grouping from an analysis -> GROUPING_VARIABLE_NOT_LINKED.
  unlinked <- event
  for (i in seq_along(unlinked$analyses)) {
    og <- unlinked$analyses[[i]]$orderedGroupings
    keep <- Filter(function(g) !identical(g$groupingId, "GF_SEVGR1N"), og)
    unlinked$analyses[[i]]$orderedGroupings <- keep
    break
  }
  f5 <- validate_ars_model(ars_to_model(unlinked))
  expect_true(any(f5$ref == "GROUPING_VARIABLE_NOT_LINKED" & f5$severity == "FAIL"))
})

test_that("the paths survive the editor model round trip", {
  td <- withr::local_tempdir()
  event <- jsonlite::fromJSON(.asym_build(td)$ars_path, simplifyVector = FALSE)
  model <- ars_to_model(event)
  back  <- model_to_ars(model)

  out <- .rgp_output(back)
  expect_false(is.null(out))
  expect_length(out$resultGroupPaths$paths, 6L)
  expect_false(is.null(out$`_meta`$column_tree))
})
