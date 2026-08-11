# Declared-path execution, end to end: shell -> ARS (resultGroupPaths) ->
# emitted {cards} blocks -> ARD. Six result columns, one per declared path;
# the subtotal N comes from the PARENT condition (88, larger than the 80
# subjects in the displayed children because 8 have unknown severity); no
# undeclared cohort-by-severity combination exists anywhere in the ARD.

.apx_stat <- function(ard, path_id, stat_names) {
  rows <- ard$result_group_id == path_id & ard$stat_name %in% stat_names
  rows[is.na(rows)] <- FALSE
  vapply(ard$stat[rows], function(x) as.numeric(x[[1]]), numeric(1))
}

test_that("declared paths execute into a six-column ARD with correct Ns", {
  td <- withr::local_tempdir()
  .asym_adam(td)
  res <- .asym_build(td)
  ard <- suppressMessages(suppressWarnings(ars_to_ard(res$ars_path, td)))

  expect_true("result_group_id" %in% names(ard))
  expect_true("result_group_path" %in% names(ard))

  path_ids <- sort(unique(ard$result_group_id[!is.na(ard$result_group_id)]))
  expect_length(path_ids, 6L)

  # No undeclared combination: Cohort B never crosses a severity level.
  paths_seen <- unique(ard$result_group_path[!is.na(ard$result_group_path)])
  expect_false(any(grepl("^Cohort B > ", paths_seen)))
  expect_setequal(paths_seen, c(
    "Cohort A > Mild", "Cohort A > Moderate", "Cohort A > Severe",
    "Cohort A > Total", "Cohort B", "Total"
  ))

  # Per-column subject counts from the AGE (continuous) analysis: the N stat
  # of each path is that path's own population.
  age_rows <- !is.na(ard$variable) & ard$variable == "AGE" &
    ard$stat_name == "N" & !is.na(ard$result_group_path)
  ns <- stats::setNames(
    vapply(ard$stat[age_rows], function(x) as.numeric(x[[1]]), numeric(1)),
    ard$result_group_path[age_rows]
  )
  expect_identical(unname(ns["Cohort A > Mild"]), 40)
  expect_identical(unname(ns["Cohort A > Moderate"]), 25)
  expect_identical(unname(ns["Cohort A > Severe"]), 15)
  # The subtotal is the PARENT population (88), not the child sum (80).
  expect_identical(unname(ns["Cohort A > Total"]), 88)
  expect_identical(unname(ns["Cohort B"]), 62)
  expect_identical(unname(ns["Total"]), 150)

  # The stamped group columns carry the display levels.
  mild <- ard[!is.na(ard$result_group_path) &
                ard$result_group_path == "Cohort A > Mild", ]
  expect_true(all(as.character(mild$group1_level) == "Cohort A"))
  expect_true(all(as.character(mild$group2_level) == "Mild"))
})

test_that("categorical percentages use the path population as denominator", {
  td <- withr::local_tempdir()
  adsl <- .asym_adam(td)
  res <- .asym_build(td)
  ard <- suppressMessages(suppressWarnings(ars_to_ard(res$ars_path, td)))

  # SEX in the subtotal column: n sums to 88 (all Cohort A, unknown severity
  # included) and percentages are out of 88.
  sub_rows <- !is.na(ard$result_group_path) &
    ard$result_group_path == "Cohort A > Total" &
    !is.na(ard$variable) & ard$variable == "SEX"
  n_vals <- vapply(ard$stat[sub_rows & ard$stat_name == "n"],
                   function(x) as.numeric(x[[1]]), numeric(1))
  expect_identical(sum(n_vals), 88)

  p_vals <- vapply(ard$stat[sub_rows & ard$stat_name == "p"],
                   function(x) as.numeric(x[[1]]), numeric(1))
  expect_equal(sum(p_vals), 1, tolerance = 1e-8)
})

test_that("the emitted deliverable script computes the same declared paths", {
  td <- withr::local_tempdir()
  .asym_adam(td)
  res <- .asym_build(td)

  code_dir <- file.path(td, "code")
  spec <- jsonlite::fromJSON(res$ars_path, simplifyVector = FALSE)
  files <- arsbridge:::write_tlf_code(spec, code_dir, adam_dir = td)
  expect_length(files, 1L)

  script <- paste(readLines(files[[1]]), collapse = "\n")
  # One block per declared path, no include_total pass, no case_when factor
  # derivation of the grouping variables.
  expect_true(grepl("result_group_id", script))
  expect_true(grepl("Cohort A > Total", script))
  expect_false(grepl("_total <-", script))

  env <- new.env(parent = globalenv())
  withr::with_dir(dirname(files[[1]]), {
    writeLines(sub("adam_dir <- \".\"", sprintf("adam_dir <- \"%s\"", td),
                   readLines(files[[1]]), fixed = TRUE),
               files[[1]])
    sys.source(files[[1]], envir = env)
  })
  ard_objs <- Filter(function(nm) startsWith(nm, "ard_"), ls(env))
  expect_length(ard_objs, 1L)
  ard <- get(ard_objs[[1]], envir = env)
  expect_length(unique(ard$result_group_id[!is.na(ard$result_group_id)]), 6L)
})

test_that("an unresolvable path declaration blocks execution instead of degrading", {
  td <- withr::local_tempdir()
  .asym_adam(td)
  res <- .asym_build(td)

  event <- jsonlite::fromJSON(res$ars_path, simplifyVector = FALSE)
  for (i in seq_along(event$outputs)) {
    rgp <- event$outputs[[i]]$resultGroupPaths
    if (is.null(rgp)) next
    rgp$paths[[1]]$groupIds <- list("GRP_DOES_NOT_EXIST")
    event$outputs[[i]]$resultGroupPaths <- rgp
  }
  broken_path <- file.path(td, "re_broken.json")
  jsonlite::write_json(event, broken_path, auto_unbox = TRUE, null = "null")

  expect_error(
    suppressMessages(suppressWarnings(ars_to_ard(broken_path, td))),
    "GROUPING_VARIABLE_NOT_LINKED"
  )
})

test_that("the path-mode ARD renders with one ordered column per path", {
  skip_if_not_installed("tfrmt")
  td <- withr::local_tempdir()
  .asym_adam(td)
  res <- .asym_build(td)
  ard <- suppressMessages(suppressWarnings(ars_to_ard(res$ars_path, td)))

  event <- jsonlite::fromJSON(res$ars_path, simplifyVector = FALSE)
  out_id <- NULL
  for (o in event$outputs) if (!is.null(o$resultGroupPaths)) out_id <- o$id

  tf <- suppressMessages(suppressWarnings(
    ars_to_tfrmt(res$ars_path, ard, out_id)
  ))
  expect_identical(attr(tf, "arsbridge_col_var"), "result_group_path")
  expect_identical(attr(tf, "arsbridge_col_levels"), c(
    "Cohort A > Mild", "Cohort A > Moderate", "Cohort A > Severe",
    "Cohort A > Total", "Cohort B", "Total"
  ))

  # And the table actually builds, columns in shell order.
  gt_tbl <- suppressMessages(suppressWarnings(
    ars_render_tlf(res$ars_path, ard, out_id)
  ))
  expect_s3_class(gt_tbl, "gt_tbl")
})
