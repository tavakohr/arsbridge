## The two shell shapes the 2026-08 field failures arrived as, driven end to
## end: shell -> ARS -> ARD -> filled workbook, with every expected value
## recomputed here from the raw dataset rather than read back out of the
## pipeline.
##
## Failure A was a subgroup table whose Low/Medium/High columns stayed as
## placeholders while Total filled. Its cause was an axis named after the
## variable it references FIRST: a subgroup table's compound headers
## (TRT01A AND SEX) reference TRT01A first, so they minted the same grouping
## id as a plain treatment axis elsewhere in the workbook and were dropped.
## The units are pinned in test-grouping_identity.R; what this file adds is
## the whole chain, through the real parser, ending at cells in a workbook.
##
## The assertion that would have caught the field failure is not any single
## number: it is that EVERY displayed subgroup column has one.

skip_if_not_installed("openxlsx2")
skip_if_not_installed("withr")

ACC_SHELL <- test_path("fixtures", "shells_apx_acceptance.xlsx")
ACC_ADAM  <- test_path("fixtures", "adam_apx_drm_301")
ACC_SPEC  <- test_path("fixtures", "adam_spec_apx_drm_301.xlsx")

## One run of the chain, reused by every test below.
acc_run <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    ars <- tempfile(fileext = ".json")
    withr::with_envvar(
      c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
        GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
      suppressMessages(suppressWarnings(spec_to_ars(
        shell_path = ACC_SHELL, adam_spec_path = ACC_SPEC, api_key = "",
        output_path = ars, report_path = tempfile(fileext = ".xlsx"),
        verbose = FALSE))))
    ard <- suppressMessages(suppressWarnings(ars_to_ard(ars, ACC_ADAM)))
    out <- tempfile(fileext = ".xlsx")
    res <- suppressMessages(suppressWarnings(ars_fill_shell(
      shell_path = ACC_SHELL, ars = ars, ard = ard, output_path = out,
      adam_dir = ACC_ADAM, overwrite = TRUE)))
    cache <<- list(
      ars  = jsonlite::fromJSON(ars, simplifyVector = FALSE),
      res  = res,
      book = xlsx_read_shell_cells(out)
    )
    cache
  }
})

acc_cell <- function(book, sheet, ref) {
  cells <- book$sheets[[sheet]]$cells
  hit <- cells$text[cells$ref == ref]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

acc_adsl <- function() {
  utils::read.csv(file.path(ACC_ADAM, "ADSL.csv"), stringsAsFactors = FALSE)
}

## The three subgroup columns of Table 14.4.1, in sheet order, each with the
## filter its header annotation states.
ACC_SUBGROUPS <- list(
  list(col = "B", arm = "Drug 10 mg", sex = "M"),
  list(col = "C", arm = "Drug 10 mg", sex = "F"),
  list(col = "D", arm = "Drug 20 mg", sex = "M")
)


test_that("a subgroup axis and a plain one keep their own definitions", {
  run <- acc_run()
  groupings <- run$ars$analysisGroupings

  ## Two axes over one variable: the plain treatment axis is data-driven, the
  ## subgroup axis carries the three compound columns its shell declared.
  expect_length(groupings, 2)
  fixed <- Filter(function(g) !isTRUE(g$dataDriven), groupings)
  expect_length(fixed, 1)
  expect_equal(
    vapply(fixed[[1]]$groups, function(g) g$label, character(1)),
    c("Drug 10 mg, Male", "Drug 10 mg, Female", "Drug 20 mg, Male")
  )

  ## Each output's analyses resolve to the axis its own sheet declared. This
  ## is the assertion the field failure would have failed: the subgroup
  ## table's analyses pointed at the plain axis, whose levels no subgroup
  ## header matched.
  referenced <- function(prefix) {
    unique(unlist(lapply(
      Filter(function(a) startsWith(a$id, prefix), run$ars$analyses),
      function(a) vapply(a$orderedGroupings, function(o) o$groupingId,
                         character(1))
    )))
  }
  plain    <- referenced("AN_T_14_1_1")
  subgroup <- referenced("AN_T_14_4_1")
  expect_length(plain, 1)
  expect_length(subgroup, 1)
  expect_false(identical(plain, subgroup))
  expect_equal(subgroup, fixed[[1]]$id)
})

test_that("every displayed subgroup column is filled with a number", {
  ## The field symptom, stated directly: not one cell in particular, but no
  ## cell left as a placeholder. A run that fills two of three columns and
  ## reports success is exactly the failure this is here to catch.
  run <- acc_run()

  for (row in c(5, 6)) {
    for (sub in ACC_SUBGROUPS) {
      value <- acc_cell(run$book, "Table 14.4.1", paste0(sub$col, row))
      label <- paste(sub$arm, sub$sex, "row", row)
      expect_false(is.na(value), info = label)
      expect_false(grepl("x", value, fixed = TRUE), info = label)
      expect_match(value, "^[0-9]", info = label)
    }
  }
})

test_that("subgroup counts equal the counts computed from the data", {
  run  <- acc_run()
  adsl <- acc_adsl()

  for (sub in ACC_SUBGROUPS) {
    in_column <- adsl$SAFFL == "Y" & adsl$TRT01A == sub$arm &
      adsl$SEX == sub$sex
    treated   <- sum(in_column)
    stopped   <- sum(in_column & adsl$EOSSTT == "DISCONTINUED")
    label     <- paste(sub$arm, sub$sex)

    expect_equal(acc_cell(run$book, "Table 14.4.1", paste0(sub$col, 5)),
                 as.character(treated), info = label)
    expect_equal(acc_cell(run$book, "Table 14.4.1", paste0(sub$col, 6)),
                 sprintf("%d (%.1f)", stopped, 100 * stopped / treated),
                 info = label)
  }
})

test_that("the plain table fills from its own axis, not the subgroup one", {
  ## The other half of the collision: sheet 1 must keep counting whole arms,
  ## not the sex-restricted subgroups sheet 2 defines.
  run  <- acc_run()
  adsl <- acc_adsl()

  for (col in list(c("B", "Placebo"), c("C", "Drug 10 mg"),
                   c("D", "Drug 20 mg"))) {
    treated <- sum(adsl$SAFFL == "Y" & adsl$TRT01A == col[[2]])
    expect_equal(acc_cell(run$book, "Table 14.1.1", paste0(col[[1]], 5)),
                 as.character(treated), info = col[[2]])
  }
})

test_that("the acceptance run leaves nothing pending and nothing failing", {
  run <- acc_run()

  findings <- run$res$findings
  fails <- findings[findings$severity %in% "FAIL", , drop = FALSE]
  expect_equal(nrow(fails), 0L)

  ## Both halves matter. A run that fills nothing reports no pending cells
  ## either, so the count of filled cells has to be asserted beside it.
  expect_gt(run$res$filled, 0L)
  expect_equal(run$res$pending, 0L)

  ## Every cell the census knows about is accounted for as filled or skipped;
  ## a pending one would name the stage it died at.
  pending <- run$res$census[
    run$res$census$status %in% c("pending", "missing_parent_key"), ,
    drop = FALSE
  ]
  expect_equal(nrow(pending), 0L)
})
