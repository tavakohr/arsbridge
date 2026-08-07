## Self-template categorical blocks (PR B1 of the categorical expansion).
##
## A mock block with NO annotated same-variable header above it -- an
## un-annotated header over "<Reason #1> [ADSL.DCSREASN]", or numbered
## subcategory mocks under a row annotated on a different variable -- used
## to fall apart into per-mock analyses. Now the first annotated mock row
## carries the block's single analysis, its layout entry is flagged
## `self_template`, the whole run's sheet rows are recorded as
## `template_rows`, and the cell map plans the block for fill-time
## expansion.

.ce_row <- function(label, annotation = "", sheet_row = NULL) {
  row <- list(label = label, annotation = annotation,
              has_annot = nzchar(annotation))
  if (!is.null(sheet_row)) row$sheet_row <- sheet_row
  row
}

.ce_section <- function(stub_rows, tlf = "T-CE") {
  list(
    tlf_number = tlf, tlf_type = "TABLE", title = "Disposition",
    population_text = "All", population_annot = "",
    footnotes = character(), source_datasets = "ADSL",
    col_headers = c("", "Drug A", "Placebo"), n_data_cols = 2L,
    ars_method_name = "Count and Percentage",
    by_variable = "TRT01A", by_variable_dataset = "ADSL",
    stub_rows = stub_rows, enriched_rows = list()
  )
}


## --- detection ---------------------------------------------------------------

test_that("a lone annotated mock under an un-annotated header is a self-template", {
  rows <- list(
    .ce_row("Primary reason for discontinuation from the study, n (%)"),
    .ce_row("<Reason #1>", "ADSL.DCSREASN")
  )
  roles <- .detect_nested_token_blocks(rows, list())
  expect_equal(roles, c(NA_character_, "self_template"))
})

test_that("numbered mocks under a differently-annotated row are one self-template run", {
  rows <- list(
    .ce_row("ELIGIBILITY CRITERIA NOT MET",
            "ADDV.DVCAT='ELIGIBILITY CRITERIA NOT MET'"),
    .ce_row("Protocol Deviation Subcategory #1", "ADDV.DVTERM"),
    .ce_row("Protocol Deviation Subcategory #2"),
    .ce_row("Protocol Deviation Subcategory #n")
  )
  roles <- .detect_nested_token_blocks(rows, list())
  expect_equal(roles[1], NA_character_)
  expect_equal(roles[2], "self_template")
  expect_true(all(roles[3:4] == "level_repeat"))
})

test_that("the annotated mock carries the self-template even when it is not first", {
  rows <- list(
    .ce_row("Header, n (%)"),
    .ce_row("<Term #1>"),
    .ce_row("<Term #2>", "ADAE.AEDECOD"),
    .ce_row("<Term #n>")
  )
  roles <- .detect_nested_token_blocks(rows, list())
  expect_equal(roles[2], "level_repeat")
  expect_equal(roles[3], "self_template")
  expect_equal(roles[4], "level_repeat")
})

test_that("a same-variable annotated header above still wins as level_repeat", {
  ## The classic convention shape must NOT become a self-template: the
  ## header's analysis owns the block.
  rows <- list(
    .ce_row("Primary reason for discontinuation, n (%)", "ADSL.DCSREASN"),
    .ce_row("<Reason #1>", "ADSL.DCSREASN"),
    .ce_row("<Reason #2>", "ADSL.DCSREASN")
  )
  roles <- .detect_nested_token_blocks(rows, list())
  expect_equal(roles[1], NA_character_)
  expect_true(all(roles[2:3] == "level_repeat"))
  expect_false(any(roles == "self_template", na.rm = TRUE))
})


## --- build: layout flags and template rows -----------------------------------

test_that("a self-template block flags its layout entry and records its rows", {
  diag_reset()
  sec <- .ce_section(list(
    .ce_row("Primary reason for discontinuation from the study, n (%)",
            sheet_row = 10L),
    .ce_row("<Reason #1>", "ADSL.DCSREASN", sheet_row = 11L),
    .ce_row("<Reason #2>", sheet_row = 12L),
    .ce_row("...", sheet_row = 13L)
  ))
  re <- build_ars_json(list(sec), study_id = "S-ST")

  ## One analysis: the annotated mock's. The bare repeats contributed none.
  expect_length(re$analyses, 1)

  layout <- re$outputs[[1]][["_meta"]][["shell_layout"]]
  expect_length(layout, 2)
  expect_equal(layout[[1]]$kind, "label")
  entry <- layout[[2]]
  expect_equal(entry$kind, "categorical")
  expect_true(isTRUE(entry$self_template))
  ## The block owns the whole run: the annotated mock's own row first, the
  ## bare repeats and the trailing "..." behind it.
  expect_equal(entry$template_rows, c(11L, 12L, 13L))

  d <- diag_records()
  expect_true(any(grepl("self-template", d$problem)))
})

test_that("a continuous self-token row is not flagged for expansion", {
  ## Only a categorical block expands one row per level. A token row that
  ## routes to a continuous method keeps its analysis un-flagged.
  sec <- .ce_section(list(
    .ce_row("Visit summary", sheet_row = 5L),
    .ce_row("<Visit #1>", "ADSL.AGE", sheet_row = 6L)
  ))
  sec$ars_method_name <- "Summary Statistics - Continuous"
  re <- build_ars_json(list(sec), study_id = "S-CT")
  layout <- re$outputs[[1]][["_meta"]][["shell_layout"]]
  entry <- layout[[length(layout)]]
  expect_false(isTRUE(entry$self_template))
  expect_null(entry$template_rows)
})


## --- the categorical fill plan ------------------------------------------------

test_that(".build_categorical_fills plans one block per template entry", {
  sec <- list(tlf_number = "T-CF", sheet_name = "Table X")
  shell_layout <- list(
    list(order = 1L, label = "Header", analysis_id = NA_character_,
         kind = "label", sheet_row = 5L),
    list(order = 2L, label = "Reasons, n (%)", analysis_id = "AN_T_CF_001",
         kind = "categorical", sheet_row = 5L,
         template_rows = c(6L, 7L, 8L), sort = "alphabetical"),
    list(order = 3L, label = "<Term #1>", analysis_id = "AN_T_CF_002",
         kind = "categorical", sheet_row = 12L,
         template_rows = c(12L, 13L), self_template = TRUE)
  )
  blocks <- .build_categorical_fills(sec, shell_layout)
  expect_length(blocks, 2)
  expect_equal(blocks[[1]]$anchor_row, 6L)
  expect_equal(blocks[[1]]$template_rows, 6:8)
  expect_equal(blocks[[1]]$analysis_id, "AN_T_CF_001")
  expect_equal(blocks[[1]]$sort, "alphabetical")
  expect_false(blocks[[1]]$self_template)
  expect_equal(blocks[[2]]$anchor_row, 12L)
  expect_true(blocks[[2]]$self_template)
})

test_that("a non-contiguous template block is skipped with a WARN", {
  diag_reset()
  sec <- list(tlf_number = "T-CG", sheet_name = "Table Y")
  shell_layout <- list(
    list(order = 1L, label = "Reasons, n (%)", analysis_id = "AN_T_CG_001",
         kind = "categorical", sheet_row = 5L, template_rows = c(6L, 9L))
  )
  blocks <- .build_categorical_fills(sec, shell_layout)
  expect_length(blocks, 0)
  d <- diag_records()
  expect_true(any(d$severity == "WARN" & grepl("contiguous", d$problem)))
})

test_that("entries without template rows plan nothing", {
  sec <- list(tlf_number = "T-CH", sheet_name = "Table Z")
  shell_layout <- list(
    list(order = 1L, label = "Sex, n (%)", analysis_id = "AN_T_CH_001",
         kind = "categorical", sheet_row = 5L),
    list(order = 2L, label = "Female", analysis_id = "AN_T_CH_001",
         kind = "level", level = "F", sheet_row = 6L)
  )
  expect_length(.build_categorical_fills(sec, shell_layout), 0)
})


## --- renderer: no mock header line --------------------------------------------

test_that("a self-template block renders levels without the mock header line", {
  sec <- .ce_section(list(
    .ce_row("Primary reason for discontinuation from the study, n (%)"),
    .ce_row("<Reason #1>", "ADSL.DCSREASN")
  ))
  sec$col_headers <- c("", "Placebo")
  sec$n_data_cols <- 1L
  re <- suppressMessages(suppressWarnings(
    build_ars_json(list(sec), study_id = "S-RD")))
  o <- re$outputs[[1]]
  layout <- .shell_layout(o)
  aid <- layout$analysis_id[layout$kind == "categorical"][1]

  ard <- data.frame(
    output_id = o$id, analysis_id = aid,
    method_id = "MTH_COUNT_AND_PERCENTAGE",
    variable = "DCSREASN",
    variable_level = c("DEATH", "LOST TO FOLLOW-UP"),
    group1_level = "Placebo",
    stat_name = "n", stat = c(4, 2), stringsAsFactors = FALSE)
  prep <- .tfrmt_prep_ard_layout(
    ard, o$id, layout, col_var = "group1_level", keep_params = "n",
    col_levels = "Placebo", fixed_vars = "TRT01A",
    params_map = list(MTH_COUNT_AND_PERCENTAGE = "n"))

  ## The mock text never prints; the levels take the block's place.
  expect_false("<Reason #1>" %in% prep[[.ARS_SHELL_LBL]])
  expect_true(all(c("DEATH", "LOST TO FOLLOW-UP") %in%
                    prep[[.ARS_SHELL_LBL]]))
})
