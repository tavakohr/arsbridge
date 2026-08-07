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

test_that("an authored sort clause on a categorical block rides the layout entry", {
  sec <- .ce_section(list(
    .ce_row("Primary reason for discontinuation, n (%)",
            "ADSL.DCSREASN; sort: alphabetical", sheet_row = 5L),
    .ce_row("<Reason #1>", "ADSL.DCSREASN", sheet_row = 6L)
  ))
  re <- build_ars_json(list(sec), study_id = "S-SO")
  layout <- re$outputs[[1]][["_meta"]][["shell_layout"]]
  entry <- layout[[1]]
  expect_equal(entry$kind, "categorical")
  expect_equal(entry$sort, "alphabetical")
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


## ===========================================================================
## PR B2: the expansion itself -- template rows become one filled row per
## level in the written workbook.
## ===========================================================================

## Level order helper, in isolation ------------------------------------------

.ce_index <- function() {
  ## Appearance order deliberately NOT alphabetical and NOT by frequency:
  ## OTHER (n=6) comes last, DEATH (n=4) first, LOST TO FOLLOW-UP (n=2)
  ## in the middle -- so each ordering mode gives a different answer.
  data.frame(
    analysis_id    = "AN_1",
    group_level    = "Placebo",
    group_fold     = .fold_label("Placebo"),
    variable_level = c("DEATH", "LOST TO FOLLOW-UP", "OTHER"),
    nest_level     = NA_character_,
    stat_name      = "n",
    value          = c(4, 2, 6),
    status         = "computed",
    stringsAsFactors = FALSE
  )
}

test_that(".categorical_block_levels keeps ARD appearance order by default", {
  block <- list(analysis_id = "AN_1", sort = NA_character_)
  expect_equal(.categorical_block_levels(.ce_index(), block),
               c("DEATH", "LOST TO FOLLOW-UP", "OTHER"))
})

test_that(".categorical_block_levels honours authored sort overrides", {
  block <- list(analysis_id = "AN_1", sort = "alphabetical")
  expect_equal(.categorical_block_levels(.ce_index(), block),
               c("DEATH", "LOST TO FOLLOW-UP", "OTHER"))

  block$sort <- "desc-freq"
  expect_equal(.categorical_block_levels(.ce_index(), block),
               c("OTHER", "DEATH", "LOST TO FOLLOW-UP"))
})

test_that(".categorical_block_levels returns nothing for an absent analysis", {
  block <- list(analysis_id = "AN_MISSING", sort = NA_character_)
  expect_length(.categorical_block_levels(.ce_index(), block), 0)
})

## The full chain --------------------------------------------------------------

.ce_spec_file <- function(td, with_codelist = TRUE) {
  vars <- data.frame(
    Dataset   = rep("ADSL", 5),
    Variable  = c("USUBJID", "TRT01A", "SAFFL", "DCSREASN", "SEX"),
    Label     = c("Unique Subject Identifier", "Actual Treatment",
                  "Safety Population Flag", "Reason for Disc from Study",
                  "Sex"),
    Type      = c("Char", "Char", "Char",
                  if (with_codelist) "integer" else "Char", "Char"),
    Origin    = rep("Derived", 5),
    Codelist  = c("", "", "", if (with_codelist) "DCSREAS" else "",
                  if (with_codelist) "SEX" else ""),
    Length    = c("40", "40", "1", "60", "1"),
    Mandatory = rep("Req", 5),
    stringsAsFactors = FALSE
  )
  path <- file.path(td, "spec_ce.xlsx")
  wb <- openxlsx2::wb_workbook() |>
    openxlsx2::wb_add_worksheet("Variables") |>
    openxlsx2::wb_add_data(sheet = "Variables", x = vars)
  if (with_codelist) {
    cls <- data.frame(
      `Codelist Name`     = c("DCSREAS", "", "", "SEX", ""),
      `Term (Code)`       = c("1", "2", "3", "F", "M"),
      `Decoded Value`     = c("DEATH", "LOST TO FOLLOW-UP", "OTHER",
                              "Female", "Male"),
      `Used By Variables` = c("ADSL.DCSREASN", "", "", "ADSL.SEX", ""),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    wb <- wb |>
      openxlsx2::wb_add_worksheet("CODELISTS") |>
      openxlsx2::wb_add_data(sheet = "CODELISTS", x = cls)
  }
  openxlsx2::wb_save(wb, file = path, overwrite = TRUE)
  path
}

.ce_adam_dir <- function(td, dcsreasn = NULL) {
  adam <- file.path(td, "adam")
  dir.create(adam, showWarnings = FALSE)
  ## Arms differ on purpose (3/2 vs 1/4): a column mix-up cannot pass.
  adsl <- data.frame(
    USUBJID  = sprintf("S%02d", 1:10),
    TRT01A   = rep(c("Drug A", "Placebo"), each = 5),
    SAFFL    = "Y",
    DCSREASN = dcsreasn %||% c(1, 1, 1, 2, 2, 1, 2, 2, 2, 2),
    SEX      = c("F", "F", "F", "M", "M", "F", "M", "M", "M", "M"),
    stringsAsFactors = FALSE
  )
  utils::write.csv(adsl, file.path(adam, "ADSL.csv"), row.names = FALSE)
  adam
}

.ce_wb_start <- function() {
  wb <- openxlsx2::wb_workbook()$add_worksheet("Table 14.1.1")
  put <- function(x, row, col = 1L) {
    wb$add_data(sheet = "Table 14.1.1", x = x, start_row = row,
                start_col = col, col_names = FALSE)
  }
  black <- openxlsx2::wb_color(hex = "FF000000")
  red   <- openxlsx2::wb_color(hex = "FFC00000")
  annotated <- function(label, annotation) {
    openxlsx2::fmt_txt(label, color = black, size = 10) +
      openxlsx2::fmt_txt(paste0("\n", annotation), color = red, size = 8,
                         italic = TRUE)
  }
  put("Table 14.1.1", 1)
  put("Summary of Subject Disposition", 2)
  put(annotated("Safety Population ", "(ADSL.SAFFL='Y')"), 3)
  for (r in 1:3) {
    wb$merge_cells(sheet = "Table 14.1.1", dims = sprintf("A%d:C%d", r, r))
  }
  put(annotated("Item", "[columns -> ADSL.TRT01A; source ADSL]"), 4)
  put("Drug A", 4, 2L)
  put("Placebo", 4, 3L)
  list(wb = wb, put = put, annotated = annotated)
}

.ce_fill_chain <- function(td, shell_path, spec_path, adam_dir) {
  ars <- file.path(td, "re.json")
  withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(suppressWarnings(spec_to_ars(
      shell_path = shell_path, adam_spec_path = spec_path, api_key = "",
      output_path = ars, report_path = file.path(td, "rep.xlsx"),
      verbose = FALSE))))
  ard <- suppressMessages(suppressWarnings(ars_to_ard(ars, adam_dir)))
  out <- file.path(td, "filled.xlsx")
  res <- suppressMessages(suppressWarnings(ars_fill_shell(
    shell_path = shell_path, ars = ars, ard = ard, output_path = out,
    adam_dir = adam_dir, overwrite = TRUE)))
  list(res = res, out = out, book = xlsx_read_shell_cells(out))
}

.ce_text <- function(book, ref, sheet = "Table 14.1.1") {
  cells <- book$sheets[[sheet]]$cells
  hit <- cells$text[cells$ref == ref]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

test_that("a convention-shape block expands to one filled row per codelist level", {
  td <- withr::local_tempdir()
  s <- .ce_wb_start()
  s$put(s$annotated("Primary reason for discontinuation, n (%)",
                    "[ADSL.DCSREASN]"), 5)
  ## Two mock rows, three codelist levels: the block must GROW by one row.
  s$put(s$annotated("<Reason #1>", "[ADSL.DCSREASN=1]"), 6)
  s$put(s$annotated("<Reason #n>", "[ADSL.DCSREASN=n]"), 7)
  for (r in 6:7) for (j in 2:3) s$put("xx (xx.x)", r, j)
  s$put("Source: synthetic.", 8)
  shell <- file.path(td, "shell.xlsx")
  s$wb$save(shell)

  run <- .ce_fill_chain(td, shell, .ce_spec_file(td), .ce_adam_dir(td))

  ## Levels in codelist order, decoded labels, the unobserved one included.
  expect_equal(.ce_text(run$book, "A6"), "DEATH")
  expect_equal(.ce_text(run$book, "A7"), "LOST TO FOLLOW-UP")
  expect_equal(.ce_text(run$book, "A8"), "OTHER")

  ## Values computed from the data, formatted by the template's placeholder.
  expect_equal(.ce_text(run$book, "B6"), "3 (60.0)")
  expect_equal(.ce_text(run$book, "C6"), "1 (20.0)")
  expect_equal(.ce_text(run$book, "B7"), "2 (40.0)")
  expect_equal(.ce_text(run$book, "C7"), "4 (80.0)")
  expect_equal(.ce_text(run$book, "B8"), "0 (0.0)")
  expect_equal(.ce_text(run$book, "C8"), "0 (0.0)")

  ## The footnote below the block moved down with the shift, intact.
  expect_equal(.ce_text(run$book, "A9"), "Source: synthetic.")

  ## The workbook is one Excel will actually show.
  expect_rows_well_formed(run$out)

  ## Nothing in the block is left on placeholder, and the awaiting-expansion
  ## reason is gone from the census.
  reasons <- run$res$diagnostics$reason %||% character()
  expect_false(any(grepl("awaiting row expansion", reasons)))
})

test_that("a self-template block expands in place of its mock row", {
  td <- withr::local_tempdir()
  s <- .ce_wb_start()
  s$put("Primary reason for discontinuation from the study, n (%)", 5)
  s$put(s$annotated("<Reason #1>", "[ADSL.DCSREASN]"), 6)
  for (j in 2:3) s$put("xx (xx.x)", 6, j)
  s$put("Source: synthetic.", 7)
  shell <- file.path(td, "shell.xlsx")
  s$wb$save(shell)

  run <- .ce_fill_chain(td, shell, .ce_spec_file(td), .ce_adam_dir(td))

  ## The single mock became three level rows; the header above is untouched.
  expect_equal(.ce_text(run$book, "A5"),
               "Primary reason for discontinuation from the study, n (%)")
  expect_equal(.ce_text(run$book, "A6"), "DEATH")
  expect_equal(.ce_text(run$book, "A7"), "LOST TO FOLLOW-UP")
  expect_equal(.ce_text(run$book, "A8"), "OTHER")
  expect_equal(.ce_text(run$book, "B6"), "3 (60.0)")
  expect_equal(.ce_text(run$book, "C7"), "4 (80.0)")
  expect_equal(.ce_text(run$book, "A9"), "Source: synthetic.")
  expect_rows_well_formed(run$out)
})

test_that("leftover template rows are blanked when levels run out", {
  td <- withr::local_tempdir()
  s <- .ce_wb_start()
  s$put(s$annotated("Primary reason for discontinuation, n (%)",
                    "[ADSL.DCSREASN]"), 5)
  ## Three mock rows; without a codelist the data offers only two levels.
  s$put(s$annotated("<Reason #1>", "[ADSL.DCSREASN]"), 6)
  s$put(s$annotated("<Reason #2>", "[ADSL.DCSREASN]"), 7)
  s$put(s$annotated("<Reason #n>", "[ADSL.DCSREASN]"), 8)
  for (r in 6:8) for (j in 2:3) s$put("xx (xx.x)", r, j)
  s$put("Source: synthetic.", 9)
  shell <- file.path(td, "shell.xlsx")
  s$wb$save(shell)

  run <- .ce_fill_chain(
    td, shell, .ce_spec_file(td, with_codelist = FALSE),
    .ce_adam_dir(td, dcsreasn = c("DEATH", "DEATH", "DEATH", "LOST",
                                  "LOST", "DEATH", "LOST", "LOST",
                                  "LOST", "LOST")))

  expect_equal(.ce_text(run$book, "A6"), "DEATH")
  expect_equal(.ce_text(run$book, "A7"), "LOST")
  expect_equal(.ce_text(run$book, "B6"), "3 (60.0)")
  expect_equal(.ce_text(run$book, "B7"), "2 (40.0)")

  ## The third template row is cleared, not left showing "<Reason #n>".
  expect_true(is.na(.ce_text(run$book, "A8")) ||
                identical(.ce_text(run$book, "A8"), ""))
  expect_true(is.na(.ce_text(run$book, "B8")) ||
                identical(.ce_text(run$book, "B8"), ""))
  ## The footnote did not move: the sheet never grew.
  expect_equal(.ce_text(run$book, "A9"), "Source: synthetic.")

  cleared <- Filter(function(i) identical(i, TRUE), lapply(
    seq_len(nrow(run$res$diagnostics)), function(i) {
      grepl("leftover", run$res$diagnostics$reason[i])
    }))
  expect_gte(length(cleared), 1)
  expect_rows_well_formed(run$out)
})

test_that("two categorical blocks on one sheet both expand, bottom-up", {
  td <- withr::local_tempdir()
  s <- .ce_wb_start()
  s$put(s$annotated("Primary reason for discontinuation, n (%)",
                    "[ADSL.DCSREASN]"), 5)
  s$put(s$annotated("<Reason #1>", "[ADSL.DCSREASN=1]"), 6)
  s$put(s$annotated("<Reason #n>", "[ADSL.DCSREASN=n]"), 7)
  s$put("Sex, n (%)", 9)
  s$put(s$annotated("<Sex #1>", "[ADSL.SEX]"), 10)
  for (r in c(6:7, 10)) for (j in 2:3) s$put("xx (xx.x)", r, j)
  s$put("Source: synthetic.", 12)
  shell <- file.path(td, "shell.xlsx")
  s$wb$save(shell)

  run <- .ce_fill_chain(td, shell, .ce_spec_file(td), .ce_adam_dir(td))

  ## Upper block grew by one (3 levels over 2 rows)...
  expect_equal(.ce_text(run$book, "A6"), "DEATH")
  expect_equal(.ce_text(run$book, "A8"), "OTHER")
  ## ...so the lower block sits one row further down, expanded to two rows
  ## (its own single mock grew by one as well).
  expect_equal(.ce_text(run$book, "A10"), "Sex, n (%)")
  expect_equal(.ce_text(run$book, "A11"), "Female")
  expect_equal(.ce_text(run$book, "A12"), "Male")
  expect_equal(.ce_text(run$book, "B11"), "3 (60.0)")
  expect_equal(.ce_text(run$book, "C12"), "4 (80.0)")
  ## The footnote moved down by both insertions.
  expect_equal(.ce_text(run$book, "A14"), "Source: synthetic.")
  expect_rows_well_formed(run$out)
})

test_that("an unshiftable sheet declines the expansion but fills fixed cells", {
  td <- withr::local_tempdir()
  s <- .ce_wb_start()
  s$put(s$annotated("Subjects in population, n", "[ADSL.USUBJID]"), 5)
  for (j in 2:3) s$put("xx", 5, j)
  s$put(s$annotated("Primary reason for discontinuation, n (%)",
                    "[ADSL.DCSREASN]"), 6)
  s$put(s$annotated("<Reason #1>", "[ADSL.DCSREASN=1]"), 7)
  s$put(s$annotated("<Reason #n>", "[ADSL.DCSREASN=n]"), 8)
  for (r in 7:8) for (j in 2:3) s$put("xx (xx.x)", r, j)
  ## A formula below the block: shifting would leave it pointing wrong.
  s$wb$add_formula(sheet = "Table 14.1.1", x = "SUM(B7:B8)", start_row = 10,
                   start_col = 2)
  shell <- file.path(td, "shell.xlsx")
  s$wb$save(shell)

  diag_reset()
  run <- .ce_fill_chain(td, shell, .ce_spec_file(td), .ce_adam_dir(td))

  ## The fixed count row above still filled.
  expect_equal(.ce_text(run$book, "B5"), "5")
  expect_equal(.ce_text(run$book, "C5"), "5")
  ## The block declined: mock text still there, skip recorded, FAIL diag.
  expect_equal(.ce_text(run$book, "A7"), "<Reason #1>")
  skipped <- run$res$diagnostics
  expect_true(any(skipped$status == "skipped" &
                    grepl("cannot be shifted", skipped$reason)))
  d <- diag_records()
  expect_true(any(d$severity == "FAIL" &
                    grepl("cannot be expanded", d$problem)))
})
