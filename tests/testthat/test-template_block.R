## Template-block retention, end to end (categorical expansion, PR A).
##
## A shell authors a categorical block whose levels are unknown at authoring
## time: a header annotated with the bare variable, then `<Reason #k>` mock
## rows. The mocks must collapse into the header's single analysis, the
## header's layout entry must record which sheet rows the block owns (so the
## fill step can later expand them), and the cell map must explain the
## untouched placeholders as awaiting expansion -- not as orphaned rows.

.tpl_spec_path <- function(td) {
  vars <- data.frame(
    Dataset   = rep("ADSL", 4),
    Variable  = c("USUBJID", "TRT01A", "SAFFL", "DCSREASN"),
    Label     = c("Unique Subject Identifier", "Actual Treatment",
                  "Safety Population Flag", "Reason for Disc from Study"),
    Type      = c("Char", "Char", "Char", "Char"),
    Origin    = c("Assigned", "Derived", "Derived", "CRF"),
    Codelist  = c("", "", "NY", ""),
    Length    = c("40", "40", "1", "60"),
    Mandatory = rep("Req", 4),
    stringsAsFactors = FALSE
  )
  path <- file.path(td, "adam_spec_tpl.xlsx")
  wb <- openxlsx2::wb_workbook() |>
    openxlsx2::wb_add_worksheet("Variables") |>
    openxlsx2::wb_add_data(sheet = "Variables", x = vars)
  openxlsx2::wb_save(wb, file = path, overwrite = TRUE)
  path
}

.tpl_shell_path <- function(td) {
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
  put(annotated("Primary reason for discontinuation from the study, n (%)",
                "[ADSL.DCSREASN]"), 5)
  ## The mocks restate the variable with illustrative level codes, closing
  ## on the generic "=n" -- the dialect the collapse must tolerate.
  put(annotated("<Reason #1>", "[ADSL.DCSREASN=1]"), 6)
  put(annotated("<Reason #2>", "[ADSL.DCSREASN=2]"), 7)
  put(annotated("<Reason #n>", "[ADSL.DCSREASN=n]"), 8)
  for (r in 6:8) for (j in 2:3) put("xx (xx.x)", r, j)
  path <- file.path(td, "shell_tpl.xlsx")
  wb$save(path)
  path
}

test_that("a categorical mock block survives as an expansion template", {
  td <- withr::local_tempdir()
  diag_reset()
  out <- file.path(td, "re.json")
  withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(suppressWarnings(spec_to_ars(
      shell_path     = .tpl_shell_path(td),
      adam_spec_path = .tpl_spec_path(td),
      api_key = "", output_path = out,
      report_path = file.path(td, "report.xlsx"), verbose = FALSE))))
  re <- jsonlite::fromJSON(out, simplifyVector = FALSE)

  ## The mocks contributed no analyses: none is labelled with placeholder
  ## text, and the header's single analysis draws the whole distribution.
  labels <- vapply(re$analyses, function(a) a$label %||% "", character(1))
  expect_false(any(grepl("<Reason", labels, fixed = TRUE)))
  expect_length(grep("Primary reason for discontinuation", labels), 1)

  layout <- re$outputs[[1]][["_meta"]][["shell_layout"]]
  parent <- Filter(function(e) grepl("Primary reason", e$label %||% ""),
                   layout)
  expect_length(parent, 1)
  parent <- parent[[1]]
  expect_equal(parent$kind, "categorical_block")
  expect_equal(as.integer(unlist(parent$template_rows)), 6:8)

  ## The block's placeholder cells in mapped columns are BOUND to the
  ## parent analysis (the fill step needs their slots to expand from), and
  ## flagged as template cells with the reason retained. Cells outside the
  ## column axis (the stub column's mock label) stay pending with the same
  ## explanation.
  fill <- re$outputs[[1]][["_meta"]][["shell_fill"]]
  parent_aid <- parent$analysis_id
  tpl_cells <- Filter(function(c) c$row %in% 6:8, fill$cells)
  expect_gte(length(tpl_cells), 6)
  for (c in tpl_cells) {
    expect_match(c$reason, "awaiting row expansion", info = c$ref)
    if (c$col %in% 2:3) {
      expect_equal(c$kind, "result", info = c$ref)
      expect_true(isTRUE(c$template), info = c$ref)
      expect_equal(c$analysis_id, parent_aid, info = c$ref)
      expect_length(c$slots, 2)
    } else {
      expect_equal(c$kind, "pending", info = c$ref)
    }
  }

  ## And the fill plan knows the block: anchor at the first mock row, the
  ## whole run recorded, owned by the parent's analysis.
  blocks <- fill$categorical
  expect_length(blocks, 1)
  expect_equal(as.integer(unlist(blocks[[1]]$template_rows)), 6:8)
  expect_equal(blocks[[1]]$anchor_row, 6L)
  expect_equal(blocks[[1]]$analysis_id, parent_aid)
  expect_false(isTRUE(blocks[[1]]$self_template))

  ## Collapse is reported as INFO; the "=n" tail never surfaces as a
  ## dropped-condition WARN.
  d <- diag_records()
  expect_equal(sum(grepl("illustrates the levels", d$problem)), 3)
  expect_false(any(d$severity == "WARN" & grepl("[Cc]ondition", d$problem)))
})

.tpl_self_shell_path <- function(td) {
  ## The other authoring shape seen in the field: the header row is plain
  ## text (no annotation) and a single mock row carries the bare variable.
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
  put("Primary reason for discontinuation from the study, n (%)", 5)
  put(annotated("<Reason #1>", "[ADSL.DCSREASN]"), 6)
  for (j in 2:3) put("xx (xx.x)", 6, j)
  path <- file.path(td, "shell_tpl_self.xlsx")
  wb$save(path)
  path
}

test_that("a lone mock under a plain header becomes a self-template block", {
  td <- withr::local_tempdir()
  out <- file.path(td, "re.json")
  withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(suppressWarnings(spec_to_ars(
      shell_path     = .tpl_self_shell_path(td),
      adam_spec_path = .tpl_spec_path(td),
      api_key = "", output_path = out,
      report_path = file.path(td, "report.xlsx"), verbose = FALSE))))
  re <- jsonlite::fromJSON(out, simplifyVector = FALSE)

  labels <- vapply(re$analyses, function(a) a$label %||% "", character(1))
  expect_length(labels, 1)

  layout <- re$outputs[[1]][["_meta"]][["shell_layout"]]
  entry <- layout[[length(layout)]]
  expect_equal(entry$kind, "categorical_block")
  expect_true(isTRUE(entry$self_template))
  expect_equal(as.integer(unlist(entry$template_rows)), 6L)

  fill <- re$outputs[[1]][["_meta"]][["shell_fill"]]
  blocks <- fill$categorical
  expect_length(blocks, 1)
  expect_equal(blocks[[1]]$anchor_row, 6L)
  expect_true(isTRUE(blocks[[1]]$self_template))

  ## The mock row's data cells are bound to its own analysis and flagged.
  row_cells <- Filter(function(c) c$row == 6 && c$col %in% 2:3, fill$cells)
  expect_length(row_cells, 2)
  for (c in row_cells) {
    expect_equal(c$kind, "result", info = c$ref)
    expect_true(isTRUE(c$template), info = c$ref)
  }
})
