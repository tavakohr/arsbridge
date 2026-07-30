## The cell map: `outputs[[i]]$_meta$shell_fill`.
##
## This is the part of the Excel work with the least room for error -- it
## decides which number lands in which cell -- so the tests are about the
## JOIN, not about the shape of the list.

fill_ars <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    out <- tempfile(fileext = ".json")
    withr::with_envvar(
      c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
        GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
      suppressMessages(suppressWarnings(spec_to_ars(
        shell_path     = test_path("fixtures", "shells_apx_drm_301.xlsx"),
        adam_spec_path = test_path("fixtures", "adam_spec_apx_drm_301.xlsx"),
        api_key = "", output_path = out,
        report_path = tempfile(fileext = ".xlsx"), verbose = FALSE))))
    cached <<- jsonlite::fromJSON(out, simplifyVector = FALSE)
    cached
  }
})

output_of <- function(id) {
  a <- fill_ars()
  hit <- Filter(function(o) identical(o$id, id), a$outputs)
  expect_length(hit, 1L)
  hit[[1]]
}

fill_of <- function(id) output_of(id)$`_meta`$shell_fill

cell_at <- function(id, ref) {
  hit <- Filter(function(c) identical(c$ref, ref), fill_of(id)$cells)
  expect_length(hit, 1L)
  hit[[1]]
}

## ---------------------------------------------------------------------------
## Presence
## ---------------------------------------------------------------------------

test_that("every Excel output carries a cell map", {
  for (o in fill_ars()$outputs) {
    expect_false(is.null(o$`_meta`$shell_fill), info = o$id)
    expect_equal(o$`_meta`$shell_fill$source$format, "xlsx")
  }
})

test_that("the map records where the sheet's body starts", {
  src <- fill_of("T_14_1_1")$source
  expect_equal(src$sheet, "Table 14.1.1")
  expect_equal(src$header_row, 4L)
  expect_equal(src$first_body_row, 5L)
})

test_that("a Word shell gets no cell map", {
  ## There are no cell addresses to fill. Emitting an empty map would make
  ## `ars_fill_shell()` look applicable to a document it cannot write.
  out <- withr::local_tempfile(fileext = ".json")
  withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(suppressWarnings(spec_to_ars(
      shell_path     = test_path("fixtures", "shells_parity_apx.docx"),
      adam_spec_path = test_path("fixtures", "adam_spec_apx_drm_301.xlsx"),
      api_key = "", output_path = out,
      report_path = withr::local_tempfile(fileext = ".xlsx"),
      verbose = FALSE))))
  ars <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  for (o in ars$outputs) expect_null(o$`_meta`$shell_fill)
})

## ---------------------------------------------------------------------------
## The three row shapes
## ---------------------------------------------------------------------------

test_that("a row with its own analysis binds to it", {
  cell <- cell_at("T_14_1_1", "B5")
  expect_equal(cell$kind, "result")
  expect_match(cell$analysis_id, "^AN_T_14_1_1_")
  expect_null(cell$variable_level)
  expect_null(cell$stat_line)
})

test_that("a statistic line takes only the statistics its label names", {
  ## "Median" under a continuous parent shows the median -- NOT the whole
  ## summary the method computes. Getting this wrong would put the mean in
  ## the median's row.
  median_cell <- cell_at("T_14_1_2", "B7")
  expect_equal(median_cell$stat_line, "Median")
  expect_length(median_cell$slots, 1L)
  expect_equal(median_cell$slots[[1]]$stat_name, "median")

  mean_cell <- cell_at("T_14_1_2", "B6")
  expect_equal(mean_cell$stat_line, "Mean (SD)")
  expect_equal(vapply(mean_cell$slots, function(s) s$stat_name, character(1)),
               c("mean", "sd"))

  quartiles <- cell_at("T_14_1_2", "B8")
  expect_equal(vapply(quartiles$slots, function(s) s$stat_name, character(1)),
               c("p25", "p75"))
})

test_that("statistic lines share their parent's analysis", {
  ids <- vapply(c("B6", "B7", "B8"),
                function(r) cell_at("T_14_1_2", r)$analysis_id, character(1))
  expect_length(unique(ids), 1L)
})

test_that("a level row selects one value of the parent's variable", {
  female <- cell_at("T_14_1_2", "B11")
  male   <- cell_at("T_14_1_2", "B12")
  expect_equal(female$variable_level, "Female")
  expect_equal(male$variable_level, "Male")
  expect_equal(female$analysis_id, male$analysis_id)
  ## ... and shows the method's own statistics for that value.
  expect_equal(vapply(female$slots, function(s) s$stat_name, character(1)),
               c("n", "p"))
})

## ---------------------------------------------------------------------------
## The join keys
## ---------------------------------------------------------------------------

test_that("statistics are named as the ARD names them, not as the method labels them", {
  ## MTH_COUNT_AND_PERCENTAGE calls its operations "Count" and "Percentage";
  ## the ARD carries "n" and "p". Joining on the display label finds nothing
  ## and silently reports a computable cell as pending.
  stats <- vapply(cell_at("T_14_1_2", "B11")$slots,
                  function(s) s$stat_name, character(1))
  expect_equal(stats, c("n", "p"))
  expect_false(any(stats %in% c("Count", "Percentage")))

  ## The operation id is kept alongside, so the binding stays traceable.
  ops <- vapply(cell_at("T_14_1_2", "B11")$slots,
                function(s) s$operation_id, character(1))
  expect_equal(ops, c("OP_N", "OP_PCT"))
})

test_that("the denominator operation is not given a cell", {
  ## MTH_COUNT_AND_PERCENTAGE declares OP_DENOM as a working quantity. Leaving
  ## it in the slot list would shift every statistic after it by one.
  for (ref in c("B11", "B12")) {
    ops <- vapply(cell_at("T_14_1_2", ref)$slots,
                  function(s) s$operation_id, character(1))
    expect_false("OP_DENOM" %in% ops)
  }
})

test_that("each result column is bound to its position on the column axis", {
  fill <- fill_of("T_14_1_1")
  expect_equal(vapply(fill$columns, function(c) c$col, integer(1)), 2:4)
  expect_equal(vapply(fill$columns, function(c) c$order, integer(1)), 1:3)
  expect_equal(vapply(fill$columns, function(c) c$label, character(1)),
               c("Placebo", "Drug 10 mg", "Drug 20 mg"))

  ## and the cell carries which one it is
  expect_equal(cell_at("T_14_1_1", "B5")$group$order, 1L)
  expect_equal(cell_at("T_14_1_1", "D5")$group$order, 3L)
  expect_equal(cell_at("T_14_1_1", "D5")$group$label, "Drug 20 mg")
  expect_equal(cell_at("T_14_1_1", "D5")$group$grouping_id, "GF_TRT01A")
})

## ---------------------------------------------------------------------------
## The placeholder is the format
## ---------------------------------------------------------------------------

test_that("decimals come from the placeholder, per slot", {
  slots <- cell_at("T_14_1_2", "B6")$slots      ## "xx.x (xx.xx)"
  expect_equal(vapply(slots, function(s) s$decimals, integer(1)), c(1L, 2L))

  count <- cell_at("T_14_1_2", "B11")$slots     ## "xx (xx.x)"
  expect_equal(vapply(count, function(s) s$decimals, integer(1)), c(0L, 1L))
})

test_that("each slot records where in the cell text it sits", {
  ## So the writer can substitute in place and keep the author's punctuation.
  slots <- cell_at("T_14_1_2", "B11")$slots
  expect_equal(slots[[1]]$start, 1L)
  expect_true(slots[[2]]$start > slots[[1]]$stop)
  expect_equal(cell_at("T_14_1_2", "B11")$placeholder, "xx xx.x")
})

## ---------------------------------------------------------------------------
## Uniqueness -- no cell may claim another's result
## ---------------------------------------------------------------------------

test_that("no two cells claim the same result", {
  ## The bijection that matters: if two cells resolved to the same
  ## (analysis, group, level, statistic) they would print the same number
  ## twice, and some other cell would be silently empty.
  for (o in fill_ars()$outputs) {
    fill <- o$`_meta`$shell_fill
    cells <- Filter(function(c) identical(c$kind, "result"),
                    fill$cells %||% list())
    if (length(cells) == 0) next

    keys <- unlist(lapply(cells, function(c) {
      vapply(c$slots %||% list(), function(s) {
        paste(c$analysis_id, c$group$order, c$variable_level %||% "-",
              c$stat_line %||% "-", s$stat_name, sep = "|")
      }, character(1))
    }))
    expect_equal(anyDuplicated(keys), 0L, info = o$id)
  }
})

test_that("no two cell records address the same cell", {
  for (o in fill_ars()$outputs) {
    refs <- vapply(o$`_meta`$shell_fill$cells %||% list(),
                   function(c) c$ref, character(1))
    expect_equal(anyDuplicated(refs), 0L, info = o$id)
  }
})

test_that("only placeholder cells are mapped -- never a label", {
  ## Writing a number over a row label is the failure this guards. Every
  ## mapped cell must sit in a result column, never in the stub.
  for (o in fill_ars()$outputs) {
    cols <- vapply(o$`_meta`$shell_fill$cells %||% list(),
                   function(c) c$col, integer(1))
    if (length(cols) > 0) expect_true(all(cols >= 2L), info = o$id)
  }
})

## ---------------------------------------------------------------------------
## Pending cells say why
## ---------------------------------------------------------------------------

test_that("a mapped cell is either bound or explained", {
  for (o in fill_ars()$outputs) {
    for (c in o$`_meta`$shell_fill$cells %||% list()) {
      if (identical(c$kind, "result")) {
        expect_true(nzchar(c$analysis_id %||% ""), info = c$ref)
      } else {
        expect_equal(c$kind, "pending", info = c$ref)
        expect_true(nzchar(c$reason %||% ""), info = c$ref)
      }
    }
  }
})

test_that("a placeholder asking for more statistics than exist is reported", {
  ## The fixture's disposition rows show "xx (xx.x)" but are typed as plain
  ## counts -- a real disagreement between the shell and the analysis typing,
  ## which used to go unnoticed because nothing compared the two.
  ##
  ## Built fresh rather than from the cache: diagnostics are global and the
  ## last run wins, so a cached result would be checked against some other
  ## build's findings.
  out <- withr::local_tempfile(fileext = ".json")
  withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(suppressWarnings(spec_to_ars(
      shell_path     = test_path("fixtures", "shells_apx_drm_301.xlsx"),
      adam_spec_path = test_path("fixtures", "adam_spec_apx_drm_301.xlsx"),
      api_key = "", output_path = out,
      report_path = withr::local_tempfile(fileext = ".xlsx"),
      verbose = FALSE))))

  d <- ars_diagnostics()
  hits <- grepl("but its analysis produces", d$problem)
  expect_true(any(hits))
  expect_equal(unique(d$severity[hits]), "WARN")

  ## The surplus slot is unbound rather than bound to the wrong statistic.
  slots <- cell_at("T_14_1_1", "B6")$slots
  expect_equal(slots[[1]]$stat_name, "n")
  expect_true(is.na(slots[[2]]$stat_name %||% NA_character_))
})

## ---------------------------------------------------------------------------
## Listings and figures get plans, not cells
## ---------------------------------------------------------------------------

test_that("a listing gets a template plan", {
  fill <- fill_of("L_16_2_1")
  expect_length(fill$cells, 0L)
  expect_equal(fill$listing$header_row, 4L)
  expect_equal(fill$listing$template_row, 5L)
  expect_equal(vapply(fill$listing$columns, function(c) c$annotation,
                      character(1)),
               c("ADAE.USUBJID", "ADAE.TRT01A", "ADAE.AEDECOD", "ADAE.ASEV",
                 "ADAE.AEOUT"))
})

test_that("a figure gets its series anchor and declared aspects", {
  fill <- fill_of("F_14_3_1")
  expect_length(fill$cells, 0L)
  expect_true(is.numeric(fill$figure$series_anchor))
  keys <- vapply(fill$figure$directives, function(d) d$key, character(1))
  expect_true(all(c("x_axis", "y_axis", "series") %in% keys))
})

## ---------------------------------------------------------------------------
## The map must not change the standard's ARS
## ---------------------------------------------------------------------------

test_that("the cell map does not affect conformance", {
  out <- withr::local_tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(fill_ars(), auto_unbox = TRUE, pretty = TRUE),
             out)
  findings <- ars_conformance(out)
  expect_equal(nrow(findings[findings$severity == "FAIL", , drop = FALSE]), 0L)
})

## ---------------------------------------------------------------------------
## The statistic-line map and the renderer must not drift apart
## ---------------------------------------------------------------------------

test_that("statistic lines round-trip through the renderer's own mapping", {
  ## .stats_for_line() is the inverse of .statline_for(). If one gains a
  ## statistic and the other does not, a shell line stops being fillable
  ## with no error anywhere -- so they are held together here.
  for (line in c("Mean (SD)", "Median")) {
    stats <- .stats_for_line(line)
    expect_false(is.null(stats), info = line)
    for (sn in stats) {
      expect_equal(.statline_for(sn), line, info = paste(line, sn))
    }
  }
  ## The quartile and range lines the renderer writes with parentheses.
  expect_equal(.stats_for_line("Q1, Q3"), c("p25", "p75"))
  expect_equal(.statline_for("p25"), "(Q1, Q3)")
  expect_equal(.stats_for_line("Min, Max"), c("min", "max"))
  expect_equal(.statline_for("min"), "(Min, Max)")
})

test_that("a label that is not a statistic line is treated as a level", {
  expect_null(.stats_for_line("Female"))
  expect_null(.stats_for_line("American Indian or Alaska Native"))
  expect_null(.stats_for_line(""))
})
