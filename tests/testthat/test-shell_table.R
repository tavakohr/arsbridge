## The pure shell-table builders behind the shell-faithful Details view:
## every shape is a plain list or data frame, tested with expect_equal like
## .tree_data(). The reactive wiring is covered in test-mod_shell_table.R.

.shell_model <- function() {
  ars_to_model(test_path("fixtures", "ars_apx_drm_301_deterministic.json"))
}

.shell_raw <- function(model, output_id) {
  model$outputs$raw[[match(output_id, model$outputs$id)]]
}


## --- .shell_table_data: the authored layout, every row ----------------------

test_that("every authored row appears, in order, with its indentation", {
  model <- .shell_model()
  data <- .shell_table_data(.shell_raw(model, "T_14_1_1"), model)

  expect_true(data$has_layout)
  expect_equal(nrow(data$rows), 14L)
  expect_equal(data$rows$order, 1:14)

  ## The five disposition counts, the categorical parent, and its eight
  ## authored reason rows -- the rows the old details panel could not show.
  expect_equal(data$rows$kind[1:5], rep("scalar_row", 5))
  expect_equal(data$rows$stat_form[1:5], rep("filtered_count", 5))
  expect_equal(data$rows$kind[6], "categorical_block")
  expect_equal(data$rows$kind[7:14], rep("label_row", 8))
  expect_equal(data$rows$label[7], "Adverse event")
  expect_equal(data$rows$indent[6], 0L)
  expect_equal(data$rows$indent[7:14], rep(4L, 8))
})

test_that("owner fill-down mirrors the renderer's block-consumption rule", {
  model <- .shell_model()
  data <- .shell_table_data(.shell_raw(model, "T_14_1_1"), model)

  ## An analysis-bearing row owns itself.
  expect_equal(data$rows$owner_analysis_id[1], "AN_T_14_1_1_001")
  expect_equal(data$rows$owner_kind[1], "scalar_row")

  ## Label rows trail their categorical parent.
  expect_equal(data$rows$owner_analysis_id[7:14],
               rep("AN_T_14_1_1_006", 8))
  expect_equal(data$rows$owner_kind[7:14], rep("categorical_block", 8))

  ## The method joins in through the owner.
  expect_equal(data$rows$method_id[7],
               model$analyses$methodId[
                 model$analyses$id == "AN_T_14_1_1_006"
               ])
})

test_that("a label row after a scalar count belongs to nothing", {
  model <- .shell_model()
  raw <- .shell_raw(model, "T_14_1_1")
  raw[["_meta"]][["shell_layout"]] <- list(
    list(order = 1L, label = "Randomized", indent = 0L,
         analysis_id = "AN_X_1", kind = "scalar_row", stat_form = "filtered_count"),
    list(order = 2L, label = "A section note", indent = 4L,
         analysis_id = NULL, kind = "label_row"),
    list(order = 3L, label = "Sex, n (%)", indent = 0L,
         analysis_id = "AN_X_2", kind = "categorical_block"),
    list(order = 4L, label = "Male", indent = 4L,
         analysis_id = NULL, kind = "label_row")
  )

  data <- .shell_table_data(raw, model)
  expect_equal(data$rows$owner_analysis_id,
               c("AN_X_1", NA, "AN_X_2", "AN_X_2"))
  ## The unowned note renders a blank body cell, like the shell.
  expect_equal(data$rows$placeholder[2], "")
})

test_that("placeholders read like the authored shell document", {
  model <- .shell_model()
  data <- .shell_table_data(.shell_raw(model, "T_14_1_2"), model)

  ## Age (years) block: parent header blank, stat lines keyed on their text.
  expect_equal(data$rows$label[1:5],
               c("Age (years)", "n", "Mean (SD)", "Median (Q1, Q3)",
                 "Min, Max"))
  expect_equal(data$rows$placeholder[1:5],
               c("", "xx", "xx.x (x.xx)", "xx.x (xx.x, xx.x)",
                 "(xx.x, xx.x)"))

  ## Category rows under a categorical parent count-and-percentage.
  expect_equal(data$rows$label[6], "Age Group, n (%)")
  expect_equal(data$rows$placeholder[6], "")
  expect_equal(data$rows$placeholder[7], "xx (xx.x%)")

  ## Disposition counts are plain subject counts.
  disposition <- .shell_table_data(.shell_raw(model, "T_14_1_1"), model)
  expect_equal(disposition$rows$placeholder[1], "xxx")
})

test_that(".shell_placeholder covers the cases no fixture row exercises", {
  ## Each case names the three questions separately -- shape, statistic form,
  ## status -- and every expected value below is the one this function
  ## returned before they were separated.
  expect_equal(.shell_placeholder("scalar_row", "Subjects",
                                  stat_form = "subject_count"), "xxx")
  expect_equal(.shell_placeholder("scalar_row", "Subjects",
                                  stat_form = "subject_count_pct"),
               "xx (xx.x%)")
  expect_equal(.shell_placeholder("level_row", "Female"), "xx (xx.x%)")
  expect_equal(.shell_placeholder(NA_character_, "Derived",
                                  status = .LAYOUT_STATUS_MANUAL),
               .MANUAL_MARKER)
  expect_equal(.shell_placeholder("label_row", "Pending",
                                  owner_status = .LAYOUT_STATUS_MANUAL),
               .MANUAL_MARKER)
  expect_equal(.shell_placeholder("label_row", "CV (%)",
                                  owner_shape = "stat_block"),
               "xx.x")
  ## A supplement-added row is a scalar row; its provenance never reaches the
  ## placeholder, so the METHOD is what shapes the cell.
  expect_equal(
    .shell_placeholder("scalar_row", "Extra",
                       method_id = "MTH_COUNT_AND_PERCENTAGE"),
    "xx (xx.x%)"
  )
  expect_equal(
    .shell_placeholder("scalar_row", "Line", method_id = "MTH_SUBJECT_COUNT"),
    "xxx"
  )
  expect_equal(
    .shell_placeholder("scalar_row", "Line",
                       method_id = "MTH_SUMMARY_STATISTICS_CONTINUOUS"),
    "xx.x"
  )
  ## A block header prints nothing in the body -- asked of the SHAPE, so a
  ## reserved row (no proved shape) does not accidentally qualify.
  expect_equal(.shell_placeholder("stat_block", "Age (years)"), "")
  expect_equal(.shell_placeholder("categorical_block", "Sex"), "")
})

test_that("the population line resolves the output's analysis sets", {
  model <- .shell_model()
  data <- .shell_table_data(.shell_raw(model, "T_14_1_2"), model)
  expect_equal(data$population, "Safety Population (ADSL.SAFFL='Y')")
})

test_that("title and footnotes come from the display node", {
  model <- .shell_model()
  data <- .shell_table_data(.shell_raw(model, "T_14_1_2"), model)
  expect_equal(
    data$title,
    "Subject Demographics and Baseline Atopic Dermatitis Characteristics"
  )
})


## --- fallback: outputs without an authored layout ----------------------------

test_that("an output without a layout synthesizes one row per reference", {
  model <- .shell_model()
  data <- .shell_table_data(.shell_raw(model, "T_14_2_1"), model)

  expect_false(data$has_layout)
  refs <- .split_values(
    model$outputs$referenced_analysis_ids[model$outputs$id == "T_14_2_1"]
  )
  expect_equal(nrow(data$rows), length(refs))
  expect_equal(data$rows$analysis_id, refs)
  expect_equal(data$rows$kind, rep("scalar_row", length(refs)))
  expect_equal(data$rows$owner_analysis_id, refs)

  ## Labels resolve through the analyses pool.
  first <- model$analyses$label[model$analyses$id == refs[1]]
  expect_equal(data$rows$label[1], first)
})

test_that("a figure with no references still builds an empty view", {
  model <- .shell_model()
  data <- .shell_table_data(.shell_raw(model, "F_14_2_1"), model)
  expect_false(data$has_layout)
  expect_equal(nrow(data$rows), 0L)
})


## --- headers -----------------------------------------------------------------

test_that("a flat display makes a single header row", {
  model <- .shell_model()
  data <- .shell_table_data(.shell_raw(model, "T_14_1_2"), model)

  expect_length(data$header, 1)
  labels <- vapply(data$header[[1]], `[[`, character(1), "label")
  expect_equal(labels[1], "Characteristic")
  expect_length(labels, 4)
  expect_equal(data$n_body_cols, 3L)
})

test_that("a column tree becomes spanning header rows with a stub cell", {
  output_raw <- list(
    id = "T_X",
    displays = list(list(order = 1, display = list(
      displayTitle = "Tree table",
      columns = list(list(label = "Characteristic"))
    ))),
    "_meta" = list(column_tree = list(mode = "spanned", nodes = list(
      list(id = "n1", label = "Placebo", level = 1, order = 1,
           nodeType = "leaf", nHint = "86"),
      list(id = "n2", label = "Xanomeline", level = 1, order = 2,
           nodeType = "group"),
      list(id = "n3", label = "Low", level = 2, order = 1,
           nodeType = "leaf", parentId = "n2"),
      list(id = "n4", label = "High", level = 2, order = 2,
           nodeType = "leaf", parentId = "n2")
    )))
  )

  header <- .shell_header_rows(output_raw)
  expect_length(header$rows, 2)
  expect_equal(header$n_body_cols, 3L)

  top <- header$rows[[1]]
  expect_equal(vapply(top, `[[`, character(1), "label"),
               c("Characteristic", "Placebo", "Xanomeline"))
  expect_equal(vapply(top, `[[`, integer(1), "colspan"), c(1L, 1L, 2L))
  ## The stub and the level-1 leaf reach down through both header rows; the
  ## spanning group sits on one.
  expect_equal(vapply(top, `[[`, integer(1), "rowspan"), c(2L, 2L, 1L))
  expect_equal(vapply(top, `[[`, character(1), "hint"),
               c(NA_character_, "86", NA_character_))

  bottom <- header$rows[[2]]
  expect_equal(vapply(bottom, `[[`, character(1), "label"), c("Low", "High"))
  expect_equal(vapply(bottom, `[[`, integer(1), "rowspan"), c(1L, 1L))
})


## --- selection helpers -------------------------------------------------------

test_that(".shell_active_row keeps owned rows only while selected", {
  model <- .shell_model()
  data <- .shell_table_data(.shell_raw(model, "T_14_1_2"), model)
  clicked <- list(output = "T_14_1_2", order = 3L)

  active <- .shell_active_row(
    clicked, list(pool = "analyses", id = "AN_T_14_1_2_001"), data, "T_14_1_2"
  )
  expect_equal(active$order, 3L)
  expect_equal(active$owner_analysis_id, "AN_T_14_1_2_001")

  ## The reviewer moved on: the click is stale.
  expect_null(.shell_active_row(
    clicked, list(pool = "analyses", id = "AN_T_14_1_2_007"), data, "T_14_1_2"
  ))
  expect_null(.shell_active_row(
    clicked, list(pool = "outputs", id = "T_14_1_2"), data, "T_14_1_2"
  ))
  ## A click on some other output's table never lights this one.
  expect_null(.shell_active_row(clicked, NULL, data, "T_14_1_1"))
  expect_null(.shell_active_row(NULL, NULL, data, "T_14_1_2"))
})

test_that(".shell_redirect_output resolves an owned click back to its output", {
  model <- .shell_model()
  clicked <- list(output = "T_14_1_2", order = 3L)

  expect_equal(
    .shell_redirect_output(clicked, "AN_T_14_1_2_001", model), "T_14_1_2"
  )
  expect_null(.shell_redirect_output(clicked, "AN_T_14_1_2_007", model))
  expect_null(.shell_redirect_output(NULL, "AN_T_14_1_2_001", model))
  expect_null(
    .shell_redirect_output(list(output = "NOPE", order = 1L),
                           "AN_T_14_1_2_001", model)
  )
})


## --- rendering ---------------------------------------------------------------

test_that(".shell_table_ui renders clickable, highlighted rows", {
  skip_if_not_installed("shiny")

  model <- .shell_model()
  data <- .shell_table_data(.shell_raw(model, "T_14_1_1"), model)
  ns <- shiny::NS("detail")

  html <- as.character(
    .shell_table_ui(data, ns, "T_14_1_1", "edit", selected_order = 7L)
  )
  expect_match(html, "shell-table")
  expect_match(html, "Subject Disposition", fixed = TRUE)
  expect_match(html, "table-active")
  ## Apostrophes arrive HTML-escaped in the serialized tags.
  expect_match(html, "detail-shell_row", fixed = TRUE)
  expect_match(html, "{output: &#39;T_14_1_1&#39;, order: 7}", fixed = TRUE)
  ## One clickable body row per authored row (the header rows carry their
  ## own shell_header input).
  expect_equal(lengths(regmatches(html, gregexpr("detail-shell_row", html))),
               14L)
  expect_match(html, "detail-shell_header", fixed = TRUE)
  ## Indented reason rows use the rescaled column-tree indent pattern.
  expect_match(html, "padding-left: 32px", fixed = TRUE)
})
