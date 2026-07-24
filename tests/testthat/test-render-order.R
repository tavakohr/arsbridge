## Column ordering / stub placement for rendered tables.

test_that("build_col_levels orders arms by the shell column-header order", {
  out_obj <- list(displays = list(list(order = 1L, display = list(
    id = "D1", name = "D1",
    columns = list(
      list(label = "UPADALIMIB 15 mg\n(N=200) n (%)"),
      list(label = "UPADALIMIB 30 mg\n(N=200) n (%)"),
      list(label = "Placebo (N=200) n (%)")
    )
  ))))
  ## ARD presents the arms alphabetically (Placebo first).
  ard_out <- data.frame(
    TRT01A = c("Placebo", "UPADALIMIB 15 mg", "UPADALIMIB 30 mg"),
    stringsAsFactors = FALSE
  )
  lv <- build_col_levels(out_obj, ard_out, "TRT01A")
  expect_equal(lv, c("UPADALIMIB 15 mg", "UPADALIMIB 30 mg", "Placebo"))
})

test_that("build_col_levels falls back to ARD order when no columns are defined", {
  out_obj <- list(displays = list(list(order = 1L, display = list(
    id = "D1", name = "D1", columns = list()
  ))))
  ard_out <- data.frame(TRT01A = c("B", "A"), stringsAsFactors = FALSE)
  expect_equal(suppressWarnings(build_col_levels(out_obj, ard_out, "TRT01A")),
               c("B", "A"))
})

test_that(".build_output emits display columns from the shell col_headers", {
  section <- list(
    tlf_number = "T_14_1_1", title = "Subject Disposition",
    tlf_type = "TABLE",
    col_headers = c("Disposition", "UPADALIMIB 15 mg", "UPADALIMIB 30 mg",
                    "Placebo"),
    footnotes = list()
  )
  out <- .build_output(section, c("AN_1"))
  ## Columns live inside the OrderedDisplay wrapper's display object.
  cols <- out$displays[[1]]$display$columns
  expect_equal(length(cols), 4L)
  expect_equal(vapply(cols, function(c) c$label, character(1)),
               c("Disposition", "UPADALIMIB 15 mg", "UPADALIMIB 30 mg", "Placebo"))
})

test_that(".gt_to_flextable keeps the row-label column on the left", {
  ## Fake a tfrmt/gt result where the label column was appended on the RIGHT.
  fake_gt <- structure(
    list(`_data` = data.frame(
      Placebo = "x", `UPADALIMIB 15 mg` = "y", rowlbl = "Randomized",
      check.names = FALSE, stringsAsFactors = FALSE)),
    arsbridge_label_var  = "rowlbl",
    arsbridge_group_vars = character()
  )
  ft <- .gt_to_flextable(fake_gt, "T_14_1_1", "Title", character())
  expect_equal(names(ft$body$dataset)[1], "rowlbl")
})
