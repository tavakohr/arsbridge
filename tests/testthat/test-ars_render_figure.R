# Tests for ars_render_figure(): builds a ggplot for a figure output from ADaM
# data. The ARS spec only supplies the title/footnotes; the data mapping is
# argument-driven with ADaM defaults. Hand-built spec + ADEFF, no LLM/network.

skip_if_not_installed("ggplot2")

# A minimal figure ARS spec + an ADEFF dataset written to a temp dir.
.fig_fixture <- function(title = "Mean EASI 75 over time",
                         envir = parent.frame()) {
  adam_dir <- withr::local_tempdir(.local_envir = envir)
  utils::write.csv(data.frame(
    USUBJID = sprintf("S%02d", 1:12),
    TRT01A  = rep(c("Drug A", "Placebo"), 6),
    AVISITN = rep(c(0, 4, 8), each = 4),
    AVISIT  = rep(c("Baseline", "Week 4", "Week 8"), each = 4),
    PARAMCD = "EASI75",
    AVAL    = c(0, 1, 0, 0,  1, 1, 0, 1,  1, 1, 1, 0),
    CNSR    = rep(c(0, 1), 6),
    stringsAsFactors = FALSE
  ), file.path(adam_dir, "ADEFF.csv"), row.names = FALSE)

  spec <- list(outputs = list(list(
    id = "F_14_2_1", name = "F-14.2.1", outputType = "FIGURE",
    displays = list(list(order = 1, displayTitle = title,
      displaySections = list(list(sectionType = "Footnote",
        subSections = list(list(text = "Synthetic figure note."))))))
  )))
  ars_path <- tempfile("ars_", fileext = ".json")
  writeLines(jsonlite::toJSON(spec, auto_unbox = TRUE, null = "null"), ars_path)
  list(ars_path = ars_path, adam_dir = adam_dir)
}

test_that("mean_over_time figure renders a ggplot", {
  fx <- .fig_fixture("Mean change in EASI over time")
  p <- ars_render_figure(fx$ars_path, fx$adam_dir, "F_14_2_1",
                         dataset = "ADEFF")
  expect_s3_class(p, "ggplot")
  expect_true(length(p$layers) >= 1)
})

test_that("responder_over_time figure renders a ggplot (auto + explicit)", {
  fx <- .fig_fixture("Proportion of responders over time")
  p_auto <- ars_render_figure(fx$ars_path, fx$adam_dir, "F_14_2_1")
  expect_s3_class(p_auto, "ggplot")
  p_exp <- ars_render_figure(fx$ars_path, fx$adam_dir, "F_14_2_1",
                             type = "responder_over_time")
  expect_s3_class(p_exp, "ggplot")
})

test_that("kaplan-meier figure renders when survival is available", {
  skip_if_not_installed("survival")
  fx <- .fig_fixture("Time to first response (Kaplan-Meier)")
  p <- ars_render_figure(fx$ars_path, fx$adam_dir, "F_14_2_1", type = "km",
                         time_event = list(time = "AVAL", event = "CNSR"))
  expect_s3_class(p, "ggplot")
})

test_that("forest plot is refused with guidance", {
  fx <- .fig_fixture("Forest plot of subgroup effects")
  expect_error(
    ars_render_figure(fx$ars_path, fx$adam_dir, "F_14_2_1", type = "forest"),
    regexp = "[Ff]orest")
})

test_that("a missing dataset is a clear error", {
  fx <- .fig_fixture()
  expect_error(
    ars_render_figure(fx$ars_path, fx$adam_dir, "F_14_2_1", dataset = "NOPE"),
    regexp = "not found")
})
