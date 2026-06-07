test_that("IDs slugify to upper-case underscore-separated tokens", {
  expect_equal(make_analysis_set_id("Safety Population"), "AS_SAFETY_POPULATION")
  expect_equal(make_grouping_id("TRT01A"), "GF_TRT01A")
  expect_equal(make_method_id("Summary Statistics - Continuous"),
               "MTH_SUMMARY_STATISTICS_CONTINUOUS")
})

test_that("Analysis IDs zero-pad the index", {
  expect_equal(make_analysis_id("T-14-1-1", 1L),  "AN_T_14_1_1_001")
  expect_equal(make_analysis_id("T-14-1-1", 42L), "AN_T_14_1_1_042")
})

test_that("Output IDs replace dots and spaces with underscores", {
  expect_equal(make_output_id("T-14-1-1"), "T_14_1_1")
  expect_equal(make_output_id("Table 14.1.1"), "TABLE_14_1_1")
})

test_that("IDs are deterministic across runs (same input -> same id)", {
  expect_identical(make_analysis_set_id("Safety Population"),
                   make_analysis_set_id("Safety Population"))
})

test_that("Empty input falls back to UNSPECIFIED", {
  expect_equal(make_analysis_set_id(""), "AS_UNSPECIFIED")
})
