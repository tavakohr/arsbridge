## Capability gate: detect tables arsbridge cannot generate.

test_that("keyword scan flags inferential indicators", {
  mk <- function(title, foot = character()) list(title = title, footnotes = foot,
                                                  stub_rows = list())
  expect_false(.capability_keyword_scan(mk("Cochran-Mantel-Haenszel p-value"))$supported)
  expect_false(.capability_keyword_scan(mk("Response rate (95% CI)",
                 "CIs computed using the Clopper-Pearson exact method."))$supported)
  expect_false(.capability_keyword_scan(mk("Difference, Newcombe method"))$supported)
  expect_false(.capability_keyword_scan(mk("Hazard ratio (Cox model)"))$supported)
  expect_false(.capability_keyword_scan(mk("Non-Responder Imputation (NRI)"))$supported)
})

test_that("keyword scan passes plain descriptive tables", {
  mk <- function(title) list(title = title, footnotes = character(), stub_rows = list())
  expect_true(.capability_keyword_scan(mk("Summary of Demographics"))$supported)
  expect_true(.capability_keyword_scan(mk("Subject Disposition"))$supported)
  expect_true(.capability_keyword_scan(mk("Adverse Events by System Organ Class"))$supported)
})

test_that("assess_capability is the union of LLM and keyword layers", {
  ## LLM says unsupported, keywords silent -> unsupported.
  sec1 <- list(title = "Custom model table", footnotes = character(),
               stub_rows = list(), is_supported = FALSE,
               unsupported_reason = "mixed model for repeated measures")
  v1 <- assess_capability(sec1)
  expect_false(v1$supported)
  expect_match(v1$reason, "repeated measures")

  ## LLM says supported, keywords catch it -> still unsupported (conservative).
  sec2 <- list(title = "Response rate, Cochran-Mantel-Haenszel p-value",
               footnotes = character(), stub_rows = list(), is_supported = TRUE)
  expect_false(assess_capability(sec2)$supported)

  ## Both clear -> supported.
  sec3 <- list(title = "Summary of Age", footnotes = character(),
               stub_rows = list(), is_supported = TRUE)
  expect_true(assess_capability(sec3)$supported)

  ## Missing LLM field defaults to supported; keywords decide.
  sec4 <- list(title = "Summary of Age", footnotes = character(), stub_rows = list())
  expect_true(assess_capability(sec4)$supported)
})

test_that(".tlf_heading reconstructs shell numbering", {
  expect_equal(.tlf_heading("T_14_2_1", "table"),   "Table 14.2.1")
  expect_equal(.tlf_heading("L_16_2_4_1", "listing"), "Listing 16.2.4.1")
  expect_equal(.tlf_heading("F_14_2_1", "figure"),  "Figure 14.2.1")
})

test_that("build_ars_json emits a numbered placeholder output (no analyses) for unsupported", {
  sec <- list(
    tlf_number = "T-14-2-1", tlf_type = "TABLE",
    title = "Proportion Achieving EASI 75", population_text = "ITT",
    population_annot = "", col_headers = character(),
    stub_rows = list(list(label = "Response", annotation = "ADEFF.AVAL",
                          has_annot = TRUE)),
    unsupported = TRUE, unsupported_reason = "requires Cochran-Mantel-Haenszel test"
  )
  re <- build_ars_json(list(sec), study_id = "S1", spec_lookup = list(ADEFF.AVAL = list()))
  ids <- vapply(re$outputs, function(o) o$id %||% "", character(1))
  expect_true(any(grepl("14_2_1", ids)))
  ## No analyses were built for it.
  expect_equal(length(re$analyses), 0L)
  ## Recorded in _meta for the renderer.
  us <- re$`_meta`$unsupported_outputs
  expect_true(length(us) >= 1)
  expect_match(us[[1]]$reason, "Cochran-Mantel-Haenszel")
})
