## parse_sap_docx(): optional SAP .docx -> heading-delimited, TLF-tagged
## sections, matched per TLF to feed the emitted block comments.

.mk_sap <- function(path) {
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, "Table 14.1.1 Subject Disposition",
                               style = "heading 1")
  doc <- officer::body_add_par(
    doc, "Disposition is summarised for the randomized population.",
    style = "Normal")
  doc <- officer::body_add_par(doc, "Table 14.2.1 Demographics",
                               style = "heading 1")
  doc <- officer::body_add_par(doc, "Age and sex summarised by treatment arm.",
                               style = "Normal")
  print(doc, target = path)
  path
}

test_that(".norm_tlf extracts the numeric TLF path key", {
  expect_equal(.norm_tlf("T-14-1-1"), "14_1_1")
  expect_equal(.norm_tlf("Table 14.2.1 Demographics"), "14_2_1")
  expect_true(is.na(.norm_tlf("No number here")))
  expect_true(is.na(.norm_tlf(NULL)))
})

test_that("parse_sap_docx splits by heading and tags TLF numbers", {
  skip_if_not_installed("officer")
  df <- parse_sap_docx(.mk_sap(tempfile(fileext = ".docx")))
  expect_true(nrow(df) >= 2)
  expect_true(all(c("14_1_1", "14_2_1") %in% df$tlf_number))
})

test_that("match_sap_section matches by TLF number, NA otherwise", {
  skip_if_not_installed("officer")
  df <- parse_sap_docx(.mk_sap(tempfile(fileext = ".docx")))
  expect_match(match_sap_section(df, "T-14-1-1"), "randomized population")
  expect_match(match_sap_section(df, "14_2_1"), "treatment arm")
  expect_true(is.na(match_sap_section(df, "T-99-9-9")))
})

test_that("parse_sap_docx is graceful on absent input", {
  expect_null(parse_sap_docx(NULL))
  expect_null(parse_sap_docx(""))
  expect_null(parse_sap_docx(tempfile(fileext = ".docx")))  # nonexistent
})

test_that(".clip_sap collapses to a single short comment line", {
  expect_equal(.clip_sap("one\n\ntwo"), "one two")
  expect_equal(.clip_sap(NA_character_), "")
  expect_true(nchar(.clip_sap(strrep("x ", 300))) <= 240)
})
