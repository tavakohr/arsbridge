## Unit tests for the listing column-header annotation detector
## (.detect_listing_header_annotation). Exercises the conventions used by
## the APX-DRM-301 real shell:
##   "Subject ID\nUSUBJID"      -> ADSL.USUBJID
##   "Arm\nACTARMCD"            -> ADSL.ACTARMCD (universal ADSL var)
##   "AE PT (Verbatim)\nAEDECOD (AETERM)" -> ADAE.AEDECOD (ADAE.AETERM)
##   "Severity\nAESEV"          -> ADAE.AESEV (falls through to source_ds)
##   "Subject ID"               -> no annotation (no line-2 variable)

.no_format <- function(text) {
  list(list(text = text, color_hex = NA_character_,
            bold = FALSE, italic = FALSE, underline = FALSE))
}

test_that("Universal ADSL var resolved to ADSL even with ADAE source", {
  res <- arsbridge:::.detect_listing_header_annotation(
    "Subject ID\nUSUBJID", .no_format("Subject ID\nUSUBJID"), "ADAE"
  )
  expect_equal(res$annotation, "ADSL.USUBJID")
  expect_equal(res$label,      "Subject ID")
  expect_equal(res$method,     "listing_header_pattern")
})

test_that("Non-universal var picks up the source dataset", {
  res <- arsbridge:::.detect_listing_header_annotation(
    "Severity\nAESEV", .no_format("Severity\nAESEV"), "ADAE"
  )
  expect_equal(res$annotation, "ADAE.AESEV")
  expect_equal(res$label,      "Severity")
})

test_that("Multi-variable header (AEDECOD (AETERM)) keeps both refs", {
  res <- arsbridge:::.detect_listing_header_annotation(
    "AE PT (Verbatim)\nAEDECOD (AETERM)",
    .no_format("AE PT (Verbatim)\nAEDECOD (AETERM)"),
    "ADAE"
  )
  expect_match(res$annotation, "ADAE\\.AEDECOD")
  expect_match(res$annotation, "ADAE\\.AETERM")
  expect_equal(res$label, "AE PT (Verbatim)")
})

test_that("Cells with only a display label produce no annotation", {
  res <- arsbridge:::.detect_listing_header_annotation(
    "Preferred Term", .no_format("Preferred Term"), "ADAE"
  )
  expect_equal(res$annotation, "")
  expect_equal(res$label, "Preferred Term")
})

test_that("Empty / NULL input returns empty annotation", {
  res <- arsbridge:::.detect_listing_header_annotation(
    "", list(), "ADAE"
  )
  expect_equal(res$annotation, "")
})

test_that("Common English-style tokens are blocklisted, not flagged", {
  res <- arsbridge:::.detect_listing_header_annotation(
    "Subject\nID", .no_format("Subject\nID"), "ADAE"
  )
  expect_equal(res$annotation, "")
})

test_that("Treatment arm header (ARM/ACTARMCD) resolves to ADSL", {
  res <- arsbridge:::.detect_listing_header_annotation(
    "Arm\nACTARMCD", .no_format("Arm\nACTARMCD"), "ADAE"
  )
  expect_equal(res$annotation, "ADSL.ACTARMCD")
})

test_that("Coloured run is picked up at Layer 1 (HIGH confidence)", {
  ## Simulate "Subject ID" in black + "USUBJID" in red C00000
  runs <- list(
    list(text = "Subject ID\n", color_hex = NA_character_,
         bold = FALSE, italic = FALSE, underline = FALSE),
    list(text = "USUBJID", color_hex = "C00000",
         bold = FALSE, italic = FALSE, underline = FALSE)
  )
  res <- arsbridge:::.detect_listing_header_annotation(
    "Subject ID\nUSUBJID", runs, "ADAE"
  )
  expect_equal(res$annotation, "ADSL.USUBJID")
  expect_equal(res$method,     "listing_header_colour")
  expect_equal(res$confidence, "high")
})
