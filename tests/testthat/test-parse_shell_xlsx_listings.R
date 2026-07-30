## parse_shell_xlsx() on listing sheets.
##
## A listing annotates its COLUMN HEADERS rather than a stub column, and its
## body is a single template row standing for every data row. Detection is
## deferred to .finalize_section(), exactly as in the Word reader, because the
## source dataset that resolves an unqualified variable is not known until the
## whole sheet has been seen.

listings <- function() {
  secs <- suppressMessages(suppressWarnings(
    parse_shell_xlsx(test_path("fixtures", "shells_apx_drm_301.xlsx"))))
  Filter(function(s) identical(s$tlf_type, "LISTING"), secs)
}

listing_of <- function(number) {
  hit <- Filter(function(s) identical(s$tlf_number, number), listings())
  expect_length(hit, 1L)
  hit[[1]]
}

test_that("listing sheets become LISTING sections", {
  secs <- listings()
  expect_length(secs, 2L)
  expect_equal(vapply(secs, function(s) s$tlf_number, character(1)),
               c("L-16-2-1", "L-16-2-2"))
})

test_that("each column header resolves to its variable", {
  sec <- listing_of("L-16-2-1")
  expect_equal(vapply(sec$header_rows, function(h) h$label, character(1)),
               c("Subject", "Treatment", "AE term", "Severity", "Outcome"))
  expect_equal(vapply(sec$header_rows, function(h) h$annotation, character(1)),
               c("ADAE.USUBJID", "ADAE.TRT01A", "ADAE.AEDECOD", "ADAE.ASEV",
                 "ADAE.AEOUT"))
  expect_true(all(vapply(sec$header_rows, function(h) h$has_annot,
                         logical(1))))
})

test_that("the display labels are the annotation-stripped header text", {
  ## Shipping "Subject[ADAE.USUBJID]" as a column header is the defect this
  ## guards: col_headers flows straight into the rendered listing.
  sec <- listing_of("L-16-2-1")
  expect_equal(sec$col_headers,
               c("Subject", "Treatment", "AE term", "Severity", "Outcome"))
  expect_false(any(grepl("[][]", sec$col_headers)))
})

test_that("a row filter in the header is an instruction, not a display column", {
  ## The stub header is "[ADAE.USUBJID]  [row: ADAE.TRTEMFL='Y'; source ADAE]".
  ## Reading both bracket groups as column variables would ship the row filter
  ## as a second variable on the Subject column.
  sec <- listing_of("L-16-2-1")
  expect_equal(sec$header_rows[[1]]$annotation, "ADAE.USUBJID")
  expect_false(grepl("TRTEMFL", sec$header_rows[[1]]$annotation))

  ## It is lifted, not dropped -- the reviewer still sees the filter.
  expect_true(any(grepl("row: ADAE.TRTEMFL='Y'", sec$programmer_annotations,
                        fixed = TRUE)))
})

test_that("the source dataset is read from the header directive", {
  ## It has nowhere else to live in a worksheet: there is no below-table
  ## "Source: ..." paragraph.
  expect_equal(listing_of("L-16-2-1")$source_datasets, "ADAE")
})

test_that("a listing with no source directive falls back and says so", {
  diag_reset()
  secs <- suppressMessages(suppressWarnings(
    parse_shell_xlsx(test_path("fixtures", "shells_apx_drm_301.xlsx"))))
  sec <- Filter(function(s) identical(s$tlf_number, "L-16-2-2"), secs)[[1]]

  ## Its headers are fully qualified, so they still resolve correctly ...
  expect_equal(vapply(sec$header_rows, function(h) h$annotation, character(1)),
               c("ADCM.USUBJID", "ADCM.CMTRT", "ADCM.ASTDT"))
  ## ... but the missing source is reported, because an unqualified header
  ## would have silently defaulted to ADSL.
  expect_true(any(grepl("Listing has no source dataset", diag_records()$problem)))
})

test_that("annotated headers are appended to stub_rows for downstream stages", {
  ## Same contract as the Word reader: validate/enrich/build see every
  ## annotated cell as a row, with no listing special case.
  sec <- listing_of("L-16-2-1")
  expect_length(sec$stub_rows, 5L)
  expect_equal(vapply(sec$stub_rows, function(r) r$annotation, character(1)),
               vapply(sec$header_rows, function(h) h$annotation, character(1)))
})

test_that("the template row is recorded and is not a stub row", {
  ## One row of placeholders stands for every data row; the fill writer
  ## expands it. It is not an analysis.
  sec <- listing_of("L-16-2-1")
  expect_equal(sec$template_row, 5L)
  labels <- vapply(sec$stub_rows, function(r) r$label, character(1))
  expect_false(any(grepl("xxx", labels)))
})

test_that("a listing carries its population like any other output", {
  sec <- listing_of("L-16-2-1")
  expect_match(sec$population_text, "^Safety Population")
  expect_match(sec$population_annot, "ADSL\\.SAFFL='Y'")
})

test_that("a two-variable header keeps the qualified variable", {
  ## "Start/Stop [ADCM.ASTDT / AENDT]" is one column displaying two dates, and
  ## only the first carries its dataset prefix.
  ##
  ## KNOWN GAP, not xlsx-specific: once
  ## .detect_listing_header_annotation() finds a fully qualified reference it
  ## returns immediately, so an UNQUALIFIED companion variable in the same
  ## cell ("AENDT") is dropped. That is shared-core behaviour -- the Word
  ## reader only keeps both on the CDSC shell because those headers happen to
  ## be annotated by below-table arrow lines, which bind their right-hand side
  ## verbatim, rather than in-cell.
  ##
  ## Left as-is deliberately: changing it alters the annotation of every
  ## listing column with a companion variable in BOTH formats, which needs its
  ## own change and its own golden review, not a quiet fix inside a
  ## structural PR. The end date is recoverable from the shell in the
  ## meantime -- the raw header text is kept.
  sec <- listing_of("L-16-2-2")
  expect_equal(sec$header_rows[[3]]$label, "Start/Stop")
  expect_equal(sec$header_rows[[3]]$annotation, "ADCM.ASTDT")
  expect_match(sec$header_rows[[3]]$raw_text, "AENDT")
})
