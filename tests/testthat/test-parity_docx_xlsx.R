## The lockstep net for dual-format support.
##
## Two readers are only worth having if they agree. This compares the SAME
## three outputs authored as Word and as Excel (fixtures built by
## fixtures/build_fixtures_parity.R) and requires the class-1 section fields
## -- the ones adr/0004-xlsx-shell-input.md defines as identical-semantics --
## to match exactly.
##
## This is the test that fails when someone improves one reader and forgets
## the other. When it does fail, the answer is almost never to relax it: it is
## either a bug in the reader that changed, or a difference that belongs in
## the class-2 whitelist below WITH a written reason.

parity_pair <- function() {
  list(
    docx = suppressMessages(suppressWarnings(parse_shell(
      test_path("fixtures", "shells_parity_apx.docx")))),
    xlsx = suppressMessages(suppressWarnings(parse_shell(
      test_path("fixtures", "shells_parity_apx.xlsx"))))
  )
}

## Class 1 -- identical semantics. A difference here is a bug until justified.
CLASS1_FIELDS <- c(
  "tlf_number", "tlf_type", "title",
  "population_text", "population_annot",
  "footnotes", "source_datasets",
  "col_headers", "n_data_cols",
  "column_annotation", "column_groups", "column_tree", "include_total_hint"
)

annotated_rows <- function(sec) {
  lapply(Filter(function(r) isTRUE(r$has_annot), sec$stub_rows), function(r) {
    list(label = r$label, annotation = r$annotation)
  })
}

test_that("both formats find the same outputs, in the same order", {
  p <- parity_pair()
  expect_length(p$docx, 3L)
  expect_equal(vapply(p$docx, function(s) s$tlf_number, character(1)),
               vapply(p$xlsx, function(s) s$tlf_number, character(1)))
  expect_equal(vapply(p$docx, function(s) s$tlf_type, character(1)),
               vapply(p$xlsx, function(s) s$tlf_type, character(1)))
})

test_that("every class-1 field matches, output by output", {
  p <- parity_pair()
  for (i in seq_along(p$docx)) {
    d <- p$docx[[i]]
    x <- p$xlsx[[i]]
    for (field in CLASS1_FIELDS) {
      expect_identical(
        d[[field]], x[[field]],
        info = sprintf("%s: field %s differs between the Word and Excel shells",
                       d$tlf_number, field))
    }
  }
})

test_that("every annotated row matches, label and annotation", {
  ## The analyses themselves. A drift here means one reader would build a
  ## different ARS from the same study.
  p <- parity_pair()
  for (i in seq_along(p$docx)) {
    expect_identical(
      annotated_rows(p$docx[[i]]), annotated_rows(p$xlsx[[i]]),
      info = paste(p$docx[[i]]$tlf_number, "annotated rows"))
  }
})

test_that("table rows match one for one, annotated or not", {
  ## An unannotated level row ("Female", "Male") still positions the analysis
  ## above it, so tables must agree on the whole row sequence -- not just the
  ## annotated part.
  p <- parity_pair()
  for (i in seq_along(p$docx)) {
    if (!identical(p$docx[[i]]$tlf_type, "TABLE")) next
    labels <- function(s) vapply(s$stub_rows, function(r) r$label, character(1))
    expect_identical(labels(p$docx[[i]]), labels(p$xlsx[[i]]),
                     info = paste(p$docx[[i]]$tlf_number, "row labels"))
  }
})

test_that("listing column headers resolve identically", {
  p <- parity_pair()
  i <- which(vapply(p$docx, function(s) identical(s$tlf_type, "LISTING"),
                    logical(1)))
  expect_length(i, 1L)
  hdr <- function(s) lapply(s$header_rows, function(h) {
    list(label = h$label, annotation = h$annotation)
  })
  expect_identical(hdr(p$docx[[i]]), hdr(p$xlsx[[i]]))
})

## ---------------------------------------------------------------------------
## Class 2 -- the differences that are allowed, pinned so they stay known
## ---------------------------------------------------------------------------

test_that("detection method is namespaced by format, on the same rows", {
  ## Allowed to differ (a reviewer needs to know which convention supplied the
  ## evidence) -- but the SET OF ROWS that carry an annotation may not.
  p <- parity_pair()
  for (i in seq_along(p$docx)) {
    flags <- function(s) vapply(s$stub_rows, function(r) isTRUE(r$has_annot),
                                logical(1))
    if (identical(p$docx[[i]]$tlf_type, "TABLE")) {
      expect_identical(flags(p$docx[[i]]), flags(p$xlsx[[i]]),
                       info = p$docx[[i]]$tlf_number)
    }
    if (!identical(p$docx[[i]]$tlf_type, "TABLE")) next
    methods <- vapply(p$xlsx[[i]]$stub_rows, function(r) {
      r$detection_method %||% NA_character_
    }, character(1))
    methods <- methods[!is.na(methods)]
    if (length(methods) > 0) {
      expect_true(all(startsWith(methods, "xlsx_")),
                  info = p$docx[[i]]$tlf_number)
    }
  }
})

test_that("a listing header reports the SAME method in both formats", {
  ## Unlike a table stub row, a listing header goes through the shared
  ## .detect_listing_header_annotation() in .finalize_section() -- one
  ## detector, so one method name. Not namespacing it is deliberate: it means
  ## the method is a class-1 field here, and this pins that.
  p <- parity_pair()
  i <- which(vapply(p$docx, function(s) identical(s$tlf_type, "LISTING"),
                    logical(1)))
  method <- function(s) vapply(s$header_rows, function(h) {
    h$detection_method %||% NA_character_
  }, character(1))
  expect_identical(method(p$docx[[i]]), method(p$xlsx[[i]]))
  expect_equal(unique(method(p$xlsx[[i]])), "listing_header_colour")
})

test_that("Excel never reports a flagged header row, Word may", {
  p <- parity_pair()
  for (sec in p$xlsx) expect_equal(sec$header_rows_flagged, 0L)
})

test_that("a listing's placeholder row is a row in Word and a template in Excel", {
  ## A KNOWN, PINNED difference rather than an ignored one.
  ##
  ## A Word listing's body row of placeholders ("xxx-xxx | xxxx | ...") is
  ## enumerated by .collect_stub_rows() as an unannotated stub row; the Excel
  ## reader records it as `template_row` instead and creates no row. The Excel
  ## behaviour is the better one -- a placeholder is not an analysis -- but
  ## changing the Word reader would alter the ARS of every existing listing,
  ## so it is pinned here instead of quietly tolerated.
  ##
  ## If this test starts failing because the counts now MATCH, that is good
  ## news: delete it.
  p <- parity_pair()
  i <- which(vapply(p$docx, function(s) identical(s$tlf_type, "LISTING"),
                    logical(1)))
  d <- p$docx[[i]]
  x <- p$xlsx[[i]]

  expect_equal(length(d$stub_rows), length(x$stub_rows) + 1L)
  extra <- Filter(function(r) !isTRUE(r$has_annot), d$stub_rows)
  expect_length(extra, 1L)
  expect_match(extra[[1]]$label, "^x+-x+$")   ## the placeholder row

  expect_null(d$template_row)
  expect_equal(x$template_row, 5L)
})

## ---------------------------------------------------------------------------
## The same shell, the same ARS
## ---------------------------------------------------------------------------

test_that("both formats build the same ARS reporting event", {
  ## The end the parity actually serves: identical analyses, methods and
  ## groupings, from the same study said two ways.
  skip_if_not_installed("withr")
  spec <- test_path("fixtures", "adam_spec_apx_drm_301.xlsx")

  build <- function(shell) {
    out <- withr::local_tempfile(fileext = ".json")
    withr::with_envvar(
      c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
        GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
      suppressMessages(suppressWarnings(spec_to_ars(
        shell_path = test_path("fixtures", shell),
        adam_spec_path = spec, api_key = "", output_path = out,
        report_path = withr::local_tempfile(fileext = ".xlsx"),
        verbose = FALSE))))
    jsonlite::fromJSON(out, simplifyVector = FALSE)
  }

  d <- build("shells_parity_apx.docx")
  x <- build("shells_parity_apx.xlsx")

  ## Everything except the provenance block, which records the input path and
  ## the time of the run.
  strip <- function(re) {
    re$`_meta` <- NULL
    re
  }
  expect_identical(strip(d), strip(x))
})
