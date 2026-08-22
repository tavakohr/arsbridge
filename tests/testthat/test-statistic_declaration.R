## PR5b-3a. The declaration readers, proved on invented vocabulary only.
##
## Every caption here is made up. That is the point: the rules must key on the
## SHAPE of a declaration -- a symbol lead, a bracket naming what the number is
## measured against, a line the author set apart -- and not on the particular
## words two real shells happened to use. Where a rule could plausibly have been
## fitted to one vocabulary, the same case is repeated in a second, unrelated
## one and the two outcomes are compared directly.

## Two disjoint prose vocabularies. Nothing in either appears in any fixture.
VOCAB_A <- c(stub = "Zeta Grouping", prose = "Qualifier", unit = "KG")
VOCAB_B <- c(stub = "Wombat Tier", prose = "Marker", unit = "FURLONG")

status_of <- function(x) .caption_declares_statistic(x)$status
stats_of  <- function(x) .caption_declares_statistic(x)$stats

## ---------------------------------------------------------------------------
## Footnote markers
## ---------------------------------------------------------------------------

test_that("markers differing only in Unicode block are semantically equal", {
  ## One declaration, cited by every modifier letter across the whole span the
  ## repair covers -- including U+1D9C and U+1DA0, which the earlier ranges
  ## missed, and U+1D78, which sits alone among lowercase letters. Two labels
  ## that differ only in WHICH footnote they cite must classify identically.
  markers <- vapply(c(0x00B2, 0x02B0, 0x1D43, 0x1D47, 0x1D48, 0x1D49,
                      0x1D78, 0x1D9C, 0x1DA0, 0x1DBF, 0x2070),
                    intToUtf8, character(1))
  expect_gt(length(markers), 5L)

  for (m in markers) {
    ## cited from the end of the label
    expect_identical(.header_line_stats(paste0(VOCAB_A[["stub"]], "\nn (%)", m)),
                     c("count", "pct"),
                     info = sprintf("trailing marker U+%04X", utf8ToInt(m)))
    ## cited from inside the bracket
    expect_identical(status_of(paste0(VOCAB_A[["prose"]], ", % (95% CI", m, ")")),
                     "stated_and_read",
                     info = sprintf("bracketed marker U+%04X", utf8ToInt(m)))
  }
})

test_that("a lowercase phonetic letter is not a footnote marker", {
  ## The span the repair reaches into interleaves modifier letters with
  ## ordinary lowercase ones. Stripping the lowercase letters would edit the
  ## author's word rather than drop a citation.
  for (cp in c(0x1D6B, 0x1D77, 0x1D79, 0x1D9A)) {
    word <- paste0(VOCAB_A[["prose"]], intToUtf8(cp))
    expect_identical(.strip_footnote_markers(word), word,
                     info = sprintf("U+%04X is a letter, not a marker", cp))
  }
})

test_that("a marker is dropped only where a footnote can be cited from", {
  m <- intToUtf8(0x1D9C)
  expect_identical(.strip_footnote_markers(paste0("Alpha", m)), "Alpha")
  expect_identical(.strip_footnote_markers(paste0("(Alpha", m, ")"), within = TRUE),
                   "(Alpha)")
  ## Mid-word, with a letter following, it is part of the word.
  mid <- paste0("Al", m, "pha")
  expect_identical(.strip_footnote_markers(mid, within = TRUE), mid)
})

test_that("the inside-bracket strip is off for the production header reader", {
  ## `.header_suffix_stats()` decides methods today. Widening its marker
  ## handling would move rows, which is not this checkpoint's business, so the
  ## two readers are deliberately different here and the difference is pinned.
  lab <- paste0(VOCAB_A[["prose"]], ", % (95% CI", intToUtf8(0x1D47), ")")
  expect_null(.header_suffix_stats(lab))
  expect_identical(status_of(lab), "stated_and_read")
})

## ---------------------------------------------------------------------------
## The four outcomes
## ---------------------------------------------------------------------------

test_that("a form the grammar reads is stated_and_read", {
  expect_identical(status_of(paste0(VOCAB_A[["stub"]], "\nn (%)")),
                   "stated_and_read")
  expect_identical(stats_of(paste0(VOCAB_A[["stub"]], "\nn (%)")),
                   c("count", "pct"))
})

test_that("a symbol-led ratio the grammar cannot read is stated_not_read", {
  ## An explicit request for something no supported statistic produces. It must
  ## not read as silence: silence and refusal call for opposite corrections.
  for (v in list(VOCAB_A, VOCAB_B)) {
    frag <- sprintf("Q (Q/50 %s)", v[["unit"]])
    expect_identical(status_of(paste0(v[["stub"]], "\n", frag)),
                     "stated_not_read",
                     info = frag)
    expect_null(.header_line_stats(paste0(v[["stub"]], "\n", frag)), info = frag)
  }
})

test_that("a readable form outside the admitted set is stated_not_admitted", {
  ## The line boundary is new, so it admits one form. A form it reads but does
  ## not admit is neither a refusal nor silence, and is recorded as itself so
  ## the case for widening can be made from evidence.
  x <- paste0(VOCAB_A[["stub"]], "\n% (95% CI)")
  expect_identical(status_of(x), "stated_not_admitted")
  expect_null(stats_of(x))
  expect_null(.header_line_stats(x))
})

test_that("a caption stating no form at all is not_stated", {
  ## Generic analogues of the shapes that would otherwise be mistaken for
  ## declarations: a prose lead with a bracketed qualifier.
  controls <- c("Cohort (Primary)", "Coded Term (Dictionary 9.9)",
                "Attribute (screening)", "Analysis (Complete)",
                "Timepoint (Cycle 4)", "Subject Grouping (all enrolled)",
                "Measure (unit)", "Term (as recorded)")
  expect_identical(length(controls), 8L)
  for (x in controls) expect_identical(status_of(x), "not_stated", info = x)
})

## ---------------------------------------------------------------------------
## Why each half of the predicate is load-bearing
## ---------------------------------------------------------------------------

test_that("a prose lead is not a declaration however short the caption", {
  ## Drop the lead test and `Measure (unit)` becomes an unsupported request,
  ## reserving a row whose author never asked for anything.
  expect_false(.lead_is_symbol("Term"))
  expect_false(.lead_is_symbol("Qty"))
  expect_true(.lead_is_symbol("n"))
  expect_true(.lead_is_symbol("%"))
  expect_true(.lead_is_symbol("E"))
  expect_true(.lead_is_symbol("n1"))
})

test_that("a bracket naming no ratio is not a declaration", {
  ## Drop this test and every short-led caption with a bracket reserves.
  expect_false(.inner_states_a_ratio("Primary"))
  expect_false(.inner_states_a_ratio("Cycle 4"))
  expect_true(.inner_states_a_ratio("%"))
  expect_true(.inner_states_a_ratio("Q/50 KG"))
  expect_identical(status_of("Q (Primary)"), "not_stated")
})

## ---------------------------------------------------------------------------
## The line boundary
## ---------------------------------------------------------------------------

test_that("a form must be the whole line, not merely its ending", {
  ## Prose that happens to end in a form is prose. Only a line the author set
  ## apart states one.
  expect_null(.header_line_stats(paste0(VOCAB_A[["stub"]], "\nCounts shown as n (%)")))
  expect_identical(.header_line_stats(paste0(VOCAB_A[["stub"]], "\nn (%)")),
                   c("count", "pct"))
})

test_that("the author's spacing does not change the declaration", {
  for (form in c("n (%)", "n(%)", "N (%)", "n  (%)")) {
    expect_identical(.header_line_stats(paste0(VOCAB_A[["stub"]], "\n", form)),
                     c("count", "pct"), info = form)
  }
})

test_that("a single-line header is not read through the line channel", {
  ## One line is prose OR a statistic line; deciding which belongs to the
  ## row-label channel, which already exists.
  expect_null(.header_line_stats("n (%)"))
  expect_null(.header_line_stats(VOCAB_A[["stub"]]))
})

test_that("a multi-line caption is classified by its final line only", {
  ## Regression: an earlier draft flattened the newlines before classifying,
  ## which let a multi-line header reach the single-line channels and acquire a
  ## form the line channel had refused. The two readers must agree.
  x <- paste0(VOCAB_A[["stub"]], "\nMean (SD)")
  expect_null(.header_line_stats(x))
  expect_null(stats_of(x))
  expect_false(identical(status_of(x), "stated_and_read"))
})

test_that("the admitted set is exactly one form, and it is a token set", {
  ## Named by grammar token, never by surface text, so an author writing the
  ## same request differently is not turned away.
  expect_identical(.DECLARED_HEADER_LINE_STATS, c("count", "pct"))
  for (form in c("Mean (SD)", "Median (Q1, Q3)", "% (95% CI)", "n (n/N)")) {
    expect_null(.header_line_stats(paste0(VOCAB_A[["stub"]], "\n", form)),
                info = form)
  }
})

## ---------------------------------------------------------------------------
## Metamorphic: rename the vocabulary, keep the shapes
## ---------------------------------------------------------------------------

test_that("renaming every prose word leaves every verdict unchanged", {
  render <- function(v) {
    c(sprintf("%s\nn (%%)", v[["stub"]]),
      sprintf("%s\nQ (Q/50 %s)", v[["stub"]], v[["unit"]]),
      sprintf("%s (Primary)", v[["prose"]]),
      sprintf("%s, n (%%)", v[["prose"]]),
      sprintf("%s\n%% (95%% CI)", v[["stub"]]),
      v[["prose"]])
  }
  a <- render(VOCAB_A)
  b <- render(VOCAB_B)
  expect_false(any(a == b))

  sa <- vapply(a, status_of, character(1), USE.NAMES = FALSE)
  sb <- vapply(b, status_of, character(1), USE.NAMES = FALSE)
  expect_identical(sa, sb)

  ## Assert the scope, so a grammar change turns this red rather than leaving
  ## it vacuously green on six captions the reader no longer understands.
  expect_identical(sum(sa == "stated_and_read"), 2L)
  expect_identical(sum(sa == "stated_not_read"), 1L)
  expect_identical(sum(sa == "stated_not_admitted"), 1L)
  expect_identical(sum(sa == "not_stated"), 2L)
})

## ---------------------------------------------------------------------------
## Shadow status
## ---------------------------------------------------------------------------

test_that("nothing in the declaration reader is wired into production", {
  ## PR5b-3a reads; it does not decide. If a call site appears, this stage's
  ## whole premise -- that no output can move -- has quietly stopped holding.
  ##
  ## Read from the namespace rather than from `R/`, so the check holds under
  ## `R CMD check`, where the sources are not beside the tests.
  readers <- c(".caption_declares_statistic", ".header_line_stats",
               ".declaration_fragment", ".line_states_form",
               ".lead_is_symbol", ".inner_states_a_ratio",
               ".final_declaration_line", ".strip_footnote_markers")
  ns <- asNamespace("arsbridge")
  others <- Filter(is.function,
                   mget(setdiff(ls(ns, all.names = TRUE), readers),
                        envir = ns, ifnotfound = list(NULL)))
  expect_gt(length(others), 200L)

  body_text <- vapply(others, function(f) paste(deparse(f), collapse = "\n"),
                      character(1))
  for (fn in setdiff(readers, ".strip_footnote_markers")) {
    expect_identical(sum(grepl(fn, body_text, fixed = TRUE)), 0L, info = fn)
  }
  ## The one exception, deliberate and singular: the shared marker set.
  expect_identical(sum(grepl(".strip_footnote_markers", body_text, fixed = TRUE)),
                   1L)
})
