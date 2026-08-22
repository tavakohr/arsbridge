## ---------------------------------------------------------------------------
## Reading a statistic DECLARATION, and telling the answers apart
## ---------------------------------------------------------------------------
##
## PR5b-3a, evidence acquisition. Everything in this file READS; nothing in it
## DECIDES. No function here is called from `build_ars_json()`, from
## `.infer_row_method()`, or from any renderer -- the readers are built and
## tested first, so that the handoff in PR5b-3b moves control onto evidence
## already proved rather than proving it and moving it in one step.
##
## The distinctions this file exists to draw:
##
##   stated_and_read      the site names a presentation form, and it parses
##   stated_not_read      the site names a presentation form that no supported
##                        statistic parses from -- an explicit request this
##                        version cannot serve
##   stated_not_admitted  the site names a form that parses, but the channel
##                        that owns this site's boundary does not admit that
##                        form yet
##   not_stated           the site names no form at all
##
## The last three all look like "no tokens" to a reader that only asks whether
## it got any, and they must not be merged, because they call for different
## actions. "We cannot compute what you asked for" is answered by naming the
## supported forms. "You did not say what you want" is answered by stating one.
## "We read you, but this channel has not been widened to that form" is
## answered by widening it on the evidence, and it is recorded separately so
## that evidence can accumulate.
##
## What is NOT here, deliberately: nothing reads a restriction, a variable's
## type, a slot count or a placeholder. A declaration is something the shell
## says in words.

## ---------------------------------------------------------------------------
## Footnote markers
## ---------------------------------------------------------------------------

#' The characters a shell author glues to the end of a label to point at a
#' footnote.
#'
#' All of these are Unicode MODIFIER LETTERS or superscript digits -- marks
#' whose whole job is to sit above the baseline and refer elsewhere. That is
#' the property being tested, and it is why the set is given as codepoint
#' ranges rather than as the particular letters one study happened to use.
#'
#' The tail of Phonetic Extensions (U+1D6B-U+1D7F) and Phonetic Extensions
#' Supplement (U+1D80-U+1DBF) interleave two different things: ordinary
#' lowercase letters used in phonetic transcription, and modifier letters. Only
#' the modifier letters belong here. Taking the whole span would strip a
#' genuine trailing letter from a label; taking none of it -- which is what the
#' package did before -- means a header marked with a superscript "c" (U+1D9C,
#' a modifier letter) reads differently from its twin marked with a superscript
#' "b" (U+1D47, also a modifier letter), purely because the two live in
#' different blocks. Two labels differing only in which footnote they cite must
#' classify the same way.
#'
#' Checked against the Unicode general category over that span: U+1D2C-U+1D6A,
#' U+1D78 and U+1D9B-U+1DBF are Lm, and everything between them is Ll.
#' @noRd
.FOOTNOTE_MARKER_CLASS <- paste0(
  "[",
  "\u00aa\u00b2\u00b3\u00b9\u00ba",  # ordinal indicators, superscript 1/2/3
  "\u02b0-\u02ff",                      # Spacing Modifier Letters
  "\u1d2c-\u1d6a",                      # Phonetic Extensions, modifier letters
  "\u1d78",                              # ditto, isolated among lowercase
  "\u1d9b-\u1dbf",                      # Phonetic Extensions Supplement, modifiers
  "\u2070-\u209f",                      # Superscripts and Subscripts
  "]+"
)

#' Drop a run of footnote markers where one can be cited.
#'
#' A marker cites a footnote from the end of the thing it annotates. That is
#' usually the end of the label, but an author who wants to annotate what is
#' inside a bracket has nowhere else to put it: `% (95% CI<marker>)` cites a
#' footnote about the interval, not about the percentage. Both positions are
#' the same act.
#'
#' @param within When TRUE, also drop a run sitting immediately before a
#'   closing bracket. Off by default, and deliberately: `.header_suffix_stats()`
#'   is production and reads this position today as "unparseable", so widening
#'   it there would change which method some rows get. The shadow declaration
#'   reader turns it on, because a reader that calls a citable interval an
#'   UNSUPPORTED request manufactures exactly the false reservation this stage
#'   exists to avoid. Reconciling the two is a decision for a later checkpoint,
#'   not a side effect of this one.
#'
#'   Not extended to a marker before a space or a comma. Nothing observed needs
#'   it, and a superscript loose in prose is as likely to be a unit as a
#'   citation.
#' @noRd
.strip_footnote_markers <- function(x, within = FALSE) {
  txt <- trimws(.shape_text(x))
  if (within) {
    txt <- gsub(paste0(.FOOTNOTE_MARKER_CLASS, "\\)"), ")", txt, perl = TRUE)
  }
  trimws(sub(paste0(.FOOTNOTE_MARKER_CLASS, "$"), "", txt, perl = TRUE))
}

## ---------------------------------------------------------------------------
## The trailing declaration
## ---------------------------------------------------------------------------

#' The bounded trailing declaration a line ends with, if it ends with one.
#'
#' A presentation form written into a caption has a shape: a short lead naming
#' what the first number is, then a bracket naming what follows it -- `n (%)`,
#' `Q (Q/50 KG)`, `% (95% CI)`. That shape is what is extracted here, and
#' nothing is judged about it yet.
#'
#' Extract first, then ask whether it parses. Asking the whole line answers a
#' different question: `Zeta Grouping n (%)` is not a statistic line, and a
#' reader that only tries the whole line concludes the author said nothing.
#'
#' Newlines are not collapsed here. This reads ONE line; choosing which line is
#' the declaration site belongs to the caller, because that choice is what
#' separates the channels.
#'
#' @param whole When TRUE the form must be the entire line, not merely its
#'   ending. Tested by anchoring the match at both ends rather than by
#'   comparing the reconstructed text against the line, which would make the
#'   answer depend on the author's spacing: `n(%)` and `n (%)` are the same
#'   declaration, and a comparison against a canonical rendering says they are
#'   not.
#' @return `list(found, lead, inner, text)`. `found` is FALSE and the rest
#'   empty when the line does not end in that shape.
#' @noRd
.declaration_fragment <- function(line, whole = FALSE) {
  none <- list(found = FALSE, lead = "", inner = "", text = "")
  txt <- .strip_footnote_markers(line, within = TRUE)
  if (!nzchar(txt) || grepl("[\r\n]", txt)) return(none)

  ## `[^ ()]+` bounds the lead at the nearest space: in `Zeta Grouping n (%)` the
  ## lead is `n`, not the whole caption. The bracket may not itself contain
  ## brackets, so `Rate (per 100 PY (95% CI))` is not read as a flat form.
  pattern <- paste0(if (whole) "^" else "", "([^ ()]+)\\s*\\(([^()]*)\\)\\s*$")
  m <- regmatches(txt, regexec(pattern, txt))[[1]]
  if (length(m) != 3L) return(none)

  list(found = TRUE, lead = m[[2]], inner = m[[3]],
       text = paste0(m[[2]], " (", m[[3]], ")"))
}

#' Is this lead a SYMBOL rather than a word?
#'
#' The load-bearing half of telling `Q (Q/50 KG)` -- a declaration -- from
#' `Population (Safety)` -- a caption that happens to end in a bracket. A
#' presentation form leads with a symbol: `n`, `N`, `%`, `E`. A caption leads
#' with prose.
#'
#' Both halves are needed. Length alone admits `Term (verbatim)`; "not a word"
#' alone admits a long symbol run that is really an abbreviation.
#' @noRd
.lead_is_symbol <- function(lead) {
  lead <- .shape_text(lead)
  nzchar(lead) && nchar(lead) <= 3L && !grepl("^[A-Za-z]{2,}$", lead)
}

#' Does the bracket state a ratio or a proportion?
#'
#' The second half. A presentation form says what its number is measured
#' against -- a percent sign, or a solidus dividing one unit by another. A
#' qualifier in brackets (`(Safety)`, `(baseline)`, `(Week 12)`) says no such
#' thing. Without this, every short-led caption would read as a declaration.
#' @noRd
.inner_states_a_ratio <- function(inner) {
  grepl("[%/]", .shape_text(inner))
}

## ---------------------------------------------------------------------------
## The stub-column header as a declaration site
## ---------------------------------------------------------------------------

#' The statistic tokens the line-bounded header channel admits.
#'
#' Deliberately one form. `.header_line_stats()` reads a boundary the existing
#' header reader does not, and a new boundary earns its scope by evidence, not
#' by symmetry. The count-and-percentage form is the one observed written that
#' way and the one whose misreading is cheapest to detect. `Mean (SD)` and the
#' interval forms are not excluded because they are wrong; they are excluded
#' because nothing yet shows an author using THIS boundary for them, and
#' admitting a form on the strength of its plausibility is how a reader starts
#' inventing requests. A form that parses but is not in this set is reported as
#' `stated_not_admitted`, which is how the evidence for widening accumulates.
#' @noRd
.DECLARED_HEADER_LINE_STATS <- c("count", "pct")

#' The form a header states on a line of its own.
#'
#' `.header_suffix_stats()` splits a header at its last comma or spaced dash.
#' That misses the other way authors bound a form: they put it on a line
#' beneath the prose, so the cell reads
#'
#'     Zeta Grouping / Subgrouping
#'     n (%)
#'
#' A line break is a stronger boundary than a comma, not a weaker one -- the
#' author separated the two deliberately -- but it is also the boundary that
#' would be most damaging to read loosely, because every multi-line caption
#' ends in SOME line. So the last line must clear three independent tests: it
#' is a whole line of its own, it is shaped like a form rather than prose, and
#' it parses to exactly the admitted token set.
#'
#' A single-line header is not read here at all. If such a header is wholly a
#' statistic line, that is the row-label channel's question, not this one.
#'
#' @return An ordered character vector of statistic tokens, or NULL.
#' @noRd
.header_line_stats <- function(header) {
  last <- .final_declaration_line(header)
  if (!nzchar(last)) return(NULL)
  toks <- .line_states_form(last)
  if (!identical(toks, .DECLARED_HEADER_LINE_STATS)) return(NULL)
  toks
}

#' The last line of a MULTI-line caption, or "" when there is only one.
#' @noRd
.final_declaration_line <- function(caption) {
  lines <- strsplit(.shape_text(caption), "[\r\n]+")[[1]]
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  if (length(lines) < 2L) "" else lines[[length(lines)]]
}

#' The statistics a line states when the WHOLE line is a form.
#'
#' `.declaration_fragment()` reads a TRAILING form by default, so a line of
#' prose ending in one would qualify without `whole = TRUE`. Used only where
#' the boundary is a line break, which is why "the line IS the form" is the
#' test rather than "the line ends with a form".
#'
#' @return Statistic tokens, or NULL -- with no view on whether they are
#'   admitted, which is the caller's question.
#' @noRd
.line_states_form <- function(line) {
  frag <- .declaration_fragment(line, whole = TRUE)
  if (!frag$found || !.lead_is_symbol(frag$lead)) return(NULL)
  .parse_stat_label(frag$text)
}

## ---------------------------------------------------------------------------
## The classifier
## ---------------------------------------------------------------------------

#' Classify a caption's declaration site.
#'
#' `caption` is any text that may carry a declaration: a stub-column header, a
#' row label, a table caption. The outcomes, and why they must stay apart, are
#' set out at the top of this file.
#'
#' A multi-line caption and a single-line caption are DIFFERENT sites and are
#' read by different channels, so this does not flatten the newlines away. On a
#' multi-line caption the declaration site is the final line, and admission is
#' `.header_line_stats()`'s call -- this function never hands back tokens that
#' reader refused, or the narrow admission set would be narrow in one reader
#' and wide in another for the same text.
#'
#' The admission set constrains only the line boundary, which is new here. A
#' single-line caption is read by the channels that already existed -- the
#' trailing form and the separator-bounded suffix -- and those are not
#' re-narrowed, because narrowing them would change what the package already
#' reads.
#'
#' @return `list(status, stats, fragment)` where `status` is one of
#'   `"stated_and_read"`, `"stated_not_read"`, `"stated_not_admitted"`,
#'   `"not_stated"`; `stats` is the ordered statistic tokens when read and
#'   admitted, otherwise NULL.
#' @noRd
.caption_declares_statistic <- function(caption) {
  out <- function(status, stats = NULL, fragment = "") {
    list(status = status, stats = stats, fragment = fragment)
  }
  txt <- .shape_text(caption)
  if (!nzchar(trimws(txt))) return(out("not_stated"))

  last <- .final_declaration_line(txt)
  if (nzchar(last)) {
    ## Multi-line: the final line is the site, and the line channel admits.
    toks <- .header_line_stats(txt)
    if (length(toks) > 0) return(out("stated_and_read", toks, last))

    read <- .line_states_form(last)
    if (length(read) > 0) return(out("stated_not_admitted", NULL, last))

    frag <- .declaration_fragment(last)
    if (frag$found && .lead_is_symbol(frag$lead) &&
          .inner_states_a_ratio(frag$inner)) {
      return(out("stated_not_read", NULL, frag$text))
    }
    return(out("not_stated"))
  }

  ## Single line: the channels that already existed.
  frag <- .declaration_fragment(txt)
  if (frag$found) {
    toks <- .parse_stat_label(frag$text)
    if (length(toks) > 0) return(out("stated_and_read", toks, frag$text))
    if (.lead_is_symbol(frag$lead) && .inner_states_a_ratio(frag$inner)) {
      return(out("stated_not_read", NULL, frag$text))
    }
  }

  toks <- .header_suffix_stats(txt)
  if (length(toks) > 0) return(out("stated_and_read", toks, trimws(txt)))

  out("not_stated")
}
