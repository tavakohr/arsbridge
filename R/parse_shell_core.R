## arsbridge -- parse_shell_core.R
## ---------------------------------------------------------------------------
## The format-agnostic half of shell parsing: everything that reads TEXT and
## RUN METADATA rather than a document. parse_shell_docx.R (OOXML) and
## parse_shell_xlsx.R (SpreadsheetML) both walk their own file format, then
## hand what they found to the code here -- so the annotation grammar,
## detection layers, and section assembly have exactly one implementation and
## the two formats cannot drift apart.
##
## Two seams join a reader to this file. A new reader is valid when it honours
## both.
##
## SEAM 1 -- the run list. One entry per formatting run of a cell:
##   list(text, raw_text, color_hex, highlight, bold, italic, underline, strike)
## Read by .is_annotation_styled_run() / .detect_annotation() and friends,
## which never see the document. Conventions:
##   * `color_hex` is 6 hex digits, upper case ("C00000"), or NA when the run
##     states no colour. SpreadsheetML writes 8-digit ARGB ("FFC00000"), so
##     the xlsx reader strips the leading alpha byte -- with that done, the
##     .GREY_HEX / .BLACK_HEXES comparisons below need no format branch.
##   * `highlight` is a lower-case Word highlight name or NA. Excel has no
##     equivalent, so it is always NA there and the highlight test simply
##     never fires.
##   * `text` is normalized (.normalize_shell_text); `raw_text` is what the
##     author actually typed, kept for the run-integrity lint.
##   * A whole-cell property (an entirely red Excel cell) is presented as ONE
##     run, so detection sees a single interface either way.
##
## SEAM 2 -- the header-grid record. One entry per header cell:
##   list(row, col_start, col_end, text, annotation, vmerge_continue)
## Consumed by R/column_tree.R, which is already format-agnostic.
##
## The product of both is the SECTION OBJECT (.new_section() below) -- the
## contract every downstream stage consumes. Anything that produces a
## conformant section object is a valid shell reader.

## ---------------------------------------------------------------------------
## Constants
## ---------------------------------------------------------------------------

.GREY_HEX <- "808080"   ## ignored as annotation -- source / disclaimer
.BLACK_HEXES <- c("000000", "FFFFFF", "AUTO", "NONE")

## Case-insensitive ("TABLE 14.1.1"), tolerates a trailing suffix letter
## ("Listing 16.2.1a"), single-number designators ("Figure 3"), and an
## optional inline title after a colon ("Table 14.1.1: Summary of
## Demographics") -- the third group is that inline title, empty when the
## heading is alone on its own paragraph. The colon is what tells a real
## inline title apart from body prose that just happens to mention a table
## number ("Table 14.1.1 shows the demographic summary" must NOT match): a
## trailing title is only ever recognised when it follows a literal ":".
.TLF_HEADING_RE <- "^(?i)(Table|Figure|Listing)\\s+(\\d{1,3}(?:\\.\\d+)*[a-z]?)\\s*(?::\\s*(.*))?\\s*$"

## Colon-less inline title ("Table 14.1.1 Summary of Subject Status ...").
## Some sponsor shells -- RWE shells especially -- put the number, title,
## population, and even the population annotation on ONE heading paragraph
## with no colon after the number. Same designator and number shape as
## .TLF_HEADING_RE, plus an optional "No."/"#" before the number and an
## optional stray "." after it; the third group is everything after the
## number. Acceptance is NOT regex-only: .match_tlf_heading() applies
## prose/TOC rejection rules on that tail so body text mentioning a table
## number ("Table 14.1.1 shows ...") still never starts a section.
.TLF_HEADING_NOCOLON_RE <-
  "^(?i)(Table|Figure|Listing)\\s+(?:No\\.?\\s*|#\\s*)?(\\d{1,3}(?:\\.\\d+)*[a-z]?)\\.?\\s+(\\S.*)$"

## First word after the number that marks the line as PROSE, not a heading
## ("Table 14.1.1 shows ..."). Real TLF titles start with a noun phrase
## ("Summary of ...", "Demographics -", "Kaplan-Meier ..."), never with one
## of these verbs/auxiliaries.
.HEADING_PROSE_WORDS <- c(
  "shows", "show", "showed", "shown", "presents", "present", "presented",
  "displays", "display", "displayed", "summarizes", "summarises",
  "summarized", "summarised", "lists", "listed", "contains", "contained",
  "provides", "provided", "includes", "included", "include", "excludes",
  "excluded", "describes", "described", "reports", "reported", "gives",
  "refers", "uses", "used", "is", "are", "was", "were", "will", "has",
  "have", "had", "may", "can", "must", "should", "does", "did",
  "below", "above"
)

## "[PROGRAMMING DATASETS USED: ADSL, ADAE]" -- a source-datasets suffix
## some sponsors carry inside the heading paragraph itself instead of a
## "Source: ..." line under the table.
.PROGRAMMING_DATASETS_RE <-
  "(?i)\\[\\s*PROGRAMMING\\s+DATASETS?\\s+USED\\s*:\\s*([^\\]]+)\\]"

## Title/population separator inside a one-line heading: an en/em dash
## (spaces optional -- Word authors drop them), or a spaced ASCII hyphen.
## The spaces around the plain hyphen are REQUIRED so hyphenated words in a
## title ("Follow-up", "Post-hoc") are never split.
.HEADING_SEP_RE <- "\\s*[\u2013\u2014]\\s*|\\s+-\\s+"

## Population wording that only counts inside a heading tail ("... - Part A
## Completed Cohort"). Kept OUT of .POPULATION_LEXICON_RE deliberately: the
## NEED_POP state machine uses that lexicon on whole paragraphs, and adding
## "cohort"/"group" there would misfile a second title line like "by Dose
## Cohort" as the population.
.HEADING_POP_HINT_RE <- "(?i)\\bcohorts?\\b|\\bgroups?\\b|\\bparticipants\\b"

## The single, canonical description of the heading form arsbridge detects
## most reliably. Surfaced by the no-heading and no-title diagnostics and
## documented verbatim in ?spec_to_ars, so the message a user sees when a
## title cannot be found matches the guidance they are pointed to. Two
## strings: a one-line recommendation for `action` fields, and a short
## bulletable form for cli messages.
.RECOMMENDED_HEADING_HINT <- paste(
  "Give each output its own heading paragraph that begins with Table,",
  "Figure, or Listing followed by its number and a title, e.g.",
  "'Table 14.1.1: Summary of Demographics'. Keep it an ordinary paragraph",
  "-- not inside a text box, shape, or table cell -- so arsbridge can find it."
)

## Accepts "Source:", "Sources:", "Data Source:", "Source datasets:", and
## "=" in place of ":".
.SOURCE_LINE_RE <- "^\\s*(?:Data\\s+)?Sources?(?:\\s+datasets?)?\\s*[:=]\\s*(.+?)\\.?\\s*$"
## Words a genuine population / analysis-set statement almost always
## contains. Used to recognise the population line even when a shell has
## no line for it right after the title (or has an extra title line first).
.POPULATION_LEXICON_RE <- "(?i)population|analysis\\s+set|subjects|patients|safety|\\bITT\\b|\\bFAS\\b|\\bPP\\b"
## A CDISC population/analysis-set flag variable ("SAFFL", "ITTFL", "FASFL",
## "RANDFL", ...) -- always ends in "FL". Used to recognise a population line
## that has no population WORDING but whose own annotation is a flag
## reference ("(ADSL.SAFFL='Y')"). This must stay narrow: a treatment-column
## mapping line ("Treatment columns -> ADSL.TRT01A") also carries an
## annotation, and if it were read as the population it would never reach
## bind_annotations() as the column-axis grouping.
.POPULATION_FLAG_RE <- "\\b[A-Z][A-Z0-9]{1,7}FL\\b"
## Below-table annotation convention: "Label -> DATASET.VAR ..." (ASCII arrow
## or U+2192). The left side names one or more stub rows / the column axis;
## the right side is the annotation. Read by bind_annotations().
.ARROW_ANNOT_RE <- "^\\s*.+?\\s*(?:->|\u2192)\\s*.+$"

## A quoted comparison value. Smart quotes are straightened to ASCII quotes
## at ingestion (.normalize_shell_text), so only ' and " can reach a regex.
.QUOTED_VALUE <- "(?:'[^']*'|\"[^\"]*\")"

## A value inside an IN (...) list: quoted either style, or bare numeric --
## real-world shells annotate pooled columns as "ADSL.COHORTN IN (1,2)".
.IN_VALUE <- paste0("(?:", .QUOTED_VALUE, "|[-+]?\\d+(?:\\.\\d+)?)")

## Multi-pattern union for annotation detection (Layer 3 + validation gate
## for Layers 1 and 2). Order matters: PCRE alternation returns the leftmost
## match of the leftmost branch that fits, so MORE-SPECIFIC patterns must
## come BEFORE the bare DATASET.VARIABLE form -- otherwise a string like
## "ADSL.SAFFL='Y'" matches the simple branch first and drops the ='Y' tail.
## Shared regex tokens .ADAM_DS / .ADAM_VAR come from R/aaa_constants.R.
.ANNOTATION_PATTERN <- paste(
  ## DATASET.VAR WHERE OTHERVAR='val'  (longest -- match first)
  paste0("\\b", .ADAM_DS, "\\.", .ADAM_VAR,
         "\\s+(?i:where)\\s+", .ADAM_VAR, "\\s*=\\s*", .QUOTED_VALUE),
  ## DATASET.VAR IN ('a','b') / NOT IN ("a","b") / IN (1,2)  (parenthesized
  ## value list; either quote style or bare numerics -- .canon_annotation
  ## rewrites double to single quotes)
  paste0("\\b", .ADAM_DS, "\\.", .ADAM_VAR,
         "\\s+(?i:NOT\\s*IN|NOTIN|IN)\\s*\\(\\s*", .IN_VALUE,
         "(?:\\s*,\\s*", .IN_VALUE, ")*\\s*\\)"),
  ## DATASET.VAR EQ 'val' / NE 'val' / ... (ARS comparator form)
  paste0("\\b", .ADAM_DS, "\\.", .ADAM_VAR,
         "\\s+(?:EQ|NE|IN|NOTIN|GT|GE|LT|LE)\\s+", .QUOTED_VALUE),
  ## DATASET.VAR='val' / =="val"  (most common shell-annotation form; the
  ## double-equals R/Python operator is accepted too -- ==? matches one or two)
  paste0("\\b", .ADAM_DS, "\\.", .ADAM_VAR, "\\s*==?\\s*", .QUOTED_VALUE),
  ## DATASET.VAR=1 / ==99  (unquoted numeric equality -- the usual convention
  ## for column-header annotations like "Cohort 1 (N=XX) ADSL.COHORTN=1" or
  ## the double-equals form "ADSL.COHORTN==99")
  paste0("\\b", .ADAM_DS, "\\.", .ADAM_VAR,
         "\\s*==?\\s*[-+]?\\d+(?:\\.\\d+)?\\b"),
  ## Call-form missing checks the way annotated shells write them -- R's
  ## "is.na(ADSL.COHORTN)" and SAS's "missing(COHORTN)", plus the negations
  ## "!is.na(...)" / "not missing(...)". Listed before the bare DS.VAR branch
  ## so the whole call is captured, not just the inner reference.
  paste0("(?i:(?:!|\\bnot\\b)\\s*(?:is\\.na|missing)\\s*\\(\\s*",
         .ADAM_DS, "\\.", .ADAM_VAR, "\\s*\\))"),
  paste0("(?i:(?:is\\.na|missing)\\s*\\(\\s*",
         .ADAM_DS, "\\.", .ADAM_VAR, "\\s*\\))"),
  ## DATASET.VAR not null / not missing
  paste0("\\b", .ADAM_DS, "\\.", .ADAM_VAR,
         "\\s+(?i:not\\s+(?:null|missing))"),
  ## DATASET.VAR is null / is missing (positive form -- listed AFTER the
  ## "not null / not missing" branch; before the bare DS.VAR branch so the
  ## " is missing" tail is captured instead of truncated)
  paste0("\\b", .ADAM_DS, "\\.", .ADAM_VAR,
         "\\s+(?i:(?:is\\s+)?(?:null|missing))\\b"),
  ## unique USUBJID in DATASET (count expression)
  paste0("(?i)unique\\s+USUBJID\\s+in\\s+", .ADAM_DS),
  ## Bare DATASET.VARIABLE  (least specific -- match last)
  paste0("\\b", .ADAM_DS, "\\.", .ADAM_VAR, "\\b"),
  sep = "|"
)

#' Rewrite double-quoted comparison values to the package-canonical single
#' quotes (ADSL.SCRNFL="Y" becomes ADSL.SCRNFL='Y') so downstream where-
#' clause parsing -- whose grammar is single-quote only -- and the emitted
#' ARS JSON stay uniform no matter which quote style the shell author used.
#' Applied to captured annotation strings only, never to display text.
#' @noRd
.canon_annotation <- function(x) {
  gsub("\"([^\"]*)\"", "'\\1'", x)
}
## ---------------------------------------------------------------------------
## Bracket normalization -- real-world "standardized bracket" shells
## (HANDOFF_realworld_bracket_shells, Phase R1)
## ---------------------------------------------------------------------------

#' Start/end character positions of the TOP-LEVEL `[...]` spans of `text`,
#' tracked with a nesting counter: real shells legally nest a
#' "[PROGRAMMING DATASETS USED: ...]" directive INSIDE a filter annotation.
#' An unclosed bracket contributes no span (an annotation continuing in the
#' next paragraph is a later phase).
#' @noRd
.bracket_spans <- function(text) {
  chars <- strsplit(text, "", fixed = TRUE)[[1]]
  depth <- 0L
  start <- NA_integer_
  spans <- list()
  for (i in seq_along(chars)) {
    if (chars[i] == "[") {
      if (depth == 0L) start <- i
      depth <- depth + 1L
    } else if (chars[i] == "]" && depth > 0L) {
      depth <- depth - 1L
      if (depth == 0L && !is.na(start)) {
        spans[[length(spans) + 1L]] <- c(start, i)
        start <- NA_integer_
      }
    }
  }
  spans
}

#' Collapse an accidental space after the dataset dot ("ADSL. COMPLFL" --
#' Word authors line-break there) so the reference matches the ADaM regex.
#' Anchored on the dataset-name shape, so sentence periods never collapse.
#' @noRd
.collapse_ds_dot_space <- function(x) {
  gsub(paste0("\\b(", .ADAM_DS, ")\\.\\s+(?=[A-Z])"), "\\1.", x, perl = TRUE)
}

#' Rewrite ONE bracket span's content. Returns list(keep, text, datasets,
#' dropped): `keep` FALSE removes the span from the cell text entirely;
#' `datasets` carries any lifted PROGRAMMING DATASETS names; `dropped` is
#' the prose removed with nothing machine-readable in it (for diagnostics).
#' @noRd
.rewrite_bracket_content <- function(content) {
  out <- list(keep = FALSE, text = "", datasets = character(0),
              dropped = character(0))

  ## Lift nested "[PROGRAMMING DATASETS USED: ...]" directives out first --
  ## they have no nesting of their own, so the flat regex is safe here.
  prog <- regmatches(content,
                     gregexpr(.PROGRAMMING_DATASETS_RE, content, perl = TRUE))[[1]]
  if (length(prog) > 0) {
    inner <- sub(.PROGRAMMING_DATASETS_RE, "\\1", prog, perl = TRUE)
    out$datasets <- unique(unlist(lapply(inner, .split_source_list)))
    content <- gsub(.PROGRAMMING_DATASETS_RE, "", content, perl = TRUE)
  }
  ## The span itself may BE the directive (unnested form, brackets already
  ## consumed by the span scan).
  bare_dir <- regexpr("(?i)^\\s*PROGRAMMING\\s+DATASETS?\\s+USED\\s*:\\s*(.+)$",
                      content, perl = TRUE)
  if (bare_dir > 0) {
    inner <- sub("(?i)^\\s*PROGRAMMING\\s+DATASETS?\\s+USED\\s*:\\s*", "",
                 content, perl = TRUE)
    out$datasets <- unique(c(out$datasets, .split_source_list(inner)))
    return(out)
  }

  content <- trimws(content)
  if (!nzchar(content)) return(out)

  ## Footnote marker ("[a]", "[c]", "[1]"): display apparatus, never an
  ## annotation -- drop it from the label silently.
  if (grepl("^(?:[A-Za-z]|[0-9]{1,2})$", content)) return(out)

  content <- .collapse_ds_dot_space(content)

  ## Repeat directive ("[Repeat the applicable row or section ...]"):
  ## template expansion is its own later phase -- drop it here so its drug
  ## list never masquerades as this row's filter, and report it.
  if (grepl("(?i)^\\s*Repeat\\b", content, perl = TRUE)) {
    out$dropped <- content
    return(out)
  }

  ## Instruction wrapper carrying the condition in the sentence:
  ## "...apply the stated condition: <COND>. Keep the display label ...".
  ## The subject-counting prefixes ("USUBJID WHERE", "UNIQUE SUBJECTS
  ## WITH", "All Patients WHERE") become the count-of-unique-USUBJID
  ## marker AFTER the condition, so the condition leads and the label
  ## split stays clean.
  if (grepl("(?i)\\bcondition\\s*:", content, perl = TRUE)) {
    cond <- sub("(?i)^.*?condition\\s*:\\s*", "", content, perl = TRUE)
    cond <- sub("(?i)\\.?\\s*Keep\\s+the\\s+display.*$", "", cond, perl = TRUE)
    cond <- trimws(sub("\\.\\s*$", "", cond))
    subject_count <- FALSE
    strip <- c(
      "(?i)^(?:count\\s+of\\s+)?(?:all\\s+)?(?:patients|subjects)\\s+WHERE\\s+",
      "(?i)^(?:unique\\s+)?USUBJID\\s+WHERE\\s+",
      "(?i)^UNIQUE\\s+SUBJECTS?\\s+WITH\\s+"
    )
    for (re in strip) {
      if (grepl(re, cond, perl = TRUE)) {
        cond <- sub(re, "", cond, perl = TRUE)
        subject_count <- TRUE
        break
      }
    }
    if (nzchar(cond)) {
      out$keep <- TRUE
      out$text <- if (subject_count) {
        paste0(cond, "; count of unique USUBJID")
      } else {
        cond
      }
    }
    return(out)
  }

  ## Per-row variant: "Use DS.VAR for this displayed row; apply <FILTER>
  ## only as the row or record filter ..." -> "DS.VAR WHERE FILTER".
  use_re <- paste0("(?i)^use\\s+(", .ADAM_DS, "\\.", .ADAM_VAR,
                   ")\\s+for\\s+this\\s+displayed\\s+row")
  um <- regexpr(use_re, content, perl = TRUE)
  if (um > 0) {
    var <- sub(paste0(use_re, ".*$"), "\\1", content, perl = TRUE)
    rest <- substr(content, um + attr(um, "match.length"), nchar(content))
    fm <- regexpr(.ANNOTATION_PATTERN, rest, perl = TRUE)
    out$keep <- TRUE
    out$text <- if (fm > 0) {
      paste0(var, " WHERE ", substr(rest, fm, fm + attr(fm, "match.length") - 1L))
    } else {
      var
    }
    return(out)
  }

  ## Machine annotation (or a compound one): keep as-is.
  if (grepl(.ANNOTATION_PATTERN, content, perl = TRUE)) {
    ## Prose count instruction without the "condition:" scaffolding
    ## ("Count distinct subjects using USUBJID ... ADMH.MHCAT=...").
    if (grepl("(?i)count\\s+distinct\\s+subjects|(?i)unique\\s+subjects",
              content, perl = TRUE)) {
      fm <- regexpr(.ANNOTATION_PATTERN, content, perl = TRUE)
      cond <- trimws(sub("\\.\\s*$", "",
                         substr(content, fm, nchar(content))))
      out$keep <- TRUE
      out$text <- paste0(cond, "; count of unique USUBJID")
      return(out)
    }
    out$keep <- TRUE
    out$text <- content
    return(out)
  }

  ## Pure guidance prose: nothing machine-readable -- remove, report.
  out$dropped <- content
  out
}

#' Normalize every top-level bracket span of a cell / heading text for the
#' real-world "standardized bracket" conventions: nested PROGRAMMING
#' DATASETS directives are lifted out, instruction wrappers are unwrapped to
#' the condition they carry, footnote markers and pure guidance prose are
#' removed. Text outside brackets is untouched. When the text has no
#' brackets at all (a coloured-run candidate arrives bare), the wrapper
#' rules are still applied to the whole string -- but a bare string is never
#' DROPPED, only rewritten.
#' @return list(text, source_datasets, dropped)
#' @noRd
.unwrap_bracket_instructions <- function(text) {
  text <- as.character(text %||% "")
  out <- list(text = text, source_datasets = character(0),
              dropped = character(0))
  if (!nzchar(trimws(text))) return(out)

  spans <- .bracket_spans(text)
  if (length(spans) == 0) {
    rewritten <- .rewrite_bracket_content(text)
    out$source_datasets <- rewritten$datasets
    if (rewritten$keep) out$text <- rewritten$text
    return(out)
  }

  for (sp in rev(spans)) {
    content <- substr(text, sp[1] + 1L, sp[2] - 1L)
    rewritten <- .rewrite_bracket_content(content)
    out$source_datasets <- unique(c(out$source_datasets, rewritten$datasets))
    out$dropped <- c(rewritten$dropped, out$dropped)
    replacement <- if (rewritten$keep) paste0("[", rewritten$text, "]") else ""
    text <- paste0(substr(text, 1, sp[1] - 1L), replacement,
                   substr(text, sp[2] + 1L, nchar(text)))
  }

  text <- gsub("[ \t]{2,}", " ", text)
  out$text <- trimws(text)
  out
}
## ---------------------------------------------------------------------------
## Heading recognition
## ---------------------------------------------------------------------------

#' Extract the named capture groups of a `regexpr(perl = TRUE)` match as a
#' named character vector (unmatched groups come back as "").
#' @noRd
.regex_named_groups <- function(text, match) {
  starts  <- attr(match, "capture.start")
  lengths <- attr(match, "capture.length")
  names_v <- attr(match, "capture.names")
  if (is.null(starts) || is.null(names_v)) return(character())

  out <- character(length(names_v))
  names(out) <- names_v
  for (i in seq_along(names_v)) {
    if (starts[1, i] > 0) {
      out[[i]] <- substr(text, starts[1, i], starts[1, i] + lengths[1, i] - 1L)
    }
  }
  out
}

#' Abort unless every custom heading pattern is usable: a character vector
#' whose every element carries the required `(?<number>...)` named group.
#' @noRd
.validate_heading_patterns <- function(heading_patterns) {
  if (is.null(heading_patterns)) return(invisible(NULL))
  if (!is.character(heading_patterns) || length(heading_patterns) == 0) {
    cli::cli_abort("{.arg heading_patterns} must be a character vector of PCRE patterns.")
  }
  missing_number <- !grepl("(?<number>", heading_patterns, fixed = TRUE)
  if (any(missing_number)) {
    cli::cli_abort(c(
      "Every {.arg heading_patterns} pattern needs a {.code (?<number>...)} named group.",
      "x" = "Missing in: {.val {heading_patterns[missing_number]}}",
      "i" = "Optional named groups: {.code (?<type>...)} (Table/Figure/Listing, defaults to Table) and {.code (?<title>...)}."
    ))
  }
  invisible(NULL)
}

#' Decide whether one paragraph is a TLF heading.
#'
#' Single entry point for every place that used to test .TLF_HEADING_RE
#' directly (body pre-scan, body walker, page-header reader), so all of
#' them agree on what a heading is. Tries, in order:
#'
#'   1. Caller-supplied `heading_patterns` (the sponsor escape hatch) --
#'      named groups `number` (required), `type`/`title` (optional). No
#'      rejection rules: the pattern author owns the grammar.
#'   2. .TLF_HEADING_RE -- the historical grammar (bare heading, or inline
#'      title after a colon), byte-identical semantics.
#'   3. .TLF_HEADING_NOCOLON_RE -- colon-less inline title, accepted only
#'      when the tail survives three rejection rules: its first word is not
#'      a prose verb (.HEADING_PROSE_WORDS), it does not start lowercase
#'      (real titles are Title Case; prose continuations are not), and it
#'      does not end like a table-of-contents entry (dot leader + page
#'      number).
#'
#' @return list(hit, reject_reason). `hit` is NULL or
#'   list(type_word, number, tail); `reject_reason` is NA or a short human
#'   explanation of why a heading-shaped line was rejected (surfaced by the
#'   zero-section near-miss diagnostics).
#' @noRd
.match_tlf_heading <- function(text, heading_patterns = NULL) {
  no_match <- list(hit = NULL, reject_reason = NA_character_)

  for (pattern in heading_patterns %||% character()) {
    m <- regexpr(pattern, text, perl = TRUE)
    if (m > 0) {
      groups <- .regex_named_groups(text, m)
      number <- if ("number" %in% names(groups)) groups[["number"]] else ""
      if (nzchar(number)) {
        type_word <- unname(groups["type"])
        if (is.na(type_word) || !nzchar(type_word)) type_word <- "Table"
        title <- unname(groups["title"])
        if (is.na(title)) title <- ""
        return(list(
          hit = list(type_word = type_word, number = unname(number),
                     tail = trimws(title)),
          reject_reason = NA_character_
        ))
      }
    }
  }

  m <- regmatches(text, regexec(.TLF_HEADING_RE, text, perl = TRUE))[[1]]
  if (length(m) == 4) {
    return(list(
      hit = list(type_word = m[2], number = m[3], tail = trimws(m[4])),
      reject_reason = NA_character_
    ))
  }

  m <- regmatches(text, regexec(.TLF_HEADING_NOCOLON_RE, text, perl = TRUE))[[1]]
  if (length(m) == 4) {
    tail <- trimws(m[4])
    first_word <- tolower(sub("^([[:alpha:]]+).*$", "\\1", tail))
    if (first_word %in% .HEADING_PROSE_WORDS) {
      return(list(hit = NULL, reject_reason = sprintf(
        "the first word after the number (%s) reads as prose, not a title",
        dQuote(first_word, q = FALSE))))
    }
    if (grepl("^[a-z]", tail)) {
      return(list(hit = NULL, reject_reason =
        "the text after the number starts lowercase, which reads as prose, not a title"))
    }
    if (grepl("\\.{2,}\\s*\\d+\\s*$", tail)) {
      return(list(hit = NULL, reject_reason =
        "the line ends like a table-of-contents entry (dot leader and page number)"))
    }
    return(list(
      hit = list(type_word = m[2], number = m[3], tail = tail),
      reject_reason = NA_character_
    ))
  }

  no_match
}

#' Trailing separators/spaces left behind when a heading tail is cut.
#' @noRd
.clean_heading_title <- function(x) {
  trimws(gsub("[-\u2013\u2014[:space:]]+$", "", x, perl = TRUE))
}

#' Split the text that follows the TLF number on a one-line heading into
#' its parts.
#'
#' RWE-style shells pack everything into the heading paragraph:
#'
#'   `Table 5.1.1 Summary of Disposition - Screened Subjects
#'    ADSL.SCRNFL="Y" [PROGRAMMING DATASETS USED: ADSL]`
#'
#' Steps, in order:
#'   1. The `[PROGRAMMING DATASETS USED: ...]` suffix is cut out into
#'      source_datasets.
#'   2. The RIGHTMOST dash separator whose right side reads as a population
#'      (population lexicon, a *FL flag annotation, or the heading-only
#'      cohort/group wording) splits title from population. Rightmost-first
#'      keeps a dash inside the title ("Summary - Part A - Safety
#'      Population") intact.
#'   3. With no dash population, a trailing annotation still splits off the
#'      title; a *FL flag is stored as the population filter, anything else
#'      is returned as extra_annot for programmer_annotations.
#'   4. Otherwise the whole tail is the title.
#'
#' @return list(title, population_text, population_annot, source_datasets,
#'   extra_annot). population_text keeps the annotation inline (same
#'   convention as a separate population paragraph); population_annot and
#'   extra_annot are quote-canonicalized.
#' @noRd
.decompose_heading_tail <- function(tail) {
  out <- list(title = "", population_text = "", population_annot = "",
              source_datasets = character(), extra_annot = "")
  tail <- trimws(tail %||% "")
  if (!nzchar(tail)) return(out)

  ## Bracket normalization lifts PROGRAMMING DATASETS directives (nested or
  ## not), unwraps instruction wrappers, and drops footnote markers /
  ## guidance prose before the title / population split runs.
  norm <- .unwrap_bracket_instructions(tail)
  out$source_datasets <- norm$source_datasets
  tail <- norm$text
  if (!nzchar(tail)) return(out)

  seps <- gregexpr(.HEADING_SEP_RE, tail, perl = TRUE)[[1]]
  if (seps[1] != -1) {
    sep_lengths <- attr(seps, "match.length")
    for (i in rev(seq_along(seps))) {
      rhs <- trimws(substr(tail, seps[i] + sep_lengths[i], nchar(tail)))
      if (!nzchar(rhs)) next
      if (.looks_like_population(rhs) ||
          grepl(.HEADING_POP_HINT_RE, rhs, perl = TRUE)) {
        out$title           <- .clean_heading_title(substr(tail, 1, seps[i] - 1L))
        out$population_text <- rhs
        annots <- regmatches(rhs, gregexpr(.ANNOTATION_PATTERN, rhs, perl = TRUE))[[1]]
        if (length(annots) > 0) {
          out$population_annot <- .canon_annotation(paste(annots, collapse = " and "))
        }
        return(out)
      }
    }
  }

  am <- regexpr(.ANNOTATION_PATTERN, tail, perl = TRUE)
  if (am > 0) {
    annot_end <- am + attr(am, "match.length") - 1L
    trailing  <- substr(tail, annot_end + 1L, nchar(tail))
    ## Only treat it as a heading-line annotation when nothing of substance
    ## follows it -- an annotation mid-title would mean this is not the
    ## simple "Title ANNOTATION" shape and is left for the body layers.
    if (!grepl("[[:alnum:]]", trailing)) {
      annot <- substr(tail, am, annot_end)
      out$title <- .clean_heading_title(
        sub("[\\s\\(\\[]+$", "", substr(tail, 1, am - 1L), perl = TRUE))
      if (grepl(.POPULATION_FLAG_RE, annot, perl = TRUE)) {
        out$population_annot <- .canon_annotation(annot)
      } else {
        out$extra_annot <- .canon_annotation(annot)
      }
      return(out)
    }
  }

  out$title <- .clean_heading_title(tail)
  out
}
## ---------------------------------------------------------------------------
## Section object constructor
## ---------------------------------------------------------------------------

#' Build an empty TLF section object with all fields present. Used by both
#' the body walker (when a heading paragraph starts a section) and the F2
#' page-header seeding block, so the two can never drift apart on the field
#' set. `title`, `population_text`, and `population_annot` are filled in
#' later as the walk sees them.
#' @noRd
.new_section <- function(tlf_number, tlf_type, title = "",
                         population_text = "", population_annot = "") {
  list(
    tlf_number             = tlf_number,
    tlf_type               = tlf_type,
    title                  = title,
    raw_heading            = "",
    population_text        = population_text,
    population_annot       = population_annot,
    footnotes              = character(),
    programmer_annotations = character(),
    source_datasets        = character(),
    col_headers            = character(),
    n_data_cols            = 0L,
    stub_rows              = list()
  )
}
## ---------------------------------------------------------------------------
## Convention-agnostic annotation binding (ADR 0003 Layer A)
## ---------------------------------------------------------------------------

#' Normalise a label for fuzzy matching: lowercase, punctuation and
#' indentation stripped, whitespace collapsed.
#' @noRd
.norm_label <- function(x) {
  x <- tolower(trimws(as.character(x %||% "")))
  x <- gsub("[[:punct:]]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}

## Substring containment only kicks in once the shorter string is at least
## this long. Otherwise a one- or two-character stub label like "n" or "%"
## substring-matches almost any longer phrase (e.g. "n" is inside "treatment
## columns"), binding an annotation to the wrong row.
.MIN_SUBSTRING_MATCH_CHARS <- 3L

#' Index of the stub row whose label matches `lhs_norm`: exact normalised
#' match first, then prefix (either direction), then substring containment.
#' Returns NA when nothing matches.
#' @noRd
.match_stub_label <- function(lhs_norm, labels_norm) {
  if (!nzchar(lhs_norm)) return(NA_integer_)

  hit <- which(labels_norm == lhs_norm)
  if (length(hit)) return(hit[1])

  hit <- which(nzchar(labels_norm) &
                 (startsWith(labels_norm, lhs_norm) |
                    startsWith(lhs_norm, labels_norm)))
  if (length(hit)) return(hit[1])

  contains_either <- function(l) {
    if (!nzchar(l)) return(FALSE)
    shorter <- min(nchar(l), nchar(lhs_norm))
    if (shorter < .MIN_SUBSTRING_MATCH_CHARS) return(FALSE)
    grepl(lhs_norm, l, fixed = TRUE) || grepl(l, lhs_norm, fixed = TRUE)
  }
  hit <- which(vapply(labels_norm, contains_either, logical(1)))
  if (length(hit)) return(hit[1])

  NA_integer_
}

## Left-side phrases that name the column (treatment) axis rather than a row.
.COLUMN_AXIS_PHRASES <- c(
  "treatment column", "treatment columns", "column", "columns",
  "treatment group", "treatment groups", "treatment arm", "treatment arms",
  "treatment"
)

#' Bind programmer annotations to their stub rows regardless of placement.
#'
#' Reads the `Label -> annotation` lines collected in
#' `sec$programmer_annotations` and binds each to the stub row whose label
#' fuzzy-matches the left side; sets that row's `annotation`,
#' `has_annot = TRUE`, `detection_method = "below_table_arrow"`. A left side
#' naming the column axis is stored as `sec$column_annotation`
#' ("DATASET.VARIABLE"); one matching the population line fills
#' `population_annot` when empty. In-cell detections always win: only rows
#' still `has_annot = FALSE` are bound. Unmatched lines stay in
#' `programmer_annotations` untouched (they still reach the validation
#' report). A multi-label left side ("Completed / Discontinued") splits on
#' "/" and, when the right side is `DS.VAR (v1 / v2)` with matching
#' cardinality, each row binds to its own `DS.VAR='v_i'`.
#'
#' @noRd
bind_annotations <- function(sec) {
  anns <- as.character(sec$programmer_annotations %||% character())
  if (length(anns) == 0) return(sec)

  labels_norm <- vapply(sec$stub_rows %||% list(),
                        function(r) .norm_label(r$label), character(1))
  pop_norm    <- .norm_label(sec$population_text)
  var_ref_re  <- paste0("\\b", .ADAM_DS, "\\.", .ADAM_VAR, "\\b")

  ## Compound lines carry several clauses separated by ";":
  ## "Subject -> ADAE.USUBJID ; Treatment -> ADAE.TRT01A".
  clauses <- unlist(lapply(anns, function(line)
    trimws(strsplit(line, ";", fixed = TRUE)[[1]])))
  clauses <- clauses[nzchar(clauses)]

  for (clause in clauses) {
    ## Split on the first arrow; fall back to the first colon ONLY when the
    ## left side matches something (a stub row / the column axis / the
    ## population line) -- a plain "Note: ..." must not bind.
    pos <- regexpr("->|\u2192", clause)
    if (pos > 0) {
      lhs <- substr(clause, 1, pos - 1)
      rhs <- substr(clause, pos + attr(pos, "match.length"), nchar(clause))
    } else {
      cpos <- regexpr(":", clause, fixed = TRUE)
      if (cpos <= 1) next
      lhs <- substr(clause, 1, cpos - 1)
      rhs <- substr(clause, cpos + 1, nchar(clause))
    }
    rhs <- trimws(rhs)
    if (!nzchar(rhs)) next
    lhs_full_norm <- .norm_label(lhs)
    if (!nzchar(lhs_full_norm)) next

    ## Left-side row candidates: the full label first (so "Start/Stop"
    ## matches its own row), then the "/"-split pieces (so
    ## "Completed / Discontinued" binds two rows).
    lhs_labels <- trimws(strsplit(lhs, "/", fixed = TRUE)[[1]])
    lhs_labels <- lhs_labels[nzchar(lhs_labels)]
    full_hit <- .match_stub_label(lhs_full_norm, labels_norm)
    if (!is.na(full_hit) || length(lhs_labels) == 0) {
      lhs_labels <- lhs
    }

    ## Multi-label left side + "DS.VAR (v1 / v2 ...)" right side with the
    ## same cardinality -> one value-filter annotation per label.
    per_values <- NULL
    pm <- regmatches(rhs, regexec(
      paste0("^\\s*(", .ADAM_DS, "\\.", .ADAM_VAR, ")\\s*\\(([^)]+)\\)\\s*$"),
      rhs, perl = TRUE))[[1]]
    if (length(pm) == 3 && length(lhs_labels) > 1) {
      vals <- trimws(strsplit(pm[3], "[/,]")[[1]])
      vals <- vals[nzchar(vals)]
      if (length(vals) == length(lhs_labels)) {
        per_values <- list(var = pm[2], vals = vals)
      }
    }

    ## 1. Stub rows claim the left side first.
    matched_any <- FALSE
    for (k in seq_along(lhs_labels)) {
      idx <- .match_stub_label(.norm_label(lhs_labels[k]), labels_norm)
      if (is.na(idx)) next
      matched_any <- TRUE
      if (isTRUE(sec$stub_rows[[idx]]$has_annot)) next   ## in-cell wins
      ann_k <- if (!is.null(per_values)) {
        paste0(per_values$var, "='", per_values$vals[k], "'")
      } else rhs
      sec$stub_rows[[idx]]$annotation           <- ann_k
      sec$stub_rows[[idx]]$has_annot            <- TRUE
      sec$stub_rows[[idx]]$detection_method     <- "below_table_arrow"
      sec$stub_rows[[idx]]$detection_confidence <- "high"
    }
    if (matched_any) next

    ## 2. Column-axis annotation: "Treatment columns -> ADSL.TRT01A",
    ## "Column N and treatment -> ...", or an exact column-header match.
    is_col_lhs <- lhs_full_norm %in% .COLUMN_AXIS_PHRASES ||
      grepl("\\b(column|columns|treatment)\\b", lhs_full_norm) ||
      any(vapply(sec$col_headers %||% character(), function(h) {
        hn <- .norm_label(h)
        nzchar(hn) && identical(hn, lhs_full_norm)
      }, logical(1)))
    if (is_col_lhs) {
      ref <- regmatches(rhs, regexpr(var_ref_re, rhs, perl = TRUE))
      if (length(ref) == 1 && is.null(sec$column_annotation)) {
        sec$column_annotation <- toupper(ref)
      }
      next
    }

    ## 3. Population annotation.
    if (nzchar(pop_norm) &&
        !is.na(.match_stub_label(lhs_full_norm, pop_norm)) &&
        !nzchar(sec$population_annot %||% "")) {
      sec$population_annot <- rhs
    }
  }

  sec
}

#' Resolve deferred listing-header detection now that the section is complete.
#'
#' By the time a section is pushed to `sections`, the entire body of the TLF
#' has been seen -- including the "Source: ..." line that provides the
#' primary source dataset. This is where we run `.detect_listing_header_annotation()`
#' on the saved header cells, attach `header_rows`, and append annotated
#' headers to `stub_rows` for uniform downstream processing.
#'
#' Resolve per-column header annotations into column-group definitions.
#'
#' A sponsor can define the column axis of a summary table entirely in the
#' header cells: "Cohort A (N=XX) ADSL.COHORTN=1", "Cohort B (N=XX)
#' ADSL.COHORTN=2", "Unknown Cohort (N=XX) ADSL.COHORTN is missing". When at
#' least two headers carry a parseable condition on the SAME variable, that
#' variable becomes the column axis and each condition becomes one display
#' column -- letting a merged/derived column (an "Unknown" bucket for missing
#' values) exist without any ADaM change. A header conditioned on a different
#' variable (a Total column's population flag) is left out of the groups; a
#' Total-labelled one instead flags `include_total_hint`.
#' @noRd
.resolve_table_column_groups <- function(sec, spec_lookup = NULL) {
  pending <- sec$.pending_column_annotations
  sec$.pending_column_annotations <- NULL
  if (is.null(pending) || !identical(sec$tlf_type, "TABLE")) return(sec)

  labels      <- pending$labels
  annotations <- pending$annotations
  has_annot   <- nzchar(annotations)
  if (!any(has_annot)) return(sec)

  ## One candidate per annotated grid column: which variable it references
  ## and whether its annotation parses into a real condition. Consecutive
  ## duplicates are one spanned cell repeated over its subcolumns.
  candidates <- list()
  for (i in which(has_annot)) {
    prev <- if (length(candidates) > 0) candidates[[length(candidates)]] else NULL
    if (!is.null(prev) && identical(prev$annotation, annotations[[i]]) &&
        identical(prev$label, labels[[i]])) {
      next
    }
    refs <- extract_annotation_vars(annotations[[i]])
    candidates[[length(candidates) + 1L]] <- list(
      label      = labels[[i]],
      annotation = annotations[[i]],
      variable   = if (length(refs) > 0) toupper(refs[[1]]) else "",
      condition  = parse_where_clause(annotations[[i]])
    )
  }
  if (length(candidates) == 0) return(sec)

  ## The axis variable is the DATASET.VARIABLE claimed by >= 2 headers whose
  ## annotations parsed into real conditions. Bare references (no operator)
  ## do not count as conditions -- a repeated bare DS.VAR keeps today's
  ## single column_annotation semantics below.
  conditioned <- Filter(function(cand) {
    nzchar(cand$variable) && !is.null(cand$condition)
  }, candidates)
  var_counts <- table(vapply(conditioned, function(cand) cand$variable,
                             character(1)))
  qualifying <- names(var_counts)[var_counts >= 2L]

  if (length(qualifying) == 0) {
    ## No per-level groups. A variable referenced bare by 2+ headers still
    ## names the column axis (existing convention for treatment headers).
    bare_vars <- vapply(candidates, function(cand) cand$variable, character(1))
    bare_tab  <- table(bare_vars[nzchar(bare_vars)])
    if (length(bare_tab) > 0 && max(bare_tab) >= 2L &&
        is.null(sec$column_annotation)) {
      sec$column_annotation <- names(bare_tab)[which.max(bare_tab)]
    }
    return(sec)
  }

  if (length(qualifying) > 1) {
    ## Most headers wins; first in document order breaks a tie.
    ordered <- names(sort(var_counts[qualifying], decreasing = TRUE))
    axis_var <- ordered[[1]]
    .diag_gap(
      stage = "parse_shell", severity = "WARN", input = INPUT_SHELL,
      problem = sprintf(
        "Column headers condition on several variables (%s); %s (most columns) was taken as the column axis.",
        paste(qualifying, collapse = ", "), axis_var),
      why = "A table has one column axis; the other conditioned headers cannot also define columns.",
      fix = "Annotate every group column with the same variable, or move the odd one out.",
      tlf_number = sec$tlf_number, location = sec$title %||% ""
    )
  } else {
    axis_var <- qualifying[[1]]
  }

  groups <- list()
  for (cand in conditioned) {
    if (!identical(cand$variable, axis_var)) next
    ## Display level label: the header text without its (N=XX) placeholder.
    level_label <- sub("\\s*\\(\\s*[Nn]\\s*=\\s*[^)]*\\)\\s*$", "", cand$label)
    level_label <- trimws(gsub("\\s+", " ", level_label))
    groups[[length(groups) + 1L]] <- list(
      label      = level_label,
      annotation = cand$annotation,
      order      = length(groups) + 1L
    )
  }
  ## Coverage check: a header that NAMES the axis variable but whose annotation
  ## did not parse into a condition is a display column silently lost from the
  ## column axis. Surface the shortfall so a shell whose (e.g.) missing-value
  ## header uses an unsupported form is caught rather than quietly narrowed.
  axis_headers <- Filter(function(cand) identical(cand$variable, axis_var),
                         candidates)
  dropped <- length(axis_headers) - length(groups)
  if (dropped > 0) {
    diag_add(
      stage = "parse_shell", severity = "WARN", input = INPUT_SHELL,
      problem = sprintf(
        "%d of %d %s column headers did not parse into a condition; %d column group(s) captured",
        dropped, length(axis_headers), axis_var, length(groups)),
      tlf_number = sec$tlf_number, location = sec$title %||% "",
      action = "Check those headers' annotations -- the dropped columns will be missing from the ARS grouping"
    )
  }

  if (length(groups) < 2) return(sec)

  ## Duplicate labels across different conditions would collide as factor
  ## levels downstream -- disambiguate and say so.
  group_labels <- vapply(groups, function(g) g$label, character(1))
  if (anyDuplicated(group_labels)) {
    fixed <- make.unique(group_labels, sep = " ")
    for (i in seq_along(groups)) groups[[i]]$label <- fixed[[i]]
    diag_add(
      stage = "parse_shell", severity = "WARN", input = INPUT_SHELL,
      problem = "Two column-group headers share the same display label; suffixes were added to keep them distinct",
      tlf_number = sec$tlf_number, location = sec$title %||% "",
      action = "Give each group column a distinct header label"
    )
  }

  dataset  <- sub("\\..*$", "", axis_var)
  variable <- sub("^.*\\.", "", axis_var)

  ## Hard spec gate is advisory here (the axis still parses without a spec):
  ## an out-of-spec axis variable is surfaced, not dropped.
  if (!is.null(spec_lookup) && length(spec_lookup) > 0 &&
      !axis_var %in% toupper(names(spec_lookup))) {
    diag_add(
      stage = "parse_shell", severity = "WARN", input = INPUT_SHELL,
      problem = sprintf("Column-axis variable %s is not in the ADaM spec", axis_var),
      tlf_number = sec$tlf_number, location = sec$title %||% "",
      action = "Verify the header annotations name a real spec variable"
    )
  }

  sec$column_groups <- list(
    variable = variable,
    dataset  = dataset,
    groups   = groups
  )
  ## In-cell header annotations claim the column axis; the arrow-line and
  ## supplement fallbacks both defer to an existing value.
  sec$column_annotation <- axis_var

  ## A leftover "Total (N=XX) ..." header (excluded from the groups because
  ## it filters a different variable, or none) marks the overall column.
  non_axis <- Filter(function(cand) !identical(cand$variable, axis_var),
                     candidates)
  total_labels <- c(
    vapply(non_axis, function(cand) cand$label, character(1)),
    labels[!has_annot]
  )
  if (any(grepl("(?i)^total\\b", total_labels, perl = TRUE))) {
    sec$include_total_hint <- TRUE
  }

  diag_add(
    stage = "parse_shell", severity = "INFO", input = INPUT_SHELL,
    problem = sprintf(
      "%d column-group definitions captured from header annotations for %s",
      length(groups), axis_var),
    tlf_number = sec$tlf_number, location = sec$title %||% "",
    action = "Each annotated header becomes one display column; verify labels and conditions in the ARS JSON"
  )

  sec
}

#' Resolve column groups from a hierarchical column tree.
#'
#' The tree-mode counterpart of `.resolve_table_column_groups()`. Each tree
#' level with conditioned nodes contributes one grouping axis; the level-1
#' axis fills the section's `column_groups`/`column_annotation` exactly as
#' the flat path would, so everything downstream that only understands one
#' axis keeps working, while the full hierarchy travels on `sec$column_tree`
#' (with `$levels` naming the per-level variables) for the path-aware
#' builder.
#'
#' @noRd
.resolve_column_groups_from_tree <- function(sec, spec_lookup = NULL) {
  tree  <- sec$column_tree
  nodes <- tree$nodes

  ## One axis variable per level, from the conditioned nodes at that level.
  ## Nodes of one level should agree on the variable; when they compete, the
  ## majority wins and the disagreement is surfaced.
  max_level <- max(vapply(nodes, function(n) n$level, integer(1)))
  levels <- list()
  for (lvl in seq_len(max_level)) {
    at_level <- Filter(function(n) {
      n$level == lvl && !is.null(n$condition) && nzchar(n$grouping_ref)
    }, nodes)
    if (length(at_level) == 0) next
    refs <- vapply(at_level, function(n) n$grouping_ref, character(1))
    tab  <- sort(table(refs), decreasing = TRUE)
    axis <- names(tab)[[1]]
    if (length(tab) > 1) {
      .diag_gap(
        stage = "parse_shell", severity = "WARN", input = INPUT_SHELL,
        problem = sprintf(
          "Level-%d column headers condition on several variables (%s); %s (most columns) was taken as that level's axis.",
          lvl, paste(names(tab), collapse = ", "), axis),
        why = "Each header level defines one grouping variable; a second variable at the same level cannot also define columns there.",
        fix = "Annotate every column at this header level with the same variable.",
        tlf_number = sec$tlf_number, location = sec$title %||% ""
      )
    }
    levels[[length(levels) + 1L]] <- list(
      level    = lvl,
      dataset  = sub("\\..*$", "", axis),
      variable = sub("^.*\\.", "", axis)
    )
    if (!is.null(spec_lookup) && length(spec_lookup) > 0 &&
        !axis %in% toupper(names(spec_lookup))) {
      diag_add(
        stage = "parse_shell", severity = "WARN", input = INPUT_SHELL,
        problem = sprintf("Column-axis variable %s (header level %d) is not in the ADaM spec", axis, lvl),
        tlf_number = sec$tlf_number, location = sec$title %||% "",
        action = "Verify the header annotations name a real spec variable"
      )
    }
  }
  sec$column_tree$levels <- levels

  ## Level-1 detail columns feed the classic single-axis fields so the
  ## existing builder keeps producing the outer grouping.
  if (length(levels) > 0) {
    axis_l1 <- paste0(levels[[1]]$dataset, ".", levels[[1]]$variable)
    l1_nodes <- Filter(function(n) {
      n$level == 1L && !is.null(n$condition) &&
        identical(n$grouping_ref, axis_l1) &&
        !n$node_type %in% c("grand_total")
    }, nodes)
    l1_nodes <- l1_nodes[order(vapply(l1_nodes, function(n) n$order, integer(1)))]
    if (length(l1_nodes) >= 2) {
      groups <- list()
      for (n in l1_nodes) {
        groups[[length(groups) + 1L]] <- list(
          label      = n$label,
          annotation = n$annotation,
          order      = length(groups) + 1L
        )
      }
      sec$column_groups <- list(
        variable = levels[[1]]$variable,
        dataset  = levels[[1]]$dataset,
        groups   = groups
      )
      sec$column_annotation <- axis_l1
    }
  }

  if (any(vapply(nodes, function(n) identical(n$node_type, "grand_total"), logical(1)))) {
    sec$include_total_hint <- TRUE
  }

  paths <- column_tree_paths(sec$column_tree)
  diag_add(
    stage = "parse_shell", severity = "INFO", input = INPUT_SHELL,
    problem = sprintf(
      "Hierarchical column header captured (%s): %d result-column paths across %d level(s)",
      tree$mode, length(paths), length(levels)),
    tlf_number = sec$tlf_number, location = sec$title %||% "",
    action = "Each declared path becomes one display column; verify the tree in the review stage"
  )

  problems <- .validate_column_tree(sec$column_tree)
  for (p in problems) {
    diag_add(
      stage = "parse_shell", severity = "WARN", input = INPUT_SHELL,
      problem = paste("Column tree:", p),
      tlf_number = sec$tlf_number, location = sec$title %||% "",
      action = "Check the header annotations; the tree must describe each display column once"
    )
  }

  sec
}

#' TRUE for a sub-column label that names a STATISTIC rather than a group:
#' "n", "(%)", "n (%)", "Mean (SD)", "Median", "95% CI", ... A spanning
#' header over these splits one column into statistic columns; it is not a
#' column hierarchy.
#' @noRd
.looks_like_stat_label <- function(x) {
  x <- trimws(as.character(x %||% ""))
  grepl(paste0("^(?i)(?:n|m|x+|\\(?%\\)?|n\\s*\\(\\s*%\\s*\\)|",
               "mean(?:\\s*\\(\\s*sd\\s*\\))?|sd|median|min|max|q1|q3|",
               "\\d+%\\s*ci|ci|se|range|total\\s*n)\\s*$"),
        x, perl = TRUE)
}

#' Report a header that LOOKS hierarchical (a spanning cell over sub-columns)
#' but produced no column tree, naming the precondition that failed. Without
#' this the fallback is silent: the shell shows a cohort spanning four
#' sub-columns and the output shows a row of undifferentiated columns, with
#' nothing to say why. A single-row / unspanned header is genuinely flat and
#' says nothing.
#' @noRd
.warn_flattened_header <- function(sec, tree, grid) {
  spanning <- Filter(function(cell) {
    !isTRUE(cell$vmerge_continue) && cell$col_start > 1L &&
      cell$col_end > cell$col_start
  }, grid)
  if (length(spanning) == 0) return(invisible(NULL))
  if (max(vapply(grid, function(cell) cell$row, integer(1))) < 2L) {
    return(invisible(NULL))
  }

  ## A spanning header over STATISTIC sub-columns ("Treatment A" over
  ## "n" / "(%)") is a display split, not a grouping hierarchy -- flat is
  ## the right answer there and warning about it would be noise.
  sub_labels <- vapply(
    Filter(function(cell) cell$row >= 2L && cell$col_start > 1L, grid),
    function(cell) trimws(cell$text %||% ""), character(1))
  sub_labels <- sub_labels[nzchar(sub_labels)]
  if (length(sub_labels) > 0 && all(.looks_like_stat_label(sub_labels))) {
    return(invisible(NULL))
  }

  nodes  <- tree$nodes %||% list()
  levels <- vapply(nodes, function(n) n$level, integer(1))
  has_c  <- vapply(nodes, function(n) !is.null(n$condition), logical(1))
  reason <- if (length(nodes) == 0) {
    "no header cell carried a usable label or annotation"
  } else if (!any(has_c & levels >= 2L)) {
    "none of the sub-columns carries a condition the parser could read"
  } else {
    "the spanning column above them declares neither a condition nor a grouping variable"
  }

  spans <- paste(vapply(spanning, function(cell) {
    trimws(cell$text %||% "")
  }, character(1)), collapse = ", ")

  .diag_gap(
    stage = "parse_shell", severity = "WARN", input = INPUT_SHELL,
    problem = sprintf(
      "Table %s has a spanning column header (%s) but arsbridge could not build the column hierarchy: %s.",
      sec$tlf_number %||% "?", spans, reason),
    why = "The spanned sub-columns are reported as one flat row of columns instead of a hierarchy, so their parent grouping is lost.",
    fix = "Annotate each sub-column with its own condition (e.g. [ADSL.COHGR1N=1]) and the spanning header with its grouping variable (e.g. [ADSL.COHORTN]).",
    tlf_number = sec$tlf_number, location = spans
  )
  invisible(NULL)
}

#' @noRd
.finalize_section <- function(sec, spec_lookup = NULL) {
  ## Working geometry only the continuation guard needed; not part of the
  ## finished section.
  sec$.table_n_cols <- NULL

  ## Build the column tree from the retained header geometry FIRST. When the
  ## tree shows a real conditioned hierarchy (parent cohort columns with
  ## conditioned child columns), the per-level resolution takes over;
  ## otherwise the existing single-axis path below runs untouched.
  grid <- sec$.pending_header_grid
  sec$.pending_header_grid <- NULL
  if (!is.null(grid) && identical(sec$tlf_type, "TABLE")) {
    tree <- .header_grid_to_tree(grid)
    if (tree$mode %in% c("NESTED", "ASYMMETRIC_NESTED")) {
      sec$column_tree <- tree
      sec$.pending_column_annotations <- NULL
      sec <- .resolve_column_groups_from_tree(sec, spec_lookup)
    } else {
      .warn_flattened_header(sec, tree, grid)
    }
  }
  ## Resolve per-column header filters into column groups BEFORE
  ## bind_annotations, so an in-cell header annotation claims the column
  ## axis first and the arrow-line fallback's is.null() guard defers to it.
  sec <- .resolve_table_column_groups(sec, spec_lookup)
  ## Bind below-table / arrow-form annotations to their rows first, so the
  ## per-section "no annotations detected" diagnostic and everything
  ## downstream see the bound rows (ADR 0003 Layer A).
  sec <- bind_annotations(sec)
  pending <- sec$.pending_header_cells
  if (length(pending %||% list()) == 0) return(sec)
  if (!identical(sec$tlf_type, "LISTING")) {
    sec$.pending_header_cells <- NULL
    return(sec)
  }

  ## Pull the first source dataset and strip any "(CONDITION)" suffix --
  ## e.g. "ADAE (TRTEMFL='Y')" -> "ADAE".
  source_ds <- if (length(sec$source_datasets) > 0) sec$source_datasets[1] else ""
  source_ds <- trimws(sub("\\s*\\(.*$", "", source_ds))
  if (!nzchar(source_ds)) {
    diag_add(
      stage = "parse_shell", severity = "WARN", input = INPUT_SHELL,
      problem = "Listing has no source dataset; header variables defaulted to ADSL",
      tlf_number = sec$tlf_number,
      location = sec$title %||% "",
      action = "Defaulted dataset prefix to ADSL -- verify each listing variable's dataset"
    )
    source_ds <- "ADSL"
  }

  hdr_rows <- vector("list", length(pending))
  for (j in seq_along(pending)) {
    p <- pending[[j]]
    d <- .detect_listing_header_annotation(p$text, p$runs, source_ds,
                                           spec_lookup = spec_lookup)
    hdr_rows[[j]] <- list(
      label                = d$label,
      annotation           = d$annotation,
      has_annot            = nzchar(d$annotation),
      detection_method     = d$method,
      detection_confidence = d$confidence,
      raw_text             = as.character(p$text %||% "")
    )
    ## A multi-line header cell that yielded no variable token usually means
    ## the variable-name convention differs from "ALL-CAPS on line 2+".
    if (!nzchar(d$annotation) &&
        length(strsplit(as.character(p$text %||% ""), "\n", fixed = TRUE)[[1]]) >= 2) {
      diag_add(
        stage = "parse_shell", severity = "INFO", input = INPUT_SHELL,
        problem = "Multi-line listing header cell yielded no variable annotation",
        tlf_number = sec$tlf_number,
        location = substr(gsub("\n", " | ", p$text), 1, 80),
        action = "Header skipped -- variable may use a convention the token extractor does not recognise"
      )
    }
  }
  ## Listing shells may annotate their columns as below-table arrow lines
  ## ("Subject -> ADAE.USUBJID ; Treatment -> ADAE.TRT01A") instead of inside
  ## the header cells -- bind those against the header rows (ADR 0003
  ## Layer A) before deciding which headers carry annotations.
  if (length(sec$programmer_annotations %||% character()) > 0) {
    tmp <- sec
    tmp$stub_rows <- hdr_rows
    tmp <- bind_annotations(tmp)
    hdr_rows <- tmp$stub_rows
    if (is.null(sec$column_annotation)) {
      sec$column_annotation <- tmp$column_annotation
    }
  }
  sec$header_rows <- hdr_rows

  ## The DISPLAY column labels must be the annotation-stripped header text.
  ## col_headers was captured raw at parse time (label plus the bracketed
  ## annotation), and flowed straight into the ARS display columns and from
  ## there into the rendered listing's header -- production output showing
  ## "Subject[ADAE.USUBJID]" instead of "Subject". The stripped labels the
  ## header detection just produced are the authoritative display text.
  stripped_labels <- vapply(hdr_rows, function(r) {
    trimws(as.character(r$label %||% ""))
  }, character(1))
  if (any(nzchar(stripped_labels))) {
    sec$col_headers <- stripped_labels[nzchar(stripped_labels)]
    sec$n_data_cols <- max(0L, length(sec$col_headers) - 1L)
  }

  ## Append annotated headers to stub_rows so validate/enrich/build see
  ## them with no special-case branch.
  annotated_headers <- Filter(function(r) isTRUE(r$has_annot), hdr_rows)
  sec$stub_rows <- c(sec$stub_rows %||% list(), annotated_headers)

  sec$.pending_header_cells <- NULL
  sec
}
## ---------------------------------------------------------------------------
## Shared text normalization and run-metadata predicates
##
## The predicates below read the per-run list of seam 1, never a node,
## so the docx run reader and the xlsx cell reader both feed them.
## ---------------------------------------------------------------------------

#' Normalize Word-flavoured Unicode before any regex sees the text: strip
#' zero-width characters, turn non-breaking space variants into plain
#' spaces, and straighten smart quotes (an annotation typed in Word usually
#' reads ADSL.SCRNFL="Y" with curly quotes, which the annotation grammar
#' would otherwise miss). En/em dashes are deliberately left alone -- they
#' are meaningful display characters in titles, and the heading decomposer
#' treats them explicitly as title/population separators.
#' @noRd
.normalize_shell_text <- function(x) {
  x <- gsub("[\u200B\uFEFF]", "", x, perl = TRUE)
  chartr("\u00A0\u2007\u202F\u2018\u2019\u201C\u201D",
         "   ''\"\"",
         x)
}
#' TRUE when `x` leaves a "[" unclosed -- the text is in the middle of a
#' bracketed annotation, so whatever follows continues it.
#' @noRd
.unclosed_bracket <- function(x) {
  chars <- strsplit(x, "", fixed = TRUE)[[1]]
  sum(chars == "[") > sum(chars == "]")
}
#' TRUE when EVERY run carrying text in this node is struck through -- the
#' shell author deleted the row but left it visible for review. A partially
#' struck cell (one value crossed out and retyped beside it) is a live row.
#' @noRd
.all_text_struck <- function(runs_meta) {
  texted <- Filter(function(m) nzchar(trimws(m$text %||% "")), runs_meta)
  if (length(texted) == 0) return(FALSE)
  all(vapply(texted, function(m) isTRUE(m$strike), logical(1)))
}
#' TRUE for a run that carries an annotation-style visual marker: a font
#' colour that isn't grey/black/white/automatic, or a highlight that isn't
#' "none"/"black". Shared by every place that decides whether a run counts
#' as "annotated" -- stub cells, the population line, and listing headers.
#' @noRd
.is_annotation_styled_run <- function(m) {
  if (!nzchar(m$text %||% "")) return(FALSE)
  has_colour    <- !is.na(m$color_hex) && !m$color_hex %in% c(.GREY_HEX, .BLACK_HEXES)
  has_highlight <- !is.na(m$highlight) && !m$highlight %in% c("none", "black")
  has_colour || has_highlight
}
## ---------------------------------------------------------------------------
## Population / footnote line grammar
## ---------------------------------------------------------------------------

#' TRUE if a paragraph looks like the population / analysis-set statement.
#'
#' Two ways to qualify:
#'   1. The text mentions the population lexicon ("Safety Population",
#'      "Analysis Set", "ITT", ...).
#'   2. The text has no population wording but carries a population-FLAG
#'      annotation ("(ADSL.SAFFL='Y')"): some shells annotate the population
#'      line and nothing else about it reads as prose.
#'
#' Case 2 is deliberately restricted to flag variables. A general "has any
#' annotation" test used to live here, but it also matched a treatment-column
#' mapping ("Treatment columns -> ADSL.TRT01A") placed right after the title;
#' that line would then be misfiled as the population and never reach
#' bind_annotations() as the column-axis grouping.
#' @noRd
.looks_like_population <- function(stripped) {
  if (grepl(.POPULATION_LEXICON_RE, stripped, perl = TRUE)) return(TRUE)
  grepl(.POPULATION_FLAG_RE, stripped, perl = TRUE)
}

## Leading markers that identify a footnote rather than a title continuation:
## a bracket/asterisk/digit at the start, or an opening word like "Note:" /
## "Abbreviations:". A real second title line ("by ATC Class") starts with a
## plain word and does not match.
.FOOTNOTE_LEAD_RE <- "^\\s*(?:[\\[\\(*\u2020\u2021]|\\d|note\\b|footnote\\b|abbreviations?\\b|key\\b)"

#' TRUE if a paragraph begins like a footnote (see .FOOTNOTE_LEAD_RE). Used
#' so the NEED_POP title-join step never glues a pre-table footnote onto the
#' title.
#' @noRd
.looks_like_footnote_lead <- function(stripped) {
  grepl(.FOOTNOTE_LEAD_RE, stripped, ignore.case = TRUE, perl = TRUE)
}
## ---------------------------------------------------------------------------
## Annotation detection -- 4-layer (Layer 4 deferred to LLM)
## ---------------------------------------------------------------------------

#' Apply the detection hierarchy to one stub cell.
#' Returns list(label, annotation, method, confidence).
#' @noRd
.detect_annotation <- function(cell_text, runs_meta) {
  ## Real-world bracket conventions first (nested directives, instruction
  ## wrappers, footnote markers) -- every layer below sees normalized text.
  cell_text <- .unwrap_bracket_instructions(cell_text)$text

  ## A styled candidate normalizes the same way, then sheds one enclosing
  ## bracket pair (real shells paint the WHOLE "[...]" red; the brackets
  ## are display syntax, not annotation content).
  norm_candidate <- function(candidate) {
    candidate <- .unwrap_bracket_instructions(candidate)$text
    sub("^\\[(.*)\\]$", "\\1", candidate, perl = TRUE)
  }

  ## Layer 1: coloured or highlighted runs that additionally match the
  ## ADaM pattern.
  coloured_runs <- Filter(.is_annotation_styled_run, runs_meta)
  if (length(coloured_runs) > 0) {
    candidate <- norm_candidate(trimws(paste(
      vapply(coloured_runs, function(m) m$text, character(1)),
      collapse = "")))
    if (grepl(.ANNOTATION_PATTERN, candidate, perl = TRUE)) {
      label <- trimws(.strip_annotation_from_text(cell_text, candidate))
      return(list(label = label, annotation = .canon_annotation(candidate),
                  method = "colour", confidence = "high"))
    }
  }

  ## Layer 2: formatted runs (bold/italic/underline) matching ADaM pattern.
  formatted_runs <- Filter(function(m) {
    (isTRUE(m$bold) || isTRUE(m$italic) || isTRUE(m$underline)) &&
      nzchar(m$text)
  }, runs_meta)
  if (length(formatted_runs) > 0) {
    candidate <- norm_candidate(trimws(paste(
      vapply(formatted_runs, function(m) m$text, character(1)),
      collapse = "")))
    if (grepl(.ANNOTATION_PATTERN, candidate, perl = TRUE)) {
      label <- trimws(.strip_annotation_from_text(cell_text, candidate))
      return(list(label = label, annotation = .canon_annotation(candidate),
                  method = "format", confidence = "medium"))
    }
  }

  ## Layer 3: plain-text ADaM regex match anywhere in the cell.
  pieces <- split_label_annotation(cell_text)
  if (nzchar(pieces$annotation)) {
    confidence <- if (.is_full_dataset_dot_variable(pieces$annotation)) "high" else "medium"
    return(list(label = pieces$label, annotation = pieces$annotation,
                method = "pattern", confidence = confidence))
  }

  ## Layer 4 fallback would happen later (LLM); here, no annotation.
  list(label = trimws(cell_text), annotation = "",
       method = NA_character_, confidence = NA_character_)
}

#' Retry annotation detection on a multi-paragraph cell with the BARE join.
#'
#' `.cell_text()` has to pick ONE way to join a cell's paragraphs, and its
#' heuristic (space, unless a bracket is open or the text ends mid-
#' reference) can guess wrong when a wrapped annotation gives no such
#' signal -- the space it inserts then splits the reference and the plain-
#' text layer misses it. This retry gives the annotation consumer its own
#' view: the paragraphs joined with NOTHING. If the pattern matches there,
#' the annotation is read from the bare join, and the label is rebuilt from
#' the paragraph list joined with SPACES (a bare-joined label would fuse
#' words: "dataextraction"). Each consumer gets the join it needs.
#'
#' Returns a detection list like `.detect_annotation()`, or NULL when the
#' bare join does not match either.
#' @noRd
.detect_annotation_wrapped <- function(paragraphs) {
  if (length(paragraphs) < 2L) return(NULL)
  bare <- paste0(paragraphs, collapse = "")
  m <- regexpr(.ANNOTATION_PATTERN, bare, perl = TRUE)
  if (m == -1) return(NULL)
  pieces <- split_label_annotation(bare)
  if (!nzchar(pieces$annotation)) return(NULL)

  ## The label is everything BEFORE the match, taken from the paragraph
  ## list so paragraph breaks come back as spaces. `paragraphs` are already
  ## normalized (see .cell_paragraphs()), so offsets into `bare` line up.
  start <- as.integer(m)
  offsets <- cumsum(c(0L, nchar(paragraphs)))
  label_parts <- character(0)
  for (i in seq_along(paragraphs)) {
    para_start <- offsets[[i]] + 1L
    if (para_start >= start) break
    take <- min(nchar(paragraphs[[i]]), start - para_start)
    piece <- trimws(substr(paragraphs[[i]], 1L, take))
    if (nzchar(piece)) label_parts <- c(label_parts, piece)
  }
  label <- trimws(sub("\\s*[\\[\\(]?\\s*$", "",
                      paste(label_parts, collapse = " "), perl = TRUE))

  list(label = label, annotation = pieces$annotation,
       method = "pattern_wrapped", confidence = "medium")
}
#' Remove an annotation substring from a cell text and return the remaining
#' label portion (used when Layer 1 / Layer 2 detected the annotation from
#' formatting alone).
#' @noRd
.strip_annotation_from_text <- function(cell_text, annotation) {
  pos <- regexpr(annotation, cell_text, fixed = TRUE)
  if (pos < 1) return(cell_text)
  before <- substr(cell_text, 1, pos - 1)
  gsub("\\s*[\\[\\(]?\\s*$", "", before, perl = TRUE)
}

#' Layer 3 split helper. Unexported. Returns list(label, annotation).
#'
#' Identifies the first ADaM-pattern match in `cell_text`, splits text into
#' a "label" (everything before) and "annotation" (the match and any
#' compound continuation). Handles bracket-enclosed forms by stripping the
#' opening bracket from the label and the closing bracket from the
#' annotation tail.
#'
#' @noRd
split_label_annotation <- function(cell_text) {
  cell_text <- as.character(cell_text %||% "")
  if (!nzchar(trimws(cell_text))) {
    return(list(label = "", annotation = ""))
  }

  m <- regexpr(.ANNOTATION_PATTERN, cell_text, perl = TRUE)
  if (m == -1) {
    return(list(label = trimws(cell_text), annotation = ""))
  }

  start <- as.integer(m)
  before <- substr(cell_text, 1, start - 1L)
  label <- trimws(sub("\\s*[\\[\\(]?\\s*$", "", before, perl = TRUE))

  annotation <- substr(cell_text, start, nchar(cell_text))
  ## Strip the closing half of a bracket-enclosed annotation ("[ADSL.AGE]",
  ## "(ADSL.SAFFL='Y')") -- but a trailing ")" that closes a parenthesis
  ## OPENED INSIDE the annotation (an IN list: "ADSL.RACE IN ('A','B')")
  ## belongs to the annotation and must stay.
  annotation <- sub("\\s*\\]\\s*$", "", annotation, perl = TRUE)
  n_open  <- lengths(regmatches(annotation, gregexpr("(", annotation, fixed = TRUE)))
  n_close <- lengths(regmatches(annotation, gregexpr(")", annotation, fixed = TRUE)))
  if (n_close > n_open) {
    annotation <- sub("\\s*\\)\\s*$", "", annotation, perl = TRUE)
  }
  annotation <- .canon_annotation(trimws(annotation))

  list(label = label, annotation = annotation)
}

#' TRUE if the annotation is exactly a DATASET.VARIABLE form (Layer 3a -- HIGH
#' confidence). FALSE for partial / abbreviated forms (Layer 3b -- MEDIUM).
#' @noRd
.is_full_dataset_dot_variable <- function(annotation) {
  pat <- paste0("^(", .ADAM_DS, "\\.", .ADAM_VAR, ")(\\b|=|\\s|$)")
  grepl(pat, annotation, perl = TRUE)
}
## ---------------------------------------------------------------------------
## Listing column-header annotation detection
##
## Listing shells annotate variables in the COLUMN HEADERS rather than
## the stub column, with a different convention than table stubs:
##
##   Cell text:     "Subject ID\nUSUBJID"
##                  display label on line 1, variable name on line 2+
##
##   Multi-var:     "AE PT (Verbatim)\nAEDECOD (AETERM)"
##                  -> primary AEDECOD, supplementary AETERM
##
## The variable lacks a DATASET prefix. We resolve the dataset from:
##   1. .UNIVERSAL_ADSL_VARS lookup (USUBJID, ARM, AGE, ... -> ADSL)
##   2. The TLF's first source dataset from "Source: ..." (ADAE, ADLB, etc.)
##
## Detection runs Layer 1 (coloured runs in the cell) first, falling back
## to a Layer 3 scan of the post-display-line text. Validation against the
## ADaM spec catches any wrong-dataset guess as a WARN finding, so the
## heuristic dataset assignment is a "draft" the human reviewer confirms.
## ---------------------------------------------------------------------------

#' Detect a listing column-header annotation.
#'
#' @param cell_text    The full concatenated text of the header cell.
#' @param runs_meta    Per-run metadata for the cell (from `.runs_metadata`).
#' @param source_ds    The TLF's primary source dataset (used as the default
#'   prefix for variables not in `.UNIVERSAL_ADSL_VARS`).
#' @param spec_lookup  Optional `"DATASET.VARIABLE"`-keyed spec lookup. When
#'   present, candidate tokens are matched case-insensitively against the
#'   spec's variable names (catching mixed-case headers like "AeDecod") and
#'   the dataset is resolved from the spec rather than guessed.
#'
#' @return list(label, annotation, method, confidence).
#'
#' @noRd
.detect_listing_header_annotation <- function(cell_text, runs_meta, source_ds,
                                              spec_lookup = NULL) {
  cell_text <- as.character(cell_text %||% "")
  no_match <- list(label = trimws(cell_text), annotation = "",
                   method = NA_character_, confidence = NA_character_)
  if (!nzchar(trimws(cell_text))) return(no_match)

  ## Layer 1: coloured (red) or highlighted runs in the header carry the
  ## annotation directly.
  coloured <- Filter(.is_annotation_styled_run, runs_meta)
  candidate_text <- NULL
  source_method  <- NULL
  if (length(coloured) > 0) {
    candidate_text <- paste(vapply(coloured, function(m) m$text, character(1)),
                            collapse = "")
    source_method <- "listing_header_colour"
  }

  ## Layer 3: line-2 fallback. Split on \n and treat first line as display,
  ## remainder as the variable-name region.
  lines <- strsplit(cell_text, "\n", fixed = TRUE)[[1]]
  lines <- trimws(lines)
  if (is.null(candidate_text)) {
    if (length(lines) < 2) return(no_match)
    candidate_text <- paste(lines[-1], collapse = " ")
    source_method <- "listing_header_pattern"
  }
  if (!nzchar(trimws(candidate_text))) return(no_match)

  ## A fully qualified DATASET.VAR reference in the annotation region is
  ## authoritative and must be taken whole -- tokenising "ADAE.TRT01A"
  ## reads the dataset name as a variable and fabricates ADAE.ADAE.
  dotted_refs <- unique(toupper(extract_annotation_vars(candidate_text)))
  if (length(dotted_refs) > 0) {
    qualified <- dotted_refs
    annotation <- if (length(qualified) == 1) {
      qualified
    } else {
      sprintf("%s (%s)", qualified[1], paste(qualified[-1], collapse = ", "))
    }
    label <- if (identical(source_method, "listing_header_colour")) {
      cleaned <- .strip_annotation_from_text(cell_text, candidate_text)
      cleaned <- trimws(strsplit(cleaned, "\n", fixed = TRUE)[[1]][1] %||% "")
      if (nzchar(cleaned)) cleaned else trimws(lines[1])
    } else if (length(lines) > 0) {
      lines[1]
    } else {
      trimws(cell_text)
    }
    return(list(
      label = label, annotation = annotation,
      method = source_method,
      confidence = if (identical(source_method, "listing_header_colour"))
                     "high" else "medium"
    ))
  }

  ## Candidate variable tokens. With a spec available, scan
  ## case-insensitively and keep only tokens that are real spec variables
  ## (catches mixed-case conventions like "AeDecod"; no blocklist needed --
  ## English noise simply fails the spec membership test). Without a spec,
  ## fall back to the ALL-CAPS heuristic + blocklist.
  spec_keys <- if (!is.null(spec_lookup)) toupper(names(spec_lookup)) else character()
  spec_vars <- unique(sub("^.*\\.", "", spec_keys))
  if (length(spec_vars) > 0) {
    tokens <- regmatches(
      candidate_text,
      gregexpr("\\b[A-Za-z][A-Za-z0-9]{2,7}\\b", candidate_text, perl = TRUE)
    )[[1]]
    tokens <- unique(toupper(tokens))
    tokens <- tokens[tokens %in% spec_vars]
  } else {
    tokens <- regmatches(
      candidate_text,
      gregexpr("\\b[A-Z][A-Z0-9]{2,7}\\b", candidate_text, perl = TRUE)
    )[[1]]
    tokens <- unique(tokens[!tokens %in% .HEADER_TOKEN_BLOCKLIST])
  }
  if (length(tokens) == 0) return(no_match)

  resolve_ds <- function(v) {
    if (length(spec_keys) > 0) {
      ## Spec-grounded resolution: source dataset first, ADSL second, then
      ## whichever spec dataset carries the variable.
      ds_hits <- unique(sub("\\..*$", "", spec_keys[sub("^.*\\.", "", spec_keys) == v]))
      if (toupper(source_ds) %in% ds_hits) return(toupper(source_ds))
      if ("ADSL" %in% ds_hits) return("ADSL")
      if (length(ds_hits) > 0) return(ds_hits[1])
    }
    if (v %in% .UNIVERSAL_ADSL_VARS) "ADSL" else source_ds
  }
  qualified <- vapply(tokens, function(v) paste0(resolve_ds(v), ".", v),
                      character(1), USE.NAMES = FALSE)
  annotation <- if (length(qualified) == 1) {
    qualified
  } else {
    sprintf("%s (%s)", qualified[1], paste(qualified[-1], collapse = ", "))
  }

  ## The display label. When the COLOUR layer found the annotation, it can
  ## sit inline in the first line ("Treatment[ADAE.TRT01A]" with the
  ## bracket run in red) -- line 1 alone is not clean, so cut the detected
  ## text out of the cell first. Otherwise line 1 is the display text and
  ## the annotation lived on the later lines.
  label <- if (identical(source_method, "listing_header_colour")) {
    cleaned <- .strip_annotation_from_text(cell_text, candidate_text)
    cleaned <- trimws(strsplit(cleaned, "\n", fixed = TRUE)[[1]][1] %||% "")
    if (nzchar(cleaned)) cleaned else trimws(lines[1])
  } else if (length(lines) > 0) {
    lines[1]
  } else {
    trimws(cell_text)
  }

  list(label = label, annotation = annotation,
       method = source_method,
       confidence = if (identical(source_method, "listing_header_colour"))
                      "high" else "medium")
}

## Tokens that look like ADaM variable names but are common English in
## listing headers -- never treat them as variables.
.HEADER_TOKEN_BLOCKLIST <- c(
  "ID", "PT", "SOC", "AE", "NA",
  "WP", "NRS", "EASI", "DLQI", "POEM",
  "CI", "SD", "SE", "PY"
)
## ---------------------------------------------------------------------------
## Source dataset list
## ---------------------------------------------------------------------------

.split_source_list <- function(raw) {
  raw   <- trimws(sub("\\.\\s*$", "", raw))
  parts <- strsplit(raw, "[,;]+")[[1]]
  parts <- toupper(trimws(parts))
  parts[nzchar(parts)]
}
