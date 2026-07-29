## tools/shell_structure_digest.R
## ---------------------------------------------------------------------------
## PRIVACY-SAFE STRUCTURE EXTRACTOR for annotated TLF shells.
##
## Purpose: let someone debug arsbridge's parsing of a shell they are NOT
## allowed to share. It reads a .docx and writes a JSON digest describing only
## the SHAPE of the document -- table geometry, merged cells, paragraph and run
## counts, formatting flags, and character-class silhouettes of the text. No
## label text, no annotation values, no titles, no study identifiers ever reach
## the output.
##
## Dependencies: xml2 and jsonlite only. arsbridge does NOT need to be
## installed, so this runs on a locked-down machine.
##
## Usage (from any directory):
##
##   source("shell_structure_digest.R")
##   digest_shell("MyStudy_Shells.docx", "digest.json")
##
## Then OPEN digest.json, read it yourself, and only send it on if you are
## satisfied. It is plain text and short enough to skim.
##
## What is deliberately NOT captured: any literal character of the document's
## text. Letters become "a"/"A", digits become "9". "SOC#1" is recorded as
## "AAA#9"; "<Preferred Term>" as "<Aaaaaaaaa Aaaa>". That is enough to tell
## which authoring dialect a shell uses and nothing about the study.

suppressPackageStartupMessages({
  library(xml2)
  library(jsonlite)
})

## --- redaction ---------------------------------------------------------------

## Character-class silhouette: keeps length, case pattern, digits and
## punctuation; destroys the words. This is what makes token dialects
## ("SOC#1", "<Reason #2>", "PT#n") recognisable without revealing content.
silhouette <- function(x, max_chars = 60) {
  x <- substr(as.character(x %||% ""), 1, max_chars)
  x <- gsub("[A-Z]", "A", x)
  x <- gsub("[a-z]", "a", x)
  gsub("[0-9]", "9", x)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

## Structural skeleton of an annotation: the GRAMMAR is kept (brackets and
## their nesting, operators, keywords, instruction-wrapper phrases) while every
## name and value is replaced by a stable token. Two occurrences of the same
## variable get the same token within one document, so relationships between
## rows and columns stay visible.
##
## "[ADSL.COHORTN=1]"                      -> "[V1=9]"
## "[ADSL.SCRNFL=\"Y\" [PROGRAMMING ...]]" -> "[V2=<val> [PROGRAMMING DATASETS USED: <ds>]]"
## "SOC#1 [ADMH.MHBODSYS]"                 -> "[V3]"
## The ONLY words allowed to survive verbatim. Everything else -- including
## every label word -- is replaced by "~". A whitelist is used rather than a
## blacklist so no unforeseen word can leak: if it is not an operator, it is
## erased.
.KEEP_WORDS <- c("WHERE", "AND", "OR", "NOT", "IN", "NOTIN", "NE", "EQ",
                 "GT", "GE", "LT", "LE", "IS", "NA", "NULL", "MISSING", "VAL")

annotation_skeleton <- function(x, vars) {
  s <- as.character(x %||% "")
  if (!nzchar(trimws(s))) return("")

  ## Quoted values first, so their contents can never survive a later rule.
  s <- gsub("'[^']*'", "<val>", s)
  s <- gsub('"[^"]*"', "<val>", s)

  ## Dataset.VARIABLE references -> stable V1, V2, ... tokens, shared across
  ## the whole document so relationships between rows and columns stay visible.
  refs <- regmatches(
    s, gregexpr("\\b[A-Za-z][A-Za-z0-9]*\\.[A-Za-z][A-Za-z0-9_]*", s,
                perl = TRUE))[[1]]
  for (ref in unique(refs)) {
    key <- toupper(ref)
    if (is.null(vars[[key]])) {
      assign(key, paste0("V", length(ls(vars)) + 1L), envir = vars)
    }
    s <- gsub(ref, get(key, envir = vars), s, fixed = TRUE)
  }

  ## Bare numbers carry meaning (=1, IN (1,2)) but no content: keep as 9.
  s <- gsub("\\b[0-9]+(?:\\.[0-9]+)?\\b", "9", s, perl = TRUE)

  ## Erase every remaining word. Operators survive ONLY inside brackets:
  ## a label sits outside them, and a label word that happens to spell an
  ## operator ("Not coded", "In progress") must not slip through. "~" means
  ## "a word was here".
  keep_re <- paste0("(?i)^(?:", paste(.KEEP_WORDS, collapse = "|"),
                    "|V[0-9]+)$")
  depth <- 0L
  chars <- strsplit(s, "", fixed = TRUE)[[1]]
  in_bracket <- vapply(chars, function(ch) {
    if (ch == "[") depth <<- depth + 1L
    inside <- depth > 0L
    if (ch == "]") depth <<- max(0L, depth - 1L)
    inside
  }, logical(1), USE.NAMES = FALSE)

  out <- character(0)
  i <- 1L
  n <- length(chars)
  while (i <= n) {
    if (grepl("[A-Za-z]", chars[i])) {
      j <- i
      while (j < n && grepl("[A-Za-z0-9_]", chars[j + 1L])) j <- j + 1L
      word <- paste(chars[i:j], collapse = "")
      ## V1/V2/... and VAL are placeholders THIS function already
      ## substituted -- they hold no document text, so they survive
      ## anywhere. Real operator words survive only inside brackets, where
      ## an annotation lives; outside, a label word that happens to spell
      ## one ("Not coded") must still be erased.
      out <- c(out, if (grepl("^(?i)(?:V[0-9]+|VAL)$", word, perl = TRUE)) {
        toupper(word)
      } else if (all(in_bracket[i:j]) && grepl(keep_re, word, perl = TRUE)) {
        toupper(word)
      } else {
        "~"
      })
      i <- j + 1L
    } else {
      out <- c(out, chars[i])
      i <- i + 1L
    }
  }
  s <- paste(out, collapse = "")
  s <- gsub("(?:~[ \t]*)+~", "~", s, perl = TRUE)
  s <- gsub("[ \t]{2,}", " ", s)

  substr(trimws(s), 1, 160)
}

## Which known authoring conventions a piece of text uses. Booleans only --
## the patterns are arsbridge's own vocabulary, never the study's.
annotation_flags <- function(x) {
  s <- as.character(x %||% "")
  f <- function(re) grepl(re, s, perl = TRUE)
  list(
    programming_datasets = f("(?i)PROGRAMMING\\s+DATASETS?\\s+USED"),
    condition_wrapper    = f("(?i)\\bcondition\\s*:"),
    use_row_variant      = f("(?i)for\\s+this\\s+displayed\\s+row"),
    count_instruction    = f("(?i)count\\s+distinct\\s+subjects|(?i)unique\\s+subjects?"),
    repeat_directive     = f("(?i)\\bRepeat\\s+(?:the|for)\\b"),
    once_per_subject     = f("(?i)once\\s*/\\s*subject"),
    footnote_marker_only = f("^\\s*\\[[A-Za-z0-9]{1,2}\\]\\s*$"),
    nested_bracket       = f("\\[[^]\\[]*\\["),
    angle_token          = f("^\\s*<[^>]+>\\s*$"),
    hash_token           = f("^\\s*[A-Za-z][A-Za-z ]{0,30}#\\s*(?:[0-9]+|[nNxX])\\s*$")
  )
}

## --- OOXML walking -----------------------------------------------------------

.local <- function(node) sub("^.*:", "", xml_name(node))
.find <- function(node, name) {
  xml_find_all(node, sprintf(".//*[local-name()='%s']", name))
}
.first <- function(node, path) xml_find_first(node, path)

run_flags <- function(run) {
  ## Word writes a boolean run property with NO val attribute when it is ON
  ## (<w:strike/>), and val="false"/"0" to switch it off. xml_attr returns NA
  ## for the missing attribute, so NA here means ON, not absent -- the node's
  ## presence is what matters.
  pr <- function(tag) {
    n <- .first(run, sprintf("./*[local-name()='rPr']/*[local-name()='%s']", tag))
    if (inherits(n, "xml_missing")) return(NA_character_)
    val <- xml_attr(n, "val")
    if (is.na(val)) "on" else tolower(val)
  }
  on <- function(tag) {
    v <- pr(tag)
    !is.na(v) && !v %in% c("false", "0", "off", "none")
  }
  colour <- pr("color")
  list(
    ## The colour VALUE is document formatting, not study content, and it is
    ## what drives arsbridge's Layer-1 annotation detection -- keep it.
    color      = if (is.na(colour)) NA_character_ else toupper(colour),
    highlight  = pr("highlight"),
    bold       = on("b"),
    italic     = on("i"),
    underline  = on("u"),
    strike     = on("strike"),
    dstrike    = on("dstrike"),
    ## A soft line break INSIDE a run is the difference between a label that
    ## wrapped visually and one authored as two paragraphs -- the exact
    ## ambiguity that cannot be seen in a screenshot.
    breaks     = length(.find(run, "br")),
    n_chars    = nchar(paste(xml_text(.find(run, "t")), collapse = ""))
  )
}

paragraph_digest <- function(para, vars) {
  runs <- xml_find_all(para, "./*[local-name()='r']")
  text <- paste(xml_text(.find(para, "t")), collapse = "")
  list(
    n_runs     = length(runs),
    n_chars    = nchar(text),
    silhouette = silhouette(text),
    skeleton   = annotation_skeleton(text, vars),
    conventions = Filter(isTRUE, annotation_flags(text)),
    runs       = lapply(runs, run_flags)
  )
}

cell_digest <- function(cell, vars) {
  span <- .first(cell, "./*[local-name()='tcPr']/*[local-name()='gridSpan']")
  vm   <- .first(cell, "./*[local-name()='tcPr']/*[local-name()='vMerge']")
  paras <- xml_find_all(cell, "./*[local-name()='p']")
  list(
    grid_span    = if (inherits(span, "xml_missing")) 1L else
      as.integer(xml_attr(span, "val") %||% "1"),
    vmerge       = if (inherits(vm, "xml_missing")) NA_character_ else
      (xml_attr(vm, "val") %||% "continue"),
    n_paragraphs = length(paras),
    paragraphs   = lapply(paras, paragraph_digest, vars = vars)
  )
}

table_digest <- function(tbl, index, vars) {
  rows <- xml_find_all(tbl, "./*[local-name()='tr']")
  row_list <- lapply(seq_along(rows), function(i) {
    r <- rows[[i]]
    hdr <- .first(r, "./*[local-name()='trPr']/*[local-name()='tblHeader']")
    list(
      row              = i,
      repeat_as_header = !inherits(hdr, "xml_missing"),
      cells            = lapply(xml_find_all(r, "./*[local-name()='tc']"),
                                cell_digest, vars = vars)
    )
  })
  list(table = index, n_rows = length(rows), rows = row_list)
}

## --- entry point -------------------------------------------------------------

#' Write a privacy-safe structural digest of a .docx shell.
#'
#' @param docx_path  the annotated shell (never modified)
#' @param out_json   where to write the digest
#' @param max_tables cap on how many tables to describe (keeps the file small)
digest_shell <- function(docx_path, out_json = "shell_digest.json",
                         max_tables = 12L) {
  stopifnot(file.exists(docx_path))
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  utils::unzip(docx_path, exdir = td)

  doc <- read_xml(file.path(td, "word", "document.xml"))
  vars <- new.env(parent = emptyenv())

  tables <- xml_find_all(doc, ".//*[local-name()='tbl']")
  n_tables <- length(tables)
  if (n_tables > max_tables) tables <- tables[seq_len(max_tables)]

  ## Body paragraphs OUTSIDE tables: headings, population lines, footnotes.
  ## Their shape matters (arsbridge finds titles and populations there) but
  ## their words do not.
  body_paras <- xml_find_all(doc, "/*/*[local-name()='body']/*[local-name()='p']")
  body <- lapply(body_paras, paragraph_digest, vars = vars)
  body <- Filter(function(p) p$n_chars > 0, body)

  ## Walk the tables BEFORE building the result: the variable-token counter
  ## must be read after every reference has been seen.
  table_list <- lapply(seq_along(tables), function(i)
    table_digest(tables[[i]], i, vars))

  digest <- list(
    generated_by  = "tools/shell_structure_digest.R",
    generated_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    note = paste("Structure only. All document text is replaced by",
                 "character-class silhouettes and tokenised skeletons;",
                 "no study text, labels, values or identifiers are included."),
    n_tables_in_document = n_tables,
    n_tables_described   = length(tables),
    n_distinct_variable_refs = length(ls(vars)),
    body_paragraphs = body,
    tables = table_list
  )

  write_json(digest, out_json, auto_unbox = TRUE, pretty = TRUE, null = "null")
  message("Wrote ", out_json, " (", n_tables, " tables in document, ",
          length(tables), " described).")
  message("Open it and read it before sharing.")
  invisible(digest)
}

## --- compact summary ---------------------------------------------------------

#' Print a short, paste-ready overview of a digest.
#'
#' The full JSON is thorough but large. This condenses it to the handful of
#' facts that actually decide a parsing question: how many header rows a table
#' has, whether any header cell spans columns, how many stub cells hold more
#' than one paragraph, how many rows are struck through, and which authoring
#' conventions appear. Small enough to paste into a message.
#'
#'   digest_summary("digest.json")
digest_summary <- function(digest, max_rows_shown = 6L) {
  if (is.character(digest)) digest <- read_json(digest, simplifyVector = FALSE)

  cat("Shell structure summary\n")
  cat("  tables in document :", digest$n_tables_in_document, "\n")
  cat("  tables described   :", digest$n_tables_described, "\n")
  cat("  distinct var refs  :", digest$n_distinct_variable_refs, "\n\n")

  all_conv <- character(0)

  for (tb in digest$tables) {
    rows <- tb$rows
    hdr_flagged <- sum(vapply(rows, function(r) isTRUE(r$repeat_as_header),
                              logical(1)))
    spans <- sum(vapply(rows, function(r) {
      sum(vapply(r$cells, function(cl) (cl$grid_span %||% 1L) > 1L, logical(1)))
    }, integer(1)))
    vmerges <- sum(vapply(rows, function(r) {
      sum(vapply(r$cells, function(cl) !is.null(cl$vmerge) &&
                   !is.na(cl$vmerge), logical(1)))
    }, integer(1)))
    multi_para <- sum(vapply(rows, function(r) {
      if (length(r$cells) == 0) return(0L)
      as.integer((r$cells[[1]]$n_paragraphs %||% 1L) > 1L)
    }, integer(1)))
    struck <- sum(vapply(rows, function(r) {
      if (length(r$cells) == 0) return(0L)
      runs <- unlist(lapply(r$cells[[1]]$paragraphs, function(p) p$runs),
                     recursive = FALSE)
      texted <- Filter(function(rr) (rr$n_chars %||% 0L) > 0L, runs)
      as.integer(length(texted) > 0 &&
                   all(vapply(texted, function(rr) isTRUE(rr$strike), logical(1))))
    }, integer(1)))

    conv <- unlist(lapply(rows, function(r)
      unlist(lapply(r$cells, function(cl)
        unlist(lapply(cl$paragraphs, function(p) names(p$conventions)))))))
    all_conv <- c(all_conv, conv)

    cat(sprintf("Table %d: %d rows | header rows flagged: %d | spanned cells: %d\n",
                tb$table, tb$n_rows, hdr_flagged, spans))
    cat(sprintf("  vertical merges: %d | multi-paragraph stubs: %d | struck rows: %d\n",
                vmerges, multi_para, struck))

    shown <- 0L
    for (r in rows) {
      if (shown >= max_rows_shown) break
      if (length(r$cells) == 0) next
      p1 <- r$cells[[1]]$paragraphs[[1]]
      if (is.null(p1) || (p1$n_chars %||% 0L) == 0L) next
      shown <- shown + 1L
      cat(sprintf("    r%-2d %s\n", r$row, substr(p1$skeleton %||% "", 1, 70)))
    }
    cat("\n")
  }

  if (length(all_conv)) {
    cat("Authoring conventions seen (count):\n")
    tab <- sort(table(all_conv), decreasing = TRUE)
    for (nm in names(tab)) cat(sprintf("  %-22s %d\n", nm, tab[[nm]]))
  } else {
    cat("Authoring conventions seen: none of the known patterns.\n")
  }
  invisible(NULL)
}

## --- diagnostics redaction ---------------------------------------------------

#' Redact an arsbridge diagnostics / blockers data frame for sharing.
#'
#' The MESSAGES are arsbridge's own text and safe, but they quote row labels
#' and annotations from the shell. This blanks anything inside quotes and
#' silhouettes the location column, keeping stage / severity / message shape.
#'
#'   d <- ars_diagnostics()
#'   write.csv(redact_diagnostics(d), "diagnostics_redacted.csv", row.names = FALSE)
redact_diagnostics <- function(df) {
  scrub <- function(x) {
    x <- as.character(x %||% "")
    x <- gsub("'[^']*'", "'<redacted>'", x)
    gsub('"[^"]*"', '"<redacted>"', x)
  }
  for (col in intersect(c("problem", "action"), names(df))) {
    df[[col]] <- scrub(df[[col]])
  }
  if ("location" %in% names(df)) df$location <- silhouette(df$location)
  df
}
