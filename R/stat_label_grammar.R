## arsbridge -- stat_label_grammar.R
## ---------------------------------------------------------------------------
## Reading a shell's statistic-line labels.
##
## A continuous block is authored as a parent row carrying the variable over
## unannotated lines naming the statistics: "Mean (SD)", "Median", "Q1; Q3".
## Those lines have no analysis of their own, so a cell on one of them is
## filled from the parent, selected by what the line is CALLED.
##
## This used to be a closed list of fifteen exact spellings. Every sponsor
## writes these lines differently -- "Q1, Q3", "Q1; Q3", "Q1 - Q3",
## "25th percentile, 75th percentile" all name the same two statistics -- so a
## lookup table of one study's spellings is a defect generator: an unlisted
## spelling matched nothing, the row was reclassified as a category level of
## its parent, and its placeholder slots were bound POSITIONALLY to the
## method's first operations. A "Q1; Q3" line filled with a count and a mean.
##
## Three things are separated here, and keeping them separate is the design:
##
##   1. What the LABEL asks for      -- a semantic token, this file
##   2. What the METHOD can provide  -- an operation, .resolve_stat_tokens()
##   3. What the ARD actually holds  -- a stat_name, .ard_value()
##
## The label never names an operation and never names an engine statistic.
## It cannot: "n" means the frequency of a category under a counting method
## and the count of non-missing observations under a continuous one, which the
## engine spells differently again. Resolving a label to an operation without
## consulting the method is guessing, and a wrong guess writes a real number of
## the wrong statistic into a shipped workbook.

## ---------------------------------------------------------------------------
## The vocabulary
## ---------------------------------------------------------------------------

## The semantic statistics a shell line may ask for.
##
## Deliberately NOT a list of engine outputs: these are the things an author
## can name. A denominator is absent by design -- it is a working quantity of
## a percentage, never a statistic a line displays -- so no label may request
## one. That absence is what keeps a continuous count from ever being read as
## a percentage denominator.
.STAT_TOKENS <- c(
  "count", "pct", "mean", "sd", "se", "median", "q1", "q3",
  "min", "max", "cv", "geomean", "ci_low", "ci_high", "events", "pvalue"
)

## Which semantic statistic each ARS operation provides.
##
## This is the join between stage 1 and stage 2. An operation with no token
## cannot be requested by a label at all: `OP_DENOM` because a denominator is
## not displayed, and `OP_PASS` / `OP_MANUAL` / `OP_STAT` because they stand
## for a passthrough, a reservation and a declarative statistic respectively --
## none of which a statistic line names.
##
## The names here are LITERAL operation ids, matching `.OP_STAT_NAMES` in
## shell_fill_meta.R. There are no `OP_*` R constants in this package --
## `.STANDARD_METHODS` writes `id = "OP_N"` as a string and
## `exists("OP_N", asNamespace("arsbridge"))` is FALSE -- so computing these
## names from bare symbols would fail with "object 'OP_N' not found".
.OP_TOKENS <- c(
  OP_N       = "count",
  OP_PCT     = "pct",
  OP_MEAN    = "mean",
  OP_SD      = "sd",
  OP_SE      = "se",
  OP_MEDIAN  = "median",
  OP_Q1      = "q1",
  OP_Q3      = "q3",
  OP_MIN     = "min",
  OP_MAX     = "max",
  OP_CV      = "cv",
  OP_GEOMEAN = "geomean",
  OP_CI_LOW  = "ci_low",
  OP_CI_HIGH = "ci_high",
  OP_EVENTS  = "events",
  OP_PVALUE  = "pvalue"
)

## The phrases authors write, per semantic statistic.
##
## Standards-level statistical vocabulary, not study knowledge: no entry is a
## dataset, a variable, an output id or a study label. Every phrase is stored
## already normalised, so the table and the input pass through the same
## functions -- the old map hand-normalised its keys and then re-derived a
## second "flat" form at lookup time to compensate.
##
## Absent on purpose, because each is a plausible CODELIST value: bare "p"
## (percentage or p-value is unresolvable anyway), "unknown", "not reported",
## "other", "total", "missing". Note this exclusion is a courtesy, not the
## safety mechanism -- the safety mechanism is that a per-category parent never
## consults this grammar at all.
.STAT_ALIASES <- list(
  count    = c("n", "count", "nobs", "n obs", "number of subjects",
               "number of observations", "non missing", "nonmissing"),
  pct      = c("%", "pct", "percent", "percentage", "proportion",
               "column %", "col %", "row %"),
  mean     = c("mean", "arithmetic mean", "average"),
  sd       = c("sd", "s d", "std", "stdev", "std dev", "standard deviation"),
  se       = c("se", "sem", "std error", "standard error",
               "standard error of the mean"),
  median   = c("median", "med", "50th percentile", "p50", "q2"),
  q1       = c("q1", "quartile 1", "first quartile", "1st quartile",
               "lower quartile", "25th percentile", "p25"),
  q3       = c("q3", "quartile 3", "third quartile", "3rd quartile",
               "upper quartile", "75th percentile", "p75"),
  min      = c("min", "minimum", "lowest"),
  max      = c("max", "maximum", "highest"),
  cv       = c("cv", "coefficient of variation"),
  geomean  = c("geometric mean", "geomean"),
  events   = c("events", "number of events"),
  pvalue   = c("p value", "pvalue", "p val")
)

## One authored word standing for an ORDERED pair.
##
## `<num>` is the single wildcard in the phrase language, matching a bare
## number. It exists for a specific ordering hazard: without it "95% CI"
## tokenises to 95, %, ci, the "%" alias fires, and the request gains a
## spurious percentage ahead of the interval. The wildcard absorbs the level
## and contributes nothing.
.STAT_COMPOSITES <- list(
  "range"                       = c("min", "max"),
  "iqr"                         = c("q1", "q3"),
  "interquartile range"         = c("q1", "q3"),
  "ci"                          = c("ci_low", "ci_high"),
  "confidence interval"         = c("ci_low", "ci_high"),
  "<num> % ci"                  = c("ci_low", "ci_high"),
  "<num> % confidence interval" = c("ci_low", "ci_high")
)

## ---------------------------------------------------------------------------
## Normalisation
## ---------------------------------------------------------------------------

#' Normalise a label for statistic matching.
#'
#' Deliberately NOT `.norm_label()`, whose `[[:punct:]]` strip destroys the
#' separators this grammar reads: `.norm_label("n (%)")` is `"n"`, which loses
#' the percentage entirely. `.norm_label()` stays the right normaliser for
#' fuzzy annotation binding.
#'
#' Unicode folding is delegated to `.normalize_shell_text()`, the package's one
#' entry point for it, so a zero-width or non-breaking space cannot reach the
#' matcher.
#' @noRd
.norm_stat_label <- function(x) {
  x <- .normalize_shell_text(as.character(x %||% ""))
  if (length(x) != 1L || is.na(x)) return("")

  ## Footnote decoration. Three narrow rules, each anchored so it cannot eat a
  ## word: trailing marker punctuation, superscript digits, and a SINGLE
  ## alphanumeric glued to a closing bracket ("Mean (SD)a").
  x <- gsub("[*\u2020\u2021\u00a7\u00b6#]+\\s*$", "", x, perl = TRUE)
  x <- gsub("[\u00b9\u00b2\u00b3\u2070-\u2079\u207a-\u207f]+", "", x,
            perl = TRUE)
  ## Explicit alternation rather than a bracket class: as a POSIX bracket
  ## expression "[)\\]]" closes at its own "]" and the rule silently never
  ## fires, which is the same trap `.STAT_SEPARATORS` fell into.
  x <- sub("(\\)|\\])[[:alnum:]][[:space:]]*$", "\\1", x, perl = TRUE)

  ## Dash unification. This belongs here and not in `.normalize_shell_text()`,
  ## whose comment correctly refuses to touch dashes because they separate
  ## titles from populations. A statistic label is a bounded context where
  ## folding them is safe.
  x <- gsub("[\u2010-\u2015\u2212]", "-", x, perl = TRUE)

  ## ASCII-safe case fold. `tolower()` is locale-dependent -- under a Turkish
  ## locale "I" becomes a dotless i and every alias miss is silent.
  x <- chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz", x)

  trimws(gsub("\\s+", " ", x))
}

## Characters that separate one statistic from the next. The hyphen is
## unconditional: "Min-Max" and "Min - Max" are both ordinary spellings, and
## the hyphenated terms worth keeping ("p-value", "non-missing") are carried
## as spaced aliases instead.
## Written for PCRE (`perl = TRUE`), where "\\[" and "\\]" are real escapes
## inside a class. As a POSIX bracket expression this silently closed at the
## first "]" and the remainder became literal text the pattern then required --
## so it split nothing at all, and every multi-statistic label was rejected.
.STAT_SEPARATORS <- "[,;/|(){}\\[\\]\u00b1-]"

#' Split a normalised label into its word stream.
#' @noRd
.stat_tokens_of <- function(x) {
  ## "+/-" first: the slash is a separator, so splitting on separators first
  ## would shred it before it could be recognised. `fixed = TRUE` takes the
  ## pattern literally, so it must not carry regex escapes.
  x <- gsub("+/-", " ", x, fixed = TRUE)
  ## The percent sign is its own word. Without this "95% ci" tokenises as
  ## "95%" and the "<num> % ci" phrase can never match.
  x <- gsub("%", " % ", x, fixed = TRUE)
  x <- gsub(.STAT_SEPARATORS, " ", x, perl = TRUE)
  ## "." and "_" separate only when NOT between two digits, so "97.5" keeps
  ## its number while `.statline_for()`'s title-case fallback ("P.value",
  ## "Geometric_mean") reads back.
  x <- gsub("(?<![0-9])[._]|[._](?![0-9])", " ", x, perl = TRUE)
  x <- gsub("\\b(and|to|vs|versus)\\b", " ", x)
  x <- trimws(gsub("\\s+", " ", x))
  if (!nzchar(x)) return(character())
  strsplit(x, " ", fixed = TRUE)[[1]]
}

## ---------------------------------------------------------------------------
## The phrase index
## ---------------------------------------------------------------------------

## Built once, not rebuilt per call -- the map it replaces looped over its
## whole table on every lookup, for every stub row of every sheet.
.stat_index_env <- new.env(parent = emptyenv())

#' phrase (space-joined token key) -> ordered token vector.
#' @noRd
.stat_index <- function() {
  if (!is.null(.stat_index_env$idx)) return(.stat_index_env$idx)

  idx <- list()
  add <- function(phrase, tokens) {
    key <- paste(.stat_tokens_of(.norm_stat_label(phrase)), collapse = " ")
    ## A phrase normalising to nothing would match every gap in the stream; a
    ## duplicate would make the vocabulary ambiguous. Both are authoring
    ## errors in the tables above, so fail loudly rather than at fill time.
    if (!nzchar(key)) {
      stop("statistic alias normalises to nothing: ", phrase, call. = FALSE)
    }
    if (!is.null(idx[[key]]) && !identical(idx[[key]], tokens)) {
      stop("statistic alias '", key, "' maps to two different statistics",
           call. = FALSE)
    }
    idx[[key]] <<- tokens
  }

  for (tok in names(.STAT_ALIASES)) {
    for (phrase in .STAT_ALIASES[[tok]]) add(phrase, tok)
  }
  for (phrase in names(.STAT_COMPOSITES)) {
    add(phrase, .STAT_COMPOSITES[[phrase]])
  }

  .stat_index_env$idx <- idx
  .stat_index_env$max_words <-
    max(vapply(names(idx), function(k) length(strsplit(k, " ")[[1]]),
               integer(1)))
  idx
}

#' The longest phrase in the vocabulary, in words -- the greedy match window.
#' @noRd
.stat_index_width <- function() {
  .stat_index()
  .stat_index_env$max_words
}

#' Leftmost-longest phrase match over a token stream.
#'
#' Longest-first is load-bearing: it is what makes "standard deviation" beat
#' nothing-at-all and "<num> % ci" beat the bare "%".
#'
#' @param whole When TRUE every token must be consumed by some phrase, and an
#'   unmatched token returns NULL. When FALSE the walk reports whether ANY
#'   phrase matched, which is what unit-stripping needs.
#' @noRd
.match_stat_phrases <- function(toks, whole = TRUE) {
  if (length(toks) == 0L) return(if (whole) NULL else FALSE)
  idx   <- .stat_index()
  width <- .stat_index_width()

  out <- character()
  i <- 1L
  while (i <= length(toks)) {
    hit <- NULL
    for (w in min(width, length(toks) - i + 1L):1L) {
      window <- toks[i:(i + w - 1L)]
      cand <- idx[[paste(window, collapse = " ")]]
      if (is.null(cand) && grepl("^[0-9]+(\\.[0-9]+)?$", window[[1]])) {
        cand <- idx[[paste(c("<num>", window[-1]), collapse = " ")]]
      }
      if (!is.null(cand)) { hit <- list(tokens = cand, width = w); break }
    }
    if (is.null(hit)) {
      if (whole) return(NULL)
      i <- i + 1L
      next
    }
    if (!whole) return(TRUE)
    out <- c(out, hit$tokens)
    i <- i + hit$width
  }
  if (!whole) return(FALSE)
  if (length(out) == 0L) return(NULL)
  out
}

## ---------------------------------------------------------------------------
## Stage 1: label -> semantic tokens
## ---------------------------------------------------------------------------

#' Drop a bracketed group that carries no statistic.
#'
#' "Mean (kg)" and "Mean (per 100 PY)" are a statistic plus a unit; "Mean (SD)"
#' and "Mean (standard deviation)" are two statistics. The difference is
#' whether the bracket's contents contain a recognised phrase, so the contents
#' are scanned with the same grammar -- structural, not a list of known units.
#' The scan runs on the raw contents with the full leftmost-longest matcher and
#' never re-enters unit stripping, so a MULTIWORD statistic inside the bracket
#' is seen whole rather than one token at a time.
#' @noRd
.strip_stat_units <- function(x) {
  m <- gregexpr("\\(([^()]*)\\)|\\[([^\\[\\]]*)\\]", x, perl = TRUE)[[1]]
  if (m[[1]] == -1) return(x)
  lens <- attr(m, "match.length")
  drop <- logical(length(m))
  for (i in seq_along(m)) {
    grp <- substr(x, m[[i]], m[[i]] + lens[[i]] - 1L)
    ## The whole label being one bracket carries no unit information.
    if (identical(trimws(grp), trimws(x))) next
    inner <- substr(grp, 2L, nchar(grp) - 1L)
    toks <- .stat_tokens_of(inner)
    drop[[i]] <- length(toks) > 0L &&
      !isTRUE(.match_stat_phrases(toks, whole = FALSE))
  }
  for (i in rev(which(drop))) {
    x <- paste0(substr(x, 1L, m[[i]] - 1L), " ",
                substr(x, m[[i]] + lens[[i]], nchar(x)))
  }
  trimws(gsub("\\s+", " ", x))
}

#' The semantic statistics a row label names, in the order it names them.
#'
#' @return An ordered character vector drawn from `.STAT_TOKENS`, or NULL when
#'   the label is not a statistic line.
#'
#' Rejection is WHOLE. A label naming one statistic plus anything unrecognised
#' ("Median of prior therapies", "Sum of DATASET.VARIABLE", "Range of motion")
#' is not
#' a statistic line, because binding what matched would pair a short statistic
#' vector against a longer placeholder and write a plausible wrong number. A
#' rejected label leaves a pending cell with a stated reason, which is the
#' answer this package gives under uncertainty.
#' @noRd
.parse_stat_label <- function(label) {
  raw <- as.character(label %||% "")
  if (length(raw) != 1L || is.na(raw)) return(NULL)

  x <- .norm_stat_label(raw)
  if (!nzchar(x)) return(NULL)
  ## A trailing colon makes it a heading, not a line.
  if (grepl(":$", x)) return(NULL)

  x <- .strip_stat_units(x)
  toks <- .stat_tokens_of(x)
  if (length(toks) == 0L) return(NULL)
  ## A real statistic line is short. The cap is the cheapest brake there is on
  ## a long prose label that happens to contain a statistic word.
  if (length(toks) > 10L) return(NULL)

  ## No de-duplication: a label stating a statistic twice asks for it twice,
  ## and collapsing would shift every later placeholder slot.
  .match_stat_phrases(toks, whole = TRUE)
}

## ---------------------------------------------------------------------------
## Stage 2: semantic tokens -> the method's declared operations
## ---------------------------------------------------------------------------

#' Bind a label's semantic request to what one method actually declares.
#'
#' This is the stage that stops a label deciding an operation on its own. A
#' token the method does not declare is NOT approximated, NOT bound
#' positionally, and NOT carried through as a bare statistic name: the whole
#' row refuses, because a partially bound row shifts its remaining slots onto
#' the wrong statistics.
#'
#' @return list(stats, tokens, unsupported, available). `stats` is empty
#'   whenever `unsupported` is non-empty, so a caller that ignores the detail
#'   still binds nothing.
#' @noRd
.resolve_stat_tokens <- function(tokens, methods, method_id) {
  slots <- .method_operation_slots(methods, method_id %||% "")
  ## Operation id -> the semantic statistic it provides. An operation outside
  ## the token map is simply not offered to a label.
  by_token <- list()
  for (s in slots) {
    op <- as.character(s$operation_id %||% "")
    if (!nzchar(op) || !op %in% names(.OP_TOKENS)) next
    tok <- unname(.OP_TOKENS[[op]])
    if (is.null(by_token[[tok]])) by_token[[tok]] <- s
  }

  available <- vapply(slots, function(s) as.character(s$operation_id %||% "?"),
                      character(1))
  unsupported <- tokens[!vapply(tokens, function(t) !is.null(by_token[[t]]),
                                logical(1))]

  if (length(unsupported) > 0L) {
    return(list(stats = list(), tokens = tokens,
                unsupported = unique(unsupported), available = available))
  }
  list(stats = lapply(tokens, function(t) by_token[[t]]),
       tokens = tokens, unsupported = character(), available = available)
}

## ---------------------------------------------------------------------------
## What is recognised, and what a method can actually deliver
## ---------------------------------------------------------------------------

#' Every (method, semantic statistic) pair, and how it resolves.
#'
#' Recognising a label and being able to COMPUTE it are different things, and
#' conflating them is how a reader ends up believing a statistic is supported
#' because the grammar could read its name. This is the honest table: for each
#' method, which statistics resolve to an operation, and which are recognised
#' but not produced.
#'
#' Generated from the vocabulary and the method catalogue, never hand-written,
#' so it cannot drift from what the code does.
#' @noRd
.stat_label_support <- function(methods = .STANDARD_METHODS) {
  out <- list()
  for (m in methods) {
    mid <- as.character(m$id %||% "")
    for (tok in names(.STAT_ALIASES)) {
      res <- .resolve_stat_tokens(tok, methods, mid)
      ok  <- length(res$unsupported) == 0L
      slot <- if (ok) res$stats[[1]] else NULL
      out[[length(out) + 1L]] <- data.frame(
        method    = mid,
        statistic = tok,
        operation = if (ok) as.character(slot$operation_id) else NA_character_,
        ard_stat  = if (ok) {
          paste(slot$stat_names %||% slot$stat_name, collapse = " or ")
        } else NA_character_,
        supported = ok,
        stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, out)
}

#' The support table rendered as Markdown, for `inst/extdata`.
#'
#' A committed rendering plus a test that regenerates and compares is the
#' cheapest drift detection there is: the documentation cannot quietly stop
#' describing the code, because the comparison fails first.
#' @noRd
.stat_label_support_md <- function(methods = .STANDARD_METHODS) {
  tab <- .stat_label_support(methods)
  aliases <- vapply(names(.STAT_ALIASES), function(tok) {
    paste0("`", paste(.STAT_ALIASES[[tok]], collapse = "`, `"), "`")
  }, character(1))

  ln <- c(
    "# Statistic labels arsbridge can read",
    "",
    "Generated from the vocabulary and the method catalogue by",
    "`.stat_label_support_md()`. Do not edit by hand -- a test regenerates",
    "this file and compares it, so an edit here fails the suite.",
    "",
    "A shell's statistic line names a SEMANTIC statistic. Whether that",
    "statistic can be produced is the analysis method's answer, not the",
    "label's, so the same label resolves differently -- or refuses -- under",
    "different methods. A statistic a method does not declare leaves the",
    "whole row unbound and is reported; it is never approximated, and never",
    "bound to whichever operation happens to come first.",
    "",
    "## Phrases recognised, per statistic",
    "",
    "| Statistic | Written as |",
    "|---|---|")
  for (tok in names(.STAT_ALIASES)) {
    ln <- c(ln, sprintf("| `%s` | %s |", tok, aliases[[tok]]))
  }

  ln <- c(ln, "",
    "One word may name an ordered pair:",
    "",
    "| Written as | Names |",
    "|---|---|")
  for (phrase in names(.STAT_COMPOSITES)) {
    ln <- c(ln, sprintf("| `%s` | `%s` |", phrase,
                        paste(.STAT_COMPOSITES[[phrase]], collapse = "`, `")))
  }

  ln <- c(ln, "",
    "## Resolution, per method",
    "",
    "`operation` is the ARS operation the statistic binds to; `ARD name` is",
    "what the execution engine calls the result. A blank row means the",
    "method declares no operation for that statistic.",
    "")
  for (mid in unique(tab$method)) {
    sub <- tab[tab$method == mid, , drop = FALSE]
    ok  <- sub[sub$supported, , drop = FALSE]
    no  <- sub[!sub$supported, , drop = FALSE]
    ln <- c(ln, sprintf("### %s", mid), "",
            "| Statistic | Operation | ARD name |", "|---|---|---|")
    if (nrow(ok) == 0L) {
      ln <- c(ln, "| _(none)_ | | |")
    } else {
      ln <- c(ln, sprintf("| `%s` | `%s` | `%s` |",
                          ok$statistic, ok$operation, ok$ard_stat))
    }
    ln <- c(ln, "",
            sprintf("Recognised but not produced by this method: %s.",
                    if (nrow(no) == 0L) "none"
                    else paste0("`", paste(no$statistic, collapse = "`, `"), "`")),
            "")
  }
  paste0(paste(ln, collapse = "\n"), "\n")
}

## ---------------------------------------------------------------------------
## Compatibility shim
## ---------------------------------------------------------------------------

#' The statistics a row label asks for, or NULL when it is not a statistic
#' line.
#'
#' With no method supplied this reports the SEMANTIC tokens; with one, the
#' engine stat names that method's operations will produce, or NULL when the
#' method cannot provide them all. Retained for
#' `.check_method_placeholder_slots()` and for the round-trip contract with
#' `.statline_for()`.
#' @noRd
.stats_for_line <- function(label, methods = NULL, method_id = NULL) {
  tokens <- .parse_stat_label(label)
  if (is.null(tokens)) return(NULL)
  if (is.null(methods) || !nzchar(method_id %||% "")) return(tokens)
  res <- .resolve_stat_tokens(tokens, methods, method_id)
  if (length(res$unsupported) > 0L) return(NULL)
  vapply(res$stats, function(s) as.character(s$stat_name), character(1))
}

## ---------------------------------------------------------------------------
## Classifying a row as a statistic row, before any method is known
## ---------------------------------------------------------------------------

## The question this section answers is NOT the one `.parse_stat_label()`
## answers, and keeping them apart is the design.
##
## At FILL time a row's parent has a method, so "which statistics does this
## label ask for?" can be resolved against what that method declares, and a
## label under a per-category parent is never read at all -- it is a codelist
## value, whatever it resembles.
##
## At PARSE time none of that exists. The only question is the narrower one:
## "is this row a statistic sub-row of the block above it, so an analysis must
## not be bound to it?" A wrong YES refuses an analysis a row genuinely wanted.
## A wrong NO lets an LLM or supplement bind an analysis to a layout row and
## produces a duplicate block.
##
## This used to be answered by a separate closed list of fifteen exact
## spellings, which is the same defect shape the grammar replaced at fill time:
## an author writing "Standard Deviation" instead of "SD" got a different
## answer for no reason anyone could state. The vocabulary is now shared; the
## DECISION stays deliberately narrower, in the two ways below.

## Which statistics a parse-time site may recognise.
##
## Narrower than `.STAT_TOKENS` on purpose. Recognising a statistic here has
## no method behind it, so the scope is held to the statistics the historical
## list already covered rather than to everything the grammar can read.
##
## `pct` is in scope for one specific reason worth writing down: the old site
## compared `.norm_label(label)`, whose `[[:punct:]]` strip turns "n (%)" into
## "n" -- so an "n (%)" row has ALWAYS been treated as a statistic sub-row,
## as a side effect of normalisation rather than by intent. Dropping `pct`
## would change that, so it stays.
##
## Deliberately OUT of scope, so no new KIND of statistic becomes a parse-time
## statistic row: `ci_low`, `ci_high`, `events`, `pvalue`. A "95% CI",
## "p-value" or "events" row classifies exactly as it did before.
.STATLINE_TOKEN_SCOPE <- c(
  "count", "pct", "mean", "sd", "median",
  "min", "max", "q1", "q3", "se", "cv", "geomean"
)

## Historical statistic-row labels the grammar cannot express.
##
## "n missing" was in the old list and the grammar rejects it, because
## `missing` is not a statistic token: no method declares a missing-count
## operation, and `missing` is also a perfectly ordinary codelist value. Both
## of those remain true, so this is NOT fixed by adding a token -- that would
## claim a resolvable statistic the engine cannot produce.
##
## It is preserved here instead, as exactly what it is: a backward-compatible
## classification with no resolution behind it. The row is still a layout row
## of the block above, so no analysis binds to it; and at fill time it is
## still unreadable, so it surfaces in `ars_unresolved_labels()` -- which is
## where a request nothing can compute belongs.
##
## Real support for a missing count is a later, explicit feature needing a
## method operation and ARD evidence. Other spellings ("Number missing") are
## deliberately NOT listed: they were not in the historical set, and adding
## them here would be that feature, smuggled in as a compatibility shim.
.STATLINE_LEGACY_LABELS <- "n missing"

#' Is this row a statistic sub-row of the block above it?
#'
#' Parse-time classification, with no method in hand. See the section comment
#' above for why this is narrower than `.parse_stat_label()`.
#' @noRd
.is_statline_row_label <- function(label) {
  raw <- as.character(label %||% "")
  if (length(raw) != 1L || is.na(raw) || !nzchar(trimws(raw))) return(FALSE)

  ## The historical exceptions, compared the way the old site compared them.
  if (.norm_label(raw) %in% .STATLINE_LEGACY_LABELS) return(TRUE)

  ## A label that is nothing but a composite WORD -- "Range", "IQR" -- is one
  ## word standing for a pair, and is exactly the shape of a codelist value.
  ## The historical set contained no such entry, and a categorical block whose
  ## levels include "Range" must keep them. Spelled-out forms ("Min - Max",
  ## "Q1, Q3") are unaffected: they name their statistics explicitly.
  if (.norm_stat_label(raw) %in% names(.STAT_COMPOSITES)) return(FALSE)

  tokens <- .parse_stat_label(raw)
  length(tokens) > 0L && all(tokens %in% .STATLINE_TOKEN_SCOPE)
}

## The placeholder shape a statistic line gets when arsbridge AUTHORS a shell.
##
## The third place that used to restate the statistic vocabulary: a chain of
## six `.norm_label()` comparisons in shell_table.R, deciding how many numbers
## a generated row shows and at what precision. Keyed on the STATISTICS the
## label names rather than on six exact spellings, so "Mean (Standard
## Deviation)" gets the same shape as "Mean (SD)" -- which it always should
## have, and did not.
##
## This is the forward direction (arsbridge writing a shell from its own ARS),
## so there is no sponsor text to misread here: the labels come from
## `.statline_for()`. The shapes below are exactly the ones the chain produced.
##
## Order matters only in that the first exact token-vector match wins; the
## vectors are distinct, so there is nothing to disambiguate.
.STATLINE_PLACEHOLDER_SHAPES <- list(
  list(tokens = c("count"),              placeholder = "xx"),
  list(tokens = c("mean", "sd"),         placeholder = "xx.x (x.xx)"),
  list(tokens = c("median"),             placeholder = "xx.x"),
  list(tokens = c("min", "max"),         placeholder = "(xx.x, xx.x)"),
  list(tokens = c("q1", "q3"),           placeholder = "(xx.x, xx.x)"),
  list(tokens = c("median", "q1", "q3"), placeholder = "xx.x (xx.x, xx.x)")
)

## A single unrecognised or unlisted statistic line shows one decimal number.
## Unchanged: it is what the chain's final `return("xx.x")` did.
.STATLINE_PLACEHOLDER_DEFAULT <- "xx.x"

#' The placeholder a generated statistic line carries.
#' @noRd
.statline_placeholder <- function(label) {
  tokens <- .parse_stat_label(label)
  if (length(tokens) > 0L) {
    for (shape in .STATLINE_PLACEHOLDER_SHAPES) {
      if (identical(tokens, shape$tokens)) return(shape$placeholder)
    }
  }
  .STATLINE_PLACEHOLDER_DEFAULT
}
