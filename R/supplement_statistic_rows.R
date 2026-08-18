## arsbridge -- supplement_statistic_rows.R
## ---------------------------------------------------------------------------
## What a supplement may say about a STATISTIC row, and what it may never say.
##
## A shell row whose label names statistics is filled from the block above it,
## and which statistics it gets is decided by reading the label. When the
## grammar cannot read it, arsbridge binds nothing and reports the row -- see
## `ars_unresolved_labels()`. Until now there was no way to ANSWER that report:
## the supplement channel skipped statistic rows entirely, and so did the LLM
## path, so a sponsor dialect the vocabulary did not know could only be fixed
## by editing the shell or by shipping a new arsbridge.
##
## This is that channel. The constraint it exists under:
##
##   A supplement may propose SEMANTIC MEANING. It may never create an
##   executable binding on its own.
##
## Three mechanisms enforce that, and none is a convention:
##
##   1. A supplement names SEMANTIC TOKENS -- "mean", "sd" -- never an ARS
##      operation id and never an engine statistic name. There is no field to
##      put one in. Resolution to an operation stays where it was: with the
##      method catalogue, in code (`.resolve_stat_tokens()`).
##   2. Only `status = "reviewed"` binds. A `"proposal"` is recorded, reported
##      and left unbound. A generator writes proposals; a person turns one
##      into a reviewed row.
##   3. The method still gates. A reviewed row asking for a statistic the
##      row's method does not declare is refused exactly as a label asking for
##      it would be -- review makes a request legible, not possible.
##
## What this does NOT do: call an LLM. Nothing here reaches the network, and
## no code in this package writes `status = "reviewed"`.
##
## The field is OPTIONAL and additive, so the format version does not move: a
## supplement written before this existed is exactly as valid as it was.

.SUPP_STAT_STATUS_PROPOSAL <- "proposal"
.SUPP_STAT_STATUS_REVIEWED <- "reviewed"
.SUPP_STAT_STATUSES <- c(.SUPP_STAT_STATUS_PROPOSAL, .SUPP_STAT_STATUS_REVIEWED)

## Where the proposal came from. Provenance only: it never affects whether the
## row binds -- `status` decides that alone. An LLM-sourced row a person has
## reviewed is normal, and it is still their review that makes it bind.
.SUPP_STAT_SOURCES <- c("supplement", "llm")

.SUPP_STAT_ROW_FIELDS <- c(
  "row_label", "semantic_tokens", "status", "source", "reviewed_by",
  "override", "expected_scope", "confidence", "generator", "evidence",
  "proposed_at"
)

## A cross-check, never an input: scope is the METHOD's property and is already
## known from the ARS. Accepting it as an input would create a second source of
## truth that can contradict the method.
.SUPP_STAT_SCOPES <- c("continuous", "categorical")

## `x` unless it is NA, in which case `y`. Local to this file and deliberately
## not used where a DECISION is made: collapsing "absent" and "malformed" to ""
## is right for provenance text and wrong for anything that gates behaviour,
## which is why every check below tests `is.na()` explicitly instead.
#' @noRd
`%|NA|%` <- function(x, y) if (length(x) != 1L || is.na(x)) y else x

## Read one field that must be a single JSON scalar.
##
## `read_supplement()` parses with `simplifyVector = FALSE`, so a JSON array
## arrives as a LIST of any length. Feeding that to `%in%` inside an `if` is
## the "the condition has length > 1" error, which aborts the whole validation
## run instead of reporting the one malformed field -- so a file carrying
## `"status": ["reviewed"]` would crash the pre-flight rather than be told what
## is wrong with it.
#' @noRd
.supp_scalar <- function(x) {
  if (is.null(x)) return("")
  if (is.list(x) || length(x) != 1L) return(NA_character_)
  v <- tryCatch(as.character(x), error = function(e) NA_character_)
  if (length(v) != 1L || is.na(v)) return(NA_character_)
  trimws(v)
}

## Read one field that must be a JSON boolean. NA for anything else, so a
## malformed value is reported rather than quietly read as FALSE -- an
## `override` silently downgraded looks exactly like a reviewer who never
## asked for one.
#' @noRd
.supp_flag <- function(x) {
  if (is.null(x)) return(FALSE)
  if (is.list(x) || length(x) != 1L || !is.logical(x)) return(NA)
  as.logical(x)
}

#' Read and validate one TLF's `statisticRows`.
#' @return list(rows, problems); `rows` holds only entries that passed.
#' @noRd
.supp_statistic_rows <- function(entry, where = "statisticRows") {
  raw <- entry$statisticRows %||% list()
  problems <- character(); rows <- list()
  note <- function(...) problems <<- c(problems, sprintf(...))

  if (length(raw) == 0) return(list(rows = rows, problems = problems))
  if (!is.list(raw) || !is.null(names(raw))) {
    note("regenerate: %s must be an ARRAY of objects, not a named object.", where)
    return(list(rows = rows, problems = problems))
  }

  for (i in seq_along(raw)) {
    r  <- raw[[i]]
    at <- sprintf("%s[%d]", where, i)
    if (!is.list(r) || is.null(names(r))) {
      note("regenerate: %s must be a JSON object with named fields.", at); next
    }
    for (f in setdiff(names(r), .SUPP_STAT_ROW_FIELDS)) {
      note("%s: unknown field '%s' ignored.", at, f)
    }

    label <- .supp_scalar(r$row_label)
    if (is.na(label) || !nzchar(label)) {
      note("regenerate: %s needs a non-empty scalar 'row_label' -- the stub text as the shell writes it.", at)
      next
    }
    tok <- .supp_stat_tokens(r$semantic_tokens, at, note)
    if (is.null(tok)) next

    status <- .supp_scalar(r$status)
    if (is.na(status) || !tolower(status) %in% .SUPP_STAT_STATUSES) {
      note("regenerate: %s ('%s') needs a scalar 'status' of %s.", at, label,
           paste(sprintf("'%s'", .SUPP_STAT_STATUSES), collapse = " or ")); next
    }
    status <- tolower(status)

    source <- .supp_scalar(r$source)
    if (is.na(source) || !tolower(source) %in% .SUPP_STAT_SOURCES) {
      note("regenerate: %s ('%s') needs a scalar 'source' of %s.", at, label,
           paste(sprintf("'%s'", .SUPP_STAT_SOURCES), collapse = " or ")); next
    }
    source <- tolower(source)

    ## A review is an act somebody performs, so it is recorded as one. This
    ## does not make a fabricated review impossible -- nothing inside a file
    ## can -- but it stops "reviewed" being the cheapest thing to type.
    reviewer <- .supp_scalar(r$reviewed_by)
    if (identical(status, .SUPP_STAT_STATUS_REVIEWED) &&
          (is.na(reviewer) || !nzchar(reviewer))) {
      note("regenerate: %s ('%s') is 'reviewed' but names no 'reviewed_by'. Only a reviewed row binds, so record who reviewed it.",
           at, label); next
    }
    reviewer <- reviewer %|NA|% ""

    override <- .supp_flag(r$override)
    if (length(override) != 1L || is.na(override)) {
      note("regenerate: %s ('%s') needs 'override' to be true or false.", at, label); next
    }
    if (override && !identical(status, .SUPP_STAT_STATUS_REVIEWED)) {
      note("regenerate: %s ('%s') sets 'override' but is not 'reviewed'. A proposal never overrides anything.",
           at, label); next
    }

    scope <- tolower(.supp_scalar(r$expected_scope) %|NA|% "")
    if (nzchar(scope) && !scope %in% .SUPP_STAT_SCOPES) {
      note("%s ('%s'): unknown 'expected_scope' '%s' ignored (expected %s).",
           at, label, scope,
           paste(sprintf("'%s'", .SUPP_STAT_SCOPES), collapse = " or "))
      scope <- ""
    }

    conf <- .supp_scalar(r$confidence) %|NA|% ""
    rows[[length(rows) + 1L]] <- list(
      row_label = label, semantic_tokens = tok, status = status,
      source = source, reviewed_by = reviewer, override = override,
      expected_scope = scope,
      ## Recorded for triage order in a review list. Deliberately NOT consulted
      ## anywhere: a model's self-reported confidence is not evidence about the
      ## world, and letting it gate acceptance would make a number the model
      ## chose decide whether a cell gets filled.
      confidence = if (nzchar(conf)) suppressWarnings(as.numeric(conf)) else NA_real_,
      generator = .supp_scalar(r$generator) %|NA|% "",
      evidence  = .supp_scalar(r$evidence)  %|NA|% "")
  }
  list(rows = rows, problems = problems)
}

#' Validate one statisticRow's `semantic_tokens`.
#'
#' This is the whole of the "may not choose an operation" rule, and it is one
#' membership test: `.STAT_TOKENS` is a CLOSED list of sixteen semantic terms,
#' so it already excludes every operation id, every engine-only name (`N`,
#' `p25`, `conf.low`) and every invented word.
#'
#' The `OP_` check is redundant against that and still worth having: reaching
#' one layer too far down is the commonest mistake a proposer makes, and "that
#' is an operation id, name the statistic" is a message the author can act on
#' where "unknown statistic" is not.
#'
#' The scan is confined to THIS field. Six of the sixteen tokens are spelled
#' identically to engine statistic names -- `mean`, `sd`, `median`, `min`,
#' `max`, `events` -- so a blanket "no engine names anywhere" rule would reject
#' the commonest valid entry the format can carry, and would also read
#' `evidence`, which is quoted shell text and decides nothing.
#' @noRd
.supp_stat_tokens <- function(x, at, note) {
  if (is.null(x) || !is.list(x)) {
    note("regenerate: %s needs 'semantic_tokens' as an ARRAY, e.g. [\"mean\", \"sd\"].", at)
    return(NULL)
  }
  if (length(x) == 0) {
    note("regenerate: %s needs a non-empty 'semantic_tokens' array.", at); return(NULL)
  }
  ok <- vapply(x, function(e) is.character(e) && length(e) == 1L && !is.na(e), logical(1))
  if (!all(ok)) {
    note("regenerate: %s has 'semantic_tokens' entries that are not strings.", at); return(NULL)
  }
  tok <- trimws(as.character(unlist(x, use.names = FALSE)))
  if (!all(nzchar(tok))) {
    note("regenerate: %s has an empty entry in 'semantic_tokens'.", at); return(NULL)
  }
  ops <- tok[grepl("^OP_[A-Z0-9_]+$", tok)]
  if (length(ops) > 0) {
    note("regenerate: %s names ARS operation id%s (%s) in 'semantic_tokens'. Name the STATISTIC instead (e.g. %s) -- arsbridge chooses the operation from the row's method.",
         at, if (length(ops) == 1L) "" else "s", paste(ops, collapse = ", "),
         paste(utils::head(.STAT_TOKENS, 4L), collapse = ", "))
    return(NULL)
  }
  unknown <- setdiff(tok, .STAT_TOKENS)
  if (length(unknown) > 0) {
    note("regenerate: %s names unknown statistic%s (%s). Known: %s.",
         at, if (length(unknown) == 1L) "" else "s",
         paste(unknown, collapse = ", "), paste(.STAT_TOKENS, collapse = ", "))
    return(NULL)
  }
  ## Duplicates are kept: a row showing a statistic twice asks for it twice,
  ## and collapsing would shift every later placeholder onto the wrong one.
  tok
}

#' Attach a TLF's reviewed statistic rows to the stub rows they name.
#'
#' Label-keyed through the same fuzzy matcher every other supplement binding
#' uses, so an assistant's own reading of the document cannot misalign rows.
#' What lands on a stub row is only ever a REQUEST -- semantic tokens -- which
#' the fill stage still has to resolve against the row's method.
#' @noRd
.apply_supplement_statistic_rows <- function(sec, supp_tlf) {
  parsed <- .supp_statistic_rows(supp_tlf %||% list())
  for (p in parsed$problems) {
    diag_add(stage = "supplement", severity = "FAIL", input = INPUT_SUPPLEMENT,
      problem = sprintf("TLF %s statisticRows: %s", sec$tlf_number %||% "?", p),
      tlf_number = sec$tlf_number,
      action = "The entry was dropped -- no statistic row was bound from it.")
  }
  if (length(parsed$rows) == 0) return(sec)

  labels_norm <- vapply(sec$stub_rows %||% list(),
                        function(r) .norm_label(r$label %||% ""), character(1))
  n_bound <- 0L; n_proposed <- 0L

  ## Two entries naming the same row are AMBIGUOUS, and ambiguity resolves to
  ## unresolved -- never to whichever came first. Detected before anything is
  ## attached, so a duplicate cannot bind and then be reported.
  keys  <- vapply(parsed$rows, function(p) .norm_label(p$row_label), character(1))
  duped <- unique(keys[duplicated(keys)])

  for (p in parsed$rows) {
    key <- .norm_label(p$row_label)
    if (key %in% duped) {
      diag_add(stage = "supplement", severity = "FAIL", input = INPUT_SUPPLEMENT,
        problem = sprintf("Row '%s': several statisticRows entries claim it.", p$row_label),
        tlf_number = sec$tlf_number,
        action = "None was applied -- ambiguity is never resolved by first match. Keep one entry per row.")
      next
    }
    idx <- .match_stub_label(key, labels_norm)
    if (is.na(idx)) {
      diag_add(stage = "supplement", severity = "WARN", input = INPUT_SUPPLEMENT,
        problem = sprintf("statisticRows label '%s' matched no stub row.", p$row_label),
        tlf_number = sec$tlf_number,
        action = "Nothing was bound -- match the label to the stub text as the shell writes it.")
      next
    }

    ## Recorded either way, so a reviewer sees what was offered even when it
    ## did not bind.
    sec$stub_rows[[idx]]$supplement_stat_proposal <- p

    if (!identical(p$status, .SUPP_STAT_STATUS_REVIEWED)) {
      n_proposed <- n_proposed + 1L
      diag_add(stage = "supplement", severity = "WARN", input = INPUT_SUPPLEMENT,
        problem = sprintf("Row '%s': statistic proposal (%s) from %s is not reviewed, so nothing was bound.",
                          p$row_label, paste(p$semantic_tokens, collapse = ", "), p$source),
        tlf_number = sec$tlf_number,
        action = "A proposal never binds. Check it against the shell, then set status to \"reviewed\" with a reviewed_by to apply it.")
      next
    }

    sec$stub_rows[[idx]]$supplement_stat_tokens   <- p$semantic_tokens
    sec$stub_rows[[idx]]$supplement_stat_source   <- p$source
    sec$stub_rows[[idx]]$supplement_stat_override <- isTRUE(p$override)
    sec$stub_rows[[idx]]$supplement_stat_reviewer <- p$reviewed_by
    n_bound <- n_bound + 1L
  }

  if (n_bound > 0 || n_proposed > 0) {
    diag_add(stage = "supplement", severity = "INFO", input = INPUT_SUPPLEMENT,
      problem = sprintf("TLF %s: %d reviewed statistic row(s) applied, %d proposal(s) recorded but not bound.",
                        sec$tlf_number %||% "?", n_bound, n_proposed),
      tlf_number = sec$tlf_number,
      action = "A reviewed row still has to resolve against its method before any cell fills.")
  }
  sec
}
