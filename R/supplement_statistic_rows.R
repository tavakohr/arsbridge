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
#'
#' Every problem this returns is FATAL, and that is the point: the entry that
#' produced one is not in `rows`, so it cannot reach a stub row and cannot
#' bind. A finding reported as FAIL and then applied anyway would make the
#' diagnostic worse than useless -- it would say the entry was dropped while
#' its tokens were being attached. If a future finding is genuinely advisory
#' it needs its own channel and its own severity, not a place in this vector.
#'
#' @return list(rows, problems); `rows` holds only entries with NO problem.
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
    ## A field the format does not define is FATAL, not ignored. The shipped
    ## schema already sets `additionalProperties: false`, so a file carrying
    ## one is invalid there; accepting it here would mean a supplement that
    ## fails its own schema still binds. And the common case is a misspelling
    ## of a field that decides something -- `overide`, `reviewd_by` -- where
    ## "ignored" means the row binds WITHOUT the qualifier its author wrote.
    unknown_fields <- setdiff(names(r), .SUPP_STAT_ROW_FIELDS)
    if (length(unknown_fields) > 0) {
      note("regenerate: %s has field%s this format does not define (%s). Known fields: %s.",
           at, if (length(unknown_fields) == 1L) "" else "s",
           paste(sprintf("'%s'", unknown_fields), collapse = ", "),
           paste(.SUPP_STAT_ROW_FIELDS, collapse = ", "))
      next
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

    ## Also fatal, and read in two steps rather than through `%|NA|%`.
    ## `.supp_scalar()` returns NA for a non-scalar, and collapsing that to ""
    ## would read `"expected_scope": ["continuous"]` as ABSENT -- the entry
    ## would bind with the safeguard silently discarded. `%|NA|%` is for
    ## provenance text, never for a field that decides something.
    ##
    ## The enum check is fatal for the same reason: this field's whole purpose
    ## is to be CHECKED against the method, so a value outside the enum is a
    ## check that cannot be performed, and the entry does not say what its
    ## author believes it says.
    scope_raw <- .supp_scalar(r$expected_scope)
    if (is.na(scope_raw)) {
      note("regenerate: %s ('%s') needs 'expected_scope' to be a single string (%s), not an array or object.",
           at, label,
           paste(sprintf("'%s'", .SUPP_STAT_SCOPES), collapse = " or ")); next
    }
    scope <- tolower(scope_raw)
    if (nzchar(scope) && !scope %in% .SUPP_STAT_SCOPES) {
      note("regenerate: %s ('%s') has an unknown 'expected_scope' '%s' (expected %s).",
           at, label, scope,
           paste(sprintf("'%s'", .SUPP_STAT_SCOPES), collapse = " or ")); next
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
      evidence  = .supp_scalar(r$evidence)  %|NA|% "",
      ## Accepted by the schema and by the field whitelist, so it is kept.
      ## A field the contract admits and the parser drops is one an author can
      ## write, watch validate clean, and never find again.
      proposed_at = .supp_scalar(r$proposed_at) %|NA|% "")
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

  ## Two entries claiming one row are AMBIGUOUS, and ambiguity resolves to
  ## unresolved -- never to whichever came first.
  ##
  ## What counts as "one row" is the row they RESOLVE to, not the text they
  ## supplied. `.match_stub_label()` matches on prefix and containment as well
  ## as on equality, so two entries can name a row differently -- "Mean" and
  ## "Mean (SD)" -- and still land on the same stub row. Comparing the
  ## supplied labels catches only the obvious half of that, and the half it
  ## misses is the dangerous one: both entries pass, both attach, and the
  ## second silently replaces the first's tokens on the row they share.
  ##
  ## So resolve every entry first, then decide. Nothing is attached until
  ## every claim on every row is known.
  keys <- vapply(parsed$rows, function(p) .norm_label(p$row_label), character(1))
  resolved_idx <- unname(vapply(keys, function(k) {
    hit <- .match_stub_label(k, labels_norm)
    if (length(hit) != 1L) NA_integer_ else as.integer(hit)
  }, integer(1)))

  contested <- rep(FALSE, length(parsed$rows))
  claimants <- function(who) {
    paste(sprintf("'%s'", vapply(parsed$rows[who], function(p) p$row_label,
                                 character(1))), collapse = ", ")
  }

  for (j in unique(resolved_idx[!is.na(resolved_idx) & duplicated(resolved_idx)])) {
    who <- which(!is.na(resolved_idx) & resolved_idx == j)
    contested[who] <- TRUE
    diag_add(stage = "supplement", severity = "FAIL", input = INPUT_SUPPLEMENT,
      problem = sprintf("Stub row '%s': %d statisticRows entries resolve to it (%s).",
                        sec$stub_rows[[j]]$label %||% "?", length(who), claimants(who)),
      tlf_number = sec$tlf_number,
      action = "None was applied -- ambiguity is never resolved by first match. Keep one entry per stub row, naming it as the shell writes it.")
  }
  ## Entries resolving to no row at all are reported one by one below, but if
  ## two of them supply the same label they are ambiguous about the row they
  ## meant, and neither should quietly become the answer if the label later
  ## starts matching.
  for (k in unique(keys[duplicated(keys)])) {
    who <- which(keys == k & is.na(resolved_idx))
    if (length(who) == 0) next
    contested[who] <- TRUE
    diag_add(stage = "supplement", severity = "FAIL", input = INPUT_SUPPLEMENT,
      problem = sprintf("%d statisticRows entries claim the row '%s', which matched no stub row.",
                        length(who), parsed$rows[[who[1]]]$row_label),
      tlf_number = sec$tlf_number,
      action = "None was applied -- keep one entry per stub row, naming it as the shell writes it.")
  }

  for (i in seq_along(parsed$rows)) {
    if (contested[i]) next
    p   <- parsed$rows[[i]]
    idx <- resolved_idx[[i]]
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
    ## Carried so the CROSS-CHECK the field promises can actually run. It is
    ## checked against the method at fill time, where the method is known --
    ## here there is no analysis yet, so there is nothing to check against.
    ## `%||%` because the field is optional: an entry that declared no scope
    ## has nothing to check and must not be read as declaring "".
    if (nzchar(p$expected_scope %||% "")) {
      sec$stub_rows[[idx]]$supplement_stat_scope <- p$expected_scope
    }
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
