## arsbridge -- utils_where_clause.R
## ---------------------------------------------------------------------------
## Converts an annotation expression (e.g. "ADSL.SAFFL='Y'",
## "ADSL.SAFFL='Y' and ADCM.CONTRTFL='Y'") into a CDISC ARS WhereClause /
## WhereClauseCondition / WhereClauseCompoundExpression object.
##
## Regex tokens .ADAM_DS, .ADAM_VAR and the `%||%` operator come from
## R/aaa_constants.R, which sources first.

## A string literal as annotated shells actually write it: single OR double
## quoted. SAS and SQL both spell a string "like this", and a lead programmer
## annotating a shell writes what they write in their programs -- so a grammar
## that accepts only 'this' silently drops half the conditions it is shown.
## That is not hypothetical: a field study annotated every table with
## [ADSL.COMPLFL="Y"], and every one of those population filters was thrown
## away without the run failing.
##
## Non-capturing, and captured WITH its quotes at each use site, so every
## pattern keeps exactly one group per literal -- the group numbering
## .one_condition() indexes by. .strip_quotes() takes them off.
.RE_QUOTED <- "(?:'[^']*'|\"[^\"]*\")"
## A number written without quotes: "1", "-2.5".
.RE_NUMBER <- "[-+]?\\d+(?:\\.\\d+)?"

## ---------------------------------------------------------------------------
## Literal masking
##
## Everything below this line that reasons about the STRUCTURE of an expression
## -- where the clauses join, which operator applies, where a range ends -- must
## not read the CONTENTS of a quoted value. The two are different languages that
## happen to share a string.
##
## Without that separation the parser reads text the author never meant as
## grammar. A value containing a joiner is the clearest case: given
##
##     ADSL.RACE='BLACK OR AFRICAN AMERICAN'
##
## the joiner scan finds " OR ", splits the expression into "ADSL.RACE='BLACK"
## and "AFRICAN AMERICAN'", and neither half parses. The condition is dropped,
## and a dropped condition does not produce an error -- it produces an
## UNRESTRICTED count, which looks exactly like a correct one.
##
## The same failure has three other doors into the same function, so the fix is
## applied once at the top rather than at each: `==` normalisation rewrote
## inside literals on the assumption that clinical values never contain "==";
## the BETWEEN protection looked for " and " anywhere; and the boilerplate strip
## matched on raw text. An assumption about what study values contain is exactly
## what breaks on the next study.
##
## So: mask every quoted literal to an opaque token, do all structural work on
## the masked text, and restore the literals before any clause is interpreted.
##
## The token delimiters are CHOSEN PER EXPRESSION rather than fixed, from code
## points the expression does not contain. A fixed pair would have to assume no
## study value ever contains it -- and "no annotation contains this character"
## is the same kind of assumption as "no value contains ==", which is the thing
## this change exists to remove. Deleting such a character from the input
## instead would be worse: it would silently alter a value that this function
## promises to preserve exactly.
##
## Candidates come from the Unicode private use area, which is reserved for
## exactly this sort of internal marker and carries no meaning in an
## annotation. Any pair chosen survives every pattern here: no spaces (the
## joiner split), no "=" (the equality normaliser), nothing in [A-Z0-9] (the
## boilerplate strip and the ADaM identifier grammar).
.MASK_CANDIDATES <- vapply(0xE000:0xE03F, intToUtf8, character(1))

#' Replace every quoted literal with an opaque token.
#'
#' The input is never modified. A delimiter is usable only if the expression
#' does not already contain it, so the pair is drawn from whatever is free --
#' deleting an inconvenient character from the input instead would silently
#' alter a value this function promises to preserve exactly.
#'
#' @return `list(text, literals, token_pattern, open, close)`. `text` is
#'   structurally identical to `expr` with each literal replaced by its token;
#'   `literals` holds the originals, quotes included, indexed by token number;
#'   `token_pattern` matches any token, for the structural patterns that must
#'   recognise one. `NULL` when no free delimiter pair exists -- the caller
#'   must treat that as "this expression cannot be analysed safely", never as
#'   "this expression has no conditions".
#' @noRd
.mask_literals <- function(expr) {
  taken <- vapply(.MASK_CANDIDATES,
                  function(ch) grepl(ch, expr, fixed = TRUE), logical(1))
  free <- .MASK_CANDIDATES[!taken]
  if (length(free) < 2L) return(NULL)
  open <- free[[1]]
  close <- free[[2]]

  matches <- gregexpr(.RE_QUOTED, expr, perl = TRUE)
  literals <- regmatches(expr, matches)[[1]]
  masked <- list(text = expr, literals = character(0),
                 token_pattern = paste0(open, "[0-9]+", close),
                 open = open, close = close)
  if (length(literals) == 0) return(masked)

  regmatches(expr, matches) <- list(paste0(open, seq_along(literals), close))
  masked$text <- expr
  masked$literals <- literals
  masked
}

## ---------------------------------------------------------------------------
## The unresolved signal
##
## `parse_where_clause()` used to answer NULL to two different questions:
## "does this annotation supply a condition?" and "could you read the
## condition it supplies?". A no to the first is ordinary -- a bare variable
## pointer, a directive, plain prose. A no to the second means a filter the
## author wrote will not be applied, and every count behind it is computed over
## the wrong records.
##
## Sharing one answer meant the second silently became the first: an unreadable
## filter executed as no filter. This is the separate answer. It carries the
## author's own text so the finding, the fix report and the editor can quote
## what was written rather than describing it.

## What makes a piece of text an attempted CONDITION rather than a variable
## with descriptive text around it.
##
## The distinction is expensive to get wrong in one direction and cheap in the
## other, which is why it is not the same predicate the warning uses. Warning
## about an annotation that turns out to be fine costs the author a moment;
## RESERVING it withholds a result that would have been correct. Real shells
## are full of qualified references with text around them that no one ever
## meant as a filter:
##
##     count of ADSL.USUBJID              a measure description
##     ADSL.AGE (unit ADSL.AGEU)          a variable and its unit
##     ADAE.ASEV / ASEVN                  a coded/decoded pair
##     ADEX.AVAL by ADEX.PARAMCD          a by-variable specification
##
## None of those states a condition, none was ever filtered, and reserving them
## would withhold results nobody needed withheld.
##
## So the evidence required is an OPERATOR: a comparison symbol, a comparator
## keyword on token boundaries, or a presence/absence test. Token boundaries
## matter -- without them "INDICATION" contains IN, and "between-group" contains
## BETWEEN, so ordinary prose would read as a filter.
## Word boundaries alone are not enough, because several comparator keywords
## are ordinary English. `\bIN\b` matches "contained in the listing"; `\bBETWEEN\b`
## matches "between-group summary". Both are prose, and both would reserve a row
## that states no filter.
##
## So each keyword is required in the SHAPE the grammar actually reads: IN and
## NOT IN take a parenthesised list, BETWEEN and CONTAINS take a following
## value. Prose uses the same words without that shape, which is what separates
## them.
.RE_CONDITION_EVIDENCE <- paste0(
  "(?i)(?:",
  ## Comparison symbols. "=" also covers "==", "<=" and ">=".
  "[<>]=?|!=|=",
  ## Two-letter comparators. Not English words, so a boundary is enough.
  "|\\b(?:EQ|NE|GT|GE|LT|LE)\\b",
  ## Value-list membership: only with the parenthesis that carries the list.
  "|\\b(?:NOT\\s+IN|NOTIN|IN)\\s*\\(",
  ## Range and substring: only with a following value.
  "|\\bBETWEEN\\s+\\S",
  "|\\bCONTAINS\\s+\\S",
  ## Presence and absence tests, in each spelling the grammar accepts.
  ## "missingness" and "nullable" do not match -- no boundary after the word.
  "|\\b(?:IS\\s+)?(?:NOT\\s+)?(?:NULL|MISSING)\\b",
  "|\\b(?:is\\.na|missing)\\s*\\(",
  ")"
)

## ---------------------------------------------------------------------------
## Derivation notes
## ---------------------------------------------------------------------------

## A shell annotation may state, after a semicolon, how the row's variable is
## DERIVED rather than how its records are FILTERED:
##
##   [ADQX.MEASDUR; = ADQX.ENDDY]
##
## Read as a filter, that clause says nothing this grammar can evaluate -- it
## compares a variable to another variable, and an ARS condition compares a
## variable to VALUES -- so the row reserved, and (because a filtered row is
## counted rather than summarised) its whole block was typed as a subject
## count. A continuous summary then read its own statistic rows as category
## levels. One misread separator, a block of unfillable cells.
##
## What separates a derivation note from a filter is structural, and both
## halves are load-bearing:
##
##   1. the clause has NO left operand -- it opens with the operator, so it
##      states a relationship rather than restricting a named variable; and
##   2. its right operand is a QUALIFIED ADaM reference, not a value.
##
## Either half alone would be wrong. Without (1), `ADQX.FLAG=Y` -- an ordinary
## filter -- would be swept in. Without (2), `; >= 65` would be, and that IS a
## filter: an author writing a threshold after the semicolon means "this
## variable, restricted", with the left operand understood.
##
## Equality only, deliberately. "Is defined as" is what `=` says; `>=` between
## two variables is a comparison whose intent this package cannot settle, so it
## keeps today's behaviour and reserves.
.RE_DERIVATION_NOTE <- paste0(
  "^\\s*(?:=|==|EQ)\\s*", .ADAM_DS, "\\.", .ADAM_VAR, "\\s*$"
)

#' Split an annotation into its head and a trailing derivation note.
#'
#' Returns `list(head, note)`; `note` is `""` unless the annotation is exactly
#' a qualified reference, a semicolon, and a derivation clause. Anything else
#' -- a second condition, prose, more than one semicolon -- is returned
#' unchanged, so this can only ever remove the one form it recognises.
#'
#' Quoting needs no masking here: the head must match the ADaM reference
#' grammar and the note the pattern above, and neither admits a quote.
#' @noRd
.split_derivation_note <- function(ann) {
  txt <- as.character(ann %||% "")
  txt <- if (length(txt) == 0L) "" else txt[[1]]
  if (is.na(txt) || !nzchar(trimws(txt))) return(list(head = "", note = ""))

  parts <- strsplit(txt, ";", fixed = TRUE)[[1]]
  if (length(parts) != 2L) return(list(head = txt, note = ""))
  head <- trimws(parts[[1]]); note <- trimws(parts[[2]])

  ref <- paste0("^", .ADAM_DS, "\\.", .ADAM_VAR, "$")
  if (!grepl(ref, head, perl = TRUE)) return(list(head = txt, note = ""))
  if (!grepl(.RE_DERIVATION_NOTE, note, perl = TRUE)) return(list(head = txt, note = ""))
  list(head = head, note = note)
}

#' The annotation with any derivation note removed.
#' @noRd
.annotation_less_derivation_note <- function(ann) {
  .split_derivation_note(ann)$head
}

#' Does this text attempt to express a condition?
#'
#' Read on MASKED text, so an operator inside a quoted value cannot be mistaken
#' for one in the expression -- `ADQX.NOTE='a=b'` states a condition because of
#' the outer `=`, not the inner one, and a label like `'up to 5%'` states none.
#' @noRd
.has_condition_evidence <- function(text) {
  masked <- .mask_literals(as.character(text)[1])
  ## No free delimiter pair means the values cannot be separated from the
  ## structure, so no honest answer is available from the text alone. Treated
  ## as evidence: the caller's other branch reserves, which is the safe
  ## direction when the question cannot be settled.
  if (is.null(masked)) return(TRUE)
  grepl(.RE_CONDITION_EVIDENCE, masked$text, perl = TRUE)
}

#' A condition that was supplied but could not be read.
#'
#' @param text The original expression, exactly as the author wrote it.
#' @param dropped The individual clauses that failed, for the message.
#' @noRd
.unresolved_condition <- function(text, dropped = character(0)) {
  structure(
    list(text = as.character(text)[1], dropped = as.character(dropped)),
    class = "arsbridge_unresolved_condition"
  )
}

#' Is this an unresolved condition rather than a WhereClause?
#'
#' Every consumer of `parse_where_clause()` must ask this before treating the
#' result as a condition: the object is deliberately NOT a valid WhereClause,
#' so writing it into an ARS would produce a malformed event rather than a
#' silently wrong number.
#' @noRd
.is_unresolved_condition <- function(x) {
  inherits(x, "arsbridge_unresolved_condition")
}

#' The author's text for an unresolved condition, or `NA`.
#' @noRd
.unresolved_condition_text <- function(x) {
  if (!.is_unresolved_condition(x)) return(NA_character_)
  txt <- x$text
  if (length(txt) == 0 || is.na(txt) || !nzchar(txt)) return(NA_character_)
  txt
}

#' Put the quoted literals back, exactly as they were written.
#' @noRd
.unmask_literals <- function(text, masked) {
  literals <- masked$literals %||% character(0)
  if (length(literals) == 0 || !length(text)) return(text)
  for (i in seq_along(literals)) {
    token <- paste0(masked$open, i, masked$close)
    text <- gsub(token, literals[[i]], text, fixed = TRUE)
  }
  text
}

## Single condition: "ADSL.SAFFL='Y'" (also matches ARS-style "EQ 'Y'")
.RE_CONDITION_EQ <- paste0(
  "(", .ADAM_DS, ")\\.(", .ADAM_VAR, ")",
  "\\s*=\\s*(", .RE_QUOTED, ")"
)
.RE_CONDITION_ARS <- paste0(
  "(", .ADAM_DS, ")\\.(", .ADAM_VAR, ")",
  "\\s+(EQ|NE|IN|NOTIN|GT|GE|LT|LE)\\s+(", .RE_QUOTED, ")"
)
## Unquoted numeric comparison: "ADSL.AGE GE 65".
.RE_CONDITION_NUM <- paste0(
  "(", .ADAM_DS, ")\\.(", .ADAM_VAR, ")",
  "\\s+(EQ|NE|GT|GE|LT|LE)\\s+(", .RE_NUMBER, ")\\b"
)
## Unquoted numeric equality: "ADSL.COHORTN=1" (the usual column-header
## annotation form). The quoted form must win when both could apply, so
## .one_condition() tries this only after .RE_CONDITION_EQ.
.RE_CONDITION_EQ_NUM <- paste0(
  "(", .ADAM_DS, ")\\.(", .ADAM_VAR, ")",
  "\\s*=\\s*(", .RE_NUMBER, ")\\b"
)
## One item of a value list: a quoted string or a bare number. A coded column
## axis is written "ADSL.COHORTN IN (1,2)" far more often than it is written
## with quotes around the codes, and requiring the quotes cost this exact
## annotation its Total column.
.RE_IN_ITEM <- paste0("(?:", .RE_QUOTED, "|", .RE_NUMBER, ")")
## Multi-value list: "ADSL.RACE IN ('WHITE','ASIAN')" / "NOT IN (...)".
.RE_CONDITION_IN_LIST <- paste0(
  "(", .ADAM_DS, ")\\.(", .ADAM_VAR, ")",
  "\\s+(?i:(NOT\\s*IN|NOTIN|IN))\\s*\\(\\s*(",
  .RE_IN_ITEM, "(?:\\s*,\\s*", .RE_IN_ITEM, ")*", ")\\s*\\)"
)
## Range: "ADSL.AGE between 18 ~AND~ 65" (the inner "and" is replaced with
## the ~AND~ marker by parse_where_clause BEFORE joiner splitting so the
## range is not torn apart). Values quoted or numeric.
.RE_BETWEEN <- paste0(
  "(", .ADAM_DS, ")\\.(", .ADAM_VAR, ")",
  "\\s+(?i:between)\\s+(", .RE_QUOTED, "|", .RE_NUMBER, ")",
  "\\s+~AND~\\s+(", .RE_QUOTED, "|", .RE_NUMBER, ")"
)
## Substring: "ADAE.AETERM contains 'rash'". CONTAINS is an arsbridge
## extension -- not in the ARS v1.0 ConditionComparatorEnum.
.RE_CONTAINS <- paste0(
  "(", .ADAM_DS, ")\\.(", .ADAM_VAR, ")",
  "\\s+(?i:contains)\\s+(", .RE_QUOTED, ")"
)
.RE_NULL_CHECK <- paste0(
  "(", .ADAM_DS, ")\\.(", .ADAM_VAR, ")",
  "\\s+(?:is\\s+)?not\\s+(?:null|missing)"
)
## Positive null check: "ADSL.DTHDT is null" / "ADSL.DTHDT missing".
.RE_IS_NULL <- paste0(
  "(", .ADAM_DS, ")\\.(", .ADAM_VAR, ")",
  "\\s+(?:is\\s+)?(?:null|missing)\\b"
)
## Call-form missing checks as annotated shells actually write them:
## R's "is.na(ADSL.COHORTN)" and SAS's "missing(COHORTN)", plus the negations
## "!is.na(...)" / "not missing(...)". The negated form embeds the positive
## one, so .one_condition() must test .RE_ISNA_NEG first.
.RE_ISNA_NEG <- paste0(
  "(?:!|\\bnot\\b)\\s*(?:is\\.na|missing)\\s*\\(\\s*",
  "(", .ADAM_DS, ")\\.(", .ADAM_VAR, ")\\s*\\)"
)
.RE_ISNA_POS <- paste0(
  "(?:is\\.na|missing)\\s*\\(\\s*",
  "(", .ADAM_DS, ")\\.(", .ADAM_VAR, ")\\s*\\)"
)

## ---------------------------------------------------------------------------
## Structures this grammar cannot represent
## ---------------------------------------------------------------------------

## Atomic forms that legitimately contain a parenthesis or the word NOT, and
## so must be taken out of the text before what REMAINS can be read as
## structure. `IN (...)` carries a value list, `is.na(...)` a call, `not null`
## a presence test -- none of them groups or negates a sub-expression.
##
## Order matters: the negated call form embeds the positive one, so it is
## removed first, exactly as `.one_condition()` tries it first.
.RE_NON_STRUCTURAL_PARTS <- c(
  "(?i)(?:!|\\bnot\\b)\\s*(?:is\\.na|missing)\\s*\\([^()]*\\)",
  "(?i)\\b(?:is\\.na|missing)\\s*\\([^()]*\\)",
  "(?i)\\b(?:NOT\\s*IN|NOTIN|IN)\\s*\\([^()]*\\)",
  "(?i)\\b(?:is\\s+)?not\\s+(?:null|missing)\\b"
)

#' Remove parentheses that wrap the entire expression, however many deep.
#'
#' Only when the opening bracket's match is the LAST character: "(A OR B) AND
#' C" opens with "(" and ends with "C", and its first bracket closes in the
#' middle, so nothing is stripped and the group is still seen.
#' @noRd
.strip_outer_parens <- function(txt) {
  repeat {
    trimmed <- trimws(txt)
    if (nchar(trimmed) < 2L || substr(trimmed, 1L, 1L) != "(") return(trimmed)
    chars <- strsplit(trimmed, "", fixed = TRUE)[[1]]
    depth <- 0L
    close_at <- NA_integer_
    for (i in seq_along(chars)) {
      if (chars[[i]] == "(") depth <- depth + 1L
      if (chars[[i]] == ")") {
        depth <- depth - 1L
        if (depth == 0L) { close_at <- i; break }
      }
    }
    ## Unbalanced, or the opener closes before the end: not a wrapper.
    if (is.na(close_at) || close_at != length(chars)) return(trimmed)
    txt <- substr(trimmed, 2L, close_at - 1L)
  }
}

#' Name the construct that would be read with the wrong meaning, or `NULL`.
#'
#' Read on MASKED text, after `==` normalisation and after BETWEEN's inner
#' "and" has become `~AND~` -- so a parenthesis, a joiner or the word NOT
#' inside a quoted value cannot reach this, and a range is not mistaken for a
#' conjunction.
#'
#' Three constructs, and each is refused for the same reason: the flat split
#' below has no representation for it, so it does not fail on one -- it
#' succeeds with a different expression.
#'
#'   grouping    `A AND (B OR C)` flattens to `AND(A, B, C)`.
#'   negation    a bare `NOT` is dropped, inverting nothing.
#'   mixed       `A AND B OR C` takes whichever joiner is tested first for the
#'               whole expression, so one of the two is silently rewritten.
#'
#' A parenthesis is structure-bearing only when what it encloses could change
#' the reading -- an operator or a joiner. A unit or a note in brackets
#' ("ADQX.MEASURE (mg)") encloses neither and is left alone, because reserving
#' a row that states no such construct withholds a result that would have been
#' correct.
#'
#' `!=` is a COMPARATOR, not a negation: the exclamation mark that opens it
#' belongs to the operator and negates no sub-expression. Reported as
#' negation it would name the wrong construct, and send the author to remove
#' something they did not write. (Whether this grammar reads `!=` at all is a
#' separate question, answered by the clause parser below, which drops it as
#' unreadable -- the honest reason.)
#' @noRd
.unsupported_structure <- function(expr, token_pattern = NULL) {
  txt <- as.character(expr %||% "")
  if (length(txt) != 1L || is.na(txt) || !nzchar(trimws(txt))) return(NULL)

  ## What is left once every atomic form is taken out is the structure.
  residue <- txt
  for (re in .RE_NON_STRUCTURAL_PARTS) {
    residue <- gsub(re, " ", residue, perl = TRUE)
  }
  ## The BETWEEN marker is this parser's own, not the author's conjunction.
  residue <- gsub("~AND~", " ", residue, fixed = TRUE)

  ## Parentheses around the WHOLE expression group nothing: there is no second
  ## operand for them to bind against. "(ADSL.SAFFL='Y')" is how a great many
  ## shells write an ordinary population, and reserving those would withhold
  ## every result in every table that uses one.
  residue <- .strip_outer_parens(residue)

  ## Grouping: a span binds a sub-expression only if it ENCLOSES a joiner.
  ## That is what makes it capable of changing the reading -- `(B OR C)` inside
  ## an AND is a different restriction from `B OR C` spread flat across it.
  ##
  ## What the span sits next to is irrelevant, and reading it that way was
  ## wrong: `A AND (B)` and `(A) AND (B)` put a bracket around a single
  ## condition, which groups nothing and means exactly what it says. Refusing
  ## those would withhold correct results for a punctuation habit.
  ##
  ## `(A AND B) AND C` is refused even though flattening it happens to give the
  ## same answer, because knowing that requires reasoning about associativity
  ## that this check does not do. Refusing a representable expression costs a
  ## reservation; accepting an unrepresentable one costs a wrong number.
  joiner_re <- "(?i)\\b(?:and|or)\\b|&|\\|"
  spans <- regmatches(residue,
                      gregexpr("\\([^()]*\\)", residue, perl = TRUE))[[1]]
  for (span in spans) {
    inner <- substr(span, 2L, nchar(span) - 1L)
    if (grepl(joiner_re, inner, perl = TRUE)) {
      return("grouped sub-expressions")
    }
  }

  ## Negation of anything other than the presence tests removed above. The
  ## "!" branch excludes "!=", whose "!" is part of a comparator.
  if (grepl("(?i)\\bnot\\b|!(?!=)", residue, perl = TRUE)) {
    return("negation")
  }

  ## Both joiners in one expression: precedence decides the meaning, and this
  ## grammar has none.
  has_and <- grepl("(?i)\\s(?:and|&&)\\s|\\s&\\s", residue, perl = TRUE)
  has_or  <- grepl("(?i)\\s(?:or|\\|\\|)\\s|\\s\\|\\s", residue, perl = TRUE)
  if (has_and && has_or) return("mixed AND/OR without explicit grouping")

  NULL
}

#' Build an ARS WhereClause object from an annotation expression.
#'
#' Returns NULL if no parseable condition is found (caller should treat as
#' "no condition / all subjects"). Returns a single Condition object for a
#' simple expression, or a CompoundExpression wrapping multiple conditions
#' joined by AND / OR / NOT.
#'
#' Supported inputs:
#'   "ADSL.SAFFL='Y'"
#'   "ADSL.SAFFL='Y' and ADCM.CONTRTFL='Y'"
#'   "ADSL.SFENRLFL='Y' or ADSL.WTHTYP='Withdrawal Prior to Treatment'"
#'   "ADSL.PARAMCD EQ 'OS'"
#'   "ADSL.AGE GE 65"                       (unquoted numeric)
#'   "ADSL.COHORTN=1"                        (unquoted numeric equality)
#'   "ADSL.RACE IN ('WHITE','ASIAN')"        (multi-value list)
#'   "ADSL.AGE between 18 and 65"            (-> GE/LE compound)
#'   "ADAE.AETERM contains 'rash'"           (CONTAINS extension)
#'   "ADSL.DCSREAS not missing" / "ADSL.DTHDT is null"
#'   "is.na(ADSL.COHORTN)" / "missing(ADSL.COHORTN)"  (call-form missing)
#'   "!is.na(ADSL.COHORTN)" / "not missing(ADSL.COHORTN)"  (call-form present)
#'
#' @noRd
parse_where_clause <- function(expr) {
  expr <- trimws(expr %||% "")
  ## Empty in, `NULL` out, and that is the honest answer: no condition was
  ## supplied. Only a non-empty expression can be UNRESOLVED, which is why the
  ## marker downstream is never written as an empty string.
  if (!nzchar(expr)) return(NULL)

  ## Kept for the unresolved signal: every step below rewrites `expr`, and what
  ## the finding and the fix report must quote is what the author wrote.
  original <- expr

  ## Everything from here to the split reads STRUCTURE, so every quoted value
  ## is masked to an opaque token first and restored once the clauses are
  ## separated. See the masking block near the top of this file for why each of
  ## the four steps below was individually unsafe on raw text.
  masked <- .mask_literals(expr)
  ## Without a free delimiter pair the structure cannot be separated from the
  ## values, so the expression cannot be read at all -- which is exactly what
  ## `unresolved` says. It reserves rather than computing, so this path is now
  ## safe and not merely visible.
  if (is.null(masked)) {
    diag_add(
      stage = "where_clause", severity = "WARN",
      problem = "Condition could not be separated from its quoted values",
      location = expr,
      action = paste("Results reserved -- the annotation uses private-use",
                     "characters this parser reserves for internal markers.",
                     "Remove them and re-run.")
    )
    return(.unresolved_condition(original))
  }
  expr <- masked$text

  ## Normalise the R/Python double-equals equality operator to the single "="
  ## the grammar below expects (shells and supplements write both
  ## "ADSL.COHORTN=99" and "ADSL.COHORTN==99"). "!=", ">=" and "<=" never
  ## contain the "==" substring, so they are left untouched. A literal
  ## containing "==" is untouched too, because literals are masked -- this no
  ## longer rests on an assumption about what study values contain.
  expr <- gsub("==", "=", expr, fixed = TRUE)

  ## Strip leading/trailing "unique USUBJID in DATASET where" boilerplate so
  ## what remains is just the conditional payload.
  expr <- sub("^(?i)\\s*unique\\s+USUBJID\\s+in\\s+[A-Z0-9]+\\s+where\\s+",
              "", expr, perl = TRUE)

  ## Protect BETWEEN's inner "and" with a marker BEFORE joiner splitting
  ## ("AGE between 18 and 65" must not be torn into two clauses). The bounds
  ## may be numbers, which are still bare here, or quoted values, which are
  ## now tokens -- so both spellings are matched.
  expr <- gsub(paste0("(?i)(between\\s+(?:", masked$token_pattern, "|",
                      .RE_NUMBER, "))\\s+and\\s+"),
               "\\1 ~AND~ ", expr, perl = TRUE)

  ## Constructs whose MEANING this parser cannot carry. Refused before any
  ## clause is read, because the split below is a flat one: it has no notion of
  ## precedence, grouping or negation, so it answers such an expression with a
  ## tree that is valid, executable, and not what the author wrote.
  ##
  ## Refusing is not a limitation added here -- it is the limitation that was
  ## always present, made visible. Until now `A AND (B OR C)` became
  ## `AND(A, B, C)`, which restricts differently and reports nothing; a
  ## population written that way emitted an unsatisfiable analysis set. An
  ## expression this parser cannot represent must reserve, exactly like one it
  ## cannot read at all -- the two are the same failure to the reader of the
  ## number.
  ## Only an expression that ATTEMPTS a condition can be refused for its
  ## structure. Prose carries English words this check reads as grammar --
  ## "not applicable", "safety population or better" -- and reserving a row
  ## whose annotation states no filter withholds a result that was never at
  ## risk. Same evidence rule the dropped-clause branch below applies, and for
  ## the same reason.
  unsupported <- if (grepl(.RE_CONDITION_EVIDENCE, expr, perl = TRUE)) {
    .unsupported_structure(expr, masked$token_pattern)
  } else {
    NULL
  }
  if (!is.null(unsupported)) {
    diag_add(
      stage = "where_clause", severity = "WARN",
      problem = sprintf("Condition uses %s, which this grammar cannot represent",
                        unsupported),
      location = original,
      action = paste("Results are reserved rather than computed, because the",
                     "expression would otherwise be read with a different",
                     "meaning. Restate it without it, or supply a typed",
                     "condition through the supplement.")
    )
    return(.unresolved_condition(original, original))
  }

  ## Detect logical joiner -- "and"/"&"/"AND" produce AND; "or"/"|"/"OR" → OR.
  ## Reading masked text, so a joiner word inside a value cannot be mistaken
  ## for the joiner between two clauses.
  joiner <- NULL
  if (grepl("\\s+(?i:and|&&|and)\\s+|\\s&\\s", expr, perl = TRUE)) joiner <- "AND"
  if (is.null(joiner) &&
      grepl("\\s+(?i:or|\\|\\|)\\s+|\\s\\|\\s", expr, perl = TRUE))  joiner <- "OR"

  ## Split into atomic clauses on the joiner, if any.
  parts <- if (!is.null(joiner)) {
    strsplit(expr, "\\s+(?i:and|or)\\s+", perl = TRUE)[[1]]
  } else {
    expr
  }
  ## Literals restored before any clause is interpreted or reported: from here
  ## on the text is the author's own again, so `.one_condition()` sees real
  ## values and a dropped-condition diagnostic quotes what they actually wrote.
  parts <- .unmask_literals(trimws(parts), masked)
  parts <- parts[nzchar(parts)]

  conditions <- lapply(parts, .one_condition)
  unparsed   <- parts[vapply(conditions, is.null, logical(1))]
  conditions <- Filter(Negate(is.null), conditions)

  ## Anything that survived boilerplate-stripping but didn't parse into a
  ## condition is silently weaker filtering downstream -- record it. Skip
  ## parts with no DATASET.VARIABLE shape at all (plain prose like
  ## "Safety Population" is not a condition attempt), AND skip a BARE
  ## DATASET.VARIABLE reference with no operator (e.g. a stub's analysis-
  ## variable annotation "ADSL.AGEGR1") -- that is a variable pointer, not a
  ## filter, so "no condition" is correct, not a parse failure.
  ## A part is an ATTEMPTED condition (worth warning) only when something
  ## remains after its DATASET.VARIABLE token -- an operator, value, or stray
  ## comparator like "like". A token alone is a bare variable pointer.
  is_attempt <- function(s) {
    rest <- sub(paste0(.ADAM_DS, "\\.", .ADAM_VAR), "", s, perl = TRUE)
    nzchar(trimws(rest))
  }
  ## A clause that is a DIRECTIVE, not a condition. "once/subject ADAE.AOCCIFL"
  ## names a variable and carries text around it, so it looks like an attempted
  ## filter -- but it has its own consumer (.once_per_subject_var(), which
  ## routes the row to the distinct-subject method) and nothing was dropped.
  ## Warning about it told the author to fix an annotation the package
  ## understood perfectly.
  is_directive <- function(s) !is.null(.once_per_subject_var(s))
  ## A clause of a CONJUNCTION (or disjunction) is held to a lower bar than a
  ## lone annotation, and deliberately.
  ##
  ## The qualified-reference requirement above exists for the single-clause
  ## case, where an annotation is as likely to be a variable pointer with
  ## descriptive text as it is to be a filter. Inside `A and B` that ambiguity
  ## is gone: the author joined two things with "and", so both are clauses. A
  ## clause carrying an operator that this grammar cannot read is therefore a
  ## dropped condition even when it names its variable without a dataset --
  ##
  ##     ADQX.QXTRT WHERE QXCAT='X' AND QXPRESP='Y' AND QXOCCUR='Y'
  ##
  ## which is how shells routinely write a filter after the head reference.
  ## Only the first clause was qualified, the other two matched no pattern,
  ## and -- carrying no DATASET.VARIABLE -- they were not even counted as
  ## dropped. The row filtered on one third of what the author wrote, and
  ## nothing said so.
  joined <- !is.null(joiner)
  dropped <- character(0)
  for (u in unparsed) {
    if (is_directive(u)) next
    if (joined && .has_condition_evidence(u)) {
      dropped <- c(dropped, u)
      diag_add(
        stage = "where_clause", severity = "WARN",
        problem = "A clause of a joined condition could not be read",
        location = u,
        action = paste("Results are reserved rather than computed on the",
                       "clauses that did parse: a conjunction missing a term",
                       "keeps more records than the author asked for, and a",
                       "disjunction missing one keeps fewer.")
      )
      next
    }
    if (grepl(paste0(.ADAM_DS, "\\.", .ADAM_VAR), u, perl = TRUE) && is_attempt(u)) {
      dropped <- c(dropped, u)
      diag_add(
        stage = "where_clause", severity = "WARN",
        problem = "Condition could not be parsed into an ARS WhereClause",
        location = u,
        action = "Condition dropped -- filtering will be weaker than the annotation intends (supported: =, EQ/NE/IN/NOTIN/GT/GE/LT/LE incl. unquoted numerics, IN lists of quoted values or bare numbers, BETWEEN x AND y, CONTAINS 'text', is/not null/missing, is.na()/missing() incl. negation; string values may be 'single' or \"double\" quoted)"
      )
    }
  }

  ## An attempted condition this grammar could not read makes the WHOLE
  ## expression unresolved -- including when other clauses parsed perfectly.
  ##
  ## Returning the clauses that did parse is the tempting behaviour and it is
  ## the dangerous one. "A and B" that loses B restricts LESS than the author
  ## wrote, so it over-counts; "A or B" that loses B restricts MORE, so it
  ## under-counts. Both produce a number, and neither looks wrong. There is no
  ## safe way to partially honour a filter.
  ##
  ## `unresolved` is not `NULL`. `NULL` says "this annotation supplies no
  ## condition", which is a legitimate, common answer -- a bare variable
  ## pointer, a directive, plain prose, an empty string. `unresolved` says
  ## "this annotation supplies a condition I could not read", which must
  ## reserve rather than compute. Collapsing the two is what let an unreadable
  ## filter execute as no filter at all.
  ##
  ## Reserving requires stronger evidence than warning does, and deliberately
  ## so. The warning above fires whenever anything follows a qualified
  ## reference; that is right for a note and wrong for a reservation, because
  ## "ADQX.MEASURE (unit ADQX.UNIT)" follows a reference and states no filter.
  ## Withholding a correct result is its own wrong answer, so only text
  ## carrying an actual operator reserves.
  unreadable <- Filter(.has_condition_evidence, dropped)
  if (length(unreadable) > 0) {
    return(.unresolved_condition(original, unreadable))
  }

  if (length(conditions) == 0) return(NULL)
  if (length(conditions) == 1) return(conditions[[1]])

  list(
    compoundExpression = list(
      logicalOperator = joiner %||% "AND",
      whereClauses    = conditions
    )
  )
}

#' Parse one atomic clause into an ARS WhereClauseCondition object (or, for
#' BETWEEN, a compoundExpression of GE + LE). Branch order matters: more
#' specific forms before less specific ones.
#' @noRd
.one_condition <- function(part) {
  ## Either quote character, and only when the two ends match -- so a value
  ## that legitimately contains the other quote ("O'Brien") survives intact.
  strip_q <- function(x) sub("^(['\"])(.*)\\1$", "\\2", x)

  ## Range: DATASET.VARIABLE between lo ~AND~ hi -> (GE lo) AND (LE hi).
  ## ARS v1.0 has no BETWEEN comparator, so emit the conformant compound.
  m <- regmatches(part, regexec(.RE_BETWEEN, part, perl = TRUE))[[1]]
  if (length(m) == 5) {
    return(list(
      compoundExpression = list(
        logicalOperator = "AND",
        whereClauses    = list(
          .cond(m[2], m[3], "GE", strip_q(m[4])),
          .cond(m[2], m[3], "LE", strip_q(m[5]))
        )
      )
    ))
  }
  ## Multi-value list: DATASET.VARIABLE IN ('a','b') / NOT IN ('a','b')
  m <- regmatches(part, regexec(.RE_CONDITION_IN_LIST, part, perl = TRUE))[[1]]
  if (length(m) == 5) {
    comp <- if (grepl("NOT", toupper(m[4]))) "NOTIN" else "IN"
    vals <- regmatches(m[5], gregexpr(.RE_IN_ITEM, m[5], perl = TRUE))[[1]]
    vals <- vapply(vals, strip_q, character(1), USE.NAMES = FALSE)
    return(.cond_multi(m[2], m[3], comp, vals))
  }
  ## ARS-style: DATASET.VARIABLE EQ 'value'
  m <- regmatches(part, regexec(.RE_CONDITION_ARS, part, perl = TRUE))[[1]]
  if (length(m) == 5) {
    return(.cond(m[2], m[3], m[4], strip_q(m[5])))
  }
  ## Unquoted numeric: DATASET.VARIABLE GE 65
  m <- regmatches(part, regexec(.RE_CONDITION_NUM, part, perl = TRUE))[[1]]
  if (length(m) == 5) {
    return(.cond(m[2], m[3], m[4], m[5]))
  }
  ## Equality: DATASET.VARIABLE='value'
  m <- regmatches(part, regexec(.RE_CONDITION_EQ, part, perl = TRUE))[[1]]
  if (length(m) == 4) {
    return(.cond(m[2], m[3], "EQ", strip_q(m[4])))
  }
  ## Unquoted numeric equality: DATASET.VARIABLE=1
  m <- regmatches(part, regexec(.RE_CONDITION_EQ_NUM, part, perl = TRUE))[[1]]
  if (length(m) == 4) {
    return(.cond(m[2], m[3], "EQ", m[4]))
  }
  ## Substring: DATASET.VARIABLE contains 'text' (arsbridge extension).
  m <- regmatches(part, regexec(.RE_CONTAINS, part, perl = TRUE))[[1]]
  if (length(m) == 4) {
    diag_add(
      stage = "where_clause", severity = "INFO",
      problem = "CONTAINS comparator emitted (arsbridge extension, not in the ARS v1.0 ConditionComparatorEnum)",
      location = part,
      action = "ars_to_ard() executes it as a case-insensitive substring match; external ARS consumers may reject it"
    )
    return(.cond(m[2], m[3], "CONTAINS", strip_q(m[4])))
  }
  ## Call-form missing checks: "!is.na(...)" / "not missing(...)" BEFORE the
  ## positive "is.na(...)" / "missing(...)" (the negated form embeds it).
  m <- regmatches(part, regexec(.RE_ISNA_NEG, part, ignore.case = TRUE, perl = TRUE))[[1]]
  if (length(m) == 3) {
    return(.cond(m[2], m[3], "NE", NA_character_))
  }
  m <- regmatches(part, regexec(.RE_ISNA_POS, part, ignore.case = TRUE, perl = TRUE))[[1]]
  if (length(m) == 3) {
    return(.cond(m[2], m[3], "EQ", NA_character_))
  }
  ## Null checks: "not null/missing" BEFORE the positive form.
  m <- regmatches(part, regexec(.RE_NULL_CHECK, part, ignore.case = TRUE, perl = TRUE))[[1]]
  if (length(m) == 3) {
    return(.cond(m[2], m[3], "NE", NA_character_))
  }
  m <- regmatches(part, regexec(.RE_IS_NULL, part, ignore.case = TRUE, perl = TRUE))[[1]]
  if (length(m) == 3) {
    return(.cond(m[2], m[3], "EQ", NA_character_))
  }
  NULL
}

## ---------------------------------------------------------------------------
## What a row's filter restricts
## ---------------------------------------------------------------------------

## The five answers to "what does this restriction do to the row's own
## variable?". They describe the RESTRICTION and nothing else: which statistic
## the row asks for is a different question, answered from the row's display
## and the variable's metadata, never from this alone.
##
## Keeping them apart is the whole point. The conflation this replaces read
## "I could not flatten the filter" as "the filter must be on the primary
## variable", and typed the row as a subject count on the strength of it -- so
## an unreadable annotation silently became a count of something, and a
## continuous summary scoped by a parameter became a count of subjects.
##
##   none               no restriction stated.
##   on_primary         every atom restricts the row's own variable.
##   scoping_other      no atom does; the filter only chooses the records.
##   mixed_conjunctive  both, joined by AND at every level -- so the atoms on
##                      the primary variable and the ones that merely scope
##                      can be separated, and each half means what it says.
##   unknown            the filter could not be read, or mixes both kinds
##                      under an OR/NOT where no such separation is valid.
##                      NEVER selects a method.
.FILTER_ROLES <- c("none", "on_primary", "scoping_other",
                   "mixed_conjunctive", "unknown")

#' Every atomic condition in a WhereClause tree, in document order.
#' @noRd
.where_atoms <- function(where) {
  if (is.null(where)) return(list())
  if (!is.null(where[["condition"]])) return(list(where[["condition"]]))
  compound <- where[["compoundExpression"]]
  if (!is.null(compound)) {
    return(unlist(lapply(compound[["whereClauses"]] %||% list(), .where_atoms),
                  recursive = FALSE))
  }
  ## A bare condition object (no wrapper) -- the shape some callers hold.
  if (!is.null(where[["variable"]])) return(list(where))
  list()
}

#' TRUE when every join in the tree is an AND.
#'
#' Only under an all-AND tree may the atoms be considered independently: each
#' one restricts the result on its own. Under an OR they do not -- the row's
#' variable may be unrestricted on the branch that was taken -- so an
#' expression containing one cannot be split into "the part about my variable"
#' and "the part that scopes".
#' @noRd
.where_all_conjunctive <- function(where) {
  if (is.null(where)) return(TRUE)
  compound <- where[["compoundExpression"]]
  if (is.null(compound)) return(TRUE)
  if (!identical(.as_scalar_char(compound[["logicalOperator"]]) %||% "", "AND")) {
    return(FALSE)
  }
  all(vapply(compound[["whereClauses"]] %||% list(),
             .where_all_conjunctive, logical(1)))
}

#' Classify what a filter restricts, relative to the row's own variable.
#'
#' @param where The row's EFFECTIVE filter -- the clause that will actually be
#'   emitted -- or an unresolved-condition object, or `NULL`.
#' @param dataset,variable The row's primary reference. Both are compared:
#'   two ADaM datasets may carry a variable of the same name, and in a table
#'   whose rows come from different domains, matching on the name alone would
#'   call a filter "on the primary variable" because some other dataset's
#'   variable is spelled the same.
#' @return One of `.FILTER_ROLES`.
#' @noRd
.filter_role <- function(where, dataset = NULL, variable = NULL) {
  if (.is_unresolved_condition(where)) return("unknown")
  atoms <- .where_atoms(where)
  if (length(atoms) == 0) return("none")

  var <- toupper(as.character(variable %||% ""))
  ds  <- toupper(as.character(dataset  %||% ""))
  ## With no primary reference to compare against there is no answer to give,
  ## and "scoping_other" would be a guess that happens to be the permissive
  ## one. The row reserves instead.
  if (!nzchar(var)) return("unknown")

  on_primary <- vapply(atoms, function(a) {
    a_var <- toupper(.as_scalar_char(a[["variable"]]) %||% "")
    a_ds  <- toupper(.as_scalar_char(a[["dataset"]])  %||% "")
    identical(a_var, var) && (!nzchar(ds) || !nzchar(a_ds) || identical(a_ds, ds))
  }, logical(1))

  if (all(on_primary)) return("on_primary")
  if (!any(on_primary)) return("scoping_other")
  if (.where_all_conjunctive(where)) return("mixed_conjunctive")
  "unknown"
}

#' Does the restriction hold the row's own variable at ONE value?
#'
#' A separate question from the role, deliberately. The role says WHICH
#' variables a restriction speaks about; this says what it does to the row's
#' own -- and only the second can tell a line that reports a distribution from
#' a line that reports one state.
#'
#'   ADQX.QXSEV='SEVERE' AND ADQX.QXFL='Y'    pinned: one value survives the
#'                                            filter, so there is nothing left
#'                                            to distribute over. The line
#'                                            reports how many.
#'   ADQX.QXVAL WHERE QXPARM='P1'
#'              AND ADQX.QXVAL GT 0           bounded: QXVAL still takes many
#'                                            values inside the subset, so the
#'                                            line still summarises it.
#'
#' Only equality pins: `EQ` with a single value, or `IN` with a list of one.
#' A threshold, a range, an inequality and a presence test all leave the
#' variable free to vary, and a row whose variable can still vary is not
#' reporting a single count of it.
#'
#' Read only under an all-AND tree: under an OR a "pinning" conjunct may sit on
#' a branch that was not taken, so it pins nothing.
#' @noRd
.filter_pins_primary <- function(where, dataset = NULL, variable = NULL) {
  var <- toupper(as.character(variable %||% ""))
  ds  <- toupper(as.character(dataset  %||% ""))
  if (!nzchar(var) || .is_unresolved_condition(where)) return(FALSE)
  if (!.where_all_conjunctive(where)) return(FALSE)

  atoms <- .where_atoms(where)
  if (length(atoms) == 0) return(FALSE)

  any(vapply(atoms, function(a) {
    a_var <- toupper(.as_scalar_char(a[["variable"]]) %||% "")
    a_ds  <- toupper(.as_scalar_char(a[["dataset"]])  %||% "")
    if (!identical(a_var, var)) return(FALSE)
    if (nzchar(ds) && nzchar(a_ds) && !identical(a_ds, ds)) return(FALSE)
    comparator <- toupper(.as_scalar_char(a[["comparator"]]) %||% "EQ")
    values <- a[["value"]] %||% list()
    ## An EQ with no value is the missing-value test, which pins the variable
    ## to "absent" -- one state, and countable.
    if (identical(comparator, "EQ")) return(length(values) <= 1L)
    if (identical(comparator, "IN")) return(length(values) == 1L)
    FALSE
  }, logical(1)))
}

#' The role of the filter an annotation states, without building a subset.
#' @noRd
.annotation_filter_role <- function(annotation, dataset = NULL,
                                    variable = NULL) {
  .filter_role(parse_where_clause(.annotation_less_derivation_note(annotation)),
               dataset, variable)
}


#' Flatten a single annotation WHERE clause into the
#' `{dataset, variable, comparator, value}` shape that
#' `.build_data_subset()` consumes. Returns NULL when the annotation has no
#' parseable condition, or when it parses to a compound expression (which the
#' single-condition DataSubset builder cannot represent yet).
#' @noRd
flat_data_subset <- function(annotation) {
  wc <- parse_where_clause(annotation)
  if (is.null(wc) || is.null(wc$condition)) return(NULL)
  cond <- wc$condition
  list(
    dataset    = cond$dataset,
    variable   = cond$variable,
    comparator = cond$comparator %||% "EQ",
    value      = cond$value      %||% list()
  )
}

## --- WhereClause -> logical mask ------------------------------------------
##
## What the EXECUTOR computes: the row mask a WhereClause puts on a data frame
## in memory. These sit here, beside the predicate-string emitter below, so
## the two halves of the equivalence guarantee are read together rather than
## a file apart.
##
## They lived inside ars_to_ard() as closures, which put them out of reach of
## every other function in the package: .complete_zero_groups() is defined at
## namespace level and called from inside that closure, and its call to the
## evaluator resolved lexically to nothing at all. Completing a zero group
## whose column was declared by CONDITION therefore died with
## "could not find function", where completing a data-driven one worked. A
## package-level function is reachable from both, and testable on its own.

#' Resolve a possibly `.`-qualified variable name against a frame's columns.
#'
#' A shell annotates `ADSL.SAFFL`; the loaded frame carries `SAFFL`. The
#' qualified name is kept when the frame really has it, so a column literally
#' named with a dot is not renamed out from under the caller.
#' @noRd
.clean_var_name <- function(var_name, df_names) {
  if (is.null(var_name) || !nzchar(var_name)) return(var_name)
  if (var_name %in% df_names) return(var_name)
  if (grepl(".", var_name, fixed = TRUE)) {
    parts <- strsplit(var_name, ".", fixed = TRUE)[[1]]
    short_var <- parts[length(parts)]
    if (short_var %in% df_names) return(short_var)
  }
  var_name
}

#' One WhereClauseCondition -> a logical mask over `df`.
#'
#' A variable the frame does not carry reads as FALSE for every row, not as an
#' error: a cross-dataset condition is answered on its own dataset and carried
#' back by subject key (see where_keep_mask() in ars_to_ard.R), so reaching
#' here with an absent variable means the condition selects nothing.
#'
#' Mirrored by .condition_to_expr() below -- change the two together.
#' @noRd
.eval_condition <- function(df, cond_obj) {
  var_name <- cond_obj[["variable"]]
  comp <- cond_obj[["comparator"]]
  val_list <- cond_obj[["value"]]

  if (is.null(var_name) || !nzchar(var_name)) {
    return(rep(TRUE, nrow(df)))
  }

  var_name <- .clean_var_name(var_name, names(df))

  if (!var_name %in% names(df)) {
    return(rep(FALSE, nrow(df)))
  }

  col_val <- df[[var_name]]
  val <- unlist(val_list)

  if (comp %in% c("EQ", "IN")) {
    if (length(val) == 0) {
      is.na(col_val) | col_val == ""
    } else {
      col_val %in% val
    }
  } else if (comp %in% c("NE", "NOTIN")) {
    if (length(val) == 0) {
      !is.na(col_val) & col_val != ""
    } else {
      !(col_val %in% val)
    }
  } else if (comp == "LT") {
    col_val < as.numeric(val)
  } else if (comp == "LE") {
    col_val <= as.numeric(val)
  } else if (comp == "GT") {
    col_val > as.numeric(val)
  } else if (comp == "GE") {
    col_val >= as.numeric(val)
  } else if (comp == "CONTAINS") {
    ## arsbridge extension comparator: case-insensitive substring match
    ## against any of the supplied values.
    if (length(val) == 0) {
      rep(FALSE, nrow(df))
    } else {
      Reduce(`|`, lapply(val, function(v) {
        grepl(tolower(v), tolower(as.character(col_val)), fixed = TRUE)
      }))
    }
  } else {
    rep(TRUE, nrow(df))
  }
}

#' A WhereClause -- condition, compound expression, or a bare condition object
#' -- as a logical mask over `df`. A NULL clause selects every row.
#'
#' Mirrored by where_to_filter_expr() below -- change the two together.
#' @noRd
.eval_where_clause <- function(df, where_clause) {
  if (is.null(where_clause)) {
    return(rep(TRUE, nrow(df)))
  }
  if (!is.null(where_clause[["condition"]])) {
    return(.eval_condition(df, where_clause[["condition"]]))
  }
  if (!is.null(where_clause[["compoundExpression"]])) {
    comp_expr <- where_clause[["compoundExpression"]]
    op <- comp_expr[["logicalOperator"]]
    clauses <- comp_expr[["whereClauses"]]

    if (length(clauses) == 0) {
      return(rep(TRUE, nrow(df)))
    }

    results <- lapply(clauses, function(clause) .eval_where_clause(df, clause))

    if (identical(op, "AND")) {
      Reduce(`&`, results)
    } else if (identical(op, "OR")) {
      Reduce(`|`, results)
    } else {
      rep(TRUE, nrow(df))
    }
  } else {
    if (!is.null(where_clause[["variable"]])) {
      return(.eval_condition(df, where_clause))
    }
    rep(TRUE, nrow(df))
  }
}

## --- WhereClause -> restriction plan ---------------------------------------
##
## One structure both halves consume, so "executed filtering == emitted
## filtering" is structural rather than two implementations kept in step by
## discipline. The executor interprets the plan against loaded frames; the
## emitter renders the same plan as dplyr source text.
##
## The rule the plan encodes, and why it is not per-atom:
##
##   A maximal subtree naming ONE dataset is evaluated ROW-WISE on that
##   dataset. Only then, if that dataset is foreign, are qualifying rows
##   projected to subject ids and turned into a membership mask on the target.
##
## Decomposing a same-dataset AND into independent existential subject tests
## would destroy same-record semantics. Given ADCM rows
##
##   S01: CMDECOD=ASPIRIN   CONTRTFL=N
##   S01: CMDECOD=IBUPROFEN CONTRTFL=Y
##
## the clause `ADCM.CMDECOD='ASPIRIN' AND ADCM.CONTRTFL='Y'` must NOT keep
## S01 -- no single conmed record satisfies both. Row-wise evaluation is the
## behaviour arsbridge has always had for a single foreign dataset; the plan
## extends it across datasets instead of replacing it.

.PLAN_UNSUPPORTED_AMBIGUOUS <- "ambiguous_row_coherence"
.PLAN_UNSUPPORTED_OPERATOR  <- "unknown_logical_operator"
.PLAN_UNSUPPORTED_CONDITION <- "multi_dataset_condition"

#' Plan a where-clause against the dataset it will filter.
#'
#' @return `list(ok = TRUE, node = <plan>)`, or `list(ok = FALSE, reason =,
#'   detail =)` with a stable reason string. Both outcomes are explicit: an
#'   unsupported clause is a planning RESULT, not an error thrown somewhere in
#'   the middle of an execution, so the executor and the emitter can both
#'   refuse it in the same way and for the same recorded reason.
#'
#' Plan nodes:
#'   list(kind = "all")                          -- no restriction
#'   list(kind = "row",     where =)             -- row-wise on the target
#'   list(kind = "subject", dataset =, where =)  -- row-wise there, then
#'                                                  subject membership here
#'   list(kind = "op",      op =, children =)    -- combine masks elementwise
#' @noRd
.where_restriction_plan <- function(where, target_ds, subject_key = "USUBJID") {
  if (is.null(where)) return(list(ok = TRUE, node = list(kind = "all")))

  node <- .plan_node(where, target_ds)

  unsupported <- .plan_first_unsupported(node)
  if (!is.null(unsupported)) {
    return(list(ok = FALSE, reason = unsupported$reason,
                detail = unsupported$detail))
  }

  ## Row coherence is only at stake where an existential projection happens,
  ## which is the foreign case. Two `row` leaves naming the target are each
  ## evaluated against the same row and so cannot disagree about which record
  ## they meant.
  foreign <- .plan_subject_datasets(node)
  repeated <- unique(foreign[duplicated(foreign)])
  if (length(repeated) > 0) {
    return(list(
      ok = FALSE, reason = .PLAN_UNSUPPORTED_AMBIGUOUS,
      detail = sprintf(
        paste0("%s appears in more than one branch that AND-regrouping cannot ",
               "rejoin, so whether its predicates must hold on the SAME record ",
               "is not determined by the expression"),
        paste(repeated, collapse = ", "))))
  }

  list(ok = TRUE, node = node)
}

## One node, recursively. Multi-dataset nodes must be boolean; a single
## condition naming several datasets is malformed and cannot be planned.
#' @noRd
.plan_node <- function(where, target_ds) {
  datasets <- .where_datasets(where)
  if (length(datasets) == 0) return(list(kind = "all"))

  if (length(datasets) == 1L) {
    if (identical(toupper(datasets), toupper(target_ds %||% ""))) {
      return(list(kind = "row", where = where))
    }
    return(list(kind = "subject", dataset = datasets, where = where))
  }

  compound <- where[["compoundExpression"]]
  if (is.null(compound)) {
    return(list(kind = "unsupported", reason = .PLAN_UNSUPPORTED_CONDITION,
                detail = paste(datasets, collapse = ", ")))
  }

  op <- .as_scalar_char(compound[["logicalOperator"]]) %||% ""
  if (!op %in% c("AND", "OR")) {
    return(list(kind = "unsupported", reason = .PLAN_UNSUPPORTED_OPERATOR,
                detail = op))
  }

  children <- compound[["whereClauses"]] %||% list()
  if (identical(op, "AND")) {
    ## Associativity, then commutativity -- and only under AND. Regrouping
    ## across an OR would require distribution, which changes the answer.
    children <- .plan_flatten_and(children)
    children <- .plan_regroup_same_dataset(children)
  }

  list(kind = "op", op = op,
       children = lapply(children, .plan_node, target_ds = target_ds))
}

## Nested ANDs are one AND. `(A AND B) AND C` -> A, B, C.
#' @noRd
.plan_flatten_and <- function(children) {
  out <- list()
  for (child in children) {
    compound <- child[["compoundExpression"]]
    if (!is.null(compound) &&
        identical(.as_scalar_char(compound[["logicalOperator"]]) %||% "", "AND")) {
      out <- c(out, .plan_flatten_and(compound[["whereClauses"]] %||% list()))
    } else {
      out <- c(out, list(child))
    }
  }
  out
}

## Same-dataset siblings of one AND become one subtree, so they are evaluated
## against the same record. `(ADCM.A AND ADSL.S) AND ADCM.B` regroups to
## `(ADCM.A AND ADCM.B) AND ADSL.S`.
#' @noRd
.plan_regroup_same_dataset <- function(children) {
  signature <- vapply(children, function(child) {
    datasets <- .where_datasets(child)
    if (length(datasets) == 1L) toupper(datasets) else NA_character_
  }, character(1))

  out <- list()
  handled <- rep(FALSE, length(children))
  for (i in seq_along(children)) {
    if (handled[[i]]) next
    if (is.na(signature[[i]])) {
      out <- c(out, list(children[[i]]))
      handled[[i]] <- TRUE
      next
    }
    same <- which(!handled & !is.na(signature) & signature == signature[[i]])
    handled[same] <- TRUE
    if (length(same) == 1L) {
      out <- c(out, list(children[[i]]))
    } else {
      out <- c(out, list(list(compoundExpression = list(
        logicalOperator = "AND", whereClauses = unname(children[same])))))
    }
  }
  out
}

## The first unsupported node anywhere in the plan, or NULL.
#' @noRd
.plan_first_unsupported <- function(node) {
  if (identical(node$kind, "unsupported")) {
    return(list(reason = node$reason, detail = node$detail))
  }
  if (!identical(node$kind, "op")) return(NULL)
  for (child in node$children) {
    hit <- .plan_first_unsupported(child)
    if (!is.null(hit)) return(hit)
  }
  NULL
}

## Datasets reached through a subject projection, in plan order.
#' @noRd
.plan_subject_datasets <- function(node) {
  if (identical(node$kind, "subject")) return(toupper(node$dataset))
  if (!identical(node$kind, "op")) return(character(0))
  unlist(lapply(node$children, .plan_subject_datasets), use.names = FALSE) %||%
    character(0)
}

## --- WhereClause -> R predicate source text -------------------------------
##
## where_to_filter_expr() turns a WhereClause into a dplyr/base predicate STRING
## that, evaluated against a dataset, reproduces the logical mask of
## .eval_where_clause()/.eval_condition() ABOVE, exactly. The cards emitter
## (R/ars_to_code.R) drops these strings into `dplyr::filter(...)`, so
## the code arsbridge emits filters identically to how arsbridge executes --
## the deterministic-equivalence guarantee of Plan B. Keep the two in lock-step:
## any change to .eval_condition() must be mirrored here (see test-where_to_filter_expr).

#' Datasets referenced anywhere in a WhereClause. Used to decide direct-filter
#' vs cross-dataset subject restriction.
#'
#' The single source of truth for both halves: the emitter reads it directly,
#' the executor through `.where_datasets_checked()`, which adds a diagnostic
#' for malformed cardinality. It used to be mirrored by a
#' `get_referenced_datasets()` closure inside ars_to_ard(), and the two
#' differed -- this one coerces each `dataset` through `.as_scalar_char()`,
#' the closure returned the raw field -- so a `dataset` written as more than
#' one value produced two different filters from one spec.
#' @noRd
.where_datasets <- function(where) {
  if (is.null(where)) return(character(0))
  if (!is.null(where[["condition"]])) {
    return(.as_scalar_char(where[["condition"]][["dataset"]]) %||% character(0))
  }
  if (!is.null(where[["compoundExpression"]])) {
    cls <- where[["compoundExpression"]][["whereClauses"]]
    return(unique(unlist(lapply(cls, .where_datasets))))
  }
  if (!is.null(where[["dataset"]])) {
    return(.as_scalar_char(where[["dataset"]]) %||% character(0))
  }
  character(0)
}

#' Render a character vector as an escaped R `c("a", "b")` literal.
#' @noRd
.r_chr_vec <- function(vals) {
  paste0("c(", paste(encodeString(as.character(vals), quote = "\""),
                     collapse = ", "), ")")
}

#' One WhereClauseCondition -> predicate string (mirrors .eval_condition()).
#' @noRd
.condition_to_expr <- function(cond) {
  var  <- .as_scalar_char(cond[["variable"]])
  comp <- .as_scalar_char(cond[["comparator"]]) %||% "EQ"
  vals <- unlist(cond[["value"]])
  vals <- vals[!is.na(vals)]

  if (is.null(var) || !nzchar(var)) return("TRUE")

  if (comp %in% c("EQ", "IN")) {
    if (length(vals) == 0) sprintf("(is.na(%s) | %s == \"\")", var, var)
    else sprintf("%s %%in%% %s", var, .r_chr_vec(vals))
  } else if (comp %in% c("NE", "NOTIN")) {
    if (length(vals) == 0) sprintf("(!is.na(%s) & %s != \"\")", var, var)
    else sprintf("!(%s %%in%% %s)", var, .r_chr_vec(vals))
  } else if (comp %in% c("LT", "LE", "GT", "GE")) {
    op <- c(LT = "<", LE = "<=", GT = ">", GE = ">=")[[comp]]
    sprintf("%s %s as.numeric(%s)", var, op,
            encodeString(as.character(vals[1]), quote = "\""))
  } else if (comp == "CONTAINS") {
    if (length(vals) == 0) return("FALSE")
    atoms <- vapply(vals, function(v) sprintf(
      "grepl(tolower(%s), tolower(as.character(%s)), fixed = TRUE)",
      encodeString(v, quote = "\""), var), character(1))
    if (length(atoms) == 1) atoms
    else paste0("(", paste(atoms, collapse = " | "), ")")
  } else {
    "TRUE"
  }
}

#' Convert an ARS WhereClause into a predicate string.
#'
#' Mirrors `.eval_where_clause()` above: `NULL` -> "TRUE" (no filter);
#' a `condition` -> the comparator predicate; a `compoundExpression` -> the
#' parenthesised atoms joined by ` & ` (AND) or ` | ` (OR); an unrecognised
#' operator -> "TRUE".
#'
#' @param where A WhereClause-bearing object (analysisSet / dataSubset /
#'   WhereClause), or `NULL`.
#' @return A single character string suitable for `eval(parse(text = .))` or
#'   `dplyr::filter()`.
#' @noRd
where_to_filter_expr <- function(where) {
  if (is.null(where)) return("TRUE")
  if (!is.null(where[["condition"]])) {
    return(.condition_to_expr(where[["condition"]]))
  }
  if (!is.null(where[["compoundExpression"]])) {
    ce      <- where[["compoundExpression"]]
    op      <- .as_scalar_char(ce[["logicalOperator"]])
    clauses <- ce[["whereClauses"]]
    if (length(clauses) == 0) return("TRUE")
    sep <- if (identical(op, "AND")) " & " else if (identical(op, "OR")) " | " else NULL
    if (is.null(sep)) return("TRUE")
    atoms <- vapply(clauses, function(cl) sprintf("(%s)", where_to_filter_expr(cl)),
                    character(1))
    return(paste(atoms, collapse = sep))
  }
  if (!is.null(where[["variable"]])) {
    return(.condition_to_expr(where))
  }
  "TRUE"
}

## --- WhereClause algebra ---------------------------------------------------
##
## Small structural helpers for composing and comparing WhereClause objects.
## They exist so hierarchical column trees can build a leaf column's condition
## as AND(parent condition, own condition), assert that a subtotal's condition
## equals the parent condition, and let validation detect duplicate result
## paths. They operate on the same shapes parse_where_clause() produces:
## NULL, a `condition` object, or a `compoundExpression`.

#' Combine WhereClause objects into one clause.
#'
#' NULL inputs are dropped (NULL means "no condition", the AND identity).
#' Nested compoundExpressions that use the same operator are flattened, so
#' AND(AND(a, b), c) becomes AND(a, b, c). Returns NULL when nothing remains,
#' the single clause unchanged when only one remains, or a compoundExpression.
#'
#' @noRd
combine_conditions <- function(..., operator = "AND") {
  stopifnot(operator %in% c("AND", "OR"))
  clauses <- Filter(Negate(is.null), list(...))
  if (length(clauses) == 0) return(NULL)

  flattened <- list()
  for (clause in clauses) {
    ce <- clause[["compoundExpression"]]
    same_op <- !is.null(ce) && identical(.as_scalar_char(ce[["logicalOperator"]]), operator)
    if (same_op) {
      flattened <- c(flattened, ce[["whereClauses"]])
    } else {
      flattened <- c(flattened, list(clause))
    }
  }

  if (length(flattened) == 1) return(flattened[[1]])
  list(
    compoundExpression = list(
      logicalOperator = operator,
      whereClauses    = flattened
    )
  )
}

#' Canonical form of a WhereClause, for order-insensitive comparison.
#'
#' A single condition becomes a condition with uppercased dataset, variable,
#' and comparator, and its values as a sorted character vector. A
#' compoundExpression gets each member canonicalized, same-operator nesting
#' flattened, and the members sorted by their serialized form. Two clauses
#' that state the same thing in a different order therefore canonicalize to
#' identical objects.
#'
#' @noRd
canonicalize_condition <- function(x) {
  if (is.null(x)) return(NULL)

  if (!is.null(x[["condition"]])) {
    cond <- x[["condition"]]
    vals <- as.character(unlist(cond[["value"]]))
    return(list(
      condition = list(
        dataset    = toupper(.as_scalar_char(cond[["dataset"]]) %||% ""),
        variable   = toupper(.as_scalar_char(cond[["variable"]]) %||% ""),
        comparator = toupper(.as_scalar_char(cond[["comparator"]]) %||% "EQ"),
        value      = as.list(sort(vals))
      )
    ))
  }

  if (!is.null(x[["compoundExpression"]])) {
    ce <- x[["compoundExpression"]]
    op <- toupper(.as_scalar_char(ce[["logicalOperator"]]) %||% "AND")
    members <- lapply(ce[["whereClauses"]], canonicalize_condition)
    members <- Filter(Negate(is.null), members)

    ## Flatten same-operator nesting after canonicalizing the members.
    flat <- list()
    for (m in members) {
      mce <- m[["compoundExpression"]]
      if (!is.null(mce) && identical(mce[["logicalOperator"]], op)) {
        flat <- c(flat, mce[["whereClauses"]])
      } else {
        flat <- c(flat, list(m))
      }
    }

    if (length(flat) == 0) return(NULL)
    if (length(flat) == 1) return(flat[[1]])

    keys <- vapply(flat, function(m) {
      paste(deparse(m), collapse = "")
    }, character(1))
    flat <- flat[order(keys)]

    return(list(
      compoundExpression = list(
        logicalOperator = op,
        whereClauses    = flat
      )
    ))
  }

  ## A bare condition body (dataset/variable at the top level, as some ARS
  ## nodes carry) is wrapped so both spellings canonicalize the same way.
  if (!is.null(x[["variable"]])) {
    return(canonicalize_condition(list(condition = x)))
  }

  NULL
}

#' Are two WhereClauses structurally equivalent?
#' @noRd
conditions_equal <- function(a, b) {
  identical(canonicalize_condition(a), canonicalize_condition(b))
}

#' The canonical atomic conditions of a pure-AND clause.
#'
#' Returns a list of canonical single-condition objects when `x` is NULL (an
#' empty set), a single condition, or an all-AND compound. Returns NA when the
#' clause contains an OR anywhere, because it then has no simple atom-set
#' reading.
#'
#' @noRd
.and_atoms <- function(x) {
  canon <- canonicalize_condition(x)
  if (is.null(canon)) return(list())
  if (!is.null(canon[["condition"]])) return(list(canon))

  ce <- canon[["compoundExpression"]]
  if (!identical(ce[["logicalOperator"]], "AND")) return(NA)

  atoms <- list()
  for (m in ce[["whereClauses"]]) {
    inner <- .and_atoms(m)
    if (!is.list(inner)) return(NA)
    atoms <- c(atoms, inner)
  }
  atoms
}

#' Does the child condition imply the parent condition?
#'
#' TRUE when every atomic condition of the parent also appears in the child,
#' so the child's subject set is a subset of the parent's. This is the check
#' that "COHORT = 1 AND SUBGROUP = 2" is a valid child of "COHORT = 1". A NULL
#' parent (no condition) is implied by anything. Clauses containing OR are
#' answered conservatively: TRUE only when the two clauses are structurally
#' equal, FALSE otherwise.
#'
#' @noRd
condition_implies <- function(child, parent) {
  if (is.null(parent)) return(TRUE)
  if (conditions_equal(child, parent)) return(TRUE)

  child_atoms  <- .and_atoms(child)
  parent_atoms <- .and_atoms(parent)
  if (!is.list(child_atoms) || !is.list(parent_atoms)) return(FALSE)

  child_keys  <- vapply(child_atoms,  function(a) paste(deparse(a), collapse = ""), character(1))
  parent_keys <- vapply(parent_atoms, function(a) paste(deparse(a), collapse = ""), character(1))
  all(parent_keys %in% child_keys)
}

#' The WhereClause of an ARS Group node, in the wrapped shape the executor
#' and emitter consume.
#'
#' The official ARS v1.0 Group IS-A WhereClause: it carries `level`, `order`,
#' and either a bare `condition` (a WhereClauseCondition: dataset, variable,
#' comparator, value) or a `compoundExpression` directly. arsbridge's
#' internal shape wraps conditions one level deeper. This accessor reads
#' both and always returns the wrapped shape (or NULL when the group has no
#' condition at all -- a data-driven level).
#' @noRd
.group_where <- function(g) {
  cond <- g[["condition"]]
  if (!is.null(cond)) {
    ## Already-wrapped legacy shape: condition/compoundExpression inside.
    if (!is.null(cond[["condition"]]) || !is.null(cond[["compoundExpression"]])) {
      return(cond)
    }
    return(list(condition = cond))
  }
  if (!is.null(g[["compoundExpression"]])) {
    return(list(compoundExpression = g[["compoundExpression"]]))
  }
  NULL
}

.cond <- function(dataset, variable, comparator, value) {
  list(
    condition = list(
      dataset    = dataset,
      variable   = variable,
      comparator = comparator,
      value      = if (is.na(value)) list() else list(value)
    )
  )
}

.cond_multi <- function(dataset, variable, comparator, values) {
  list(
    condition = list(
      dataset    = dataset,
      variable   = variable,
      comparator = comparator,
      value      = as.list(values)
    )
  )
}
