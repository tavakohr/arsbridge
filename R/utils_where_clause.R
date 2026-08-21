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

## Residue that CHANGES what a recognised leaf means, although it states no
## condition of its own.
##
## `.RE_CONDITION_EVIDENCE` asks whether leftover text states a condition. On
## its own that is the wrong question, because text can decide what a
## condition MEANS without stating one. Two kinds do, and POSITION separates
## them -- the same asymmetry the operand-slot rule already relies on.
##
## A NEGATING WORD, wherever it sits, inverts the leaf. "with no ADQX record
## where QXFL='Y'" is not "QXFL='Y'"; and, as `.where_from_boolean()` sets out
## below, negating row-wise and negating the per-subject projection are not
## even the same set. Dropping the word silently selects something close to
## the complement of the population the author asked for, which is the worst
## available answer -- so this applies to a prefix and a suffix alike.
.RE_RESIDUE_NEGATION <-
  "(?i)\\b(?:no|not|without|never|excluding|except)\\b"

## A QUALIFIED REFERENCE IN A PREFIX scopes the condition that follows it.
## "ADQX.USUBJID with an ADZZ record where <condition>" asks a per-subject
## existential question this grammar does not answer; reading the inner
## condition alone answers a different question and presents it as the same
## one. Two such clauses joined are worse still -- row-wise AND asks for one
## record satisfying both, which is not what "a record where A, and a record
## where B" says.
##
## A qualified reference AFTER a fully-read condition is not that. It is an
## aside about the row -- which variable supplies the display label, which
## flag the count is once-per-subject on -- and it has always been allowed to
## follow a condition without unmaking it.
.RE_RESIDUE_SCOPING_PREFIX <- paste0("\\b", .ADAM_DS, "\\.", .ADAM_VAR, "\\b")

## Data that is NOT THERE, as a construction rather than as a stray negative.
##
## This is the clause that makes the test structural instead of a
## co-occurrence. A negating word can attach to anything -- "(records are not
## shown separately)" negates the SHOWING, "(no record-level adjustment is
## applied)" negates the ADJUSTMENT -- and neither says a record is absent.
## What the invariant needs is the absence itself: `with no`, `without`,
## `has no`, `missing`, `absent`. Those are the ways a shell says a unit has
## no data, and a unit with no data is precisely what a filter cannot reach.
##
## General-language and domain vocabulary, deliberately -- no study's dataset,
## variable or label appears here, and none may.
.RE_ABSENT_OBSERVATION <- paste0(
  "(?i)\\b(?:with\\s+no\\b|without\\b|ha(?:s|ve|d)\\s+no\\b",
  "|having\\s+no\\b|missing\\b|absent\\b|not\\s+present\\b)"
)

## The unit of observation, in the words shells actually use for it. What the
## absence above has to be an absence OF, so that "(without adjustment)" --
## absent, but of no record -- is not mistaken for a rule about the data.
##
## General-language and domain vocabulary, deliberately -- no study's dataset,
## variable or label appears here, and none may.
.RE_OBSERVATION_UNIT <- paste0(
  "(?i)\\b(?:subject|subjects|patient|patients",
  "|record|records|observation|observations|visit|visits)\\b"
)

## The aside ASSIGNS those records a treatment, rather than merely mentioning
## them. This is the clause that separates a rule from a remark: "(except
## visit 1)" names records and says nothing about what becomes of them, while
## "(a subject with no visit record is a non-responder)" says what they count
## as. Only the second is something a computation would have to carry out.
##
## Kept behind the negation and unit tests, so these very ordinary words can
## never flag an aside on their own.
.RE_INSTRUCTION_ASSIGNS <- paste0(
  "(?i)\\b(?:is|are|was|were|count|counts|counted|treat|treated|impute",
  "|imputed|assign|assigned|consider|considered|classif(?:y|ied)",
  "|set|carried|excluded|included)\\b"
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
## Boolean structure
## ---------------------------------------------------------------------------
##
## An annotation's Boolean structure is a language of its own, sitting on top
## of the atomic conditions. Reading it with a flat split -- find one joiner,
## cut on it -- does not fail on an expression it cannot represent. It answers
## with a DIFFERENT expression that is valid, executable, and restricts other
## records: `A AND (B OR C)` became `AND(A, B, C)`, and a population written
## that way emitted an analysis set no subject satisfies.
##
## So the structure is parsed, with the precedence every SQL and SAS author
## already writes to:
##
##   parentheses   bind tightest
##   NOT           binds tighter than AND
##   AND           binds tighter than OR
##
## and the parse must CONSUME THE WHOLE TOKEN STREAM. A tree built from part
## of the input is the same class of wrong answer as a flat split: it looks
## finished. Anything left over -- trailing structure, a dangling operator, an
## unmatched bracket -- refuses, and the expression reserves exactly as one
## that could not be read at all.
##
## Three separations make this safe, and each is load-bearing:
##
##   1. QUOTED VALUES are already masked when the structure is read (see the
##      masking block at the top of this file), so `RACE='BLACK OR AFRICAN
##      AMERICAN'` carries no joiner and `NOTE='(see appendix)'` no bracket.
##   2. ATOMIC FORMS that own a bracket or the word NOT -- `IN (...)`,
##      `is.na(...)`, `not missing` -- are masked too, so their internal
##      syntax is never read as structure.
##   3. NOTES IN BRACKETS group nothing. `(mg)`, `(N=XX)`, `(per protocol)`
##      enclose no operand, and refusing an expression over a punctuation
##      habit withholds a result that would have been correct.
##
## The leaf parser is unchanged: `.one_condition()` still reads every atomic
## form, and this layer only decides which spans of text are atoms.

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

## The logical operators, and only where they are operators.
##
## The word forms are fenced off from "." and "~" as well as from word
## characters. "." keeps a variable spelled `ADQX.NOT` from opening a
## negation; "~" keeps this parser's own BETWEEN marker, `~AND~`, from being
## read as the conjunction it was introduced to hide.
##
## The symbol forms require surrounding whitespace, which is the discipline
## the flat splitter already used. `A|B` inside an unquoted label is not a
## disjunction, and reading it as one would tear a value in half.
##
## `!` opens a negation only when it is not the head of `!=`: that "!" belongs
## to a comparator and negates no sub-expression.
.RE_BOOL_OPERATOR <- paste0(
  "(?<![.~\\w])(?:AND|OR|NOT)(?![.~\\w])",
  "|(?<=\\s)(?:&&|\\|\\||&|\\|)(?=\\s)",
  "|!(?!=)"
)
## Everything the structural lexer recognises. Brackets included; anything
## else is atom text.
.RE_BOOL_TOKEN <- paste0("(?i)\\(|\\)|", .RE_BOOL_OPERATOR)

## Where a bracket would have to be an OPERAND rather than an aside: at the
## start of the expression, straight after an opening bracket, or straight
## after a logical operator. The symbol operators keep the whitespace
## requirement they are lexed with.
.RE_OPERAND_SLOT <- paste0(
  "(?i)(?:\\(|(?<![.~\\w])(?:AND|OR|NOT)|(?<=\\s)(?:&&|\\|\\||&|\\|)|!)",
  "\\s*$")

#' Hide the spans that are not Boolean structure.
#'
#' Two kinds, masked in this order because the first owns brackets the second
#' would otherwise inspect:
#'
#'   the atomic forms of `.RE_NON_STRUCTURAL_PARTS`, and
#'   bracketed spans that enclose neither an operator nor a qualified
#'   reference -- a unit, a planned-N, an aside.
#'
#' Bracketed spans are taken innermost first and re-scanned, so a note inside
#' a real group is removed without the group being disturbed.
#'
#' POSITION DECIDES whether such a span is an aside at all. `A='Y' (N=XX)`
#' puts the bracket after a condition, where an aside is exactly what it is.
#' `A='Y' AND (N=XX)` puts it after a joiner, where the author has said the
#' next thing is an OPERAND -- and an operand that quietly evaluates to
#' nothing removes a term the author wrote. `A AND ()` is the same failure
#' with the span empty. In operand position the bracket therefore stays
#' structural, and the parser answers for it.
#'
#' @param operand_context `FALSE` when the text is not an expression at all
#'   but the leftover around a recognised leaf, where there are no operand
#'   slots and every such bracket is an aside.
#' @return `list(text, spans, open, close, token_pattern)`, or `NULL` when no
#'   free delimiter pair exists -- which the caller must treat as "this
#'   expression cannot be analysed safely", never as "it has no structure".
#' @noRd
.mask_non_structural <- function(txt, operand_context = TRUE) {
  used <- strsplit(txt, "", fixed = TRUE)[[1]]
  free <- setdiff(.MASK_CANDIDATES, used)
  if (length(free) < 2L) return(NULL)
  open  <- free[[1L]]
  close <- free[[2L]]

  spans <- character(0)
  hide <- function(text, start, stop) {
    spans[[length(spans) + 1L]] <<- substr(text, start, stop)
    paste0(substr(text, 1L, start - 1L),
           open, length(spans), close,
           substr(text, stop + 1L, nchar(text)))
  }

  for (re in .RE_NON_STRUCTURAL_PARTS) {
    repeat {
      at <- regexpr(re, txt, perl = TRUE)
      if (at[[1L]] < 0L) break
      txt <- hide(txt, at[[1L]], at[[1L]] + attr(at, "match.length") - 1L)
    }
  }

  operator_re <- paste0("(?i)", .RE_BOOL_OPERATOR)
  reference_re <- paste0(.ADAM_DS, "\\.", .ADAM_VAR)
  repeat {
    at <- gregexpr("\\([^()]*\\)", txt, perl = TRUE)[[1L]]
    if (at[[1L]] < 0L) break
    lens <- attr(at, "match.length")
    hidden <- FALSE
    for (i in seq_along(at)) {
      start <- at[[i]]
      stop  <- start + lens[[i]] - 1L
      inner <- substr(txt, start + 1L, stop - 1L)
      ## A bracket is structure-bearing when it could hold an operand: an
      ## operator to bind, a reference that could be one, or a position where
      ## the author already said an operand goes.
      if (grepl(operator_re, inner, perl = TRUE)) next
      if (grepl(reference_re, inner, perl = TRUE)) next
      if (operand_context && .in_operand_slot(txt, start)) next
      txt <- hide(txt, start, stop)
      hidden <- TRUE
      break
    }
    if (!hidden) break
  }

  list(text = txt, spans = spans, open = open, close = close,
       token_pattern = paste0(open, "\\d+", close))
}

#' An authored instruction that no restriction can carry out.
#'
#' A filter says which records SURVIVE. So an aside that speaks about the
#' records a filter EXCLUDES is, by construction, stating something no filter
#' can implement -- those records are not there to be acted on. "a subject with
#' no <visit> record is a non-responder" is a rule about absent records, and a
#' `WHERE` clause has no way to say it.
#'
#' THREE things must hold, and none is sufficient alone. Each one exists
#' because dropping it lets an ordinary aside reserve a row that computes
#' perfectly well -- the failure direction that looks safe and is not.
#'
#'   NEGATION -- a negation is never decorative; it changes who is counted.
#'     The same reasoning as `.RE_RESIDUE_NEGATION` in an operand. Without it,
#'     every aside reserves.
#'   THE UNIT OF OBSERVATION -- a subject, a record, a visit. This is what
#'     makes the sentence a claim about the DATA rather than the display.
#'     Without it, `(no units)` and `(except per protocol)` reserve.
#'   AN ASSIGNMENT -- the aside says what becomes of those records. Without it,
#'     `(except visit 1)` reserves: it names records and says nothing about
#'     what they count as, so there is no computation to be missing.
#'
#' Together they read as one sentence: *records that are NOT there ARE treated
#' as something*. `(a subject with no <visit> record is a non-responder)` says
#' all three; every descriptive aside in the corpus says at most two.
#'
#' Deliberately NOT flagged, because these describe the row rather than
#' instructing a computation: a unit, a planned N, a protocol reference, a
#' variable that supplies a label, a comparator inside a quoted value. Literals
#' are masked before the spans are examined, so a category legitimately called
#' `'Not Reported'` -- or an age band written `'Adult (18-65)'` -- is a value
#' and never an aside at all.
#'
#' KNOWN LIMIT, recorded rather than papered over: an unrepresented instruction
#' phrased without a negation ("values are imputed from baseline") is not
#' detected here. This closes the case where the instruction concerns records
#' the filter cannot reach, which is the one that turns a filter into a wrong
#' number rather than an incomplete one.
#'
#' @return The aside's text as the author wrote it, or `""` when every aside
#'   is descriptive.
#' @noRd
.unrepresented_instruction <- function(ann) {
  ann <- as.character(ann %||% "")[1]
  if (is.na(ann) || !nzchar(trimws(ann))) return("")

  masked <- .mask_literals(ann)
  if (is.null(masked)) return("")
  hidden <- .mask_non_structural(masked$text, operand_context = TRUE)
  if (is.null(hidden)) return("")

  ## `spans` is exactly what the masker set aside as a note. A bracket holding
  ## an operator or a qualified reference never reaches here -- the masker
  ## keeps those as structure -- so this asks only about text already judged
  ## to contribute nothing to the filter.
  for (span in hidden$spans %||% character(0)) {
    if (grepl(.RE_ABSENT_OBSERVATION, span, perl = TRUE) &&
        grepl(.RE_OBSERVATION_UNIT, span, perl = TRUE) &&
        grepl(.RE_INSTRUCTION_ASSIGNS, span, perl = TRUE)) {
      return(trimws(.unmask_literals(span, masked)))
    }
  }
  ""
}

#' Is the span opening at `start` sitting where an operand is required?
#' @noRd
.in_operand_slot <- function(txt, start) {
  before <- substr(txt, 1L, start - 1L)
  if (!nzchar(trimws(before))) return(TRUE)
  grepl(.RE_OPERAND_SLOT, before, perl = TRUE)
}

#' Put the non-structural spans back, exactly as they were written.
#'
#' Highest token first: a bracketed note masked later may contain a token
#' masked earlier, never the other way round, so descending order restores
#' the nesting in one pass.
#' @noRd
.unmask_non_structural <- function(text, masked) {
  spans <- masked$spans %||% character(0)
  if (length(spans) == 0 || !length(text)) return(text)
  for (i in rev(seq_along(spans))) {
    token <- paste0(masked$open, i, masked$close)
    text <- gsub(token, spans[[i]], text, fixed = TRUE)
  }
  text
}

#' Split masked text into structural tokens and the atom text between them.
#'
#' @return A list of `list(type, text)`, `type` one of "atom", "and", "or",
#'   "not", "lparen", "rparen".
#' @noRd
.lex_boolean <- function(txt) {
  tokens <- list()
  add <- function(type, text) tokens[[length(tokens) + 1L]] <<- list(type = type, text = text)
  add_atom <- function(text) {
    text <- trimws(text)
    if (nzchar(text)) add("atom", text)
  }

  at <- gregexpr(.RE_BOOL_TOKEN, txt, perl = TRUE)[[1L]]
  pos <- 1L
  if (at[[1L]] > 0L) {
    lens <- attr(at, "match.length")
    for (i in seq_along(at)) {
      start <- at[[i]]
      stop  <- start + lens[[i]] - 1L
      add_atom(substr(txt, pos, start - 1L))
      lexeme <- substr(txt, start, stop)
      type <- switch(
        toupper(lexeme),
        "(" = "lparen", ")" = "rparen",
        "AND" = "and", "&&" = "and", "&" = "and",
        "OR" = "or", "||" = "or", "|" = "or",
        "not")
      add(type, lexeme)
      pos <- stop + 1L
    }
  }
  add_atom(substr(txt, pos, nchar(txt)))
  tokens
}

#' Parse a structural token stream into a Boolean syntax tree.
#'
#' Recursive descent, one function per precedence level, lowest first. Same
#' operator at the same level collects into one n-ary node, so `A AND B AND C`
#' is a single three-child AND rather than a nest -- which is the shape ARS
#' writes and the shape the flat splitter produced for the expressions it
#' could handle.
#'
#' @return `list(kind = "atom"|"not"|"and"|"or", ...)`, or
#'   `list(kind = "error", reason = )` naming what stopped the parse. Every
#'   error is a REFUSAL, never a partial tree.
#' @noRd
.parse_boolean <- function(tokens) {
  i <- 1L
  refuse <- function(reason) list(kind = "error", reason = reason)
  peek <- function() if (i <= length(tokens)) tokens[[i]][["type"]] else "eof"
  take <- function() i <<- i + 1L

  primary <- function() {
    what <- peek()
    if (identical(what, "atom")) {
      node <- list(kind = "atom", text = tokens[[i]][["text"]])
      take()
      return(node)
    }
    if (identical(what, "lparen")) {
      take()
      ## Brackets with nothing between them sit exactly where an operand is
      ## required. Reading them as an aside would drop a term the author
      ## wrote and leave the expression looking complete.
      if (identical(peek(), "rparen")) return(refuse("an empty operand"))
      inner <- disjunction()
      if (identical(inner$kind, "error")) return(inner)
      if (!identical(peek(), "rparen")) return(refuse("an unclosed parenthesis"))
      take()
      return(inner)
    }
    if (identical(what, "eof")) return(refuse("an operator with nothing after it"))
    if (identical(what, "rparen")) return(refuse("a parenthesis that closes nothing"))
    refuse("an operator with no condition before it")
  }

  negation <- function() {
    if (!identical(peek(), "not")) return(primary())
    take()
    child <- negation()
    if (identical(child$kind, "error")) return(child)
    list(kind = "not", child = child)
  }

  ## One level per precedence rung. `level()` collects operands separated by
  ## `op`, each parsed by the rung that binds tighter, so `A OR B AND C` puts
  ## the AND underneath the OR without either rung knowing about the other.
  level <- function(op, tighter) {
    first <- tighter()
    if (identical(first$kind, "error")) return(first)
    operands <- list(first)
    while (identical(peek(), op)) {
      take()
      nxt <- tighter()
      if (identical(nxt$kind, "error")) return(nxt)
      operands[[length(operands) + 1L]] <- nxt
    }
    if (length(operands) == 1L) return(first)
    list(kind = op, children = operands)
  }

  conjunction <- function() level("and", negation)
  disjunction <- function() level("or", conjunction)

  tree <- disjunction()
  if (identical(tree$kind, "error")) return(tree)
  ## The whole stream, or nothing. A tree covering a prefix of the input is a
  ## different restriction from the one that was written, and it looks
  ## finished.
  if (i <= length(tokens)) return(refuse("text this grammar cannot place"))
  tree
}

## Commas: an annotation shorthand, and deliberately nothing more.
##
## Shells write a population as a list -- `(ADSL.SAFFL='Y', ADVS.ANL01FL='Y')`
## -- and mean every item of it. This grammar has no comma operator, so the
## leaf battery read the first item and the rest disappeared: a real study
## emitted that population as the safety flag alone, with the second condition
## absent from the reporting event entirely.
##
## The rule that fixes it is narrow ON PURPOSE. A comma is NOT a Boolean
## synonym for AND. It is read only when the WHOLE operand is a list of two or
## more independently complete atomic conditions -- each one read to its end,
## none leaving residue -- and only when the expression states no Boolean
## operator of its own. Anywhere else the comma is refused rather than
## interpreted, because `A, B OR C` would require deciding how a comma binds
## against OR, and this grammar has no answer to give.
##
## The negative side is the point of the rule, not an afterthought:
##
##   VAR IN ('A','B')        the list belongs to the comparator
##   VAR='A,B'               the comma is inside a value
##   A='Y' (N=10, planned)   the comma is inside an aside
##   A='Y', prose            not every item is a condition -> reserves
##   A='Y', B                a bare reference is not a condition -> reserves
##   A='Y',                  an empty item is a missing one -> reserves
##   A='Y', B='N' OR C='Y'   a Boolean operator is present -> reserves

#' Remove parentheses that wrap the whole span, however many deep.
#'
#' Only when the opening bracket's match is the LAST character, so
#' `(A), (B)` -- whose first bracket closes in the middle -- is left alone.
#' @noRd
.strip_wrapping_parens <- function(txt) {
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
    if (is.na(close_at) || close_at != length(chars)) return(trimmed)
    txt <- substr(trimmed, 2L, close_at - 1L)
  }
}

#' The items of a comma-separated operand, or `NULL` when it is not one.
#'
#' Split on the masked text, so a comma inside a value, inside a comparator's
#' own list, or inside an aside is not a separator. Only commas at bracket
#' depth zero divide the list.
#'
#' EMPTY ITEMS ARE KEPT. `A='Y',` and `A='Y',, B='N'` name a place where an
#' item goes and put nothing in it, which is the same missing operand as
#' `A AND ()`. Dropping them here would let the list read as though the
#' author had written only the items that survived.
#' @noRd
.comma_items <- function(part) {
  masked <- .mask_literals(part)
  if (is.null(masked)) return(NULL)
  hidden <- .mask_non_structural(masked$text, operand_context = FALSE)
  if (is.null(hidden)) return(NULL)

  txt <- .strip_wrapping_parens(hidden$text)
  chars <- strsplit(txt, "", fixed = TRUE)[[1]]
  depth <- 0L
  cuts <- integer(0)
  for (i in seq_along(chars)) {
    ch <- chars[[i]]
    if (ch == "(") depth <- depth + 1L
    else if (ch == ")") depth <- depth - 1L
    else if (ch == "," && depth == 0L) cuts <- c(cuts, i)
  }
  if (length(cuts) == 0L) return(NULL)

  starts <- c(1L, cuts + 1L)
  stops  <- c(cuts - 1L, length(chars))
  items <- trimws(substring(txt, starts, stops))
  .unmask_literals(.unmask_non_structural(items, hidden), masked)
}

#' Read one atomic operand, and refuse it when it leaves a condition behind.
#'
#' Consuming every STRUCTURAL token is not the same as consuming every
#' CONDITION. The leaf battery is unanchored and stops at the first form it
#' recognises, so `A='Y' B='N'` is read as `A='Y'` and the second condition
#' simply disappears -- the silent-loss class this parser exists to remove,
#' one level below the Boolean tree.
#'
#' So the invariant has two halves: the structure is fully consumed, AND each
#' operand leaves no unaccounted condition-bearing residue.
#'
#' What is NOT residue is as important. A descriptive suffix follows a
#' condition all the time and states none -- `AVAL GT 0 (per protocol)`,
#' `SAFFL='Y' (N=XX)` -- so the leftover is put through the same note masking
#' the expression itself gets before it is asked whether it carries an
#' operator. Withholding a correct result is its own wrong answer.
#'
#' @return `list(condition, residue)`. Exactly one is non-NULL, or both are
#'   NULL when the text states no condition at all.
#' @noRd
.read_atom <- function(part, commas = FALSE) {
  if (isTRUE(commas)) {
    items <- .comma_items(part)
    if (length(items) >= 2L) {
      reads <- lapply(items, .read_leaf)
      complete <- !vapply(reads, function(r) is.null(r$condition), logical(1))
      if (all(complete)) {
        return(list(condition = list(compoundExpression = list(
          logicalOperator = "AND",
          whereClauses    = lapply(reads, `[[`, "condition"))),
          residue = NULL))
      }
      ## Some items are conditions and some are not -- prose, a bare
      ## reference, or nothing at all. The author wrote a list; honouring the
      ## half of it this grammar can read would filter on less than was asked
      ## for, and say nothing.
      if (any(complete)) return(list(condition = NULL, residue = part))
      ## No item is a condition, so this is not a list of conditions at all --
      ## fall through and let the ordinary reading answer. Prose containing a
      ## comma states no filter and must not reserve.
    }
  }
  .read_leaf(part)
}

#' One operand, read by the leaf grammar alone.
#' @noRd
.read_leaf <- function(part) {
  report <- new.env(parent = emptyenv())
  cond <- .one_condition(part, report = report)
  if (is.null(cond)) return(list(condition = NULL, residue = NULL))
  residue <- .unread_residue(part, report$span)
  if (nzchar(residue)) return(list(condition = NULL, residue = residue))
  list(condition = cond, residue = NULL)
}

#' The part of an atom the recognised leaf did not read, when it states a
#' condition of its own. `""` otherwise.
#' @noRd
.unread_residue <- function(part, span) {
  if (is.null(span) || length(span) != 2L) return("")
  before <- trimws(substr(part, 1L, span[[1L]] - 1L))
  after  <- trimws(substr(part, span[[2L]] + 1L, nchar(part)))
  rest   <- trimws(paste(before, after))
  if (!nzchar(rest)) return("")

  ## Read exactly as the expression is read: values masked so an operator
  ## inside one is not counted, and asides masked so a note is not. There are
  ## no operand slots in a leftover, so every note-shaped bracket here IS a
  ## note.
  ##
  ## No free delimiter pair means no honest answer from the text, and the safe
  ## direction is to treat it as residue -- the caller reserves.
  readable <- function(txt) {
    masked <- .mask_literals(txt)
    if (is.null(masked)) return(NULL)
    hidden <- .mask_non_structural(masked$text, operand_context = FALSE)
    if (is.null(hidden)) masked$text else hidden$text
  }

  text <- readable(rest)
  if (is.null(text)) return(rest)

  ## Text that alters the leaf's meaning counts as unread even when it states
  ## no condition itself -- a dropped negation is not a harmless aside.
  if (grepl(.RE_RESIDUE_NEGATION, text, perl = TRUE)) return(rest)

  ## A semicolon ENDS the head. `ADQX.TERM; ADQX.EVFL='Y'` announces the row's
  ## variable and then states a filter on another one -- a form this grammar
  ## has always read, and the reference before the `;` is the row's own
  ## subject, not a phrase scoping what follows. So only the text after the
  ## last semicolon can be a scoping prefix. Literals are already masked, so a
  ## semicolon inside a value does not end anything.
  head_text <- readable(before)
  if (is.null(head_text)) return(rest)
  head_text <- sub("^.*;", "", head_text)
  if (grepl(.RE_RESIDUE_SCOPING_PREFIX, head_text, perl = TRUE)) return(rest)

  if (!grepl(.RE_CONDITION_EVIDENCE, text, perl = TRUE)) return("")
  rest
}

#' Turn a Boolean syntax tree into an ARS WhereClause.
#'
#' Atoms are handed to `.one_condition()` with their literals and masked
#' spans restored, so the leaf grammar sees exactly the text the author wrote.
#'
#' NEGATION IS NOT BUILT. ARS has a NOT operator, but nothing downstream of
#' here executes one: the evaluator and the predicate emitter both answer an
#' unrecognised operator with "keep every row", so an emitted NOT would be a
#' filter that silently does nothing -- the precise failure this parser
#' exists to remove. Worse for a foreign dataset, where negating row-wise and
#' then projecting to subjects ("has a record that is not X") is a different
#' set from negating the projection ("has no record that is X"), and the
#' expression does not say which was meant. So a NOT that governs a real
#' condition is reported and the expression reserves.
#'
#' @return `list(where, dropped, unread, parsed_any, negation)`. `where` is
#'   NULL when nothing in the tree stated a condition; `dropped` holds the
#'   operands the leaf grammar read no condition from, `unread` the operands
#'   whose text stated more than the leaf grammar consumed, and `parsed_any`
#'   says whether any operand yielded a condition at all -- which is what
#'   separates an expression of conditions from a sentence.
#' @noRd
.where_from_boolean <- function(tree, restore) {
  state <- new.env(parent = emptyenv())
  state$dropped <- character(0)
  state$unread <- character(0)
  state$parsed_any <- FALSE
  state$negation <- FALSE

  build <- function(node) {
    if (identical(node$kind, "atom")) {
      part <- restore(node$text)
      read <- .read_atom(part)
      if (!is.null(read$residue)) {
        state$unread <- c(state$unread, part)
        return(NULL)
      }
      if (is.null(read$condition)) {
        state$dropped <- c(state$dropped, part)
        return(NULL)
      }
      state$parsed_any <- TRUE
      return(read$condition)
    }
    if (identical(node$kind, "not")) {
      ## A NOT over text that states no condition negates nothing: "not
      ## applicable" is prose, and reserving it would withhold a result that
      ## was never at risk.
      inner <- build(node$child)
      if (is.null(inner)) return(NULL)
      state$negation <- TRUE
      return(NULL)
    }
    kids <- Filter(Negate(is.null), lapply(node$children, build))
    if (length(kids) == 0L) return(NULL)
    ## ARS wants at least two clauses in a compound; one surviving operand is
    ## the operand itself.
    if (length(kids) == 1L) return(kids[[1L]])
    list(compoundExpression = list(
      logicalOperator = if (identical(node$kind, "and")) "AND" else "OR",
      whereClauses    = kids
    ))
  }

  where <- build(tree)
  list(where = where, dropped = state$dropped, unread = state$unread,
       parsed_any = state$parsed_any, negation = state$negation)
}


## ---------------------------------------------------------------------------
## The annotation envelope
## ---------------------------------------------------------------------------
##
## A shell names what a row reports, and then says which records it reports on:
##
##     ADPR.PRTRT (when PRCAT='DIAGNOSTIC TESTING' AND PRPRESP='Y')
##     ADQX.QXTRT WHERE QXCAT='X'
##     ADQX.QXTRT [where QXCAT='X']
##
## Those are two languages in one string. The head NAMES a variable; the
## envelope STATES conditions. Read as a single expression the head is an
## unplaceable token and the whole annotation reserves -- a row written
## perfectly clearly, withheld.
##
## What the envelope also supplies is CONTEXT: inside it the author writes
## bare variable names, because the dataset is the head's. That inheritance is
## the dangerous half, and it is why this is not a text substitution.
##
##   A bare name is qualified only when the ADaM spec confirms that exact
##   DATASET.VARIABLE exists. Otherwise the filter is unresolved.
##
## Assuming instead of confirming is how a filter comes to name a variable the
## study does not have -- and a filter on a variable that is not there is not
## a filter at all, it is an unrestricted count wearing a restriction's text.
## The rule is ALL-OR-NONE for the same reason a dropped clause is: honouring
## the conditions that happen to be provable filters on less than the author
## wrote, and says nothing.
##
## The context is the HEAD's dataset throughout, and does not move. An
## explicitly qualified clause names its own dataset and changes nothing for
## the clauses after it -- reading `ADXX.FOO='1' AND BAR='2'` as though BAR
## were ADXX's would silently re-point a condition at a dataset the author
## never wrote.

## Keywords that open an envelope. Deliberately just these two: `WHERE` was
## already supported, `when` is what real shells write, and every further
## spelling is a widening this file does not need.
.RE_ENVELOPE_KEYWORD <- "^\\s*(?i:when|where)\\s+(.+)$"

## Names that are grammar, not variables. Without this, the `NOT` of `NOT IN`
## reads as a bare operand sitting before a comparator.
.ENVELOPE_RESERVED <- c("AND", "OR", "NOT", "IS", "IN", "NOTIN", "EQ", "NE",
                        "GT", "GE", "LT", "LE", "BETWEEN", "CONTAINS",
                        "NULL", "MISSING", "NA")

## A bare name is an operand when a comparator follows it, or when it is the
## argument of a presence-test call. Group 1 is the name in every pattern.
.RE_BARE_OPERAND <- c(
  ## symbol comparators
  "(?<![.\\w])([A-Z][A-Z0-9]{0,7})\\s*(?==|!=|<>|<=|>=|<|>)",
  ## word comparators
  paste0("(?i)(?<![.\\w])([A-Z][A-Z0-9]{0,7})\\s+",
         "(?=(?:EQ|NE|GT|GE|LT|LE|NOT\\s+IN|NOTIN|IN|BETWEEN|CONTAINS)\\b)"),
  ## presence tests
  paste0("(?i)(?<![.\\w])([A-Z][A-Z0-9]{0,7})\\s+",
         "(?=(?:IS\\s+)?(?:NOT\\s+)?(?:NULL|MISSING)\\b)"),
  ## call-form presence tests
  "(?i)(?:is\\.na|missing)\\s*\\(\\s*([A-Z][A-Z0-9]{0,7})\\s*\\)"
)

#' The inner text of a span wrapped in one balanced bracket pair, or `NULL`.
#'
#' Only when the opener's match is the LAST character, so `(when A) and (B)`
#' is not mistaken for one envelope.
#'
#' Reads MASKED text. A bracket inside a quoted value is part of the value --
#' `QXCAT='A)B'` closes nothing -- and counting it here would end the envelope
#' in the middle of a literal, cutting the filter in half and keeping the
#' fragment that happened to come first.
#' @noRd
.wrapped_inner <- function(txt, open, close) {
  if (nchar(txt) < 2L || substr(txt, 1L, 1L) != open) return(NULL)
  chars <- strsplit(txt, "", fixed = TRUE)[[1]]
  depth <- 0L
  for (i in seq_along(chars)) {
    if (chars[[i]] == open) depth <- depth + 1L
    else if (chars[[i]] == close) {
      depth <- depth - 1L
      if (depth == 0L) {
        if (i != length(chars)) return(NULL)
        return(substr(txt, 2L, i - 1L))
      }
    }
  }
  NULL
}

#' Split an annotation into its head reference and its filter text.
#'
#' @return `list(dataset, variable, filter)`, or `NULL` when the annotation is
#'   not head-plus-envelope -- which is the ordinary case and not a failure.
#' @noRd
.annotation_envelope <- function(ann) {
  txt <- trimws(as.character(ann %||% "")[1])
  if (!nzchar(txt) || is.na(txt)) return(NULL)

  head <- regmatches(txt, regexec(
    paste0("^(", .ADAM_DS, ")\\.(", .ADAM_VAR, ")\\s*(.*)$"), txt,
    perl = TRUE))[[1]]
  if (length(head) != 4L) return(NULL)

  rest <- trimws(head[[4L]])
  if (!nzchar(rest)) return(NULL)

  ## Where the envelope CLOSES is structure, so it is read on masked text --
  ## the same separation the Boolean parser makes. Only literals are hidden;
  ## real brackets, including a comparator's own `IN (...)`, are still there
  ## to be counted.
  masked <- .mask_literals(rest)
  if (is.null(masked)) return(NULL)
  scan <- masked$text

  inner <- .wrapped_inner(scan, "(", ")") %||%
    .wrapped_inner(scan, "[", "]") %||% scan

  keyed <- regmatches(inner, regexec(.RE_ENVELOPE_KEYWORD, inner,
                                     perl = TRUE))[[1]]
  if (length(keyed) != 2L) return(NULL)

  ## The author's own text again, before anything reads it as a condition.
  filter <- trimws(.unmask_literals(keyed[[2L]], masked))
  if (!nzchar(filter)) return(NULL)
  list(dataset = head[[2L]], variable = head[[3L]], filter = filter)
}

#' Qualify every bare variable in a filter against one dataset.
#'
#' @param resolves `function(dataset, variable)` answering whether that exact
#'   pair exists in the study's ADaM spec.
#' @return The filter with each bare operand written `DATASET.VARIABLE`, or
#'   `NULL` when any one of them cannot be confirmed -- all-or-none, because a
#'   filter honoured in part restricts by less than was written.
#' @noRd
.qualify_bare_operands <- function(filter, dataset, resolves) {
  ## Values are masked throughout: a bare name inside a quoted literal is part
  ## of the value, and rewriting it would change what the filter selects.
  masked <- .mask_literals(filter)
  if (is.null(masked)) return(NULL)
  txt <- masked$text

  found <- list()
  for (re in .RE_BARE_OPERAND) {
    at <- gregexpr(re, txt, perl = TRUE)[[1L]]
    if (at[[1L]] < 0L) next
    starts <- attr(at, "capture.start")[, 1L]
    lens   <- attr(at, "capture.length")[, 1L]
    for (i in seq_along(starts)) {
      name <- substr(txt, starts[[i]], starts[[i]] + lens[[i]] - 1L)
      if (toupper(name) %in% .ENVELOPE_RESERVED) next
      found[[length(found) + 1L]] <- list(start = starts[[i]], name = name)
    }
  }
  ## An envelope whose clauses all name their own dataset asks nothing of the
  ## spec: there is no inheritance to confirm, so it needs no predicate.
  if (length(found) == 0L) return(filter)

  ## Every bare name must be confirmed before ANY is rewritten. Without a
  ## predicate nothing is confirmable, and the safe answer is the same as a
  ## failed confirmation.
  if (!is.function(resolves)) return(NULL)
  for (hit in found) {
    if (!isTRUE(resolves(dataset, hit$name))) return(NULL)
  }

  ## Right to left, so an earlier rewrite cannot move a later position.
  order_desc <- order(vapply(found, function(h) h$start, numeric(1)),
                      decreasing = TRUE)
  for (i in order_desc) {
    at <- found[[i]]$start
    txt <- paste0(substr(txt, 1L, at - 1L), dataset, ".",
                  substr(txt, at, nchar(txt)))
  }
  .unmask_literals(txt, masked)
}

#' The condition an annotation states, with its envelope read and its bare
#' operands qualified against the head dataset.
#'
#' @return `NULL` when the annotation states no condition, a WhereClause when
#'   it states one this grammar can read, or an unresolved-condition object.
#'   The unresolved object carries the AUTHOR's text, never the rewritten
#'   form -- a finding has to quote what was written.
#' @noRd
.annotation_condition <- function(ann, resolves = NULL) {
  env <- .annotation_envelope(ann)
  if (is.null(env)) return(parse_where_clause(ann))

  qualified <- .qualify_bare_operands(env$filter, env$dataset, resolves)
  if (is.null(qualified)) {
    diag_add(
      stage = "where_clause", severity = "WARN",
      problem = paste("A filter's bare variable could not be confirmed on",
                      "the dataset its annotation names"),
      location = as.character(ann)[1],
      action = paste("Results are reserved rather than computed. A bare name",
                     "inherits the head reference's dataset only when the",
                     "ADaM spec confirms that exact DATASET.VARIABLE; a",
                     "filter naming a variable the study does not have is",
                     "not a filter. Qualify the variable, or supply a typed",
                     "condition through the supplement.")
    )
    return(.unresolved_condition(as.character(ann)[1], env$filter))
  }

  where <- parse_where_clause(qualified)
  if (.is_unresolved_condition(where)) {
    ## Re-stated in the author's own words: the rewritten text is this
    ## package's working form and was never on the page.
    return(.unresolved_condition(as.character(ann)[1], env$filter))
  }
  where
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

  ## The Boolean structure of the expression, PARSED. Everything the parser
  ## must not read as structure -- an atomic form that owns a bracket, a note
  ## in brackets -- is hidden first; see `.mask_non_structural()`.
  structural <- .mask_non_structural(expr)
  if (is.null(structural)) {
    diag_add(
      stage = "where_clause", severity = "WARN",
      problem = "Condition structure could not be separated from its text",
      location = original,
      action = paste("Results reserved -- the annotation uses private-use",
                     "characters this parser reserves for internal markers.",
                     "Remove them and re-run.")
    )
    return(.unresolved_condition(original))
  }

  ## Any span of the doubly-masked text, back to what the author wrote. The
  ## leaf grammar and every diagnostic see the real thing, never a token.
  restore <- function(text) {
    .unmask_literals(.unmask_non_structural(text, structural), masked)
  }

  tokens <- .lex_boolean(structural$text)
  joined <- any(vapply(tokens,
                       function(tok) tok[["type"]] %in% c("and", "or", "not"),
                       logical(1)))

  where <- NULL
  unparsed <- character(0)
  ## Operands whose text stated more than the leaf grammar read. These always
  ## reserve: by construction the leftover carries an operator.
  unread <- character(0)
  ## Did anything in this expression parse into a condition? An operand that
  ## states none means two different things depending on the answer. In
  ## "safety population or better" nothing did, so the words are a sentence
  ## and reserving them would withhold a result that was never at risk. In
  ## "A='Y' AND (per protocol)" one operand did, so the author was joining
  ## conditions -- and the operand that states none is a term that vanished.
  parsed_any <- FALSE

  if (!joined) {
    ## No operator anywhere, so there is no structure to parse and the whole
    ## expression is one clause. Brackets alone cannot group: with nothing to
    ## bind against, "(ADSL.SAFFL='Y')" and "ADQX.QXVAL GT 0 (per protocol)"
    ## mean exactly what they say, and sending them through the tree parser
    ## would refuse a punctuation habit and withhold correct results.
    part <- trimws(restore(structural$text))
    if (nzchar(part)) {
      ## The only place a comma list is read: the expression states no
      ## Boolean operator, so combining the items as one AND cannot be
      ## deciding how a comma binds against an AND or an OR that is also
      ## present. Where one IS present, the comma reserves.
      read <- .read_atom(part, commas = TRUE)
      where <- read$condition
      parsed_any <- !is.null(read$condition)
      if (!is.null(read$residue)) {
        unread <- part
      } else if (is.null(read$condition)) {
        unparsed <- part
      }
    }
  } else {
    tree <- .parse_boolean(tokens)
    if (identical(tree$kind, "error")) {
      ## Only an expression that ATTEMPTS a condition is refused for its
      ## structure. Prose carries English words this parser reads as
      ## operators -- "not applicable", "safety population or better" -- and
      ## reserving a row whose annotation states no filter withholds a result
      ## that was never at risk. Same evidence rule the dropped-clause branch
      ## below applies, and for the same reason.
      if (!grepl(.RE_CONDITION_EVIDENCE, expr, perl = TRUE)) return(NULL)
      diag_add(
        stage = "where_clause", severity = "WARN",
        problem = sprintf(
          "Condition has %s, so its Boolean structure could not be read",
          tree$reason),
        location = original,
        action = paste("Results are reserved rather than computed: a tree",
                       "built from part of the expression restricts by",
                       "something the author did not write. Restate the",
                       "condition, or supply a typed condition through the",
                       "supplement.")
      )
      return(.unresolved_condition(original, original))
    }

    built <- .where_from_boolean(tree, restore)
    if (isTRUE(built$negation)) {
      diag_add(
        stage = "where_clause", severity = "WARN",
        problem = "Condition uses negation, which this grammar cannot represent",
        location = original,
        action = paste("Results are reserved rather than computed. Nothing",
                       "downstream executes a NOT, and for a condition on",
                       "another dataset the expression does not say whether",
                       "the negation applies to the record or to the",
                       "subject. Restate it with a negative comparator",
                       "(NE, NOT IN, not missing), or supply a typed",
                       "condition through the supplement.")
      )
      return(.unresolved_condition(original, original))
    }
    where <- built$where
    unparsed <- built$dropped
    unread <- built$unread
    parsed_any <- isTRUE(built$parsed_any)
  }

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
  ## An operand of a joined expression is held to a lower bar than a lone
  ## annotation, and deliberately.
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
  ## Text that reserves whatever the evidence test would say about it. The
  ## evidence test asks "does this look like a filter?", which is the right
  ## question for a lone annotation and the wrong one for an operand: the
  ## author already answered it by writing a joiner.
  reserve_regardless <- character(0)

  for (u in unread) {
    reserve_regardless <- c(reserve_regardless, u)
    diag_add(
      stage = "where_clause", severity = "WARN",
      problem = "A condition was read, and text stating another was left over",
      location = u,
      action = paste("Results are reserved rather than computed on the part",
                     "that was read: the remaining text states a condition",
                     "this grammar did not apply, so the filter would be",
                     "weaker than the annotation asks for. Join the",
                     "conditions with AND/OR, or supply a typed condition",
                     "through the supplement.")
    )
  }

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
    if (joined && parsed_any) {
      ## An operand of an expression whose other operands ARE conditions.
      reserve_regardless <- c(reserve_regardless, u)
      diag_add(
        stage = "where_clause", severity = "WARN",
        problem = "An operand of a joined condition states no condition",
        location = u,
        action = paste("Results are reserved rather than computed. The",
                       "joiner says this text is a term of the restriction,",
                       "and a term that evaluates to nothing removes itself",
                       "from the filter without saying so.")
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
  unreadable <- c(Filter(.has_condition_evidence, dropped), reserve_regardless)
  if (length(unreadable) > 0) {
    return(.unresolved_condition(original, unreadable))
  }

  where
}

#' One leaf pattern against an atom, recording the span it consumed.
#'
#' The battery in `.one_condition()` is unanchored and returns on the first
#' recognised form. That is deliberate -- it is what lets a condition sit
#' inside descriptive text -- but it means the match alone does not say
#' whether the WHOLE operand was read. The span is what makes the remainder
#' answerable. Anchoring every pattern instead would change what each one
#' accepts; recording where it matched changes nothing about the reading.
#' @noRd
.leaf_match <- function(re, part, report = NULL, ignore.case = FALSE) {
  at <- regexec(re, part, perl = TRUE, ignore.case = ignore.case)[[1]]
  if (at[[1L]] < 0L) return(character(0))
  if (!is.null(report)) {
    report$span <- c(at[[1L]], at[[1L]] + attr(at, "match.length")[[1L]] - 1L)
  }
  regmatches(part, list(at))[[1L]]
}

#' Parse one atomic clause into an ARS WhereClauseCondition object (or, for
#' BETWEEN, a compoundExpression of GE + LE). Branch order matters: more
#' specific forms before less specific ones.
#'
#' @param report An environment, or `NULL`. When supplied, `report$span`
#'   receives the character positions the recognised form occupied.
#' @noRd
.one_condition <- function(part, report = NULL) {
  if (!is.null(report)) report$span <- NULL
  ## Either quote character, and only when the two ends match -- so a value
  ## that legitimately contains the other quote ("O'Brien") survives intact.
  strip_q <- function(x) sub("^(['\"])(.*)\\1$", "\\2", x)

  ## Range: DATASET.VARIABLE between lo ~AND~ hi -> (GE lo) AND (LE hi).
  ## ARS v1.0 has no BETWEEN comparator, so emit the conformant compound.
  m <- .leaf_match(.RE_BETWEEN, part, report)
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
  m <- .leaf_match(.RE_CONDITION_IN_LIST, part, report)
  if (length(m) == 5) {
    comp <- if (grepl("NOT", toupper(m[4]))) "NOTIN" else "IN"
    vals <- regmatches(m[5], gregexpr(.RE_IN_ITEM, m[5], perl = TRUE))[[1]]
    vals <- vapply(vals, strip_q, character(1), USE.NAMES = FALSE)
    return(.cond_multi(m[2], m[3], comp, vals))
  }
  ## ARS-style: DATASET.VARIABLE EQ 'value'
  m <- .leaf_match(.RE_CONDITION_ARS, part, report)
  if (length(m) == 5) {
    return(.cond(m[2], m[3], m[4], strip_q(m[5])))
  }
  ## Unquoted numeric: DATASET.VARIABLE GE 65
  m <- .leaf_match(.RE_CONDITION_NUM, part, report)
  if (length(m) == 5) {
    return(.cond(m[2], m[3], m[4], m[5]))
  }
  ## Equality: DATASET.VARIABLE='value'
  m <- .leaf_match(.RE_CONDITION_EQ, part, report)
  if (length(m) == 4) {
    return(.cond(m[2], m[3], "EQ", strip_q(m[4])))
  }
  ## Unquoted numeric equality: DATASET.VARIABLE=1
  m <- .leaf_match(.RE_CONDITION_EQ_NUM, part, report)
  if (length(m) == 4) {
    return(.cond(m[2], m[3], "EQ", m[4]))
  }
  ## Substring: DATASET.VARIABLE contains 'text' (arsbridge extension).
  m <- .leaf_match(.RE_CONTAINS, part, report)
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
  m <- .leaf_match(.RE_ISNA_NEG, part, report, ignore.case = TRUE)
  if (length(m) == 3) {
    return(.cond(m[2], m[3], "NE", NA_character_))
  }
  m <- .leaf_match(.RE_ISNA_POS, part, report, ignore.case = TRUE)
  if (length(m) == 3) {
    return(.cond(m[2], m[3], "EQ", NA_character_))
  }
  ## Null checks: "not null/missing" BEFORE the positive form.
  m <- .leaf_match(.RE_NULL_CHECK, part, report, ignore.case = TRUE)
  if (length(m) == 3) {
    return(.cond(m[2], m[3], "NE", NA_character_))
  }
  m <- .leaf_match(.RE_IS_NULL, part, report, ignore.case = TRUE)
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
#' own.
#'
#'   ADQX.QXSEV='SEVERE' AND ADQX.QXFL='Y'    pinned: one value of QXSEV
#'                                            survives the filter.
#'   ADQX.QXVAL WHERE QXPARM='P1'
#'              AND ADQX.QXVAL GT 0           bounded: QXVAL still takes many
#'                                            values inside the subset.
#'
#' Only equality pins: `EQ` with a single value, or `IN` with a list of one.
#' A threshold, a range, an inequality and a presence test all leave the
#' variable free to vary.
#'
#' Read only under an all-AND tree: under an OR a "pinning" conjunct may sit on
#' a branch that was not taken, so it pins nothing.
#'
#' WHAT THIS IS NOT. It is not evidence that the line reports a count. A shell
#' may perfectly well filter `AVAL = 30` and then ask for Mean/SD -- the filter
#' still only says which observations survive, and what is reported about them
#' is stated by the shell. `.infer_row_method()` currently treats a pinned
#' restriction as a count anyway, and that is a TEMPORARY dependency of the
#' block builder rather than a semantic rule; see the comment at its use site.
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
