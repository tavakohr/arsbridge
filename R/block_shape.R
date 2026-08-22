## arsbridge -- block_shape.R
## ---------------------------------------------------------------------------
## These functions separate four questions that the legacy path answers as one:
##
##   structure    where and how the row's result is drawn
##   statistic    what the shell asks to report
##   method       which supported computation can provide it
##   restriction  which observations are included
##
## HOW MUCH OF THIS IS LIVE (PR5b-2). Partly, and deliberately unevenly:
##
##   `.block_shape()`    ACTIVE, for one decision. The builder asks it whether
##                       a row expands over data levels, and that answer alone
##                       decides whether the row claims the rows beneath it as
##                       its levels. Nothing else it returns is acted on.
##   `.requested_statistic()`, `.resolve_method()`
##                       DORMANT. `.infer_row_method()` remains the active
##                       production path for both. These answer in parallel so
##                       their verdicts can be compared against the legacy
##                       decision before either is given control, in PR5b-3.
##
## WHAT A RESTRICTION MAY AND MAY NOT DECIDE. The fourth question is no longer
## absent from this file, and the distinction that replaced its absence is the
## load-bearing one:
##
##   restriction  -->  may prove a STRUCTURAL relationship
##   restriction  -/->  the requested statistic
##   restriction  -/->  the method
##
## Structural reasoning may inspect the canonical resolved restriction -- the
## one `.row_restriction_view()` produces, whatever channel supplied it -- to
## prove parent/child, level-refinement and partition relationships between
## rows. That is reading what the author drew: rows that select values of the
## variable the row above them names ARE that row's levels, and the conditions
## saying so are the evidence.
##
## What a restriction still may not do is choose what is REPORTED. It says
## which records survive; it says nothing about what statistic is asked of
## them, and nothing about which method computes it. "This row's filter pins
## its own variable, therefore the row reports a count" is the inference this
## migration exists to remove, and no function here draws it.
##
## Written after a first attempt replaced the legacy path outright and broke 64
## assertions, none of which was an approved change. Five contracts had been
## hanging off the method, and each is now stated explicitly and marked
## CONTRACT below, so that handing control over later cannot quietly drop it.

## Where a block's rows come from. Not a shape -- the shape follows from it --
## but the evidence the shape is read off, kept separate so an unproved shape
## can say WHY it is unproved.
##
##   authored_stats  the rows beneath it are authored statistic lines
##   data_levels     the rows come from the data, one per level of a variable
##   none            there are no further rows; the result is on this line
##   unknown         the evidence is absent, or it points two ways at once
.EXPANSION_SOURCES <- c("authored_stats", "data_levels", "none", "unknown")

## How each catalogue method lays its result out. Package knowledge about the
## catalogue, not about any study: a count-and-percentage draws one line per
## observed level while a subject count draws one line, whatever is being
## counted. It cannot be derived from the operations -- both declare `count`
## and `pct` -- because the difference is in the grouping, not the statistics.
.METHOD_STRUCTURE <- c(
  MTH_COUNT_AND_PERCENTAGE          = "categorical_block",
  MTH_AE_FREQUENCY_COUNT            = "categorical_block",
  MTH_SUBJECT_COUNT                 = "scalar_row",
  MTH_SUBJECT_COUNT_PCT             = "scalar_row",
  MTH_SUMMARY_STATISTICS_CONTINUOUS = "stat_block",
  MTH_KAPLAN_MEIER_ESTIMATE         = "stat_block"
)

## What each counting method counts. An authored `once/subject VAR` clause is a
## statement about counting CARDINALITY, and cardinality is an independent
## constraint -- not a rewrite applied to one particular method id after the
## fact, which is how the directive came to be droppable.
##
## Two methods can declare the same statistics, lay out the same way, and still
## differ here: a count-and-percentage counts records within each level, an AE
## frequency count counts each subject once within each level. The same shape
## of number, and a different number.
##
## NA means the question does not arise -- a continuous summary is not counting
## anything, so neither answer applies and the constraint leaves it alone.
.METHOD_CARDINALITY <- c(
  MTH_COUNT_AND_PERCENTAGE          = "record",
  MTH_AE_FREQUENCY_COUNT            = "distinct_subject",
  MTH_SUBJECT_COUNT                 = "distinct_subject",
  MTH_SUBJECT_COUNT_PCT             = "distinct_subject",
  MTH_SUMMARY_STATISTICS_CONTINUOUS = NA_character_,
  MTH_KAPLAN_MEIER_ESTIMATE         = NA_character_
)

#' One character value, with NA folded to "".
#'
#' `%||%` only answers for NULL, and an absent shell cell arrives as NA. Every
#' `nzchar()` and `grepl()` below runs on the result of this, so a missing
#' label cannot reach an `if` as NA.
#' @noRd
.shape_text <- function(x) {
  if (is.null(x) || length(x) == 0L) return("")
  x <- as.character(x)[[1]]
  if (is.na(x)) "" else x
}

#' The same, showing an unknown value as "?" -- for messages, where an empty
#' string reads as if the field were blank rather than unknown.
#' @noRd
.shape_or_q <- function(x) {
  v <- .shape_text(x)
  if (nzchar(v)) v else "?"
}

## ---------------------------------------------------------------------------
## The conservative header-suffix reader
## ---------------------------------------------------------------------------

#' The statistics a DESCRIPTIVE header states in its trailing form.
#'
#' Separate from `.parse_stat_label()` on purpose, and it must stay separate.
#' That function is a WHOLE-label grammar: a label naming a statistic plus
#' anything unrecognised is not a statistic line, and rejecting wholesale is
#' what stops "Median of prior therapies" binding a median. Loosening it so it
#' could read "Age group, n (%)" would loosen it for every statistic LINE too,
#' and the rows that grammar protects are the ones whose placeholder slots
#' would then bind to the wrong statistics.
#'
#' So this reads a different thing. A header is prose followed by a bounded
#' statement of the form its rows take. The rule:
#'
#'   * split at the LAST comma or spaced dash, and take what follows;
#'   * that fragment must parse WHOLLY through the statistic-line grammar.
#'
#' No prose is searched for statistic words, and no fragment is accepted
#' partially. "Sex, n (%) - Female" therefore yields nothing: its trailing
#' fragment is "Female", which is a level, not a request.
#'
#' @return An ordered character vector of statistic tokens, or NULL.
#' @noRd
.header_suffix_stats <- function(label) {
  lab <- trimws(.shape_text(label))
  if (!nzchar(lab)) return(NULL)

  ## The separators a header uses to introduce its form. A bare hyphen inside
  ## a word ("Non-serious") is not one, so the dash must be spaced.
  seps <- gregexpr("(,|\\s[-\u2010-\u2015\u2212]\\s)", lab, perl = TRUE)[[1]]
  if (seps[[1]] == -1L) return(NULL)

  last <- seps[[length(seps)]]
  len  <- attr(seps, "match.length")[[length(seps)]]
  suffix <- trimws(substr(lab, last + len, nchar(lab)))

  ## A footnote marker glued to the end of a header ("n (%)d"). The statistic-
  ## line grammar strips the marker forms it meets on statistic LINES; a header
  ## meets a wider set, because authors mark headers more often than the rows
  ## beneath. Stripped here, inside the bounded fragment, rather than by
  ## widening that grammar for every label in the package.
  suffix <- trimws(sub("[\u00aa\u00b2\u00b3\u00b9\u00ba\u02b0-\u02ff\u1d2c-\u1d6a\u2070-\u209f]+$", "", suffix, perl = TRUE))
  if (!nzchar(suffix)) return(NULL)

  ## A long trailing fragment is prose that happens to end the label, not a
  ## statement of form. Statistic forms are short ("n", "n (%)", "Mean (SD)").
  if (nchar(suffix) > 24L) return(NULL)

  .parse_stat_label(suffix)
}

#' Does this label LEAD with a statistic name?
#'
#' The difference between a label silent about statistics and one that asked
#' for something the whole-label grammar refused. "Average daily dose" and
#' "Mean arterial pressure" lead with a statistic and do not parse as statistic
#' lines: they asked, and answering with a count instead would be exactly the
#' substitution that grammar's wholesale rejection prevents.
#'
#' Leading position, not presence anywhere. Presence refuses "Adverse events of
#' special interest" over `events` and "TEAE by maximum severity" over `max`,
#' neither of which is a statistic request in any reading.
#'
#' Read over leading PREFIXES, bounded by the vocabulary's own longest phrase,
#' so that two- and four-word statistics ("standard deviation", "95 %
#' confidence interval") count as leading. `.match_stat_phrases(whole = TRUE)`
#' returns a character vector or NULL -- not the logical the `whole = FALSE`
#' contract returns -- and the prefix must be consumed entirely.
#' @noRd
.label_leads_with_statistic <- function(label) {
  toks <- .stat_tokens_of(.norm_stat_label(.shape_text(label)))
  for (k in seq_len(min(.stat_index_width(), length(toks)))) {
    if (!is.null(.match_stat_phrases(toks[seq_len(k)], whole = TRUE))) {
      return(TRUE)
    }
  }
  FALSE
}

#' Is this token set a COUNT presentation -- the form a level distribution
#' takes? Corroboration only, for a shape that already has a binding behind it.
#' @noRd
.is_count_presentation <- function(tokens) {
  length(tokens) > 0L && all(tokens %in% c("count", "pct"))
}

## ---------------------------------------------------------------------------
## Reading the authored relationships around a row
## ---------------------------------------------------------------------------

#' The annotation reduced to a single qualified variable reference, or "".
#'
#' A bare reference is the author naming an AXIS: "these rows are the values of
#' this variable". Anything else -- a condition, a head directive, a derivation
#' -- is the author saying something more specific, and an axis reading is then
#' not available. Two authored clauses pass through because they qualify HOW
#' the variable is counted rather than which of its values is meant.
#' @noRd
.bare_variable_reference <- function(ann) {
  text <- .shape_text(.annotation_less_derivation_note(.shape_text(ann)))
  if (!nzchar(trimws(text))) return("")

  text <- gsub("(?i);?\\s*once\\s*/\\s*subject\\s+[A-Za-z0-9_.]+", " ", text,
               perl = TRUE)
  text <- gsub("(?i);?\\s*sort\\s*:\\s*[^;]+", " ", text, perl = TRUE)
  text <- trimws(gsub("\\s+", " ", gsub(";", " ", text, fixed = TRUE)))
  if (!nzchar(text)) return("")

  refs <- extract_annotation_vars(text)
  if (length(refs) != 1L) return("")
  ## What remains once the reference is removed must be nothing. Residue is the
  ## author saying something the axis reading does not account for.
  residue <- trimws(sub(refs[[1]], "", text, fixed = TRUE))
  if (nzchar(gsub("[[:punct:][:space:]]", "", residue))) return("")
  toupper(refs[[1]])
}

#' What a resolved restriction says about ONE variable.
#'
#' The structural stage reasons about RESTRICTIONS, and a restriction is not a
#' string. It may have been authored in the annotation, supplied as a typed
#' supplement clause, or carried by an enrichment; by the time anything asks
#' what it MEANS, precedence has already been settled once by
#' `.row_restriction()` and the answer is a clause. Reading the annotation text
#' again here would be a second reading of a question already answered -- and
#' it would be blind to every channel that does not go through the text, which
#' is exactly how a supplement-supplied level run came to look like a run of
#' unrelated scalars.
#'
#' Four answers, and each of the last three is load-bearing:
#'
#'   unrestricted  the restriction provably says nothing about this variable,
#'                 so the variable is free to take any of its values;
#'   enumerated    it confines the variable to a stated, finite set of values;
#'   narrowed      it speaks about the variable but names no finite set -- a
#'                 threshold, a range, an inequality. The variable is still
#'                 free to vary among the survivors, which is what keeps a run
#'                 of cumulative thresholds from reading as a partition;
#'   unknown       nothing can be proved. The restriction is unresolved, or it
#'                 joins its terms with something other than AND, where no term
#'                 restricts the result on its own. `unknown` must never be
#'                 treated as `unrestricted` -- that is the permissive
#'                 direction, and it is wrong.
#'
#' @param restriction A resolved restriction record: `list(status, where)` with
#'   status one of `none` / `clause` / `unresolved`, as `.row_restriction_view()`
#'   produces.
#' @return list(status, values)
#' @noRd
.restriction_domain_on <- function(restriction, dataset, variable) {
  unknown <- list(status = "unknown", values = character())
  free    <- list(status = "unrestricted", values = character())

  var <- toupper(.shape_text(variable))
  ds  <- toupper(.shape_text(dataset))
  if (!nzchar(var)) return(unknown)

  state <- .shape_text((restriction %||% list())$status)
  if (identical(state, "unresolved")) return(unknown)
  if (!identical(state, "clause")) return(free)

  where <- restriction$where
  if (is.null(where)) return(free)
  if (!.where_all_conjunctive(where)) return(unknown)

  mine <- Filter(function(a) .atom_speaks_of(a, ds, var), .where_atoms(where))
  if (length(mine) == 0) return(free)
  ## Two terms about one variable state a relationship this reader does not
  ## resolve (they may intersect to one value, or to none). The variable is
  ## restricted; which values survive is not established.
  if (length(mine) > 1) return(list(status = "narrowed", values = character()))

  atom <- mine[[1]]
  comparator <- toupper(.as_scalar_char(atom[["comparator"]]) %||% "EQ")
  values <- as.character(unlist(atom[["value"]] %||% list(), use.names = FALSE))
  if (!comparator %in% c("EQ", "IN") || !length(values) || anyNA(values)) {
    return(list(status = "narrowed", values = character()))
  }
  list(status = "enumerated", values = unique(values))
}

#' Does this atom speak about `ds.var`?
#'
#' Both halves are compared when both are stated: two ADaM datasets may carry a
#' variable of the same name, and in a table whose rows come from different
#' domains, matching on the name alone would attribute one domain's restriction
#' to another's variable.
#' @noRd
.atom_speaks_of <- function(atom, dataset, variable) {
  if (!identical(toupper(.as_scalar_char(atom[["variable"]]) %||% ""),
                 variable)) {
    return(FALSE)
  }
  a_ds <- toupper(.as_scalar_char(atom[["dataset"]]) %||% "")
  !(nzchar(dataset) && nzchar(a_ds) && !identical(a_ds, dataset))
}

#' Everything a restriction says APART FROM what it says about one variable.
#'
#' This is what makes a level collapse safe to perform. When a child row is
#' folded into the block above it, the child stops being computed on its own
#' terms: it is displayed from the parent's single computation, restricted to
#' one level. That is faithful only if the child's restriction was the parent's
#' restriction plus that level, and nothing else. A term the child states and
#' the parent does not would simply be discarded; a term the parent states and
#' the child does not would silently be imposed on it. Comparing the residues
#' is how both are caught.
#'
#' Compared as a SET of canonical atom signatures, so `A AND B` and `B AND A`
#' are the same restriction -- which they are -- without this becoming a
#' Boolean prover. Anything the canonical form cannot express is reported as
#' not known rather than as equal.
#' @return list(known, signature)
#' @noRd
.restriction_residue <- function(restriction, dataset, variable) {
  unknown <- list(known = FALSE, signature = character())
  empty   <- list(known = TRUE,  signature = character())

  state <- .shape_text((restriction %||% list())$status)
  if (identical(state, "unresolved")) return(unknown)
  if (!identical(state, "clause")) return(empty)

  where <- restriction$where
  if (is.null(where)) return(empty)
  if (!.where_all_conjunctive(where)) return(unknown)

  var <- toupper(.shape_text(variable))
  ds  <- toupper(.shape_text(dataset))
  sigs <- character()
  for (atom in .where_atoms(where)) {
    if (nzchar(var) && .atom_speaks_of(atom, ds, var)) next
    values <- as.character(unlist(atom[["value"]] %||% list(), use.names = FALSE))
    sigs <- c(sigs, sprintf(
      "%s.%s %s [%s]",
      toupper(.as_scalar_char(atom[["dataset"]]) %||% ""),
      toupper(.as_scalar_char(atom[["variable"]]) %||% ""),
      toupper(.as_scalar_char(atom[["comparator"]]) %||% "EQ"),
      paste(sort(values), collapse = "|")))
  }
  list(known = TRUE, signature = sort(unique(sigs)))
}

#' Does the child select values from the parent's own variable domain?
#'
#' The STRUCTURAL half of refinement, and the only half the layout question
#' needs: does this row name values of the axis the row above it names? It says
#' nothing about what else either restriction states.
#'
#' Conservative by construction, with three outcomes rather than two: `TRUE`
#' proves the relationship, `FALSE` disproves it, and `NA` means it could not
#' be established. Only `TRUE` may be acted on. `NA` never becomes evidence of
#' levels, because an unreadable restriction narrows something unknown, and
#' folding a row on that basis would compute it under a population nobody
#' stated.
#' @noRd
.restriction_refines_domain <- function(parent, child, dataset, variable) {
  parent_domain <- .restriction_domain_on(parent, dataset, variable)
  child_domain  <- .restriction_domain_on(child,  dataset, variable)
  if (identical(parent_domain$status, "unknown") ||
        identical(child_domain$status, "unknown")) {
    return(NA)
  }
  ## A parent already narrowed on its own variable is not that variable's
  ## axis; it is one selection among its values, whatever the rows beneath say.
  if (identical(parent_domain$status, "narrowed")) return(FALSE)
  if (!identical(child_domain$status, "enumerated")) return(FALSE)

  if (identical(parent_domain$status, "enumerated") &&
        !all(child_domain$values %in% parent_domain$values)) {
    return(FALSE)
  }
  TRUE
}

#' Is the child restriction the parent restriction, refined on one variable --
#' and NOTHING else?
#'
#' The full relation: the structural half above, plus the requirement that the
#' two restrictions agree on everything they say about every other variable.
#'
#' That second half is what makes a level collapse faithful. A collapsed child
#' stops being computed on its own terms -- it is displayed from the parent's
#' single computation, restricted to one level -- so the child's declared
#' restriction must be exactly the parent's plus that level. A term the child
#' states and the parent does not is silently DISCARDED by the collapse; a term
#' the parent states and the child does not is silently IMPOSED on it.
#'
#' NOT CONSULTED BY PRODUCTION YET. Defect A-23 records that today's collapse
#' can discard a child's extra restriction, and the correction -- factoring a
#' residue every child explicitly states up onto the parent -- is a change to
#' what is computed, not to what is structurally recognised. This function is
#' the proof obligation that correction will have to discharge; until then it
#' is exercised in isolation and `.restriction_partition_relation()` reports
#' its verdict without acting on it.
#' @noRd
.restriction_refines <- function(parent, child, dataset, variable) {
  structural <- .restriction_refines_domain(parent, child, dataset, variable)
  if (!isTRUE(structural)) return(structural)

  parent_rest <- .restriction_residue(parent, dataset, variable)
  child_rest  <- .restriction_residue(child,  dataset, variable)
  if (!parent_rest$known || !child_rest$known) return(NA)
  if (!identical(parent_rest$signature, child_rest$signature)) return(FALSE)
  TRUE
}

#' Do the rows beneath subdivide this row's variable?
#'
#' The one structural question the level channel asks. It is a fact about
#' LAYOUT -- did the author draw a variable's values out by hand? -- and it is
#' asked of resolved restrictions, so the answer is the same whether the shell
#' wrote the conditions in its annotations or a reviewed supplement supplied
#' them as typed clauses.
#'
#' Two shapes qualify, and they are not the same claim:
#'
#'   the parent leaves the variable FREE. Then the axis is the variable's own
#'   domain, which this reader cannot enumerate, so nothing about coverage can
#'   be checked and each child must name exactly ONE value. A multi-value child
#'   under a free parent may overlap its siblings and could not be shown not to.
#'
#'   the parent ENUMERATES the domain (`VAR in (A, B, C)`). Now the author has
#'   stated what is being subdivided, so a child naming several of those values
#'   is checkable -- and is checked: the children must be contained in the
#'   parent's set, must not overlap each other, and must together cover it
#'   exactly. Partial cover is not a subdivision; it is a selection.
#'
#' The walk stops at the first row that is not a refinement, so an unrelated
#' analysis below the block is never swept into it.
#'
#' `status` answers the LAYOUT question and is decided on domains alone --
#' today's behaviour, unchanged. `residue_match` answers a second, separate
#' question that nothing acts on yet: would folding these rows into the block
#' above preserve everything they declared?
#'
#'   TRUE   every child states exactly what the parent states, plus its level.
#'          The collapse is faithful.
#'   FALSE  the children state something the parent does not, or state
#'          different things from one another. The collapse discards or
#'          imposes a restriction, silently. This is defect A-23.
#'   NA     not established -- some restriction could not be read. "I could not
#'          check" is not "I checked and it differs", and a later unreadable
#'          row never erases a mismatch already proved.
#'
#' Reported rather than enforced ON PURPOSE. Enforcing it would change which
#' rows collapse, which changes computed populations -- a semantic correction
#' owing its own before/after proof, not something a structural handoff should
#' do on the way past. `common_residue` carries what the children share,
#' because that is the term the eventual correction lifts onto the parent; it
#' is `NULL` whenever they share nothing provable, so the answer can never
#' depend on which row happened to come first.
#' @return list(status = "proved" | "unproved", levels, n_children,
#'   residue_match, common_residue)
#' @noRd
.restriction_partition_relation <- function(parent, children, dataset,
                                            variable) {
  unproved <- list(status = "unproved", levels = character(), n_children = 0L,
                   residue_match = NA, common_residue = NULL)

  parent_domain <- .restriction_domain_on(parent, dataset, variable)
  declared <- identical(parent_domain$status, "enumerated")
  if (!declared && !identical(parent_domain$status, "unrestricted")) {
    return(unproved)
  }
  ## One declared value is a level, not a domain to subdivide.
  if (declared && length(parent_domain$values) < 2L) return(unproved)

  levels <- character()
  taken <- 0L
  ## The residue verdict, accumulated so that it does not depend on the order
  ## the rows happen to be in: a proved difference stays proved, and anything
  ## unreadable only ever weakens a TRUE to NA.
  children_differ <- FALSE
  unreadable <- FALSE
  common <- NULL
  have_common <- FALSE

  for (child in children) {
    if (!isTRUE(.restriction_refines_domain(parent, child, dataset, variable))) {
      break
    }
    values <- .restriction_domain_on(child, dataset, variable)$values
    if (!declared && length(values) != 1L) break
    if (length(intersect(values, levels)) > 0) break

    child_rest <- .restriction_residue(child, dataset, variable)
    if (!child_rest$known) {
      unreadable <- TRUE
    } else if (!have_common) {
      common <- child_rest$signature
      have_common <- TRUE
    } else if (!identical(common, child_rest$signature)) {
      children_differ <- TRUE
    }

    levels <- c(levels, values)
    taken <- taken + 1L
  }
  if (taken == 0L) return(unproved)
  if (declared && !setequal(levels, parent_domain$values)) return(unproved)

  ## No common residue exists to report when the children do not share one, and
  ## returning the first row's would make the answer row-order dependent.
  if (children_differ) {
    common <- NULL
    have_common <- FALSE
  }
  mismatch <- children_differ

  ## The children may agree with each other and still disagree with the parent,
  ## which is the shape A-23 was found in.
  parent_rest <- .restriction_residue(parent, dataset, variable)
  if (!parent_rest$known) {
    unreadable <- TRUE
  } else if (have_common) {
    if (!identical(parent_rest$signature, common)) mismatch <- TRUE
  } else if (!children_differ) {
    ## Nothing readable on either side to compare.
    unreadable <- TRUE
  }

  list(status = "proved", levels = levels, n_children = taken,
       residue_match = if (mismatch) FALSE else if (unreadable) NA else TRUE,
       common_residue = common)
}

#' Does this label name one exact level of the variable the row binds?
#'
#' Exact, case-insensitive equality against the codelist's terms and decodes.
#' Deliberately NOT the renderer's `.match_level()`, which falls back to prefix
#' and substring matching: that is the right rule for placing a computed level
#' into an authored slot, and the wrong rule for evidence, where a partial
#' match would let "Male" convict "Malignancy".
#'
#' Both the whole label and its last bounded fragment are tested, because a
#' header and its first level are sometimes authored on one line.
#' @return The matching level as authored, or "".
#' @noRd
.label_names_a_level <- function(label, decodes) {
  lab <- trimws(.shape_text(label))
  if (!nzchar(lab) || is.null(decodes) || NROW(decodes) == 0) return("")

  pool <- toupper(trimws(c(as.character(decodes$term %||% character()),
                           as.character(decodes$decode %||% character()))))
  pool <- pool[!is.na(pool) & nzchar(pool)]
  if (!length(pool)) return("")

  candidates <- lab
  seps <- gregexpr("(,|\\s[-\u2010-\u2015\u2212]\\s)", lab, perl = TRUE)[[1]]
  if (seps[[1]] != -1L) {
    last <- seps[[length(seps)]]
    len  <- attr(seps, "match.length")[[length(seps)]]
    candidates <- c(candidates, trimws(substr(lab, last + len, nchar(lab))))
  }
  for (cand in candidates) {
    if (nzchar(cand) && toupper(cand) %in% pool) return(cand)
  }
  ""
}

#' What the shell puts around one row, as evidence for its shape.
#'
#' Three relationships, all read from the authored row sequence and none from
#' any identifier:
#'
#' LEVEL CHILDREN -- rows beneath that subdivide this row's variable, read from
#' RESOLVED restrictions rather than from annotation text, so a supplement that
#' supplied the leaves as typed clauses proves the same layout the shell would
#' have proved by writing them out. `.restriction_partition_relation()` states
#' the rule; the run ends at the first row that is not a refinement, so an
#' unrelated analysis below is not swept in.
#'
#' SUBORDINATE RUN -- authored rows immediately beneath that carry no
#' annotation. That is exactly the run the RENDERER consumes as a block's
#' sub-lines, so shape evidence and rendering cannot disagree about which rows
#' a block owns.
#'
#' ENCLOSING HEADER -- a row above, written at a smaller indent than every row
#' between it and this one, which makes the enclosure structural rather than
#' adjacency. A shell that indents nothing proves no enclosure and the row
#' inherits nothing, which is the conservative direction.
#'
#' @param binding The row's RESOLVED primary binding. Reading the annotation
#'   again here would disagree with the rest of the pipeline: an axis is often
#'   written beside its siblings ("ADSL.STRAT1 / STRAT2 / STRAT3") or with an
#'   ordering clause, and which of those is primary is settled upstream.
#' @param restrictions The section's RESOLVED restrictions, one per row, as
#'   `.row_restriction_view()` produces them. The builder holds that view
#'   already and passes it in; the default is the same constructor, so there is
#'   one reading of restriction precedence and never a second.
#' @noRd
.row_layout_context <- function(rows, index, roles = NULL, indents = NULL,
                                binding = NULL, restrictions = NULL) {
  n <- length(rows)
  roles <- roles %||% rep(NA_character_, n)
  indents <- as.integer(indents %||% rep(0L, n))
  bind <- binding %||% list()
  restrictions <- restrictions %||% .row_restriction_view(rows)

  is_label_row <- function(k) {
    !isTRUE(rows[[k]]$has_annot) &&
      !.shape_text(roles[[k]]) %in% c("level_repeat", "nested_repeat")
  }

  ## --- authored level rows -------------------------------------------------
  own_ds  <- toupper(.shape_text(bind$dataset))
  own_var <- toupper(.shape_text(bind$variable))
  level_children <- character()
  if (nzchar(own_var)) {
    ## The candidate run: the authored rows immediately beneath that carry a
    ## restriction of their own. A row with no annotation is a sub-line of this
    ## block rather than a level of it, and ends the run.
    candidates <- list()
    k <- index + 1L
    while (k <= n && isTRUE(rows[[k]]$has_annot)) {
      candidates[[length(candidates) + 1L]] <- restrictions[[k]]
      k <- k + 1L
    }
    relation <- .restriction_partition_relation(restrictions[[index]],
                                                candidates, own_ds, own_var)
    if (identical(relation$status, "proved")) level_children <- relation$levels
  }

  ## --- the subordinate run -------------------------------------------------
  subordinate <- character()
  template_rows <- integer()
  k <- index + 1L
  while (k <= n) {
    role <- .shape_text(roles[[k]])
    if (role %in% c("level_repeat", "nested_repeat")) {
      ## A mock row the block expands over: not a sub-line, but a record of how
      ## many rows the author drew for the block to fill.
      sheet <- suppressWarnings(as.integer(rows[[k]]$sheet_row %||% NA_integer_))
      if (!is.na(sheet)) template_rows <- c(template_rows, sheet)
      k <- k + 1L
      next
    }
    if (!is_label_row(k)) break
    subordinate <- c(subordinate, .shape_text(rows[[k]]$label))
    k <- k + 1L
  }
  subordinate <- subordinate[nzchar(subordinate)]

  ## --- the enclosing header ------------------------------------------------
  ## `limit` is the shallowest indent seen between a candidate and this row. A
  ## candidate encloses only if it is shallower than ALL of them, which is what
  ## carrying that running minimum upward tests. Siblings at the same depth do
  ## not end the search -- two rows under one header are both enclosed by it.
  ## A shallower row stating no statistic form is not the header being looked
  ## for, so the search continues above it, now needing something shallower
  ## still. At indent 0 nothing can be shallower and the walk ends.
  limit <- indents[[index]]
  inherited <- character()
  inherited_from <- ""
  for (step in seq_len(index - 1L)) {
    j <- index - step
    if (indents[[j]] >= limit) next
    toks <- .header_suffix_stats(rows[[j]]$label)
    if (length(toks) > 0) {
      inherited <- toks
      inherited_from <- .shape_text(rows[[j]]$label)
      break
    }
    limit <- indents[[j]]
  }

  list(level_children = unique(level_children),
       subordinate_labels = subordinate,
       template_rows = template_rows,
       ## CONTRACT (root cause 4): the token-run role is EVIDENCE, produced
       ## upstream and immutable here. `.block_shape()` may weigh it; it must
       ## never be written back from the shape it helped prove, or a
       ## provisional role becomes its own proof.
       detected_role = .shape_text(roles[[index]]),
       inherited_stats = inherited,
       inherited_from = inherited_from)
}

## ---------------------------------------------------------------------------
## Stage 1 -- what SHAPE is this row?
## ---------------------------------------------------------------------------

#' What shape is this row, on structural evidence alone?
#'
#' Channel order is load-bearing, and is stated here because getting it wrong
#' is a silent-wrongness bug rather than a crash:
#'
#'   CONTRACT (root cause 3). Exact level/decode evidence outranks
#'   statistic-looking spelling. A codelist whose values are `Median` and
#'   `Range` is a set of levels; the words are a lexical coincidence. So the
#'   level channels are consulted BEFORE authored statistic children.
#'
#' @return list(shape, expansion_source, evidence, conflicts, reason). `shape`
#'   is NA when structure is not proved -- never a synonym for `scalar_row`,
#'   because "I could not tell" and "it is one row" lead to different work.
#' @noRd
.block_shape <- function(row, binding = NULL, layout_context = NULL) {
  ev   <- character()
  bad  <- character()
  ctx  <- layout_context %||% list()
  bind <- binding %||% list()

  label <- .shape_text(row$label)
  ann   <- .shape_text(row$annotation)

  out <- function(shape, source, reason) {
    list(shape = shape, expansion_source = source, evidence = ev,
         conflicts = bad, reason = reason)
  }

  ## The row's own label may name a LEVEL. Checked first because it is the
  ## evidence most often overridden by the others: a row labelled with one of
  ## its variable's values IS that value, whatever else is true of it.
  level_named <- .label_names_a_level(label, bind$decodes)
  if (nzchar(level_named)) {
    bad <- c(bad, sprintf("label names the level '%s' of %s.%s", level_named,
                          .shape_or_q(bind$dataset), .shape_or_q(bind$variable)))
  }

  ## --- Channel A: authored rows pin this variable, one value each ----------
  level_children <- as.character(ctx$level_children %||% character())
  if (length(level_children) > 0) {
    ev <- c(ev, sprintf("%d authored row(s) beneath pin %s.%s to one value each: %s",
                        length(level_children), .shape_or_q(bind$dataset),
                        .shape_or_q(bind$variable),
                        paste(level_children, collapse = ", ")))
    if (nzchar(level_named)) {
      return(out(NA_character_, "unknown",
                 "the rows beneath pin values of the bound variable, but so does this row's own label"))
    }
    return(out("categorical_block", "data_levels",
               "the authored rows beneath select one value each of the variable this row names"))
  }

  ## --- Channel B: subordinate labels decode against the bound variable -----
  subordinate <- as.character(ctx$subordinate_labels %||% character())
  decoded <- character()
  if (length(subordinate) > 0 && !is.null(bind$decodes)) {
    hits <- vapply(subordinate,
                   function(x) nzchar(.label_names_a_level(x, bind$decodes)),
                   logical(1))
    decoded <- subordinate[hits]
  }
  if (length(decoded) > 0 && length(decoded) == length(subordinate)) {
    ev <- c(ev, sprintf("every authored row beneath is a level of %s.%s: %s",
                        .shape_or_q(bind$dataset), .shape_or_q(bind$variable),
                        paste(decoded, collapse = ", ")))
    if (nzchar(level_named)) {
      return(out(NA_character_, "unknown",
                 "the rows beneath are levels of the bound variable, but so is this row's own label"))
    }
    return(out("categorical_block", "data_levels",
               "the authored rows beneath are levels of the variable this row binds"))
  }

  ## --- Channel C: an authored expansion template ---------------------------
  ## CONTRACT (root cause 4): the detected token role is read, never written.
  ## It is also not sufficient alone -- a run of repeats says the rows are
  ## mocks, not that the variable has levels -- so a binding the spec calls
  ## continuous cannot expand.
  n_template <- length(unlist(ctx$template_rows %||% integer()))
  is_self_template <- identical(.shape_text(ctx$detected_role), "self_template")
  if ((is_self_template || n_template > 0) && !identical(bind$discrete, FALSE)) {
    ev <- c(ev, sprintf("authored expansion template (%d mock row(s)%s)",
                        n_template,
                        if (is_self_template) ", self-template" else ""))
    return(out("categorical_block", "data_levels",
               "the shell authored an expansion template for this block"))
  }

  ## --- Channel D: authored statistic children ------------------------------
  ## Only now, after every level reading has been tried and failed.
  stat_children <- subordinate[
    vapply(subordinate, function(x) length(.parse_stat_label(x)) > 0L,
           logical(1))]
  if (length(stat_children) > 0) {
    ev <- c(ev, sprintf("owns %d authored statistic line(s): %s",
                        length(stat_children),
                        paste(stat_children, collapse = ", ")))
    if (nzchar(level_named)) {
      return(out(NA_character_, "unknown",
                 "the row owns authored statistic lines but its label names a level of its own variable"))
    }
    return(out("stat_block", "authored_stats",
               "authored statistic lines beneath the row state what it expands into"))
  }

  ## --- Channel E: a bare axis reference, corroborated ----------------------
  ## The weakest channel and the one that must not stand alone. A bare
  ## reference to a discrete variable is CANDIDATE evidence of an axis, and it
  ## is equally consistent with a scalar condition on that variable whose
  ## predicate a lossy reader dropped. Both readings admissible means the shape
  ## is not proved, so it needs a second, independent fact: the row presents
  ## itself in the form a level distribution takes.
  ##
  ## The subject key is excluded, and that is standards knowledge rather than
  ## study knowledge: USUBJID identifies subjects in every ADaM dataset, so a
  ## bare reference to it is never a variable axis.
  bare <- .bare_variable_reference(ann)
  axis_binding <- nzchar(bare) && isTRUE(bind$discrete) &&
    !identical(toupper(.shape_text(bind$variable)), "USUBJID")
  if (axis_binding) {
    ev <- c(ev, sprintf("annotation is a bare reference to the discrete variable %s",
                        bare))
    if (nzchar(level_named)) {
      return(out(NA_character_, "unknown",
                 "the annotation reads as an axis but the label names one level of that same variable"))
    }
    if (.is_count_presentation(.header_suffix_stats(label))) {
      ev <- c(ev, "the label states a count presentation for its rows")
      return(out("categorical_block", "data_levels",
                 "a bare axis reference to a discrete variable, presented as a level distribution"))
    }
    bad <- c(bad, "a bare discrete reference reads equally as an axis or as a condition on that variable")
    return(out(NA_character_, "unknown",
               "the row binds a discrete variable by a bare reference, with nothing to say whether it is that variable's axis or a condition on it"))
  }

  if (nzchar(level_named)) {
    return(out(NA_character_, "unknown",
               "the label names a level of its own variable but no block above it claims it"))
  }
  ev <- c(ev, "no level children, no expansion template, no statistic lines, no axis reading")
  out("scalar_row", "none", "the row's result lives on the row itself")
}

## ---------------------------------------------------------------------------
## Stage 2 -- what STATISTIC is the shell asking for?
## ---------------------------------------------------------------------------

#' The statistics one row asks for, and where each request was read from.
#'
#' Requests are ROW-GRANULAR, which is the second contract this file is built
#' around:
#'
#'   CONTRACT (root cause 1). A block's authored statistic lines are separate
#'   requests, not one union. A line naming a statistic no method can produce
#'   refuses ITSELF; it must not eliminate the method its supportable siblings
#'   need. So `requests` is a list -- one entry per authored line plus one for
#'   the row's own claim -- and `.resolve_method()` intersects only the entries
#'   some method can satisfy.
#'
#' Sources for the row's OWN request, in the order consulted:
#'
#'   supplement*      a reviewed supplement that declared itself an override
#'   label            the whole label is a statistic line ("Mean (SD)")
#'   head_directive   the annotation asks for a count of subjects
#'   header_suffix    the label's own bounded trailing form
#'   inherited        the structurally enclosing header's trailing form
#'   supplement       a reviewed supplement, where the shell stated nothing
#'
#' The shell is ground truth, so a supplement is consulted LAST -- except where
#' it was reviewed and explicitly marked as overriding, the existing
#' `supplement_stat_override` contract.
#'
#' @return list(tokens, requests, child_requests, source, evidence, reason,
#'   refused, cardinality). `refused` marks a label that LED with a statistic
#'   the whole-label grammar would not bind: it asked for something, so silence
#'   is the wrong reading. `cardinality` carries an authored `once/subject`
#'   directive as an independent constraint (root cause 2) -- it survives
#'   whatever method is provisionally in view, because it is a statement about
#'   counting, not about a method id.
#' @noRd
.requested_statistic <- function(row, layout_context = NULL) {
  ctx   <- layout_context %||% list()
  label <- .shape_text(row$label)
  ann   <- .shape_text(.annotation_less_derivation_note(.shape_text(row$annotation)))

  ## Every authored statistic line beneath, each as its own request.
  subordinate <- as.character(ctx$subordinate_labels %||% character())
  child <- list()
  for (lab in subordinate) {
    toks <- .parse_stat_label(lab)
    if (length(toks) > 0) {
      child[[length(child) + 1L]] <- list(label = lab, tokens = toks,
                                          source = "stat_child")
    }
  }

  ## CONTRACT (root cause 2): authored counting cardinality, read from the
  ## annotation and independent of any method.
  once_var <- .once_per_subject_var(row$annotation)
  cardinality <- if (!is.null(once_var)) "distinct_subject" else NA_character_

  finish <- function(tokens, source, evidence, reason = "", refused = FALSE) {
    own <- if (length(tokens) > 0) {
      list(list(label = label, tokens = tokens, source = source))
    } else {
      list()
    }
    list(tokens = tokens, requests = c(own, child), child_requests = child,
         source = source, evidence = evidence, reason = reason,
         refused = refused, cardinality = cardinality)
  }

  supp <- as.character(row$supplement_stat_tokens %||% character())
  supp <- supp[!is.na(supp) & nzchar(supp)]
  if (length(supp) > 0 && isTRUE(row$supplement_stat_override)) {
    return(finish(supp, "supplement",
                  "a reviewed supplement stated the row's statistics"))
  }

  own <- .parse_stat_label(label)
  if (length(own) > 0) {
    return(finish(own, "label",
                  sprintf("the whole label is a statistic line: %s",
                          paste(own, collapse = ", "))))
  }

  ## The placeholder may declare that the cell shows a percentage beside its
  ## count. Presentation evidence: it decides what the cell SHOWS, never how
  ## many rows the shell draws.
  slots <- suppressWarnings(as.integer(row$n_slots %||% NA_integer_))
  has_pct_slot <- length(slots) == 1L && !is.na(slots) && slots >= 2L
  if (grepl("(?i)\\bcount\\s+of\\b|(?i)\\bunique\\s+USUBJID\\b", ann, perl = TRUE) ||
      grepl(paste0("\\b", .ADAM_DS, "\\.USUBJID\\b"), ann, perl = TRUE)) {
    return(finish(if (has_pct_slot) c("count", "pct") else "count",
                  "head_directive",
                  "the annotation asks for a count of subjects"))
  }

  suffix <- .header_suffix_stats(label)
  if (length(suffix) > 0) {
    return(finish(suffix, "header_suffix",
                  sprintf("the label's trailing form asks for %s",
                          paste(suffix, collapse = ", "))))
  }

  inherited <- as.character(ctx$inherited_stats %||% character())
  inherited <- inherited[!is.na(inherited) & nzchar(inherited)]
  if (length(inherited) > 0) {
    return(finish(inherited, "inherited",
                  sprintf("the enclosing header '%s' asks for %s",
                          .shape_or_q(ctx$inherited_from),
                          paste(inherited, collapse = ", "))))
  }

  if (length(supp) > 0) {
    return(finish(supp, "supplement",
                  "a reviewed supplement stated the row's statistics"))
  }

  if (.label_leads_with_statistic(label)) {
    return(finish(character(), "refused", "",
                  reason = sprintf(
                    "the label '%s' leads with a statistic but is not a statistic line, so what it asks for cannot be bound",
                    label),
                  refused = TRUE))
  }

  finish(character(), "none", "",
         reason = "the shell states no statistic for this row")
}

## ---------------------------------------------------------------------------
## Stage 3 -- which method can legally satisfy shape + requests + metadata?
## ---------------------------------------------------------------------------

#' Eliminate methods that cannot satisfy the row, and see what is left.
#'
#' Elimination, not scoring. Every constraint is hard, and a method failing any
#' of them is out with a stated reason. What survives is one method (the
#' answer), none (reserve), or several (reserve, unless one declared rule
#' separates them). Nothing picks a "closest" method: one that nearly fits
#' writes a number that formats and reads as an answer.
#'
#' The order is deliberate. The constraints that do not depend on what was
#' ASKED FOR -- structure, cardinality, the variable's own type -- are applied
#' first and together form the BASELINE. Each statistic request is then judged
#' against that baseline, never against a set already narrowed by an earlier
#' request. Judging against a narrowed set would let the first request make a
#' later one look unsupported, so a line some method could have satisfied would
#' be silently ignored rather than honoured -- and the surviving method would
#' not satisfy every supportable request.
#'
#' @param requests A LIST of statistic requests, each `list(label, tokens)`.
#'   A request no baseline method can satisfy is recorded in
#'   `unsupported_requests` and eliminates nothing: it refuses its own row. The
#'   rest are intersected. An EMPTY intersection means two supportable requests
#'   contradict each other, which reserves rather than ignoring either.
#' @param cardinality "distinct_subject" when the shell authored `once/subject`,
#'   else NA. An independent constraint on how counting is done.
#' @param presentation How many statistics the placeholder draws (`n_slots`),
#'   or NA. Separates two methods left standing that differ only in how many
#'   statistics they report. It never selects a shape, and is never read when
#'   the shell stated what it wants.
#' @return list(method, candidates, eliminated, unsupported_requests,
#'   constraints_applied, reason). `candidates` is what SURVIVED.
#' @noRd
.resolve_method <- function(shape, requests = list(), binding = NULL,
                            methods = .STANDARD_METHODS,
                            cardinality = NA_character_,
                            presentation = NA_integer_) {
  bind <- binding %||% list()
  applied <- character()
  shape <- .shape_text(shape)

  ## An unproved shape is not rescued by a method. Choosing one for a row whose
  ## structure is unknown decides that structure by the back door, which is the
  ## circularity this file exists to break.
  if (!nzchar(shape)) {
    return(list(method = NULL, candidates = character(), eliminated = character(),
                unsupported_requests = character(),
                constraints_applied = "shape unproved",
                reason = "no method is chosen for a row whose shape is not proved"))
  }

  known <- vapply(methods, function(m) as.character(m$id), character(1))
  base <- names(.METHOD_STRUCTURE)
  base <- base[base %in% known]
  eliminated <- character()
  drop <- function(ids, why) {
    if (!length(ids)) return(invisible(NULL))
    eliminated <<- c(eliminated, sprintf("%s (%s)", ids, why))
  }

  ## --- Baseline constraint 1: structure ------------------------------------
  bad <- base[.METHOD_STRUCTURE[base] != shape]
  drop(bad, sprintf("lays out as %s, not %s", .METHOD_STRUCTURE[bad], shape))
  base <- setdiff(base, bad)
  applied <- c(applied, sprintf("structure = %s", shape))

  ## --- Baseline constraint 2: authored counting cardinality ----------------
  ## Independent of the method id, so an authored directive cannot be lost
  ## because some other method happens to report the same numbers.
  ##
  ## Both directions are stated. Where the shell asked for distinct subjects,
  ## record counters are out. Where it asked for nothing, a method that would
  ## count each subject once is making a claim the shell did not -- so among
  ## methods that differ ONLY in cardinality, the record-counting reading is
  ## the default. Applied only when a record-counting alternative actually
  ## exists, so the scalar counts (which count subjects by definition, and have
  ## no record-counting sibling) are untouched.
  card <- .shape_text(cardinality)
  cardinality_of <- function(ids) unname(.METHOD_CARDINALITY[ids])
  if (identical(card, "distinct_subject")) {
    bad <- base[!cardinality_of(base) %in% c("distinct_subject", NA)]
    drop(bad, "counts records, but the shell asked for distinct subjects")
    base <- setdiff(base, bad)
    applied <- c(applied, "cardinality = distinct subject (authored)")
  } else if (any(cardinality_of(base) %in% "record")) {
    bad <- base[cardinality_of(base) %in% "distinct_subject"]
    drop(bad, "counts each subject once, which the shell did not ask for")
    base <- setdiff(base, bad)
    applied <- c(applied, "cardinality = record (nothing authored)")
  }

  ## --- Baseline constraint 3: what the spec says the variable is -----------
  ## A continuous summary of a text variable is NaN; a level distribution over
  ## a free numeric measure is one row per distinct number.
  if (length(base) > 0 && !is.null(bind$discrete) && !is.na(bind$discrete)) {
    bad <- character()
    for (id in base) {
      form <- .METHOD_STRUCTURE[[id]]
      if (identical(form, "stat_block") && isTRUE(bind$discrete)) {
        bad <- c(bad, id)
        drop(id, "summarises a discrete variable as continuous")
      }
      if (identical(form, "categorical_block") && identical(bind$discrete, FALSE)) {
        bad <- c(bad, id)
        drop(id, "distributes the levels of a non-discrete variable")
      }
    }
    base <- setdiff(base, bad)
    applied <- c(applied, sprintf("variable %s.%s is %s",
                                  .shape_or_q(bind$dataset),
                                  .shape_or_q(bind$variable),
                                  if (isTRUE(bind$discrete)) "discrete" else "continuous"))
  }

  support <- .stat_label_support(methods)
  declared <- function(id) {
    support$statistic[support$method == id & support$supported]
  }

  ## --- Each request, judged against the BASELINE, then intersected ---------
  unsupported <- character()
  stated <- 0L
  cand <- base
  conflict <- FALSE
  for (req in requests) {
    toks <- as.character(req$tokens %||% character())
    toks <- toks[!is.na(toks) & nzchar(toks)]
    if (!length(toks)) next
    stated <- stated + 1L
    fits <- base[vapply(base,
                        function(id) length(setdiff(toks, declared(id))) == 0L,
                        logical(1))]
    if (!length(fits)) {
      ## Row-local: this line asked for something no baseline method produces.
      ## It refuses itself and eliminates nothing.
      unsupported <- c(unsupported,
                       sprintf("%s (%s)", .shape_or_q(req$label),
                               paste(toks, collapse = ", ")))
      next
    }
    drop(setdiff(cand, fits),
         sprintf("cannot produce %s asked for by '%s'",
                 paste(toks, collapse = ", "), .shape_or_q(req$label)))
    cand <- intersect(cand, fits)
    applied <- c(applied, sprintf("statistics of '%s' = %s",
                                  .shape_or_q(req$label),
                                  paste(toks, collapse = ", ")))
    if (!length(cand)) {
      conflict <- TRUE
      break
    }
  }

  answer <- function(method, cands, extra = character()) {
    list(method = method, candidates = cands, eliminated = eliminated,
         unsupported_requests = unsupported,
         constraints_applied = c(applied, extra), reason = "")
  }
  reserve <- function(cands, why) {
    list(method = NULL, candidates = cands, eliminated = eliminated,
         unsupported_requests = unsupported, constraints_applied = applied,
         reason = why)
  }

  if (conflict) {
    return(reserve(character(),
                   "two authored statistic requests are each satisfiable but not by the same method"))
  }
  if (length(cand) == 0) {
    return(reserve(character(),
                   "no catalogue method satisfies the row's shape, statistics and variable together"))
  }
  ## Every request refused: the row stated what it wants, nothing produces it,
  ## so there is no positive method evidence to act on.
  if (stated > 0 && length(unsupported) == stated) {
    return(reserve(cand,
                   "every statistic the row asks for is beyond the admissible methods"))
  }
  if (length(cand) == 1) return(answer(cand[[1]], cand))

  ## Two declared tie-breaks, both exact equality, both resolving nothing when
  ## they do not match. Neither scores a "closest" method.
  if (stated > length(unsupported)) {
    asked <- unique(unlist(lapply(requests, function(r) r$tokens),
                           use.names = FALSE))
    exact <- cand[vapply(cand, function(id) setequal(declared(id), asked),
                         logical(1))]
    if (length(exact) == 1) {
      drop(setdiff(cand, exact), "declares statistics beyond those asked for")
      return(answer(exact[[1]], exact,
                    "declared precedence: exact statistic-set match"))
    }
  }

  ## Where the shell said NOTHING about statistics, the placeholder still shows
  ## how many the cell holds. Admissible here and nowhere else: the shape
  ## constraint has already settled what KIND of result the row reports, so
  ## this decides only how many of them the survivors produce.
  slots <- suppressWarnings(as.integer(presentation %||% NA_integer_))[1]
  if (stated == 0 && !is.na(slots)) {
    fits <- cand[vapply(cand, function(id) length(declared(id)) == slots,
                        logical(1))]
    if (length(fits) == 1) {
      drop(setdiff(cand, fits), "reports a different number of statistics")
      return(answer(fits[[1]], fits,
                    sprintf("declared precedence: the placeholder draws %d statistic(s)",
                            slots)))
    }
  }

  reserve(cand,
          sprintf("%d methods remain admissible (%s) and no declared rule separates them",
                  length(cand), paste(cand, collapse = ", ")))
}
