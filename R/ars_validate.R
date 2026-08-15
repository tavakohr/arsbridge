## arsbridge -- ars_validate.R
## ---------------------------------------------------------------------------
## Integrity checks over an ars_model. This is the layer that makes the review
## stage GUIDED correction rather than a generic JSON grid: it answers "what is
## wrong with this reporting event, and what should I do about it?"
##
## Everything here takes the model (and optionally an ADaM spec and the
## annotation validation report) as plain arguments -- no Shiny, no LLM, no
## file IO -- so the same findings are available from a script and from the
## editor's validation panel.
##
## Findings follow the shape ars_validate_supplement() established: one row per
## problem, ordered severity-first, with an action a reviewer can act on.

## Methods the engine can compute natively, plus the listing pass-through.
#' @noRd
.NATIVE_METHOD_IDS <- function() {
  c(names(.ARD_EXECUTORS), "MTH_LISTING")
}

## ---- semantic source ------------------------------------------------------
##
## An analysis may only compute from data its own annotation named. Everything
## below serves that one sentence.
##
## Why it exists: a display label is not an identity. The same stub text is a
## legitimate level of more than one variable, so when two rows share a label
## the second has been observed to inherit the first's variable and filter. The
## resulting number is well-formed, plausible, and computed from the wrong
## column. Where the two variables happen to agree in a given data cut, nothing
## about the output looks wrong at all.
##
## The check compares what the built analysis USES against what its own
## annotation NAMED, and that is a property of the reporting event alone -- no
## data, no execution.

.SEMANTIC_SOURCE_REF <- "SEMANTIC_SOURCE_NOT_DECLARED"
.UNRESOLVED_ROLE_REF <- "UNRESOLVED_VARIABLE_ROLE"

## Every code `validate_ars_model()` may emit, in one place.
##
## A finding's `ref` is what the rest of the package joins on: which scope a
## defect invalidates, which cells the engine must reserve, which fix to offer
## the author. Matching any of that on the finding's `problem` text would tie
## behaviour to display wording -- the same mistake `.check_semantic_source()`
## exists to police -- so `ref` is a closed vocabulary and `.add_finding()`
## refuses a code that is not listed here.
##
## The practical effect: a new check cannot ship without registering its code,
## and nothing downstream can silently ignore a code it has no handling for.
.VALIDATION_REFS <- c(
  ## Identity and references -- the event does not resolve against itself.
  ENTITY_ID_MISSING                        = "event",
  ENTITY_ID_DUPLICATED                     = "event",
  METHOD_REF_UNRESOLVED                    = "analysis",
  ANALYSIS_SET_REF_UNRESOLVED              = "analysis",
  DATA_SUBSET_REF_UNRESOLVED               = "analysis",
  GROUPING_REF_UNRESOLVED                  = "analysis",
  ANALYSIS_REF_UNRESOLVED                  = "advisory",
  ANALYSIS_NOT_DISPLAYED                   = "advisory",
  OUTPUT_HAS_NO_ANALYSES                   = "advisory",
  CONTENTS_ANALYSIS_STALE                  = "advisory",
  CONTENTS_OUTPUT_STALE                    = "advisory",

  ## Semantics -- the analysis computes something other than what it declared.
  SEMANTIC_SOURCE_NOT_DECLARED             = "analysis",
  UNRESOLVED_VARIABLE_ROLE                 = "analysis",

  ## Groupings and the column axis. A grouping is reserved through the
  ## analyses that reference it, which is what "grouping" scope expands to.
  GROUPING_DATASET_CONFLICT                = "grouping",
  FIXED_GROUPING_EMPTY                     = "grouping",
  GROUPING_VARIABLE_NOT_LINKED             = "analysis",
  GROUPING_ORDER_AMBIGUOUS                 = "advisory",
  FLAT_AXIS_COLUMN_COUNT_MISMATCH          = "advisory",
  FLAT_AXIS_COLUMN_LABEL_MISMATCH          = "advisory",

  ## Methods -- what the row shows against what the method computes.
  METHOD_NOT_ASSIGNED                      = "analysis",
  METHOD_PLACEHOLDER_SLOT_MISMATCH         = "cell",
  METHOD_STRATA_MISSING                    = "advisory",
  METHOD_NOT_EXECUTABLE                    = "advisory",
  ## Advisory, not reserving. A method with no executor is approximated by the
  ## generic summarizer, which IS a way to show a statistic the shell did not
  ## ask for -- but the method exists and the event declared it, the old gate
  ## never blocked on it (it is a WARN), and the behaviour is long-standing and
  ## tested. Reserving here would be new behaviour rather than a replacement
  ## for what the gate did, so it stays reported. The genuinely unresolvable
  ## cases -- a methodId pointing at nothing, or no method at all -- are
  ## METHOD_REF_UNRESOLVED and METHOD_NOT_ASSIGNED above, and those reserve.
  METHOD_FALLBACK_SUMMARIZER               = "advisory",
  METHOD_CONDITIONAL                       = "advisory",

  ## Nested displays and declared result paths.
  NESTED_CHILD_UNLINKED                    = "advisory",
  NESTED_GROUPING_MISSING                  = "advisory",
  HEADER_TREE_MISSING                      = "advisory",
  DISPLAY_COLUMN_COUNT_MISMATCH            = "output",
  UNMAPPED_LEAF_COLUMN                     = "output",
  INVALID_CARTESIAN_PRODUCT                = "output",
  SUBTOTAL_SCOPE_UNDEFINED                 = "output",
  SUBTOTAL_EXCLUDES_UNDISPLAYED_CATEGORIES = "advisory",
  DUPLICATE_RESULT_PATH                    = "output",

  ## The shell and the ADaM spec.
  SHELL_LINE_NOT_ANALYSED                  = "advisory",
  POPULATION_NOT_PARSED                    = "advisory",
  DATASET_NOT_IN_SPEC                      = "analysis",
  VARIABLE_NOT_IN_SPEC                     = "advisory",
  SEPARATOR_IN_CONDITION_VALUE             = "advisory"
)

## The scopes a finding may invalidate, widest last.
##
##   advisory  nothing is withheld; the reader is told, and that is all
##   cell      one displayed cell has no statistic behind it
##   analysis  this analysis must not compute
##   grouping  every analysis referencing this grouping must not compute
##   output    every analysis displayed on this output must not compute
##   event     the defect is in the event's own identity, not in one entity
.FINDING_SCOPES <- c("advisory", "cell", "analysis", "grouping", "output",
                     "event")

## The severities a FINDING may carry.
##
##   GAP   a specific result will not be produced, and the run has reserved it
##   WARN  a number exists, but it came from a fallback or an inference
##   INFO  worth knowing; nothing is affected
##
## GAP replaces FAIL here, and the new word is the point. DIAGNOSTICS keep
## FAIL | WARN | INFO -- the two channels say different things, and conflating
## them is exactly why one broken grouping used to refuse ten sound outputs: a
## finding is a statement about the REPORTING EVENT, a diagnostic is a statement
## about a RUN.
##
## Why a new word rather than redefining FAIL. `validation_report.xlsx` is an
## archived deliverable in a regulated workflow, and an auditor comparing two
## months' reports must not find one word meaning both "the run was refused"
## and "one cell was reserved". A new word cannot be misread; a redefined one
## can only be misread.
.FINDING_SEVERITIES <- c("GAP", "WARN", "INFO")

## Ranking for display, most serious first.
##
## FAIL is kept at the front although nothing emits it any more: reports written
## before this change are archived payloads that must still sort and render, and
## a severity the ranking does not know sorts to NA and drifts to the bottom --
## the most serious findings quietly last.
.FINDING_SEVERITY_RANK <- c("FAIL", "GAP", "WARN", "INFO")

## The severities that mean "a result is missing". Two entries for the same
## reason the ranking has four: an archived report still reads correctly.
.GAP_SEVERITIES <- c("GAP", "FAIL")

## Which scope a code invalidates.
##
## Declared here, beside the code, rather than passed at each of the forty-odd
## call sites. One table is auditable for completeness in a way forty arguments
## are not, and no code in this package needs to raise the same code at two
## different scopes. It is a declaration, not an inference from the string: a
## code with no scope is a code `.add_finding()` refuses outright.
#' @noRd
.scope_for_ref <- function(ref) {
  scope <- unname(.VALIDATION_REFS[ref])
  if (is.na(scope)) NA_character_ else scope
}

#' An analysis's annotation as plain text, with "absent" spelled one way.
#'
#' `annotation` is an arsbridge extension: an ARS written by another tool
#' carries no such field, and pooling leaves those analyses with `NA`. That is
#' the ordinary case for a foreign file, not an edge case.
#'
#' `NA` has to be normalised explicitly rather than left to `nzchar()`, which
#' reports `TRUE` for it -- an unguarded `NA` would read as an annotation that
#' happens to declare nothing, and would then be tallied against the unprovable
#' count as though the row had made a claim this check could not settle.
#' @noRd
.annotation_text <- function(annotation) {
  txt <- as.character(annotation %||% "")
  if (length(txt) == 0 || is.na(txt[[1]])) return("")
  trimws(txt[[1]])
}

#' Qualified `DATASET.VARIABLE` identifiers an annotation explicitly names.
#'
#' Deliberately **independent of the where-clause parser**. The question here
#' is only "which identifiers appear in this text", never what the condition
#' means -- so an annotation the condition grammar cannot yet read still
#' declares its sources correctly. Two known parser weaknesses both come from
#' operator-like text inside a quoted literal: a literal containing the clause
#' joiner splits the clause in the wrong place, and one containing a comparison
#' character defeats operator-shaped matching. Neither can affect this
#' function, because it never looks for operators at all.
#'
#' Reusing the condition parser here would make the detector blind in exactly
#' the cases it exists to police.
#' @noRd
.annotation_source_refs <- function(annotation) {
  annotation <- .annotation_text(annotation)
  if (!nzchar(annotation)) return(character(0))
  refs <- toupper(extract_annotation_vars(annotation))
  refs <- refs[grepl(".", refs, fixed = TRUE)]
  unique(refs[nzchar(refs)])
}

#' One `DATASET.VARIABLE` token, or nothing when there is no variable.
#'
#' A pair whose dataset the event never recorded is returned bare, so a missing
#' dataset degrades the comparison to the variable alone rather than inventing
#' a mismatch out of an absent field.
#' @noRd
.source_pair <- function(dataset, variable) {
  variable <- toupper(trimws(as.character(variable %||% "")))
  if (!nzchar(variable) || is.na(variable)) return(character(0))
  dataset <- toupper(trimws(as.character(dataset %||% "")))
  if (!nzchar(dataset) || is.na(dataset)) return(variable)
  paste0(dataset, ".", variable)
}

#' Every qualified pair a WhereClause references, following compound
#' expressions to their leaves. A clause that restricts on three variables
#' declares three sources, and all of them have to have been named.
#' @noRd
.where_source_pairs <- function(node) {
  if (is.null(node) || !is.list(node)) return(character(0))
  out <- character(0)
  condition <- node[["condition"]]
  if (!is.null(condition)) {
    out <- c(out, .source_pair(condition[["dataset"]], condition[["variable"]]))
  }
  if (!is.null(node[["variable"]])) {
    out <- c(out, .source_pair(node[["dataset"]], node[["variable"]]))
  }
  compound <- node[["compoundExpression"]]
  children <- compound[["whereClauses"]] %||% node[["whereClauses"]] %||%
    (if (is.list(compound)) compound else NULL)
  if (is.list(children)) {
    for (child in children) out <- c(out, .where_source_pairs(child))
  }
  unique(out[nzchar(out)])
}

#' Does a resolved pair appear among the annotation's declared references?
#'
#' A bare pair (no dataset on the resolved side) matches on the variable alone.
#' @noRd
.source_is_declared <- function(pair, declared) {
  if (grepl(".", pair, fixed = TRUE)) return(pair %in% declared)
  pair %in% sub("^[^.]*\\.", "", declared)
}

#' An analysis may only compute from data its own annotation named.
#'
#' **Containment, one way.** Everything the analysis resolves -- its analysis
#' variable and every variable of its data subset -- must be among the
#' references its annotation declares. The reverse is deliberately NOT checked:
#' an annotation may legitimately name more than the analysis uses (a unit
#' qualifier, a label hint). The dangerous direction is the other one, and only
#' that one is a finding.
#'
#' **Not equality.** An annotation of the form `<dataset>.<summarised> WHERE
#' <dataset>.<restricted>='...'` summarises one variable while restricting on
#' another, which is ordinary and correct; requiring the analysis variable to
#' equal the annotation's would condemn every analysis of that shape.
#'
#' Grouping variables are excluded throughout: they come from the column
#' header, not the row's annotation, so the row never names them.
#'
#' Scope: an analysis whose annotation yields no qualified reference is not
#' checkable -- nothing about its source is provable from the document -- and
#' is counted rather than judged.
#' @noRd
.check_semantic_source <- function(findings, model) {
  analyses <- model$analyses
  if (is.null(analyses) || nrow(analyses) == 0) return(findings)

  subsets <- model$data_subsets
  subset_raw <- if (!is.null(subsets)) attr(subsets, "raw") %||% subsets$raw else NULL
  pairs_for_subset <- function(subset_id) {
    if (is.na(subset_id) || !nzchar(subset_id) || is.null(subsets) ||
        nrow(subsets) == 0) {
      return(character(0))
    }
    hit <- match(subset_id, subsets$id)
    if (is.na(hit)) return(character(0))
    node <- if (!is.null(subset_raw)) subset_raw[[hit]] else NULL
    .where_source_pairs(node)
  }

  for (i in seq_len(nrow(analyses))) {
    annotation <- analyses$annotation[[i]]
    declared <- .annotation_source_refs(annotation)
    if (length(declared) == 0) next          # not checkable -- see .semantic_source_scope()

    used <- unique(c(
      .source_pair(analyses$dataset[[i]], analyses$variable[[i]]),
      pairs_for_subset(analyses$dataSubsetId[[i]])
    ))
    used <- used[nzchar(used)]
    undeclared <- used[!vapply(used, .source_is_declared, logical(1),
                               declared = declared)]
    if (length(undeclared) == 0) next

    findings <- .add_finding(
      findings, "GAP", "analyses", analyses$id[[i]], "variable",
      sprintf(
        paste0("Analysis %s resolves %s, but its annotation declares only %s. ",
               "The resolved analysis uses a semantic source not named by its ",
               "own annotation (%s)."),
        analyses$id[[i]], paste(undeclared, collapse = ", "),
        paste(declared, collapse = ", "),
        trimws(as.character(annotation))
      ),
      paste0("Rebuild this row from its own annotation. A number computed from ",
             "an undeclared source can look correct -- it may even match, when ",
             "the two variables happen to agree in this data cut."),
      ref = .SEMANTIC_SOURCE_REF
    )
  }
  findings
}

#' An analysis whose variable role could not be attributed may not execute.
#'
#' **Why this is a FAIL and not a WARN.** Of everything an enrichment carries,
#' dataset, variable and subset are all restated by the row's own annotation, so
#' when a pairing cannot be decided those are rebuilt from the annotation and
#' nothing is lost. `variableRole` is the exception: no annotation restates it,
#' and the ARS default is `ANALYSIS`. Dropping an unattributable non-default
#' role therefore does not leave a gap -- it silently asserts a role the row
#' never claimed, and the event runs and produces numbers under it.
#'
#' The distinction the package keeps: nothing specified, a legitimate default
#' may apply; specified and understood, use it; specified but unresolvable,
#' refuse. This is the third case, so it blocks. It is deliberately *not*
#' `manual_pending`, which means "the semantics are known and a human must
#' supply the value" -- here the semantics are precisely what is unknown.
#'
#' Structural, like every check here: it reads the event alone, no data.
#' @noRd
.check_unresolved_variable_role <- function(findings, model) {
  analyses <- model$analyses
  if (is.null(analyses) || nrow(analyses) == 0) return(findings)
  raw <- attr(analyses, "raw") %||% analyses$raw
  if (is.null(raw)) return(findings)

  for (i in seq_len(nrow(analyses))) {
    node <- raw[[i]]
    if (!is.list(node)) next
    roles <- as.character(unlist(node[["unresolvedVariableRole"]] %||% list()))
    roles <- roles[!is.na(roles) & nzchar(roles)]
    if (length(roles) == 0) next

    findings <- .add_finding(
      findings, "GAP", "analyses", analyses$id[[i]], "variableRole",
      sprintf(
        paste0("Analysis %s carries an unresolved variable role (%s). A role ",
               "was proposed for this row but could not be attributed to it, ",
               "and the annotation cannot restate it, so the analysis would ",
               "otherwise execute as the default ANALYSIS role it never ",
               "claimed."),
        analyses$id[[i]], paste(roles, collapse = ", ")),
      paste0("Settle which variable role this row has -- give the row an ",
             "unambiguous label or state the role explicitly -- and rebuild. ",
             "arsbridge will not choose one on your behalf."),
      ref = .UNRESOLVED_ROLE_REF
    )
  }
  findings
}

#' How much of the event the semantic-source check could speak to.
#'
#' **Not currently part of `validate_ars_model()`'s return value.** This exists
#' so the suite can prove a run was non-vacuous -- that the rule actually
#' reached the analyses a test claims it judged -- and so coverage can be
#' measured per study. Surfacing it to callers would change the validator's
#' result shape and is deliberately left to its own change.
#'
#' Coverage is a measurement per study, never an assumption: "no violations"
#' must never be read as "every analysis was examined". Three states, and they
#' must stay distinct -- collapsing any two would let an unexamined event read
#' as a clean one:
#'
#' * `checkable` -- the annotation declares qualified references, so
#'   containment can be evaluated. Only these can produce a finding.
#' * `not_checkable` -- an annotation is present, but no qualified reference
#'   can be extracted from it. The row made a source claim this check could not
#'   settle; the invariant is unavailable, not satisfied.
#' * `no_claim` -- no annotation at all. `annotation` is an arsbridge
#'   extension, so an ARS produced by another tool legitimately carries none.
#'   Such an event has not *passed* this check; it has not been assessed by it,
#'   and every other ARS check still applies.
#'
#' The guarantee is therefore conditional, and worth stating in those terms:
#' where an analysis carries annotation from which qualified references can be
#' established, arsbridge verifies that the resolved analysis introduces no
#' undeclared semantic source.
#' @noRd
.semantic_source_scope <- function(model) {
  analyses <- model$analyses
  if (is.null(analyses) || nrow(analyses) == 0) {
    return(list(checkable = 0L, not_checkable = 0L, no_claim = 0L))
  }
  annotated <- vapply(
    seq_len(nrow(analyses)),
    function(i) nzchar(.annotation_text(analyses$annotation[[i]])),
    logical(1)
  )
  has_refs <- vapply(
    seq_len(nrow(analyses)),
    function(i) length(.annotation_source_refs(analyses$annotation[[i]])) > 0,
    logical(1)
  )
  list(checkable     = sum(annotated & has_refs),
       not_checkable = sum(annotated & !has_refs),
       no_claim      = sum(!annotated))
}

#' @noRd
.new_findings <- function() {
  data.frame(
    severity = character(0),
    entity   = character(0),
    id       = character(0),
    field    = character(0),
    problem  = character(0),
    action   = character(0),
    ref      = character(0),
    detail   = character(0),
    scope    = character(0),
    ## Where in the author's own documents to go and change something. A
    ## finding addressed only as "analyses / AN_..." names an entity the
    ## author never typed; these name the cell they did.
    source_doc = character(0),
    sheet      = character(0),
    cell_ref   = character(0),
    row        = integer(0),
    col        = integer(0),
    locator    = character(0),
    stringsAsFactors = FALSE
  )
}

#' Where a finding came from, in the author's documents.
#'
#' `source_doc` says WHICH document to open -- the shell, the ADaM spec, the
#' supplement, or the ARS itself. That is what makes "fix this in the
#' annotation" distinguishable from "fix this in the spec" per finding rather
#' than per code.
#'
#' Never synthesise an address. An absent one is NA. A wrong address sends the
#' author to edit the wrong cell, which is the same class of harm as a wrong
#' number -- and a check that genuinely cannot address a cell (a shared pool
#' id, a Word shell with no cell grid) says so honestly through `locator`.
#' @noRd
.finding_at <- function(source_doc = NA_character_, sheet = NA_character_,
                        cell_ref = NA_character_, row = NA_integer_,
                        col = NA_integer_, locator = NA_character_) {
  list(source_doc = as.character(source_doc), sheet = as.character(sheet),
       cell_ref = as.character(cell_ref),
       row = suppressWarnings(as.integer(row)),
       col = suppressWarnings(as.integer(col)),
       locator = as.character(locator))
}

## `ref` names WHAT KIND of defect this is, from the closed vocabulary in
## `.VALIDATION_REFS`. `detail` carries the finding's own machine-readable
## payload where it has one -- a DATASET.VARIABLE for a shell line no analysis
## covers, say -- so the editor can act on a finding rather than only describe
## it.
##
## The two were one field until the reservation work needed to join on the
## code: a column that is sometimes a code and sometimes a variable name cannot
## be a reliable key, and the gap findings that carried a payload were silently
## outside every code-matching branch in the package.
#' @noRd
.add_finding <- function(findings, severity, entity, id, field, problem,
                         action, ref, detail = NA_character_, at = NULL) {
  ## A closed vocabulary for the same reason `ref` is one: everything
  ## downstream branches on severity, and a check that raised an unknown word
  ## would be counted by nothing and reported by nothing.
  if (!severity %in% .FINDING_SEVERITIES) {
    ## Bound to a local first: cli reads `{.name}` as a style, not a value.
    allowed <- .FINDING_SEVERITIES
    cli::cli_abort(c(
      "Finding severity {.val {severity}} is not one a check may raise.",
      "i" = "Use {.val {allowed}}.",
      "i" = paste("{.val FAIL} belongs to the diagnostics channel; a finding",
                  "that withholds a result is {.val GAP}.")
    ))
  }
  if (!ref %in% names(.VALIDATION_REFS)) {
    cli::cli_abort(c(
      "Finding code {.val {ref}} is not registered.",
      "i" = paste("Add it to {.code .VALIDATION_REFS} so the rest of the",
                  "package can act on it.")
    ))
  }

  ## `at` is the optional address argument declared in this function's own
  ## signature (at = NULL); the empty frame in .new_findings() already declares
  ## every column assembled below, so the rbind schemas match.
  at <- at %||% .finding_at()

  rbind(findings, data.frame(
    severity = severity,
    entity   = entity,
    id       = id,
    field    = field,
    problem  = problem,
    action   = action,
    ref      = ref,
    detail   = detail,
    scope    = .scope_for_ref(ref),
    source_doc = at$source_doc,
    sheet      = at$sheet,
    cell_ref   = at$cell_ref,
    row        = at$row,
    col        = at$col,
    locator    = at$locator,
    stringsAsFactors = FALSE
  ))
}

## How a method will behave when ars_to_ard() reaches it. This is the check
## that tells a reviewer "this line will not produce numbers" before they run
## the engine rather than after.
#' @noRd
.method_execution_class <- function(method_id, strata = NA_character_) {
  if (is.na(method_id) || !nzchar(method_id)) return("missing")

  if (method_id %in% .NATIVE_METHOD_IDS()) return("native")

  if (identical(method_id, "MTH_CMH_TEST")) {
    return(if (!is.na(strata) && nzchar(strata)) "conditional" else "blocked")
  }
  if (identical(method_id, "MTH_PROPORTION_CI_EXACT")) return("conditional")
  if (method_id %in% names(.UNEXECUTABLE_METHODS)) return("unsupported")

  "fallback"
}

## Ids referenced by the tables of contents. The LOPA sublists are padded by
## repeating the last analysis id (a siera workaround), so duplicates here are
## expected and must not be reported as a problem.
#' @noRd
.contents_referenced_ids <- function(template) {
  analysis_ids <- character(0)
  output_ids   <- character(0)

  lopa <- template[["mainListOfContents"]]
  for (item in lopa[["contentsList"]][["listItems"]] %||% list()) {
    output_ids <- c(output_ids, .chr_field(item[["outputId"]]))
    for (sub in item[["sublist"]][["listItems"]] %||% list()) {
      analysis_ids <- c(analysis_ids, .chr_field(sub[["analysisId"]]))
    }
  }

  for (lopo in template[["otherListsOfContents"]] %||% list()) {
    for (item in lopo[["contentsList"]][["listItems"]] %||% list()) {
      output_ids <- c(output_ids, .chr_field(item[["outputId"]]))
    }
  }

  list(
    analyses = unique(stats::na.omit(analysis_ids)),
    outputs  = unique(stats::na.omit(output_ids))
  )
}

#' @noRd
.check_ids <- function(findings, model) {
  labels <- c(
    analyses      = "analysis",
    methods       = "method",
    analysis_sets = "analysis set",
    data_subsets  = "data subset",
    groupings     = "grouping",
    outputs       = "output"
  )

  for (pool in names(labels)) {
    ids <- model[[pool]]$id

    missing <- which(is.na(ids) | !nzchar(ids))
    for (i in missing) {
      findings <- .add_finding(
        findings, "GAP", pool, paste0("row ", i), "id",
        paste0("This ", labels[[pool]], " has no id."),
        "Give it a unique id -- every reference in the event resolves by id.",
        ref = "ENTITY_ID_MISSING"
      )
    }

    duplicated_ids <- unique(ids[duplicated(ids) & !is.na(ids)])
    for (dup in duplicated_ids) {
      findings <- .add_finding(
        findings, "GAP", pool, dup, "id",
        paste0("Id ", dup, " is used by more than one ", labels[[pool]], "."),
        "Make each id unique -- references to it are ambiguous.",
        ref = "ENTITY_ID_DUPLICATED"
      )
    }
  }

  findings
}

#' @noRd
.check_references <- function(findings, model) {
  analyses <- model$analyses

  ## Shared entities referenced by each analysis.
  reference_checks <- list(
    list(column = "methodId",      pool = "methods",
         what = "method",       ref = "METHOD_REF_UNRESOLVED"),
    list(column = "analysisSetId", pool = "analysis_sets",
         what = "analysis set", ref = "ANALYSIS_SET_REF_UNRESOLVED"),
    list(column = "dataSubsetId",  pool = "data_subsets",
         what = "data subset",  ref = "DATA_SUBSET_REF_UNRESOLVED")
  )

  for (check in reference_checks) {
    known <- model[[check$pool]]$id
    values <- analyses[[check$column]]

    for (i in seq_len(nrow(analyses))) {
      value <- values[i]
      ## An empty dataSubsetId means "no subset", which is a valid state.
      if (is.na(value) || !nzchar(value)) next
      if (value %in% known) next

      findings <- .add_finding(
        findings, "GAP", "analyses", analyses$id[i], check$column,
        paste0("References ", check$what, " ", value,
               ", which is not in the reporting event."),
        paste0("Point it at an existing ", check$what,
               ", or add ", value, " to the ", check$what, " pool."),
        ref = check$ref
      )
    }
  }

  ## Grouping references.
  known_groupings <- model$groupings$id
  for (i in seq_len(nrow(analyses))) {
    for (grouping_id in .split_values(analyses$grouping_ids[i])) {
      if (grouping_id %in% known_groupings) next
      findings <- .add_finding(
        findings, "GAP", "analyses", analyses$id[i], "grouping_ids",
        paste0("References grouping ", grouping_id,
               ", which is not in the reporting event."),
        "Point it at an existing grouping, or add that grouping.",
        ref = "GROUPING_REF_UNRESOLVED"
      )
    }
  }

  ## Output -> analysis references, and analyses no output shows.
  known_analyses <- analyses$id
  referenced <- character(0)
  for (i in seq_len(nrow(model$outputs))) {
    ids <- .split_values(model$outputs$referenced_analysis_ids[i])
    referenced <- c(referenced, ids)
    for (analysis_id in setdiff(ids, known_analyses)) {
      findings <- .add_finding(
        findings, "GAP", "outputs", model$outputs$id[i],
        "referenced_analysis_ids",
        paste0("References analysis ", analysis_id,
               ", which is not in the reporting event."),
        "Remove the reference, or add the analysis it points at.",
        ref = "ANALYSIS_REF_UNRESOLVED"
      )
    }
  }

  for (analysis_id in setdiff(known_analyses, referenced)) {
    findings <- .add_finding(
      findings, "WARN", "analyses", analysis_id, "output_id",
      "No output references this analysis, so nothing will display it.",
      "Add it to an output's analysis list, or delete it.",
      ref = "ANALYSIS_NOT_DISPLAYED"
    )
  }

  ## An output with nothing behind it renders empty. The generator leaves
  ## these behind when it cannot derive any analysis for a display, so this is
  ## one of the most common things a reviewer has to fix.
  for (i in seq_len(nrow(model$outputs))) {
    if (length(.split_values(model$outputs$referenced_analysis_ids[i])) > 0) {
      next
    }
    findings <- .add_finding(
      findings, "WARN", "outputs", model$outputs$id[i],
      "referenced_analysis_ids",
      "This output has no analyses, so it would render empty.",
      "Add the analyses it should display, or drop the output.",
      ref = "OUTPUT_HAS_NO_ANALYSES"
    )
  }

  ## Tables of contents. These are regenerated on a structural save, so a
  ## stale reference is a note rather than a blocker.
  contents <- .contents_referenced_ids(model$template)
  for (analysis_id in setdiff(contents$analyses, known_analyses)) {
    findings <- .add_finding(
      findings, "WARN", "contents", analysis_id, "mainListOfContents",
      paste0("The table of contents lists analysis ", analysis_id,
             ", which is not in the reporting event."),
      "Saving after any structural change rebuilds the contents lists.",
      ref = "CONTENTS_ANALYSIS_STALE"
    )
  }
  for (output_id in setdiff(contents$outputs, model$outputs$id)) {
    findings <- .add_finding(
      findings, "WARN", "contents", output_id, "listOfContents",
      paste0("The table of contents lists output ", output_id,
             ", which is not in the reporting event."),
      "Saving after any structural change rebuilds the contents lists.",
      ref = "CONTENTS_OUTPUT_STALE"
    )
  }

  findings
}

#' @noRd
.check_grouping_shapes <- function(findings, model) {
  groupings <- model$groupings

  ## A grouping can name its dataset twice -- the flat `groupingDataset`
  ## arsbridge writes, and the nested `groupingVariable$dataset` a
  ## spec-correct ARS carries. Both are read; if they DISAGREE neither wins,
  ## because whichever were chosen decides whether the denominator is joined
  ## from a domain, and that moves every percentage in the analysis.
  ##
  ## Structural, so it is checked without an ADaM spec: the two fields
  ## contradict each other whatever the study data looks like.
  for (i in seq_len(nrow(groupings))) {
    resolved <- .grouping_dataset(groupings$raw[[i]])
    if (!isTRUE(resolved$conflict)) next
    findings <- .add_finding(
      findings, "GAP", "groupings", groupings$id[i], "groupingDataset",
      paste0("groupingDataset says ", resolved$flat,
             " but groupingVariable.dataset says ", resolved$nested, "."),
      "Make the two agree -- they decide which frame the denominator comes from.",
      ref = "GROUPING_DATASET_CONFLICT"
    )
  }

  is_fixed <- is.na(groupings$dataDriven) | !groupings$dataDriven
  invalid <- which(is_fixed & groupings$n_groups == 0L)
  for (i in invalid) {
    findings <- .add_finding(
      findings, "GAP", "groupings", groupings$id[i], "groups",
      "This fixed grouping declares no groups, so it defines no result columns.",
      "Add the fixed groups, or mark the grouping as data-driven.",
      ref = "FIXED_GROUPING_EMPTY"
    )
  }

  findings
}

#' Result-column labels carried by the display for both Word and Excel shells.
#' Arsbridge-authored shell layouts retain the physical stub as the first
#' display column. Displays without shell layout metadata use the compact ARS
#' shape and carry result columns only.
#' @noRd
.flat_display_labels <- function(output_node) {
  display <- .display_node(output_node)
  labels <- vapply(
    display[["columns"]] %||% list(),
    function(column) .chr_field(column[["label"]]),
    character(1)
  )
  if (!is.null(.shell_layout(output_node)) && length(labels) > 0L) {
    labels <- labels[-1]
  }

  labels <- .strip_n_placeholder(labels[!is.na(labels)])
  labels
}

#' @noRd
.check_flat_axes <- function(findings, model) {
  for (i in seq_len(nrow(model$outputs))) {
    if (!identical(model$outputs$outputType[i], "TABLE")) next

    output_node <- model$outputs$raw[[i]]

    tree_mode <- .chr_field(output_node[["_meta"]][["column_tree"]][["mode"]])
    if (!is.na(tree_mode) &&
        tree_mode %in% c("NESTED", "ASYMMETRIC_NESTED")) {
      next
    }

    analysis_ids <- .split_values(model$outputs$referenced_analysis_ids[i])
    analysis_rows <- match(analysis_ids, model$analyses$id)
    analysis_rows <- analysis_rows[!is.na(analysis_rows)]
    if (length(analysis_rows) == 0) next

    fixed_by_analysis <- vapply(analysis_rows, function(analysis_row) {
      grouping_ids <- .split_values(
        model$analyses$grouping_ids[analysis_row]
      )
      grouping_rows <- match(grouping_ids, model$groupings$id)
      grouping_rows <- grouping_rows[!is.na(grouping_rows)]
      fixed_rows <- grouping_rows[
        !is.na(model$groupings$dataDriven[grouping_rows]) &
          !model$groupings$dataDriven[grouping_rows] &
          model$groupings$n_groups[grouping_rows] > 0L
      ]
      if (length(fixed_rows) == 0L) NA_integer_ else fixed_rows[[1]]
    }, integer(1))
    fixed_rows <- unique(stats::na.omit(fixed_by_analysis))
    if (length(fixed_rows) == 0L) next

    signatures <- vapply(fixed_rows, function(grouping_row) {
      .grouping_signature(model$groupings$raw[[grouping_row]])
    }, character(1))
    if (length(unique(signatures)) > 1L) {
      group_counts <- model$groupings$n_groups[fixed_rows]
      ref <- if (length(unique(group_counts)) > 1L) {
        "FLAT_AXIS_COLUMN_COUNT_MISMATCH"
      } else {
        "FLAT_AXIS_COLUMN_LABEL_MISMATCH"
      }
      findings <- .add_finding(
        findings, "GAP", "outputs", model$outputs$id[i], "columns",
        "The analyses displayed on this flat output use different fixed grouping definitions.",
        "Make every displayed analysis reference the same flat column grouping definition.",
        ref = ref
      )
      next
    }

    ## The first fixed grouping in orderedGroupings is the flat column axis.
    ## Resolving through each analysis reference is essential: registration may
    ## have renamed this definition to a variant id during deduplication.
    grouping_row <- fixed_rows[[1]]
    groups <- model$groupings$raw[[grouping_row]][["groups"]] %||% list()
    group_order <- vapply(groups, function(group) {
      .int_field(group[["order"]])
    }, integer(1))
    groups <- groups[order(group_order, na.last = TRUE)]
    group_labels <- vapply(groups, function(group) {
      .chr_field(group[["label"]] %||% group[["name"]])
    }, character(1))
    group_labels <- .strip_n_placeholder(group_labels)

    result_labels <- .flat_display_labels(output_node)

    analysis_nodes <- model$analyses$raw[analysis_rows]
    includes_total <- vapply(analysis_nodes, function(analysis) {
      isTRUE(analysis[["includeTotal"]])
    }, logical(1))
    if (length(unique(includes_total)) > 1L) {
      findings <- .add_finding(
        findings, "GAP", "outputs", model$outputs$id[i], "columns",
        "The analyses displayed on this flat output disagree on whether a Total column is included.",
        "Make every displayed analysis use the same includeTotal setting.",
        ref = "FLAT_AXIS_COLUMN_COUNT_MISMATCH"
      )
      next
    }

    has_total <- includes_total[[1]]
    expected_count <- length(group_labels) + as.integer(has_total)

    if (length(result_labels) != expected_count) {
      findings <- .add_finding(
        findings, "GAP", "outputs", model$outputs$id[i], "columns",
        sprintf(
          "The flat shell displays %d result columns but grouping %s defines %d.",
          length(result_labels), model$groupings$id[grouping_row], expected_count
        ),
        "Make the display columns match the grouping levels and optional Total column.",
        ref = "FLAT_AXIS_COLUMN_COUNT_MISMATCH"
      )
      next
    }

    displayed_groups <- result_labels
    total_matches <- integer(0)
    if (has_total) {
      total_labels <- vapply(analysis_nodes, function(analysis) {
        label <- .chr_field(analysis[["totalLabel"]])
        if (is.na(label) || !nzchar(label)) "Total" else label
      }, character(1))
      total_labels <- .strip_n_placeholder(total_labels)
      if (length(unique(.fold_label(total_labels))) > 1L) {
        findings <- .add_finding(
          findings, "GAP", "outputs", model$outputs$id[i], "columns",
          "The analyses displayed on this flat output use different Total labels.",
          "Give every displayed analysis the same Total label.",
          ref = "FLAT_AXIS_COLUMN_LABEL_MISMATCH"
        )
        next
      }

      total_matches <- which(
        .fold_label(result_labels) == .fold_label(total_labels[[1]])
      )
      if (length(total_matches) == 1L) {
        displayed_groups <- result_labels[-total_matches]
      }
    }

    labels_match <- identical(displayed_groups, group_labels)
    if (has_total) labels_match <- labels_match && length(total_matches) == 1L
    if (!labels_match) {
      findings <- .add_finding(
        findings, "GAP", "outputs", model$outputs$id[i], "columns",
        sprintf(
          "The flat shell column labels do not match grouping %s.",
          model$groupings$id[grouping_row]
        ),
        "Use the grouping's labels in display order, including the declared Total label.",
        ref = "FLAT_AXIS_COLUMN_LABEL_MISMATCH"
      )
    }
  }

  findings
}

#' @noRd
.check_method_placeholder_slots <- function(findings, model) {
  method_ids <- model$methods$id[
    !is.na(model$methods$id) & nzchar(model$methods$id)
  ]
  stats_by_method <- stats::setNames(lapply(method_ids, function(method_id) {
    slots <- .method_operation_slots(model$methods$raw, method_id)
    available <- vapply(slots, function(slot) {
      slot$stat_name %||% NA_character_
    }, character(1))
    stats::na.omit(available)
  }), method_ids)
  method_by_analysis <- stats::setNames(
    model$analyses$methodId,
    model$analyses$id
  )

  ## A method that declares it computes nothing -- arsbridge's own reservation
  ## method, or one a foreign ARS marked the same way -- cannot under-provide
  ## slots. It provides none by construction, and the row is reserved on
  ## purpose. Read from the method's own `supported` flag rather than from its
  ## id, so an ARS that declares its own unsupported method is handled too.
  ##
  ## Without this the reservation defeats itself: the no-drop path parks a row
  ## on MTH_UNSUPPORTED_ANALYSIS (one operation), the shell shows an ordinary
  ## "xx (xx.x)" (two slots), and the row arsbridge deliberately reserved
  ## becomes a blocking finding against the whole event. `.check_methods()`
  ## already reports the analysis as not executable, so there is nothing to say
  ## a second time per cell.
  declares_no_result <- stats::setNames(
    vapply(method_ids, function(method_id) {
      isFALSE(model$methods$supported[match(method_id, model$methods$id)])
    }, logical(1)),
    method_ids
  )

  check_request <- function(findings, analysis_id, description,
                            requested = character(0), n_slots = length(requested),
                            at = NULL) {
    if (is.na(analysis_id) || !nzchar(analysis_id)) return(findings)
    method_id <- method_by_analysis[[analysis_id]] %||% NA_character_
    if (is.na(method_id) || !nzchar(method_id)) return(findings)
    if (isTRUE(unname(declares_no_result[method_id]))) return(findings)

    available <- stats_by_method[[method_id]] %||% character(0)
    missing <- setdiff(requested, available)
    too_many <- n_slots > length(available)
    if (length(missing) == 0L && !too_many) return(findings)

    problem <- if (length(missing) > 0L) {
      sprintf(
        "%s requests %s, which method %s does not provide.",
        description, paste(missing, collapse = ", "), method_id
      )
    } else {
      sprintf(
        "%s has %d slots, but method %s provides %d visible operation%s.",
        description, n_slots, method_id, length(available),
        if (length(available) == 1L) "" else "s"
      )
    }

    .add_finding(
      findings, "GAP", "analyses", analysis_id, "methodId",
      problem,
      "Assign a method whose operation slots cover every statistic on this line.",
      ref = "METHOD_PLACEHOLDER_SLOT_MISMATCH", at = at
    )
  }

  for (i in seq_len(nrow(model$outputs))) {
    output_node <- model$outputs$raw[[i]]
    layout <- .shell_layout(output_node)
    if (is.null(layout)) next

    ## Excel persists the concrete placeholder tokens and their statistic
    ## bindings. Check each distinct row request once, not once per result
    ## column.
    cells <- output_node[["_meta"]][["shell_fill"]][["cells"]] %||% list()
    seen_cells <- character(0)
    for (cell in cells) {
      if (!identical(.chr_field(cell[["kind"]]), "result")) next
      slots <- cell[["slots"]] %||% list()
      if (length(slots) == 0L) next

      analysis_id <- .chr_field(cell[["analysis_id"]])
      placeholder <- .chr_field(cell[["placeholder"]])
      if (is.na(placeholder) || !nzchar(placeholder)) placeholder <- "?"
      requested <- vapply(slots, function(slot) {
        .chr_field(slot[["stat_name"]])
      }, character(1))
      requested <- unique(requested[!is.na(requested) & nzchar(requested)])
      key <- paste(
        analysis_id, placeholder, paste(requested, collapse = ","),
        length(slots), sep = "|"
      )
      if (key %in% seen_cells) next
      seen_cells <- c(seen_cells, key)

      ## The cell map has been carrying these coordinates all along; they are
      ## the difference between "analyses / AN_..." and "sheet X, cell D27".
      findings <- check_request(
        findings,
        analysis_id,
        sprintf("Placeholder '%s'", placeholder),
        requested = requested,
        n_slots = length(slots),
        at = .finding_at(
          source_doc = "shell",
          sheet    = .chr_field(output_node[["_meta"]][["shell_fill"]][["source"]][["sheet"]]),
          cell_ref = .chr_field(cell[["ref"]]),
          row      = .int_field(cell[["row"]]),
          col      = .int_field(cell[["col"]])
        )
      )
    }

    ## Word has no cell addresses, but its parser records the number of
    ## placeholders on each authored row. Use that count when no Excel cell map
    ## is available.
    if (length(cells) == 0L) {
      for (j in seq_len(nrow(layout))) {
        n_slots <- layout$n_slots[j]
        if (is.na(n_slots) || n_slots == 0L) next
        ## A Word shell has no cell grid, so the row is the whole address.
        ## Saying so beats inventing a cell reference that does not exist.
        findings <- check_request(
          findings,
          layout$analysis_id[j],
          sprintf("Shell row '%s'", layout$label[j]),
          n_slots = n_slots,
          at = .finding_at(source_doc = "shell", locator = layout$label[j])
        )
      }
    }

    ## Statistic-line labels remain useful for older repair models that predate
    ## persisted placeholder metadata.
    rows <- .shell_table_data(output_node, model)$rows
    for (j in seq_len(nrow(rows))) {
      requested <- .stats_for_line(rows$label[j])
      analysis_id <- rows$owner_analysis_id[j]
      method_id <- rows$method_id[j]
      if (is.null(requested) || is.na(analysis_id) || is.na(method_id)) next
      ## Same reasoning as check_request(): a reserved row is not a mismatch.
      if (isTRUE(unname(declares_no_result[method_id]))) next

      available <- stats_by_method[[method_id]] %||% character(0)
      missing <- setdiff(requested, available)
      if (length(missing) == 0L) next

      findings <- .add_finding(
        findings, "GAP", "analyses", analysis_id, "methodId",
        sprintf(
          "Shell line '%s' requests %s, which method %s does not provide.",
          rows$label[j], paste(missing, collapse = ", "), method_id
        ),
        "Assign a method whose operation slots cover every statistic on this line.",
        ref = "METHOD_PLACEHOLDER_SLOT_MISMATCH"
      )
    }
  }

  findings
}

#' @noRd
.check_methods <- function(findings, model) {
  analyses <- model$analyses

  for (i in seq_len(nrow(analyses))) {
    method_id <- analyses$methodId[i]
    strata    <- analyses$strata[i]
    class     <- .method_execution_class(method_id, strata)

    if (identical(class, "missing")) {
      findings <- .add_finding(
        findings, "GAP", "analyses", analyses$id[i], "methodId",
        "No method is assigned, so this analysis cannot be executed.",
        "Assign a method -- the engine computes results from it.",
        ref = "METHOD_NOT_ASSIGNED"
      )
    } else if (identical(class, "blocked")) {
      findings <- .add_finding(
        findings, "WARN", "analyses", analyses$id[i], "strata",
        "A CMH test needs a stratification variable, and none is set.",
        "Set the stratification variable, or the engine reserves an empty cell.",
        ref = "METHOD_STRATA_MISSING"
      )
    } else if (identical(class, "unsupported")) {
      findings <- .add_finding(
        findings, "WARN", "analyses", analyses$id[i], "methodId",
        paste0("Method ", method_id,
               " has no executor, so the engine reserves an empty cell."),
        "Plan to compute this result manually, or choose an executable method.",
        ref = "METHOD_NOT_EXECUTABLE"
      )
    } else if (identical(class, "fallback")) {
      findings <- .add_finding(
        findings, "WARN", "analyses", analyses$id[i], "methodId",
        paste0("Method ", method_id,
               " has no executor; the generic summarizer runs instead."),
        "Check the result is what the shell asks for, or change the method.",
        ref = "METHOD_FALLBACK_SUMMARIZER"
      )
    } else if (identical(class, "conditional")) {
      findings <- .add_finding(
        findings, "INFO", "analyses", analyses$id[i], "methodId",
        paste0("Method ", method_id,
               " executes only when its prerequisites are met."),
        "No action needed if the required package and operands are present.",
        ref = "METHOD_CONDITIONAL"
      )
    }
  }

  findings
}

## Populations arsbridge could not parse into a where-clause keep the raw
## annotation text instead. They are honest, but the engine cannot filter on
## them, so the reviewer needs to know.
#' @noRd
.check_unparsed_populations <- function(findings, model) {
  sets <- model$analysis_sets
  for (i in seq_len(nrow(sets))) {
    if (is.na(sets$annotationText[i])) next
    findings <- .add_finding(
      findings, "WARN", "analysis_sets", sets$id[i], "annotationText",
      paste0("The population \"", sets$annotationText[i],
             "\" was not parsed into a condition, so it filters nothing."),
      "Express it as a condition, or confirm the analysis is unfiltered.",
      ref = "POPULATION_NOT_PARSED"
    )
  }
  findings
}

## Composite columns join several values with ";". A value that contains the
## separator would split wrongly on the next edit.
#' @noRd
.check_separator_safety <- function(findings, model) {
  contains_separator <- function(x) {
    !is.na(x) & grepl(.MODEL_SEP, x, fixed = TRUE)
  }

  for (pool in c("analysis_sets", "data_subsets")) {
    df <- model[[pool]]
    hits <- which(contains_separator(df$condition_value) & !df$is_compound)
    for (i in hits) {
      findings <- .add_finding(
        findings, "INFO", pool, df$id[i], "condition_value",
        paste0("A condition value contains a semicolon, which the editor ",
               "uses to separate values."),
        "Edit this condition through the raw-JSON escape hatch instead.",
        ref = "SEPARATOR_IN_CONDITION_VALUE"
      )
    }
  }

  findings
}

## --- spec overlay ----------------------------------------------------------
## Wired in phase 2, when the editor loads the ADaM spec alongside the JSON.

#' @noRd
.check_against_spec <- function(findings, model, spec) {
  known_datasets <- unique(spec$variables$dataset)

  check_reference <- function(findings, entity, id, field, dataset, variable) {
    if (is.na(dataset) || !nzchar(dataset)) return(findings)

    if (!dataset %in% known_datasets) {
      return(.add_finding(
        findings, "GAP", entity, id, field,
        paste0("Dataset ", dataset, " is not in the ADaM spec."),
        "Correct the dataset, or add it to the spec.",
        ref = "DATASET_NOT_IN_SPEC", detail = dataset
      ))
    }
    if (is.na(variable) || !nzchar(variable)) return(findings)

    key <- paste0(dataset, ".", variable)
    if (!is.null(spec$lookup[[key]])) return(findings)

    .add_finding(
      findings, "WARN", entity, id, field,
      paste0("Variable ", key, " is not in the ADaM spec."),
      "Correct the variable, or confirm it is derived downstream.",
      ref = "VARIABLE_NOT_IN_SPEC", detail = key
    )
  }

  for (i in seq_len(nrow(model$analyses))) {
    findings <- check_reference(
      findings, "analyses", model$analyses$id[i], "variable",
      model$analyses$dataset[i], model$analyses$variable[i]
    )
  }

  for (i in seq_len(nrow(model$groupings))) {
    findings <- check_reference(
      findings, "groupings", model$groupings$id[i], "groupingVariable",
      model$groupings$groupingDataset[i], model$groupings$groupingVariable[i]
    )

  }

  for (pool in c("analysis_sets", "data_subsets")) {
    df <- model[[pool]]
    for (i in seq_len(nrow(df))) {
      if (isTRUE(df$is_compound[i])) {
        ## The flat columns hold nothing for a compound, so the loop above
        ## skipped it entirely and never looked at its clauses.
        for (ref in .where_clause_refs(df$raw[[i]])) {
          findings <- check_reference(
            findings, pool, df$id[i], "condition_variable",
            ref$dataset, ref$variable
          )
        }
        next
      }
      findings <- check_reference(
        findings, pool, df$id[i], "condition_variable",
        df$condition_dataset[i], df$condition_variable[i]
      )
    }
  }

  ## A grouping's CHILD groups define the result columns, and each carries its
  ## own where clause -- the one place a wrong variable is both easiest to
  ## write and hardest to notice, because the column still renders, just
  ## empty. Nothing looked at them until now.
  for (i in seq_len(nrow(model$groupings))) {
    groups <- model$groupings$raw[[i]][["groups"]] %||% list()
    for (group in groups) {
      group_id <- .chr_field(group[["id"]])
      for (ref in .where_clause_refs(group)) {
        findings <- check_reference(
          findings, "groupings", model$groupings$id[i],
          paste0("group ", .blank_na(group_id), " condition"),
          ref$dataset, ref$variable
        )
      }
    }
  }

  findings
}

## Every (dataset, variable) a where clause names, however deeply nested.
##
## A condition contributes one pair; a compoundExpression contributes its
## clauses, recursively, so a wrong variable is found at any depth rather than
## only at the top. Duplicates are dropped so one misspelling in a clause used
## twice is one finding, not two.
#' @noRd
.where_clause_refs <- function(where) {
  if (is.null(where) || !is.list(where)) return(list())

  refs <- list()
  if (!is.null(where[["condition"]])) {
    condition <- where[["condition"]]
    refs <- list(list(dataset  = .chr_field(condition[["dataset"]]),
                      variable = .chr_field(condition[["variable"]])))
  } else if (!is.null(where[["compoundExpression"]])) {
    clauses <- where[["compoundExpression"]][["whereClauses"]] %||% list()
    refs <- unlist(lapply(clauses, .where_clause_refs), recursive = FALSE)
  }

  refs <- Filter(function(ref) {
    !is.na(ref$dataset) || !is.na(ref$variable)
  }, refs %||% list())

  keys <- vapply(refs, function(ref) {
    paste0(.blank_na(ref$dataset), ".", .blank_na(ref$variable))
  }, character(1))
  refs[!duplicated(keys)]
}

## --- gap detection ---------------------------------------------------------
## The annotation validation report already knows every line the programmer
## annotated in the shell. An annotation with no matching analysis is a line
## the generator missed -- the single most valuable thing this tool surfaces.

#' @noRd
.check_gaps <- function(findings, model, report) {
  required <- c("tlf_number", "stub_label", "annotation", "variable_ref")
  if (!all(required %in% names(report))) {
    cli::cli_warn(c(
      "Ignoring {.arg report}: it does not look like a validation report.",
      "i" = "Expected the columns {.val {required}}."
    ))
    return(findings)
  }

  analyses <- model$analyses

  ## Match on DATASET.VARIABLE rather than the annotation string: the
  ## dataset and variable are always on the analysis, whereas the annotation
  ## text is only shipped when spec_to_ars(ship_annotations = TRUE).
  analysis_refs <- paste0(analyses$dataset, ".", analyses$variable)

  for (i in seq_len(nrow(report))) {
    ## A <population> row describes the analysis set, not an analysis line.
    if (identical(report$stub_label[i], "<population>")) next

    tlf <- report$tlf_number[i]
    if (is.na(tlf) || !nzchar(tlf)) next

    output_id <- make_output_id(tlf)
    if (!output_id %in% model$outputs$id) next

    variable_ref <- report$variable_ref[i]
    if (is.na(variable_ref) || !nzchar(variable_ref)) next

    in_output <- !is.na(analyses$output_id) & analyses$output_id == output_id
    if (variable_ref %in% analysis_refs[in_output]) next

    stub <- report$stub_label[i]
    described <- if (is.na(stub) || !nzchar(stub)) variable_ref else stub

    findings <- .add_finding(
      findings, "WARN", "outputs", output_id, "analyses",
      paste0("The shell annotates \"", described, "\" (", variable_ref,
             ") but no analysis in this output uses that variable."),
      "Add the missing analysis, or confirm the line is not an analysis.",
      ref = "SHELL_LINE_NOT_ANALYSED", detail = variable_ref
    )
  }

  findings
}


#' Structural checks over each output's declared result-group paths (the
#' hierarchical column model). The codes ride in `ref` so the editor and
#' tests can key on them:
#'
#'   HEADER_TREE_MISSING                      WARN
#'   DISPLAY_COLUMN_COUNT_MISMATCH            FAIL
#'   UNMAPPED_LEAF_COLUMN                     FAIL
#'   INVALID_CARTESIAN_PRODUCT                FAIL
#'   GROUPING_VARIABLE_NOT_LINKED             FAIL
#'   SUBTOTAL_SCOPE_UNDEFINED                 FAIL
#'   DUPLICATE_RESULT_PATH                    FAIL
#'   GROUPING_ORDER_AMBIGUOUS                 WARN
#'   SUBTOTAL_EXCLUDES_UNDISPLAYED_CATEGORIES INFO
#'
## Nested block integrity (HANDOFF_nested_soc_pt_hierarchy, Phase N4). A
## nested_child layout row must resolve to a nested_parent row through
## parent_order, and its analysis must carry the parent's variable as a row
## grouping -- without the link the renderer degrades to a flat block,
## without the grouping the child's results cannot nest at all.
##
##   NESTED_CHILD_UNLINKED    WARN
##   NESTED_GROUPING_MISSING  WARN
#' @noRd
.check_nested_layout <- function(findings, model) {
  for (i in seq_len(nrow(model$outputs))) {
    layout <- .shell_layout(model$outputs$raw[[i]])
    if (is.null(layout)) next
    output_id <- model$outputs$id[i]

    for (j in which(layout$kind == "nested_child")) {
      parent_hit <- if (is.na(layout$parent_order[j])) {
        integer(0)
      } else {
        which(layout$order == layout$parent_order[j] &
                layout$kind == "nested_parent")
      }
      if (length(parent_hit) != 1) {
        findings <- .add_finding(
          findings, "WARN", "outputs", output_id, "shell_layout",
          sprintf(
            "Nested child row '%s' (order %d) has no linked parent row",
            layout$label[j], layout$order[j]),
          paste0("Relink parent_order to the parent-level row (or re-author ",
                 "the block) -- the renderer falls back to a flat block ",
                 "until then"),
          ref = "NESTED_CHILD_UNLINKED"
        )
        next
      }

      child_id  <- layout$analysis_id[j]
      parent_id <- layout$analysis_id[parent_hit]
      child_at  <- match(child_id, model$analyses$id)
      parent_at <- match(parent_id, model$analyses$id)
      ## Dangling analysis references are another check's finding.
      if (is.na(child_at) || is.na(parent_at)) next

      parent_var <- model$analyses$variable[parent_at]
      if (is.na(parent_var) || !nzchar(parent_var)) next
      parent_var <- toupper(parent_var)

      grouping_ids <- .split_values(model$analyses$grouping_ids[child_at])
      grouping_vars <- toupper(model$groupings$groupingVariable[
        match(grouping_ids, model$groupings$id)
      ])
      if (!parent_var %in% stats::na.omit(grouping_vars)) {
        findings <- .add_finding(
          findings, "WARN", "analyses", child_id, "grouping_ids",
          sprintf(
            "Nested child analysis does not group by its parent's variable (%s)",
            parent_var),
          paste0("Add ", parent_var, " as a data-driven grouping on this ",
                 "analysis -- without it the child's results cannot nest ",
                 "under the parent levels"),
          ref = "NESTED_GROUPING_MISSING"
        )
      }
    }
  }
  findings
}

#' @noRd
.check_result_paths <- function(findings, model) {
  outputs <- model$outputs
  if (nrow(outputs) == 0) return(findings)

  ## Group id -> its grouping factor and condition, across the pool.
  group_index <- list()
  for (i in seq_len(nrow(model$groupings))) {
    gf_node <- model$groupings$raw[[i]]
    for (g in gf_node[["groups"]] %||% list()) {
      gid <- .chr_field(g[["id"]])
      if (is.na(gid) || !nzchar(gid)) next
      group_index[[gid]] <- list(
        gf_id     = .chr_field(gf_node[["id"]]),
        condition = .group_where(g)
      )
    }
  }

  path_condition <- function(path) {
    conds <- lapply(unlist(path[["groupIds"]] %||% list()), function(gid) {
      entry <- group_index[[gid]]
      if (is.null(entry)) NULL else entry$condition
    })
    do.call(combine_conditions, conds)
  }
  path_label <- function(path) {
    paste(unlist(path[["labelPath"]] %||% list()), collapse = " > ")
  }

  for (i in seq_len(nrow(outputs))) {
    node   <- outputs$raw[[i]]
    out_id <- outputs$id[i]
    rgp    <- node[["resultGroupPaths"]]
    tree   <- node[["_meta"]][["column_tree"]]
    tree_mode <- .chr_field(tree[["mode"]])

    if (is.null(rgp)) {
      if (!is.na(tree_mode) &&
          tree_mode %in% c("NESTED", "ASYMMETRIC_NESTED")) {
        findings <- .add_finding(
          findings, "WARN", "outputs", out_id, "resultGroupPaths",
          "The shell header parsed as a hierarchical column tree, but this output declares no result-group paths.",
          "Regenerate the event -- without declared paths the executor falls back to flat groupings and the child columns are lost.",
          ref = "HEADER_TREE_MISSING"
        )
      }
      next
    }

    paths <- rgp[["paths"]] %||% list()
    tree_nodes <- tree[["nodes"]] %||% list()
    tree_ids   <- vapply(tree_nodes, function(n) .chr_field(n[["id"]]) %||% "",
                         character(1))
    leaf_nodes <- Filter(function(n) {
      .chr_field(n[["nodeType"]]) %in% c("leaf", "subtotal", "grand_total")
    }, tree_nodes)
    path_node_ids <- vapply(paths, function(p) .chr_field(p[["nodeId"]]) %||% "",
                            character(1))

    ## The shell's own column count is the ground truth: a path list that
    ## disagrees with it means display columns were added or lost.
    if (length(leaf_nodes) > 0 && length(paths) != length(leaf_nodes)) {
      findings <- .add_finding(
        findings, "GAP", "outputs", out_id, "resultGroupPaths",
        sprintf("The shell header has %d result columns but %d path(s) are declared.",
                length(leaf_nodes), length(paths)),
        "Each display column needs exactly one declared path -- regenerate or fix the path list.",
        ref = "DISPLAY_COLUMN_COUNT_MISMATCH"
      )
    }
    for (leaf in leaf_nodes) {
      leaf_id <- .chr_field(leaf[["id"]])
      if (!is.na(leaf_id) && nzchar(leaf_id) && !leaf_id %in% path_node_ids) {
        findings <- .add_finding(
          findings, "GAP", "outputs", out_id, "resultGroupPaths",
          sprintf("Shell column '%s' has no declared result path.",
                  .chr_field(leaf[["label"]]) %||% leaf_id),
          "Add the missing path (or regenerate) -- this display column would otherwise be silently dropped.",
          ref = "UNMAPPED_LEAF_COLUMN"
        )
      }
    }

    used_gf_order <- character(0)
    seen_conditions <- list()
    for (p in paths) {
      label <- path_label(p)
      role  <- .chr_field(p[["role"]]) %||% "DETAIL"
      gids  <- unlist(p[["groupIds"]] %||% list())

      ## A path pointing at no shell column is an invented combination --
      ## exactly the Cartesian product this model exists to prevent.
      nid <- .chr_field(p[["nodeId"]])
      if (length(tree_ids) > 0 && !is.na(nid) && nzchar(nid) &&
          !nid %in% tree_ids) {
        findings <- .add_finding(
          findings, "GAP", "outputs", out_id, "resultGroupPaths",
          sprintf("Path '%s' matches no column of the shell header tree.", label),
          "Remove it -- only columns the shell displays may be declared.",
          ref = "INVALID_CARTESIAN_PRODUCT"
        )
      }
      gf_of <- vapply(gids, function(gid) {
        entry <- group_index[[gid]]
        if (is.null(entry)) NA_character_ else entry$gf_id
      }, character(1))
      if (anyDuplicated(stats::na.omit(gf_of))) {
        findings <- .add_finding(
          findings, "GAP", "outputs", out_id, "resultGroupPaths",
          sprintf("Path '%s' references two groups of the same grouping factor.", label),
          "A column cannot be two levels of one variable at once -- fix the path's groupIds.",
          ref = "INVALID_CARTESIAN_PRODUCT"
        )
      }
      for (gid in gids) {
        if (is.null(group_index[[gid]])) {
          findings <- .add_finding(
            findings, "GAP", "outputs", out_id, "resultGroupPaths",
            sprintf("Path '%s' references unknown group id '%s'.", label, gid),
            "Point the path at an existing group level, or add the missing group to its grouping factor.",
            ref = "GROUPING_VARIABLE_NOT_LINKED"
          )
        }
      }

      if (identical(role, "SUBTOTAL") &&
          (is.na(.chr_field(p[["totalStrategy"]])) || length(gids) == 0)) {
        findings <- .add_finding(
          findings, "GAP", "outputs", out_id, "resultGroupPaths",
          sprintf("Subtotal path '%s' has no defined scope.", label),
          "Give it totalStrategy 'condition_based' and its parent group id -- an unscoped subtotal is ambiguous (parent condition vs sum of children).",
          ref = "SUBTOTAL_SCOPE_UNDEFINED"
        )
      }

      cond_key <- paste(deparse(canonicalize_condition(path_condition(p))),
                        collapse = "")
      prior <- seen_conditions[[cond_key]]
      if (!is.null(prior)) {
        findings <- .add_finding(
          findings, "GAP", "outputs", out_id, "resultGroupPaths",
          sprintf("Paths '%s' and '%s' compose the same condition -- two columns would compute identical results.",
                  prior, label),
          "Make each path a distinct subject set (check the groupIds).",
          ref = "DUPLICATE_RESULT_PATH"
        )
      } else {
        seen_conditions[[cond_key]] <- label
      }

      used_gf_order <- union(used_gf_order, stats::na.omit(gf_of))
    }

    ## The subtotal-vs-children scope question is the one a reviewer must
    ## answer deliberately: a subtotal equal to the OR of its displayed
    ## children may be excluding unknown-category subjects.
    detail_paths <- Filter(function(p) identical(.chr_field(p[["role"]]), "DETAIL"), paths)
    for (p in paths) {
      if (!identical(.chr_field(p[["role"]]), "SUBTOTAL")) next
      lp <- unlist(p[["labelPath"]] %||% list())
      if (length(lp) < 2) next
      prefix <- lp[-length(lp)]
      siblings <- Filter(function(d) {
        dlp <- unlist(d[["labelPath"]] %||% list())
        length(dlp) == length(lp) && identical(dlp[-length(dlp)], prefix)
      }, detail_paths)
      if (length(siblings) == 0) next
      union_cond <- do.call(
        combine_conditions,
        c(lapply(siblings, path_condition), list(operator = "OR"))
      )
      if (conditions_equal(path_condition(p), union_cond)) {
        findings <- .add_finding(
          findings, "INFO", "outputs", out_id, "resultGroupPaths",
          sprintf("Subtotal '%s' equals the union of its displayed children -- it may exclude subjects whose category is unknown.",
                  path_label(p)),
          "Confirm the intended scope: a parent-condition subtotal includes undisplayed categories; a child-union subtotal does not.",
          ref = "SUBTOTAL_EXCLUDES_UNDISPLAYED_CATEGORIES"
        )
      }
    }

    ## Every analysis this output displays must link all grouping factors the
    ## paths reference, in the same level order.
    analyses <- model$analyses[
      !is.na(model$analyses$output_id) & model$analyses$output_id == out_id, ,
      drop = FALSE
    ]
    for (j in seq_len(nrow(analyses))) {
      an_gids <- .split_values(analyses$grouping_ids[j] %||% "")
      missing <- setdiff(used_gf_order, an_gids)
      if (length(missing) > 0) {
        findings <- .add_finding(
          findings, "GAP", "analyses", analyses$id[j], "grouping_ids",
          sprintf("This analysis does not reference grouping factor(s) %s that the output's result paths require.",
                  paste(missing, collapse = ", ")),
          "Add the grouping(s) in 'Grouped by' -- without them this line cannot fill the hierarchical columns.",
          ref = "GROUPING_VARIABLE_NOT_LINKED"
        )
        next
      }
      shared <- an_gids[an_gids %in% used_gf_order]
      if (length(shared) > 1 &&
          !identical(shared, used_gf_order[used_gf_order %in% shared])) {
        findings <- .add_finding(
          findings, "WARN", "analyses", analyses$id[j], "grouping_ids",
          "This analysis orders its groupings differently from the output's header levels.",
          "Match the 'Grouped by' order to the header (outermost level first) so the columns come out in shell order.",
          ref = "GROUPING_ORDER_AMBIGUOUS"
        )
      }
    }
  }

  findings
}

#' Check an ARS model for integrity, spec and coverage problems
#'
#' Runs the checks that make the review stage guided rather than generic: that
#' every reference resolves, that every method can actually be executed, that
#' variables exist in the ADaM spec, and that no annotated shell line was
#' missed by the generator.
#'
#' @param model An `ars_model` from [ars_to_model()].
#' @param spec Optional ADaM spec, as returned by the package's spec reader
#'   (a list with `variables` and `lookup`). When supplied, datasets and
#'   variables are checked against it.
#' @param report Optional annotation validation report -- the data frame
#'   `spec_to_ars()` returns as `$validation`, or the "Validation" sheet of the
#'   report it writes. When supplied, annotated shell lines with no
#'   corresponding analysis are reported as gaps.
#'
#' @return A data frame of findings, most severe first, with columns
#'   `severity` (`"GAP"`, `"WARN"` or `"INFO"`), `entity` (the pool the
#'   finding is about), `id`, `field`, `problem` and `action`. Zero rows means
#'   nothing to fix.
#'
#'   `GAP` means a specific result will not be produced and the run has
#'   reserved it; `WARN` means a number exists but came from a fallback or an
#'   inference, so check it. Reports written before this release carry `FAIL`
#'   where they would now carry `GAP`, and still sort and render.
#'
#' @section What is checked:
#' \describe{
#'   \item{Identity}{Every entity has an id, and no id is used twice.}
#'   \item{References}{Every `methodId`, `analysisSetId`, `dataSubsetId` and
#'     grouping id resolves, and every output references analyses that exist.
#'     An empty `dataSubsetId` means "no subset" and is not a dangling
#'     reference. Analyses no output displays are reported.}
#'   \item{Executability}{Whether [ars_to_ard()] can compute each analysis
#'     natively, needs a prerequisite, falls back to the generic summarizer,
#'     or will reserve an empty cell for manual computation.}
#'   \item{Populations}{Analysis sets whose population text could not be
#'     parsed into a condition, and so filter nothing.}
#'   \item{Spec}{With `spec`: datasets and variables that are not in the ADaM
#'     spec.}
#'   \item{Result paths}{For an output with declared hierarchical result-group
#'     paths: the path count matches the shell's column count, every shell
#'     column has a path and every path a shell column (no invented
#'     Cartesian combinations), subtotals have a defined scope, no two paths
#'     compose the same condition, and every displayed analysis links all
#'     required grouping factors in header order.}
#'   \item{Coverage}{With `report`: shell annotations that no analysis
#'     carries -- lines the generator missed.}
#' }
#'
#' @seealso [ars_to_model()], [model_to_ars()].
#'
#' @examples
#' \dontrun{
#' model <- ars_to_model("reporting_event.json")
#' findings <- validate_ars_model(model)
#' subset(findings, severity == "GAP")
#' }
#' @export
validate_ars_model <- function(model, spec = NULL, report = NULL) {
  .assert_ars_model(model)

  findings <- .new_findings()
  findings <- .check_ids(findings, model)
  findings <- .check_references(findings, model)
  findings <- .check_grouping_shapes(findings, model)
  findings <- .check_flat_axes(findings, model)
  findings <- .check_methods(findings, model)
  findings <- .check_method_placeholder_slots(findings, model)
  findings <- .check_unparsed_populations(findings, model)
  findings <- .check_separator_safety(findings, model)
  findings <- .check_result_paths(findings, model)
  findings <- .check_nested_layout(findings, model)
  findings <- .check_semantic_source(findings, model)
  findings <- .check_unresolved_variable_role(findings, model)

  if (!is.null(spec)) {
    findings <- .check_against_spec(findings, model, spec)
  }
  if (!is.null(report) && nrow(report) > 0) {
    findings <- .check_gaps(findings, model, report)
  }

  ## Most severe first, so the panel and the console summary agree on order.
  severity_rank <- match(findings$severity, .FINDING_SEVERITY_RANK)
  findings <- findings[order(severity_rank), , drop = FALSE]
  rownames(findings) <- NULL
  findings
}

## One representation for every place that reads what validation concluded.
##
## `gaps` and `gap_refs` are the vocabulary from here on: the findings naming
## results that will not be produced. `blocking_findings` and `blocking_refs`
## are the same rows under their old names, kept because several callers and
## archived payloads read them, and renaming a field is not what this change is
## for.
#' @noRd
.validation_gate <- function(findings) {
  gaps <- findings[findings$severity %in% .GAP_SEVERITIES, , drop = FALSE]
  refs <- gaps$ref[
    !is.na(gaps$ref) & nzchar(gaps$ref)
  ]
  refs <- unique(refs)

  actions <- gaps$action[
    !is.na(gaps$action) & nzchar(gaps$action)
  ]
  actions <- unique(actions)

  blocked <- nrow(gaps) > 0L
  summary <- if (blocked) {
    action_text <- if (length(actions) > 0L) {
      paste(actions, collapse = " ")
    } else {
      "Review the gap findings and repair the reporting event."
    }
    paste0(
      nrow(gaps), " gap finding",
      if (nrow(gaps) == 1L) "" else "s",
      ". To fix: ", action_text
    )
  } else {
    "No gap findings. WARN and INFO findings remain available for review."
  }

  list(
    blocked = blocked,
    status = if (blocked) "needs-fixes" else "ready",
    findings = findings,
    gaps = gaps,
    gap_refs = refs,
    blocking_findings = gaps,
    blocking_refs = refs,
    summary = summary
  )
}

#' @noRd
.model_validation_gate <- function(model, spec = NULL, report = NULL) {
  .validation_gate(validate_ars_model(model, spec = spec, report = report))
}

## Refuse to turn a structurally invalid event into a runnable artifact. This is
## deliberately called at each direct execution boundary, not only by the
## higher-level workflow, because these helpers are also callable on their own.
#' @noRd
.assert_runnable_ars <- function(ars) {
  model <- if (inherits(ars, "ars_model")) ars else ars_to_model(ars)
  gate <- .model_validation_gate(model)
  if (!gate$blocked) return(invisible(gate))

  refs <- if (length(gate$blocking_refs) > 0L) {
    paste(gate$blocking_refs, collapse = ", ")
  } else {
    "unreferenced structural finding"
  }
  cli::cli_abort(c(
    "This reporting event cannot be executed because structural validation failed.",
    "x" = gate$summary,
    "i" = paste("Blocking references:", refs),
    "i" = "Repair the reporting event and validate it again before execution."
  ))
}

## Translate model findings into the workflow diagnostics contract without
## throwing away entity, id and field context.
##
## The severity is translated, not copied. The two channels have deliberately
## different vocabularies -- a finding is a statement about the reporting event
## and says GAP, a diagnostic is a statement about a run and says FAIL -- and
## this is the one place they meet. `ars_blockers()` is an exported contract
## that promises FAIL, so a GAP crossing here without translation would empty
## it: the most serious findings would arrive under a word no consumer counts.
#' @noRd
.validation_gate_diagnostics <- function(gate) {
  findings <- gate$findings
  if (is.null(findings) || nrow(findings) == 0L) {
    return(.EMPTY_DIAGNOSTICS())
  }

  locations <- vapply(seq_len(nrow(findings)), function(i) {
    parts <- c(findings$entity[i], findings$id[i], findings$field[i])
    parts <- parts[!is.na(parts) & nzchar(parts)]
    if (length(parts) == 0L) NA_character_ else paste(parts, collapse = " / ")
  }, character(1))

  severity <- findings$severity
  severity[severity %in% .GAP_SEVERITIES] <- "FAIL"

  data.frame(
    stage = "validate_ars",
    severity = severity,
    input = INPUT_ARS,
    tlf_number = NA_character_,
    location = locations,
    problem = findings$problem,
    action = findings$action,
    stringsAsFactors = FALSE
  )
}
