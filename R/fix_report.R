## arsbridge -- fix_report.R
##
## The fix report: what this run could not resolve, and where to go and change
## it.
##
## The validation report says what is wrong with the event. The fill debrief
## says what happened to each cell. Neither answers the question an author
## actually has after a run that completed with gaps -- "which document do I
## open, and what do I type?" -- because the answer lives in three places at
## once: the finding's code, the address it came from, and what the engine then
## did about it.
##
## This file joins those three. A finding contributes its code and its address;
## `.FIX_HINTS` contributes the standing advice for that code; the reservation
## map contributes what was actually withheld. The last part is why
## `reserved_as` is derived rather than written into the catalogue: a hint that
## claimed "this reserves the analysis" would keep saying so after a change that
## stopped reserving, and the report would be confidently wrong about the
## engine's own behaviour.

## Where a fix belongs. Five documents, because those are the five an author of
## a reporting event can actually edit -- the shell's annotation, a supplement
## workbook, the ARS JSON itself, the ADaM spec, and the data.
.FIX_WHERE <- c("annotation", "supplement", "ARS edit", "ADaM spec", "data")

## One standing hint per registered finding code.
##
## Keyed on `ref` rather than on the finding's `problem` text for the reason
## `.VALIDATION_REFS` exists at all: display wording is written for a human and
## changes freely, so joining on it would tie the report to prose. The coverage
## test in test-fix_report.R holds the contract both ways -- a new code cannot
## ship without a hint, and a hint cannot outlive the code it explains.
##
## Each entry answers three different questions, which is why it is a list and
## not a sentence:
##
##   cause        what the event actually says, in the author's own terms
##   consequence  what the run does about it -- and, where a result is
##                withheld, why producing one anyway would be worse
##   fix_where    which document to open
##   fix          what to change there
##
## `consequence` earns its place: an author who is told only "this is broken"
## reasonably asks why arsbridge could not carry on. Saying what the number
## would have been if it had is what makes a reserved cell read as a decision
## rather than a failure.
.FIX_HINTS <- list(

  ## ---- Identity and references ---------------------------------------------

  ENTITY_ID_MISSING = list(
    cause = "An entity in the reporting event carries no id.",
    consequence = paste(
      "Nothing can reference it, and nothing it should have restricted is",
      "restricted. The whole event is reserved, because resolution by id is",
      "exactly what stopped being trustworthy: a finding about a missing id",
      "can only name the entity by position, so it cannot be narrowed to the",
      "analyses at risk."),
    fix_where = "ARS edit",
    fix = "Give the entity a unique id, then re-run."
  ),

  ENTITY_ID_DUPLICATED = list(
    cause = "Two entities in the same pool share one id.",
    consequence = paste(
      "A reference to that id resolves to one of them, and not necessarily",
      "the same one at every stage -- the same id can select one object when",
      "the analysis runs and another when the workbook is filled. Both",
      "entities are reserved rather than guessing which was meant."),
    fix_where = "ARS edit",
    fix = "Make the ids unique. Re-point whatever referenced the duplicate."
  ),

  METHOD_REF_UNRESOLVED = list(
    cause = "The analysis names a method the event does not contain.",
    consequence = paste(
      "There is no operation to compute, so the analysis is reserved.",
      "Falling back to a default would show a statistic nobody asked for --",
      "a mean where the shell asked for a median formats and renders just as",
      "well as the right number."),
    fix_where = "ARS edit",
    fix = paste(
      "Point the analysis at a method the event defines, or add the method.",
      "If the row's intent came from the annotation, correct it there and",
      "rebuild instead.")
  ),

  ANALYSIS_SET_REF_UNRESOLVED = list(
    cause = "The analysis names a population the event does not contain.",
    consequence = paste(
      "The analysis is reserved. Running it unfiltered is the dangerous",
      "alternative: every count and every percentage would be computed over",
      "the whole dataset instead of the population, and the numbers would",
      "look entirely ordinary."),
    fix_where = "ARS edit",
    fix = paste(
      "Point the analysis at an analysis set the event defines, or add that",
      "population. If the shell row named the population, fix the annotation",
      "and rebuild.")
  ),

  DATA_SUBSET_REF_UNRESOLVED = list(
    cause = "The analysis names a data subset the event does not contain.",
    consequence = paste(
      "The analysis is reserved, for the same reason as a missing",
      "population: without the filter the analysis would compute over every",
      "record rather than the subset the row asked for."),
    fix_where = "ARS edit",
    fix = "Add the data subset, or point the analysis at one that exists."
  ),

  GROUPING_REF_UNRESOLVED = list(
    cause = "The analysis names a grouping the event does not contain.",
    consequence = paste(
      "The analysis is reserved. Ungrouped, it would still produce a number",
      "-- and that number would match any cell carrying no group level, so a",
      "total would silently appear under a treatment column."),
    fix_where = "ARS edit",
    fix = "Add the grouping, or point the analysis at one the event defines."
  ),

  ANALYSIS_REF_UNRESOLVED = list(
    cause = "An output lists an analysis the event does not contain.",
    consequence = paste(
      "Nothing is reserved: an analysis that does not exist computes",
      "nothing, so the cells it would have filled stay empty on their own.",
      "The other analyses on the output are unaffected and still compute."),
    fix_where = "ARS edit",
    fix = "Remove the stale reference, or add the analysis it names."
  ),

  ANALYSIS_NOT_DISPLAYED = list(
    cause = "An analysis exists but no output displays it.",
    consequence = paste(
      "It computes and lands in the ARD, but no cell shows it. Nothing is",
      "withheld -- this is a note about coverage, not a defect."),
    fix_where = "ARS edit",
    fix = paste(
      "Add it to an output's displayed analyses if it was meant to appear,",
      "or leave it if it is deliberately ARD-only.")
  ),

  OUTPUT_HAS_NO_ANALYSES = list(
    cause = "An output displays no analyses.",
    consequence = paste(
      "That output produces no numbers. Nothing else is affected, and no",
      "other output is held back for it."),
    fix_where = "annotation",
    fix = paste(
      "Usually the shell's rows never bound to anything -- check the row",
      "annotations name a dataset and variable the ADaM spec carries.")
  ),

  CONTENTS_ANALYSIS_STALE = list(
    cause = "The event's contents list names an analysis that is gone.",
    consequence = "Bookkeeping only; no result is affected.",
    fix_where = "ARS edit",
    fix = "Refresh the contents list, or re-run the build to regenerate it."
  ),

  CONTENTS_OUTPUT_STALE = list(
    cause = "The event's contents list names an output that is gone.",
    consequence = "Bookkeeping only; no result is affected.",
    fix_where = "ARS edit",
    fix = "Refresh the contents list, or re-run the build to regenerate it."
  ),

  ## ---- Semantics -----------------------------------------------------------

  SEMANTIC_SOURCE_NOT_DECLARED = list(
    cause = paste(
      "The analysis computes from a variable its own annotation never",
      "named."),
    consequence = paste(
      "The analysis is reserved. This check exists precisely because the",
      "wrong number here is plausible: two variables can agree in one data",
      "cut and diverge in the next, so the result would pass review and be",
      "wrong later."),
    fix_where = "annotation",
    fix = paste(
      "Either annotate the row with the variable it should compute from, or",
      "correct the analysis to use the variable the annotation declared.",
      "They must name the same source.")
  ),

  UNRESOLVED_VARIABLE_ROLE = list(
    cause = paste(
      "A variable's role in the analysis -- what it groups, filters or",
      "measures -- never resolved."),
    consequence = paste(
      "The analysis is reserved. A guessed role changes what the number",
      "means, not merely how it is labelled."),
    fix_where = "annotation",
    fix = paste(
      "Make the row's annotation state the variable's role explicitly, or",
      "supply it through the supplement.")
  ),

  ## ---- Groupings and the column axis ---------------------------------------

  GROUPING_DATASET_CONFLICT = list(
    cause = paste(
      "One grouping names two different datasets -- a flat grouping dataset",
      "and a nested grouping variable's dataset that disagree."),
    consequence = paste(
      "Every analysis using that grouping is reserved. This is the most",
      "dangerous of the structural defects: the denominator would be taken",
      "from whichever dataset happened to win, so percentages would be over",
      "the wrong subject count while looking completely normal."),
    fix_where = "ARS edit",
    fix = "Make both name the same dataset."
  ),

  FIXED_GROUPING_EMPTY = list(
    cause = "A fixed grouping declares no groups.",
    consequence = paste(
      "Every analysis using it is reserved. Ungrouped, those analyses would",
      "produce a single total that matches any cell with no group level --",
      "so the total would appear under a column that asked for a subgroup."),
    fix_where = "ARS edit",
    fix = "Add the groups, or make the grouping data-driven."
  ),

  GROUPING_VARIABLE_NOT_LINKED = list(
    cause = "A grouping's variable is not linked to a dataset.",
    consequence = paste(
      "The analyses using it are reserved: without a dataset the grouping",
      "variable cannot be read, and the analysis would fall back to",
      "computing ungrouped."),
    fix_where = "ARS edit",
    fix = "Link the grouping variable to the dataset that carries it."
  ),

  GROUPING_ORDER_AMBIGUOUS = list(
    cause = "Two groupings on one analysis declare the same order.",
    consequence = paste(
      "Numbers are still produced; only the order the groupings nest in is",
      "undefined, which can transpose a nested display's rows and columns."),
    fix_where = "ARS edit",
    fix = "Give the ordered groupings distinct order values."
  ),

  FLAT_AXIS_COLUMN_COUNT_MISMATCH = list(
    cause = paste(
      "The shell's column axis and the grouping behind it have different",
      "numbers of columns."),
    consequence = paste(
      "Nothing is reserved. The columns that do match still compute and",
      "fill; the unmatched ones simply find no result and stay empty.",
      "Reserving the analysis would throw away the correct columns to",
      "punish the incorrect ones."),
    fix_where = "annotation",
    fix = paste(
      "Align the shell's columns with the grouping's levels -- add the",
      "missing column, remove the extra one, or correct the grouping.")
  ),

  FLAT_AXIS_COLUMN_LABEL_MISMATCH = list(
    cause = "A shell column's label does not match any level of its grouping.",
    consequence = paste(
      "Nothing is reserved. That one column finds no result and stays empty;",
      "the rest of the row fills normally."),
    fix_where = "annotation",
    fix = paste(
      "Make the column header read as a value of the grouping variable, as",
      "the ADaM spec records that value.")
  ),

  ## ---- Methods -------------------------------------------------------------

  METHOD_NOT_ASSIGNED = list(
    cause = "The analysis has no method at all.",
    consequence = paste(
      "The analysis is reserved. With no declared operation the engine would",
      "fall back to a generic summary, which shows a statistic the shell",
      "never asked for."),
    fix_where = "annotation",
    fix = paste(
      "Annotate the row so its intent is readable -- a count, a count with",
      "percentage, a summary -- or assign a method directly in the ARS.")
  ),

  METHOD_PLACEHOLDER_SLOT_MISMATCH = list(
    cause = paste(
      "The placeholder shows more numbers than the row's method computes --",
      "most often an 'xx (xx.x)' on a count-only row."),
    consequence = paste(
      "Nothing is reserved, and the statistics the method does compute still",
      "fill. Only the surplus slot stays empty, because there is no",
      "statistic behind it to write."),
    fix_where = "annotation",
    fix = paste(
      "Annotate the row as what the placeholder actually shows, or simplify",
      "the placeholder to match the statistic.")
  ),

  METHOD_STRATA_MISSING = list(
    cause = "A method declares stratification but names no strata.",
    consequence = paste(
      "The analysis computes unstratified. The number is produced, so check",
      "it is the one the row intended."),
    fix_where = "ARS edit",
    fix = "Name the strata on the method, or remove the stratification."
  ),

  METHOD_NOT_EXECUTABLE = list(
    cause = paste(
      "The method is one arsbridge deliberately does not compute -- an",
      "inferential or model-based result."),
    consequence = paste(
      "The cell is reserved for manual derivation and appears on",
      "ars_manual_worklist(). This is somebody's job rather than nobody's:",
      "the semantics are known, arsbridge simply does not compute them."),
    fix_where = "ARS edit",
    fix = paste(
      "Nothing to fix if the row is genuinely inferential. Derive the value",
      "and enter it, or point the row at a method the engine computes.")
  ),

  METHOD_FALLBACK_SUMMARIZER = list(
    cause = "The method has no dedicated executor, so a generic summary ran.",
    consequence = paste(
      "A number was produced by approximation. Nothing is reserved -- the",
      "method exists and the event declared it -- but the statistic may not",
      "be the one the method names."),
    fix_where = "ARS edit",
    fix = paste(
      "Check the value the summary produced. If that value is not the",
      "statistic the row intended, point the row at a method the engine",
      "computes directly.")
  ),

  METHOD_CONDITIONAL = list(
    cause = paste(
      "The method's operation depends on a condition evaluated at run time."),
    consequence = paste(
      "A number was produced under whichever branch the data selected.",
      "Nothing is reserved; the note exists so the branch is checked."),
    fix_where = "ARS edit",
    fix = "Confirm the branch the data selected is the intended one."
  ),

  ## ---- Nested displays and declared result paths ---------------------------

  NESTED_CHILD_UNLINKED = list(
    cause = "A nested child row is not linked to a parent row.",
    consequence = paste(
      "The child computes but has no parent to nest under, so its cells may",
      "not find a home in the display. Nothing is reserved."),
    fix_where = "annotation",
    fix = "Annotate the child row so it names its parent."
  ),

  NESTED_GROUPING_MISSING = list(
    cause = paste(
      "A nested display is missing the grouping that defines its nesting."),
    consequence = paste(
      "The display cannot nest; its rows fill flat. Nothing is reserved."),
    fix_where = "ARS edit",
    fix = "Add the nesting grouping to the analysis's ordered groupings."
  ),

  HEADER_TREE_MISSING = list(
    cause = "A multi-level column header has no header tree recorded.",
    consequence = paste(
      "Columns cannot be addressed by their full header path, so some cells",
      "may not bind. Nothing is reserved."),
    fix_where = "annotation",
    fix = "Rebuild from the current shell so the header rows are read again."
  ),

  DISPLAY_COLUMN_COUNT_MISMATCH = list(
    cause = paste(
      "The output's declared columns and the columns its analyses produce",
      "differ in number."),
    consequence = paste(
      "Every analysis on the output is reserved: the mapping from result to",
      "column is no longer one-to-one, so a value could be written under the",
      "wrong header."),
    fix_where = "annotation",
    fix = "Align the shell's column axis with the groupings the rows use."
  ),

  UNMAPPED_LEAF_COLUMN = list(
    cause = "A leaf column of the display maps to no result path.",
    consequence = paste(
      "The output's analyses are reserved, because a column with no declared",
      "path can receive a value that belongs to a neighbour."),
    fix_where = "annotation",
    fix = "Annotate the column header so it names the grouping level it shows."
  ),

  INVALID_CARTESIAN_PRODUCT = list(
    cause = paste(
      "The output's groupings do not combine into the column grid the",
      "display declares."),
    consequence = paste(
      "The output's analyses are reserved: the intended grid cannot be",
      "reconstructed, so column positions cannot be trusted."),
    fix_where = "ARS edit",
    fix = paste(
      "Correct the ordered groupings so their combination matches the",
      "display.")
  ),

  SUBTOTAL_SCOPE_UNDEFINED = list(
    cause = "A subtotal column does not say which categories it totals.",
    consequence = paste(
      "The output's analyses are reserved. A subtotal computed over an",
      "assumed scope is a plausible number over the wrong set of rows."),
    fix_where = "annotation",
    fix = "State the categories the subtotal covers in the column's annotation."
  ),

  SUBTOTAL_EXCLUDES_UNDISPLAYED_CATEGORIES = list(
    cause = paste(
      "A subtotal covers categories the display does not show, so its total",
      "exceeds the visible rows."),
    consequence = paste(
      "The number is produced as declared. Nothing is reserved -- this is",
      "correct behaviour when it is intended, and a note when it is not."),
    fix_where = "annotation",
    fix = "Show the missing categories, or narrow the subtotal's scope."
  ),

  DUPLICATE_RESULT_PATH = list(
    cause = "Two columns of one output declare the same result path.",
    consequence = paste(
      "The output's analyses are reserved: both columns would show the same",
      "number under different headers, which reads as agreement between two",
      "independent results."),
    fix_where = "annotation",
    fix = "Give each column a distinct grouping level, or remove the duplicate."
  ),

  ## ---- The shell and the ADaM spec -----------------------------------------

  SHELL_LINE_NOT_ANALYSED = list(
    cause = "A shell row produced no analysis.",
    consequence = paste(
      "That row's cells stay on their placeholders. Nothing else is",
      "affected."),
    fix_where = "annotation",
    fix = paste(
      "Annotate the row with a DATASET.VARIABLE the ADaM spec carries, or",
      "add the row through the supplement.")
  ),

  POPULATION_NOT_PARSED = list(
    cause = "A population statement could not be read as a condition.",
    consequence = paste(
      "The analysis falls back to the output's population, so the number is",
      "produced over a wider set than the row's own text asked for. Check",
      "it."),
    fix_where = "annotation",
    fix = paste(
      "Restate the population as a simple condition on a variable the spec",
      "carries, or declare it through the supplement.")
  ),

  DATASET_NOT_IN_SPEC = list(
    cause = "The analysis names a dataset the ADaM spec does not carry.",
    consequence = paste(
      "The analysis is reserved. The engine cannot read the dataset either,",
      "so no result exists for it in any case."),
    fix_where = "ADaM spec",
    fix = paste(
      "Add the dataset to the spec, correct the annotation to a dataset the",
      "spec has, or supply the dataset with the study data.")
  ),

  VARIABLE_NOT_IN_SPEC = list(
    cause = "An annotation names a variable its dataset does not carry.",
    consequence = paste(
      "Nothing is reserved here; the row simply fails to bind and its cells",
      "stay on their placeholders."),
    fix_where = "ADaM spec",
    fix = paste(
      "Correct the annotation to a variable the spec records, or add the",
      "variable to the spec if the data has it.")
  ),

  SEPARATOR_IN_CONDITION_VALUE = list(
    cause = paste(
      "A condition's value contains the character used to separate values,",
      "so the condition may split in the wrong place."),
    consequence = paste(
      "A number is produced, but possibly over a mis-split condition.",
      "Nothing is reserved; check the value that contains the separator."),
    fix_where = "annotation",
    fix = paste(
      "Quote the value, or restate the condition so the separator does not",
      "appear inside it.")
  )
)

#' The standing hint for one finding code.
#'
#' An unregistered code returns a placeholder rather than erroring. The report
#' is bookkeeping about a run, and bookkeeping must never take a finished run
#' down -- but it must not look complete either, so the placeholder says plainly
#' that no hint exists. `.add_finding()` refuses unregistered codes anyway, and
#' the coverage test keeps the catalogue and the vocabulary in step; this is the
#' behaviour if both are somehow bypassed.
#' @noRd
.fix_hint <- function(ref) {
  hint <- if (length(ref) == 1L && !is.na(ref)) .FIX_HINTS[[ref]] else NULL
  if (is.null(hint)) {
    return(list(cause = NA_character_, consequence = NA_character_,
                fix_where = NA_character_,
                fix = "No fix hint is registered for this code."))
  }
  hint
}

#' Where the fix report for a run belongs.
#'
#' The phase is in the FILE NAME, so two runs of the same study leave two
#' reports side by side and the supplement's effect is visible as the difference
#' between them. The artefact key stays singular, so the app shows one line
#' rather than three "not produced" ones.
#' @noRd
.fix_report_path <- function(output_dir, mode = NA_character_) {
  mode <- as.character(mode %||% NA_character_)[1]
  if (is.na(mode) || !nzchar(mode)) {
    return(file.path(output_dir, "fix_report.xlsx"))
  }
  file.path(output_dir, paste0("fix_report_", mode, ".xlsx"))
}

#' Findings ordered most serious first.
#' @noRd
.order_by_severity <- function(findings) {
  rank <- match(findings$severity, .FINDING_SEVERITY_RANK)
  rank[is.na(rank)] <- length(.FINDING_SEVERITY_RANK) + 1L
  order(rank, findings$ref %||% rep("", nrow(findings)))
}

#' What the engine did about each finding, as a phrase.
#'
#' Read from the reservation map rather than from the finding's scope, because
#' the two can legitimately differ: a reserving finding that names an entity
#' nothing references reserves nothing at all, and saying otherwise would send
#' the author looking for withheld cells that do not exist.
#' @noRd
.reserved_as_column <- function(findings, reservations) {
  by_finding <- reservations$by_finding %||% list()
  scope <- findings$scope %||% rep(NA_character_, nrow(findings))

  vapply(seq_len(nrow(findings)), function(i) {
    reached <- by_finding[[as.character(i)]]
    n <- length(reached %||% character(0))
    if (n > 0) {
      return(sprintf("reserved %d analys%s", n, if (n == 1L) "is" else "es"))
    }
    this_scope <- scope[i]
    if (is.na(this_scope)) return(NA_character_)
    if (identical(this_scope, "advisory")) return("reported only")
    if (identical(this_scope, "cell")) return("refused per cell at fill")
    ## A reserving scope that reached nothing: real, and worth saying plainly.
    "nothing references it, so nothing was withheld"
  }, character(1))
}

#' The Fix list sheet: one row per finding, with its address and its remedy.
#' @noRd
.fix_list_sheet <- function(findings, reservations) {
  if (is.null(findings) || nrow(findings) == 0) {
    return(data.frame(
      Status = "INFO",
      message = "Nothing to fix: this run reported no findings.",
      stringsAsFactors = FALSE
    ))
  }

  ## Computed against the ORIGINAL row order, because `by_finding` is keyed by
  ## row number in the frame the map was built from. Sorting first would join
  ## each finding to another finding's provenance.
  reserved_as <- .reserved_as_column(findings, reservations)
  hints <- lapply(findings$ref, .fix_hint)

  field_of <- function(name) {
    vapply(hints, function(h) h[[name]] %||% NA_character_, character(1))
  }

  sheet <- data.frame(
    Status      = findings$severity,
    ref         = findings$ref,
    scope       = findings$scope,
    entity      = findings$entity,
    id          = findings$id,
    field       = findings$field,
    source_doc  = findings$source_doc,
    sheet       = findings$sheet,
    cell_ref    = findings$cell_ref,
    row         = findings$row,
    col         = findings$col,
    locator     = findings$locator,
    problem     = findings$problem,
    cause       = field_of("cause"),
    consequence = field_of("consequence"),
    fix_where   = field_of("fix_where"),
    fix         = field_of("fix"),
    reserved_as = reserved_as,
    stringsAsFactors = FALSE
  )
  sheet[.order_by_severity(findings), , drop = FALSE]
}

#' The By ref sheet: one row per code, with how far it reached.
#' @noRd
.by_ref_sheet <- function(findings, reservations) {
  if (is.null(findings) || nrow(findings) == 0) {
    return(data.frame(
      Status = "INFO",
      message = "No findings, so no codes to summarise.",
      stringsAsFactors = FALSE
    ))
  }

  by_finding <- reservations$by_finding %||% list()
  refs <- unique(findings$ref)

  rows <- lapply(refs, function(ref) {
    which_rows <- which(findings$ref == ref)
    reached <- unlist(
      lapply(which_rows, function(i) by_finding[[as.character(i)]]),
      use.names = FALSE
    )
    hint <- .fix_hint(ref)
    data.frame(
      Status     = .worst_severity(findings$severity[which_rows]),
      ref        = ref,
      scope      = findings$scope[which_rows][1],
      n_findings = length(which_rows),
      n_analyses_reserved = length(unique(reached %||% character(0))),
      fix_where  = hint$fix_where %||% NA_character_,
      cause      = hint$cause %||% NA_character_,
      fix        = hint$fix %||% NA_character_,
      stringsAsFactors = FALSE
    )
  })

  sheet <- do.call(rbind, rows)
  rank <- match(sheet$Status, .FINDING_SEVERITY_RANK)
  rank[is.na(rank)] <- length(.FINDING_SEVERITY_RANK) + 1L
  sheet[order(rank, -sheet$n_findings, sheet$ref), , drop = FALSE]
}

#' The Reserved cells sheet: every cell that will carry no number.
#'
#' Written even when there is no ARD and no fill, with a row saying so. An
#' omitted sheet reads as "nothing was reserved", which is the opposite of the
#' truth in exactly the runs this report exists for.
#' @noRd
.reserved_cells_sheet <- function(reservations, census) {
  by_analysis <- reservations$by_analysis %||% list()

  ## With a census in hand the answer is addressable: real sheets and real
  ## cells the author can go and look at.
  if (!is.null(census) && nrow(census) > 0 && length(by_analysis) > 0) {
    reserved_rows <- which(census$analysis_id %in% names(by_analysis))
    if (length(reserved_rows) > 0) {
      cells <- census[reserved_rows, , drop = FALSE]
      keep <- intersect(c("sheet", "ref", "row", "col", "analysis_id",
                          "status", "reason"), names(cells))
      out <- cells[, keep, drop = FALSE]
      out$finding_ref <- vapply(
        out$analysis_id,
        function(id) by_analysis[[id]]$ref %||% NA_character_,
        character(1)
      )
      return(cbind(
        data.frame(Status = "GAP", stringsAsFactors = FALSE),
        out
      ))
    }
  }

  ## No census: the reservation is still known per analysis, which is the
  ## honest answer before a fill has addressed it to cells.
  if (length(by_analysis) > 0) {
    return(data.frame(
      Status = "GAP",
      analysis_id = names(by_analysis),
      finding_ref = vapply(by_analysis,
                           function(r) r$ref %||% NA_character_, character(1),
                           USE.NAMES = FALSE),
      scope = vapply(by_analysis,
                     function(r) r$scope %||% NA_character_, character(1),
                     USE.NAMES = FALSE),
      reason = vapply(by_analysis,
                      function(r) r$reason %||% NA_character_, character(1),
                      USE.NAMES = FALSE),
      note = paste("Reserved at the analysis level. Run the fill to see which",
                   "cells this covers."),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    Status = "PASS",
    message = "No result was reserved: every analysis in this event computes.",
    stringsAsFactors = FALSE
  )
}

#' The Run sheet: what produced this report.
#' @noRd
.run_sheet <- function(run) {
  facts <- list(
    "arsbridge version" = as.character(utils::packageVersion("arsbridge")),
    "written (UTC)"     = run$timestamp %||% NA_character_,
    "extraction mode"   = run$extraction_mode %||% NA_character_,
    "supplement trust"  = run$supplement_trust %||% NA_character_,
    "verdict"           = run$verdict %||% NA_character_,
    "shell"             = run$shell_path %||% NA_character_,
    "ADaM spec"         = run$adam_spec_path %||% NA_character_,
    "ADaM data"         = run$adam_dir %||% NA_character_,
    "ARS"               = run$ars_path %||% NA_character_,
    "findings (GAP)"    = run$n_gap %||% NA_character_,
    "findings (WARN)"   = run$n_warn %||% NA_character_,
    "findings (INFO)"   = run$n_info %||% NA_character_,
    "analyses reserved" = run$n_reserved %||% NA_character_
  )
  data.frame(
    Item  = names(facts),
    Value = vapply(facts, function(v) as.character(v)[1], character(1),
                   USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
}

#' Write the fix report workbook.
#'
#' The per-phase record of what a run could not resolve and where to change it.
#' Always six sheets: the run's own facts, the fix list, the per-code rollup,
#' the cells that carry no number, the run diagnostics, and the shared legend.
#' A sheet with nothing to report says so in a row rather than being omitted --
#' a missing sheet cannot be told apart from a writer that gave up.
#'
#' @param findings The findings frame from `validate_ars_model()` -- or, better,
#'   the gate's own `findings`, so the report and the engine describe one set of
#'   defects.
#' @param output_path Path of the `.xlsx` to write.
#' @param reservations The `list(by_analysis, by_finding)` map built from
#'   **these** findings. Built from a different set, `reserved_as` would
#'   describe reservations that never happened.
#' @param census Optionally the `census` frame from `ars_fill_shell()`, which
#'   turns analysis-level reservations into addressable cells.
#' @param diagnostics Optionally a diagnostics frame; its sheet is omitted when
#'   empty.
#' @param run A named list of run facts for the Run sheet: `extraction_mode`,
#'   `verdict`, `timestamp`, input paths, and counts.
#' @return Invisibly, `output_path`.
#' @export
write_fix_report <- function(findings, output_path, reservations = NULL,
                             census = NULL, diagnostics = NULL, run = list()) {
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    cli::cli_abort("openxlsx2 is required to write the fix report.")
  }
  findings <- findings %||% .new_findings()
  reservations <- reservations %||%
    list(by_analysis = list(), by_finding = list())

  severities <- findings$severity %||% character(0)
  reserved <- reservations$by_analysis %||% list()
  run$n_gap <- run$n_gap %||% sum(severities %in% .GAP_SEVERITIES)
  run$n_warn <- run$n_warn %||% sum(severities == "WARN")
  run$n_info <- run$n_info %||% sum(severities == "INFO")
  run$n_reserved <- run$n_reserved %||% length(reserved)

  wb <- openxlsx2::wb_workbook(creator = "arsbridge")

  wb$add_worksheet("Run")
  .write_styled_sheet(wb, "Run", .run_sheet(run), tint_col = NULL)

  wb$add_worksheet("Fix list")
  .write_styled_sheet(wb, "Fix list", .fix_list_sheet(findings, reservations),
                      tint_col = "Status")

  wb$add_worksheet("By ref")
  .write_styled_sheet(wb, "By ref", .by_ref_sheet(findings, reservations),
                      tint_col = "Status")

  wb$add_worksheet("Reserved cells")
  .write_styled_sheet(wb, "Reserved cells",
                      .reserved_cells_sheet(reservations, census),
                      tint_col = "Status")

  ## Unconditional, like every other sheet here, and unlike the fill debrief's
  ## equivalent. The report promises a fixed six sheets, so a reader who finds
  ## five cannot tell whether the run was quiet or the writer gave up -- the
  ## same ambiguity that makes "Reserved cells" unconditional above.
  wb$add_worksheet("Diagnostics (run)")
  if (!is.null(diagnostics) && nrow(diagnostics) > 0) {
    .write_styled_sheet(wb, "Diagnostics (run)", diagnostics,
                        tint_col = "severity")
  } else {
    .write_styled_sheet(
      wb, "Diagnostics (run)",
      data.frame(Status = "PASS",
                 message = "This run recorded no diagnostics.",
                 stringsAsFactors = FALSE),
      tint_col = "Status")
  }

  .write_legend_sheet(wb)

  openxlsx2::wb_save(wb, file = output_path, overwrite = TRUE)
  invisible(output_path)
}
