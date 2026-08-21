## arsbridge -- build_ars_json.R
## ---------------------------------------------------------------------------
## Assembles enriched TLF sections into a CDISC ARS v1.0 ReportingEvent
## object suitable for jsonlite serialisation AND consumable by
## siera::readARS().
##
## Siera (pharmaverse) reads ARS JSON via .read_ars_json_metadata() which
## demands a specific field shape. Where the ARS v1.0 spec and siera's
## shape diverge, we emit BOTH:
##
##   - Flat fields (siera-required): dataset / variable / groupingDataset
##     / groupingVariable / level / order on every set.
##   - Nested fields (ARS-correct): analysisVariable / condition objects
##     kept alongside so other ARS consumers (cards, ARS Excel exporters)
##     still see the structured form.
##
## Two table-of-contents structures (otherListsOfContents = LOPO,
## mainListOfContents = LOPA) are emitted in the format siera demands,
## because if either is missing, siera silently writes nothing.

## Per-operation placeholder for `referencedOperationRelationships`. siera
## reads `json_from$methods$operations[[i]]$referencedOperationRelationships`
## and merges by `operation_id` -- an entirely empty JSONAML3 crashes the
## merge ('by' must specify a uniquely valid column). Self-reference is
## harmless: siera uses these only for NUM/DEN cross-operation lookups.
.op_self_rel <- function(op_id) {
  list(list(
    id          = paste0(op_id, "_SELF"),
    operationId = op_id,
    description = "",
    ## The role vocabulary is closed (NUMERATOR/DENOMINATOR) and required.
    ## For a self-referential placeholder either term is semantically vacuous;
    ## NUMERATOR keeps the object schema-valid. siera carries the value into
    ## its metadata without branching on it.
    referencedOperationRole = list(controlledTerm = "NUMERATOR")
  ))
}

#' Attach a placeholder `referencedOperationRelationships` to each operation
#' of a method spec. Mutates a copy of the spec; original is left alone.
#' @noRd
.with_op_self_rels <- function(method_spec) {
  method_spec$operations <- lapply(method_spec$operations, function(op) {
    op$referencedOperationRelationships <- .op_self_rel(op$id)
    op
  })
  method_spec
}

## ---------------------------------------------------------------------------
## Standard AnalysisMethod catalogue. Each entry includes a `codeTemplate`
## block (code + parameters) so siera can expand a runnable R script per
## analysis. Templates use siera's substitution placeholders -- siera
## replaces `analysisidhere` -> Analysis.id, `methodidhere` -> Method.id,
## and each parameter `name` -> the value resolved from `valueSource`
## (`ana_var`, `AG_var1`, `by_vars`, etc., populated by siera's readARS()
## loop). Templates write into `df3_analysisidhere` because siera's
## post-processing then attaches AnalysisId/MethodId/OutputId columns.
##
## Templates here are intentionally minimal but RUNNABLE -- the lead
## programmer is expected to refine them per study, but the generated
## ARD_*.R files will at least parse and execute on the bundled
## 60-subject example data.
## ---------------------------------------------------------------------------

## Long character vectors here for readability; collapse with newlines on use.
.STANDARD_METHODS <- list(
  "Summary Statistics - Continuous" = list(
    id          = "MTH_SUMMARY_STATISTICS_CONTINUOUS",
    name        = "Summary Statistics - Continuous",
    label       = "Summary Statistics - Continuous",
    description = "n, mean, SD, median, Q1, Q3, min, max",
    operations = list(
      list(id = "OP_N",      name = "n",      label = "n",      order = 1L, resultPattern = "XXX"),
      list(id = "OP_MEAN",   name = "Mean",   label = "Mean",   order = 2L, resultPattern = "XXX.X"),
      list(id = "OP_SD",     name = "SD",     label = "SD",     order = 3L, resultPattern = "XXX.XX"),
      list(id = "OP_MEDIAN", name = "Median", label = "Median", order = 4L, resultPattern = "XXX.X"),
      list(id = "OP_Q1",     name = "Q1",     label = "Q1",     order = 5L, resultPattern = "XXX.X"),
      list(id = "OP_Q3",     name = "Q3",     label = "Q3",     order = 6L, resultPattern = "XXX.X"),
      list(id = "OP_MIN",    name = "Min",    label = "Min",    order = 7L, resultPattern = "XXX"),
      list(id = "OP_MAX",    name = "Max",    label = "Max",    order = 8L, resultPattern = "XXX")
    ),
    codeTemplate = list(
      context = "R (siera)",
      code = paste(
        "df3_analysisidhere <- df2_analysisidhere |>",
        "  dplyr::select(USUBJID, anavarhere) |>",
        "  unique() |>",
        "  dplyr::summarise(",
        "    OP_N      = sum(!is.na(anavarhere)),",
        "    OP_MEAN   = mean(anavarhere, na.rm = TRUE),",
        "    OP_SD     = stats::sd(anavarhere, na.rm = TRUE),",
        "    OP_MEDIAN = stats::median(anavarhere, na.rm = TRUE),",
        "    OP_Q1     = stats::quantile(anavarhere, 0.25, na.rm = TRUE),",
        "    OP_Q3     = stats::quantile(anavarhere, 0.75, na.rm = TRUE),",
        "    OP_MIN    = suppressWarnings(min(anavarhere, na.rm = TRUE)),",
        "    OP_MAX    = suppressWarnings(max(anavarhere, na.rm = TRUE))",
        "  ) |>",
        "  tidyr::pivot_longer(dplyr::everything(), names_to = 'operation', values_to = 'res') |>",
        "  dplyr::mutate(pattern = 'XXX.X')",
        sep = "\n"
      ),
      parameters = list(
        list(name = "anavarhere", valueSource = "ana_var",
             description = "Analysis variable name (resolved from Analyses.variable)")
      )
    )
  ),
  "Count and Percentage" = list(
    id          = "MTH_COUNT_AND_PERCENTAGE",
    name        = "Count and Percentage",
    label       = "Count and Percentage",
    description = "n (%) per category",
    operations = list(
      list(id = "OP_N",     name = "Count",       label = "Count",       order = 1L, resultPattern = "XXX"),
      list(id = "OP_PCT",   name = "Percentage",  label = "Percentage",  order = 2L, resultPattern = "XX.X"),
      list(id = "OP_DENOM", name = "Denominator", label = "Denominator", order = 3L, resultPattern = "XXX")
    ),
    codeTemplate = list(
      context = "R (siera)",
      code = paste(
        "denom_n <- length(unique(df2_analysisidhere$USUBJID))",
        "df3_analysisidhere <- df2_analysisidhere |>",
        "  dplyr::group_by(anavarhere) |>",
        "  dplyr::summarise(",
        "    OP_N     = dplyr::n_distinct(USUBJID),",
        "    OP_DENOM = denom_n,",
        "    .groups  = 'drop'",
        "  ) |>",
        "  dplyr::mutate(OP_PCT = 100 * OP_N / OP_DENOM) |>",
        "  tidyr::pivot_longer(",
        "    dplyr::starts_with('OP_'),",
        "    names_to  = 'operation',",
        "    values_to = 'res'",
        "  ) |>",
        "  dplyr::mutate(pattern = 'XXX')",
        sep = "\n"
      ),
      parameters = list(
        list(name = "anavarhere", valueSource = "ana_var",
             description = "Categorical variable being counted")
      )
    )
  ),
  "Subject Count" = list(
    id          = "MTH_SUBJECT_COUNT",
    name        = "Subject Count",
    label       = "Subject Count",
    description = "Unique subject count",
    operations = list(
      list(id = "OP_N", name = "n", label = "n", order = 1L, resultPattern = "XXX")
    ),
    codeTemplate = list(
      context = "R (siera)",
      code = paste(
        "df3_analysisidhere <- df2_analysisidhere |>",
        "  dplyr::distinct(USUBJID) |>",
        "  dplyr::summarise(res = dplyr::n()) |>",
        "  dplyr::mutate(operation = 'OP_N', pattern = 'XXX')",
        sep = "\n"
      ),
      parameters = list()
    )
  ),
  ## Subject Count's own arithmetic, plus the percentage the cell shows.
  ##
  ## A disposition or exposure row -- "Completed [ADSL.EOSSTT = 'COMPLETED']"
  ## displayed as "xx (xx.x)" -- counts subjects in a state AND states what
  ## share of the arm they are. Subject Count declares only the count, so the
  ## percentage the executor already computes is never asked for and the cell
  ## ships as "58 (xx.x)". Count and Percentage would declare it, but counts
  ## RECORDS: on a record-level dataset ("[ADAE.TRTEMFL = 'Y']") that turns a
  ## subject count into an event count without saying so. This method is the
  ## honest pairing: one row per subject first, then n / % / denominator.
  "Subject Count and Percentage" = list(
    id          = "MTH_SUBJECT_COUNT_PCT",
    name        = "Subject Count and Percentage",
    label       = "Subject Count and Percentage",
    description = "Unique subject count with percentage of the denominator",
    operations = list(
      list(id = "OP_N",     name = "Count",       label = "Count",       order = 1L, resultPattern = "XXX"),
      list(id = "OP_PCT",   name = "Percentage",  label = "Percentage",  order = 2L, resultPattern = "XX.X"),
      list(id = "OP_DENOM", name = "Denominator", label = "Denominator", order = 3L, resultPattern = "XXX")
    ),
    codeTemplate = list(
      context = "R (siera)",
      code = paste(
        "denom_n <- length(unique(df2_analysisidhere$USUBJID))",
        "df3_analysisidhere <- df2_analysisidhere |>",
        "  dplyr::distinct(USUBJID) |>",
        "  dplyr::summarise(",
        "    OP_N     = dplyr::n(),",
        "    OP_DENOM = denom_n,",
        "    .groups  = 'drop'",
        "  ) |>",
        "  dplyr::mutate(OP_PCT = 100 * OP_N / OP_DENOM) |>",
        "  tidyr::pivot_longer(",
        "    dplyr::starts_with('OP_'),",
        "    names_to  = 'operation',",
        "    values_to = 'res'",
        "  ) |>",
        "  dplyr::mutate(pattern = 'XXX')",
        sep = "\n"
      ),
      parameters = list()
    )
  ),
  "Kaplan-Meier Estimate" = list(
    id          = "MTH_KAPLAN_MEIER_ESTIMATE",
    name        = "Kaplan-Meier Estimate",
    label       = "Kaplan-Meier Estimate",
    description = "KM event rate, median survival, 95% CI",
    operations = list(
      list(id = "OP_EVENTS",  name = "Events",          label = "Events",          order = 1L, resultPattern = "XXX"),
      list(id = "OP_MEDIAN",  name = "Median (months)", label = "Median (months)", order = 2L, resultPattern = "XXX.X"),
      list(id = "OP_CI_LOW",  name = "95% CI Lower",    label = "95% CI Lower",    order = 3L, resultPattern = "XXX.X"),
      list(id = "OP_CI_HIGH", name = "95% CI Upper",    label = "95% CI Upper",    order = 4L, resultPattern = "XXX.X")
    ),
    codeTemplate = list(
      context = "R (siera)",
      code = paste(
        "## Kaplan-Meier template (placeholder -- refine per study).",
        "## Expects ADTTE-style df with AVAL (time) and CNSR (1=censored).",
        "df3_analysisidhere <- data.frame(",
        "  operation = c('OP_EVENTS', 'OP_MEDIAN', 'OP_CI_LOW', 'OP_CI_HIGH'),",
        "  res = c(",
        "    sum(df2_analysisidhere$CNSR == 0, na.rm = TRUE),",
        "    suppressWarnings(stats::median(df2_analysisidhere$AVAL, na.rm = TRUE)),",
        "    NA_real_, NA_real_",
        "  ),",
        "  pattern = c('XXX', 'XXX.X', 'XXX.X', 'XXX.X')",
        ")",
        sep = "\n"
      ),
      parameters = list()
    )
  ),
  "AE Frequency Count" = list(
    id          = "MTH_AE_FREQUENCY_COUNT",
    name        = "AE Frequency Count",
    label       = "AE Frequency Count",
    description = "Unique subjects with event, n (%)",
    operations = list(
      list(id = "OP_N",   name = "n",   label = "n",   order = 1L, resultPattern = "XXX"),
      list(id = "OP_PCT", name = "(%)", label = "(%)", order = 2L, resultPattern = "XX.X")
    ),
    codeTemplate = list(
      context = "R (siera)",
      code = paste(
        "denom_n <- length(unique(df2_analysisidhere$USUBJID))",
        "df3_analysisidhere <- df2_analysisidhere |>",
        "  dplyr::distinct(USUBJID, anavarhere) |>",
        "  dplyr::group_by(anavarhere) |>",
        "  dplyr::summarise(OP_N = dplyr::n(), .groups = 'drop') |>",
        "  dplyr::mutate(OP_PCT = 100 * OP_N / denom_n) |>",
        "  tidyr::pivot_longer(",
        "    dplyr::starts_with('OP_'),",
        "    names_to  = 'operation',",
        "    values_to = 'res'",
        "  ) |>",
        "  dplyr::mutate(pattern = 'XXX')",
        sep = "\n"
      ),
      parameters = list(
        list(name = "anavarhere", valueSource = "ana_var",
             description = "Event-categorising variable (e.g. AEDECOD)")
      )
    )
  ),
  "Listing" = list(
    id          = "MTH_LISTING",
    name        = "Listing",
    label       = "Listing",
    description = "Subject-level data listing",
    operations  = list(
      list(id = "OP_PASS", name = "Passthrough", label = "Passthrough", order = 1L, resultPattern = "X")
    ),
    codeTemplate = list(
      context = "R (siera)",
      code = paste(
        "## Listing: pass through the filtered ADaM rows as-is.",
        "df3_analysisidhere <- df2_analysisidhere |>",
        "  dplyr::mutate(operation = 'OP_PASS', res = NA_real_, pattern = 'X')",
        sep = "\n"
      ),
      parameters = list()
    )
  )
)

## A count expressed over person-time -- "E (E/100 PY)", "n/1000 patient-years".
## Its denominator is a sum of exposure, which no method in the catalogue
## carries, so a row asking for one cannot be computed from the standard
## methods however it is otherwise annotated.
##
## Standards-level clinical wording, in the same way "n (%)" is. No study's
## dataset, variable or row identifier participates.
.RE_PERSON_TIME_RATE <- paste0(
  "(?i)/\\s*[0-9]*\\s*",
  "(?:PY\\b|(?:patient|person|subject)[-\\s]*(?:year|time))"
)

## The description carried by the shared MTH_UNSUPPORTED_ANALYSIS object when
## rows are reserved for stating a statistic this package cannot compute.
##
## Deliberately generic: one method object serves every such row in a section,
## so a reason taken from whichever row happened to register it first would be
## wrong for the others. The row-specific reason travels with the row instead,
## in its own diagnostic.
.UNSUPPORTED_ROW_REASON <-
  "the row states a statistic arsbridge does not compute"

#' The statistic a row asks for that arsbridge has no method to compute, or
#' NULL when the row states nothing of the kind.
#'
#' Saying so plainly is the whole value of this function. Both shapes below
#' were previously built as a summary of whichever variable the row named,
#' which puts a number -- the wrong one -- into a cell that looks finished:
#'
#'   * an aggregation over records, which no catalogue method performs
#'   * a rate over exposure time, whose denominator no catalogue method carries
#'
#' The annotation states the derivation and the label states the statistic the
#' cell shows. The author speaks about intent in either, so both are read.
#'
#' @param row Stub row (`annotation`, and `label` when the shell carries one).
#' @return A reason string for the reservation, or NULL.
#' @noRd
.unsupported_row_intent <- function(row) {
  text <- paste(as.character(row$annotation %||% ""),
                as.character(row$label %||% ""))
  if (!nzchar(trimws(text))) return(NULL)

  ## Masked first, so an aggregation word or a person-time unit occurring
  ## INSIDE a quoted level is data rather than intent -- the same rule the
  ## where-clause grammar follows, for the same reason.
  masked <- .mask_literals(text)
  probe  <- if (is.null(masked)) text else masked$text

  if (grepl("(?i)\\bsum\\s+of\\b", probe, perl = TRUE)) {
    return("the row asks for a sum over records, which arsbridge does not compute")
  }
  if (grepl(.RE_PERSON_TIME_RATE, probe, perl = TRUE)) {
    return("the row asks for a rate over exposure time, which arsbridge does not compute")
  }
  NULL
}

#' Infer the analysis method for one stub row from its bound annotation form
#' (ADR 0003 Layer C). Deterministic: the annotation is authored ground truth,
#' so it overrides the section-level LLM method for this row.
#'
#' @param row  Stub row (needs `annotation`; `n_slots` when the shell said how
#'   many statistics the row displays).
#' @param var_is_categorical NA/TRUE/FALSE -- the spec's verdict on the row's
#'   primary variable (from `.var_is_categorical`).
#' @param filter The row's EFFECTIVE filter -- the clause that will be emitted
#'   for it -- as a WhereClause, an unresolved-condition object, or `NULL`.
#'   Supplied by the caller rather than re-derived here, so the method is
#'   inferred from the restriction the row will actually compute under. When
#'   absent it is read from the annotation, which is what the free-standing
#'   caller has.
#' @return list(method = standard-catalogue name, kind = layout kind); or
#'   list(method = NULL, kind = "manual", unsupported = <reason>) when the row
#'   asks for a statistic no catalogue method computes, which the caller
#'   reserves rather than substituting for; or NULL when the form is
#'   unrecognised (caller keeps the section default).
#' @noRd
.infer_row_method <- function(row, var_is_categorical = NA,
                              filter = NULL, filter_known = FALSE) {
  ## A trailing derivation note ("[ADQX.MEASDUR; = ADQX.ENDDY]") says how the
  ## variable is DERIVED, not which records to keep. Read as a filter it makes
  ## a continuous summary into a subject count, and the block's statistic rows
  ## then read as category levels. The variable's own type decides the method,
  ## exactly as it does when the author writes no note at all.
  ann <- .annotation_less_derivation_note(row$annotation)
  if (!nzchar(trimws(ann))) return(NULL)

  ## Before any method is chosen: the row may ask for a statistic this package
  ## has no way to produce. Choosing some other method for it is exactly how a
  ## wrong number ships -- the cell fills, looks finished, and reads as an
  ## answer to the question the author asked.
  unsupported <- .unsupported_row_intent(row)
  if (!is.null(unsupported)) {
    return(list(method = NULL, kind = "manual", unsupported = unsupported))
  }

  ## The placeholder shape decides whether a subject-count row also declares a
  ## percentage. Both Excel and Word readers carry `n_slots` when the shell
  ## makes that shape explicit. Without it, keep the count-only method.
  slots <- suppressWarnings(as.integer(row$n_slots %||% NA_integer_))
  has_percentage_slot <- !is.na(slots) && slots >= 2L
  subject_count_method <- if (has_percentage_slot) {
    "Subject Count and Percentage"
  } else {
    "Subject Count"
  }

  ## Count expression or a bare USUBJID reference -> distinct subject count.
  if (grepl("(?i)\\bcount\\s+of\\b|(?i)\\bunique\\s+USUBJID\\b", ann, perl = TRUE) ||
      grepl(paste0("\\b", .ADAM_DS, "\\.USUBJID\\b"), ann, perl = TRUE)) {
    kind <- if (has_percentage_slot) "subject_count_pct" else "subject_count"
    return(list(method = subject_count_method, kind = kind))
  }
  ## A condition is present. It says which RECORDS survive; it does not say
  ## which STATISTIC those records are reported with, and this function no
  ## longer reads it as if it did.
  ##
  ## What it used to do, and what that cost:
  ##
  ##   A condition on the row's own variable was read as "count subjects in
  ##   this state". `ADQX.QXVAL WHERE ADQX.QXVAL GT 0` therefore became a
  ##   subject count under a block asking for Mean/SD -- the threshold selects
  ##   observations, and what is reported about them is stated by the shell,
  ##   on rows this function cannot see.
  ##
  ##   Worse, "the condition is on my own variable" and "I could not read the
  ##   condition" both presented as an empty filter variable, so an unreadable
  ##   annotation took the same branch: a one-operation method under a line
  ##   displaying "xx (xx.x)", and a second, spurious finding about statistic
  ##   slots on a row whose only real defect was the filter.
  ##
  ## One case remains where the restriction still selects the method, and it is
  ## a TEMPORARY STRUCTURAL DEPENDENCY, not a semantic rule. Read the next
  ## paragraph before treating it as one.
  ##
  ## A restriction that pins the row's variable to a single value keeps the
  ## subject-count family. That is NOT because equality means "count": a shell
  ## may filter `AVAL = 30` and then ask for Mean/SD, and the filter would
  ## still only be saying which observations survive. It is because block
  ## construction downstream currently decides what a block IS from the method
  ## its first row was given -- so classifying pinned sibling rows as
  ## distributions makes the first one a categorical parent and collapses the
  ## rest into it as levels of a subset pinned to the first level. The
  ## dependency runs from the block builder, not from the semantics of `=`.
  ##
  ## To be REMOVED once block shape is determined independently, before method
  ## selection: at that point equality alone must no longer be sufficient
  ## evidence for the requested statistic, and the method must come from
  ## block/display/statistic evidence plus metadata through the constraint
  ## resolver. Until then, two conditions must hold for a pinned reading --
  ##
  ##   the clause was READ  an unresolved clause is evidence about nothing, so
  ##                        nothing is known about what it pins, however it is
  ##                        written; and
  ##   it is an equality    a threshold or a range leaves the variable free to
  ##                        vary among the survivors.
  ##
  ## Everything else falls through to the variable's own type. That includes
  ## `unknown`, deliberately and literally: the row is reserved by the caller
  ## on the marker it carries, and whatever method it shows for layout comes
  ## from evidence that is independently known.
  ##
  ## `.filter_role()` classifies what the restriction speaks about -- none,
  ## on_primary, scoping_other, mixed_conjunctive, unknown. It is evidence, not
  ## a decision, and is deliberately NOT consulted here: the constraint
  ## resolution that weighs it against block shape and requested statistics is
  ## the next piece of work, and until it exists a role must not quietly become
  ## a method by itself.
  ##
  ## Still guarded by condition evidence, and the guard is load-bearing for a
  ## second reason now: an annotation carrying no condition at all can still
  ## contain a word the structure check reads as negation ("not applicable"),
  ## and prose that states no filter must not reserve a row that computes.
  if (.has_condition_evidence(ann)) {
    ## The primary reference is read WITH its dataset, and both halves are
    ## compared: in a table whose rows come from different ADaM domains,
    ## matching a filter to the primary variable by name alone calls a filter
    ## "on the primary variable" because some other domain spells a variable
    ## the same.
    primary_ref <- list(dataset = "", variable = "")
    refs <- extract_annotation_vars(ann)
    if (length(refs) > 0) {
      pieces <- strsplit(refs[[1]], ".", fixed = TRUE)[[1]]
      primary_ref$dataset  <- pieces[[1]]
      primary_ref$variable <- if (length(pieces) >= 2) pieces[[2]] else ""
    }

    ## The row's EFFECTIVE restriction: the clause it will actually compute
    ## under, handed in by the caller, or parsed from the annotation for the
    ## free-standing caller that has only that. Read once, so nothing here can
    ## answer from a different view of the same restriction than the emitted
    ## DataSubset was built from.
    where <- if (isTRUE(filter_known)) filter else parse_where_clause(ann)

    ## The single-value case: a compatibility dependency of the block builder,
    ## scheduled for removal with the block-shape work. See the paragraph above
    ## for why it is not evidence about the requested statistic.
    if (.filter_pins_primary(where, primary_ref$dataset,
                             primary_ref$variable)) {
      kind <- if (has_percentage_slot) "filtered_count_pct" else "filtered_count"
      return(list(method = subject_count_method, kind = kind))
    }
    ## Anything else falls through: the restriction has said all it can, and
    ## the variable's own type decides what the line reports.
  }
  ## Primary variable type (from the ADaM spec) decides the method.
  if (isTRUE(var_is_categorical)) {
    return(list(method = "Count and Percentage", kind = "categorical"))
  }
  if (identical(var_is_categorical, FALSE)) {
    return(list(method = "Summary Statistics - Continuous", kind = "continuous"))
  }
  NULL
}

#' The variable a "once/subject VAR" annotation clause names, or NULL. Shell
#' authors use it to say a row counts each subject once (AE first-occurrence
#' flags like ADAE.AOCCIFL); its presence routes a count row to the
#' distinct-subject method.
#' @noRd
.once_per_subject_var <- function(ann) {
  ann <- as.character(ann %||% "")
  if (!nzchar(trimws(ann))) return(NULL)
  hit <- regmatches(
    ann,
    regexpr("(?i)once\\s*/\\s*subject\\s+([A-Za-z0-9_]+\\.)?[A-Za-z0-9_]+",
            ann, perl = TRUE)
  )
  if (length(hit) == 0) return(NULL)
  toupper(trimws(sub("(?i)^once\\s*/\\s*subject\\s+", "", hit, perl = TRUE)))
}

#' The "sort: ..." clause of a nested parent row's annotation, or NULL when
#' the annotation carries none. Shell authors use it to pick the row order of
#' a nested block; without a clause the block defaults to descending
#' frequency (most frequent parent first, its child terms descending under
#' it). Recognised forms (case-insensitive):
#'   sort: alphabetical          -- A-Z at both levels
#'   sort: desc-freq             -- descending count, all columns combined
#'   sort: desc-freq('Drug A')   -- descending count in that column (quotes
#'                                  optional)
#' @return list(basis, column, raw); an unreadable clause comes back with
#'   basis = NA so the caller can warn and keep the default.
#' @noRd
.nested_sort_clause <- function(ann) {
  ann <- as.character(ann %||% "")
  if (!nzchar(trimws(ann))) return(NULL)
  hit <- regmatches(ann, regexpr("(?i)\\bsort\\s*:\\s*[^;]+", ann, perl = TRUE))
  if (length(hit) == 0) return(NULL)
  body <- trimws(sub("(?i)^sort\\s*:\\s*", "", hit, perl = TRUE))
  if (grepl("(?i)^(alphabetical|alpha)$", body, perl = TRUE)) {
    return(list(basis = "alphabetical", column = NULL, raw = body))
  }
  if (grepl("(?i)^desc[-_ ]?freq$", body, perl = TRUE)) {
    return(list(basis = "desc-freq", column = NULL, raw = body))
  }
  m <- regmatches(
    body,
    regexec("(?i)^desc[-_ ]?freq\\s*\\(\\s*'?([^')]+?)'?\\s*\\)$", body,
            perl = TRUE)
  )[[1]]
  if (length(m) == 2) {
    return(list(basis = "desc-freq", column = trimws(m[2]), raw = body))
  }
  list(basis = NA_character_, column = NULL, raw = body)
}

#' Nested two-level token blocks (AE by SOC/PT, MH by body system, ConMeds by
#' ATC class): the shell authors a data-driven parent token row
#' ("<System Organ Class>" annotated ADAE.AESOC) followed by child token rows
#' on a different variable of the same dataset ("<Preferred Term>",
#' ADAE.AEDECOD), the whole block repeating as further mock examples.
#'
#' Returns one role per row: "nested_parent" (first parent token row --
#' becomes the parent-level analysis), "nested_child" (first child token row
#' -- becomes the child analysis carrying the parent's variable as a
#' data-driven row grouping), "nested_repeat" (every other row of the
#' pattern; template examples, emitted nowhere), or NA.
#'
#' The cue is the token labels plus the variable sequence -- indentation is
#' deliberately not consulted (shell tables indent these rows
#' inconsistently). A run qualifies only when exactly two variables from one
#' dataset alternate as (parent, child+)+, so ordinary token rows (a lone
#' "<Visit>" row, mixed sequences) are never captured.
#' @noRd
#' The "stem" of a mock/token row label, or NULL when the label is not a
#' token. Shells author data-driven placeholder rows in two dialects:
#'
#'   angle    "<System Organ Class>", "<Preferred Term>", "<Reason #2>"
#'   numbered "SOC#1", "PT#1", "PT#2", "PT#n"
#'
#' Rows of the SAME stem are repeats of one template ("PT#1", "PT#2",
#' "PT#n" all stand for a Preferred Term), so the stem is what links a bare
#' repeat back to the annotated row that defines its variable. A trailing
#' number / "#n" / "n" is dropped from the stem; the angle dialect's stem is
#' its inner text, likewise de-numbered.
#' @noRd
.token_stem <- function(label) {
  label <- trimws(as.character(label %||% ""))
  if (!nzchar(label)) return(NULL)
  inner <- if (grepl("^<.+>$", label)) {
    sub("^<(.*)>$", "\\1", label)
  } else if (grepl("^[A-Za-z][A-Za-z ]{0,30}#\\s*(?:\\d+|[nNxX])$", label,
                   perl = TRUE)) {
    sub("#.*$", "", label)
  } else {
    return(NULL)
  }
  inner <- trimws(sub("#?\\s*(?:\\d+|[nNxX])\\s*$", "", trimws(inner)))
  if (!nzchar(inner)) return(NULL)
  toupper(inner)
}

#' TRUE for a bare continuation row ("...", a single ellipsis character,
#' "etc.") -- the shell's way of saying "and so on for every level"; it
#' names no analysis of its own. The ellipsis is written as an escape so
#' the source file stays ASCII.
#' @noRd
.is_continuation_row <- function(row) {
  if (isTRUE(row$has_annot)) return(FALSE)
  label <- trimws(as.character(row$label %||% ""))
  grepl("^(?:\\.{2,}|\u2026|etc\\.?)$", label, perl = TRUE)
}

#' @param enriched_rows the section's enrichments, unkeyed -- which one belongs
#'   to a row is decided per row by `.enrichment_for_row()`, never by name.
#' @noRd
.detect_nested_token_blocks <- function(rows, enriched_rows) {
  n <- length(rows)
  roles <- rep(NA_character_, n)
  if (n < 2) return(roles)

  ## The dataset/variable a row's own annotation names, or NULL.
  ##
  ## Was a lookup keyed on the row's label, which hands back whichever
  ## enrichment came first when two rows share their visible text -- the same
  ## first-match defect the pairing sites had, landing here on the one value
  ## this whole classification turns on. Every test below compares variables,
  ## so reading one row's variable as another's can take a two-variable
  ## parent/child block for a one-variable level block, or the reverse.
  ##
  ## .enrichment_for_row() lets the label assist the match but never outrank
  ## the row's own declared source, and yields nothing rather than a guess --
  ## in which case the annotation fallback just below reads the row directly,
  ## which for this purpose is the authoritative answer anyway.
  row_ref <- function(row) {
    if (!isTRUE(row$has_annot)) return(NULL)
    er <- .enrichment_for_row(row, enriched_rows) %||% list()
    ds  <- er$primary_dataset  %||% ""
    var <- er$primary_variable %||% ""
    if (!nzchar(var)) {
      refs <- extract_annotation_vars(row$annotation)
      if (length(refs) == 0) return(NULL)
      pieces <- strsplit(refs[1], ".", fixed = TRUE)[[1]]
      ds  <- pieces[1]
      var <- if (length(pieces) >= 2) pieces[2] else ""
    }
    if (!nzchar(var)) return(NULL)
    list(ds = toupper(ds), var = toupper(var))
  }

  stems <- vapply(rows, function(r) .token_stem(r$label) %||% NA_character_,
                  character(1))
  own   <- lapply(rows, row_ref)

  ## Un-annotated repeats inherit from the first ANNOTATED row sharing their
  ## token stem: the numbered dialect annotates "SOC#1"/"PT#1" once and
  ## leaves "PT#2", "PT#n", "SOC#2" bare.
  stem_ref <- list()
  for (k in seq_len(n)) {
    st <- stems[k]
    if (is.na(st) || is.null(own[[k]]) || !is.null(stem_ref[[st]])) next
    stem_ref[[st]] <- own[[k]]
  }

  refs <- lapply(seq_len(n), function(k) {
    if (is.na(stems[k])) return(NULL)
    own[[k]] %||% stem_ref[[stems[k]]]
  })

  i <- 1L
  while (i <= n) {
    if (is.null(refs[[i]])) {
      i <- i + 1L
      next
    }
    ## The run of consecutive token rows starting here.
    j <- i
    while (j < n && !is.null(refs[[j + 1L]])) j <- j + 1L
    run <- i:j
    run_vars <- vapply(run, function(k) refs[[k]]$var, character(1))
    run_ds   <- vapply(run, function(k) refs[[k]]$ds, character(1))
    distinct <- unique(run_vars)

    if (length(run) >= 2 && length(distinct) == 2 &&
          length(unique(run_ds)) == 1 &&
          identical(run_vars[1], distinct[1])) {
      parent <- distinct[1]
      child  <- distinct[2]
      ## Every parent row must be directly followed by at least one child.
      shape_ok <- TRUE
      for (k in seq_along(run_vars)) {
        if (identical(run_vars[k], parent) &&
              (k == length(run_vars) ||
                 !identical(run_vars[k + 1L], child))) {
          shape_ok <- FALSE
          break
        }
      }
      if (shape_ok) {
        ## The rows that CARRY the two analyses must be ones with their own
        ## annotation -- a bare repeat has only an inherited variable, and
        ## the analysis builder needs the real annotation text.
        pick <- function(v) {
          cand <- run[run_vars == v]
          annotated <- cand[!vapply(cand, function(k) is.null(own[[k]]),
                                    logical(1))]
          if (length(annotated) > 0) annotated[1] else cand[1]
        }
        roles[run] <- "nested_repeat"
        roles[pick(parent)] <- "nested_parent"
        roles[pick(child)]  <- "nested_child"
      }
    }

    ## ONE-level mock block: a run of token rows on a single variable that
    ## merely illustrates the levels of the categorical row ABOVE them
    ## ("Primary reason for discontinuation [ADSL.DCSREASN]" followed by
    ## "<Reason #1>", "<Reason #2>", "..."). That parent's categorical
    ## analysis already expands every level, so the mocks are emitted
    ## nowhere -- rendering them literally is the defect.
    if (all(is.na(roles[run])) && length(distinct) == 1) {
      above <- if (i > 1L) own[[i - 1L]] else NULL
      same_var_above <- !is.null(above) && is.na(stems[i - 1L]) &&
        identical(above$var, distinct[1]) &&
        identical(above$ds, run_ds[1])
      if (same_var_above) {
        roles[run] <- "level_repeat"
      } else {
        ## No annotated same-variable row above to lean on (an un-annotated
        ## header, a row on a different variable, or the sheet's first
        ## row): the block is SELF-describing. Its first annotated row
        ## carries the analysis; the repeats collapse into it exactly like
        ## the parented dialect.
        annotated <- run[!vapply(run, function(k) is.null(own[[k]]),
                                 logical(1))]
        if (length(annotated) > 0) {
          roles[run] <- "level_repeat"
          roles[annotated[1]] <- "self_template"
        }
      }
    }
    i <- j + 1L
  }

  ## A bare "..." continuation directly under a mock block belongs to it.
  for (k in seq_len(n)) {
    if (!is.na(roles[k]) || !.is_continuation_row(rows[[k]])) next
    if (k > 1L && !is.na(roles[k - 1L]) &&
          roles[k - 1L] %in% c("level_repeat", "nested_repeat",
                               "nested_child", "self_template")) {
      roles[k] <- "level_repeat"
    }
  }
  roles
}

#' The restriction an annotation states, as a typed WhereClause.
#'
#' The row-facing reader. It strips the derivation note -- a note is not a
#' filter, and asking the where-clause grammar to read one reserves a row that
#' computes perfectly well -- and hands back what `.annotation_condition()`
#' makes of the rest: the envelope forms `DATASET.VAR WHERE ...`,
#' `DATASET.VAR (when ...)` and `DATASET.VAR [where ...]`, and every other
#' annotation through the ordinary where-clause grammar.
#'
#' @param resolves `function(dataset, variable)` answering whether that exact
#'   pair exists in the study's ADaM spec. Without one nothing is provable and
#'   an envelope carrying bare names reserves -- the safe direction, since the
#'   alternative is filtering on a variable that may not exist.
#' @return `NULL` when the annotation states no restriction, an
#'   unresolved-condition object when it states one that cannot be read, or a
#'   WhereClause -- flat or compound.
#' @noRd
.annotation_where <- function(ann, resolves = NULL) {
  ann <- .annotation_less_derivation_note(ann)
  if (!nzchar(trimws(ann))) return(NULL)
  .annotation_condition(ann, resolves)
}

#' How a row should carry the restriction its annotation states.
#'
#' One reading, used by both row builders, so the primary row and a nested
#' child can never disagree about what an annotation restricts.
#'
#' Three outcomes, and the third is the one that is easy to lose:
#'
#'   `unresolved`  the annotation states a restriction that cannot be read, OR
#'                 states one the grammar read as nothing at all. Both reserve.
#'                 An analysis with no DataSubset computes over every record,
#'                 so "I found no condition in text that plainly carries one"
#'                 must never present as "there is no condition".
#'   `compound`    a WhereClause the flat shape cannot hold; rides on
#'                 `data_subset_compound`, the carrier the supplement path
#'                 already uses and `.build_data_subset()` already emits.
#'   `flat`        a single condition, in the shape the builder has always
#'                 consumed.
#'
#' An empty list means the annotation states no restriction, which is the
#' ordinary case for most rows.
#' @noRd
.row_restriction <- function(ann, resolves = NULL, supplement = NULL) {
  stated <- .annotation_less_derivation_note(ann)

  ## A typed supplement clause is AUTHORITATIVE about the filter and is never
  ## re-parsed from the annotation string. So the annotation's own filter is
  ## not read at all when one is supplied -- reading it could only produce a
  ## second answer to a question already settled.
  supplied <- !is.null(supplement)
  where <- if (supplied) NULL else .annotation_where(ann, resolves)
  if (!supplied && .is_unresolved_condition(where)) {
    return(list(unresolved = where))
  }

  ## A readable filter is not enough when the author also stated a rule about
  ## records the filter excludes -- and WHERE THE FILTER CAME FROM does not
  ## change that. A supplement is authoritative about which records survive;
  ## it is not evidence that an instruction about ABSENT records was
  ## implemented, and no WhereClause could express one. So the gate is asked
  ## of the annotation whatever supplies the filter, in the one reading both
  ## row builders share, and neither the carrier nor the supplement path can
  ## route around it.
  if (supplied || !is.null(where)) {
    instruction <- .unrepresented_instruction(stated)
    if (nzchar(instruction)) {
      return(list(unresolved =
        .stated_instruction_unrepresented(stated, instruction)))
    }
  }

  if (supplied) {
    flat <- .where_flat(supplement)
    if (is.null(flat)) return(list(compound = supplement))
    return(list(flat = flat))
  }

  if (is.null(where)) {
    gap <- .stated_filter_unrepresented(stated)
    if (.is_unresolved_condition(gap)) return(list(unresolved = gap))
    return(list())
  }

  flat <- .where_flat(where)
  if (is.null(flat)) return(list(compound = where))
  list(flat = flat)
}

#' The same restriction in the FLAT `{dataset, variable, comparator, value}`
#' shape, for callers whose contract is that shape.
#'
#' Kept deliberately rather than widened: the flat shape is what several
#' callers and their tests consume, and changing what it returns would change
#' what they mean. The row builders use `.row_restriction()` instead, which
#' routes a compound to `data_subset_compound` -- the carrier the supplement
#' path already uses.
#'
#' A compound therefore still reserves HERE -- not because it cannot be
#' carried, but because this return contract has nowhere to put it. So does an
#' annotation that states a restriction this grammar read as nothing at all:
#' `.stated_filter_unrepresented()` answers `NULL` for text carrying no
#' condition evidence and reserves for text that does, which is the same
#' answer this function gave before the typed reader existed.
#' @noRd
.subset_from_annotation <- function(ann, resolves = NULL) {
  stated <- .annotation_less_derivation_note(ann)
  where  <- .annotation_where(ann, resolves)
  if (.is_unresolved_condition(where)) return(where)

  if (!is.null(where)) {
    instruction <- .unrepresented_instruction(stated)
    if (nzchar(instruction)) {
      return(.stated_instruction_unrepresented(stated, instruction))
    }
  }

  flat <- .where_flat(where)
  if (!is.null(flat)) return(flat)
  .stated_filter_unrepresented(stated)
}

#' A restriction the grammar read, alongside an authored instruction it cannot
#' carry out.
#'
#' The filter here is not wrong -- it says exactly what it was written to say.
#' What is missing is a SECOND thing the author wrote: a rule about records the
#' filter excludes, which no `WHERE` clause can express. Computing the filter
#' alone produces a plausible number under a different definition than the one
#' asked for, and nothing on the analysis would say so.
#'
#' So the row reserves. It is a RESERVATION, not a refusal: when arsbridge can
#' represent the instruction, these rows compute, and nothing about the
#' annotation has to change.
#' @noRd
.stated_instruction_unrepresented <- function(ann, instruction) {
  diag_add(
    stage = "build_ars", severity = "WARN",
    problem = sprintf(
      paste("Filter '%s' was read, but the annotation also states '%s',",
            "which this version cannot represent"),
      ann, instruction),
    location = ann,
    action = paste("Results are reserved rather than computed under the filter",
                   "alone. The filter selects records; this instruction",
                   "concerns records the filter excludes, so applying one",
                   "without the other would report a different population",
                   "than the annotation describes.")
  )
  .unresolved_condition(ann, instruction)
}

#' The restriction a built row will actually compute under.
#'
#' One reading of one field set, so the method and the emitted DataSubset can
#' never disagree about what the row is filtered by. The order mirrors the
#' precedence the row loop applied when it filled these fields: an unresolved
#' condition outranks everything (there IS no usable filter), then a typed
#' compound, then a flat subset.
#' @noRd
.row_effective_filter <- function(er) {
  unresolved <- as.character(er$unresolved_condition %||% "")
  if (length(unresolved) > 0 && nzchar(unresolved[[1]])) {
    return(.unresolved_condition(unresolved[[1]]))
  }
  compound <- er$data_subset_compound
  if (!is.null(compound)) return(compound)
  flat <- er[["data_subset"]]
  if (!is.null(flat) && length(flat) > 0) return(flat)
  NULL
}

#' A filter the author stated that this builder cannot turn into a subset.
#'
#' Reached when the grammar READ the expression -- it is not unresolved -- but
#' the subset this function can build cannot hold it. Today that means one
#' thing: a compound expression, which `flat_data_subset()` answers with `NULL`
#' because it flattens to a single condition or to nothing.
#'
#' `NULL` was the old answer, and `NULL` here means "this annotation states no
#' filter". So a perfectly readable `A AND B` produced an analysis with no
#' DataSubset, which computes over every record and reports nothing -- the
#' silent over-count. The same failure the unresolved marker exists to prevent,
#' arriving through the one door that marker did not cover.
#'
#' So the row reserves instead, carrying the author's own text. It is a
#' RESERVATION, not a refusal: when the subset builder can carry a compound
#' expression, these rows compute, and nothing about the annotation has to
#' change.
#'
#' Returns `NULL` -- the honest "no filter stated" -- when the annotation
#' carries no condition evidence at all.
#' @noRd
.stated_filter_unrepresented <- function(ann) {
  if (!.has_condition_evidence(ann)) return(NULL)
  diag_add(
    stage = "build_ars", severity = "WARN",
    problem = sprintf(
      "Filter '%s' was read but cannot be carried as a DataSubset", ann),
    location = ann,
    action = paste("Results are reserved rather than computed over every",
                   "record. Supply the clause through the supplement, which",
                   "carries a compound expression, or state a single",
                   "condition.")
  )
  .unresolved_condition(ann, ann)
}

#' Build a CDISC ARS v1.0 ReportingEvent list from enriched sections.
#'
#' Emits a JSON-ready structure that satisfies BOTH the CDISC ARS v1.0
#' logical model AND `siera::readARS()`'s expected JSON shape. Where the
#' two disagree (flat vs nested fields), we emit both forms.
#'
#' @param sections   List of enriched TLF sections (output of
#'   `enrich_with_llm()` applied to each section).
#' @param study_id   Study identifier.
#' @param study_name Human-readable study name.
#' @param codelists  Optional codelists from `parse_adam_spec()$codelists`.
#'   When a categorical analysis variable resolves to one (via the spec's
#'   Codelist column or a codelist's Used-By list), its value -> decoded-label
#'   map ships in the ReportingEvent's `_meta$value_decodes`, so the engine
#'   and the emitted cards code display decoded values ("DEATH") instead of
#'   raw codes ("1"). Codelists above `.CODELIST_DECODE_MAX_TERMS` terms are
#'   skipped with a diagnostic.
#' @param ship_annotations When `TRUE`, programmer annotation lines captured
#'   below the shell tables are appended to each output's Footnote display
#'   section (debug escape hatch). Default `FALSE`: annotations are kept in
#'   the parsed sections / validation report only and never shipped.
#'
#' @return Named list ready for [jsonlite::toJSON()] (use
#'   `auto_unbox = TRUE, pretty = TRUE, null = "null"`).
#'
#' @keywords internal
#' @noRd
build_ars_json <- function(sections,
                           study_id   = "STUDY-001",
                           study_name = NULL,
                           spec_lookup = NULL,
                           codelists = NULL,
                           ship_annotations = FALSE,
                           extraction_mode = "llm",
                           supplement_trust = NULL,
                           analysis_reason  = .DEFAULT_ANALYSIS_REASON,
                           analysis_purpose = .DEFAULT_ANALYSIS_PURPOSE) {
  if (length(sections) == 0) {
    cli::cli_abort("Cannot build ReportingEvent: no TLF sections provided.")
  }

  ## TRUE when the ADaM spec marks this variable as character-typed or
  ## controlled-terminology (has a codelist) -- i.e. a categorical variable
  ## that must be summarised by counts, never by continuous statistics.
  .var_is_categorical <- function(dataset, variable) {
    if (is.null(spec_lookup) || is.null(variable) || !nzchar(variable)) return(NA)
    variable <- toupper(sub("^.*\\.", "", variable))
    dataset  <- toupper(dataset %||% "")
    rec <- spec_lookup[[paste0(dataset, ".", variable)]]
    ## Exact DATASET.VARIABLE, or no answer. There used to be a fallback that
    ## searched every dataset for a variable of the same NAME and took the
    ## first hit -- which in a table whose rows come from different ADaM
    ## domains answers a question about one dataset's variable with another
    ## dataset's metadata. Two domains carrying a same-named variable of
    ## different type is ordinary, and the verdict decides which METHOD the
    ## row computes.
    ##
    ## NA is not a worse answer than a borrowed one: it means "the spec does
    ## not describe this variable", which is true, and the caller keeps the
    ## section default rather than acting on evidence that belongs to another
    ## dataset. Dataset identity is part of the evidence, not decoration.
    if (is.null(rec)) return(NA)
    type <- tolower(as.character(rec$type %||% ""))
    cl   <- as.character(rec$codelist %||% "")
    grepl("char|text|string|^c$", type) || (nzchar(cl) && !is.na(cl))
  }
  count_method_id <- .STANDARD_METHODS[["Count and Percentage"]]$id

  ## --- Codelist decode support ---------------------------------------------
  ## Ordered term -> decoded-label map for one DATASET.VARIABLE, or NULL when
  ## no codelist resolves / the codelist is too large to expand. Memoised so
  ## the size-guard diagnostic fires once per variable, not once per row.
  decode_cache <- list()
  .decode_terms_for <- function(dataset, variable) {
    bare_var <- toupper(sub("^.*\\.", "", variable %||% ""))
    ds       <- toupper(dataset %||% "")
    if (!nzchar(bare_var) || length(codelists %||% list()) == 0) return(NULL)
    key <- paste0(ds, ".", bare_var)
    if (!is.null(decode_cache[[key]])) {
      cached <- decode_cache[[key]]
      return(if (identical(cached, "none")) NULL else cached)
    }
    cl <- .codelist_for(codelists, ds, bare_var, spec_lookup[[key]])
    terms <- if (is.null(cl) || nrow(cl$terms) == 0) {
      NULL
    } else {
      if (nrow(cl$terms) > .CODELIST_DECODE_MAX_TERMS) {
        ## Decoded, but not expanded: the engine drops the levels the data
        ## never took, so this shows the observed terms under their proper
        ## labels rather than 195 rows of zeros. INFO, not WARN -- the values
        ## are decoded either way, and only the row count differs.
        diag_add(
          stage = "build_ars", severity = "INFO",
          problem = sprintf(
            "Codelist '%s' for %s has %d terms (limit %d) -- decoded, showing observed terms only",
            cl$name %||% "?", key, nrow(cl$terms), .CODELIST_DECODE_MAX_TERMS),
          action = "Terms the data never takes are left out rather than listed as zero rows"
        )
      }
      cl$terms
    }
    decode_cache[[key]] <<- if (is.null(terms)) "none" else terms
    terms
  }

  ## `_meta$value_decodes` accumulator: one entry per DATASET.VARIABLE that
  ## some categorical-family analysis displays, each an ordered list of
  ## {value, label, order}. The subject key never gets a decode (its
  ## "categories" are subjects).
  value_decodes <- list()
  note_value_decode <- function(dataset, variable, method_id) {
    if (!(method_id %||% "") %in% .DECODE_METHOD_IDS) return(invisible(NULL))
    bare_var <- toupper(sub("^.*\\.", "", variable %||% ""))
    if (!nzchar(bare_var) || identical(bare_var, "USUBJID")) return(invisible(NULL))
    key <- paste0(toupper(dataset %||% ""), ".", bare_var)
    if (!is.null(value_decodes[[key]])) return(invisible(NULL))
    terms <- .decode_terms_for(dataset, bare_var)
    if (is.null(terms)) return(invisible(NULL))
    value_decodes[[key]] <<- lapply(seq_len(nrow(terms)), function(i) list(
      value = terms$term[i],
      label = terms$decode[i],
      order = terms$order[i]
    ))
    invisible(NULL)
  }

  ## TRUE when a "DATASET.VARIABLE" annotation reference is resolvable against
  ## the ADaM spec (exact DATASET.VAR key, or VAR present in any dataset).
  ## Used to warn, per TLF, when a shell references variables the spec lacks.
  ## spec_lookup is named by DATASET.VARIABLE (that's how .var_is_categorical
  ## indexes it). A reference is "developable" only when its exact
  ## dataset+variable key exists -- a same-named variable in a DIFFERENT
  ## dataset must not satisfy "ADSL.WEIGHT" (that would mask a genuine gap).
  .spec_keys_up <- toupper(names(spec_lookup %||% list()))
  .ref_present <- function(ref) toupper(ref) %in% .spec_keys_up
  ## The same question, asked the way the envelope reader needs it: does this
  ## exact dataset carry this exact variable? A bare name inside a filter
  ## inherits the head reference's dataset only when the answer is yes.
  .spec_resolves <- function(dataset, variable) {
    .ref_present(paste0(dataset, ".", variable))
  }

  analysis_sets    <- list(); seen_as  <- character()
  data_subsets     <- list(); seen_ds  <- character()
  grouping_factors <- list(); seen_gf  <- character()
  ## The same parallel-signature trick the groupings use, for the two other
  ## pools whose ids are minted from a name or a tag rather than from what
  ## they filter.
  as_signatures    <- character()
  ds_signatures    <- character()
  ## One definition signature per registered grouping, parallel to
  ## grouping_factors, so an axis already in the event is found by what it
  ## MEANS rather than by the id it happens to carry.
  gf_signatures    <- character()
  methods          <- list(); seen_mth <- character()
  analyses         <- list()
  outputs          <- list()
  unsupported      <- list()   ## output_id -> reason, for _meta + placeholders

  ## Register one GroupingFactor and hand back the object the event actually
  ## carries -- which is NOT always the one passed in. Two outputs that define
  ## an axis the same way share a single factor; two outputs that group the
  ## same VARIABLE differently each keep their own definition, the later one
  ## under a variant id. Matching on the id alone is what used to let a second
  ## output's columns inherit the first output's groups, so every caller must
  ## use the returned object rather than the one it built.
  register_grouping <- function(gf_obj, tlf_number = NULL) {
    signature <- .grouping_signature(gf_obj)
    known     <- match(signature, gf_signatures)
    if (!is.na(known)) return(grouping_factors[[known]])

    if (gf_obj$id %in% seen_gf) {
      gf_obj <- .rename_grouping(gf_obj, .next_variant_id(seen_gf, gf_obj$id))
      diag_add(
        stage = "build_ars", severity = "INFO", tlf_number = tlf_number,
        problem = sprintf(
          "%s is grouped differently here than in an earlier output; kept as %s",
          gf_obj$groupingVariable %||% "?", gf_obj$id),
        action = "Both definitions are kept -- confirm the two outputs really do mean different columns"
      )
    }
    if (isTRUE(attr(gf_obj, "codelist_derived"))) {
      diag_add(
        stage = "build_ars", severity = "WARN",
        problem = sprintf(
          "Column groups for %s derived from the spec codelist (%d levels) -- no header conditions were annotated",
          gf_obj$groupingVariable %||% "?", length(gf_obj$groups)),
        tlf_number = tlf_number,
        action = "Verify the derived columns (labels, order, missing-value handling) against the shell headers"
      )
    }
    grouping_factors[[length(grouping_factors) + 1L]] <<- gf_obj
    seen_gf       <<- c(seen_gf, gf_obj$id)
    gf_signatures <<- c(gf_signatures, signature)
    gf_obj
  }

  ## Register one AnalysisSet and hand back the object the event carries.
  ##
  ## A set's id comes from the population TEXT while its condition comes from
  ## the annotation, so two tables that both say "Safety Population" and filter
  ## differently used to collide -- the first won and every later analysis ran
  ## the wrong population. That is invisible in the output: the table still
  ## fills, with the wrong denominator.
  ##
  ## The awkward case is the population named but not annotated, which is
  ## ordinary in real shells: one table spells the filter out and another
  ## leaves it implied. Those are one set, not two, so an under-specified set
  ## joins its namesake -- and if the under-specified one arrived FIRST, the
  ## annotated one replaces it, because the alternative is every analysis
  ## silently running unfiltered.
  register_analysis_set <- function(as_obj, tlf_number = NULL) {
    signature <- .where_signature(as_obj)
    known     <- match(signature, as_signatures)
    if (!is.na(known)) return(analysis_sets[[known]])

    incumbent <- match(as_obj$id, seen_as)
    if (!is.na(incumbent)) {
      if (.entity_is_unscoped(as_obj)) {
        return(analysis_sets[[incumbent]])
      }
      if (.entity_is_unscoped(analysis_sets[[incumbent]])) {
        as_obj$order <- analysis_sets[[incumbent]]$order
        as_obj$level <- analysis_sets[[incumbent]]$level
        analysis_sets[[incumbent]] <<- as_obj
        as_signatures[[incumbent]] <<- signature
        diag_add(
          stage = "build_ars", severity = "INFO", tlf_number = tlf_number,
          problem = sprintf(
            "Population '%s' was carried without a filter until this output annotated one; the filter now applies to every analysis that uses it",
            as_obj$name %||% as_obj$id),
          action = "Confirm the earlier outputs really do mean this population"
        )
        return(as_obj)
      }

      as_obj$id <- .next_variant_id(seen_as, as_obj$id)
      diag_add(
        stage = "build_ars", severity = "WARN", tlf_number = tlf_number,
        problem = sprintf(
          "Population '%s' is defined differently here than in an earlier output; kept as %s",
          as_obj$name %||% "?", as_obj$id),
        action = "Two populations sharing one name is usually an authoring slip -- confirm both definitions are meant"
      )
    }

    as_obj$order <- length(analysis_sets) + 1L
    as_obj$level <- 1L
    analysis_sets[[length(analysis_sets) + 1L]] <<- as_obj
    seen_as       <<- c(seen_as, as_obj$id)
    as_signatures <<- c(as_signatures, signature)
    as_obj
  }

  ## Register one DataSubset and hand back the object the event carries. The
  ## id tag is lossy by construction -- dataset, variable and the FIRST value,
  ## no comparator -- so `AESEV = 'SEVERE'` and `AESEV NE 'SEVERE'` mint the
  ## same id while meaning opposite things. A clash here is expected rather
  ## than suspicious, hence INFO.
  register_data_subset <- function(ds_obj, tlf_number = NULL) {
    if (is.null(ds_obj)) return(NULL)

    signature <- .where_signature(ds_obj)
    known     <- match(signature, ds_signatures)
    if (!is.na(known)) return(data_subsets[[known]])

    if (ds_obj$id %in% seen_ds) {
      ds_obj$id <- .next_variant_id(seen_ds, ds_obj$id)
      diag_add(
        stage = "build_ars", severity = "INFO", tlf_number = tlf_number,
        problem = sprintf(
          "Row filter %s differs from an earlier one that shortens to the same id; kept as %s",
          ds_obj$name %||% "?", ds_obj$id),
        action = "Both filters are kept -- no action needed unless the two outputs meant one filter"
      )
    }

    ds_obj$order <- length(data_subsets) + 1L
    ds_obj$level <- 1L
    data_subsets[[length(data_subsets) + 1L]] <<- ds_obj
    seen_ds       <<- c(seen_ds, ds_obj$id)
    ds_signatures <<- c(ds_signatures, signature)
    ds_obj
  }

  for (sec in sections) {
    ## --- Capability-gated (unsupported) section ---------------------------
    ## arsbridge cannot compute these statistics, but -- unlike before -- the
    ## ARS still carries the analysis + a declarative (supported = FALSE) method
    ## so the Output -> Analysis -> Method chain stays intact (ADR 0002 phase 3).
    ## The engine reserves manual_pending stub ARD rows for them; the renderer
    ## still shows a numbered placeholder (recorded in _meta.unsupported_outputs)
    ## until partial rendering lands. Never coerce into a meaningless count.
    sec_unsupported <- isTRUE(sec$unsupported)
    ## Classify which gated statistics arsbridge can now actually compute (ADR
    ## 0001). A gated section with executable methods is built as a *partial*
    ## section: descriptive rows compute, each executable inferential method is
    ## appended as its own analysis, and only the residual is reserved. A gated
    ## section with nothing executable is reserved wholesale (Phase 3).
    cls <- if (sec_unsupported) classify_section_methods(sec) else
      list(executable = list(), residual = character())
    has_exec      <- length(cls$executable) > 0
    gated_generic <- sec_unsupported && !has_exec
    oid_ph        <- make_output_id(sec$tlf_number)
    if (gated_generic) {
      unsupported[[length(unsupported) + 1L]] <- list(
        id     = oid_ph,
        reason = sec$unsupported_reason %||% "not supported by arsbridge")
    } else if (sec_unsupported && length(cls$residual) > 0) {
      ## Partial: some cells compute, but a residual (e.g. a Newcombe interval)
      ## stays reserved -- numbered placeholder text names it.
      unsupported[[length(unsupported) + 1L]] <- list(
        id     = oid_ph,
        reason = paste(cls$residual, collapse = "; "))
    }

    ## --- TLF-level developability check ---
    ## Warn (once per TLF) when the shell references variables the ADaM spec
    ## does not contain -- those rows can't be developed and will be skipped.
    ## Skipped for gated sections: their rows are manual by definition.
    if (!sec_unsupported && !is.null(spec_lookup) && length(spec_lookup)) {
      ann_refs <- unique(unlist(lapply(
        Filter(function(r) isTRUE(r$has_annot), sec$stub_rows),
        function(r) extract_annotation_vars(r$annotation))))
      miss <- ann_refs[!vapply(ann_refs, .ref_present, logical(1))]
      if (length(miss)) {
        cli::cli_warn(
          "TLF {sec$tlf_number}: cannot be fully developed -- {length(miss)} variable{?s} not in the ADaM spec: {.val {miss}}")
        diag_add(
          stage = "build_ars", severity = "WARN", tlf_number = sec$tlf_number,
          location = sec$title %||% "",
          problem = sprintf("TLF %s references %d variable(s) not in the ADaM spec: %s",
                            sec$tlf_number, length(miss), paste(miss, collapse = ", ")),
          action = "These rows will be skipped at execution. Add the variable(s) to the ADaM dataset and spec to develop this TLF fully.")
      }
    }

    ## --- AnalysisSet from population ---
    ## Every later reference must use the RETURNED object: this is not always
    ## the one just built.
    as_obj <- register_analysis_set(.build_analysis_set(sec), sec$tlf_number)

    ## --- GroupingFactors from the (ordered) grouping list ---
    gf_objs <- .build_groupings(sec, codelists = codelists,
                                spec_lookup = spec_lookup)
    ## The resolved objects, not the ones just built: a definition already in
    ## the event comes back under the id it was registered with, so both
    ## gf_ids and the result paths below name groups the event really carries.
    gf_objs <- lapply(gf_objs, register_grouping, tlf_number = sec$tlf_number)
    gf_ids <- vapply(gf_objs, function(g) g$id, character(1))

    ## Hierarchical column tree: the declared result-column paths become an
    ## output-level extension, and the overall Total travels as an explicit
    ## GRAND_TOTAL path rather than the includeTotal boolean.
    result_group_paths <- .build_result_group_paths(sec, gf_objs)
    if (!is.null(result_group_paths)) sec$include_total <- FALSE

    ## --- AnalysisMethod (standard catalogue, or declarative-unsupported) ---
    ## A wholesale-gated section's descriptive rows are reserved; a partial
    ## section's descriptive rows compute with the normal method. A LISTING
    ## section's method is structural, not statistical -- force MTH_LISTING
    ## regardless of the LLM's analysis-type guess, otherwise the listing
    ## renderer finds no MTH_LISTING columns and the output degrades to a
    ## placeholder (ADR 0003 Phase 5).
    mth_obj <- if (gated_generic) {
      .build_unsupported_method(sec)
    } else if (identical(sec$tlf_type, "LISTING")) {
      .with_op_self_rels(.STANDARD_METHODS[["Listing"]])
    } else {
      .build_method(sec)
    }
    ## Methods are deliberately NOT registered by definition, unlike the
    ## groupings, analysis sets and data subsets above. Every id here comes
    ## from the .STANDARD_METHODS catalogue except .build_method()'s unknown-
    ## name fallback, and that fallback's whole object is a pure function of
    ## the name -- so two sections that mint one id necessarily built the same
    ## method. There is no definition for a signature to tell apart.
    if (!mth_obj$id %in% seen_mth) {
      methods[[length(methods) + 1L]] <- mth_obj
      seen_mth <- c(seen_mth, mth_obj$id)
    }

    ## --- One Analysis per annotated row; layout entry per authored row ---
    ## For a plain TABLE section every authored stub row is walked in order:
    ## annotated rows become analyses (method inferred from the annotation
    ## form when it is recognisable), label-only rows become layout entries
    ## with no analysis, and no annotated row is ever dropped (an
    ## unresolvable one is reserved as manual_pending). Gated sections and
    ## listings/figures keep the previous annotated-rows-only path.
    build_layout <- identical(sec$tlf_type %||% "TABLE", "TABLE") &&
      !sec_unsupported
    rows_iter <- if (build_layout) sec$stub_rows %||% list() else
      Filter(function(r) isTRUE(r$has_annot), sec$stub_rows)
    enriched_rows  <- sec$enriched_rows %||% list()

    shell_layout <- list()
    analysis_ids <- character()
    seen_row_sig <- character()
    ## Last categorical analysis emitted -- candidate parent for authored
    ## level rows that follow it (option A of the level-row model).
    cat_parent <- NULL

    ## Nested two-level token blocks (AE by SOC/PT and kin): detected up
    ## front over the whole row list, because the pattern is a property of
    ## the row SEQUENCE, not of any single row.
    nested_roles <- if (build_layout) {
      .detect_nested_token_blocks(rows_iter, enriched_rows)
    } else {
      rep(NA_character_, length(rows_iter))
    }
    ## Set when the nested parent analysis is emitted; the child's layout
    ## row points back at it via parent_order.
    nested_parent_ctx <- NULL

    ## Build a free-standing analysis for an annotation the ROW loop did not
    ## itself produce -- used for the two "nothing the supplement added is
    ## dropped" cases: a supplement proposal that conflicts with the shell's own
    ## annotation (built alongside it), and a supplement binding whose label
    ## matched no stub row (built as a labelled analysis on the output). Mirrors
    ## the primary row path's method precedence and dedup, updates the section
    ## accumulators in place, and never touches `cat_parent` (a free-standing
    ## analysis is never a level of the block above it). Returns invisibly.
    emit_extra_analysis <- function(label, annotation, indent = 0L,
                                    where = NULL, default_method_id = NULL) {
      annotation <- trimws(as.character(annotation %||% ""))
      refs <- if (nzchar(annotation)) extract_annotation_vars(annotation) else character()
      if (length(refs) == 0) return(invisible(NULL))
      pieces <- strsplit(refs[1], ".", fixed = TRUE)[[1]]
      er2 <- list(
        label            = label %||% "",
        primary_dataset  = pieces[1],
        primary_variable = if (length(pieces) >= 2) pieces[2] else "",
        variable_role    = "ANALYSIS"
      )
      ## The typed clause (when the caller carries one) is authoritative and
      ## never re-parsed from the annotation string -- the same precedence the
      ## primary row path applies. A compound expression rides as
      ## data_subset_compound, which .build_data_subset() already emits as a
      ## compoundExpression DataSubset (RC-2 of the render-fidelity handoff).
      carry2 <- .row_restriction(annotation, .spec_resolves, supplement = where)
      if (!is.null(carry2$unresolved)) {
        er2$unresolved_condition <- .unresolved_condition_text(carry2$unresolved)
      } else if (!is.null(carry2$compound)) {
        er2$data_subset_compound <- carry2$compound
      } else if (!is.null(carry2$flat)) {
        er2$data_subset <- carry2$flat
      }
      if (!nzchar(er2$primary_variable %||% "")) return(invisible(NULL))
      idx2    <- length(analysis_ids) + 1L
      ds_obj2 <- .build_data_subset(er2, sec$tlf_number, idx2)
      ## An annotation that declares a filter but yields no DataSubset would
      ## compute UNFILTERED -- for a categorical method that is the full value
      ## distribution deparsed into one cell. Surface it in the validation
      ## report instead of in a rendered cell.
      if (is.null(ds_obj2) &&
          grepl("\\bwhere\\b", annotation, ignore.case = TRUE)) {
        diag_add(
          stage = "build_ars", severity = "WARN",
          problem = sprintf(
            "Extra analysis '%s' declares a filter (%s) that could not be parsed into a DataSubset",
            label %||% "?", annotation),
          tlf_number = sec$tlf_number,
          action = "The analysis would compute unfiltered -- pass the typed whereClause through the supplement, or simplify the annotation"
        )
      }
      ds_obj2 <- register_data_subset(ds_obj2, sec$tlf_number)
      ## Method: annotation-inferred form, else the spec's categorical verdict,
      ## else the section method -- the same precedence the primary row uses.
      row2         <- list(label = label %||% "", annotation = annotation,
                           has_annot = TRUE)
      cat_verdict2 <- .var_is_categorical(er2$primary_dataset, er2$primary_variable)
      ## Same effective-filter rule as the primary row path: a free-standing
      ## analysis built from a typed supplement clause is typed from THAT
      ## clause, not from a re-reading of its display annotation.
      inferred2    <- .infer_row_method(row2, cat_verdict2,
                                        filter = .row_effective_filter(er2),
                                        filter_known = TRUE)
      method2_id   <- mth_obj$id
      ## A statistic this package cannot produce reserves the row, and is
      ## checked before the catalogue: there is no catalogue entry to consult.
      unsupported2 <- if (!is.null(inferred2)) inferred2$unsupported else NULL
      cand2        <- if (!is.null(inferred2) && is.null(unsupported2)) {
        .STANDARD_METHODS[[inferred2$method]]
      } else NULL
      if (!is.null(unsupported2)) {
        method2_id <- "MTH_UNSUPPORTED_ANALYSIS"
        if (!method2_id %in% seen_mth) {
          methods[[length(methods) + 1L]] <<- .build_unsupported_method(
            list(unsupported_reason = .UNSUPPORTED_ROW_REASON))
          seen_mth <<- c(seen_mth, method2_id)
        }
        diag_add(
          stage = "build_ars", severity = "WARN",
          problem = sprintf("Row '%s': %s", label %||% "?", unsupported2),
          tlf_number = sec$tlf_number,
          action = "Reserved as manual_pending rather than computed as a different statistic -- see ars_manual_worklist()"
        )
      } else if (!is.null(cand2)) {
        method2_id <- cand2$id
        if (!cand2$id %in% seen_mth) {
          methods[[length(methods) + 1L]] <<- .with_op_self_rels(cand2)
          seen_mth <<- c(seen_mth, cand2$id)
        }
      } else if (identical(mth_obj$id, "MTH_SUMMARY_STATISTICS_CONTINUOUS") &&
                 isTRUE(cat_verdict2)) {
        method2_id <- count_method_id
        if (!count_method_id %in% seen_mth) {
          methods[[length(methods) + 1L]] <<-
            .with_op_self_rels(.STANDARD_METHODS[["Count and Percentage"]])
          seen_mth <<- c(seen_mth, count_method_id)
        }
      }
      ## A conflict secondary is the SAME authored row under a different
      ## filter, so it inherits the winning row's counting discipline: a
      ## distinct-subject AE count must never degrade to record counting
      ## (records over a subject denominator renders p > 1). An explicit
      ## once/subject clause in the annotation does the same on its own.
      if (identical(method2_id, count_method_id) &&
            (identical(default_method_id, "MTH_AE_FREQUENCY_COUNT") ||
               !is.null(.once_per_subject_var(annotation)))) {
        method2_id <- "MTH_AE_FREQUENCY_COUNT"
        if (!method2_id %in% seen_mth) {
          methods[[length(methods) + 1L]] <<-
            .with_op_self_rels(.STANDARD_METHODS[["AE Frequency Count"]])
          seen_mth <<- c(seen_mth, method2_id)
        }
      }
      row_sig2 <- paste(method2_id,
                        toupper(er2$primary_dataset  %||% ""),
                        toupper(er2$primary_variable %||% ""),
                        if (!is.null(ds_obj2)) ds_obj2$id else "",
                        if (identical(method2_id, count_method_id)) ""
                        else trimws(label %||% ""),
                        sep = "|")
      if (row_sig2 %in% seen_row_sig) return(invisible(NULL))
      seen_row_sig <<- c(seen_row_sig, row_sig2)
      an2 <- .build_analysis(
        section = sec, row = row2, enrichment = er2, index = idx2,
        as_id = as_obj$id, gf_ids = gf_ids, method_id = method2_id,
        ds_id = if (!is.null(ds_obj2)) ds_obj2$id else NULL)
      analyses[[length(analyses) + 1L]]         <<- an2
      analysis_ids                              <<- c(analysis_ids, an2$id)
      note_value_decode(er2$primary_dataset, er2$primary_variable, method2_id)
      shell_layout[[length(shell_layout) + 1L]] <<- .with_sheet_row(list(
        order = length(shell_layout) + 1L,
        label = label %||% "", indent = indent,
        analysis_id = an2$id, kind = "supplement_added"), row2)
      invisible(an2$id)
    }

    for (ridx in seq_along(rows_iter)) {
      row <- rows_iter[[ridx]]
      raw <- as.character(row$raw_text %||% "")
      indent <- nchar(regmatches(raw, regexpr("^ *", raw))[[1]] %||% "")

      ## Mock/template rows are resolved BEFORE the label-only branch: the
      ## numbered dialect leaves its repeats un-annotated ("PT#2", "PT#n",
      ## "SOC#2"), and a bare "..." continuation carries no annotation
      ## either. Treating those as authored label rows would print the
      ## placeholder text as if it were a real row.
      nested_role <- nested_roles[[ridx]]

      if (!isTRUE(row$has_annot) &&
            !nested_role %in% c("level_repeat", "nested_repeat")) {
        ## Authored label-only row (section header / spacer): persisted in
        ## the layout so the renderer keeps it, but it has no analysis.
        ## A spacer also ends any categorical block above it.
        cat_parent <- NULL
        label_entry <- .with_sheet_row(list(
          order = length(shell_layout) + 1L,
          label = row$label %||% "", indent = indent,
          analysis_id = NA_character_, kind = "label"), row)
        ## A REVIEWED supplement may say what a statistic row means when the
        ## label grammar could not read it. Carried on the layout entry
        ## because that is the only record of this row the fill stage sees.
        ## Only TOKENS travel -- what the row asks for. Whether the row's
        ## method can provide them is decided at fill time, by the method,
        ## exactly as it is for a label the grammar did read.
        if (length(row$supplement_stat_tokens %||% character()) > 0) {
          label_entry$stat_tokens <- as.character(row$supplement_stat_tokens)
          label_entry$stat_tokens_source <-
            as.character(row$supplement_stat_source %||% "supplement")
          label_entry$stat_tokens_override <-
            isTRUE(row$supplement_stat_override)
          ## Declared scope travels with the tokens, and only as a claim to be
          ## checked: the method decides what the row's scope IS.
          if (nzchar(row$supplement_stat_scope %||% "")) {
            label_entry$stat_expected_scope <-
              as.character(row$supplement_stat_scope)
          }
        }
        shell_layout[[length(shell_layout) + 1L]] <- label_entry
        next
      }

      if (identical(nested_role, "level_repeat")) {
        ## A mock of the levels the row ABOVE already expands ("<Reason
        ## #2>", "PT#n", a trailing "..."): emitted nowhere, or the shell's
        ## placeholder text would render as if it were a real level. Its
        ## sheet row is recorded on the parent's layout entry, so the fill
        ## step knows which rows the block owns when it expands the levels.
        mock_row <- suppressWarnings(as.integer(row$sheet_row %||% NA_integer_))
        if (build_layout && !is.na(mock_row) && !is.null(cat_parent) &&
              !is.null(cat_parent$layout_order)) {
          parent_entry <- shell_layout[[cat_parent$layout_order]]
          if (mock_row > (parent_entry$sheet_row %||% 0L)) {
            parent_entry$template_rows <- c(parent_entry$template_rows,
                                            mock_row)
            shell_layout[[cat_parent$layout_order]] <- parent_entry
          }
        }
        diag_add(
          stage = "build_ars", severity = "INFO",
          problem = sprintf(
            "Row '%s' illustrates the levels of the row above it -- collapsed into that analysis",
            row$label %||% "?"),
          tlf_number = sec$tlf_number,
          action = "The categorical analysis above expands every observed level; the row is kept as the block's expansion template"
        )
        next
      }
      if (identical(nested_role, "nested_repeat")) {
        ## A further mock example of an already-captured nested block
        ## (another "<Preferred Term>" row, the repeated "<System Organ
        ## Class>" block): the first parent/child pair carries the
        ## analyses, so these rows are emitted nowhere.
        diag_add(
          stage = "build_ars", severity = "INFO",
          problem = sprintf(
            "Row '%s' repeats the nested block's template -- collapsed into the first parent/child pair",
            row$label %||% "?"),
          tlf_number = sec$tlf_number,
          action = "Nested example blocks expand once; the renderer repeats them per data level"
        )
        next
      }

      idx <- length(analysis_ids) + 1L
      ## Was: a lookup keyed on row$label, returning whichever enrichment came
      ## first under a shared label. Two rows may legitimately carry the same
      ## visible text, and the second then inherited the first's variable and
      ## filter. .enrichment_for_row() lets the label assist the match but never
      ## outrank the row's own declared source, and yields nothing rather than a
      ## guess -- the block below then builds the row from its own annotation.
      er  <- .enrichment_for_row(row, enriched_rows)
      if (is.null(er)) {
        ## Two outcomes hide behind one unresolved pairing, and they are not
        ## equally serious.
        ##
        ## Dataset, variable and subset are all restated by the row's own
        ## annotation, so for those the fallback below is the correct answer
        ## rather than a degraded one: nothing is lost and a WARN says so.
        ##
        ## A non-default variable_role is different in kind. It reaches the ARS
        ## as variableRole, no annotation can restate it, and dropping it
        ## silently rewrites the row as an ordinary ANALYSIS -- a semantics this
        ## row never claimed. arsbridge does not know which role was meant, so
        ## it must not pick one. The role is recorded on the analysis and
        ## validation then refuses to execute the event.
        shared <- Filter(function(e) identical(e$label %||% "",
                                               row$label %||% ""),
                         enriched_rows)
        unresolved_role <- .enrichment_unrecoverable(shared)
        if (length(shared) > 0) {
          diag_add(
            stage = "build_ars",
            severity = if (length(unresolved_role)) "FAIL" else "WARN",
            problem = sprintf(
              "Could not decide which enrichment belongs to row '%s'",
              row$label %||% "?"),
            location = row$annotation %||% "",
            tlf_number = sec$tlf_number,
            action = if (length(unresolved_role)) sprintf(
              paste("A proposed variable role (%s) cannot be rebuilt from the",
                    "annotation, and which role is correct is exactly what",
                    "could not be decided. The analysis is marked unresolved",
                    "and the event will not execute until the role is settled"),
              paste(unresolved_role, collapse = ", ")
            ) else paste("Built from the row's own annotation, which is",
                         "authoritative for its dataset, variable and filter"))
        }
        er <- list()
        if (length(unresolved_role)) {
          er$unresolved_variable_role <- unresolved_role
        }
      }
      ## Deterministic safety net: when the LLM enrichment omitted this row,
      ## derive dataset/variable and the subset filter straight from the
      ## bound annotation so the authored row still computes.
      if (!nzchar(er$primary_variable %||% "")) {
        refs <- extract_annotation_vars(row$annotation)
        if (length(refs) > 0) {
          pieces <- strsplit(refs[1], ".", fixed = TRUE)[[1]]
          er$label            <- er$label %||% row$label
          er$primary_dataset  <- pieces[1]
          er$primary_variable <- if (length(pieces) >= 2) pieces[2] else ""
          er$variable_role    <- er$variable_role %||% "ANALYSIS"
        }
      }
      ## Recorded for every row that carries one, and deliberately OUTSIDE the
      ## branch below: whether some other channel already supplied a subset has
      ## no bearing on the fact that this row states a relationship the
      ## deterministic reader did not apply as a filter. Reporting it only when
      ## nothing else had spoken would make the diagnostic depend on an
      ## unrelated condition, and a reader who meant it as a restriction would
      ## be told on some rows and not others.
      deriv_note <- .split_derivation_note(row$annotation)$note
      if (nzchar(deriv_note)) {
        diag_add(
          stage = "build_ars", severity = "INFO", input = INPUT_SHELL,
          problem = sprintf(
            "Row '%s' states '%s', read as a derivation note rather than a row filter.",
            row$label %||% "?", deriv_note),
          location = row$annotation %||% "",
          tlf_number = sec$tlf_number,
          action = paste("The row is summarised by its variable's own type.",
                         "If this was meant to RESTRICT the rows, state the",
                         "condition against a value (e.g. 'ADQX.FLAG=Y')."))
      }
      if (is.null(er$data_subset) || length(er$data_subset) == 0) {
        ## ONE reading, whatever supplies the filter. A typed supplement row
        ## filter (v3) is authoritative and never re-parsed from a string, and
        ## `.row_restriction()` routes it -- but it asks the annotation about
        ## an unrepresented instruction either way, because a filter being
        ## authoritative says nothing about a rule concerning records no
        ## filter can reach.
        carry <- .row_restriction(row$annotation, .spec_resolves,
                                  supplement = row$supplement_where)
        if (!is.null(carry$unresolved)) {
          ## The row states a filter that could not be read, or a rule this
          ## version cannot carry out. It gets NO data subset -- there is
          ## nothing valid to give it -- and carries the marker instead, which
          ## `.build_analysis()` writes onto the Analysis and
          ## `.check_unresolved_condition()` turns into a GAP, so the
          ## reservation map withholds this analysis rather than letting it
          ## compute over every record.
          er$unresolved_condition <- .unresolved_condition_text(carry$unresolved)
        } else if (!is.null(carry$compound)) {
          er$data_subset_compound <- carry$compound
        } else if (!is.null(carry$flat)) {
          er$data_subset <- carry$flat
        }
      }

      ## Authored LEVEL row of the categorical block above it: the row's
      ## subset filters the parent's own variable ("18-64" under
      ## "Age group, n (%)" -> AGEGR1='18-64'). The parent analysis already
      ## computes every level once, so this row gets NO analysis of its own;
      ## it becomes a level slot the renderer fills from the parent's
      ## computed levels -- authored label and order win, values come from
      ## the single parent computation.
      ## NB: exact [[ ]] indexing -- with only data_subset_compound set,
      ## er$data_subset would PARTIAL-MATCH the compound and defeat both
      ## checks below silently.
      fs_child <- er[["data_subset"]]
      ## A compound leaf hides its level condition inside an AND (the
      ## supplement restates the section's record filter on every row, e.g.
      ## AND(TRTEMFL='Y', ASEV='MILD')). Pull out the single EQ term on the
      ## parent's own variable so clause SHAPE never decides whether an
      ## authored row is a level (RC-1 of the render-fidelity handoff).
      if (is.null(fs_child) && !is.null(cat_parent) &&
          !is.null(er$data_subset_compound)) {
        fs_child <- .where_leaf_on(er$data_subset_compound, cat_parent$var)
      }
      if (build_layout && !is.null(cat_parent) && !is.null(fs_child) &&
          identical(toupper(fs_child$variable %||% ""), cat_parent$var)) {
        lv <- fs_child$value %||% list()
        lv <- if (length(lv) > 0) as.character(lv[[1]]) else ""
        ## When the parent variable ships a codelist decode, its computed
        ## levels are decoded labels -- translate the authored coded value so
        ## the renderer's level matching sees the same vocabulary. The raw
        ## code rides along as level_code for traceability.
        lv_display <- lv
        decode_terms <- .decode_terms_for(cat_parent$ds, cat_parent$var)
        if (!is.null(decode_terms) && nzchar(lv)) {
          hit <- match(lv, as.character(decode_terms$term))
          if (!is.na(hit)) lv_display <- decode_terms$decode[hit]
        }
        shell_layout[[length(shell_layout) + 1L]] <- .with_sheet_row(list(
          order = length(shell_layout) + 1L,
          label = row$label %||% "", indent = indent,
          analysis_id = cat_parent$aid, kind = "level", level = lv_display,
          level_code = lv), row)
        diag_add(
          stage = "build_ars", severity = "INFO",
          problem = sprintf("Row '%s' is a level of the categorical block above (%s='%s')",
                            row$label %||% "?", cat_parent$var, lv),
          tlf_number = sec$tlf_number,
          action = "Rendered from the parent analysis's computed levels -- no duplicate analysis emitted"
        )
        next
      }

      ds_obj <- register_data_subset(
        .build_data_subset(er, sec$tlf_number, idx), sec$tlf_number
      )

      row_method_id <- mth_obj$id
      row_kind      <- "row"
      cat_verdict   <- .var_is_categorical(er$primary_dataset,
                                           er$primary_variable)
      ## Annotation-form inference applies to TABLE rows only: a listing
      ## column annotated "ADAE.AEDECOD" is a passthrough column, never a
      ## count analysis.
      ##
      ## The EFFECTIVE filter is handed in rather than re-derived from the
      ## annotation string. By this point the row's restriction has already
      ## been settled once -- from a typed supplement clause, from the
      ## annotation, or not at all -- and reading the string a second time can
      ## reach a different answer than the one the row will compute under. That
      ## is how a row filtered through the supplement could still be typed from
      ## a shell annotation the grammar could not read.
      inferred <- if (build_layout) {
        .infer_row_method(row, cat_verdict, filter = .row_effective_filter(er),
                          filter_known = TRUE)
      } else {
        NULL
      }

      if (build_layout &&
          !nzchar(er$primary_variable %||% "") &&
          !nzchar(er$primary_dataset %||% "")) {
        ## Annotated row whose variable never resolved (ADR 0003 no-drop):
        ## reserve a traceable manual_pending cell instead of dropping the
        ## row. ADSL.USUBJID keys the stub so the engine's dataset/variable
        ## guards pass.
        er$primary_dataset  <- "ADSL"
        er$primary_variable <- "USUBJID"
        row_method_id <- "MTH_UNSUPPORTED_ANALYSIS"
        row_kind      <- "manual"
        if (!"MTH_UNSUPPORTED_ANALYSIS" %in% seen_mth) {
          methods[[length(methods) + 1L]] <- .build_unsupported_method(sec)
          seen_mth <- c(seen_mth, "MTH_UNSUPPORTED_ANALYSIS")
        }
        diag_add(
          stage = "build_ars", severity = "WARN",
          problem = sprintf("Annotated row '%s' has no resolvable variable",
                            row$label %||% "?"),
          tlf_number = sec$tlf_number,
          action = "Reserved as manual_pending so the authored row is kept -- see ars_manual_worklist()"
        )
      } else if (!is.null(inferred) && !is.null(inferred$unsupported)) {
        ## The row states a statistic this package cannot produce. Reserving it
        ## is the honest outcome: computing a different statistic into the same
        ## cell yields a number that formats, renders, and answers a question
        ## the author did not ask.
        row_method_id <- "MTH_UNSUPPORTED_ANALYSIS"
        row_kind      <- "manual"
        if (!"MTH_UNSUPPORTED_ANALYSIS" %in% seen_mth) {
          methods[[length(methods) + 1L]] <- .build_unsupported_method(
            list(unsupported_reason = .UNSUPPORTED_ROW_REASON))
          seen_mth <- c(seen_mth, "MTH_UNSUPPORTED_ANALYSIS")
        }
        diag_add(
          stage = "build_ars", severity = "WARN",
          problem = sprintf("Row '%s': %s", row$label %||% "?",
                            inferred$unsupported),
          tlf_number = sec$tlf_number,
          action = "Reserved as manual_pending rather than computed as a different statistic -- see ars_manual_worklist()"
        )
      } else if (!is.null(inferred)) {
        ## Deterministic method from the annotation form -- overrides the
        ## section-level (LLM) method for this row.
        row_kind <- inferred$kind
        cand <- .STANDARD_METHODS[[inferred$method]]
        if (!is.null(cand)) {
          if (!cand$id %in% seen_mth) {
            methods[[length(methods) + 1L]] <- .with_op_self_rels(cand)
            seen_mth <- c(seen_mth, cand$id)
          }
          if (!identical(cand$id, mth_obj$id)) {
            diag_add(
              stage = "build_ars", severity = "INFO",
              problem = sprintf("Row '%s': annotation form implies %s (section method was %s)",
                                row$label %||% "?", cand$id, mth_obj$id),
              tlf_number = sec$tlf_number,
              action = "Used the annotation-inferred method for this row"
            )
          }
          row_method_id <- cand$id
        }
      } else if (!gated_generic &&
                 identical(mth_obj$id, "MTH_SUMMARY_STATISTICS_CONTINUOUS") &&
                 isTRUE(cat_verdict)) {
        ## Per-row method correction: a section classified as continuous may
        ## still contain categorical rows (e.g. SEX, RACE in a demographics
        ## table). Summarising those with continuous stats yields NaN -- so when
        ## the ADaM spec marks the row variable as categorical, route it to the
        ## count-and-percentage method instead.
        row_method_id <- count_method_id
        if (!count_method_id %in% seen_mth) {
          methods[[length(methods) + 1L]] <-
            .with_op_self_rels(.STANDARD_METHODS[["Count and Percentage"]])
          seen_mth <- c(seen_mth, count_method_id)
        }
        diag_add(
          stage = "build_ars", severity = "INFO",
          problem = sprintf("Row variable %s is categorical but its TLF was classified continuous",
                            er$primary_variable %||% row$label %||% "?"),
          tlf_number = sec$tlf_number,
          action = "Routed this row to Count and Percentage instead of continuous summary"
        )
      }

      ## Supplement-specified per-row method (MIXED_SUMMARY, per-row methodId):
      ## honoured for a supplement-bound row, or for any row under
      ## prefer_supplement. It never overrides a shell-annotated row's inferred
      ## method in fill_gaps mode (such a row's detection_method is not
      ## "supplement"), keeping deterministic ground truth highest there.
      supp_mid <- row$supplement_method_id
      if (!is.null(supp_mid) && nzchar(supp_mid %||% "") &&
          (identical(row$detection_method, "supplement") ||
           identical(supplement_trust, "prefer_supplement"))) {
        supp_nm   <- .method_name_from_id(supp_mid)
        supp_cand <- if (!is.null(supp_nm)) .STANDARD_METHODS[[supp_nm]] else NULL
        if (!is.null(supp_cand)) {
          row_method_id <- supp_cand$id
          if (!supp_cand$id %in% seen_mth) {
            methods[[length(methods) + 1L]] <- .with_op_self_rels(supp_cand)
            seen_mth <- c(seen_mth, supp_cand$id)
          }
        }
      }

      ## "once/subject VAR" annotation clause: the shell says this count row
      ## counts each subject once (AE first-occurrence flags). A plain
      ## count-and-percentage would count RECORDS, so route it to the
      ## distinct-subject AE frequency method.
      once_var <- if (build_layout) .once_per_subject_var(row$annotation)
                  else NULL
      if (!is.null(once_var) && identical(row_method_id, count_method_id)) {
        row_method_id <- "MTH_AE_FREQUENCY_COUNT"
        if (!row_method_id %in% seen_mth) {
          methods[[length(methods) + 1L]] <-
            .with_op_self_rels(.STANDARD_METHODS[["AE Frequency Count"]])
          seen_mth <- c(seen_mth, row_method_id)
        }
        diag_add(
          stage = "build_ars", severity = "INFO",
          problem = sprintf(
            "Row '%s' declares once/subject (%s) -- counted as distinct subjects, not records",
            row$label %||% "?", once_var),
          tlf_number = sec$tlf_number,
          action = "Routed to AE Frequency Count (distinct subject per term)"
        )
      }

      ## Nested two-level block: both levels count DISTINCT subjects per
      ## term, and the child analysis carries the parent's variable as a
      ## data-driven row grouping so the SOC->PT hierarchy survives into the
      ## ARS (HANDOFF_nested_soc_pt_hierarchy, Phase N1). The engine already
      ## turns every grouping into a {cards} `by` variable, so the child's
      ## ARD comes back keyed by (arm, parent level, child level).
      row_grouping_ids <- character(0)
      if (identical(nested_role, "nested_parent") ||
            identical(nested_role, "nested_child")) {
        row_method_id <- "MTH_AE_FREQUENCY_COUNT"
        if (!row_method_id %in% seen_mth) {
          methods[[length(methods) + 1L]] <-
            .with_op_self_rels(.STANDARD_METHODS[["AE Frequency Count"]])
          seen_mth <- c(seen_mth, row_method_id)
        }
        row_kind <- nested_role
        if (identical(nested_role, "nested_child") &&
              !is.null(nested_parent_ctx)) {
          ## Through the same registrar as the column axes: a data-driven row
          ## grouping enumerates no groups, so it is never the same definition
          ## as a condition-defined column axis on that variable, and the two
          ## can no longer absorb each other.
          rg <- register_grouping(
            .build_row_grouping(nested_parent_ctx$var, nested_parent_ctx$ds),
            tlf_number = sec$tlf_number)
          row_grouping_ids <- rg$id
          diag_add(
            stage = "build_ars", severity = "INFO",
            problem = sprintf(
              "Rows '%s' / '%s' form a nested block: %s levels expand under each %s level",
              nested_parent_ctx$label, row$label %||% "?",
              er$primary_variable %||% "?", nested_parent_ctx$var),
            tlf_number = sec$tlf_number,
            action = sprintf(
              "The %s analysis carries %s as a data-driven row grouping (%s)",
              er$primary_variable %||% "?", nested_parent_ctx$var, rg$id)
          )
        }
      }

      ## Duplicate-template dedup (nested AE shells author example blocks:
      ## "<Preferred Term>" placeholder rows plus repeated "Preferred Term"
      ## mock rows all annotated AEDECOD). A Count-and-Percentage row expands
      ## the FULL categorical distribution of its variable, so two such rows on
      ## the same variable + subset would draw the same distribution twice and
      ## collide in the renderer -- those collapse on method + variable + subset
      ## regardless of their (placeholder) labels.
      ##
      ## Every OTHER row (a continuous summary, a subject count, ...) is one
      ## authored line, so two of them that happen to share a variable are still
      ## distinct analyses -- their LABEL joins the signature so a genuinely
      ## distinct row (e.g. "Age (years)" and "Age (years) - total") is never
      ## silently dropped from the ARD.
      is_cat_distribution <- identical(row_method_id, count_method_id)
      row_sig <- paste(row_method_id,
                       toupper(er$primary_dataset  %||% ""),
                       toupper(er$primary_variable %||% ""),
                       if (!is.null(ds_obj)) ds_obj$id else "",
                       if (is_cat_distribution) "" else trimws(row$label %||% ""),
                       sep = "|")
      if (build_layout && row_sig %in% seen_row_sig) {
        diag_add(
          stage = "build_ars", severity = "INFO",
          problem = sprintf("Row '%s' duplicates an earlier row's analysis (%s); collapsed",
                            row$label %||% "?", row_sig),
          tlf_number = sec$tlf_number,
          action = "Template/example rows expand once -- the first matching row carries the analysis"
        )
        next
      }
      seen_row_sig <- c(seen_row_sig, row_sig)

      an_obj <- .build_analysis(
        section = sec, row = row, enrichment = er,
        index = idx, as_id = as_obj$id,
        gf_ids = gf_ids,
        method_id = row_method_id,
        ds_id = if (!is.null(ds_obj)) ds_obj$id else NULL,
        row_grouping_ids = row_grouping_ids
      )
      analyses[[length(analyses) + 1L]] <- an_obj
      analysis_ids <- c(analysis_ids, an_obj$id)
      note_value_decode(er$primary_dataset, er$primary_variable, row_method_id)
      layout_kind <- if (identical(row_method_id, "MTH_COUNT_AND_PERCENTAGE") &&
                           identical(row_kind, "row")) "categorical"
                     else if (identical(row_method_id, "MTH_SUMMARY_STATISTICS_CONTINUOUS") &&
                                identical(row_kind, "row")) "continuous"
                     else row_kind
      layout_entry <- .with_sheet_row(list(
        order = length(shell_layout) + 1L,
        label = row$label %||% "", indent = indent,
        analysis_id = an_obj$id,
        kind = layout_kind), row)
      if (identical(nested_role, "nested_child") &&
            !is.null(nested_parent_ctx)) {
        layout_entry$parent_order <- nested_parent_ctx$order
      }
      ## A self-template block's annotated mock row: the analysis lives on
      ## the mock itself, so the mock's own row opens the template. The
      ## run's bare repeats add theirs through the level_repeat branch, via
      ## cat_parent below. Only a categorical block expands one row per
      ## level -- a token row routed to any other method keeps its analysis
      ## un-flagged.
      if (identical(nested_role, "self_template") &&
            identical(layout_kind, "categorical")) {
        layout_entry$self_template <- TRUE
        own_row <- suppressWarnings(as.integer(row$sheet_row %||%
                                                 NA_integer_))
        if (!is.na(own_row)) {
          layout_entry$template_rows <- own_row
        }
        diag_add(
          stage = "build_ars", severity = "INFO",
          problem = sprintf(
            "Row '%s' is a self-template categorical block -- its label stands for the variable's levels",
            row$label %||% "?"),
          tlf_number = sec$tlf_number,
          action = "One analysis draws the distribution; the block expands one row per level at fill time"
        )
      }
      ## An authored "sort: ..." clause travels on the layout entry -- from
      ## a nested parent token row, or from any categorical row (whose mock
      ## block below may expand). Without one, nested blocks default to
      ## descending frequency and categorical blocks to codelist order.
      if (identical(nested_role, "nested_parent") ||
            identical(layout_kind, "categorical")) {
        sort_clause <- .nested_sort_clause(row$annotation)
        if (!is.null(sort_clause)) {
          if (is.na(sort_clause$basis)) {
            diag_add(
              stage = "build_ars", severity = "WARN",
              problem = sprintf(
                "Row '%s' carries a sort clause I can't read: 'sort: %s'",
                row$label %||% "?", sort_clause$raw),
              tlf_number = sec$tlf_number,
              action = "Keeping the default order (descending frequency, parent then child)"
            )
          } else {
            layout_entry$sort <- if (is.null(sort_clause$column)) {
              sort_clause$basis
            } else {
              paste0(sort_clause$basis, ":", sort_clause$column)
            }
            diag_add(
              stage = "build_ars", severity = "INFO",
              problem = sprintf(
                "Row '%s' declares its own sort order (sort: %s)",
                row$label %||% "?", sort_clause$raw),
              tlf_number = sec$tlf_number,
              action = sprintf("The block sorts by %s", layout_entry$sort)
            )
          }
        }
      }
      shell_layout[[length(shell_layout) + 1L]] <- layout_entry
      if (identical(nested_role, "nested_parent")) {
        nested_parent_ctx <- list(
          var   = toupper(er$primary_variable %||% ""),
          ds    = toupper(er$primary_dataset %||% ""),
          label = row$label %||% "",
          order = layout_entry$order
        )
      }
      ## This analysis becomes (or clears) the candidate parent for authored
      ## level rows that follow.
      cat_parent <- if (identical(layout_kind, "categorical") &&
                          nzchar(er$primary_variable %||% "")) {
        list(var = toupper(er$primary_variable),
             ds  = toupper(er$primary_dataset %||% ""),
             aid = an_obj$id,
             ## Where this entry sits in shell_layout (order == index), so a
             ## collapsed mock row below can record itself on the entry.
             layout_order = layout_entry$order)
      } else {
        NULL
      }

      ## The losing side of a row conflict is built as its OWN analysis
      ## alongside the winner, so nothing either side contributed is dropped.
      ## Symmetric across trust modes: in fill_gaps the supplement's differing
      ## proposal is the secondary; in prefer_supplement the shell's overridden
      ## original is. Both arrive on the row as `secondary_annotation` (the
      ## older `supplement_proposed_annotation` name is still honoured).
      extra_ann <- trimws(as.character(
        row$secondary_annotation %||% row$supplement_proposed_annotation %||% ""))
      if (build_layout && nzchar(extra_ann)) {
        extra_id <- emit_extra_analysis(row$label %||% "", extra_ann, indent,
                                        where = row$secondary_where,
                                        default_method_id = row_method_id)
        if (!is.null(extra_id)) {
          diag_add(
            stage = "build_ars", severity = "INFO",
            problem = sprintf(
              "Row '%s': the conflicting proposal (%s) was added as an additional analysis; the winning annotation was kept too",
              row$label %||% "?", extra_ann),
            tlf_number = sec$tlf_number,
            action = "Both sides of the conflict are computed -- nothing either side contributed is dropped"
          )
        }
      }
    }

    ## Supplement bindings whose label matched no stub row: the shell never
    ## authored a place for them, but they passed the ADaM-spec gate, so build
    ## each as a free-standing, labelled analysis on this output rather than
    ## dropping it. Only for plain tables -- listings/figures/gated sections do
    ## not summarise per-row analyses.
    if (build_layout) {
      for (extra in sec$supplement_extra_rows %||% list()) {
        extra_id <- emit_extra_analysis(extra$label %||% "", extra$annotation %||% "",
                                        where = extra$where)
        if (!is.null(extra_id)) {
          diag_add(
            stage = "build_ars", severity = "INFO",
            problem = sprintf(
              "Supplement binding '%s' (%s) matched no stub row; added as a free-standing analysis on TLF %s",
              extra$label %||% "?", extra$annotation %||% "", sec$tlf_number),
            tlf_number = sec$tlf_number,
            action = "The supplement named an analysis the shell has no row for -- verify its label against the shell"
          )
        }
      }
    }
    ## The layout is dropped for output kinds that have no stub axis at all: a
    ## listing's rows are records and a figure has none, so a row-to-analysis
    ## map would describe nothing.
    ##
    ## A capability-gated TABLE used to be caught by the same test, because the
    ## flag above is false for it too. It is a different case: still a table,
    ## its rows still sit on sheet rows, and its analyses are reserved rather
    ## than absent. Discarding its layout left the fill unable to bind them, so
    ## every cell reported "no analysis covers this row" while the event held
    ## reserved results for those very analyses -- the table came back blank
    ## instead of visibly reserved, which reads as "not run yet".
    ##
    ## The flag stays in the test so that a future reason to skip building a
    ## layout still takes effect for the kinds that have no stub.
    if (!build_layout && !identical(sec$tlf_type %||% "TABLE", "TABLE")) {
      shell_layout <- list()
    }

    ## Executable inferential methods (ADR 0001): one analysis each on the
    ## section's primary response variable, carrying any operand (e.g. CMH
    ## strata). These compute through the arsbridge engine.
    if (has_exec) {
      resp_er  <- .section_primary_enrichment(sec)
      resp_row <- list(label = sec$title %||% sec$tlf_number,
                       annotation = "", has_annot = TRUE)
      for (k in seq_along(cls$executable)) {
        em <- cls$executable[[k]]
        if (!em$method_id %in% seen_mth) {
          methods[[length(methods) + 1L]] <- .build_exec_method(em$method_id)
          seen_mth <- c(seen_mth, em$method_id)
        }
        an <- .build_analysis(
          section = sec, row = resp_row, enrichment = resp_er,
          index = length(analysis_ids) + 1L, as_id = as_obj$id,
          gf_ids = gf_ids, method_id = em$method_id, ds_id = NULL)
        if (!is.null(em$strata)) an$strata <- em$strata
        analyses[[length(analyses) + 1L]] <- an
        analysis_ids <- c(analysis_ids, an$id)
      }
    }

    ## Residual reserve: a generic manual_pending cell for indicators that are
    ## still not computable (e.g. a Newcombe difference), so they appear marked.
    if (!gated_generic && sec_unsupported && length(cls$residual) > 0) {
      if (!"MTH_UNSUPPORTED_ANALYSIS" %in% seen_mth) {
        methods[[length(methods) + 1L]] <- .build_unsupported_method(sec)
        seen_mth <- c(seen_mth, "MTH_UNSUPPORTED_ANALYSIS")
      }
      an <- .build_analysis(
        section = sec, row = list(label = "Manual", annotation = "",
                                  has_annot = TRUE),
        enrichment = .section_primary_enrichment(sec),
        index = length(analysis_ids) + 1L, as_id = as_obj$id,
        gf_ids = gf_ids, method_id = "MTH_UNSUPPORTED_ANALYSIS", ds_id = NULL)
      analyses[[length(analyses) + 1L]] <- an
      analysis_ids <- c(analysis_ids, an$id)
    }

    ## The cell map, for a shell that came from a workbook: which worksheet
    ## cell each computed number belongs in. Built HERE because this is the
    ## only point at which the shell's geometry and the analyses are both in
    ## hand (see R/shell_fill_meta.R and ADR 0005).
    shell_fill <- .build_shell_fill(sec, shell_layout, analyses, methods)

    outputs[[length(outputs) + 1L]] <-
      .build_output(sec, analysis_ids, ship_annotations = ship_annotations,
                    shell_layout = shell_layout,
                    result_group_paths = result_group_paths,
                    shell_fill = shell_fill)
  }

  ## Siera iterates `seq_len(nrow(JSON_DataSubsets))` and
  ## `seq_len(nrow(JSON_AG_1))` without guarding for empty arrays
  ## (metadata.R:128, 194). Emit a placeholder no-op entry when either
  ## list is empty so siera doesn't crash on `nrow(NULL)`.
  if (length(data_subsets) == 0L) {
    data_subsets <- list(.default_data_subset())
    diag_add(
      stage = "build_ars", severity = "INFO",
      problem = "No DataSubsets derived from any annotation",
      action = "Emitted a placeholder no-op DataSubset for siera compatibility"
    )
  }
  if (length(grouping_factors) == 0L) {
    grouping_factors <- list(.default_grouping())
    diag_add(
      stage = "build_ars", severity = "WARN",
      problem = "No grouping variable was derived for any TLF",
      action = "Emitted placeholder TRT01A grouping -- verify the study's treatment/grouping variable"
    )
  }

  ## The standard requires reason and purpose on every analysis, as
  ## controlled-terminology objects. One run-level default is stamped here --
  ## at the assembly point, so every extraction path (LLM, supplement,
  ## deterministic) gets it -- and the reviewer adjusts the exceptions (e.g.
  ## the primary-endpoint table) per line in edit_ars().
  analyses <- lapply(analyses, function(an) {
    an$reason  <- list(controlledTerm = analysis_reason)
    an$purpose <- list(controlledTerm = analysis_purpose)
    an
  })

  list(
    id                    = study_id,
    name                  = study_name %||% study_id,
    version               = 1L,
    ## siera-required tables of contents (formerly only listOfPlannedAnalyses)
    otherListsOfContents  = .build_lopo(outputs),
    mainListOfContents    = .build_lopa(outputs),
    analysisSets          = analysis_sets,
    dataSubsets           = data_subsets,
    analysisGroupings     = grouping_factors,
    methods               = methods,
    analyses              = analyses,
    outputs               = outputs,
    `_meta` = list(
      generator             = paste0("arsbridge ", utils::packageVersion("arsbridge")),
      generated_at_utc      = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      ars_model_version     = "1.0",
      ## Which tier produced the semantic metadata: "llm" (live API),
      ## "supplement" (chat-assistant supplement file), or "deterministic"
      ## (regex + keyword heuristics only, reduced accuracy).
      extraction_mode       = extraction_mode,
      ## How supplement values resolved against the regex ("fill_gaps" or
      ## "prefer_supplement"); NULL/absent when no supplement was used.
      supplement_trust      = supplement_trust,
      ## Spec-codelist decodes for categorical analysis variables, keyed
      ## "DATASET.VARIABLE" -> ordered {value, label, order} lists. The
      ## engine / emitted code derive a decoded factor from these so the ARD
      ## shows "DEATH", not "1" (resolve_analysis() reads them back).
      value_decodes         = value_decodes,
      requires_human_review = TRUE,
      ## TLFs where no grouping variable could be resolved (built
      ## ungrouped) -- start the human review here.
      sections_needing_review = as.list(vapply(
        Filter(function(s) isTRUE(s$needs_review), sections),
        function(s) s$tlf_number %||% "", character(1)
      )),
      ## Outputs arsbridge cannot generate (inferential / model-based). The
      ## renderer emits a numbered placeholder for each; the programmer
      ## produces them manually.
      unsupported_outputs = unsupported
    )
  )
}


## --- Per-object builders --------------------------------------------------

.build_analysis_set <- function(sec) {
  pop_text  <- sec$population_text %||% "All Subjects"
  pop_annot <- trimws(sec$population_annot %||% "")
  ## A typed supplement population (v3) is already an ARS WhereClause, so it is
  ## used directly; otherwise parse the shell/supplement annotation string.
  cond      <- sec$population_where %||% parse_where_clause(pop_annot)
  obj <- list(
    id    = make_analysis_set_id(pop_text),
    name  = pop_text,
    label = pop_text
  )
  if (.is_unresolved_condition(cond)) {
    ## A population WAS written and could not be read. The text is kept, as it
    ## always was, and the set is additionally MARKED so validation reserves
    ## the analyses that use it.
    ##
    ## Marking matters more here than anywhere else in the builder: without it
    ## the set carries no condition, every analysis pointing at it computes
    ## over the whole dataset, and each percentage lands over the wrong N while
    ## looking entirely ordinary.
    obj$annotationText <- pop_annot
    obj$unresolvedCondition <- .unresolved_condition_text(cond)
    diag_add(
      stage = "build_ars", severity = "WARN", tlf_number = sec$tlf_number,
      location = sec$title %||% "",
      problem = sprintf(
        "Population filter '%s' did not parse into an ARS WhereClause; carried as annotationText",
        pop_annot),
      action = "Results using this population are reserved until the filter is expressible -- review the population annotation"
    )
  } else if (!is.null(cond)) {
    obj <- modifyList(obj, cond)
  } else if (nzchar(pop_annot)) {
    ## Non-empty, but the parser reports no condition ATTEMPT -- prose like
    ## "Safety Population", or a bare variable pointer. Unchanged behaviour:
    ## the text is kept so the renderer and QA can see the intent, and the
    ## analyses run on the full population.
    ##
    ## Deliberately NOT marked unresolved. Nothing failed to parse here; the
    ## annotation simply expresses no filter, and reserving on it would
    ## withhold results for every population named in words.
    obj$annotationText <- pop_annot
    diag_add(
      stage = "build_ars", severity = "WARN", tlf_number = sec$tlf_number,
      location = sec$title %||% "",
      problem = sprintf(
        "Population filter '%s' did not parse into an ARS WhereClause; carried as annotationText",
        pop_annot),
      action = "Analyses will run on the full population until this filter is expressible -- review the population annotation"
    )
  }
  ## level + order are stamped by build_ars_json() at the dedup append site.
  obj
}

#' All GroupingFactors for a section, ordered outermost first. Prefers the
#' multi-level `sec$groupings` list (P8); falls back to the legacy single
#' `by_variable` / `by_variable_dataset` pair.
#' @noRd
.build_groupings <- function(sec, codelists = NULL, spec_lookup = NULL) {
  ## Hierarchical column tree: one standard flat GroupingFactor per header
  ## level, so siera and every other flat consumer keep working -- the
  ## hierarchy itself travels as the output's resultGroupPaths extension.
  tree <- sec$column_tree
  if (!is.null(tree) &&
      (tree$mode %||% "FLAT") %in% c("NESTED", "ASYMMETRIC_NESTED") &&
      length(tree$levels %||% list()) > 0) {
    out <- lapply(tree$levels, function(lvl) {
      cg <- if (lvl$level == 1L) sec$column_groups
            else .tree_level_column_groups(tree, lvl)
      .build_grouping_one(lvl$variable, lvl$dataset %||% "ADSL",
                          column_groups = cg,
                          codelists = codelists, spec_lookup = spec_lookup)
    })
    return(Filter(Negate(is.null), out))
  }

  groupings <- sec$groupings
  if (is.null(groupings) || length(groupings) == 0) {
    single <- .build_grouping(sec, codelists, spec_lookup)
    return(if (is.null(single)) list() else list(single))
  }
  out <- lapply(groupings, function(g) {
    if (!nzchar(g$variable %||% "")) return(NULL)
    .build_grouping_one(g$variable, g$dataset %||% "ADSL",
                        column_groups = sec$column_groups,
                        codelists = codelists, spec_lookup = spec_lookup)
  })
  Filter(Negate(is.null), out)
}

#' Legacy single-grouping builder (kept for sections enriched before the
#' multi-level model and for direct unit tests).
#' @noRd
## A data-driven ROW grouping: the parent level of a nested block (AESOC for
## an AE table's PT analysis). Unlike a column grouping, its levels come
## from the data at execution time, so no groups are enumerated and
## dataDriven is TRUE -- exactly the ARS shape the engine turns into a
## second {cards} `by` variable.
#' @noRd
.build_row_grouping <- function(variable, dataset) {
  list(
    id               = make_grouping_id(variable),
    name             = variable,
    label            = paste0("Grouping by ", variable),
    groupingDataset  = dataset,
    groupingVariable = variable,
    dataDriven       = TRUE,
    groups           = list()
  )
}

.build_grouping <- function(sec, codelists = NULL, spec_lookup = NULL) {
  by_var <- sec$by_variable %||% ""
  if (!nzchar(by_var)) return(NULL)
  .build_grouping_one(by_var, sec$by_variable_dataset %||% "ADSL",
                      column_groups = sec$column_groups,
                      codelists = codelists, spec_lookup = spec_lookup)
}

.build_grouping_one <- function(variable, dataset, column_groups = NULL,
                                codelists = NULL, spec_lookup = NULL) {
  ## Header-annotated column groups always win; the spec codelist is the
  ## fallback so an unannotated coded column axis (e.g. COHORTN) still gets
  ## labelled, condition-defined columns instead of raw coded values.
  groups <- .build_group_levels(variable, column_groups)
  codelist_derived <- FALSE
  if (length(groups) == 0) {
    groups <- .build_group_levels_from_codelist(variable, dataset,
                                                codelists, spec_lookup)
    codelist_derived <- length(groups) > 0
  }
  gf <- list(
    id               = make_grouping_id(variable),
    name             = variable,
    label            = paste0("Grouping by ", variable),
    ## siera reads these as FLAT strings (metadata.R lines 188-189).
    ## Dataset comes from the spec-resolved enrichment field -- grouping
    ## variables are not always ADSL (e.g. AVISIT in a BDS dataset).
    groupingDataset  = dataset,
    groupingVariable = variable,
    dataDriven       = length(groups) == 0L,
    ## Per-level groups when the shell's column headers defined them (each
    ## header condition = one display column); otherwise the empty array
    ## siera expects (it iterates JSON_AnalysisGroupings$groups[[e]]).
    groups           = groups
  )
  ## Marker for the caller's once-per-factor diagnostic; attributes on lists
  ## are dropped by jsonlite::toJSON, so nothing leaks into the ARS file.
  attr(gf, "codelist_derived") <- codelist_derived
  gf
}

#' Canonical signature of a grouping DEFINITION -- what makes two groupings
#' the same column axis, whatever id each was minted under. Every group
#' condition is canonicalized first, so "IN (1,2)" and "IN (2,1)" agree.
#'
#' The grouping DATASET is deliberately left out. One treatment axis is
#' routinely recorded against ADSL on the demographics table and against the
#' occurrence dataset on the AE table; those are one axis, not two. What
#' tells two axes apart is the columns they produce.
#' @noRd
.grouping_signature <- function(gf) {
  groups <- lapply(gf$groups %||% list(), function(g) {
    list(
      label = as.character(g$label %||% g$name %||% ""),
      order = as.integer(g$order %||% NA_integer_),
      where = canonicalize_condition(.group_where(g))
    )
  })
  group_order <- vapply(groups, function(group) group$order, integer(1))
  groups <- groups[order(group_order, seq_along(groups), na.last = TRUE)]

  definition <- list(
    variable   = toupper(gf$groupingVariable %||% ""),
    dataDriven = isTRUE(gf$dataDriven),
    groups     = groups
  )
  paste(deparse(definition), collapse = "")
}

#' Canonical signature of what an AnalysisSet or a DataSubset FILTERS --
#' the thing that makes two of them the same entity, whatever id each was
#' minted under. `.group_where()` reads both the simple and the compound
#' shapes, and `canonicalize_condition()` settles spelling, so
#' `IN (1,2)` and `IN (2,1)` agree.
#'
#' A population whose annotation did not parse is defined by that text: two
#' different unparsed filters are two different populations, even though
#' neither carries a condition.
#' @noRd
.where_signature <- function(node) {
  definition <- list(
    where      = canonicalize_condition(.group_where(node)),
    annotation = as.character(node$annotationText %||% "")
  )
  paste(deparse(definition), collapse = "")
}

#' TRUE when an entity says which population or rows it means, and nothing
#' about how to select them -- a name with no condition, no compound
#' expression and no unparsed annotation text.
#' @noRd
.entity_is_unscoped <- function(node) {
  is.null(node$condition) &&
    is.null(node$compoundExpression) &&
    !nzchar(node$annotationText %||% "")
}

#' Move a grouping to a new id. The per-level group ids move with it: a group
#' id is only variable + label, so two definitions of one variable that share
#' a column label would otherwise mint the same group id twice --  and
#' resolve_analysis() keeps ONE group index across every factor, so one
#' output's result path would quietly resolve the other output's condition.
#' @noRd
.rename_grouping <- function(gf, new_id) {
  gf$id <- new_id
  stem  <- sub("^GF_", "", new_id)
  gf$groups <- lapply(gf$groups %||% list(), function(g) {
    g$id <- make_group_id(stem, g$label %||% g$name %||% "")
    g
  })
  gf
}

#' One officially-shaped ARS Group. Per ARS v1.0, Group IS-A WhereClause:
#' it must carry `level` and `order`, and its condition sits directly on the
#' node -- a bare WhereClauseCondition under `condition`, or a
#' `compoundExpression`. `where` arrives in the internal wrapped shape
#' (what `parse_where_clause()` returns) and is unwrapped here, at the ARS
#' boundary; internal consumers keep reading through `.group_where()`.
#' @noRd
.official_group <- function(variable, label, order, where) {
  g <- list(
    id    = make_group_id(variable, label),
    name  = label,
    label = label,
    level = 1L,
    order = order
  )
  if (!is.null(where[["condition"]])) {
    g$condition <- where[["condition"]]
  } else if (!is.null(where[["compoundExpression"]])) {
    g$compoundExpression <- where[["compoundExpression"]]
  }
  g
}

#' A Group whose condition was written but could not be read.
#'
#' Deliberately carries NO `condition` and NO `compoundExpression`: the group
#' must not look like a level that selects records, because it does not select
#' any. It keeps its identity, its label and its order so the column axis holds
#' its shape, and it carries the author's text under `unresolvedCondition` so
#' validation can reserve what computes through it and the fix report can quote
#' what was written.
#'
#' The model invariant elsewhere in this package -- a group holds either a
#' condition or a compoundExpression, never both -- is respected by holding
#' neither, which is the honest description of a level nobody can evaluate.
#' @noRd
.official_group_unresolved <- function(variable, label, order, text) {
  list(
    id    = make_group_id(variable, label),
    name  = label,
    label = label,
    level = 1L,
    order = order,
    unresolvedCondition = text
  )
}

#' Per-level Group objects derived from the ADaM spec codelist of the
#' grouping variable -- the fallback when the shell's column headers carried
#' no parseable conditions. One EQ condition per codelist term, labelled by
#' its decoded value, in codelist order. Empty list when no codelist
#' resolves or the codelist is too large to expand into columns.
#' @noRd
.build_group_levels_from_codelist <- function(variable, dataset,
                                              codelists, spec_lookup) {
  if (is.null(codelists) || length(codelists) == 0) return(list())
  bare_var <- toupper(sub("^.*\\.", "", variable %||% ""))
  ds       <- toupper(dataset %||% "ADSL")
  if (!nzchar(bare_var)) return(list())

  rec <- if (!is.null(spec_lookup)) spec_lookup[[paste0(ds, ".", bare_var)]] else NULL
  cl  <- .codelist_for(codelists, ds, bare_var, rec)
  if (is.null(cl) || nrow(cl$terms) == 0) return(list())
  if (nrow(cl$terms) > .CODELIST_DECODE_MAX_TERMS) return(list())

  out <- list()
  for (i in seq_len(nrow(cl$terms))) {
    label <- cl$terms$decode[i]
    out[[length(out) + 1L]] <- .official_group(
      bare_var, label, length(out) + 1L,
      list(condition = list(
        dataset    = ds,
        variable   = bare_var,
        comparator = "EQ",
        value      = list(cl$terms$term[i])
      ))
    )
  }
  out
}

#' Per-level Group objects for a grouping factor, from the parser's
#' `sec$column_groups`. Empty list when the section has none or they belong
#' to a different variable. Groups are emitted in the OFFICIAL ARS shape
#' (`.official_group()`); internal consumers read them back through
#' `.group_where()`.
#' @noRd
.build_group_levels <- function(variable, column_groups) {
  if (is.null(column_groups) || length(column_groups$groups %||% list()) == 0) {
    return(list())
  }
  bare_var <- toupper(sub("^.*\\.", "", variable))
  if (!identical(toupper(column_groups$variable %||% ""), bare_var)) {
    return(list())
  }

  out <- list()
  for (def in column_groups$groups) {
    ## A typed supplement group (v3) carries the ARS WhereClause directly;
    ## a shell-derived group carries only the annotation string to parse.
    condition <- def$condition %||% parse_where_clause(def$annotation %||% "")
    if (.is_unresolved_condition(condition)) {
      ## The level WAS defined and could not be read. Dropping it silently
      ## re-shapes the column axis: the remaining levels close the gap, so a
      ## column ends up showing a different subgroup than its header claims.
      ##
      ## So the level is kept and marked instead. Validation reserves whatever
      ## computes through this grouping, and the header keeps its own identity.
      out[[length(out) + 1L]] <- .official_group_unresolved(
        bare_var, def$label %||% "", def$order %||% (length(out) + 1L),
        .unresolved_condition_text(condition)
      )
      diag_add(
        stage = "build_ars", severity = "WARN",
        problem = sprintf(
          "Column group '%s' has a condition that could not be read (%s)",
          def$label %||% "", def$annotation %||% ""),
        action = "Results in that column are reserved until the condition is expressible"
      )
      next
    }
    if (is.null(condition)) {
      diag_add(
        stage = "build_ars", severity = "WARN",
        problem = sprintf(
          "Column group '%s' has no parseable condition (%s); level dropped",
          def$label %||% "", def$annotation %||% ""),
        action = "That header column will be data-driven instead of condition-defined"
      )
      next
    }
    out[[length(out) + 1L]] <- .official_group(
      bare_var, def$label %||% "", def$order %||% (length(out) + 1L),
      condition
    )
  }
  out
}

#' Column-groups-shaped level definitions for one deeper tree level: the
#' detail leaf columns at that level whose grouping variable is the level's
#' axis. A subtotal node contributes no level (its scope is its parent's
#' condition), and repeated child labels under different parents collapse to
#' one level per distinct condition.
#' @noRd
.tree_level_column_groups <- function(tree, lvl) {
  axis <- paste0(toupper(lvl$dataset %||% "ADSL"), ".", toupper(lvl$variable))
  nodes <- Filter(function(n) {
    n$level == lvl$level && identical(n$node_type, "leaf") &&
      identical(n$grouping_ref, axis) && !is.null(n$condition)
  }, tree$nodes)
  nodes <- nodes[order(vapply(nodes, function(n) n$order, integer(1)))]

  groups <- list()
  seen <- character(0)
  for (n in nodes) {
    key <- paste(deparse(canonicalize_condition(n$condition)), collapse = "")
    if (key %in% seen) next
    seen <- c(seen, key)
    groups[[length(groups) + 1L]] <- list(
      label      = n$label,
      annotation = n$annotation,
      condition  = n$condition,
      order      = length(groups) + 1L
    )
  }
  list(variable = lvl$variable, dataset = lvl$dataset %||% "ADSL",
       groups = groups)
}

#' The output's `resultGroupPaths` extension: one entry per declared result
#' column, in display order, each referencing the standard group levels whose
#' conditions compose the column. NULL when the section has no hierarchical
#' column tree (flat and crossed tables carry no path metadata).
#'
#' Conditions are NOT duplicated here -- a path's condition is the AND of its
#' referenced groups' conditions, so the GroupingFactors stay the single
#' source of condition truth. A SUBTOTAL path references only its parent's
#' group (the parent condition IS its scope); a GRAND_TOTAL path references
#' no group at all (the analysis set alone scopes it).
#' @noRd
.build_result_group_paths <- function(sec, gf_objs) {
  tree <- sec$column_tree
  if (is.null(tree) ||
      !(tree$mode %||% "FLAT") %in% c("NESTED", "ASYMMETRIC_NESTED")) {
    return(NULL)
  }
  paths <- column_tree_paths(tree)
  if (length(paths) == 0) return(NULL)

  ## Canonical condition -> group id, across every grouping factor.
  condition_key <- function(x) paste(deparse(canonicalize_condition(x)), collapse = "")
  index <- list()
  for (gf in gf_objs) {
    for (g in gf$groups %||% list()) {
      key <- condition_key(.group_where(g))
      if (is.null(index[[key]])) index[[key]] <- g$id
    }
  }
  node_by_id <- stats::setNames(
    tree$nodes, vapply(tree$nodes, function(n) n$id, character(1)))

  tlf_key <- gsub("[^A-Za-z0-9]+", "_", toupper(sec$tlf_number %||% "TLF"))
  out_paths  <- list()
  provenance <- list()
  for (p in paths) {
    group_ids <- character(0)
    for (nid in p$node_ids) {
      node <- node_by_id[[nid]]
      if (is.null(node) || is.null(node$condition)) next
      ## A subtotal's own condition (when annotated) duplicates its parent's;
      ## a grand total is scoped by the analysis set. Neither is a group.
      if (node$node_type %in% c("subtotal", "grand_total")) next
      gid <- index[[condition_key(node$condition)]]
      if (is.null(gid)) {
        diag_add(
          stage = "build_ars", severity = "WARN",
          problem = sprintf(
            "Result path '%s': node '%s' has no matching group level; the path may not execute",
            paste(p$label_path, collapse = " > "), node$label),
          tlf_number = sec$tlf_number,
          action = "Check the header annotations -- every detail column needs a group level with the same condition"
        )
        next
      }
      group_ids <- c(group_ids, gid)
    }

    entry <- list(
      pathId    = sprintf("PATH_%s_%02d", tlf_key, p$order),
      order     = p$order,
      role      = p$role,
      nodeId    = p$path_id,
      labelPath = as.list(p$label_path),
      groupIds  = as.list(group_ids)
    )
    if (!is.null(p$total_strategy)) entry$totalStrategy <- p$total_strategy
    out_paths[[length(out_paths) + 1L]] <- entry

    if (!is.null(p$source)) {
      provenance[[length(provenance) + 1L]] <- list(
        pathId    = entry$pathId,
        headerRow = p$source$header_row,
        colStart  = p$source$col_start,
        colEnd    = p$source$col_end
      )
    }
  }

  list(mode = tree$mode, paths = out_paths, provenance = provenance)
}

.build_method <- function(sec) {
  name <- sec$ars_method_name %||% "Count and Percentage"
  std  <- .STANDARD_METHODS[[name]]
  if (is.null(std)) {
    diag_add(
      stage = "build_ars", severity = "WARN",
      problem = sprintf("Analysis method '%s' is not in the standard catalogue", name),
      tlf_number = sec$tlf_number,
      action = "Emitted a placeholder no-op method -- this TLF's results will not compute until the method is implemented"
    )
    ## Unknown method name -- still emit a minimal codeTemplate so siera
    ## generates a runnable (no-op) ARD_*.R rather than failing at metadata.
    fallback <- list(
      id          = make_method_id(name),
      name        = name,
      label       = name,
      description = name,
      operations  = list(list(id = "OP_PASS", name = "Passthrough",
                              label = "Passthrough", order = 1L,
                              resultPattern = "X")),
      codeTemplate = list(
        context    = "R (siera)",
        code       = paste(
          "## Unknown method '", name, "' -- placeholder template.",
          "df3_analysisidhere <- data.frame(operation = 'OP_PASS',",
          "                                  res = NA_real_,",
          "                                  pattern = 'X')",
          sep = "\n"
        ),
        parameters = list()
      )
    )
    return(.with_op_self_rels(fallback))
  }
  .with_op_self_rels(std)
}

## Declarative method for a capability-gated (unsupported) section. The ARS
## still carries the analysis + this method so the Output -> Analysis -> Method
## chain stays intact (ADR 0002 phase 3); the engine reserves manual_pending
## stub ARD rows for it (id matches .UNEXECUTABLE_METHODS in ars_to_ard.R).
## Tagged supported = FALSE with the capability reason for traceability.
#' @noRd
.build_unsupported_method <- function(sec) {
  reason <- sec$unsupported_reason %||% "not supported by arsbridge"
  .with_op_self_rels(list(
    id          = "MTH_UNSUPPORTED_ANALYSIS",
    name        = "Unsupported analysis (manual)",
    label       = "Unsupported analysis (manual)",
    description = reason,
    supported   = FALSE,
    operations  = list(list(id = "OP_MANUAL", name = "Manual derivation",
                            label = "Manual derivation", order = 1L,
                            resultPattern = "X")),
    codeTemplate = list(
      context    = "R (siera)",
      code       = paste0(
        "## Manual derivation required -- ", reason, "\n",
        "## arsbridge reserves a manual_pending ARD cell for this analysis;\n",
        "## compute it with a validated script and fill the reserved row\n",
        "## (see ars_manual_worklist())."),
      parameters = list())
  ))
}

## A supported AnalysisMethod for an arsbridge-executable inferential method
## (ADR 0001) -- the exact CI or the CMH p-value. Unlike .build_unsupported_method
## this is tagged supported = TRUE; the arsbridge engine emits the cardx / base-R
## call for it. A minimal codeTemplate keeps siera happy.
#' @noRd
.build_exec_method <- function(method_id) {
  nm <- switch(method_id,
    MTH_CMH_TEST            = "Cochran-Mantel-Haenszel test",
    MTH_PROPORTION_CI_EXACT = "Clopper-Pearson exact confidence interval",
    method_id)
  .with_op_self_rels(list(
    id          = method_id,
    name        = nm,
    label       = nm,
    description = nm,
    supported   = TRUE,
    operations  = list(list(id = "OP_STAT", name = nm, label = nm, order = 1L,
                            resultPattern = "X")),
    codeTemplate = list(
      context    = "R (arsbridge)",
      code       = paste0("## ", nm,
                          " -- computed by the arsbridge engine (cardx / base R)."),
      parameters = list())
  ))
}

## The enrichment (primary dataset + variable) of a section's main response row,
## used as the analysis variable for the section-level inferential methods.
## Returns the first enriched row that names a primary variable, or list().
#' @noRd
.section_primary_enrichment <- function(sec) {
  for (er in sec$enriched_rows %||% list()) {
    if (!is.null(er$primary_variable) && nzchar(er$primary_variable %||% ""))
      return(er)
  }
  list()
}

.build_data_subset <- function(enrichment, tlf_number, index) {
  ## A compound supplement filter (v3) that could not flatten to a single
  ## condition is emitted as a compoundExpression DataSubset. .eval_where_clause()
  ## and where_to_filter_expr() both consume this shape, so the engine and the
  ## code emitter execute it with no extra translation.
  comp <- enrichment$data_subset_compound
  if (!is.null(comp) && !is.null(comp$compoundExpression)) {
    tag <- paste0(tlf_number, "_", index, "_cmp")
    return(list(
      id    = make_data_subset_id(tag),
      name  = tag,
      label = tag,
      compoundExpression = comp$compoundExpression
    ))
  }
  ds <- enrichment$data_subset
  if (is.null(ds) || length(ds) == 0) return(NULL)
  tag <- if (!is.null(ds$variable)) {
    first_val <- if (length(ds$value)) ds$value[[1]] else ""
    paste0(ds$dataset, "_", ds$variable, "_", first_val)
  } else {
    paste0(tlf_number, "_", index)
  }
  list(
    id    = make_data_subset_id(tag),
    name  = tag,
    label = tag,
    condition = list(
      dataset    = ds$dataset    %||% "",
      variable   = ds$variable   %||% "",
      comparator = ds$comparator %||% "EQ",
      value      = ds$value      %||% list()
    )
    ## level + order stamped by build_ars_json() at dedup append site.
  )
}

.build_analysis <- function(section, row, enrichment, index,
                            as_id, gf_ids, method_id, ds_id,
                            row_grouping_ids = character(0)) {
  gf_ids <- gf_ids[nzchar(gf_ids %||% character())]
  ## Column groupings first (treatment axis), then any data-driven row
  ## groupings (the parent level of a nested block) -- the order the engine
  ## hands them to {cards} `by`.
  all_gf_ids <- c(gf_ids,
                  row_grouping_ids[nzchar(row_grouping_ids %||% character())])
  groupings <- lapply(seq_along(all_gf_ids), function(i) {
    list(order = i, groupingId = all_gf_ids[[i]], resultsByGroup = TRUE)
  })

  dataset_str  <- enrichment$primary_dataset  %||% ""
  variable_str <- enrichment$primary_variable %||% ""
  self_id      <- make_analysis_id(section$tlf_number, index)

  ## siera's metadata.R only appends rows to AN_refs (and creates the "id"
  ## column it later merges by) when `referencedAnalysisOperations` is
  ## non-null on at least one analysis. If no analysis carries refs,
  ## `merge(JSON_AnalysesL1, AN_refs, by = "id")` fails with "'by' must
  ## specify a uniquely valid column". And siera's downstream code reads
  ## both `*_analysisId1` (NUM) and `*_analysisId2` (DEN) -- if either is
  ## missing, the grouping-variable lookup returns character(0) and a
  ## bare `if (... %in% ...)` errors with "argument is of length zero".
  ##
  ## We have no genuine NUM/DEN relationship to express, so emit two
  ## self-references. siera's filter then resolves both NUM and DEN to
  ## this same analysis -- a harmless no-op that keeps the pipeline
  ## flowing.
  ref_ops <- list(
    list(referencedOperationRelationshipId = "SELF_NUM", analysisId = self_id),
    list(referencedOperationRelationshipId = "SELF_DEN", analysisId = self_id)
  )

  node <- list(
    id            = self_id,
    name          = paste0("Analysis ", index, " for ", section$tlf_number),
    label         = row$label %||% "",
    description   = row$label %||% "",
    version       = 1L,
    categoryIds   = list(),
    analysisSetId = as_id,
    ## siera reads `dataset` and `variable` as FLAT strings (metadata.R
    ## lines 232-233). The nested analysisVariable is kept alongside for
    ## ARS-spec-correct consumers.
    dataset       = dataset_str,
    variable      = variable_str,
    analysisVariable = list(
      dataset  = dataset_str,
      variable = variable_str
    ),
    dataSubsetId                 = if (is.null(ds_id)) "" else ds_id,
    orderedGroupings             = groupings,
    referencedAnalysisOperations = ref_ops,
    methodId                     = method_id,
    annotation                   = row$annotation,
    ## SAP prose matched to this TLF (when a SAP was supplied); the emitter
    ## prints it as the human-readable comment above the {cards} block.
    sapDescription               = section$sap_text %||% "",
    variableRole                 = enrichment$variable_role %||% "ANALYSIS",
    ## Extension field: TRUE when the shell carries an overall/Total column
    ## in addition to the per-group columns. The executor then also
    ## computes an ungrouped pass and binds it into the ARD.
    includeTotal                 = isTRUE(section$include_total),
    ## What that overall column is scoped to, and what the shell calls it.
    ##
    ## totalWhere is the Total column's own WhereClause when the shell
    ## annotated one ("Total [ADSL.COHORTN IN (1,2)]" -- which may deliberately
    ## exclude a displayed column), otherwise the union of the group columns.
    ## Absent means the analysis set alone scopes it. totalLabel is the
    ## header's own text, which the ARD row must carry for the fill to find
    ## the physical column it belongs to.
    totalWhere                   = section$total_condition,
    totalLabel                   = section$total_label
  )

  ## Extension field: a variable role that was proposed for this row but could
  ## not be attributed to it, and that no annotation can restate. Carried so the
  ## decision is visible in the file itself rather than only in a build-time
  ## message; .check_unresolved_variable_role() turns it into a blocking FAIL.
  ## Written as a list so a single role still reads as an array.
  unresolved_role <- enrichment$unresolved_variable_role %||% character(0)
  if (length(unresolved_role) > 0) {
    node$unresolvedVariableRole <- as.list(as.character(unresolved_role))
  }

  ## Extension field: a row filter the author wrote that this grammar could not
  ## read. Written only when a non-empty condition failed -- never as an empty
  ## string, and never when the row simply states no filter.
  ##
  ## The analysis deliberately carries NO data subset in this case, so without
  ## the marker it would be indistinguishable from a row that never asked to be
  ## filtered, and would compute over every record.
  ## `.check_unresolved_condition()` turns this into a GAP that reserves.
  ## The section's Total column contributes here too. An authored Total whose
  ## own annotation could not be read has no grouping level to carry it -- a
  ## Total is never a level -- so the analysis displaying it is marked instead,
  ## and `totalWhere` is left absent rather than holding an unreadable object.
  ##
  ## Every OTHER unreadable header is a level, and the grouping it belongs to
  ## reserves whatever computes through it, the Total cell included. This
  ## branch is only for the case nothing else covers.
  unresolved_condition <- c(
    enrichment$unresolved_condition %||% character(0),
    section$total_unresolved %||% character(0)
  )
  unresolved_condition <- unresolved_condition[
    !is.na(unresolved_condition) & nzchar(unresolved_condition)
  ]
  if (length(unresolved_condition) > 0) {
    node$unresolvedCondition <- unresolved_condition[[1]]
  }
  node
}

.build_output <- function(section, analysis_ids, ship_annotations = FALSE,
                          shell_layout = NULL, result_group_paths = NULL,
                          shell_fill = NULL) {
  ## Shipped footnotes are the true footnotes only; programmer annotation
  ## lines are mapping instructions, not display text (ADR 0003 Layer B).
  ## ship_annotations = TRUE re-attaches them for debugging.
  shipped_notes <- as.character(section$footnotes %||% character())
  if (isTRUE(ship_annotations)) {
    shipped_notes <- c(shipped_notes,
                       as.character(section$programmer_annotations %||% character()))
  }
  ## Output-private metadata (ADR 0003 Layer C). ARS v1.0 has no first-class
  ## stub model, so the authored layout travels in an arsbridge `_meta` block
  ## that standard consumers ignore and the renderer keys on.
  out_meta <- list(
    source_datasets = as.list(as.character(section$source_datasets %||% character()))
  )
  if (length(shell_layout %||% list()) > 0) {
    out_meta$shell_layout <- shell_layout
  }
  ## What the overall column MEANS, in words a reviewer can check against the
  ## shell without reading a WhereClause: the scope it was given, the
  ## annotation it came from, and the header it fills.
  if (isTRUE(section$include_total)) {
    out_meta$overall_column <- list(
      label      = section$total_label %||% "Total",
      scope      = section$total_scope %||% "analysis_set",
      annotation = section$total_annotation %||% ""
    )
  }
  ## The cell map for writing results back into the shell workbook. Absent
  ## for a Word shell, which has no cell addresses.
  if (length(shell_fill %||% list()) > 0) {
    out_meta$shell_fill <- shell_fill
  }
  ## Supplement channels arsbridge records but does not yet compute
  ## (record filters, sorting, denominators, provenance). They travel in the
  ## output _meta so a reviewer -- and a future engine -- can see them.
  if (length(section$supplement_extras %||% list()) > 0) {
    out_meta$supplement <- section$supplement_extras
  }
  ## The parsed/declared column tree travels as provenance so the review
  ## stage can show it and validation can hold the emitted paths against the
  ## shell's own column count. Conditions stay out -- the GroupingFactors and
  ## resultGroupPaths carry the semantics; this is display structure.
  if (!is.null(section$column_tree)) {
    out_meta$column_tree <- list(
      mode  = section$column_tree$mode,
      nodes = lapply(section$column_tree$nodes, function(n) {
        node <- list(
          id         = n$id,
          label      = n$label,
          level      = n$level,
          order      = n$order,
          nodeType   = n$node_type,
          annotation = n$annotation %||% ""
        )
        if (!is.null(n$parent_id)) node$parentId <- n$parent_id
        if (!is.na(n$n_hint %||% NA_integer_)) node$nHint <- n$n_hint
        if (!is.null(n$source)) node$source <- n$source
        node
      })
    )
  }
  output_id <- make_output_id(section$tlf_number)

  out <- list(
    id                    = output_id,
    name                  = section$tlf_number,
    label                 = section$title %||% "",
    version               = 1L,
    outputType            = section$tlf_type %||% "TABLE",
    ## The official shape: each entry is an OrderedDisplay wrapping the
    ## display itself, which carries its own id and name.
    displays              = list(list(
      order   = 1L,
      display = list(
        id           = paste0(output_id, "_D1"),
        name         = section$title %||% section$tlf_number,
        displayTitle = section$title %||% "",
        ## Carry the shell's column-header order so the renderer lays
        ## treatment columns out as the author wrote them (build_col_levels
        ## reads this), instead of falling back to alphabetical ARD order.
        ## An arsbridge extension; ars_conformance() strips it. Word sections
        ## keep the physical header vector so its blank stub remains column 1;
        ## older/internal section builders carry only nonblank col_headers.
        columns      = local({
          headers <- section$col_labels_full
          if (is.null(headers)) {
            headers <- Filter(
              nzchar,
              as.character(section$col_headers %||% character())
            )
          }
          lapply(as.character(headers), function(h) list(label = h))
        }),
        displaySections = list(list(
          sectionType = "Footnote",
          ## Official shape: each footnote is an OrderedSubSection wrapping a
          ## DisplaySubSection, which requires its own id and text.
          orderedSubSections = local({
            notes <- as.list(shipped_notes)
            lapply(seq_along(notes), function(i) list(
              order      = i,
              subSection = list(
                id   = paste0(output_id, "_D1_FN", i),
                text = notes[[i]]
              )
            ))
          })
        ))
      )
    )),
    fileSpecifications    = list(list(
      name     = paste0(section$tlf_number, ".rtf"),
      ## A terminology object, per the standard (enum: pdf, rtf, txt).
      fileType = list(controlledTerm = "rtf")
    )),
    referencedAnalysisIds = as.list(analysis_ids),
    `_meta`               = out_meta
  )
  ## Declared result-column paths (arsbridge extension; ars_conformance()
  ## strips it). Only the paths named here are ever executed -- never a
  ## Cartesian product of the grouping variables.
  if (!is.null(result_group_paths)) {
    out$resultGroupPaths <- result_group_paths
  }
  out
}


## --- siera-empty-array safety nets ----------------------------------------
##
## siera's .read_ars_json_metadata() unconditionally walks
## `seq_len(nrow(JSON_DataSubsets))` and `seq_len(nrow(JSON_AG_1))`, so
## an empty `dataSubsets: []` or `analysisGroupings: []` crashes the
## reader with "argument must be coercible to non-negative integer".
## These fallbacks emit a structurally valid no-op so siera proceeds.

#' Placeholder DataSubset representing "all subjects" (no filter).
#' Condition is USUBJID != "" which matches every record.
#' @noRd
.default_data_subset <- function() {
  list(
    id        = "DS_ALL",
    name      = "All subjects",
    label     = "All subjects (no filter)",
    level     = 1L,
    order     = 1L,
    condition = list(
      dataset    = "ADSL",
      variable   = "USUBJID",
      comparator = "NE",
      value      = list("")
    )
  )
}

#' Placeholder GroupingFactor for the default TRT01A grouping.
#' @noRd
.default_grouping <- function() {
  list(
    id               = "GF_TRT01A",
    name             = "TRT01A",
    label            = "Default treatment grouping",
    groupingDataset  = "ADSL",
    groupingVariable = "TRT01A",
    dataDriven       = TRUE,
    groups           = list()
  )
}

## --- siera-required tables of contents ------------------------------------

#' Build otherListsOfContents (List of Planned Outputs / LOPO).
#'
#' siera reads this as `json_from$otherListsOfContents$contentsList$listItems[[1]]`
#' (metadata.R:64) -- the outer array contains one LOPO; the inner listItems
#' is the array of outputs.
#' @noRd
.build_lopo <- function(outputs) {
  list(list(
    name  = "List of Planned Outputs",
    label = "LOPO",
    contentsList = list(
      listItems = lapply(seq_along(outputs), function(i) list(
        name     = outputs[[i]]$label %||% outputs[[i]]$name,
        level    = 1L,
        order    = i,
        outputId = outputs[[i]]$id
      ))
    )
  ))
}

#' Build mainListOfContents (List of Planned Analyses / LOPA).
#'
#' siera reads this as `json_from$mainListOfContents$contentsList$listItems`
#' and iterates `$sublist$listItems[[a]]$analysisId` per output (metadata.R:73,82).
#'
#' siera's `.generate_analysis_set_code()` indexes `anas[3, ]$listItem_analysisId`
#' unconditionally (AnalysisSet.R:141). When an Output has <3 analyses, that
#' returns NA and `gsub("analysisADAMhere", NA, ...)` crashes with "invalid
#' 'replacement' argument". We pad the sublist by repeating the last
#' analysisId to a minimum of 3 entries -- the duplicates are filtered out
#' by siera's downstream `unique()` calls or land on the same-dataset path
#' (a no-op).
#' @noRd
.build_lopa <- function(outputs) {
  pad_to_min <- function(ids, min_n = 3L) {
    if (length(ids) == 0L) return(ids)
    if (length(ids) >= min_n) return(ids)
    c(ids, rep(utils::tail(ids, 1L), min_n - length(ids)))
  }

  list(
    name  = "List of Planned Analyses",
    label = "LOPA",
    contentsList = list(
      listItems = lapply(seq_along(outputs), function(i) {
        o <- outputs[[i]]
        an_ids <- unlist(o$referencedAnalysisIds %||% list())
        an_ids_padded <- pad_to_min(an_ids)
        list(
          name     = o$label %||% o$name,
          level    = 1L,
          order    = i,
          outputId = o$id,
          sublist  = list(
            listItems = lapply(seq_along(an_ids_padded), function(j) list(
              ## The standard requires a name on every list item. The id is
              ## the only thing known here (this list is rebuilt from the
              ## outputs alone), and it is an honest one.
              name       = an_ids_padded[j],
              analysisId = an_ids_padded[j],
              level      = 2L,
              order      = j
            ))
          )
        )
      })
    )
  )
}
