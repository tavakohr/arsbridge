## arsbridge -- aaa_constants.R
## ---------------------------------------------------------------------------
## Shared, package-wide constants and tiny utilities. Named `aaa_` so the
## R alphabetical source order puts it first -- other files that reference
## these names at top level (e.g. parse_shell_docx.R's `.ANNOTATION_PATTERN`)
## can safely use them.

## ADaM naming-convention regex pieces. ADaM dataset names always start with
## "AD" plus 1-6 uppercase letters; variable names are 1-8 uppercase
## alphanumeric chars starting with a letter. These two are the building
## blocks for every annotation pattern in the package.
.ADAM_DS  <- "AD[A-Z]{1,6}"
.ADAM_VAR <- "[A-Z][A-Z0-9]{0,7}"

## Null-coalescing operator. Returns `b` when `a` is NULL or zero-length;
## otherwise returns `a`. Mirrors rlang::`%||%` semantics without the dep.
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

## Is an optional package installed? A one-line wrapper over
## requireNamespace() so that a capability guard can be TESTED: a test can
## rebind this in the namespace and watch the fallback run, which it cannot do
## with requireNamespace() itself. Callers keep using
## rlang::check_installed() when the right answer is to stop; this is for the
## guards that degrade instead.
.pkg_available <- function(pkg) requireNamespace(pkg, quietly = TRUE)

## Largest codelist that EXPANDS -- into one row per term, and into column
## groups. A categorical analysis on a decoded variable shows every codelist
## term, unobserved ones as n = 0, which is right for a 9-term
## discontinuation codelist and would turn a 195-term COUNTRY codelist into
## 195 rows of mostly nothing.
##
## Above this size the decode still applies; only the expansion stops. The
## emitted block drops the unobserved levels, so a large codelist shows the
## terms the data took, with their decoded labels. Dropping the decode too --
## which is what used to happen -- left the ARD carrying raw codes, so no
## shell row labelled with a decoded value matched one and the whole block
## filled as placeholders.
.CODELIST_DECODE_MAX_TERMS <- 15L

## Analysis methods whose results are per-category and therefore display the
## variable's own values -- the ones a decode applies to. Continuous
## summaries, listings, and the inferential methods report no category label.
##
## Stated once because three places must agree: build_ars_json() decides
## which variables GET a decode, and the emitter and the legacy engine each
## decide which ones USE it. If those drifted, the ARS would either declare a
## decode nobody applies (silent raw codes) or an engine would decode a
## variable the ARS never declared (emitted != executed).
.DECODE_METHOD_IDS <- c("MTH_COUNT_AND_PERCENTAGE",
                        "MTH_AE_FREQUENCY_COUNT",
                        "MTH_SUBJECT_COUNT",
                        "MTH_SUBJECT_COUNT_PCT")

## Variables that always live in ADSL regardless of the listing's primary
## source dataset. Used by parse_shell_docx() to resolve a bare variable
## name (e.g. "USUBJID", "ACTARMCD") in a listing column header to its
## fully-qualified DATASET.VARIABLE form. Variables NOT in this set are
## prefixed with the listing's first source dataset (from "Source: ...").
.UNIVERSAL_ADSL_VARS <- c(
  ## Subject identifiers
  "USUBJID", "SUBJID", "STUDYID", "SITEID",
  ## Treatment / arm
  "ARM", "ACTARM", "ARMCD", "ACTARMCD",
  "TRT01P", "TRT01A", "TRT01PN", "TRT01AN",
  ## Population flags
  "SAFFL", "ITTFL", "FASFL", "PPROTFL", "MITTFL", "EFFFL", "RANDFL", "ENRLFL",
  ## Demographics
  "AGE", "AGEU", "AGEGR1", "AGEGR1N",
  "SEX", "RACE", "ETHNIC", "COUNTRY", "REGION",
  ## Treatment dates
  "TRTSDT", "TRTEDT", "TRTDURD"
)

## CDISC ARS v1.0 controlled terminology for Analysis.reason and
## Analysis.purpose (AnalysisReasonEnum / AnalysisPurposeEnum in the bundled
## schema). The generator stamps a default from each onto every analysis --
## overridable per run via spec_to_ars() and per line in edit_ars() -- because
## the standard requires both fields and an absent value fails conformance.
.ANALYSIS_REASONS <- c(
  "SPECIFIED IN PROTOCOL",
  "SPECIFIED IN SAP",
  "DATA DRIVEN",
  "REQUESTED BY REGULATORY AGENCY"
)
.ANALYSIS_PURPOSES <- c(
  "PRIMARY OUTCOME MEASURE",
  "SECONDARY OUTCOME MEASURE",
  "EXPLORATORY OUTCOME MEASURE"
)

## The blanket defaults. "SPECIFIED IN SAP" because the annotated shells the
## pipeline reads are SAP-derived; "EXPLORATORY OUTCOME MEASURE" because it is
## the safest understatement for the non-endpoint displays that make up most
## of a TLF package. The reviewer corrects the handful of endpoint tables in
## the editor.
.DEFAULT_ANALYSIS_REASON  <- "SPECIFIED IN SAP"
.DEFAULT_ANALYSIS_PURPOSE <- "EXPLORATORY OUTCOME MEASURE"

## The stat_name a reserved cell carries.
##
## Two independent code paths have to agree on this string or the cell map and
## the ARD never meet. `.build_table_cells()` derives a cell's stat_name from
## the method's operation; `.reserve_blocked_ard()` and the manual_pending stub
## write the ARD row. `.ard_value()` joins them on stat_name, so a disagreement
## does not fail loudly -- the lookup simply misses, and a cell that was
## deliberately reserved gets reported as "no result in the ARD", which reads
## as "the analysis failed" rather than "this is somebody's job".
##
## Naming it once is the fix. Neither side may spell it independently.
.MANUAL_STAT_NAME <- "result"

## Why a statistic row's label bound nothing.
##
## Stated once because three places must agree: `.build_table_cells()` records
## the code, `ars_unresolved_labels()` reports it, and the supplement channel
## that will answer these rows decides what it may answer FROM it. The two are
## genuinely different problems and lead to different work, so they are never
## collapsed into one "unresolved":
##
##   unreadable  arsbridge could not read the label as a statistic at all.
##               The vocabulary is missing a spelling, or the row is not a
##               statistic row. Somebody must say what it means.
##   unsupported the label was read correctly and names a statistic the row's
##               method does not declare. The meaning is not in doubt; the
##               method is wrong for the row, or the statistic is beyond the
##               engine. Nobody can supply a synonym that fixes this.
.UNRESOLVED_UNREADABLE  <- "unreadable"
.UNRESOLVED_UNSUPPORTED <- "unsupported"
.UNRESOLVED_REASONS     <- c(.UNRESOLVED_UNREADABLE, .UNRESOLVED_UNSUPPORTED)

## ---------------------------------------------------------------------------
## The shell layout's STRUCTURAL vocabulary.
##
## One question only: how does this authored row turn into displayed rows?
## Not what is calculated in them, not where the row came from, not whether
## arsbridge could resolve it. Those are separate fields (`status`,
## `provenance`) because they answer separate questions, and a single field
## answering three of them is how a filter came to decide block structure.
##
##   categorical_block  one analysis expands into data-driven level rows
##   stat_block         one analysis expands into statistic rows
##   scalar_row         one analysis renders as one row
##   nested_parent      opens a child block
##   nested_child       expands within its parent
##   level_row          an authored slot for one level of the block above it
##   label_row          an authored line carrying no analysis
##
## `NA_character_` is a real answer and means the structure was not proved.
## It is never a synonym for `scalar_row`: "I could not tell" and "it is one
## row" lead to different work.
##
## Deliberately NOT shapes:
##   * `self_template` is a property OF a categorical block (its own mock row
##     opens it), carried as a boolean field on the same entry;
##   * `level_repeat` / `nested_repeat` are control-flow decisions upstream --
##     those rows produce no layout entry at all, so they can have no shape.
.LAYOUT_SHAPES <- c("categorical_block", "stat_block", "scalar_row",
                    "nested_parent", "nested_child", "level_row", "label_row")

## Where a layout entry came from, when that is worth recording. Provenance
## changes how a row is LABELLED and grouped, never how it expands.
.LAYOUT_PROV_SUPPLEMENT <- "supplement_added"

## What a scalar row's cell SHOWS before real numbers exist. Not a shape --
## every one of these renders as exactly one row, and they differ only in the
## statistic the cell will hold. They lived in the shape field because the
## placeholder needed them, which is how "one row showing a count" and "a
## block expanding into levels" ended up as sibling values of one vocabulary.
.LAYOUT_STAT_FORMS <- c("subject_count", "subject_count_pct",
                        "filtered_count", "filtered_count_pct")

## Why a layout entry carries no usable analysis. A status, not a shape: a
## reserved row's structure is unknown, not "one row".
.LAYOUT_STATUS_MANUAL <- "manual"

## Does this entry own a block -- that is, do displayed rows BELOW it belong
## to it rather than standing on their own?
##
## Structural shapes answer this from their own definition. `status` is the
## exception and is deliberately narrow: a `manual` row has NO proved shape,
## and every renderer has always treated it as block-owning. Preserving that
## is a RENDERING COMPATIBILITY FALLBACK, not evidence that the row expands --
## nothing may read it as structural proof, and when the shape of a reserved
## row can be established this argument goes away without the renderers
## changing.
#' @noRd
.layout_owns_block <- function(shape, status = NULL) {
  shape  <- as.character(shape  %||% NA_character_)[1]
  status <- as.character(status %||% NA_character_)[1]
  if (!is.na(status) && identical(status, .LAYOUT_STATUS_MANUAL)) return(TRUE)
  !is.na(shape) && shape %in% c("categorical_block", "stat_block")
}

## The layout entry's optional metadata, keeping only the fields that carry an
## answer. `NA` and "absent" mean the same thing here -- nothing to say -- so
## emitting both spellings would put `"status": null` on every ordinary row
## and invite readers to treat the two as different.
#' @noRd
.layout_notes <- function(...) {
  notes <- list(...)
  keep <- vapply(notes, function(v) {
    length(v) == 1L && !is.na(v)
  }, logical(1))
  notes[keep]
}
