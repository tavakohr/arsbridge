## Mutations for the unrepresented-instruction reservation. Run with:
##   Rscript tools/mutation_harness.R tools/mutations/unrepresented_instruction.R

mutations <- list(
  ## The rule never fires: an instruction about excluded records is ignored
  ## and the filter computes alone -- a plausible number under a different
  ## definition than the one authored.
  list(id = "M-blind", file = "R/utils_where_clause.R",
       from = "    if (grepl(.RE_ABSENT_OBSERVATION, span, perl = TRUE) &&\n        grepl(.RE_OBSERVATION_UNIT, span, perl = TRUE) &&\n        .aside_assigns_state(span)) {",
       to   = "    if (FALSE) {"),

  ## The rule fires on EVERY aside, not just an instruction. `(per protocol)`
  ## and `(N=XX)` would reserve rows that compute correctly -- the failure
  ## direction that looks safe and is not.
  list(id = "M-greedy", file = "R/utils_where_clause.R",
       from = "    if (grepl(.RE_ABSENT_OBSERVATION, span, perl = TRUE) &&\n        grepl(.RE_OBSERVATION_UNIT, span, perl = TRUE) &&\n        .aside_assigns_state(span)) {",
       to   = "    if (TRUE) {"),

  ## The unit-of-observation half is dropped, so the rule fires on any negated
  ## aside: `(no units)` and `(except per protocol)` reserve rows that compute
  ## correctly -- the failure direction that looks safe and is not.
  list(id = "M-nounit", file = "R/utils_where_clause.R",
       from = "    if (grepl(.RE_ABSENT_OBSERVATION, span, perl = TRUE) &&\n        grepl(.RE_OBSERVATION_UNIT, span, perl = TRUE) &&\n        .aside_assigns_state(span)) {",
       to   = "    if (grepl(.RE_ABSENT_OBSERVATION, span, perl = TRUE) &&\n        .aside_assigns_state(span)) {"),

  ## The assignment clause is dropped, so an aside that merely NAMES records
  ## reserves: `(except visit 1)` withholds a row that computes correctly.
  list(id = "M-noassign", file = "R/utils_where_clause.R",
       from = "    if (grepl(.RE_ABSENT_OBSERVATION, span, perl = TRUE) &&\n        grepl(.RE_OBSERVATION_UNIT, span, perl = TRUE) &&\n        .aside_assigns_state(span)) {",
       to   = "    if (grepl(.RE_ABSENT_OBSERVATION, span, perl = TRUE) &&\n        grepl(.RE_OBSERVATION_UNIT, span, perl = TRUE)) {"),

  ## The refinement is reverted: clause one goes back to "a negating word
  ## somewhere" instead of an absence CONSTRUCTION. Co-occurrence then lets
  ## `(records are not shown separately)` satisfy all three tests, and rows
  ## describing the display start reserving.
  list(id = "M-cooccur", file = "R/utils_where_clause.R",
       from = "    if (grepl(.RE_ABSENT_OBSERVATION, span, perl = TRUE) &&\n        grepl(.RE_OBSERVATION_UNIT, span, perl = TRUE) &&\n        .aside_assigns_state(span)) {",
       to   = "    if (grepl(.RE_RESIDUE_NEGATION, span, perl = TRUE) &&\n        grepl(.RE_OBSERVATION_UNIT, span, perl = TRUE) &&\n        .aside_assigns_state(span)) {"),

  ## The ordering inside .aside_assigns_state() is reverted to a flat
  ## "treatment verb OR copula" test, so a bare copula counts again and
  ## `(missing visits are displayed separately)` reserves a row that computes.
  list(id = "M-copula", file = "R/utils_where_clause.R",
       from = "  if (!grepl(.RE_COPULA, span, perl = TRUE)) return(FALSE)\n  !grepl(.RE_PRESENTATION_VERB, span, perl = TRUE)",
       to   = "  grepl(.RE_COPULA, span, perl = TRUE)"),

  ## The `as <state>` path is removed, so an assignment carried by a
  ## presentation verb -- "reported as non-responders" -- stops being seen.
  list(id = "M-asstate", file = "R/utils_where_clause.R",
       from = "  at <- regexpr(\"(?i)\\\\bas\\\\s+\", span, perl = TRUE)",
       to   = "  at <- regexpr(\"(?i)\\\\bZZNEVERMATCHZZ\", span, perl = TRUE)"),

  ## Literals stop being masked first, so a value called 'Not Reported'
  ## reserves the row it belongs to.
  list(id = "M-literals", file = "R/utils_where_clause.R",
       from = "  masked <- .mask_literals(ann)\n  if (is.null(masked)) return(\"\")\n  hidden <- .mask_non_structural(masked$text, operand_context = TRUE)",
       to   = "  masked <- .mask_literals(ann)\n  if (is.null(masked)) return(\"\")\n  hidden <- .mask_non_structural(ann, operand_context = TRUE)"),

  ## The flat path stops consulting the rule: the exact A-19 shape with a
  ## single condition computes again.
  list(id = "M-flatgap", file = "R/build_ars_json.R",
       from = "  fs <- flat_data_subset(ann)\n  if (!is.null(fs)) {\n    if (nzchar(instruction)) {",
       to   = "  fs <- flat_data_subset(ann)\n  if (!is.null(fs)) {\n    if (FALSE) {"),

  ## The envelope path stops consulting it.
  list(id = "M-envgap", file = "R/build_ars_json.R",
       from = "    if (is.null(where)) return(NULL)\n    if (nzchar(instruction)) {",
       to   = "    if (is.null(where)) return(NULL)\n    if (FALSE) {")
)

test_files <- c(
  "tests/testthat/test-unrepresented_instruction.R",
  "tests/testthat/test-residue_meaning.R",
  "tests/testthat/test-derivation_note_annotation.R",
  "tests/testthat/test-build_ars_json.R"
)
