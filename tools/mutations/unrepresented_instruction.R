## Mutations for the unrepresented-instruction reservation. Run with:
##   Rscript tools/mutation_harness.R tools/mutations/unrepresented_instruction.R

mutations <- list(
  ## The rule never fires: an instruction about excluded records is ignored
  ## and the filter computes alone -- a plausible number under a different
  ## definition than the one authored.
  list(id = "M-blind", file = "R/utils_where_clause.R",
       from = "    if (grepl(.RE_RESIDUE_NEGATION, span, perl = TRUE) &&\n        grepl(.RE_OBSERVATION_UNIT, span, perl = TRUE) &&\n        grepl(.RE_INSTRUCTION_ASSIGNS, span, perl = TRUE)) {",
       to   = "    if (FALSE) {"),

  ## The rule fires on EVERY aside, not just an instruction. `(per protocol)`
  ## and `(N=XX)` would reserve rows that compute correctly -- the failure
  ## direction that looks safe and is not.
  list(id = "M-greedy", file = "R/utils_where_clause.R",
       from = "    if (grepl(.RE_RESIDUE_NEGATION, span, perl = TRUE) &&\n        grepl(.RE_OBSERVATION_UNIT, span, perl = TRUE) &&\n        grepl(.RE_INSTRUCTION_ASSIGNS, span, perl = TRUE)) {",
       to   = "    if (TRUE) {"),

  ## The unit-of-observation half is dropped, so the rule fires on any negated
  ## aside: `(no units)` and `(except per protocol)` reserve rows that compute
  ## correctly -- the failure direction that looks safe and is not.
  list(id = "M-nounit", file = "R/utils_where_clause.R",
       from = "    if (grepl(.RE_RESIDUE_NEGATION, span, perl = TRUE) &&\n        grepl(.RE_OBSERVATION_UNIT, span, perl = TRUE) &&\n        grepl(.RE_INSTRUCTION_ASSIGNS, span, perl = TRUE)) {",
       to   = "    if (grepl(.RE_RESIDUE_NEGATION, span, perl = TRUE) &&\n        grepl(.RE_INSTRUCTION_ASSIGNS, span, perl = TRUE)) {"),

  ## The assignment clause is dropped, so an aside that merely NAMES records
  ## reserves: `(except visit 1)` withholds a row that computes correctly.
  list(id = "M-noassign", file = "R/utils_where_clause.R",
       from = "    if (grepl(.RE_RESIDUE_NEGATION, span, perl = TRUE) &&\n        grepl(.RE_OBSERVATION_UNIT, span, perl = TRUE) &&\n        grepl(.RE_INSTRUCTION_ASSIGNS, span, perl = TRUE)) {",
       to   = "    if (grepl(.RE_RESIDUE_NEGATION, span, perl = TRUE) &&\n        grepl(.RE_OBSERVATION_UNIT, span, perl = TRUE)) {"),

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
