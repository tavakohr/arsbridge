## arsbridge -- utils_ars_ids.R
## ---------------------------------------------------------------------------
## Deterministic ID generation for ARS objects. IDs are derived from content
## so the same input always produces the same JSON (testable, diff-friendly,
## not random UUIDs).

#' Slugify a string for use in an ARS id (uppercase, underscores, ASCII only)
#' @noRd
.slug <- function(x) {
  if (is.null(x) || !nzchar(x)) return("UNSPECIFIED")
  s <- toupper(as.character(x))
  s <- gsub("[^A-Z0-9]+", "_", s, perl = TRUE)
  s <- gsub("^_+|_+$", "", s)
  if (!nzchar(s)) "UNSPECIFIED" else s
}

#' AnalysisSet ID from population name
#' @noRd
make_analysis_set_id <- function(population_name) {
  paste0("AS_", .slug(population_name))
}

#' DataSubset ID from a short tag describing the subset condition
#' @noRd
make_data_subset_id <- function(tag) {
  paste0("DS_", .slug(tag))
}

#' GroupingFactor ID from the by-variable name
#' @noRd
make_grouping_id <- function(by_variable) {
  paste0("GF_", .slug(by_variable))
}

#' Per-level Group ID inside a GroupingFactor (condition-defined columns)
#' @noRd
make_group_id <- function(variable, label) {
  paste0("GRP_", .slug(variable), "_", .slug(label))
}

#' AnalysisMethod ID from method name
#' @noRd
make_method_id <- function(method_name) {
  paste0("MTH_", .slug(method_name))
}

#' Analysis ID from TLF number and a per-TLF analysis index
#' @noRd
make_analysis_id <- function(tlf_number, index) {
  sprintf("AN_%s_%03d", .slug(tlf_number), as.integer(index))
}

#' Output ID from TLF number ("T-14-1-1" -> "T_14_1_1")
#' @noRd
make_output_id <- function(tlf_number) {
  .slug(tlf_number)
}
