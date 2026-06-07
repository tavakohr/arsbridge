#' arsbridge: convert annotated TLF shells to CDISC ARS v1.0 JSON
#'
#' arsbridge reads a lead programmer's already-annotated TLF shell Word
#' document and the study's ADaM specification Excel, and produces a valid
#' CDISC Analysis Results Standard (ARS) v1.0 ARM-TS JSON file consumable by
#' [siera::readARS()]. Annotation extraction is style-agnostic: detects ADaM
#' variable references whether marked by font colour, character formatting,
#' brackets, or plain text appended after the stub label.
#'
#' Core principle: the parser extracts and converts -- it does not invent.
#' Every variable in the ARS output traces back to an annotation written by
#' a lead programmer in the annotated shell.
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom stats setNames
#' @importFrom utils modifyList packageVersion head tail
NULL
