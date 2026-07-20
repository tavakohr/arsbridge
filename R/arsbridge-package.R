#' arsbridge: convert annotated TLF shells to CDISC ARS v1.0 JSON
#'
#' arsbridge reads a lead programmer's already-annotated TLF shell Word
#' document and the study's ADaM specification Excel, and produces a valid
#' CDISC Analysis Results Standard (ARS) v1.0 ARM-TS JSON file consumable by
#' [siera::readARS()].
#'
#' Annotation reading is style-agnostic and uses two passes together to
#' extract as many annotation variants as possible: a deterministic
#' four-layer regex detector (font colour, character formatting, brackets,
#' plain text) and an LLM primary reader that separates display label from
#' variable reference in layouts no regex was written for. A row is read if
#' either pass finds it; on conflict the LLM wins and a warning flags it.
#'
#' Core principle: the package extracts and converts -- it does not invent.
#' Every LLM-proposed variable passes a hard gate against the ADaM
#' specification, so a variable absent from the spec is rejected and logged,
#' never shipped. Every variable in the ARS output traces back to a real
#' annotation grounded in the study's ADaM spec. See the "How arsbridge reads
#' an annotated shell" vignette.
#'
#' @section Working without an API key:
#' The LLM is opt-in. `spec_to_ars()` runs in one of three tiers:
#' **deterministic** (the default -- shell + spec only, regex plus heuristics,
#' no LLM call even if a key is configured), **supplement** (a JSON file a chat
#' assistant such as Copilot produces from the uploaded shell + spec, fed via
#' `spec_to_ars(supplement =)` with no API call), or **llm** (opt in with
#' `spec_to_ars(use_llm = TRUE)` and a configured key). A missing key never
#' stops a run and never raises a key-related warning. Start the no-API
#' supplement path with [ars_copilot_instructions()]; see
#' `vignette("no-api-access")`.
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom stats setNames
#' @importFrom utils modifyList packageVersion head tail
#' @importFrom rlang .data
NULL
