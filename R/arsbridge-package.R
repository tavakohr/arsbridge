#' arsbridge: convert annotated TLF shells to CDISC ARS v1.0 JSON
#'
#' arsbridge reads a lead programmer's already-annotated TLF shell Word
#' document and the study's ADaM specification Excel, and produces a valid
#' CDISC Analysis Results Standard (ARS) v1.0 ARM-TS JSON file consumable by
#' [siera::readARS()].
#'
#' Annotation reading is style-agnostic, and runs in two stages. The
#' deterministic four-layer regex detector (font colour, character formatting,
#' brackets, plain text) ALWAYS runs and is a fully supported mode on its own.
#' On top of it, at most one gap-filler applies, in this order of preference:
#' an offline **supplement** -- a reviewed JSON file, typically produced by a
#' chat assistant inside a closed environment, applied with no API key and no
#' network -- or, when no supplement is supplied and a provider key is
#' configured, an optional **live LLM** pass via the `ellmer` package. A
#' supplement always wins: supplying one makes no live call whatever `use_llm`
#' says. Where a gap-filler runs, a row is read if either it or the regex finds
#' it, and the gap-filler wins a conflict with a warning flagging it.
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
#' `spec_to_ars(use_llm = TRUE)`, a configured key, and the optional `ellmer`
#' package installed). A missing key never stops a run and never raises a
#' key-related warning; `ellmer` is asked for only once a run has resolved
#' that it will actually call an LLM. Start the no-API
#' supplement path with [ars_copilot_instructions()]; see
#' `vignette("no-api-access")`.
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom stats setNames
#' @importFrom utils modifyList packageVersion head tail
#' @importFrom rlang .data
NULL
