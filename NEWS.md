# arsbridge 0.1.0

* Initial release.
* Hybrid shell reading: a deterministic four-layer regex detector and an
  LLM primary reader (`extract_shell_llm()`) run together and take the
  union, to extract as many annotation variants as possible. Every
  LLM-proposed `DATASET.VARIABLE` passes a hard ADaM-spec gate -- out-of-spec
  proposals are rejected and logged as blockers, never shipped. With no API
  key the reader degrades to the deterministic pass. See
  `vignette("reading-engine")`.
* Provider registry (`R/llm_providers.R`): Anthropic, OpenAI, Gemini, and
  OpenAI-compatible providers such as GLM are defined in one place. Adding a
  provider is a single entry. New generic `set_llm_key()` setter; select the
  active provider with `ARS_LLM_PROVIDER`.
* `spec_to_ars()`: parse an annotated TLF shell `.docx` plus an ADaM
  specification (`define.xml` or Excel) into CDISC Analysis Results
  Standard (ARS) v1.0 JSON.
* `ars_to_ard()`: execute an ARS JSON natively into a tidy Analysis
  Results Data (ARD) object via `{cards}`, applying `analysisSets` and
  `dataSubsets` filters against `.xpt` / `.csv` datasets.
* `ars_render_tlf()`, `ars_render_all()`, `ars_to_tfrmt()`: render ARS
  outputs to publication-ready GT and Word tables via `{tfrmt}`.
* Multi-provider LLM enrichment (Anthropic, OpenAI, Gemini) with a
  keyword-heuristic fallback so the pipeline runs without an API key.
* `ars_diagnostics()` / `ars_blockers()`: plain-English diagnostics that
  point the user at the input document to fix.
