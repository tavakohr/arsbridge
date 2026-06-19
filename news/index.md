# Changelog

## arsbridge 0.1.0

- Initial release.
- Hybrid shell reading: a deterministic four-layer regex detector and an
  LLM primary reader (`extract_shell_llm()`) run together and take the
  union, to extract as many annotation variants as possible. Every
  LLM-proposed `DATASET.VARIABLE` passes a hard ADaM-spec gate –
  out-of-spec proposals are rejected and logged as blockers, never
  shipped. With no API key the reader degrades to the deterministic
  pass. See
  [`vignette("reading-engine")`](../articles/reading-engine.md).
- Provider registry (`R/llm_providers.R`): Anthropic, OpenAI, Gemini,
  and OpenAI-compatible providers such as GLM are defined in one place.
  Adding a provider is a single entry. New generic
  [`set_llm_key()`](../reference/set_llm_key.md) setter; select the
  active provider with `ARS_LLM_PROVIDER`.
- [`spec_to_ars()`](../reference/spec_to_ars.md): parse an annotated TLF
  shell `.docx` plus an ADaM specification (`define.xml` or Excel) into
  CDISC Analysis Results Standard (ARS) v1.0 JSON.
- [`ars_to_ard()`](../reference/ars_to_ard.md): execute an ARS JSON
  natively into a tidy Analysis Results Data (ARD) object via
  [cards](https://github.com/insightsengineering/cards), applying
  `analysisSets` and `dataSubsets` filters against `.xpt` / `.csv`
  datasets.
- [`ars_render_tlf()`](../reference/ars_render_tlf.md),
  [`ars_render_all()`](../reference/ars_render_all.md),
  [`ars_to_tfrmt()`](../reference/ars_to_tfrmt.md): render ARS outputs
  to publication-ready GT and Word tables via
  [tfrmt](https://GSK-Biostatistics.github.io/tfrmt/).
- Multi-provider LLM enrichment (Anthropic, OpenAI, Gemini) with a
  keyword-heuristic fallback so the pipeline runs without an API key.
- [`ars_diagnostics()`](../reference/ars_diagnostics.md) /
  [`ars_blockers()`](../reference/ars_blockers.md): plain-English
  diagnostics that point the user at the input document to fix.
