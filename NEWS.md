# arsbridge 0.1.0

* Initial release.
* Partial-results traceability (ADR 0002, phases 1-2). `ars_to_ard()` now stamps
  every row with provenance columns (`result_status`, `value_source`,
  `derivation_ref`, `derived_by`, `derived_dt`). A declared-but-unexecutable
  method (e.g. `MTH_CMH_TEST` -- a statistic describable in the ARS but with no
  `{cards}`/`{cardx}` executor) no longer skips or coerces the analysis: it
  reserves keyed `manual_pending` stub ARD rows (`stat = NA`) so the table cell
  keeps a slot tied to its analysis/method/output. A later validated manual
  computation fills that slot rather than typing an orphan value into the
  rendered output. New `ars_manual_worklist()` lists every pending cell as the
  analyst's checklist. Additive only -- computed results are unchanged.
* Architecture decision records under `docs/adr/`: ADR 0001 sets the
  statistical-method extensibility boundary (bound the boundary, not the
  contents -- descriptor contract on the shared ARD shape, tiered honest
  degradation, deterministic emission with the LLM only classifying); ADR 0002
  proposes partial results with intact traceability (reserved stub ARD rows +
  provenance columns so a cell arsbridge cannot compute keeps a keyed slot for
  a validated manual fill, never an orphan value). ADR 0002 is a plan, not yet
  implemented.
* Capability gate: tables needing inferential or model-based methods
  (Cochran-Mantel-Haenszel, Clopper-Pearson / Newcombe intervals, p-values,
  odds/hazard ratios, regression, ANCOVA/MMRM, NRI imputation) are detected
  (LLM + keyword scan), raised as blockers, and NOT coerced into a
  meaningless count. They are carried to the final output as a numbered
  placeholder so the table numbering still matches the shell exactly. The
  placeholder now reads as an intentional capability gate (not a bug) and
  points to a separate validated analysis script; render *failures* emit a
  distinct placeholder clearly labelled as an error, not a gate. The
  rationale and the path to extending coverage are recorded in
  `docs/adr/0001-statistical-method-extensibility.md`.
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
