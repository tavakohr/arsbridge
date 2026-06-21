# arsbridge 0.1.0

* Initial release.
* Classification wiring (ADR 0001): a capability-gated table is no longer
  reserved wholesale. `build_ars_json()` now classifies which of its statistics
  arsbridge can compute (deterministic keyword scan of the section's title,
  footnotes, and labels) and builds a *partial* section -- the descriptive rows
  compute, and each detected executable method (a Clopper-Pearson CI; a CMH
  p-value when "stratified by <VAR>" names a strata variable) is appended as its
  own analysis with operands. Only the residual indicators it still cannot
  compute (e.g. a Newcombe difference) are reserved as `manual_pending` and
  named on the placeholder. With no residual, the table is no longer flagged
  unsupported at all -- it renders with the computed CI / CMH cells. An LLM
  enrichment can supersede the keyword layer later.
* Second executable descriptor: Cochran-Mantel-Haenszel p-value (ADR 0001). New
  exported `ard_cmh_test()` wraps base R's `stats::mantelhaen.test()` (the cardx
  wrapper is not used) and returns the CMH p-value as a one-row ARD. When a
  `MTH_CMH_TEST` analysis carries a stratification operand (`strata` on the
  analysis, resolved against the data), arsbridge emits an
  `arsbridge::ard_cmh_test()` call and computes the p-value (`value_source =
  "stats"`); with no resolvable strata it degrades to a `manual_pending` stub.
  The executable-method registry is now general (`.EXEC_DESCRIPTORS`: a
  `value_source` plus an `available(res)` predicate per method), replacing the
  cardx-only flag. `resolve_analysis()` carries the new `strata` operand.
* First executable descriptor: exact (Clopper-Pearson) binomial CI (ADR 0001).
  When `{cardx}` is installed, the `MTH_PROPORTION_CI_EXACT` method is no longer
  reserved as a manual cell -- arsbridge emits a `cardx::ard_categorical_ci()`
  call and computes the per-arm CIs like any other result (`value_source =
  "cardx"`). It needs no operand beyond the response variable and the treatment
  grouping. Without `{cardx}` the same cell degrades gracefully to a
  `manual_pending` stub. Cochran-Mantel-Haenszel and the Newcombe difference
  stay reserve-only until their stratification / reference-group operands are
  carried through the spec. `{cardx}` is a soft dependency (Suggests).
* Manual-fill round-trip + guard (ADR 0002, phase 5). After computing a reserved
  `manual_pending` cell with a validated script, the analyst writes the value
  back into the ARD row (`stat`, `result_status = "manual_filled"`,
  `value_source`, `derivation_ref`) -- the ARD is a diffable, auditable data
  frame. New `ars_validate_manual_fills()` flags any `manual_filled` cell that
  has no `derivation_ref` or no value; `ars_render_all()` raises each as a
  blocker before rendering, so an untraceable manual number can never ship. A
  filled cell then renders its value like any other. See
  `vignette("getting-started")` for the round-trip. ADR 0002 is now fully
  implemented (phases 1-5).
* Partial table rendering (ADR 0002, phase 4). An output that arsbridge can
  compute only in part now renders: the computable cells are filled and each
  reserved `manual_pending` cell renders as a loud `[‡ manual]` marker (never
  blank, never `NA`, never a number), keyed to a table footnote. An output with
  no computable cell at all stays a whole-table numbered placeholder, which now
  also names the reserved cells. The render manifest flags a partial table as
  `partial -- manual cells reserved`.
* Partial-results traceability (ADR 0002, phases 1-3). `ars_to_ard()` now stamps
  every row with provenance columns (`result_status`, `value_source`,
  `derivation_ref`, `derived_by`, `derived_dt`). A declared-but-unexecutable
  method (e.g. `MTH_CMH_TEST` -- a statistic describable in the ARS but with no
  `{cards}`/`{cardx}` executor) no longer skips or coerces the analysis: it
  reserves keyed `manual_pending` stub ARD rows (`stat = NA`) so the table cell
  keeps a slot tied to its analysis/method/output. A later validated manual
  computation fills that slot rather than typing an orphan value into the
  rendered output. New `ars_manual_worklist()` lists every pending cell as the
  analyst's checklist. The capability gate no longer strips analyses from a
  gated table: the ARS keeps the analysis and a declarative
  `MTH_UNSUPPORTED_ANALYSIS` method (flagged `supported = FALSE` with the
  capability reason), so the Output -> Analysis -> Method chain is intact and
  the engine reserves a stub cell for it. The renderer still emits a numbered
  placeholder until partial rendering (phase 4) lands. Additive only --
  computed results are unchanged.
* Architecture decision records under `adr/`: ADR 0001 sets the
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
  `adr/0001-statistical-method-extensibility.md`.
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
