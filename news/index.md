# Changelog

## arsbridge (development version)

- **The validation report now carries a `Legend` sheet.**
  `spec_validation_report.xlsx` gains a final worksheet that names each
  status/severity, its meaning, and the exact fill hex it is tinted with
  (PASS `E2EFDA`, WARN `FFF2CC`, FAIL `FCE4D6`, INFO `DDEBF7`). The same
  legend is documented in the README. The tint palette is now a single
  constant so the key can never drift from the report.

- **The Copilot supplement workflow is hardened against invalid JSON.**
  The most common failure – a value quoted with double quotes (e.g.
  `MHSCAT="UNDERLYING CONDITIONS"`), which breaks the JSON – is now
  called out explicitly in the instruction file (single quotes inside
  every value, plus a “before you send” self-check), and
  `read_supplement()`’s error now names that cause and the single-quote
  fix.

- **The supplement now confirms the correct set of tables.** A Copilot
  supplement may carry a `title` per TLF (the instruction file now asks
  the assistant to enumerate every output with its exact title).
  [`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
  cross-checks that inventory against what it parsed and records
  non-blocking WARNs for a supplement entry that matches no parsed
  table, a parsed table the supplement never mentions, and a title that
  disagrees between the two – so a wrong or incomplete table set
  surfaces for review. When the shell heading gave no title but the
  supplement has one, the parsed section adopts it (INFO). `title` is
  optional and backward compatible (no version bump); a supplement
  without one still runs, and
  [`ars_validate_supplement()`](https://tavakohr.github.io/arsbridge/reference/ars_validate_supplement.md)
  suggests adding it.

- **One-line TLF headings are now read deterministically.** The shell
  parser previously recognised an inline title only after a literal
  colon (`Table 14.1.1: Title`). It now also reads a colon-less one-line
  heading that packs the number, title, a dash-separated population, an
  inline annotation, and a `[PROGRAMMING DATASETS USED: ...]` suffix
  into a single paragraph – e.g.
  `Table 14.1.1 Summary of Disposition - Screened Subjects ADSL.SCRNFL='Y' [PROGRAMMING DATASETS USED: ADSL]`.
  The title, population, population annotation, and source datasets are
  split out of that line. Recognition stays conservative: ordinary prose
  that mentions a table number (`Table 14.1.1 shows the summary`),
  cross-references (`See Table 14.1.1 ...`), table-of-contents entries,
  and bare section numbers (`14.1 Demographic and Baseline Tables`) are
  still not headings.

- Annotation values written with straight or smart **double quotes**
  (`ADSL.SCRNFL="Y"`) and **unquoted numeric equality**
  (`ADSL.COHORTN=1`, common in column headers) are now detected.
  Captured values are canonicalized to single quotes so the emitted ARS
  JSON stays uniform regardless of the shell’s quote style. Text is
  Unicode-normalized before matching (non-breaking spaces, zero-width
  characters, and smart quotes), while en/em dashes are preserved as
  meaningful title separators.

- New `spec_to_ars(heading_patterns = ...)` escape hatch: a character
  vector of PCRE patterns (with named `number`/`type`/`title` groups)
  tried before the built-in grammars, for sponsor shells whose headings
  the built-ins do not recognise – no package edit required.

- When no TLF sections are found, the warning and the
  [`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
  abort now list the heading-shaped lines that were seen and rejected,
  with the reason for each, and repeat a one-line recommendation for how
  to write an identifiable heading before pointing at
  `heading_patterns`.

- New WARN when a heading’s number is found but **no title text** is
  identified (e.g. a bare `Table 14.1.1` with the title stranded in a
  text box): the section is kept but flagged with the same
  how-to-write-an- identifiable-heading guidance, so a missing title is
  surfaced rather than shipped silently.

- Documented the recommended heading convention in one place –
  [`?spec_to_ars`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
  gains a “Writing identifiable TLF headings” section, and the README
  gains a “TLF heading format” section – so the guidance the error and
  warning messages give matches the docs.

- The cosmetic “Undefined namespace prefix” warning that `officer` emits
  while reading `docProps/core.xml` in some e-signed (DocuSign) shells
  is now muffled; every other warning still surfaces.

## arsbridge 0.1.0

- **The LLM tier is now opt-in.**
  [`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
  gains `use_llm` (default `FALSE`): by default the pipeline runs
  regex-only (deterministic) and makes NO live LLM call, *even when an
  API key is configured*. Pass `use_llm = TRUE` to use the LLM for
  extraction and enrichment when a key is available. This makes regex
  the first-class default and the LLM an explicit choice – ideal for CI,
  automation, and regex baselines. (A `supplement` still takes
  precedence; it also makes no live LLM calls.) **Breaking:** callers
  that relied on a configured key auto-selecting the LLM must now pass
  `use_llm = TRUE`.

- Deterministic (regex) and supplement (Copilot) runs are fully silent
  about API keys:
  [`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
  never asks for a key nor raises a key-related error or warning in
  those modes. The old “running in deterministic mode” WARN is now a
  neutral INFO provenance note, and the “no API key?” console nudge is
  gone. Genuine, table-specific findings (e.g. a capability blocker for
  an inferential table) are unaffected and still surface in every mode.

- Three-tier reading engine; the LLM API key is now optional
  (`R/spec_to_ars.R`, `R/supplement.R`).
  [`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
  no longer aborts without a key: it resolves a mode from what you have
  —

  - **deterministic** (shell + spec only): regex + keyword heuristics,
    one `WARN` recording the reduced accuracy;
  - **supplement** (`spec_to_ars(supplement = "supplement.json")`): a
    JSON file produced by a chat assistant from the uploaded shell +
    spec fills the annotations the regex could not find and supplies
    per-TLF enrichment, with **no API call**;
  - **llm** (API key set): unchanged live behaviour.

  New exports:
  [`ars_copilot_instructions()`](https://tavakohr.github.io/arsbridge/reference/ars_copilot_instructions.md)
  writes the static, versioned instruction file to upload to
  Copilot/ChatGPT alongside the shell and spec;
  [`ars_validate_supplement()`](https://tavakohr.github.io/arsbridge/reference/ars_validate_supplement.md)
  pre-flights the reply. Supplement bindings fill gaps only — authored
  shell annotations win any disagreement (`WARN`) — and every proposed
  variable passes the same hard ADaM-spec gate as a live LLM proposal.
  The tier is recorded in `_meta.extraction_mode` of the ARS JSON and in
  the
  [`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
  result. See
  [`vignette("no-api-access")`](https://tavakohr.github.io/arsbridge/articles/no-api-access.md).

  [`ars_copilot_instructions()`](https://tavakohr.github.io/arsbridge/reference/ars_copilot_instructions.md)
  copies the instruction file shipped inside the installed package
  (`inst/copilot/`) into the working directory (creating the target
  folder if needed), so users never touch the internal package path. The
  no-API path is now cross-referenced from
  [`?arsbridge`](https://tavakohr.github.io/arsbridge/reference/arsbridge-package.md),
  [`?spec_to_ars`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md),
  every `?set_*_key` /
  [`?get_active_llm`](https://tavakohr.github.io/arsbridge/reference/get_active_llm.md)
  help page, the README (including an install-time pointer), and the
  `getting-started` vignette.

- Shell-parsing robustness for cross-sponsor variation
  (`R/parse_shell_docx.R`, robustness findings F1-F4). The shell reader
  now tolerates inline headings (`Table 14.1.1: Title`), two-line
  titles, listings with no population line, page-header-stored
  titles/populations, `gridSpan`/`vMerge` merged cells and multi-row
  headers, and Word comments, highlights, tracked changes, and text
  boxes as annotation channels. Pre-merge hardening:

  - A page-header title/population is adopted only when the header’s TLF
    number matches the body section’s; a mismatch (stale template
    header, or a header belonging to another TLF) is refused with a WARN
    instead of silently mislabelling the section.
  - A multi-row nested header with no `<w:tblHeader/>` flag is inferred
    from the spanned first row (so the subcolumn labels survive and no
    ghost stub row is produced), with a WARN that the header was a
    heuristic guess.
  - A treatment-column mapping line (`Treatment columns -> ADSL.TRT01A`)
    placed right after the title is no longer misread as the population;
    it now reaches `bind_annotations()` as the column-axis grouping. A
    paragraph with no population wording counts as the population only
    when its annotation is a population-flag reference (`...FL='Y'`).
  - A pre-table footnote (`Note: ...`) between title and table is kept
    as a footnote instead of being glued onto the title.
  - A Word comment carrying an annotation is bound even when it is
    anchored to a data cell rather than the stub cell.
  - Fuzzy stub-label matching no longer lets a one/two-character label
    (`n`, `%`) substring-match an unrelated longer phrase.
  - Known limitations, consciously deferred: page headers are read only
    for single-section documents (a multi-section docx with per-section
    headers is not attempted); the annotation highlight-exclusion list
    is `none`/`black` only; the text-box fixture uses the direct
    `w:txbxContent` shape rather than Word’s `mc:AlternateContent`
    wrapper.

- Shell layout fidelity (ADR 0003, phases 1-5). arsbridge now carries a
  first-class model of the authored table layout from the annotated
  shell all the way to the rendered output:

  - *Footnote/annotation split.* Programmer annotation lines outside the
    stub cells (coloured runs, ADaM-pattern text, or
    `Label -> DATASET.VAR` arrow paragraphs below a table) are routed to
    `programmer_annotations` and never shipped as footnotes.
    `spec_to_ars(ship_annotations = FALSE)` is the default; `TRUE`
    re-attaches them for debugging.
  - *Convention-agnostic binding.* New `bind_annotations()`
    fuzzy-matches each below-table `Label -> annotation` line back to
    its stub row (in-cell detections still win), splits multi-label
    lines
    (`Completed / Discontinued -> ADSL.EOSSTT (COMPLETED / DISCONTINUED)`)
    into per-row value filters, and captures a
    `Treatment columns -> ADSL.TRT01A` line as the authoritative
    column-axis grouping.
  - *Layout persistence + no-drop.* `build_ars_json()` walks every
    authored stub row in order: annotated rows become analyses whose
    method is inferred deterministically from the annotation form (count
    expression -\> subject count; `VAR='val'` -\> filtered subject
    count; bare variable -\> categorical or continuous per the ADaM
    spec), label-only rows are kept as layout entries, and an annotated
    row whose variable cannot resolve is reserved as a traceable
    `manual_pending` analysis instead of being dropped. The ordered
    layout is persisted per output as arsbridge-private
    `_meta.shell_layout`, alongside `_meta.source_datasets`.
  - *Layout-driven rendering + column restriction.* When
    `_meta.shell_layout` is present,
    [`ars_render_tlf()`](https://tavakohr.github.io/arsbridge/reference/ars_render_tlf.md)
    builds the stub from the authored labels (joined to the ARD by
    `analysis_id`), pins the authored row order, expands
    categorical/continuous analyses beneath their authored label,
    renders missing rows blank (never dropped), and restricts the
    treatment columns to the arm levels named in the shell headers – a
    population level like “Screen Failure” in `TRT01A` no longer leaks
    in as a treatment column. Outputs without the layout metadata render
    exactly as before.
  - *Listings/figures.* A LISTING section always emits `MTH_LISTING`
    analyses regardless of the LLM’s analysis-type guess, and
    [`ars_render_figure()`](https://tavakohr.github.io/arsbridge/reference/ars_render_figure.md)
    now resolves its default dataset from the shell’s `Source:` line
    (`_meta.source_datasets`) instead of assuming `ADEFF`.
  - Fixed a tfrmt warning (“Unable to apply `frmt_combine` due to
    uniqueness of column/row identifiers”) by using named
    single-parameter formats instead of one-parameter `frmt_combine()`
    in generated body plans.

- Initial release.

- Classification wiring (ADR 0001): a capability-gated table is no
  longer reserved wholesale. `build_ars_json()` now classifies which of
  its statistics arsbridge can compute (deterministic keyword scan of
  the section’s title, footnotes, and labels) and builds a *partial*
  section – the descriptive rows compute, and each detected executable
  method (a Clopper-Pearson CI; a CMH p-value when “stratified by
  `” names a strata variable) is appended as its own analysis with operands. Only the residual indicators it still cannot compute (e.g. a Newcombe difference) are reserved as ``manual_pending`` and named on the placeholder. With no residual, the table is no longer flagged unsupported at all – it renders with the computed CI / CMH cells. An LLM enrichment can supersede the keyword layer later.`

- Second executable descriptor: Cochran-Mantel-Haenszel p-value (ADR
  0001). New exported
  [`ard_cmh_test()`](https://tavakohr.github.io/arsbridge/reference/ard_cmh_test.md)
  wraps base R’s
  [`stats::mantelhaen.test()`](https://rdrr.io/r/stats/mantelhaen.test.html)
  (the cardx wrapper is not used) and returns the CMH p-value as a
  one-row ARD. When a `MTH_CMH_TEST` analysis carries a stratification
  operand (`strata` on the analysis, resolved against the data),
  arsbridge emits an
  [`arsbridge::ard_cmh_test()`](https://tavakohr.github.io/arsbridge/reference/ard_cmh_test.md)
  call and computes the p-value (`value_source = "stats"`); with no
  resolvable strata it degrades to a `manual_pending` stub. The
  executable-method registry is now general (`.EXEC_DESCRIPTORS`: a
  `value_source` plus an `available(res)` predicate per method),
  replacing the cardx-only flag. `resolve_analysis()` carries the new
  `strata` operand.

- First executable descriptor: exact (Clopper-Pearson) binomial CI (ADR
  0001). When [cardx](https://github.com/insightsengineering/cardx) is
  installed, the `MTH_PROPORTION_CI_EXACT` method is no longer reserved
  as a manual cell – arsbridge emits a
  [`cardx::ard_categorical_ci()`](https://rdrr.io/pkg/cardx/man/ard_categorical_ci.html)
  call and computes the per-arm CIs like any other result
  (`value_source = "cardx"`). It needs no operand beyond the response
  variable and the treatment grouping. Without
  [cardx](https://github.com/insightsengineering/cardx) the same cell
  degrades gracefully to a `manual_pending` stub.
  Cochran-Mantel-Haenszel and the Newcombe difference stay reserve-only
  until their stratification / reference-group operands are carried
  through the spec.
  [cardx](https://github.com/insightsengineering/cardx) is a soft
  dependency (Suggests).

- Manual-fill round-trip + guard (ADR 0002, phase 5). After computing a
  reserved `manual_pending` cell with a validated script, the analyst
  writes the value back into the ARD row (`stat`,
  `result_status = "manual_filled"`, `value_source`, `derivation_ref`) –
  the ARD is a diffable, auditable data frame. New
  [`ars_validate_manual_fills()`](https://tavakohr.github.io/arsbridge/reference/ars_validate_manual_fills.md)
  flags any `manual_filled` cell that has no `derivation_ref` or no
  value;
  [`ars_render_all()`](https://tavakohr.github.io/arsbridge/reference/ars_render_all.md)
  raises each as a blocker before rendering, so an untraceable manual
  number can never ship. A filled cell then renders its value like any
  other. See
  [`vignette("getting-started")`](https://tavakohr.github.io/arsbridge/articles/getting-started.md)
  for the round-trip. ADR 0002 is now fully implemented (phases 1-5).

- Partial table rendering (ADR 0002, phase 4). An output that arsbridge
  can compute only in part now renders: the computable cells are filled
  and each reserved `manual_pending` cell renders as a loud `[‡ manual]`
  marker (never blank, never `NA`, never a number), keyed to a table
  footnote. An output with no computable cell at all stays a whole-table
  numbered placeholder, which now also names the reserved cells. The
  render manifest flags a partial table as
  `partial -- manual cells reserved`.

- Partial-results traceability (ADR 0002, phases 1-3).
  [`ars_to_ard()`](https://tavakohr.github.io/arsbridge/reference/ars_to_ard.md)
  now stamps every row with provenance columns (`result_status`,
  `value_source`, `derivation_ref`, `derived_by`, `derived_dt`). A
  declared-but-unexecutable method (e.g. `MTH_CMH_TEST` – a statistic
  describable in the ARS but with no
  [cards](https://github.com/insightsengineering/cards)/[cardx](https://github.com/insightsengineering/cardx)
  executor) no longer skips or coerces the analysis: it reserves keyed
  `manual_pending` stub ARD rows (`stat = NA`) so the table cell keeps a
  slot tied to its analysis/method/output. A later validated manual
  computation fills that slot rather than typing an orphan value into
  the rendered output. New
  [`ars_manual_worklist()`](https://tavakohr.github.io/arsbridge/reference/ars_manual_worklist.md)
  lists every pending cell as the analyst’s checklist. The capability
  gate no longer strips analyses from a gated table: the ARS keeps the
  analysis and a declarative `MTH_UNSUPPORTED_ANALYSIS` method (flagged
  `supported = FALSE` with the capability reason), so the Output -\>
  Analysis -\> Method chain is intact and the engine reserves a stub
  cell for it. The renderer still emits a numbered placeholder until
  partial rendering (phase 4) lands. Additive only – computed results
  are unchanged.

- Architecture decision records under `adr/`: ADR 0001 sets the
  statistical-method extensibility boundary (bound the boundary, not the
  contents – descriptor contract on the shared ARD shape, tiered honest
  degradation, deterministic emission with the LLM only classifying);
  ADR 0002 proposes partial results with intact traceability (reserved
  stub ARD rows + provenance columns so a cell arsbridge cannot compute
  keeps a keyed slot for a validated manual fill, never an orphan
  value). ADR 0002 is a plan, not yet implemented.

- Capability gate: tables needing inferential or model-based methods
  (Cochran-Mantel-Haenszel, Clopper-Pearson / Newcombe intervals,
  p-values, odds/hazard ratios, regression, ANCOVA/MMRM, NRI imputation)
  are detected (LLM + keyword scan), raised as blockers, and NOT coerced
  into a meaningless count. They are carried to the final output as a
  numbered placeholder so the table numbering still matches the shell
  exactly. The placeholder now reads as an intentional capability gate
  (not a bug) and points to a separate validated analysis script; render
  *failures* emit a distinct placeholder clearly labelled as an error,
  not a gate. The rationale and the path to extending coverage are
  recorded in `adr/0001-statistical-method-extensibility.md`.

- Hybrid shell reading: a deterministic four-layer regex detector and an
  LLM primary reader (`extract_shell_llm()`) run together and take the
  union, to extract as many annotation variants as possible. Every
  LLM-proposed `DATASET.VARIABLE` passes a hard ADaM-spec gate –
  out-of-spec proposals are rejected and logged as blockers, never
  shipped. With no API key the reader degrades to the deterministic
  pass. See
  [`vignette("reading-engine")`](https://tavakohr.github.io/arsbridge/articles/reading-engine.md).

- Provider registry (`R/llm_providers.R`): Anthropic, OpenAI, Gemini,
  and OpenAI-compatible providers such as GLM are defined in one place.
  Adding a provider is a single entry. New generic
  [`set_llm_key()`](https://tavakohr.github.io/arsbridge/reference/set_llm_key.md)
  setter; select the active provider with `ARS_LLM_PROVIDER`.

- [`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md):
  parse an annotated TLF shell `.docx` plus an ADaM specification
  (`define.xml` or Excel) into CDISC Analysis Results Standard (ARS)
  v1.0 JSON.

- [`ars_to_ard()`](https://tavakohr.github.io/arsbridge/reference/ars_to_ard.md):
  execute an ARS JSON natively into a tidy Analysis Results Data (ARD)
  object via [cards](https://github.com/insightsengineering/cards),
  applying `analysisSets` and `dataSubsets` filters against `.xpt` /
  `.csv` datasets.

- [`ars_render_tlf()`](https://tavakohr.github.io/arsbridge/reference/ars_render_tlf.md),
  [`ars_render_all()`](https://tavakohr.github.io/arsbridge/reference/ars_render_all.md),
  [`ars_to_tfrmt()`](https://tavakohr.github.io/arsbridge/reference/ars_to_tfrmt.md):
  render ARS outputs to publication-ready GT and Word tables via
  [tfrmt](https://GSK-Biostatistics.github.io/tfrmt/).

- Multi-provider LLM enrichment (Anthropic, OpenAI, Gemini) with a
  keyword-heuristic fallback so the pipeline runs without an API key.

- [`ars_diagnostics()`](https://tavakohr.github.io/arsbridge/reference/ars_diagnostics.md)
  /
  [`ars_blockers()`](https://tavakohr.github.io/arsbridge/reference/ars_blockers.md):
  plain-English diagnostics that point the user at the input document to
  fix.
