# arsbridge (development version)

* **An ARS reporting event can now be read as editable tables and written
  back losslessly.** `ars_to_model()` turns the nested JSON into one data
  frame per entity pool (analyses, methods, analysis sets, data subsets,
  groupings, outputs), each row carrying the flat fields a reviewer edits
  plus the original untouched node; `model_to_ars()` is its exact inverse.
  Fields the model does not surface -- including `_meta` and any future ARS
  key -- ride along untouched, so an unedited model round-trips to a
  structurally identical event and an edited one differs only where it was
  edited. The two tables of contents are copied verbatim unless a structural
  change means they have to be rebuilt from the outputs. This is the
  foundation of the human review stage between `spec_to_ars()` and
  `ars_to_ard()`.

* **`validate_ars_model()` checks a reporting event for the problems that
  matter before execution.** Every reference resolves (an empty
  `dataSubsetId` correctly means "no subset"), no id is duplicated, and each
  analysis is classified by how `ars_to_ard()` will actually treat its method
  -- computed natively, dependent on a prerequisite, silently falling back to
  the generic summarizer, or reserved for manual computation. Given the ADaM
  spec it also checks datasets and variables exist, and given the annotation
  validation report it reports annotated shell lines that no analysis
  covers -- the lines the generator missed.

* **Spec codelists decode coded categorical variables end to end.**
  `parse_adam_spec()` now reads the spec workbook's Codelists sheet (both the
  `"Codelist Name" / "Term (Code)" / "Decoded Value"` and the
  `"ID" / "Term" / "Order" / "Decoded Value"` header conventions, with
  merged-cell fill-down and a `Used By Variables` fallback link) and, for
  define.xml input, the `CodeList` / `CodeListRef` nodes. The parsed
  codelists ship in the ARS JSON as `_meta$value_decodes`, and both the
  execution engine and the emitted {cards} deliverable derive the coded
  analysis variable as a factor (`factor(as.character(VAR), levels = codes,
  labels = decodes)`) before `cards::ard_categorical()`. The ARD therefore
  shows decoded labels ("DEATH") instead of raw codes ("1"), keeps codelist
  order, and reports EVERY codelist term -- unobserved categories appear
  with n = 0, matching disposition shells that list all reasons. Authored
  level rows (`ADSL.DCSREASN=1` under the parent) are stamped with the
  decoded label (raw code kept as `level_code`) so renderer level matching
  is unaffected. Codelists larger than 15 terms (e.g. COUNTRY) are skipped
  with a WARN so tables never explode into hundreds of zero rows.

* **Column groups fall back to the spec codelist.** When a grouping
  variable's column headers carry no condition annotations (the
  `GF_<VAR>` `groups: []` case that rendered coded column labels), the
  grouping factor's per-level groups are now derived from the variable's
  codelist -- decoded label, `EQ` term condition, codelist order -- with a
  WARN asking for review. Header-annotated groups always win.

* **BREAKING: supplement format version 3 -- typed CDISC ARS conditions.**
  The no-API supplement now carries every filter, population, and column
  condition as a typed ARS `WhereClause` object
  (`{condition: {dataset, variable, comparator, value}}` /
  `{compoundExpression: {logicalOperator, whereClauses}}`) instead of a string.
  This ends the string-parsing fragility (double `=`, smart quotes, `OR`,
  `="..."` value repair) that caused real-world extraction failures. Per-TLF
  entries gain typed `analysisSet`, `groupings` (with typed group conditions),
  `analyses` (was `bindings`; `variable` is now a `{dataset, variable}` object
  and the filter a typed `whereClause`), plus `listingColumns`, `recordFilter`,
  `sorting`, per-row `methodId`/`parentRowLabel`/`denominator`, `anchors`, and
  `provenance`. `read_supplement()` accepts only `supplement_version` 3 and
  aborts loudly on a v2 file -- regenerate with `ars_copilot_instructions()`.
  A JSON Schema (`inst/schema/arsbridge_supplement_v3.schema.json`) ships with
  the package, is uploaded to the assistant for self-checking, and is used by
  `ars_validate_supplement()` when `jsonvalidate` (new Suggests) is installed.

* **`spec_to_ars(supplement_trust=)` -- configurable conflict resolution.**
  `"fill_gaps"` (default, unchanged) lands a supplement value only where the
  regex left a gap; `"prefer_supplement"` lets a validated, spec-gated
  supplement value override the shell on a conflict, with a WARN recording both
  and the shell original kept as a secondary analysis. The hard ADaM-spec gate
  is never bypassed in either mode. The mode is recorded at
  `_meta.supplement_trust`.

* **Packaged two-phase Copilot workflow.**
  `ars_copilot_instructions(workflow = "two_phase")` writes a Phase-1
  (evidence blueprint) and Phase-2 (semantic construction + repair) instruction
  set for large or complex shells, alongside the single-file workflow. Both
  emit supplement version 3 and are shipped in step with the reader, so the
  instructions and the accepted format can no longer diverge. Each instruction
  file opens with a **"How to run this"** block -- the operator steps (which
  files to attach, what to save) plus a paste-ready prompt for the chat.
  `ars_copilot_instructions()` now returns the vector of paths it wrote.

* **`ars_validate_supplement()` rewritten for v3** with typed-condition checks,
  comparator/enum/arity validation, parent-row resolution, and a paste-ready
  `repair_prompt` attribute that bundles every FAIL for the assistant.

* **Native SAS `.sas7bdat` ADaM cuts are now read everywhere.** The per-TLF
  standalone `{cards}` scripts emitted by `write_tlf_code()`, the execution
  engine (`ars_to_ard()`), and the listing/figure renderers previously loaded
  only `.xpt` and `.csv`; they now also match and read `.sas7bdat` via
  `haven::read_sas()`. Loaders remain case-insensitive on the dataset name, and
  when several formats of the same dataset are present the native SAS formats
  (`.xpt`, then `.sas7bdat`) are preferred over `.csv`.

* **Rewritten Copilot instruction file (extraction guidance version 3).** The
  file `ars_copilot_instructions()` writes is substantially expanded to make a
  chat assistant classify each annotation before emitting JSON: a mandatory
  role taxonomy (population / result-column / displayed-row / listing-column /
  supporting-filter / programming-note), Word label reconstruction rules,
  footnote-mapping resolution, data-type-aware literal quoting, TLF inventory
  reconciliation against the Table of Contents, and per-TLF and final
  validation checklists. The `column_groups` fallback (format version 2) is
  folded into the new column-structure guidance: the assistant supplies
  per-column conditions there only when the shell headers carry no
  machine-readable `DATASET.VARIABLE=value` filter, each `where` written with
  the full dataset prefix. All examples use generic ADaM datasets/variables and
  generic TLF labels.

* **Supplement format version 2: a `column_groups` field.** In the no-API
  supplement workflow, a table whose result columns are values of one variable
  (cohort columns keyed on `ADSL.COHORTN`) could not be expressed when the shell
  header cells carried no machine-readable `DATASET.VAR=value` filter -- the
  format had nowhere to hold the per-column conditions, so the grouping shipped
  with an empty `groups[]`. Each TLF entry may now carry an ordered
  `column_groups` array of `{label, where}` objects; `where` is a full
  condition (`ADSL.COHORTN=1`, `is.na(ADSL.COHORTN)`) using the same grammar as
  an annotated header. `spec_to_ars(supplement = ...)` feeds these into the
  existing group builder, so each becomes one display column (including a
  missing/Unknown bucket) with a WhereClause in the ARS JSON. The shell's own
  header-annotation path still wins when it captured the columns itself; every
  `where` passes the ADaM-spec gate; `ars_validate_supplement()` checks the new
  field. The instruction file `ars_copilot_instructions()` writes is updated to
  version 2 and now tells the assistant to put column conditions here, never to
  fold them into `bindings`. Existing version-1 supplements must be regenerated.

* **Column-header annotations now parse the `is.na()` / `missing()` call
  forms** that annotated shells actually use for a missing/Unknown group --
  R's `is.na(ADSL.COHORTN)` and SAS's `missing(COHORTN)`, plus the negations
  `!is.na(...)` / `not missing(...)`. Previously only the prose
  `DATASET.VAR is missing` form was recognized, so a call-form Unknown-cohort
  header silently failed to parse and its column vanished from the axis. A
  companion coverage check now WARNs when a header names the column-axis
  variable but its annotation does not parse into a condition, reporting how
  many columns were captured versus expected -- so a narrowed axis is
  surfaced rather than shipped quietly.

* **Annotation-defined column axis: per-column filters in table header
  cells.** When two or more column headers carry a filter on the same
  variable -- `Cohort A (N=XX) ADSL.COHORTN=1`, `Cohort B (N=XX)
  ADSL.COHORTN=2`, `Unknown Cohort (N=XX) ADSL.COHORTN is missing` -- each
  condition now becomes one display column, in shell order. This makes a
  merged/derived column (an "Unknown" bucket collecting missing values)
  expressible purely by annotation, with no ADaM change: the engine derives
  the grouping in memory from the conditions, identically in the executed
  ARD and the emitted `{cards}` scripts (a `case_when` factor built from the
  same where-clause predicates). The conditions are carried in the ARS JSON
  as per-level `groups[]` entries with WhereClauses. Rows matching no column
  are excluded from the group columns and counted in a WARN; a
  `Total (N=XX) ...` header is recognized as the overall column and switches
  `includeTotal` on. The annotation grammar also gains the positive
  `DATASET.VAR is missing` / `is null` form and parenthesized
  `IN ('a','b')` value lists.

* **The Copilot instruction file now asks for a downloadable `supplement.json`
  file** (written programmatically with a real JSON serializer) rather than an
  on-screen block, with the fenced block kept as a fallback. Delivering a
  serialized file avoids the copy-paste smart-quote / stray-double-quote /
  truncation errors that a pasted chat block introduces.

* **A supplement with double-quoted where-clause values now loads instead of
  aborting.** The most common Copilot mistake -- a comparison value quoted with
  double quotes (`MHSCAT="UNDERLYING CONDITIONS"`), which breaks the JSON --
  is now auto-repaired to single quotes before parsing. In valid JSON a `"` is
  never preceded by `=`, so `="..."` can only be a value comparison, making the
  rewrite safe; escaped, already-valid quotes are left untouched. The
  instruction file still asks for single quotes, and `read_supplement()`'s error
  still names the fix for any malformation the repair cannot cover.

* **The validation report now carries a `Legend` sheet.** `spec_validation_report.xlsx`
  gains a final worksheet that names each status/severity, its meaning, and the
  exact fill hex it is tinted with (PASS `E2EFDA`, WARN `FFF2CC`, FAIL `FCE4D6`,
  INFO `DDEBF7`). The same legend is documented in the README. The tint palette
  is now a single constant so the key can never drift from the report.

* **The Copilot supplement workflow is hardened against invalid JSON.** The most
  common failure -- a value quoted with double quotes (e.g.
  `MHSCAT="UNDERLYING CONDITIONS"`), which breaks the JSON -- is now called out
  explicitly in the instruction file (single quotes inside every value, plus a
  "before you send" self-check), and `read_supplement()`'s error now names that
  cause and the single-quote fix.

* **The supplement now confirms the correct set of tables.** A Copilot
  supplement may carry a `title` per TLF (the instruction file now asks the
  assistant to enumerate every output with its exact title). `spec_to_ars()`
  cross-checks that inventory against what it parsed and records non-blocking
  WARNs for a supplement entry that matches no parsed table, a parsed table the
  supplement never mentions, and a title that disagrees between the two -- so a
  wrong or incomplete table set surfaces for review. When the shell heading
  gave no title but the supplement has one, the parsed section adopts it
  (INFO). `title` is optional and backward compatible (no version bump); a
  supplement without one still runs, and `ars_validate_supplement()` suggests
  adding it.

* **One-line TLF headings are now read deterministically.** The shell parser
  previously recognised an inline title only after a literal colon
  (`Table 14.1.1: Title`). It now also reads a colon-less one-line heading
  that packs the number, title, a dash-separated population, an inline
  annotation, and a `[PROGRAMMING DATASETS USED: ...]` suffix into a single
  paragraph -- e.g.
  `Table 14.1.1 Summary of Disposition - Screened Subjects ADSL.SCRNFL='Y' [PROGRAMMING DATASETS USED: ADSL]`.
  The title, population, population annotation, and source datasets are split
  out of that line. Recognition stays conservative: ordinary prose that
  mentions a table number (`Table 14.1.1 shows the summary`), cross-references
  (`See Table 14.1.1 ...`), table-of-contents entries, and bare section
  numbers (`14.1 Demographic and Baseline Tables`) are still not headings.

* Annotation values written with straight or smart **double quotes**
  (`ADSL.SCRNFL="Y"`) and **unquoted numeric equality** (`ADSL.COHORTN=1`,
  common in column headers) are now detected. Captured values are
  canonicalized to single quotes so the emitted ARS JSON stays uniform
  regardless of the shell's quote style. Text is Unicode-normalized before
  matching (non-breaking spaces, zero-width characters, and smart quotes),
  while en/em dashes are preserved as meaningful title separators.

* New `spec_to_ars(heading_patterns = ...)` escape hatch: a character vector
  of PCRE patterns (with named `number`/`type`/`title` groups) tried before
  the built-in grammars, for sponsor shells whose headings the built-ins do
  not recognise -- no package edit required.

* When no TLF sections are found, the warning and the `spec_to_ars()` abort
  now list the heading-shaped lines that were seen and rejected, with the
  reason for each, and repeat a one-line recommendation for how to write an
  identifiable heading before pointing at `heading_patterns`.

* New WARN when a heading's number is found but **no title text** is
  identified (e.g. a bare `Table 14.1.1` with the title stranded in a text
  box): the section is kept but flagged with the same how-to-write-an-
  identifiable-heading guidance, so a missing title is surfaced rather than
  shipped silently.

* Documented the recommended heading convention in one place --
  `?spec_to_ars` gains a "Writing identifiable TLF headings" section, and the
  README gains a "TLF heading format" section -- so the guidance the error
  and warning messages give matches the docs.

* The cosmetic "Undefined namespace prefix" warning that `officer` emits while
  reading `docProps/core.xml` in some e-signed (DocuSign) shells is now
  muffled; every other warning still surfaces.

# arsbridge 0.1.0

* **The LLM tier is now opt-in.** `spec_to_ars()` gains `use_llm` (default
  `FALSE`): by default the pipeline runs regex-only (deterministic) and makes
  NO live LLM call, *even when an API key is configured*. Pass `use_llm = TRUE`
  to use the LLM for extraction and enrichment when a key is available. This
  makes regex the first-class default and the LLM an explicit choice -- ideal
  for CI, automation, and regex baselines. (A `supplement` still takes
  precedence; it also makes no live LLM calls.) **Breaking:** callers that
  relied on a configured key auto-selecting the LLM must now pass
  `use_llm = TRUE`.

* Deterministic (regex) and supplement (Copilot) runs are fully silent about
  API keys: `spec_to_ars()` never asks for a key nor raises a key-related
  error or warning in those modes. The old "running in
  deterministic mode" WARN is now a neutral INFO provenance note, and the
  "no API key?" console nudge is gone. Genuine, table-specific findings
  (e.g. a capability blocker for an inferential table) are unaffected and
  still surface in every mode.

* Three-tier reading engine; the LLM API key is now optional
  (`R/spec_to_ars.R`, `R/supplement.R`). `spec_to_ars()` no longer aborts
  without a key: it resolves a mode from what you have —
  * **deterministic** (shell + spec only): regex + keyword heuristics, one
    `WARN` recording the reduced accuracy;
  * **supplement** (`spec_to_ars(supplement = "supplement.json")`): a JSON
    file produced by a chat assistant from the uploaded shell + spec fills
    the annotations the regex could not find and supplies per-TLF
    enrichment, with **no API call**;
  * **llm** (API key set): unchanged live behaviour.

  New exports: `ars_copilot_instructions()` writes the static, versioned
  instruction file to upload to Copilot/ChatGPT alongside the shell and
  spec; `ars_validate_supplement()` pre-flights the reply. Supplement
  bindings fill gaps only — authored shell annotations win any disagreement
  (`WARN`) — and every proposed variable passes the same hard ADaM-spec gate
  as a live LLM proposal. The tier is recorded in
  `_meta.extraction_mode` of the ARS JSON and in the `spec_to_ars()` result.
  See `vignette("no-api-access")`.

  `ars_copilot_instructions()` copies the instruction file shipped inside the
  installed package (`inst/copilot/`) into the working directory (creating the
  target folder if needed), so users never touch the internal package path.
  The no-API path is now cross-referenced from `?arsbridge`, `?spec_to_ars`,
  every `?set_*_key` / `?get_active_llm` help page, the README (including an
  install-time pointer), and the `getting-started` vignette.

* Shell-parsing robustness for cross-sponsor variation
  (`R/parse_shell_docx.R`, robustness findings F1-F4). The shell reader now
  tolerates inline headings (`Table 14.1.1: Title`), two-line titles, listings
  with no population line, page-header-stored titles/populations,
  `gridSpan`/`vMerge` merged cells and multi-row headers, and Word comments,
  highlights, tracked changes, and text boxes as annotation channels.
  Pre-merge hardening:
  - A page-header title/population is adopted only when the header's TLF
    number matches the body section's; a mismatch (stale template header,
    or a header belonging to another TLF) is refused with a WARN instead of
    silently mislabelling the section.
  - A multi-row nested header with no `<w:tblHeader/>` flag is inferred from
    the spanned first row (so the subcolumn labels survive and no ghost stub
    row is produced), with a WARN that the header was a heuristic guess.
  - A treatment-column mapping line (`Treatment columns -> ADSL.TRT01A`)
    placed right after the title is no longer misread as the population; it
    now reaches `bind_annotations()` as the column-axis grouping. A paragraph
    with no population wording counts as the population only when its
    annotation is a population-flag reference (`...FL='Y'`).
  - A pre-table footnote (`Note: ...`) between title and table is kept as a
    footnote instead of being glued onto the title.
  - A Word comment carrying an annotation is bound even when it is anchored
    to a data cell rather than the stub cell.
  - Fuzzy stub-label matching no longer lets a one/two-character label (`n`,
    `%`) substring-match an unrelated longer phrase.
  - Known limitations, consciously deferred: page headers are read only for
    single-section documents (a multi-section docx with per-section headers
    is not attempted); the annotation highlight-exclusion list is
    `none`/`black` only; the text-box fixture uses the direct
    `w:txbxContent` shape rather than Word's `mc:AlternateContent` wrapper.

* Shell layout fidelity (ADR 0003, phases 1-5). arsbridge now carries a
  first-class model of the authored table layout from the annotated shell all
  the way to the rendered output:
  - *Footnote/annotation split.* Programmer annotation lines outside the stub
    cells (coloured runs, ADaM-pattern text, or `Label -> DATASET.VAR` arrow
    paragraphs below a table) are routed to `programmer_annotations` and never
    shipped as footnotes. `spec_to_ars(ship_annotations = FALSE)` is the
    default; `TRUE` re-attaches them for debugging.
  - *Convention-agnostic binding.* New `bind_annotations()` fuzzy-matches each
    below-table `Label -> annotation` line back to its stub row (in-cell
    detections still win), splits multi-label lines
    (`Completed / Discontinued -> ADSL.EOSSTT (COMPLETED / DISCONTINUED)`)
    into per-row value filters, and captures a
    `Treatment columns -> ADSL.TRT01A` line as the authoritative column-axis
    grouping.
  - *Layout persistence + no-drop.* `build_ars_json()` walks every authored
    stub row in order: annotated rows become analyses whose method is inferred
    deterministically from the annotation form (count expression -> subject
    count; `VAR='val'` -> filtered subject count; bare variable -> categorical
    or continuous per the ADaM spec), label-only rows are kept as layout
    entries, and an annotated row whose variable cannot resolve is reserved as
    a traceable `manual_pending` analysis instead of being dropped. The
    ordered layout is persisted per output as arsbridge-private
    `_meta.shell_layout`, alongside `_meta.source_datasets`.
  - *Layout-driven rendering + column restriction.* When `_meta.shell_layout`
    is present, `ars_render_tlf()` builds the stub from the authored labels
    (joined to the ARD by `analysis_id`), pins the authored row order, expands
    categorical/continuous analyses beneath their authored label, renders
    missing rows blank (never dropped), and restricts the treatment columns to
    the arm levels named in the shell headers -- a population level like
    "Screen Failure" in `TRT01A` no longer leaks in as a treatment column.
    Outputs without the layout metadata render exactly as before.
  - *Listings/figures.* A LISTING section always emits `MTH_LISTING` analyses
    regardless of the LLM's analysis-type guess, and `ars_render_figure()` now
    resolves its default dataset from the shell's `Source:` line
    (`_meta.source_datasets`) instead of assuming `ADEFF`.
  - Fixed a tfrmt warning ("Unable to apply `frmt_combine` due to uniqueness
    of column/row identifiers") by using named single-parameter formats
    instead of one-parameter `frmt_combine()` in generated body plans.
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
