# Changelog

## arsbridge (development version)

- **Excel shells, end to end:
  [`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
  reads them, and
  [`ars_fill_shell()`](https://tavakohr.github.io/arsbridge/reference/ars_fill_shell.md)
  writes the results back into them.** A study can now be authored as a
  `.xlsx` with one worksheet per output, annotated in-cell in red, and
  the rest of the pipeline behaves exactly as it does for the `.docx`
  you have always passed —
  [`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md),
  [`parse_decision_digest()`](https://tavakohr.github.io/arsbridge/reference/parse_decision_digest.md),
  [`write_supplement_draft()`](https://tavakohr.github.io/arsbridge/reference/write_supplement_draft.md)
  and
  [`ars_workflow()`](https://tavakohr.github.io/arsbridge/reference/ars_workflow.md)
  all dispatch on the file extension. Word support is unchanged and
  permanent: every Word fixture was checked byte-for-byte at each step
  of this work.

  The reason to author in Excel is the new deliverable.
  [`ars_fill_shell()`](https://tavakohr.github.io/arsbridge/reference/ars_fill_shell.md)
  takes the shell, the ARS built from it and the ARD, and returns the
  author’s own workbook with the numbers in their placeholders and the
  annotations gone. Nothing is laid out or re-created: the layout, row
  labels, column headers, merges, fonts, column widths and footnotes
  come through untouched because none of them were ever lost, and each
  placeholder’s own `xx.x (xx.xx)` decides how its number is formatted,
  punctuation and all.

  Which result belongs in which cell is decided when the ARS is built,
  not when the workbook is written — each output carries a
  `_meta$shell_fill` cell map, recorded at the one moment the shell’s
  geometry and the analyses are both in hand. `_meta` is arsbridge’s own
  namespace, so conformance is unaffected and a consumer that ignores it
  sees what it always saw.

  Nothing uncertain is written. A cell keeps its placeholder and is
  reported when the ARD has no result for it, when the result is
  reserved for a manual derivation (ADR 0002), or when the row is a
  template standing for a repeated block such as `<System Organ Class>`
  — writing one system organ class’s count there would hide every other
  one behind a real-looking number. An empty cell in a clinical table
  reads as a zero, which is why the placeholder stays;
  `keep_pending_placeholders = FALSE` blanks them instead, and
  `strip_annotations = FALSE` keeps the annotations beside the numbers
  while reviewing. Listings and figures are not filled yet.

  Two disagreements this surfaced that nothing compared before. A
  placeholder asking for more statistics than its analysis produces — a
  row showing `xx (xx.x)` typed as a plain subject count — is now a WARN
  naming the row. And a shell’s decoded row labels are matched to the
  data’s codes, so rows reading “Female” and “Male” fill from an
  `ADSL.SEX` holding `F` and `M`; that pairing is refused rather than
  guessed unless each label names exactly one value and the whole block
  is one-to-one, and every pairing used is reported as an INFO
  diagnostic.

  The two readers are held together by a test rather than by intention:
  `test-parity_docx_xlsx.R` parses the same study authored both ways and
  requires the identical-semantics fields to match and the same ARS to
  come out. Design records: `adr/0004-xlsx-shell-input.md` (why both
  formats are permanent, and the three classes every section field falls
  in) and `adr/0005-filled-shell-output.md` (the cell map, and why the
  writer edits run XML instead of going through openxlsx2’s data model —
  a run in a real shell carries `rFont` and `sz`, which rebuilding would
  silently reset). New tooling: `tools/parity_check_shell.R` compares
  your own Word/Excel pair outside CI,
  `tools/shell_structure_digest_xlsx.R` is the privacy-safe geometry
  digest for a locked machine, and `tools/xlsx_roundtrip_check.R`
  re-verifies the writer’s assumptions after an openxlsx2 upgrade.

- **Fixed:
  [`ars_to_ard()`](https://tavakohr.github.io/arsbridge/reference/ars_to_ard.md)
  failed outright on a study mixing a declared multi-grouping column
  axis with ordinary by-treatment analyses.** The two executor paths
  disagreed on whether the `*_level` columns are list columns, and
  binding them aborted with “Can’t combine `<list>` and `<character>`”,
  losing the entire ARD for a perfectly well-formed study. Columns are
  now aligned to the
  [cards](https://github.com/insightsengineering/cards) convention
  before binding.

- **New
  [`parse_decision_digest()`](https://tavakohr.github.io/arsbridge/reference/parse_decision_digest.md):
  privacy-safe record of what the parser decided.** The counterpart of
  `tools/shell_structure_digest.R`: the same A/a/9 silhouette rules
  applied to the parser’s conclusions – header rows flagged vs inferred,
  physical grid width vs flattened column-label count, column labels and
  stub-row labels as silhouettes, column-tree shape, and per-severity
  diagnostic counts. Diffing the two JSONs on a locked machine localizes
  where a parse diverged from the document (e.g. column headers that
  came out placeholder-shaped) without any study text leaving it.
  `.populate_table()` now records its header decision
  (`header_rows_flagged` / `header_rows_inferred` / `n_header_rows` /
  `n_physical_cols`) on the section to support this.

- **A cell’s paragraph list is now the lossless source both consumers
  build from.** New `.cell_paragraphs()` keeps a multi-paragraph cell’s
  paragraphs intact (normalized, trimmed, order preserved), and
  annotation detection gains a second view: when the joined cell text
  yields nothing, the plain-text layer retries on the paragraphs joined
  BARE (`.detect_annotation_wrapped()`), because a wrapped annotation
  belongs joined with nothing while a wrapped label belongs joined with
  a space – one string cannot serve both. The recovered row carries
  `detection_method = "pattern_wrapped"` (medium confidence); the label
  is rebuilt from the paragraph list with spaces, so words never fuse.
  This removes the failure mode behind the `45e0481`/`1985e15`
  regression-of-a-regression: a wrap the join heuristic cannot classify
  no longer silently drops the annotation. Applied to stub cells, header
  cells, and the column-tree header grid.

- **Grid-first table model.** New internal `.table_grid()` expands a
  Word table into its physical R x C occupancy grid – gridSpan cells
  repeated across the columns they cover, vMerge continuations resolving
  to their anchor, ragged rows padded – so geometry questions are
  answered from the grid instead of raw cell indices. First
  consumers: (1) a continuation table under the same heading is appended
  only when its physical column count matches the open display; a
  mismatched table is refused with a FAIL diagnostic instead of silently
  welding misaligned rows on (two genuinely distinct tables under one
  heading no longer merge); (2) the “annotation found in data column N”
  diagnostic reports the physical grid column, which a gridSpan cell
  earlier in the row previously shifted.

- **New reviewed-manifest workflow:
  [`write_supplement_draft()`](https://tavakohr.github.io/arsbridge/reference/write_supplement_draft.md).**
  The parse can now be exported as a v4 supplement JSON – one entry per
  TLF with the title, typed analysis-set condition, and one analysis per
  annotated stub row; everything the parser was unsure about
  (unannotated rows, out-of-spec references, annotated statistic
  sub-rows) is listed in that entry’s `provenance$reviewItems`. Review
  the draft once, correct it, keep it under version control, and feed it
  back with
  `spec_to_ars(supplement = ..., supplement_trust = "prefer_supplement")`:
  the reviewed file then overrides a wrong parse instead of only filling
  gaps, turning a parser miss into a file edit instead of a code change.

- **A supplement can now REMOVE a wrongly-parsed row.** A per-analysis
  `"suppress": true` entry (rowLabel only, no variable) drops the
  matching stub row – for captions swept into the stub column or
  wrongly-merged continuation rows. Honoured only under
  `supplement_trust = "prefer_supplement"`; under `fill_gaps` the
  request is surfaced as a WARN instead of applied, and either way
  nothing disappears silently.

- **Table/figure number collisions no longer cross-bind.** A study can
  have both Table 14.3.1 and Figure 14.3.1; supplement matching
  previously reduced both to “14.3.1” and bound whichever entry came
  first. `.match_supplement_tlf()` now breaks number ties with the key’s
  designator prefix (“T-14-3-1”, “Figure 14.3.1”) or the entry’s
  `outputType`, and drafts are keyed by the full designator-bearing
  number.

- **Row and table accounting is now an enforced invariant.** Every
  `<w:tr>` in a parsed table must end up classified – header row, data
  row, or skipped with a known reason (no cells, vMerge ghost, struck
  through) – and every top-level `<w:tbl>` in the body must be handled:
  attached to a section, recognised as the TOC, or flagged as unparsed.
  Any miss raises a FAIL diagnostic naming the TLF, so the
  silently-dropped-rows bug class (the one that once cost 23 tables
  without a message) now fails loudly at parse time. A table appearing
  before any recognised TLF heading is reported with a WARN instead of
  vanishing. A new combinatorial fixture generator sweeps all 64
  combinations of continuation table, unflagged multi-row header, vMerge
  stub, multi-paragraph cell, strikethrough, and gridSpan data row,
  asserting the invariant on each.

- **Continuation tables are no longer dropped.** A shell that splits one
  display across several Word tables (a page break mid-table) lost every
  row after the first table: arsbridge modelled one table per TLF
  heading and discarded the rest with a warning. On the study that
  surfaced this, 23 tables were being dropped, which is why several
  outputs came out with no rows at all. Extra tables under the same
  heading now append their rows to the open display, in document order,
  with an INFO diagnostic. A repeated header row at the top of a
  continuation is recognised and skipped, and never overwrites the
  captured column geometry.

- **An annotation that wraps mid-token is read whole again.** Word
  breaks a narrow header cell in the middle of a reference (`[ADSL.C` /
  `OHORTN=1]`). Joining a cell’s paragraphs with a space – introduced to
  stop wrapped LABELS fusing (“dataextraction”) – truncated such an
  annotation to `ADSL.C` and dropped its condition silently, which left
  conditioned sub-columns unreadable and their column hierarchy flat.
  Paragraphs now join bare when the break falls inside an unclosed
  bracket or mid-reference, and with a space otherwise, so both cases
  are correct.

- **Client-content guards hardened.** `inputs/` is now default-deny in
  `.gitignore` with the synthetic APX-DRM-301 practice files
  allowlisted, so a sponsor document copied in under any name cannot be
  swept into a commit by `git add -A` (it previously could – only
  `inputs/ADaM/` and `inputs/.shell_backup/` were ignored). Local
  `HANDOFF_*.md` planning notes are ignored by the same principle. Both
  are reversible with `git add -f`.

- **New: privacy-safe shell structure extractor** (`tools/`). For shells
  that may not leave the machine they live on, `digest_shell()` writes a
  JSON digest of a `.docx` describing only its SHAPE – table geometry,
  merged cells, paragraph and run counts, strikethrough and colour flags
  – with all text reduced to character-class silhouettes and tokenised
  annotation skeletons. Needs only xml2 and jsonlite, so it runs where
  arsbridge is not installed. `redact_diagnostics()` does the same for a
  diagnostics frame.

- **The `(Q1, Q3)` row is no longer empty.** A continuous summary maps
  `p25`/`p75` (and `q1`/`q3`) onto a “(Q1, Q3)” stat line, but no
  formatter emitted that structure – the row rendered with its label and
  no numbers, silently dropping the quartiles. Both stat-name spellings
  now format as `(xx.x, xx.x)`, matching the `(Min, Max)` line beside
  it.

- **Figures carry their number and title.** A figure was written into
  the combined deliverable as a bare image: no heading, no title, no
  footnotes – nothing tying it to its shell. Figures now get the same
  heading / title / footnote band that tables and listings get.

- **A spanning header that names its grouping VARIABLE now builds a
  column hierarchy.** A two-level header whose parent carries the
  variable (`Alpha Cohort [ADSL.COHORTN]`) and whose sub-columns carry
  the values (`Low [ADSL.COHGR1N=1]`, …) was classified FLAT, collapsing
  an unambiguous hierarchy into a row of undifferentiated columns. A
  top-level node now declares the column axis either by carrying its own
  condition or by being a group over conditioned children. Genuinely
  flat headers are unaffected.

- **A header that cannot be resolved into a hierarchy says so.** When a
  spanning header exists but no column tree could be built, a WARN
  diagnostic names the spanning columns and the precondition that failed
  (no readable sub-column conditions, or a parent declaring neither a
  condition nor a variable) and states what to annotate. Previously the
  fallback to flat columns was silent. A spanning header over STATISTIC
  sub-columns (“Treatment A” over “n” / “(%)”) is a display split, not a
  hierarchy, and is never warned about.

- **Numbered mock rows (`SOC#1` / `PT#n`) are a second token dialect.**
  Shells author a nested block either with angle tokens
  (`<System Organ Class>` / `<Preferred Term>`) or with numbered ones
  (`SOC#1`, `PT#1`, `PT#2`, `PT#n`, then `SOC#2` …), annotating only the
  first of each and leaving the repeats bare. Only the angle dialect was
  recognised, so a numbered block produced no hierarchy and its bare
  repeats were persisted as authored label rows – the placeholder text
  rendered as if it were real data. Token rows now inherit their
  variable from the first annotated row sharing their stem, and mock
  rows are resolved before the label-only branch.

- **Single-level mock rows collapse into the row they illustrate.** A
  run of token rows on ONE variable directly under a categorical row of
  that same variable
  (`Primary reason for discontinuation [ADSL.DCSREASN]` followed by
  `<Reason `[`#1`](https://github.com/tavakohr/arsbridge/issues/1)`>`,
  `<Reason `[`#2`](https://github.com/tavakohr/arsbridge/issues/2)`>`,
  `...`) illustrates that row’s levels rather than naming new analyses.
  Those rows – and a trailing bare `...` continuation – are now emitted
  nowhere, with an INFO diagnostic each; the categorical analysis above
  already expands every observed level. A mock run on a DIFFERENT
  variable than the row above it is left alone.

- **Struck-through shell rows are no longer programmed.** A stub row
  whose every run carries `<w:strike/>` is a row the shell author
  removed from scope; arsbridge parsed it as live and re-added the
  dropped analysis to the deliverable. Such rows are now skipped with an
  INFO diagnostic naming the row, so the reviewer can see what was
  dropped and why. A PARTIALLY struck cell (a value crossed out and
  retyped beside it) stays live.

- **Stub labels that wrap in Word keep their spaces.** A cell holding
  several paragraphs was flattened by concatenating every text run with
  nothing between them, so a label wrapped after “…the data” came back
  as “dataextraction”, and two categorical levels authored as separate
  paragraphs fused into one line. Paragraphs inside a cell now join with
  a space; runs within a paragraph still join bare.

- **Real-world “standardized bracket” shell conventions are
  understood.** Shells that nest a `[PROGRAMMING DATASETS USED: ...]`
  directive inside a filter annotation used to corrupt the population
  string (the directive text leaked into the filter VALUE, poisoning
  every downstream subset). A new bracket normalizer scans top-level
  `[...]` spans with a nesting counter and rewrites each one: nested
  directives are lifted out; instruction wrappers are UNWRAPPED to the
  condition they carry
  (`[Use the stated source variable ... apply the stated condition: USUBJID WHERE ADSL.COMPLFL="Y". Keep the display label separate ...]`
  becomes `ADSL.COMPLFL='Y'; count of unique USUBJID`, and the
  `[Use DS.VAR for this displayed row; apply FILTER ...]` variant
  becomes `DS.VAR WHERE FILTER`); prose count instructions
  (`UNIQUE SUBJECTS WITH ...`) gain the distinct-subject marker;
  footnote markers (`[a]`, `Total[c]`) are stripped from labels;
  `Repeat ...` template directives and pure guidance prose are dropped
  with a record of what was removed. A space after the dataset dot
  (`ADSL. COMPLFL`, a Word line break) is closed up. Numeric `IN` lists
  (`ADSL.COHORTN IN (1,2)`) are now a recognised annotation form.
  Applied at stub cells, column headers, population paragraphs, and
  heading tails.

- **Workflow projects use a single `copilot/` exchange folder.** The
  `phase1/` and `phase2/` subfolders are gone; the blueprint,
  `supplement.json`, and the extraction validation report now land in
  `copilot/` beside the instruction files, so a project folder is just
  `copilot/` + `ars/` + the state file.

- **Nested AE/MH/ConMed blocks sort by descending frequency by
  default.** A nested SOC/PT (or body-system/term, ATC/medication) block
  now renders the most frequent parent level first, with the child terms
  in descending frequency under each parent (ties alphabetical) – the
  standard safety presentation – instead of A-Z at both levels. An
  authored `sort:` clause on the parent token row overrides the default:
  `sort: alphabetical` keeps A-Z, `sort: desc-freq('<column>')` counts a
  single treatment column as the sort basis (falling back to all columns
  where that column saw no events). The clause travels on the
  `_meta.shell_layout` entry; an unreadable clause raises a WARN
  diagnostic and keeps the default.

- **Conflict secondaries inherit the winning row’s counting
  discipline.** A supplement proposal that conflicts with the shell’s
  own annotation is built as its own analysis beside the winner – but
  its method was re-inferred from the annotation alone, so the variant
  of a distinct-subject AE row degraded to record-counting
  count-and-percentage. Records over a subject denominator rendered
  impossible percentages (p \> 1) in the variant block; the per-analysis
  rescale guard contained the display, but the numbers were wrong. The
  secondary now inherits `MTH_AE_FREQUENCY_COUNT` from the winning row
  (and an explicit `once/subject` clause achieves the same without one).
  On the incident study every computed proportion is now within \[0,
  1\].

- **Listing headers no longer leak annotations into production output.**
  Rendering the CDSC-ALZ-201 listings surfaced three related parser
  defects. (1) A listing’s DISPLAY column labels were captured raw at
  parse time, so the in-cell programmer annotation travelled into the
  ARS display columns and the rendered header (“Subject\[ADAE.USUBJID\]”
  instead of “Subject”); the annotation-stripped labels the header
  detection produces are now the display text. (2) When the colour layer
  found an annotation inline in a single-paragraph header cell, the
  label kept the annotation text; the detected text is now cut out of
  the label. (3) A fully qualified DATASET.VAR reference in a header
  annotation was tokenised – the dataset name read as a variable,
  fabricating “ADAE.ADAE (ADSL.TRT01A)” from “\[ADAE.TRT01A\]”; dotted
  references are now taken whole. Net effect on the incident study:
  clean listing headers end to end and eight fewer pipeline warnings.

- **Layout renderer: three containment fixes for corrupt cells** (found
  rendering the regenerated CDSC-ALZ-201 study end to end). (1) A
  `supplement_added` row whose analysis computed a multi-level
  distribution (the conflict-variant of a categorical or nested block)
  now expands like a categorical block instead of piling every level
  onto one authored line and corrupting the pivot into list-cells. (2)
  An authored EMPTY label (a spacer) is excluded from the tolerant
  sub-row matching – `startsWith(x, "")` matched every computed level,
  so a spacer could swallow a level and render it blank at the spacer’s
  position. Supplement rows also get their own (noprint) group identity,
  so a conflict-variant repeating its shell twin’s exact label can never
  collide with it. (3) The proportion-to-percent rescale is now gated
  per analysis rather than per output: one analysis whose `p` escaped
  \[0, 1\] (a denominator defect in a supplement variant, tracked
  separately) no longer silently blocks the rescale for every healthy
  analysis in the table, which had been printing `0.2%` where `20.0%`
  was meant.

- **Nested two-level row hierarchies, validation and polish** (nested
  SOC/PT handoff, Phase N4 – completes the handoff). Two
  [`validate_ars_model()`](https://tavakohr.github.io/arsbridge/reference/validate_ars_model.md)
  checks guard the new structure: `NESTED_CHILD_UNLINKED` (WARN) when a
  nested_child layout row no longer resolves to its parent row, and
  `NESTED_GROUPING_MISSING` (WARN) when the child analysis lost the
  parent’s variable from its groupings – each naming the repair. The
  editor’s shell view badges nested rows. An integration test now proves
  the whole chain from the real annotated shell document: parsing the
  authored `<System Organ Class>` / `<Preferred Term>` token blocks
  yields the linked analyses, the data-driven SOC grouping, the
  distinct-subject method, and a clean validation. The deterministic
  fixture was regenerated through the new generator – a near-no-op
  (version stamp plus one validation row from a newer codelist check),
  confirming non-nested studies are untouched.

- **Nested two-level row hierarchies, renderer interleave** (nested
  SOC/PT handoff, Phase N3). The layout renderer now expands a
  `nested_parent`/`nested_child` pair per parent level: the parent’s own
  line (SOC counts), then every child term observed under it, repeating
  per level – the standard interleaved AE presentation the shell
  authored, replacing the two stacked flat blocks. The authored token
  labels never print; the data levels take their place. {cards}’ full
  parent-by-child level cartesian is filtered to observed cells, so a
  term never lists under a parent it did not occur in, while a term
  genuinely coded under two parents renders in both blocks with per-cell
  counts. Levels sort alphabetically (the frequency-sort annotation is a
  later phase). `.shell_layout()` carries the new `parent_order`
  linkage; an orphaned child row (parent removed) degrades to the flat
  categorical block instead of erroring.

- **Nested two-level row hierarchies, engine verification** (nested
  SOC/PT handoff, Phase N2). The distinct-subject counting behind
  `MTH_AE_FREQUENCY_COUNT` now includes the analysis’s grouping
  variables: a subject is counted once per (arm, parent level, term)
  cell, so dirty data that repeats one term string under two parent
  levels counts its subjects in each cell instead of silently dropping
  them from all but the first (previously the distinct ran on subject +
  term only). Applied identically to the emitted {cards} block and the
  legacy executor, with an engine-equivalence test. End-to-end tests on
  a generated nested event pin the rest of the Phase N2 checklist
  against hand computations: the child ARD is keyed by (arm, SOC, PT)
  through the standard {cards} `by` path, duplicate records collapse to
  one subject, and percentage denominators are subjects per arm from the
  ADSL population (a non-population subject is excluded from N).

- **Nested two-level row hierarchies, generation side** (nested SOC/PT
  handoff, Phase N1). The shell generator now recognises the nested
  token-block pattern AE/MH/ConMed shells author – a data-driven parent
  token row (“”, `ADAE.AESOC`) followed by child token rows on a second
  variable of the same dataset (“”, `ADAE.AEDECOD`), repeating as
  further mock examples – and stops flattening it into unrelated
  analyses. The first parent/child pair now becomes two linked analyses:
  both count DISTINCT subjects (`MTH_AE_FREQUENCY_COUNT`, previously
  record-counting count-and-percentage), and the child carries the
  parent’s variable as a new data-driven row grouping in
  `orderedGroupings` – standard ARS vocabulary, so {cards}, siera and
  the ARD engine all see the hierarchy. The authored skeleton records
  the pair as `nested_parent` / `nested_child` layout rows linked by
  `parent_order`; template repeats collapse with an INFO diagnostic.
  Separately, the previously ignored `once/subject VAR` annotation
  clause now routes a count row to the distinct-subject method.
  Rendering still shows the two blocks unmerged – the interleaved
  SOC-then-its-PTs presentation is Phase N3 (renderer).

- **Shell-faithful review view** (shell-faithful editor handoff, Phases
  1-2). The Details tab of
  [`view_ars()`](https://tavakohr.github.io/arsbridge/reference/view_ars.md)
  /
  [`edit_ars()`](https://tavakohr.github.io/arsbridge/reference/edit_ars.md)
  now opens an output as the table the reviewer actually thinks in: a
  centered title band, the population line, the shell’s column header,
  and every authored row from `_meta.shell_layout` – including the
  section headers, stat lines and category rows that own no analysis and
  were previously invisible – in shell order and indentation, with
  `xx.x (x.xx)`-style placeholders in the body cells. Clicking a row
  highlights it and opens its owning analysis in a panel directly
  beneath the full-width table (the full edit form in edit mode, the
  read-only detail in view mode), so a correction never replaces the
  table being reviewed; rows without an analysis explain themselves
  instead. Every analysis panel now carries a “Back to the output table”
  link, so selecting a line from the tree or the bottom list no longer
  strands the reviewer without a way back. Clicking the column header
  opens a read-only panel naming the grouping entities that produce the
  columns (variable, groups, shared-by count) and where each piece is
  edited – deliberately not an edit surface, since the column axis is
  shared state with no single safe write target from one table’s header.
  Both that panel and the analysis form’s shared-entity notice now carry
  an “Edit in Entities” jump that switches to the Entities tab and
  selects the entity’s row, so shared definitions (groupings,
  populations, subsets, methods) are one click from wherever they are
  encountered. Built on a new pure builder (`.shell_table_data()` in
  `R/shell_table.R`) that reuses the renderer’s `.shell_layout()` parser
  and mirrors its block-consumption rule for row ownership. Outputs
  without a layout (listings, figures, older events) fall back to one
  row per referenced analysis. The old metadata block moved into a
  collapsed “Output properties” section; the analysis reorder list is
  unchanged. No new dependencies, no renderer changes.

- **Compound-clause awareness in the supplement merge** (render-fidelity
  handoff, Phase 1: RC-1 + RC-2). Two build paths inspected only simple
  (single-condition) where-clauses while the compound machinery already
  existed and worked; a supplement that declared leaf rows as
  `AND(TRTEMFL='Y', ASEV='MILD')` therefore rendered every categorical
  level twice, and 28 of 43 analyses on the incident study ran with no
  row filter at all (an unfiltered `ard_categorical()` whose full value
  distribution then deparsed into single cells as `c(" 69", " 0")`).
  Level-row detection now sees through a compound AND via the new
  `.where_leaf_on()` helper – the single EQ term on the parent block’s
  own variable is extracted, so clause *shape* no longer decides whether
  an authored row is a level. And `emit_extra_analysis()` (conflict
  secondaries, free-standing supplement rows) now receives the typed
  whereClause instead of re-parsing the annotation string, so a compound
  clause builds the same `compoundExpression` DataSubset the primary row
  path already produced. A new WARN diagnostic surfaces any extra
  analysis whose declared filter could not be parsed into a DataSubset,
  so the next occurrence lands in the validation report instead of a
  rendered cell.

- **Annotation corruption guardrails.** Four independent fixes closing
  gaps surfaced by a real hand-edited annotation that arsbridge’s own
  validation passed as clean. `validate_annotations_spec()` now
  cross-checks the *value* bound to a DATASET.VARIABLE reference against
  the spec’s declared length and codelist, not just that the variable
  exists – so a seriousness flag bound to a multi-character severity
  term (right variable, wrong literal) is now a FAIL instead of a silent
  PASS. `parse_shell_docx()` lints each programmer annotation for
  authoring corruption – typographic quotes and mid-annotation colour
  drift, both visual signs a line was hand-edited/pasted over rather
  than typed fresh – and WARNs through the existing diagnostics stream.
  [`ars_validate_supplement()`](https://tavakohr.github.io/arsbridge/reference/ars_validate_supplement.md)
  strips ANSI/OSC-8 escape codes from caught condition messages before
  they reach a FAIL/WARN finding, so a parse error never renders as
  literal escape-code soup in the Shiny repair-prompt panel. The
  two-phase Copilot instruction files now warn against pasting quoted
  annotation text into a JSON string unescaped, the gap that let a chat
  assistant’s own repair-prompt explanation break its own JSON in the
  incident that surfaced all of this.

- **[`ars_workflow()`](https://tavakohr.github.io/arsbridge/reference/ars_workflow.md):
  a guided app for the whole journey.** One stepper walks a study
  project from an annotated shell to a reviewed reporting event: project
  setup (fixed folder layout – `copilot/`, `phase1/`, `phase2/`, `ars/`
  – plus a small `arsbridge_project.json`), writing the two-phase
  Copilot instruction files, the two manual assistant round-trips (paste
  or upload the replies straight from the chat; a new blueprint
  pre-flight catches truncated or wrong-version files before they cost a
  Phase 2 session, and
  [`ars_validate_supplement()`](https://tavakohr.github.io/arsbridge/reference/ars_validate_supplement.md)
  runs the moment `supplement.json` lands, with the repair prompt shown
  on FAILs), the
  [`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
  build (supplement-driven, or deterministic when the Copilot phases are
  skipped), and a hand-off into
  [`edit_ars()`](https://tavakohr.github.io/arsbridge/reference/edit_ars.md)
  – when the editor closes, the workflow returns with every status
  re-derived from the files on disk, so closing the app never loses
  progress. Launchable standalone via `inst/shiny/ars_workflow/`. In
  passing, the shipped Copilot instruction files’ stale “format version
  3” doc-control strings were corrected to version 4.

- **Declared result-group paths in the ARS JSON (`resultGroupPaths`).**
  An output built from a hierarchical column tree now declares its
  result columns explicitly: one path per display column, in shell
  order, each referencing the standard group levels whose conditions
  compose it. The grouping factors themselves stay fully standard (one
  flat factor per header level, all linked from every analysis through
  `orderedGroupings`, outermost first) – the extension only records
  WHICH combinations are valid, because ARS v1.0 has no valid-path
  construct and an ordered grouping list alone would invite Cartesian
  products. A SUBTOTAL path references only its parent’s group
  (`totalStrategy: condition_based`); a GRAND_TOTAL path references no
  group (`totalStrategy: analysis_set`) and replaces the `includeTotal`
  boolean for these outputs.
  [`ars_conformance()`](https://tavakohr.github.io/arsbridge/reference/ars_conformance.md)
  strips the documented extension; the parsed column tree travels in the
  output’s `_meta` as provenance.
  [`validate_ars_model()`](https://tavakohr.github.io/arsbridge/reference/validate_ars_model.md)
  gains structural checks keyed by machine-readable codes
  (`DISPLAY_COLUMN_COUNT_MISMATCH`, `UNMAPPED_LEAF_COLUMN`,
  `INVALID_CARTESIAN_PRODUCT`, `GROUPING_VARIABLE_NOT_LINKED`,
  `SUBTOTAL_SCOPE_UNDEFINED`, `DUPLICATE_RESULT_PATH`,
  `GROUPING_ORDER_AMBIGUOUS`, `HEADER_TREE_MISSING`,
  `SUBTOTAL_EXCLUDES_UNDISPLAYED_CATEGORIES`), so a structurally damaged
  event fails review before any ARD is computed.

- **Path-aware rendering.**
  [`ars_to_tfrmt()`](https://tavakohr.github.io/arsbridge/reference/ars_to_tfrmt.md)
  recognizes a declared-path ARD: the column axis is the stable
  `result_group_path` (“Cohort A \> Mild”, …, “Total”), one display
  column per declared path, locked to shell order via
  `result_group_order`; the per-level `group*_level` columns are treated
  as the same column identity, never as row groups. Spanning parent
  headers (a two-level
  [`tfrmt::span_structure`](https://gsk-biostatistics.github.io/tfrmt/reference/col_plan.html))
  are deliberately deferred to a visual-QA cycle – the flat “parent \>
  child” labels carry the full identity meanwhile.

- **LLM enrichment understands the header tree.** When the parser found
  a hierarchical header, the enrichment payload includes the parsed tree
  and the response schema gains an optional `column_hierarchy` answer.
  Hard grounding: the model may only reclassify a column’s role
  (DETAIL/SUBTOTAL/GRAND_TOTAL) or name the grouping variable of an
  unannotated level – an answer describing different columns is
  discarded whole, and a variable outside the ADaM spec is ignored, both
  with WARNs. Geometry is parser truth; the LLM never redraws it.

- **Saving and leaving the editor are now separate decisions.** The
  review stage gains **Save** (writes to the opened file and stays in
  the editor, after a confirmation showing the destination path, the
  change summary, and the validation status; the previous file is backed
  up first and the dirty state resets) and **Save As** (writes a
  timestamped copy elsewhere; the original file and the editing session
  are untouched). *Save and close* and *Discard* keep their existing
  behavior, and every save modal now names the exact file it will touch.

- **Bulk grouping assignment.** One table usually shares one column
  layout, so the analysis panel gains “Apply this line’s groupings to
  every line in this output”: a preview modal lists the affected lines,
  the ordered `Grouped by` set is copied across in one action, one undo
  reverses all of it, and each changed line is recorded in the edit log.

- **Grouping lifecycle in the entity library.** The Groupings pool gains
  Add (spec-gated dataset/variable choices, starts data-driven), Clone,
  and Delete. Delete is refused while any analysis still references the
  grouping – the dependent analysis lines are named, so
  unassign-then-delete is explicit – and a grouping’s detail view now
  lists the analyses that use it, not just the count.

- **A “Columns” panel in the review stage.**
  [`view_ars()`](https://tavakohr.github.io/arsbridge/reference/view_ars.md)
  /
  [`edit_ars()`](https://tavakohr.github.io/arsbridge/reference/edit_ars.md)
  gain a tab that shows the parsed header tree (node type badges, N
  hints, raw header annotations), the declared result paths in shell
  order, and the structural warnings for the selected output. In edit
  mode a path’s `role` and `totalStrategy` are editable in place – the
  one judgment a reviewer must make deliberately is a subtotal’s scope
  (parent condition, which includes unknown-category subjects, vs the
  sum of the displayed children) – with full history/undo, edit-log, and
  re-validation like every other edit. Deeper surgery (adding/removing
  paths, changing conditions) stays in the raw-JSON escape hatch by
  design.

- **Declared-path execution.** For an output carrying
  `resultGroupPaths`,
  [`ars_to_ard()`](https://tavakohr.github.io/arsbridge/reference/ars_to_ard.md)
  and the emitted [cards](https://github.com/insightsengineering/cards)
  deliverable scripts compute one pass per declared result column
  instead of crossing the grouping variables: each pass filters both the
  analysis frame and the denominator frame by the path’s composed
  condition and runs the method idiom ungrouped, so a subtotal’s N is
  the parent-condition count (unknown child categories included), the
  grand total’s N the analysis-set count, and an undeclared combination
  (a sibling cohort crossed with another parent’s child levels) can
  never appear in the ARD. Each row carries its display identity – the
  per-level `group*`/`group*_level` columns plus stable
  `result_group_id` / `result_group_path` / `result_group_order` /
  `result_group_level` fields. The emitted script IS the executor (the
  emitted == executed invariant is unchanged), and a declaration that
  does not resolve blocks the whole output with a FAIL instead of
  silently degrading to flat groupings. Flat and crossed tables execute
  exactly as before.

- **Condition-defined `groups[]` now ship in the official ARS v1.0 Group
  shape.** Per the standard, a Group IS-A WhereClause: it carries
  `level` and `order`, with its `condition` (or `compoundExpression`)
  directly on the node. arsbridge previously emitted its internal
  wrapped WhereClause under `condition`, which the official schema
  rejects – a gap the conformance suite never saw because no conformance
  fixture used condition-defined groups. Emission is now official at the
  boundary (`.official_group()`), and internal consumers read both
  shapes through one accessor (`.group_where()`), so existing fixtures
  and supplements keep working.

- **Supplement format v4: `columnHierarchy`.** A supplement TLF entry
  can now declare the hierarchical column tree explicitly – one typed
  node per display column or spanning parent
  (`GROUP`/`LEAF`/`SUBTOTAL`/`GRAND_TOTAL`, each with its own
  condition), mirroring what the parser infers from header geometry. The
  declared tree wins over the parsed one, passes the same hard ADaM-spec
  gate (one bad node rejects the whole hierarchy – a partial tree would
  silently drop display columns), and
  [`ars_validate_supplement()`](https://tavakohr.github.io/arsbridge/reference/ars_validate_supplement.md)
  pre-flights the structure: the `includeTotal`/`columnHierarchy`
  conflict, node completeness, parent references, and every node
  condition. v4 is the single supported version (early-phase policy: no
  readers for older arsbridge formats); the shipped schema and both
  Copilot instruction files are updated, and a v2/v3 file fails loudly
  with a regenerate message.

- **Hierarchical and asymmetric column headers parse into an explicit
  column tree.** The shell parser now retains the raw header-cell
  geometry (grid spans, vertical merges) instead of only the flattened
  per-column labels, and builds `sec$column_tree`: every visible result
  column becomes one declared path with a composed condition. A parent
  column spanning conditioned child columns (with an optional per-parent
  subtotal), a sibling column with no children, and an overall Total
  therefore all survive parsing as first-class structure – the child
  grouping variable is no longer dropped with a “several variables”
  warning. A subtotal column’s condition is the parent’s condition by
  construction (not the union of the displayed children), so a subtotal
  that includes unknown-category subjects computes correctly. Detection
  is condition-driven: spanned headers over statistic sub-columns (“n” /
  “(%)”) and classic one-row conditioned headers keep the existing flat
  single-axis behavior exactly.

- **WhereClause algebra helpers** (internal groundwork for hierarchical
  column groupings): `combine_conditions()` composes clauses under
  AND/OR with NULL-tolerant, same-operator flattening;
  `canonicalize_condition()` produces an order-insensitive,
  case-normalized canonical form; `conditions_equal()` compares two
  clauses structurally; and `condition_implies(child, parent)` answers
  whether a child column’s condition sits inside its parent’s scope
  (conservative FALSE when a clause contains OR). These let a
  hierarchical column tree build a leaf condition as AND(ancestor
  conditions, own condition) and assert that a parent subtotal’s
  condition is exactly the parent condition.

- **A freshly generated reporting event now validates clean against the
  official CDISC ARS v1.0 schema** (beyond the documented extensions,
  which
  [`ars_conformance()`](https://tavakohr.github.io/arsbridge/reference/ars_conformance.md)
  strips and reports). All six known divergences are closed:

  - Every analysis carries the required `reason` and `purpose`
    controlled terminology. The run-level defaults –
    `"SPECIFIED IN SAP"` and `"EXPLORATORY OUTCOME MEASURE"` – are new
    [`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
    arguments (`analysis_reason` / `analysis_purpose`, validated against
    the closed CDISC vocabularies), and both fields are editable per
    line in
    [`edit_ars()`](https://tavakohr.github.io/arsbridge/reference/edit_ars.md),
    which is where the handful of endpoint tables get their
    `PRIMARY`/`SECONDARY` purpose corrected.
  - `version` fields are integers, as required.
  - Displays are written in the official
    `OrderedDisplay{order, display}` wrapper with their own `id`/`name`,
    and footnotes as `orderedSubSections[]` wrapping
    `subSection{id, text}`.
  - `fileSpecifications[].fileType` is a terminology object.
  - The self-referential operation-role placeholders carry a valid term
    (`NUMERATOR`) instead of `""`.
  - Contents-list analysis entries are named.

  siera compatibility was verified by running
  [`siera::readARS()`](https://clymbclinical.github.io/siera/reference/readARS.html)
  on the same event in both shapes: identical output. (siera reads none
  of the changed fields by position, and the new keys are additive.)

- **No compatibility readers for arsbridge’s own earlier output
  shapes.** Early-phase policy: when a correction changes the emitted
  JSON, the old shape is not carried. Readers target the official shape
  only – an unrecognized shape reads as unset rather than crashing or
  being misread, and the round trip preserves what it does not
  understand rather than guessing at it. The remedy for an outdated file
  is regenerating it with
  [`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md),
  and
  [`ars_conformance()`](https://tavakohr.github.io/arsbridge/reference/ars_conformance.md)
  names exactly what is wrong with it. The bundled minimal fixture is
  now official-shaped too.

- **[`ars_conformance()`](https://tavakohr.github.io/arsbridge/reference/ars_conformance.md)
  validates a reporting event against the official CDISC ARS v1.0 JSON
  Schema.** The schema ships with the package, pinned to the `v1.0.0`
  release of `cdisc-org/analysis-results-standard` alongside its LinkML
  source (`inst/schema/`, provenance in the README there), so the answer
  never changes because the standard’s development branch did.
  arsbridge’s documented extensions – `_meta`, `referencedAnalysisIds`,
  the nested `analysisVariable` duplicate and friends – are stripped
  before validating and reported in an attribute, so the findings show
  genuine divergences instead of burying them under sanctioned fields.
  The generator’s known gaps (missing `reason`/`purpose` terminology on
  analyses, string `version`s, flat display objects, string `fileType`s,
  placeholder operation roles) are reported honestly and pinned by
  tests, and every save from
  [`edit_ars()`](https://tavakohr.github.io/arsbridge/reference/edit_ars.md)
  prints a one-line conformance count. This resolves the open question
  of *which* schema to validate against: the LinkML model is the source
  of truth, vendored at `inst/schema/cdisc_ars_v1.0.0_ldm.yaml`.

- **The editor no longer goes quietly stale around structural changes.**
  Moving a line now visibly reorders the panel, applying a raw-JSON
  replacement refreshes the fields it changed, the outputs tree keeps
  its open panels across edits instead of collapsing, and the
  entity-library tables update their data in place so row selection
  survives the very edit it enabled. Same family as the undo-display
  fix: the model changed and the screen did not.

- **Writing to a derived model column is now refused instead of silently
  reverted.** Columns computed from the node (`output_id`, `n_analyses`,
  `condition_summary`, …) used to accept a write that the next refresh
  undid; `model_set_field()` now says so instead.

- **A review session can no longer be lost.** Every change is undoable
  (and redoable) with the arrows in the header – field edits, added and
  removed lines, reordering, detaching, library edits, all of it. And
  every change is written to a crash-recovery copy in the user’s cache
  directory, so a browser or an R session that dies mid-review offers
  the work back when the editor is next opened on the same file. The
  file being edited is never touched until an explicit save, and the
  recovery copy is cleared once the work is safely on disk.

- **Shared entities are editable from the library.** Methods, analysis
  sets, data subsets and groupings can now be corrected in the Entities
  tab, which is the right place to fix a population that is wrong
  everywhere rather than opening thirty analyses. The panel says how
  many analyses a change will affect, and points at detaching when the
  intent is to change one line only. Nested shapes the flat fields
  cannot express – compound conditions, grouping levels – have a
  raw-JSON escape hatch that refuses anything invalid instead of
  applying it.

- **[`export_edit_log()`](https://tavakohr.github.io/arsbridge/reference/export_edit_log.md)
  turns a review session into a QC record.** The sidecar
  `<name>.edits.json` becomes a styled workbook: what changed, from what
  to what, who saved it and when. Repeated edits to one field collapse
  to a single before/after row, and a field edited back to where it
  started does not appear at all. The ARS JSON still carries no
  provenance fields, so the deliverable stays CDISC-conformant.

- **[`review_ars()`](https://tavakohr.github.io/arsbridge/reference/edit_ars.md)**
  is an alias for
  [`edit_ars()`](https://tavakohr.github.io/arsbridge/reference/edit_ars.md),
  for whichever framing fits.

- **Fixed: the raw-JSON escape hatch silently discarded the edit it
  reported applying.** Replacing an entity’s node re-derived its row by
  patching the new node from the row’s *old* column values, which undid
  the replacement. Node-replacing edits now treat the node as
  authoritative.

- **The lines the generator missed can now be added, and a gap tells you
  exactly which one.** Selecting a coverage finding – “the shell
  annotates this but no analysis uses that variable” – opens the
  add-analysis wizard pre-filled with the dataset and variable the shell
  named, at the output it belongs to. The wizard is reuse-first by
  construction: method, population, data subset and groupings all
  default to what the output’s other lines already use, so the normal
  outcome of adding a line is that no new shared entity appears and the
  event stays readable. Display order is meaningful, so the line can be
  inserted at a chosen position, and an output’s lines can be reordered
  or removed.

  An added line is indistinguishable from a generated one: the same node
  shape, the same `AN_<TLF>_<nnn>` id convention (collision-checked),
  the same self-referential operation placeholders siera needs. The
  tables of contents rebuild themselves, because they were already
  derived from the outputs. Adding a line and then removing it restores
  the event byte for byte.

- **A shared population, data subset or grouping can be detached for one
  analysis.** Editing a population used by thirty analyses changes all
  thirty; detaching copies it under a new id and repoints only this
  line, so it can then be changed on its own. Methods deliberately
  cannot be detached: the engine dispatches on the method id, so a
  per-analysis copy would have no executor and would quietly degrade a
  computed line into a generic summary. Changing which method one line
  uses is what the method dropdown is for.

- **[`validate_ars_model()`](https://tavakohr.github.io/arsbridge/reference/validate_ars_model.md)
  findings gained a `ref` column** carrying what the finding is about in
  machine-readable form, which is what lets a coverage gap turn into a
  pre-filled wizard rather than a re-typing exercise.

- **[`edit_ars()`](https://tavakohr.github.io/arsbridge/reference/edit_ars.md)
  closes the loop: generate, review and correct, then execute.** The
  same structured viewer, with the detail panels editable. Methods,
  populations, data subsets and groupings are chosen from what actually
  exists – the entities in the file, the methods the engine can execute
  (labelled with what it will do with each: computed, needs a
  prerequisite, reserved for manual computation), and, with the ADaM
  spec supplied, the variables the study really has. Choosing a standard
  method the file does not carry adds it first, so an analysis can never
  point at a method that is not there. Every dropdown says how many
  analyses share the entity, because editing a shared method edits all
  of them.

  Nothing is written until you save, and saving shows a from/to table of
  what changed first. The previous file is backed up to
  `<name>.json.bak-<time>`, the new content is written to a temporary
  file in the same directory and renamed into place so an interrupted
  save cannot destroy the file it was replacing, and the edit log is
  written to `<name>.edits.json` beside it – keeping provenance out of
  the ARS JSON so the deliverable stays CDISC-clean.
  [`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
  now also returns `adam_spec_path`, so `edit_ars(result)` wires up
  spec-driven dropdowns and gap detection on its own.

- **[`view_ars()`](https://tavakohr.github.io/arsbridge/reference/view_ars.md)
  opens a reporting event as the structure a programmer already
  recognises.** Each output is a collapsible panel with its analysis
  lines beneath it – the shell’s skeleton, read straight from the
  standard’s `mainListOfContents` – and validation findings are overlaid
  as badges, so the displays needing attention are visible before
  anything is opened. Selecting a line resolves its ids into what they
  mean: the method’s name plus whether the engine can actually execute
  it, the population’s condition, the variables results are grouped by.
  A shared-entity library shows how many analyses each method, analysis
  set, data subset and grouping is used by, which is the thing a flat
  JSON view hides. The viewer never writes anything. `shiny`, `bslib`
  and `DT` are Suggests, so the package is unaffected when they are not
  installed.

- **An ARS reporting event can now be read as editable tables and
  written back losslessly.**
  [`ars_to_model()`](https://tavakohr.github.io/arsbridge/reference/ars_to_model.md)
  turns the nested JSON into one data frame per entity pool (analyses,
  methods, analysis sets, data subsets, groupings, outputs), each row
  carrying the flat fields a reviewer edits plus the original untouched
  node;
  [`model_to_ars()`](https://tavakohr.github.io/arsbridge/reference/model_to_ars.md)
  is its exact inverse. Fields the model does not surface – including
  `_meta` and any future ARS key – ride along untouched, so an unedited
  model round-trips to a structurally identical event and an edited one
  differs only where it was edited. The two tables of contents are
  copied verbatim unless a structural change means they have to be
  rebuilt from the outputs. This is the foundation of the human review
  stage between
  [`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
  and
  [`ars_to_ard()`](https://tavakohr.github.io/arsbridge/reference/ars_to_ard.md).

- **[`validate_ars_model()`](https://tavakohr.github.io/arsbridge/reference/validate_ars_model.md)
  checks a reporting event for the problems that matter before
  execution.** Every reference resolves (an empty `dataSubsetId`
  correctly means “no subset”), no id is duplicated, and each analysis
  is classified by how
  [`ars_to_ard()`](https://tavakohr.github.io/arsbridge/reference/ars_to_ard.md)
  will actually treat its method – computed natively, dependent on a
  prerequisite, silently falling back to the generic summarizer, or
  reserved for manual computation. Given the ADaM spec it also checks
  datasets and variables exist, and given the annotation validation
  report it reports annotated shell lines that no analysis covers – the
  lines the generator missed.

- **Spec codelists decode coded categorical variables end to end.**
  `parse_adam_spec()` now reads the spec workbook’s Codelists sheet
  (both the `"Codelist Name" / "Term (Code)" / "Decoded Value"` and the
  `"ID" / "Term" / "Order" / "Decoded Value"` header conventions, with
  merged-cell fill-down and a `Used By Variables` fallback link) and,
  for define.xml input, the `CodeList` / `CodeListRef` nodes. The parsed
  codelists ship in the ARS JSON as `_meta$value_decodes`, and both the
  execution engine and the emitted {cards} deliverable derive the coded
  analysis variable as a factor
  (`factor(as.character(VAR), levels = codes, labels = decodes)`) before
  [`cards::ard_categorical()`](https://rdrr.io/pkg/cards/man/deprecated.html).
  The ARD therefore shows decoded labels (“DEATH”) instead of raw codes
  (“1”), keeps codelist order, and reports EVERY codelist term –
  unobserved categories appear with n = 0, matching disposition shells
  that list all reasons. Authored level rows (`ADSL.DCSREASN=1` under
  the parent) are stamped with the decoded label (raw code kept as
  `level_code`) so renderer level matching is unaffected. Codelists
  larger than 15 terms (e.g. COUNTRY) are skipped with a WARN so tables
  never explode into hundreds of zero rows.

- **Column groups fall back to the spec codelist.** When a grouping
  variable’s column headers carry no condition annotations (the
  `GF_<VAR>` `groups: []` case that rendered coded column labels), the
  grouping factor’s per-level groups are now derived from the variable’s
  codelist – decoded label, `EQ` term condition, codelist order – with a
  WARN asking for review. Header-annotated groups always win.

- **BREAKING: supplement format version 3 – typed CDISC ARS
  conditions.** The no-API supplement now carries every filter,
  population, and column condition as a typed ARS `WhereClause` object
  (`{condition: {dataset, variable, comparator, value}}` /
  `{compoundExpression: {logicalOperator, whereClauses}}`) instead of a
  string. This ends the string-parsing fragility (double `=`, smart
  quotes, `OR`, `="..."` value repair) that caused real-world extraction
  failures. Per-TLF entries gain typed `analysisSet`, `groupings` (with
  typed group conditions), `analyses` (was `bindings`; `variable` is now
  a `{dataset, variable}` object and the filter a typed `whereClause`),
  plus `listingColumns`, `recordFilter`, `sorting`, per-row
  `methodId`/`parentRowLabel`/`denominator`, `anchors`, and
  `provenance`. `read_supplement()` accepts only `supplement_version` 3
  and aborts loudly on a v2 file – regenerate with
  [`ars_copilot_instructions()`](https://tavakohr.github.io/arsbridge/reference/ars_copilot_instructions.md).
  A JSON Schema (`inst/schema/arsbridge_supplement_v3.schema.json`)
  ships with the package, is uploaded to the assistant for
  self-checking, and is used by
  [`ars_validate_supplement()`](https://tavakohr.github.io/arsbridge/reference/ars_validate_supplement.md)
  when `jsonvalidate` (new Suggests) is installed.

- **`spec_to_ars(supplement_trust=)` – configurable conflict
  resolution.** `"fill_gaps"` (default, unchanged) lands a supplement
  value only where the regex left a gap; `"prefer_supplement"` lets a
  validated, spec-gated supplement value override the shell on a
  conflict, with a WARN recording both and the shell original kept as a
  secondary analysis. The hard ADaM-spec gate is never bypassed in
  either mode. The mode is recorded at `_meta.supplement_trust`.

- **Packaged two-phase Copilot workflow.**
  `ars_copilot_instructions(workflow = "two_phase")` writes a Phase-1
  (evidence blueprint) and Phase-2 (semantic construction + repair)
  instruction set for large or complex shells, alongside the single-file
  workflow. Both emit supplement version 3 and are shipped in step with
  the reader, so the instructions and the accepted format can no longer
  diverge. Each instruction file opens with a **“How to run this”**
  block – the operator steps (which files to attach, what to save) plus
  a paste-ready prompt for the chat.
  [`ars_copilot_instructions()`](https://tavakohr.github.io/arsbridge/reference/ars_copilot_instructions.md)
  now returns the vector of paths it wrote.

- **[`ars_validate_supplement()`](https://tavakohr.github.io/arsbridge/reference/ars_validate_supplement.md)
  rewritten for v3** with typed-condition checks, comparator/enum/arity
  validation, parent-row resolution, and a paste-ready `repair_prompt`
  attribute that bundles every FAIL for the assistant.

- **Native SAS `.sas7bdat` ADaM cuts are now read everywhere.** The
  per-TLF standalone
  [cards](https://github.com/insightsengineering/cards) scripts emitted
  by `write_tlf_code()`, the execution engine
  ([`ars_to_ard()`](https://tavakohr.github.io/arsbridge/reference/ars_to_ard.md)),
  and the listing/figure renderers previously loaded only `.xpt` and
  `.csv`; they now also match and read `.sas7bdat` via
  [`haven::read_sas()`](https://haven.tidyverse.org/reference/read_sas.html).
  Loaders remain case-insensitive on the dataset name, and when several
  formats of the same dataset are present the native SAS formats
  (`.xpt`, then `.sas7bdat`) are preferred over `.csv`.

- **Rewritten Copilot instruction file (extraction guidance version
  3).** The file
  [`ars_copilot_instructions()`](https://tavakohr.github.io/arsbridge/reference/ars_copilot_instructions.md)
  writes is substantially expanded to make a chat assistant classify
  each annotation before emitting JSON: a mandatory role taxonomy
  (population / result-column / displayed-row / listing-column /
  supporting-filter / programming-note), Word label reconstruction
  rules, footnote-mapping resolution, data-type-aware literal quoting,
  TLF inventory reconciliation against the Table of Contents, and
  per-TLF and final validation checklists. The `column_groups` fallback
  (format version 2) is folded into the new column-structure guidance:
  the assistant supplies per-column conditions there only when the shell
  headers carry no machine-readable `DATASET.VARIABLE=value` filter,
  each `where` written with the full dataset prefix. All examples use
  generic ADaM datasets/variables and generic TLF labels.

- **Supplement format version 2: a `column_groups` field.** In the
  no-API supplement workflow, a table whose result columns are values of
  one variable (cohort columns keyed on `ADSL.COHORTN`) could not be
  expressed when the shell header cells carried no machine-readable
  `DATASET.VAR=value` filter – the format had nowhere to hold the
  per-column conditions, so the grouping shipped with an empty
  `groups[]`. Each TLF entry may now carry an ordered `column_groups`
  array of `{label, where}` objects; `where` is a full condition
  (`ADSL.COHORTN=1`, `is.na(ADSL.COHORTN)`) using the same grammar as an
  annotated header. `spec_to_ars(supplement = ...)` feeds these into the
  existing group builder, so each becomes one display column (including
  a missing/Unknown bucket) with a WhereClause in the ARS JSON. The
  shell’s own header-annotation path still wins when it captured the
  columns itself; every `where` passes the ADaM-spec gate;
  [`ars_validate_supplement()`](https://tavakohr.github.io/arsbridge/reference/ars_validate_supplement.md)
  checks the new field. The instruction file
  [`ars_copilot_instructions()`](https://tavakohr.github.io/arsbridge/reference/ars_copilot_instructions.md)
  writes is updated to version 2 and now tells the assistant to put
  column conditions here, never to fold them into `bindings`. Existing
  version-1 supplements must be regenerated.

- **Column-header annotations now parse the
  [`is.na()`](https://rdrr.io/r/base/NA.html) /
  [`missing()`](https://rdrr.io/r/base/missing.html) call forms** that
  annotated shells actually use for a missing/Unknown group – R’s
  `is.na(ADSL.COHORTN)` and SAS’s `missing(COHORTN)`, plus the negations
  `!is.na(...)` / `not missing(...)`. Previously only the prose
  `DATASET.VAR is missing` form was recognized, so a call-form
  Unknown-cohort header silently failed to parse and its column vanished
  from the axis. A companion coverage check now WARNs when a header
  names the column-axis variable but its annotation does not parse into
  a condition, reporting how many columns were captured versus expected
  – so a narrowed axis is surfaced rather than shipped quietly.

- **Annotation-defined column axis: per-column filters in table header
  cells.** When two or more column headers carry a filter on the same
  variable – `Cohort A (N=XX) ADSL.COHORTN=1`,
  `Cohort B (N=XX) ADSL.COHORTN=2`,
  `Unknown Cohort (N=XX) ADSL.COHORTN is missing` – each condition now
  becomes one display column, in shell order. This makes a
  merged/derived column (an “Unknown” bucket collecting missing values)
  expressible purely by annotation, with no ADaM change: the engine
  derives the grouping in memory from the conditions, identically in the
  executed ARD and the emitted
  [cards](https://github.com/insightsengineering/cards) scripts (a
  `case_when` factor built from the same where-clause predicates). The
  conditions are carried in the ARS JSON as per-level `groups[]` entries
  with WhereClauses. Rows matching no column are excluded from the group
  columns and counted in a WARN; a `Total (N=XX) ...` header is
  recognized as the overall column and switches `includeTotal` on. The
  annotation grammar also gains the positive `DATASET.VAR is missing` /
  `is null` form and parenthesized `IN ('a','b')` value lists.

- **The Copilot instruction file now asks for a downloadable
  `supplement.json` file** (written programmatically with a real JSON
  serializer) rather than an on-screen block, with the fenced block kept
  as a fallback. Delivering a serialized file avoids the copy-paste
  smart-quote / stray-double-quote / truncation errors that a pasted
  chat block introduces.

- **A supplement with double-quoted where-clause values now loads
  instead of aborting.** The most common Copilot mistake – a comparison
  value quoted with double quotes (`MHSCAT="UNDERLYING CONDITIONS"`),
  which breaks the JSON – is now auto-repaired to single quotes before
  parsing. In valid JSON a `"` is never preceded by `=`, so `="..."` can
  only be a value comparison, making the rewrite safe; escaped,
  already-valid quotes are left untouched. The instruction file still
  asks for single quotes, and `read_supplement()`’s error still names
  the fix for any malformation the repair cannot cover.

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
