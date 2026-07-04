# ADR 0003 — Shell layout fidelity & convention-agnostic annotation binding

Status: Accepted (implemented 2026-07-03, phases 1-5)
Date: 2026-07-03
Supersedes/extends: builds on ADR 0001 (method extensibility), ADR 0002 (partial-results traceability)

## Context — what actually goes wrong

Rendering the CDSC-ALZ-201 disposition table (`T_14_1_1`) exposed a class of
failures, not a one-off bug. The annotated shell had 6 authored stub rows
(Subjects enrolled / Screen failures / Randomized·treated / Completed /
Discontinued / End-of-study status). The rendered arsbridge table had 3 data
rows (EOSSTT: Total/COMPLETED/DISCONTINUED), a spurious "Screen Failure"
**column**, and the programmer annotations printed as **footnotes**.

Verified root causes (file:line):

1. **Annotation convention is too narrow.** `parse_shell_docx()` only reads an
   annotation when it sits *inside the stub cell* (colour/bold/bracket/plain
   `DATASET.VAR`), via `.detect_annotation()` (parse_shell_docx.R:453). A shell
   that annotates as red `Label -> DATASET.VAR` lines *below the table* (a
   common, reasonable convention) yields `has_annot = FALSE` for every row.
   Confirmed: all 6 rows parsed with empty annotation.

2. **Annotation text leaks into footnotes.** The post-table paragraph handler
   (parse_shell_docx.R:163-167) routes any paragraph >10 chars into
   `footnotes`, with no test for annotation runs. The 5 `Label -> ADSL.VAR`
   lines all landed in `footnotes`, flowed through `build_ars_json` into the
   ARS display `Footnote` section, and `extract_footnotes()`
   (ars_to_tfrmt.R:69) shipped them as GT source notes.

3. **Rows are rebuilt from the ARD, not the shell.** `detect_row_roles()`
   (ars_to_tfrmt.R:183) sets `label_var = .ARS_ROW_LABEL` (synthetic) and groups
   by ADaM `variable`; `.tfrmt_prep_ard()` (ars_to_tfrmt.R:371-390) labels rows
   by `variable_level` (COMPLETED/DISCONTINUED) — the authored stub labels and
   their order are never consulted.

4. **Rows silently dropped / collapsed.** With no in-cell annotations, the LLM
   inferred analyses from labels + SAP text; it produced 4 analyses for 6 rows
   (enrolled & screen-fail dropped) and pointed 3 at EOSSTT, which the renderer
   then dedups into one block.

5. **Population levels leak into columns.** `detect_col_var()` (ars_to_tfrmt.R:131)
   picks `TRT01A`, which carries a "Screen Failure" level (52 subjects), so
   screen failures render as a treatment column instead of a stub row.

The unifying problem: **arsbridge is semantics-first (variable+method → generic
cross-tab). It has no first-class model of the authored table layout, and its
annotation reader supports only one placement convention.**

## Decision — introduce a Shell Layout Model + convention-agnostic binding

Add an explicit, persisted **Shell Layout Model (SLM)** captured at parse time,
carried through the ARS as private metadata, and used to drive rendering. Make
annotation *reading* independent of annotation *placement* by adding a binding
layer that associates any annotation to its stub row regardless of where it sits.

Design goal ("layout fidelity"): the final table's stub rows, order, labels,
indentation, and column set match the annotated shell as closely as the data
allows, for **any** shell design, while every value stays auditable (ADR 0002).

## Architecture — five layers

### Layer A. Convention-agnostic annotation binding (parse_shell_docx.R, new extract_annotation_vars.R)

A stub row's annotation may live in any of these places; detect and bind all:

| Convention | Where | Binding key |
|---|---|---|
| In-cell (current) | stub cell runs | same cell |
| Bracketed | `Label [ADSL.VAR WHERE ...]` in cell | same cell |
| Below-table arrow block | red `Label -> DATASET.VAR` paragraphs after the table | **label match** to stub row |
| Dedicated annotation column | last table column | same row |
| Population/column annotations | red runs on pop line / header | section / column |

New function `bind_annotations(section)`:
- Collect all "annotation-bearing" paragraphs after the table (any run
  non-grey/non-black colour, OR text matching `.ANNOTATION_PATTERN`, OR the
  `^\s*<label>\s*->\s*<annotation>` arrow form).
- For each, split on `->`/`:`; fuzzy-match the left side to a `stub_rows$label`
  (normalised: lowercase, strip punctuation/indent). On match, set that row's
  `annotation`, `has_annot=TRUE`, `detection_method="below_table_arrow"`.
- Rows still unbound keep `has_annot=FALSE` and fall to LLM inference (unchanged).
- Everything consumed here is removed from the footnote candidate set (Layer B).

This is the change that makes "any convention" real: placement no longer matters,
only that each annotation can be tied back to a row/section.

### Layer B. Footnote vs annotation separation (parse_shell_docx.R:163-167)

Replace the footnote catch-all with a classifier. A post-table paragraph is:
- an **annotation** if it has an annotation-coloured run, matches
  `.ANNOTATION_PATTERN`, or is `label -> …` arrow form → goes to Layer A,
  never to `footnotes`;
- a **source line** if it matches `.SOURCE_LINE_RE` → `source_datasets`;
- a **true footnote** otherwise → `footnotes`.

Add `sec$programmer_annotations` (kept for the validation report only, never
shipped). Add `spec_to_ars(ship_annotations = FALSE)` and
`ars_render_*(keep_source_note = TRUE)` toggles. Net: rendered footnotes contain
only real footnotes.

### Layer C. Persist the layout in the ARS (build_ars_json.R)

- Emit **one analysis per authored stub row**, in authored order, each carrying
  its shell label and an inferred method from the bound annotation form:
  - bare `DATASET.VAR` "count of USUBJID" → `MTH_SUBJECT_COUNT` (distinct USUBJID)
  - `DATASET.VAR='val'` / `WHERE` → data subset + subject count
  - flag `SAFFL='Y'` → subject count within subset
  - categorical var (AGEGR1, SEX) → `MTH_COUNT_AND_PERCENTAGE` (expands to level rows nested under the stub label)
  - continuous var (AGE) → `MTH_SUMMARY_STATISTICS_CONTINUOUS` (expands to Mean(SD)/Median/(Min,Max) sub-rows)
  - no variable (section header / spacer) → label-only row, no analysis
- Write `output$_meta.shell_layout`: ordered list of
  `{order, label, indent, analysis_id | NA, kind}`. ARS v1.0 has no first-class
  stub model, so this is an arsbridge-private `_meta` block (consumers ignore it;
  the renderer keys on it).
- Never drop an annotated authored row.

### Layer D. Column model — keep populations out of the treatment axis (ars_to_tfrmt.R:131, build_col_levels)

- Restrict the column axis to the arm levels named in the shell `col_headers`.
  Levels present in the ARD grouping but absent from the shell columns (e.g.
  "Screen Failure" in TRT01A) are **excluded from columns**; if the shell has a
  "Screen failures" *row*, they are counted there instead.
- Support an explicit "Total" column when the shell header has one.

### Layer E. Layout-driven rendering (ars_to_tfrmt.R detect_row_roles / .tfrmt_prep_ard)

- When `output$_meta.shell_layout` exists, build the tfrmt `label` from the
  **authored stub label** (a real column joined onto the ARD by `analysis_id`),
  not the synthetic `.ARS_ROW_LABEL`.
- Pin row **order** to the layout order (factor levels / `row_grp_plan`).
- Categorical-expansion analyses nest their level rows under the stub label as an
  indented group; continuous analyses expand to stat-lines under the label.
- Build the display frame by **left-joining the ordered layout to the ARD stats**,
  so every authored row appears in order with its authored label; a row whose
  stat is missing renders blank or as the ADR-0002 `[‡ manual]` marker — never
  dropped, never silently reordered.
- Degeneracy: a pure cross-tab shell (demographics: one variable → level rows)
  produces a trivial 1-row layout and falls back to today's behaviour — **no
  regression** for tables that already render well.

## Footnote fix (called out explicitly, ships first)

Layer B alone fixes the reported footnote problem and is independent of the rest.
Ship it as Phase 1.

## Generality contract (any shell)

- Any authored non-spacer row appears in the output (`#rendered ≥ #authored`).
- Annotation placement is irrelevant to correctness (Layer A).
- Unreadable rows degrade to a traceable `manual_pending` row, never a drop.
- Listings (column-annotated) and figures keep their own layout paths; the
  current listing-placeholder issue (`MTH_LISTING` columns not detected from
  `header_rows`) is fixed under the same Layer C/E work.

## Phases (each independently shippable, test-gated)

- **Phase 1 — Footnote/annotation split** (Layer B). Smallest, immediate win.
  Files: parse_shell_docx.R, ars_to_tfrmt.R (extract_footnotes), spec_to_ars.R.
- **Phase 2 — Convention-agnostic binding** (Layer A). New extract_annotation_vars
  path + `bind_annotations()`; below-table arrow + annotation-column conventions.
- **Phase 3 — Layout persistence + no-drop** (Layer C). One analysis per authored
  row; `_meta.shell_layout`; method inference from bound annotation.
- **Phase 4 — Layout-driven render + column restriction** (Layers D, E).
- **Phase 5 — Listings/figures fidelity + generality fallbacks.**

## Testing strategy

- Golden-output fixtures for ≥3 deliberately different shell designs:
  (a) disposition stub (heterogeneous rows, below-table arrows),
  (b) demographics cross-tab (in-cell brackets),
  (c) AE SOC/PT nested, (d) exposure continuous, (e) a deliberately odd layout.
- Assert per fixture: row set, row order, row labels, column set, and that
  rendered footnotes contain **no** `->`/`DATASET.VAR` annotation text.
- Regression: CDSC-ALZ-201 `T_14_1_1` renders the 6 authored rows in order,
  screen failures as a **row**, TRT01A columns without a "Screen Failure" column,
  footnotes free of annotations.
- Property test: for every parsed section, `#rendered stub rows ≥ #authored
  non-spacer rows`.

## Consequences

- Positive: faithful reproduction of arbitrary shell layouts; annotations may be
  placed however a study prefers; no orphaned/dropped rows; clean footnotes.
- Cost: a private `_meta.shell_layout` (not standard ARS — documented as
  arsbridge-private); more parser/render complexity; new fixtures to maintain.
- Boundary unchanged: statistics beyond `{cards}` scope remain ADR-0002 reserved
  cells; this ADR is about *layout*, not new statistical methods.
