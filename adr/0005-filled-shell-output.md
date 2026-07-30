# ADR 0005 — Writing results back into the shell workbook

Status: Accepted (cell map and the table writer implemented 2026-07-29;
listings and figures follow)
Date: 2026-07-29
Builds on: ADR 0004 (Excel shells as input), ADR 0003 (shell layout fidelity),
ADR 0002 (partial results stay traceable)

## Context

An Excel shell already contains everything a finished table needs except the
numbers: the layout, the row labels, the column headers, the footnotes, the
merges, and — in each placeholder — the number of decimal places that cell
takes. Rendering a *new* table from the ARS reproduces all of that from
scratch and can only approximate it. Filling the shell reproduces none of it,
because it was never lost.

So the deliverable for an Excel shell is the shell itself, with the red
annotations stripped and the placeholders replaced.

That requires answering one question per cell: **which result goes here?**

## Decision

**1. The answer is recorded at build time, in `outputs[[i]]$_meta$shell_fill`.**

Not at fill time. Build time is the only moment when the shell's geometry and
the analyses exist together: the parser knows that row 11 of "Table 14.1.2" is
the "Female" level under a categorical parent, and the builder knows that
parent became `AN_T_14_1_2_002`. By the time `ars_fill_shell()` runs, the
workbook is a file and the ARD is a table, and the join would have to be
re-derived from row labels — the guesswork the shell annotations exist to
eliminate.

**2. A cell record says what the shell asked for, not what the ARD has.**

```
row, col, ref, placeholder, kind
analysis_id
group           { grouping_id, order, label }
variable_level  (a level row)
stat_line       (a statistic line)
slots           [{ order, token, decimals, start, stop,
                   operation_id, stat_name }]
```

`kind` is `result` when the cell is bound to an analysis, `pending` when it is
not — and a pending cell carries the *reason*, because "no analysis covers
this row" and "this column is not on the column axis" are different problems
for whoever opens the filled workbook. Whether a bound cell's value actually
exists is decided at fill time; a miss there is reported as pending too, never
as a blank that reads like a zero (ADR 0002).

**3. Three row shapes select from the ARD differently, and each is decided
explicitly.**

| Shape | Example | Selects |
|---|---|---|
| own analysis | `Any TEAE [ADAE.TRTEMFL='Y']` | the row's own analysis; statistics in the method's operation order |
| statistic line | `Mean (SD)` under `Age (years) [ADSL.AGE]` | the parent's analysis; the statistics the LABEL names |
| level | `Female` under `Sex, n (%) [ADSL.SEX]` | the parent's analysis, restricted to `variable_level` |

The middle case is the one worth naming: a continuous block's lines have no
analysis of their own and must not get the method's full statistic list —
"Median" shows the median only. The label-to-statistic mapping is the inverse
of `.statline_for()` in `ars_to_tfrmt.R`, which turns the same statistics back
into the same labels when rendering, and a test holds the two together.

**4. The join key to the ARD is the operation ID, not its name.**

An operation's `name` is a display label and differs between methods for the
same statistic: count-and-percentage calls them `Count`/`Percentage`, AE
frequency calls them `n`/`(%)`, subject count calls it `n`. The ARD joins on
`stat_name` from `{cards}` (`n`, `p`, `mean`, `sd`, `median`, `p25`, `p75`,
`min`, `max`), so the map translates operation ID → stat name through one
table. Getting this wrong fails silently — no ARD row matches and a
computable cell is reported pending — which is why it is a table with a test
rather than a lower-cased name.

**5. The placeholder is the format specification.**

`xx.x (xx.xx)` means one decimal then two. Nothing else in the pipeline
carries that, and nothing else needs to: the shell author already made the
decision, in the cell, and the token positions (`start`, `stop`) let the
writer substitute in place and keep the punctuation they chose.

**6. Listings and figures get plans, not cell records.** A listing is filled
by expanding one template row into N, so its plan is the header row, the
template row, the footnote row to relocate, and the column variables. A figure
is filled by writing a computed series where its annotation block was, so its
plan is that anchor row and the declared aspects.

## Mechanism

Established by `tools/xlsx_roundtrip_check.R`, which is kept so the findings
below can be re-tested whenever openxlsx2 changes.

**7. The writer edits the author's file; it never rebuilds one.**
`openxlsx2::wb_load()` → mutate → `wb_save()` was measured on both an
openpyxl-authored workbook and an openxlsx2-authored one, and is lossless:
sheets, cells, text, formatting runs, colours, merged ranges, column widths,
row heights, per-cell style indices and every zip part come back unchanged.
Overwriting a cell also leaves its `s=` style index alone, so alignment,
borders, indentation and number format survive a fill for free.

**8. Cells are edited as run XML, not through the data model.** The obvious
route — `wb_add_data()`, or rebuilding a stripped cell with `fmt_txt()` —
loses run-level formatting, because seam 1 models only what annotation
*detection* needs (`color_hex`, `highlight`, `bold`, `italic`, `underline`,
`strike`). Real shells put more than that on a run:

```xml
<r><rPr><rFont val="Arial"/><i val="1"/><color rgb="FF000000"/><sz val="10"/></rPr>
   <t xml:space="preserve">Safety Population  </t></r>
```

`rFont` and `sz` are on every run of the exemplar workbook and in neither the
run model nor `fmt_txt()`'s reach here; a superscript footnote marker
(`vertAlign`) would go the same way. Rebuilding from the model would silently
reset Arial 10 to the workbook default on every cell the writer touched — a
change nobody asked for, on exactly the cells under edit.

So the writer works on the run XML directly, through
`wb$worksheets[[i]]$sheet_data$cc$is`, which openxlsx2 stores verbatim. Runs
that are kept are kept *byte-identical*, because they are never deserialized:
stripping an annotation removes an `<r>` element and leaves its siblings
untouched, and filling a placeholder rewrites the text inside one `<t>`. The
properties arsbridge does not model are preserved precisely because it does
not model them.

This is the plan's raw-XML contingency, chosen for fidelity rather than
forced by corruption — openxlsx2 still owns the file mechanics, so there is no
hand-rolled unzip/rezip anywhere in the writer.

**9. Editing a shared string converts the cell to an inline one.** A cell's
runs live in one of two places: inline in the sheet (`t="inlineStr"`, how
openpyxl writes, and how every current fixture is authored) or interned in
`xl/sharedStrings.xml` (`t="s"`, how Excel writes — so any shell a user has
opened and re-saved). One `<si>` can back many cells, and in a TLF shell it
routinely does: `xx.x (xx.xx)` is the same string in every data cell of a
table. Editing the `<si>` in place would write one analysis's result into
every cell that happened to share its placeholder.

The writer therefore never edits a shared entry. It copies the `<si>` to the
cell as an `<is>`, sets `t="inlineStr"`, clears `<v>`, and edits the copy —
detaching that one cell and leaving every other user of the string alone. The
cost is a slightly larger file; the alternative is silent cross-contamination.

## Consequences

**A mismatch between the shell and the analysis typing becomes visible.** A
row showing `xx (xx.x)` whose analysis produces only a count leaves a slot
unbound, and that is reported as a WARN naming the row and the placeholder. On
the nine-sheet fixture this fires five times — every one of them a real
disagreement about whether a row is a count or a count with a percentage.
Previously nothing compared the two.

**`_meta` is arsbridge's own namespace**, stripped before conformance
checking, so none of this changes the ARS the standard defines. A consumer
that ignores `_meta` sees exactly what it saw before.

**Word shells are unaffected.** `shell_fill` is emitted only when the section
came from a workbook; a Word shell has no cell addresses to fill, and its
output `_meta` is byte-identical to before.

## Alternatives considered

**Re-derive the join in `ars_fill_shell()` from row labels.** Rejected: label
matching is exactly what the annotations exist to replace, and it would put a
second, weaker copy of the row model in the writer.

**Emit ARS `OutputDisplay` cell references instead of a `_meta` block.**
Rejected: ARS v1.0 has no cell-addressed display model, and inventing one
inside the standard's namespace would break conformance for a purpose the
standard does not cover.

**Fill from the rendered `{tfrmt}` table rather than the ARD.** Rejected: the
rendered table has already made formatting decisions, so the shell's own
decimal specification would be overridden by the renderer's. The ARD is the
last point at which a value is still a number.
