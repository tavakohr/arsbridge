# ADR 0004 — Excel workbooks as shell input, alongside Word

Status: Accepted (implemented 2026-07-29, PRs 1–4 of the Excel series)
Date: 2026-07-29
Builds on: ADR 0003 (shell layout fidelity — the section object and the three
annotation layers it defines are what both readers now produce)

## Context — why a second input format

`parse_shell_docx()` needed four consecutive hardening PRs (#38–#41) to survive
real-world OOXML geometry: row accounting as an invariant, a grid-first table
model, a continuation-geometry guard, and a lossless cell-paragraph model with
dual-join annotation detection. Each fixed a real defect. None of them was
about clinical content — all four were about Word.

The reason is structural. A `.docx` table is not a grid: `gridSpan` makes a row
carry fewer cells than the table has columns, `vMerge` leaves ghost cells, a
"repeat header row" flag that authors rarely set is the only reliable marker of
a multi-row header, a label that wraps becomes several paragraphs, and an
annotation can break mid-token across them. Every one of those is a place where
the parser has to *infer* what the author meant. The inference is now good, but
it will never be free.

An annotated shell authored as an Excel workbook removes most of that
inference. A worksheet **is** a grid: a cell has a row and a column, a merge is
declared explicitly with its range, and a wrapped label is still one cell.
Sponsors already maintain shells in Excel. So the input side gains a second
reader rather than swapping one for the other.

## Decision

**1. Both formats are supported, permanently.** Word input is not deprecated
and there is no removal phase. Users with Word shells keep working; the
docx-vs-xlsx comparison also becomes the correctness oracle for the new reader
(PR5's `tools/parity_check_shell.R`).

**2. The two readers share one implementation of everything above the file
format.** `R/parse_shell_core.R` holds the annotation grammar, the bracket
tokenizer, the four detection layers, the heading grammar, row binding, the
column-group resolvers, and section finalization. `parse_shell_docx.R` and
`parse_shell_xlsx.R` hold only the walk over their own format. A change to what
an annotation *means* is made once.

**3. Two seams join a reader to the core**, both documented at the top of
`parse_shell_core.R`:

- the **per-run metadata list** — `text`, `raw_text`, `color_hex`, `highlight`,
  `bold`, `italic`, `underline`, `strike` — which is what the detection layers
  read;
- the **header-grid record** — `row`, `col_start`, `col_end`, `text`,
  `annotation`, `vmerge_continue` — which is what `column_tree.R` reads.

A reader is valid when it honours both and produces a conformant section
object. Nothing else about it is prescribed.

**4. The section object is the contract**, and every field is in exactly one
of three classes.

### Class 1 — identical semantics

The same shell content must produce the same value from either reader.

`tlf_number`, `tlf_type`, `title`, `population_text`, `population_annot`,
`footnotes`, `source_datasets`, `col_headers`, `n_data_cols`,
`column_annotation`, `column_groups`, `column_tree`, `include_total_hint`, and
each stub row's `label` / `annotation` / `has_annot`.

These are what `tools/parity_check_shell.R` compares between the Word and Excel
renderings of the same study. **A difference here is a bug in one of the two
readers until it is justified in writing.**

### Class 2 — format-specific, whitelisted

Fields that legitimately differ because the formats differ:

| Field | Why it differs |
|---|---|
| `detection_method` | Namespaced per format (`xlsx_rich_run`, `xlsx_cell_font`) so a reviewer can see which convention supplied the evidence. The `annotation` it accompanies is class 1 and must match. |
| `raw_heading` | A Word heading paragraph has no Excel equivalent; the sheet's number row (or its name) is used. |
| `header_rows_flagged` | Always `0` in Excel — there is no `<w:tblHeader/>` to flag, so a multi-row header is always inferred. |
| `header_rows_inferred`, `n_header_rows`, `n_physical_cols` | Counted against different geometries. |

### Class 3 — additive, Excel-only

`sheet_name`, `source_format`, `layout`, `figure_spec`, `template_row`, and
each stub row's `sheet_row` / `row_kind` / `is_template`.

These exist so the fill writer can find its way back to the cell a number
belongs in. **No existing consumer may require them.** Anything that reads a
section object must still work when they are absent — which is what keeps the
Word path unaffected by their existence.

## Consequences

**The Word path must not change.** Every PR in this series re-runs
`parse_decision_digest()` over all 21 Word fixtures and `spec_to_ars()` over 7
shell/spec pairs, and requires byte-identical output. Shared code was extracted
by verbatim motion, and the property has held through four PRs.

**Excel conventions are read, not imposed.** The layout convention (banner in
rows 1–4, one worksheet per output, red italic bracket annotations) is the
first guess in every locator, never the only one. A shell with a row inserted
above the banner still parses; the deviation is reported. Only a sheet with no
recognisable output number at all is skipped.

**Two things Excel states that Word cannot.** A placeholder (`xx (xx.x)`)
declares both where a result goes and how many decimals it takes, which is what
makes writing results back into the shell possible without a separate format
declaration (ADR 0005). And a figure sheet can carry its whole specification as
`X axis -> ADVS.AVISITN` prose, which has no Word precedent — so that grammar
is new here, and unreadable lines are reported and kept rather than dropped.

**One repair is Excel-only and deliberate.** A population line with a doubled
dot (`ADSL..SAFFL='Y'`) is repaired and read, with a WARN naming the repair,
*only* when the line carries no annotation colour at all. The Word reader
leaves the same typo unread. This is an accepted class-2 divergence: it is
strictly more information, it is always reported as a repair rather than as
what the shell says, and the alternative — changing the shared grammar — would
alter Word output for every existing user.

## Alternatives considered

**Replace Word with Excel.** Rejected by the user after first accepting it:
existing Word shells are real work, and keeping both gives a free correctness
oracle.

**Convert `.xlsx` to `.docx` and reuse one reader.** Rejected. The conversion
would have to synthesize the OOXML geometry the docx reader infers from, which
means writing the ambiguity back in on purpose.

**Use readxl / `openxlsx2::wb_to_df()` instead of parsing the XML.** Rejected,
and this is not a preference. An annotated shell carries the annotation as a
second formatting *run* inside the cell; every high-level reader returns that
cell as one flattened string and discards the run colours. Red font is the
primary annotation signal, so per-run reading is a requirement. `tidyxl` does
expose runs, but adds a compiled dependency that duplicates what the reader
already needs `xml2` for.

**Add a `format` argument to `parse_shell_docx()`.** Rejected: one function
with two file walkers inside it is the fork this ADR exists to prevent.
