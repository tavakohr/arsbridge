# Root-causing a parse divergence on a locked machine

For when arsbridge misreads a client shell that cannot leave the machine it
lives on — e.g. a 4-visual-column table rendering as 10 columns whose headers
are literally `XX` / `(XX.X)`. Nothing in this workflow moves a single literal
character of study text off the machine.

Two shell formats, two sets of tools. Word shells use
`tools/shell_structure_digest.R`; Excel workbooks use
`tools/shell_structure_digest_xlsx.R`. Both write the same kind of A/a/9
digest and `parse_decision_digest()` works on either, so Steps 0–3 read the
same whichever you have. Where a step differs, the Excel version is called out.

**If you have the study in BOTH formats, start with Step 0b instead** — the
comparison localizes a divergence far faster than either digest alone.

## Step 0 — the diagnostics you already have (5 minutes)

After parsing the shell (any `spec_to_ars()` run, or `parse_shell()`
directly), look at `diag_records()` / the validation report for the affected
TLF. Two existing messages settle a lot:

- *"appears to have a N-row header but no row is flagged `<w:tblHeader/>`;
  inferred it from the spanned first row"* — the header row count was a
  guess; note N.
- *"has a spanning column header (...) but arsbridge could not build the
  column hierarchy: `<reason>`"* — the cohort hierarchy fell back to flat
  per-grid-column headers; the reason names what was missing.

## Step 0b — if the study exists in both formats (2 minutes)

A shell migrated from Word to Excel, or kept in both, gives you a second
opinion for free. `tools/parity_check_shell.R` reads both and reports every
class-1 section field that disagrees:

```sh
Rscript tools/parity_check_shell.R Client_Shells.docx Client_Shells.xlsx spec.xlsx
```

It exits 0 when the two agree and 1 otherwise, and it names the exact field
and output that differ — usually turning "the table is wrong" into "the column
axis resolved in one reader and not the other" before you open a digest at
all. Differences that are legitimately allowed (the class-2 whitelist from
`adr/0004-xlsx-shell-input.md`) are printed with the reason each is allowed.

**This output quotes your shell's text.** It stays on the locked machine —
only the digests below are shareable.

## Step 1 — capture both digests

```r
## What the DOCUMENT is (standalone; xml2 + jsonlite only):
source("tools/shell_structure_digest.R")          # a Word shell
digest_shell("Client_Shells.docx", "geometry.json")
digest_summary("geometry.json")     # short paste-ready overview

source("tools/shell_structure_digest_xlsx.R")     # an Excel workbook
digest_shell_xlsx("Client_Shells.xlsx", "geometry.json")
digest_xlsx_summary(digest_shell_xlsx("Client_Shells.xlsx", "geometry.json"))

## What the PARSER decided (arsbridge >= 0.1.0.9048; either format):
library(arsbridge)
parse_decision_digest("Client_Shells.xlsx", "decision.json")
```

Both files contain only A/a/9 character-class silhouettes ("Cohort 1 (N=XX)"
→ `Aaaaaa 9 (A=AA)`; a placeholder `xx (xx.x)` → `aa (aa.a)`). **Open and
read both before sending them anywhere** — that is the rule the tools are
built around, not a substitute for it.

## Step 2 — the diff

Find the affected table in `geometry.json` (tables appear in document
order) and its TLF in `decision.json`, then answer these in order. The
first mismatch is the root cause:

1. **Header flag.** Geometry: does any row have `repeat_as_header = true`?
   Decision: `header_rows_flagged`. If geometry says 0, the header was
   inferred — go to 2.
2. **Header count.** Geometry: which leading rows LOOK like headers
   (cohort-shaped silhouettes `Aaa/AAA Aaaaaa (A=AA)`, spanned cells)?
   Decision: `n_header_rows`. If decision counts MORE rows than look like
   headers, a placeholder row (silhouette `aa` / `(aa.a)`, blank first
   cell) was absorbed into the header — that is the blank-first-cell
   inference rule firing on the wrong row.
3. **Label survival.** Decision: do the `col_headers` silhouettes look like
   cohorts (`Aaaaaa 9 (A=AA)`) or placeholders (`aa` / `(aa.a)`)?
   Placeholder-shaped labels with `n_header_rows = 1` mean the REAL header
   cells produced empty labels (annotation detection swallowed the whole
   cell) and lower rows won.
4. **Width.** Geometry grid width (count of `gridCol`, or widest row) vs
   decision `n_physical_cols` vs `n_col_headers`. `n_col_headers` equal to
   the physical width with a spanning header in geometry means the
   hierarchy flattened — check `tree_mode` (`"none"`/`"FLAT"` = no
   hierarchy was built) and Step 0's second message for the reason.
5. **Rows.** Geometry struck/vmerge rows vs decision `stub_rows` (each has
   `has_annot` + `detection_method`) — confirms nothing was dropped or
   double-counted around the header boundary.

### Reading an Excel geometry digest

The questions are the same; the evidence sits in different fields.

- **Banner.** `populated_rows` is the sparse list of rows that exist at all —
  the gaps are the author's spacer rows. If the first populated row is not 1,
  something was inserted above the banner and the locators had to scan.
- **Header.** `merges` on the header row is what makes a multi-row header:
  a merge with `col_start > 1` spanning several columns is a group header,
  and the parser then takes the row below it as sub-columns. `n_header_rows`
  in the decision digest says whether it did.
- **Annotations.** `n_coloured_cells` per sheet against the decision digest's
  count of `has_annot` rows. A sheet with coloured cells but no annotated rows
  means the colour is there and the grammar did not match it — compare the
  `colours` list against `C00000`.
- **Placeholders.** Cell silhouettes: a placeholder is `aa`, `aa (aa.a)`,
  `aa, aa`; a label is anything with letters left in it. A result column whose
  cells silhouette as labels was never going to be filled.

## Step 3 — reproduce and fix off the locked machine

Bring back only the two JSONs. Rebuild the offending geometry as a neutral
fixture:

- **Word** — `tests/testthat/helper-rowgen.R` (`rowgen_cell(gridspan=,
  vmerge=)`, multi-paragraph cells, `rowgen_table(n_cols=)`).
- **Excel** — `tests/testthat/helper-gridgen.R` (`gridgen_cell(annot=, red=)`,
  `gridgen_sheet(merges=)`, `gridgen_tlf(offset=, population=, header=,
  footnote=)`), which builds the sheet object directly, so no workbook is
  needed.

Use invented cohort names and the digest's numbers — spans, populated rows,
merges, row order. Failing test first, then the fix, then the standard gate:
full suite, byte-identical golden diff on the bundled example and the CDSC
fixture, and — for any change to `R/parse_shell_core.R` — the docx↔xlsx parity
test plus `tools/parity_check_shell.R` on your own pair.

## When the workbook comes back unfilled

The fill stage keeps a per-cell census with a REASON string for every cell it
did not fill. The reason strings are arsbridge's own wording -- no study text
-- so their frequency table is safe to share as-is, and on a run that filled
nothing its value distribution IS the diagnosis. From a workflow project:

```r
table(readRDS(file.path(project, "ars", "last_run.rds"))$unfilled_cells$reason)
```

(For a hand-run pipeline, the same frame is `ars_fill_shell(...)$diagnostics`.
Read it from the SAME call's return value -- `ars_to_ard()` resets the shared
diagnostic collector, so nothing upstream survives a re-read.)

What a dominant reason means:

| dominant reason | what happened | first move |
|---|---|---|
| `no analysis covers this row` | No annotation was bound to the rows -- a clean shell, or a convention the parser missed. | Look at the shell: if it is unannotated, that is the whole story -- annotate it, or declare the bindings in a reviewed supplement (it can bind a clean shell's rows). If it IS annotated, run the digest diff (Step 1). |
| `no result in the ARD for this cell` | The rows bound and executed, but the value-to-cell join failed, or the level is absent from the data. | Before 0.1.0.9060, check the column headers for decorations like `(N=XX)` -- the join used the header verbatim. From .9060 they are stripped; a residue of these on scattered cells usually means a level with no data (a zero-count arm), which is left as authored by design. |
| `the column is not on the output's column axis` | A column never mapped -- historically a blank header cell (an annotation-only stub header) shifting the axis. | Fixed in 0.1.0.9060 (physical column indices). If it persists, send the decision digest: `col_headers` count vs `n_physical_cols` is the tell. |
| `reserved for manual derivation` | ADR 0002: an inferential cell arsbridge does not compute. | Expected. `ars_manual_worklist()` lists them. |
| `the placeholder asks for a statistic the analysis does not produce` | The placeholder has more tokens than the method has statistics (e.g. `xx (xx.x)` on a row whose method yields only a count). | Expected today for filter rows (`[VAR = 'value']`); the count fills and the extra token stays. |
| `the row stands for a repeated block, which needs row expansion` | A template row (`<System Organ Class>`) answered by many levels. | Expected outside listings; report if it appears on a plain data row. |
| `the output names sheet '...', which is not in the workbook` / `no cell map recorded for this output` | The fill was pointed at a different workbook than the ARS was built from, or the output carried no map. | Fill the exact file `spec_to_ars()` parsed, unrenamed. |

The census is also on screen: the app's Results step renders it as a
filterable table, and a build that filled nothing says so in its completion
notification.
