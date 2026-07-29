# Root-causing a parse divergence on a locked machine

For when arsbridge misreads a client shell that cannot leave the machine it
lives on — e.g. a 4-visual-column table rendering as 10 columns whose headers
are literally `XX` / `(XX.X)`. Nothing in this workflow moves a single literal
character of study text off the machine.

## Step 0 — the diagnostics you already have (5 minutes)

After parsing the shell (any `spec_to_ars()` run, or `parse_shell_docx()`
directly), look at `diag_records()` / the validation report for the affected
TLF. Two existing messages settle a lot:

- *"appears to have a N-row header but no row is flagged `<w:tblHeader/>`;
  inferred it from the spanned first row"* — the header row count was a
  guess; note N.
- *"has a spanning column header (...) but arsbridge could not build the
  column hierarchy: `<reason>`"* — the cohort hierarchy fell back to flat
  per-grid-column headers; the reason names what was missing.

## Step 1 — capture both digests

```r
## What the DOCUMENT is (standalone; xml2 + jsonlite only):
source("tools/shell_structure_digest.R")
digest_shell("Client_Shells.docx", "geometry.json")
digest_summary("geometry.json")     # short paste-ready overview

## What the PARSER decided (arsbridge >= 0.1.0.9048):
library(arsbridge)
parse_decision_digest("Client_Shells.docx", "decision.json")
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

## Step 3 — reproduce and fix off the locked machine

Bring back only the two JSONs. Rebuild the offending geometry as a neutral
fixture with `tests/testthat/helper-rowgen.R` (`rowgen_cell(gridspan=,
vmerge=)`, multi-paragraph cells, `rowgen_table(n_cols=)`) using invented
cohort names and the digest's numbers — spans, paragraph counts, row order.
Failing test first, then the fix, then the standard gate (full suite +
byte-identical golden diff on the bundled example and CDSC fixture).
