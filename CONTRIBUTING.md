# Contributing to arsbridge

This outlines how to propose a change to arsbridge.

## Fixing typos

Small typos or grammatical errors in documentation may be edited directly
using the GitHub web interface, so long as the changes are made in the
_source_ file. This generally means you'll need to edit
[roxygen2 comments](https://roxygen2.r-lib.org/articles/roxygen2.html) in an
`.R`, not a `.Rd` file. You can find the `.R` file that generates the `.Rd`
by reading the comment in the first line.

## Bigger changes

If you want to make a bigger change, it's a good idea to first file an issue
and make sure someone from the team agrees that it's needed. If you've found
a bug, please file an issue that illustrates the bug with a minimal
[reprex](https://www.tidyverse.org/help/#reprex) (this will also help you
write a unit test, if needed).

### Pull request process

* Fork the package and clone onto your computer. If you haven't done this
  before, we recommend using `usethis::create_from_github("tavakohr/arsbridge", fork = TRUE)`.
* Install all development dependencies with `devtools::install_dev_deps()`,
  and then make sure the package passes R CMD check by running
  `devtools::check()`. If R CMD check doesn't pass cleanly, it's a good idea
  to ask for help before continuing.
* Create a Git branch for your pull request (PR). We recommend using
  `usethis::pr_init("brief-description-of-change")`.
* Make your changes, commit to git, and then create a PR by running
  `usethis::pr_push()`, and following the prompts in your browser. The title
  of your PR should briefly describe the change. The body of your PR should
  contain `Fixes #issue-number`.
* For user-facing changes, add a bullet to the top of `NEWS.md` (i.e. just
  below the first header). Follow the style described in
  <https://style.tidyverse.org/news.html>.

### Code style

* New code should follow the tidyverse [style guide](https://style.tidyverse.org).
  You can use the [styler](https://CRAN.R-project.org/package=styler) package
  to apply these styles, but please don't restyle code that has nothing to do
  with your PR.
* We use [roxygen2](https://cran.r-project.org/package=roxygen2), with
  [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html),
  for documentation.
* We use [testthat](https://cran.r-project.org/package=testthat) (edition 3)
  for unit tests. Contributions with test cases included are easier to accept.

## Where shell parsing lives

Reading an annotated shell is split in two, and a change usually belongs in
exactly one half:

* [`R/parse_shell_core.R`](R/parse_shell_core.R) — **format-agnostic.** The
  annotation grammar (`.ANNOTATION_PATTERN`, the bracket tokenizer), the
  detection layers, the TLF-heading grammar, and section assembly
  (`.new_section()`, `bind_annotations()`, the column-group resolvers,
  `.finalize_section()`). This code reads text and per-run formatting
  metadata; it never sees a document. Change it when the *convention* changes
  — a new annotation form, a new heading shape.
* [`R/parse_shell_docx.R`](R/parse_shell_docx.R) — **OOXML only.** The body
  walker, the table and header grid readers, the Word-comment and page-header
  readers, and the run/cell readers that turn `<w:r>` nodes into the run list.
  Change it when the *file format* changes — a Word construct that was being
  read wrong.
* [`R/xlsx_cells.R`](R/xlsx_cells.R) — **SpreadsheetML only.** The workbook
  counterpart: sheet index, styles, shared and inline strings, merged ranges,
  and the cell reader that turns `<c>` nodes into the same run list. Read its
  file header before changing it — it documents which format variations real
  workbooks differ on (where strings live, how boolean run properties are
  spelled, how relationship targets are written) and why the reader has to
  absorb all of them.
* [`R/xlsx_grid.R`](R/xlsx_grid.R) — **Excel sheet-layout semantics.** What
  the cells *mean*: sheet classification, banner rows, body-row kinds, the
  placeholder lexicon, and figure arrow-directives. It reads only the cell
  tables from `xlsx_cells.R`, never a file, so it is tested against synthetic
  sheets built by `tests/testthat/helper-gridgen.R` — build a sheet with the
  geometry you need rather than authoring a workbook.
* [`R/parse_shell_xlsx.R`](R/parse_shell_xlsx.R) — **assembly only.** Walks the
  sheets and fills in section objects from the two layers above. Before
  changing it, read the section-object compatibility contract in its header
  and in [`adr/0004-xlsx-shell-input.md`](adr/0004-xlsx-shell-input.md): every
  field is either identical-semantics (a difference from the Word reader is a
  bug), format-specific and whitelisted, or additive Excel-only — and nothing
  outside the fill writer may require the additive ones.

Nothing calls a reader directly: [`R/parse_shell.R`](R/parse_shell.R)
dispatches on the file extension, and adding a format is a change there plus a
new reader.

Writing back into a workbook is the mirror of reading one, and splits the same
way:

* [`R/shell_fill_meta.R`](R/shell_fill_meta.R) — **which result goes in which
  cell**, decided at build time and recorded as `_meta$shell_fill`. Change it
  when the *binding* is wrong: a row shape that selects the wrong statistics,
  a column that resolves to the wrong group.
* [`R/ars_fill_shell.R`](R/ars_fill_shell.R) — **putting it there.** Reads that
  map, looks each value up in the ARD, and edits the cell. It decides nothing
  about bindings; if a number lands in the wrong cell, the bug is almost always
  in the map, not here.

Two rules hold in the writer, and both are load-bearing:

* **Edit the run XML, never rebuild the cell.** Cells are changed through
  `wb$worksheets[[i]]$sheet_data$cc$is`, so a run that is kept is never
  deserialized. Going through `wb_add_data()` or `fmt_txt()` instead would
  drop every run property arsbridge does not model — `rFont` and `sz` are on
  every run of a real shell, and a superscript footnote marker would add
  `vertAlign`. See the Mechanism section of
  [`adr/0005-filled-shell-output.md`](adr/0005-filled-shell-output.md), and
  re-run `tools/xlsx_roundtrip_check.R` after any openxlsx2 upgrade — the
  writer depends on that internal representation.
* **Never write a value you are not sure of.** A cell whose result is missing,
  ambiguous, or reserved keeps its placeholder and is reported. An empty cell
  in a clinical table reads as a zero, and a plausible wrong number is the one
  that survives review.

The three output kinds are filled from three different places, and that is
the thing to hold in mind when changing any of them:

| | filled from | shape |
|---|---|---|
| table | the ARD, via the cell map | fixed — cells are substituted in place |
| listing | the ADaM data, via `.listing_data()` | **changes** — one template row becomes N |
| figure | the ADaM data, via the shell's prose directives | fixed block at the annotation anchor |

A listing is the awkward one. openxlsx2 has no row-insertion API, so
`.shift_rows_down()` does it: cell references, per-row records, merged ranges
and the declared extent all carry row numbers and must move together. A file
with only some of them updated still opens, so the failure is silent — a
stale merge swallows a data row, a stale dimension makes Excel ignore rows
past the old extent. Change it with a test that reads the saved file back.

It moves four things and knows it: `.unshiftable_features()` lists what it
cannot move — conditional formatting, data validation, hyperlinks, an
autofilter, worksheet tables, row breaks, formulas — and a sheet carrying any
of them is declined with a FAIL rather than shifted wrongly. If you teach the
shifter one of those, take it off that list in the same change.

Annotations are stripped BEFORE cells are filled, not after: a figure's series
is written into the cells its annotation block occupied, so the other order
erases it.

Two gates apply to any change in shared code:

* **The Word path must stay byte-identical.** Re-run `parse_decision_digest()`
  over the `.docx` fixtures and `spec_to_ars()` over the shell/spec pairs and
  require identical output (bar the version stamp and timestamp).
* **The two readers must stay in lockstep.** `test-parity_docx_xlsx.R` is the
  net; run `tools/parity_check_shell.R` against a real pair as well if you
  have one. When parity fails, the answer is almost never to relax the test —
  it is either a bug in the reader you changed, or a difference that belongs
  in the class-2 whitelist *with a written reason*.

The two halves meet at two seams, documented at the top of the core file: the
**per-run metadata list** (`text`, `raw_text`, `color_hex`, `highlight`,
`bold`, `italic`, `underline`, `strike`) and the **header-grid record**
(`row`, `col_start`, `col_end`, `text`, `annotation`, `vmerge_continue`),
which feeds the format-agnostic [`R/column_tree.R`](R/column_tree.R). Their
product is the **section object** — the contract every downstream stage
consumes. Anything that produces a conformant section object is a valid shell
reader, which is how a second input format plugs in without forking the
grammar.

## Architecture decisions

Design-level decisions live as numbered Architecture Decision Records in
[`adr/`](adr/). Read them before proposing a change to the engine's
scope or the ARD contract — they explain *why* the current boundaries exist:

* `0001-statistical-method-extensibility.md` — arsbridge bounds the *boundary*,
  not the *contents*, of the statistics space. New statistics are added as
  descriptors on the shared ARD shape, not as new `switch` branches. Inferential
  and model-based methods are tiered (descriptive → standard test → model-based
  scaffold → placeholder), and code emission stays deterministic while the LLM
  only classifies the shell.
* `0002-partial-results-traceability.md` — how a partially-computable table
  stays traceable: arsbridge fills the cells it can and reserves a keyed
  `manual_pending` stub ARD row, with provenance columns, for the rest. All
  values — computed or manual — enter at the ARD layer; nothing is typed
  straight into the rendered output. Status: proposed (phased plan, not yet
  implemented).
* `0003-shell-layout-fidelity.md` — how much of the shell's own layout the ARS
  carries (`_meta.shell_layout`, `_meta.column_tree`) so a rendered output can
  be compared against the shell it came from, and the three annotation layers
  (in-cell, below-table arrow lines, supplement) that can bind a row.
* `0004-xlsx-shell-input.md` — why Word and Excel shells are both supported
  permanently, the two seams a reader plugs into, and the three classes every
  section-object field falls in. Read it before changing either reader.
* `0005-filled-shell-output.md` — the cell map (`_meta.shell_fill`) that binds
  each placeholder to the result belonging in it: why it is built at build
  time, the three row shapes that select from the ARD differently, and why the
  ARD join keys on the operation id rather than its display name.

When you make a design-level change, add or update an ADR in the same PR. Keep
the standard `ARS → ARD → tfrmt` pipeline — the shell is never the source of
truth for layout.

## Handing work over

Investigations that outlive a session — especially any that happened on a
machine whose data cannot leave it — get written up under `notes/handoffs/`.
Start from [`tools/HANDOFF_TEMPLATE.md`](tools/HANDOFF_TEMPLATE.md).

Two of its blocks are mandatory, and both earn it. **Repository state** records
`packageVersion("arsbridge")` alongside the `HEAD` sha, because a handoff
written against a stale install describes a build nobody can identify — half
the findings of one 2026 RCA turned out to have been fixed days before it was
written. **Fill debrief** records all three `ars_fill_summary()` frames, which
name the stage each pending cell died at; without them the next reader
reconstructs that by hand.

`notes/` is gitignored, so handoffs travel by hand and have to be
self-contained. The template lives in `tools/` so that it does not.

## Code of Conduct

Please note that the arsbridge project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this
project you agree to abide by its terms.
