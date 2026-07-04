# arsbridge — Layout Fidelity & Annotation Binding: What Happened and What To Do Next

**Purpose of this file:** a self-contained handoff to move into the
`arsbridge/` repo. It explains a real failure found while running
arsbridge on a study, the verified root causes (with <file:line>), and
the concrete next steps. No prior chat context required.

**Companion:** the full design decision is in
`arsbridge/adr/0003-shell-layout-fidelity.md`. This file is the
narrative + action list; the ADR is the architecture of record.

------------------------------------------------------------------------

## 1. The test that exposed the problem

Study fixture: **CDSC-ALZ-201** (synthetic Alzheimer’s / xanomeline;
data from `pharmaverseadam`, Apache-2.0). Inputs used:

- Annotated TLF shell: `CDSC-ALZ-201_TLF_Shells_v1.0_annotated.docx`
- ADaM spec: `adam_spec_CDSC-ALZ-201.xlsx`
- SAP: `CDSC-ALZ-201_SAP_v1.0.docx`

Pipeline run (Gemini provider, only key available):
[`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
→
[`ars_to_ard()`](https://tavakohr.github.io/arsbridge/reference/ars_to_ard.md)
→
[`ars_render_all()`](https://tavakohr.github.io/arsbridge/reference/ars_render_all.md).
Runner script: `run_arsbridge_CDSC-ALZ-201.R`.

Focus table: **T_14_1_1 Subject Disposition**. The annotated shell
authored **6 stub rows** in this order:

1.  Subjects enrolled
2.  Screen failures
3.  Randomized / treated (Safety)
4.  Completed study
5.  Discontinued study
6.  (End-of-study status)

with treatment columns Placebo / Xanomeline Low / Xanomeline High, and
the variable mapping given as **red annotation lines placed *below* the
table**:

    Percentages based on number of subjects enrolled in each treatment group.   <- real footnote
    Subjects enrolled -> count of ADSL.USUBJID                                   <- annotation
    Screen failures -> ADSL.TRT01P = 'Screen Failure'                            <- annotation
    Randomized / treated (Safety) -> ADSL.SAFFL = 'Y'                            <- annotation
    Completed / Discontinued -> ADSL.EOSSTT (COMPLETED / DISCONTINUED)           <- annotation
    Treatment columns -> ADSL.TRT01A                                             <- annotation

## 2. What arsbridge produced (the failure)

The rendered table had **3 data rows**, not 6:

- row stub = `EOSSTT` levels: `Total / COMPLETED / DISCONTINUED`
- a spurious **“Screen Failure” treatment column** (4th column)
- the 5 annotation lines printed as **footnotes / source notes**

So: authored row structure lost, screen failures became a column instead
of a row, two rows (enrolled, screen-fail) dropped, three rows collapsed
onto one variable, and the programmer annotations leaked into the final
footnotes.

## 3. Verified evidence (not inference)

Running `parse_shell_docx()` on the shell and inspecting section 1
returned:

    STUB ROWS (label | has_annot | annotation):
      Subjects enrolled              | FALSE |
      Screen failures                | FALSE |
      Randomized / treated (Safety)  | FALSE |
      Completed study                | FALSE |
      Discontinued study             | FALSE |
      (End-of-study status)          | FALSE |

    FOOTNOTES captured (6):
      - Percentages based on number of subjects enrolled in each treatment group.
      - Subjects enrolled -> count of ADSL.USUBJID
      - Screen failures -> ADSL.TRT01P = 'Screen Failure'
      - Randomized / treated (Safety) -> ADSL.SAFFL = 'Y'
      - Completed / Discontinued -> ADSL.EOSSTT (COMPLETED / DISCONTINUED)
      - Treatment columns -> ADSL.TRT01A

**Every stub row parsed with `has_annot = FALSE`**, and **all 5
annotation lines were captured as footnotes.** The ARS analyses that did
appear (SAFFL, EOSSTT×3) came from the **LLM guessing** off row labels +
SAP text — not from the annotations. That is why enrolled/screen-fail
were dropped and EOSSTT/TRT01A were inferred.

## 4. Root causes (<file:line> in the arsbridge repo)

1.  **Annotation reader supports only one placement.**
    `.detect_annotation()` (`R/parse_shell_docx.R:453`) reads an
    annotation only when it is *inside the stub cell* (colour / bold /
    bracket / plain `DATASET.VAR`). The shell here annotates as
    `Label -> DATASET.VAR` lines *below the table*, a reasonable
    convention arsbridge does not read → all rows `has_annot = FALSE`.

2.  **Footnote catch-all swallows annotations.**
    `R/parse_shell_docx.R:163-167` routes any post-table paragraph \>10
    chars into `footnotes` with no test for annotation runs. The
    annotation lines flow through `build_ars_json` into the ARS
    `Footnote` display section, and `extract_footnotes()`
    (`R/ars_to_tfrmt.R:69`) ships them as GT source notes.

3.  **Rows are rebuilt from the ARD, not the shell.**
    `detect_row_roles()` (`R/ars_to_tfrmt.R:183`) sets
    `label_var = .ARS_ROW_LABEL` (synthetic) and groups by ADaM
    `variable`; `.tfrmt_prep_ard()` (`R/ars_to_tfrmt.R:371-390`) labels
    rows by `variable_level` (COMPLETED/DISCONTINUED). Authored stub
    labels and order are never consulted.

4.  **Rows dropped / collapsed.** With no in-cell annotations, the LLM
    produced 4 analyses for 6 rows (enrolled & screen-fail dropped) and
    pointed 3 at EOSSTT, which the renderer dedups into one block.

5.  **Population level leaks into columns.** `detect_col_var()`
    (`R/ars_to_tfrmt.R:131`) picks `TRT01A`, which carries a “Screen
    Failure” level (52 subjects) → renders as a treatment column.

**Unifying problem:** arsbridge is semantics-first (variable + method →
generic cross-tab). It has **no first-class model of the authored table
layout**, and its annotation reader supports only one placement
convention.

## 5. What to do next — five layers, phased

Full detail in `adr/0003-shell-layout-fidelity.md`. Summary of the work:

| Phase | Change | Files |
|----|----|----|
| **1 — Footnote/annotation split** (do first; isolated win) | Classify each post-table paragraph as annotation / source / true footnote. Route annotations out of `footnotes` into a new `sec$programmer_annotations` (kept for the validation report, never shipped). | `R/parse_shell_docx.R:163-167`, `R/ars_to_tfrmt.R:69`, `R/spec_to_ars.R` (add `ship_annotations = FALSE`) |
| **2 — Convention-agnostic binding** | New `bind_annotations(section)`: collect annotation-bearing paragraphs (coloured run, or `.ANNOTATION_PATTERN`, or `^\s*<label>\s*->\s*<annotation>` arrow form), fuzzy-match the left side to a `stub_rows$label`, and set that row’s `annotation` + `has_annot=TRUE`. Makes placement irrelevant. | `R/parse_shell_docx.R`, new `R/extract_annotation_vars.R` path |
| **3 — Layout persistence + no-drop** | Emit one analysis per authored stub row, in order, method inferred from the bound annotation (subject-count / filtered count / categorical / continuous / label-only). Write `output$_meta.shell_layout` = ordered `{order,label,indent,analysis_id,kind}`. Never drop an annotated row. | `R/build_ars_json.R` |
| **4 — Layout-driven render + column restriction** | When `_meta.shell_layout` exists, build tfrmt `label` from the authored stub label (join ARD by `analysis_id`), pin row order to layout order, restrict the column axis to the arm levels in the shell `col_headers` (screen failures stay a row). | `R/ars_to_tfrmt.R` (`detect_row_roles`, `.tfrmt_prep_ard`, `build_col_levels`) |
| **5 — Listings/figures fidelity** | Fix listing column detection from `header_rows` (`MTH_LISTING`) and figure dataset mapping (the pulse figure was mis-assigned to a non-existent `ADEFF`). | `R/build_ars_json.R`, `R/ars_render_figure.R`, `R/ars_render_listing.R` |

### Phase 1 concrete steps (start here)

1.  In `parse_shell_docx.R`, before the footnote branch (line ~163), add
    a classifier: a paragraph is an **annotation** if any run is
    non-grey/non-black coloured, OR its text matches
    `.ANNOTATION_PATTERN`, OR it matches `^\s*.+?\s*->\s*.+$`. Append
    those to `current$programmer_annotations` and
    [`next`](https://rdrr.io/r/base/Control.html) (do not add to
    `footnotes`).
2.  Keep the source-line and true-footnote branches as they are.
3.  Ensure `build_ars_json` does **not** copy `programmer_annotations`
    into the ARS `Footnote` section.
4.  Add `ship_annotations = FALSE` to
    [`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
    (default off).
5.  Test: assert the parsed disposition section has `footnotes` == just
    the one real footnote and `programmer_annotations` has the 5 arrow
    lines; assert the rendered docx source-notes contain no `->` /
    `DATASET.VAR` text.

## 6. Testing strategy

- Copy `CDSC-ALZ-201_TLF_Shells_v1.0_annotated.docx` +
  `adam_spec_CDSC-ALZ-201.xlsx` into `tests/testthat/fixtures/` as a
  permanent regression case.
- Golden-output fixtures for ≥3 different shell designs (heterogeneous
  disposition stub / demographics cross-tab / AE SOC-PT nested /
  continuous exposure / a deliberately odd layout).
- Per fixture assert: row set, row order, row labels, column set, and
  **no annotation text in footnotes**.
- Property test: for every parsed section,
  `#rendered stub rows ≥ #authored non-spacer rows` (guards against
  silent drops).
- Regression target for `T_14_1_1`: 6 authored rows in order, screen
  failures as a **row**, TRT01A columns without a “Screen Failure”
  column, clean footnotes.

## 7. How to work (folder workflow)

- **Do the code work in `arsbridge/`** — the changes need
  `devtools::load_all`, `roxygen2`, `testthat`, `R CMD check`, which
  only work inside the package.

- **Use this study as the integration fixture.** After each phase: run
  the package unit tests, then re-run `run_arsbridge_CDSC-ALZ-201.R` (in
  the simulator folder) and eyeball
  `arsbridge_out/CDSC-ALZ-201_TLFs_arsbridge.docx`.

- Reproduce the parse evidence any time with:

  ``` r

  devtools::load_all("path/to/arsbridge")
  spec <- parse_adam_spec("adam_spec_CDSC-ALZ-201.xlsx")
  secs <- parse_shell_docx("CDSC-ALZ-201_TLF_Shells_v1.0_annotated.docx",
                           spec_lookup = spec$lookup)
  str(secs[[1]]$stub_rows); secs[[1]]$footnotes
  ```

## 8. Scope boundary (unchanged)

This is a **layout** problem, not a statistics problem. Methods beyond
the [cards](https://github.com/insightsengineering/cards) scope remain
ADR-0002 reserved (`[‡ manual]`) cells. ADR 0003 does not add
statistical methods; it makes arsbridge reproduce the authored shell
layout and read annotations regardless of where they are placed.

## 9. Environment notes

- R 4.6.0 at `C:\Program Files\R\R-4.6.0`.
- arsbridge dependencies resolve from the Pharmaverse renv library:
  `projects/Pharmaverse/renv/library/windows/R-4.6/x86_64-w64-mingw32`
  (append to [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) when
  running from a bare Rscript).
- Only `GEMINI_API_KEY` is set in this environment;
  [`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
  requires an active LLM provider even for regex-only parsing.
