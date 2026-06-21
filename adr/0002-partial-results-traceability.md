# ADR 0002 — Partial results with intact traceability (stub ARD rows + provenance)

* Status: Accepted -- phases 1-5 implemented
* Date: 2026-06-19 (accepted 2026-06-20)
* Depends on: ADR 0001 (statistical-method extensibility)
* Pipeline constraint: stay on the standard **ARS → ARD → tfrmt** flow. No
  shell-dominant rendering. The shell is never the source of truth for layout.

## Problem

A high-complexity table (e.g. T-14.2.1) mixes cells arsbridge *can* compute
(`n / N' (%)`) with cells it *cannot* (`{cardx}`-class CMH, Clopper-Pearson,
Newcombe; or fully out-of-scope NRI). We want arsbridge to fill what it can and
leave the rest for an analyst to compute manually — **without breaking
traceability**.

The hazard: a manually-computed value typed straight into the rendered Word
cell is an **orphan** — no `analysis_id`, no `method_id`, no provenance. The
audit chain Output → Analysis → Method → result → cell is severed.

Worse, today the chain is *already* severed upstream for gated tables. The
capability gate (`R/build_ars_json.R:311`) emits the Output with **no
analyses**:

```r
if (isTRUE(sec$unsupported)) {
  out_obj <- .build_output(sec, character())   # <-- analyses stripped
  outputs[[length(outputs) + 1L]] <- out_obj
  unsupported[[length(unsupported) + 1L]] <- list(id = ..., reason = ...)
  next
}
```

So the ARS describes the table's *existence* but not its *analysis*. There is
nothing in the ARS or ARD to hang a manual value on.

## Decision

**Every result — computed or manual — is a keyed ARD row. The ARD is the single
point of entry for all values.** Manual values enter at the ARD layer into a
pre-reserved, fully-keyed **stub row**, never at the render layer.

Three moves:

1. **ARS keeps the analysis + method for gated tables.** The ARS *describes*;
   arsbridge *executes*. A CMH analysis is fully describable even with no
   executor. Stop stripping analyses at the gate.
2. **ARD reserves a keyed stub row** (`result_status = "manual_pending"`,
   `stat = NA`) for every cell the engine cannot compute, instead of skipping
   the analysis.
3. **Provenance columns** travel on every ARD row, so a later manual fill
   records *where the value came from* (a validated program), and an auditor
   can tell computed from manual at the cell level.

tfrmt renders the ARD unchanged — it does not care whether a `stat` came from
`{cardx}` or a human; both are keyed rows.

## ARD contract change — provenance columns

Add to the ARD produced by `ars_to_ard()` (today’s traceability columns are
stamped at `R/ars_to_ard.R:552-557`):

| column | type | computed cell | manual_pending stub | manual_filled |
| --- | --- | --- | --- | --- |
| `result_status` | chr | `computed` | `manual_pending` | `manual_filled` |
| `value_source` | chr | `cards` / `cardx` | `NA` | `manual` |
| `derivation_ref` | chr | emitted block id / path | `NA` | path/id of the validated program that produced the value |
| `derived_by` | chr | `arsbridge` | `NA` | analyst id |
| `derived_dt` | chr (ISO-8601) | run timestamp | `NA` | fill timestamp |

Notes:
* `derivation_ref` is the real trace: not "a human typed 0.023" but "value from
  `cmh_t1421.R`, validated under ticket ABC". A manual computation is itself a
  validated script; the ARD row points at it. Double-programming/QC preserved.
* Keep columns as plain character to survive `cards::bind_ard()` and round-trips
  through CSV. `derived_dt` is character ISO-8601, not POSIXct, to avoid
  timezone drift in stored ARDs.
* Computed rows get `result_status = "computed"` by default, so existing
  consumers see a fully-populated, self-describing ARD.

## Edits — phase by phase

### Phase 1 — provenance columns on computed rows (no behaviour change)

*Goal: ship the contract first; every computed row self-describes. Low risk.*

* `R/ars_to_ard.R` (~L552, the metadata stamp block): after the existing
  `ard[["method_actual"]] <- method_actual`, add
  ```r
  ard[["result_status"]] <- "computed"
  ard[["value_source"]]  <- if (method_actual %in% c("FALLBACK_CONTINUOUS",
                                                     "FALLBACK_CATEGORICAL"))
                              "cards" else "cardx_or_cards"   # refine per descriptor
  ard[["derivation_ref"]] <- res$analysis_id
  ard[["derived_by"]]     <- "arsbridge"
  ard[["derived_dt"]]     <- NA_character_   # stamped by caller, see below
  ```
  `derived_dt`: do **not** call `Sys.time()` deep in the loop (keeps the
  function pure / reproducible for the engine-equivalence test). Stamp once
  after assembly in `ars_to_ard()` just before `return(final_ard)`:
  `final_ard$derived_dt <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")`, guarded so
  tests can pass a fixed value.
* Update the ARD documentation in the `@return` roxygen of `ars_to_ard()` and in
  `vignette("getting-started")`.
* Test: `tests/testthat/test-ars_to_ard.R` — assert the five columns exist and
  `result_status == "computed"` for a normal run.

### Phase 2 — stub-row helper + emit instead of skip (engine)

*Goal: analyses the engine can’t compute produce a keyed `manual_pending` row.*

* New internal helper in `R/ars_to_ard.R`:
  ```r
  ## A keyed, value-less ARD row reserved for a cell arsbridge cannot compute.
  ## Same `card` schema as a real result so tfrmt and bind_ard treat it
  ## identically; the value is NA and result_status flags it for manual fill.
  .stub_ard_row <- function(res, stat_name, group_label = NA_character_) {
    tibble::tibble(
      group1       = NA_character_, group1_level = group_label,
      variable     = res$variable %||% NA_character_,
      stat_name    = stat_name,
      stat         = list(NA_real_),
      stat_label   = stat_name,
      context      = res$method_id %||% NA_character_,
      result_status = "manual_pending",
      value_source  = NA_character_,
      derivation_ref = NA_character_,
      derived_by     = NA_character_, derived_dt = NA_character_
    ) |> structure(class = c("card", "tbl_df", "tbl", "data.frame"))
  }
  ```
  (Exact columns to be matched against a live `cards::ard_*` row before coding —
  see "Open question 1".)
* In the executor loop, the two skip sites become stub emitters:
  * unknown/unsupported method (currently the fallback at `R/ars_to_ard.R:445`):
    when the method is a *declared-but-unexecutable* descriptor (not a fallback
    candidate), emit `.stub_ard_row()` rows for each statistic the method’s
    descriptor declares, instead of the generic count fallback.
  * `cards` calculation error (`tryCatch` at `R/ars_to_ard.R:535`): on error for
    a known-but-failed cell, optionally emit a stub rather than dropping the row
    (keeps the cell visible and keyed). Gate behind a flag to preserve current
    behaviour for genuine bugs.
* The stub rows still get the traceability stamp block, so they carry
  `analysis_id`, `method_id`, `output_id`.

### Phase 3 — ARS keeps the analysis for gated tables (spec)

*Goal: restore the Output → Analysis → Method chain for capability-gated tables.*

* `R/build_ars_json.R:311-318`: stop stripping analyses. Build the analysis +
  method as normal, but tag them, e.g. add `supported = FALSE` and
  `unsupported_reason` to the method/analysis object, and still record
  `_meta.unsupported_outputs` for the render manifest.
* Requires a **declarative method id** for the gated statistic (e.g.
  `MTH_CMH_TEST`) even before an executor exists — this is the ADR 0001
  descriptor registry. A descriptor with no `emit_template` = "describable, not
  executable" → engine emits stub rows in Phase 2.
* Test: `tests/testthat/test-build_ars_json.R` — a gated section yields an Output
  *with* a referenced analysis whose method is flagged unsupported.

### Phase 4 — manual worklist + render marking

* New exported helper `ars_manual_worklist(ard)`: returns a data frame of every
  `result_status == "manual_pending"` row (`output_id`, `analysis_id`,
  `method_id`, `stat_name`, reason). This is the analyst’s checklist and the
  validation trace of which cells need hand-computation.
* `R/ars_render_docx.R`: when a rendered table contains `manual_pending` cells,
  render the cell with the loud filler `[‡ manual]` (never blank, never `NA`)
  and add a table footnote: "Cells marked ‡ require manual derivation — see
  manual worklist." Driven entirely by `result_status`; **no layout change**,
  pure ARD→tfrmt.
* The existing whole-table placeholder path stays as the fallback when an Output
  has *no* computable cells at all.

### Phase 5 — manual fill round-trip (docs + guard)

* Document the supported manual-fill workflow: export the ARD (CSV), the analyst
  fills `stat`, sets `result_status = "manual_filled"`, `value_source =
  "manual"`, `derivation_ref = <validated program>`, `derived_by`, `derived_dt`,
  re-imports, re-renders. The ARD is a dataset — diffable, versioned, auditable.
* Add a validation guard: a `manual_filled` row with empty `derivation_ref` is a
  blocker (no untraceable manual values), surfaced via `ars_diagnostics()`.

## The chain, intact

```
ARS:   Output ─ Analysis ─ Method(MTH_CMH_TEST, supported=FALSE)   ← Phase 3
ARD:   keyed stub row, result_status = manual_pending              ← Phase 2
QC:    analyst runs validated program → fills the row,
       value_source=manual, derivation_ref=<program>               ← Phase 5
tfrmt: renders the row like any other; cell marked ‡               ← Phase 4
```

Nothing is ever "nothing". The slot is always reserved and keyed; the human
fills a known slot, not an orphan; the fill records its source. Traceability
holds because the ARD is the single point of entry for all results.

## Implementation status

All five phases are implemented (arsbridge 0.1.0 development line).

| Phase | What shipped | Where |
| --- | --- | --- |
| 1 | Provenance columns on every computed row | `ars_to_ard()` |
| 2 | `.UNEXECUTABLE_METHODS` registry, `.stub_ard_for_method()`, `manual_pending` stubs, `ars_manual_worklist()` | `ars_to_ard.R` |
| 3 | Gate keeps the analysis + declarative `MTH_UNSUPPORTED_ANALYSIS` method | `build_ars_json.R` |
| 4 | `[‡ manual]` marker + footnote; render-vs-placeholder by computable-cell count | `ars_to_tfrmt.R`, `ars_render_docx.R` |
| 5 | `ars_validate_manual_fills()` guard, render-time blocker, filled-value rendering, round-trip docs | `ars_to_ard.R`, `ars_render_docx.R`, `vignette("getting-started")` |

The four open questions below were resolved during a read-only spike before
Phase 2: cards 0.8.0 row schema confirmed; `bind_ard()` tolerates the extra
provenance columns and a `stat = list(NA_real_)` list-column; an NA-stat row
survives flattening and renders to the marker via `frmt(missing=)`; and the
engine-equivalence test's `.eq_norm` projection already drops provenance
columns, so it needed no change.

## Open questions (resolved during the Phase 2 spike)

1. **Exact stub schema.** Match `.stub_ard_row()` column-for-column against a
   live `cards::ard_categorical()` row (group/variable/stat/context columns vary
   by `{cards}` version, pinned `>= 0.2.0`). The stub must `bind_ard()` cleanly
   with real rows.
2. **bind_ard tolerance.** Confirm `cards::bind_ard()` accepts the extra
   provenance columns without dropping them or erroring on the list-column
   `stat = list(NA_real_)`.
3. **tfrmt NA handling.** Confirm tfrmt renders a `manual_pending` row to the
   `[‡ manual]` filler rather than silently dropping a row whose `stat` is NA.
4. **Engine-equivalence test.** The legacy registry path
   (`test-engine_equivalence.R`) must either gain the same provenance columns or
   be excluded from the comparison on those columns.

## Consequences

* **Positive:** partial tables become possible while every cell — computed or
  manual — is keyed and provenance-stamped. The whole-table reject becomes a
  per-cell worklist. Fully inside the standard pipeline; render layer barely
  changes.
* **Negative:** five new ARD columns ripple through ARD consumers, tests, and
  any stored ARDs (a migration note for existing CSVs). Phase 3 touches spec
  generation, the most safety-sensitive stage. Manual fill introduces a
  human-in-the-loop step that must be guarded (Phase 5) so untraceable values
  cannot ship.
