# Write the fix report workbook.

The per-phase record of what a run could not resolve and where to change
it. Six sheets: the run's own facts, the fix list, the per-code rollup,
the cells that carry no number, the run diagnostics, and the shared
legend.

## Usage

``` r
write_fix_report(
  findings,
  output_path,
  reservations = NULL,
  census = NULL,
  diagnostics = NULL,
  run = list()
)
```

## Arguments

- findings:

  The findings frame from
  [`validate_ars_model()`](https://tavakohr.github.io/arsbridge/reference/validate_ars_model.md)
  – or, better, the gate's own `findings`, so the report and the engine
  describe one set of defects.

- output_path:

  Path of the `.xlsx` to write.

- reservations:

  The `list(by_analysis, by_finding)` map built from **these** findings.
  Built from a different set, `reserved_as` would describe reservations
  that never happened.

- census:

  Optionally the `census` frame from
  [`ars_fill_shell()`](https://tavakohr.github.io/arsbridge/reference/ars_fill_shell.md),
  which turns analysis-level reservations into addressable cells.

- diagnostics:

  Optionally a diagnostics frame; its sheet is omitted when empty.

- run:

  A named list of run facts for the Run sheet: `extraction_mode`,
  `verdict`, `timestamp`, input paths, and counts.

## Value

Invisibly, `output_path`.
