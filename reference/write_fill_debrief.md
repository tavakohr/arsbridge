# Write the fill debrief workbook.

The durable, human-readable record of what a fill did – for the locked
machine where the app's screen and this file are the entire diagnosis.
Sheets: the full cell census (rows tinted by outcome), the per-column
rollup, the reason histogram with its hints, the run's diagnostics (when
any), and the shared legend.

## Usage

``` r
write_fill_debrief(census, findings, output_path)
```

## Arguments

- census:

  The `census` frame
  [`ars_fill_shell()`](https://tavakohr.github.io/arsbridge/reference/ars_fill_shell.md)
  returned.

- findings:

  A diagnostics frame (the workflow's accumulated table, or the fill's
  own `findings`); its sheet is omitted when empty.

- output_path:

  Path of the `.xlsx` to write.

## Value

Invisibly, `output_path`.
