# Validate manually-filled ARD cells

Checks the manual fills in an ARD (ADR 0002 phase 5). A cell whose
`result_status` was set to `"manual_filled"` must carry both a value
(`stat`) and a `derivation_ref` – the path/id of the validated program
that produced it. A manual value with no derivation reference is
untraceable and must never ship;
[`ars_render_all()`](https://tavakohr.github.io/arsbridge/reference/ars_render_all.md)
surfaces any offending row as a blocker diagnostic before rendering. Run
it yourself on a filled ARD to clear the worklist.

## Usage

``` r
ars_validate_manual_fills(ard)
```

## Arguments

- ard:

  An ARD data frame (class `"card"`), typically one whose
  `manual_pending` cells (see
  [`ars_manual_worklist()`](https://tavakohr.github.io/arsbridge/reference/ars_manual_worklist.md))
  have been filled.

## Value

A data frame, one row per offending cell: `output_id`, `analysis_id`,
`method_id`, `stat_name`, and `problem`. Zero rows (with those columns)
when every manual fill is traceable.

## See also

[`ars_manual_worklist()`](https://tavakohr.github.io/arsbridge/reference/ars_manual_worklist.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  bad <- ars_validate_manual_fills(filled_ard)
  if (nrow(bad)) stop("untraceable manual values present")
} # }
```
