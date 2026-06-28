# Manual-derivation worklist from an ARD

Lists every reserved `manual_pending` cell in an ARD produced by
[`ars_to_ard()`](https://tavakohr.github.io/arsbridge/reference/ars_to_ard.md)
– statistics arsbridge could not compute (a declared-but-unexecutable
method, e.g. a Cochran-Mantel-Haenszel p-value) and reserved as keyed
stub rows. This is the analyst's checklist: each row must be computed
with a validated analysis script and written back into the ARD (set
`stat`, `result_status = "manual_filled"`, `value_source`,
`derivation_ref`) before the table is final. See
[`vignette("getting-started")`](https://tavakohr.github.io/arsbridge/articles/getting-started.md)
and the ADRs under `docs/adr/` for the round-trip.

## Usage

``` r
ars_manual_worklist(ard)
```

## Arguments

- ard:

  An ARD data frame of class `"card"` from
  [`ars_to_ard()`](https://tavakohr.github.io/arsbridge/reference/ars_to_ard.md).

## Value

A data frame with one row per pending cell: `output_id`, `analysis_id`,
`method_id`, `group1`, `group1_level`, `variable`, `stat_name`. Zero
rows (with those columns) when nothing is pending.

## Examples

``` r
if (FALSE) { # \dontrun{
  ard <- ars_to_ard("outputs/reporting_event.json", "inputs/ADaM")
  ars_manual_worklist(ard)
} # }
```
