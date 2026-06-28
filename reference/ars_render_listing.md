# Render an ARS listing output to a GT table

Assembles the columns of a listing output (one per `MTH_LISTING`
analysis), merging variables from auxiliary datasets onto the primary
dataset by subject, applies the listing's population filter, and returns
a `gt_tbl`.

## Usage

``` r
ars_render_listing(
  ars_path,
  adam_dir,
  output_id,
  subject_key = "USUBJID",
  max_rows = 500
)
```

## Arguments

- ars_path:

  Path to the CDISC ARS JSON.

- adam_dir:

  Directory containing the ADaM datasets (.xpt/.csv).

- output_id:

  Listing output id or name (case-insensitive).

- subject_key:

  Subject identifier for cross-dataset merges. Default `"USUBJID"`.

- max_rows:

  Cap on listed rows (default 500). Set `Inf` for all rows; a note is
  added when rows are truncated.

## Value

A `gt_tbl`.

## See also

[`ars_render_tlf()`](https://tavakohr.github.io/arsbridge/reference/ars_render_tlf.md),
[`ars_to_ard()`](https://tavakohr.github.io/arsbridge/reference/ars_to_ard.md)
