# Export a review session's changes as a QC workbook

Turns the edit log written beside a corrected reporting event
(`<name>.edits.json`) into a styled Excel workbook: who changed what,
when, and what the value was before and after.

## Usage

``` r
export_edit_log(edits, output_path = NULL)
```

## Arguments

- edits:

  Either the path to a `<name>.edits.json` sidecar, the path to the
  reporting event it sits beside, or the data frame of edits itself.

- output_path:

  Path to the `.xlsx` to write. Defaults to the sidecar's name with an
  `.xlsx` extension.

## Value

Invisibly, the path written.

## Details

The ARS JSON itself carries no provenance fields, which is deliberate –
the deliverable stays CDISC-conformant. This is where the provenance
lives instead.

## Sheets

- Summary:

  One row per field that ended up different, with its before and after
  value – repeated edits to the same field collapse into one row, and a
  field edited back to its original value does not appear at all.

- All changes:

  Every recorded edit in order, including the ones the summary
  collapses.

- Session:

  Who saved it, when, and with which version of arsbridge.

## See also

[`edit_ars()`](https://tavakohr.github.io/arsbridge/reference/edit_ars.md),
which writes the sidecar this reads.

## Examples

``` r
if (FALSE) { # \dontrun{
corrected <- edit_ars("reporting_event.json")
export_edit_log(corrected, "review_record.xlsx")
} # }
```
