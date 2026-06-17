# Retrieve pipeline diagnostics from the most recent run

Returns every fallback, parsing miss, skipped sheet, LLM failure,
unknown analysis method, and dropped where-clause condition recorded
during the most recent [`spec_to_ars()`](spec_to_ars.md) or
[`ars_to_ard()`](ars_to_ard.md) call in this R session. The same records
are written to the "Diagnostics" sheet of the validation report and
returned in the `diagnostics` element of the
[`spec_to_ars()`](spec_to_ars.md) result; this accessor exists for
interactive inspection after the fact.

## Usage

``` r
ars_diagnostics()
```

## Value

Data frame with columns `stage`, `severity` (`FAIL` / `WARN` / `INFO`),
`input` (which input document the finding concerns), `tlf_number`,
`location`, `problem`, `action`.

## Examples

``` r
if (FALSE) { # \dontrun{
spec_to_ars("shells.docx", "adam_spec.xlsx")
ars_diagnostics()
} # }
```
