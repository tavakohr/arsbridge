# Run spec_to_ars() against the bundled example inputs

Zero-argument entry point that runs the full
[`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
pipeline against the bundled CDSC-ALZ-201 shell and ADaM spec. Useful as
a first call after installation – you get a real ARS JSON and validation
report from the training shell without owning a study.

## Usage

``` r
spec_to_ars_example(
  output_path = file.path(tempdir(), "reporting_event.json"),
  report_path = file.path(tempdir(), "spec_validation_report.xlsx"),
  shell_format = c("docx", "xlsx"),
  ...
)
```

## Arguments

- output_path:

  Where to write the ARS JSON. Default: `"reporting_event.json"` in
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- report_path:

  Where to write the spec validation report. Default:
  `"spec_validation_report.xlsx"` in
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- shell_format:

  Which bundled shell to read: `"docx"` (default) or `"xlsx"` (the Excel
  worksheets – the shell that
  [`ars_fill_shell()`](https://tavakohr.github.io/arsbridge/reference/ars_fill_shell.md)
  can write back filled). Both carry the same 8 outputs.

- ...:

  Additional arguments forwarded to
  [`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md).

## Value

Invisibly returns the
[`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
result list.

## Examples

``` r
if (FALSE) { # \dontrun{
# One-call demo. Deterministic (no API key needed); seconds, not minutes.
res <- spec_to_ars_example()
res$n_tlfs       # 8
str(res$reporting_event, max.level = 1)
} # }
```
