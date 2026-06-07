# Run spec_to_ars() against the bundled example inputs

Zero-argument entry point that runs the full
[`spec_to_ars()`](spec_to_ars.md) pipeline against
`arsbridge_example("annotated_shell.docx")` +
`arsbridge_example("adam_spec.xlsx")`. Useful as a first call after
installation – you get a real ARS JSON and validation report from the
APX-DRM-301 training shell without owning a study.

## Usage

``` r
spec_to_ars_example(
  output_path = file.path(tempdir(), "reporting_event.json"),
  report_path = file.path(tempdir(), "spec_validation_report.xlsx"),
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

- ...:

  Additional arguments forwarded to [`spec_to_ars()`](spec_to_ars.md).

## Value

Invisibly returns the [`spec_to_ars()`](spec_to_ars.md) result list.

## Examples

``` r
if (FALSE) { # \dontrun{
# One-call demo. Takes ~6 minutes (40 LLM calls).
res <- spec_to_ars_example()
res$n_tlfs       # 40
res$n_analyses   # ~226
res$n_warnings   # ~29
str(res$reporting_event, max.level = 1)
} # }
```
