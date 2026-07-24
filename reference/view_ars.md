# Review an ARS reporting event in a structured, clickable viewer

Opens the reporting event as the structure a clinical programmer already
recognises – each output with its analysis lines beneath it – with
validation findings overlaid, so problems are visible without reading
JSON.

## Usage

``` r
view_ars(ars, adam_spec_path = NULL, report_path = NULL)
```

## Arguments

- ars:

  What to review. Either a path to an ARS JSON file, an already parsed
  reporting event, or the whole result of
  [`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
  – which carries the event, its validation table and the paths it
  wrote, so gap detection and spec-aware display are wired up with no
  further arguments.

- adam_spec_path:

  Optional path to the ADaM spec (`define.xml` or Excel). When supplied,
  datasets and variables are checked against it.

- report_path:

  Optional path to the validation report
  [`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
  wrote. When supplied, annotated shell lines that no analysis covers
  are reported as gaps.

## Value

Invisibly `NULL`. Called for the viewer it opens.

## Details

This viewer never writes: use
[`edit_ars()`](https://tavakohr.github.io/arsbridge/reference/edit_ars.md)
to correct what it surfaces.

## See also

[`ars_to_model()`](https://tavakohr.github.io/arsbridge/reference/ars_to_model.md)
for the same content as data frames,
[`validate_ars_model()`](https://tavakohr.github.io/arsbridge/reference/validate_ars_model.md)
for the findings without a browser.

## Examples

``` r
if (FALSE) { # \dontrun{
# Review what spec_to_ars() just generated.
result <- spec_to_ars(shell_path = "shells.docx",
                      adam_spec_path = "adam_spec.xlsx")
view_ars(result)

# Or review a JSON file directly.
view_ars("reporting_event.json", adam_spec_path = "adam_spec.xlsx")
} # }
```
