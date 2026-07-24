# Review and correct an ARS reporting event interactively

Opens the reporting event in the same structured viewer as
[`view_ars()`](https://tavakohr.github.io/arsbridge/reference/view_ars.md),
with the detail panels editable: methods, populations, data subsets,
groupings and analysis variables are chosen from what actually exists –
the entities in the file, the methods the engine can execute, and (when
the ADaM spec is supplied) the variables the study really has.

## Usage

``` r
edit_ars(ars, adam_spec_path = NULL, report_path = NULL, output_path = NULL)

review_ars(ars, adam_spec_path = NULL, report_path = NULL, output_path = NULL)
```

## Arguments

- ars:

  What to edit. Either a path to an ARS JSON file, an already parsed
  reporting event, or the whole result of
  [`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
  – which carries the event, its validation table and the paths it
  wrote.

- adam_spec_path:

  Optional path to the ADaM spec (`define.xml` or Excel). When supplied,
  variables are chosen from the spec rather than typed, and datasets and
  variables are checked against it.

- report_path:

  Optional path to the validation report
  [`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
  wrote. When supplied, annotated shell lines that no analysis covers
  are reported as gaps.

- output_path:

  Where to write. Defaults to the file `ars` was read from; required
  when `ars` is an in-memory reporting event, since there is no file to
  write back to.

## Value

Invisibly, the path written – or `NULL` if the session was closed
without saving.

## Details

Nothing is written until you choose to save, and saving shows what
changed first.

`review_ars()` is an alias for `edit_ars()`. Both open the same tool;
the name is a matter of which framing fits – "review" is what a clinical
QC process calls this step, "edit" is what the tool does.

## What saving does

The previous file is copied to `<name>.json.bak-<timestamp>` before the
first overwrite. The new content is written to a temporary file in the
same directory and renamed into place, so an interrupted save cannot
destroy the file it was replacing. An edit log is written to
`<name>.edits.json` alongside it: the ARS JSON itself stays free of
non-standard fields, so the deliverable remains CDISC-clean.

## See also

[`view_ars()`](https://tavakohr.github.io/arsbridge/reference/view_ars.md)
to review without editing,
[`validate_ars_model()`](https://tavakohr.github.io/arsbridge/reference/validate_ars_model.md)
for the findings on the command line.

## Examples

``` r
if (FALSE) { # \dontrun{
# Correct what spec_to_ars() just generated, then execute it.
result <- spec_to_ars(shell_path = "shells.docx",
                      adam_spec_path = "adam_spec.xlsx")
corrected <- edit_ars(result)
ard <- ars_to_ard(corrected, adam_dir = "adam")
} # }
```
