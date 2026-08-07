# Write results into the shell workbook they came from

Takes the Excel shell that was parsed to build an ARS, and returns a
copy of it with the computed results written into their placeholders and
the programming annotations removed. The layout, labels, column headers,
merges, fonts and footnotes are the author's own – they are never
rebuilt, only left alone.

## Usage

``` r
ars_fill_shell(
  shell_path,
  ars,
  ard,
  output_path,
  adam_dir = NULL,
  strip_annotations = TRUE,
  keep_pending_placeholders = TRUE,
  overwrite = FALSE
)
```

## Arguments

- shell_path:

  Path to the Excel shell (`.xlsx`) the ARS was built from.

- ars:

  The reporting event: a path to `reporting_event.json`, or the parsed
  list.

- ard:

  The ARD from
  [`ars_to_ard()`](https://tavakohr.github.io/arsbridge/reference/ars_to_ard.md)
  holding the results to write.

- output_path:

  Where to write the filled workbook.

- adam_dir:

  Directory holding the ADaM datasets (`.xpt`, `.sas7bdat` or `.csv`).
  Required to fill a listing, whose rows are the subject-level data
  itself, and a figure, whose series the shell states as prose rather
  than as an analysis. Tables need only the ARD; leave it `NULL` and any
  listing or figure sheet is reported instead of filled.

- strip_annotations:

  Remove the red programming annotations. `TRUE` for a deliverable;
  `FALSE` to keep them beside the numbers while reviewing.

- keep_pending_placeholders:

  Leave an unfillable cell showing its placeholder (default) rather than
  blanking it.

- overwrite:

  Allow `output_path` to be replaced.

## Value

Invisibly, a list with `path`, the counts `filled`, `pending` and
`skipped`, plus two frames: `census` – one row per cell record, the
FILLED cells included, with position, display-column label, owning
analysis, status and (when unresolved) the reason – and `findings`, the
diagnostics this run itself raised (lost columns, declined expansions),
which previously lived only in the session collector. Roll the census up
with
[`ars_fill_summary()`](https://tavakohr.github.io/arsbridge/reference/ars_fill_summary.md).

## Details

Which result belongs in which cell is not decided here. It was recorded
when the ARS was built, in each output's `_meta$shell_fill` cell map
(see `vignette("shell-fidelity")` and ADR 0005), so a shell that was
parsed by an older version, or a Word shell, has no map and nothing to
fill.

A cell whose result does not exist keeps its placeholder and is reported
as pending rather than blanked, because an empty cell in a clinical
table reads as a zero. `keep_pending_placeholders = FALSE` clears them
instead, for a workbook that is going to someone who will complete it by
hand.

## See also

[`ars_to_ard()`](https://tavakohr.github.io/arsbridge/reference/ars_to_ard.md)
for the results,
[`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
for the map.

## Examples

``` r
if (FALSE) { # \dontrun{
  ard <- ars_to_ard("outputs/reporting_event.json", "inputs/ADaM")
  ars_fill_shell(
    shell_path  = "inputs/shells.xlsx",
    ars         = "outputs/reporting_event.json",
    ard         = ard,
    output_path = "outputs/filled_shells.xlsx")
} # }
```
