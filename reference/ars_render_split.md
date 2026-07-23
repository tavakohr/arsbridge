# Render each output to its own ARD and table file

The "one file per program" companion to
[`ars_render_all()`](https://tavakohr.github.io/arsbridge/reference/ars_render_all.md):
instead of a single combined document, this writes a separate table file
(and, by default, a separate ARD `.rds`) for every output in the
reporting event – the layout most clinical repositories expect, one
deliverable per TLF program.

## Usage

``` r
ars_render_split(
  ars_path,
  dir,
  adam_dir = NULL,
  ard = NULL,
  output_ids = NULL,
  format = c("rtf", "docx"),
  write_ard = TRUE,
  max_rows = 500
)
```

## Arguments

- ars_path:

  Path to the ARS reporting-event JSON.

- dir:

  Output directory. Created (recursively) if it does not exist.

- adam_dir:

  Directory of ADaM datasets. Required to compute the ARD when `ard` is
  `NULL`, and to render listings/figures.

- ard:

  Optional precomputed ARD (from
  [`ars_to_ard()`](https://tavakohr.github.io/arsbridge/reference/ars_to_ard.md)).
  When `NULL` it is computed from `ars_path` + `adam_dir`.

- output_ids:

  Optional character vector restricting which outputs to render (matched
  against output id or name, case-insensitively).

- format:

  Table file format: `"rtf"` (default) or `"docx"`.

- write_ard:

  Also write each output's ARD slice as `<dir>/<output_id>.rds`. Default
  `TRUE`.

- max_rows:

  Row cap for listings. Default 500.

## Value

Invisibly, a manifest data frame: `output_id`, `type`, `status`
(`"rendered"`/`"error"`/`"skipped"`), `ard_file`, `doc_file`, `reason`.
`attr(., "dir")` is the output directory.

## Details

Tables and listings are written with the same regulatory flextable
styling as
[`ars_render_all()`](https://tavakohr.github.io/arsbridge/reference/ars_render_all.md);
figures are written as `.png`. Each output's ARD slice (its rows of the
big ARD, cards list-columns intact) is saved as `<dir>/<output_id>.rds`
unless `write_ard = FALSE`.

## See also

[`ars_render_combined()`](https://tavakohr.github.io/arsbridge/reference/ars_render_combined.md)
for one big ARD + one combined RTF,
[`ars_render_all()`](https://tavakohr.github.io/arsbridge/reference/ars_render_all.md)
for a single combined Word document.
