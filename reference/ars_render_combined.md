# Render every output into one combined ARD and one combined RTF

The "run all" companion to
[`ars_render_split()`](https://tavakohr.github.io/arsbridge/reference/ars_render_split.md):
computes (or accepts) the single big ARD covering every analysis,
optionally saves it, and writes all tables and listings into ONE
combined RTF file (each table carries its own id + title header).
Figures are not included in an RTF and are reported as skipped – use
[`ars_render_split()`](https://tavakohr.github.io/arsbridge/reference/ars_render_split.md)
or
[`ars_render_all()`](https://tavakohr.github.io/arsbridge/reference/ars_render_all.md)
for those.

## Usage

``` r
ars_render_combined(
  ars_path,
  file,
  adam_dir = NULL,
  ard = NULL,
  ard_file = NULL,
  output_ids = NULL,
  max_rows = 500
)
```

## Arguments

- ars_path:

  Path to the ARS reporting-event JSON.

- file:

  Path of the combined `.rtf` to write.

- adam_dir:

  Directory of ADaM datasets. Required to compute the ARD when `ard` is
  `NULL`, and to render listings.

- ard:

  Optional precomputed ARD (from
  [`ars_to_ard()`](https://tavakohr.github.io/arsbridge/reference/ars_to_ard.md)).
  When `NULL` it is computed from `ars_path` + `adam_dir`.

- ard_file:

  Optional path to also save the big ARD as an `.rds`.

- output_ids:

  Optional character vector restricting which outputs to include
  (matched against output id or name, case-insensitively).

- max_rows:

  Row cap for listings. Default 500.

## Value

Invisibly, a manifest data frame: `output_id`, `type`, `status`
(`"rendered"`/`"error"`/`"skipped"`), `reason`. `attr(., "file")` is the
RTF path and `attr(., "ard_file")` the saved ARD path (or `NA`).

## See also

[`ars_render_split()`](https://tavakohr.github.io/arsbridge/reference/ars_render_split.md)
for one file per program,
[`ars_render_all()`](https://tavakohr.github.io/arsbridge/reference/ars_render_all.md)
for a single combined Word document.
