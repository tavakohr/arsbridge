# Render an ARS output to a formatted clinical table

Convenience wrapper: builds the
[`tfrmt::tfrmt()`](https://gsk-biostatistics.github.io/tfrmt/reference/tfrmt.html)
spec with [`ars_to_tfrmt()`](ars_to_tfrmt.md), flattens and rescales the
ARD, renders to a GT table, and attaches any ARS footnotes as GT source
notes.

## Usage

``` r
ars_render_tlf(
  ars_path,
  ard,
  output_id,
  format = c("gt", "docx", "rtf"),
  file = NULL,
  rtf_path = NULL,
  ...
)
```

## Arguments

- ars_path:

  Path to the CDISC ARS v1.0 JSON (output of
  [`spec_to_ars()`](spec_to_ars.md)).

- ard:

  Tidy ARD data frame (output of [`ars_to_ard()`](ars_to_ard.md)).

- output_id:

  `character(1)` ARS output id or name to render (case-insensitive).

- format:

  Output format. `"gt"` (default) returns a `gt_tbl`; `"docx"` and
  `"rtf"` write a regulatory-style Word / RTF file (via `{flextable}` +
  `{officer}`) and return the path invisibly.

- file:

  Output path for `format = "docx"` / `"rtf"`. Defaults to
  `<output_id>.<format>` in
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- rtf_path:

  Deprecated alias for `file`.

- ...:

  Passed to [`ars_to_tfrmt()`](ars_to_tfrmt.md) (e.g. `col_var`,
  `label_var`).

## Value

A `gt_tbl` when `format = "gt"`; otherwise the written file path,
invisibly.

## See also

[`ars_to_tfrmt()`](ars_to_tfrmt.md),
[`ars_render_all()`](ars_render_all.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  gt_tbl <- ars_render_tlf(ars_path, ard, "T_14_1_1")
  ars_render_tlf(ars_path, ard, "T_14_1_1", format = "docx", file = "t1.docx")
} # }
```
