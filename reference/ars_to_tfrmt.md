# Build a tfrmt specification for one ARS output

Translates one output of a CDISC ARS v1.0 reporting event, together with
the tidy ARD produced by
[`ars_to_ard()`](https://tavakohr.github.io/arsbridge/reference/ars_to_ard.md),
into a
[`tfrmt::tfrmt()`](https://gsk-biostatistics.github.io/tfrmt/reference/tfrmt.html)
specification. The returned object can be rendered with
[`tfrmt::print_to_gt()`](https://gsk-biostatistics.github.io/tfrmt/reference/print_to_gt.html)
or
[`tfrmt::print_mock_gt()`](https://gsk-biostatistics.github.io/tfrmt/reference/print_mock_gt.html)
– but the ARD must be flattened and rescaled first (see
[`ars_render_tlf()`](https://tavakohr.github.io/arsbridge/reference/ars_render_tlf.md),
which does this for you).

## Usage

``` r
ars_to_tfrmt(
  ars_path,
  ard,
  output_id,
  col_var = NULL,
  label_var = NULL,
  group_vars = NULL
)
```

## Arguments

- ars_path:

  Path to the CDISC ARS v1.0 JSON (output of
  [`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)).

- ard:

  Tidy ARD data frame (output of
  [`ars_to_ard()`](https://tavakohr.github.io/arsbridge/reference/ars_to_ard.md)).

- output_id:

  `character(1)` ARS output id or name to render (case-insensitive).

- col_var, label_var, group_vars:

  Optional overrides for the auto-detected column roles described above.

## Value

A
[`tfrmt::tfrmt()`](https://gsk-biostatistics.github.io/tfrmt/reference/tfrmt.html)
object. Extracted footnotes are attached as the attribute
`"arsbridge_footnotes"`;
[`ars_render_tlf()`](https://tavakohr.github.io/arsbridge/reference/ars_render_tlf.md)
applies them as GT source notes.

## Details

Column roles are auto-detected from the `{cards}` ARD column names
unless supplied explicitly:

- `col_var` – the `group*_level` column whose grouping variable is a
  fixed (treatment) grouping in the ARS spec.

- `label_var` – `variable_level` when it carries text, else `variable`.

- `group_vars` – remaining `group*_level` columns plus `variable` when
  more than one analysis variable is present.

## See also

[`ars_render_tlf()`](https://tavakohr.github.io/arsbridge/reference/ars_render_tlf.md),
[`ars_to_tfrmt_list()`](https://tavakohr.github.io/arsbridge/reference/ars_to_tfrmt_list.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  ars  <- arsbridge_example("reporting_event.json")
  ard  <- ars_to_ard(ars, "inputs/ADaM")
  spec <- ars_to_tfrmt(ars, ard, "T_14_1_1")
} # }
```
