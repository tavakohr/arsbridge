# Build tfrmt specs for every renderable ARS output

Returns a named list of
[`tfrmt::tfrmt()`](https://gsk-biostatistics.github.io/tfrmt/reference/tfrmt.html)
specs, one per output id that is present in both the ARS spec and the
ARD. Outputs that fail to build (e.g. listings with no summarised
statistics) are skipped with a warning and returned as `NULL`.

## Usage

``` r
ars_to_tfrmt_list(ars_path, ard)
```

## Arguments

- ars_path:

  Path to the CDISC ARS v1.0 JSON (output of
  [`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)).

- ard:

  Tidy ARD data frame (output of
  [`ars_to_ard()`](https://tavakohr.github.io/arsbridge/reference/ars_to_ard.md)).

## Value

A named list of
[`tfrmt::tfrmt()`](https://gsk-biostatistics.github.io/tfrmt/reference/tfrmt.html)
objects (or `NULL` per skipped output), keyed by output id.

## See also

[`ars_to_tfrmt()`](https://tavakohr.github.io/arsbridge/reference/ars_to_tfrmt.md),
[`ars_render_tlf()`](https://tavakohr.github.io/arsbridge/reference/ars_render_tlf.md)
