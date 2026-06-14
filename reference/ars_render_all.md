# Render every output of a reporting event into one Word document

Walks all outputs of an ARS reporting event, rendering tables with
[`ars_render_tlf()`](ars_render_tlf.md), listings with
[`ars_render_listing()`](ars_render_listing.md), and figures with
[`ars_render_figure()`](ars_render_figure.md), and assembles them into a
single landscape `.docx` (one output per page). Returns a manifest
recording, for every output, whether it rendered and – if not – why.

## Usage

``` r
ars_render_all(
  ars_path,
  ard,
  adam_dir = NULL,
  file = NULL,
  types = c("table", "listing", "figure"),
  max_rows = 500,
  progress = NULL
)
```

## Arguments

- ars_path:

  Path to the ARS JSON.

- ard:

  Tidy ARD from [`ars_to_ard()`](ars_to_ard.md) (drives the tables).

- adam_dir:

  Directory of ADaM datasets, required to render listings and figures.
  If `NULL`, those are skipped (recorded in the manifest).

- file:

  Output `.docx` path. Default: `reporting_event_tlfs.docx` in
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- types:

  Which output kinds to render. Default all three.

- max_rows:

  Row cap for listings (see
  [`ars_render_listing()`](ars_render_listing.md)).

- progress:

  Optional `function(i, n, output_id)` for progress reporting.

## Value

A data frame manifest (`output_id`, `type`, `status`, `reason`),
invisibly carrying the written file path as attribute `"file"`.

## See also

[`ars_render_tlf()`](ars_render_tlf.md),
[`ars_render_listing()`](ars_render_listing.md),
[`ars_render_figure()`](ars_render_figure.md)
