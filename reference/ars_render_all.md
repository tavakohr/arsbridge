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
  output_ids = NULL,
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

- output_ids:

  Optional character vector of output ids or names (case-insensitive) to
  render – any mix of tables, listings, and figures. `NULL` (default)
  renders every output. Ids absent from the spec are reported in the
  manifest as skipped.

- types:

  Which output kinds to render. Default all three. Applied in addition
  to `output_ids`.

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

## Examples

``` r
if (FALSE) { # \dontrun{
  # Just three specific outputs into one Word document:
  ars_render_all(ars, ard, adam_dir,
                 output_ids = c("T_14_1_1", "L_16_2_4_1", "F_14_2_1"))
} # }
```
