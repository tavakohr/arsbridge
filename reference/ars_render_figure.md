# Render an ARS figure output to a ggplot

Builds a treatment-group figure from ADaM data for a figure output.
Because the ARS spec does not describe figure analyses, the data mapping
is supplied here; only the title and footnotes are taken from the spec.

## Usage

``` r
ars_render_figure(
  ars_path,
  adam_dir,
  output_id,
  type = c("auto", "mean_over_time", "responder_over_time", "km", "forest"),
  dataset = "ADEFF",
  value_var = "AVAL",
  time_var = NULL,
  by_var = NULL,
  paramcd = NULL,
  responder_flag = NULL,
  time_event = NULL,
  subject_key = "USUBJID"
)
```

## Arguments

- ars_path:

  Path to the CDISC ARS JSON.

- adam_dir:

  Directory of ADaM datasets (.xpt/.csv).

- output_id:

  Figure output id or name.

- type:

  Figure type; `"auto"` infers from the title.

- dataset:

  ADaM dataset to plot (default `"ADEFF"`).

- value_var:

  Response value column (default `"AVAL"`).

- time_var:

  Visit/time column; default auto (`AVISITN` then `AVISIT`).

- by_var:

  Grouping column; default auto (`TRT01A` then `TRTP`).

- paramcd:

  Optional `PARAMCD` filter; default inferred from the title.

- responder_flag:

  Optional flag column marking responders.

- time_event:

  Optional `list(time=, event=)` columns for `type = "km"`.

- subject_key:

  Subject id. Default `"USUBJID"`.

## Value

A `ggplot` object.

## Details

Supported `type`s:

- `"mean_over_time"` – mean of `value_var` by visit, one line per group.

- `"responder_over_time"` – percentage of responders by visit, one line
  per group (responder = `responder_flag == "Y"`, or `value_var == 1`).

- `"km"` – Kaplan-Meier curve (requires a time-to-event dataset; errors
  with guidance if absent).

- `"forest"` – not supported from raw data (needs fitted effect
  estimates); errors with guidance.

## See also

[`ars_render_tlf()`](https://tavakohr.github.io/arsbridge/reference/ars_render_tlf.md),
[`ars_render_listing()`](https://tavakohr.github.io/arsbridge/reference/ars_render_listing.md)
