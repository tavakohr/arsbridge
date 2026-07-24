# Turn an ARS reporting event into an editable tabular model

Reads a CDISC ARS v1.0 reporting event and returns one data frame per
entity pool, each row carrying the flat fields a reviewer edits plus a
`raw` list-column holding the original untouched node.
[`model_to_ars()`](https://tavakohr.github.io/arsbridge/reference/model_to_ars.md)
is the exact inverse: an unedited model round-trips to a structurally
identical reporting event.

## Usage

``` r
ars_to_model(ars)
```

## Arguments

- ars:

  Either a path to an ARS JSON file, or an already parsed reporting
  event (a list, as returned in `spec_to_ars()$reporting_event`).

## Value

An object of class `ars_model`: a list with one data frame per pool –
`analyses`, `methods`, `analysis_sets`, `data_subsets`, `groupings`,
`outputs` – plus `template` (the untouched parsed reporting event) and
`source_path` (the file it was read from, or `NULL`).

Pools that are absent from the reporting event come back as zero-row
data frames with the full column set, so downstream code can always rely
on the columns existing.

## Details

This is the foundation of the review/correct stage
([`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
-\> review -\>
[`ars_to_ard()`](https://tavakohr.github.io/arsbridge/reference/ars_to_ard.md)).
It depends only on `jsonlite` – no Shiny, no LLM – so the pools are
equally usable from a plain script.

## Column conventions

Columns named exactly like an ARS field (camelCase, e.g. `methodId`) map
onto that field one-to-one. Snake_case columns are either derived and
read-only (`output_id`, `n_analyses`, `condition_summary`) or documented
composites that several ARS fields are rebuilt from – `grouping_ids`
(semicolon-separated, ordered, rebuilds `orderedGroupings`) and
`referenced_analysis_ids` (semicolon-separated, rebuilds the output's
analysis references and therefore the tables of contents).

An `NA` in an optional column such as `strata` means the key is absent
from the node; setting it back to `NA` removes the key again.

## See also

[`model_to_ars()`](https://tavakohr.github.io/arsbridge/reference/model_to_ars.md)
to serialize back,
[`validate_ars_model()`](https://tavakohr.github.io/arsbridge/reference/validate_ars_model.md)
to check integrity.

## Examples

``` r
if (FALSE) { # \dontrun{
model <- ars_to_model("reporting_event.json")
model$analyses[, c("id", "methodId", "dataset", "variable")]
} # }
```
