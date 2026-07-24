# Serialize an editable ARS model back to a reporting event

The inverse of
[`ars_to_model()`](https://tavakohr.github.io/arsbridge/reference/ars_to_model.md).
Each pool row's edited fields are written back into the original node it
came from, and the reporting event is reassembled from the template so
that every field the model does not surface – including `_meta` and any
future ARS key – survives untouched.

## Usage

``` r
model_to_ars(model, template = NULL)
```

## Arguments

- model:

  An `ars_model` from
  [`ars_to_model()`](https://tavakohr.github.io/arsbridge/reference/ars_to_model.md).

- template:

  The reporting event to reassemble from. Defaults to the template the
  model was read from, which is what you almost always want.

## Value

A reporting event as a nested list, ready for
`jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE, null = "null")`.

## Details

The two tables of contents (`mainListOfContents` and
`otherListsOfContents`) are pure derivations of the outputs list. They
are copied verbatim when nothing structural changed, and regenerated
from the outputs when analyses or output references were added, removed
or reordered.

## See also

[`ars_to_model()`](https://tavakohr.github.io/arsbridge/reference/ars_to_model.md),
[`validate_ars_model()`](https://tavakohr.github.io/arsbridge/reference/validate_ars_model.md).

## Examples

``` r
if (FALSE) { # \dontrun{
model <- ars_to_model("reporting_event.json")
model$analyses$methodId[1] <- "MTH_SUBJECT_COUNT"
ars <- model_to_ars(model)
} # }
```
