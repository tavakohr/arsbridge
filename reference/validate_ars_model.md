# Check an ARS model for integrity, spec and coverage problems

Runs the checks that make the review stage guided rather than generic:
that every reference resolves, that every method can actually be
executed, that variables exist in the ADaM spec, and that no annotated
shell line was missed by the generator.

## Usage

``` r
validate_ars_model(model, spec = NULL, report = NULL)
```

## Arguments

- model:

  An `ars_model` from
  [`ars_to_model()`](https://tavakohr.github.io/arsbridge/reference/ars_to_model.md).

- spec:

  Optional ADaM spec, as returned by the package's spec reader (a list
  with `variables` and `lookup`). When supplied, datasets and variables
  are checked against it.

- report:

  Optional annotation validation report – the data frame
  [`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
  returns as `$validation`, or the "Validation" sheet of the report it
  writes. When supplied, annotated shell lines with no corresponding
  analysis are reported as gaps.

## Value

A data frame of findings, most severe first, with columns `severity`
(`"FAIL"`, `"WARN"` or `"INFO"`), `entity` (the pool the finding is
about), `id`, `field`, `problem` and `action`. Zero rows means nothing
to fix.

## What is checked

- Identity:

  Every entity has an id, and no id is used twice.

- References:

  Every `methodId`, `analysisSetId`, `dataSubsetId` and grouping id
  resolves, and every output references analyses that exist. An empty
  `dataSubsetId` means "no subset" and is not a dangling reference.
  Analyses no output displays are reported.

- Executability:

  Whether
  [`ars_to_ard()`](https://tavakohr.github.io/arsbridge/reference/ars_to_ard.md)
  can compute each analysis natively, needs a prerequisite, falls back
  to the generic summarizer, or will reserve an empty cell for manual
  computation.

- Populations:

  Analysis sets whose population text could not be parsed into a
  condition, and so filter nothing.

- Spec:

  With `spec`: datasets and variables that are not in the ADaM spec.

- Coverage:

  With `report`: shell annotations that no analysis carries – lines the
  generator missed.

## See also

[`ars_to_model()`](https://tavakohr.github.io/arsbridge/reference/ars_to_model.md),
[`model_to_ars()`](https://tavakohr.github.io/arsbridge/reference/model_to_ars.md).

## Examples

``` r
if (FALSE) { # \dontrun{
model <- ars_to_model("reporting_event.json")
findings <- validate_ars_model(model)
subset(findings, severity == "FAIL")
} # }
```
