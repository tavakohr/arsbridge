# Check a reporting event against the official CDISC ARS v1.0 schema

Validates against the JSON Schema of the CDISC Analysis Results
Standard, pinned to the `v1.0.0` release of
<https://github.com/cdisc-org/analysis-results-standard> and shipped
with this package – so the answer does not change when the standard's
development branch does.

## Usage

``` r
ars_conformance(ars, strip_extensions = TRUE, schema_path = NULL)
```

## Arguments

- ars:

  What to check: a path to an ARS JSON file, a parsed reporting event,
  or an `ars_model` from
  [`ars_to_model()`](https://tavakohr.github.io/arsbridge/reference/ars_to_model.md).

- strip_extensions:

  Strip arsbridge's documented extension fields before validating
  (default `TRUE`), so the findings show genuine divergences rather than
  the extensions the pipeline relies on. Set to `FALSE` to see
  everything the schema would reject, extensions included.

- schema_path:

  Path to an alternative JSON Schema, if you want to validate against
  something other than the bundled v1.0.0 export.

## Value

A data frame of schema violations – `where` (the JSON path), `keyword`
(the schema rule), `problem` – with zero rows meaning the event conforms
(after stripping, if enabled). The fields that were stripped are
attached as `attr(, "stripped_extensions")`.

## Details

This is a different question from
[`validate_ars_model()`](https://tavakohr.github.io/arsbridge/reference/validate_ars_model.md).
That asks "will this execute, and does every reference resolve?"; this
asks "is the file structurally what the standard says a reporting event
is?". A file can pass either one alone.

## The sanctioned extensions

arsbridge extends ARS v1.0 in documented places: `_meta` blocks (top
level and per output), `referencedAnalysisIds` and `outputType` on
outputs, display `columns`, the nested `analysisVariable` duplicate plus
`annotation`/`sapDescription`/`includeTotal`/`strata`/`variableRole` on
analyses, `annotationText` on analysis sets, and `supported` on methods.
These exist for siera compatibility, the renderer, and review
provenance; stripping them is what makes the remaining findings
meaningful.

## What a freshly generated event reports

Nothing. The generator emits everything the standard requires –
`reason`/`purpose` terminology on every analysis (defaults settable in
[`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)),
integer `version`s, displays in the official `OrderedDisplay` wrapper,
terminology-object `fileType`s, valid operation-role terms, and named
contents-list entries – so a fresh file validates clean once the
extensions are stripped.

A file in any other shape – from an earlier arsbridge, or from another
tool – simply has its divergences reported. arsbridge deliberately
carries no compatibility readers for its own older shapes: the remedy
for an outdated file is regenerating it with
[`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md).

## See also

[`validate_ars_model()`](https://tavakohr.github.io/arsbridge/reference/validate_ars_model.md)
for referential and executability checks;
[`edit_ars()`](https://tavakohr.github.io/arsbridge/reference/edit_ars.md)
notes the conformance count after each save.

## Examples

``` r
if (FALSE) { # \dontrun{
findings <- ars_conformance("reporting_event.json")
subset(findings, keyword == "required")
attr(findings, "stripped_extensions")
} # }
```
