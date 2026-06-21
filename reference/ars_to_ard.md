# Execute ARS JSON and return an ARD object using 'cards'

Reads a CDISC ARS JSON specification and executes the analyses defined
within it directly using the `{cards}` package, dynamically loading the
ADaM datasets (.csv or .xpt) and combining individual ARD tables into a
single tidy ARD object.

## Usage

``` r
ars_to_ard(
  ars_path,
  adam_dir,
  output_ids = NULL,
  analysis_ids = NULL,
  subject_key = "USUBJID",
  legacy = FALSE
)
```

## Arguments

- ars_path:

  Path to the CDISC ARS JSON file.

- adam_dir:

  Directory containing the ADaM datasets (.csv or .xpt).

- output_ids:

  Optional character vector of Output IDs to run only analyses
  referenced by those outputs. Matching is case-insensitive and checks
  both Output ID and Output Name (e.g. "T-14-1-1" or "T_14_1_1").

- analysis_ids:

  Optional character vector of Analysis IDs to run only those specific
  analyses.

- subject_key:

  Subject-level identifier variable used for distinct-subject counting
  and cross-dataset population joins. Default `"USUBJID"`; set e.g.
  `"SUBJID"` or `"PATID"` for studies with a non-standard subject key.

- legacy:

  Deprecated execution path. When `FALSE` (default) each analysis is
  computed by sourcing the pure-`{cards}` block arsbridge emits, so the
  ARD is produced by the same code shipped as the deliverable. When
  `TRUE` the retired `.ARD_EXECUTORS` registry is used instead (kept
  only for the engine-equivalence test and as a transitional escape
  hatch).

## Value

A tidy ARD data frame of class `"card"`, with traceability columns
`analysis_id`, `method_id`, `output_id`, `method_intended`, and
`method_actual` (differs from `method_intended` when the generic
fallback summarizer was used), plus provenance columns (ADR 0002):
`result_status` (`"computed"` for engine output), `value_source`
(`"cards"`), `derivation_ref` (the emitted block,
`arsbridge:emitted:<id>`), `derived_by` (`"arsbridge"`), and
`derived_dt` (run timestamp, ISO-8601; pin with
`options(arsbridge.derived_dt=)`). These let a later partial / manual
fill be distinguished from engine output without breaking traceability.

## Examples

``` r
if (FALSE) { # \dontrun{
  ard <- ars_to_ard("outputs/reporting_event.json", "inputs/ADaM")
} # }
```
