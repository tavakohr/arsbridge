# Execute ARS JSON and return an ARD object using cards

Reads a CDISC ARS JSON specification and executes the analyses defined
within it directly using the `{cards}` package, dynamically loading the
ADaM datasets (.csv or .xpt) and combining individual ARD tables into a
single tidy ARD object.

## Usage

``` r
ars_to_ard(ars_path, adam_dir, output_ids = NULL, analysis_ids = NULL)
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

## Value

A tidy ARD data frame of class `"card"`.

## Examples

``` r
if (FALSE) { # \dontrun{
  ard <- ars_to_ard("outputs/reporting_event.json", "inputs/ADaM")
} # }
```
