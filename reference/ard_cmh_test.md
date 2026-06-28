# Cochran-Mantel-Haenszel test as an ARD row

Stratified CMH chi-square test of association between a response and a
treatment grouping, returned as a single `{cards}`-shaped ARD row
carrying the `p.value`. arsbridge emits a call to this function for a
`MTH_CMH_TEST` analysis (ADR 0001), so the deliverable script and the
executed ARD are the same code. It wraps
[`stats::mantelhaen.test()`](https://rdrr.io/r/stats/mantelhaen.test.html)
on the response x group x strata contingency table – base R, no extra
dependency (the cardx wrapper is not used). The continuity correction is
applied only for a 2x2xK table.

## Usage

``` r
ard_cmh_test(data, response, by, strata, correct = TRUE)
```

## Arguments

- data:

  A data frame.

- response, by, strata:

  Length-1 column names of the response variable, the treatment
  grouping, and the stratification variable.

- correct:

  Logical; request the continuity correction (default `TRUE`, applied
  only when the response and grouping are both binary).

## Value

A one-row ARD (`card`) with the CMH `p.value`; `stat` is `NA` and the
`error` column is populated if the test could not be computed.

## See also

[`ars_to_ard()`](https://tavakohr.github.io/arsbridge/reference/ars_to_ard.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  ard_cmh_test(adeff, response = "AVAL", by = "TRT01A", strata = "REGION")
} # }
```
