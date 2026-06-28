# Exact (Clopper-Pearson) binomial confidence interval as ARD rows

Per-group exact binomial CIs for a categorical response, returned as a
`{cards}`-shaped ARD carrying only the interval bounds (`conf.low`,
`conf.high`). arsbridge emits a call to this function for a
`MTH_PROPORTION_CI_EXACT` analysis (ADR 0001), so the deliverable script
and the executed ARD are the same code. Wraps
[`cardx::ard_categorical_ci()`](https://insightsengineering.github.io/cardx/latest-tag/reference/ard_categorical_ci.html)
with `method = "clopper-pearson"`; the n / N / estimate rows cardx also
returns are dropped (a paired count analysis supplies them, and
duplicate statistic identities would make
[`cards::bind_ard()`](https://insightsengineering.github.io/cards/latest-tag/reference/bind_ard.html)
error). The `"card"` class is re-asserted after the subset so the result
binds cleanly across `{cards}` versions.

## Usage

``` r
ard_proportion_ci_exact(data, variables, by = NULL, conf.level = 0.95)
```

## Arguments

- data:

  A data frame.

- variables:

  Length-1 column name of the response variable.

- by:

  Optional column name(s) of the grouping (treatment) variable.

- conf.level:

  Confidence level (default `0.95`).

## Value

A `{cards}` ARD of `conf.low` / `conf.high` rows per group level.

## See also

[`ars_to_ard()`](https://tavakohr.github.io/arsbridge/reference/ars_to_ard.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  ard_proportion_ci_exact(adrs, variables = "AVALC", by = "TRT01A")
} # }
```
