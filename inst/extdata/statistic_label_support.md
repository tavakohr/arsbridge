# Statistic labels arsbridge can read

Generated from the vocabulary and the method catalogue by
`.stat_label_support_md()`. Do not edit by hand -- a test regenerates
this file and compares it, so an edit here fails the suite.

A shell's statistic line names a SEMANTIC statistic. Whether that
statistic can be produced is the analysis method's answer, not the
label's, so the same label resolves differently -- or refuses -- under
different methods. A statistic a method does not declare leaves the
whole row unbound and is reported; it is never approximated, and never
bound to whichever operation happens to come first.

## Phrases recognised, per statistic

| Statistic | Written as |
|---|---|
| `count` | `n`, `count`, `nobs`, `n obs`, `number of subjects`, `number of observations`, `non missing`, `nonmissing` |
| `pct` | `%`, `pct`, `percent`, `percentage`, `proportion`, `column %`, `col %`, `row %` |
| `mean` | `mean`, `arithmetic mean`, `average` |
| `sd` | `sd`, `s d`, `std`, `stdev`, `std dev`, `standard deviation` |
| `se` | `se`, `sem`, `std error`, `standard error`, `standard error of the mean` |
| `median` | `median`, `med`, `50th percentile`, `p50`, `q2` |
| `q1` | `q1`, `quartile 1`, `first quartile`, `1st quartile`, `lower quartile`, `25th percentile`, `p25` |
| `q3` | `q3`, `quartile 3`, `third quartile`, `3rd quartile`, `upper quartile`, `75th percentile`, `p75` |
| `min` | `min`, `minimum`, `lowest` |
| `max` | `max`, `maximum`, `highest` |
| `cv` | `cv`, `coefficient of variation` |
| `geomean` | `geometric mean`, `geomean` |
| `events` | `events`, `number of events` |
| `pvalue` | `p value`, `pvalue`, `p val` |

One word may name an ordered pair:

| Written as | Names |
|---|---|
| `range` | `min`, `max` |
| `iqr` | `q1`, `q3` |
| `interquartile range` | `q1`, `q3` |
| `ci` | `ci_low`, `ci_high` |
| `confidence interval` | `ci_low`, `ci_high` |
| `<num> % ci` | `ci_low`, `ci_high` |
| `<num> % confidence interval` | `ci_low`, `ci_high` |

## Resolution, per method

`operation` is the ARS operation the statistic binds to; `ARD name` is
what the execution engine calls the result. A blank row means the
method declares no operation for that statistic.

### MTH_SUMMARY_STATISTICS_CONTINUOUS

| Statistic | Operation | ARD name |
|---|---|---|
| `count` | `OP_N` | `N or n` |
| `mean` | `OP_MEAN` | `mean` |
| `sd` | `OP_SD` | `sd` |
| `median` | `OP_MEDIAN` | `median` |
| `q1` | `OP_Q1` | `p25` |
| `q3` | `OP_Q3` | `p75` |
| `min` | `OP_MIN` | `min` |
| `max` | `OP_MAX` | `max` |

Recognised but not produced by this method: `pct`, `se`, `cv`, `geomean`, `events`, `pvalue`.

### MTH_COUNT_AND_PERCENTAGE

| Statistic | Operation | ARD name |
|---|---|---|
| `count` | `OP_N` | `n` |
| `pct` | `OP_PCT` | `p` |

Recognised but not produced by this method: `mean`, `sd`, `se`, `median`, `q1`, `q3`, `min`, `max`, `cv`, `geomean`, `events`, `pvalue`.

### MTH_SUBJECT_COUNT

| Statistic | Operation | ARD name |
|---|---|---|
| `count` | `OP_N` | `n` |

Recognised but not produced by this method: `pct`, `mean`, `sd`, `se`, `median`, `q1`, `q3`, `min`, `max`, `cv`, `geomean`, `events`, `pvalue`.

### MTH_SUBJECT_COUNT_PCT

| Statistic | Operation | ARD name |
|---|---|---|
| `count` | `OP_N` | `n` |
| `pct` | `OP_PCT` | `p` |

Recognised but not produced by this method: `mean`, `sd`, `se`, `median`, `q1`, `q3`, `min`, `max`, `cv`, `geomean`, `events`, `pvalue`.

### MTH_KAPLAN_MEIER_ESTIMATE

| Statistic | Operation | ARD name |
|---|---|---|
| `median` | `OP_MEDIAN` | `median` |
| `events` | `OP_EVENTS` | `events` |

Recognised but not produced by this method: `count`, `pct`, `mean`, `sd`, `se`, `q1`, `q3`, `min`, `max`, `cv`, `geomean`, `pvalue`.

### MTH_AE_FREQUENCY_COUNT

| Statistic | Operation | ARD name |
|---|---|---|
| `count` | `OP_N` | `n` |
| `pct` | `OP_PCT` | `p` |

Recognised but not produced by this method: `mean`, `sd`, `se`, `median`, `q1`, `q3`, `min`, `max`, `cv`, `geomean`, `events`, `pvalue`.

### MTH_LISTING

| Statistic | Operation | ARD name |
|---|---|---|
| _(none)_ | | |

Recognised but not produced by this method: `count`, `pct`, `mean`, `sd`, `se`, `median`, `q1`, `q3`, `min`, `max`, `cv`, `geomean`, `events`, `pvalue`.

