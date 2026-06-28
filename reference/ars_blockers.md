# Blocking problems from the most recent run, in plain English

The show-stoppers: every `FAIL`-severity finding from the most recent
[`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
or
[`ars_to_ard()`](https://tavakohr.github.io/arsbridge/reference/ars_to_ard.md)
call – the gaps that mean arsbridge could not produce clean ARS / ARD /
ready-to-run R code. Each row names the input document to open
(`input`), what is wrong and why (`problem`), and how to fix it
(`action`). A zero-row result means there were no blocking gaps.

## Usage

``` r
ars_blockers(diagnostics = ars_diagnostics())
```

## Arguments

- diagnostics:

  Data frame of diagnostics to summarise. Defaults to the findings from
  the most recent run
  ([`ars_diagnostics()`](https://tavakohr.github.io/arsbridge/reference/ars_diagnostics.md)).

## Value

Data frame with columns `input`, `problem`, `action`, `stage`,
`tlf_number`, `location` – one row per blocking (FAIL) finding.

## Details

This is the same set surfaced at the top of the validation report ("What
to fix first") and returned in the `blockers` element of the
[`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
result; this accessor exists for interactive inspection.

## Examples

``` r
if (FALSE) { # \dontrun{
spec_to_ars("shells.docx", "adam_spec.xlsx")
ars_blockers()   # what must be fixed, in plain English
} # }
```
