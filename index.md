# arsbridge

Convert an annotated TLF shells Word document and an ADaM specification
into a CDISC Analysis Results Standard (ARS) v1.0 ARM-TS JSON file,
ready for consumption by
[`siera::readARS()`](https://pharmaverse.github.io/siera/).

The package is designed for the real CRO workflow: a **lead programmer
annotates the shell** with the ADaM variables that drive each row, then
`arsbridge` mechanically extracts and structures those annotations as
CDISC ARS without ever inventing a variable name.

## Key design ideas

- **Extracts, never invents.** The lead programmer’s annotations are
  authoritative. `arsbridge` will never substitute an LLM guess for a
  variable name the programmer wrote.
- **Style-agnostic annotation detection.** Four detection layers: red
  `C00000` font → bold/italic/underline → plain-text ADaM regex → LLM
  fallback. Works on shells that follow your CRO’s house style, whatever
  it is.
- **LLM used only for semantic enrichment** — analysis type, method
  name, row role — never for variable name resolution. One Claude call
  per TLF section.
- **Bundled training example** so you can run the full pipeline before
  you own a study.

## Installation

``` r

# install.packages("devtools")
devtools::install_github("tavakohr/arsbridge")
```

CRAN release pending pharmaverse community review.

## Quick start

``` r

library(arsbridge)

# One-time API key setup (interactive prompt; key written to ~/.Renviron)
set_anthropic_key()
check_anthropic_key()

# Zero-arg demo against the bundled APX-DRM-301 training example
res <- spec_to_ars_example()

res$n_tlfs       #  40
res$n_analyses   #  ~226
res$n_warnings   #  ~29  (these are real signal -- spec gaps the lead
                 #        programmer flagged for ADaM team review)

# Inspect the structured output without re-reading the JSON
str(res$reporting_event, max.level = 1)
table(res$validation$status)
```

Run takes about six minutes (forty LLM calls).

## Using your own files

``` r

res <- spec_to_ars(
  shell_path     = "inputs/annotated_shell.docx",
  adam_spec_path = "inputs/adam_spec.xlsx",      # or define.xml
  output_path    = "outputs/reporting_event.json",
  report_path    = "outputs/spec_validation_report.xlsx",
  study_id       = "ABC-123",
  study_name     = "ABC-123 Phase 3"
)
```

`adam_spec_path` accepts either `.xml` (ADaM define.xml, preferred when
available) or `.xlsx` / `.xls` (ADaM spec Excel, used during development
before define.xml exists). The SDTM spec is **not** a valid input – TLF
annotations reference ADaM variables, so the grounding source has to be
the ADaM spec.

## What the validation report tells you

`outputs/spec_validation_report.xlsx` has one row per annotation with
PASS / WARN / FAIL status, status-tinted. WARN and FAIL findings are the
testable surface for the lead programmer and the ADaM team: every
“variable X not in spec but dataset Y exists” line is either a typo in
the shell or a real spec gap to fill. The JSON itself is written
regardless of validation outcome – WARN/FAIL findings are signal, not
blockers.

## Bundled example

``` r

arsbridge_example()                        # list bundled files
arsbridge_example("annotated_shell.docx")  # absolute path
arsbridge_example("adam_spec.xlsx")
arsbridge_example("ADaM.zip")              # 60-subject simulated ADaM
                                           # data for downstream testing
```

The bundle is a small, anonymised slice of a Phase 3 atopic dermatitis
study (`APX-DRM-301`): 40 TLFs (24 tables + 10 listings + 6 figures), 8
ADaM domains, 60 subjects stratified by treatment arm. The synthetic
ADaM data is not consumed by `arsbridge` itself – it is for testing the
downstream consumer (`siera`) end-to-end.

## License

MIT (c) Hamid Tavakoli. See [LICENSE.md](LICENSE.md).

## Acknowledgements

Built on the CDISC Analysis Results Standard v1.0. Prompt conventions
derived from the public ADaM Implementation Guide and PHUSE community
papers. The lead-programmer-first design philosophy is borrowed from the
[pharmaverse](https://pharmaverse.org/) community consensus that human
clinical judgment must own the variable-to-row mapping; automation
operationalises that judgment, never replaces it.
