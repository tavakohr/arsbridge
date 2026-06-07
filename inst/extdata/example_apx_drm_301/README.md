# Bundled training example -- APX-DRM-301

A small, anonymised slice of a Phase 3 atopic dermatitis study used to
teach `arsbridge` and exercise the full upstream/downstream pipeline.

## Files

| File | Size | Notes |
|---|---:|---|
| `annotated_shell.docx` | 79 KB | Lead-programmer annotated TLF shells: 40 outputs (24 tables, 10 listings, 6 figures). Standard red `C00000` annotation convention. |
| `adam_spec.xlsx` | 94 KB | ADaM specification workbook: 8 domains (ADSL, ADAE, ADCM, ADEFF, ADEX, ADLB, ADMH, ADVS). |
| `ADaM.zip` | 662 KB | 60-subject ADaM XPT subsample, stratified by treatment arm (13 / 23 / 24 across Placebo / UPADALIMIB 15 mg / UPADALIMIB 30 mg). Consumed by the `siera_workflow/` runner downstream. NOT consumed by arsbridge itself. |

## How to use from R

```r
library(arsbridge)

# List the bundle
arsbridge_example()

# Get a path to one bundled file
arsbridge_example("annotated_shell.docx")

# Run the full pipeline against the bundle
res <- spec_to_ars_example()
```

For the downstream side (siera -> ADaM data -> TLFs), see
`siera_workflow/README.md` in the package repository.
