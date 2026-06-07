# Bundled training files shipped with arsbridge

Returns the absolute path to a bundled example file, or – when called
with no argument – the names of all files available in the bundle. The
bundle is a small, anonymised slice of the APX-DRM-301 atopic dermatitis
study used throughout the documentation and tests.

## Usage

``` r
arsbridge_example(file = NULL)
```

## Arguments

- file:

  Character. Name of a bundled file (e.g. `"annotated_shell.docx"`). If
  `NULL` (default), returns a character vector of every file in the
  bundle.

## Value

A character path (absolute) when `file` is named; a character vector of
file names when `file` is `NULL`.

## Details

Files currently in the bundle:

- `annotated_shell.docx`:

  Lead-programmer annotated TLF shells for 40 TLFs (24 tables + 10
  listings + 6 figures). Uses the standard red `C00000` run convention
  for ADaM variable references. Roughly 80 KB.

- `adam_spec.xlsx`:

  ADaM specification workbook covering 8 domains (ADSL, ADAE, ADCM,
  ADEFF, ADEX, ADLB, ADMH, ADVS). Roughly 95 KB.

- `ADaM.zip`:

  Simulated 60-subject ADaM data as XPT files (the eight domains above).
  Stratified by treatment arm (13 / 23 / 24 across Placebo / UPADALIMIB
  15 mg / UPADALIMIB 30 mg). Roughly 680 KB compressed, 12 MB extracted.
  Consumed by the downstream `siera_workflow/` runner, not by arsbridge
  itself.

## Examples

``` r
arsbridge_example()                       # list bundle contents
#> [1] "ADaM.zip"             "README.md"            "adam_spec.xlsx"      
#> [4] "annotated_shell.docx"
arsbridge_example("annotated_shell.docx") # path to the shell
#> [1] "/home/runner/work/_temp/Library/arsbridge/extdata/example_apx_drm_301/annotated_shell.docx"
arsbridge_example("adam_spec.xlsx")       # path to the ADaM spec
#> [1] "/home/runner/work/_temp/Library/arsbridge/extdata/example_apx_drm_301/adam_spec.xlsx"
```
