# Convert annotated TLF shell and ADaM spec to CDISC ARS JSON

Reads a lead programmer's already-annotated TLF shells Word document and
the study's ADaM specification Excel, and produces a valid CDISC
Analysis Results Standard (ARS) v1.0 ARM-TS JSON file consumable by
[`siera::readARS()`](https://clymbclinical.github.io/siera/reference/readARS.html).

## Usage

``` r
spec_to_ars(
  shell_path,
  adam_spec_path,
  output_path = "reporting_event.json",
  study_id = "STUDY-001",
  study_name = NULL,
  model = NULL,
  api_key = NULL,
  provider = NULL,
  validate = TRUE,
  report_path = "spec_validation_report.xlsx",
  verbose = TRUE
)
```

## Arguments

- shell_path:

  Path to annotated TLF shells `.docx`.

- adam_spec_path:

  Path to the ADaM specification. Accepts either:

  - `.xml` – ADaM `define.xml` (preferred when available)

  - `.xlsx` / `.xls` – ADaM specification Excel (fallback used during
    development before `define.xml` is produced)

  One of the two is required. The SDTM spec is NOT a valid input – TLF
  annotations reference ADaM variables, so the grounding source must be
  the ADaM spec.

- output_path:

  Path for the ARS JSON. Default `"reporting_event.json"`.

- study_id:

  Study identifier. Default `"STUDY-001"`.

- study_name:

  Human-readable study name. Defaults to `study_id`.

- model:

  Anthropic model. Default `"claude-sonnet-4-6"`.

- api_key:

  Anthropic API key. Defaults to env `ANTHROPIC_API_KEY`.

- validate:

  If `TRUE` (default), cross-reference annotations against the ADaM spec
  and write a validation report.

- report_path:

  Path for the validation report `.xlsx`. Default
  `"spec_validation_report.xlsx"`.

- verbose:

  Print progress messages. Default `TRUE`.

## Value

Invisibly returns a named list:

- `ars_path`:

  Path to the generated ARS JSON file.

- `report_path`:

  Path to the validation report (if validate=TRUE).

- `n_tlfs`:

  Number of TLF sections processed.

- `n_analyses`:

  Number of ARS Analysis objects created.

- `n_warnings`:

  Number of spec validation warnings.

- `reporting_event`:

  The full ARS ReportingEvent as a nested R list – the same content that
  was serialised to `ars_path`. Inspect interactively with e.g.
  `str(res$reporting_event, max.level = 2)`.

- `validation`:

  Data frame of per-annotation validation results (`tlf_number`,
  `stub_label`, `annotation`, `variable_ref`, `status`, `message`).
  `NULL` when `validate = FALSE`.

## Human review

The generated ARS JSON is a draft. A qualified clinical programmer MUST
review it before downstream use. The JSON includes a
`_meta.requires_human_review = TRUE` field that consumers can key on.

## Examples

``` r
if (FALSE) { # \dontrun{
spec_to_ars(
  shell_path     = "inputs/annotated_shells.docx",
  adam_spec_path = "inputs/adam_spec.xlsx",
  output_path    = "outputs/reporting_event.json",
  report_path    = "outputs/spec_validation_report.xlsx"
)
} # }
```
