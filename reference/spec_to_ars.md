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
  sap_path = NULL,
  output_path = file.path(tempdir(), "reporting_event.json"),
  study_id = "STUDY-001",
  study_name = NULL,
  model = NULL,
  api_key = NULL,
  provider = NULL,
  spec_column_aliases = NULL,
  extract_with_llm = TRUE,
  validate = TRUE,
  report_path = file.path(tempdir(), "spec_validation_report.xlsx"),
  code_dir = NULL,
  adam_dir = ".",
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

- sap_path:

  Optional path to the Statistical Analysis Plan `.docx`. When supplied,
  its prose is matched per TLF and carried into each analysis as
  `sapDescription`, becoming the human-readable comment above the
  emitted `{cards}` block. Gracefully ignored when absent or unreadable.

- output_path:

  Path for the ARS JSON. Defaults to `reporting_event.json` in
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html); pass an explicit
  path to write it somewhere permanent.

- study_id:

  Study identifier. Default `"STUDY-001"`.

- study_name:

  Human-readable study name. Defaults to `study_id`.

- model:

  LLM model. Defaults to the active provider's default model.

- api_key:

  LLM API key. Defaults to the active provider's key.

- provider:

  LLM provider: `"anthropic"`, `"openai"`, or `"gemini"`. Defaults to
  the active provider.

- spec_column_aliases:

  Optional named list of extra column-name aliases for the ADaM spec
  Excel (see `parse_adam_spec()`); useful when a workbook uses
  non-standard or non-English headers. Example:
  `list(variable = "nom de variable", dataset = "domaine")`.

- extract_with_llm:

  If `TRUE` (default), the LLM re-reads each section's raw shell cells
  as the primary annotation reader, separating display label from
  variable reference in variant layouts. Every proposed
  `DATASET.VARIABLE` is gated against the ADaM spec – out-of-spec
  proposals are rejected and logged as blockers, never shipped. With no
  API key the pass degrades to the deterministic regex result and emits
  one warning. Set `FALSE` to use deterministic parsing only.

- validate:

  If `TRUE` (default), cross-reference annotations against the ADaM spec
  and write a validation report.

- report_path:

  Path for the validation report `.xlsx`. Defaults to
  `spec_validation_report.xlsx` in
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- code_dir:

  Directory for the emitted per-TLF pure-`{cards}` `.R` deliverables.
  When `NULL` (default) a `code/` folder next to `output_path` is used.
  These scripts are both the human-readable deliverable and the engine
  [`ars_to_ard()`](ars_to_ard.md) sources to build the ARD.

- adam_dir:

  ADaM directory baked into each emitted script's header (the reader can
  edit it). Default `"."`.

- verbose:

  Print progress messages. Default `TRUE`.

## Value

Invisibly returns a named list:

- `ars_path`:

  Path to the generated ARS JSON file.

- `report_path`:

  Path to the validation report (if validate=TRUE).

- `code_dir`:

  Directory holding the emitted per-TLF `{cards}` `.R` deliverables.

- `code_paths`:

  Named character vector of the emitted `.R` paths (names = output ids).

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

- `diagnostics`:

  Data frame of pipeline diagnostics – every fallback, parsing miss,
  skipped sheet, LLM failure, unknown method, and dropped where-clause
  condition recorded during the run (`stage`, `severity`, `tlf_number`,
  `location`, `problem`, `action`). Also written to the "Diagnostics"
  sheet of the validation report and retrievable via
  [`ars_diagnostics()`](ars_diagnostics.md).

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
