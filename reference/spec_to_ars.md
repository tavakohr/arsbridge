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
  supplement = NULL,
  use_llm = FALSE,
  spec_column_aliases = NULL,
  extract_with_llm = TRUE,
  ship_annotations = FALSE,
  heading_patterns = NULL,
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

- supplement:

  Optional path to a supplement `.json` produced by a chat assistant
  from the instruction file written by
  [`ars_copilot_instructions()`](https://tavakohr.github.io/arsbridge/reference/ars_copilot_instructions.md).
  When supplied, NO live LLM calls are made (even if a key is set): the
  supplement's bindings fill only the rows the deterministic pass left
  unannotated (shell annotations always win; disagreements are WARN
  findings) and its per-TLF fields feed the same enrichment path a live
  LLM answer would. Every supplement variable passes the hard ADaM-spec
  gate. Pre-flight a file with
  [`ars_validate_supplement()`](https://tavakohr.github.io/arsbridge/reference/ars_validate_supplement.md).

  Regex is the always-on baseline and the default; the LLM is opt-in
  (see `use_llm`). Deterministic and supplement are first-class modes –
  the function never asks for a key nor raises a key-related error or
  warning in them; the mode that ran is recorded as a neutral INFO note
  and in `extraction_mode` / `_meta.extraction_mode`.

- use_llm:

  Opt in to the live LLM tier. Default `FALSE` – the pipeline runs
  regex-only (deterministic) and makes NO live LLM call, *even when an
  API key is configured*. Set `TRUE` to use the LLM for annotation
  extraction and semantic enrichment when a key is available; with
  `TRUE` but no key, the run still degrades silently to deterministic
  (never an error). Ignored when `supplement` is given (that path makes
  no live LLM calls either).

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
  proposals are rejected and logged as blockers, never shipped. This is
  a sub-control of the `llm` tier: it only has any effect when
  `use_llm = TRUE`. Set `FALSE` to keep the LLM enrichment pass but skip
  the LLM extraction pass.

- ship_annotations:

  If `FALSE` (default), programmer annotation lines found outside the
  stub cells (e.g. red `Label -> DATASET.VAR` paragraphs below a table)
  are kept for row binding and the validation report but are NEVER
  emitted into the ARS Footnote display section – rendered footnotes
  then contain only true footnotes. Set `TRUE` to append them to the
  footnotes (debug escape hatch).

- heading_patterns:

  Optional character vector of PCRE patterns tried BEFORE the built-in
  TLF heading grammars, for sponsor shells whose headings the built-ins
  do not recognise. Each pattern must use named capture groups:
  `(?<number>...)` (required – the dotted TLF number), `(?<type>...)`
  (optional, matching Table/Figure/Listing; defaults to Table), and
  `(?<title>...)` (optional inline title; the title tail is then
  decomposed into title/population/source datasets the same way built-in
  headings are). Custom patterns are accepted as-is – the built-in
  prose/TOC rejection rules are not applied to them. Not needed for the
  built-in formats – a bare `"Table 14.1.1"`, a colon inline title
  `"Table 14.1.1: Title"`, and one-line headings that carry the title, a
  dash-separated population, an inline annotation, and a
  programming-datasets suffix together. Example:
  `"^(?i)Output\\s+(?<number>\\d+(?:\\.\\d+)*)\\s*:\\s*(?<title>.*)$"`.

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
  [`ars_to_ard()`](https://tavakohr.github.io/arsbridge/reference/ars_to_ard.md)
  sources to build the ARD.

- adam_dir:

  ADaM directory baked into each emitted script's header (the reader can
  edit it). Default `"."`.

- verbose:

  Print progress messages. Default `TRUE`.

## Value

Invisibly returns a named list:

- `ars_path`:

  Path to the generated ARS JSON file.

- `extraction_mode`:

  Which tier ran: `"llm"`, `"supplement"`, or `"deterministic"`. Also
  stored in the JSON as `_meta.extraction_mode`.

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
  [`ars_diagnostics()`](https://tavakohr.github.io/arsbridge/reference/ars_diagnostics.md).

## Writing identifiable TLF headings

arsbridge splits the shell into outputs by finding TLF heading
paragraphs. For a heading to be recognised reliably, write it as **its
own ordinary paragraph** (not inside a text box, shape, table cell, or
field code) that **begins with `Table`, `Figure`, or `Listing` followed
by the output number**. A title should follow the number. All of these
are read:

    Table 14.1.1
    Table 14.1.1: Summary of Demographics
    Table 14.1.1 Summary of Demographics
    Table 14.1.1 Summary of Demographics - Safety Population ADSL.SAFFL='Y'
    Table 14.1.1 Demographics - Screened Subjects ADSL.SCRNFL='Y' [PROGRAMMING DATASETS USED: ADSL]

The population, an inline annotation, and a
`[PROGRAMMING DATASETS USED: ...]` suffix may all ride on the same line;
annotation values may use single quotes, double quotes, or an unquoted
number (`ADSL.COHORTN=1`). The **recommended** form for a clean,
portable shell is the explicit colon title –
`Table 14.1.1: Descriptive Title` – with the population on the next
line.

These are deliberately **not** treated as headings, to avoid false
splits: prose that mentions a number (`Table 14.1.1 shows ...`),
cross-references (`See Table 14.1.1 ...`), table-of-contents lines, and
bare section numbers with no designator
(`14.1 Demographic and Baseline Tables`). When the parser finds no
heading, or finds a number but no title, it says so and repeats this
guidance. For a sponsor template whose headings genuinely differ, pass
`heading_patterns` rather than reformatting the shell.

## Human review

The generated ARS JSON is a draft. A qualified clinical programmer MUST
review it before downstream use. The JSON includes a
`_meta.requires_human_review = TRUE` field that consumers can key on.

## See also

[`ars_copilot_instructions()`](https://tavakohr.github.io/arsbridge/reference/ars_copilot_instructions.md)
and
[`ars_validate_supplement()`](https://tavakohr.github.io/arsbridge/reference/ars_validate_supplement.md)
for the no-API `supplement =` workflow;
[`set_llm_key()`](https://tavakohr.github.io/arsbridge/reference/set_llm_key.md)
to configure a live LLM. Background:
[`vignette("no-api-access")`](https://tavakohr.github.io/arsbridge/articles/no-api-access.md).

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
