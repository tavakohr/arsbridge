# Build every output for a study, in one call

Runs the whole pipeline –
[`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md),
then
[`ars_to_ard()`](https://tavakohr.github.io/arsbridge/reference/ars_to_ard.md),
then
[`ars_fill_shell()`](https://tavakohr.github.io/arsbridge/reference/ars_fill_shell.md)
for an Excel shell – and returns one structured list describing what it
produced. Takes paths rather than data, holds no state, and never
throws, so it can be sent to a background process and its result
rendered by a UI.

## Usage

``` r
ars_workflow_run(
  shell_path,
  adam_spec_path,
  output_dir,
  adam_dir = NULL,
  study_id = "STUDY-001",
  supplement = NULL,
  use_llm = FALSE,
  llm_provider = NULL,
  api_key = NULL,
  derived_dt = NULL,
  log_path = NULL,
  on_progress = NULL
)
```

## Arguments

- shell_path:

  Annotated TLF shell, `.docx` or `.xlsx`.

- adam_spec_path:

  ADaM spec (`.xlsx` or define.xml).

- output_dir:

  Directory to write the outputs into; created if absent.

- adam_dir:

  Directory of ADaM datasets. Without it the ARD and the filled workbook
  are skipped and reported, and only the ARS is built.

- study_id:

  Study identifier stamped into the reporting event.

- supplement:

  Optional Copilot supplement JSON.

- use_llm:

  Whether to call an LLM for the annotations the regex cannot resolve.
  `FALSE` runs deterministically.

- llm_provider, api_key:

  LLM settings, passed explicitly because a background process inherits
  neither the option nor the environment.

- derived_dt:

  ISO-8601 timestamp to stamp on computed ARD rows. Pass a fixed value
  to make a run byte-reproducible; `NULL` uses the clock.

- log_path:

  Where the run's console output was captured, if the caller redirected
  it. Recorded in `artifacts$run_log` so the payload can point at its
  own log.

- on_progress:

  Optional function called with one progress event at a time: a list
  with `stage`, `stage_idx`, `n_stages`, `i`, `n`, `label`. Each stage
  announces itself with `i = 0`, then ticks once per TLF, analysis, or
  sheet – `i` counting the items already FINISHED and `label` naming the
  one now in flight. The work on either side of those loops (reading the
  inputs, writing the reporting event, saving the workbook) ticks as a
  named step with no count, so no stage runs out silent. Errors it
  raises are swallowed – a progress bar must never take a build down.
  `NULL` (the default) reports nothing and changes nothing.

## Value

A list with `status` (`"success"`, `"partial"` or `"error"`), `timings`,
`artifacts`, `metadata`, `diagnostics` (one row per finding, with
`severity` – never split, so `INFO` findings survive), `pending` (cells
reserved for manual derivation, ADR 0002), `fill` (the fill stage's
headline counts – `filled`, `pending`, `skipped` – or `NULL` when no
fill ran), `unfilled_cells` (workbook cells left showing a placeholder,
and why), and `error`.

## Details

This is what
[`ars_workflow()`](https://tavakohr.github.io/arsbridge/reference/ars_workflow.md)'s
build step calls. It is exported because a background worker has to be
able to reach it (`arsbridge::ars_workflow_run`), and because running
the whole pipeline headlessly is useful on its own – in a script, in a
scheduled job, or in a validation run.

## See also

[`ars_workflow()`](https://tavakohr.github.io/arsbridge/reference/ars_workflow.md)
for the app,
[`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
for the build step.

## Examples

``` r
if (FALSE) { # \dontrun{
  payload <- ars_workflow_run(
    shell_path     = "inputs/shells.xlsx",
    adam_spec_path = "inputs/adam_spec.xlsx",
    adam_dir       = "inputs/ADaM",
    output_dir     = "outputs")
  payload$status
  subset(payload$diagnostics, severity == "FAIL")
} # }
```
