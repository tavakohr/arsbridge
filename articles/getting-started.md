# Getting started with arsbridge

## What arsbridge does

`arsbridge` reads a lead programmer’s annotated TLF shell Word document
together with the study’s ADaM specification, and produces a CDISC
Analysis Results Standard (ARS) v1.0 JSON file. That JSON drives the
rest of the pipeline: it executes against real ADaM datasets using
[cards](https://github.com/insightsengineering/cards) to produce a tidy
ARD, then renders to a formatted GT clinical table with no manual
formatting step.

**The core principle:** arsbridge extracts and converts. It does not
invent. The shell is read by a deterministic regex detector *and* an LLM
primary reader together, to capture as many annotation variants as
possible. Every LLM-proposed variable is then checked against the ADaM
spec, so a variable absent from the spec is rejected and logged, never
shipped. Every variable in the ARS output traces back to a real
annotation grounded in the spec.

For the full reading-engine detail, see
[`vignette("reading-engine")`](https://tavakohr.github.io/arsbridge/articles/reading-engine.md).

------------------------------------------------------------------------

## Prerequisites

``` r

# Install from GitHub
devtools::install_github("tavakohr/arsbridge")
```

`arsbridge` runs **regex-first**: by default it needs no API key at all.
The LLM is **opt-in** — pass `use_llm = TRUE` to have Claude help with
annotation extraction and light per-TLF enrichment (analysis type,
method name, row role). Even then, all variable names come from the
shell annotations, never from the LLM.

``` r

# Only if you want the LLM tier: one-time key setup (hides input with {askpass})
arsbridge::set_anthropic_key()
arsbridge::check_anthropic_key()   # confirm the key loaded
# ...then call spec_to_ars(..., use_llm = TRUE)
```

> **No API key, or it’s blocked?** The default run already works with no
> key. For near-LLM accuracy without an API call,
> [`ars_copilot_instructions()`](https://tavakohr.github.io/arsbridge/reference/ars_copilot_instructions.md)
> sets up a workflow where a chat assistant (Copilot/ChatGPT) produces a
> supplement file
> [`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
> consumes. See
> [`vignette("no-api-access")`](https://tavakohr.github.io/arsbridge/articles/no-api-access.md).

### Writing headings arsbridge can find

arsbridge splits the shell into outputs by finding TLF heading
paragraphs, so give each output its own **ordinary paragraph** that
begins with `Table`, `Figure`, or `Listing` and its number. The
recommended, most portable form is an explicit colon title —
`Table 14.1.1: Descriptive Title` — with the population on the next
line. Several styles are read out of the box: a bare `Table 14.1.1`, the
colon title, and a one-line heading that also carries a dash-separated
population, an inline annotation, and a
`[PROGRAMMING DATASETS USED: ...]` suffix.

Keep the heading a normal paragraph: a title stranded in a **text box,
shape, table cell, or field/content control** is not read, and neither
is prose that only mentions a number (`Table 14.1.1 shows ...`) or a
bare section number (`14.1 Demographic and Baseline Tables`). When
arsbridge finds no heading — or a number but no title — it says so and
repeats this guidance. For a sponsor template with a genuinely different
convention, pass a custom PCRE pattern via
`spec_to_ars(heading_patterns = ...)` (named `number`/`type`/`title`
groups; see
[`?spec_to_ars`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md))
instead of reformatting the shell.

------------------------------------------------------------------------

## The fastest path: `spec_to_ars_example()`

The package ships with a small bundled training example so you can run
the entire pipeline before you own a study. This is the best way to get
a feel for the output and the validation report.

``` r

library(arsbridge)

# Default: regex-only, no API key needed, runs in seconds.
res <- spec_to_ars_example()

# Or, with a key configured, opt in to the LLM tier:
res <- spec_to_ars_example(use_llm = TRUE)

# ARS JSON and validation report land in tempdir(); paths are in res.
res$n_tlfs        # 40 TLF shells
res$n_analyses    # ~226 analyses
res$n_warnings    # ~29 warnings to review
res$extraction_mode   # "deterministic" by default, "llm" with use_llm = TRUE
```

The default deterministic run is fast and offline. With `use_llm = TRUE`
it makes about 40 LLM calls (one per TLF section) and takes a few
minutes.

------------------------------------------------------------------------

## Inspecting the result

Once
[`spec_to_ars_example()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars_example.md)
finishes, you have a structured `reporting_event` object and a row-level
validation report.

``` r

# Top-level shape of the ARS ReportingEvent
str(res$reporting_event, max.level = 1)

# Validation findings: one row per annotation
table(res$validation$status)
subset(res$validation, status %in% c("WARN", "FAIL"))

# Drill into one output
out <- Filter(function(o) o$name == "T-14-1-2",
              res$reporting_event$outputs)[[1]]
length(out$referencedAnalysisIds)
```

The **PASS / WARN / FAIL** stamps tell you:

| Status   | Meaning                                                         |
|----------|-----------------------------------------------------------------|
| **PASS** | Variable found in ADaM spec; annotation is clean.               |
| **WARN** | Regex and LLM disagree; human review recommended.               |
| **FAIL** | Variable not found in ADaM spec; will be rejected as a blocker. |

------------------------------------------------------------------------

## From ARS to a formatted clinical table

With a valid ARS JSON in hand, two more functions close the loop.

[`ars_to_ard()`](https://tavakohr.github.io/arsbridge/reference/ars_to_ard.md)
executes the ARS against ADaM datasets and returns a tidy ARD in
[cards](https://github.com/insightsengineering/cards) format.
[`ars_render_tlf()`](https://tavakohr.github.io/arsbridge/reference/ars_render_tlf.md)
then formats any output from that ARD into a publication-ready GT table,
with no manual formatting step.

``` r

# Unzip the bundled simulated ADaM data
adam_dir <- file.path(tempdir(), "ADaM")
unzip(arsbridge_example("ADaM.zip"), exdir = adam_dir)

# Execute the ARS specification into a tidy ARD
ard <- ars_to_ard(
  ars_path = res$ars_path,
  adam_dir = adam_dir
)

# Render the Subject Disposition table (T_14_1_1) as a GT clinical table
gt_table <- ars_render_tlf(
  ars_path  = res$ars_path,
  ard       = ard,
  output_id = "T_14_1_1"
)

gt_table
```

[`ars_render_tlf()`](https://tavakohr.github.io/arsbridge/reference/ars_render_tlf.md)
auto-detects the treatment column, row groups, and row labels from the
ARD; rescales [cards](https://github.com/insightsengineering/cards)
proportions to percentages; lays continuous summaries out as `Mean (SD)`
/ `Median` / `(Min, Max)` rows; and carries ARS footnotes through as GT
source notes.

To inspect or customise the underlying
[tfrmt](https://GSK-Biostatistics.github.io/tfrmt/) spec before
printing:

``` r

# One output: get the tfrmt spec
spec <- ars_to_tfrmt(res$ars_path, ard, "T_14_1_1")

# All outputs at once
specs <- ars_to_tfrmt_list(res$ars_path, ard)
names(specs)

all_tables <- lapply(
  names(specs),
  function(oid) ars_render_tlf(res$ars_path, ard, oid)
)
```

------------------------------------------------------------------------

## Using your own study files

``` r

res <- spec_to_ars(
  shell_path     = "inputs/annotated_shell.docx",
  adam_spec_path = "inputs/adam_spec.xlsx",        # or define.xml
  output_path    = "outputs/reporting_event.json",
  report_path    = "outputs/spec_validation_report.xlsx",
  study_id       = "ABC-123",
  study_name     = "ABC-123 Phase 3"
)
```

`adam_spec_path` accepts either `.xml` (ADaM `define.xml`, preferred
when available) or `.xlsx` / `.xls` (ADaM spec Excel, used during
development before `define.xml` exists). The SDTM spec is **not** a
valid input: TLF annotations reference ADaM variables.

------------------------------------------------------------------------

## The bundled example, file by file

``` r

arsbridge_example()                              # list all bundle contents
arsbridge_example("annotated_shell.docx")        # absolute path to a file
arsbridge_example("adam_spec.xlsx")
arsbridge_example("ADaM.zip")
```

| File | Purpose |
|----|----|
| `annotated_shell.docx` | TLF shells with red ADaM annotations (the input) |
| `adam_spec.xlsx` | ADaM variable spec (the grounding truth) |
| `ADaM.zip` | 60-subject simulated ADaM datasets (for [`ars_to_ard()`](https://tavakohr.github.io/arsbridge/reference/ars_to_ard.md)) |

------------------------------------------------------------------------

## What the shell parser actually detects

arsbridge uses a four-layer detection hierarchy and stops at the
highest-confidence layer that produces a result for each cell:

| Layer | Trigger | Confidence |
|----|----|----|
| 1\. Colour-based | Red `#C00000` runs matching an ADaM pattern | HIGH |
| 2\. Formatting-based | Bold / italic / underline runs matching a pattern | MEDIUM |
| 3\. Plain-text pattern | `DATASET.VARIABLE` strings identified by regex alone | HIGH |
| 4\. LLM pass | Cells where layers 1-3 produce nothing and context implies an annotation | Validated against spec |

In practice, the bundled `annotated_shell.docx` triggers Layer 1 for all
130 stub-row annotations. When shells use non-standard conventions, the
LLM pass catches what the regex misses.

------------------------------------------------------------------------

## Listing column-header detection

Listings differ from summary tables: the variable lives in the column
header, not the stub. A cell like:

    Subject ID
    USUBJID

gets split automatically. “Subject ID” becomes the display label and
`USUBJID` becomes the annotation. The dataset prefix is resolved by
combining a universal ADSL list (variables like `USUBJID`, `ARM`,
`TRT01A` always live in ADSL) with the TLF’s declared source dataset
from a `Source: ADAE` line below the table.

------------------------------------------------------------------------

## Partial tables and the manual-fill round-trip

Some tables mix statistics arsbridge can compute (counts, percentages,
summary statistics) with ones it cannot (a Cochran-Mantel-Haenszel
p-value, an exact confidence interval, an NRI-imputed responder rate).
arsbridge does not fabricate the latter or drop the whole table. Instead
it **reserves a keyed cell** for each: the ARD carries a
`manual_pending` stub row with `stat = NA`, tied to the same output,
analysis, and method as a real result.

List what is outstanding:

``` r

ard <- ars_to_ard(ars_path, "inputs/ADaM")
ars_manual_worklist(ard)   # one row per reserved cell
```

When rendered, computed cells appear with their values and each reserved
cell shows a loud `[‡ manual]` marker keyed to a footnote. No blank
cells, no `NA`, no misleading zeros.

To fill a reserved cell, compute it with a validated analysis script,
then write the result back into the ARD row:

``` r

i <- which(ard$result_status == "manual_pending")[1]
ard$stat[[i]]         <- 0.012
ard$result_status[i]  <- "manual_filled"
ard$value_source[i]   <- "manual"
ard$derivation_ref[i] <- "programs/cmh_t1421.R"
```

`derivation_ref` is the audit trail: not “someone typed 0.012” but
“value from `cmh_t1421.R`.” Before rendering, arsbridge checks every
manual fill:

``` r

ars_validate_manual_fills(ard)   # zero rows = every fill is traceable
```

[`ars_render_all()`](https://tavakohr.github.io/arsbridge/reference/ars_render_all.md)
runs this automatically and blocks any untraceable value before it
reaches the final document. The design and full rationale are in
`adr/0002-partial-results-traceability.md`.

------------------------------------------------------------------------

## What arsbridge will NOT do

- Invent variable names from shell row labels or titles
- Write Word documents (it only reads shells, not annotates them)
- Generate R analysis code (that is
  [siera](https://clymbclinical.github.io/siera/)’s job)
- Process SAS data files directly without conversion to `.xpt` or `.csv`

------------------------------------------------------------------------

## Where to look when something goes wrong

| What to check | Where to look |
|----|----|
| Per-annotation PASS / WARN / FAIL | `res$validation` and `outputs/spec_validation_report.xlsx` |
| Runtime warnings during extraction | [`cli::cli_alert_warning`](https://cli.r-lib.org/reference/cli_alert.html) lines printed during [`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md) |
| Known-good minimal shells | `tests/testthat/fixtures/` in the package source |
| Blocker-only view | `arsbridge::ars_blockers(res$diagnostics)` |
| Everything recorded | `arsbridge::ars_diagnostics(res$diagnostics)` |
