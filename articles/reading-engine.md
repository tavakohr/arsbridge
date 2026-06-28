# How arsbridge reads an annotated shell

## The problem: every shell is annotated differently

A TLF shell is a Word document. The lead programmer marks which ADaM
variable feeds each row, but there is **no single convention**. Across
studies, sponsors, and individuals you see the variable reference
carried by:

- a coloured run (`ADSL.AGE` in red),
- bold, italic, or underlined text,
- square brackets (`[ADAE.AEDECOD WHERE AEREL='RELATED']`),
- plain text appended after the label (`Age (years) ADSL.AGE`), or
- a layout no regex has ever seen: the variable on line 2 of the cell,
  in a sentence, split across runs, mixed-case, abbreviated.

A pure pattern matcher reads the conventions it was written for and
silently misses the rest. arsbridge is built to **extract as many
annotation variants as possible** while never shipping a variable it
cannot prove exists.

------------------------------------------------------------------------

## The engine: deterministic regex plus LLM, together

arsbridge reads each shell **twice and takes the union.**

                     annotated shell (.docx)
                              |
            +-----------------+-----------------+
            |                                   |
            v                                   v
    1. DETERMINISTIC PASS               2. LLM PASS (primary)
       parse_shell_docx()                  extract_shell_llm()
       four-layer regex detector            re-reads each raw cell,
       - colour                             separates label vs variable
       - bold/italic/underline              in ANY layout
       - plain-text DATASET.VAR
       - bracket [DATASET.VAR]
            |                                   |
            +-----------------+-----------------+
                              |
                              v
                3. HARD ADaM-SPEC GATE
           every proposed DATASET.VARIABLE must
           exist in the ADaM spec, or it is
           rejected and logged as a blocker
                              |
                              v
                  validated annotations -> ARS JSON

------------------------------------------------------------------------

### Pass 1: deterministic regex (fast, free, offline)

`parse_shell_docx()` walks the document’s OOXML and runs a four-layer
detector on every stub cell and listing header. It recognises the known
annotation conventions above with no API call. This pass alone fully
handles shells that follow a standard annotation style, and it always
runs even with no API key.

The four layers, in priority order:

| Layer | Signal | Confidence |
|----|----|----|
| 1\. Colour | Red `#C00000` runs matching a `DATASET.VARIABLE` pattern | HIGH |
| 2\. Formatting | Bold / italic / underline runs matching a pattern | MEDIUM |
| 3\. Plain-text | `DATASET.VARIABLE` strings identified by regex alone | HIGH |
| 4\. Handoff | Cells where 1-3 produce nothing; passed to the LLM | Validated |

------------------------------------------------------------------------

### Pass 2: the LLM as primary reader (handles the variants)

`extract_shell_llm()` re-reads the **raw** text of each cell and asks
the model to separate the human display label from the machine variable
reference, in whatever layout the shell happens to use. This is where
the “any convention” power comes from: the LLM generalises to formats
the regex was never written for.

It returns structured output via an `ellmer` type, never free text to
parse. The model is asked to produce `NULL` for cells with no variable,
which means it does not hallucinate variables for row-label-only cells.

------------------------------------------------------------------------

### Pass 3: the hard spec gate (the seatbelt)

The LLM is powerful but it can hallucinate. So **every** proposed
`DATASET.VARIABLE` is checked against your ADaM specification
(`define.xml` or Excel). A proposal that is not in the spec is
**dropped, not shipped**, and recorded as a blocking finding that names
the row and the rejected token.

The spec is the ground-truth oracle: the model can only pick variables
that actually exist in your study. This preserves arsbridge’s founding
promise, that it extracts and converts rather than invents, while
letting the LLM read freely.

------------------------------------------------------------------------

## How the two passes combine

A row’s annotation is taken if **either** pass finds it. On a conflict,
the LLM wins (it is the primary reader) and a warning flags the
disagreement for a human to check.

| Situation | Regex | LLM | Result |
|----|:--:|:--:|----|
| Known annotation convention | ✓ | silent | Regex result kept |
| Known convention, same read | ✓ | ✓ same | Confirmed |
| Known convention, different reads | ✓ | ✓ different | **LLM wins** + `WARN` |
| Variant layout regex cannot parse | ✗ | ✓ | **LLM adds the row** |
| LLM proposes a non-spec variable | ✗ | rejected | Dropped + **blocker logged** |
| Row genuinely has no variable | ✗ | silent | Empty |

Guards keep the regex result safe in failure modes: if the LLM returns
an empty dataset or variable, omits a row, or no API key is set, the
deterministic result stands.

------------------------------------------------------------------------

## Degraded mode: it still runs with no key

No API key, or `extract_with_llm = FALSE`, means Pass 2 is skipped and
arsbridge runs on the deterministic regex alone, emitting one warning
that variant-format extraction is unavailable. The pipeline still
produces ARS, ARD, and rendered tables for standard shells: offline, in
CI, and without a billing account.

``` r

res <- arsbridge::spec_to_ars(
  shell_path       = "inputs/shells.docx",
  adam_spec_path   = "inputs/adam_spec.xlsx",
  extract_with_llm = TRUE    # default; set FALSE for regex-only mode
)
```

------------------------------------------------------------------------

## Every gap is reported in plain English

Nothing is dropped silently. Rejected variables, regex/LLM
disagreements, LLM outages, and unparsed rows all land in the
diagnostics with the **input document location to fix** named:

``` r

arsbridge::ars_diagnostics(res$diagnostics)  # everything recorded during the run
arsbridge::ars_blockers(res$diagnostics)     # only what blocks clean output
```

A blocker tells you *what* (rejected variable / missed row), *why* (not
in spec / unknown convention), and *how to fix* (correct the shell
annotation, or add the variable to the ADaM spec).

------------------------------------------------------------------------

## How the listing header reader works

Listings differ from summary tables: the variable reference lives in the
column header, not the stub. A header cell like:

    Subject ID
    USUBJID

gets split automatically. The first line becomes the display label and
the second becomes the annotation. The dataset prefix is resolved by
combining:

1.  A universal list of variables that always live in ADSL (`USUBJID`,
    `ARM`, `TRT01A`, etc.).
2.  The source dataset declared in the TLF-level `Source: ADAE` line
    below the table.

This means listing headers do not need a `DATASET.VARIABLE` bracket: the
dataset is inferred from context.

------------------------------------------------------------------------

## Choosing and adding a model

The reader works with any provider in the registry: Anthropic, OpenAI,
Gemini, or an OpenAI-compatible provider such as GLM. For clinical text,
Anthropic has the lowest false-positive content-filter rate. Some
providers block adverse-event terminology outright.

``` r

arsbridge::set_anthropic_key()                 # recommended for clinical text
arsbridge::set_llm_key("glm", "your-glm-key")  # any registry provider
Sys.setenv(ARS_LLM_PROVIDER = "anthropic")     # select the active provider
arsbridge::show_active_llm()
```

Adding a brand-new provider is a single entry in the provider registry
(`R/llm_providers.R`): its key environment variable, default model, the
`ellmer` chat constructor, and a `base_url` if it is OpenAI-compatible.
No other code changes.

------------------------------------------------------------------------

## What the spec gate catches in practice

During extraction on the bundled APX-DRM-301 study, the spec gate
produces a distribution like this:

| Gate result | Typical cause |
|----|----|
| Passed | Variable exists in ADaM spec |
| `WARN` | Regex and LLM disagree on the same row; human review resolves it |
| Blocker | Typo in the shell annotation, or variable added to shell before spec |

Blockers are not errors in the programming sense. They are intentional
stops that name the exact annotation to fix: correct the shell or add
the variable to the ADaM spec, then re-run. The pipeline is designed to
be run iteratively.
