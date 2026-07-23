# Write the Copilot instruction files for the supplement workflow

Environments with no LLM API access can still boost
[`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
accuracy with a chat assistant (GitHub Copilot, ChatGPT, an enterprise
portal): upload the instruction file(s) this function writes TOGETHER
WITH your annotated shell `.docx`, ADaM spec `.xlsx`, and the shipped
JSON Schema, and the assistant replies with one strict `supplement.json`
(format v3). Pass that file to
`spec_to_ars(supplement = "supplement.json")`.

## Usage

``` r
ars_copilot_instructions(
  dir = ".",
  workflow = c("single", "two_phase"),
  open = interactive(),
  overwrite = FALSE
)
```

## Arguments

- dir:

  Directory to write the files into. Default: the current working
  directory.

- workflow:

  `"single"` (default) or `"two_phase"`. See Details.

- open:

  Open the first written file for reading. Default: `TRUE` in
  interactive sessions.

- overwrite:

  Overwrite existing copies. Default `FALSE` (existing copies are
  reported and kept).

## Value

Invisibly, a character vector of the absolute paths written.

## Details

Two workflows are offered:

- `"single"` (default): one instruction file. The assistant reads the
  shell and spec and returns the supplement in one pass. Best for
  small/medium shells.

- `"two_phase"`: two instruction files. Phase 1 produces an evidence
  blueprint (`tlf_extraction_blueprints.json`); Phase 2 turns that
  blueprint into the supplement plus a validation report. Best for large
  or complex shells where a single pass misses items – the phases force
  explicit evidence discovery then semantic construction with review
  cycles.

The files are static and versioned – do not edit them; the format they
request is what
[`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
knows how to validate. The JSON Schema
(`arsbridge_supplement_v3.schema.json`) is written alongside so the
assistant can self-check its reply, and so can
[`ars_validate_supplement()`](https://tavakohr.github.io/arsbridge/reference/ars_validate_supplement.md)
(when `jsonvalidate` is installed).

## Data note

Uploading the shell and spec to a chat assistant transmits their text
(TLF titles, stub labels, variable names – never patient data, which
these documents do not contain) to that provider. Confirm your
organisation's policy first.

## See also

[`ars_validate_supplement()`](https://tavakohr.github.io/arsbridge/reference/ars_validate_supplement.md)
to pre-flight the reply,
[`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
to consume it (`supplement =`), and
[`set_llm_key()`](https://tavakohr.github.io/arsbridge/reference/set_llm_key.md)
if an API key becomes available. Full walkthrough:
[`vignette("no-api-access")`](https://tavakohr.github.io/arsbridge/articles/no-api-access.md).

## Examples

``` r
if (FALSE) { # \dontrun{
ars_copilot_instructions()                       # single-file workflow
ars_copilot_instructions(workflow = "two_phase") # blueprint + build
} # }
```
