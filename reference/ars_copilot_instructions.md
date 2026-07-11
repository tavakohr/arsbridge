# Write the Copilot instruction file for the supplement workflow

Environments with no LLM API access can still boost
[`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
accuracy with a chat assistant (GitHub Copilot, ChatGPT, an enterprise
portal): upload the instruction file this function writes TOGETHER WITH
your annotated shell `.docx` and ADaM spec `.xlsx`, and the assistant
replies with one standard `supplement.json`. Pass that file to
`spec_to_ars(supplement = "supplement.json")`.

## Usage

``` r
ars_copilot_instructions(dir = ".", open = interactive(), overwrite = FALSE)
```

## Arguments

- dir:

  Directory to write the file into. Default: the current working
  directory.

- open:

  Open the file for reading after writing it (so you can see what the
  assistant will be told). Default: `TRUE` in interactive sessions.

- overwrite:

  Overwrite an existing copy. Default `FALSE` (the existing copy is
  reported and kept).

## Value

Invisibly, the absolute path of the instruction file.

## Details

The instruction file is static and versioned – do not edit it; the
format it requests is what
[`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
knows how to validate.

## Where the file comes from

The instruction file ships *inside* the installed package at
`inst/copilot/arsbridge_copilot_instructions.md`. This function resolves
it with `system.file("copilot", ...)` and copies it into `dir`, so you
never need to know the internal package path. (Under
`devtools::load_all()` it falls back to the source tree's
`inst/copilot/`.)

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
ars_copilot_instructions()   # writes ./arsbridge_copilot_instructions.md
} # }
```
