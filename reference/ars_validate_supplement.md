# Validate a Copilot supplement file before running spec_to_ars()

Pre-flight check for the supplement workflow (see
[`ars_copilot_instructions()`](https://tavakohr.github.io/arsbridge/reference/ars_copilot_instructions.md)):
parses the file, checks the format version, every binding's shape and
variable syntax, and the enrichment enums. With `adam_spec_path` it
additionally verifies every proposed variable against the ADaM spec –
the same hard gate
[`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
applies. Findings are printed and returned; messages are worded so they
can be pasted back to the assistant to regenerate the file.

## Usage

``` r
ars_validate_supplement(path, adam_spec_path = NULL)
```

## Arguments

- path:

  Path to the supplement `.json`.

- adam_spec_path:

  Optional path to the ADaM spec (`.xlsx`/`.xml`); enables the spec gate
  check.

## Value

Invisibly, a data frame of findings with columns `severity`
(`FAIL`/`WARN`/`INFO`), `tlf`, `where`, `problem`. Zero rows = clean.

## See also

[`ars_copilot_instructions()`](https://tavakohr.github.io/arsbridge/reference/ars_copilot_instructions.md)
to produce the upload file,
[`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
to consume the validated supplement (`supplement =`). Full walkthrough:
[`vignette("no-api-access")`](https://tavakohr.github.io/arsbridge/articles/no-api-access.md).

## Examples

``` r
if (FALSE) { # \dontrun{
ars_validate_supplement("supplement.json", "adam_spec.xlsx")
} # }
```
