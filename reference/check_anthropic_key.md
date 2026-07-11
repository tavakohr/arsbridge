# Check whether the Anthropic API key is set

Reports whether `ANTHROPIC_API_KEY` is visible to the current R session,
without printing the key itself.

## Usage

``` r
check_anthropic_key()
```

## Value

Invisibly returns `TRUE` if the key is set, `FALSE` otherwise.

## See also

No API key available?
[`ars_copilot_instructions()`](https://tavakohr.github.io/arsbridge/reference/ars_copilot_instructions.md)
sets up the no-API supplement workflow – see
[`vignette("no-api-access")`](https://tavakohr.github.io/arsbridge/articles/no-api-access.md).
