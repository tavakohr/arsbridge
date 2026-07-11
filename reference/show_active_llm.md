# Show the active LLM provider and API key status

Prints a summary of the active LLM provider, the model that will be
used, and the set/missing status of API keys for Anthropic, OpenAI, and
Gemini.

## Usage

``` r
show_active_llm()
```

## Value

Invisibly returns the active provider name (character), or `NULL`.

## See also

No API key available?
[`ars_copilot_instructions()`](https://tavakohr.github.io/arsbridge/reference/ars_copilot_instructions.md)
sets up the no-API supplement workflow – see
[`vignette("no-api-access")`](https://tavakohr.github.io/arsbridge/articles/no-api-access.md).

## Examples

``` r
show_active_llm()
#> 
#> ── LLM Configuration Status ──
#> 
#> ℹ ANTHROPIC: {.danger NOT SET}
#> ℹ OPENAI: {.danger NOT SET}
#> ℹ GEMINI: {.danger NOT SET}
#> ℹ GLM: {.danger NOT SET}
#> ✖ No active LLM provider found. Set an API key to get started.
```
