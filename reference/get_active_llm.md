# Get the active LLM provider and configurations

Inspects the environment variables and system options to determine the
active LLM provider (Anthropic, OpenAI, or Gemini) and their
corresponding keys.

## Usage

``` r
get_active_llm()
```

## Value

A list containing:

- `provider`:

  The name of the active provider (`"anthropic"`, `"openai"`, or
  `"gemini"`), or `NULL` if none are active.

- `model`:

  The default model name for the active provider.

- `keys_set`:

  A character vector listing the providers that currently have keys set.

- `active_key_masked`:

  A masked version of the active API key (showing only first 7 and last
  3 characters), or `NULL`.

## Details

If multiple API keys are set, it prioritizes the provider specified in
the `ARS_LLM_PROVIDER` environment variable or global option. If that is
not set, it defaults to the first available key in the order of:
Anthropic, OpenAI, Gemini.

## See also

No API key available?
[`ars_copilot_instructions()`](https://tavakohr.github.io/arsbridge/reference/ars_copilot_instructions.md)
sets up the no-API supplement workflow – see
[`vignette("no-api-access")`](https://tavakohr.github.io/arsbridge/articles/no-api-access.md).

## Examples

``` r
get_active_llm()
#> $provider
#> NULL
#> 
#> $model
#> NULL
#> 
#> $keys_set
#> character(0)
#> 
#> $active_key_masked
#> NULL
#> 
```
