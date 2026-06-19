# Set the API key for any supported LLM provider

Generic key setter driven by the provider registry. Use this for
providers that have no dedicated `set_*_key()` helper (e.g. `"glm"`), or
call the named wrappers ([`set_anthropic_key()`](set_anthropic_key.md),
[`set_openai_key()`](set_openai_key.md),
[`set_gemini_key()`](set_gemini_key.md)) for the common three. Sets the
provider's API-key environment variable for the current session; in an
interactive session you are then asked whether to also persist it to
your `.Renviron`.

## Usage

``` r
set_llm_key(provider, key = NULL, scope = c("user", "project"))
```

## Arguments

- provider:

  Provider id. One of the registry names (currently `"anthropic"`,
  `"openai"`, `"gemini"`, `"glm"`).

- key:

  Character. The API key. If `NULL` (default) and R is running
  interactively, you are prompted.

- scope:

  `"user"` (default) targets your home `.Renviron`; `"project"` targets
  `.Renviron` in the current working directory.

## Value

Invisibly returns the path to the `.Renviron` that would be / was
written.

## Examples

``` r
if (FALSE) { # \dontrun{
# Add a trending OpenAI-compatible provider already in the registry:
set_llm_key("glm", "your-glm-key")
Sys.setenv(ARS_LLM_PROVIDER = "glm")  # make it the active provider
} # }
```
