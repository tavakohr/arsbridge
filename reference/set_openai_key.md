# Set your OpenAI API key for arsbridge

Sets `OPENAI_API_KEY` for the current R session. In an interactive
session you are then asked whether to also persist it to your
`.Renviron`; no file is written without that confirmation.

## Usage

``` r
set_openai_key(key = NULL, scope = c("user", "project"))
```

## Arguments

- key:

  Character. Your OpenAI API key (starts with `"sk-"`). If `NULL`
  (default) and R is running interactively, prompts you.

- scope:

  `"user"` (default) targets your home `.Renviron`; `"project"` targets
  `.Renviron` in the current working directory. Only used if you confirm
  the (optional) persistence step.

## Value

Invisibly returns the path to the `.Renviron` that would be / was
written.

## See also

No API key available?
[`ars_copilot_instructions()`](https://tavakohr.github.io/arsbridge/reference/ars_copilot_instructions.md)
sets up the no-API supplement workflow – see
[`vignette("no-api-access")`](https://tavakohr.github.io/arsbridge/articles/no-api-access.md).
