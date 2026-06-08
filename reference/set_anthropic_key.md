# Set your Anthropic API key for arsbridge

Writes `ANTHROPIC_API_KEY=...` to your user `.Renviron` file so it loads
automatically every time you start R, AND sets it in the current session
so you can call [`spec_to_ars()`](spec_to_ars.md) immediately.

## Usage

``` r
set_anthropic_key(key = NULL, scope = c("user", "project"))
```

## Arguments

- key:

  Character. Your Anthropic API key (starts with `"sk-ant-"`). If `NULL`
  (default) and R is running interactively, prompts you.

- scope:

  `"user"` (default) writes to your home `.Renviron`. `"project"` writes
  to `.Renviron` in the current working directory.

## Value

Invisibly returns the path to the `.Renviron` file that was updated.
