# Set (or update) your Anthropic API key for arsbridge

Writes `ANTHROPIC_API_KEY=...` to your user `.Renviron` file so it loads
automatically every time you start R, AND sets it in the current session
so you can call [`spec_to_ars()`](spec_to_ars.md) immediately – no
restart required.

## Usage

``` r
set_anthropic_key(key = NULL, scope = c("user", "project"))
```

## Arguments

- key:

  Character. Your Anthropic API key (starts with `"sk-ant-"`). If `NULL`
  (default) and R is running interactively, prompts you to paste the
  key.

- scope:

  `"user"` (default) writes to your home `.Renviron` (recommended – one
  key shared across all your R projects). `"project"` writes to
  `.Renviron` in the current working directory (useful when
  collaborating on a shared project where each contributor has their own
  key).

## Value

Invisibly returns the path to the `.Renviron` file that was updated.

## Details

Get a key at <https://console.anthropic.com/settings/keys>.

## Examples

``` r
if (FALSE) { # \dontrun{
# Interactive prompt (recommended -- key is not echoed to the screen
# when the 'askpass' package is installed)
set_anthropic_key()

# Or paste it directly
set_anthropic_key("sk-ant-api03-...")

# Project-scoped key (writes to ./.Renviron, not your home one)
set_anthropic_key("sk-ant-api03-...", scope = "project")
} # }
```
