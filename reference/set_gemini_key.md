# Set your Gemini API key for arsbridge

Writes `GEMINI_API_KEY=...` to your user `.Renviron` file so it loads
automatically every time you start R, AND sets it in the current
session.

## Usage

``` r
set_gemini_key(key = NULL, scope = c("user", "project"))
```

## Arguments

- key:

  Character. Your Gemini API key. If `NULL` (default) and R is running
  interactively, prompts you.

- scope:

  `"user"` (default) writes to your home `.Renviron`. `"project"` writes
  to `.Renviron` in the current working directory.

## Value

Invisibly returns the path to the `.Renviron` file that was updated.
