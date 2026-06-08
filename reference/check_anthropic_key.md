# Check whether the Anthropic API key is set

Reports whether `ANTHROPIC_API_KEY` is visible to the current R session,
without printing the key itself.

## Usage

``` r
check_anthropic_key()
```

## Value

Invisibly returns `TRUE` if the key is set, `FALSE` otherwise.
