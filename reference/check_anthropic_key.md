# Check whether the Anthropic API key is set

Reports whether `ANTHROPIC_API_KEY` is visible to the current R session,
without printing the key itself. Useful as a quick "am I set up?" check
before calling [`spec_to_ars()`](spec_to_ars.md).

## Usage

``` r
check_anthropic_key()
```

## Value

Invisibly returns `TRUE` if the key is set, `FALSE` otherwise. Prints a
status message either way.

## Examples

``` r
if (FALSE) { # \dontrun{
check_anthropic_key()
} # }
```
