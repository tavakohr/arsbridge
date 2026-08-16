# Roll a fill census up into the three tables a reader actually asks for.

Roll a fill census up into the three tables a reader actually asks for.

## Usage

``` r
ars_fill_summary(census)
```

## Arguments

- census:

  The `census` frame
  [`ars_fill_shell()`](https://tavakohr.github.io/arsbridge/reference/ars_fill_shell.md)
  returns – one row per cell record, filled cells included – or the fill
  result itself, which carries that frame. Both are accepted, because
  both are what a reader has in hand after a fill. `NULL` summarises to
  three empty tables; anything else is an error naming what to pass.

## Value

A list of three data frames:

- `sheets`:

  One row per sheet: cell counts by status.

- `columns`:

  One row per display column: how many of its cells filled, and – when
  some did not – the reason most of them share.

- `reasons`:

  One row per distinct reason across the run, with the author-facing
  hint for it.
