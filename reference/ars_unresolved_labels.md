# The statistic rows arsbridge refused to bind

A shell row whose label names statistics is filled from the block above
it, and which statistics it gets is decided by what the label says. When
that cannot be established, arsbridge binds *nothing* on the row rather
than binding whichever statistic the method happens to list first – a
part-bound row shifts its remaining placeholders onto the wrong
statistics, which writes a real number of the wrong thing. The cells
stay on their placeholders and the build stage logs a WARN.

## Usage

``` r
ars_unresolved_labels(ars)
```

## Arguments

- ars:

  The ARS to read, in any of the three shapes a caller has one in: the
  path to an ARS `.json`, the run result
  [`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
  returns (the reporting event is taken from its `reporting_event`
  element), or a reporting event itself. Anything else is an error
  rather than an empty queue – "nothing is unresolved" and "I was handed
  the wrong object" must not look the same, because the first is the
  answer a caller acts on.

## Value

A data frame, one row per refused shell row, with columns:

- `output_id`:

  The ARS output the row belongs to.

- `tlf`:

  That output's TLF number, as the shell numbered it.

- `sheet`:

  The worksheet the row is on.

- `row`:

  The sheet row number.

- `label`:

  The row label, exactly as authored.

- `analysis_id`:

  The analysis the row would have been filled from – its block's parent,
  or its own when it has one.

- `method_id`:

  That analysis's ARS method.

- `reason`:

  `"unreadable"` or `"unsupported"`, as above.

- `tokens`:

  List column: the semantic statistics the label asked for, in reading
  order. Empty when `reason` is `"unreadable"`.

- `unsupported`:

  List column: the subset of `tokens` the method does not declare. Empty
  when `reason` is `"unreadable"`.

- `available`:

  List column: the operations the method DOES declare – what the row
  could have asked for instead.

Zero rows means every statistic row bound. The columns are present
either way, so a caller can bind or filter the result without a special
case.

Only a shell built from an `.xlsx` carries a cell map, so a reporting
event built from a Word shell always returns zero rows.

## Details

This function returns those same rows as a data frame, so they can be
worked as a queue instead of read as prose.

There are two reasons, and they lead to different work:

- `"unreadable"`:

  The label could not be read as a statistic request at all – an
  unfamiliar spelling, or a row that is not a statistic row. `tokens` is
  empty. Somebody has to say what the row means.

- `"unsupported"`:

  The label was read correctly, and names a statistic the row's method
  does not declare – a standard-error line over a method that produces
  no standard error, say. `tokens` holds what the label asked for and
  `unsupported` holds the part the method cannot supply. No synonym
  fixes this: either the row's method is wrong for it, or the statistic
  is beyond the engine.

## See also

[`ars_manual_worklist()`](https://tavakohr.github.io/arsbridge/reference/ars_manual_worklist.md)
for cells reserved because their METHOD is beyond the engine – a
different queue: those rows are understood and waiting on a derivation,
these are not yet understood.

## Examples

``` r
if (FALSE) { # \dontrun{
built <- spec_to_ars("shell.xlsx", "adam_spec.xlsx")
todo  <- ars_unresolved_labels(built)          # or built$ars_path
todo[todo$reason == "unreadable", c("tlf", "row", "label")]
} # }
```
