# Write a draft supplement from a parsed shell

Parses an annotated TLF shell and writes what it found as a v4
supplement JSON: one entry per TLF with the title, population (as a
typed analysis-set condition), and one analysis per annotated stub row.
The file is a *draft*: review it, correct anything the parser got wrong,
and keep the reviewed file under version control as the study's
extraction manifest. Feed it back with
`spec_to_ars(supplement = ..., supplement_trust = "prefer_supplement")`
– a reviewed value then overrides a wrong parse instead of only filling
gaps.

## Usage

``` r
write_supplement_draft(
  shell_path,
  output_path = "supplement_draft.json",
  adam_spec_path = NULL,
  heading_patterns = NULL
)
```

## Arguments

- shell_path:

  Path to the annotated TLF shell (`.docx` or `.xlsx`).

- output_path:

  Where to write the draft JSON.

- adam_spec_path:

  Optional path to the ADaM specification workbook. When given, a draft
  analysis whose variable is not in the spec is flagged in `reviewItems`
  (the same gate
  [`spec_to_ars()`](https://tavakohr.github.io/arsbridge/reference/spec_to_ars.md)
  enforces later).

- heading_patterns:

  Optional custom heading patterns, as understood by the shell parser.

## Value

Invisibly, the normalized output path. The draft is also validated with
[`ars_validate_supplement()`](https://tavakohr.github.io/arsbridge/reference/ars_validate_supplement.md)
and any findings are shown.

## Details

Rows the parser could not annotate, references outside the ADaM spec,
and statistic sub-rows carrying annotations are not silently dropped:
each is listed in the entry's `provenance$reviewItems` so the reviewer
sees the full roster of open questions per TLF.
