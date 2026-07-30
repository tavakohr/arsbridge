# Write a privacy-safe digest of the parser's decisions for a shell

Parses `shell_path` and writes a JSON describing, per TLF, what the
parser decided: header rows flagged vs inferred, physical column count
vs resulting column-label count, the column labels and stub-row labels
as character-class silhouettes, the column-tree shape, and per-severity
diagnostic counts. The output contains no literal document text, so –
after you read it yourself – it can leave a machine the shell itself
cannot. Diff it against the raw-geometry digest from
`tools/shell_structure_digest.R` to localize a parsing divergence.

## Usage

``` r
parse_decision_digest(
  shell_path,
  out_json = "parse_decision_digest.json",
  heading_patterns = NULL
)
```

## Arguments

- shell_path:

  Path to the annotated TLF shell (`.docx` or `.xlsx`).

- out_json:

  Where to write the digest JSON.

- heading_patterns:

  Optional custom heading patterns, as understood by the shell parser.

## Value

Invisibly, the digest list (also written to `out_json`).
