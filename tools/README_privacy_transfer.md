# Debugging a shell you are not allowed to share

Some shells cannot leave the machine they live on. This folder holds a way to
get **the information arsbridge debugging actually needs** off that machine
without moving any study content.

The insight: diagnosing a parsing failure needs the document's *shape* — how
many header rows, which cells are merged, whether a wrapped annotation is two
paragraphs or one, whether a row is struck through. None of that requires a
single word of the study.

## What to run on the restricted PC

Copy **`shell_structure_digest.R`** across (it is one plain-text R file; open
it and read it first). It needs only `xml2` and `jsonlite` — arsbridge itself
does **not** have to be installed.

```r
source("shell_structure_digest.R")
digest_shell("YourStudy_Shells.docx", "digest.json")
```

Then **open `digest.json` and read it** before it goes anywhere. It is plain
text. If anything in it looks like study content, stop and tell me — that is a
bug in the tool, not something to work around.

## What the digest contains

For every table, row and cell:

| Captured | Why it matters |
|---|---|
| `grid_span`, `vmerge` | settles whether the cohort spanners are real merged cells |
| `n_paragraphs` per cell | a label split across paragraphs vs one that merely wraps |
| `breaks` per run | a soft line break inside a run — invisible in a screenshot |
| `strike` / `dstrike` | whether a removed row uses `w:strike` or `w:dstrike` |
| `color`, `highlight`, `bold`, `italic`, `underline` | which layer of annotation detection should fire |
| `repeat_as_header` | whether header rows carry `<w:tblHeader/>` |
| `silhouette` | character classes only: `SOC#1` → `AAA#9`, `<Preferred Term>` → `<Aaaaaaaaa Aaaa>` |
| `skeleton` | annotation grammar with names tokenised: `[V1=<VAL>]`, `[V2 NE <VAL> AND V3=<VAL>]` |
| `conventions` | booleans: `condition_wrapper`, `nested_bracket`, `count_instruction`, … |

## What it cannot contain

Every alphabetic word is destroyed by construction:

- text is replaced by character-class silhouettes (`a`, `A`, `9`);
- quoted values become `<VAL>` before anything else runs;
- `DATASET.VARIABLE` references become stable `V1`, `V2`, … tokens (the same
  variable keeps the same token across the document, so relationships stay
  visible while names do not);
- only a fixed list of **operators** survives verbatim (`WHERE`, `AND`, `OR`,
  `IN`, `NE`, `EQ`, `is.na`, …) and only **inside brackets**, so a label word
  that happens to spell an operator ("Not coded", "In progress") is still
  erased.

The word list is a whitelist, not a blacklist: anything unforeseen is erased
rather than kept.

## Second channel: redacted diagnostics

arsbridge's own messages are safe text, but they quote row labels back at you.
On the restricted PC, after a run:

```r
source("shell_structure_digest.R")
d <- ars_diagnostics()
write.csv(redact_diagnostics(d), "diagnostics_redacted.csv", row.names = FALSE)
```

That blanks anything inside quotes and silhouettes the `location` column,
keeping `stage`, `severity`, and the message shape — which is what identifies
*which* code path fired.

## Third channel: screenshots

Still useful, and now complementary rather than primary. A photograph shows
intent (what the table is meant to look like); the digest shows mechanism
(what the file actually contains). Where they disagree, the digest wins —
a photo cannot distinguish a straight quote from a curly one, or a line wrap
from a paragraph break.

## What to send

`digest.json` plus `diagnostics_redacted.csv` is enough to work from. Both are
small (tens of KB) and human-readable.

## Judging arsbridge's PERFORMANCE, not just the input

The shell digest says what the document contains. To see what arsbridge made
of it, digest the reporting event the run produced:

```r
digest_reporting_event("ars/reporting_event.json", "ars_digest.json")
digest_ars_summary("ars_digest.json")
```

Almost everything that matters here is arsbridge's own vocabulary and carries
no study content: layout kinds (`categorical`, `nested_parent`, `level`,
`supplement_added`), method ids, column-tree mode, grouping and subset counts,
extraction mode and supplement trust. Labels are silhouetted like everywhere
else.

That summary answers the open questions directly:

| Question | What to look for |
|---|---|
| Did the cohort columns stay flat? | `column_tree=` per output — `NESTED` / `ASYMMETRIC_NESTED` vs `none (flat)` |
| Did the SOC/PT hierarchy form? | `nested_parent` / `nested_child` in the kind counts |
| Are mock rows still rendering? | token-shaped `label_shape` values with their own analysis |
| Are supplement rows duplicating? | `supplement_added` counts and "repeated label shapes" |
| Did struck rows get dropped? | row count vs the shell digest's row count |

## The complete package to send back

1. `digest.json` (or just the `digest_summary()` output) — the shell's shape
2. `ars_digest.json` (or `digest_ars_summary()` output) — what arsbridge built
3. `diagnostics_redacted.csv` — which code paths fired, and what they said

Read all three before they leave the machine.
