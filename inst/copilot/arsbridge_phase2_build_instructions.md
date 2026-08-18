# arsbridge Phase 2: Semantic Construction, Repair, and Readiness

## How to run this

Start a NEW chat session and attach four files: **this file**,
`arsbridge_supplement_v4.schema.json`, your annotated TLF shell (`.docx` or
`.xlsx`), and the `tlf_extraction_blueprints.json` from Phase 1. Select the highest reasoning
mode. Paste the prompt below. Save the two replies as `supplement.json` and
`extraction_validation_report.json`, then run
`spec_to_ars(supplement = "supplement.json")` (optionally pre-flight with
`ars_validate_supplement("supplement.json", "<adam_spec>.xlsx")`).

Prompt to paste:

```text
Read all four attached files completely: this Phase 2 instruction file, the
JSON Schema, the annotated TLF shell, and tlf_extraction_blueprints.json.

Perform Phase 2 ONLY, following this instruction file exactly. Do not repeat
Phase 1. Run both internal cycles (2A construct, 2B repair) and all mandatory
reviews. Every condition must be a typed WhereClause object (never a string).
Validate the result against the attached JSON Schema before answering.

Return exactly two strict-JSON files: supplement.json (supplement_version 4)
and extraction_validation_report.json.
```

## Document control

- Instruction version: 8.1 (packaged with arsbridge)
- Phase: 2 only
- Input: `tlf_extraction_blueprints.json` (blueprint version 2) + the annotated shell + this file + `arsbridge_supplement_v4.schema.json`
- Outputs: `supplement.json` (format version 4) and `extraction_validation_report.json`

Use the highest available reasoning mode. Do not use a quick-response mode.

## 1. Role and cycles

Act as a senior CDISC statistical programmer, ARS metadata designer, JSON
schema reviewer, and skeptical independent QC programmer. Run two internal
cycles: **2A** construct normalized semantic metadata, **2B** repair incomplete
or incorrect TLFs and run readiness validation. Do not stop after the first
draft.

## 2. The target format (supplement version 4)

The supplement is a strict JSON document. **Every condition is a typed
WhereClause object, never a string.**

```json
{"condition": {"dataset": "ADSL", "variable": "SAFFL", "comparator": "EQ", "value": ["Y"]}}
{"compoundExpression": {"logicalOperator": "AND", "whereClauses": [ <WhereClause>, ... ]}}
```

- `comparator`: `EQ NE GT GE LT LE IN NOTIN` (CONTAINS is a tolerated
  substring extension).
- `value`: always an array of strings (a number is `["65"]`). A missing-value
  test is `EQ`/`NE` with an empty array `[]`.
- `logicalOperator`: `AND` or `OR`. Never `NOT` over a compound -- use
  `NE`/`NOTIN` or an `OR` of negations.

Top level:

```json
{
  "supplement_version": 4,
  "validation_report": "extraction_validation_report.json",
  "generator": {"workflow": "two_phase", "instruction_version": "8.0"},
  "tlfs": { "<TLF number>": { ...entry... } }
}
```

Per-TLF entry (required: `title`, `analysis_type`, `is_supported`):

| field | type | notes |
|---|---|---|
| `title` | string | clean output title |
| `outputType` | enum | `TABLE`/`LISTING`/`FIGURE` |
| `analysis_type` | enum | `CONTINUOUS`, `CATEGORICAL`, `CATEGORICAL_HIERARCHICAL`, `MIXED_SUMMARY`, `SUBJECT_COUNT`, `SURVIVAL`, `AE_FREQUENCY`, `SHIFT_TABLE`, `LISTING`, `FIGURE`, `MODEL_BASED`, `OTHER` |
| `methodId` | enum | section method (catalogue below) |
| `is_supported` / `unsupported_reason` | bool / string | `false` for inferential/model-based methods |
| `analysisSet` | object | `{label, condition|compoundExpression}` -- the population |
| `groupings` | array | ordered result-column axis; each `{groupingDataset, groupingVariable, dataDriven, label, groups}` |
| `includeTotal` | bool | overall/Total column present (never together with `columnHierarchy`) |
| `columnHierarchy` | object | ONLY for a multi-level column header (a parent column spanning conditioned child columns): `{mode: NESTED|ASYMMETRIC_NESTED, nodes[]}` -- one node per column/parent, `{id, label, parentId, level, order, nodeType GROUP|LEAF|SUBTOTAL|GRAND_TOTAL, groupingDataset, groupingVariable, condition|compoundExpression, totalStrategy}`. A SUBTOTAL is scoped by its PARENT's condition (`totalStrategy: condition_based`, no own condition); a GRAND_TOTAL by the analysis set (`totalStrategy: analysis_set`). Declare only the columns the shell shows |
| `analyses` | array | displayed rows (below) |
| `listingColumns` | array | `{label, variable{dataset,variable}, order, format}` |
| `recordFilter` | WhereClause | report-wide record filter |
| `sorting` | array | `{dataset, variable, direction, order}` |
| `anchors` | object | `{firstRowLabel, lastRowLabel, rowCount}` |
| `provenance` | object | `{blueprintStatus, reviewItems}` |

Per-`analyses` entry (required: `rowLabel`, `variable`):

`rowLabel` (stub verbatim), `variable` (`{dataset, variable}`), `whereClause`
(typed row filter), `methodId` (per-row method for MIXED_SUMMARY),
`parentRowLabel` (hierarchy parent), `order`, `denominator`
(`{scope, whereClause}`), `evidenceIds`, `confidence` (`HIGH`/`MEDIUM`/`LOW`).

Group entry (in `groupings[].groups`): `{label, order, condition|compoundExpression}`.

Method id catalogue: `MTH_SUMMARY_STATISTICS_CONTINUOUS`,
`MTH_COUNT_AND_PERCENTAGE`, `MTH_SUBJECT_COUNT`, `MTH_SUBJECT_COUNT_PCT`,
`MTH_KAPLAN_MEIER_ESTIMATE`, `MTH_AE_FREQUENCY_COUNT`, `MTH_LISTING`.

## 3. Mapping Phase-1 evidence to v4 fields

| Phase-1 role / component | v4 destination |
|---|---|
| POPULATION | `analysisSet.condition` / `.compoundExpression` |
| RESULT_COLUMN_GROUP, GROUPING_VARIABLE | `groupings[]` (+ `groups[].condition`) |
| TOTAL_COLUMN_RULE | `includeTotal: true` (flat axis) or a `SUBTOTAL`/`GRAND_TOTAL` node in `columnHierarchy` (multi-level header) |
| DISPLAYED_ROW_VARIABLE, PARAMETER_RULE | `analyses[].variable` |
| ROW_FILTER | `analyses[].whereClause` |
| STATISTIC / method | `methodId` (section) or `analyses[].methodId` (per row) |
| parent/child hierarchy | `analyses[].parentRowLabel` |
| DISPLAYED_LISTING_COLUMN | `listingColumns[]` |
| RECORD_FILTER | `recordFilter` |
| SORT_RULE | `sorting[]` |
| DENOMINATOR_RULE | `analyses[].denominator` |
| provenance / review | `provenance`, `analyses[].evidenceIds` |

### Fields arsbridge records but does not yet compute

`recordFilter`, `sorting`, `denominator`, `parentRowLabel`, and `provenance`
are accepted, stored, and reported (carried into the output `_meta`), but not
yet used in computation. Populate them for completeness and review; do NOT mark
a TLF incomplete because one of these is the only outstanding item.

## 4. Construction (2A)

For each TLF: load its blueprint and evidence, reinspect its shell section,
correct any wrong Phase-1 classification, choose the analysis-specific
structure, and convert evidence into typed metadata. Separate population,
record, section, row, and column conditions. Use exact displayed labels. Never
substitute raw evidence text, generic labels, or unresolved structures for
final metadata.

- **MIXED_SUMMARY**: one `analyses` entry per displayed parameter, each with its
  own `variable`, `whereClause` (if a value row), and `methodId`.
- **CATEGORICAL_HIERARCHICAL**: every child names its `parentRowLabel`; keep the
  full hierarchy in title order (`by SOC and PT` -> SOC then PT).
- **Column axis**: natural variable levels -> one grouping with
  `dataDriven: true`. Condition-defined columns (Cohort A/B/Unknown) ->
  `dataDriven: false` with `groups` (>= 2 typed conditions).

## 5. Logical filter validation

Test whether each filter can return records. Never require one variable to
equal several different values on the same record (`X = A AND X = B`). Use `IN`
for one combined set, separate `analyses[].whereClause` filters for separately
displayed rows, or separate groups for separate columns.

## 6. Repair (2B)

For every TLF not `COMPLETE`: read each missing / incorrect-role /
raw-evidence-only / failed-check / unresolved item; return to the shell section
and blueprint; resolve from direct evidence, validated metadata, cross-reference
inheritance, or deterministic ADaM relationships; decide genuine
`NOT_APPLICABLE`; update the JSON; repeat the reviews. A TLF stays incomplete
only when required evidence is absent, contradictory, unreadable, or
unvalidated. Never invent information to reach completion.

## 7. Mandatory reviews (every TLF)

1. Shell-to-JSON coverage: every population, header, displayed row, variable,
   filter, statistic, footnote, sort, and no-data instruction maps to a JSON
   path.
2. JSON-to-shell validity: every claim checks against the shell and validated
   metadata.
3. Logical consistency: filter satisfiability, hierarchy, denominator scope,
   Total behaviour, analysis-type consistency.
4. Independent critic: what is missing, generic, wrong-role, impossible, or
   flattened? Could arsbridge compute the output from this JSON? Would two
   programmers read it the same way?

## 8. Schema gate

Validate the supplement against the uploaded
`arsbridge_supplement_v4.schema.json`: required properties, property names
(case-sensitive), object/array types, allowed enumerations, comparator values,
and additional-property restrictions. Every condition must be a typed object.

## 9. Outputs

Create exactly two files:

- `supplement.json` -- every blueprint TLF exactly once, `supplement_version` 4,
  no duplicate keys, no placeholders, strict JSON, schema-valid.
- `extraction_validation_report.json` -- per TLF: extraction status, support
  status, the semantic state of every required field, not-applicable reasons,
  missing / incorrect-role / raw-evidence-only fields, annotation dispositions,
  logical-filter checks, hierarchy checks, schema status, and review items.

When a review item or provenance note must quote the shell's own annotation
text, never paste it character-for-character if it contains a quote mark --
either escape every `"` as `\"`, or paraphrase instead of quoting. A raw
quote character copied into a JSON string ends that string early and breaks
the entire file's JSON syntax.

Before delivery: compare inventory counts against the blueprint; reject
duplicates and placeholders; confirm no incomplete TLF is labelled complete;
confirm the report agrees with the supplement; parse both as strict JSON.
Deliver both complete files, not a preview.

## Statistic rows (optional): `statisticRows`

Some rows name STATISTICS rather than a variable -- "Mean (SD)", "Median",
"Q1; Q3". arsbridge reads those labels itself and usually needs no help. When
it cannot read one, it fills nothing on that row and reports it, because
guessing would write a real number of the wrong statistic.

`statisticRows` is how you answer that report. Include an entry ONLY for a row
whose label arsbridge could not read; there is no value in restating labels it
already understands.

```json
"statisticRows": [
  {
    "row_label": "Adjusted mean (95% CI)",
    "semantic_tokens": ["mean", "ci_low", "ci_high"],
    "status": "proposal",
    "source": "llm",
    "confidence": 0.94,
    "evidence": "row 12, under \"LS mean change [ADQX.CHG]\""
  }
]
```

- **`row_label`** — the stub text exactly as the shell writes it. Rows are
  matched by label, never by position.
- **`semantic_tokens`** — the statistics the row shows, **in the order it shows
  them**, from exactly this list: `count`, `pct`, `mean`, `sd`, `se`, `median`,
  `q1`, `q3`, `min`, `max`, `cv`, `geomean`, `ci_low`, `ci_high`, `events`,
  `pvalue`. Repeat a token if the row shows that statistic twice.
- **`status`** — **write `"proposal"`.** Only a person may write `"reviewed"`,
  and only a `"reviewed"` row is ever applied. A proposal is recorded and
  reported so a human can check it; it changes no output.
- **`source`** — `"llm"` if you produced it, `"supplement"` if a person did.
- **`reviewed_by`** — required when `status` is `"reviewed"`. Do not write this.
- **`override`** — for a reviewed row only, and only when arsbridge's own
  reading of the label disagrees and the reviewer wants theirs used instead.

**Never name an ARS operation (`OP_MEAN`) or an engine statistic name (`N`,
`p25`, `conf.low`) in `semantic_tokens`.** Name the statistic; arsbridge
chooses the operation from the row's method. A row asking for a statistic the
method does not produce is refused whole and reported -- being reviewed makes a
request legible, not possible.
