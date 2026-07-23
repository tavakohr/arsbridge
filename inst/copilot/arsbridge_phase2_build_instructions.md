# arsbridge Phase 2: Semantic Construction, Repair, and Readiness

## How to run this

Start a NEW chat session and attach four files: **this file**,
`arsbridge_supplement_v3.schema.json`, your annotated TLF shell (`.docx`), and
the `tlf_extraction_blueprints.json` from Phase 1. Select the highest reasoning
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

Return exactly two strict-JSON files: supplement.json (supplement_version 3)
and extraction_validation_report.json.
```

## Document control

- Instruction version: 8.1 (packaged with arsbridge)
- Phase: 2 only
- Input: `tlf_extraction_blueprints.json` (blueprint version 2) + the annotated shell + this file + `arsbridge_supplement_v3.schema.json`
- Outputs: `supplement.json` (format version 3) and `extraction_validation_report.json`

Use the highest available reasoning mode. Do not use a quick-response mode.

## 1. Role and cycles

Act as a senior CDISC statistical programmer, ARS metadata designer, JSON
schema reviewer, and skeptical independent QC programmer. Run two internal
cycles: **2A** construct normalized semantic metadata, **2B** repair incomplete
or incorrect TLFs and run readiness validation. Do not stop after the first
draft.

## 2. The target format (supplement version 3)

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
  "supplement_version": 3,
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
| `includeTotal` | bool | overall/Total column present |
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
`MTH_COUNT_AND_PERCENTAGE`, `MTH_SUBJECT_COUNT`, `MTH_KAPLAN_MEIER_ESTIMATE`,
`MTH_AE_FREQUENCY_COUNT`, `MTH_LISTING`.

## 3. Mapping Phase-1 evidence to v3 fields

| Phase-1 role / component | v3 destination |
|---|---|
| POPULATION | `analysisSet.condition` / `.compoundExpression` |
| RESULT_COLUMN_GROUP, GROUPING_VARIABLE | `groupings[]` (+ `groups[].condition`) |
| TOTAL_COLUMN_RULE | `includeTotal: true` |
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
`arsbridge_supplement_v3.schema.json`: required properties, property names
(case-sensitive), object/array types, allowed enumerations, comparator values,
and additional-property restrictions. Every condition must be a typed object.

## 9. Outputs

Create exactly two files:

- `supplement.json` -- every blueprint TLF exactly once, `supplement_version` 3,
  no duplicate keys, no placeholders, strict JSON, schema-valid.
- `extraction_validation_report.json` -- per TLF: extraction status, support
  status, the semantic state of every required field, not-applicable reasons,
  missing / incorrect-role / raw-evidence-only fields, annotation dispositions,
  logical-filter checks, hierarchy checks, schema status, and review items.

Before delivery: compare inventory counts against the blueprint; reject
duplicates and placeholders; confirm no incomplete TLF is labelled complete;
confirm the report agrees with the supplement; parse both as strict JSON.
Deliver both complete files, not a preview.
