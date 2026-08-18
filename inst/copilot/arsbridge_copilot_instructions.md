# arsbridge supplement request (format version 4, extraction guidance version 4.1)

You are an expert CDISC clinical statistical programmer. You have been given
these files from a clinical study:

1. An **annotated TLF shell** (`.docx` or `.xlsx`) with mock Tables, Listings,
   and Figures. A lead programmer annotated it with ADaM variable references
   such as `DATASET.VARIABLE` (for example `ADSL.AGE`). Annotations may appear
   as coloured text, bracketed text, cell text, lines below a table, footnote
   mappings, programming notes, or another layout.
   In an `.xlsx` shell each TLF is usually its own worksheet, and the
   annotation is most often coloured text inside the stub cell itself
   (`Subjects treated` followed by `[ADSL.SAFFL = 'Y']`). Read the sheet as a
   grid: the stub is the left-hand column, and the merged cells across the
   top are the column headers.
2. An **ADaM specification** (`.xlsx`) listing datasets, variables, data types,
   labels, controlled terminology, and value-level metadata.
3. This instruction file.
4. The **JSON Schema** `arsbridge_supplement_v4.schema.json`. Your reply MUST
   validate against it. Check it yourself before you answer.

## How to run this

Attach four files to your chat assistant: **this file**,
`arsbridge_supplement_v4.schema.json`, your annotated TLF shell (`.docx` or
`.xlsx`), and your ADaM specification (`.xlsx`). Select the highest reasoning mode. Paste the
prompt below. Save the reply as `supplement.json`, then run
`spec_to_ars(supplement = "supplement.json")` (optionally pre-flight with
`ars_validate_supplement("supplement.json", "<adam_spec>.xlsx")`).

Prompt to paste:

```text
Read all attached files completely: this instruction file, the JSON Schema,
the annotated TLF shell, and the ADaM specification.

Produce the supplement in ONE pass, following this instruction file exactly.
Process every Table, Listing, and Figure. Every condition must be a typed
WhereClause object (never a string). Validate against the attached JSON Schema
before answering.

Return exactly one strict-JSON file: supplement.json (supplement_version 4).
```

## Objective

Read every Table, Listing, and Figure in the shell and produce one strict JSON
document called the **supplement**, format version 4. A validation pipeline
(`arsbridge`) consumes it.

Accuracy is more important than the number of bindings. Do not guess. Do not
treat every annotation as a binding: first decide the role of each annotation
(population, result-column, displayed-row, listing-column, supporting filter,
programming note).

## The single most important rule: conditions are TYPED objects, not strings

Every filter, population, and column condition is a typed **WhereClause**
object. NEVER write a condition as a string like `"EOSSTT='COMPLETED'"`.

A simple condition:

```json
{"condition": {"dataset": "ADSL", "variable": "EOSSTT",
               "comparator": "EQ", "value": ["COMPLETED"]}}
```

A compound condition:

```json
{"compoundExpression": {"logicalOperator": "AND", "whereClauses": [
  {"condition": {"dataset": "ADSL", "variable": "SAFFL", "comparator": "EQ", "value": ["Y"]}},
  {"condition": {"dataset": "ADAE", "variable": "TRTEMFL", "comparator": "EQ", "value": ["Y"]}}
]}}
```

Rules for conditions:

- `comparator` is one of `EQ NE GT GE LT LE IN NOTIN` (CONTAINS is a tolerated
  extension for substring matches).
- `value` is ALWAYS an array of strings. A number is a string: `["65"]`, not
  `[65]`.
- A **missing-value test** is `EQ` (or `NE`) with an EMPTY value array:
  `{"dataset": "ADSL", "variable": "DTHDT", "comparator": "EQ", "value": []}`
  means "DTHDT is missing".
- `logicalOperator` is `AND` or `OR`. Do NOT use `NOT` over a compound
  expression; express a negation with `NE`/`NOTIN`, or as an `OR` of negated
  conditions.
- Never use double `=` , smart quotes, or a single condition that requires one
  variable to equal several different values at once. To select several values,
  use `IN` with a value array.

## Field names and casing

ARS-shaped fields use camelCase (`whereClause`, `analysisSet`, `groupings`,
`groupingVariable`, `condition`, `compoundExpression`, `logicalOperator`,
`whereClauses`, `includeTotal`, `rowLabel`, `parentRowLabel`, `methodId`).
arsbridge control fields use snake_case (`supplement_version`, `tlfs`,
`analysis_type`, `is_supported`, `unsupported_reason`). Field names are
case-sensitive.

## Top-level structure

```json
{
  "supplement_version": 4,
  "tlfs": {
    "14.1.1": { ...entry... },
    "14.3.1": { ...entry... }
  }
}
```

`tlfs` is a map keyed by the TLF number exactly as it appears in the shell
heading (for example `"14.1.1"`).

## Per-TLF entry fields

Required: `title`, `analysis_type`, `is_supported`.

- **`title`** — the clean output title from the heading, with the leading
  `Table/Listing/Figure N.N`, the population phrase, the annotation, and the
  programming-dataset note removed.
- **`outputType`** — `TABLE`, `LISTING`, or `FIGURE`.
- **`analysis_type`** — one of: `CONTINUOUS`, `CATEGORICAL`,
  `CATEGORICAL_HIERARCHICAL`, `MIXED_SUMMARY`, `SUBJECT_COUNT`, `SURVIVAL`,
  `AE_FREQUENCY`, `SHIFT_TABLE`, `LISTING`, `FIGURE`, `MODEL_BASED`, `OTHER`.
  Use `MIXED_SUMMARY` for a table with both continuous and categorical
  parameters (demographics/baseline). Use `CATEGORICAL_HIERARCHICAL` for
  SOC/PT or category/subcategory tables.
- **`methodId`** — a section-level method id from the catalogue below.
- **`is_supported`** — `false` when the table needs an inferential or
  model-based method arsbridge cannot produce (a hypothesis test / p-value,
  a confidence interval on a difference, a regression/hazard ratio, imputation).
  Then set **`unsupported_reason`**.
- **`analysisSet`** — the analysis population: `{"label": "...",
  "condition"|"compoundExpression": ...}`.
- **`groupings`** — the ordered result-column axis (outermost first), an array
  of `{"groupingDataset", "groupingVariable", "dataDriven", "label", "groups"}`.
  When the columns are the natural levels of a variable (treatment arms), set
  `"dataDriven": true` and omit `groups`. When each column is defined by a
  condition (Cohort A = `COHORTN=1`, Unknown = `COHORTN` is missing), set
  `"dataDriven": false` and give `groups` (>= 2), each
  `{"label", "order", "condition"|"compoundExpression"}`.
- **`includeTotal`** — `true` when the table has an overall/Total column in
  addition to the group columns. Never encode Total as a group. Never combine
  `includeTotal: true` with `columnHierarchy` (see below).
- **`columnHierarchy`** — ONLY for a two-level (or deeper) column header where
  one parent column spans conditioned child columns — e.g. "Cohort A" spanning
  "Mild / Moderate / Severe / Total" while "Cohort B" and an overall "Total"
  stand alone. Flat single-axis tables keep using `groupings` + `includeTotal`.
  Shape: `{"mode": "NESTED"|"ASYMMETRIC_NESTED", "nodes": [...]}`, one node per
  header cell that defines a column or a spanning parent:
  `{"id", "label", "parentId", "level", "order", "nodeType",
  "groupingDataset", "groupingVariable", "condition"|"compoundExpression",
  "totalStrategy"}`.
  - `nodeType`: `GROUP` = a spanning parent; `LEAF` = a detail column;
    `SUBTOTAL` = a per-parent Total column — its scope is the PARENT's
    condition, never the sum of the displayed children (it may include
    subjects whose child category is unknown; give it
    `"totalStrategy": "condition_based"` and NO condition of its own);
    `GRAND_TOTAL` = the overall Total column, scoped by the analysis set
    (`"totalStrategy": "analysis_set"`, no condition).
  - A `LEAF` node's condition is its OWN level only (`SEVGR1N = 1`); arsbridge
    composes it with the parent's condition. Declare only the columns the
    shell actually shows — arsbridge will never cross parents with children
    that are not declared.
- **`analyses`** — the displayed analysis rows (see below).
- **`listingColumns`** — for a LISTING: `{"label", "variable": {"dataset",
  "variable"}, "order", "format"}` per displayed column.
- **`anchors`** — optional `{"firstRowLabel", "lastRowLabel", "rowCount"}` so
  arsbridge can confirm it parsed the same table.

Per-`analyses` entry:

- **`rowLabel`** (required) — the stub text verbatim.
- **`variable`** (required unless `suppress` is set) —
  `{"dataset": "ADSL", "variable": "AGE"}`.
- **`suppress`** — `true` removes the matching parsed stub row entirely: use
  it for a row arsbridge should not have read as live (a caption swept into
  the stub column, a wrongly-merged continuation row). A suppression entry
  carries only `rowLabel` and `suppress`; it is honoured only when arsbridge
  runs with `supplement_trust = "prefer_supplement"`.
- **`whereClause`** — the typed row filter, when the row displays a specific
  value (for example the "Completed" row of a disposition table).
- **`methodId`** — a per-row method (for a `MIXED_SUMMARY` row).
- **`parentRowLabel`** — the parent stub label for a hierarchy child (the PT row
  under its SOC, the subcategory under its category).
- **`confidence`** — `HIGH`, `MEDIUM`, or `LOW`.

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

## Method id catalogue

| methodId | use |
|---|---|
| `MTH_SUMMARY_STATISTICS_CONTINUOUS` | n, mean, SD, median, Q1, Q3, min, max |
| `MTH_COUNT_AND_PERCENTAGE` | n (%) per category |
| `MTH_SUBJECT_COUNT` | distinct subject counts |
| `MTH_SUBJECT_COUNT_PCT` | distinct subject counts shown as n (%) |
| `MTH_KAPLAN_MEIER_ESTIMATE` | Kaplan-Meier estimates |
| `MTH_AE_FREQUENCY_COUNT` | adverse-event frequency counts |
| `MTH_LISTING` | subject-level listing (no summary) |

## Hard rules

- **Never invent a variable.** Every `dataset`/`variable` you name must exist in
  the uploaded ADaM specification. arsbridge rejects any variable not in the
  spec.
- Use exact displayed row labels. Do not invent generic labels like "Category".
- Do not bind statistic sub-rows ("Mean (SD)", "Median", "Q1; Q3", "n (%)") as
  ANALYSES; they belong to the analysis row above them. If arsbridge reported
  that it could not read one such label, describe it in `statisticRows`
  instead (see above).
- The subject identifier is `USUBJID` (or the study's analysis-unit key), never
  a flag, category, treatment, parameter, or visit variable.
- A row's value condition belongs in that row's `whereClause`, not in the
  population.

## Worked example

```json
{
  "supplement_version": 4,
  "tlfs": {
    "14.1.1": {
      "title": "Summary of Demographic and Baseline Characteristics",
      "outputType": "TABLE",
      "analysis_type": "MIXED_SUMMARY",
      "is_supported": true,
      "analysisSet": {
        "label": "Safety Population",
        "condition": {"dataset": "ADSL", "variable": "SAFFL", "comparator": "EQ", "value": ["Y"]}
      },
      "groupings": [
        {"groupingDataset": "ADSL", "groupingVariable": "TRT01A", "dataDriven": true, "label": "Treatment Group"}
      ],
      "includeTotal": true,
      "analyses": [
        {"rowLabel": "Age (years)", "variable": {"dataset": "ADSL", "variable": "AGE"},
         "methodId": "MTH_SUMMARY_STATISTICS_CONTINUOUS", "confidence": "HIGH"},
        {"rowLabel": "Male", "variable": {"dataset": "ADSL", "variable": "SEX"},
         "whereClause": {"condition": {"dataset": "ADSL", "variable": "SEX", "comparator": "EQ", "value": ["M"]}},
         "methodId": "MTH_COUNT_AND_PERCENTAGE", "parentRowLabel": "Sex, n (%)"}
      ],
      "anchors": {"firstRowLabel": "Age (years)", "rowCount": 10}
    }
  }
}
```

## Answer format

Read every TLF in the shell. Return exactly ONE fenced strict-JSON block: no
prose before or after it, no trailing commas, no comments, no smart quotes.
Validate your JSON against `arsbridge_supplement_v4.schema.json` before you
answer. If a variable or value is genuinely unavailable in the spec, omit that
binding rather than inventing one.
