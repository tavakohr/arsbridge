# arsbridge supplement request (format version 3, extraction guidance version 4.1)

You are an expert CDISC clinical statistical programmer. You have been given
these files from a clinical study:

1. An **annotated TLF shell** (`.docx`) with mock Tables, Listings, and Figures.
   A lead programmer annotated it with ADaM variable references such as
   `DATASET.VARIABLE` (for example `ADSL.AGE`). Annotations may appear as
   coloured text, bracketed text, cell text, lines below a table, footnote
   mappings, programming notes, or another layout.
2. An **ADaM specification** (`.xlsx`) listing datasets, variables, data types,
   labels, controlled terminology, and value-level metadata.
3. This instruction file.
4. The **JSON Schema** `arsbridge_supplement_v3.schema.json`. Your reply MUST
   validate against it. Check it yourself before you answer.

## How to run this

Attach four files to your chat assistant: **this file**,
`arsbridge_supplement_v3.schema.json`, your annotated TLF shell (`.docx`), and
your ADaM specification (`.xlsx`). Select the highest reasoning mode. Paste the
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

Return exactly one strict-JSON file: supplement.json (supplement_version 3).
```

## Objective

Read every Table, Listing, and Figure in the shell and produce one strict JSON
document called the **supplement**, format version 3. A validation pipeline
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
  "supplement_version": 3,
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
  addition to the group columns. Never encode Total as a group.
- **`analyses`** — the displayed analysis rows (see below).
- **`listingColumns`** — for a LISTING: `{"label", "variable": {"dataset",
  "variable"}, "order", "format"}` per displayed column.
- **`anchors`** — optional `{"firstRowLabel", "lastRowLabel", "rowCount"}` so
  arsbridge can confirm it parsed the same table.

Per-`analyses` entry:

- **`rowLabel`** (required) — the stub text verbatim.
- **`variable`** (required) — `{"dataset": "ADSL", "variable": "AGE"}`.
- **`whereClause`** — the typed row filter, when the row displays a specific
  value (for example the "Completed" row of a disposition table).
- **`methodId`** — a per-row method (for a `MIXED_SUMMARY` row).
- **`parentRowLabel`** — the parent stub label for a hierarchy child (the PT row
  under its SOC, the subcategory under its category).
- **`confidence`** — `HIGH`, `MEDIUM`, or `LOW`.

## Method id catalogue

| methodId | use |
|---|---|
| `MTH_SUMMARY_STATISTICS_CONTINUOUS` | n, mean, SD, median, Q1, Q3, min, max |
| `MTH_COUNT_AND_PERCENTAGE` | n (%) per category |
| `MTH_SUBJECT_COUNT` | distinct subject counts |
| `MTH_KAPLAN_MEIER_ESTIMATE` | Kaplan-Meier estimates |
| `MTH_AE_FREQUENCY_COUNT` | adverse-event frequency counts |
| `MTH_LISTING` | subject-level listing (no summary) |

## Hard rules

- **Never invent a variable.** Every `dataset`/`variable` you name must exist in
  the uploaded ADaM specification. arsbridge rejects any variable not in the
  spec.
- Use exact displayed row labels. Do not invent generic labels like "Category".
- Do not bind statistic sub-rows ("Mean (SD)", "Median", "Q1; Q3", "n (%)");
  they belong to the analysis row above them.
- The subject identifier is `USUBJID` (or the study's analysis-unit key), never
  a flag, category, treatment, parameter, or visit variable.
- A row's value condition belongs in that row's `whereClause`, not in the
  population.

## Worked example

```json
{
  "supplement_version": 3,
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
Validate your JSON against `arsbridge_supplement_v3.schema.json` before you
answer. If a variable or value is genuinely unavailable in the spec, omit that
binding rather than inventing one.
