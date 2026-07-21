# arsbridge supplement request (format version 2, extraction guidance version 3)

You are an expert CDISC clinical statistical programmer. You have been given two files from a clinical study:

1. An **annotated TLF shell** (`.docx`) containing mock Tables, Listings, and Figures. A lead programmer has annotated it with ADaM variable references such as `DATASET.VARIABLE`, for example `ADSL.AGE`. Annotations may appear as colored text, bracketed text, text inside cells, lines below a table, footnote mappings, programming notes, or another layout.
2. An **ADaM specification** (`.xlsx`) listing the datasets, variables, data types, labels, controlled terminology, and value-level metadata that exist in the study.

## Objective

Read every Table, Listing, and Figure section in the shell and create one strict JSON document called the **supplement**. A validation pipeline will consume this file.

The supplement must accurately describe:

- the complete TLF inventory,
- the clean report title,
- displayed row or listing-column bindings,
- result-column structure,
- analysis population,
- analysis type and closest arsbridge method,
- grouping variables,
- Total-column status,
- support status for the requested analysis.

Accuracy is more important than the number of bindings. Do not guess. Do not treat every annotation as a binding. First determine the role of each annotation.

## Required JSON fields and meanings

For each TLF report, use the following fields when they can be identified.

### `title`

Use the output title exactly as written in the shell heading, with these parts removed:

- the leading `Table N.N`, `Listing N.N`, or `Figure N.N`,
- the analysis-population phrase,
- the annotation text,
- the programming-dataset note.

Example heading:

`Table 12.1.1 Summary of Subject Disposition - Analysis Population ADSL.ANALFL='Y' [PROGRAMMING DATASETS USED: ADSL]`

Correct title:

`Summary of Subject Disposition`

Do not remove meaningful title words. Use the Table of Contents and the report heading together to resolve obvious heading-layout problems.

### `bindings`

A binding maps a **displayed analysis row label** or a **displayed listing column label** to the ADaM variable that supplies the displayed or counted value.

Example:

```json
{"label": "Age at Enrollment (Years)", "variable": "ADSL.AAGE1"}
```

For a value-specific row, add a `where` condition:

```json
{"label": "Completed", "variable": "ADSL.EOSSTT", "where": "EOSSTT='COMPLETED'"}
```

A binding is not automatically created for every variable reference. Column headers, populations, supporting filters, programming notes, and source notes have separate roles.

### `columns`

Use the single `DATASET.VARIABLE` whose values form the result columns.

Example:

```json
"columns": "ADSL.GROUPN"
```

When each displayed column has a condition, such as `GROUPN=1`, `GROUPN=2`, or `GROUPN is missing`, do not add those headers as row bindings. If the shell's header cells carry those conditions as machine-readable `DATASET.VARIABLE=value` text, arsbridge reads the per-column conditions from the shell and you need only `columns`. If the header cells do NOT carry that machine-readable text (the column labels are prose such as `Group A`, `Group B`, `Unknown Group`), supply the conditions yourself in `column_groups` so arsbridge can build the columns.

### `column_groups`

Use `column_groups` ONLY when the result columns are values of the `columns` variable but the shell header cells do not already carry a machine-readable `DATASET.VARIABLE=value` filter you can read from the cell. Give an ordered array (left-to-right = display order), one entry per result column:

```json
"columns": "ADSL.GROUPN",
"column_groups": [
  {"label": "Group A", "where": "ADSL.GROUPN=1"},
  {"label": "Group B", "where": "ADSL.GROUPN=2"},
  {"label": "Unknown Group", "where": "is.na(ADSL.GROUPN)"}
]
```

Rules for `column_groups`:

- Write each `where` as a COMPLETE condition using the full `DATASET.VARIABLE`, for example `ADSL.GROUPN=1` or `is.na(ADSL.GROUPN)`. Unlike a binding `where` (which is bare because the binding names the dataset), a `column_groups` `where` has no separate variable field, so it must carry the dataset prefix.
- Enumerate EVERY value column, including the missing/Unknown bucket, written as `is.na(ADSL.GROUPN)` or `ADSL.GROUPN is missing`.
- Do NOT add a `Total` column here; use `include_total`.
- Give at least two entries; a single column is not a grouping axis.
- Omit `column_groups` entirely when the shell already carries the per-column filters (arsbridge reads those itself), or when `columns` is a treatment variable arsbridge groups by value automatically.

### `population`

Use the analysis-population condition that controls which records or subjects are included in the report.

Example:

```json
"population": "ADSL.SCRNFL='Y'"
```

Keep the dataset prefix in `population`.

### `analysis_type`

Use exactly one of:

- `CONTINUOUS`: descriptive summaries such as n, mean, SD, median, quartiles, minimum, and maximum.
- `CATEGORICAL`: counts or percentages by category.
- `SURVIVAL`: Kaplan-Meier or another time-to-event analysis.
- `AE_FREQUENCY`: adverse event frequency by System Organ Class and Preferred Term.
- `FIGURE`: graphical output.
- `LISTING`: subject-level or record-level listing.
- `OTHER`: a structure that does not fit the choices above, such as a shift table, overlap analysis, regression, or specialized variability analysis.

Do not use `AE_FREQUENCY` for medical history, medication, or procedure frequency tables only because they have a hierarchical structure. Use `AE_FREQUENCY` only for adverse event frequency outputs.

For a table that contains both continuous and categorical sections, choose the dominant or primary arsbridge method represented by the table. If the table cannot be represented reliably by one supported method, use `OTHER` and explain why.

### `ars_method_name`

Use the closest applicable value:

- `Summary Statistics - Continuous`
- `Count and Percentage`
- `Subject Count`
- `Kaplan-Meier Estimate`
- `AE Frequency Count`
- `Listing`

For `FIGURE` or `OTHER`, omit `ars_method_name` when no listed method is appropriate. Do not assign `Listing` to a figure only because it is the closest available text value.

### `by_variables`

Use an ordered array of bare variable names that form the result-column hierarchy, with the outermost grouping first.

Examples:

```json
["TRT01A"]
```

```json
["GROUPN", "SUBGRPN"]
```

Use an empty array when there are no grouping columns.

Do not include a variable for a Total column. Use `include_total` instead.

The first `by_variables` entry normally corresponds to `columns`. Additional entries represent nested groupings inside the outer result columns.

### `include_total`

Set to `true` when the result headers contain an overall or Total column. Otherwise set to `false`.

A Total column is not a variable and must not be added to `bindings`, `columns`, or `by_variables`.

### `is_supported`

Set to `false` only when the output requires a method that cannot be represented by the available arsbridge method, such as:

- model-based estimates,
- regression,
- inferential comparisons,
- confidence intervals for differences or contrasts,
- p-values,
- multiple imputation,
- specialized overlap calculations,
- specialized variability models.

When `is_supported` is `false`, add a clear `unsupported_reason` naming the required method or structure.

For supported outputs, omit `unsupported_reason` or set it to an empty string consistently. Prefer omission when no reason is needed.

## Mandatory annotation classification

Before creating JSON, classify every annotation into exactly one of these roles.

### 1. Population annotation

A condition in the report heading or subtitle that defines the analysis population.

Example:

`Analysis Population ADSL.ANALFL='Y'`

Output:

```json
"population": "ADSL.SCRNFL='Y'"
```

Do not create a binding for the population condition unless the same variable is also explicitly attached to a displayed analysis row elsewhere.

### 2. Result-column annotation

A variable or condition attached to a displayed result-column header.

Example:

- `Group A (N=XX) ADSL.GROUPN=1`
- `Group B (N=XX) ADSL.GROUPN=2`
- `Unknown Group (N=XX) is.na(ADSL.GROUPN)`
- `Total (N=XX)`

Output:

```json
"columns": "ADSL.GROUPN",
"by_variables": ["GROUPN"],
"include_total": true
```

Do not create bindings for these column headers.

If the header cells show the conditions as machine-readable text (`Group A (N=XX) ADSL.GROUPN=1`), stop here — arsbridge reads them from the shell. If the header cells are prose only (`Group A`, `Group B`, `Unknown Group`) and cannot be read as `DATASET.VARIABLE=value`, also emit `column_groups` (see the `column_groups` field) so the conditions are not lost:

```json
"column_groups": [
  {"label": "Group A", "where": "ADSL.GROUPN=1"},
  {"label": "Group B", "where": "ADSL.GROUPN=2"},
  {"label": "Unknown Group", "where": "is.na(ADSL.GROUPN)"}
]
```

### 3. Displayed row annotation

A variable attached to a displayed table stub or analysis row.

Example:

`Subjects screened, n ADSL.SCRNFN=1`

Output:

```json
{"label": "Subjects screened, n", "variable": "ADSL.SCRNFN", "where": "SCRNFN=1"}
```

### 4. Displayed listing-column annotation

A variable attached to a displayed listing column heading.

Example:

`Site-Subject ID ADSL.SITEPAT`

Output:

```json
{"label": "Site-Subject ID", "variable": "ADSL.SITEPAT"}
```

### 5. Supporting filter annotation

A variable condition that restricts the displayed value but does not supply the displayed category itself.

Example:

`Event category, n (%) ADEVENT.EVENTTERM when ADEVENT.EVENTCAT='TARGET CATEGORY' and ADEVENT.ANLFL='Y'`

Correct output:

```json
{
  "label": "Event category, n (%)",
  "variable": "ADEVENT.EVENTTERM",
  "where": "EVENTCAT='TARGET CATEGORY' and ANLFL='Y'"
}
```

Do not create three duplicate bindings for `MHTERM`, `MHSCAT`, and `MHPRESP`.

### 6. Programming-note annotation

An annotation in a programming note, source line, derivation explanation, or general instruction.

Do not create a binding unless the note clearly defines the derivation of a displayed row or listing column. If it does, attach the information to that displayed row or column.

## Reconstruct labels split by Word formatting

Word may split one displayed label across:

- multiple paragraphs,
- multiple lines in one cell,
- merged cells,
- adjacent cells,
- a paragraph followed by a table row,
- separate text runs with different color or formatting.

Before assigning a variable, reconstruct the complete displayed label.

Example:

```text
Subjects who met all
eligibility criteria, n ADSL.METALFN=1
```

Correct label:

`Subjects who met all eligibility criteria, n`

Incorrect label:

`eligibility criteria, n`

Another example:

```text
Subjects discontinued
the study, n (%) [a] xx (xx.x) ... ADSL.EOSSTTN=2
```

Correct label:

`Subjects discontinued the study, n (%) [a]`

Incorrect label:

`the study, n (%) [a] xx (xx.x) xx (xx.x)`

Apply these rules:

1. Join consecutive label fragments using one normal space.
2. Preserve the displayed word order.
3. Preserve capitalization, units, parentheses, and footnote markers.
4. Remove annotation text from the label.
5. Remove result placeholders such as `xx`, `XX`, `xx.x`, `N=XX`, and `xx (xx.x)`.
6. Do not use a footnote marker alone as a label.
7. Preserve a footnote marker when it is part of the complete displayed label.
8. Do not include text from an adjacent result column.
9. Stop the label before the first annotation or result placeholder.
10. If text extraction splits a label incorrectly, inspect the underlying Word table cells and paragraph order before deciding.

## Footnote mapping

Resolve row-to-variable mappings expressed through footnotes.

Example:

- Displayed row: `AGE (1)`
- Mapping below the table: `1: (ADSL.AGE)`

Output:

```json
{"label": "AGE (1)", "variable": "ADSL.AGE"}
```

Do not use `1` or `(1)` alone as the label.

Do not confuse explanatory footnotes with variable-mapping footnotes. An explanatory note such as `[a] Percentage based on Eligible Population` does not create a binding by itself.

## Rows with multiple annotations

When a displayed row contains several variable references:

1. Identify the primary variable whose values are displayed, summarized, categorized, or counted.
2. Put that variable in `variable`.
3. Put the remaining row-selection conditions in one `where` string.
4. Keep the order of conditions as written in the shell when practical.
5. Do not create duplicate bindings with the same label unless the shell clearly defines separate displayed rows.
6. If no primary displayed variable can be identified, omit the binding rather than guessing.

Example:

```text
Treatment category, n (%)
ADTRT.TRTTERM when ADTRT.TRTCAT='TARGET TREATMENT'
and ADTRT.ANLFL='Y'
```

Correct output:

```json
{
  "label": "Treatment category, n (%)",
  "variable": "ADTRT.TRTTERM",
  "where": "TRTCAT='TARGET TREATMENT' and ANLFL='Y'"
}
```

## Row filters and data types

When a displayed row represents one value of a variable, put that value in `where`.

Before writing the condition, check the variable data type in the ADaM specification.

### Character variables

Use single-quoted values:

```json
"where": "COMPLFL='Y'"
```

### Numeric variables

Use unquoted numeric values:

```json
"where": "EOSSTTN=2"
```

### Missing values

Use a clear missing condition that matches the variable type and the pipeline's accepted syntax. Prefer the syntax shown in the shell when it is valid. Examples:

```json
"where": "GROUPN is missing"
```

```json
"where": "COHORT is missing"
```

### Compound conditions

Use single quotes only for character literals:

```json
"where": "MHTERM not null and MHSCAT='UNDERLYING CONDITIONS' and MHPRESP='Y'"
```

Do not:

- quote numeric values,
- remove an explicit row condition,
- move a population condition into a row binding,
- move a column condition into a row binding,
- include the dataset prefix inside `where` unless the pipeline requires it.

Use bare variable names in `where` by default because the binding already identifies the dataset.

## Distinguish row labels from statistic subrows

Statistic subrows are not bindings when they belong to an annotated analysis row above them.

Common statistic subrows include:

- `n`
- `Mean (SD)`
- `Median`
- `Q1; Q3`
- `Min; Max`
- `Geometric Mean`
- `CV (%)`

Example:

```text
Age at Enrollment (Years) ADSL.AAGE1
  n
  Mean (SD)
  Median
  Q1; Q3
  Min; Max
```

Create one binding for `Age at Enrollment (Years)`. Do not create bindings for the statistic subrows.

## Column structure rules

For each table, inspect all header levels before selecting `columns` and `by_variables`.

### Single grouping level

Example:

```text
Group A
Group B
Total
```

If the annotated grouping variable is `ADSL.GROUPN`, use:

```json
"columns": "ADSL.GROUPN",
"by_variables": ["GROUPN"],
"include_total": true
```

### Nested grouping levels

Example:

```text
Group A
  Subgroup 1
  Subgroup 2
  Subgroup 3
  Group Total
Group B
Overall Total
```

If the outer grouping is `GROUPN` and the nested grouping is `SUBGRPN`, use:

```json
"columns": "ADSL.GROUPN",
"by_variables": ["GROUPN", "SUBGRPN"],
"include_total": true
```

Do not reduce the structure to `SUBGRPN` only when group is the outer header.

### Header filters

Per-column filters belong to the shell, not `bindings`.

Do not create bindings for:

- `Group A (N=XX)`
- `Group B (N=XX)`
- `Unknown Group (N=XX)`
- `Total (N=XX)`
- `Low (N=XX)` when it is a result-column header
- `Male (N=XX)` when it is a result-column header

Use these annotations only to identify `columns`, `by_variables`, and `include_total`. When the per-column conditions are NOT machine-readable in the shell header cells, additionally record them in `column_groups` (never in `bindings`).

## Population rules

A population condition usually appears:

- in the report heading,
- immediately below the title,
- in a report-level programming note applying to the full output.

Examples:

```json
"population": "ADSL.COMPLFL='Y'"
```

```json
"population": "ADSL.ANALFL='Y' and ADSL.REGIONN=1"
```

Use dataset prefixes in all population terms when more than one condition is present.

Do not infer a population only from a column header. For example, `GROUPN=1` in one column header is not the report population when the table also contains comparison and Total columns.

When a listing heading and a report-level filter identify several required conditions, include the full condition if all variables exist in the specification.

If an annotated population references a dataset or variable that does not exist in the specification, do not invent a replacement. Omit the unresolved part and do not guess.

## TLF numbering and inventory

Key every TLF by the correct report number.

Use the report heading as the primary source, but compare it with:

- the Table of Contents,
- the preceding and following TLF numbers,
- section numbering,
- repeated references in programming notes.

If the body contains an obvious typographical error such as `Table 2.3` between `12.2.2` and `12.2.4`, and the Table of Contents shows `12.2.3`, use `12.2.3`.

Do not silently renumber a report when the evidence is ambiguous.

Include every Table, Listing, and Figure in the shell. Before finalizing:

1. Build an inventory from the Table of Contents.
2. Build a second inventory from report headings in the document body.
3. Reconcile the two inventories.
4. Verify that every final JSON key maps to the correct title.
5. Check for missing keys, duplicate keys, and numbering typographical errors.

## Variable validation against the ADaM specification

Every emitted `DATASET.VARIABLE` must exist in the uploaded ADaM specification.

For every variable used in `bindings`, `columns`, or `population`:

1. Confirm the dataset exists.
2. Confirm the variable exists in that dataset.
3. Confirm the data type.
4. Use the data type to normalize literals in `where` and `population`.
5. Review value-level metadata when `PARAMCD`, `PARAM`, `AVAL`, or `AVALC` is used.
6. Do not treat a codelist name as a dataset variable.
7. Do not treat a method identifier as a dataset variable.
8. Do not invent a likely correction for a misspelled annotation.

If the shell contains an invalid variable annotation:

- omit that variable or binding,
- retain other valid parts of the TLF entry,
- do not substitute another variable based only on similarity.

## Handling shell annotation errors

The shell may contain:

- misspelled variables,
- incomplete conditions,
- smart quotes,
- mismatched quotes,
- labels joined to result placeholders,
- report-number typographical errors,
- annotations placed in an adjacent cell,
- an annotation that conflicts with the ADaM specification.

Apply these rules:

1. Normalize smart single quotes and smart double quotes to straight single quotes inside conditions.
2. Reconstruct labels from displayed text, not from annotation fragments.
3. Validate every variable against the specification.
4. Normalize value quoting based on the specification data type.
5. Resolve an obvious report-number typo only when supported by the Table of Contents and surrounding sequence.
6. Do not repair an invalid variable name by guessing.
7. Omit uncertain metadata instead of inventing it.
8. Never place result placeholders in a binding label.

## Analysis method selection

Choose the method from the displayed analysis, not only from the variables.

### Continuous summary

Use:

```json
"analysis_type": "CONTINUOUS",
"ars_method_name": "Summary Statistics - Continuous"
```

when the displayed output contains descriptive statistics such as mean, SD, median, quartiles, minimum, and maximum.

### Categorical count and percentage

Use:

```json
"analysis_type": "CATEGORICAL",
"ars_method_name": "Count and Percentage"
```

when the displayed output contains category counts and percentages.

### Subject count

Use `Subject Count` when the output consists of counts without percentages and without continuous summaries.

### Adverse event frequency

Use:

```json
"analysis_type": "AE_FREQUENCY",
"ars_method_name": "AE Frequency Count"
```

only for an adverse event frequency analysis, usually by SOC and PT.

### Listing

Use:

```json
"analysis_type": "LISTING",
"ars_method_name": "Listing"
```

for record-level or subject-level listings.

### Figure

Use:

```json
"analysis_type": "FIGURE"
```

Omit `ars_method_name` unless one of the available methods truly represents the figure's analysis data.

### Unsupported or specialized analysis

Use:

```json
"analysis_type": "OTHER",
"is_supported": false,
"unsupported_reason": "Specialized variability analysis requiring model-based estimates"
```

when a standard method cannot represent the analysis.

## Internal structure review for every TLF

Before writing each JSON entry, determine the following internally:

- TLF number
- clean title
- population
- outer result-column variable
- nested result-column variables
- Total-column status
- displayed analysis rows or listing columns
- primary variable for each displayed row
- row-specific conditions
- analysis type
- closest arsbridge method
- support status

Then verify:

1. No result-column header appears in `bindings`.
2. No population-only annotation appears in `bindings`.
3. Every binding label is a complete displayed label.
4. No binding label contains `xx`, `XX`, a result value, or annotation syntax.
5. No footnote marker is used alone as a binding label.
6. Every explicit row condition is retained.
7. Numeric and character literals match the ADaM specification data type.
8. Multiple row filters are combined into one `where` clause.
9. `columns` is one fully qualified variable.
10. `by_variables` preserves the header hierarchy.
11. Total is represented only by `include_total`.
12. Unsupported methods have a specific reason.

## Hard rules

1. **Never invent a variable.** Every `DATASET.VARIABLE` must exist in the uploaded ADaM specification.
2. **Column headers are not bindings.** Use them only for `columns`, `by_variables`, `include_total`, and (when the shell does not carry machine-readable per-column filters) `column_groups`.
3. **Population annotations are not row bindings.** Put them in `population`.
4. **Labels must represent the full displayed text.** Reconstruct labels split across Word lines, paragraphs, runs, or cells.
5. **Do not include result placeholders in labels.** Remove `xx`, `XX`, `N=XX`, and mock result values.
6. **Statistic subrows are not bindings.** Do not bind `Mean (SD)`, `Median`, `Q1; Q3`, `Min; Max`, or `n` when they belong to an annotated row above.
7. **Preserve explicit row filters.** Put them in `where`.
8. **Use data-type-correct literals.** Quote character values with single quotes. Do not quote numeric values.
9. **Use single quotes inside JSON string values for condition literals.** Never use raw double quotes for condition values.
10. **Combine supporting filters.** Use one binding with a compound `where` clause rather than duplicate bindings for the same displayed row.
11. **Do not guess from similar variable names.** Omit unresolved annotations.
12. **Cover every TLF.** Reconcile the Table of Contents and body headings before finalizing.
13. **Keep every JSON object key unique.**
14. **Omit optional fields that cannot be determined reliably.**
15. **Use a real JSON serializer.** Do not hand-type the final file.

## Worked example: generic categorical summary table

This example demonstrates the intended logic. It is not tied to a specific study or shell.

### Example shell structure

```text
Table 12.1.1 Summary of Subject Status - Analysis Population ADSL.ANALFL='Y'

Columns:
Group A (N=XX) ADSL.GROUPN=1
Group B (N=XX) ADSL.GROUPN=2
Unknown Group (N=XX) ADSL.GROUPN is missing
Total (N=XX)

Rows:
Subjects included, n ADSL.INCLFN=1
Subjects completed, n (%) [a] ADSL.STATUSN=1
Subjects discontinued, n (%) [a] ADSL.STATUSN=2
Subjects ongoing, n (%) [a] ADSL.STATUSN=3
Primary reason for discontinuation, n (%) [a] ADSL.REASONN
```

### Correct interpretation

- Population: `ADSL.ANALFL='Y'`
- Result-column variable: `ADSL.GROUPN`
- Grouping variables: `GROUPN`
- Total column: yes
- Analysis type: categorical
- Method: count and percentage
- Column headers: not bindings
- Displayed subject-status rows: bindings

### Correct JSON entry

```json
{
  "title": "Summary of Subject Status",
  "bindings": [
    {
      "label": "Subjects included, n",
      "variable": "ADSL.INCLFN",
      "where": "INCLFN=1"
    },
    {
      "label": "Subjects completed, n (%) [a]",
      "variable": "ADSL.STATUSN",
      "where": "STATUSN=1"
    },
    {
      "label": "Subjects discontinued, n (%) [a]",
      "variable": "ADSL.STATUSN",
      "where": "STATUSN=2"
    },
    {
      "label": "Subjects ongoing, n (%) [a]",
      "variable": "ADSL.STATUSN",
      "where": "STATUSN=3"
    },
    {
      "label": "Primary reason for discontinuation, n (%) [a]",
      "variable": "ADSL.REASONN"
    }
  ],
  "columns": "ADSL.GROUPN",
  "population": "ADSL.ANALFL='Y'",
  "analysis_type": "CATEGORICAL",
  "ars_method_name": "Count and Percentage",
  "by_variables": ["GROUPN"],
  "include_total": true,
  "is_supported": true
}
```

### Incorrect interpretations to avoid

Do not create a binding for a result-column header:

```json
{"label": "Group A (N=XX)", "variable": "ADSL.GROUPN", "where": "GROUPN=1"}
```

Do not create an incomplete label after a Word line break:

```json
{"label": "discontinued, n (%) [a]", "variable": "ADSL.STATUSN"}
```

Do not include mock results in a label:

```json
{"label": "Subjects discontinued, n (%) [a] xx (xx.x)", "variable": "ADSL.STATUSN"}
```

Do not use a footnote marker alone:

```json
{"label": "[a]", "variable": "ADSL.STATUSN"}
```

Do not quote a numeric value:

```json
{"label": "Subjects discontinued, n (%) [a]", "variable": "ADSL.STATUSN", "where": "STATUSN='2'"}
```

## Required output format

### Preferred output

Write the final JSON to a downloadable file named exactly:

`supplement.json`

Create it programmatically with a real JSON serializer, for example:

```python
json.dump(obj, f, ensure_ascii=True, indent=2)
```

Do not hand-type the JSON into the file.

### Fallback output

Only when file creation is unavailable, return exactly one fenced `json` code block and no prose before or after it.

## Required top-level structure

```json
{
  "supplement_version": 2,
  "tlfs": {
    "12.1.1": {
      "title": "Summary of Subject Status",
      "bindings": []
    }
  }
}
```

The `supplement_version` value must be exactly `2`.

Every TLF entry must contain `bindings`, even when no valid binding can be resolved. Other fields are optional and must be omitted when uncertain.

If a TLF cannot be processed at all, omit that TLF rather than inventing an entry. However, first make every reasonable effort to process it because complete inventory coverage is required.

## Final validation checklist

Before delivering `supplement.json`, perform all checks below.

### JSON syntax

- Parse the file with a strict JSON parser.
- Confirm there are no trailing commas.
- Confirm there are no comments.
- Confirm there are no placeholder ellipses.
- Confirm each object key is unique.
- Confirm no string contains an unintended raw double quote.
- Confirm the top-level `supplement_version` is `2`.

### TLF inventory

- Compare JSON keys with the Table of Contents.
- Compare JSON keys with body headings.
- Confirm every TLF number maps to the correct title.
- Confirm there are no duplicate or malformed keys.
- Resolve only obvious numbering typographical errors supported by the document structure.

### Variables

- Confirm every binding variable exists in the ADaM specification.
- Confirm every `columns` variable exists.
- Confirm every variable used in `population` exists.
- Confirm numeric and character literal syntax matches the specification data type.
- Confirm value-level metadata was reviewed when needed.

### Bindings

- Confirm no result-column header is in `bindings`.
- Confirm no population-only annotation is in `bindings`.
- Confirm labels are complete and displayed in the shell.
- Confirm labels do not contain annotations or mock results.
- Confirm statistic subrows were not emitted.
- Confirm explicit row filters were retained.
- Confirm supporting filters were combined into `where`.
- Confirm no duplicate bindings were created from one displayed row.

### Column structure

- Confirm `columns` identifies the outer result-column variable.
- Confirm `by_variables` preserves all nested header levels in order.
- Confirm Total is represented only by `include_total`.
- Confirm per-column filters were not converted into row bindings.
- Confirm per-column conditions that were not machine-readable in the shell were recorded in `column_groups`, each `where` using the full `DATASET.VARIABLE`, and that `column_groups` was omitted when the shell already carries them.

### Method

- Confirm `analysis_type` matches the displayed analysis.
- Confirm `ars_method_name` is one of the allowed methods.
- Confirm `AE_FREQUENCY` is used only for adverse event frequency outputs.
- Confirm figures do not receive `Listing` as a default method.
- Confirm unsupported analyses have a specific reason.

### Delivery

- Confirm the file is named `supplement.json`.
- Confirm the file is downloadable.
- Do not provide only an on-screen preview when file creation is available.
