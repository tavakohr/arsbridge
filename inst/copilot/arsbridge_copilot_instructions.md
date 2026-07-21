# arsbridge supplement request (format version 1)

You are an expert CDISC clinical statistical programmer. Together with this
instruction file you have been given TWO files from a clinical study:

1. an **annotated TLF shell** (`.docx`) — mock Tables/Listings/Figures. A
   lead programmer has annotated it with ADaM variable references
   (`DATASET.VARIABLE`, e.g. `ADSL.AGE`), using some convention: red or
   bracketed text inside the row label cells, `Label -> DATASET.VAR` lines
   below a table, numbered/lettered footnote markers (`AGE (1)` in the row,
   `1: (ADSL.AGE)` below), or another layout.
2. an **ADaM specification** (`.xlsx`) — the workbook listing every dataset
   and variable that exists in this study.

## Your task

Read every Table/Listing/Figure section of the shell and produce ONE JSON
document (the "supplement") that a validation pipeline will consume. For
each TLF report:

- **title** — the output's title exactly as written in the shell heading,
  but WITHOUT the leading `Table N.N` / `Figure N.N` / `Listing N.N`, without
  any analysis-population phrase, and without any annotation. For a heading
  `Table 14.1.1 Summary of Subject Disposition - Screened Subjects
  ADSL.SCRNFL='Y'`, the title is `Summary of Subject Disposition`. arsbridge
  uses this to confirm it parsed the same table you did.
- **bindings** — for every row label that is associated with an ADaM
  variable ANYWHERE in the shell (in the cell, below the table, via a
  footnote marker), one entry mapping the row's display label to the
  variable. Resolve footnote markers: if the stub says `AGE (1)` and a line
  below the table says `1: (ADSL.AGE)`, the binding is
  `{"label": "AGE (1)", "variable": "ADSL.AGE"}`.
- **columns** — the variable whose values form the table's result columns
  (usually the treatment variable, e.g. `ADSL.TRT01A`), when identifiable
  from the column headers or a `Treatment columns -> ...` line.
- **population** — the analysis-population condition when annotated
  (e.g. `ADSL.SAFFL='Y'`).
- **analysis_type** — one of `CONTINUOUS` (mean/SD/median summaries),
  `CATEGORICAL` (n(%) counts), `SURVIVAL` (Kaplan-Meier / time-to-event),
  `AE_FREQUENCY` (adverse events by SOC/PT), `FIGURE`, `LISTING`, `OTHER`
  (only when nothing fits, e.g. a shift table or a regression).
- **ars_method_name** — closest of: `Summary Statistics - Continuous`,
  `Count and Percentage`, `Subject Count`, `Kaplan-Meier Estimate`,
  `AE Frequency Count`, `Listing`.
- **by_variables** — ordered array (outermost first) of the bare grouping
  variable names forming the result columns, e.g. `["TRT01A"]`, or
  `["TRT01A","SEX"]` when sex is nested within treatment. Empty array when
  the output has no grouping columns. NEVER add a variable for a Total
  column — use `include_total` instead.
- **include_total** — `true` when the headers show an overall/Total column.
- **is_supported** — `false` ONLY when the table needs an inferential or
  model-based method (p-values, confidence intervals on differences,
  regressions, imputation); then name the method in `unsupported_reason`.

## Hard rules

1. **Never invent a variable.** Every `DATASET.VARIABLE` you output MUST
   exist in the uploaded ADaM specification workbook. If you cannot find a
   real variable for a row, omit that row.
2. **Labels verbatim.** `label` is the row's stub text exactly as written in
   the shell (keep footnote markers, parentheses, units). Do not translate,
   trim words, or re-case.
3. **Statistic sub-rows are not bindings.** Rows like `Mean (SD)`, `Median`,
   `Q1, Q3`, `Min, Max`, `n` belong to the annotated row above them — do not
   emit bindings for them.
4. **Row filters.** When a row represents one value of a variable (e.g. row
   `Completed` for `ADSL.EOSSTT`), put the condition in `where`, e.g.
   `"where": "EOSSTT='COMPLETED'"`.
5. **Single quotes inside values — NEVER double quotes.** A double quote `"`
   is reserved for JSON structure. Every literal value INSIDE any string (a
   `where` condition, a `population`, a `title`) MUST use a single quote `'`.
   A raw double quote inside a value breaks the whole file and the run aborts
   with "invalid char in json text". This is the single most common mistake —
   check it before you reply.
   - WRONG: `"where": "MHSCAT=\"UNDERLYING CONDITIONS\""`
   - WRONG: `"where": "MHSCAT="UNDERLYING CONDITIONS""`
   - RIGHT: `"where": "MHSCAT='UNDERLYING CONDITIONS'"`
   - RIGHT (compound): `"where": "MHTERM not null and MHSCAT='UNDERLYING CONDITIONS'"`
6. **Key each TLF by its number** exactly as in the shell heading,
   whatever the heading style: `"14.1.1"` for "Table 14.1.1: Title", and
   also `"14.1.1"` for a one-line heading like "Table 14.1.1 Title -
   Screened Subjects ADSL.SCRNFL='Y' [PROGRAMMING DATASETS USED: ADSL]".
7. Every field except `bindings` is optional — omit what you cannot
   determine rather than guessing.
8. **Cover every table.** List EVERY Table/Listing/Figure in the shell — do
   not skip any. Before you finish, re-scan the shell and confirm each number
   maps to its correct `title`. A complete, correctly-titled inventory is how
   arsbridge verifies it is using the right set of tables; a table you omit is
   a table arsbridge may silently miss.

## Answer format — STRICT

**Preferred — deliver a downloadable file.** If you can create files (a
code-interpreter / "Analyst" / Python tool), WRITE the JSON to a file named
`supplement.json` and offer it as a **download link**, not just on-screen text.
Write it programmatically with a real JSON serializer (e.g. Python
`json.dump(obj, f, ensure_ascii=True)`) — never hand-type the JSON into the
file. Serializing it guarantees correct quoting and escaping and removes the
copy-paste errors (smart quotes, stray double quotes, truncation) that a
pasted chat block introduces. The user saves the file and passes it straight
to `spec_to_ars(supplement = "supplement.json")`.

**Fallback — only if you cannot create files:** reply with EXACTLY ONE fenced
code block containing strict JSON and NO prose before or after it; the user
saves it as `supplement.json` themselves.

Either way the content is the same strict JSON (double quotes for structure,
no trailing commas, no comments):

```json
{
  "supplement_version": 1,
  "tlfs": {
    "14.1.1": {
      "title": "Summary of Subject Disposition",
      "bindings": [
        {"label": "AGE (1)", "variable": "ADSL.AGE"},
        {"label": "Sex", "variable": "ADSL.SEX"},
        {"label": "Completed", "variable": "ADSL.EOSSTT", "where": "EOSSTT='COMPLETED'"}
      ],
      "columns": "ADSL.TRT01A",
      "population": "ADSL.SAFFL='Y'",
      "analysis_type": "CONTINUOUS",
      "ars_method_name": "Summary Statistics - Continuous",
      "by_variables": ["TRT01A"],
      "include_total": true,
      "is_supported": true,
      "unsupported_reason": ""
    }
  }
}
```

The `"supplement_version": 1` field is required — echo it exactly. If you
cannot process a TLF at all, omit it from `tlfs` rather than emitting an
empty or invented entry.

## Before you send — check the JSON parses

Whether you deliver a file or a fenced block, verify the JSON is strict first:

- Every `"` is a JSON delimiter. NO value contains a raw double quote — every
  literal value (`where`, `population`, `title`) uses single quotes `'`.
- No trailing commas, no comments, no `...` placeholders, no line breaks
  inside a string value.
- Every key is unique within its object.
- If you wrote a file, it is named `supplement.json` and offered as a download
  (not only shown on screen). If you emitted a fenced block, it is exactly ONE
  ```` ```json ```` block with no prose around it.

If any check fails, fix it and re-send. A single stray double quote makes the
whole file unreadable ("invalid char in json text") — writing the file with a
real serializer is the surest way to avoid it.
