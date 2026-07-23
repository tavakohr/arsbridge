# arsbridge Phase 1: Evidence Discovery and TLF Blueprint

## How to run this

In your chat assistant (Copilot / ChatGPT / enterprise portal), start a NEW
session and attach three files: **this file**, your annotated TLF shell
(`.docx`), and your ADaM specification (`.xlsx`). Select the highest reasoning
mode. Paste the prompt below. Save the reply as
`tlf_extraction_blueprints.json` and carry it into the Phase 2 session.

Prompt to paste:

```text
Read all three attached files completely: this Phase 1 instruction file, the
annotated TLF shell, and the ADaM specification.

Perform Phase 1 ONLY, following this instruction file exactly. Process every
Table, Listing, and Figure. Discover, classify, and validate all evidence and
build one table-specific blueprint per output. Do NOT produce supplement.json.

Return exactly one strict-JSON file: tlf_extraction_blueprints.json
(blueprint version 2) -- every TLF once, no duplicate keys.
```

## Document control

- Instruction version: 8.1 (packaged with arsbridge)
- Phase: 1 only
- Primary output: `tlf_extraction_blueprints.json` (blueprint version 2)
- Inputs: complete annotated TLF shell (`.docx`) + complete ADaM specification (`.xlsx`) + this file
- Next step: Phase 2 turns the blueprint into `supplement.json` (format version 3)

Use the highest available reasoning mode. Do not use a quick-response mode.

## 1. Role

Act as a senior CDISC statistical programmer, annotated-shell metadata
specialist, and ARS requirements analyst. Phase 1 is NOT final JSON generation.
Phase 1 discovers, preserves, classifies, and validates the evidence Phase 2
needs. Do not produce `supplement.json` in Phase 1.

## 2. Inputs and authority

The shell is authoritative for: output identity, displayed titles and
population labels, row/column structure, indentation and hierarchy, mock
statistics, footnotes, programming notes, sorting, no-data behaviour, and
repeated/inherited instructions.

The ADaM specification is authoritative for: dataset and variable existence,
type, labels, keys, controlled terminology, value-level metadata (PARAMCD,
PARAM, AVAL, AVALC), derivations, and origins.

## 3. Full-document requirement

Process every Table, Listing, and Figure. Build two inventories -- a
table-of-contents inventory and a body-heading inventory -- and reconcile them
before analysing individual outputs. For each output capture the TLF number,
output ID, output type, raw title, clean-title candidate, population label,
boundaries, continuation-page relationship, and any cross-reference to another
TLF. Record (do not silently resolve) any ambiguous numbering or title
conflict.

## 4. Preserve shell location and structure

Do not flatten the shell to plain text without location. Capture page,
paragraph, table, row, column, cell, header/footer, text box, and comment
locations. Reconstruct text split across runs, cells, merged cells, and
adjacent paragraphs. Inspect headings, all header levels, spanning/merged
headers, stub labels, indentation, statistic sub-rows, mock values, footnote
markers and text, programming notes, sorting notes, no-data instructions, and
source statements.

## 5. Evidence inventory

Create one evidence record for every annotation and analytical instruction:

- evidence ID, original text, normalized text, location, TLF number
- primary semantic role (and secondary roles when justified)
- datasets and variables referenced, value literals
- data-type validation against the ADaM spec, validation result
- intended Phase-2 destination
- confidence: `HIGH`, `MEDIUM`, or `LOW`
- disposition: `CONSUME`, `INFORMATIONAL_ONLY`, `DUPLICATE`,
  `INVALID_REFERENCE`, `OUT_OF_SCOPE`, or `UNRESOLVED` (with a reason for every
  non-consumed disposition)

Allowed primary roles include: `OUTPUT_IDENTITY`, `POPULATION`,
`SOURCE_DATASET`, `SUBJECT_IDENTIFIER`, `JOIN_RULE`, `RECORD_FILTER`,
`ROW_FILTER`, `SECTION_FILTER`, `RESULT_COLUMN_GROUP`, `TOTAL_COLUMN_RULE`,
`DISPLAYED_ROW_VARIABLE`, `DISPLAYED_LISTING_COLUMN`, `X_VARIABLE`,
`Y_VARIABLE`, `GROUPING_VARIABLE`, `PANEL_VARIABLE`, `PARAMETER_RULE`,
`STATISTIC`, `DENOMINATOR_RULE`, `COUNTING_RULE`, `DEDUPLICATION_RULE`,
`SORT_RULE`, `FOOTNOTE_RULE`, `NO_DATA_RULE`, `FORMAT_RULE`, `REFERENCE_LINE`,
`PROGRAMMING_NOTE`, `PROVENANCE_OR_REVIEW`, `INFORMATIONAL_ONLY`, `UNRESOLVED`.

## 6. Split compound annotations

A compound annotation can contain several rules; split it before classifying.
For example `ADDV.DVCAT where ADDV.ANL01FL='Y'` and
`ADDV.DVDECOD where ADDV.ANL01FL='Y'` yields separate evidence for the category
variable `ADDV.DVCAT`, the subcategory variable `ADDV.DVDECOD`, and the
report-wide record filter `ADDV.ANL01FL='Y'`.

## 7. Validate every metadata reference

For every dataset/variable reference: confirm the dataset exists, confirm the
variable exists, capture type and label, identify the subject identifier and
keys, and inspect controlled terms and value-level metadata when values are
used. Do NOT repair an invalid name by similarity -- record the invalid
reference with its original text and location.

## 8. Analysis family

Choose from the displayed shell structure, not the first variable found. Use
exactly these families (they are the families Phase 2 emits, aligned with
arsbridge's supplement `analysis_type`):

`CONTINUOUS`, `CATEGORICAL`, `CATEGORICAL_HIERARCHICAL`, `MIXED_SUMMARY`,
`SUBJECT_COUNT`, `SURVIVAL`, `AE_FREQUENCY`, `SHIFT_TABLE`, `LISTING`,
`FIGURE`, `MODEL_BASED`, `OTHER`.

Use `MIXED_SUMMARY` when one table mixes categorical and continuous parameters
(demographics/baseline). Use `CATEGORICAL_HIERARCHICAL` for SOC/PT or
category/subcategory.

## 9. Component status

For every applicable ARS component assign `PRESENT`, `DERIVABLE`,
`NOT_APPLICABLE` (with a reason), `UNRESOLVED`, or `MISSING_BUT_REQUIRED`:
output identity, clean title, population label + condition, source datasets,
subject identifier, join plan, record filter, section filters, row filters,
column hierarchy, group levels, Total column and scope, denominator, row
hierarchy, displayed row labels, analysis variables, display variables,
parent-child relationships, analysis methods, counting unit, deduplication
keys, statistics, precision/format, visit/timepoint, sorting, zero-row and
no-data behaviour, footnotes, abbreviations, listing columns, figure roles,
validation rules, expected ARD contract, support status, extraction
completeness, and provenance/review items. Decide whether a field applies
before marking it missing.

## 10. Cross-references

When a shell says `same as`, `repeat`, `replace`, or refers to another output,
capture the referenced TLF number and the exact inheritance instruction,
identify inherited components and replacements, and record the relationship in
the blueprint. Do not mark the current TLF as lacking evidence until the
referenced output is inspected.

## 11. Blueprint per TLF

Create one blueprint per TLF, matched to that actual output (not a universal
template): output identity, shell boundary, clean-title candidate, population
label, analysis family, shell-structure summary, component status, required
final fields, required row roles, listing columns or figure roles, validated
metadata references, the complete evidence inventory, cross-reference
inheritance, unresolved items, and a blueprint status. Use placeholders only
inside the blueprint (`__REQUIRED_VALUE__`, `__RESOLVE_OR_REVIEW__`).

## 12. Reviews and completion gate

Before writing: (A) confirm every TLF appears once and every shell section is
assigned; (B) confirm every annotation has a disposition; (C) compare title,
row labels, indentation, headers, footnotes, and notes against the chosen
family; (D) confirm all references are validated or explicitly unresolved;
(E) confirm each blueprint lists its required final fields.

Blueprint status is one of `READY_FOR_PHASE_2`, `READY_WITH_REVIEW`,
`BLUEPRINT_INCOMPLETE`, or `FAILED`.

## 13. Output

Create exactly one file: `tlf_extraction_blueprints.json`. Requirements: strict
JSON, blueprint version 2, complete TLF inventory, no duplicate TLF keys,
evidence and validated metadata stored per TLF, and a strict parse check before
delivery. Do NOT create `supplement.json` in Phase 1.
