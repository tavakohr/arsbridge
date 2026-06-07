# Build spec_to_ars() for arsbridge R Package

## Exported Surface

[`spec_to_ars()`](reference/spec_to_ars.md) is the **only exported
function**. Any previously existing functions (`shell_to_ars`,
`shell_annotate`) are deleted entirely and not replaced.

------------------------------------------------------------------------

## Context

This is one function in the `arsbridge` R package. The function reads a
lead programmer’s completed annotated TLF shell (Word docx) and an ADaM
specification (Excel), and produces a CDISC ARS v1.0 ARM-TS JSON file.
The JSON is consumed by `siera::readARS()` to generate QC R scripts.

**Core principle:** The parser extracts and converts — it does not
invent. Every variable in the ARS output must trace back to an
annotation written by a lead programmer in the annotated shell. No blank
shells. No variable inference.

**Style-agnostic parsing:** The parser must not assume any single
annotation style. CROs have different conventions — red font colour,
bold/italic, brackets, parentheses, a dedicated annotation column, or no
formatting at all. **Font colour is one signal among several, not the
primary requirement.** The ADaM naming convention itself (`ADSL.AGE`,
`ADAE.TRTEMFL='Y'`) is structurally distinctive enough to identify an
annotation from text alone. A stub cell that contains
`"Age (years) ADSL.AGE"` is unambiguously annotated regardless of
whether the text `ADSL.AGE` is red, black, bold, or plain. The parser
must detect annotations using a hierarchy of evidence and should never
fail to extract an annotation simply because no special formatting was
applied.

------------------------------------------------------------------------

## Part 1 — Annotated Shell Format: Ground Truth

### 1.1 Color Convention

Every run in the Word document has a font colour. Two colours carry
meaning:

| Hex | RGB | Meaning |
|----|----|----|
| `C00000` | (192, 0, 0) | ADaM annotation — variable reference, filter condition, or count expression |
| `808080` | (128, 128, 128) | Source line, disclaimer, synthetic note — ignore for annotation extraction |
| Black / no colour | — | Regular display text — stub row label, footnote, title |

### 1.2 Document Structure

A combined TLF shells document has this repeating pattern:

    [Optional: Cover page / TOC table (skip entirely)]

    Table X.X.X                     ← paragraph matching TLF_HEADING_REGEX
    Title of the table               ← first non-empty paragraph after heading
    Population Name (FLAG_IN_RED)    ← second non-empty paragraph; red run = population annotation
    [optional footnote lines]        ← lines starting with [a], [b], *, or other footnote markers
    Source: DATASET1, DATASET2.      ← grey (808080) — extract dataset list, do not treat as annotation
    [the shell table]                ← first table element after the heading

**TLF_HEADING_REGEX:**

    ^(Table|Figure|Listing)\s+(\d{1,3}\.\d+(?:\.\d+)*)\s*$

Examples that match: `Table 14.1.1`, `Table 14.2.3`, `Listing 16.2.1`,
`Figure 14.2.1`

**TOC detection:** If the very first table in the document has a
first-row cell containing “Number” or “Table Number”, it is a TOC — skip
it entirely.

### 1.3 Population Line Annotation

The population paragraph contains black text (the population name) and
one or more red runs (the flag expression).

**Observed patterns:**

    "Safety Population (ADSL.SAFFL='Y')"
      black: "Safety Population ("     red: "ADSL.SAFFL='Y'"     black: ")"

    "Intent-to-Treat Population (ADSL.ITTFL='Y')"
      black: "Intent-to-Treat Population ("     red: "ADSL.ITTFL='Y'"     black: ")"

    "Safety Population (ADSL.SAFFL='Y' and ADCM.CONTRTFL='Y')"
      black: "Safety Population ("
      red:   "ADSL.SAFFL='Y'"
      black: " and "
      red:   "ADCM.CONTRTFL='Y'"
      black: ")"

    "All Subjects (N=250)"
      black: "All Subjects (N="   red: "250"   black: ")"
      → No flag; population is All Subjects. AnalysisSet has no condition.

**Extraction rule:** Concatenate all C00000 runs in the population
paragraph to get the population annotation string. If empty → no
population flag (All Subjects).

**AnalysisSet construction from population annotation:**

| Annotation text | AnalysisSet condition |
|----|----|
| `ADSL.SAFFL='Y'` | Simple: dataset=ADSL, variable=SAFFL, comparator=EQ, value=\[“Y”\] |
| `ADSL.ITTFL='Y'` | Simple: dataset=ADSL, variable=ITTFL, comparator=EQ, value=\[“Y”\] |
| `ADSL.PPROTFL='Y'` | Simple: dataset=ADSL, variable=PPROTFL, comparator=EQ, value=\[“Y”\] |
| `ADSL.SAFFL='Y' and ADCM.CONTRTFL='Y'` | Compound AND of two conditions |
| (empty / no red) | No condition — All Subjects |

### 1.4 Stub Row Annotations

**The annotation always appears as red text appended after two spaces
within the same table cell.**

The cell contains: - Black runs: the display label (what will appear in
the final table) - Red runs: the ADaM annotation (DATASET.VARIABLE or
expression)

**Observed patterns:**

    CELL: "Age at Informed Consent (years)  ADSL.AGE"
      black: "Age at Informed Consent (years)  "    red: "ADSL.AGE"
      → stub_label = "Age at Informed Consent (years)"
      → annotation  = "ADSL.AGE"

    CELL: "Screen Failure  ADSL.SCRFFL='Y'"
      black: "Screen Failure  "    red: "ADSL.SCRFFL='Y'"
      → stub_label = "Screen Failure"
      → annotation  = "ADSL.SCRFFL='Y'"

    CELL: "Subjects with at least one concomitant medication  unique USUBJID in ADCM where ADCM.CONTRTFL='Y'"
      black: "Subjects with at least one concomitant medication  "
      red:   "unique USUBJID in ADCM where ADCM.CONTRTFL='Y'"
      → stub_label = "Subjects with at least one concomitant medication"
      → annotation  = "unique USUBJID in ADCM where ADCM.CONTRTFL='Y'"

    CELL: "Discontinued Prior to Receiving Treatment  ADSL.SFENRLFL='Y' or ADSL.WTHTYP='Withdrawal Prior to Treatment'"
      red text is the entire compound expression
      → annotation = "ADSL.SFENRLFL='Y' or ADSL.WTHTYP='Withdrawal Prior to Treatment'"

    CELL: "Reason for Study Discontinuation  ADSL.DCSREAS"
      → stub_label = "Reason for Study Discontinuation"
      → annotation  = "ADSL.DCSREAS"

    CELL: "n"                                         → no red runs → no annotation (child row)
    CELL: "Mean (SD)"                                  → no red runs → no annotation (child row)
    CELL: "Adverse event"                              → no red runs → no annotation (sub-category row)

**Extraction rule:** 1. For each row’s stub cell (column 0), collect all
C00000 runs → annotation string 2. The black text = stub label (trim
trailing whitespace and separator) 3. If annotation string is empty →
this is a child/sub-row → skip for ARS Analysis creation 4. Only rows
WITH red annotations become ARS Analysis objects

### 1.5 Column Headers (GroupingFactor)

Column headers are plain black text — no annotations. They reveal the
`by_variable` (grouping variable).

**Detection rule:** If any column header contains a treatment name
pattern (`"n (%)"`, `"(N=XXX)"`, treatment name), extract the treatment
label. The `by_variable` comes from the population line or ADaM spec
(typically `TRT01A` for safety, `TRT01P` for efficacy).

**Observed examples:**

    "PROSVALIN 0.5 mcg\n(N=125)"  →  treatment arm 1
    "Placebo\n(N=125)"             →  treatment arm 2
    "UPADALIMIB 15 mg\n(N=200)"    →  treatment arm 1 (3-arm study)
    "UPADALIMIB 30 mg\n(N=200)"    →  treatment arm 2

The number of non-stub data columns = number of treatment arms = number
of ARS Groups.

### 1.6 Source Line

The grey (808080) paragraph starting with `"Source:"` lists the ADaM
datasets for this TLF.

    "Source: ADSL, ADIE."      →  ["ADSL", "ADIE"]
    "Source: ADCM, ADSL."      →  ["ADCM", "ADSL"]
    "Source: ADSL."            →  ["ADSL"]

Use regex: `r"^Source\s*:\s*(.+?)\.?\s*$"` (case-insensitive), split
result on `[,;]`.

------------------------------------------------------------------------

### 1.7 Multi-Layer Annotation Detection (Style-Agnostic)

**The colour convention in sections 1.1–1.5 is one CRO’s standard. It is
not universal.** Different organisations annotate shells differently.
The parser must detect annotations regardless of formatting style by
applying four detection layers in order, stopping at the first layer
that produces confident results.

------------------------------------------------------------------------

#### Layer 1 — Colour-Based Detection (Highest Confidence)

Check every run in a cell or paragraph for a non-black, non-grey font
colour.

``` r

ANNOT_RED_HEX  <- "C00000"

is_annotation_run <- function(run) {
  if (is.null(run$font$color$rgb)) return(FALSE)
  hex <- toupper(as.character(run$font$color$rgb))
  !hex %in% c("808080", "000000", "FFFFFF", "AUTO")
}
```

If ANY cell runs are coloured (excluding grey), apply pattern validation
(Layer 3) to confirm the coloured text is an ADaM reference before
accepting it as an annotation. Coloured text that does NOT match an ADaM
pattern (e.g. a red footnote marker `[a]`) must NOT be treated as an
annotation.

**Confidence: HIGH** — when coloured text also matches ADaM pattern.
**Confidence: MEDIUM** — when coloured text does not match ADaM pattern
(treat as uncertain).

------------------------------------------------------------------------

#### Layer 2 — Formatting-Based Detection (Medium Confidence)

Some CROs use bold, italic, or underline instead of colour for
annotations.

``` r

is_formatted_run <- function(run) {
  isTRUE(run$bold) || isTRUE(run$italic) || isTRUE(run$underline)
}
```

Apply pattern validation (Layer 3) to all formatted runs. Only accept
formatted runs as annotations if they match the ADaM pattern AND the
rest of the cell text looks like a stub row label (plain English, title
case, not matching ADaM pattern).

**Confidence: MEDIUM** — formatted text matching ADaM pattern.

------------------------------------------------------------------------

#### Layer 3 — ADaM Naming Pattern Detection (Text Only)

**This is the most universally applicable detection layer.** Many
annotated shells — particularly those produced in-house or by smaller
CROs — apply no special formatting whatsoever. The lead programmer
simply appends the ADaM variable name to the stub row label, separated
by whitespace, a tab, or a delimiter. This is sufficient for unambiguous
identification because ADaM naming conventions are highly distinctive:

- Standard dataset prefix always starts with `AD` (`ADSL`, `ADAE`,
  `ADLB`, `ADTTE`, etc.)
- Variable names are 1–8 uppercase alphanumeric characters with a dot
  separator
- Filter conditions follow a fixed grammar: `DATASET.VARIABLE='VALUE'`
- Count expressions follow fixed prose:
  `unique USUBJID in DATASET where ...`

No natural-language stub row label (`"Age at Informed Consent (years)"`,
`"Reason for Study Discontinuation"`, `"Subjects with at Least One AE"`)
will ever accidentally match these patterns. **When text in a stub cell
matches an ADaM pattern, it is an annotation with high certainty — even
if the text is plain black and indistinguishable in appearance from the
rest of the cell.**

The parser must apply this layer to every cell regardless of whether
Layers 1 or 2 already fired. It serves as both a primary detection
method (for unformatted shells) and a confirmation method (validating
that coloured/formatted text is actually ADaM, not a footnote marker or
decorative element).

------------------------------------------------------------------------

**Structurally distinctive patterns — why plain text is unambiguous:**

    "Age (years)  ADSL.AGE"
      Plain text, no formatting. "ADSL.AGE" matches DATASET.VARIABLE exactly.
      Confidence: HIGH — no English word could match this pattern.

    "Screen Failure  ADSL.SCRFFL='Y'"
      The equality expression DATASET.VARIABLE='VALUE' is unique to ADaM.
      Confidence: HIGH.

    "Overall Survival  ADTTE.PARAMCD EQ 'OS'"
      ARS comparator syntax. Unambiguous.
      Confidence: HIGH.

    "Concomitant Medications  unique USUBJID in ADCM where ADCM.CONTRTFL='Y'"
      Count expression with dataset and filter. Unambiguous.
      Confidence: HIGH.

    "Discontinuation Reason  ADSL.DCSREAS not missing"
      "not missing" null check pattern. Unambiguous.
      Confidence: HIGH.

    "Race, n (%)  ADSL.RACE"
      Even embedded in a display label with comma and parentheses — ADSL.RACE
      is structurally distinct from any English text.
      Confidence: HIGH.

**Core ADaM regex patterns (R syntax):**

``` r

ADAM_DS  <- "AD[A-Z]{1,6}"
ADAM_VAR <- "[A-Z][A-Z0-9]{0,7}"

PATTERN_SIMPLE  <- paste0("\\b", ADAM_DS, "\\.", ADAM_VAR, "\\b")
PATTERN_EQ      <- paste0("\\b", ADAM_DS, "\\.", ADAM_VAR, "\\s*=\\s*'[^']*'")
PATTERN_ARS     <- paste0("\\b", ADAM_DS, "\\.", ADAM_VAR,
                           "\\s+(?:EQ|NE|IN|NOTIN|GT|GE|LT|LE)\\s+'[^']*'")
PATTERN_COUNT   <- paste0("(?i)unique\\s+USUBJID\\s+in\\s+", ADAM_DS)
PATTERN_WHERE   <- paste0("\\b", ADAM_DS, "\\.", ADAM_VAR,
                           "\\s+(?i:where)\\s+", ADAM_VAR, "\\s*=\\s*'[^']*'")
PATTERN_NULL    <- paste0("\\b", ADAM_DS, "\\.", ADAM_VAR,
                           "\\s+(?i:not\\s+(?:null|missing))")

ADAM_ANNOTATION_PATTERN <- paste(
  PATTERN_SIMPLE, PATTERN_EQ, PATTERN_ARS,
  PATTERN_COUNT,  PATTERN_WHERE, PATTERN_NULL,
  sep = "|"
)
```

**Splitting label from annotation when no colour available:**

When the cell contains both a stub row label and an ADaM annotation in
plain text, the separator is typically one of the following (check in
order):

1.  **Two or more spaces:** `"Age (years) ADSL.AGE"` — most common
    convention
2.  **Newline:** `"Age (years)\nADSL.AGE"` — some shells use line breaks
3.  **Tab character:** `"Age (years)\tADSL.AGE"`
4.  **Square brackets:** `"Age (years) [ADSL.AGE]"` — bracket-enclosed
    annotation
5.  **Parentheses wrapping ADaM text:** `"Age (years) (ADSL.AGE)"` —
    distinguish from `"(years)"` by checking whether the parenthesised
    content matches the ADaM pattern
6.  **No separator — annotation starts immediately after label:** Less
    common but occurs when the cell contains only the ADaM reference
    with no display label (e.g. a dedicated annotation column)

``` r

split_label_annotation <- function(cell_text) {
  # Find position of first ADaM pattern match
  m <- regexpr(ADAM_ANNOTATION_PATTERN, cell_text, perl = TRUE)
  if (m == -1) return(list(label = trimws(cell_text), annotation = ""))

  annotation_start <- m[1]
  before_match     <- substr(cell_text, 1, annotation_start - 1)

  # Strip trailing separator characters to get clean label
  label      <- trimws(gsub("\\s*[\\[\\(]?\\s*$", "", before_match))

  # Extract full annotation (may extend beyond single match — e.g. compound OR)
  annotation <- trimws(substr(cell_text, annotation_start, nchar(cell_text)))
  annotation <- gsub("[\\]\\)]\\s*$", "", annotation)  # remove closing bracket if any

  list(label = label, annotation = annotation)
}
```

**When a cell contains ONLY an ADaM reference with no label text:**

Some shells use a dedicated annotation column where cells contain
nothing but `ADSL.AGE` or `ADSL.SAFFL='Y'`. In this case
`split_label_annotation` returns `label = ""` and
`annotation = "ADSL.AGE"`. The label is taken from the stub column of
the same row instead.

**False positive guard — do not flag these as annotations:**

    "n"             — single letter, does not match ADAM_DS.ADAM_VAR
    "Mean (SD)"     — no dot-separated uppercase token
    "Q1, Q3"        — no ADaM dataset prefix
    "xxx"           — placeholder, explicitly excluded
    "[a]"           — footnote marker, content does not match ADaM pattern
    "N=125"         — sample size, not a variable reference

Apply `ADAM_ANNOTATION_PATTERN` strictly — if the text does not match,
it is not an annotation regardless of its position in the cell.

**Confidence: HIGH** when the match is a full `DATASET.VARIABLE`
reference (Patterns 1–6 above). The structural uniqueness of ADaM naming
means a plain-text match is as reliable as a colour match.

------------------------------------------------------------------------

#### Layer 4 — LLM Detection (Fallback for Ambiguous Cases)

When Layers 1–3 produce no confident annotation for a cell that
contextually should have one.

**Trigger conditions:** - Cell text contains a partial ADaM match
(e.g. `"SAFFL='Y'"` without dataset prefix) - Coloured text does NOT
match ADaM pattern (non-standard annotation format) - Cell appears to be
an annotated parent row but no annotation was extracted - Cell contains
shorthand like `"see ADaM spec"`, `"per spec"`, `"from DM.AGE"`,
`"derived"`

**Output:** Annotation string + `detection_method="llm_inference"` +
`detection_confidence="low"`. Always included in the validation report
for programmer review.

------------------------------------------------------------------------

#### Detection Priority Table

| Layer | Method | Trigger | Confidence | Action |
|----|----|----|----|----|
| 1a | Coloured run + ADaM pattern | C00000 + regex match | HIGH | Use directly |
| 1b | Coloured run without ADaM pattern | C00000 but no ADSL.X match | MEDIUM | Pass to LLM |
| 2 | Bold/italic/underline + ADaM pattern | Formatted run + regex | MEDIUM | Use with `detection_method="format"` |
| 3a | Plain text — full `DATASET.VARIABLE` match | No formatting; unambiguous ADaM reference | **HIGH** | Use directly with `detection_method="pattern"` |
| 3b | Plain text — partial or abbreviated match | No dataset prefix, e.g. `"SAFFL='Y'"` | MEDIUM | Use with `detection_method="pattern"`, flag for review |
| 4 | LLM inference | No formatting, no pattern, contextually expected | LOW | Use with `detection_method="llm_inference"`, flag for review |
| — | No detection | No evidence at any layer | NONE | Cell has no annotation |

**Important:** Layer 3a (plain-text full ADaM reference) is HIGH
confidence — equal to Layer 1a — because the `AD[A-Z]+.[A-Z0-9]+`
structure cannot occur in natural-language stub row labels. A shell
without any formatting is just as parseable as one with red text.

All detections at MEDIUM confidence or below appear in the validation
report for QC programmer review.

------------------------------------------------------------------------

#### Common Alternative Annotation Styles

**Bracket style:**

    "Age (years) [ADSL.AGE]"            →  label="Age (years)",  annotation="ADSL.AGE"
    "Screen Failure [ADSL.SCRFFL='Y']"  →  annotation="ADSL.SCRFFL='Y'"

Detection: `\\[([^\\]]+)\\]` — extract bracket content, apply ADaM
pattern.

**Separate annotation column:** Some shells have 3+ columns where column
1 (or the last non-data column) contains only annotations. Detect by
checking if a non-stub column has no placeholder values (`xxx`, `xx.x`)
and most non-empty cells match the ADaM pattern.

``` r

is_annotation_column <- function(col_cells) {
  no_placeholders <- !any(grepl("^x+(?:\\.x+)?$", col_cells, ignore.case = TRUE))
  mostly_adam     <- mean(grepl(ADAM_ANNOTATION_PATTERN,
                                col_cells[nchar(col_cells) > 0])) > 0.5
  no_placeholders && mostly_adam
}
```

**Footnote-linked annotation:** After extracting footnotes, check each
for ADaM patterns. If footnote `[a]` contains an ADaM reference, link it
back to rows bearing `[a]`. Flag as `detection_method="footnote_link"`.

------------------------------------------------------------------------

## Part 2 — ADaM Spec Format

The ADaM specification Excel typically has a sheet called
`"Variable Level"`, `"Variables"`, or similar. Parse all sheets and
identify the variable-level sheet.

**Expected columns (flexible — handle missing/renamed):**

| Canonical name | Common aliases                     |
|----------------|------------------------------------|
| `Dataset`      | `dataset`, `DATASET`               |
| `Variable`     | `variable`, `VAR`, `Variable Name` |
| `Label`        | `label`, `LABEL`, `Variable Label` |
| `Type`         | `type`, `DATA TYPE`, `DataType`    |
| `Origin`       | `origin`                           |
| `Codelist`     | `codelist`, `CODELIST`             |

**Output:** Named list indexed by `"DATASET.VARIABLE"` for O(1) lookup.

``` r

spec_lookup <- list(
  "ADSL.AGE"     = list(dataset="ADSL", variable="AGE",    label="Age",                    type="Num", origin="CRF"),
  "ADSL.SAFFL"   = list(dataset="ADSL", variable="SAFFL",  label="Safety Population Flag", type="Char"),
  "ADAE.TRTEMFL" = list(dataset="ADAE", variable="TRTEMFL",label="Treatment Emergent Flag", type="Char")
)
```

------------------------------------------------------------------------

## Part 3 — Annotation Validation (Cross-Reference)

After extracting all annotations from the shell, validate each against
the ADaM spec.

**For each extracted annotation string:**

1.  Parse to extract variable references:

    - `"ADSL.AGE"` → check `"ADSL.AGE"` in spec_lookup
    - `"ADSL.SCRFFL='Y'"` → extract `"ADSL.SCRFFL"`, check in
      spec_lookup
    - `"ADSL.SFENRLFL='Y' or ADSL.WTHTYP='Withdrawal'"` → extract both,
      check each
    - `"unique USUBJID in ADCM where ADCM.CONTRTFL='Y'"` → extract
      `"ADCM.USUBJID"` and `"ADCM.CONTRTFL"`

2.  Validation outcomes:

    - `PASS` → variable found in spec exactly
    - `WARN` → dataset found but variable not — possible typo or not yet
      in spec
    - `FAIL` → dataset not found

3.  Build a validation report data frame:
    `analysis_id, annotation, variable_ref, status, message`

4.  Proceed with ARS JSON generation regardless — warnings do not block
    output.

------------------------------------------------------------------------

## Part 4 — LLM Role

### 4.1 LLM for Annotation Detection (Layer 4 Fallback)

When Layers 1–3 fail on a cell that contextually should have an
annotation. Output includes `detection_method="llm_inference"` and
`detection_confidence="low"` for programmer review.

### 4.2 LLM for Semantic Enrichment

**Required for:**

1.  **Analysis type determination** — classify TLF as `CONTINUOUS` /
    `CATEGORICAL` / `SURVIVAL` / `AE_FREQUENCY` / `FIGURE` / `LISTING`
    from title + stub row structure.
2.  **Method name derivation** — determine ARS AnalysisMethod name from
    analysis type and stub rows.
3.  **Complex annotation parsing** — for
    `"unique USUBJID in ADCM where ADCM.CONTRTFL='Y'"`, extract dataset,
    variable, DataSubset condition.
4.  **DataSubset construction** — annotations with `where`, `=`, `IN`,
    `and`, `or` imply DataSubset conditions.
5.  **Variable role identification** — `"ADSL.DCSREAS"` on row
    `"Reason for Study Discontinuation"` is a grouping variable, not an
    analysis value variable.
6.  **Abbreviated annotation resolution** — `"AGE"` without dataset
    prefix → resolve to `"ADSL.AGE"` using source_datasets context.

### 4.3 LLM is NOT Required For

- Colour run extraction (deterministic, `officer`)
- Population flag parsing (regex)
- Source dataset extraction (regex)
- ADaM pattern detection in plain text (regex, Layer 3)
- Spec lookup validation (exact string match)
- Bracket/parenthesis annotation extraction (regex)

### 4.4 LLM Call Structure

**One call per TLF section.** Never one call per row.

------------------------------------------------------------------------

## Part 5 — ARS JSON Construction

### 5.1 Object Mapping

| Shell element | ARS object | Key fields |
|----|----|----|
| Population annotation | `AnalysisSet` | `id`, `name`, `condition` |
| Complex population (`and`) | `AnalysisSet` | `compoundExpression` |
| Column header treatment arms | `GroupingFactor` + `[Group]` | `groupingVariable=TRT01A` |
| Stub row WITH annotation | `Analysis` | `dataset`, `variable`, `analysisSetId`, `methodId` |
| Annotation with `where` clause | `DataSubset` | `condition` |
| TLF number + title | `Output` | `id`, `name` |
| Table title | `OutputDisplay` | `displayTitle` |
| Footnote lines | `DisplaySection` | `sectionType=Footnote`, `text` |
| Analysis type → method | `AnalysisMethod` | `name`, `[Operation]` |

### 5.2 ID Generation Convention

Deterministic IDs from content — not random UUIDs.

``` r

as_id  <- paste0("AS_", toupper(gsub("[^A-Za-z0-9]", "_", population_name)))
gf_id  <- paste0("GF_", toupper(gsub("[^A-Za-z0-9]", "_", by_variable)))
mth_id <- paste0("MTH_", toupper(gsub("[^A-Za-z0-9 ]", "", method_name)) |> gsub(" ", "_", x=_))
an_id  <- paste0("AN_", toupper(gsub("[^A-Za-z0-9]", "_", tlf_number)),
                 "_", sprintf("%03d", analysis_index))
out_id <- toupper(gsub("[. ]", "_", tlf_number))
```

### 5.3 Standard AnalysisMethod Definitions

Pre-define these five methods — they cover \>90% of clinical trial
tables:

``` json
[
  {
    "id": "MTH_SUMMARY_STATISTICS_CONTINUOUS",
    "name": "Summary Statistics - Continuous",
    "description": "n, mean, SD, median, Q1, Q3, min, max",
    "operations": [
      {"id": "OP_N",      "name": "n",      "order": 1, "resultPattern": "XXX"},
      {"id": "OP_MEAN",   "name": "Mean",   "order": 2, "resultPattern": "XXX.X"},
      {"id": "OP_SD",     "name": "SD",     "order": 3, "resultPattern": "XXX.XX"},
      {"id": "OP_MEDIAN", "name": "Median", "order": 4, "resultPattern": "XXX.X"},
      {"id": "OP_Q1",     "name": "Q1",     "order": 5, "resultPattern": "XXX.X"},
      {"id": "OP_Q3",     "name": "Q3",     "order": 6, "resultPattern": "XXX.X"},
      {"id": "OP_MIN",    "name": "Min",    "order": 7, "resultPattern": "XXX"},
      {"id": "OP_MAX",    "name": "Max",    "order": 8, "resultPattern": "XXX"}
    ]
  },
  {
    "id": "MTH_COUNT_AND_PERCENTAGE",
    "name": "Count and Percentage",
    "description": "n (%)",
    "operations": [
      {"id": "OP_N",     "name": "Count",       "order": 1, "resultPattern": "XXX"},
      {"id": "OP_PCT",   "name": "Percentage",  "order": 2, "resultPattern": "XX.X"},
      {"id": "OP_DENOM", "name": "Denominator", "order": 3, "resultPattern": "XXX"}
    ]
  },
  {
    "id": "MTH_SUBJECT_COUNT",
    "name": "Subject Count",
    "description": "Unique subject count",
    "operations": [
      {"id": "OP_N", "name": "n", "order": 1, "resultPattern": "XXX"}
    ]
  },
  {
    "id": "MTH_KM_ESTIMATE",
    "name": "Kaplan-Meier Estimate",
    "description": "KM event rate, median survival, confidence interval",
    "operations": [
      {"id": "OP_EVENTS",  "name": "Events",         "order": 1, "resultPattern": "XXX"},
      {"id": "OP_MEDIAN",  "name": "Median (months)", "order": 2, "resultPattern": "XXX.X"},
      {"id": "OP_CI_LOW",  "name": "95% CI Lower",   "order": 3, "resultPattern": "XXX.X"},
      {"id": "OP_CI_HIGH", "name": "95% CI Upper",   "order": 4, "resultPattern": "XXX.X"}
    ]
  },
  {
    "id": "MTH_AE_FREQUENCY",
    "name": "AE Frequency Count",
    "description": "Unique subjects with event, n (%)",
    "operations": [
      {"id": "OP_N",   "name": "n",   "order": 1, "resultPattern": "XXX"},
      {"id": "OP_PCT", "name": "(%)", "order": 2, "resultPattern": "XX.X"}
    ]
  }
]
```

------------------------------------------------------------------------

## Part 6 — Function Specification

### `spec_to_ars()`

``` r

#' Convert Annotated TLF Shell and ADaM Spec to CDISC ARS JSON
#'
#' @param shell_path      Path to annotated TLF shells Word document (.docx).
#' @param adam_spec_path  Path to ADaM specification Excel file (.xlsx or .xls).
#' @param output_path     Path for the output ARS JSON file. Default "reporting_event.json".
#' @param study_id        Study identifier. Default "STUDY-001".
#' @param study_name      Human-readable study name for the ReportingEvent.
#' @param model           Anthropic model. Default "claude-sonnet-4-5".
#' @param api_key         Anthropic API key. Reads from ANTHROPIC_API_KEY env var by default.
#' @param validate        If TRUE (default), cross-reference annotations against the ADaM spec.
#' @param report_path     Path for the validation report Excel. Default "spec_validation_report.xlsx".
#' @param verbose         Print progress. Default TRUE.
#'
#' @return Invisibly returns a named list:
#'   \describe{
#'     \item{ars_path}{Path to the generated ARS JSON file.}
#'     \item{report_path}{Path to the validation report (if validate=TRUE).}
#'     \item{n_tlfs}{Number of TLF sections processed.}
#'     \item{n_analyses}{Number of ARS Analysis objects created.}
#'     \item{n_warnings}{Number of spec validation warnings.}
#'   }
#'
#' @export
spec_to_ars <- function(
    shell_path,
    adam_spec_path,
    output_path  = "reporting_event.json",
    study_id     = "STUDY-001",
    study_name   = NULL,
    model        = "claude-sonnet-4-6",
    api_key      = Sys.getenv("ANTHROPIC_API_KEY"),
    validate     = TRUE,
    report_path  = "spec_validation_report.xlsx",
    verbose      = TRUE
) { ... }
```

------------------------------------------------------------------------

## Part 7 — Internal Functions

Build in this order. Each depends on the previous.

### `R/parse_shell_docx.R`

`split_label_annotation()` lives here as an **unexported helper** — it
has no use outside this file and should not appear in the package
namespace.

``` r

parse_shell_docx(docx_path)
# Uses officer::read_docx()
# Returns list of TLF section objects:
# list(
#   tlf_number        = "T-14-1-1",
#   tlf_type          = "TABLE",         # TABLE / FIGURE / LISTING
#   title             = "Subject Disposition",
#   population_text   = "Safety Population",
#   population_annot  = "ADSL.SAFFL='Y'",
#   footnotes         = c("[a] ...", "[b] ..."),
#   source_datasets   = c("ADSL", "ADAE"),
#   col_headers       = c("PROSVALIN 0.5 mcg\n(N=125)", "Placebo\n(N=125)"),
#   n_data_cols       = 2,
#   stub_rows = list(
#     list(label="Age (years)",    annotation="ADSL.AGE",        has_annot=TRUE,
#          detection_method="colour", detection_confidence="high"),
#     list(label="n",              annotation="",                has_annot=FALSE,
#          detection_method=NA,       detection_confidence=NA),
#     list(label="Screen Failure", annotation="ADSL.SCRFFL='Y'", has_annot=TRUE,
#          detection_method="colour", detection_confidence="high")
#   )
# )
#
# Key logic:
# 1. Walk body elements (paragraphs + tables) in document order
# 2. Detect TLF heading with TLF_HEADING_REGEX
# 3. Skip TOC table (first table with "Number" in first cell)
# 4. For each TLF section, collect: title para, population para, footnote paras
# 5. For the first table after the heading:
#    a. Row 0 = column headers (extract cell text)
#    b. Rows 1+ = stub rows (col 0 only)
#    c. Apply detection layers 1-4 per cell
```

### `R/parse_adam_spec.R`

``` r

parse_adam_spec(excel_path)
# Uses readxl::read_excel() on all sheets
# Identifies variable-level sheet by presence of "Variable" and "Dataset" columns
# Returns:
# list(
#   variables = data.frame(dataset, variable, label, type, origin, codelist),
#   lookup    = named list "DATASET.VARIABLE" -> row info
# )
```

### `R/extract_annotation_vars.R`

``` r

extract_annotation_vars(annotation_string)
# Parses annotation string → all "DATASET.VARIABLE" references
# "ADSL.AGE"                               → c("ADSL.AGE")
# "ADSL.SCRFFL='Y'"                         → c("ADSL.SCRFFL")
# "ADSL.SFENRLFL='Y' or ADSL.WTHTYP='...'" → c("ADSL.SFENRLFL", "ADSL.WTHTYP")
# "unique USUBJID in ADCM where CONTRTFL"   → c("ADCM.USUBJID", "ADCM.CONTRTFL")
```

### `R/validate_annotations.R`

``` r

validate_annotations(tlf_sections, spec_lookup)
# For each annotation in each TLF section:
#   1. Call extract_annotation_vars()
#   2. Look up each in spec_lookup
#   3. Build validation report data frame
# Returns data frame: tlf_number, stub_label, annotation, variable_ref, status, message
```

### `R/enrich_with_llm.R`

``` r

enrich_with_llm(tlf_section, spec_lookup, model, api_key)
# ONE LLM call per TLF section — never one per row
# Uses ellmer::chat_anthropic() — do NOT use raw httr2 here
# Input JSON to LLM:
# {
#   "tlf_number": "T-14-1-1",
#   "tlf_type": "TABLE",
#   "title": "Subject Demographics and Baseline Characteristics",
#   "population": "Safety Population",
#   "population_annotation": "ADSL.SAFFL='Y'",
#   "col_headers": ["PROSVALIN 0.5 mcg (N=125)", "Placebo (N=125)"],
#   "annotated_rows": [
#     {"label": "Age at Informed Consent (years)", "annotation": "ADSL.AGE"},
#     ...
#   ],
#   "available_variables": ["ADSL.AGE", "ADSL.AGEGR1", ...]
# }
#
# LLM JSON output:
# {
#   "analysis_type": "CONTINUOUS",
#   "ars_method_name": "Summary Statistics - Continuous",
#   "by_variable": "TRT01A",
#   "row_enrichments": [
#     {
#       "label": "Age at Informed Consent (years)",
#       "primary_dataset": "ADSL",
#       "primary_variable": "AGE",
#       "data_subset": null,
#       "variable_role": "ANALYSIS"
#     },
#     ...
#   ]
# }
```

### `R/build_ars_json.R`

``` r

build_ars_json(tlf_sections_enriched, study_id, study_name)
# 1. Deduplicate AnalysisSets (same population across TLFs → one AnalysisSet)
# 2. Deduplicate GroupingFactors
# 3. Deduplicate AnalysisMethods
# 4. Build one Analysis per annotated stub row per TLF
# 5. Build one Output per TLF
# 6. Assemble and serialise ReportingEvent
#    jsonlite::toJSON(pretty=TRUE, auto_unbox=TRUE)
```

------------------------------------------------------------------------

## Part 8 — LLM Prompt for enrich_with_llm()

Store in `inst/prompts/enrich_tlf_prompt.txt`:

    You are an expert CDISC ADaM and ARS programmer. You are given a parsed TLF shell
    section from a clinical trial. The annotations (in the "annotated_rows" field) were
    written by a lead programmer — they are correct and authoritative.
    Do NOT invent new variable names. Only use variables present in "available_variables".

    TLF Section:
    {tlf_json}

    Return a JSON object with these fields:

    "analysis_type": One of CONTINUOUS, CATEGORICAL, SURVIVAL, AE_FREQUENCY, FIGURE, LISTING
      - CONTINUOUS: n, mean, SD, median stats on a numeric variable
      - CATEGORICAL: frequency counts n(%) of category values
      - SURVIVAL: Kaplan-Meier time-to-event analysis
      - AE_FREQUENCY: adverse event counts by SOC/PT hierarchy
      - FIGURE: graphical output
      - LISTING: subject-level data rows

    "ars_method_name": Choose the closest match:
      "Summary Statistics - Continuous" | "Count and Percentage" | "Subject Count" |
      "Kaplan-Meier Estimate" | "AE Frequency Count" | "Listing"

    "by_variable": ADaM variable used for treatment group columns. Use "TRT01A" unless
      inferred otherwise from column headers.

    "row_enrichments": Array with one entry per annotated_row:
      {
        "label":            stub_row label exactly as given,
        "primary_dataset":  dataset portion of annotation (e.g. "ADSL"),
        "primary_variable": variable portion (e.g. "AGE"),
        "data_subset":      if annotation contains a WHERE clause beyond the population flag:
                            {"dataset":"X","variable":"Y","comparator":"EQ","value":["Z"]}
                            Otherwise null.
        "variable_role":    "ANALYSIS" | "GROUPING" | "COUNT" | "FLAG"
      }

    Return ONLY valid JSON. No text outside the JSON block.

------------------------------------------------------------------------

## Part 9 — File Structure

    R/
      spec_to_ars.R                  ← exported function
      parse_shell_docx.R             ← deterministic Word parsing (officer)
      parse_adam_spec.R              ← ADaM spec Excel parsing (readxl)
      extract_annotation_vars.R      ← annotation string → variable refs
      validate_annotations_spec.R   ← cross-reference with spec
      enrich_with_llm.R             ← one LLM call per TLF (httr2 + Anthropic API)
      build_ars_json.R               ← assemble ARS objects → JSON (jsonlite)
      write_validation_report.R      ← validation report Excel (openxlsx2)
      utils_ars_ids.R                ← deterministic ID generation
      utils_where_clause.R           ← annotation string → ARS WhereClause object

    inst/
      prompts/
        enrich_tlf_prompt.txt
      extdata/
        adam_standard_variables.csv
        example_study/
          annotated_shell_minimal.docx
          adam_spec_minimal.xlsx

    tests/testthat/
      fixtures/
        APX-DRM-301_TLF_Shells_v1.0_sample_annotated.docx  ← PRIMARY fixture (real CRO document,
                                                               read-only copy; tests real formatting)
        annotated_shell_2tlf_minimal.docx  ← synthetic 2-TLF fixture (demographics + AE summary)
                                              for unit tests that need a predictable minimal document
        adam_spec_10vars.xlsx               ← 10 variables covering both synthetic TLFs
      test-parse_shell_docx.R              ← unit tests use synthetic fixture
      test-parse_adam_spec.R
      test-extract_annotation_vars.R
      test-annotation_detection_layers.R
      test-spec_to_ars_integration.R       ← integration / smoke test uses real APX-DRM-301 fixture

**Key R package dependencies:**

| Package | Purpose |
|----|----|
| `officer` | Read Word docx, extract runs with font colour |
| `readxl` | Parse ADaM spec Excel |
| `jsonlite` | Serialise ARS JSON |
| `ellmer` | Anthropic API calls (Posit-endorsed SDK; already installed) — do NOT use raw `httr2` for LLM calls |
| `openxlsx2` | Write validation report (modern rewrite of openxlsx — use this, not `openxlsx`) |
| `cli` | Progress messages and errors |

------------------------------------------------------------------------

## Part 10 — Tests

### `test-parse_shell_docx.R`

``` r

test_that("parse_shell_docx extracts 2 TLF sections from fixture", {
  sections <- parse_shell_docx(test_path("fixtures/annotated_shell_2tlf.docx"))
  expect_equal(length(sections), 2)
})

test_that("population annotation extracted correctly — simple flag", {
  sections <- parse_shell_docx(test_path("fixtures/annotated_shell_2tlf.docx"))
  expect_equal(sections[[1]]$population_annot, "ADSL.SAFFL='Y'")
})

test_that("stub rows with annotations are flagged has_annot=TRUE", {
  sections <- parse_shell_docx(test_path("fixtures/annotated_shell_2tlf.docx"))
  annotated <- Filter(function(r) r$has_annot, sections[[1]]$stub_rows)
  expect_true(length(annotated) > 0)
})

test_that("child rows (n, Mean, SD) have has_annot=FALSE", {
  sections <- parse_shell_docx(test_path("fixtures/annotated_shell_2tlf.docx"))
  child_rows <- Filter(function(r) r$label %in% c("n", "Mean (SD)", "Median"),
                       sections[[1]]$stub_rows)
  expect_true(all(sapply(child_rows, function(r) !r$has_annot)))
})

test_that("source datasets extracted from grey Source line", {
  sections <- parse_shell_docx(test_path("fixtures/annotated_shell_2tlf.docx"))
  expect_true("ADSL" %in% sections[[1]]$source_datasets)
})
```

### `test-extract_annotation_vars.R`

``` r

test_that("simple DATASET.VARIABLE extracted", {
  expect_equal(extract_annotation_vars("ADSL.AGE"), "ADSL.AGE")
})

test_that("flag condition — variable extracted without value", {
  expect_equal(extract_annotation_vars("ADSL.SAFFL='Y'"), "ADSL.SAFFL")
})

test_that("compound OR — both variables extracted", {
  result <- extract_annotation_vars("ADSL.SFENRLFL='Y' or ADSL.WTHTYP='Withdrawal'")
  expect_true("ADSL.SFENRLFL" %in% result)
  expect_true("ADSL.WTHTYP" %in% result)
})

test_that("count expression — dataset and variable extracted", {
  result <- extract_annotation_vars("unique USUBJID in ADCM where ADCM.CONTRTFL='Y'")
  expect_true("ADCM.USUBJID" %in% result)
  expect_true("ADCM.CONTRTFL" %in% result)
})
```

### `test-annotation_detection_layers.R`

``` r

test_that("Layer 3 detects ADSL.AGE in plain text with no colour", {
  result <- split_label_annotation("Age at Informed Consent (years)  ADSL.AGE")
  expect_equal(result$label, "Age at Informed Consent (years)")
  expect_equal(result$annotation, "ADSL.AGE")
})

test_that("Layer 3 detects flag condition in plain text", {
  result <- split_label_annotation("Screen Failure  ADSL.SCRFFL='Y'")
  expect_equal(result$annotation, "ADSL.SCRFFL='Y'")
})

test_that("Layer 3 detects bracket-enclosed annotation", {
  result <- split_label_annotation("Age (years) [ADSL.AGE]")
  expect_equal(result$annotation, "ADSL.AGE")
  expect_equal(result$label, "Age (years)")
})

test_that("Layer 3 does not falsely detect plain English as annotation", {
  result <- split_label_annotation("Number of subjects enrolled")
  expect_equal(result$annotation, "")
})

test_that("statistical sub-rows are never flagged as annotations", {
  for (label in c("n", "Mean (SD)", "Median", "Q1, Q3", "Min, Max")) {
    result <- split_label_annotation(label)
    expect_equal(result$annotation, "", info = paste("Failed for:", label))
  }
})

test_that("detection_method is 'colour' for C00000 runs", {
  sections    <- parse_shell_docx(test_path("fixtures/annotated_shell_2tlf.docx"))
  annotated   <- Filter(function(r) r$has_annot, sections[[1]]$stub_rows)
  colour_rows <- Filter(function(r) r$detection_method == "colour", annotated)
  expect_true(length(colour_rows) > 0)
})

test_that("detection_confidence is 'high' for colour+pattern matches", {
  sections  <- parse_shell_docx(test_path("fixtures/annotated_shell_2tlf.docx"))
  annotated <- Filter(function(r) r$has_annot, sections[[1]]$stub_rows)
  high_conf <- Filter(function(r) r$detection_confidence == "high", annotated)
  expect_true(length(high_conf) > 0)
})
```

------------------------------------------------------------------------

## Part 11 — Build Order

Build strictly in this sequence:

1.  Create package scaffold with `usethis::create_package()`
2.  `utils_ars_ids.R` — ID generation helpers
3.  `utils_where_clause.R` — annotation string → WhereClause
4.  `extract_annotation_vars.R` — with tests
5.  `parse_adam_spec.R` — with tests using minimal xlsx fixture
6.  `parse_shell_docx.R` — with tests using minimal docx fixture
7.  `validate_annotations_spec.R` — with tests
8.  `inst/prompts/enrich_tlf_prompt.txt`
9.  `enrich_with_llm.R` — with mocked LLM response for tests
10. `build_ars_json.R` — with tests against known input
11. `write_validation_report.R`
12. `spec_to_ars.R` — integration test using real fixtures
13. `devtools::check()` — must pass 0 errors, 0 warnings

------------------------------------------------------------------------

## Key Constraints

- Do NOT call the LLM for anything that can be done deterministically
  (colour extraction, regex, Excel parsing).
- The LLM receives ONLY already-extracted structured data — never raw
  docx bytes.
- All variables in the ARS output must trace to a red annotation in the
  shell. Never infer variables from title text or stub row labels alone.
- If `validate=TRUE` and warnings exist, print a
  [`cli::cli_warn()`](https://cli.r-lib.org/reference/cli_abort.html)
  summary but still write the JSON.
- Use
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html)
  for file-not-found, wrong extension, missing API key.
- Follow tidy conventions: `|>` base pipe, `cli::` for messages, no
  [`print()`](https://rdrr.io/r/base/print.html).
