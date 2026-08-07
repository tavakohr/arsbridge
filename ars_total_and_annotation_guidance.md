# Correct Handling of Total Columns and Annotations in ARS and ARD Workflows

## 1. Purpose

This document explains a common issue that can occur when an annotated
table shell is converted into Analysis Results Standard (ARS) metadata
and then used to generate Analysis Results Data (ARD) or a review
display.

The issue occurs when the metadata for a **Total column** is missing,
incomplete, or assigned to the wrong part of the table. A related issue
occurs when a column-level annotation is confused with a row-level
analysis instruction. This can cause the Total values to be missing from
the ARD or cause annotations in the review application to look different
from the original annotated shell.

This document provides a general method to identify and correct these
issues.

## 2. General Scenario

A typical summary table may contain several analysis columns:

- Group 1
- Group 2
- Group 3
- Total

Each group column represents a subset of subjects or records. The Total
column normally represents all applicable groups combined.

The table may also contain rows such as:

- Subjects included in the analysis
- Subjects meeting a condition
- Subjects with an event
- Reason for discontinuation

The column definition and the row definition serve different purposes:

- A **column annotation** defines the population or group represented by
  a column.
- A **row annotation** defines the analysis performed for a specific
  row.

These two annotation types must remain separate throughout shell
extraction, ARS creation, ARS review, ARD execution, and final
rendering.

## 3. Main Issue

The Total column may appear in the shell but may not be correctly
represented in the ARS metadata. As a result:

- The generated ARD may contain results for individual groups but not
  for Total.
- The final rendered table may show placeholders for Total.
- Total may be available for some analyses but missing for others.
- A review application may display a row calculation beside the Total
  column label.
- The annotation shown in the application may not match the annotation
  in the original annotated shell.

This usually indicates that the Total column was detected as visible
text but was not created as a valid analysis group, or that the
annotation was attached at the wrong metadata level.

## 4. Difference Between Column-Level and Row-Level Annotations

### 4.1 Column-Level Annotation

A column-level annotation defines which subjects or records belong to
the displayed column.

Example:

``` text
[ADSL.GROUPN = 1]
```

``` text
[ADSL.GROUPN = 2]
```

``` text
[ADSL.GROUPN IN (1,2)]
```

The third condition can represent Total when Total includes both groups.

A column-level annotation should normally be stored in ARS as part of an
analysis grouping or group definition.

### 4.2 Row-Level Annotation

A row-level annotation defines what should be calculated for a row.

Example:

``` text
[ADSL.FLAG = "Y"; count distinct USUBJID]
```

This instruction means that the analysis should filter records using the
stated condition and count distinct subjects.

A row-level annotation should normally be represented by the analysis
dataset, data subset, analysis method, and statistic metadata associated
with the row analysis.

### 4.3 Why the Separation Matters

The following display is not appropriate:

``` text
Total (N=XX) [ADSL.FLAG="Y"; count distinct USUBJID]
```

The text combines a column label with a row-specific calculation. The
Total column should define the group scope, while the row should define
the calculation.

A better separation is:

``` text
Column label: Total (N=XX)
Column condition: ADSL.GROUPN IN (1,2)
```

``` text
Row label: Subjects meeting the condition, n
Row condition: ADSL.FLAG = "Y"
Statistic: Count distinct USUBJID
```

## 5. Expected Metadata Structure

A general grouping structure can be represented as follows:

| Display column | Group label | Group condition |
|----|----|----|
| Group 1 | Group 1 | `ADSL.GROUPN = 1` |
| Group 2 | Group 2 | `ADSL.GROUPN = 2` |
| Total | Total | `ADSL.GROUPN IN (1,2)` or no additional group restriction |

Whether Total should have an explicit condition depends on the intended
population and the execution design.

### Option A: Explicit Total Condition

Use an explicit condition when Total must include a defined set of
groups:

``` text
ADSL.GROUPN IN (1,2)
```

This approach makes the intended scope clear and is useful when some
group values must be excluded.

### Option B: No Additional Group Restriction

Use no group restriction when the analysis population already defines
the complete Total population.

For example, if the analysis population is already restricted to
eligible subjects, Total may be calculated by applying only the row
condition within that population.

The chosen method should be applied consistently and should match the
annotated shell.

## 6. Likely Causes

### 6.1 Total Header Detected Only as Display Text

The parser may detect the text `Total (N=XX)` but fail to create a
corresponding group object. In this case, ARS contains individual groups
but no Total group.

### 6.2 Annotation Assigned to the Wrong Cell or Role

A row-level annotation may be incorrectly assigned to the Total header.
This may occur when the parser reads adjacent cells, merged cells, or
flattened table text without preserving the original table structure.

### 6.3 Display Label and Annotation Not Separated

The parser may store the complete cell text as a single label:

``` text
Total (N=XX) [annotation]
```

Instead, the parser should create separate values:

``` text
display_label = "Total (N=XX)"
annotation = "[annotation]"
```

### 6.4 Total Group Not Linked to Analyses

A Total group may exist in the reporting event but may not be referenced
by the analyses or results. This can cause the group to appear in ARS
review while remaining absent from the ARD.

### 6.5 Inconsistent Annotation Extraction

The annotated shell may contain formatting such as font color, brackets,
multiple text runs, or merged cells. If the parser reconstructs the cell
incorrectly, the extracted annotation may differ from the original
annotation.

### 6.6 LLM Interpretation Replacing Source Annotation

An extraction workflow may use an LLM to interpret the meaning of a
cell. The interpretation may be useful for classification, but it should
not replace the authoritative annotation from the shell.

The original source annotation should remain traceable and should be
stored separately from any normalized or interpreted form.

## 7. What to Review in ARS

For each affected output, review the following areas.

### 7.1 Analysis Groupings

Confirm that:

- A grouping factor exists for the displayed group or treatment columns.
- Every displayed individual group has a group definition.
- Total has its own group definition when Total is expected as a result
  column.
- The Total condition includes the intended groups.
- The grouping dataset and grouping variable are correct.

### 7.2 Analyses

Confirm that:

- Every applicable analysis references the required grouping.
- Total is included in the expected result groups.
- Row conditions are stored with the row analysis, not with the column
  label.
- The statistic matches the shell, such as count, percentage, mean, or
  distinct-subject count.

### 7.3 Results or ARD Mapping

Confirm that:

- ARD contains records for each expected group.
- ARD contains records for Total.
- The Total group identifier matches the identifier defined in ARS.
- Result records can be linked back to the correct output, analysis,
  method, and group.

### 7.4 Display Metadata

Confirm that:

- The visible column label contains only display text.
- Bracketed annotations are not included in the final displayed label.
- The annotation shown in the review application matches the source
  annotation.
- Normalized metadata is clearly separated from the original annotation
  text.

## 8. Recommended Solution

### Step 1: Preserve the Original Cell Structure

During shell extraction, preserve:

- Row number
- Column number
- Cell text
- Text runs
- Font color or other annotation formatting
- Merged-cell information
- Original annotation text

Do not flatten the complete table before determining which annotation
belongs to which cell.

### Step 2: Separate Display Text From Annotation

For each cell, store at least two fields:

``` text
display_text
source_annotation
```

For example:

``` text
display_text = "Total (N=XX)"
source_annotation = "[ADSL.GROUPN IN (1,2)]"
```

The display text should be used for rendering. The source annotation
should be used for metadata construction and traceability.

### Step 3: Classify the Annotation Role

Classify each annotation as one of the following:

- Population-level annotation
- Column-level grouping annotation
- Row-level variable annotation
- Row-level filter annotation
- Statistic or count instruction
- Footnote or non-analysis text

The classification should use the cell location and table structure, not
only the annotation text.

An annotation found in a header cell should normally be evaluated as a
grouping annotation. An annotation found in a stub row should normally
be evaluated as a row analysis annotation.

### Step 4: Create the Total Group

When a Total column is present, create a Total group in the analysis
grouping metadata.

Conceptual example:

``` json
{
  "id": "GRP_TOTAL",
  "name": "Total",
  "label": "Total",
  "condition": "ADSL.GROUPN IN (1,2)"
}
```

The exact JSON structure must follow the ARS schema and the conventions
used by the implementation.

### Step 5: Link Total to Applicable Analyses

Confirm that the analyses used to produce table rows request results for
the Total group. Creating a Total group at the reporting-event level is
not sufficient if the execution logic never produces a Total result.

### Step 6: Keep Row Logic With the Row Analysis

Store the row-level filter and statistic with the row analysis.

Conceptual example:

``` text
Dataset: ADSL
Filter: FLAG = "Y"
Statistic: Count distinct USUBJID
Grouping: Group 1, Group 2, Total
```

The same row analysis can then be evaluated for each applicable group.

### Step 7: Validate ARS Against the Shell

The review process should compare:

- Source display label versus ARS display label
- Source annotation versus extracted annotation
- Extracted annotation versus normalized metadata
- Expected columns versus defined groups
- Expected groups versus generated ARD group values

Any difference should be reported as a warning or error before final
execution.

## 9. Suggested Validation Rules

The following automated checks can prevent the issue.

### Rule 1: Displayed Total Requires Metadata

If the shell contains a Total column, ARS must contain a Total group or
an explicitly documented method for producing Total.

### Rule 2: Total Must Produce Results

If Total is expected for an analysis, ARD must contain at least one
corresponding result record for the Total group.

### Rule 3: Header Annotation Must Not Become Row Logic

A header annotation should not contain or inherit row-specific
calculation text unless the source shell explicitly places that
instruction in the header.

### Rule 4: Row Annotation Must Not Become a Header Label

A row filter or statistic instruction should not be appended to a
visible column label.

### Rule 5: Source Annotation Must Be Preserved

The exact source annotation should be retained for comparison, even when
a normalized condition is also created.

Example:

``` text
source_annotation = "[ADSL.GROUPN IN (1, 2)]"
normalized_condition = "ADSL.GROUPN IN (1,2)"
```

### Rule 6: Group Coverage Must Be Complete

The set of expected display groups should match the set of generated ARD
groups, subject to documented exceptions.

### Rule 7: Total Scope Must Be Unambiguous

The metadata should clearly state whether Total means:

- All displayed groups
- All subjects in the analysis population
- Selected displayed groups
- A separate overall category from the source data

## 10. Recommended Review Display

A review application should show source and interpreted information
separately.

### Column Review

``` text
Display label: Total (N=XX)
Source annotation: [ADSL.GROUPN IN (1,2)]
Metadata role: Column grouping
Normalized condition: ADSL.GROUPN IN (1,2)
```

### Row Review

``` text
Display label: Subjects meeting the condition, n
Source annotation: [ADSL.FLAG="Y"; count distinct USUBJID]
Metadata role: Row analysis
Dataset: ADSL
Filter: FLAG = "Y"
Statistic: Count distinct USUBJID
```

This design allows the reviewer to see whether the application has
interpreted the annotation correctly without changing the original
source text.

## 11. Testing Recommendations

Include tests for the following scenarios:

1.  Two groups plus Total.
2.  Three groups plus Total.
3.  Total with an explicit `IN` condition.
4.  Total with no additional group condition.
5.  A row with a distinct-subject count.
6.  A row with a count and percentage.
7.  A header annotation and row annotation in the same table.
8.  An annotation stored in colored text runs.
9.  A bracketed annotation in plain text.
10. Merged header cells.
11. A Total column that should apply only to selected groups.
12. A table that intentionally has no Total column.

Each test should compare:

- Extracted display text
- Extracted source annotation
- Assigned annotation role
- Generated ARS grouping
- Generated analysis metadata
- ARD group values
- Final rendered column labels

## 12. Acceptance Criteria

The issue can be considered resolved when all of the following
conditions are met:

- Total is represented correctly in ARS whenever Total appears in the
  shell.
- Total results are generated in ARD for all applicable analyses.
- The Total group scope matches the annotated shell.
- Row-level calculations remain associated with row analyses.
- Column-level conditions remain associated with column groups.
- The review application shows the original annotation accurately.
- The visible table label does not include machine-readable annotation
  text.
- Differences between source annotations and normalized metadata are
  visible and traceable.
- Automated validation identifies missing or misassigned Total metadata
  before final rendering.

## 13. Summary

A missing Total result is usually not a formatting issue. It is
generally caused by incomplete or incorrect grouping metadata, a missing
link between the Total group and an analysis, or a failure to generate
the corresponding ARD result.

An annotation mismatch usually occurs when display text and annotation
text are not separated, or when a column-level annotation is confused
with row-level analysis logic.

The general solution is to preserve the original table structure, retain
the exact source annotation, classify annotations by their role and
location, define Total as a valid analysis group, link Total to
applicable analyses, and validate the generated ARD against the expected
groups from the shell.
