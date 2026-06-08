# Developer Guide: Integrating {cards} into arsbridge for Automated ARD Generation

This document summarizes the current metadata-driven pipeline and
details the implementation plan for updating the `arsbridge` package to
natively generate and execute **Analysis Results Data (ARD)** using the
[cards](https://github.com/insightsengineering/cards) package, bypassing
the need for third-party parsers like `siera`.

------------------------------------------------------------------------

## 1. How the Current Process Works (Under the Hood)

Currently, the end-to-end workflow behaves as follows:

    [Annotated Shell (docx)] + [ADaM Spec (xlsx)]
        │
        ▼  (1. LLM Enrichment via `arsbridge`)
    [Enriched Table Specifications]
        │
        ▼  (2. JSON Compilation via `arsbridge::build_ars_json()`)
    [reporting_event.json (CDISC ARS v1.0 standard + siera-compat patches)]
        │
        ▼  (3. Code Compilation via `siera::readARS()`)
    [Generated R Scripts (containing {cards} calls and templates)]
        │
        ▼  (4. Execution)
    [ARD Dataset (Standard Tidy Format)]

### Why we had to patch the JSON:

- `arsbridge` produces a strict **CDISC ARS v1.0 compliant JSON**.
- `siera` uses a custom, non-compliant schema (e.g. flat variable
  mappings, hardcoded code-templates, and required tables of contents).
- Without the compatibility logic (flattening variables, injecting
  standard `cards` code templates, and creating dummy relationships),
  `siera`’s parser crashed.

------------------------------------------------------------------------

## 2. Proposed Native Workflow (The `arsbridge` Package Update)

Instead of relying on `siera` as a middleman, `arsbridge` can directly
execute the ARS JSON specification and output the
[cards](https://github.com/insightsengineering/cards) ARD. This keeps
the user entirely within your package interface in RStudio:

    [Annotated Shell]
        │
        ▼  (arsbridge::spec_to_ars())
    [ARS JSON Specification]
        │
        ▼  (arsbridge::ars_to_ard())
    [Final ARD Object (Tidy Data Frame)]

### Advantages:

1.  **No Dependency on
    [siera](https://clymbclinical.github.io/siera/)**: Avoids crashes
    caused by rigid parsing rules in third-party packages.
2.  **Natively Handles Formats**: `arsbridge` can natively support
    `.xpt` SAS transport files or `.csv` files.
3.  **Clean, Readable Code**: You can write clean
    [cards](https://github.com/insightsengineering/cards) functions
    instead of string-manipulating templates.

------------------------------------------------------------------------

## 3. Implementation Plan for `arsbridge` developers

To implement this, add a new function
[`ars_to_ard()`](reference/ars_to_ard.md) in the `arsbridge` package.

### Step 1: Read the ARS JSON

Load the JSON using `jsonlite::fromJSON(path, simplifyVector = FALSE)`.

### Step 2: Iterate over the Planned Outputs & Analyses

For each output, loop through its referenced analyses. For each
analysis, look up: \* The target dataset (e.g., `ADSL` or `ADAE`). \*
The analysis variable (e.g., `AGE` or `SEX`). \* The grouping variables
(e.g., `TRT01A`). \* The population filter (e.g., `ITTFL == "Y"`). \*
The analysis method (e.g., `MTH_SUMMARY_STATISTICS_CONTINUOUS`).

### Step 3: Map Method IDs directly to `{cards}` / `{cardx}` calls

Inside R, write a mapper that translates the method ID into a
[cards](https://github.com/insightsengineering/cards) function call.

#### 1. Continuous Summaries (`MTH_SUMMARY_STATISTICS_CONTINUOUS`)

Maps directly to
[`cards::ard_continuous()`](https://insightsengineering.github.io/cards/latest-tag/reference/deprecated.html):

``` r

# Example translation code:
cards::ard_continuous(
  data = df_filtered,
  by = !!rlang::sym(grouping_var),
  variables = !!rlang::sym(analysis_var)
)
```

#### 2. Categorical Counts & Percentages (`MTH_COUNT_AND_PERCENTAGE` or `MTH_AE_FREQUENCY_COUNT`)

Maps directly to
[`cards::ard_categorical()`](https://insightsengineering.github.io/cards/latest-tag/reference/deprecated.html):

``` r

# Example translation code:
cards::ard_categorical(
  data = df_filtered,
  by = !!rlang::sym(grouping_var),
  variables = !!rlang::sym(analysis_var),
  denominator = df_population  # Denominator dataset (e.g. ADSL)
)
```

#### 3. Subject Counts (`MTH_SUBJECT_COUNT`)

Maps directly to
[`cards::ard_categorical()`](https://insightsengineering.github.io/cards/latest-tag/reference/deprecated.html)
or a custom summary:

``` r

cards::ard_categorical(
  data = df_filtered,
  by = !!rlang::sym(grouping_var),
  variables = !!rlang::sym(analysis_var)
) |> 
  dplyr::filter(stat_name == "n")
```

### Step 4: Bind and Return the ARD

Collect all the individual ARD tables and merge them using
[`cards::bind_ard()`](https://insightsengineering.github.io/cards/latest-tag/reference/bind_ard.html).

``` r

#' Execute ARS JSON and return an ARD object
#'
#' @param ars_path Path to the ARS JSON file.
#' @param adam_dir Directory containing the ADaM datasets (.csv or .xpt).
#' @return A tidy ARD data frame.
#' @export
ars_to_ard <- function(ars_path, adam_dir) {
  # 1. Parse JSON
  spec <- jsonlite::fromJSON(ars_path, simplifyVector = FALSE)
  
  # 2. Load ADaM datasets dynamically
  # (Can support reading both .csv and .xpt using haven::read_xpt)
  
  # 3. Loop through analyses and build ARD lists
  ard_list <- list()
  for (ana in spec$analyses) {
    # Resolve parameters...
    # Filter dataset...
    # Call cards::ard_* based on methodId...
    # Append to list
  }
  
  # 4. Bind results into a single tidy ARD object
  final_ard <- cards::bind_ard(!!!ard_list)
  return(final_ard)
}
```

This workflow enables clinical programmers to perform metadata-driven
table generation in a robust, standardized way.
