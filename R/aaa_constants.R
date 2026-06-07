## arsbridge -- aaa_constants.R
## ---------------------------------------------------------------------------
## Shared, package-wide constants and tiny utilities. Named `aaa_` so the
## R alphabetical source order puts it first -- other files that reference
## these names at top level (e.g. parse_shell_docx.R's `.ANNOTATION_PATTERN`)
## can safely use them.

## ADaM naming-convention regex pieces. ADaM dataset names always start with
## "AD" plus 1-6 uppercase letters; variable names are 1-8 uppercase
## alphanumeric chars starting with a letter. These two are the building
## blocks for every annotation pattern in the package.
.ADAM_DS  <- "AD[A-Z]{1,6}"
.ADAM_VAR <- "[A-Z][A-Z0-9]{0,7}"

## Null-coalescing operator. Returns `b` when `a` is NULL or zero-length;
## otherwise returns `a`. Mirrors rlang::`%||%` semantics without the dep.
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

## Variables that always live in ADSL regardless of the listing's primary
## source dataset. Used by parse_shell_docx() to resolve a bare variable
## name (e.g. "USUBJID", "ACTARMCD") in a listing column header to its
## fully-qualified DATASET.VARIABLE form. Variables NOT in this set are
## prefixed with the listing's first source dataset (from "Source: ...").
.UNIVERSAL_ADSL_VARS <- c(
  ## Subject identifiers
  "USUBJID", "SUBJID", "STUDYID", "SITEID",
  ## Treatment / arm
  "ARM", "ACTARM", "ARMCD", "ACTARMCD",
  "TRT01P", "TRT01A", "TRT01PN", "TRT01AN",
  ## Population flags
  "SAFFL", "ITTFL", "FASFL", "PPROTFL", "MITTFL", "EFFFL", "RANDFL", "ENRLFL",
  ## Demographics
  "AGE", "AGEU", "AGEGR1", "AGEGR1N",
  "SEX", "RACE", "ETHNIC", "COUNTRY", "REGION",
  ## Treatment dates
  "TRTSDT", "TRTEDT", "TRTDURD"
)
