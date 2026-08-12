## Golden ARS: building, hardening and canonicalisation.
##
## The gate these serve pins the WHOLE converted reporting event for three
## shells, so a change in groupings, conditions, analysis sets, output
## references, methods, columns or result metadata shows up as a diff instead
## of passing unnoticed. Focused unit tests pin behaviours; this pins output.
##
## Canonicalisation removes ordering noise and two genuinely volatile fields.
## It is deliberately NOT a repair step: malformed input fails loudly here
## rather than being tidied into something comparable, because a gate that
## sanitises its own inputs stops being evidence.

## The two fields that legitimately change between identical runs. Nothing
## else does -- the converter emits no paths, no run ids, no environment.
.GOLDEN_GENERATOR_RE <- "^arsbridge [0-9]+\\.[0-9]+\\.[0-9]+(\\.[0-9]+)?$"
.GOLDEN_TIMESTAMP_RE <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$"

.GOLDEN_GENERATOR_SENTINEL <- "arsbridge <VERSION>"
.GOLDEN_TIMESTAMP_SENTINEL <- "<GENERATED_AT_UTC>"

## Collections whose ARRAY POSITION carries no meaning: every one of them is
## reached by id from somewhere else, so build order is noise. Sorting them is
## what lets an unrelated reordering stop failing the gate.
##
## Everything NOT listed here keeps its order and is compared ordered, because
## for those the position IS the semantics: `orderedGroupings` (element 1 is
## the column axis), `referencedAnalysisIds` (display row order), a grouping's
## `groups` (display column order), and the table of contents.
.GOLDEN_ID_KEYED <- c("analysisSets", "dataSubsets", "analysisGroupings",
                      "methods", "analyses", "outputs")

## A scalar the way jsonlite hands it back with simplifyVector = FALSE.
.golden_scalar <- function(x) {
  if (is.null(x)) return(NA_character_)
  x <- unlist(x, use.names = FALSE)
  if (length(x) != 1L) return(NA_character_)
  as.character(x)
}

#' The volatile metadata must look like itself before it is replaced.
#'
#' Substituting a sentinel into a field nobody checked would hide exactly the
#' change worth seeing: a generator string that stopped carrying the version,
#' a timestamp that arrived in local time, a field that vanished. So the shape
#' is asserted first, and only a well-formed value is replaced.
.assert_volatile_meta <- function(ars, label = "ARS") {
  meta <- ars[["_meta"]]
  if (is.null(meta)) {
    stop(sprintf("%s: no `_meta` block, so there is no generator to check.",
                 label), call. = FALSE)
  }

  generator <- .golden_scalar(meta[["generator"]])
  if (is.na(generator) || !grepl(.GOLDEN_GENERATOR_RE, generator)) {
    stop(sprintf(
      "%s: `_meta.generator` is %s, which is not the expected form %s.",
      label, if (is.na(generator)) "missing or not a scalar" else
        sprintf("\"%s\"", generator), .GOLDEN_GENERATOR_RE), call. = FALSE)
  }

  stamp <- .golden_scalar(meta[["generated_at_utc"]])
  if (is.na(stamp) || !grepl(.GOLDEN_TIMESTAMP_RE, stamp)) {
    stop(sprintf(
      "%s: `_meta.generated_at_utc` is %s, which is not an ISO-8601 UTC stamp.",
      label, if (is.na(stamp)) "missing or not a scalar" else
        sprintf("\"%s\"", stamp)), call. = FALSE)
  }

  invisible(TRUE)
}

#' Every sorted collection must have unique, present ids.
#'
#' Sorting by id assumes the id identifies the element. A missing or duplicated
#' id breaks that assumption, and sorting anyway would produce a stable-looking
#' golden built on an unstable premise -- two entries with one id could swap
#' between runs and the gate would never say so. Duplicated ids are also a real
#' converter defect in their own right (a grouping-id collision has shipped
#' here before), so this fails rather than repairs.
.assert_unique_ids <- function(ars, label = "ARS") {
  for (collection in .GOLDEN_ID_KEYED) {
    entries <- ars[[collection]]
    if (is.null(entries) || length(entries) == 0) next

    ids <- vapply(entries, function(e) .golden_scalar(e[["id"]]), character(1))

    missing <- which(is.na(ids) | !nzchar(ids))
    if (length(missing) > 0) {
      stop(sprintf("%s: %s has %d entr%s with no id (position%s %s).",
                   label, collection, length(missing),
                   if (length(missing) == 1L) "y" else "ies",
                   if (length(missing) == 1L) "" else "s",
                   paste(missing, collapse = ", ")), call. = FALSE)
    }

    duplicated_ids <- unique(ids[duplicated(ids)])
    if (length(duplicated_ids) > 0) {
      stop(sprintf("%s: %s carries duplicate id(s): %s.",
                   label, collection, paste(duplicated_ids, collapse = ", ")),
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

## Every string in the document, path included, for the assertions that have to
## look everywhere rather than at named fields.
.golden_strings <- function(x, path = "") {
  if (is.character(x)) {
    return(stats::setNames(as.list(x), rep(path, length(x))))
  }
  if (!is.list(x) || length(x) == 0) return(list())
  nms <- names(x)
  out <- list()
  for (i in seq_along(x)) {
    step <- if (!is.null(nms) && nzchar(nms[[i]])) {
      paste0(path, "$", nms[[i]])
    } else {
      sprintf("%s[[%d]]", path, i)
    }
    out <- c(out, .golden_strings(x[[i]], step))
  }
  out
}

## Absolute or machine-local paths. A converted reporting event describes a
## study, not the machine that built it, so any of these appearing is a leak.
.GOLDEN_PATH_RES <- c(
  "^/[A-Za-z0-9_.-]+/",                 # POSIX absolute
  "^[A-Za-z]:[\\\\/]",                  # Windows drive
  "^\\\\\\\\",                          # UNC
  "/var/folders/",                      # macOS tempdir
  "(^|/)tmp/",                          # POSIX tempdir
  "[Tt]emp[\\\\/]",                     # Windows tempdir
  "/Users/", "/home/", "\\\\Users\\\\"  # home directories
)

#' No absolute or temporary path may appear anywhere in the ARS.
#'
#' Path-independence is an OBSERVED property today, not an enforced one, and it
#' is what makes a committed golden possible at all: the same inputs converted
#' on another machine, or from another working directory, must produce the same
#' document. Asserted separately from normalisation on purpose -- a normaliser
#' that quietly scrubbed paths would make the gate pass while the property it
#' depends on had already been lost.
.assert_no_paths <- function(ars, label = "ARS") {
  found <- .golden_strings(ars)
  hits <- character(0)
  for (i in seq_along(found)) {
    value <- found[[i]]
    if (is.na(value) || !nzchar(value)) next
    if (any(vapply(.GOLDEN_PATH_RES, grepl, logical(1), x = value))) {
      hits <- c(hits, sprintf("%s = \"%s\"", names(found)[[i]], value))
    }
  }
  if (length(hits) > 0) {
    stop(sprintf("%s: absolute or temporary path(s) in the output:\n  %s",
                 label, paste(utils::head(hits, 10), collapse = "\n  ")),
         call. = FALSE)
  }
  invisible(TRUE)
}

#' The comparable form: validated, sentinel-substituted, ordering noise gone.
#'
#' Substitution replaces rather than deletes, so a generator that stops being
#' emitted still fails the comparison instead of matching a golden that also
#' lacks it.
.ars_canonical <- function(ars, label = "ARS") {
  .assert_volatile_meta(ars, label)
  .assert_unique_ids(ars, label)

  ars[["_meta"]][["generator"]]        <- .GOLDEN_GENERATOR_SENTINEL
  ars[["_meta"]][["generated_at_utc"]] <- .GOLDEN_TIMESTAMP_SENTINEL

  for (collection in .GOLDEN_ID_KEYED) {
    entries <- ars[[collection]]
    if (is.null(entries) || length(entries) < 2L) next
    ids <- vapply(entries, function(e) .golden_scalar(e[["id"]]), character(1))
    ## Radix order is C-locale, so the golden does not depend on the collation
    ## of the machine that wrote it.
    ars[[collection]] <- entries[order(ids, method = "radix")]
  }

  ars
}

## One writer for both sides of the comparison and for the golden on disk, so
## the file a reviewer reads in a diff is exactly what the test produced.
.golden_json_text <- function(ars) {
  jsonlite::toJSON(ars, auto_unbox = TRUE, pretty = TRUE, null = "null",
                   digits = NA)
}

#' Canonical ARS, round-tripped through JSON.
#'
#' Both sides of the comparison go through write-then-read so that integer /
#' double representation is settled the same way on each. Without it a freshly
#' built `1L` and a golden's parsed `1` differ for no reason anyone cares about.
.golden_roundtrip <- function(ars) {
  jsonlite::fromJSON(.golden_json_text(ars), simplifyVector = FALSE)
}

## testthat runs with the test directory as the working directory, and so does
## the regeneration script -- but test_path() resolves differently outside a
## running test, so the plain relative path is tried first and test_path() is
## the fallback for any context that does not.
.golden_rel <- function(...) {
  here <- file.path(...)
  if (file.exists(here)) return(here)
  testthat::test_path(...)
}

.golden_dir <- function() {
  if (dir.exists("goldens")) "goldens" else testthat::test_path("goldens")
}

.golden_file <- function(name) file.path(.golden_dir(), paste0(name, ".json"))

.read_golden <- function(name) {
  path <- .golden_file(name)
  if (!file.exists(path)) {
    stop(sprintf("No golden at %s -- run data-raw/regenerate_goldens.R.", path),
         call. = FALSE)
  }
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

## --- what gets a golden ------------------------------------------------------
##
## Deterministic tier only: the LLM reader is a moving oracle and cannot be
## pinned this way. The two CDSC-ALZ-201 entries are the same study through
## both readers, and are deliberately NOT compared to each other -- the Excel
## shell omits analyses this study's public data cannot support, so equality
## between them would be a false invariant.
.golden_cases <- function() {
  list(
    list(name     = "alz_xlsx",
         study_id = "CDSC-ALZ-201",
         needs    = "openxlsx2",
         shell    = function() arsbridge_example("annotated_shell.xlsx"),
         spec     = function() arsbridge_example("adam_spec.xlsx")),
    list(name     = "alz_docx",
         study_id = "CDSC-ALZ-201",
         needs    = "officer",
         shell    = function() arsbridge_example("annotated_shell.docx"),
         spec     = function() arsbridge_example("adam_spec.xlsx")),
    list(name     = "apx_acceptance",
         study_id = "APX-DRM-301",
         needs    = "openxlsx2",
         shell    = function() .golden_rel("fixtures", "shells_apx_acceptance.xlsx"),
         spec     = function() .golden_rel("fixtures", "adam_spec_apx_drm_301.xlsx"))
  )
}

#' Convert one case, with every LLM route closed so the run is deterministic.
.build_golden_ars <- function(case, envir = parent.frame()) {
  out <- withr::local_tempfile(fileext = ".json", .local_envir = envir)
  withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(suppressWarnings(spec_to_ars(
      shell_path     = case$shell(),
      adam_spec_path = case$spec(),
      api_key        = "",
      output_path    = out,
      study_id       = case$study_id,
      use_llm        = FALSE,
      verbose        = FALSE
    )))
  )
  jsonlite::fromJSON(out, simplifyVector = FALSE)
}
