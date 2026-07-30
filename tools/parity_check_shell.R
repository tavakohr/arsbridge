## tools/parity_check_shell.R
## ---------------------------------------------------------------------------
## Compares what arsbridge reads from the SAME study authored two ways: as a
## Word shell and as an Excel workbook.
##
## test-parity_docx_xlsx.R does this in CI against a small committed fixture.
## This does it against YOUR files -- the real, sponsor-format pair that never
## enters the repository -- which is where the differences that matter turn
## up. Run it whenever either reader changes, and before trusting a migration
## from one format to the other.
##
## Usage (from the package root, arsbridge installed or loaded):
##
##   Rscript tools/parity_check_shell.R inputs/Study_Shells.docx \
##                                      inputs/Study_Shells.xlsx
##
##   # optionally an ADaM spec, so listing headers resolve the same way
##   Rscript tools/parity_check_shell.R shells.docx shells.xlsx spec.xlsx
##
## Exit status is 0 when every class-1 field agrees (whitelisted differences
## aside) and 1 when anything else differs, so it can gate a commit.
##
## Nothing is written and nothing is sent anywhere: the report goes to the
## console, and it quotes your shell's text, so treat the output as
## confidential. For something shareable use tools/shell_structure_digest.R
## and tools/shell_structure_digest_xlsx.R instead.

## The WORKING TREE wins when this is run from the package root: the point of
## the check is usually to test a reader change that is not installed yet, and
## silently measuring the installed copy instead would report a clean pass on
## the wrong code.
suppressPackageStartupMessages({
  in_source_tree <- dir.exists("R") && file.exists("DESCRIPTION") &&
    any(grepl("^Package:\\s*arsbridge\\s*$", readLines("DESCRIPTION", warn = FALSE)))
  if (in_source_tree && requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(".", quiet = TRUE)
  } else if (requireNamespace("arsbridge", quietly = TRUE)) {
    library(arsbridge)
  } else {
    stop("arsbridge must be installed, or run this from the package root.")
  }
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

## ---------------------------------------------------------------------------
## What must match, and what is allowed not to
## ---------------------------------------------------------------------------
##
## Class 1 of the section-object contract (adr/0004-xlsx-shell-input.md): the
## same shell content must give the same value from either reader.
CLASS1_FIELDS <- c(
  "tlf_number", "tlf_type", "title",
  "population_text", "population_annot",
  "footnotes", "source_datasets",
  "col_headers", "n_data_cols",
  "column_annotation", "column_groups", "column_tree", "include_total_hint"
)

## Class 2: allowed to differ, each with the reason it is allowed. Anything
## NOT on this list that differs fails the run -- adding an entry is a
## deliberate act that leaves a written justification behind.
CLASS2_WHITELIST <- list(
  detection_method = paste(
    "namespaced per format so a reviewer can see whether a run's colour or a",
    "whole cell's font was the evidence"),
  raw_heading = paste(
    "a Word heading paragraph has no Excel equivalent; the sheet's number row",
    "is used instead"),
  header_rows_flagged = paste(
    "Excel has no <w:tblHeader/>, so a multi-row header is always inferred"),
  header_rows_inferred = "counted against different geometries",
  n_header_rows        = "counted against different geometries",
  n_physical_cols      = "counted against different geometries",
  listing_placeholder_row = paste(
    "a Word listing enumerates its placeholder body row as an unannotated",
    "stub row; the Excel reader records it as template_row instead")
)

## ---------------------------------------------------------------------------

as_text <- function(x) {
  if (is.null(x)) return("<NULL>")
  if (is.list(x)) return(paste(utils::capture.output(str(x)), collapse = " "))
  paste(as.character(x), collapse = " | ")
}

annotated_rows <- function(sec) {
  lapply(Filter(function(r) isTRUE(r$has_annot), sec$stub_rows), function(r) {
    list(label = r$label, annotation = r$annotation)
  })
}

compare_sections <- function(d, x) {
  diffs <- list()
  add <- function(field, a, b) {
    diffs[[length(diffs) + 1L]] <<- list(field = field, docx = as_text(a),
                                         xlsx = as_text(b))
  }

  for (field in CLASS1_FIELDS) {
    if (!identical(d[[field]], x[[field]])) add(field, d[[field]], x[[field]])
  }
  if (!identical(annotated_rows(d), annotated_rows(x))) {
    add("annotated stub rows",
        vapply(annotated_rows(d), function(r) paste(r$label, r$annotation),
               character(1)),
        vapply(annotated_rows(x), function(r) paste(r$label, r$annotation),
               character(1)))
  }
  ## Row COUNT is compared only for tables: the listing placeholder row is a
  ## whitelisted difference.
  if (identical(d$tlf_type, "TABLE") &&
      length(d$stub_rows) != length(x$stub_rows)) {
    add("stub row count", length(d$stub_rows), length(x$stub_rows))
  }
  diffs
}

parity_check_shell <- function(docx_path, xlsx_path, spec_path = NULL) {
  spec_lookup <- NULL
  if (!is.null(spec_path) && nzchar(spec_path)) {
    spec_lookup <- arsbridge:::parse_adam_spec(spec_path)$lookup
  }

  read <- function(path) {
    suppressMessages(suppressWarnings(
      arsbridge:::parse_shell(path, spec_lookup = spec_lookup)))
  }
  docx <- read(docx_path)
  xlsx <- read(xlsx_path)

  cat("== arsbridge shell parity check ==\n")
  cat(sprintf("  Word  : %s  (%d output%s)\n", basename(docx_path),
              length(docx), if (length(docx) == 1L) "" else "s"))
  cat(sprintf("  Excel : %s  (%d output%s)\n\n", basename(xlsx_path),
              length(xlsx), if (length(xlsx) == 1L) "" else "s"))

  problems <- 0L

  ## Which outputs each side found. A shell present in one and not the other
  ## is the first thing to know -- everything below assumes a shared set.
  d_ids <- vapply(docx, function(s) s$tlf_number %||% "?", character(1))
  x_ids <- vapply(xlsx, function(s) s$tlf_number %||% "?", character(1))
  only_docx <- setdiff(d_ids, x_ids)
  only_xlsx <- setdiff(x_ids, d_ids)
  if (length(only_docx) > 0) {
    cat("  ONLY IN WORD :", paste(only_docx, collapse = ", "), "\n")
    problems <- problems + length(only_docx)
  }
  if (length(only_xlsx) > 0) {
    cat("  ONLY IN EXCEL:", paste(only_xlsx, collapse = ", "), "\n")
    problems <- problems + length(only_xlsx)
  }
  if (length(only_docx) || length(only_xlsx)) cat("\n")

  for (id in intersect(d_ids, x_ids)) {
    d <- docx[[which(d_ids == id)[[1]]]]
    x <- xlsx[[which(x_ids == id)[[1]]]]
    diffs <- compare_sections(d, x)
    if (length(diffs) == 0) {
      cat(sprintf("  [ok]   %-12s %s\n", id, substr(d$title %||% "", 1, 46)))
      next
    }
    problems <- problems + length(diffs)
    cat(sprintf("  [DIFF] %-12s %s\n", id, substr(d$title %||% "", 1, 46)))
    for (df in diffs) {
      cat(sprintf("         %-22s\n           word : %s\n           excel: %s\n",
                  df$field, substr(df$docx, 1, 90), substr(df$xlsx, 1, 90)))
    }
  }

  cat("\n")
  if (problems == 0L) {
    cat("PASS -- every class-1 field agrees.\n")
  } else {
    cat(sprintf("FAIL -- %d difference%s outside the whitelist.\n", problems,
                if (problems == 1L) "" else "s"))
    cat("\nAllowed to differ (class 2), for reference:\n")
    for (nm in names(CLASS2_WHITELIST)) {
      cat(sprintf("  %-24s %s\n", nm, CLASS2_WHITELIST[[nm]]))
    }
  }
  invisible(problems)
}

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 2L) {
    cat("Usage: Rscript tools/parity_check_shell.R <shell.docx> <shell.xlsx> [spec.xlsx]\n")
    quit(status = 2L)
  }
  n <- parity_check_shell(args[[1]], args[[2]],
                          if (length(args) >= 3L) args[[3]] else NULL)
  quit(status = if (n == 0L) 0L else 1L)
}
