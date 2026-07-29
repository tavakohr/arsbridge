## arsbridge -- parse_decision_digest.R
## ---------------------------------------------------------------------------
## PRIVACY-SAFE record of what the parser DECIDED about a shell.
##
## tools/shell_structure_digest.R describes the raw geometry of a shell a
## user is not allowed to share (spans, merges, header flags, text
## silhouettes). This is its counterpart: the same silhouette rules applied
## to what parse_shell_docx() concluded -- how many header rows it chose,
## what its column labels became, what shape the column tree took, how each
## stub row was classified. Diffing the two JSONs on the locked machine
## pinpoints WHERE the parse diverged from the document without a single
## literal character of study text leaving it.
##
## No label text, no annotation values, no titles ever reach the output:
## letters become "a"/"A", digits become "9" -- "Cohort 1 (N=XX)" is
## recorded as "Aaaaaa 9 (A=AA)". Enough to tell a cohort-shaped column
## label from a placeholder ("xx (xx.x)" -> "aa (aa.a)"), nothing more.

#' Character-class silhouette, same rules as tools/shell_structure_digest.R:
#' keeps length, case pattern, digits and punctuation; destroys the words.
#' @noRd
.silhouette <- function(x, max_chars = 60L) {
  x <- substr(as.character(x %||% ""), 1L, max_chars)
  x <- gsub("[A-Z]", "A", x)
  x <- gsub("[a-z]", "a", x)
  gsub("[0-9]", "9", x)
}

#' Write a privacy-safe digest of the parser's decisions for a shell
#'
#' Parses `shell_path` and writes a JSON describing, per TLF, what the
#' parser decided: header rows flagged vs inferred, physical column count
#' vs resulting column-label count, the column labels and stub-row labels
#' as character-class silhouettes, the column-tree shape, and per-severity
#' diagnostic counts. The output contains no literal document text, so --
#' after you read it yourself -- it can leave a machine the shell itself
#' cannot. Diff it against the raw-geometry digest from
#' `tools/shell_structure_digest.R` to localize a parsing divergence.
#'
#' @param shell_path Path to the annotated TLF shell `.docx`.
#' @param out_json Where to write the digest JSON.
#' @param heading_patterns Optional custom heading patterns, as understood
#'   by the shell parser.
#'
#' @return Invisibly, the digest list (also written to `out_json`).
#' @export
parse_decision_digest <- function(shell_path,
                                  out_json = "parse_decision_digest.json",
                                  heading_patterns = NULL) {
  diag_reset()
  sections <- suppressMessages(suppressWarnings(
    parse_shell_docx(shell_path, heading_patterns = heading_patterns)))
  d <- diag_records()

  tlfs <- lapply(sections, .section_decision_digest)
  names(tlfs) <- vapply(sections, function(s) s$tlf_number %||% "?",
                        character(1))

  digest <- list(
    format = "arsbridge parse decision digest v1",
    note = paste("All text is character-class silhouettes (A/a/9);",
                 "no literal document text is present."),
    generator = paste("arsbridge",
                      as.character(utils::packageVersion("arsbridge"))),
    n_sections = length(sections),
    diagnostics = as.list(table(paste(d$severity, d$stage, sep = ":"))),
    tlfs = tlfs
  )

  writeLines(jsonlite::toJSON(digest, auto_unbox = TRUE, pretty = TRUE),
             out_json)
  cli::cli_inform(c(
    "v" = "Parse decision digest written to {.path {out_json}} ({length(sections)} TLF{?s}).",
    "i" = "Open and READ it before sending it anywhere -- it should contain only A/a/9 silhouettes."
  ))
  invisible(digest)
}

#' The decision record for one parsed section. Everything textual is
#' silhouetted.
#' @noRd
.section_decision_digest <- function(sec) {
  tree <- sec$column_tree %||% list()
  nodes <- lapply(tree$nodes %||% list(), function(n) {
    list(
      level         = n$level,
      order         = n$order,
      nodeType      = n$nodeType,
      has_condition = !is.null(n$condition) || !is.null(n$compoundExpression),
      label         = .silhouette(n$label)
    )
  })

  rows <- lapply(sec$stub_rows %||% list(), function(r) {
    list(
      label            = .silhouette(r$label),
      has_annot        = isTRUE(r$has_annot),
      detection_method = r$detection_method %||% NA_character_
    )
  })

  list(
    tlf_type             = sec$tlf_type %||% NA_character_,
    title                = .silhouette(sec$title),
    ## The header decision (recorded by .populate_table()).
    header_rows_flagged  = sec$header_rows_flagged %||% NA_integer_,
    header_rows_inferred = sec$header_rows_inferred %||% NA_integer_,
    n_header_rows        = sec$n_header_rows %||% NA_integer_,
    ## Physical grid width vs what the header flattening produced.
    n_physical_cols      = sec$n_physical_cols %||% NA_integer_,
    n_col_headers        = length(sec$col_headers %||% character(0)),
    n_data_cols          = sec$n_data_cols %||% NA_integer_,
    col_headers          = vapply(sec$col_headers %||% character(0),
                                  .silhouette, character(1),
                                  USE.NAMES = FALSE),
    ## Column tree shape: FLAT means per-grid-column fallback.
    tree_mode            = tree$mode %||% "none",
    tree_nodes           = nodes,
    n_stub_rows          = length(sec$stub_rows %||% list()),
    stub_rows            = rows
  )
}
