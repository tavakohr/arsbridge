## arsbridge -- parse_shell.R
## ---------------------------------------------------------------------------
## The one entry point for reading an annotated TLF shell, whatever it was
## authored in. Everything upstream -- spec_to_ars(), the supplement exporter,
## the decision digest -- calls this rather than a format-specific reader, so
## adding or changing a format is a change in exactly one place.
##
## Both readers return the same thing (see the compatibility contract in
## parse_shell_xlsx.R and adr/0004-xlsx-shell-input.md), so nothing downstream
## has to know which one ran.

## Extension -> reader. The pattern is matched case-insensitively against the
## file extension only; ".doc" is accepted because Word writes it for a
## document saved in the legacy format, and officer reads what it can.
.SHELL_READERS <- list(
  list(pattern = "\\.docx?$", format = "docx", label = ".docx"),
  list(pattern = "\\.xlsx$",  format = "xlsx", label = ".xlsx")
)

#' The shell formats arsbridge reads, as a human-readable list for messages
#' and labels ("`.docx` or `.xlsx`").
#' @noRd
.shell_format_labels <- function() {
  vapply(.SHELL_READERS, function(r) r$label, character(1))
}

#' Which reader a path belongs to, or NA when the extension names no format.
#' @noRd
.shell_format <- function(shell_path) {
  path <- as.character(shell_path %||% "")
  for (reader in .SHELL_READERS) {
    if (grepl(reader$pattern, path, ignore.case = TRUE)) return(reader$format)
  }
  NA_character_
}

#' TRUE when the path names a shell format arsbridge can read.
#' @noRd
.is_shell_path <- function(shell_path) {
  !is.na(.shell_format(shell_path))
}

#' Parse an annotated TLF shell, in either supported format.
#'
#' Dispatches on the file extension to `parse_shell_docx()` or
#' `parse_shell_xlsx()`. Both return the same list of section objects, so the
#' caller never branches on format.
#'
#' @param shell_path Path to the annotated shell (`.docx` or `.xlsx`).
#' @param spec_lookup,heading_patterns,progress Passed through to the reader;
#'   see `parse_shell_docx()`.
#'
#' @return List of TLF section objects.
#'
#' @keywords internal
#' @noRd
parse_shell <- function(shell_path, spec_lookup = NULL,
                        heading_patterns = NULL, progress = FALSE) {
  format <- .shell_format(shell_path)
  if (is.na(format)) {
    cli::cli_abort(c(
      "Cannot tell what kind of shell {.path {shell_path}} is.",
      "x" = "Its extension is not one arsbridge reads.",
      "i" = "Supported shell formats: {.val {.shell_format_labels()}}."
    ))
  }
  switch(
    format,
    docx = parse_shell_docx(shell_path, spec_lookup = spec_lookup,
                            heading_patterns = heading_patterns,
                            progress = progress),
    xlsx = parse_shell_xlsx(shell_path, spec_lookup = spec_lookup,
                            heading_patterns = heading_patterns,
                            progress = progress)
  )
}
