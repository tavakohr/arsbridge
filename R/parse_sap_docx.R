## arsbridge -- parse_sap_docx.R
## ---------------------------------------------------------------------------
## Optional first-class input: the Statistical Analysis Plan (.docx). Split it
## into heading-delimited sections, tag each with any TLF number it mentions,
## and match a section to a shell TLF so its prose can become the human-readable
## comment above each emitted {cards} block. Reuses officer (already a
## dependency of parse_shell_docx) -- no new package. All internal (@noRd).

## Numeric TLF path key: "T-14-1-1" / "Table 14.1.1" / "14_1_1" -> "14_1_1".
#' @noRd
.norm_tlf <- function(x) {
  if (is.null(x) || !length(x) || is.na(x[1]) || !nzchar(x[1])) {
    return(NA_character_)
  }
  m <- regmatches(x[1], regexpr("\\d+(?:[._-]\\d+)+", x[1], perl = TRUE))
  if (!length(m) || !nzchar(m)) return(NA_character_)
  gsub("[._-]", "_", m)
}

#' Parse a SAP .docx into heading-delimited sections.
#'
#' @param sap_path Path to the SAP `.docx`, or `NULL`.
#' @return A data frame with `heading`, `text`, and `tlf_number` (the numeric
#'   path key, or `NA`), one row per section; or `NULL` when `sap_path` is
#'   absent/unreadable (callers treat that as "no SAP").
#' @noRd
parse_sap_docx <- function(sap_path) {
  if (is.null(sap_path) || !nzchar(sap_path) || !file.exists(sap_path)) {
    return(NULL)
  }
  summ <- tryCatch(
    officer::docx_summary(officer::read_docx(sap_path)),
    error = function(e) NULL
  )
  if (is.null(summ) || !nrow(summ)) return(NULL)

  paras <- summ[summ$content_type == "paragraph" &
                  !is.na(summ$text) & nzchar(trimws(summ$text)), , drop = FALSE]
  if (nrow(paras) == 0) return(NULL)

  style <- paras$style_name %||% rep("", nrow(paras))
  style[is.na(style)] <- ""
  is_heading <- grepl("(?i)heading|title", style)
  if (!any(is_heading)) is_heading[1] <- TRUE  # ensure at least one section
  sec_id <- cumsum(is_heading)

  rows <- lapply(split(seq_len(nrow(paras)), sec_id), function(ix) {
    list(heading = paras$text[ix[1]],
         text    = paste(paras$text[ix], collapse = "\n"))
  })
  df <- data.frame(
    heading = vapply(rows, `[[`, character(1), "heading"),
    text    = vapply(rows, `[[`, character(1), "text"),
    stringsAsFactors = FALSE
  )
  df$tlf_number <- vapply(seq_len(nrow(df)), function(i) {
    h <- .norm_tlf(df$heading[i])
    if (is.na(h)) .norm_tlf(substr(df$text[i], 1, 120)) else h
  }, character(1))
  df
}

#' Best SAP section text for a TLF: exact TLF-number match, else title-word
#' overlap on the heading, else `NA`.
#' @noRd
match_sap_section <- function(sap_df, tlf_number, title = NULL) {
  if (is.null(sap_df) || !nrow(sap_df)) return(NA_character_)

  key <- .norm_tlf(tlf_number)
  if (!is.na(key)) {
    hit <- which(sap_df$tlf_number == key)
    if (length(hit)) return(sap_df$text[hit[1]])
  }

  if (!is.null(title) && nzchar(title)) {
    tw <- tolower(strsplit(gsub("[^A-Za-z ]", " ", title), "\\s+")[[1]])
    tw <- tw[nchar(tw) >= 4]
    if (length(tw)) {
      scores <- vapply(sap_df$heading, function(h) {
        hw <- tolower(strsplit(gsub("[^A-Za-z ]", " ", h), "\\s+")[[1]])
        length(intersect(tw, hw))
      }, integer(1))
      if (length(scores) && max(scores) >= 2) {
        return(sap_df$text[which.max(scores)])
      }
    }
  }
  NA_character_
}

#' Clip SAP prose to a single short line suitable for a code comment.
#' @noRd
.clip_sap <- function(s) {
  if (is.null(s) || is.na(s) || !nzchar(s)) return("")
  lines <- trimws(strsplit(s, "\n")[[1]])
  lines <- lines[nzchar(lines)]
  one <- paste(lines, collapse = " ")
  if (nchar(one) > 240) paste0(substr(one, 1, 237), "...") else one
}
