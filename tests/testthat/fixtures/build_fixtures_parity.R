## tests/testthat/fixtures/build_fixtures_parity.R
## ---------------------------------------------------------------------------
## Builds the MATCHED PAIR that test-parity_docx_xlsx.R compares:
##
##   shells_parity_apx.docx    the same three outputs, authored as Word
##   shells_parity_apx.xlsx    the same three outputs, authored as Excel
##
## Run from the package root:
##
##   Rscript tests/testthat/fixtures/build_fixtures_parity.R
##
## Why a purpose-built pair rather than a Word twin of
## shells_apx_drm_301.xlsx: that fixture deliberately contains things only an
## Excel shell can express or get wrong -- a figure stated as arrow prose, a
## tab name disagreeing with its own first row. A parity fixture must contain
## only what BOTH formats can say, or the test would be asserting a
## difference it built itself.
##
## The two files therefore state the same three outputs using each format's
## OWN convention, which is the whole point: the same study said two ways must
## parse to the same class-1 section fields.
##
##   what              Word                          Excel
##   ----------------- ----------------------------- ---------------------------
##   output heading    a heading paragraph           the worksheet name + row 1
##   population        a paragraph, annotation red   row 3, annotation red
##   column axis       "Treatment columns -> VAR"    "[columns -> VAR]" in the
##                     below the table                stub header
##   source datasets   "Source: ADSL"                "source ADSL" in the same
##                                                    stub header
##   stub annotation   red run in the stub cell      red run in the stub cell
##   listing column    label / variable on two       label / variable on two
##                     paragraphs of the header cell  lines of the header cell
##   footnote          a paragraph below the table   a merged row below the body

suppressPackageStartupMessages({
  library(officer)
  library(openxlsx2)
  library(xml2)
})

here <- if (dir.exists("tests/testthat/fixtures")) {
  "tests/testthat/fixtures"
} else if (dir.exists("fixtures")) {
  "fixtures"
} else {
  "."
}

BLACK <- "FF000000"
RED   <- "FFC00000"

## The single description of the content, so the two writers below cannot
## drift apart by an edit to one of them.
arms <- c("Placebo", "Drug 10 mg", "Drug 20 mg")

disposition_rows <- list(
  list(label = "Subjects treated",   annot = "ADSL.SAFFL = 'Y'",
       value = "xx"),
  list(label = "Completed study",    annot = "ADSL.EOSSTT = 'COMPLETED'",
       value = "xx (xx.x)"),
  list(label = "Discontinued study", annot = "ADSL.EOSSTT = 'DISCONTINUED'",
       value = "xx (xx.x)")
)

demography_rows <- list(
  list(label = "Sex, n (%)", annot = "ADSL.SEX", value = NULL),
  list(label = "Female",     annot = NULL,       value = "xx (xx.x)"),
  list(label = "Male",       annot = NULL,       value = "xx (xx.x)")
)

listing_cols <- list(
  list(label = "Subject",   annot = "ADAE.USUBJID",  value = "xxx-xxx"),
  list(label = "Treatment", annot = "ADAE.TRT01A",   value = "xxxx"),
  list(label = "AE term",   annot = "ADAE.AEDECOD",  value = "xxxx"),
  list(label = "Severity",  annot = "ADAE.ASEV",     value = "x")
)

DISPOSITION_FOOTNOTE <- "Percentages are based on the number of treated subjects."

## ---------------------------------------------------------------------------
## The Word rendering
## ---------------------------------------------------------------------------

## Word states the stub annotation inline in the cell, in brackets, exactly as
## Excel does -- the difference between the formats is WHERE the axis and the
## source are stated, not how a row is annotated.
docx_stub <- function(row) {
  if (is.null(row$annot)) row$label else paste0(row$label, "  [", row$annot, "]")
}

table_frame <- function(rows, stub_header) {
  df <- data.frame(
    stub = vapply(rows, docx_stub, character(1)),
    stringsAsFactors = FALSE)
  for (a in arms) {
    df[[a]] <- vapply(rows, function(r) r$value %||% "", character(1))
  }
  names(df)[[1]] <- stub_header
  df
}

`%||%` <- function(a, b) if (is.null(a)) b else a

listing_frame <- function() {
  df <- as.data.frame(
    matrix(vapply(listing_cols, function(c) c$value, character(1)), nrow = 1),
    stringsAsFactors = FALSE)
  ## The listing convention: display label on line 1, variable on line 2.
  names(df) <- vapply(listing_cols,
                      function(c) paste0(c$label, "\n", c$annot), character(1))
  df
}

doc <- read_docx() |>
  body_add_par("Table 14.1.1", style = "heading 2") |>
  body_add_par("Summary of Subject Disposition") |>
  body_add_par("Safety Population (ADSL.SAFFL='Y')") |>
  body_add_table(value = table_frame(disposition_rows, "Category"),
                 style = "table_template") |>
  body_add_par("Treatment columns -> ADSL.TRT01A") |>
  body_add_par("Source: ADSL") |>
  body_add_par(DISPOSITION_FOOTNOTE) |>

  body_add_par("Table 14.1.2", style = "heading 2") |>
  body_add_par("Demographics and Baseline Characteristics") |>
  body_add_par("Safety Population (ADSL.SAFFL='Y')") |>
  body_add_table(value = table_frame(demography_rows, "Characteristic"),
                 style = "table_template") |>
  body_add_par("Treatment columns -> ADSL.TRT01A") |>
  body_add_par("Source: ADSL") |>

  body_add_par("Listing 16.2.1", style = "heading 2") |>
  body_add_par("Listing of Adverse Events") |>
  body_add_par("Safety Population (ADSL.SAFFL='Y')") |>
  body_add_table(value = listing_frame(), style = "table_template") |>
  body_add_par("Source: ADAE")

docx_path <- file.path(here, "shells_parity_apx.docx")
print(doc, target = docx_path)

## Re-open the OOXML and paint every ADaM reference red, so Layer 1 (colour)
## is the detection path in the Word rendering just as it is in the Excel one.
## Same approach as build_fixtures.R's repaint_red().
repaint_red_parity <- function(docx_path) {
  td <- tempfile()
  dir.create(td)
  utils::unzip(docx_path, exdir = td)
  doc_xml_path <- file.path(td, "word", "document.xml")
  d <- xml2::read_xml(doc_xml_path)

  ## A wrapping parenthesis pair is taken INTO the red run. Both real shells
  ## paint "(ADSL.SAFFL='Y')" red as a whole on the population line, and the
  ## Excel writer below does the same -- if this rendering painted only the
  ## inner reference, the pair would differ on how it was AUTHORED rather than
  ## on how it was parsed, and the parity test would be measuring the fixture.
  adam_re <- paste0(
    "\\(?\\bAD[A-Z]{1,6}\\.[A-Z][A-Z0-9]{0,7}",
    "(?:\\s*=\\s*(?:'[^']*'|\"[^\"]*\"|[-+]?\\d+(?:\\.\\d+)?))?\\)?")

  for (p in xml2::xml_find_all(d, ".//*[local-name()='p']")) {
    t_nodes <- xml2::xml_find_all(p, ".//*[local-name()='t']")
    if (length(t_nodes) == 0) next
    full <- paste(xml2::xml_text(t_nodes), collapse = "")
    m <- regexpr(adam_re, full, perl = TRUE)
    if (m == -1) next

    start <- as.integer(m)
    len   <- attr(m, "match.length")
    label    <- substr(full, 1, start - 1L)
    annot    <- substr(full, start, start + len - 1L)
    trailing <- substr(full, start + len, nchar(full))

    runs <- xml2::xml_find_all(p, "./*[local-name()='r']")
    if (length(runs) == 0) next
    first_t <- xml2::xml_find_first(runs[[1]], ".//*[local-name()='t']")
    if (inherits(first_t, "xml_missing")) next
    xml2::xml_text(first_t) <- label
    if (length(runs) > 1) for (r in runs[-1]) xml2::xml_remove(r)

    add_run <- function(text, colour = NULL) {
      r <- xml2::xml_add_sibling(runs[[1]], "w:r", .where = "after")
      if (!is.null(colour)) {
        rpr <- xml2::xml_add_child(r, "w:rPr")
        xml2::xml_add_child(rpr, "w:color", "w:val" = colour)
      }
      t <- xml2::xml_add_child(r, "w:t", text)
      xml2::xml_attr(t, "xml:space") <- "preserve"
      r
    }
    ## Added after the first run, so append in reverse to keep the order.
    if (nzchar(trailing)) add_run(trailing)
    add_run(annot, colour = "C00000")
  }

  xml2::xml_ns_strip(d)
  xml2::write_xml(d, doc_xml_path)
  old <- setwd(td)
  on.exit(setwd(old), add = TRUE)
  unlink(docx_path)
  utils::zip(normalizePath(docx_path, mustWork = FALSE), files = ".",
             flags = "-q -r -X")
  setwd(old)
  invisible(docx_path)
}

repaint_red_parity(normalizePath(docx_path))
cat("wrote", docx_path, "\n")

## ---------------------------------------------------------------------------
## The Excel rendering
## ---------------------------------------------------------------------------

annotated <- function(label, annot) {
  fmt_txt(label, color = wb_color(hex = BLACK), size = 10) +
    fmt_txt(paste0("\n[", annot, "]"), color = wb_color(hex = RED),
            size = 8, italic = TRUE)
}

wbx <- wb_workbook()

xlsx_banner <- function(wb, sheet, number, title, n_cols) {
  wb$add_data(sheet = sheet, x = number, start_row = 1, start_col = 1,
              col_names = FALSE)
  wb$add_data(sheet = sheet, x = title, start_row = 2, start_col = 1,
              col_names = FALSE)
  wb$add_data(
    sheet = sheet,
    x = fmt_txt("Safety Population ", color = wb_color(hex = BLACK), size = 10) +
      fmt_txt("(ADSL.SAFFL='Y')", color = wb_color(hex = RED), size = 8,
              italic = TRUE),
    start_row = 3, start_col = 1, col_names = FALSE)
  for (r in 1:3) {
    wb$merge_cells(sheet = sheet,
                   dims = sprintf("A%d:%s%d", r, int2col(n_cols), r))
  }
  invisible(wb)
}

xlsx_table <- function(wb, sheet, number, title, stub_header, rows, source_ds,
                       footnote = NULL) {
  wb$add_worksheet(sheet)
  n_cols <- length(arms) + 1L
  xlsx_banner(wb, sheet, number, title, n_cols)

  wb$add_data(sheet = sheet,
              x = annotated(stub_header,
                            paste0("columns -> ADSL.TRT01A; source ", source_ds)),
              start_row = 4, start_col = 1, col_names = FALSE)
  for (j in seq_along(arms)) {
    wb$add_data(sheet = sheet, x = arms[[j]], start_row = 4,
                start_col = j + 1L, col_names = FALSE)
  }

  for (i in seq_along(rows)) {
    row <- rows[[i]]
    r <- 4L + i
    wb$add_data(sheet = sheet,
                x = if (is.null(row$annot)) row$label else
                  annotated(row$label, row$annot),
                start_row = r, start_col = 1, col_names = FALSE)
    if (!is.null(row$value)) {
      for (j in seq_along(arms)) {
        wb$add_data(sheet = sheet, x = row$value, start_row = r,
                    start_col = j + 1L, col_names = FALSE)
      }
    }
  }

  if (!is.null(footnote)) {
    r <- 5L + length(rows)
    wb$add_data(sheet = sheet, x = footnote, start_row = r, start_col = 1,
                col_names = FALSE)
    wb$merge_cells(sheet = sheet,
                   dims = sprintf("A%d:%s%d", r, int2col(n_cols), r))
  }
  invisible(wb)
}

xlsx_table(wbx, "Table 14.1.1", "Table 14.1.1",
           "Summary of Subject Disposition", "Category", disposition_rows,
           "ADSL", DISPOSITION_FOOTNOTE)
xlsx_table(wbx, "Table 14.1.2", "Table 14.1.2",
           "Demographics and Baseline Characteristics", "Characteristic",
           demography_rows, "ADSL")

## The listing: annotated headers, then one template row.
wbx$add_worksheet("Listing 16.2.1")
xlsx_banner(wbx, "Listing 16.2.1", "Listing 16.2.1",
            "Listing of Adverse Events", length(listing_cols))
for (j in seq_along(listing_cols)) {
  col <- listing_cols[[j]]
  annot <- if (j == 1L) {
    paste0(col$annot, "]  [source ADAE")   ## the source rides the stub header
  } else {
    col$annot
  }
  wbx$add_data(sheet = "Listing 16.2.1", x = annotated(col$label, annot),
               start_row = 4, start_col = j, col_names = FALSE)
  wbx$add_data(sheet = "Listing 16.2.1", x = col$value, start_row = 5,
               start_col = j, col_names = FALSE)
}

xlsx_path <- file.path(here, "shells_parity_apx.xlsx")
wbx$save(xlsx_path)
cat("wrote", xlsx_path, "\n")

## ---------------------------------------------------------------------------
## The ADaM spec both APX-DRM-301 shells are gated against
## ---------------------------------------------------------------------------
##
## Every DATASET.VARIABLE either shell annotates has to exist here, or the
## hard spec gate drops it -- which would make an end-to-end test measure the
## gate rather than the reader. Covers shells_parity_apx.* and the nine-sheet
## shells_apx_drm_301.xlsx.

spec_vars <- list(
  ADSL = c("USUBJID", "SAFFL", "EOSSTT", "SEX", "AGE", "AGEU", "AGEGR1",
           "RACE", "TRT01A", "TRT01AN", "SCRNFL"),
  ADAE = c("USUBJID", "TRT01A", "TRTA", "AEDECOD", "AESOC", "ASEV", "ASEVN",
           "AESER", "AEREL", "AEOUT", "AEACN", "TRTEMFL", "ASTDT", "AENDT"),
  ADEX = c("USUBJID", "TRT01A", "TRT01AN", "TRTDURD", "AVISITN"),
  ADDV = c("USUBJID", "DVCAT", "DVDECOD"),
  ADCM = c("USUBJID", "TRTA", "CMTRT", "CMDECOD", "CMCLAS", "CMINDC",
           "ASTDT", "AENDT"),
  ADVS = c("USUBJID", "TRTA", "PARAMCD", "AVAL", "AVISIT", "AVISITN",
           "SAFFL", "DTYPE")
)

is_numeric_var <- function(v) {
  grepl("N$|^AGE$|^AVAL$|^TRTDURD$", v) && !grepl("^AGEU$", v)
}

spec_df <- do.call(rbind, lapply(names(spec_vars), function(ds) {
  vars <- spec_vars[[ds]]
  data.frame(
    Dataset   = ds,
    Variable  = vars,
    Label     = paste(ds, vars),
    Type      = ifelse(vapply(vars, is_numeric_var, logical(1)), "Num", "Char"),
    Origin    = "Derived",
    Codelist  = "",
    Length    = "40",
    Mandatory = "Req",
    stringsAsFactors = FALSE)
}))

spec_path <- file.path(here, "adam_spec_apx_drm_301.xlsx")
spec_wb <- openxlsx2::wb_workbook() |>
  openxlsx2::wb_add_worksheet("Variables") |>
  openxlsx2::wb_add_data(sheet = "Variables", x = spec_df)
openxlsx2::wb_save(spec_wb, file = spec_path, overwrite = TRUE)
cat("wrote", spec_path, "(", nrow(spec_df), "variables )\n")
