## tests/testthat/fixtures/build_rwb_fixture.R
## ---------------------------------------------------------------------------
## Generates annotated_shell_rwb_bracket.docx -- the NEUTRAL fixture for the
## real-world "standardized bracket" shell conventions
## (HANDOFF_realworld_bracket_shells). Every convention observed in the wild
## is reproduced here with invented study content; no sponsor material.
## Run from the package root:
##
##   Rscript tests/testthat/fixtures/build_rwb_fixture.R
##
## Conventions exercised:
##   * nested brackets: population filter wrapping a PROGRAMMING DATASETS
##     directive, double-quoted value, space after the dataset dot
##   * per-column header annotations: numeric equality, is.na()/OR compound,
##     numeric IN (1,2) list
##   * instruction wrapper carrying the condition in the sentence
##   * "Use DS.VAR for this displayed row; apply ..." per-row variant
##   * prose count instruction (UNIQUE SUBJECTS WITH ...)
##   * NE "" empty-string comparison with an AND chain
##   * footnote marker [a] on a stub label + its definition line below
##   * Repeat template-expansion directive on a continuation row
##   * a STRUCK-THROUGH stub row (the author removed it from scope)
##   * a stub label authored as SEVERAL paragraphs in one cell (a label that
##     wrapped in Word) -- must join with a space, not fuse into one word

suppressPackageStartupMessages({
  library(officer)
  library(xml2)
})

.W_NS_URL <- "http://schemas.openxmlformats.org/wordprocessingml/2006/main"

here <- if (dir.exists("tests/testthat/fixtures")) {
  "tests/testthat/fixtures"
} else if (dir.exists("fixtures")) {
  "fixtures"
} else {
  "."
}

stub <- c(
  paste0("Subjects screened, n [ADSL.SCRNFN=1]"),
  paste0("Subjects completed [a] [Use the stated source variable for the ",
         "displayed item and apply the stated condition: USUBJID WHERE ",
         "ADSL.COMPLFL=\"Y\". Keep the display label separate from the ",
         "filter.]"),
  paste0("Subjects with any history [Count distinct subjects using USUBJID. ",
         "Apply the following condition: UNIQUE SUBJECTS WITH ",
         "ADMH.MHCAT=\"GENERAL HISTORY\".]"),
  paste0("Not coded [ADMH.MHDECOD NE \"\" AND ADMH.MHBODSYS=\"\"]"),
  paste0("Treatment added [Use ADCM.TRTSTAT for this displayed row; apply ",
         "ADCM.ONMEDFL=\"Y\" only as the row or record filter indicated by ",
         "the shell context.]"),
  paste0("... [Repeat the applicable row or section using this ordered ",
         "instruction: Repeat for ADCM.MEDPRES IN (\"DrugA\", \"DrugB\") ",
         "WHERE ADCM.ONMEDFL=\"Y\". Expand all repeated items before final ",
         "programming.]"),
  ## Post-processed below into a struck-through row (author removed it).
  "Subjects rescreened, n [ADSL.RESCRFL='Y']",
  ## Post-processed below into TWO paragraphs of one cell, split exactly
  ## where Word would wrap it: "...the data" / "extraction, n (%)".
  "Ongoing subjects at the time of the data@@extraction, n (%) [ADSL.EOSSTT='ONGOING']"
)

tbl <- data.frame(
  ` ` = stub,
  ## Post-processed below: this header annotation is split mid-token across
  ## two paragraphs, exactly as Word wraps a narrow header cell.
  `Alpha Cohort (N=XX) [ADSL.C@@OHORTN=1]` = rep("xx (xx.x)", length(stub)),
  `Beta Cohort (N=XX) [ADSL.COHORTN=2]`  = rep("xx (xx.x)", length(stub)),
  `Unknown (N=XX) [is.na(ADSL.COHORTN) OR ADSL.COHORTN==99]` =
    rep("xx (xx.x)", length(stub)),
  `Total (N=XX) [ADSL.COHORTN IN (1,2)]` = rep("xx (xx.x)", length(stub)),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

doc <- read_docx() |>
  body_add_par(
    paste0("Table 14.9.1 Subject Disposition - Screened Subjects ",
           "[ADSL. SCRNFL=\"Y\" [PROGRAMMING DATASETS USED: ADSL]]"),
    style = "heading 2") |>
  body_add_table(value = tbl, style = "table_template") |>
  body_add_table(
    value = data.frame(
      ` ` = c("Subjects re-screened after washout [ADSL.RESCRN=1]",
              "Subjects re-consented [ADSL.RECONFL=\'Y\']"),
      `Alpha Cohort (N=XX)` = rep("xx (xx.x)", 2),
      `Beta Cohort (N=XX)`  = rep("xx (xx.x)", 2),
      `Unknown (N=XX)`      = rep("xx (xx.x)", 2),
      `Total (N=XX)`        = rep("xx (xx.x)", 2),
      check.names = FALSE, stringsAsFactors = FALSE),
    style = "table_template") |>
  body_add_par("[a] Each subject will be counted only once.") |>
  body_add_par("Source: ADSL, ADMH, ADCM")

out_docx <- file.path(here, "annotated_shell_rwb_bracket.docx")
print(doc, target = out_docx)

## ---------------------------------------------------------------------------
## Post-process the OOXML for the two structural conventions officer cannot
## author directly: a struck-through row, and a stub label split across two
## paragraphs of one cell (the "@@" marker planted above).
## ---------------------------------------------------------------------------

rezip_docx <- function(td, docx_path) {
  ## Resolve against the DIRECTORY (which exists even after the old file is
  ## removed) -- normalizePath on a deleted relative path returns it
  ## unchanged, and the subsequent setwd() would then aim it inside `td`.
  target <- file.path(normalizePath(dirname(docx_path)), basename(docx_path))
  old <- setwd(td)
  on.exit(setwd(old), add = TRUE)
  utils::zip(target, files = list.files(".", recursive = TRUE, all.files = TRUE),
             flags = "-q -r -X")
}

td <- tempfile()
dir.create(td)
utils::unzip(out_docx, exdir = td)
doc_xml_path <- file.path(td, "word", "document.xml")
d <- xml2::read_xml(doc_xml_path)

cells <- xml2::xml_find_all(d, ".//*[local-name()='tc']")
for (cell in cells) {
  txt <- paste(xml2::xml_text(
    xml2::xml_find_all(cell, ".//*[local-name()='t']")), collapse = "")

  ## Struck-through row: mark EVERY run in the stub cell.
  if (grepl("Subjects rescreened", txt, fixed = TRUE)) {
    for (r in xml2::xml_find_all(cell, ".//*[local-name()='r']")) {
      rpr <- xml2::xml_find_first(r, "./*[local-name()='rPr']")
      if (inherits(rpr, "xml_missing")) {
        xml2::xml_add_child(r, xml2::read_xml(sprintf(
          '<w:rPr xmlns:w="%s"><w:strike/></w:rPr>', .W_NS_URL)), .where = 0)
      } else {
        xml2::xml_add_child(rpr, xml2::read_xml(sprintf(
          '<w:strike xmlns:w="%s"/>', .W_NS_URL)))
      }
    }
  }

  ## Wrapped label: replace the single paragraph with two, split at "@@".
  if (grepl("@@", txt, fixed = TRUE)) {
    halves <- strsplit(txt, "@@", fixed = TRUE)[[1]]
    para <- xml2::xml_find_first(cell, "./*[local-name()='p']")
    for (r in xml2::xml_find_all(para, "./*[local-name()='r']")) xml2::xml_remove(r)
    esc <- function(x) gsub("&", "&amp;", gsub("<", "&lt;", gsub(">", "&gt;", x)))
    xml2::xml_add_child(para, xml2::read_xml(sprintf(
      '<w:r xmlns:w="%s"><w:t xml:space="preserve">%s</w:t></w:r>',
      .W_NS_URL, esc(halves[1]))))
    xml2::xml_add_sibling(para, xml2::read_xml(sprintf(
      '<w:p xmlns:w="%s"><w:r><w:t xml:space="preserve">%s</w:t></w:r></w:p>',
      .W_NS_URL, esc(halves[2]))), .where = "after")
  }
}

xml2::write_xml(d, doc_xml_path)
unlink(out_docx)
rezip_docx(td, out_docx)
cat("Wrote:", out_docx, "\n")
