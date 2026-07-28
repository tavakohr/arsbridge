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

suppressPackageStartupMessages(library(officer))

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
         "programming.]")
)

tbl <- data.frame(
  ` ` = stub,
  `Alpha Cohort (N=XX) [ADSL.COHORTN=1]` = rep("xx (xx.x)", length(stub)),
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
  body_add_par("[a] Each subject will be counted only once.") |>
  body_add_par("Source: ADSL, ADMH, ADCM")

out_docx <- file.path(here, "annotated_shell_rwb_bracket.docx")
print(doc, target = out_docx)
cat("Wrote:", out_docx, "\n")
