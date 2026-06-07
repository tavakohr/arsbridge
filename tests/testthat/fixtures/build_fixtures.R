## tests/testthat/fixtures/build_fixtures.R
## ---------------------------------------------------------------------------
## Re-generates the binary test fixtures (.docx and .xlsx files). Run from
## the package root:
##
##   Rscript tests/testthat/fixtures/build_fixtures.R
##
## The XLSX fixture is built with openxlsx2; the DOCX fixture is built with
## officer for the basic structure, then the OOXML body is post-processed
## with xml2 to inject red C00000 runs into the population paragraph and
## the stub-column cells of each shell table.

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

## ---------------------------------------------------------------------------
## ADaM spec fixture (10 variables across ADSL + ADAE)
## ---------------------------------------------------------------------------

adam_vars <- data.frame(
  Dataset   = c(rep("ADSL", 6), rep("ADAE", 4)),
  Variable  = c("USUBJID","AGE","SEX","RACE","SAFFL","TRT01A",
                "USUBJID","AEDECOD","AEBODSYS","TRTEMFL"),
  Label     = c("Unique Subject Identifier","Age","Sex","Race",
                "Safety Population Flag","Actual Treatment for Period 01",
                "Unique Subject Identifier","Dictionary-Derived Term",
                "Body System or Organ Class","Treatment Emergent Analysis Flag"),
  Type      = c("Char","Num","Char","Char","Char","Char",
                "Char","Char","Char","Char"),
  Origin    = c("Assigned","CRF","CRF","CRF","Derived","Assigned",
                "Assigned","CRF","CRF","Derived"),
  Codelist  = c("","","SEX","RACE","NY","",
                "","MEDDRA","MEDDRA","NY"),
  Length    = c("40","8","1","40","1","40",
                "40","200","200","1"),
  Mandatory = c("Req","Req","Req","Req","Req","Req",
                "Req","Req","Req","Req"),
  stringsAsFactors = FALSE
)

wb <- openxlsx2::wb_workbook() |>
  openxlsx2::wb_add_worksheet("Variables") |>
  openxlsx2::wb_add_data(sheet = "Variables", x = adam_vars)
openxlsx2::wb_save(wb,
                   file = file.path(here, "adam_spec_minimal.xlsx"),
                   overwrite = TRUE)
cat("Wrote:", file.path(here, "adam_spec_minimal.xlsx"), "\n")

## ---------------------------------------------------------------------------
## Annotated 2-TLF shell fixture
## Step 1: skeleton with officer (plain text -- title, pop line, table cells)
## Step 2: post-process OOXML to add C00000 red runs to specific cells
## ---------------------------------------------------------------------------

doc <- read_docx() |>
  body_add_par("Table 14.1.1", style = "heading 2") |>
  body_add_par("Summary of Demographic and Baseline Characteristics") |>
  body_add_par("Safety Population (ADSL.SAFFL='Y')") |>
  body_add_table(
    value = data.frame(
      Characteristic = c("Age (years)  ADSL.AGE",
                         "  n",
                         "  Mean (SD)",
                         "  Median",
                         "Sex, n (%)  ADSL.SEX",
                         "  Male",
                         "  Female",
                         "Race, n (%)  ADSL.RACE",
                         "  White",
                         "  Black or African American"),
      `Treatment A` = rep("", 10),
      `Placebo`     = rep("", 10),
      check.names   = FALSE,
      stringsAsFactors = FALSE
    ),
    style = "table_template"
  ) |>
  body_add_par("Source: ADSL") |>

  body_add_par("Table 14.3.1", style = "heading 2") |>
  body_add_par("Summary of Treatment-Emergent Adverse Events") |>
  body_add_par("Safety Population (ADSL.SAFFL='Y')") |>
  body_add_table(
    value = data.frame(
      Category = c("Any TEAE  ADAE.TRTEMFL='Y'",
                   "  n (%)",
                   "Most Common TEAEs (>=5%)  ADAE.AEDECOD",
                   "  Headache",
                   "  Nausea"),
      `Treatment A` = rep("", 5),
      `Placebo`     = rep("", 5),
      check.names   = FALSE,
      stringsAsFactors = FALSE
    ),
    style = "table_template"
  ) |>
  body_add_par("Source: ADSL, ADAE")

out_docx <- file.path(here, "annotated_shell_2tlf_minimal.docx")
print(doc, target = out_docx)

## Post-process: re-open as raw OOXML and re-style specific cells/paras so
## the annotation portion of each cell gets a red C00000 run (Layer 1
## detection path). The Layer 3 path is already exercised because the
## annotation text alone matches the ADaM regex even in plain text.
.W_NS_URL <- "http://schemas.openxmlformats.org/wordprocessingml/2006/main"

repaint_red <- function(docx_path) {
  td <- tempfile()
  dir.create(td)
  utils::unzip(docx_path, exdir = td)
  doc_xml_path <- file.path(td, "word", "document.xml")
  d <- xml2::read_xml(doc_xml_path)

  ## Walk every paragraph; for any paragraph whose concatenated text contains
  ## "  ADxxx.xxx..." (two-space separator + ADaM pattern), split its single
  ## run into a black "label  " run and a red "annotation" run.
  ADAM_RE <- paste0("\\bAD[A-Z]{1,6}\\.[A-Z][A-Z0-9]{0,7}\\b")

  paras <- xml2::xml_find_all(d, ".//*[local-name()='p']")
  for (p in paras) {
    t_nodes <- xml2::xml_find_all(p, ".//*[local-name()='t']")
    if (length(t_nodes) == 0) next
    full <- paste(xml2::xml_text(t_nodes), collapse = "")
    m <- regexpr(ADAM_RE, full, perl = TRUE)
    if (m == -1) next
    pos <- as.integer(m)
    label <- substr(full, 1, pos - 1L)
    annot <- substr(full, pos, nchar(full))

    ## Replace the FIRST t node's text with the label; remove the runs of the
    ## remaining t nodes and append a new red run with the annotation.
    runs <- xml2::xml_find_all(p, "./*[local-name()='r']")
    if (length(runs) == 0) next
    ## Set first run's text to label, then strip every following run.
    first_r <- runs[[1]]
    first_t <- xml2::xml_find_first(first_r, ".//*[local-name()='t']")
    if (!inherits(first_t, "xml_missing")) {
      xml2::xml_text(first_t) <- label
      xml2::xml_set_attr(first_t, "xml:space", "preserve")
    }
    if (length(runs) > 1) for (rr in runs[-1]) xml2::xml_remove(rr)

    ## Add the red annotation run.
    red_run <- xml2::read_xml(sprintf(
      '<w:r xmlns:w="%s"><w:rPr><w:color w:val="C00000"/></w:rPr><w:t xml:space="preserve">%s</w:t></w:r>',
      .W_NS_URL,
      gsub("&", "&amp;", gsub("<", "&lt;", gsub(">", "&gt;", annot)))
    ))
    xml2::xml_add_child(p, red_run)
  }

  ## Mark the Source: line(s) grey so detection skips them correctly.
  source_paras <- Filter(function(p) {
    txt <- paste(xml2::xml_text(xml2::xml_find_all(p, ".//*[local-name()='t']")),
                 collapse = "")
    grepl("^\\s*Source\\s*:", txt, ignore.case = TRUE)
  }, paras)
  for (sp in source_paras) {
    sp_runs <- xml2::xml_find_all(sp, "./*[local-name()='r']")
    for (r in sp_runs) {
      ## Inject a grey color into rPr (create rPr if missing).
      rpr <- xml2::xml_find_first(r, "./*[local-name()='rPr']")
      grey_color <- xml2::read_xml(sprintf(
        '<w:color xmlns:w="%s" w:val="808080"/>', .W_NS_URL
      ))
      if (inherits(rpr, "xml_missing")) {
        new_rpr <- xml2::read_xml(sprintf(
          '<w:rPr xmlns:w="%s"><w:color w:val="808080"/></w:rPr>', .W_NS_URL
        ))
        xml2::xml_add_child(r, new_rpr, .where = 0)
      } else {
        xml2::xml_add_child(rpr, grey_color)
      }
    }
  }

  xml2::write_xml(d, doc_xml_path)

  ## Re-zip into the original .docx (must preserve relative paths inside zip).
  cwd <- getwd()
  on.exit(setwd(cwd), add = TRUE)
  setwd(td)
  files <- list.files(".", recursive = TRUE, full.names = FALSE)
  files <- sub("^./", "", files)
  ## Remove the original file before re-zipping with absolute path.
  abs_out <- normalizePath(docx_path, winslash = "/", mustWork = FALSE)
  unlink(abs_out)
  utils::zip(zipfile = abs_out, files = files, flags = "-r9X")
  setwd(cwd)
  invisible(docx_path)
}

repaint_red(out_docx)
cat("Wrote:", out_docx, "\n")
