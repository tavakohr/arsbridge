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
  ## an ADaM reference (a bare "DATASET.VAR", or "DATASET.VAR='value'"),
  ## split its text into three runs: a black "label" run before the match, a
  ## red "annotation" run for the match itself, and -- for a population line
  ## like "... (ADSL.SAFFL='Y')" -- a black "trailing" run for whatever
  ## comes after the match (the closing paren here), so that text is kept
  ## instead of silently swallowed into the red run.
  ## Value forms: single-quoted, straight double-quoted, smart double-quoted
  ## (repaint runs on raw OOXML, BEFORE the parser's quote normalization),
  ## or unquoted numeric.
  ADAM_RE <- paste0(
    "\\bAD[A-Z]{1,6}\\.[A-Z][A-Z0-9]{0,7}",
    "(?:\\s*=\\s*(?:'[^']*'|\"[^\"]*\"|“[^“”]*”|[-+]?\\d+(?:\\.\\d+)?))?"
  )

  paras <- xml2::xml_find_all(d, ".//*[local-name()='p']")
  for (p in paras) {
    t_nodes <- xml2::xml_find_all(p, ".//*[local-name()='t']")
    if (length(t_nodes) == 0) next
    full <- paste(xml2::xml_text(t_nodes), collapse = "")
    m <- regexpr(ADAM_RE, full, perl = TRUE)
    if (m == -1) next
    start    <- as.integer(m)
    match_len <- attr(m, "match.length")
    label    <- substr(full, 1, start - 1L)
    annot    <- substr(full, start, start + match_len - 1L)
    trailing <- substr(full, start + match_len, nchar(full))

    ## Replace the FIRST t node's text with the label; remove the runs of the
    ## remaining t nodes and append a new red run with the annotation (plus
    ## a plain trailing run, if any text follows the match).
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

    escape_xml_text <- function(x) gsub("&", "&amp;", gsub("<", "&lt;", gsub(">", "&gt;", x)))

    red_run <- xml2::read_xml(sprintf(
      '<w:r xmlns:w="%s"><w:rPr><w:color w:val="C00000"/></w:rPr><w:t xml:space="preserve">%s</w:t></w:r>',
      .W_NS_URL, escape_xml_text(annot)
    ))
    xml2::xml_add_child(p, red_run)

    if (nzchar(trailing)) {
      trailing_run <- xml2::read_xml(sprintf(
        '<w:r xmlns:w="%s"><w:t xml:space="preserve">%s</w:t></w:r>',
        .W_NS_URL, escape_xml_text(trailing)
      ))
      xml2::xml_add_child(p, trailing_run)
    }
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
  rezip_docx(td, docx_path)
}

#' Re-zip an unpacked docx directory `td` back into `docx_path` (must
#' preserve relative paths inside the zip). Shared by every fixture builder
#' below that edits `word/document.xml` directly.
#' @noRd
rezip_docx <- function(td, docx_path) {
  ## Resolve the output path BEFORE changing directory -- a relative
  ## `docx_path` (the normal case when this script runs from the repo root)
  ## must resolve against the original working directory, not against `td`.
  abs_out <- normalizePath(docx_path, winslash = "/", mustWork = FALSE)

  cwd <- getwd()
  on.exit(setwd(cwd), add = TRUE)
  setwd(td)
  ## all.files = TRUE: a docx package requires dotfiles like `_rels/.rels`,
  ## which list.files() otherwise silently drops as "hidden".
  files <- list.files(".", recursive = TRUE, all.files = TRUE, full.names = FALSE)
  files <- sub("^./", "", files)
  unlink(abs_out)
  utils::zip(zipfile = abs_out, files = files, flags = "-r9X")
  setwd(cwd)
  invisible(docx_path)
}

repaint_red(out_docx)
cat("Wrote:", out_docx, "\n")

## ---------------------------------------------------------------------------
## F1 fixtures -- flexible heading / title / population state machine.
## Each one is a single minimal TLF, plain Layer-3 text annotations (no
## colour post-processing needed): F1 is about the paragraph state machine,
## not about which detection layer fires.
## ---------------------------------------------------------------------------

## Heading and title share one paragraph: "Table 14.1.1: Summary of ...".
doc_inline <- read_docx() |>
  body_add_par("Table 14.1.1: Summary of Demographic Characteristics",
              style = "heading 2") |>
  body_add_par("Safety Population (ADSL.SAFFL='Y')") |>
  body_add_table(
    value = data.frame(
      Characteristic = c("Age (years)  ADSL.AGE", "  n", "  Mean (SD)"),
      `Treatment A` = rep("", 3),
      `Placebo`     = rep("", 3),
      check.names   = FALSE,
      stringsAsFactors = FALSE
    ),
    style = "table_template"
  ) |>
  body_add_par("Source: ADSL")
print(doc_inline, target = file.path(here, "annotated_shell_inline_heading.docx"))
cat("Wrote:", file.path(here, "annotated_shell_inline_heading.docx"), "\n")

## A listing with no population line at all: heading, title, then straight
## into the table.
doc_no_pop <- read_docx() |>
  body_add_par("Listing 16.2.1", style = "heading 2") |>
  body_add_par("Subject-Level Listing of Deaths") |>
  body_add_table(
    value = data.frame(
      `Subject ID`   = c("USUBJID", "101", "102"),
      `Age (years)`  = c("AGE", "64", "71"),
      check.names    = FALSE,
      stringsAsFactors = FALSE
    ),
    style = "table_template"
  ) |>
  body_add_par("Source: ADSL")
print(doc_no_pop, target = file.path(here, "annotated_shell_no_population.docx"))
cat("Wrote:", file.path(here, "annotated_shell_no_population.docx"), "\n")

## Title wraps two paragraphs before the population line arrives.
doc_two_line <- read_docx() |>
  body_add_par("Table 14.7.1", style = "heading 2") |>
  body_add_par("Summary of Concomitant Medications") |>
  body_add_par("by ATC Class") |>
  body_add_par("Safety Population (ADSL.SAFFL='Y')") |>
  body_add_table(
    value = data.frame(
      Category = c("Any Medication  ADCM.CMDECOD", "  Aspirin", "  Ibuprofen"),
      `Treatment A` = rep("", 3),
      `Placebo`     = rep("", 3),
      check.names   = FALSE,
      stringsAsFactors = FALSE
    ),
    style = "table_template"
  ) |>
  body_add_par("Source: ADSL, ADCM")
print(doc_two_line, target = file.path(here, "annotated_shell_two_line_title.docx"))
cat("Wrote:", file.path(here, "annotated_shell_two_line_title.docx"), "\n")

## ---------------------------------------------------------------------------
## F3 fixture -- merged/spanned headers, a vMerge stub continuation, and an
## annotation living in a data cell instead of the stub column. officer's
## body_add_table() can't express gridSpan/vMerge, so this table is written
## as raw OOXML and spliced in where a placeholder paragraph used to be.
## ---------------------------------------------------------------------------

#' Splice a raw `<w:tbl>` XML fragment into `docx_path`, replacing the first
#' paragraph whose text is exactly `marker`.
#' @noRd
inject_raw_table <- function(docx_path, table_xml, marker) {
  td <- tempfile()
  dir.create(td)
  utils::unzip(docx_path, exdir = td)
  doc_xml_path <- file.path(td, "word", "document.xml")
  d <- xml2::read_xml(doc_xml_path)

  paras <- xml2::xml_find_all(d, ".//*[local-name()='p']")
  marker_text <- function(p) {
    paste(xml2::xml_text(xml2::xml_find_all(p, ".//*[local-name()='t']")),
         collapse = "")
  }
  hit <- Filter(function(p) identical(marker_text(p), marker), paras)
  if (length(hit) == 0) stop("marker paragraph not found: ", marker)

  tbl_node <- xml2::read_xml(table_xml)
  xml2::xml_add_sibling(hit[[1]], tbl_node, .where = "before")
  xml2::xml_remove(hit[[1]])

  xml2::write_xml(d, doc_xml_path)
  rezip_docx(td, docx_path)
}

## Two header rows (arm names spanning their n / (%) subcolumns), one data
## row whose annotation sits in a data cell (not the stub), and one stub
## cell ("Headache") vertically merged across two rows -- the second row is
## a vMerge continuation that must be dropped, not read as its own row.
merged_header_table_xml <- paste0(
  '<w:tbl xmlns:w="', .W_NS_URL, '">',
  '<w:tblPr><w:tblStyle w:val="TableGrid"/><w:tblW w:w="0" w:type="auto"/></w:tblPr>',
  '<w:tblGrid><w:gridCol/><w:gridCol/><w:gridCol/><w:gridCol/><w:gridCol/></w:tblGrid>',

  '<w:tr><w:trPr><w:tblHeader/></w:trPr>',
  '<w:tc><w:tcPr/><w:p><w:r><w:t>Category</w:t></w:r></w:p></w:tc>',
  '<w:tc><w:tcPr><w:gridSpan w:val="2"/></w:tcPr><w:p><w:r><w:t>Treatment A</w:t></w:r></w:p></w:tc>',
  '<w:tc><w:tcPr><w:gridSpan w:val="2"/></w:tcPr><w:p><w:r><w:t>Placebo</w:t></w:r></w:p></w:tc>',
  '</w:tr>',

  '<w:tr><w:trPr><w:tblHeader/></w:trPr>',
  '<w:tc><w:tcPr/><w:p/></w:tc>',
  '<w:tc><w:tcPr/><w:p><w:r><w:t>n</w:t></w:r></w:p></w:tc>',
  '<w:tc><w:tcPr/><w:p><w:r><w:t>(%)</w:t></w:r></w:p></w:tc>',
  '<w:tc><w:tcPr/><w:p><w:r><w:t>n</w:t></w:r></w:p></w:tc>',
  '<w:tc><w:tcPr/><w:p><w:r><w:t>(%)</w:t></w:r></w:p></w:tc>',
  '</w:tr>',

  '<w:tr>',
  '<w:tc><w:tcPr/><w:p><w:r><w:t>Any AE</w:t></w:r></w:p></w:tc>',
  "<w:tc><w:tcPr/><w:p><w:r><w:t>ADAE.TRTEMFL='Y'</w:t></w:r></w:p></w:tc>",
  '<w:tc><w:tcPr/><w:p/></w:tc>',
  '<w:tc><w:tcPr/><w:p/></w:tc>',
  '<w:tc><w:tcPr/><w:p/></w:tc>',
  '</w:tr>',

  '<w:tr>',
  '<w:tc><w:tcPr><w:vMerge w:val="restart"/></w:tcPr><w:p><w:r><w:t>Headache</w:t></w:r></w:p></w:tc>',
  '<w:tc><w:tcPr/><w:p><w:r><w:t>3</w:t></w:r></w:p></w:tc>',
  '<w:tc><w:tcPr/><w:p><w:r><w:t>15.0</w:t></w:r></w:p></w:tc>',
  '<w:tc><w:tcPr/><w:p><w:r><w:t>1</w:t></w:r></w:p></w:tc>',
  '<w:tc><w:tcPr/><w:p><w:r><w:t>5.0</w:t></w:r></w:p></w:tc>',
  '</w:tr>',

  '<w:tr>',
  '<w:tc><w:tcPr><w:vMerge/></w:tcPr><w:p/></w:tc>',
  '<w:tc><w:tcPr/><w:p><w:r><w:t>1</w:t></w:r></w:p></w:tc>',
  '<w:tc><w:tcPr/><w:p><w:r><w:t>5.0</w:t></w:r></w:p></w:tc>',
  '<w:tc><w:tcPr/><w:p><w:r><w:t>0</w:t></w:r></w:p></w:tc>',
  '<w:tc><w:tcPr/><w:p><w:r><w:t>0.0</w:t></w:r></w:p></w:tc>',
  '</w:tr>',

  '</w:tbl>'
)

doc_merged <- read_docx() |>
  body_add_par("Table 14.2.1", style = "heading 2") |>
  body_add_par("Adverse Events by Treatment and Severity") |>
  body_add_par("Safety Population (ADSL.SAFFL='Y')") |>
  body_add_par("%%TABLE%%") |>
  body_add_par("Source: ADSL, ADAE")
out_merged <- file.path(here, "annotated_shell_merged_headers.docx")
print(doc_merged, target = out_merged)
inject_raw_table(out_merged, merged_header_table_xml, "%%TABLE%%")
cat("Wrote:", out_merged, "\n")

## ---------------------------------------------------------------------------
## F4 fixture -- Word comments, highlight (no font colour), a tracked-change
## deletion sitting next to its live replacement, and a text box's stray
## text. officer's default docx skeleton already ships an (empty)
## word/comments.xml and its content-type registration, so we only need to
## append one <w:comment> element to it.
## ---------------------------------------------------------------------------

#' Append a `<w:comment>` element to `word/comments.xml` inside `docx_path`.
#' @noRd
add_comment_to_docx <- function(docx_path, comment_id, comment_text) {
  td <- tempfile()
  dir.create(td)
  utils::unzip(docx_path, exdir = td)
  comments_path <- file.path(td, "word", "comments.xml")
  d <- xml2::read_xml(comments_path)

  comment_node <- xml2::read_xml(paste0(
    '<w:comment xmlns:w="', .W_NS_URL, '" w:id="', comment_id,
    '" w:author="Lead Programmer">',
    '<w:p><w:r><w:t>', comment_text, '</w:t></w:r></w:p>',
    '</w:comment>'
  ))
  xml2::xml_add_child(d, comment_node)
  xml2::write_xml(d, comments_path)
  rezip_docx(td, docx_path)
}

## Row 1: the annotation lives in a comment anchored to the cell, not the
## cell text itself (label stays plain: "Weight (kg)").
## Row 2: the annotation is a highlighted (yellow) run, no font colour.
## Row 3: a stale annotation was deleted (tracked changes) next to its live
## replacement. Some producers leave a plain <w:t> (not <w:delText>) inside
## <w:del> -- that non-conforming shape is exercised directly here.
## Row 4: a text box's callout text ("IGNORE ME") must not leak into the
## stub label ("Notes").
comments_highlights_table_xml <- paste0(
  '<w:tbl xmlns:w="', .W_NS_URL, '">',
  '<w:tblPr><w:tblStyle w:val="TableGrid"/><w:tblW w:w="0" w:type="auto"/></w:tblPr>',
  '<w:tblGrid><w:gridCol/><w:gridCol/><w:gridCol/></w:tblGrid>',

  '<w:tr>',
  '<w:tc><w:tcPr/><w:p><w:r><w:t>Category</w:t></w:r></w:p></w:tc>',
  '<w:tc><w:tcPr/><w:p><w:r><w:t>Treatment A</w:t></w:r></w:p></w:tc>',
  '<w:tc><w:tcPr/><w:p><w:r><w:t>Placebo</w:t></w:r></w:p></w:tc>',
  '</w:tr>',

  '<w:tr>',
  '<w:tc><w:tcPr/><w:p>',
  '<w:r><w:t>Weight (kg)</w:t></w:r>',
  '<w:r><w:commentReference w:id="1"/></w:r>',
  '</w:p></w:tc>',
  '<w:tc><w:tcPr/><w:p/></w:tc>',
  '<w:tc><w:tcPr/><w:p/></w:tc>',
  '</w:tr>',

  '<w:tr>',
  '<w:tc><w:tcPr/><w:p>',
  '<w:r><w:t xml:space="preserve">Height (cm)  </w:t></w:r>',
  '<w:r><w:rPr><w:highlight w:val="yellow"/></w:rPr><w:t>ADSL.HEIGHT</w:t></w:r>',
  '</w:p></w:tc>',
  '<w:tc><w:tcPr/><w:p/></w:tc>',
  '<w:tc><w:tcPr/><w:p/></w:tc>',
  '</w:tr>',

  '<w:tr>',
  '<w:tc><w:tcPr/><w:p>',
  '<w:r><w:t xml:space="preserve">Age (years)  </w:t></w:r>',
  '<w:del w:id="9" w:author="Lead Programmer"><w:r><w:t>ADSL.OLDVAR</w:t></w:r></w:del>',
  '<w:r><w:t>ADSL.AGE</w:t></w:r>',
  '</w:p></w:tc>',
  '<w:tc><w:tcPr/><w:p/></w:tc>',
  '<w:tc><w:tcPr/><w:p/></w:tc>',
  '</w:tr>',

  '<w:tr>',
  '<w:tc><w:tcPr/><w:p>',
  '<w:r><w:t>Notes</w:t></w:r>',
  '<w:txbxContent><w:p><w:r><w:t>IGNORE ME</w:t></w:r></w:p></w:txbxContent>',
  '</w:p></w:tc>',
  '<w:tc><w:tcPr/><w:p/></w:tc>',
  '<w:tc><w:tcPr/><w:p/></w:tc>',
  '</w:tr>',

  '</w:tbl>'
)

doc_comments <- read_docx() |>
  body_add_par("Table 14.9.1", style = "heading 2") |>
  body_add_par("Body Weight and Age Summary") |>
  body_add_par("Safety Population (ADSL.SAFFL='Y')") |>
  body_add_par("%%TABLE%%") |>
  body_add_par("Source: ADSL")
out_comments <- file.path(here, "annotated_shell_comments_highlights.docx")
print(doc_comments, target = out_comments)
inject_raw_table(out_comments, comments_highlights_table_xml, "%%TABLE%%")
add_comment_to_docx(out_comments, "1", "Use ADSL.WEIGHT for this row.")
cat("Wrote:", out_comments, "\n")

## ---------------------------------------------------------------------------
## F2 fixture -- TLF number, title, and population live in the page header;
## the body has only the table and a Source line, no heading paragraph at
## all. arsbridge's reader only looks at the header PART's own content, so
## this fixture skips the relationship/content-type wiring a "real" Word
## header needs -- that plumbing is irrelevant to what the reader parses.
## ---------------------------------------------------------------------------

#' Add a minimal `word/headerN.xml` part directly to `docx_path`, containing
#' one paragraph per element of `paragraphs`.
#' @noRd
add_header_part <- function(docx_path, header_filename, paragraphs) {
  td <- tempfile()
  dir.create(td)
  utils::unzip(docx_path, exdir = td)

  paras_xml <- paste(vapply(paragraphs, function(txt) {
    sprintf('<w:p><w:r><w:t xml:space="preserve">%s</w:t></w:r></w:p>', txt)
  }, character(1)), collapse = "")
  header_node <- xml2::read_xml(paste0(
    '<w:hdr xmlns:w="', .W_NS_URL, '">', paras_xml, '</w:hdr>'
  ))
  xml2::write_xml(header_node, file.path(td, "word", header_filename))

  rezip_docx(td, docx_path)
}

doc_header_title <- read_docx() |>
  body_add_table(
    value = data.frame(
      Characteristic = c("BMD (g/cm2)  ADSL.WEIGHT", "  n", "  Mean (SD)"),
      `Treatment A` = rep("", 3),
      `Placebo`     = rep("", 3),
      check.names   = FALSE,
      stringsAsFactors = FALSE
    ),
    style = "table_template"
  ) |>
  body_add_par("Source: ADSL")
out_header_title <- file.path(here, "annotated_shell_page_header_title.docx")
print(doc_header_title, target = out_header_title)
add_header_part(out_header_title, "header1.xml", c(
  "Table 14.5.1",
  "Bone Mineral Density Change from Baseline",
  "Safety Population (ADSL.SAFFL='Y')"
))
cat("Wrote:", out_header_title, "\n")

## ---------------------------------------------------------------------------
## Re-evaluation fixtures -- regression cover for the issues confirmed in
## REEVALUATION_p0-parsing_premerge.md.
## ---------------------------------------------------------------------------

## §2.1 -- the body has its own heading (Table 1.1) but no title; the only
## page header names a DIFFERENT TLF (Table 9.9.9). The parser must NOT adopt
## the mismatched header title/population, and must WARN.
doc_mismatch <- read_docx() |>
  body_add_par("Table 1.1", style = "heading 2") |>
  body_add_table(
    value = data.frame(
      Characteristic = c("Age (years)  ADSL.AGE", "  n"),
      `Treatment A` = rep("", 2),
      `Placebo`     = rep("", 2),
      check.names   = FALSE,
      stringsAsFactors = FALSE
    ),
    style = "table_template"
  ) |>
  body_add_par("Source: ADSL")
out_mismatch <- file.path(here, "annotated_shell_header_mismatch.docx")
print(doc_mismatch, target = out_mismatch)
add_header_part(out_mismatch, "header1.xml", c(
  "Table 9.9.9",
  "A Stale Template Title That Belongs To Nothing",
  "Per-Protocol Population"
))
cat("Wrote:", out_mismatch, "\n")

## §2.2 -- a two-row nested header (arm labels spanning n / (%) subcolumns)
## with NO <w:tblHeader/> flag on either row, which is how most shell authors
## actually produce nested headers. The reader must infer the second header
## row (so no ghost stub row, and the subcolumn labels survive) and WARN.
unflagged_header_table_xml <- paste0(
  '<w:tbl xmlns:w="', .W_NS_URL, '">',
  '<w:tblPr><w:tblStyle w:val="TableGrid"/><w:tblW w:w="0" w:type="auto"/></w:tblPr>',
  '<w:tblGrid><w:gridCol/><w:gridCol/><w:gridCol/><w:gridCol/><w:gridCol/></w:tblGrid>',

  ## header row 1 -- spanned arm labels, NO tblHeader flag
  '<w:tr>',
  '<w:tc><w:tcPr/><w:p><w:r><w:t>Category</w:t></w:r></w:p></w:tc>',
  '<w:tc><w:tcPr><w:gridSpan w:val="2"/></w:tcPr><w:p><w:r><w:t>Treatment A</w:t></w:r></w:p></w:tc>',
  '<w:tc><w:tcPr><w:gridSpan w:val="2"/></w:tcPr><w:p><w:r><w:t>Placebo</w:t></w:r></w:p></w:tc>',
  '</w:tr>',

  ## header row 2 -- n / (%) subcolumns, blank first cell, NO tblHeader flag
  '<w:tr>',
  '<w:tc><w:tcPr/><w:p/></w:tc>',
  '<w:tc><w:tcPr/><w:p><w:r><w:t>n</w:t></w:r></w:p></w:tc>',
  '<w:tc><w:tcPr/><w:p><w:r><w:t>(%)</w:t></w:r></w:p></w:tc>',
  '<w:tc><w:tcPr/><w:p><w:r><w:t>n</w:t></w:r></w:p></w:tc>',
  '<w:tc><w:tcPr/><w:p><w:r><w:t>(%)</w:t></w:r></w:p></w:tc>',
  '</w:tr>',

  ## the single data row
  '<w:tr>',
  "<w:tc><w:tcPr/><w:p><w:r><w:t>Any AE  ADAE.TRTEMFL='Y'</w:t></w:r></w:p></w:tc>",
  '<w:tc><w:tcPr/><w:p/></w:tc><w:tc><w:tcPr/><w:p/></w:tc>',
  '<w:tc><w:tcPr/><w:p/></w:tc><w:tc><w:tcPr/><w:p/></w:tc>',
  '</w:tr>',

  '</w:tbl>'
)
doc_unflagged <- read_docx() |>
  body_add_par("Table 14.2.9", style = "heading 2") |>
  body_add_par("Adverse Events (unflagged nested header)") |>
  body_add_par("Safety Population") |>
  body_add_par("%%TABLE%%") |>
  body_add_par("Source: ADAE")
out_unflagged <- file.path(here, "annotated_shell_unflagged_headers.docx")
print(doc_unflagged, target = out_unflagged)
inject_raw_table(out_unflagged, unflagged_header_table_xml, "%%TABLE%%")
cat("Wrote:", out_unflagged, "\n")

## §3a -- a treatment-column mapping arrow line sits directly after the title
## (no population line). It must NOT be read as the population; it must reach
## programmer_annotations and bind as the column-axis grouping.
doc_arrow <- read_docx() |>
  body_add_par("Table 14.4.1", style = "heading 2") |>
  body_add_par("Summary of Exposure") |>
  body_add_par("Treatment columns -> ADSL.TRT01A") |>
  body_add_table(
    value = data.frame(
      Characteristic = c("Duration (days)  ADSL.TRTDURD", "  Mean (SD)"),
      `Treatment A` = rep("", 2),
      `Placebo`     = rep("", 2),
      check.names   = FALSE,
      stringsAsFactors = FALSE
    ),
    style = "table_template"
  ) |>
  body_add_par("Source: ADSL")
out_arrow <- file.path(here, "annotated_shell_arrow_after_title.docx")
print(doc_arrow, target = out_arrow)
cat("Wrote:", out_arrow, "\n")

## Probe 3 -- a pre-table footnote ("Note: ...") sits between the title and
## the table. It must NOT be glued onto the title; it belongs in footnotes.
doc_pretable_fn <- read_docx() |>
  body_add_par("Table 14.3.1", style = "heading 2") |>
  body_add_par("Summary of Vital Signs") |>
  body_add_par("Note: baseline is the last non-missing value before first dose.") |>
  body_add_table(
    value = data.frame(
      Characteristic = c("Systolic BP (mmHg)  ADVS.AVAL", "  Mean (SD)"),
      `Treatment A` = rep("", 2),
      `Placebo`     = rep("", 2),
      check.names   = FALSE,
      stringsAsFactors = FALSE
    ),
    style = "table_template"
  ) |>
  body_add_par("Source: ADVS")
out_pretable_fn <- file.path(here, "annotated_shell_pretable_footnote.docx")
print(doc_pretable_fn, target = out_pretable_fn)
cat("Wrote:", out_pretable_fn, "\n")

## Probe 5 -- a Word comment carrying the annotation is anchored to a DATA
## cell (the value cell), not the stub. It must still bind to the row.
datacell_comment_table_xml <- paste0(
  '<w:tbl xmlns:w="', .W_NS_URL, '">',
  '<w:tblPr><w:tblStyle w:val="TableGrid"/><w:tblW w:w="0" w:type="auto"/></w:tblPr>',
  '<w:tblGrid><w:gridCol/><w:gridCol/></w:tblGrid>',
  '<w:tr>',
  '<w:tc><w:tcPr/><w:p><w:r><w:t>Category</w:t></w:r></w:p></w:tc>',
  '<w:tc><w:tcPr/><w:p><w:r><w:t>Treatment A</w:t></w:r></w:p></w:tc>',
  '</w:tr>',
  '<w:tr>',
  '<w:tc><w:tcPr/><w:p><w:r><w:t>Weight (kg)</w:t></w:r></w:p></w:tc>',
  '<w:tc><w:tcPr/><w:p><w:r><w:t>xx.x</w:t></w:r><w:r><w:commentReference w:id="1"/></w:r></w:p></w:tc>',
  '</w:tr>',
  '</w:tbl>'
)
doc_dc_comment <- read_docx() |>
  body_add_par("Table 14.6.1", style = "heading 2") |>
  body_add_par("Body Weight") |>
  body_add_par("Safety Population") |>
  body_add_par("%%TABLE%%") |>
  body_add_par("Source: ADSL")
out_dc_comment <- file.path(here, "annotated_shell_datacell_comment.docx")
print(doc_dc_comment, target = out_dc_comment)
inject_raw_table(out_dc_comment, datacell_comment_table_xml, "%%TABLE%%")
add_comment_to_docx(out_dc_comment, "1", "Use ADSL.WEIGHT for this row.")
cat("Wrote:", out_dc_comment, "\n")

## ---------------------------------------------------------------------------
## RWE-style fixtures -- one-line headings that carry the number, title,
## dash-separated population, inline annotation (smart double quotes), and a
## "[PROGRAMMING DATASETS USED: ...]" suffix all in ONE paragraph, the way
## some sponsor RWE shells are authored. Also: a numbered section heading
## ("14.1 ...") that must NOT start a section, an e-signature banner line,
## unquoted-numeric column annotations, a tracked-change deletion that must
## not leak into cell text, and a docProps/core.xml whose namespace officer
## warns about (the warning arsbridge now muffles).
## ---------------------------------------------------------------------------

## Spec fixture for the RWE shells (kept separate from adam_spec_minimal so
## existing tests keep their exact variable universe).
rwe_vars <- data.frame(
  Dataset   = rep("ADSL", 7),
  Variable  = c("USUBJID", "AGE", "AGEGR1N", "SEX", "SCRNFL", "COMPLFL",
                "COHORTN"),
  Label     = c("Unique Subject Identifier", "Age", "Age Group 1 (N)",
                "Sex", "Screened Population Flag", "Completer Population Flag",
                "Cohort (N)"),
  Type      = c("Char", "Num", "Num", "Char", "Char", "Char", "Num"),
  Origin    = c("Assigned", "CRF", "Derived", "CRF", "Derived", "Derived",
                "Assigned"),
  Codelist  = c("", "", "", "SEX", "NY", "NY", ""),
  Length    = c("40", "8", "8", "1", "1", "1", "8"),
  Mandatory = rep("Req", 7),
  stringsAsFactors = FALSE
)
wb_rwe <- openxlsx2::wb_workbook() |>
  openxlsx2::wb_add_worksheet("Variables") |>
  openxlsx2::wb_add_data(sheet = "Variables", x = rwe_vars)
openxlsx2::wb_save(wb_rwe,
                   file = file.path(here, "adam_spec_rwe.xlsx"),
                   overwrite = TRUE)
cat("Wrote:", file.path(here, "adam_spec_rwe.xlsx"), "\n")

## The two one-line headings, written with a real en dash and real smart
## double quotes -- exactly as Word autocorrect stores them.
rwe_heading_1 <- paste0(
  "Table 14.1.1 Summary of Subject Status and Subject Disposition ",
  "– Screened Subjects ADSL.SCRNFL=“Y” ",
  "[PROGRAMMING DATASETS USED: ADSL]"
)
rwe_heading_2 <- paste0(
  "Table 14.1.3 Demographics – Completed Population ",
  "ADSL.COMPLFL = “Y”"
)

doc_rwe <- read_docx() |>
  body_add_par("Docusign Envelope ID: 00000000-DEMO-FIXTURE-0000") |>
  body_add_par("14.1 Demographic and Baseline Tables", style = "heading 1") |>
  body_add_par(rwe_heading_1) |>
  body_add_table(
    value = data.frame(
      ` `        = c("Subjects screened, n  ADSL.SCRNFL=“Y”",
                     "Age at enrollment (years)  ADSL.AGE",
                     "  n",
                     "  Mean (SD)",
                     "Age group at enrollment, n (%)  ADSL.AGEGR1N",
                     "  <65",
                     "  >=65 TRACKDEL"),
      `Cohort 1 (N=XX) ADSL.COHORTN=1` = rep("", 7),
      `Cohort 2 (N=XX) ADSL.COHORTN=2` = rep("", 7),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    style = "table_template"
  ) |>
  body_add_par(rwe_heading_2) |>
  body_add_table(
    value = data.frame(
      ` `     = c("Sex, n (%)  ADSL.SEX", "  Male", "  Female"),
      `Cohort 1 (N=XX)` = rep("", 3),
      `Cohort 2 (N=XX)` = rep("", 3),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    style = "table_template"
  ) |>
  body_add_par("Source: ADSL")
out_rwe <- file.path(here, "annotated_shell_rwe_style.docx")
print(doc_rwe, target = out_rwe)

#' Wrap the marker text in a `<w:del>` tracked-change deletion so the parser
#' can prove deleted text never leaks into cell text.
#' @noRd
inject_del_run <- function(docx_path, marker) {
  td <- tempfile()
  dir.create(td)
  utils::unzip(docx_path, exdir = td)
  doc_xml_path <- file.path(td, "word", "document.xml")
  d <- xml2::read_xml(doc_xml_path)

  t_nodes <- xml2::xml_find_all(d, ".//*[local-name()='t']")
  for (t in t_nodes) {
    full <- xml2::xml_text(t)
    if (!grepl(marker, full, fixed = TRUE)) next
    kept <- sub(paste0("\\s*", marker), "", full)
    xml2::xml_text(t) <- kept
    xml2::xml_set_attr(t, "xml:space", "preserve")
    r <- xml2::xml_parent(t)
    del_run <- xml2::read_xml(sprintf(
      '<w:del xmlns:w="%s" w:id="901" w:author="reviewer"><w:r><w:t>%s</w:t></w:r></w:del>',
      .W_NS_URL, marker
    ))
    xml2::xml_add_sibling(r, del_run, .where = "after")
    break
  }

  xml2::write_xml(d, doc_xml_path)
  rezip_docx(td, docx_path)
}

#' Rewrite docProps/core.xml with an off-standard default namespace, the way
#' some e-signature tools do. officer's core-properties reader then hits an
#' "Undefined namespace prefix" XPath warning -- which arsbridge muffles.
#' @noRd
break_core_props <- function(docx_path) {
  td <- tempfile()
  dir.create(td)
  utils::unzip(docx_path, exdir = td)
  writeLines(
    c('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<coreProperties xmlns="urn:example:esign-demo"><title/></coreProperties>'),
    file.path(td, "docProps", "core.xml")
  )
  rezip_docx(td, docx_path)
}

inject_del_run(out_rwe, "TRACKDEL")
repaint_red(out_rwe)
break_core_props(out_rwe)
cat("Wrote:", out_rwe, "\n")

## Near-miss fixture: heading-shaped lines that must all be rejected, so
## parse_shell_docx() returns zero sections and explains each rejection.
doc_near_miss <- read_docx() |>
  body_add_par("14.1 Demographic and Baseline Tables", style = "heading 1") |>
  body_add_par("Table 14.1.1 shows the demographic summary") |>
  body_add_table(
    value = data.frame(
      Characteristic = c("Age (years)", "  n"),
      `Treatment A` = rep("", 2),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    style = "table_template"
  ) |>
  body_add_par("Source: ADSL")
out_near_miss <- file.path(here, "annotated_shell_near_miss.docx")
print(doc_near_miss, target = out_near_miss)
cat("Wrote:", out_near_miss, "\n")

## Custom-grammar fixture for the heading_patterns escape hatch: a sponsor
## that headlines outputs as "Output N.N.N: Title".
doc_custom <- read_docx() |>
  body_add_par("Output 14.1.1: Summary of Demographics", style = "heading 2") |>
  body_add_par("Safety Population (ADSL.SAFFL='Y')") |>
  body_add_table(
    value = data.frame(
      Characteristic = c("Age (years)  ADSL.AGE", "  n", "  Mean (SD)"),
      `Treatment A` = rep("", 3),
      `Placebo`     = rep("", 3),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    style = "table_template"
  ) |>
  body_add_par("Source: ADSL")
out_custom <- file.path(here, "annotated_shell_custom_heading.docx")
print(doc_custom, target = out_custom)
cat("Wrote:", out_custom, "\n")

## A heading whose number is found but that has NO title text and no page
## header to recover one from: the table follows the heading directly. Drives
## the "heading number found but no title" WARN.
doc_no_title <- read_docx() |>
  body_add_par("Table 14.9.1", style = "heading 2") |>
  body_add_table(
    value = data.frame(
      Characteristic = c("Age (years)  ADSL.AGE", "  n", "  Mean (SD)"),
      `Treatment A` = rep("", 3),
      `Placebo`     = rep("", 3),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    style = "table_template"
  ) |>
  body_add_par("Source: ADSL")
out_no_title <- file.path(here, "annotated_shell_no_title.docx")
print(doc_no_title, target = out_no_title)
cat("Wrote:", out_no_title, "\n")
