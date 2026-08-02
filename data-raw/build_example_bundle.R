## Build the bundled CDSC-ALZ-201 training study.
## ---------------------------------------------------------------------------
## Run from the package root:   Rscript data-raw/build_example_bundle.R
##
## This script is the single source of truth for the example study that
## `arsbridge_example()` serves. Everything it writes is derived from PUBLIC
## material: the datasets and their metadata come from {pharmaverseadam}
## (the CDISC pilot Xanomeline study, Apache-2.0, no real patient data), and
## the annotated Word shell is the CDSC-ALZ-201 synthetic mock that already
## lives in tests/testthat/fixtures/.
##
## Outputs (all overwritten in place):
##   inst/extdata/example_cdsc_alz_201/annotated_shell.docx   copied fixture
##   inst/extdata/example_cdsc_alz_201/annotated_shell.xlsx   generated here
##   inst/extdata/example_cdsc_alz_201/adam_spec.xlsx         generated here
##   inst/extdata/example_cdsc_alz_201/ADaM.zip               generated here
##   inst/extdata/example_cdsc_alz_201/README.md              generated here
##   inputs/adam_spec_CDSC-ALZ-201.xlsx                       same spec again,
##                                                            for hands-on use
##
## data-raw/ is .Rbuildignore'd: none of this ships; only its outputs do.

for (pkg in c("pharmaverseadam", "haven", "openxlsx2")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package '", pkg, "' is required to rebuild the example bundle. ",
         "It is a build-time tool, not a package dependency -- install it ",
         "and re-run.", call. = FALSE)
  }
}
library(openxlsx2)

## Self-contained null-default (base gains %||% only in R 4.4).
`%||%` <- function(a, b) if (is.null(a)) b else a

root <- if (file.exists("DESCRIPTION")) "." else stop(
  "Run this script from the package root.", call. = FALSE)
bundle_dir <- file.path(root, "inst", "extdata", "example_cdsc_alz_201")
dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)

## ---------------------------------------------------------------------------
## The curated variable list -- the study's whole surface, stated once.
##
## Seeded from the CDSC-ALZ-201 fixture spec (the five domains the annotated
## shell references), written out as literals so this script depends on
## nothing but {pharmaverseadam}. Every downstream artifact -- the spec, the
## datasets, the zip -- derives from this list.
## ---------------------------------------------------------------------------

CURATED <- list(
  ADSL = c("STUDYID", "USUBJID", "SUBJID", "SITEID", "COUNTRY", "REGION1",
           "AGE", "AGEU", "AGEGR1", "SEX", "RACE", "RACEGR1", "ETHNIC",
           "ARM", "ARMCD", "ACTARM", "ACTARMCD", "TRT01P", "TRT01A",
           "TRTSDT", "TRTEDT", "TRTDURD", "RANDDT", "SAFFL", "EOSSTT",
           "EOSDT", "DTHFL", "DTHDT", "LSTALVDT"),
  ADAE = c("STUDYID", "USUBJID", "AGE", "AGEGR1", "SEX", "RACE", "SAFFL",
           "TRT01P", "TRT01A", "AESEQ", "AETERM", "AEDECOD", "AEBODSYS",
           "AESOC", "AESOCCD", "AESEV", "ASEV", "ASEVN", "AESER", "AEREL",
           "AEACN", "AEOUT", "ASTDT", "ASTDY", "AENDT", "AENDY", "TRTEMFL",
           "AOCCIFL"),
  ADEX = c("STUDYID", "USUBJID", "SAFFL", "TRT01P", "TRT01A", "TRTSDT",
           "TRTEDT", "TRTDURD", "EXSEQ", "EXTRT", "EXDOSE", "EXDOSU",
           "EXDOSFRQ", "EXROUTE", "PARAM", "PARAMCD", "AVAL", "AVALC",
           "ASTDT", "ASTDY", "AENDT", "AENDY"),
  ADCM = c("STUDYID", "USUBJID", "SAFFL", "TRTP", "TRTA", "CMSEQ", "CMTRT",
           "CMDECOD", "CMCLAS", "CMINDC", "CMSTDTC", "CMENDTC", "ASTDT",
           "ASTDY", "AENDT", "ONTRTFL", "AOCCPFL"),
  ADVS = c("STUDYID", "USUBJID", "SUBJID", "SITEID", "SAFFL", "TRTP", "TRTA",
           "PARAM", "PARAMCD", "PARAMN", "AVAL", "AVISIT", "AVISITN", "ADT",
           "ADY", "ATPT", "ATPTN", "ABLFL", "BASE", "BASETYPE", "CHG", "PCHG",
           "ANRIND", "BNRIND", "DTYPE", "ONTRTFL", "ANL01FL")
)

## The row-category codelists. Only variables whose decoded labels appear as
## sub-rows in a shell get one: without the decode, the fill writer can never
## pair a shell's "Female" with the data's "F". NEVER the treatment variables
## (TRT01A/TRTA/TRT01P) -- a codelist on the grouping variable would flip the
## column axis from data-driven to enumerated groups and change how the
## engine executes every table.
CODELISTS <- list(
  SEX = list(terms = c(F = "Female", M = "Male"),
             used_by = "ADSL.SEX, ADAE.SEX"),
  AESEV = list(terms = c(MILD = "Mild", MODERATE = "Moderate",
                         SEVERE = "Severe"),
               used_by = "ADAE.AESEV"),
  ASEV = list(terms = c(MILD = "Mild", MODERATE = "Moderate",
                        SEVERE = "Severe"),
              used_by = "ADAE.ASEV"),
  AESER = list(terms = c(Y = "Yes", N = "No"),
               used_by = "ADAE.AESER")
)

## ---------------------------------------------------------------------------
## Load the data, once, and hold this script to its own claims.
## ---------------------------------------------------------------------------

data_env <- new.env()
datasets <- list()
for (ds in names(CURATED)) {
  utils::data(list = tolower(ds), package = "pharmaverseadam",
              envir = data_env)
  d <- as.data.frame(get(tolower(ds), envir = data_env))
  missing <- setdiff(CURATED[[ds]], names(d))
  if (length(missing) > 0) {
    stop(ds, ": curated variables not in pharmaverseadam: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  datasets[[ds]] <- d[, CURATED[[ds]], drop = FALSE]
}

## ADVS carries every vital sign at every visit -- far more than the shells
## use. The bundled figure (and nothing else) reads ADVS, and it reads only
## pulse rate, so the bundle keeps only that parameter. Stated here because
## anyone extending the shells to another parameter must widen this filter.
datasets$ADVS <- datasets$ADVS[datasets$ADVS$PARAMCD == "PULSE", ,
                               drop = FALSE]

## The codelist terms must exist in the data they claim to decode -- a decode
## for a value the data never takes silently empties a category at fill time.
observed <- list(SEX = datasets$ADSL$SEX, AESEV = datasets$ADAE$AESEV,
                 ASEV = datasets$ADAE$ASEV, AESER = datasets$ADAE$AESER)
for (cl in names(CODELISTS)) {
  terms <- names(CODELISTS[[cl]]$terms)
  stray <- setdiff(terms, unique(observed[[cl]]))
  if (length(stray) > 0) {
    stop("Codelist ", cl, " declares terms absent from the data: ",
         paste(stray, collapse = ", "), call. = FALSE)
  }
}

## ---------------------------------------------------------------------------
## ADaM.zip -- five XPT v5 files, lowercase names at the zip root.
## ---------------------------------------------------------------------------

stage <- file.path(tempdir(), "example_adam_stage")
unlink(stage, recursive = TRUE)
dir.create(stage)
for (ds in names(datasets)) {
  d <- datasets[[ds]]
  ## XPT v5 has no Date type or list columns; keep it plain.
  for (j in seq_along(d)) {
    if (inherits(d[[j]], "Date")) {
      lab <- attr(d[[j]], "label", exact = TRUE)
      d[[j]] <- as.character(d[[j]])
      if (!is.null(lab)) attr(d[[j]], "label") <- lab
    }
  }
  haven::write_xpt(d, file.path(stage, paste0(tolower(ds), ".xpt")),
                   version = 5)
}

zip_path <- file.path(bundle_dir, "ADaM.zip")
unlink(zip_path)
old_wd <- setwd(stage)
utils::zip(zipfile = file.path("staged.zip"),
           files = sort(list.files(".")), flags = "-q9X")
setwd(old_wd)
invisible(file.copy(file.path(stage, "staged.zip"), zip_path,
                    overwrite = TRUE))

zip_kb <- round(file.size(zip_path) / 1024)
if (file.size(zip_path) <= 100000) {
  stop("ADaM.zip is ", zip_kb, " KB -- under the 100 KB floor the example ",
       "tests assert. Something got dropped.", call. = FALSE)
}
if (file.size(zip_path) > 2e6) {
  stop("ADaM.zip is ", zip_kb, " KB -- over the ~2 MB budget. Tighten the ",
       "curated list or the ADVS filter.", call. = FALSE)
}

## ---------------------------------------------------------------------------
## The ADaM specification -- one workbook, written twice (bundle + inputs/).
## ---------------------------------------------------------------------------

xpt_type <- function(col) {
  if (is.numeric(col)) "integer" else "text"
}
xpt_length <- function(col) {
  if (is.numeric(col)) 8L else max(1L, nchar(col), na.rm = TRUE)
}
codelist_of <- function(ds, var) {
  for (cl in names(CODELISTS)) {
    pairs <- strsplit(CODELISTS[[cl]]$used_by, ",\\s*")[[1]]
    if (paste(ds, var, sep = ".") %in% pairs) return(cl)
  }
  ""
}

spec_wb <- wb_workbook()

spec_wb$add_worksheet("SUMMARY")
summary_rows <- data.frame(
  Field = c("Specification", "Study", "Protocol", "Title", "Data source",
            "Licence", "Generated by", "Datasets"),
  Value = c("ADaM Specification -- CDSC-ALZ-201",
            "CDSC-ALZ-201 (synthetic)",
            "Phase 2, Randomized, Double-Blind, Placebo-Controlled",
            "Xanomeline Transdermal System in Mild-to-Moderate Alzheimer's Disease",
            paste0("{pharmaverseadam} ",
                   as.character(utils::packageVersion("pharmaverseadam")),
                   " (CDISC pilot; no real patient data)"),
            "Apache-2.0",
            "data-raw/build_example_bundle.R",
            paste(names(CURATED), collapse = ", ")),
  stringsAsFactors = FALSE
)
spec_wb$add_data(sheet = "SUMMARY", x = summary_rows, col_names = FALSE)

for (ds in names(CURATED)) {
  d <- datasets[[ds]]
  rows <- data.frame(
    Dataset         = ds,
    `Variable Name` = names(d),
    Label           = vapply(d, function(col) {
      attr(col, "label", exact = TRUE) %||% ""
    }, character(1)),
    `Data Type`     = vapply(d, xpt_type, character(1)),
    Length          = vapply(d, xpt_length, integer(1)),
    Codelist        = vapply(names(d), function(v) codelist_of(ds, v),
                             character(1)),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  spec_wb$add_worksheet(ds)
  spec_wb$add_data(sheet = ds, x = rows)
}

codelist_rows <- do.call(rbind, lapply(names(CODELISTS), function(cl) {
  data.frame(
    `Codelist Name`      = cl,
    `Term (Code)`        = names(CODELISTS[[cl]]$terms),
    `Decoded Value`      = unname(CODELISTS[[cl]]$terms),
    `Used By Variables`  = CODELISTS[[cl]]$used_by,
    check.names = FALSE, stringsAsFactors = FALSE
  )
}))
spec_wb$add_worksheet("CODELISTS")
spec_wb$add_data(sheet = "CODELISTS", x = codelist_rows)

spec_bundle <- file.path(bundle_dir, "adam_spec.xlsx")
spec_inputs <- file.path(root, "inputs", "adam_spec_CDSC-ALZ-201.xlsx")
spec_wb$save(spec_bundle)
spec_wb$save(spec_inputs)

## ---------------------------------------------------------------------------
## annotated_shell.docx -- the public CDSC-ALZ-201 mock, copied from the
## tracked fixture. The one deliberate fixture dependency in this script.
## ---------------------------------------------------------------------------

docx_src <- file.path(root, "tests", "testthat", "fixtures",
                      "CDSC-ALZ-201_TLF_Shells_v1.0_annotated.docx")
if (!file.exists(docx_src)) stop("Fixture shell not found: ", docx_src)
invisible(file.copy(docx_src, file.path(bundle_dir, "annotated_shell.docx"),
                    overwrite = TRUE))

## ---------------------------------------------------------------------------
## annotated_shell.xlsx -- the Excel counterpart, generated.
##
## Four worksheets covering one of each output kind, in the same conventions
## the docx uses: number/title/population banner, red italic in-cell
## annotations, xx placeholders. The arm headers are the EXACT TRT01A values
## in the data ("Xanomeline Low Dose", not "Xanomeline Low") -- the fill
## writer joins ARD group levels to columns by that label, so a paraphrase
## would leave every cell unfilled. One header carries "(N=XX)" on purpose:
## the writer strips the decoration before joining, and the bundle should
## demonstrate that.
## ---------------------------------------------------------------------------

BLACK <- "FF000000"
RED   <- "FFC00000"
arms  <- c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")

annotated_cell <- function(label, annotation) {
  fmt_txt(label, color = wb_color(hex = BLACK), size = 10) +
    fmt_txt(paste0("\n", annotation), color = wb_color(hex = RED),
            size = 8, italic = TRUE)
}

wb <- wb_workbook()

banner <- function(sheet, number, title, n_cols = 4L) {
  wb$add_worksheet(sheet)
  wb$add_data(sheet = sheet, x = number, start_row = 1, start_col = 1,
              col_names = FALSE)
  wb$add_data(sheet = sheet, x = title, start_row = 2, start_col = 1,
              col_names = FALSE)
  wb$merge_cells(sheet = sheet, dims = paste0("A1:", int2col(n_cols), "1"))
  wb$merge_cells(sheet = sheet, dims = paste0("A2:", int2col(n_cols), "2"))
  wb$add_data(
    sheet = sheet,
    x = fmt_txt("Safety Population ", color = wb_color(hex = BLACK),
                size = 10) +
      fmt_txt("(ADSL.SAFFL='Y')", color = wb_color(hex = RED), size = 8,
              italic = TRUE),
    start_row = 3, start_col = 1, col_names = FALSE)
  wb$merge_cells(sheet = sheet, dims = paste0("A3:", int2col(n_cols), "3"))
}

header_row <- function(sheet, stub, directives, cols = arms) {
  wb$add_data(sheet = sheet, x = annotated_cell(stub, directives),
              start_row = 4, start_col = 1, col_names = FALSE)
  for (j in seq_along(cols)) {
    wb$add_data(sheet = sheet, x = cols[[j]], start_row = 4,
                start_col = j + 1L, col_names = FALSE)
  }
}

data_row <- function(sheet, row, label, annot = NULL, value = "xx (xx.x)",
                     n_vals = 3L) {
  x <- if (is.null(annot)) label else annotated_cell(label, annot)
  wb$add_data(sheet = sheet, x = x, start_row = row, start_col = 1,
              col_names = FALSE)
  if (!is.null(value)) {
    for (j in seq_len(n_vals)) {
      wb$add_data(sheet = sheet, x = value, start_row = row,
                  start_col = j + 1L, col_names = FALSE)
    }
  }
}

footnote <- function(sheet, row, text, n_cols = 4L) {
  wb$add_data(sheet = sheet, x = text, start_row = row, start_col = 1,
              col_names = FALSE)
  wb$merge_cells(sheet = sheet,
                 dims = sprintf("A%d:%s%d", row, int2col(n_cols), row))
}

## --- Table 14.1.1: disposition. One arm header keeps its (N=XX). ----------
banner("Table 14.1.1", "Table 14.1.1", "Summary of Subject Disposition")
header_row("Table 14.1.1", "Category",
           "[columns -> ADSL.TRT01A; source ADSL]",
           cols = c("Placebo (N=XX)", "Xanomeline Low Dose",
                    "Xanomeline High Dose"))
data_row("Table 14.1.1", 5, "Subjects in safety population",
         "[ADSL.SAFFL = 'Y']", value = "xx")
data_row("Table 14.1.1", 6, "Completed study",
         "[ADSL.EOSSTT = 'COMPLETED']")
data_row("Table 14.1.1", 7, "Discontinued study",
         "[ADSL.EOSSTT = 'DISCONTINUED']")
footnote("Table 14.1.1", 9,
         "Percentages are based on the number of subjects in the safety population.")

## --- Table 14.1.2: demographics, stat lines + decoded sub-rows. -----------
banner("Table 14.1.2", "Table 14.1.2",
       "Demographics and Baseline Characteristics")
header_row("Table 14.1.2", "Characteristic",
           "[columns -> ADSL.TRT01A; source ADSL]")
data_row("Table 14.1.2", 5, "Age (years)", "[ADSL.AGE]", value = NULL)
data_row("Table 14.1.2", 6, "Mean (SD)", value = "xx.x (xx.xx)")
data_row("Table 14.1.2", 7, "Median", value = "xx.x")
data_row("Table 14.1.2", 8, "Min, Max", value = "xx, xx")
## row 9 blank -- spacer
data_row("Table 14.1.2", 10, "Sex, n (%)", "[ADSL.SEX]", value = NULL)
data_row("Table 14.1.2", 11, "Female")
data_row("Table 14.1.2", 12, "Male")
footnote("Table 14.1.2", 14, "Age is at informed consent.")

## --- Listing 16.2.7.1: adverse events. -------------------------------------
## The full banner (number / title / population) matters: the layout reader
## finds the header band relative to it, and a missing population row slides
## everything down one -- the template row then reads as the header.
banner("Listing 16.2.7.1", "Listing 16.2.7.1", "Listing of Adverse Events",
       n_cols = 5L)
listing_cols <- c(
  "Subject"   = "[ADAE.USUBJID]  [row: ADAE.TRTEMFL='Y'; source ADAE]",
  "Treatment" = "[ADAE.TRT01A]",
  "AE term"   = "[ADAE.AEDECOD]",
  "Severity"  = "[ADAE.ASEV]",
  "Serious"   = "[ADAE.AESER]")
for (j in seq_along(listing_cols)) {
  wb$add_data(sheet = "Listing 16.2.7.1",
              x = annotated_cell(names(listing_cols)[[j]],
                                 listing_cols[[j]]),
              start_row = 4, start_col = j, col_names = FALSE)
}
for (j in seq_along(listing_cols)) {
  wb$add_data(sheet = "Listing 16.2.7.1",
              x = c("xxx-xxxx", "xxxx", "xxxx", "xxxx", "x")[[j]],
              start_row = 5, start_col = j, col_names = FALSE)
}

## --- Figure 14.3.1: pulse over time, red arrow-prose directives. -----------
banner("Figure 14.3.1", "Figure 14.3.1",
       "Mean (+/- SE) Pulse Rate Over Time by Treatment", n_cols = 2L)
wb$add_data(sheet = "Figure 14.3.1", x = "Chart type", start_row = 5,
            start_col = 1, col_names = FALSE)
wb$add_data(sheet = "Figure 14.3.1", x = "Line plot with error bars",
            start_row = 5, start_col = 2, col_names = FALSE)
fig_lines <- c("Programming annotations",
               "Source: ADVS.",
               "X axis -> ADVS.AVISITN (label ADVS.AVISIT)",
               "Y axis -> mean of ADVS.AVAL",
               "Series (colour) -> ADVS.TRTA",
               "Filter -> ADVS.PARAMCD='PULSE'",
               "Error bars -> SE = sd(ADVS.AVAL) / sqrt(n)")
for (i in seq_along(fig_lines)) {
  row <- 6L + i
  wb$add_data(sheet = "Figure 14.3.1", x = fig_lines[[i]], start_row = row,
              start_col = 1, col_names = FALSE)
  wb$merge_cells(sheet = "Figure 14.3.1", dims = sprintf("A%d:B%d", row, row))
}
wb$add_font(sheet = "Figure 14.3.1", dims = "A7:A13",
            color = wb_color(hex = RED), size = 9, italic = TRUE)

xlsx_path <- file.path(bundle_dir, "annotated_shell.xlsx")
wb$save(xlsx_path)

## ---------------------------------------------------------------------------
## The bundle's README.
## ---------------------------------------------------------------------------

size_kb <- function(path) sprintf("%d KB", round(file.size(path) / 1024))
readme <- c(
  "# CDSC-ALZ-201 -- the bundled training study",
  "",
  "A synthetic Alzheimer's disease study built entirely from public",
  "material: the datasets are `{pharmaverseadam}` (the CDISC pilot",
  "Xanomeline study, Apache-2.0 -- **no real patient data**), and the",
  "shells are the CDSC-ALZ-201 synthetic mock. Everything here is",
  "regenerated by `data-raw/build_example_bundle.R`.",
  "",
  "| File | What it is | Size |",
  "|---|---|---|",
  paste0("| `annotated_shell.docx` | annotated TLF shell, Word: 5 tables, ",
         "2 listings, 1 figure | ",
         size_kb(file.path(bundle_dir, "annotated_shell.docx")), " |"),
  paste0("| `annotated_shell.xlsx` | annotated TLF shell, Excel: one ",
         "worksheet per output, fillable by `ars_fill_shell()` | ",
         size_kb(xlsx_path), " |"),
  paste0("| `adam_spec.xlsx` | ADaM specification: 5 domains, ",
         "pharmaverseadam labels/types, codelists | ",
         size_kb(spec_bundle), " |"),
  paste0("| `ADaM.zip` | 5 XPT datasets (adsl, adae, adex, adcm, advs; ",
         "ADVS restricted to PARAMCD='PULSE') | ", size_kb(zip_path), " |"),
  "",
  "Access from R with `arsbridge_example()`; run the whole study with",
  "`spec_to_ars_example()`. The Excel shell demos the full chain offline:",
  "build the ARS, execute it against the unzipped data, and write the",
  "shell back filled."
)
writeLines(readme, file.path(bundle_dir, "README.md"))

## ---------------------------------------------------------------------------
cat("Bundle written to", bundle_dir, "\n")
for (f in sort(list.files(bundle_dir))) {
  cat(sprintf("  %-24s %8s\n", f,
              size_kb(file.path(bundle_dir, f))))
}
cat("Spec also written to", spec_inputs,
    sprintf("(%s)\n", size_kb(spec_inputs)))
