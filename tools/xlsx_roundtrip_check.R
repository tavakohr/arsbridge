## tools/xlsx_roundtrip_check.R
## ---------------------------------------------------------------------------
## Does a shell workbook survive being opened, edited and written back?
##
## The fill writer (R/ars_fill_shell.R) does not build a workbook -- it takes
## the author's own file and changes a handful of cells in it. That only works
## if the library it goes through returns everything it was not asked to touch,
## exactly as it found it. This measures that, against real files.
##
## Two questions, and they are different:
##
##   1. IDENTITY -- openxlsx2::wb_load() -> wb_save() with no edit at all.
##      Sheets, cells, text, runs, colours, merges, column widths, row
##      heights, style indices and zip parts must all come back unchanged.
##      A failure here means the writer cannot use openxlsx2 for anything.
##
##   2. SURGERY -- edit one cell's inline-string XML the way the writer does,
##      then save. The retained runs must come back BYTE-IDENTICAL, including
##      the properties arsbridge's own run model does not carry (rFont, sz,
##      vertAlign). This is the check that decided the writer's mechanism:
##      see "Mechanism" in adr/0005-filled-shell-output.md.
##
## Usage (from the package root):
##
##   Rscript tools/xlsx_roundtrip_check.R                  # committed fixtures
##   Rscript tools/xlsx_roundtrip_check.R inputs/Study.xlsx
##
## Exit status is 0 when every workbook survives both passes and 1 otherwise,
## so it can gate a commit or an openxlsx2 upgrade. Re-run it whenever
## openxlsx2 changes: the writer depends on an internal representation
## (`wb$worksheets[[i]]$sheet_data$cc`), and this is what would notice it
## moving.
##
## Only structural counts and cell references are printed, never cell text, so
## the output is safe to paste into an issue even when the workbook is not.

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

if (!requireNamespace("openxlsx2", quietly = TRUE)) {
  stop("openxlsx2 is required for this check.")
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

failures <- 0L

note <- function(...) cat(" ", sprintf(...), "\n", sep = "")

check <- function(ok, what) {
  if (isTRUE(ok)) {
    note("ok   %s", what)
  } else {
    note("FAIL %s", what)
    failures <<- failures + 1L
  }
  isTRUE(ok)
}

## ---------------------------------------------------------------------------
## Reading the parts of a workbook that openxlsx2 is not asked about
## ---------------------------------------------------------------------------

unzip_workbook <- function(path) {
  dir <- tempfile()
  dir.create(dir)
  utils::unzip(path, exdir = dir)
  dir
}

## The sheet-level structure the reader in R/xlsx_cells.R deliberately ignores
## -- column widths, row heights, and each cell's style index. A filled shell
## that lost these would still parse and still be wrong.
sheet_structure <- function(dir, part) {
  file <- file.path(dir, part)
  if (!file.exists(file)) return(NULL)
  doc <- xml2::read_xml(file)
  attrs <- function(tag, attr) {
    nodes <- xml2::xml_find_all(doc, paste0("//*[local-name()='", tag, "']"))
    xml2::xml_attr(nodes, attr)
  }
  cells <- xml2::xml_find_all(doc, "//*[local-name()='c']")
  list(
    col_widths  = attrs("col", "width"),
    row_heights = attrs("row", "ht"),
    styles      = stats::setNames(xml2::xml_attr(cells, "s"),
                                  xml2::xml_attr(cells, "r"))
  )
}

## The seam-1 view: what the parser would see. Compared cell by cell so a lost
## formatting run shows up as the run count changing, not as equal text.
cell_view <- function(sheet) {
  cells <- sheet$cells
  runs <- lapply(cells$runs, function(rs) {
    lapply(rs, function(r) {
      r[c("text", "color_hex", "bold", "italic", "underline", "strike")]
    })
  })
  list(
    text   = stats::setNames(cells$text, cells$ref),
    colour = stats::setNames(cells$cell_color, cells$ref),
    runs   = stats::setNames(runs, cells$ref),
    merges = sort(sheet$merges$ref)
  )
}

## ---------------------------------------------------------------------------
## Pass 1 -- identity
## ---------------------------------------------------------------------------

identity_pass <- function(path) {
  before <- xlsx_read_shell_cells(path)
  out <- tempfile(fileext = ".xlsx")
  openxlsx2::wb_save(openxlsx2::wb_load(path), out, overwrite = TRUE)
  after <- xlsx_read_shell_cells(out)

  check(identical(names(before$sheets), names(after$sheets)),
        "sheet names and tab order")

  dir_b <- unzip_workbook(path)
  dir_a <- unzip_workbook(out)
  parts_b <- sort(list.files(dir_b, recursive = TRUE))
  parts_a <- sort(list.files(dir_a, recursive = TRUE))
  lost <- setdiff(parts_b, parts_a)
  check(length(lost) == 0,
        sprintf("all %d zip parts survive%s", length(parts_b),
                if (length(lost)) paste0(" (lost: ", paste(lost, collapse = ", "), ")")
                else ""))

  for (name in names(before$sheets)) {
    sb <- before$sheets[[name]]
    sa <- after$sheets[[name]]
    tag <- sprintf("[%s]", name)
    if (is.null(sa)) {
      check(FALSE, sprintf("%s sheet present after save", tag))
      next
    }
    vb <- cell_view(sb)
    va <- cell_view(sa)
    check(identical(vb$text, va$text),   sprintf("%s cell text (%d cells)", tag, nrow(sb$cells)))
    check(identical(vb$runs, va$runs),   sprintf("%s formatting runs", tag))
    check(identical(vb$colour, va$colour), sprintf("%s cell colours", tag))
    check(identical(vb$merges, va$merges), sprintf("%s merged ranges (%d)", tag, nrow(sb$merges)))

    stb <- sheet_structure(dir_b, sb$part)
    sta <- sheet_structure(dir_a, sa$part)
    check(identical(stb$col_widths, sta$col_widths), sprintf("%s column widths", tag))
    check(identical(stb$row_heights, sta$row_heights), sprintf("%s row heights", tag))
    check(identical(stb$styles, sta$styles), sprintf("%s per-cell style indices", tag))
  }
}

## ---------------------------------------------------------------------------
## Pass 2 -- surgery
## ---------------------------------------------------------------------------

## Pick a cell worth operating on: an inline string with more than one run, so
## the check exercises removing one run and keeping its siblings intact.
find_rich_cell <- function(sheet) {
  n_runs <- vapply(sheet$cells$runs, length, integer(1))
  candidates <- which(n_runs > 1)
  if (length(candidates) == 0) return(NULL)
  sheet$cells$ref[[candidates[[1]]]]
}

surgery_pass <- function(path) {
  before <- xlsx_read_shell_cells(path)
  target <- NULL
  for (i in seq_along(before$sheets)) {
    ref <- find_rich_cell(before$sheets[[i]])
    if (!is.null(ref)) {
      target <- list(index = i, name = names(before$sheets)[[i]], ref = ref)
      break
    }
  }
  if (is.null(target)) {
    note("skip surgery: no multi-run cell in this workbook")
    return(invisible())
  }

  wb <- openxlsx2::wb_load(path)
  cc <- wb$worksheets[[target$index]]$sheet_data$cc
  if (!"is" %in% names(cc)) {
    check(FALSE, "openxlsx2 exposes inline-string XML in sheet_data$cc$is")
    return(invisible())
  }
  slot <- which(cc$r == target$ref)
  check(length(slot) == 1,
        sprintf("cell %s!%s found in the internal store", target$name, target$ref))
  if (length(slot) != 1) return(invisible())

  ## A cell's runs live in one of two places, and the writer has to reach
  ## both. Inline: the XML is right there in cc$is. Shared: cc$is is empty and
  ## cc$v indexes wb$sharedStrings, where ONE <si> can back many cells --
  ## which is why editing it in place is never right. Converting the edited
  ## cell to an inline string is the whole fix: it detaches this cell from the
  ## shared entry, leaving every other user of that string alone.
  shared <- identical(cc$c_t[[slot]], "s")
  if (shared) {
    index <- suppressWarnings(as.integer(cc$v[[slot]])) + 1L
    check(!is.na(index) && index >= 1 && index <= length(wb$sharedStrings),
          sprintf("shared-string index %s resolves", cc$v[[slot]]))
    if (is.na(index)) return(invisible())
    original <- sub("^<si", "<is", sub("</si>$", "</is>", wb$sharedStrings[[index]]))
    users <- 0L
    for (ws in wb$worksheets) {
      wcc <- ws$sheet_data$cc
      if (is.null(wcc) || !nrow(wcc)) next
      users <- users + sum(wcc$c_t == "s" & wcc$v == cc$v[[slot]])
    }
    note("cell %s!%s is a SHARED string used by %d cell(s); converting to inline",
         target$name, target$ref, users)
  } else {
    original <- cc$is[[slot]]
  }
  doc <- xml2::read_xml(original)
  runs <- xml2::xml_find_all(doc, "./*[local-name()='r']")
  check(length(runs) > 1,
        sprintf("cell %s!%s carries %d runs", target$name, target$ref, length(runs)))

  ## What the writer does: keep run 1 untouched, drop the rest.
  kept <- as.character(runs[[1]], options = "no_declaration")
  for (i in seq_along(runs)[-1]) xml2::xml_remove(runs[[i]])
  cc$is[[slot]] <- as.character(doc, options = "no_declaration")
  if (shared) {
    cc$c_t[[slot]] <- "inlineStr"
    cc$v[[slot]] <- ""
  }
  wb$worksheets[[target$index]]$sheet_data$cc <- cc

  out <- tempfile(fileext = ".xlsx")
  openxlsx2::wb_save(wb, out, overwrite = TRUE)

  check(!inherits(try(openxlsx2::wb_load(out), silent = TRUE), "try-error"),
        "the edited workbook still loads")

  after <- xlsx_read_shell_cells(out)
  sa <- after$sheets[[target$name]]
  row <- sa$cells[sa$cells$ref == target$ref, , drop = FALSE]
  check(nrow(row) == 1 && length(row$runs[[1]]) == 1,
        sprintf("cell %s!%s now has exactly one run", target$name, target$ref))

  ## The point of the whole exercise: the run that stayed must be unchanged
  ## down to the properties arsbridge does not model.
  dir_a <- unzip_workbook(out)
  doc_a <- xml2::read_xml(file.path(dir_a, sa$part))
  node <- xml2::xml_find_first(
    doc_a, sprintf("//*[local-name()='c'][@r='%s']/*[local-name()='is']/*[local-name()='r']",
                   target$ref))
  survived <- if (inherits(node, "xml_missing")) "" else
    as.character(node, options = "no_declaration")
  check(identical(survived, kept),
        sprintf("retained run of %s!%s is byte-identical", target$name, target$ref))
  if (!identical(survived, kept)) {
    note("   kept    : %s", substr(kept, 1, 160))
    note("   survived: %s", substr(survived, 1, 160))
  }

  ## Everything else on that sheet must be untouched.
  vb <- cell_view(before$sheets[[target$name]])
  va <- cell_view(sa)
  others <- setdiff(names(vb$runs), target$ref)
  check(identical(vb$runs[others], va$runs[others]),
        sprintf("[%s] every other cell untouched by the edit", target$name))
  check(identical(vb$merges, va$merges),
        sprintf("[%s] merged ranges survive the edit", target$name))
}

## ---------------------------------------------------------------------------
## Run
## ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(
    "tests/testthat/fixtures/shells_apx_drm_301.xlsx",
    "tests/testthat/fixtures/shells_parity_apx.xlsx",
    "tests/testthat/fixtures/shell_cells_shared_apx.xlsx",
    ## Present only on a machine that has the real shells; never committed.
    "inputs/CDSCALZ201_TLF_Shells_combined.xlsx"
  )
  args <- args[file.exists(args)]
}

cat("openxlsx2 ", as.character(utils::packageVersion("openxlsx2")), "\n", sep = "")

for (path in args) {
  if (!file.exists(path)) {
    cat("\n--", path, "-- NOT FOUND\n")
    failures <- failures + 1L
    next
  }
  cat("\n-- ", basename(path), " ------------------------------------------\n",
      sep = "")
  cat("identity\n")
  identity_pass(path)
  cat("surgery\n")
  surgery_pass(path)
}

cat("\n")
if (failures == 0L) {
  cat("PASS -- every workbook survives load, edit and save.\n")
  quit(status = 0)
}
cat("FAIL --", failures, "check(s) failed.\n")
cat("The writer's mechanism assumes all of these hold; see the Mechanism\n")
cat("section of adr/0005-filled-shell-output.md before working around one.\n")
quit(status = 1)
