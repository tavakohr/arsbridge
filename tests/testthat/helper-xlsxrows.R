## helper-xlsxrows.R -----------------------------------------------------------
## The one check that catches a filled workbook Excel will refuse to show.
##
## openxlsx2 groups cells into <row> elements by an internal key, and its own
## reader places cells by their `r` attribute instead. So a cell serialized
## inside the wrong <row> reads back through openxlsx2 as perfectly fine, and
## disappears when Excel opens the file. No R-side assertion about cell values
## can see that -- only the XML can.
##
## Every test that produces a filled workbook runs this against it.

#' Cells whose reference disagrees with the `<row>` element holding them, and
#' rows that hold cells while declaring themselves empty.
#'
#' @param path A written .xlsx.
#' @return A data frame, one row per sheet: part, cells, misplaced.
xlsx_row_grouping <- function(path) {
  dir <- tempfile()
  dir.create(dir)
  utils::unzip(path, exdir = dir)

  parts <- list.files(file.path(dir, "xl", "worksheets"), pattern = "\\.xml$",
                      full.names = TRUE)
  out <- lapply(parts, function(part) {
    doc <- xml2::read_xml(part)
    rows <- xml2::xml_find_all(doc, "//*[local-name()='row']")
    cells <- 0L
    misplaced <- 0L
    for (row in rows) {
      declared <- xml2::xml_attr(row, "r")
      refs <- xml2::xml_attr(
        xml2::xml_find_all(row, "./*[local-name()='c']"), "r")
      cells <- cells + length(refs)
      misplaced <- misplaced + sum(gsub("[^0-9]", "", refs) != declared)
    }
    data.frame(part = basename(part), cells = cells, misplaced = misplaced,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

#' Assert that every cell in a written workbook is where Excel will look for it.
expect_rows_well_formed <- function(path) {
  grouping <- xlsx_row_grouping(path)
  bad <- grouping[grouping$misplaced > 0, , drop = FALSE]
  testthat::expect_equal(
    nrow(bad), 0L,
    info = paste0("cells outside their own <row>: ",
                  paste(sprintf("%s (%d of %d)", bad$part, bad$misplaced,
                                bad$cells), collapse = ", ")))
  invisible(grouping)
}

#' The text of one cell as the raw XML holds it -- Excel's view, not
#' openxlsx2's, so a cell in the wrong row element does NOT show up here.
xlsx_raw_cell_text <- function(path, part, ref) {
  dir <- tempfile()
  dir.create(dir)
  utils::unzip(path, exdir = dir)
  doc <- xml2::read_xml(file.path(dir, "xl", "worksheets", part))
  row <- gsub("[^0-9]", "", ref)
  hit <- xml2::xml_find_all(doc, sprintf(
    "//*[local-name()='row'][@r='%s']/*[local-name()='c'][@r='%s']", row, ref))
  if (length(hit) == 0) return(NA_character_)
  paste(xml2::xml_text(
    xml2::xml_find_all(hit[[1]], ".//*[local-name()='t']")), collapse = "")
}
