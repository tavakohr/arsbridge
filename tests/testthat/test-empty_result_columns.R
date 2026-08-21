## R-01: a column that shows nothing is not a result column.
##
## General defect class: a shell's physical column count can exceed the columns
## the shell actually displays -- a spreadsheet's used range routinely reaches
## past the table -- and counting those phantom columns makes a structurally
## correct output disagree with its own grouping. The author is told the shell
## displays more columns than the grouping defines, over columns that are blank
## on the page and invisible to them.
##
## General invariant: a display column participates in the result-column axis
## only where it can be shown to carry something -- a header label, or a body
## cell that would receive a result. A column with neither is not a result
## column. The converse is what keeps this honest: a BLANK header above real
## body cells IS a result column, one nothing can bind to a group level, and
## that finding must survive untouched.
##
## Positive evidence only. Where a shell offers no cell map at all -- a Word
## shell has no cell addresses -- the second half of the test cannot be
## evaluated, and nothing is dropped.

.erc_vocabs <- list(
  first  = list(ds = "ADQX", arm = "QXARM", pop = "QXFL", subj = "USUBJID",
                sheet = "Table 14.2.1", a = "Alfa", b = "Bravo"),
  ## The same structure in a vocabulary sharing no identifier with the first,
  ## so a rule that keyed on a familiar name rather than on the relationship
  ## would show up as a difference between the two halves of every test.
  second = list(ds = "ADZZ", arm = "ZZTRTP", pop = "ZZANLFL", subj = "USUBJID",
                sheet = "Table 9.9.9", a = "Kappa", b = "Lambda")
)

.erc_spec <- function(vocab, dir) {
  vars <- data.frame(
    Dataset  = c("ADSL", "ADSL", "ADSL", rep(vocab$ds, 3)),
    Variable = c("USUBJID", vocab$arm, vocab$pop,
                 vocab$subj, vocab$arm, vocab$pop),
    Label    = c("Subject", "Treatment", "Population Flag",
                 "Subject", "Treatment", "Population Flag"),
    Type = "Char", Origin = "Derived", Codelist = "", Length = "40",
    Mandatory = "Req", stringsAsFactors = FALSE
  )
  path <- file.path(dir, "spec.xlsx")
  wb <- openxlsx2::wb_workbook()$add_worksheet("Variables")
  wb$add_data(sheet = "Variables", x = vars)
  wb$save(path)
  path
}

.erc_adam <- function(vocab, dir) {
  adam <- file.path(dir, "adam")
  dir.create(adam, showWarnings = FALSE)
  subjects <- data.frame(
    USUBJID = sprintf("S%02d", 1:8),
    ARM = rep(c(vocab$a, vocab$b), each = 4),
    FLAG = "Y", stringsAsFactors = FALSE
  )
  names(subjects) <- c(vocab$subj, vocab$arm, vocab$pop)
  utils::write.csv(subjects, file.path(adam, paste0(vocab$ds, ".csv")),
                   row.names = FALSE)
  names(subjects)[[1]] <- "USUBJID"
  utils::write.csv(subjects, file.path(adam, "ADSL.csv"), row.names = FALSE)
  adam
}

## Three shapes of one table, differing only in what sits past -- or between --
## the columns the author meant to show.
##
##   "clean"    two arms and a Total, nothing else
##   "padded"   plus a column between the arms and two past the Total, every
##              one of them blank in the header AND in the body
##   "occupied" plus one column blank in the header but holding placeholders
##
## The column headers carry their own conditions, which is what builds a FIXED
## grouping. A data-driven axis returns early from `.check_flat_axes()` before
## any count is compared, so a fixture built the other way would assert nothing.
.erc_shell <- function(vocab, dir, kind) {
  wb <- openxlsx2::wb_workbook()$add_worksheet(vocab$sheet)
  put <- function(x, row, col = 1L) {
    wb$add_data(sheet = vocab$sheet, x = x, start_row = row, start_col = col,
                col_names = FALSE)
  }
  black <- openxlsx2::wb_color(hex = "FF000000")
  red   <- openxlsx2::wb_color(hex = "FFC00000")
  ann <- function(label, annotation) {
    openxlsx2::fmt_txt(label, color = black, size = 10) +
      openxlsx2::fmt_txt(paste0("\n", annotation), color = red, size = 8,
                         italic = TRUE)
  }
  arm_header <- function(level) {
    ann(sprintf("%s (N=XX)", level),
        sprintf("%s.%s='%s'", vocab$ds, vocab$arm, level))
  }

  put(vocab$sheet, 1)
  put("Empty-column regression", 2)
  put(ann("Analysed Population ",
          sprintf("(%s.%s='Y')", vocab$ds, vocab$pop)), 3)

  ## Where the three real result columns sit, and which physical columns are
  ## left blank between and after them.
  cols <- switch(
    kind,
    clean    = list(arms = c(2L, 3L), total = 4L, blank = integer(0)),
    padded   = list(arms = c(2L, 4L), total = 5L, blank = c(3L, 6L, 7L)),
    occupied = list(arms = c(2L, 3L), total = 4L, blank = integer(0))
  )

  put("Item", 4)
  put(arm_header(vocab$a), 4, cols$arms[[1]])
  put(arm_header(vocab$b), 4, cols$arms[[2]])
  put("Total (N=XX)", 4, cols$total)
  put(ann("Subjects in population, n", sprintf("[%s.%s]", vocab$ds, vocab$subj)),
      5)
  put(ann("Subjects with a record, n", sprintf("[%s.%s]", vocab$ds, vocab$subj)),
      6)
  for (r in 5:6) for (j in c(cols$arms, cols$total)) put("xx", r, j)

  ## Blank means blank: written empty in the header row and both body rows, so
  ## the column exists in the sheet's used range and shows nothing.
  for (j in cols$blank) for (r in 4:6) put("", r, j)

  ## The one column that is blank in the header and NOT empty: it holds the
  ## same placeholders the real columns do.
  if (identical(kind, "occupied")) {
    put("", 4, 5L)
    for (r in 5:6) put("xx", r, 5L)
  }

  path <- file.path(dir, paste0(kind, ".xlsx"))
  wb$save(path)
  path
}

.erc_run <- function(vocab, dir, kind) {
  shell <- .erc_shell(vocab, dir, kind)
  res <- withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(suppressWarnings(spec_to_ars(
      shell_path = shell,
      adam_spec_path = .erc_spec(vocab, dir),
      api_key = "", use_llm = FALSE, verbose = FALSE,
      output_path = file.path(dir, paste0(kind, ".json")),
      report_path = file.path(dir, paste0(kind, "-report.xlsx")),
      emit_code = FALSE)))
  )
  model <- ars_to_model(res$ars_path)
  findings <- res$ars_validation
  list(
    labels = .flat_display_labels(model$outputs$raw[[1]]),
    axis   = findings$ref[grepl("^FLAT_AXIS", findings$ref)],
    ## Non-vacuity: the count check is only reached through a FIXED grouping
    ## with at least one group. If this is ever 0 the fixture stopped
    ## exercising the code under test and every assertion below is empty.
    fixed_groups = sum(!is.na(model$groupings$dataDriven) &
                         !model$groupings$dataDriven &
                         model$groupings$n_groups > 0L)
  )
}


test_that("blank columns with nothing in them are not counted as results", {
  skip_if_not_installed("openxlsx2")
  checked <- 0L

  for (vname in names(.erc_vocabs)) {
    vocab <- .erc_vocabs[[vname]]
    td <- withr::local_tempdir()
    .erc_adam(vocab, td)

    clean  <- .erc_run(vocab, td, "clean")
    padded <- .erc_run(vocab, td, "padded")

    ## The fixture reaches the check at all.
    expect_gt(clean$fixed_groups, 0L)
    expect_gt(padded$fixed_groups, 0L)

    ## The control: a table with no blank columns names its three columns and
    ## raises nothing. Without this the test below could pass on a build that
    ## dropped every column.
    expect_equal(clean$labels, c(vocab$a, vocab$b, "Total"), info = vname)
    expect_length(clean$axis, 0L)

    ## The defect: three blank columns -- one BETWEEN the arms, two past the
    ## Total -- leave the axis exactly as the author drew it. Position does not
    ## enter into it, which is why one of them is in the middle.
    expect_equal(padded$labels, clean$labels, info = vname)
    expect_length(padded$axis, 0L)

    checked <- checked + 1L
  }

  ## Both vocabularies really ran. The two share no dataset, variable or level
  ## name, so an implementation keying on a familiar identifier could not
  ## satisfy both.
  expect_equal(checked, length(.erc_vocabs))
})


test_that("a blank header above real cells is still a result column", {
  skip_if_not_installed("openxlsx2")
  ## The over-suppression guard, and the reason the rule is written as positive
  ## evidence rather than as "drop blank labels". This column has no header the
  ## grouping can be matched against, but it holds placeholders, so it is a
  ## result column that nothing will fill -- a true finding, and it must
  ## survive a change whose whole purpose is removing false ones.
  checked <- 0L

  for (vname in names(.erc_vocabs)) {
    vocab <- .erc_vocabs[[vname]]
    td <- withr::local_tempdir()
    .erc_adam(vocab, td)

    occupied <- .erc_run(vocab, td, "occupied")
    expect_gt(occupied$fixed_groups, 0L)

    expect_equal(occupied$labels, c(vocab$a, vocab$b, "Total", ""),
                 info = vname)
    expect_true("FLAT_AXIS_COLUMN_COUNT_MISMATCH" %in% occupied$axis,
                info = vname)

    checked <- checked + 1L
  }
  expect_equal(checked, length(.erc_vocabs))
})


## The branches above cannot reach: an output with no cell map at all, and the
## compact ARS shape where a display index is not a sheet column. Both are
## exercised directly on the node, which is the only input the function takes.
.erc_node <- function(labels, cell_cols = NULL, layout = TRUE) {
  node <- list(
    displays = list(list(order = 1L, display = list(
      columns = lapply(labels, function(l) list(label = l))
    )))
  )
  meta <- list()
  if (layout) {
    meta$shell_layout <- list(list(order = 1L, label = "Item", kind = "scalar_row"))
  }
  if (!is.null(cell_cols)) {
    meta$shell_fill <- list(
      cells = lapply(cell_cols, function(j) list(col = j, kind = "pending"))
    )
  }
  if (length(meta) > 0) node[["_meta"]] <- meta
  node
}

test_that("a column is dropped only on evidence that it is empty", {
  ## Physical columns 1..4: a stub, two named columns, one blank. The cell map
  ## says cells live in 2 and 3 only, so column 4 shows nothing.
  expect_equal(
    .flat_display_labels(.erc_node(c("Item", "Alfa", "Bravo", ""),
                                   cell_cols = c(2L, 3L))),
    c("Alfa", "Bravo")
  )

  ## The same sheet, with the map recording a cell in column 4. Now there is
  ## evidence the column carries something, and it stays.
  expect_equal(
    .flat_display_labels(.erc_node(c("Item", "Alfa", "Bravo", ""),
                                   cell_cols = c(2L, 3L, 4L))),
    c("Alfa", "Bravo", "")
  )

  ## Off-by-one guard. The stub is display column 1 and physical column 1, and
  ## it is dropped before the map is read. If the physical index were taken
  ## after that drop, the blank at physical 4 would be tested against the cell
  ## at physical 3 and wrongly kept.
  expect_equal(
    .flat_display_labels(.erc_node(c("Item", "Alfa", "", "Bravo"),
                                   cell_cols = c(2L, 4L))),
    c("Alfa", "Bravo")
  )

  ## A Word shell: no cell map, so the emptiness of a column cannot be shown
  ## and nothing is dropped. Absence of evidence is not evidence of absence.
  expect_equal(
    .flat_display_labels(.erc_node(c("Item", "Alfa", "Bravo", ""))),
    c("Alfa", "Bravo", "")
  )

  ## The compact ARS shape carries result columns only: no stub to drop, and
  ## no cell map to read a physical position against.
  ##
  ## An earlier version of this case gave the node a cell map while withholding
  ## the layout, to assert that the map "must not be read". That combination
  ## cannot occur: a cell map is built only for a parsed workbook section, and
  ## such a section always has a physical stub. Asserting against an
  ## unreachable state pinned the wrong invariant -- it said the stub is
  ## identified by the layout, when what identifies it is having come from a
  ## shell at all. A gated section, which emits no layout, is exactly the case
  ## that distinguishes the two.
  expect_equal(
    .flat_display_labels(.erc_node(c("Alfa", "Bravo", ""), layout = FALSE)),
    c("Alfa", "Bravo", "")
  )

  ## And the case that motivates reading the map rather than the layout: an
  ## output with a cell map but NO layout still has a stub, because only a
  ## workbook section produces a cell map.
  expect_equal(
    .flat_display_labels(.erc_node(c("Item", "Alfa", "Bravo", ""),
                                   cell_cols = c(2L, 3L), layout = FALSE)),
    c("Alfa", "Bravo")
  )

  ## Blankness is judged after the "(N=XX)" decoration is stripped, the same
  ## point the fill judges it -- but a decorated column is a real column, so
  ## the label survives the test it is put to.
  expect_equal(
    .flat_display_labels(.erc_node(c("Item", "Alfa (N=XX)", ""),
                                   cell_cols = 2L)),
    "Alfa"
  )
})
