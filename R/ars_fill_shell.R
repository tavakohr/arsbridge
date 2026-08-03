## arsbridge -- ars_fill_shell.R
## ---------------------------------------------------------------------------
## The fill writer: the author's own shell workbook, with the numbers in it.
##
## Everything a finished table needs is already in the shell except the
## results -- the layout, the row labels, the column headers, the merges, the
## footnotes, and in each placeholder the number of decimal places that cell
## takes. Rendering a new table from the ARS reproduces all of that from
## scratch and can only approximate it. Filling the shell reproduces none of
## it, because none of it was ever lost.
##
## So this file does the narrowest thing that produces a deliverable: it opens
## the shell, changes the cells the cell map points at, and saves. Nothing is
## laid out, styled or created.
##
## WHERE THE ANSWERS COME FROM. Nothing here decides which result belongs in
## which cell -- that was decided at build time and recorded in
## `outputs[[i]]$_meta$shell_fill` (shell_fill_meta.R, ADR 0005). This file
## consumes that map. The division matters: the map is written while the
## shell's geometry and the analyses are both in hand, and re-deriving it here
## would mean matching row labels against analysis names, which is the
## guesswork the shell annotations exist to eliminate.
##
## HOW A CELL IS EDITED. Not through openxlsx2's data model -- through the run
## XML underneath it, `wb$worksheets[[i]]$sheet_data$cc$is`. A run in a real
## shell carries more than arsbridge models:
##
##   <r><rPr><rFont val="Arial"/><i val="1"/><color rgb="FF000000"/>
##            <sz val="10"/></rPr><t>Safety Population</t></r>
##
## `rFont` and `sz` are on every run of the exemplar workbook, and a
## superscript footnote marker would add `vertAlign`. None of them are in the
## run model, which exists to DETECT annotations, not to reproduce formatting.
## Rebuilding a cell from that model would quietly reset Arial 10 to the
## workbook default on exactly the cells being edited. Editing the XML instead
## means a run that is kept is never deserialized, so it cannot be degraded --
## stripping an annotation removes an `<r>` and leaves its siblings alone, and
## filling a placeholder rewrites the text inside one `<t>`.
##
## openxlsx2 still owns the file: `wb_load()` and `wb_save()` handle the zip,
## the content types and the relationships. tools/xlsx_roundtrip_check.R is
## the standing proof that it returns everything it was not asked to change.
##
## WHAT IS NEVER WRITTEN. A cell whose result does not exist is left showing
## its placeholder and reported as pending (ADR 0002). Blanking it would make
## an incomplete table look finished, and an empty cell in a TLF reads as a
## zero. Every literal in the sheet -- labels, footnotes, numbers the author
## typed -- is left exactly as authored.

## ---------------------------------------------------------------------------
## Reading results out of the ARD
## ---------------------------------------------------------------------------

#' Flatten the columns of an ARD that the cell map keys on.
#'
#' `{cards}` stores several of these as list columns, because a group level
#' can be any type. The lookup compares them as text, so they are unboxed
#' once here rather than at every cell.
#' @noRd
.ard_chr <- function(column) {
  if (is.null(column)) return(NULL)
  if (!is.list(column)) return(as.character(column))
  vapply(column, function(x) {
    if (is.null(x) || length(x) == 0) return(NA_character_)
    v <- x[[1]]
    if (is.null(v) || is.function(v)) NA_character_ else as.character(v)[[1]]
  }, character(1))
}

#' Numeric values of the ARD's `stat` column, NA for anything not a number.
#' @noRd
.ard_num <- function(column) {
  if (is.null(column)) return(numeric())
  vapply(column, function(x) {
    if (is.null(x) || length(x) == 0) return(NA_real_)
    v <- x[[1]]
    if (is.null(v) || !is.numeric(v)) NA_real_ else as.numeric(v)[[1]]
  }, numeric(1))
}

## The proportion stats {cards} reports on a 0-1 scale, which a shell shows as
## a percentage.
.PROPORTION_STATS <- c("p", "pct", "percent")

#' Put proportion stats on the 0-100 scale a shell's placeholder expects.
#'
#' Gated per analysis, and only when every one of that analysis's proportions
#' is inside [0, 1]: a value outside it is already a percentage (or a
#' denominator defect), and multiplying again would be worse than leaving it.
#' Shared with the rendering path (`.tfrmt_prep_ard_layout()` in
#' `ars_to_tfrmt.R`), which calls this rather than keeping its own copy: a
#' filled shell and a rendered table disagreeing about whether a cell says
#' 25.0 or 0.3 is the kind of difference nobody notices until a reviewer does.
#' @noRd
.rescale_proportions <- function(values, stat_names, analysis_ids) {
  is_pct <- !is.na(stat_names) & stat_names %in% .PROPORTION_STATS
  if (!any(is_pct)) return(values)
  for (id in unique(analysis_ids[is_pct])) {
    rows <- is_pct & analysis_ids %in% id
    vals <- values[rows]
    if (all(is.na(vals) | (vals >= 0 & vals <= 1.0000001))) {
      values[rows] <- vals * 100
    }
  }
  values
}

#' Index an ARD for cell lookup.
#'
#' One flat frame carrying only the columns a cell record keys on, with
#' proportions already rescaled. Built once per fill rather than per cell.
#'
#' @return data.frame(analysis_id, group_level, variable_level, stat_name,
#'   value, status), or NULL when the ARD has no usable columns.
#' @noRd
.ard_index <- function(ard) {
  if (is.null(ard) || !is.data.frame(ard) || nrow(ard) == 0) return(NULL)
  if (!all(c("analysis_id", "stat_name") %in% names(ard))) return(NULL)

  analysis_id <- .ard_chr(ard[["analysis_id"]])
  stat_name   <- .ard_chr(ard[["stat_name"]])
  value       <- .ard_num(ard[["stat"]])
  value       <- .rescale_proportions(value, stat_name, analysis_id)

  status <- if ("result_status" %in% names(ard)) {
    .ard_chr(ard[["result_status"]])
  } else {
    rep("computed", nrow(ard))
  }

  ## The column-axis level. A single-grouping analysis puts it in group1_level;
  ## a declared cross of two groupings spreads it over group1_level and
  ## group2_level, and the shell's column label is then the two joined -- which
  ## is what result_group_path already carries, so it is preferred when present.
  group_level <- if ("result_group_path" %in% names(ard)) {
    path <- .ard_chr(ard[["result_group_path"]])
    fallback <- .ard_chr(ard[["group1_level"]])
    ifelse(is.na(path) | !nzchar(path), fallback, path)
  } else {
    .ard_chr(ard[["group1_level"]])
  }

  ## The row grouping a nested child carries: its parent's level (the system
  ## organ class a preferred term sits under). It is the group level that is
  ## NOT the column axis, which for a nested block is always the one after it
  ## -- the column grouping is authored first and the row grouping appended.
  nest_cols <- setdiff(grep("^group[0-9]+_level$", names(ard), value = TRUE),
                       "group1_level")
  nest_level <- rep(NA_character_, nrow(ard))
  for (col in nest_cols) {
    v <- .ard_chr(ard[[col]])
    take <- is.na(nest_level) & !is.na(v)
    nest_level[take] <- v[take]
  }

  data.frame(
    analysis_id    = analysis_id,
    group_level    = group_level %||% NA_character_,
    variable_level = .ard_chr(ard[["variable_level"]]) %||% NA_character_,
    nest_level     = nest_level,
    stat_name      = stat_name,
    value          = value,
    status         = status,
    stringsAsFactors = FALSE
  )
}

#' Compare two labels ignoring case, punctuation and repeated spaces.
#'
#' Used only where the two sides are known to be THE SAME text written two
#' ways. A crossed column axis is the case that needs it: the shell heads the
#' column "Drug 10 mg Week 12" by stacking two header rows, and the ARD names
#' the same column "Drug 10 mg > Week 12" by joining the grouping path with
#' its own separator. The components are identical and only the punctuation
#' between them differs, so folding punctuation matches them without
#' inventing anything. It cannot make two different labels equal.
#' @noRd
.same_label <- function(a, b) {
  fold <- function(x) {
    x <- tolower(as.character(x %||% ""))
    trimws(gsub("[[:space:]]+", " ", gsub("[^[:alnum:]]+", " ", x)))
  }
  fold(a) == fold(b)
}

#' The one value a cell slot asks for.
#'
#' @return list(value, status). `status` is "computed" when a number is in
#'   hand, "manual_pending" when the ARD reserved the cell for a manual
#'   derivation (ADR 0002), "no_value" when the analysis ran but produced
#'   nothing for this cell, and "no_row" when the ARD has no such row at all.
#'   Only "computed" is ever written.
#' @noRd
.ard_value <- function(index, analysis_id, group_level, variable_level,
                       stat_name, nest_level = NA_character_) {
  none <- function(status) list(value = NA_real_, status = status)
  if (is.null(index) || is.na(stat_name %||% NA_character_)) {
    return(none("no_row"))
  }

  rows <- index$analysis_id %in% analysis_id & index$stat_name %in% stat_name

  ## A nested child's cell names one term UNDER one parent level; the same
  ## term can appear under two parents, so the parent has to be part of the
  ## key or the two would answer each other's cells.
  if (!is.na(nest_level %||% NA_character_) && "nest_level" %in% names(index)) {
    rows <- rows & !is.na(index$nest_level) & index$nest_level %in% nest_level
  }

  ## A group level of NA means the analysis is not reported by column, so
  ## every column shows the same value and the level must not be matched on.
  if (!is.null(group_level) && !is.na(group_level) && nzchar(group_level)) {
    rows <- rows & !is.na(index$group_level) &
      (index$group_level %in% group_level |
         .same_label(index$group_level, group_level))
  }
  if (!is.null(variable_level) && !is.na(variable_level)) {
    rows <- rows & !is.na(index$variable_level) &
      index$variable_level %in% variable_level
  }

  hits <- which(rows)
  if (length(hits) == 0) return(none("no_row"))

  ## More than one level answering to one cell means the row is a TEMPLATE for
  ## a repeated block -- "<System Organ Class>" stands for however many system
  ## organ classes the data has, not for one. Writing the first of them would
  ## put a real number in the cell and make a whole missing block invisible,
  ## which is the failure this whole design exists to avoid. Filling it needs
  ## row expansion; until then it is reported.
  levels_hit <- unique(index$variable_level[hits])
  if (length(levels_hit) > 1) return(none("ambiguous"))

  ## Prefer a computed row: an analysis can carry both a reserved stub and a
  ## real result when part of it was derived later.
  usable <- index$status[hits] %in% "computed" & !is.na(index$value[hits])
  computed <- hits[usable]
  if (length(computed) > 0) {
    return(list(value = index$value[[computed[[1]]]], status = "computed"))
  }
  if (any(index$status[hits] %in% "manual_pending")) {
    return(none("manual_pending"))
  }
  none("no_value")
}

#' Pair a categorical block's shell row labels with the data values they name.
#'
#' A shell writes the decode and the data holds the code: the rows say
#' "Female" and "Male" while ADSL.SEX holds "F" and "M". Nothing in the ARS
#' carries the codelist, so the two have to be reconciled here or the most
#' ordinary table in clinical reporting fills nothing.
#'
#' The reconciliation is deliberately allowed to FAIL rather than guess. A
#' label is paired with a data value only when:
#'
#'   * it matches exactly (ignoring case and punctuation), or
#'   * exactly ONE data value is a case-insensitive prefix of it, and
#'   * the resulting pairing is one-to-one across the whole block.
#'
#' So "Female"/"Male" against F/M pairs, and "Male"/"Missing" against M/M does
#' not -- it is ambiguous, and an ambiguous pairing is dropped entirely rather
#' than resolved by order. Every pairing that is used gets reported, because a
#' filled cell whose row was matched by inference should be visible to whoever
#' reviews the workbook.
#'
#' @param labels The shell's level labels for one analysis.
#' @return Named character vector, label -> data value. Absent names are
#'   labels that could not be paired.
#' @noRd
.level_decode <- function(index, analysis_id, labels) {
  labels <- unique(labels[!is.na(labels)])
  if (is.null(index) || length(labels) == 0) return(character())

  values <- unique(index$variable_level[index$analysis_id %in% analysis_id])
  values <- values[!is.na(values)]
  if (length(values) == 0) return(character())

  ## Anything that already matches needs no pairing at all.
  unmatched <- labels[!vapply(labels, function(l) {
    any(values %in% l | .same_label(values, l))
  }, logical(1))]
  if (length(unmatched) == 0) return(character())

  spare <- values[!vapply(values, function(v) {
    any(labels %in% v | .same_label(labels, v))
  }, logical(1))]

  pairs <- character()
  for (label in unmatched) {
    hit <- spare[startsWith(tolower(label), tolower(spare))]
    if (length(hit) != 1) next
    pairs[[label]] <- hit
  }

  ## All of the block or none of it. Two labels claiming one data value means
  ## the prefix rule identified neither; and one label left unpaired means
  ## some value is still unaccounted for, so a pairing that looked unique may
  ## only look that way because its real partner was not considered. "Mild"
  ## uniquely begins with "M" -- but if "Moderate" is also on the sheet and
  ## unresolved, "M" is as likely to be Moderate.
  if (anyDuplicated(pairs) || length(pairs) != length(unmatched)) {
    return(character())
  }
  pairs
}

## ---------------------------------------------------------------------------
## Turning a value into the text the placeholder asked for
## ---------------------------------------------------------------------------

#' Format one value to the decimals its placeholder token declared.
#'
#' The placeholder IS the format specification -- "xx.x" means one decimal --
#' so nothing else in the pipeline has to carry it. Written as text, and
#' deliberately: a TLF cell shows "12.0", and a number would print as "12".
#' @noRd
.format_value <- function(value, decimals) {
  if (is.na(value)) return(NA_character_)
  digits <- if (is.na(decimals %||% NA_integer_)) 0L else as.integer(decimals)
  formatC(value, format = "f", digits = digits)
}

#' Rewrite a placeholder's text with the values its slots resolved to.
#'
#' Substitution is by the recorded character range, right to left so earlier
#' offsets stay valid, which keeps whatever punctuation the author put between
#' the tokens: "xx.x (xx.xx)" becomes "58.0 (3.65)", parentheses and all.
#'
#' @param values Character, one per slot, NA where the slot did not resolve.
#' @return The new text, or NULL when no slot resolved (nothing to write).
#' @noRd
.format_placeholder <- function(text, slots, values) {
  if (length(slots) == 0 || all(is.na(values))) return(NULL)
  out <- text
  for (i in rev(seq_along(slots))) {
    if (is.na(values[[i]])) next
    slot <- slots[[i]]
    out <- paste0(substr(out, 1L, slot$start - 1L),
                  values[[i]],
                  substr(out, slot$stop + 1L, nchar(out)))
  }
  out
}

## ---------------------------------------------------------------------------
## Cell XML
## ---------------------------------------------------------------------------

#' Where a cell's runs live in the workbook, as an `<is>` container.
#'
#' A cell stores its string in one of two places and the writer has to reach
#' both. Inline (`t="inlineStr"`) is how openpyxl writes and how every fixture
#' is authored; shared (`t="s"`) is how Excel writes, so any shell a user has
#' opened and re-saved. A shared entry can back MANY cells -- in a TLF shell
#' it routinely does, since "xx.x (xx.xx)" is one string for a whole table --
#' so it is never edited in place. It is copied into the cell instead, which
#' detaches this one cell and leaves every other user of the string alone.
#'
#' @return The `<is>` XML, or NA when the cell holds no string.
#' @noRd
.cell_is_xml <- function(wb, cc, slot) {
  if (identical(cc$c_t[[slot]], "s")) {
    index <- suppressWarnings(as.integer(cc$v[[slot]])) + 1L
    if (is.na(index) || index < 1L || index > length(wb$sharedStrings)) {
      return(NA_character_)
    }
    entry <- wb$sharedStrings[[index]]
    return(sub("^<si", "<is", sub("</si>$", "</is>", entry)))
  }
  xml <- cc$is[[slot]]
  if (is.null(xml) || !nzchar(xml)) NA_character_ else xml
}

#' Write a cell's runs back, always as an inline string. See `.cell_is_xml()`
#' for why a shared cell is converted rather than updated in place.
#'
#' `drop_style` also clears the cell's style index. Used when a cell was
#' ENTIRELY an annotation: emptying its text but keeping its formatting leaves
#' a cell that is still red, which shows up the moment anything is written
#' there -- as the figure series is, into the block the annotation occupied.
#' @noRd
.cell_set_is <- function(cc, slot, xml, drop_style = FALSE) {
  cc$is[[slot]]  <- xml
  cc$c_t[[slot]] <- "inlineStr"
  cc$v[[slot]]   <- ""
  if (isTRUE(drop_style)) cc$c_s[[slot]] <- ""
  cc
}

## ---------------------------------------------------------------------------
## Where openxlsx2 thinks a cell is
## ---------------------------------------------------------------------------
##
## Every cell in `sheet_data$cc` carries a `key`, and the writer serializes the
## sheet FROM that key alone: cells are grouped into `<row>` elements by
## `key %/% 16384`, whatever their `r` and `row_r` say, and a row with no
## `row_attr` record is not written at all. Both failures are silent, and both
## are worse than a crash:
##
##   * A cell serialized inside the wrong `<row>` is invalid XLSX, so Excel
##     discards it -- while openxlsx2's own reader places cells by `r` and
##     reports the sheet as complete. A filled listing can therefore pass every
##     check on the R side and open in Excel with its columns empty.
##   * A cell in a row with no `row_attr` record simply is not in the file.
##
## So everything below that moves a cell or adds one maintains the key, and
## every row written to gets a row record first. `.cc_problems()` states both
## invariants and is checked before the workbook is saved.

## openxlsx2's stride: one row is 16384 columns wide, and the key is
## row * stride + column.
.CC_ROW_STRIDE <- 16384

#' The key openxlsx2 files a cell under.
#' @noRd
.cc_key <- function(row, col) as.numeric(row) * .CC_ROW_STRIDE + as.numeric(col)

#' Add a cell to `cc`, cloning `template` for its style and filing it under
#' the key its own row and column give it.
#' @noRd
.cc_add <- function(cc, template, row, col) {
  new <- template
  new$key   <- .cc_key(row, col)
  new$r     <- .xlsx_rc_to_a1(row, col)
  new$row_r <- as.character(row)
  new$c_r   <- .xlsx_num_to_col(col)
  rbind(cc, new)
}

#' Put `cc` back in the order openxlsx2 keeps it in.
#' @noRd
.cc_sorted <- function(cc) {
  cc <- cc[order(cc$key), , drop = FALSE]
  rownames(cc) <- NULL
  cc
}

#' Give every row in `rows` a `row_attr` record, so cells written there are
#' serialized at all. Rows that already have one keep theirs, heights and all.
#' @noRd
.ensure_row_records <- function(ws, rows) {
  attr_df <- ws$sheet_data$row_attr
  if (is.null(attr_df) || nrow(attr_df) == 0) return(ws)

  have <- suppressWarnings(as.integer(attr_df$r))
  need <- setdiff(rows, have[!is.na(have)])
  if (length(need) == 0) return(ws)

  added <- attr_df[rep(1, length(need)), , drop = FALSE]
  added[] <- lapply(added, function(column) rep("", length(need)))
  added$r <- as.character(need)

  attr_df <- rbind(attr_df, added)
  attr_df <- attr_df[order(suppressWarnings(as.integer(attr_df$r))), , drop = FALSE]
  rownames(attr_df) <- NULL
  ws$sheet_data$row_attr <- attr_df
  ws
}

#' Every way one worksheet's cells could fail to survive being written.
#'
#' Cheap enough to run on every sheet of every fill: it is two vector
#' comparisons. See the section note above for why it exists.
#'
#' @return A character vector of problems, empty when the sheet is sound.
#' @noRd
.cc_problems <- function(ws) {
  cc <- ws$sheet_data$cc
  if (is.null(cc) || nrow(cc) == 0) return(character())

  rows <- suppressWarnings(as.integer(cc$row_r))
  cols <- vapply(cc$c_r, .xlsx_col_to_num, integer(1), USE.NAMES = FALSE)
  problems <- character()

  misfiled <- which(!is.na(rows) & !is.na(cols) &
                      cc$key != .cc_key(rows, cols))
  if (length(misfiled) > 0) {
    problems <- c(problems, sprintf(
      "%d cell(s) are filed under the wrong row, starting at %s",
      length(misfiled), cc$r[[misfiled[[1]]]]))
  }

  attr_df <- ws$sheet_data$row_attr
  have <- suppressWarnings(as.integer(attr_df$r %||% character()))
  orphans <- setdiff(rows[!is.na(rows)], have[!is.na(have)])
  if (length(orphans) > 0) {
    problems <- c(problems, sprintf(
      "%d row(s) hold cells but have no row record, starting at row %d",
      length(orphans), min(orphans)))
  }

  problems
}

#' Check every sheet against `.cc_problems()` on the way out.
#'
#' Nothing a user does can make this fire -- it fires when arsbridge itself has
#' built a workbook that will lose cells on the way to Excel. It reports rather
#' than stops so the rest of the run still lands, but it says plainly that the
#' file is not trustworthy.
#' @noRd
.check_sheets_writable <- function(wb, sheet_names = character()) {
  for (i in seq_along(wb$worksheets)) {
    problems <- .cc_problems(wb$worksheets[[i]])
    if (length(problems) == 0) next
    name <- if (i <= length(sheet_names)) sheet_names[[i]] else as.character(i)
    .diag_gap(
      stage = "fill_shell", severity = "FAIL", input = INPUT_SHELL,
      problem = sprintf("Sheet %s came out malformed: %s.",
                        name, paste(problems, collapse = "; ")),
      why = paste("Excel discards cells written like this, so the sheet would",
                  "open with data missing even though R can still read it."),
      fix = paste("This is an arsbridge bug, not a shell problem -- please",
                  "report it with the shell that produced it."),
      location = name)
  }
  invisible(NULL)
}

#' The `<t>` nodes of an `<is>` container, in document order -- one per run,
#' or a single bare one for an unformatted cell.
#' @noRd
.is_text_nodes <- function(doc) {
  xml2::xml_find_all(doc, ".//*[local-name()='t']")
}

#' Map each character of the NORMALIZED text back to its position in the raw
#' text.
#'
#' The cell map's offsets index the text as the parser saw it, and the parser
#' normalizes: zero-width characters are dropped and a few typographic ones
#' are folded to ASCII. Folding is one-for-one, but dropping is not, so
#' normalized position i is not necessarily raw position i. Both operations
#' are per-character, so the mapping can be recovered by walking once.
#' @noRd
.raw_index_map <- function(raw) {
  chars <- strsplit(raw, "", fixed = TRUE)[[1]]
  which(!chars %in% c("\u200B", "\uFEFF"))
}

#' Replace character ranges in a cell's runs.
#'
#' Ranges are in the coordinates of the cell's normalized concatenated text,
#' which is what the cell map recorded. Each run's span in that text is walked
#' so a range is applied to the run that actually holds it; a range spanning
#' two runs puts the whole replacement in the first and deletes the remainder
#' from the others, which cannot lose text because a placeholder token is
#' only ever x's and digits.
#'
#' @param ranges List of list(start, stop, text), in any order.
#' @return TRUE when anything changed.
#' @noRd
.replace_ranges <- function(doc, ranges) {
  nodes <- .is_text_nodes(doc)
  if (length(nodes) == 0 || length(ranges) == 0) return(FALSE)

  raw <- vapply(nodes, xml2::xml_text, character(1))
  maps <- lapply(raw, .raw_index_map)
  lens <- vapply(maps, length, integer(1))
  ends <- cumsum(lens)
  starts <- ends - lens + 1L

  ## Right to left, so a replacement never invalidates an earlier offset.
  ranges <- ranges[order(vapply(ranges, function(r) r$start, integer(1)),
                         decreasing = TRUE)]
  changed <- FALSE

  for (range in ranges) {
    written <- FALSE
    for (i in seq_along(nodes)) {
      outside <- range$stop < starts[[i]] || range$start > ends[[i]]
      if (lens[[i]] == 0 || outside) next
      ## The part of this range that falls inside this run, in run-local
      ## normalized coordinates, then mapped back to raw ones.
      from <- max(range$start, starts[[i]]) - starts[[i]] + 1L
      to   <- min(range$stop,  ends[[i]])   - starts[[i]] + 1L
      raw_from <- maps[[i]][[from]]
      raw_to   <- maps[[i]][[to]]
      text <- raw[[i]]
      replacement <- if (written) "" else range$text
      raw[[i]] <- paste0(substr(text, 1L, raw_from - 1L), replacement,
                         substr(text, raw_to + 1L, nchar(text)))
      xml2::xml_text(nodes[[i]]) <- raw[[i]]
      written <- TRUE
      changed <- TRUE
    }
  }

  ## Trailing or leading spaces decide whether a label reads "Mean (SD)" or
  ## "Mean(SD)", so every edited run has to keep them.
  for (node in nodes) xml2::xml_set_attr(node, "xml:space", "preserve")
  changed
}

#' Remove the annotation runs from a cell, keeping the rest byte-identical.
#'
#' A cell with no run left carrying text is emptied rather than left holding
#' an empty run, which is how a figure sheet's all-red annotation block
#' disappears.
#'
#' @return TRUE when a run was removed.
#' @noRd
.drop_annotation_runs <- function(doc) {
  runs <- xml2::xml_find_all(doc, "./*[local-name()='r']")
  if (length(runs) == 0) return(FALSE)

  removed <- FALSE
  for (run in runs) {
    props <- xml2::xml_find_first(run, "./*[local-name()='rPr']")
    if (inherits(props, "xml_missing")) next
    ## The same test the parser used to call this run an annotation, given the
    ## same shape it reads: the run's own properties plus its text. Reusing it
    ## is the point -- a run the reader treated as an annotation and the writer
    ## left behind would put a red bracket in a deliverable.
    meta <- c(.xlsx_font_props(props),
              list(text = xml2::xml_text(run), highlight = NA_character_))
    if (.is_annotation_styled_run(meta)) {
      xml2::xml_remove(run)
      removed <- TRUE
    }
  }
  removed
}

#' The text a cell would read as, from its `<is>` XML.
#' @noRd
.is_text <- function(doc) {
  nodes <- .is_text_nodes(doc)
  if (length(nodes) == 0) return("")
  .normalize_shell_text(paste(vapply(nodes, xml2::xml_text, character(1)),
                              collapse = ""))
}

#' Serialize an `<is>` container back to the form openxlsx2 stores.
#' @noRd
.is_xml <- function(doc) as.character(doc, options = "no_declaration")

## ---------------------------------------------------------------------------
## Making room: shifting rows down
## ---------------------------------------------------------------------------

## Worksheet constructs that carry a row reference `.shift_rows_down()` does
## not move. Each maps to the field openxlsx2 keeps it in.
.UNSHIFTABLE <- c(
  conditionalFormatting = "conditional formatting",
  dataValidations       = "data validation",
  hyperlinks            = "hyperlinks",
  autoFilter            = "an autofilter",
  rowBreaks             = "manual row breaks",
  protectedRanges       = "protected ranges",
  tableParts            = "a worksheet table",
  cellWatches           = "cell watches"
)

#' What on this sheet would be left pointing at the wrong row by a shift.
#'
#' A shift moves cell references, per-row records, merged ranges and the
#' declared extent -- the four things every shell in scope actually uses.
#' A worksheet can carry several more row-bounded constructs, and a formula
#' spanning the insertion point would need rewriting rather than moving.
#' None of them appear in the shells this was built against, and rather than
#' half-support them the writer declines the sheet: a listing left exactly as
#' authored is a visible gap, whereas a conditional format still pointing at
#' the row a footnote used to occupy is a wrong deliverable that opens
#' cleanly.
#'
#' @return Character vector of human-readable names; empty when safe.
#' @noRd
.unshiftable_features <- function(ws, from_row) {
  found <- character()
  for (field in names(.UNSHIFTABLE)) {
    value <- tryCatch(ws[[field]], error = function(e) NULL)
    if (!is.null(value) && length(value) > 0) {
      found <- c(found, unname(.UNSHIFTABLE[[field]]))
    }
  }

  ## A formula at or below the insertion point moves with its cell, but one
  ## ABOVE it may address a range that the shift resizes -- and neither is
  ## rewritten here.
  cc <- ws$sheet_data$cc
  if (!is.null(cc) && nrow(cc) > 0 && "f" %in% names(cc)) {
    if (any(!is.na(cc$f) & nzchar(cc$f))) found <- c(found, "formulas")
  }
  unique(found)
}

#' Move every row at or below `from_row` down by `by` rows.
#'
#' A listing's shell has ONE template row standing for however many rows the
#' data has, so filling it means inserting rows -- and openxlsx2 has no API
#' for that (there is no `wb_insert_rows()`; `delete_data` and
#' `wb_clean_sheet` are the whole of it). The insertion is therefore done
#' here, against the four pieces of worksheet state that carry a row number:
#'
#'   sheet_data$cc        every cell's reference (`r`), row (`row_r`) and key
#'   sheet_data$row_attr  the per-row record, including its height
#'   mergeCells           a merged range that starts at or below the point
#'   dimension            the sheet's declared extent
#'
#' Miss any one of them and the file still opens, which is the danger: a
#' stale merge silently swallows a data row, and a stale dimension makes
#' Excel ignore rows past the old extent.
#'
#' The moved rows keep their own heights, and the rows opened up inherit the
#' template row's, so an expanded listing keeps the row height its author
#' chose rather than reverting to the default.
#'
#' @param template_row The row whose height newly-opened rows inherit.
#' @return The worksheet, mutated.
#' @noRd
.shift_rows_down <- function(ws, from_row, by, template_row = NA_integer_) {
  if (by <= 0) return(ws)

  ## Cells.
  cc <- ws$sheet_data$cc
  if (!is.null(cc) && nrow(cc) > 0) {
    rows <- suppressWarnings(as.integer(cc$row_r))
    move <- !is.na(rows) & rows >= from_row
    if (any(move)) {
      cc$row_r[move] <- as.character(rows[move] + by)
      cc$r[move] <- paste0(cc$c_r[move], cc$row_r[move])
      ## The key is what the cell is actually written by, so it moves too --
      ## by whole rows, which is exactly one stride each.
      cc$key[move] <- cc$key[move] + by * .CC_ROW_STRIDE
    }
    ws$sheet_data$cc <- .cc_sorted(cc)
  }

  ## Per-row records, plus one new record per opened row so the inserted
  ## rows are real rows rather than gaps.
  attr_df <- ws$sheet_data$row_attr
  if (!is.null(attr_df) && nrow(attr_df) > 0) {
    rows <- suppressWarnings(as.integer(attr_df$r))
    move <- !is.na(rows) & rows >= from_row
    attr_df$r[move] <- as.character(rows[move] + by)

    template <- attr_df[!is.na(rows) & rows == template_row, , drop = FALSE]
    blank <- if (nrow(template) == 1) template[1, , drop = FALSE] else attr_df[1, , drop = FALSE]
    added <- blank[rep(1, by), , drop = FALSE]
    added$r <- as.character(seq.int(from_row, length.out = by))
    attr_df <- rbind(attr_df, added)
    attr_df <- attr_df[order(suppressWarnings(as.integer(attr_df$r))), , drop = FALSE]
    rownames(attr_df) <- NULL
    ws$sheet_data$row_attr <- attr_df
  }

  ## Merged ranges. A range that STRADDLES the insertion point is extended;
  ## one entirely below it moves whole.
  if (length(ws$mergeCells)) {
    ws$mergeCells <- vapply(ws$mergeCells, function(node) {
      ref <- sub('.*ref="([^"]+)".*', "\\1", node)
      halves <- strsplit(ref, ":", fixed = TRUE)[[1]]
      if (length(halves) != 2) return(node)
      from <- .xlsx_a1_to_rc(halves[[1]])
      to   <- .xlsx_a1_to_rc(halves[[2]])
      if (anyNA(c(from, to)) || to[["row"]] < from_row) return(node)
      r1 <- if (from[["row"]] >= from_row) from[["row"]] + by else from[["row"]]
      r2 <- to[["row"]] + by
      sprintf('<mergeCell ref="%s:%s"/>',
              .xlsx_rc_to_a1(r1, from[["col"]]), .xlsx_rc_to_a1(r2, to[["col"]]))
    }, character(1))
  }

  ## Declared extent.
  if (length(ws$dimension) == 1 && !is.na(ws$dimension)) {
    ref <- sub('.*ref="([^"]+)".*', "\\1", ws$dimension)
    halves <- strsplit(ref, ":", fixed = TRUE)[[1]]
    if (length(halves) == 2) {
      from <- .xlsx_a1_to_rc(halves[[1]])
      to   <- .xlsx_a1_to_rc(halves[[2]])
      if (!anyNA(c(from, to))) {
        ws$dimension <- sprintf(
          '<dimension ref="%s:%s"/>',
          .xlsx_rc_to_a1(from[["row"]], from[["col"]]),
          .xlsx_rc_to_a1(to[["row"]] + by, to[["col"]]))
      }
    }
  }

  ws
}

## ---------------------------------------------------------------------------
## Filling one sheet
## ---------------------------------------------------------------------------

#' Resolve every slot of one mapped cell against the ARD.
#'
#' @return list(values, statuses) -- one entry per slot, values as formatted
#'   text and NA where the slot did not resolve.
#' @noRd
.resolve_cell <- function(cell, index, decode = character(),
                          nest_level = NA_character_) {
  slots <- cell$slots %||% list()
  values <- rep(NA_character_, length(slots))
  statuses <- rep(NA_character_, length(slots))

  level <- cell$variable_level %||% NA_character_
  if (!is.na(level) && level %in% names(decode)) level <- decode[[level]]

  for (i in seq_along(slots)) {
    slot <- slots[[i]]
    stat <- slot$stat_name %||% NA_character_
    if (is.na(stat)) {
      statuses[[i]] <- "unbound"
      next
    }
    hit <- .ard_value(
      index          = index,
      analysis_id    = cell$analysis_id %||% NA_character_,
      group_level    = (cell$group %||% list())$label %||% NA_character_,
      variable_level = level,
      stat_name      = stat,
      nest_level     = nest_level)
    statuses[[i]] <- hit$status
    if (identical(hit$status, "computed")) {
      values[[i]] <- .format_value(hit$value, slot$decimals)
    }
  }
  list(values = values, statuses = statuses)
}

#' Why a cell could not be filled, in the words its reader needs.
#'
#' Reported for the FIRST slot that failed, not the worst one. A cell showing
#' "xx (xx.x)" whose count is missing and whose percentage is surplus has two
#' problems, and the one that matters is the leading number: saying only that
#' the percentage was surplus would describe the cell as a typing quibble when
#' the actual result is absent.
#' @noRd
.pending_reason <- function(statuses) {
  failed <- statuses[!statuses %in% "computed"]
  if (length(failed) == 0) return(NA_character_)
  switch(
    failed[[1]],
    manual_pending = "reserved for manual derivation",
    unbound   = paste("the placeholder asks for a statistic the analysis",
                      "does not produce"),
    no_value  = "the analysis produced no value for this cell",
    ambiguous = paste("the row stands for a repeated block, which needs",
                      "row expansion"),
    "no result in the ARD for this cell")
}

#' Resolve the shell-label-to-data-value pairing for every analysis on a
#' sheet, once, and report each pairing that had to be inferred.
#' @noRd
.sheet_level_decodes <- function(cells, index, sheet_name) {
  by_analysis <- list()
  for (cell in cells) {
    id <- cell$analysis_id %||% ""
    level <- cell$variable_level %||% NA_character_
    if (!nzchar(id) || is.na(level)) next
    by_analysis[[id]] <- unique(c(by_analysis[[id]], level))
  }

  decodes <- list()
  for (id in names(by_analysis)) {
    pairs <- .level_decode(index, id, by_analysis[[id]])
    if (length(pairs) == 0) next
    decodes[[id]] <- pairs
    .diag_gap(
      stage = "fill_shell", severity = "INFO", input = INPUT_SHELL,
      problem = sprintf(
        "Matched %s to the data by name: %s.", id,
        paste(sprintf("%s -> %s", names(pairs), unname(pairs)),
              collapse = "; ")),
      why = paste("The shell shows decoded labels and the data holds codes,",
                  "and each label named exactly one value."),
      fix = paste("Check the pairing. State the codelist in the shell",
                  "annotation if it is wrong."),
      location = sheet_name %||% "")
  }
  decodes
}

#' Fill the mapped cells of one table sheet.
#'
#' Mutates the workbook in place (openxlsx2 worksheets are R6 objects).
#'
#' @return list(records) -- one record per mapped cell, saying what happened
#'   to it.
#' @noRd
.fill_table_sheet <- function(wb, sheet_index, cells, index, keep_pending,
                              sheet_name = NA_character_) {
  cc <- wb$worksheets[[sheet_index]]$sheet_data$cc
  records <- list()

  decodes <- .sheet_level_decodes(cells, index, sheet_name)

  for (cell in cells) {
    record <- list(ref = cell$ref, row = cell$row, col = cell$col,
                   analysis_id = cell$analysis_id %||% NA_character_)

    ## The map already knew this cell had nothing to bind to.
    if (!identical(cell$kind, "result")) {
      record$status <- "pending"
      record$reason <- cell$reason %||% "not bound to an analysis"
      records[[length(records) + 1L]] <- record
      next
    }

    slot <- which(cc$r == cell$ref)
    if (length(slot) != 1) {
      record$status <- "skipped"
      record$reason <- "the cell is not in the workbook"
      records[[length(records) + 1L]] <- record
      next
    }

    xml <- .cell_is_xml(wb, cc, slot)
    if (is.na(xml)) {
      record$status <- "skipped"
      record$reason <- "the cell holds no text to replace"
      records[[length(records) + 1L]] <- record
      next
    }

    decode <- decodes[[cell$analysis_id %||% ""]] %||% character()
    resolved <- .resolve_cell(cell, index, decode)
    if (!any(resolved$statuses %in% "computed")) {
      record$status <- "pending"
      record$reason <- .pending_reason(resolved$statuses)
      records[[length(records) + 1L]] <- record
      next
    }

    doc <- xml2::read_xml(xml)
    ranges <- list()
    for (i in seq_along(cell$slots)) {
      if (is.na(resolved$values[[i]])) next
      ranges[[length(ranges) + 1L]] <- list(
        start = cell$slots[[i]]$start,
        stop  = cell$slots[[i]]$stop,
        text  = resolved$values[[i]])
    }

    if (.replace_ranges(doc, ranges)) {
      cc <- .cell_set_is(cc, slot, .is_xml(doc))
      record$status <- "filled"
      record$text   <- .is_text(doc)
      ## Partly filled: some slots resolved, others did not. The cell is
      ## written -- a table showing "58.0 (xx.xx)" is more honest than one
      ## showing nothing -- but it is still counted as pending.
      if (any(is.na(resolved$values))) {
        record$status <- "partial"
        record$reason <- .pending_reason(resolved$statuses)
      }
    } else {
      record$status <- "skipped"
      record$reason <- "the placeholder text could not be located in the cell"
    }
    records[[length(records) + 1L]] <- record
  }

  if (!keep_pending) {
    cc <- .blank_pending_cells(cc, records)
  }

  wb$worksheets[[sheet_index]]$sheet_data$cc <- cc
  list(records = records)
}

## ---------------------------------------------------------------------------
## The big N in a column header
## ---------------------------------------------------------------------------

#' The `xx` of an `(N=xx)` header placeholder, as a range in the cell's
#' normalized text -- the coordinates `.replace_ranges()` works in.
#'
#' Only the number is replaced, never the decoration around it: the author
#' wrote "(N=XX)" or "(N = xx)" and gets their own spacing and brackets back.
#' A header that already states a number ("(N=86)") is left alone -- it is a
#' literal the author typed, and this writer never overwrites those.
#' @noRd
.n_placeholder_span <- function(text) {
  hit <- regexpr("\\(\\s*[Nn]\\s*=\\s*[xX]+\\s*\\)", text %||% "", perl = TRUE)
  if (hit[[1]] == -1L) return(NULL)
  group <- substr(text, hit[[1]], hit[[1]] + attr(hit, "match.length") - 1L)
  token <- regexpr("[xX]+", group)
  start <- hit[[1]] + token[[1]] - 1L
  list(start = start, stop = start + attr(token, "match.length") - 1L)
}

#' The denominator one column's percentages are computed against.
#'
#' A header's big N is the population of that column, and the reading of it a
#' reviewer can CHECK is the denominator the percentages under it divide by --
#' `N` in the ARD, for the analyses this column actually shows a percentage
#' for. Sourcing it from anywhere else (a subject-count row, the dataset)
#' would let a header read 86 over percentages computed out of 84, and
#' nothing in the deliverable would say which was wrong.
#'
#' A column showing no percentage therefore gets nothing: `N` on a continuous
#' analysis is {cards}' count of non-missing values, which is a different
#' quantity that happens to share a name, and writing it into a header would
#' state a population the table cannot be checked against.
#'
#' Nothing is written unless the column's analyses agree either. They can
#' disagree legitimately -- a table whose rows are reported against different
#' populations -- and then the header is not one number.
#'
#' @return list(value, status, reason).
#' @noRd
.column_denominator <- function(index, analysis_ids, group_label) {
  none <- function(reason) list(value = NA_real_, status = "pending",
                                reason = reason)
  if (is.null(index) || length(analysis_ids) == 0) {
    return(none("no result in this column is shown as a percentage"))
  }

  rows <- index$analysis_id %in% analysis_ids &
    index$stat_name %in% "N" &
    index$status %in% "computed" & !is.na(index$value)
  if (!is.null(group_label) && !is.na(group_label) && nzchar(group_label)) {
    rows <- rows & !is.na(index$group_level) &
      (index$group_level %in% group_label |
         .same_label(index$group_level, group_label))
  }

  values <- unique(index$value[rows])
  if (length(values) == 0) {
    return(none("no analysis in this column reports a denominator"))
  }
  if (length(values) > 1) {
    return(none(paste("the column's analyses report different denominators",
                      paste(sort(values), collapse = " / "))))
  }
  list(value = values[[1]], status = "computed", reason = NA_character_)
}

#' Write each result column's big N into its header.
#'
#' Separate from the body fill because the answer is a property of the COLUMN,
#' not of any one cell: it is looked up once per column rather than per row,
#' and a header cell is never blanked when pending placeholders are cleared --
#' emptying it would take the arm's name with it.
#'
#' @return list(records) -- one record per header that carried a placeholder.
#' @noRd
.fill_header_n <- function(wb, sheet_index, fill, index) {
  rows <- fill$source$header_rows %||% integer()
  columns <- fill$columns %||% list()
  if (length(rows) == 0 || length(columns) == 0) return(list(records = list()))

  cc <- wb$worksheets[[sheet_index]]$sheet_data$cc
  records <- list()

  for (column in columns) {
    ## The analyses this column shows a PERCENTAGE for -- the ones whose
    ## denominator the header has to agree with. Not every analysis in the
    ## column, and not the table at large: see `.column_denominator()`.
    own <- Filter(function(cell) {
      identical(cell$col, column$col) && identical(cell$kind, "result") &&
        any(vapply(cell$slots %||% list(), function(s) {
          identical(s$stat_name %||% NA_character_, "p")
        }, logical(1)))
    }, fill$cells %||% list())
    ids <- unique(vapply(own, function(cell) {
      cell$analysis_id %||% NA_character_
    }, character(1)))
    ids <- ids[!is.na(ids)]

    for (row in rows) {
      ref  <- .xlsx_rc_to_a1(row, column$col)
      slot <- which(cc$r == ref)
      if (length(slot) != 1) next
      xml <- .cell_is_xml(wb, cc, slot)
      if (is.na(xml)) next

      doc  <- xml2::read_xml(xml)
      span <- .n_placeholder_span(.is_text(doc))
      if (is.null(span)) next

      record <- list(ref = ref, row = row, col = column$col,
                     analysis_id = NA_character_, kind = "header_n",
                     placeholder = "(N=xx)")
      hit <- .column_denominator(index, ids, column$label)
      if (!identical(hit$status, "computed")) {
        record$status <- "pending"
        record$reason <- hit$reason
      } else if (.replace_ranges(doc, list(list(start = span$start,
                                                stop  = span$stop,
                                                text  = .format_value(hit$value, 0L))))) {
        cc <- .cell_set_is(cc, slot, .is_xml(doc))
        record$status <- "filled"
        record$text   <- .is_text(doc)
      } else {
        record$status <- "skipped"
        record$reason <- "the (N=xx) placeholder could not be located in the cell"
      }
      records[[length(records) + 1L]] <- record
    }
  }

  wb$worksheets[[sheet_index]]$sheet_data$cc <- cc
  list(records = records)
}

#' Clear the placeholders of cells that were never filled.
#'
#' Off by default. A shell whose unfilled cells still read "xx.x" says which
#' numbers are missing; one whose unfilled cells are blank looks finished and
#' reads as zero. Blanking is offered for the case where the workbook goes to
#' someone who will fill it by hand.
#' @noRd
.blank_pending_cells <- function(cc, records) {
  for (record in records) {
    if (!record$status %in% c("pending", "partial")) next
    slot <- which(cc$r == record$ref)
    if (length(slot) != 1) next
    cc <- .cell_set_is(cc, slot, "<is><t/></is>")
  }
  cc
}

## ---------------------------------------------------------------------------
## Nested blocks: two authored rows become the whole hierarchy
## ---------------------------------------------------------------------------

#' The levels of a nested block, in the order both writers present them.
#'
#' Parent levels are whatever either analysis observed; a parent's terms are
#' the child's own levels under it. Ordering goes through
#' `.nested_level_order()`, which the Word renderer also calls -- a filled
#' workbook and a rendered document that put system organ classes in a
#' different order would be two answers to one question.
#'
#' @return list of list(level, terms), in presentation order.
#' @noRd
.nested_block_levels <- function(index, plan) {
  parent_rows <- index$analysis_id %in% (plan$parent$analysis_id %||% "")
  child_rows  <- index$analysis_id %in% (plan$child$analysis_id %||% "")
  if (!any(child_rows) && !any(parent_rows)) return(list())

  basis <- .nested_sort_basis(plan$sort %||% NA_character_)
  ## The index has no column labels of its own; the group level IS the column.
  freq_arg <- function(rows, keys) {
    .nested_level_freq(keys, index$stat_name[rows], index$value[rows],
                       index$group_level[rows], basis_col = basis)
  }

  parents <- unique(c(index$variable_level[parent_rows],
                      index$nest_level[child_rows]))
  parent_freq <- freq_arg(child_rows, index$nest_level[child_rows])
  own <- freq_arg(parent_rows, index$variable_level[parent_rows])
  if (length(own) > 0) parent_freq[names(own)] <- own
  parents <- .nested_level_order(parents, parent_freq, plan$sort %||% NA_character_)

  lapply(parents, function(level) {
    in_block <- child_rows & !is.na(index$nest_level) & index$nest_level == level
    ## {cards} expands the full parent x child cartesian; a term belongs under
    ## a parent only where a subject was actually observed, so a term whose
    ## count is zero everywhere in this block is not part of it.
    counted <- in_block & index$stat_name %in% "n" & !is.na(index$value) &
      index$value > 0
    terms <- unique(index$variable_level[counted])
    terms <- .nested_level_order(
      terms, freq_arg(in_block, index$variable_level[in_block]),
      plan$sort %||% NA_character_)
    list(level = level, terms = terms)
  })
}

#' Expand a nested block's two authored rows into the hierarchy they stand for.
#'
#' The shell writes `<System Organ Class>` over `<Preferred Term>` and means
#' "repeat this per system organ class". Filling it therefore changes the
#' shape of the sheet, like a listing: rows below move down
#' (`.shift_rows_down()`), and each written line takes its own template row's
#' cells, so the author's fonts, indents and number formats carry through the
#' block.
#'
#' The parent's line is written from the parent row's cells, each term's from
#' the child row's -- which is what keeps a term indented under its class.
#'
#' @return list(records) -- one per cell written or declined.
#' @noRd
.fill_nested_block <- function(wb, sheet_index, fill, index,
                               sheet_name = NA_character_) {
  plan <- fill$nested
  if (is.null(plan)) return(list(records = list()))

  blocks <- .nested_block_levels(index, plan)
  record <- list(ref = NA_character_, row = plan$parent$row,
                 col = NA_integer_,
                 analysis_id = plan$parent$analysis_id %||% NA_character_)
  if (length(blocks) == 0) {
    record$status <- "pending"
    record$reason <- "the nested block's analyses produced no levels"
    return(list(records = list(record)))
  }

  ## The cells the two authored rows carry, by column: the pattern every
  ## written line follows.
  cells_of <- function(row) {
    Filter(function(cell) identical(cell$row, row), fill$cells %||% list())
  }
  parent_cells <- cells_of(plan$parent$row)
  child_cells  <- cells_of(plan$child$row)

  lines <- list()
  for (block in blocks) {
    lines[[length(lines) + 1L]] <- list(source = plan$parent$row,
                                        label = block$level,
                                        cells = parent_cells,
                                        variable_level = block$level,
                                        nest_level = NA_character_)
    for (term in block$terms) {
      lines[[length(lines) + 1L]] <- list(source = plan$child$row,
                                          label = term,
                                          cells = child_cells,
                                          variable_level = term,
                                          nest_level = block$level)
    }
  }

  ws <- wb$worksheets[[sheet_index]]
  extra <- length(lines) - 2L   # the pair already occupies two rows

  ## Refuse rather than half-move, exactly as an expanding listing does.
  if (extra > 0) {
    blockers <- .unshiftable_features(ws, plan$child$row + 1L)
    if (length(blockers) > 0) {
      .diag_gap(
        stage = "fill_shell", severity = "FAIL", input = INPUT_SHELL,
        problem = sprintf(
          "Nested block on sheet %s cannot be expanded: the sheet carries %s.",
          sheet_name %||% "?", paste(blockers, collapse = ", ")),
        why = paste("Expanding the block shifts rows, and those would be left",
                    "pointing at the wrong ones."),
        fix = paste("Remove them from the shell, or fill this table by hand.",
                    "Every other sheet is still filled."),
        location = sheet_name %||% "")
      record$status <- "skipped"
      record$reason <- sprintf("the sheet carries %s, which cannot be shifted",
                               paste(blockers, collapse = ", "))
      return(list(records = list(record)))
    }
    ws <- .shift_rows_down(ws, from_row = plan$child$row + 1L, by = extra,
                           template_row = plan$child$row)
  }

  cc <- ws$sheet_data$cc
  ## Both authored rows' cells, read BEFORE anything is written: every line
  ## clones them, and the first two lines overwrite the originals.
  source_row <- function(row) cc[cc$row_r == as.character(row), , drop = FALSE]
  source_cells <- list()
  source_cells[[as.character(plan$parent$row)]] <- source_row(plan$parent$row)
  source_cells[[as.character(plan$child$row)]]  <- source_row(plan$child$row)

  in_col <- function(template, col) {
    which(vapply(template$c_r, .xlsx_col_to_num, integer(1)) == col)
  }

  ## Write one cell, cloning the authored row's own cell when the line is on a
  ## row the shift opened up -- so a written line carries the template's style
  ## index, not the workbook default.
  put <- function(cc, row, col, xml, template) {
    ref  <- .xlsx_rc_to_a1(row, col)
    slot <- which(cc$r == ref)
    if (length(slot) == 1) return(.cell_set_is(cc, slot, xml))
    hit <- in_col(template, col)
    proto <- if (length(hit) >= 1) template[hit[[1]], , drop = FALSE] else
      template[1, , drop = FALSE]
    proto$is  <- xml
    proto$c_t <- "inlineStr"
    proto$v   <- ""
    .cc_add(cc, proto, row, col)
  }

  records <- list()
  for (i in seq_along(lines)) {
    line <- lines[[i]]
    target <- plan$parent$row + i - 1L

    ## The stub label: the data level takes the token's place, in the token
    ## cell's own styling -- which is what indents a term under its class.
    template <- source_cells[[as.character(line$source)]]
    if (length(in_col(template, 1L)) >= 1) {
      cc <- put(cc, target, 1L,
                sprintf("<is><t xml:space=\"preserve\">%s</t></is>",
                        .xml_escape(line$label)), template)
    }

    for (cell in line$cells) {
      rec <- list(ref = .xlsx_rc_to_a1(target, cell$col), row = target,
                  col = cell$col,
                  analysis_id = cell$analysis_id %||% NA_character_)
      if (!identical(cell$kind, "result")) {
        rec$status <- "pending"
        rec$reason <- cell$reason %||% "not bound to an analysis"
        records[[length(records) + 1L]] <- rec
        next
      }

      ## The authored cell's own placeholder is the format, as everywhere
      ## else: it is re-read from the template so each written line keeps the
      ## decimals the author chose.
      hit <- in_col(template, cell$col)
      xml <- if (length(hit) >= 1) .cell_is_xml(wb, template, hit[[1]]) else NA_character_
      if (is.na(xml)) {
        rec$status <- "skipped"
        rec$reason <- "the cell holds no text to replace"
        records[[length(records) + 1L]] <- rec
        next
      }

      cell$variable_level <- line$variable_level
      resolved <- .resolve_cell(cell, index, nest_level = line$nest_level)
      if (!any(resolved$statuses %in% "computed")) {
        rec$status <- "pending"
        rec$reason <- .pending_reason(resolved$statuses)
        records[[length(records) + 1L]] <- rec
        next
      }

      doc <- xml2::read_xml(xml)
      ranges <- list()
      for (k in seq_along(cell$slots)) {
        if (is.na(resolved$values[[k]])) next
        ranges[[length(ranges) + 1L]] <- list(
          start = cell$slots[[k]]$start, stop = cell$slots[[k]]$stop,
          text  = resolved$values[[k]])
      }
      if (.replace_ranges(doc, ranges)) {
        cc <- put(cc, target, cell$col, .is_xml(doc), template)
        rec$status <- "filled"
        rec$text   <- .is_text(doc)
        if (any(is.na(resolved$values))) {
          rec$status <- "partial"
          rec$reason <- .pending_reason(resolved$statuses)
        }
      } else {
        rec$status <- "skipped"
        rec$reason <- "the placeholder text could not be located in the cell"
      }
      records[[length(records) + 1L]] <- rec
    }
  }

  ws$sheet_data$cc <- .cc_sorted(cc)
  ws <- .ensure_row_records(ws, seq.int(plan$parent$row,
                                        length.out = length(lines)))
  wb$worksheets[[sheet_index]] <- ws
  list(records = records)
}

## ---------------------------------------------------------------------------
## Listings: one template row becomes N
## ---------------------------------------------------------------------------

#' An `<is>` container carrying `text`, in the formatting of an existing one.
#'
#' The template row's cells are the model for every row written under it, so
#' their run properties are reused rather than reconstructed -- the same
#' reason the table writer edits run XML (see the file header). Runs after the
#' first are dropped: the template cell holds one placeholder, and the value
#' replacing it is one string.
#' @noRd
.cell_with_text <- function(template_xml, text) {
  if (is.na(template_xml) || !nzchar(template_xml)) {
    return(sprintf("<is><t xml:space=\"preserve\">%s</t></is>", .xml_escape(text)))
  }
  doc <- xml2::read_xml(template_xml)
  runs <- xml2::xml_find_all(doc, "./*[local-name()='r']")
  if (length(runs) > 1) for (i in seq_along(runs)[-1]) xml2::xml_remove(runs[[i]])
  node <- xml2::xml_find_first(doc, ".//*[local-name()='t']")
  if (inherits(node, "xml_missing")) {
    return(sprintf("<is><t xml:space=\"preserve\">%s</t></is>", .xml_escape(text)))
  }
  xml2::xml_text(node) <- text
  xml2::xml_set_attr(node, "xml:space", "preserve")
  .is_xml(doc)
}

#' The five characters XML cannot carry raw.
#' @noRd
.xml_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

#' How a listing value is written.
#'
#' Everything becomes text, because a listing column is a display of what the
#' data says, not a recomputation of it: a subject id, a coded severity, a
#' date that is already a formatted string in the ADaM data. `NA` becomes an
#' empty cell -- in a listing, unlike a table, a blank means "not recorded"
#' rather than zero.
#' @noRd
.listing_value <- function(x) {
  if (length(x) == 0 || is.na(x)) return("")
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if (is.numeric(x)) {
    return(if (x == round(x)) format(x, trim = TRUE) else format(x, trim = TRUE))
  }
  as.character(x)
}

#' Expand a listing's template row into one row per record.
#'
#' The shell states a listing as a header row and ONE row of placeholders
#' standing for however many rows the data has. Filling it therefore changes
#' the shape of the sheet, which is what makes a listing different from a
#' table: rows below the template -- a footnote and its merge -- have to move
#' down to make room (`.shift_rows_down()`).
#'
#' Each written row is built from the template row's own cells, so the
#' author's fonts, alignment and borders carry down the block.
#'
#' @return list(records) -- one record for the listing as a whole.
#' @noRd
.fill_listing_sheet <- function(wb, sheet_index, plan, rows, keep_pending,
                                sheet_name = NA_character_) {
  template_row <- plan$template_row %||% NA_integer_
  record <- list(ref = NA_character_, row = template_row, col = NA_integer_,
                 analysis_id = NA_character_)

  if (is.na(template_row)) {
    record$status <- "pending"
    record$reason <- "the shell has no template row to expand"
    return(list(records = list(record)))
  }
  if (is.null(rows)) {
    record$status <- "pending"
    record$reason <- "the listing's rows could not be read from the data"
    return(list(records = list(record)))
  }
  if (nrow(rows) == 0) {
    ## A listing with no rows is a real answer -- no subject met the criteria
    ## -- but it is not the same as one that was never filled, so the template
    ## stays and it is reported.
    record$status <- "pending"
    record$reason <- "the listing selected no rows"
    return(list(records = list(record)))
  }

  n <- nrow(rows)
  ws <- wb$worksheets[[sheet_index]]

  ## Refuse rather than half-move. Expanding a listing shifts rows, and a
  ## construct this cannot shift would be left addressing the row something
  ## else now occupies -- a file that opens cleanly and is wrong. The sheet is
  ## left exactly as authored and reported instead.
  if (n > 1) {
    blockers <- .unshiftable_features(ws, template_row + 1L)
    if (length(blockers) > 0) {
      .diag_gap(
        stage = "fill_shell", severity = "FAIL", input = INPUT_SHELL,
        problem = sprintf(
          "Listing sheet %s carries %s, which arsbridge cannot move.",
          sheet_name %||% "?", paste(blockers, collapse = ", ")),
        why = paste("Expanding the listing shifts rows, and those would be",
                    "left pointing at the wrong ones."),
        fix = paste("Remove them from the shell, or fill this listing by",
                    "hand. Every other sheet is still filled."),
        location = sheet_name %||% "")
      record$status <- "skipped"
      record$reason <- sprintf("the sheet carries %s, which cannot be shifted",
                               paste(blockers, collapse = ", "))
      return(list(records = list(record)))
    }
  }

  ## Make room BEFORE writing, so the template row's own cells are still where
  ## the plan says they are.
  if (n > 1) {
    ws <- .shift_rows_down(ws, from_row = template_row + 1L, by = n - 1L,
                           template_row = template_row)
  }

  cc <- ws$sheet_data$cc
  template <- cc[cc$row_r == as.character(template_row), , drop = FALSE]
  if (nrow(template) == 0) {
    record$status <- "skipped"
    record$reason <- "the template row is not in the workbook"
    return(list(records = list(record)))
  }

  written <- 0L
  for (i in seq_len(n)) {
    target <- template_row + i - 1L
    for (k in seq_len(nrow(template))) {
      col <- .xlsx_col_to_num(template$c_r[[k]])
      if (is.na(col) || col > ncol(rows)) next
      value <- .listing_value(rows[[col]][[i]])
      xml <- .cell_with_text(.cell_is_xml(wb, template, k), value)
      ref <- .xlsx_rc_to_a1(target, col)

      slot <- which(cc$r == ref)
      if (length(slot) == 1) {
        cc <- .cell_set_is(cc, slot, xml)
      } else {
        ## A row opened up by the shift has no cells yet: clone the template's,
        ## so the new row inherits its style index.
        new <- template[k, , drop = FALSE]
        new$is <- xml
        new$c_t <- "inlineStr"
        new$v <- ""
        cc <- .cc_add(cc, new, target, col)
      }
      written <- written + 1L
    }
  }

  ws$sheet_data$cc <- .cc_sorted(cc)
  ws <- .ensure_row_records(ws, seq.int(template_row, length.out = n))
  wb$worksheets[[sheet_index]] <- ws

  record$status <- "filled"
  record$rows <- n
  record$cells <- written
  list(records = list(record))
}

#' The rows a listing displays, or NULL when they cannot be read.
#'
#' Delegates to `.listing_data()` in `ars_render_listing.R` -- the same
#' assembly the renderer uses, so a filled listing and a rendered one cannot
#' contain different subjects.
#' @noRd
.listing_rows <- function(spec, output, adam_dir, sheet) {
  if (is.null(adam_dir) || !nzchar(adam_dir) || !dir.exists(adam_dir)) {
    .diag_gap(
      stage = "fill_shell", severity = "WARN", input = INPUT_DATA,
      problem = sprintf("Listing %s needs the subject-level data to fill.",
                        output$id %||% sheet),
      why = "A listing's rows ARE the data; there is nothing to compute them from.",
      fix = "Pass adam_dir = the folder holding your ADaM datasets.",
      location = sheet %||% "")
    return(NULL)
  }
  tryCatch(
    .listing_data(spec, output, adam_dir, max_rows = Inf)$data,
    error = function(e) {
      .diag_gap(
        stage = "fill_shell", severity = "WARN", input = INPUT_DATA,
        problem = sprintf("Listing %s could not be assembled: %s",
                          output$id %||% sheet, conditionMessage(e)),
        why = "Its sheet is left as authored.",
        fix = "Check that the datasets the listing references are in adam_dir.",
        location = sheet %||% "")
      NULL
    })
}

## ---------------------------------------------------------------------------
## Figures: a series computed from the shell's own prose
## ---------------------------------------------------------------------------

#' `DATASET.VARIABLE` out of a figure directive's value.
#'
#' A directive says things like `ADVS.AVISITN (label ADVS.AVISIT)` or
#' `mean of ADVS.AVAL`; only the first qualified reference is the variable.
#' @noRd
.figure_ref <- function(value) {
  m <- regmatches(value, regexpr("[A-Za-z][A-Za-z0-9]*\\.[A-Za-z][A-Za-z0-9_]*",
                                 value %||% ""))
  if (length(m) == 0) return(NULL)
  parts <- strsplit(m[[1]], ".", fixed = TRUE)[[1]]
  list(dataset = toupper(parts[[1]]), variable = parts[[2]])
}

#' The second qualified reference, when a directive names a label variable
#' alongside the plotted one (`ADVS.AVISITN (label ADVS.AVISIT)`).
#' @noRd
.figure_label_ref <- function(value) {
  m <- gregexpr("[A-Za-z][A-Za-z0-9]*\\.[A-Za-z][A-Za-z0-9_]*", value %||% "")
  hits <- regmatches(value %||% "", m)[[1]]
  if (length(hits) < 2) return(NULL)
  parts <- strsplit(hits[[2]], ".", fixed = TRUE)[[1]]
  list(dataset = toupper(parts[[1]]), variable = parts[[2]])
}

#' Write a figure's data series where its annotation block was.
#'
#' A figure sheet has no analyses -- the shell states the plot as prose
#' ("Y axis -> mean of ADVS.AVAL", "Series (colour) -> ADVS.TRTA"), and the
#' ARS carries those directives rather than an analysis per series. So unlike
#' a table or a listing, a figure's numbers are computed here, from the
#' datasets, against what the shell asked for.
#'
#' What is written is the series TABLE, not a chart: one row per series and
#' x-value, with n, the mean and its standard error. The shell's own chart
#' object stays whatever the author made it; a programmer picks the series up
#' from these cells. Drawing a chart into someone else's workbook is a
#' different job from filling one in.
#'
#' @return list(records).
#' @noRd
.fill_figure_sheet <- function(wb, sheet_index, plan, adam_dir, sheet) {
  anchor <- plan$series_anchor %||% NA_integer_
  record <- list(ref = NA_character_, row = anchor, col = NA_integer_,
                 analysis_id = NA_character_)

  directive <- function(key) {
    hit <- Filter(function(d) identical(d$key, key), plan$directives %||% list())
    if (length(hit) == 0) NULL else hit[[1]]$value
  }

  if (is.na(anchor)) {
    record$status <- "pending"
    record$reason <- "the shell states no annotation block to write the series into"
    return(list(records = list(record)))
  }
  if (is.null(adam_dir) || !nzchar(adam_dir) || !dir.exists(adam_dir)) {
    record$status <- "pending"
    record$reason <- "no ADaM directory was given, so the series could not be computed"
    return(list(records = list(record)))
  }

  x <- .figure_ref(directive("x_axis"))
  y <- .figure_ref(directive("y_axis"))
  s <- .figure_ref(directive("series"))
  if (is.null(x) || is.null(y)) {
    record$status <- "pending"
    record$reason <- "the shell does not name both an x axis and a y axis variable"
    return(list(records = list(record)))
  }

  df <- .listing_load(adam_dir, y$dataset)
  if (is.null(df)) {
    record$status <- "pending"
    record$reason <- sprintf("dataset %s is not in the ADaM directory", y$dataset)
    return(list(records = list(record)))
  }

  ## The filter is an ordinary where clause, so it goes through the same
  ## parser the annotations use rather than a second dialect.
  filter_txt <- directive("filter")
  if (!is.null(filter_txt) && nzchar(filter_txt)) {
    ## parse_where_clause() already returns the { condition: ... } wrapper
    ## .listing_eval_where() expects; wrapping it again makes the filter a
    ## silent no-op that averages every parameter in the dataset together.
    wc <- parse_where_clause(filter_txt)
    if (is.null(wc)) {
      .diag_gap(
        stage = "fill_shell", severity = "WARN", input = INPUT_SHELL,
        problem = sprintf("Figure filter %s could not be parsed.",
                          dQuote(filter_txt, q = FALSE)),
        why = "The series would be computed over unfiltered data.",
        fix = "Write the filter as DATASET.VAR='VALUE'.",
        location = sheet %||% "")
      record$status <- "pending"
      record$reason <- "the figure's filter could not be parsed"
      return(list(records = list(record)))
    }
    df <- df[.listing_eval_where(df, wc), , drop = FALSE]
  }

  need <- c(x$variable, y$variable, if (!is.null(s)) s$variable)
  missing <- setdiff(need, names(df))
  if (length(missing) > 0) {
    record$status <- "pending"
    record$reason <- sprintf("%s does not have %s",
                             y$dataset, paste(missing, collapse = ", "))
    return(list(records = list(record)))
  }
  if (nrow(df) == 0) {
    record$status <- "pending"
    record$reason <- "the figure's filter selected no records"
    return(list(records = list(record)))
  }

  label_ref <- .figure_label_ref(directive("x_axis"))
  label_var <- if (!is.null(label_ref) && label_ref$variable %in% names(df)) {
    label_ref$variable
  } else NULL

  ## One row per series and x value: n, mean, and the standard error the
  ## shell's error-bar directive asks for.
  keys <- list(df[[x$variable]])
  names(keys) <- x$variable
  if (!is.null(label_var)) keys[[label_var]] <- df[[label_var]]
  if (!is.null(s)) keys[[s$variable]] <- df[[s$variable]]

  key_df <- data.frame(keys, stringsAsFactors = FALSE, check.names = FALSE)
  grp    <- do.call(paste, c(key_df, sep = "\r"))
  parts  <- split(df[[y$variable]], grp)
  series <- as.data.frame(
    do.call(rbind, strsplit(names(parts), "\r", fixed = TRUE)),
    stringsAsFactors = FALSE)
  names(series) <- names(key_df)
  series$n    <- vapply(parts, function(v) sum(!is.na(v)), integer(1))
  series$mean <- vapply(parts, function(v) mean(v, na.rm = TRUE), numeric(1))
  series$se   <- vapply(parts, function(v) {
    v <- v[!is.na(v)]
    if (length(v) < 2) NA_real_ else stats::sd(v) / sqrt(length(v))
  }, numeric(1))

  ord <- order(suppressWarnings(as.numeric(series[[x$variable]])),
               if (!is.null(s)) series[[s$variable]] else seq_len(nrow(series)))
  series <- series[ord, , drop = FALSE]

  ## Write a header row then the series, plainly: these cells were the red
  ## annotation block a moment ago, so they carry no formatting worth keeping.
  headers <- c(names(keys), "n", "mean", "se")
  ws <- wb$worksheets[[sheet_index]]
  cc <- ws$sheet_data$cc

  ## A number goes in as a NUMBER, not as formatted text. This block is data
  ## a programmer picks up to draw the chart, not a table cell -- rounding it
  ## to look tidy would mean the series no longer equals what the reader
  ## computes from the same datasets, and they would have to work out why.
  ## (The opposite of a table placeholder, where text is what preserves the
  ## trailing zero the author asked for.)
  put <- function(cc, row, col, value) {
    ref  <- .xlsx_rc_to_a1(row, col)
    slot <- which(cc$r == ref)
    numeric_cell <- is.numeric(value) && !is.na(value)

    if (length(slot) != 1) {
      new <- cc[1, , drop = FALSE]
      new$c_s <- ""; new$f <- ""; new$f_attr <- ""
      cc <- .cc_add(cc, new, row, col)
      slot <- nrow(cc)
    }

    if (numeric_cell) {
      cc$c_t[[slot]] <- ""
      cc$v[[slot]]   <- format(value, scientific = FALSE, digits = 15,
                               trim = TRUE)
      cc$is[[slot]]  <- ""
    } else {
      text <- if (is.na(value)) "" else as.character(value)
      cc$c_t[[slot]] <- "inlineStr"
      cc$v[[slot]]   <- ""
      cc$is[[slot]]  <- sprintf("<is><t xml:space=\"preserve\">%s</t></is>",
                                .xml_escape(text))
    }
    cc
  }

  for (j in seq_along(headers)) cc <- put(cc, anchor, j, headers[[j]])
  for (i in seq_len(nrow(series))) {
    for (j in seq_along(headers)) {
      value <- series[[headers[[j]]]][[i]]
      ## The x axis and any key columns come out of a split as text; put the
      ## numeric ones back so the sheet sorts and plots as numbers.
      if (!is.numeric(value)) {
        as_num <- suppressWarnings(as.numeric(value))
        if (!is.na(as_num) && identical(as.character(as_num), value)) value <- as_num
      }
      cc <- put(cc, anchor + i, j, value)
    }
  }

  ws$sheet_data$cc <- .cc_sorted(cc)
  ## A series longer than the annotation block it replaces runs past the last
  ## row the author left behind, and those rows do not exist yet.
  ws <- .ensure_row_records(ws, seq.int(anchor, length.out = nrow(series) + 1L))
  wb$worksheets[[sheet_index]] <- ws

  record$status <- "filled"
  record$rows <- nrow(series)
  list(records = list(record))
}

#' Strip every annotation run from one sheet.
#'
#' The annotations are instructions to the programmer, not part of the table,
#' so a deliverable must not carry them. What is left is the black label the
#' author wrote, untouched -- including the properties arsbridge does not
#' model, since the run is never rebuilt.
#'
#' @param annotated Cell references on this sheet whose text is annotation
#'   all the way through, as the reader sees them. A cell can be red without
#'   any run saying so -- the colour comes from its style, and the run
#'   inherits it -- which is how a figure sheet states its whole programming
#'   block. Those cells have nothing to strip a run from; the cell goes.
#' @return list(stripped) -- how many cells changed.
#' @noRd
.strip_sheet_annotations <- function(wb, sheet_index, annotated = character()) {
  cc <- wb$worksheets[[sheet_index]]$sheet_data$cc
  if (is.null(cc) || nrow(cc) == 0) return(list(stripped = 0L))

  stripped <- 0L
  for (slot in seq_len(nrow(cc))) {
    if (cc$r[[slot]] %in% annotated) {
      cc <- .cell_set_is(cc, slot, "<is><t/></is>", drop_style = TRUE)
      stripped <- stripped + 1L
      next
    }

    xml <- .cell_is_xml(wb, cc, slot)
    if (is.na(xml)) next
    if (!grepl("<r>|<r ", xml)) next

    doc <- xml2::read_xml(xml)
    if (!.drop_annotation_runs(doc)) next

    remaining <- .is_text_nodes(doc)
    cc <- if (length(remaining) == 0 || !nzchar(trimws(.is_text(doc)))) {
      .cell_set_is(cc, slot, "<is><t/></is>", drop_style = TRUE)
    } else {
      .cell_set_is(cc, slot, .is_xml(doc))
    }
    stripped <- stripped + 1L
  }

  wb$worksheets[[sheet_index]]$sheet_data$cc <- cc
  list(stripped = stripped)
}

#' The cells of one sheet that are annotation through and through.
#'
#' Decided from the reader's view, not the raw XML, because the reader is
#' where a cell's style font is resolved into each run -- the writer asking
#' the same question a second way is how the two would drift.
#' @noRd
.wholly_annotated_cells <- function(sheet) {
  if (is.null(sheet) || nrow(sheet$cells) == 0) return(character())
  keep <- vapply(seq_len(nrow(sheet$cells)), function(i) {
    runs <- Filter(function(r) nzchar(trimws(r$text %||% "")),
                   sheet$cells$runs[[i]])
    length(runs) > 0 && all(vapply(runs, .is_annotation_styled_run, logical(1)))
  }, logical(1))
  sheet$cells$ref[keep]
}

## ---------------------------------------------------------------------------
## Entry point
## ---------------------------------------------------------------------------

#' Write results into the shell workbook they came from
#'
#' Takes the Excel shell that was parsed to build an ARS, and returns a copy
#' of it with the computed results written into their placeholders and the
#' programming annotations removed. The layout, labels, column headers,
#' merges, fonts and footnotes are the author's own -- they are never
#' rebuilt, only left alone.
#'
#' Which result belongs in which cell is not decided here. It was recorded
#' when the ARS was built, in each output's `_meta$shell_fill` cell map (see
#' `vignette("shell-fidelity")` and ADR 0005), so a shell that was parsed by
#' an older version, or a Word shell, has no map and nothing to fill.
#'
#' A cell whose result does not exist keeps its placeholder and is reported as
#' pending rather than blanked, because an empty cell in a clinical table
#' reads as a zero. `keep_pending_placeholders = FALSE` clears them instead,
#' for a workbook that is going to someone who will complete it by hand.
#'
#' @param shell_path Path to the Excel shell (`.xlsx`) the ARS was built from.
#' @param ars The reporting event: a path to `reporting_event.json`, or the
#'   parsed list.
#' @param ard The ARD from [ars_to_ard()] holding the results to write.
#' @param output_path Where to write the filled workbook.
#' @param adam_dir Directory holding the ADaM datasets (`.xpt`, `.sas7bdat`
#'   or `.csv`). Required to fill a listing, whose rows are the subject-level
#'   data itself, and a figure, whose series the shell states as prose rather
#'   than as an analysis. Tables need only the ARD; leave it `NULL` and any
#'   listing or figure sheet is reported instead of filled.
#' @param strip_annotations Remove the red programming annotations. `TRUE` for
#'   a deliverable; `FALSE` to keep them beside the numbers while reviewing.
#' @param keep_pending_placeholders Leave an unfillable cell showing its
#'   placeholder (default) rather than blanking it.
#' @param overwrite Allow `output_path` to be replaced.
#'
#' @return Invisibly, a list with `path`, the counts `filled`, `pending` and
#'   `skipped`, and `diagnostics` -- one row per cell that was not filled,
#'   with the output, the cell reference and the reason.
#'
#' @seealso [ars_to_ard()] for the results, [spec_to_ars()] for the map.
#' @export
#' @examples
#' \dontrun{
#'   ard <- ars_to_ard("outputs/reporting_event.json", "inputs/ADaM")
#'   ars_fill_shell(
#'     shell_path  = "inputs/shells.xlsx",
#'     ars         = "outputs/reporting_event.json",
#'     ard         = ard,
#'     output_path = "outputs/filled_shells.xlsx")
#' }
ars_fill_shell <- function(shell_path, ars, ard, output_path, adam_dir = NULL,
                           strip_annotations = TRUE,
                           keep_pending_placeholders = TRUE,
                           overwrite = FALSE) {
  .require_file(shell_path, "shell_path", INPUT_SHELL)
  if (!grepl("\\.xlsx$", shell_path, ignore.case = TRUE)) {
    cli::cli_abort(c(
      "Only an Excel shell can be filled.",
      "x" = "{.path {shell_path}} is not an .xlsx file.",
      "i" = "A Word shell has no cells to write into; render the output with
             {.fn ars_render_table} instead."
    ))
  }
  if (file.exists(output_path) && !isTRUE(overwrite)) {
    cli::cli_abort(c(
      "{.path {output_path}} already exists.",
      "i" = "Pass {.code overwrite = TRUE} to replace it."
    ))
  }
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    cli::cli_abort("The {.pkg openxlsx2} package is required to fill a shell.")
  }

  spec <- if (is.character(ars)) {
    .require_file(ars, "ars", INPUT_ARS)
    .read_json(ars)
  } else {
    ars
  }

  outputs <- spec$outputs %||% list()
  fills <- Filter(Negate(is.null),
                  lapply(outputs, function(o) o$`_meta`$shell_fill))
  if (length(fills) == 0) {
    cli::cli_abort(c(
      "This reporting event carries no cell map, so nothing can be filled.",
      "i" = "A cell map is recorded only for an ARS built from an Excel
             shell. Rebuild with {.fn spec_to_ars} against the .xlsx."
    ))
  }

  index <- .ard_index(ard)
  if (is.null(index)) {
    cli::cli_warn(c(
      "The ARD carries no results, so every cell will be reported pending.",
      "i" = "Check that {.fn ars_to_ard} ran against the intended datasets."
    ))
  }

  wb <- openxlsx2::wb_load(shell_path)
  sheet_names <- unname(wb$get_sheet_names())

  ## The same read the parser does, so "is this cell an annotation?" is
  ## answered once, by the code that owns the question.
  book <- xlsx_read_shell_cells(shell_path)
  annotated <- lapply(book$sheets, .wholly_annotated_cells)

  records <- list()
  stripped <- 0L
  n_walked <- 0L   # progress ticks count every output, mapless ones included

  for (output in outputs) {
    n_walked <- n_walked + 1L
    .progress_tick(n_walked, length(outputs),
                   as.character(output$id %||% "?"))
    fill <- output$`_meta`$shell_fill
    if (is.null(fill)) {
      ## Exiting silently here is how a run "does nothing" with no trace: the
      ## output simply never appears in the census. One skipped record keeps
      ## the accounting honest -- the caller sees the output was passed over
      ## and why, instead of inferring it from an absence.
      records[[length(records) + 1L]] <- list(
        output_id = output$id %||% NA_character_,
        sheet = NA_character_, ref = NA_character_, status = "skipped",
        reason = "no cell map recorded for this output")
      next
    }
    sheet <- fill$source$sheet %||% NA_character_
    sheet_index <- match(sheet, sheet_names)
    if (is.na(sheet_index)) {
      .diag_gap(
        stage = "fill_shell", severity = "WARN", input = INPUT_SHELL,
        problem = sprintf("Output %s names sheet %s, which is not in %s.",
                          output$id %||% "?", dQuote(sheet, q = FALSE),
                          basename(shell_path)),
        why = "Its results cannot be written.",
        fix = "Fill the shell the ARS was built from.",
        location = sheet %||% "")
      ## The diagnostic above lives in a collector this function neither
      ## resets nor returns, so on its own it vanishes in a hand-run
      ## pipeline. The record travels with the return value.
      records[[length(records) + 1L]] <- list(
        output_id = output$id %||% NA_character_,
        sheet = sheet, ref = NA_character_, status = "skipped",
        reason = sprintf("the output names sheet '%s', which is not in the workbook",
                         sheet %||% "?"))
      next
    }

    ## Strip BEFORE filling, not after. A figure's series is written into the
    ## very cells its annotation block occupied, so stripping second would
    ## erase what was just written. Placeholders carry no annotation runs, so
    ## a table is unaffected by the order.
    if (isTRUE(strip_annotations)) {
      stripped <- stripped + .strip_sheet_annotations(
        wb, sheet_index, annotated[[sheet]] %||% character())$stripped
    }

    result <- NULL
    cells <- fill$cells %||% list()
    ## A nested block's two authored rows are not cells to substitute into --
    ## they are a pattern to expand. They are written by .fill_nested_block()
    ## instead, and left out here so one row is never reported twice.
    nested_rows <- if (is.null(fill$nested)) integer() else {
      c(fill$nested$parent$row, fill$nested$child$row)
    }
    if (length(cells) > 0) {
      fixed <- Filter(function(cell) !cell$row %in% nested_rows, cells)
      result <- .fill_table_sheet(wb, sheet_index, fixed, index,
                                  keep_pending_placeholders, sheet)
      ## The block expands AFTER the fixed cells are written: it moves the
      ## rows below it down, and a cell written before the shift travels with
      ## its row.
      result$records <- c(
        result$records,
        .fill_nested_block(wb, sheet_index, fill, index, sheet)$records,
        ## The arm headers last: the denominator written there is read out of
        ## the same ARD the cells underneath it came from.
        .fill_header_n(wb, sheet_index, fill, index)$records)
    } else if (!is.null(fill$listing)) {
      result <- .fill_listing_sheet(
        wb, sheet_index, fill$listing,
        .listing_rows(spec, output, adam_dir, sheet), keep_pending_placeholders,
        sheet)
    } else if (!is.null(fill$figure)) {
      result <- .fill_figure_sheet(wb, sheet_index, fill$figure, adam_dir, sheet)
    }
    for (record in (result$records %||% list())) {
      record$output_id <- output$id %||% NA_character_
      record$sheet <- sheet
      records[[length(records) + 1L]] <- record
    }

  }

  ## The annotations on a sheet that carries no cell map -- a listing, a
  ## figure, the formatting-notes tab -- are still annotations, and a
  ## deliverable must not ship them.
  if (isTRUE(strip_annotations)) {
    mapped <- vapply(outputs, function(o) {
      o$`_meta`$shell_fill$source$sheet %||% NA_character_
    }, character(1))
    mapped <- mapped[!is.na(mapped)]
    for (name in setdiff(sheet_names, mapped)) {
      stripped <- stripped + .strip_sheet_annotations(
        wb, match(name, sheet_names),
        annotated[[name]] %||% character())$stripped
    }
  }

  .check_sheets_writable(wb, sheet_names)
  openxlsx2::wb_save(wb, output_path, overwrite = TRUE)

  status <- vapply(records, function(r) r$status, character(1))
  filled  <- sum(status %in% c("filled", "partial"))
  pending <- sum(status %in% c("pending", "partial"))
  skipped <- sum(status %in% "skipped")

  unresolved <- Filter(function(r) !identical(r$status, "filled"), records)
  diagnostics <- data.frame(
    output_id = vapply(unresolved, function(r) r$output_id %||% NA_character_,
                       character(1)),
    sheet     = vapply(unresolved, function(r) r$sheet %||% NA_character_,
                       character(1)),
    ref       = vapply(unresolved, function(r) r$ref %||% NA_character_,
                       character(1)),
    status    = vapply(unresolved, function(r) r$status, character(1)),
    reason    = vapply(unresolved, function(r) r$reason %||% NA_character_,
                       character(1)),
    stringsAsFactors = FALSE
  )

  cli::cli_inform(c(
    "v" = "Filled {filled} cell{?s} in {.path {basename(output_path)}}.",
    if (pending > 0) c("!" = "{pending} cell{?s} left pending.") else NULL,
    if (skipped > 0) c("!" = "{skipped} cell{?s} skipped.") else NULL,
    if (stripped > 0) {
      c("i" = "Removed annotations from {stripped} cell{?s}.")
    } else NULL
  ))

  ## "Filled 0 cells" scrolls past; a workbook that LOOKS identical to the
  ## shell does not. When nothing at all was filled, name the dominant
  ## reason here, where the user is -- the per-cell census alone is easy to
  ## never open. The commonest cause in the field is the simplest: a clean
  ## shell, with nothing annotated, has nothing for the pipeline to bind.
  if (filled == 0 && length(records) > 0) {
    reasons <- vapply(records, function(r) r$reason %||% "unstated",
                      character(1))
    dominant <- names(sort(table(reasons), decreasing = TRUE))[[1]]
    cli::cli_warn(c(
      "Nothing was filled: all {length(records)} cell{?s} came back unresolved.",
      "i" = "The most common reason: {dominant}",
      if (identical(dominant, "no analysis covers this row")) {
        c("i" = paste("That usually means no annotations were detected in the",
                      "shell. Annotate it, or declare the bindings in a",
                      "reviewed supplement -- a supplement can bind the rows",
                      "of a clean shell."))
      } else NULL
    ))
  }

  invisible(list(path = output_path, filled = filled, pending = pending,
                 skipped = skipped, diagnostics = diagnostics))
}
