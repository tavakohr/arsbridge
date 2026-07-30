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

  data.frame(
    analysis_id    = analysis_id,
    group_level    = group_level %||% NA_character_,
    variable_level = .ard_chr(ard[["variable_level"]]) %||% NA_character_,
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
                       stat_name) {
  none <- function(status) list(value = NA_real_, status = status)
  if (is.null(index) || is.na(stat_name %||% NA_character_)) {
    return(none("no_row"))
  }

  rows <- index$analysis_id %in% analysis_id & index$stat_name %in% stat_name

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
#' @noRd
.cell_set_is <- function(cc, slot, xml) {
  cc$is[[slot]]  <- xml
  cc$c_t[[slot]] <- "inlineStr"
  cc$v[[slot]]   <- ""
  cc
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
## Filling one sheet
## ---------------------------------------------------------------------------

#' Resolve every slot of one mapped cell against the ARD.
#'
#' @return list(values, statuses) -- one entry per slot, values as formatted
#'   text and NA where the slot did not resolve.
#' @noRd
.resolve_cell <- function(cell, index, decode = character()) {
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
      stat_name      = stat)
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
      cc <- .cell_set_is(cc, slot, "<is><t/></is>")
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
      .cell_set_is(cc, slot, "<is><t/></is>")
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
#' @param datasets Unused; accepted so a later listing fill can take the
#'   subject-level data it expands from.
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
ars_fill_shell <- function(shell_path, ars, ard, output_path, datasets = NULL,
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

  for (output in outputs) {
    fill <- output$`_meta`$shell_fill
    if (is.null(fill)) next
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
      next
    }

    cells <- fill$cells %||% list()
    if (length(cells) > 0) {
      result <- .fill_table_sheet(wb, sheet_index, cells, index,
                                  keep_pending_placeholders, sheet)
      for (record in result$records) {
        record$output_id <- output$id %||% NA_character_
        record$sheet <- sheet
        records[[length(records) + 1L]] <- record
      }
    }

    if (isTRUE(strip_annotations)) {
      stripped <- stripped + .strip_sheet_annotations(
        wb, sheet_index, annotated[[sheet]] %||% character())$stripped
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

  invisible(list(path = output_path, filled = filled, pending = pending,
                 skipped = skipped, diagnostics = diagnostics))
}
