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

  group_level <- group_level %||% NA_character_
  data.frame(
    analysis_id    = analysis_id,
    group_level    = group_level,
    ## The folded column axis, computed once for the whole fill.
    ##
    ## A cell whose column label is punctuated differently from the ARD's is
    ## matched by folding both sides (see .same_label). Folding the ARD side
    ## per cell made the fill quadratic: two regex passes over every ARD row,
    ## for every cell of every output. On the bundled example that was 94% of
    ## the fill's runtime and 138 seconds on a single nested AE table -- the
    ## point at which the workflow app's bar looked hung.
    group_fold     = .fold_label(group_level),
    variable_level = .ard_chr(ard[["variable_level"]]) %||% NA_character_,
    nest_level     = nest_level,
    stat_name      = stat_name,
    value          = value,
    status         = status,
    ## Why a blocked row was blocked, carried from the ARD so the workbook can
    ## say it. Without this the fill knows only that the cell has no number,
    ## and reports a deliberate reservation in the same words as a cell nobody
    ## ever asked for.
    block_reason   = if ("block_reason" %in% names(ard)) {
      .ard_chr(ard[["block_reason"]])
    } else {
      rep(NA_character_, nrow(ard))
    },
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
  .fold_label(a) == .fold_label(b)
}

#' A label reduced to the form two spellings of it share.
#'
#' Its own function because folding is the expensive half of `.same_label()`
#' -- two regex passes over every element -- and the hot callers fold one side
#' ONCE and reuse it, rather than refolding a whole ARD column per cell.
#' @noRd
.fold_label <- function(x) {
  x <- tolower(as.character(x %||% ""))
  trimws(gsub("[[:space:]]+", " ", gsub("[^[:alnum:]]+", " ", x)))
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
                       stat_name, nest_level = NA_character_,
                       total_column = FALSE) {
  none <- function(status, reason = NA_character_) {
    list(value = NA_real_, status = status, reason = reason)
  }
  if (is.null(index) || is.na(stat_name %||% NA_character_)) {
    return(none("no_row"))
  }

  rows <- index$analysis_id %in% analysis_id & index$stat_name %in% stat_name

  ## A group level of NA means the analysis is not reported by column, so
  ## every column shows the same value and the level must not be matched on.
  if (!is.null(group_level) && !is.na(group_level) && nzchar(group_level)) {
    ## One fold of the needle against a column folded once for the whole
    ## fill. An index built by hand rather than by .ard_index() pays to fold
    ## here, which is the right price for a fixture and the wrong one for a
    ## study.
    folded <- index$group_fold %||% .fold_label(index$group_level)
    rows <- rows & !is.na(index$group_level) &
      (index$group_level %in% group_level |
         folded %in% .fold_label(group_level))
  }
  if (!is.null(variable_level) && !is.na(variable_level)) {
    rows <- rows & !is.na(index$variable_level) &
      index$variable_level %in% variable_level
  }

  ## A nested child's cell names one term UNDER one parent level; the same
  ## term can appear under two parents, so the parent has to be part of the
  ## key or the two would answer each other's cells. Keep the otherwise-
  ## complete rows long enough to distinguish the one narrow diagnostic case:
  ## a child in the Total column whose analysis/stat/term all exist, but whose
  ## required parent key does not.
  parent_needed <- !is.na(nest_level %||% NA_character_)
  rows_without_parent <- rows
  if (parent_needed) {
    if ("nest_level" %in% names(index)) {
      rows <- rows & !is.na(index$nest_level) & index$nest_level %in% nest_level
    } else {
      rows[] <- FALSE
    }
  }

  hits <- which(rows)
  if (length(hits) == 0 && parent_needed && isTRUE(total_column) &&
      any(rows_without_parent)) {
    return(none("missing_parent_key"))
  }
  if (length(hits) == 0) {
    ## A structurally reserved analysis computes NOTHING, so it carries no
    ## statistic for any key to match on -- and every cell of it would
    ## otherwise fall through to "no result in the ARD", the words for a cell
    ## nobody asked about. The reservation is a fact about the ANALYSIS, so it
    ## is looked up by analysis alone once the keyed lookup has missed.
    ##
    ## Deliberately last: a computed row that matches the key always wins, so
    ## an analysis carrying both a reservation and a real result (part of it
    ## derived later) still fills the part that exists.
    reserved <- which(index$analysis_id %in% analysis_id &
                        index$status %in% "blocked")
    if (length(reserved) > 0) {
      why <- index$block_reason[reserved]
      why <- why[!is.na(why) & nzchar(why)]
      return(none("blocked", if (length(why) > 0) why[[1]] else NA_character_))
    }
    return(none("no_row"))
  }

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
    return(list(value = index$value[[computed[[1]]]], status = "computed",
                reason = NA_character_))
  }
  if (any(index$status[hits] %in% "manual_pending")) {
    return(none("manual_pending"))
  }
  ## A blocked cell is NOT manual work and NOT an absent result. Falling
  ## through to "no_value" would render it identically to a cell nobody ever
  ## asked for, losing the one thing worth saying about it.
  ##
  ## The ARD's own reason travels with it, so a cell reserved because the
  ## reporting event does not resolve says which finding reserved it rather
  ## than only that it is empty.
  blocked <- hits[index$status[hits] %in% "blocked"]
  if (length(blocked) > 0) {
    reasons <- index$block_reason[blocked]
    reasons <- reasons[!is.na(reasons) & nzchar(reasons)]
    return(none("blocked",
                if (length(reasons) > 0) reasons[[1]] else NA_character_))
  }
  none("no_value")
}

#' Pair a categorical block's shell row labels with the data values they name.
#'
#' A shell writes the decode and the data holds the code: the rows say
#' "Female" and "Male" while ADSL.SEX holds "F" and "M".
#'
#' Normally the ARS settles this long before the fill. A spec CODELISTS sheet
#' becomes `_meta$value_decodes`, and both engines turn it into a factor whose
#' labels are the decoded values, so the ARD already reads "Female" and there
#' is nothing left to reconcile. This is the last resort for the case that
#' cannot: a spec with no codelist for the variable at all.
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
#' `slot` may be one cell or many -- a listing writes a whole column at a time
#' -- with `xml` either one string for all of them or one per slot.
#'
#' `drop_style` also clears the cell's style index. Used when a cell was
#' ENTIRELY an annotation: emptying its text but keeping its formatting leaves
#' a cell that is still red, which shows up the moment anything is written
#' there -- as the figure series is, into the block the annotation occupied.
#' @noRd
.cell_set_is <- function(cc, slot, xml, drop_style = FALSE) {
  cc$is[slot]  <- xml
  cc$c_t[slot] <- "inlineStr"
  cc$v[slot]   <- ""
  if (isTRUE(drop_style)) cc$c_s[slot] <- ""
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

#' One typed record for the fill census.
#'
#' Every writer path starts here, including records that have no cell address
#' or no ARD lookup. Typed missing values keep those cases in one stable schema
#' instead of letting data-frame assembly infer a different type from each run.
#' @noRd
.fill_record <- function(output_id = NA_character_, sheet = NA_character_,
                         ref = NA_character_, row = NA_integer_,
                         col = NA_integer_, col_label = NA_character_,
                         analysis_id = NA_character_, status = NA_character_,
                         reason = NA_character_, row_label = NA_character_,
                         method_id = NA_character_, placeholder = NA_character_,
                         ars_grouping_id = NA_character_,
                         ars_group_label = NA_character_,
                         variable_level = NA_character_,
                         parent_level = NA_character_,
                         ard_lookup_key = NA_character_) {
  chr <- function(value) {
    if (is.null(value) || length(value) == 0 || is.na(value[[1]])) {
      return(NA_character_)
    }
    as.character(value[[1]])
  }
  int <- function(value) {
    if (is.null(value) || length(value) == 0 || is.na(value[[1]])) {
      return(NA_integer_)
    }
    as.integer(value[[1]])
  }

  list(
    output_id = chr(output_id),
    sheet = chr(sheet),
    ref = chr(ref),
    row = int(row),
    col = int(col),
    col_label = chr(col_label),
    analysis_id = chr(analysis_id),
    status = chr(status),
    reason = chr(reason),
    row_label = chr(row_label),
    method_id = chr(method_id),
    placeholder = chr(placeholder),
    ars_grouping_id = chr(ars_grouping_id),
    ars_group_label = chr(ars_group_label),
    variable_level = chr(variable_level),
    parent_level = chr(parent_level),
    ard_lookup_key = chr(ard_lookup_key)
  )
}

#' A readable rendering of the dimensions passed to `.ard_value()`.
#'
#' This is diagnostic only. `.resolve_cell()` still performs the one real
#' lookup; the key simply records its arguments so a pending cell can be traced
#' without reimplementing the match.
#' @noRd
.ard_lookup_key <- function(analysis_id, slots, group_level,
                            variable_level, parent_level = NA_character_) {
  show <- function(value) {
    value <- as.character(value %||% NA_character_)
    value <- value[!is.na(value) & nzchar(value)]
    if (length(value) == 0) "<NA>" else paste(value, collapse = ",")
  }
  slot_labels <- vapply(slots %||% list(), function(slot) {
    operation <- slot$operation_id %||% NA_character_
    statistic <- slot$stat_name %||% NA_character_
    if (is.na(operation) && is.na(statistic)) return("<unbound>")
    if (is.na(operation)) return(as.character(statistic))
    if (is.na(statistic)) return(paste0(as.character(operation), ":<unbound>"))
    paste0(as.character(operation), ":", as.character(statistic))
  }, character(1))

  paste0(
    "analysis=", show(analysis_id),
    " | slots=", show(slot_labels),
    " | column=", show(group_level),
    " | variable=", show(variable_level),
    " | parent=", show(parent_level)
  )
}

#' Resolve every slot of one mapped cell against the ARD.
#'
#' @return list(values, statuses, reasons) -- one entry per slot, values as
#'   formatted text and NA where the slot did not resolve. `reasons` carries
#'   the ARD's own explanation for a slot it deliberately reserved, which is
#'   the only thing distinguishing that cell from one nobody asked about.
#' @noRd
.resolve_cell <- function(cell, index, decode = character(),
                          nest_level = NA_character_) {
  slots <- cell$slots %||% list()
  values <- rep(NA_character_, length(slots))
  statuses <- rep(NA_character_, length(slots))
  ## Same length as values/statuses, filled in the same loop below.
  reasons <- rep(NA_character_, length(slots))

  level <- cell$variable_level %||% NA_character_
  if (!is.na(level) && level %in% names(decode)) level <- decode[[level]]
  group <- cell$group %||% list()
  group_level <- group$label %||% NA_character_
  total_column <- isTRUE(group$is_total)
  analysis_id <- cell$analysis_id %||% NA_character_

  for (i in seq_along(slots)) {
    slot <- slots[[i]]
    stat <- slot$stat_name %||% NA_character_
    if (is.na(stat)) {
      statuses[[i]] <- "unbound"
      next
    }
    hit <- .ard_value(
      index          = index,
      analysis_id    = analysis_id,
      group_level    = group_level,
      variable_level = level,
      stat_name      = stat,
      nest_level     = nest_level,
      total_column   = total_column)
    statuses[[i]] <- hit$status
    reasons[[i]] <- hit$reason %||% NA_character_
    if (identical(hit$status, "computed")) {
      values[[i]] <- .format_value(hit$value, slot$decimals)
    }
  }
  list(
    values = values,
    statuses = statuses,
    reasons = reasons,
    variable_level = level,
    parent_level = nest_level,
    ard_lookup_key = .ard_lookup_key(
      analysis_id = analysis_id,
      slots = slots,
      group_level = group_level,
      variable_level = level,
      parent_level = nest_level
    )
  )
}

#' The ARD's reason for the FIRST slot that came back blocked.
#'
#' Strictly that slot's own reason. Skipping ahead to a later slot's text when
#' the first has none would attribute one slot's cause to another -- and
#' `.pending_reason()` reports on the first failure precisely because that is
#' the one the reader is looking at.
#'
#' Tolerant of a caller with no reasons to give: an older payload, or a path
#' that resolved cells before the ARD carried the column.
#' @noRd
.first_reason <- function(statuses, reasons = NULL) {
  if (is.null(reasons) || length(reasons) != length(statuses)) {
    return(NA_character_)
  }
  hit <- which(statuses %in% "blocked")
  if (length(hit) == 0) return(NA_character_)
  found <- reasons[[hit[[1L]]]]
  if (is.null(found) || length(found) != 1L || is.na(found) ||
      !nzchar(found)) {
    return(NA_character_)
  }
  found
}

#' A blocked cell's reason, in the words a shell reviewer needs.
#'
#' Two kinds arrive here and they are not the same news. A STRUCTURAL block --
#' written as `structural:<CODE>` by `.structural_block()` -- means the
#' reporting event itself does not resolve, and the code is what joins this
#' cell to its row in the validation report. Anything else is a run-time block:
#' the event was sound and the DATA could not answer, and that text is already
#' written for a reader.
#' @noRd
.reserved_cell_reason <- function(reason) {
  if (is.null(reason) || length(reason) != 1L || is.na(reason) ||
      !nzchar(reason)) {
    return("reserved: the reporting event does not resolve for this cell")
  }
  if (grepl("^structural:", reason)) {
    code <- sub("^structural:", "", reason)
    return(paste0(
      "reserved: the reporting event does not resolve for this cell (",
      code, ")"
    ))
  }
  paste0("reserved: ", reason)
}

#' Why a cell could not be filled, in the words its reader needs.
#'
#' Reported for the FIRST slot that failed, not the worst one. A cell showing
#' "xx (xx.x)" whose count is missing and whose percentage is surplus has two
#' problems, and the one that matters is the leading number: saying only that
#' the percentage was surplus would describe the cell as a typing quibble when
#' the actual result is absent.
#' @noRd
.pending_reason <- function(statuses, parent_level = NA_character_,
                            reasons = NULL) {
  failed <- statuses[!statuses %in% "computed"]
  if (length(failed) == 0) return(NA_character_)

  ## A blocked cell used to fall through to the default and report "no result
  ## in the ARD for this cell" -- the words for a cell nobody asked about, said
  ## of one arsbridge deliberately withheld. The ARD's own reason is carried
  ## here so the workbook and the validation report name the same cause.
  if (identical(failed[[1]], "blocked")) {
    return(.reserved_cell_reason(.first_reason(statuses, reasons)))
  }

  switch(
    failed[[1]],
    manual_pending = "reserved for manual derivation",
    unbound   = paste("the placeholder asks for a statistic the analysis",
                      "does not produce"),
    no_value  = "the analysis produced no value for this cell",
    ambiguous = paste("the row stands for a repeated block, which needs",
                      "row expansion"),
    missing_parent_key = sprintf(
      "the ARD has no row for required parent/nest key '%s'",
      parent_level %||% "?"
    ),
    "no result in the ARD for this cell")
}

#' The census status for a cell whose slots did not all resolve.
#' @noRd
.pending_status <- function(statuses) {
  failed <- statuses[!statuses %in% "computed"]
  if (length(failed) > 0 && identical(failed[[1]], "missing_parent_key")) {
    return("missing_parent_key")
  }
  ## Distinguished from "pending": pending means the number is coming, blocked
  ## means it cannot be computed until the spec or the data is repaired.
  if (any(failed %in% "blocked")) return("blocked")
  "pending"
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

#' Report a display column that received nothing while its siblings filled.
#'
#' Guidance rule 6: the columns the shell displays and the groups the ARD
#' generated must be the same set. The per-cell census already records every
#' unfilled cell, but a reader has to notice that all of one column's cells
#' happen to share a reason -- and the field failure that motivated this was
#' exactly that shape: three cohort columns full of numbers, a Total column of
#' placeholders, and nothing anywhere saying a whole column had been lost.
#'
#' Only fires when at least one OTHER column filled: a table where nothing
#' filled at all has a different problem, already reported elsewhere, and
#' repeating it once per column would bury it.
#' @noRd
.check_column_coverage <- function(output_id, columns, records, sheet) {
  if (length(columns %||% list()) < 2 || length(records %||% list()) == 0) {
    return(invisible(NULL))
  }
  filled_cols <- unique(unlist(lapply(records, function(r) {
    if (identical(r$status, "filled") || identical(r$status, "partial")) {
      r$col
    }
  })))
  if (length(filled_cols) == 0) return(invisible(NULL))

  mapped_cols <- unique(unlist(lapply(records, function(r) r$col)))
  for (column in columns) {
    if (!column$col %in% mapped_cols) next     # never mapped; not a loss here
    if (column$col %in% filled_cols) next
    .diag_gap(
      stage = "fill_shell", severity = "WARN", input = INPUT_ARS,
      problem = sprintf(
        "Output %s, column %s: every cell kept its placeholder while %d other column(s) filled.",
        output_id %||% "?", dQuote(column$label, q = FALSE),
        length(filled_cols)),
      why = "The column is displayed but no result reached it, so the deliverable shows a column of placeholders beside columns of numbers.",
      fix = "Check that the header's annotation resolves to subjects the ARD carries -- a column the reporting event never declared produces nothing.",
      location = sheet %||% "")
  }
  invisible(NULL)
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
    record <- .fill_record(
      ref = cell$ref,
      row = cell$row,
      col = cell$col,
      analysis_id = cell$analysis_id %||% NA_character_,
      row_label = cell$stat_line %||% cell$variable_level %||% NA_character_,
      placeholder = cell$placeholder %||% NA_character_,
      ars_grouping_id = (cell$group %||% list())$grouping_id %||%
        NA_character_,
      ars_group_label = (cell$group %||% list())$label %||% NA_character_,
      variable_level = cell$variable_level %||% NA_character_
    )

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
    record$variable_level <- resolved$variable_level
    record$parent_level <- resolved$parent_level
    record$ard_lookup_key <- resolved$ard_lookup_key
    if (!any(resolved$statuses %in% "computed")) {
      record$status <- .pending_status(resolved$statuses)
      record$reason <- .pending_reason(
        resolved$statuses,
        resolved$parent_level,
        resolved$reasons
      )
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
        record$reason <- .pending_reason(
          resolved$statuses,
          resolved$parent_level,
          resolved$reasons
        )
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
    folded <- index$group_fold %||% .fold_label(index$group_level)
    rows <- rows & !is.na(index$group_level) &
      (index$group_level %in% group_label |
         folded %in% .fold_label(group_label))
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

      grouping_ids <- unique(vapply(own, function(cell) {
        (cell$group %||% list())$grouping_id %||% NA_character_
      }, character(1)))
      grouping_ids <- grouping_ids[!is.na(grouping_ids)]
      record <- .fill_record(
        ref = ref,
        row = row,
        col = column$col,
        analysis_id = NA_character_,
        placeholder = "(N=xx)",
        ars_grouping_id = if (length(grouping_ids) == 1) {
          grouping_ids[[1]]
        } else {
          NA_character_
        },
        ars_group_label = column$label %||% NA_character_,
        ard_lookup_key = .ard_lookup_key(
          analysis_id = ids,
          slots = list(list(stat_name = "N")),
          group_level = column$label,
          variable_level = NA_character_
        )
      )
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
    if (!record$status %in% c("pending", "partial", "missing_parent_key")) next
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
  record <- .fill_record(
    ref = NA_character_,
    row = plan$parent$row,
    col = NA_integer_,
    analysis_id = plan$parent$analysis_id %||% NA_character_,
    row_label = plan$parent$label %||% NA_character_
  )
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

  in_col <- .tpl_col_hit
  put    <- .tpl_put

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
      rec <- .fill_record(
        ref = .xlsx_rc_to_a1(target, cell$col),
        row = target,
        col = cell$col,
        analysis_id = cell$analysis_id %||% NA_character_,
        row_label = line$label,
        placeholder = cell$placeholder %||% NA_character_,
        ars_grouping_id = (cell$group %||% list())$grouping_id %||%
          NA_character_,
        ars_group_label = (cell$group %||% list())$label %||% NA_character_,
        variable_level = line$variable_level,
        parent_level = line$nest_level
      )
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
      rec$variable_level <- resolved$variable_level
      rec$parent_level <- resolved$parent_level
      rec$ard_lookup_key <- resolved$ard_lookup_key
      if (!any(resolved$statuses %in% "computed")) {
        rec$status <- .pending_status(resolved$statuses)
        rec$reason <- .pending_reason(
          resolved$statuses,
          resolved$parent_level,
          resolved$reasons
        )
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
          rec$reason <- .pending_reason(
            resolved$statuses,
            resolved$parent_level,
            resolved$reasons
          )
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

#' The cells a template row holds in one column, by position in its cc frame.
#' Shared by every block expander so a written line finds the authored cell
#' whose style it clones.
#' @noRd
.tpl_col_hit <- function(template, col) {
  which(vapply(template$c_r, .xlsx_col_to_num, integer(1)) == col)
}

#' Write one cell, cloning the authored row's own cell when the line is on a
#' row the shift opened up -- so a written line carries the template's style
#' index, not the workbook default.
#' @noRd
.tpl_put <- function(cc, row, col, xml, template) {
  ref  <- .xlsx_rc_to_a1(row, col)
  slot <- which(cc$r == ref)
  if (length(slot) == 1) return(.cell_set_is(cc, slot, xml))
  hit <- .tpl_col_hit(template, col)
  proto <- if (length(hit) >= 1) template[hit[[1]], , drop = FALSE] else
    template[1, , drop = FALSE]
  proto$is  <- xml
  proto$c_t <- "inlineStr"
  proto$v   <- ""
  .cc_add(cc, proto, row, col)
}

#' The levels a categorical block expands into, in presentation order.
#'
#' The default order is the ARD's own -- which, when the ADaM spec shipped a
#' codelist, IS the codelist order (the generated code makes the variable a
#' factor over the codelist terms), zero-count levels included. That is also
#' the order the Word renderer prints, so the workbook and the document
#' cannot disagree. An authored "sort:" clause overrides through the same
#' helpers the nested blocks use.
#' @noRd
.categorical_block_levels <- function(index, block) {
  rows <- index$analysis_id %in% (block$analysis_id %||% "")
  levels <- unique(index$variable_level[rows & !is.na(index$variable_level)])
  if (length(levels) == 0) return(character())

  sort_spec <- block$sort %||% NA_character_
  if (is.na(sort_spec)) return(levels)

  basis <- .nested_sort_basis(sort_spec)
  freq <- .nested_level_freq(index$variable_level[rows],
                             index$stat_name[rows], index$value[rows],
                             index$group_level[rows], basis_col = basis)
  .nested_level_order(levels, freq, sort_spec)
}

#' Expand one categorical template block into a row per level.
#'
#' The shell wrote mock rows ("<Reason #1>", "<Reason #n>") and meant "one
#' row per level of this variable". The block's recorded template rows are
#' replaced: each level takes the next row, the anchor row's cells are the
#' formatting template, rows below move down when the levels outgrow the
#' authored rows (`.shift_rows_down()`), and authored rows the levels do not
#' reach are cleared rather than left showing placeholder text.
#'
#' @return list(records) -- one per cell written, cleared or declined.
#' @noRd
.fill_categorical_block <- function(wb, sheet_index, fill, block, index,
                                    sheet_name = NA_character_) {
  ## The plan crossed the ARS JSON: scalars survive, vectors come back as
  ## lists. Normalised once, here, so nothing below has to care.
  block$anchor_row    <- as.integer(block$anchor_row)
  block$template_rows <- as.integer(unlist(block$template_rows))

  record <- .fill_record(
    ref = NA_character_,
    row = block$anchor_row,
    col = NA_integer_,
    analysis_id = block$analysis_id %||% NA_character_,
    row_label = block$label %||% NA_character_
  )

  levels <- .categorical_block_levels(index, block)
  if (length(levels) == 0) {
    record$status <- "pending"
    record$reason <- "the categorical block's analysis produced no levels"
    return(list(records = list(record)))
  }

  ## The bound template cells: normally on the anchor row; when the anchor's
  ## columns were unmapped, the first template row that carries any.
  template_cells <- list()
  for (row in block$template_rows) {
    template_cells <- Filter(function(cell) {
      identical(cell$row, row) && isTRUE(cell$template)
    }, fill$cells %||% list())
    if (length(template_cells) > 0) break
  }

  ws <- wb$worksheets[[sheet_index]]
  extra <- length(levels) - length(block$template_rows)

  ## Refuse rather than half-move, exactly as the nested expander does.
  if (extra > 0) {
    blockers <- .unshiftable_features(ws, max(block$template_rows) + 1L)
    if (length(blockers) > 0) {
      .diag_gap(
        stage = "fill_shell", severity = "FAIL", input = INPUT_SHELL,
        problem = sprintf(
          "Categorical block '%s' on sheet %s cannot be expanded: the sheet carries %s.",
          block$label %||% "?", sheet_name %||% "?",
          paste(blockers, collapse = ", ")),
        why = paste("Expanding the block shifts rows, and those would be",
                    "left pointing at the wrong ones."),
        fix = paste("Remove them from the shell, or fill this table by hand.",
                    "Every other sheet is still filled."),
        location = sheet_name %||% "")
      record$status <- "skipped"
      record$reason <- sprintf("the sheet carries %s, which cannot be shifted",
                               paste(blockers, collapse = ", "))
      return(list(records = list(record)))
    }
    ws <- .shift_rows_down(ws, from_row = max(block$template_rows) + 1L,
                           by = extra, template_row = block$anchor_row)
  }

  cc <- ws$sheet_data$cc
  ## The anchor row's cells, read BEFORE anything is written: every line
  ## clones them, and the first line overwrites the originals.
  template <- cc[cc$row_r == as.character(block$anchor_row), , drop = FALSE]

  records <- list()
  for (i in seq_along(levels)) {
    level  <- levels[[i]]
    target <- block$anchor_row + i - 1L

    ## The stub label: the level takes the mock's place, in the mock cell's
    ## own styling.
    if (length(.tpl_col_hit(template, 1L)) >= 1) {
      cc <- .tpl_put(cc, target, 1L,
                     sprintf("<is><t xml:space=\"preserve\">%s</t></is>",
                             .xml_escape(level)), template)
    }

    for (cell in template_cells) {
      rec <- .fill_record(
        ref = .xlsx_rc_to_a1(target, cell$col),
        row = target,
        col = cell$col,
        analysis_id = cell$analysis_id %||% NA_character_,
        row_label = level,
        placeholder = cell$placeholder %||% NA_character_,
        ars_grouping_id = (cell$group %||% list())$grouping_id %||%
          NA_character_,
        ars_group_label = (cell$group %||% list())$label %||% NA_character_,
        variable_level = level
      )

      hit <- .tpl_col_hit(template, cell$col)
      xml <- if (length(hit) >= 1) .cell_is_xml(wb, template, hit[[1]]) else
        NA_character_
      if (is.na(xml)) {
        rec$status <- "skipped"
        rec$reason <- "the cell holds no text to replace"
        records[[length(records) + 1L]] <- rec
        next
      }

      cell$variable_level <- level
      resolved <- .resolve_cell(cell, index)
      rec$variable_level <- resolved$variable_level
      rec$parent_level <- resolved$parent_level
      rec$ard_lookup_key <- resolved$ard_lookup_key
      if (!any(resolved$statuses %in% "computed")) {
        rec$status <- .pending_status(resolved$statuses)
        rec$reason <- .pending_reason(
          resolved$statuses,
          resolved$parent_level,
          resolved$reasons
        )
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
        cc <- .tpl_put(cc, target, cell$col, .is_xml(doc), template)
        rec$status <- "filled"
        rec$text   <- .is_text(doc)
        if (any(is.na(resolved$values))) {
          rec$status <- "partial"
          rec$reason <- .pending_reason(
            resolved$statuses,
            resolved$parent_level,
            resolved$reasons
          )
        }
      } else {
        rec$status <- "skipped"
        rec$reason <- "the placeholder text could not be located in the cell"
      }
      records[[length(records) + 1L]] <- rec
    }
  }

  ## Authored rows the levels did not reach: cleared, not left showing mock
  ## text -- and reported, so the census says why the row is empty.
  if (extra < 0) {
    leftover <- block$template_rows[seq.int(length(levels) + 1L,
                                            length(block$template_rows))]
    clear_cols <- unique(c(1L, vapply(template_cells,
                                      function(cell) cell$col, integer(1))))
    for (row in leftover) {
      for (col in clear_cols) {
        slot <- which(cc$r == .xlsx_rc_to_a1(row, col))
        if (length(slot) == 1) {
          cc <- .cell_set_is(cc, slot, "<is><t/></is>")
        }
      }
      records[[length(records) + 1L]] <- .fill_record(
        ref = .xlsx_rc_to_a1(row, 1L),
        row = row,
        col = 1L,
        analysis_id = block$analysis_id %||% NA_character_,
        status = "skipped",
        reason = paste("the block has fewer observed levels than authored",
                       "template rows; the leftover row was cleared")
      )
    }
  }

  ws$sheet_data$cc <- .cc_sorted(cc)
  ws <- .ensure_row_records(ws, seq.int(
    block$anchor_row,
    length.out = max(length(levels), length(block$template_rows))))
  wb$worksheets[[sheet_index]] <- ws
  list(records = records)
}

#' Every row a sheet's expansions own -- excluded from the fixed-cell fill,
#' because an expansion writes them itself.
#' @noRd
.sheet_expansion_rows <- function(fill) {
  nested <- if (is.null(fill$nested)) integer() else {
    c(fill$nested$parent$row, fill$nested$child$row)
  }
  template <- unlist(lapply(fill$categorical %||% list(),
                            function(block) block$template_rows),
                     use.names = FALSE)
  as.integer(c(nested, template))
}

#' Run every expansion a sheet carries, bottom-up.
#'
#' Bottom-up is what lets several blocks (and a nested pair alongside them)
#' compose: a shift only moves rows at or below its insertion point, so
#' expanding the lowest block first means every block still waiting sits
#' above every row that has moved.
#' @noRd
.fill_sheet_expansions <- function(wb, sheet_index, fill, index,
                                   sheet_name = NA_character_) {
  jobs <- list()
  if (!is.null(fill$nested)) {
    jobs[[length(jobs) + 1L]] <- list(anchor = fill$nested$parent$row,
                                      kind = "nested", block = NULL)
  }
  for (block in fill$categorical %||% list()) {
    jobs[[length(jobs) + 1L]] <- list(anchor = block$anchor_row,
                                      kind = "categorical", block = block)
  }
  if (length(jobs) == 0) return(list(records = list()))

  anchors <- vapply(jobs, function(job) as.integer(job$anchor), integer(1))
  records <- list()
  for (job in jobs[order(anchors, decreasing = TRUE)]) {
    result <- if (identical(job$kind, "nested")) {
      .fill_nested_block(wb, sheet_index, fill, index, sheet_name)
    } else {
      .fill_categorical_block(wb, sheet_index, fill, job$block, index,
                              sheet_name)
    }
    records <- c(records, result$records)
  }
  list(records = records)
}

## ---------------------------------------------------------------------------
## Listings: one template row becomes N
## ---------------------------------------------------------------------------

#' Build the writer for one listing column's cells.
#'
#' The template row's cells are the model for every row written under it, so
#' their run properties are reused rather than reconstructed -- the same
#' reason the table writer edits run XML (see the file header). Runs after the
#' first are dropped: the template cell holds one placeholder, and the value
#' replacing it is one string.
#'
#' A writer rather than a plain `template_xml, text` function because the
#' template is the SAME for every row of a listing column, and parsing it once
#' per cell is what made a real-sized listing take minutes: the parse and the
#' node lookup happen here, once, and each call then only rewrites the text.
#'
#' @return A function of one string, returning that cell's `<is>` XML.
#' @noRd
.cell_text_writer <- function(template_xml) {
  ## No template to follow: a plain single-run cell.
  bare <- function(text) {
    sprintf("<is><t xml:space=\"preserve\">%s</t></is>", .xml_escape(text))
  }
  if (is.na(template_xml) || !nzchar(template_xml)) return(bare)

  doc <- xml2::read_xml(template_xml)
  runs <- xml2::xml_find_all(doc, "./*[local-name()='r']")
  if (length(runs) > 1) for (i in seq_along(runs)[-1]) xml2::xml_remove(runs[[i]])
  node <- xml2::xml_find_first(doc, ".//*[local-name()='t']")
  if (inherits(node, "xml_missing")) return(bare)

  xml2::xml_set_attr(node, "xml:space", "preserve")
  function(text) {
    xml2::xml_text(node) <- text
    .is_xml(doc)
  }
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
#' date. `NA` becomes an empty cell -- in a listing, unlike a table, a blank
#' means "not recorded" rather than zero.
#'
#' A date is spelled ISO rather than left to `as.character()`, which follows
#' the session's locale. Whether one arrives typed at all depends on the
#' format the data came in: .xpt and .sas7bdat give a `Date`, .csv gives the
#' string that was in the file.
#' @noRd
.listing_value <- function(x) {
  if (length(x) == 0 || is.na(x)) return("")
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if (is.numeric(x)) return(format(x, trim = TRUE))
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
  record <- .fill_record(
    ref = NA_character_,
    row = template_row,
    col = NA_integer_,
    analysis_id = NA_character_
  )

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

  ## A listing is the one sheet where the same write runs tens of thousands of
  ## times, so it is done a COLUMN at a time rather than a cell at a time:
  ## everything fixed for a column is worked out once, and the cells go into
  ## `cc` in two vector operations at the end. Done cell by cell, growing `cc`
  ## by one row each time, a listing of a few thousand rows took minutes.
  ##
  ## Which of the template's cells are written, and to which data column. A
  ## template cell past the data's last column shows nothing and is left
  ## exactly as the author wrote it.
  cols <- vapply(template$c_r, .xlsx_col_to_num, integer(1), USE.NAMES = FALSE)
  shown <- which(!is.na(cols) & cols <= ncol(rows))
  cols <- cols[shown]
  targets <- seq.int(template_row, length.out = n)

  ## The text of every cell, a column at a time -- see `.cell_text_writer()`
  ## for why the column and not the cell is the unit of work here.
  cell_xml <- unlist(lapply(seq_along(shown), function(j) {
    write_cell <- .cell_text_writer(.cell_is_xml(wb, template, shown[[j]]))
    values <- rows[[cols[[j]]]]
    vapply(seq_len(n), function(i) write_cell(.listing_value(values[[i]])),
           character(1), USE.NAMES = FALSE)
  }), use.names = FALSE)

  ## Where each of those cells goes, in the same column-by-column order.
  cell_row <- rep(targets, times = length(shown))
  cell_col <- rep(cols, each = n)
  cell_letter <- rep(vapply(cols, .xlsx_num_to_col, character(1),
                            USE.NAMES = FALSE), each = n)
  cell_ref <- paste0(cell_letter, cell_row)
  cell_template <- rep(shown, each = n)

  ## The template row's own cells are already in `cc` and are rewritten where
  ## they stand. The rows the shift opened up have none, so theirs are cloned
  ## from the template -- that is how a new row inherits its style index --
  ## and added in one go, which is the whole of the speed-up.
  slot <- match(cell_ref, cc$r)
  known <- !is.na(slot)
  if (any(known)) cc <- .cell_set_is(cc, slot[known], cell_xml[known])
  if (any(!known)) {
    added <- template[cell_template[!known], , drop = FALSE]
    added <- .cell_set_is(added, seq_len(nrow(added)), cell_xml[!known])
    added$key   <- .cc_key(cell_row[!known], cell_col[!known])
    added$r     <- cell_ref[!known]
    added$row_r <- as.character(cell_row[!known])
    added$c_r   <- cell_letter[!known]
    rownames(added) <- NULL
    cc <- rbind(cc, added)
  }
  written <- length(cell_ref)

  ws$sheet_data$cc <- .cc_sorted(cc)
  ws <- .ensure_row_records(ws, targets)
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
  ## Normalised ONCE, then used for both arguments. Guarding only the pattern's
  ## input left `regmatches()` with a zero-length `x` against a length-one
  ## match, which errors -- so a figure sheet naming no series directive
  ## crashed the fill instead of simply having no series.
  ## `.figure_label_ref()` below already normalises both, which is why it never
  ## showed the same failure.
  value <- value %||% ""
  m <- regmatches(value, regexpr("[A-Za-z][A-Za-z0-9]*\\.[A-Za-z][A-Za-z0-9_]*",
                                 value))
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
  record <- .fill_record(
    ref = NA_character_,
    row = anchor,
    col = NA_integer_,
    analysis_id = NA_character_
  )

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
    ## A figure's filter never reaches the reporting event -- it is a fill-time
    ## directive on the sheet -- so there is no entity to mark and no finding to
    ## raise. The reservation happens here instead, and it is the same
    ## reservation either way: the series is not computed and the cell says why.
    ##
    ## Both outcomes must be caught. `NULL` means the directive stated no
    ## readable filter; an unresolved clause means it stated one this grammar
    ## could not read. Only the first was handled before, and an unresolved
    ## clause is NOT null -- left to fall through it would reach the evaluator
    ## as an object that is not a WhereClause at all.
    if (is.null(wc) || .is_unresolved_condition(wc)) {
      .diag_gap(
        stage = "fill_shell", severity = "WARN", input = INPUT_SHELL,
        problem = sprintf("Figure filter %s could not be parsed.",
                          dQuote(filter_txt, q = FALSE)),
        why = paste("The series is reserved rather than computed over",
                    "unfiltered data."),
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
#'   `skipped`, plus two frames: `census` -- one row per cell record, the
#'   FILLED cells included, with position, display-column label, owning
#'   analysis, status and (when unresolved) the reason -- and `findings`,
#'   the diagnostics this run itself raised (lost columns, declined
#'   expansions), which previously lived only in the session collector.
#'   Roll the census up with [ars_fill_summary()].
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

  ## Where the session's diagnostics stand before this run: everything past
  ## this mark is the fill's own, returned as `findings`.
  diag_baseline <- nrow(diag_records())

  spec <- if (is.character(ars)) {
    .require_file(ars, "ars", INPUT_ARS)
    .read_json(ars)
  } else {
    ars
  }
  outputs <- spec$outputs %||% list()
  analysis_by_id <- list()
  for (analysis in spec$analyses %||% list()) {
    id <- analysis$id %||% NA_character_
    if (!is.na(id) && nzchar(id)) analysis_by_id[[id]] <- analysis
  }
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

  ## Loading the workbook twice is the slow start of this stage, and it used
  ## to happen with nothing on screen. Announced so the bar has something to
  ## say before the first output is walked.
  .progress_tick(0L, 0L, "reading the workbook")
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
    ## The count is of outputs FINISHED, and the label is the one now being
    ## filled -- so the bar reaches 100% when the work is done rather than
    ## when the last output starts. The increment therefore comes after the
    ## tick, not before it.
    .progress_tick(n_walked, length(outputs),
                   as.character(output$id %||% "?"))
    n_walked <- n_walked + 1L
    fill <- output$`_meta`$shell_fill
    if (is.null(fill)) {
      ## Exiting silently here is how a run "does nothing" with no trace: the
      ## output simply never appears in the census. One skipped record keeps
      ## the accounting honest -- the caller sees the output was passed over
      ## and why, instead of inferring it from an absence.
      records[[length(records) + 1L]] <- .fill_record(
        output_id = output$id %||% NA_character_,
        sheet = NA_character_,
        ref = NA_character_,
        status = "skipped",
        reason = "no cell map recorded for this output"
      )
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
      records[[length(records) + 1L]] <- .fill_record(
        output_id = output$id %||% NA_character_,
        sheet = sheet,
        ref = NA_character_,
        status = "skipped",
        reason = sprintf(
          "the output names sheet '%s', which is not in the workbook",
          sheet %||% "?"
        )
      )
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
    ## An expansion's authored rows -- a nested pair, a categorical template
    ## block -- are not cells to substitute into: they are a pattern to
    ## expand. They are written by .fill_sheet_expansions() instead, and
    ## left out here so one row is never reported twice.
    expansion_rows <- .sheet_expansion_rows(fill)
    if (length(cells) > 0) {
      fixed <- Filter(function(cell) !cell$row %in% expansion_rows, cells)
      result <- .fill_table_sheet(wb, sheet_index, fixed, index,
                                  keep_pending_placeholders, sheet)
      ## The blocks expand AFTER the fixed cells are written: a shift moves
      ## the rows below it down, and a cell written before the shift travels
      ## with its row.
      result$records <- c(
        result$records,
        .fill_sheet_expansions(wb, sheet_index, fill, index, sheet)$records,
        ## The arm headers last: the denominator written there is read out of
        ## the same ARD the cells underneath it came from.
        .fill_header_n(wb, sheet_index, fill, index)$records)
      .check_column_coverage(output$id, fill$columns, result$records, sheet)
    } else if (!is.null(fill$listing)) {
      result <- .fill_listing_sheet(
        wb, sheet_index, fill$listing,
        .listing_rows(spec, output, adam_dir, sheet), keep_pending_placeholders,
        sheet)
    } else if (!is.null(fill$figure)) {
      result <- .fill_figure_sheet(wb, sheet_index, fill$figure, adam_dir, sheet)
    }
    ## The display label of each result column, so the census can say
    ## "column 'Low (N=XX)' lost every cell" instead of "column 4".
    label_by_col <- stats::setNames(
      vapply(fill$columns %||% list(), function(column) {
        as.character(column$label %||% NA_character_)
      }, character(1)),
      vapply(fill$columns %||% list(), function(column) {
        as.character(column$col)
      }, character(1)))
    for (record in (result$records %||% list())) {
      record$output_id <- as.character(output$id %||% NA_character_)
      record$sheet <- as.character(sheet)

      analysis_id <- record$analysis_id %||% NA_character_
      analysis <- if (!is.na(analysis_id)) analysis_by_id[[analysis_id]] else NULL
      if (!is.null(analysis)) {
        record$method_id <- as.character(
          analysis$methodId %||% NA_character_
        )
        if (is.na(record$row_label %||% NA_character_)) {
          record$row_label <- as.character(
            analysis$label %||% analysis$name %||% NA_character_
          )
        }
      }

      col <- record$col %||% NA_integer_
      if (length(col) == 1 && !is.na(col) &&
            as.character(col) %in% names(label_by_col)) {
        record$col_label <- label_by_col[[as.character(col)]]
      }
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

  ## Writing the workbook is the single slowest step of the fill on a real
  ## study, and it runs after the last output has been walked. Left silent it
  ## is what makes the bar look hung at 100%: every tick has fired, and the
  ## run still has its longest step to go.
  .progress_tick(length(outputs), length(outputs), "saving the workbook")
  .check_sheets_writable(wb, sheet_names)
  openxlsx2::wb_save(wb, output_path, overwrite = TRUE)

  status <- vapply(records, function(r) r$status, character(1))
  filled  <- sum(status %in% c("filled", "partial"))
  pending <- sum(status %in% c("pending", "partial", "missing_parent_key"))
  skipped <- sum(status %in% "skipped")

  ## The census keeps EVERY record, the filled ones included: a per-column
  ## fill rate needs its denominator, and "what filled" is as much a part of
  ## the debrief as what did not.
  census <- .fill_census(records)

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

  ## The fill stage's own findings (lost columns, declined expansions):
  ## previously these lived only in the session collector, which a CLI
  ## caller loses on exit. Sliced from the collector so only THIS run's
  ## rows travel.
  findings <- diag_records()
  findings <- findings[seq_len(nrow(findings)) > diag_baseline, ,
                       drop = FALSE]

  invisible(list(path = output_path, filled = filled, pending = pending,
                 skipped = skipped, census = census, findings = findings))
}
