## arsbridge -- shell_fill_meta.R
## ---------------------------------------------------------------------------
## The CELL MAP: which worksheet cell each computed number belongs in.
##
## An Excel shell is not only an input. Once the numbers exist, the same
## workbook is the natural place to put them -- the layout, the labels, the
## decimal conventions and the footnotes are already exactly as the study
## wants them. What is missing is the bridge from a result to a cell address,
## and that is what this file builds: `outputs[[i]]$_meta$shell_fill`.
##
## The map is emitted at BUILD time, not fill time, for one reason: this is
## the only moment when the shell's geometry and the analyses are both in
## hand. By the time `ars_fill_shell()` runs, the workbook is just a file and
## the ARD is just a table; without a recorded map, the writer would have to
## re-derive the join from labels, which is exactly the guesswork the shell
## annotations exist to avoid. ADR 0005 is the long form.
##
## WHAT A CELL RECORD CLAIMS, and what it does not:
##
##   claims  "the value at (row, col) is analysis A's operation O for the
##            column-axis group in position G, formatted to D decimals"
##   does not claim  that such a value exists in any particular ARD
##
## The second half matters. The map is written from the SHELL, so it says what
## the shell asked for; whether the ARD has it is decided at fill time, where
## a miss becomes a reported pending cell rather than a wrong number. That
## split is deliberate -- a map that quietly dropped the cells it could not
## satisfy would make an incomplete table look complete.
##
## `_meta` is arsbridge's own namespace and is stripped before conformance
## checking, so none of this affects the ARS the standard defines.

#' Attach the sheet row a section row came from to its layout entry, when the
#' reader recorded one (Excel does; Word has no cell addresses).
#' @noRd
.with_sheet_row <- function(entry, row) {
  sheet_row <- row$sheet_row %||% NULL
  if (!is.null(sheet_row) && !is.na(sheet_row)) {
    entry$sheet_row <- as.integer(sheet_row)
  }
  entry
}

## An ARS operation id to the {cards} stat_name the ARD will carry for it.
##
## The operation's *name* cannot be used for this: it is a display label and
## it differs between methods for the same statistic -- count-and-percentage
## calls them "Count" and "Percentage", AE frequency calls them "n" and
## "(%)", subject count calls it "n". The ID is stable across all of them,
## and the ARD joins on stat_name, so the id is what the mapping keys on.
## Getting this wrong does not fail loudly; it silently finds no ARD row and
## reports a filled cell as pending.
.OP_STAT_NAMES <- c(
  OP_N      = "n",
  OP_PCT    = "p",
  OP_DENOM  = "N",
  OP_MEAN   = "mean",
  OP_SD     = "sd",
  OP_MEDIAN = "median",
  OP_Q1     = "p25",
  OP_Q3     = "p75",
  OP_MIN    = "min",
  OP_MAX    = "max"
)

#' The ARD stat_name an operation will produce. Falls back to the operation's
#' own name lower-cased, which is right for the declarative methods whose
#' single operation is generated FROM a stat name (`OP_STAT`).
#' @noRd
.operation_stat_name <- function(op_id, op_name) {
  id <- as.character(op_id %||% "")
  if (nzchar(id) && id %in% names(.OP_STAT_NAMES)) {
    return(unname(.OP_STAT_NAMES[[id]]))
  }
  nm <- trimws(as.character(op_name %||% ""))
  if (!nzchar(nm)) return(NA_character_)
  tolower(nm)
}

#' The statistics one method emits, in the order its operations declare --
#' which is the order a compound placeholder's slots are read in.
#'
#' "xx (xx.x)" against a count-and-percentage method means slot 1 is the
#' count and slot 2 the percentage, because that is the order the method
#' lists them. A shell that wrote "(xx.x) xx" would be mapped the same way
#' and is wrong; that is a shell-authoring error the placeholder grammar
#' cannot see, and it is why the map records the token alongside the
#' operation so a reviewer can check the pairing.
#' @noRd
.method_operation_slots <- function(methods, method_id) {
  if (!nzchar(method_id %||% "")) return(list())
  hit <- Filter(function(m) identical(m$id, method_id), methods %||% list())
  if (length(hit) == 0) return(list())
  ops <- hit[[1]]$operations %||% list()
  ops <- ops[order(vapply(ops, function(o) o$order %||% 0L, integer(1)))]
  ## The denominator is a working quantity of the percentage, not a
  ## statistic the shell shows a cell for -- including it would shift every
  ## slot after it onto the wrong statistic.
  ops <- Filter(function(o) !identical(o$id, "OP_DENOM"), ops)
  lapply(ops, function(o) {
    list(operation_id = o$id %||% NA_character_,
         stat_name    = .operation_stat_name(o$id, o$name))
  })
}

## Which statistics a STATISTIC LINE asks for.
##
## A continuous block is authored as a parent row carrying the variable
## ("Age (years) [ADSL.AGE]") over unannotated lines that name the statistics
## ("Mean (SD)", "Median", "Q1, Q3"). Those lines have no analysis of their
## own -- they are the parent's results, one line each -- so a cell on one of
## them is filled from the parent, selected by what the line is called.
##
## The names on the right are {cards} stat_names, and the mapping is the
## inverse of `.statline_for()` in ars_to_tfrmt.R, which turns the same
## statistics back into the same line labels when rendering.
## test-shell_fill_meta.R asserts the two agree, so a change to one is caught
## rather than quietly splitting the round trip.
.STAT_LINE_STATS <- list(
  "n"          = "n",
  "n (%)"      = c("n", "p"),
  "mean"       = "mean",
  "mean (sd)"  = c("mean", "sd"),
  "sd"         = "sd",
  "median"     = "median",
  "min, max"   = c("min", "max"),
  "max, min"   = c("max", "min"),
  "range"      = c("min", "max"),
  "q1, q3"     = c("p25", "p75"),
  "q3, q1"     = c("p75", "p25")
)

#' The statistics a row label names, or NULL when the label is not a
#' statistic line at all (in which case it is a level of the parent).
#' @noRd
.stats_for_line <- function(label) {
  key <- tolower(trimws(gsub("[()]", " ", as.character(label %||% ""))))
  key <- trimws(gsub("\\s+", " ", key))
  ## "Mean (SD)" normalises to "mean sd"; look the label up both with its
  ## parentheses collapsed to a space and in its original punctuated form,
  ## so "n (%)" and "Q1, Q3" both resolve.
  direct <- tolower(trimws(as.character(label %||% "")))
  for (nm in names(.STAT_LINE_STATS)) {
    flat <- trimws(gsub("\\s+", " ", gsub("[()]", " ", nm)))
    if (identical(direct, nm) || identical(key, flat)) {
      return(.STAT_LINE_STATS[[nm]])
    }
  }
  NULL
}

#' The result columns of a table sheet: which sheet column shows which
#' position on the column axis, and what the shell labelled it.
#'
#' Column 1 is the stub. Everything to its right is a result column, in
#' order, and its position IS its binding to the column axis -- the shell
#' states the axis once ("[columns -> ADSL.TRT01A]") and then relies on
#' left-to-right order for the rest, exactly as a reader of the table does.
#'
#' The label is carried too, because at fill time the ARD's observed group
#' values have to be matched to these columns, and the label is the only
#' evidence the shell offers for which is which.
#' @noRd
.fill_result_columns <- function(section) {
  ## Two lessons from the first workbook that came back unfilled, both of
  ## which broke the value-to-column join silently:
  ##
  ## 1. Shells decorate arm headers with an "(N=XX)" placeholder, but the ARD
  ##    carries the bare group level ("Placebo", never "Placebo (N=XX)") --
  ##    the ARS side strips the decoration when it derives grouping levels,
  ##    so the fill side must strip it the same way or nothing ever matches.
  ## 2. The compacted `col_headers` loses physical positions: a header cell
  ##    whose only content was an annotation leaves a blank that compaction
  ##    drops, shifting every later column one to the left. `col_labels_full`
  ##    (one entry per physical column, blanks preserved) is the honest map.
  full <- as.character(section$col_labels_full %||% character())
  if (length(full) == 0) {
    ## Older sections carry only the compacted vector. Same treatment, minus
    ## the physical fidelity it cannot offer.
    full <- as.character(section$col_headers %||% character())
  }
  if (length(full) < 2L) return(list())

  columns <- list()
  for (j in seq_along(full)[-1]) {
    label <- .strip_n_placeholder(full[[j]])
    ## A blank label cannot be matched to an ARD group level, and an empty
    ## group_level fails .ard_value()'s nzchar gate and would match across
    ## every column. Leave the column unmapped; its cells stay pending under
    ## the column-axis reason instead of joining wrongly.
    if (!nzchar(label)) next
    ## order stays col - 1: it is the column's physical rank, and keeping the
    ## old arithmetic keeps existing _meta.shell_fill output byte-identical.
    columns[[length(columns) + 1L]] <-
      list(col = j, order = j - 1L, label = label)
  }
  columns
}

#' The cell map for one output.
#'
#' @param section The parsed section (needs the Excel class-3 fields:
#'   `sheet_name`, `layout`, `cell_grid`).
#' @param shell_layout The layout entries built alongside the analyses, each
#'   carrying `analysis_id` and -- for an Excel shell -- `sheet_row`.
#' @param analyses,methods The reporting event's analyses and methods, so an
#'   analysis can be resolved to the statistics it will produce.
#'
#' @return The `shell_fill` list, or NULL when the section did not come from
#'   a workbook (a Word shell has no cell addresses to fill).
#' @noRd
.build_shell_fill <- function(section, shell_layout, analyses, methods) {
  if (!identical(section$source_format %||% "", "xlsx")) return(NULL)
  grid <- section$cell_grid
  if (is.null(grid) || nrow(grid) == 0) return(NULL)

  layout <- section$layout %||% list()
  fill <- list(
    source = list(
      sheet          = section$sheet_name %||% NA_character_,
      format         = "xlsx",
      first_body_row = layout$first_body_row %||% NA_integer_,
      header_row     = layout$header_row %||% NA_integer_,
      ## Every header row, not just the last one: a column's "(N=XX)" sits
      ## wherever its arm label does, which in a stacked header is the row
      ## above the one the body columns are read from.
      header_rows    = as.integer(layout$header_rows %||% integer())
    ),
    columns = .fill_result_columns(section),
    cells   = list()
  )

  if (identical(section$tlf_type, "LISTING")) {
    fill$listing <- .build_listing_fill(section, layout)
    return(fill)
  }
  if (identical(section$tlf_type, "FIGURE")) {
    fill$figure <- .build_figure_fill(section)
    return(fill)
  }

  fill$cells  <- .build_table_cells(section, shell_layout, analyses, methods,
                                    fill$columns, grid)
  fill$nested <- .build_nested_fill(shell_layout)
  categorical <- .build_categorical_fills(section, shell_layout)
  if (length(categorical) > 0) {
    fill$categorical <- categorical
  }
  fill
}

#' The categorical expansion plan: one block per layout entry that recorded
#' template rows.
#'
#' Unlike the nested plan (one parent/child pair per sheet), categorical
#' blocks are plural -- a disposition table can hold several. Each block
#' names the rows the shell authored as its template and the analysis whose
#' levels replace them. The anchor is the first template row: it is both
#' where the first level lands and the row whose formatting the inserted
#' rows clone.
#'
#' A block whose recorded rows are not consecutive is not the authored
#' pattern (most likely two blocks on the same variable collapsed into one
#' entry); expanding it would write levels over unrelated rows, so it is
#' reported and skipped instead.
#' @noRd
.build_categorical_fills <- function(section, shell_layout) {
  blocks <- list()
  for (entry in shell_layout %||% list()) {
    rows <- as.integer(unlist(entry$template_rows %||% integer()))
    if (length(rows) == 0) next
    if (is.na(entry$analysis_id %||% NA_character_)) next
    rows <- sort(unique(rows))
    if (length(rows) > 1 && any(diff(rows) != 1L)) {
      .diag_gap(
        stage = "build_ars", severity = "WARN", input = INPUT_SHELL,
        problem = sprintf(
          "The template rows of '%s' are not contiguous (%s), so the block cannot be expanded.",
          entry$label %||% "?", paste(rows, collapse = ", ")),
        why = "Expanding a broken run would write level rows over unrelated rows between the fragments.",
        fix = "Keep a block's mock rows together under one header, or author the blocks on distinct variables/subsets.",
        tlf_number = section$tlf_number,
        location = section$sheet_name %||% "")
      next
    }
    blocks[[length(blocks) + 1L]] <- list(
      anchor_row    = rows[1],
      template_rows = rows,
      analysis_id   = entry$analysis_id,
      label         = entry$label %||% "",
      sort          = entry$sort %||% NA_character_,
      self_template = isTRUE(entry$self_template)
    )
  }
  blocks
}

#' What a sheet row's cells should be filled from.
#'
#' Three shapes of row carry results, and they select from the ARD
#' differently. Getting this wrong is how a number lands in the wrong cell,
#' so each is decided explicitly rather than by fallthrough:
#'
#'   own       the row has its own analysis ("Any TEAE [ADAE.TRTEMFL='Y']").
#'             Its statistics are whatever its method declares, in order.
#'   stat_line the row is an unannotated statistic line under a parent that
#'             does ("Mean (SD)" under "Age (years) [ADSL.AGE]"). Its
#'             statistics are the ones its LABEL names -- not the method's
#'             full list, because each line shows a different subset of it.
#'   level     the row is an unannotated level under a categorical parent
#'             ("Female" under "Sex, n (%) [ADSL.SEX]"). It selects one value
#'             of the parent's variable and shows the method's statistics for
#'             that value.
#'
#' @return list(analysis_id, stats, variable_level, stat_line) or NULL when
#'   the row has nothing to fill from.
#' @noRd
.fill_row_binding <- function(entry, parent, methods, analyses) {
  by_id <- function(id) {
    hit <- Filter(function(a) identical(a$id, id), analyses %||% list())
    if (length(hit) == 0) NULL else hit[[1]]
  }
  method_stats <- function(analysis) {
    .method_operation_slots(methods, analysis$methodId %||% "")
  }

  ## The row's own analysis.
  own_id <- entry$analysis_id %||% NA_character_
  if (!is.na(own_id)) {
    analysis <- by_id(own_id)
    if (is.null(analysis)) return(NULL)
    binding <- list(analysis_id = own_id, analysis = analysis,
                    stats = method_stats(analysis))
    ## A level row recorded as such by the builder already knows its value.
    if (identical(entry$kind, "level")) {
      binding$variable_level <- entry$level %||% NA_character_
    }
    return(binding)
  }

  ## No analysis of its own: it belongs to the block above it.
  if (is.null(parent)) return(NULL)
  analysis <- by_id(parent$analysis_id)
  if (is.null(analysis)) return(NULL)

  named <- .stats_for_line(entry$label)
  if (!is.null(named)) {
    all_stats <- method_stats(analysis)
    by_name <- stats::setNames(all_stats,
                               vapply(all_stats, function(s) s$stat_name,
                                      character(1)))
    ## Keep the order the LINE states, and carry a stat the method did not
    ## declare rather than dropping it -- the shell asked for it, and the
    ## fill writer reporting it as pending is more use than its silent
    ## absence.
    stats_out <- lapply(named, function(sn) {
      by_name[[sn]] %||% list(operation_id = NA_character_, stat_name = sn)
    })
    return(list(analysis_id = parent$analysis_id, analysis = analysis,
                stats = stats_out, stat_line = entry$label))
  }

  ## Not a statistic line, so it is a level of the parent's variable.
  list(analysis_id = parent$analysis_id, analysis = analysis,
       stats = method_stats(analysis),
       variable_level = entry$label %||% NA_character_)
}

#' One record per body cell of a table sheet.
#' @noRd
.build_table_cells <- function(section, shell_layout, analyses, methods,
                               columns, grid) {
  ## Only the layout entries that came from a sheet row can address a cell.
  entries <- Filter(function(e) !is.null(e$sheet_row), shell_layout %||% list())
  entries <- entries[order(vapply(entries, function(e) e$sheet_row, integer(1)))]

  ## Each row's binding, resolved once, walking down the sheet so every row
  ## knows the analysis block it sits in.
  bindings <- list()
  parent <- NULL
  for (entry in entries) {
    if (!is.na(entry$analysis_id %||% NA_character_) &&
        !identical(entry$kind, "level")) {
      parent <- list(analysis_id = entry$analysis_id, label = entry$label)
    }
    bindings[[as.character(entry$sheet_row)]] <-
      .fill_row_binding(entry, parent, methods, analyses)
  }

  col_by_index <- stats::setNames(columns,
                                  vapply(columns, function(c) as.character(c$col),
                                         character(1)))

  ## Sheet rows a categorical parent recorded as its expansion template
  ## ("<Reason #1>" mock rows): their cells bind to the OWNING analysis so
  ## the fill step has slots to expand from, and each carries a template
  ## flag plus the reason it may still be on placeholder -- an orphaned row
  ## and a row awaiting expansion are different problems.
  template_owner <- list()
  for (e in entries) {
    for (r in e$template_rows %||% integer()) {
      template_owner[[as.character(r)]] <- e
    }
  }

  cells <- list()
  ## Rows whose placeholder asks for more statistics than the analysis
  ## produces -- collected here and reported once per row after the walk.
  unbound_rows <- list()
  for (i in seq_len(nrow(grid))) {
    ## A literal is a label, a footnote, or a number the author typed. The
    ## fill writer must leave every one of them exactly as authored.
    if (!grid$kind[[i]] %in% c("placeholder", "template")) next

    binding <- bindings[[as.character(grid$row[[i]])]]
    column  <- col_by_index[[as.character(grid$col[[i]])]]

    ## A template row without a binding of its own (the convention shape's
    ## bare mocks never reach the layout) borrows the owning entry's
    ## analysis, so its cells carry bound slots into the fill plan.
    owner <- template_owner[[as.character(grid$row[[i]])]]
    if (is.null(binding) && !is.null(owner) && !is.null(column)) {
      analysis <- Filter(function(a) identical(a$id, owner$analysis_id),
                         analyses %||% list())
      if (length(analysis) > 0) {
        analysis <- analysis[[1]]
        binding <- list(
          analysis_id = owner$analysis_id,
          analysis    = analysis,
          stats       = .method_operation_slots(methods,
                                                analysis$methodId %||% "")
        )
      }
    }

    record <- list(
      row         = grid$row[[i]],
      col         = grid$col[[i]],
      ref         = grid$ref[[i]],
      placeholder = paste(vapply(grid$slots[[i]], function(s) s$token,
                                 character(1)), collapse = " "),
      kind        = "pending"
    )

    ## Pending, and WHY: "no analysis produces this row" and "this column is
    ## not on the column axis" are different problems for whoever opens the
    ## filled workbook, and neither is the same as "the number was not
    ## computed".
    if (is.null(binding) || is.null(column)) {
      record$reason <- if (!is.null(owner)) {
        "a template row of the categorical block above -- awaiting row expansion"
      } else if (is.null(column)) {
        "the column is not on the output's column axis"
      } else {
        "no analysis covers this row"
      }
      cells[[length(cells) + 1L]] <- record
      next
    }

    if (!is.null(owner)) {
      record$template <- TRUE
      record$reason   <-
        "a template row of the categorical block above -- awaiting row expansion"
    }
    record$kind        <- "result"
    record$analysis_id <- binding$analysis_id
    record$group       <- list(
      grouping_id = .fill_grouping_id(binding$analysis),
      order       = column$order,
      label       = column$label
    )
    if (!is.null(binding$variable_level)) {
      record$variable_level <- binding$variable_level
    }
    if (!is.null(binding$stat_line)) {
      record$stat_line <- binding$stat_line
    }
    record$slots <- .bind_slots(grid$slots[[i]], binding$stats)
    if (length(record$slots) == 0) {
      record$kind   <- "pending"
      record$reason <- "the method declares no statistic for this placeholder"
    }
    unbound <- sum(vapply(record$slots, function(s) is.na(s$stat_name),
                          logical(1)))
    if (unbound > 0) {
      unbound_rows[[as.character(grid$row[[i]])]] <- list(
        row = grid$row[[i]], placeholder = record$placeholder,
        n_stats = length(binding$stats))
    }
    cells[[length(cells) + 1L]] <- record
  }

  ## A placeholder that asks for two numbers where the analysis produces one
  ## is the shell and the analysis typing disagreeing about what the row IS
  ## -- most often a count-and-percentage row typed as a plain count. The
  ## cell is still mapped, with the surplus slot unbound, so the filled
  ## workbook shows the gap rather than a confident wrong number.
  for (u in unbound_rows) {
    .diag_gap(
      stage = "build_ars", severity = "WARN", input = INPUT_SHELL,
      problem = sprintf(
        "Row %d of %s shows %s but its analysis produces %d statistic%s.",
        u$row, section$sheet_name %||% section$tlf_number %||% "?",
        dQuote(u$placeholder, q = FALSE), u$n_stats,
        if (u$n_stats == 1L) "" else "s"),
      why = "The extra placeholder cannot be filled, so the cell will be written incomplete.",
      fix = paste("Either simplify the placeholder, or annotate the row so it",
                  "is read as the statistic it shows (e.g. a count with a",
                  "percentage)."),
      tlf_number = section$tlf_number, location = section$sheet_name %||% "")
  }
  cells
}

#' The column-axis grouping an analysis reports its results by.
#' @noRd
.fill_grouping_id <- function(analysis) {
  groupings <- analysis$orderedGroupings %||% list()
  by_group <- Filter(function(g) isTRUE(g$resultsByGroup), groupings)
  if (length(by_group) == 0) return(NA_character_)
  by_group[[1]]$groupingId %||% NA_character_
}

#' Pair each placeholder token with the statistic it stands for.
#'
#' The pairing is positional -- token 1 to operation 1 -- and the decimals
#' come from the token itself, which is the whole reason a filled shell needs
#' no separate format declaration. A placeholder with more tokens than the
#' method has statistics binds what it can and leaves the rest unbound, so
#' the extra token is visible in the map rather than silently formatted with
#' the wrong number.
#' @noRd
.bind_slots <- function(tokens, operations) {
  if (length(tokens) == 0 || length(operations) == 0) return(list())
  out <- list()
  for (i in seq_along(tokens)) {
    tok <- tokens[[i]]
    op  <- if (i <= length(operations)) operations[[i]] else NULL
    out[[length(out) + 1L]] <- list(
      order        = i,
      token        = tok$token,
      type         = tok$type,
      decimals     = tok$decimals,
      start        = tok$start,
      stop         = tok$stop,
      operation_id = if (is.null(op)) NA_character_ else op$operation_id,
      stat_name    = if (is.null(op)) NA_character_ else op$stat_name
    )
  }
  out
}

#' A nested block's fill plan: the two authored token rows that stand for a
#' whole SOC-and-its-PTs presentation.
#'
#' The shell writes the block as a pair -- `<System Organ Class>` over
#' `<Preferred Term>` -- and means "repeat this per system organ class". The
#' rows are template examples, not rows: filling them means expanding the
#' pair into one line per level and per term underneath it, which is a change
#' of shape, so it is planned separately from the fixed cells.
#'
#' Only a pair whose rows are adjacent is planned. A parent and child with
#' anything authored between them is not the pattern this expands (the block
#' inserts as one run of rows), and the cells stay pending with their reason.
#'
#' @return list(parent, child) or NULL when the sheet has no nested block.
#' @noRd
.build_nested_fill <- function(shell_layout) {
  entries <- Filter(function(e) !is.null(e$sheet_row), shell_layout %||% list())
  parents <- Filter(function(e) identical(e$kind, "nested_parent"), entries)
  if (length(parents) == 0) return(NULL)

  for (parent in parents) {
    kids <- Filter(function(e) {
      identical(e$kind, "nested_child") &&
        identical(e$parent_order %||% NA_integer_, parent$order)
    }, entries)
    if (length(kids) != 1) next
    child <- kids[[1]]
    if (!identical(as.integer(child$sheet_row), as.integer(parent$sheet_row) + 1L)) next

    return(list(
      parent = list(row         = as.integer(parent$sheet_row),
                    analysis_id = parent$analysis_id %||% NA_character_,
                    label       = parent$label %||% ""),
      child  = list(row         = as.integer(child$sheet_row),
                    analysis_id = child$analysis_id %||% NA_character_,
                    label       = child$label %||% ""),
      ## The authored "sort:" clause travels on the parent row, and the fill
      ## writer orders levels with it through the same function the renderer
      ## uses -- see .nested_level_order().
      sort = parent$sort %||% NA_character_))
  }
  NULL
}

#' A listing's fill plan: where the header is, which row is the template to
#' repeat, and what each column displays. There are no per-cell records --
#' a listing is filled by expanding one row into N, not by substituting
#' statistics into fixed cells.
#' @noRd
.build_listing_fill <- function(section, layout) {
  header_rows <- section$header_rows %||% list()
  list(
    header_row   = layout$header_row %||% NA_integer_,
    template_row = section$template_row %||% NA_integer_,
    footnote_row = .fill_footnote_row(section, layout),
    columns = lapply(seq_along(header_rows), function(i) {
      h <- header_rows[[i]]
      list(col        = i,
           label      = h$label %||% "",
           annotation = h$annotation %||% "")
    })
  )
}

#' A figure's fill plan: where the annotation block starts (the computed
#' series data replaces it) and which aspects the shell declared.
#' @noRd
.build_figure_fill <- function(section) {
  spec <- section$figure_spec %||% list()
  list(
    series_anchor = spec$anchor_row %||% NA_integer_,
    directives = lapply(names(spec$directives %||% list()), function(k) {
      list(key = k, value = spec$directives[[k]]$value %||% "")
    })
  )
}

#' The sheet row carrying the footnote, when there is one -- the fill writer
#' has to move it down when a listing expands.
#' @noRd
.fill_footnote_row <- function(section, layout) {
  last <- layout$last_row %||% NA_integer_
  if (is.na(last) || length(section$footnotes %||% character()) == 0) {
    return(NA_integer_)
  }
  last
}
