## arsbridge -- ars_to_ard.R
## ---------------------------------------------------------------------------
## Executes CDISC ARS JSON specifications directly using the {cards} package.
## Dynamically loads ADaM datasets and binds results into a single tidy ARD.

## Method-id -> native {cards} executor registry. Keys MUST be ids from
## .STANDARD_METHODS (build_ars_json.R) -- a consistency test enforces
## this. Methods absent here (e.g. MTH_KAPLAN_MEIER_ESTIMATE) run through
## the generic fallback summarizer, which is recorded in the ARD's
## method_actual column and as a diagnostic.
## Each executor: function(df, var, by, denom, subject_key) -> card.
.ARD_EXECUTORS <- list(
  MTH_SUMMARY_STATISTICS_CONTINUOUS = function(df, var, by, denom, subject_key) {
    if (!is.numeric(df[[var]])) {
      df[[var]] <- as.numeric(df[[var]])
    }
    cards::ard_continuous(
      data = df,
      variables = all_of(var),
      by = all_of(by)
    )
  },
  MTH_COUNT_AND_PERCENTAGE = function(df, var, by, denom, subject_key) {
    cards::ard_categorical(
      data = df,
      variables = all_of(var),
      by = all_of(by),
      denominator = denom
    )
  },
  MTH_AE_FREQUENCY_COUNT = function(df, var, by, denom, subject_key) {
    ## Distinct per subject WITHIN each by-cell, not just per term: a nested
    ## analysis is keyed by (arm, parent level, term), and dirty data can
    ## repeat one term string under two parents -- a subject must count once
    ## in EACH cell it appears in, never be swallowed by another cell's copy.
    df_unique <- df |>
      dplyr::distinct(
        dplyr::across(dplyr::all_of(c(subject_key, by, var))),
        .keep_all = TRUE
      )
    cards::ard_categorical(
      data = df_unique,
      variables = all_of(var),
      by = all_of(by),
      denominator = denom
    )
  },
  MTH_SUBJECT_COUNT = function(df, var, by, denom, subject_key) {
    ## Counting the subject key itself is one row per subject, so the by
    ## variables are all that join it. Counting anything else means an
    ## occurrence frame -- one subject, several rows, a different term on
    ## each -- and there the term has to join the distinct as well, exactly
    ## as the emitted block does (see .analysis_block()). Deduplicating on
    ## the subject alone would keep only whichever row happened to come
    ## first and report every one of that subject's other terms as zero.
    distinct_vars <- if (identical(var, subject_key)) {
      unique(c(subject_key, by))
    } else {
      unique(c(subject_key, by, var))
    }
    df_unique <- df |>
      dplyr::distinct(
        dplyr::across(dplyr::all_of(distinct_vars)),
        .keep_all = TRUE
      )
    if (identical(var, subject_key) && length(by)) {
      df_unique[[subject_key]] <- subject_key
      cards::ard_categorical(
        data = df_unique,
        variables = all_of(subject_key),
        by = all_of(by),
        denominator = denom
      )
    } else if (identical(var, subject_key)) {
      cards::ard_total_n(df_unique)
    } else {
      cards::ard_categorical(
        data = df_unique,
        variables = all_of(var),
        by = all_of(by),
        denominator = denom
      )
    }
  }
)

## Subject Count with the percentage declared as well. Grouped execution is the
## same distinct-subject calculation as Subject Count. An ungrouped subject-key
## pass must use a constant categorical level, however, because ard_total_n()
## emits only N while this method declares n, N, and p.
.ARD_EXECUTORS$MTH_SUBJECT_COUNT_PCT <- function(df, var, by, denom,
                                                  subject_key) {
  if (!identical(var, subject_key) || length(by)) {
    return(.ARD_EXECUTORS$MTH_SUBJECT_COUNT(
      df, var, by, denom, subject_key
    ))
  }

  df_unique <- df |>
    dplyr::distinct(
      dplyr::across(dplyr::all_of(subject_key)),
      .keep_all = TRUE
    )
  df_unique[[subject_key]] <- subject_key
  cards::ard_categorical(
    data = df_unique,
    variables = all_of(subject_key),
    denominator = denom
  )
}

## ---------------------------------------------------------------------------
## Declared-but-unexecutable methods (ADR 0001 descriptor seeds / ADR 0002).
## These statistics are describable in the ARS but have no {cards}/{cardx}
## executor yet, so the engine cannot compute them. Rather than skip the
## analysis (which orphans the table cell) or coerce it into a meaningless
## count, ars_to_ard() emits a reserved, fully-keyed stub ARD row per declared
## statistic with result_status = "manual_pending" and stat = NA. A validated
## manual computation later fills that keyed slot (ADR 0002 phases 3-5), keeping
## the Output -> Analysis -> Method -> result chain intact.
##
## Each entry: stats = the stat_names the method declares; by_group = whether
## each statistic is reported per group level (e.g. an exact CI per arm) or once
## for the analysis (e.g. a single CMH p-value, or a between-group difference).
.UNEXECUTABLE_METHODS <- list(
  ## Generic declarative method the spec generator assigns to a
  ## capability-gated section (ADR 0002 phase 3) when the specific statistic is
  ## not yet classified. Reserves one manual_pending cell per analysis row.
  MTH_UNSUPPORTED_ANALYSIS     = list(stats = "result", by_group = FALSE),
  ## Specific declarative methods (ADR 0001 descriptor seeds) -- assigned once
  ## the shell reader classifies the exact statistic.
  MTH_CMH_TEST                 = list(stats = "p.value", by_group = FALSE),
  MTH_PROPORTION_CI_EXACT      = list(stats = c("conf.low", "conf.high"),
                                      by_group = TRUE),
  MTH_PROPORTION_DIFF_NEWCOMBE = list(stats = c("estimate", "conf.low",
                                                "conf.high"), by_group = FALSE)
)

## Methods that DO have an executor emitted by .emit_block, and so are computed
## -- not reserved -- when their prerequisites are met. Each entry: the
## `value_source` to stamp on the result, and an `available(res)` predicate that
## decides, per analysis, whether the method can actually run. A method stays
## listed in .UNEXECUTABLE_METHODS above, so when `available()` is FALSE (a
## missing package, or an operand the spec did not carry) the engine degrades
## gracefully and reserves a manual_pending stub instead of erroring.
##   * Exact (Clopper-Pearson) CI -- via {cardx}; needs only response + grouping.
##   * CMH p-value -- via base R (ard_cmh_test); needs a resolved strata operand.
## Newcombe stays reserve-only until its reference-group operand is carried.
.EXEC_DESCRIPTORS <- list(
  MTH_PROPORTION_CI_EXACT = list(
    value_source = "cardx",
    available = function(res) requireNamespace("cardx", quietly = TRUE)),
  MTH_CMH_TEST = list(
    value_source = "stats",
    available = function(res) {
      s <- res$strata
      !is.null(s) && length(s) == 1L && nzchar(s)
    })
)

## A one-row {cards} card used as a schema prototype, so stub rows carry exactly
## the columns of the installed {cards} version (list-cols and all) instead of a
## hand-coded guess that could drift across versions. Built with a `by` so the
## group1 / group1_level columns are present.
#' @noRd
.ard_schema_proto <- function() {
  cards::ard_categorical(
    data.frame(.v = factor("a"), .g = factor("x")),
    variables = ".v", by = ".g")[1, , drop = FALSE]
}

## Make one column type consistent across the per-analysis ARDs before they
## are bound.
##
## Two executor paths build cards in different ways. The {cards} path returns
## the package's own shape, in which the `*_level` columns are LIST columns --
## a level can be any type, so cards boxes it. The declared-path executor
## builds its frame directly and leaves those columns atomic. Neither is
## wrong on its own, but a study using both -- a plain by-treatment table and
## one whose column axis is a declared cross of two groupings -- produces a
## list column in one card and a character column in the other, and binding
## them fails outright with "Can't combine <list> and <character>". The whole
## ARD is lost, for a study that is perfectly well formed.
##
## So any column that is a list ANYWHERE becomes a list EVERYWHERE, which is
## the cards convention and the wider of the two shapes. Columns already
## agreeing are untouched.
#' @noRd
.align_ard_columns <- function(ard_list) {
  if (length(ard_list) < 2) return(ard_list)

  listed <- unique(unlist(lapply(ard_list, function(ard) {
    names(ard)[vapply(ard, is.list, logical(1))]
  })))
  if (length(listed) == 0) return(ard_list)

  lapply(ard_list, function(ard) {
    for (col in intersect(listed, names(ard))) {
      if (!is.list(ard[[col]])) ard[[col]] <- as.list(ard[[col]])
    }
    ard
  })
}

## Methods whose result is a COUNT, so a group with no qualifying records has
## an answer -- zero -- rather than no answer. A continuous summary is never
## completed this way: the mean of nothing is not 0, it is nothing.
.COUNTING_METHODS <- c("MTH_SUBJECT_COUNT", "MTH_SUBJECT_COUNT_PCT",
                       "MTH_COUNT_AND_PERCENTAGE", "MTH_AE_FREQUENCY_COUNT")

#' Give every treatment group a row, including the ones the filter emptied.
#'
#' An analysis's data subset is applied BEFORE the executor runs, so an arm
#' with no qualifying records is not in the frame {cards} is handed, and
#' `ard_categorical()` can only report the `by` levels it is given. The arm
#' then has no row in the ARD at all, and everything downstream -- which
#' trusts the ARD -- shows nothing: a filled shell leaves the cell reading
#' "xx (xx.x)", and a blank in a clinical table reads as missing when the
#' answer is zero. No Placebo subject had a serious adverse event is a
#' finding; "we did not look" is not.
#'
#' Closed here rather than at fill time so every consumer -- the fill writer,
#' the renderer, an exported ARD -- gets the same complete answer. For each
#' group the denominator population has and the results do not mention, one
#' row per statistic the other groups report: `n` = 0, `p` = 0, and `N` = that
#' group's own denominator, counted the same way the executor counts it.
#'
#' Left alone, deliberately: a crossed column axis (two grouping variables,
#' where "the missing group" is not a single level), a grouping variable the
#' denominator population does not carry (nothing to count a denominator
#' from), and every method that is not counting anything.
#'
#' @param group_defs The annotation-defined column groups for `by_arg`, when
#'   the shell declared its columns by condition rather than by the variable's
#'   own values. The groups are then the LABELS those conditions carry -- the
#'   population column still holds the raw values, and completing from those
#'   would invent a column per code.
#' @return The ARD, with the missing groups' rows appended.
#' @noRd
.complete_zero_groups <- function(ard, by_arg, df_population, subject_key,
                                  group_defs = NULL) {
  if (is.null(ard) || nrow(ard) == 0) return(ard)
  if (length(by_arg) != 1L) return(ard)
  if (!all(c("group1_level", "stat_name", "stat") %in% names(ard))) return(ard)
  if (is.null(df_population) || !by_arg %in% names(df_population)) return(ard)

  defs <- group_defs[[by_arg]]
  in_pop <- as.character(df_population[[by_arg]])
  if (length(defs) > 0) {
    expected <- vapply(defs, function(d) as.character(d$label %||% ""),
                       character(1))
    expected <- unique(expected[nzchar(expected)])
    ## Each declared column is its own condition over the population.
    in_group <- function(level) {
      hit <- Filter(function(d) identical(as.character(d$label %||% ""), level),
                    defs)
      if (length(hit) == 0) return(rep(FALSE, length(in_pop)))
      isTRUE_vec <- .eval_where_clause(df_population, hit[[1]]$condition)
      !is.na(isTRUE_vec) & isTRUE_vec
    }
  } else {
    expected <- unique(in_pop[!is.na(in_pop) & nzchar(in_pop)])
    in_group <- function(level) !is.na(in_pop) & in_pop == level
  }
  reported <- .ard_chr(ard[["group1_level"]])
  present  <- unique(reported[!is.na(reported)])
  missing  <- setdiff(expected, present)
  if (length(missing) == 0 || length(present) == 0) return(ard)

  ## One reported group's rows are the shape every other group takes: the same
  ## statistics over the same variable levels, formatted the same way. Only
  ## the three statistics whose zero-value is knowable are carried over --
  ## anything else the method emits is left absent rather than invented.
  same_group <- !is.na(reported) & reported == present[[1]]
  template <- ard[same_group & ard[["stat_name"]] %in% c("n", "N", "p"), ,
                  drop = FALSE]
  if (nrow(template) == 0) return(ard)

  denominator <- function(level) {
    rows <- in_group(level)
    if (!is.null(subject_key) && subject_key %in% names(df_population)) {
      length(unique(df_population[[subject_key]][rows]))
    } else {
      sum(rows)
    }
  }

  as_column <- function(values, like) if (is.list(like)) as.list(values) else values

  added <- lapply(missing, function(level) {
    rows <- template
    rows[["group1_level"]] <- as_column(rep(level, nrow(rows)),
                                        ard[["group1_level"]])
    values <- ifelse(rows[["stat_name"]] == "N", denominator(level), 0)
    rows[["stat"]] <- as_column(values, ard[["stat"]])
    ## The template group's warnings and errors are its own.
    for (col in intersect(c("warning", "error"), names(rows))) {
      rows[[col]] <- rep(list(NULL), nrow(rows))
    }
    rows
  })

  out <- dplyr::bind_rows(c(list(ard), added))
  if (inherits(ard, "card") && !inherits(out, "card")) {
    class(out) <- union("card", class(out))
  }
  out
}

#' Carry the column variable onto the denominator frame, by subject.
#'
#' Percentages are computed against a population frame -- ADSL under the
#' analysis set's filter -- and {cards} splits that frame by the same `by`
#' variable the analysis uses. When the frame does not HAVE that variable,
#' cards says nothing and uses the whole frame for every column:
#'
#'   ard_categorical(cm, variables = "CMCLAS", by = "TRTA", denominator = adsl)
#'   #  A: n=2 N=10 p=0.2      <- N is the whole study, both columns
#'   #  B: n=1 N=10 p=0.1
#'
#' Every percentage in the table is then out of the study rather than out of
#' its own arm, and nothing in the deliverable says so. It is the ordinary
#' shape of a conmeds or medical-history table: those domains carry `TRTA`,
#' and ADSL carries `TRT01A`.
#'
#' Only the COLUMN axis is repaired -- `by_arg[[1]]`, the first ordered
#' grouping, which `.build_analysis()` guarantees is a column grouping and
#' `.ard_index()` already reads as the column. A row grouping (the parent
#' level of a nested block) is appended after it and must NOT key the
#' denominator: a preferred term's percentage is out of its arm, not out of
#' its system organ class.
#'
#' The variable is subject-level, so it can be learned from the domain and
#' joined on: one row per subject, from the domain under the POPULATION filter
#' only -- a data subset selects events, not subjects, and must never shrink a
#' denominator.
#'
#' Two things are refused rather than guessed:
#'   * a subject carrying two different values -- that is a data problem, and
#'     averaging it away would silently pick one arm for them;
#'   * a population subject with no record in the domain -- their column is
#'     unknowable from the domain, and inventing a treatment mapping ("TRTA
#'     must be TRT01A") is exactly the silent wrongness this exists to
#'     prevent. Joining anyway would swap one wrong denominator for another:
#'     each column's N would become "subjects with a record in this domain",
#'     which is neither the population nor the study. So the join is refused
#'     whole, the numbers are left as they were, and the diagnostic says what
#'     the percentages ARE out of and how to fix the shell.
#'
#' @return list(data = the population frame with the variable joined on where
#'   it could be, joined = the variables that were).
#' @noRd
.denominator_by_subject <- function(df_population, df_domain, by_arg,
                                    subject_key, analysis_id = NA_character_,
                                    analysis_ds = NA_character_) {
  none <- list(data = df_population, joined = character(), block = NULL)
  blocked <- function(reason, detail) {
    list(data = df_population, joined = character(),
         block = .block_signal(reason, detail))
  }
  if (is.null(df_population) || is.null(df_domain)) return(none)
  if (length(by_arg %||% character()) == 0) return(none)

  ## POPULATION-FIRST. When the denominator frame already carries the grouping
  ## variable it is authoritative, whatever the grouping metadata names as its
  ## dataset. A treatment variable copied onto an event domain must not decide
  ## the denominator: subjects with no event would drop out of it, and the
  ## denominator is the POPULATION, not the subjects some domain happens to
  ## know about.
  ## by_arg[[1]] is the COLUMN axis and is one variable name; a row grouping
  ## is appended after it and must not key the denominator.
  missing_vars <- setdiff(by_arg[[1]], names(df_population))
  if (length(missing_vars) == 0) {
    ## Both frames carry the variable. The NUMERATOR is grouped by the
    ## analysis frame's copy and the DENOMINATOR by the population's, so if
    ## the two disagree for a subject that subject is counted under one group
    ## and measured against another -- which is how an analysis comes to
    ## report n = 3 against N = 2. There is no safe reading of that.
    ##
    ## Subjects the domain does not know are fine: the population answers for
    ## them and they stay in their own column's denominator.
    disagree <- .grouping_value_disagreement(df_population, df_domain,
                                             by_arg[[1]], subject_key)
    if (!is.null(disagree)) return(blocked(.BLOCK_GROUPING_DISAGREE, disagree))
    return(none)
  }

  ## Past here the population does NOT have it, so the foreign dataset is the
  ## only source of group membership -- and every requirement below follows
  ## from that, because a subject whose group cannot be resolved cannot be
  ## counted in any group's denominator.
  if (!subject_key %in% names(df_population) ||
      !subject_key %in% names(df_domain)) {
    return(blocked(
      .BLOCK_MISSING_SUBJECT_KEY,
      sprintf("%s is needed to carry %s from %s and is not on both frames",
              subject_key, paste(missing_vars, collapse = ", "),
              analysis_ds %||% "the analysis dataset")))
  }

  joined <- character()
  for (var in missing_vars) {
    if (!var %in% names(df_domain)) next   # the analysis itself runs ungrouped

    map <- unique(df_domain[!is.na(df_domain[[var]]),
                            c(subject_key, var), drop = FALSE])
    per_subject <- table(map[[subject_key]])
    if (any(per_subject > 1)) {
      ## Two different values for one subject: there is no fact of the matter
      ## about which group they belong to, and assigning either would invent
      ## one. This used to warn and leave N as the whole population.
      return(blocked(
        .BLOCK_DENOM_AMBIGUOUS,
        sprintf("%d subject(s) carry more than one %s in %s",
                sum(per_subject > 1), var,
                analysis_ds %||% "the analysis dataset")))
    }

    with_var <- merge(df_population, map, by = subject_key, all.x = TRUE,
                      sort = FALSE)
    unmapped <- sum(is.na(with_var[[var]]))
    if (unmapped > 0) {
      ## The population frame cannot supply this variable and the domain does
      ## not know these subjects, so their group membership is unknown and any
      ## per-group N would be partly invented. NOT the same as a subject
      ## missing from the domain when the POPULATION has the variable -- there
      ## the population answers and the subject stays in its own denominator.
      return(blocked(
        .BLOCK_DENOM_UNRESOLVED,
        sprintf("%d population subject(s) have no resolvable %s value in %s",
                unmapped, var, analysis_ds %||% "the analysis dataset")))
    } else {
      diag_add(
        stage = "execute_ard", severity = "INFO",
        problem = sprintf("Denominator keyed by %s, taken from %s per subject",
                          var, analysis_ds %||% "the analysis dataset"),
        location = analysis_id %||% "",
        action = "Percentages are out of each column's own population")
    }
    df_population <- with_var
    joined <- c(joined, var)
  }
  list(data = df_population, joined = joined, block = NULL)
}

## Build one keyed, value-less stub ARD row from a prototype row.
#' @noRd
.stub_ard_row <- function(proto, variable, stat_name, by_var, by_level) {
  r <- proto
  r$group1         <- if (is.na(by_var)) NA_character_ else by_var
  r$group1_level   <- list(if (is.na(by_level)) NA_character_ else by_level)
  r$variable       <- variable %||% NA_character_
  r$variable_level <- list(NA_character_)
  if ("context" %in% names(r))    r$context    <- "manual_pending"
  r$stat_name      <- stat_name
  r$stat_label     <- stat_name
  r$stat           <- list(NA_real_)
  if ("fmt_fun" %in% names(r))    r$fmt_fun    <- list(NULL)
  if ("warning" %in% names(r))    r$warning    <- list(NULL)
  if ("error"   %in% names(r))    r$error      <- list(NULL)
  r
}

## Assemble all stub rows for one unexecutable method: one row per declared
## statistic, times the group levels present in the data when the statistic is
## reported by group. Returns a `card`, or NULL if nothing to reserve.
#' @noRd
.stub_ard_for_method <- function(res, method_id, df, by, var) {
  desc  <- .UNEXECUTABLE_METHODS[[method_id]]
  proto <- .ard_schema_proto()
  by1   <- if (!is.null(by) && length(by) && by[1] %in% names(df))
    by[1] else NA_character_
  levels <- if (isTRUE(desc$by_group) && !is.na(by1)) {
    unique(as.character(df[[by1]]))
  } else {
    NA_character_
  }
  rows <- list()
  for (lv in levels) {
    for (st in desc$stats) {
      rows[[length(rows) + 1L]] <- .stub_ard_row(
        proto, var, st,
        by_var   = if (isTRUE(desc$by_group)) by1 else NA_character_,
        by_level = lv)
    }
  }
  if (length(rows) == 0) return(NULL)
  cards::bind_ard(!!!rows)
}

#' The group levels an ARD frame carries, as plain text.
#'
#' `group1_level` is a LIST column in a cards ARD -- one element per row -- so
#' it needs flattening before it can be compared with a label.
#' @noRd
.ard_group_levels <- function(ard) {
  if (is.null(ard) || !"group1_level" %in% names(ard)) return(character())
  levels <- vapply(ard[["group1_level"]], function(x) {
    value <- as.character(x)
    if (length(value) == 0) NA_character_ else value[[1]]
  }, character(1))
  unique(levels[!is.na(levels)])
}

#' Did the overall column the metadata promised actually produce rows?
#'
#' Guidance rule 2. `includeTotal` says a Total column will be computed; this
#' is the only place that checks it was. Silent otherwise -- an analysis with
#' no overall column has nothing to deliver.
#' @noRd
.check_total_delivered <- function(ard, res, analysis_id) {
  label <- res$total_label %||% ""
  if (!isTRUE(res$include_total) || !nzchar(label)) return(invisible(NULL))
  if (label %in% .ard_group_levels(ard)) return(invisible(NULL))

  .diag_gap(
    stage = "execute_ard", severity = "WARN", input = INPUT_ARS,
    problem = sprintf(
      "Analysis %s declares an overall column (%s) but produced no result for it.",
      analysis_id, label),
    why = "The column is displayed, so its cells stay on their placeholder while the columns beside them fill.",
    fix = "Check the Total header's annotation -- a scope that matches no subjects yields no rows.",
    location = analysis_id)
  invisible(NULL)
}

## --- blocking -------------------------------------------------------------
##
## Three things stop a where-clause being applied at all: a referenced dataset
## that cannot be read, one with no subject key to carry an answer back on, and
## an expression whose row coherence is not determined. None may quietly widen
## the population -- a filter that did not run is a wrong denominator, and a
## wrong denominator is invisible in a rendered table.
##
## The mask functions return this signal instead of a logical vector. The
## analysis loop turns it into a FAIL naming the analysis and reserved
## `blocked` rows, so the reason stays recoverable through ars_blockers().

.BLOCK_MISSING_DATASET     <- "missing_dataset"
.BLOCK_MISSING_SUBJECT_KEY <- "missing_subject_key"
.BLOCK_AMBIGUOUS_PLAN      <- "ambiguous_row_coherence"
## Denominator construction, when the population frame cannot supply the
## grouping variable and the foreign dataset cannot resolve it either.
.BLOCK_DENOM_AMBIGUOUS    <- "denominator_grouping_ambiguous"
.BLOCK_DENOM_UNRESOLVED   <- "denominator_grouping_unresolved"
.BLOCK_GROUPING_DISAGREE  <- "grouping_value_disagreement"

#' @noRd
.block_signal <- function(reason, detail = NA_character_) {
  structure(list(reason = reason, detail = detail), class = "arsbridge_block")
}

#' @noRd
.is_block <- function(x) inherits(x, "arsbridge_block")

## TRUE only for something that can actually index `df`. A NULL or wrong-length
## mask is a bug, not a filter, and must never reach `df[mask, ]`.
#' @noRd
.is_usable_mask <- function(mask, df) {
  is.logical(mask) && !is.null(df) && length(mask) == nrow(df)
}

## One sentence a reviewer can act on, per reason. The analysis id is in the
## text AND in the diagnostic's `location`, which is the mapping from a blocked
## row back to why it is blocked.
#' @noRd
.block_problem <- function(block, analysis_id) {
  switch(
    block$reason,
    missing_dataset = sprintf(
      "Analysis %s is blocked: dataset %s named by a where-clause is not in the ADaM directory",
      analysis_id, block$detail),
    missing_subject_key = sprintf(
      "Analysis %s is blocked: dataset %s named by a where-clause has no subject key, so its filter cannot be carried back",
      analysis_id, block$detail),
    ambiguous_row_coherence = sprintf(
      "Analysis %s is blocked: a where-clause naming %s cannot be planned without guessing whether its predicates hold on the same record",
      analysis_id, block$detail),
    denominator_grouping_ambiguous = sprintf(
      "Analysis %s is blocked: cannot construct grouped denominator -- %s, so their group membership has no single answer",
      analysis_id, block$detail),
    denominator_grouping_unresolved = sprintf(
      "Analysis %s is blocked: cannot construct grouped denominator -- %s, so any per-group N would be partly invented",
      analysis_id, block$detail),
    grouping_value_disagreement = sprintf(
      "Analysis %s is blocked: %s -- the numerator would be counted under one group and the denominator under another",
      analysis_id, block$detail),
    sprintf("Analysis %s is blocked (%s): %s", analysis_id, block$reason,
            block$detail)
  )
}

## Subjects whose grouping value differs between the population frame and the
## analysis domain. NULL when they agree, or when they cannot be compared.
#' @noRd
.grouping_value_disagreement <- function(df_population, df_domain, var,
                                         subject_key) {
  if (!all(c(var, subject_key) %in% names(df_population))) return(NULL)
  if (!all(c(var, subject_key) %in% names(df_domain))) return(NULL)

  pop <- unique(df_population[, c(subject_key, var), drop = FALSE])
  dom <- unique(df_domain[!is.na(df_domain[[var]]),
                          c(subject_key, var), drop = FALSE])
  both <- merge(pop, dom, by = subject_key, suffixes = c(".pop", ".dom"))
  if (nrow(both) == 0) return(NULL)

  pop_val <- as.character(both[[paste0(var, ".pop")]])
  dom_val <- as.character(both[[paste0(var, ".dom")]])
  differs <- !is.na(pop_val) & !is.na(dom_val) & pop_val != dom_val
  if (!any(differs)) return(NULL)

  sprintf("%d subject(s) have a different %s in the population frame than in the analysis dataset",
          length(unique(both[[subject_key]][differs])), var)
}

#' The stamped `blocked` rows for one refused analysis.
#'
#' Package level rather than a closure so both refusal points -- the
#' where-clause plan and the denominator -- reserve identically.
#' @noRd
.reserve_blocked_ard <- function(block, analysis_id, res, method_id, out_id,
                                 analysis_var) {
  diag_add(
    stage = "execute_ard", severity = "FAIL", input = INPUT_DATA,
    problem = .block_problem(block, analysis_id),
    location = analysis_id,
    action = "Analysis blocked -- reserved cells, no computed results"
  )
  out <- .blocked_ard_for_analysis(analysis_var, block)
  out[["analysis_id"]]     <- analysis_id
  out[["analysis_descr"]]  <- res$description
  out[["method_id"]]       <- method_id
  out[["output_id"]]       <- out_id %||% NA_character_
  out[["method_intended"]] <- method_id
  out[["method_actual"]]   <- method_id
  out[["result_status"]]   <- "blocked"
  out[["block_reason"]]    <- block$reason
  out[["value_source"]]    <- NA_character_
  out[["derivation_ref"]]  <- NA_character_
  out[["derived_by"]]      <- NA_character_
  out[["derived_dt"]]      <- NA_character_
  out
}

#' Reserved rows for an analysis that could not safely be computed.
#'
#' Keyed like the manual_pending stubs so the same downstream machinery finds
#' them, but a different status: `manual_pending` means a human must derive the
#' number, and putting an unfixable analysis on that worklist would be wrong.
#' The fix here is to the spec or the data.
#' @noRd
.blocked_ard_for_analysis <- function(variable, block) {
  row <- .stub_ard_row(.ard_schema_proto(), variable, NA_character_,
                       by_var = NA_character_, by_level = NA_character_)
  if ("context" %in% names(row)) row$context <- "blocked"
  cards::bind_ard(row)
}

## --- reading ADaM, and applying a where-clause across datasets --------------
##
## These four lived inside ars_to_ard() as closures, which put them out of
## reach of every other function and of any test. That is not a theoretical
## cost: `.complete_zero_groups()` called the evaluator from namespace level,
## resolved lexically to nothing, and its declared-column branch died with
## "could not find function" -- the same shape of defect these carried.
##
## `.where_keep_mask()` decides the population, and therefore every
## denominator, on BOTH execution paths. Behaviour for valid input is
## unchanged and pinned numerically in test-where_clause_execution.R.

#' A cache over the ADaM directory: name in, data frame out.
#'
#' The store owns the FAIL diagnostic for a dataset that is not there, exactly
#' as the closure it replaces did -- `diag_add()` is a package-level function
#' writing to the run's diagnostics, not something captured from a frame, so
#' severity, wording and control flow are the same by construction.
#'
#' A dataset that is NOT there is remembered as such. `dfs[[name]] <- NULL`
#' removes the element rather than storing one, so before this every lookup
#' re-read the directory and re-reported -- one FAIL per analysis that named
#' the same absent dataset, which is noisiest exactly when it matters most. The
#' sentinel is keyed on the canonical identity resolution already uses (the
#' upper-cased name), so ADSL / adsl / AdSl are one dataset and one diagnostic,
#' while genuinely different datasets each get their own.
#' @noRd
## Stored in place of a frame for a dataset that could not be resolved, so the
## absence is remembered. NULL cannot do this job: assigning it to a list
## element deletes the element.
.ADAM_ABSENT <- structure(list(), class = "arsbridge_absent_dataset")

.adam_store <- function(adam_dir) {
  dfs <- list()

  get <- function(name) {
    if (is.null(name) || !nzchar(name)) return(NULL)
    ## The canonical identity resolution itself uses: every spelling of one
    ## dataset resolves to one cache slot, and so to one diagnostic.
    name_upper <- toupper(name)
    if (!name_upper %in% names(dfs)) {
      files <- list.files(adam_dir, full.names = TRUE)
      basenames <- tolower(basename(files))
      csv_file <- files[basenames == tolower(paste0(name_upper, ".csv"))]
      xpt_file <- files[basenames == tolower(paste0(name_upper, ".xpt"))]
      sas_file <- files[basenames == tolower(paste0(name_upper, ".sas7bdat"))]

      if (length(xpt_file) > 0) {
        ## .read_dataset reads .xpt/.sas7bdat/.csv and, on a read failure,
        ## records a FAIL diagnostic naming the dataset and returns NULL (this
        ## analysis is then skipped) rather than throwing a cryptic base-R
        ## error. Native SAS formats are preferred over .csv when both exist.
        dfs[[name_upper]] <<- .read_dataset(xpt_file[1], name_upper) %||%
          .ADAM_ABSENT
      } else if (length(sas_file) > 0) {
        dfs[[name_upper]] <<- .read_dataset(sas_file[1], name_upper) %||%
          .ADAM_ABSENT
      } else if (length(csv_file) > 0) {
        dfs[[name_upper]] <<- .read_dataset(csv_file[1], name_upper) %||%
          .ADAM_ABSENT
      } else {
        cli::cli_warn("Dataset {.val {name_upper}} not found in {.path {adam_dir}}.")
        diag_add(
          stage = "execute_ard", severity = "FAIL", input = INPUT_DATA,
          problem = sprintf("Dataset %s not found in ADaM directory", name_upper),
          location = adam_dir,
          action = "All analyses against this dataset were skipped"
        )
        dfs[[name_upper]] <<- .ADAM_ABSENT
      }
    }
    resolved <- dfs[[name_upper]]
    if (inherits(resolved, "arsbridge_absent_dataset")) return(NULL)
    resolved
  }

  list(get = get)
}

#' The datasets a where-clause references, read once for both halves.
#'
#' `.where_datasets()` is the single source of truth. The executor used to
#' carry its own copy, and the two differed in one respect: this one coerces
#' every `dataset` through `.as_scalar_char()`, while the copy returned the
#' raw field. A `dataset` written as more than one value therefore meant the
#' emitter filtered on the first and the executor intersected them all -- the
#' same spec, two filters.
#'
#' The ARS schema has `dataset` as a scalar, so the scalar reading wins. What
#' changes is that narrowing is no longer silent: cardinality > 1 is malformed
#' input and is reported.
#' @noRd
.where_datasets_checked <- function(where_clause, location = NA_character_) {
  ## One diagnostic per malformed field, so the count names DATASETS rather
  ## than however many fields happened to be wrong.
  for (field in Filter(function(x) length(x) > 1L,
                       .where_datasets_raw(where_clause))) {
    values <- as.character(unlist(field, use.names = FALSE))
    diag_add(
      stage = "execute_ard", severity = "WARN", input = INPUT_ARS,
      problem = sprintf(
        "A where-clause `dataset` field names %d datasets: %s",
        length(values), paste(values, collapse = ", ")),
      location = location,
      action = sprintf("Only %s is used, matching the emitted code",
                       values[[1]])
    )
  }
  .where_datasets(where_clause)
}

## Every `dataset` field as written, uncoerced, so cardinality can be judged
## before `.where_datasets()` flattens it away.
#' @noRd
.where_datasets_raw <- function(where_clause) {
  if (is.null(where_clause)) return(list())
  if (!is.null(where_clause[["condition"]])) {
    field <- where_clause[["condition"]][["dataset"]]
    return(if (is.null(field)) list() else list(field))
  }
  if (!is.null(where_clause[["compoundExpression"]])) {
    clauses <- where_clause[["compoundExpression"]][["whereClauses"]]
    return(unlist(lapply(clauses, .where_datasets_raw), recursive = FALSE))
  }
  if (!is.null(where_clause[["dataset"]])) {
    return(list(where_clause[["dataset"]]))
  }
  list()
}

#' The mask a where-clause puts on one frame that is already in hand.
#'
#' A clause naming only the target dataset is evaluated row by row. A clause
#' naming another one is answered on THAT dataset and carried back by subject
#' key, because `.eval_condition()` reads a variable the frame does not have as
#' FALSE for every row -- so an ADSL condition applied directly to an
#' occurrence frame keeps nothing at all. This is the one place that rule
#' lives; `.apply_where_clause()` and the Total pass both go through it, and
#' the emitted code says the same thing in dplyr (`.apply_where_expr()`).
#'
#' It interprets the plan `.where_restriction_plan()` builds, which the emitter
#' renders as dplyr for the same clause, so the two cannot drift apart.
#'
#' Two ways of declining to filter are carried over unchanged, and are the
#' deferred defects rather than the design: a referenced dataset that cannot be
#' read is skipped, and one without the subject key is reported and skipped --
#' in both cases that branch of the plan contributes no restriction. See
#' notes/plans/POST_ROADMAP_risks_and_order.md.
#' @noRd
.where_keep_mask <- function(df, target_ds_name, where_clause, store,
                             subject_key) {
  if (is.null(df)) return(logical(0))
  all_rows <- rep(TRUE, nrow(df))
  if (is.null(where_clause)) return(all_rows)

  ## Reported here, not in the planner, so planning stays pure: malformed
  ## `dataset` cardinality is a diagnostic about the spec being run.
  if (length(.where_datasets_checked(where_clause, target_ds_name)) == 0) {
    return(all_rows)
  }

  plan <- .where_restriction_plan(where_clause, target_ds_name, subject_key)
  if (!isTRUE(plan$ok)) {
    ## Diagnosed by the caller, which knows the analysis. Returning the signal
    ## keeps one FAIL per blocked analysis instead of one per frame the clause
    ## is applied to.
    return(.block_signal(plan$reason, plan$detail))
  }

  .plan_mask(plan$node, df, store, subject_key, target_ds_name)
}

## Interpret one plan node as a mask over `df`.
#' @noRd
.plan_mask <- function(node, df, store, subject_key, target_ds_name) {
  all_rows <- rep(TRUE, nrow(df))

  if (identical(node$kind, "all")) return(all_rows)

  if (identical(node$kind, "row")) {
    return(.eval_where_clause(df, node$where))
  }

  if (identical(node$kind, "subject")) {
    ref_df <- store$get(node$dataset)
    ## Not readable: the clause cannot be applied, so nothing is computed.
    ## Carrying on unfiltered would report a population nobody asked for.
    if (is.null(ref_df)) {
      return(.block_signal(.BLOCK_MISSING_DATASET, node$dataset))
    }

    ## The whole subtree, row-wise on its own dataset: this is what keeps two
    ## predicates on one conmed record from becoming two separate records.
    keep <- .eval_where_clause(ref_df, node$where)
    ref_df_filtered <- ref_df[keep, , drop = FALSE]

    ## No key on either side means the answer cannot be carried back.
    if (!subject_key %in% names(ref_df_filtered) ||
        !subject_key %in% names(df)) {
      return(.block_signal(.BLOCK_MISSING_SUBJECT_KEY, node$dataset))
    }
    return(df[[subject_key]] %in% unique(ref_df_filtered[[subject_key]]))
  }

  masks <- lapply(node$children, .plan_mask, df = df, store = store,
                  subject_key = subject_key, target_ds_name = target_ds_name)
  ## A blocked branch blocks the whole clause, whatever the operator: an OR
  ## whose other side is unknown is not TRUE, it is unknown.
  blocked <- Filter(.is_block, masks)
  if (length(blocked) > 0) return(blocked[[1]])
  if (length(masks) == 0) return(all_rows)
  Reduce(if (identical(node$op, "OR")) `|` else `&`, masks)
}

#' A dataset read from the store and filtered by a where-clause.
#' @noRd
.apply_where_clause <- function(target_ds_name, where_clause, store,
                                subject_key) {
  df <- store$get(target_ds_name)
  if (is.null(df) || is.null(where_clause)) {
    return(df)
  }
  mask <- .where_keep_mask(df, target_ds_name, where_clause, store, subject_key)
  if (.is_block(mask)) return(mask)
  ## Anything that is not a usable mask blocks rather than being indexed with:
  ## df[NULL, ] is a silent zero-row frame, which reads downstream as "this
  ## analysis genuinely had no data".
  if (!.is_usable_mask(mask, df)) {
    return(.block_signal("unusable_mask", target_ds_name %||% NA_character_))
  }
  df[mask, , drop = FALSE]
}

#' Execute ARS JSON and return an ARD object using 'cards'
#'
#' Reads a CDISC ARS JSON specification and executes the analyses defined within
#' it directly using the `{cards}` package, dynamically loading the ADaM datasets
#' (.csv, .xpt, or .sas7bdat) and combining individual ARD tables into a single tidy ARD object.
#'
#' @param ars_path   Path to the CDISC ARS JSON file.
#' @param adam_dir   Directory containing the ADaM datasets (.csv, .xpt, or .sas7bdat).
#' @param output_ids Optional character vector of Output IDs to run only analyses
#'   referenced by those outputs. Matching is case-insensitive and checks both
#'   Output ID and Output Name (e.g. "T-14-1-1" or "T_14_1_1").
#' @param analysis_ids Optional character vector of Analysis IDs to run only
#'   those specific analyses.
#' @param subject_key Subject-level identifier variable used for
#'   distinct-subject counting and cross-dataset population joins.
#'   Default `"USUBJID"`; set e.g. `"SUBJID"` or `"PATID"` for studies with
#'   a non-standard subject key.
#' @param legacy Deprecated execution path. When `FALSE` (default) each analysis
#'   is computed by sourcing the pure-`{cards}` block arsbridge emits, so the
#'   ARD is produced by the same code shipped as the deliverable. When `TRUE`
#'   the retired `.ARD_EXECUTORS` registry is used instead (kept only for the
#'   engine-equivalence test and as a transitional escape hatch).
#'
#' @return A tidy ARD data frame of class `"card"`, with traceability
#'   columns `analysis_id`, `method_id`, `output_id`, `method_intended`,
#'   and `method_actual` (differs from `method_intended` when the generic
#'   fallback summarizer was used), plus provenance columns (ADR 0002):
#'   `result_status`, `value_source` (`"cards"`), `derivation_ref` (the emitted
#'   block, `arsbridge:emitted:<id>`), `derived_by` (`"arsbridge"`), and
#'   `derived_dt` (run timestamp, ISO-8601; pin with
#'   `options(arsbridge.derived_dt=)`). These let a later partial / manual fill
#'   be distinguished from engine output without breaking traceability.
#'
#'   `result_status` takes one of four values, and they are not
#'   interchangeable:
#'
#'   \describe{
#'     \item{`"computed"`}{a trustworthy engine result.}
#'     \item{`"manual_pending"`}{a cell reserved because the method has no
#'       executor. Valid work that a programmer must derive by hand; listed by
#'       [ars_manual_worklist()].}
#'     \item{`"manual_filled"`}{a manual result that has been supplied and
#'       validated; checked by [ars_validate_manual_fills()].}
#'     \item{`"blocked"`}{computation could not safely proceed, because
#'       required data or filter semantics could not be satisfied -- a
#'       referenced dataset that is not in `adam_dir`, one with no subject key
#'       to carry its filter back on, or a where-clause whose row semantics are
#'       not determined. A blocked analysis emits NO computed rows: a filter
#'       that did not run is a wrong denominator, and a wrong denominator is
#'       invisible in a rendered table. It is NOT manual work -- nobody can
#'       derive it by hand until the spec or the ADaM cut is repaired -- so it
#'       is excluded from [ars_manual_worklist()]. The cause is in the
#'       accompanying FAIL diagnostic, whose `location` is the analysis id; see
#'       [ars_blockers()]. The row also carries `block_reason`.}
#'   }
#' @importFrom tidyselect all_of
#' @export
#' @examples
#' \dontrun{
#'   ard <- ars_to_ard("outputs/reporting_event.json", "inputs/ADaM")
#' }
ars_to_ard <- function(ars_path, adam_dir, output_ids = NULL,
                       analysis_ids = NULL, subject_key = "USUBJID",
                       legacy = FALSE) {
  .require_file(ars_path, "ars_path", INPUT_ARS)
  .require_dir(adam_dir,  "adam_dir", INPUT_DATA)

  ## Fresh diagnostics for this execution run (inspect via ars_diagnostics()).
  diag_reset()

  spec <- .read_json(ars_path)
  .assert_runnable_ars(spec)

  store <- .adam_store(adam_dir)
  # Scalar character helper (shared file-level implementation in resolve_analysis.R).
  as_scalar_char <- .as_scalar_char

  # Lookup maps built once from the spec and shared with resolve_analysis().
  analysis_to_output <- .build_analysis_to_output(spec)
  grouping_map       <- .build_grouping_map(spec)
  grouping_groups    <- .build_grouping_groups_map(spec)
  output_paths       <- .build_output_paths_map(spec)

  ## Structural gate: an output that DECLARES result-group paths but whose
  ## declaration does not resolve (an unknown group id, say) must not fall
  ## back to flat execution -- that is exactly the three-columns-instead-of-
  ## six failure this model exists to prevent. Its analyses are skipped with
  ## a FAIL; validate_ars_model() names the precise problem.
  unresolvable_path_outputs <- character(0)
  for (out_node in spec[["outputs"]] %||% list()) {
    oid <- .as_scalar_char(out_node[["id"]])
    if (is.null(oid) || is.null(out_node[["resultGroupPaths"]])) next
    if (is.null(output_paths[[oid]])) {
      unresolvable_path_outputs <- c(unresolvable_path_outputs, oid)
      .diag_gap(
        stage = "execute_ard", severity = "FAIL", input = INPUT_ARS,
        problem = sprintf(
          "Output %s declares result-group paths that do not resolve; its analyses were skipped.", oid),
        why = "Executing with flat groupings instead would silently drop the declared display columns.",
        fix = "Run validate_ars_model() on this event -- it names the broken path/group reference -- then fix or regenerate."
      )
    }
  }

  # Walk analyses and execute
  user_filtered <- !is.null(output_ids) || !is.null(analysis_ids)
  n_selected    <- 0L   # how many analyses passed the user's id selection
  n_walked      <- 0L   # progress ticks count every analysis, skips included
  ard_list <- list()
  for (ana in spec[["analyses"]]) {
    ## Finished count, then the label of the analysis now running -- see the
    ## same pattern in ars_fill_shell(). An analysis reads its ADaM datasets,
    ## so a single tick can stand for a long wait; counting it as done before
    ## it ran is what made the bar look finished while it was not.
    .progress_tick(n_walked, length(spec[["analyses"]]),
                   as.character(ana[["id"]] %||% "?"))
    n_walked <- n_walked + 1L
    res <- resolve_analysis(ana, spec, subject_key, grouping_map,
                            analysis_to_output, grouping_groups, output_paths)
    analysis_id <- res$analysis_id
    if (is.null(analysis_id)) {
      .diag_gap(
        stage = "execute_ard", severity = "WARN", input = INPUT_ARS,
        problem = "An analysis in the ARS spec has no analysis id and was skipped.",
        why = "Without an id the analysis cannot be tied to an output, so it yields no ARD row.",
        fix = "Regenerate the ARS with spec_to_ars() -- a hand-edited spec may be missing an analysis 'id'."
      )
      next
    }
    out_id <- res$output_id
    if (!is.null(out_id) && out_id %in% unresolvable_path_outputs) next

    # Filter by user-selected output_ids and analysis_ids
    if (user_filtered) {
      matched <- FALSE
      if (!is.null(analysis_ids) && tolower(analysis_id) %in% tolower(analysis_ids)) {
        matched <- TRUE
      }
      if (!is.null(output_ids) && !is.null(out_id)) {
        # Match output ID or output name
        out_obj <- Filter(function(o) identical(as_scalar_char(o[["id"]]), out_id), spec[["outputs"]])
        out_name <- if (length(out_obj) > 0) as_scalar_char(out_obj[[1]][["name"]]) else NULL
        if (tolower(out_id) %in% tolower(output_ids) || (!is.null(out_name) && tolower(out_name) %in% tolower(output_ids))) {
          matched <- TRUE
        }
      }
      if (!matched) next
    }
    n_selected <- n_selected + 1L

    method_id    <- res$method_id
    analysis_var <- res$variable
    analysis_ds  <- res$dataset

    if (is.null(analysis_ds) || !nzchar(analysis_ds)) {
      cli::cli_warn("Skipping analysis {.val {analysis_id}}: primary dataset not specified.")
      .diag_gap(
        stage = "execute_ard", severity = "FAIL", input = INPUT_ARS,
        problem = sprintf("Analysis %s does not name a source dataset and was skipped.", analysis_id),
        why = "Without a dataset the result cannot be computed, so this output stays empty.",
        fix = "Check the ADaM spec annotation for this TLF names a dataset, then regenerate the ARS.",
        location = analysis_id
      )
      next
    }
    if (is.null(analysis_var) || !nzchar(analysis_var)) {
      cli::cli_warn("Skipping analysis {.val {analysis_id}}: analysis variable not specified.")
      .diag_gap(
        stage = "execute_ard", severity = "FAIL", input = INPUT_ARS,
        problem = sprintf("Analysis %s does not name an analysis variable and was skipped.", analysis_id),
        why = "Without a variable the result cannot be computed, so this output stays empty.",
        fix = "Check the shell/spec annotation for this row names a variable, then regenerate the ARS.",
        location = analysis_id
      )
      next
    }

    df_base <- store$get(analysis_ds)
    if (is.null(df_base)) {
      cli::cli_warn("Skipping analysis {.val {analysis_id}}: dataset {.val {analysis_ds}} not loaded.")
      next
    }

    pop_where    <- res$pop_where
    subset_where <- res$subset_where

    # Apply filters
    ## A block anywhere here stops the analysis: no computed rows, a FAIL
    ## naming the analysis and the cause, and reserved `blocked` rows so the
    ## deliverable shows a placeholder that can be explained rather than a
    ## number nobody should trust.
    block <- NULL

    df_filtered <- df_base
    if (is.null(block) && !is.null(pop_where)) {
      df_filtered <- .apply_where_clause(analysis_ds, pop_where, store, subject_key)
      if (.is_block(df_filtered)) block <- df_filtered
    }

    df_population <- NULL
    if (is.null(block)) {
      adsl_df <- store$get("ADSL")
      if (!is.null(adsl_df)) {
        df_population <- .apply_where_clause("ADSL", pop_where, store, subject_key)
        if (.is_block(df_population)) {
          block <- df_population
          df_population <- NULL
        }
      }
      if (is.null(df_population)) df_population <- df_filtered
    }

    ## The frame a missing column variable can be learned from: the domain
    ## under the population filter, BEFORE the data subset. See
    ## .denominator_by_subject() -- a subset selects events, not subjects.
    df_domain <- df_filtered

    if (is.null(block) && !is.null(subset_where)) {
      df_filtered <- .apply_where_clause(analysis_ds, subset_where, store, subject_key)
      if (.is_block(df_filtered)) block <- df_filtered
    }

    ## The overall column's scope is checked here rather than deep inside the
    ## legacy branch, so a blocked Total reserves rows like any other block.
    ## EVERY frame the Total clause will later be applied to is probed, because
    ## each can block independently -- the numerator on the analysis dataset
    ## and the denominator on ADSL.
    if (is.null(block) && isTRUE(res$include_total) &&
        !is.null(res$total_where)) {
      for (probe_on in list(list(df = df_filtered,   ds = analysis_ds),
                            list(df = df_population, ds = "ADSL"))) {
        if (!is.data.frame(probe_on$df)) next
        probe <- .where_keep_mask(probe_on$df, probe_on$ds, res$total_where,
                                  store, subject_key)
        if (.is_block(probe)) { block <- probe; break }
      }
    }

    if (!is.null(block)) {
      ard_list[[length(ard_list) + 1L]] <- .reserve_blocked_ard(
        block, analysis_id, res, method_id, out_id, analysis_var)
      next
    }

    analysis_var <- unname(.clean_var_name(analysis_var, names(df_filtered)))
    if (!analysis_var %in% names(df_filtered)) {
      cli::cli_warn("Skipping analysis {.val {analysis_id}}: variable {.val {analysis_var}} not in dataset {.val {analysis_ds}}.")
      diag_add(
        stage = "execute_ard", severity = "FAIL", input = INPUT_DATA,
        problem = sprintf("Variable %s not found in dataset %s", analysis_var, analysis_ds),
        location = analysis_id,
        action = paste0("Analysis skipped -- the shell references ", analysis_var,
                        " but the supplied ", analysis_ds,
                        " does not contain it. Provide an ADaM cut that includes this variable.")
      )
      next
    }

    # Resolve groupings (raw names from the shared resolver; cleaned below)
    grouping_vars <- res$by

    grouping_vars <- unname(sapply(grouping_vars, .clean_var_name, df_names = names(df_filtered)))
    dropped_groupings <- grouping_vars[!grouping_vars %in% names(df_filtered)]
    if (length(dropped_groupings) > 0) {
      diag_add(
        stage = "execute_ard", severity = "WARN", input = INPUT_DATA,
        problem = sprintf("Grouping variable(s) %s not present in dataset %s",
                          paste(dropped_groupings, collapse = ", "), analysis_ds),
        location = analysis_id,
        action = "Analysis ran UNGROUPED -- results are totals, not by-group"
      )
    }
    grouping_vars <- grouping_vars[grouping_vars %in% names(df_filtered)]
    by_arg <- if (length(grouping_vars) > 0) grouping_vars else NULL

    ## A column variable the denominator frame lacks -- ADCM.TRTA against an
    ## ADSL that carries TRT01A -- makes every percentage come out of the whole
    ## study, silently. Carry it over by subject before anything divides by it.
    denom_fix <- .denominator_by_subject(
      df_population, df_domain, by_arg, subject_key, analysis_id, analysis_ds)
    ## A denominator that cannot be constructed is refused, not approximated.
    if (!is.null(denom_fix$block)) {
      ard_list[[length(ard_list) + 1L]] <- .reserve_blocked_ard(
        denom_fix$block, analysis_id, res, method_id, out_id, analysis_var)
      next
    }
    df_population <- denom_fix$data
    ## The emitted block must do the same join, or emitted stops equalling
    ## executed -- .denom_join_expr() writes down what was decided here.
    res$denom_by_subject <- denom_fix$joined

    ## Annotation-defined column groups: the shell's header cells carried one
    ## condition per column ("Cohort A ... ADSL.COHORTN=1", "... is missing").
    ## Keep only definitions whose variable survived into by_arg and whose
    ## conditions are ADSL-resolvable (the derived factor must exist in both
    ## the analysis frame and the ADSL denominator frame, or cards cannot
    ## join them). The emitted code derives the same factor via case_when
    ## (.group_mutate_expr); the legacy path derives it here so the
    ## engine-equivalence guarantee holds.
    group_defs <- list()
    for (raw_var in names(res$group_defs %||% list())) {
      clean <- unname(.clean_var_name(raw_var, names(df_filtered)))
      defs  <- res$group_defs[[raw_var]]
      if (!clean %in% (by_arg %||% character())) next
      adsl_only <- all(vapply(defs, function(d) {
        refs <- .where_datasets(d$condition)
        length(refs) == 0 || all(toupper(refs) == "ADSL")
      }, logical(1)))
      if (!adsl_only) next
      group_defs[[clean]] <- defs
    }
    res$group_defs <- group_defs

    for (grp_var in names(group_defs)) {
      defs <- group_defs[[grp_var]]
      ## Rows no condition claims fall out of the group columns (they become
      ## a factor NA, which cards drops from the grouped pass) -- say how
      ## many, once per analysis, so a shrinking column set is never silent.
      matched <- Reduce(`|`, lapply(defs, function(d) {
        .eval_where_clause(df_filtered, d$condition)
      }))
      n_unmatched <- sum(!matched)
      if (n_unmatched > 0) {
        diag_add(
          stage = "execute_ard", severity = "WARN", input = INPUT_DATA,
          problem = sprintf(
            "%d row(s) in %s match no column-group condition for %s",
            n_unmatched, analysis_ds, grp_var),
          location = analysis_id,
          action = "Excluded from the group columns; still counted in the Total column when the shell has one"
        )
      }
      if (legacy) {
        derive_group_factor <- function(df) {
          labels <- vapply(defs, function(d) d$label, character(1))
          lbl <- rep(NA_character_, nrow(df))
          for (d in defs) {
            hit <- .eval_where_clause(df, d$condition)
            lbl[is.na(lbl) & hit] <- d$label
          }
          df[[grp_var]] <- factor(lbl, levels = labels)
          df
        }
        df_filtered <- derive_group_factor(df_filtered)
        if (grp_var %in% names(df_population)) {
          df_population <- derive_group_factor(df_population)
        }
      }
    }


    ## Resolve the stratification operand (CMH etc.) against the data, like the
    ## grouping vars. An absent or unknown strata means the method cannot
    ## execute, so .EXEC_DESCRIPTORS$available() returns FALSE and the cell is
    ## reserved as manual_pending instead.
    strata_clean <- if (!is.null(res$strata) && nzchar(res$strata))
      unname(.clean_var_name(res$strata, names(df_filtered))) else NULL
    res$strata <- if (!is.null(strata_clean) &&
                      strata_clean %in% names(df_filtered)) strata_clean else NULL

    # Handle listings
    if (identical(method_id, "MTH_LISTING")) {
      cli::cli_inform("Skipping listing analysis {.val {analysis_id}} (listings are not summarized in ARD).")
      next
    }

    # Resolve the method to a cards idiom. Unknown methods fall back to a
    # continuous/categorical summary chosen by the data type, recorded in
    # method_actual. eff_method_id drives the emitter; the legacy registry is
    # only consulted when legacy = TRUE.
    method_actual <- method_id
    eff_method_id <- method_id
    ## A method with an executor descriptor computes via its emitted block when
    ## its prerequisites are met (its package is installed, its operand resolved);
    ## otherwise it degrades to a reserved stub. A method with no executor at all
    ## always reserves a stub (ADR 0002). A stub skips fallback coercion and the
    ## all-missing data check -- it needs no tabulable values.
    exec_desc <- .EXEC_DESCRIPTORS[[method_id]]
    is_exec   <- !is.null(exec_desc) && isTRUE(exec_desc$available(res))
    is_stub   <- (method_id %in% names(.UNEXECUTABLE_METHODS)) && !is_exec
    if (!is_stub && !is_exec && is.null(.ARD_EXECUTORS[[method_id]])) {
      is_num <- is.numeric(df_filtered[[analysis_var]])
      method_actual <- if (is_num) "FALLBACK_CONTINUOUS" else "FALLBACK_CATEGORICAL"
      eff_method_id <- if (is_num) "MTH_SUMMARY_STATISTICS_CONTINUOUS" else
        "MTH_COUNT_AND_PERCENTAGE"
      cli::cli_warn("Method {.val {method_id}} for analysis {.val {analysis_id}} not natively supported. Using fallback summarizer.")
      diag_add(
        stage = "execute_ard", severity = "WARN", input = INPUT_ARS,
        problem = sprintf("Method %s not natively supported by the executor", method_id),
        location = analysis_id,
        action = sprintf("Generic %s summary used instead (method_actual = %s) -- verify the statistics match the shell",
                         if (is_num) "continuous" else "categorical", method_actual)
      )
    }

    if (!subject_key %in% names(df_filtered) &&
        method_id %in% c("MTH_AE_FREQUENCY_COUNT", "MTH_SUBJECT_COUNT",
                         "MTH_SUBJECT_COUNT_PCT")) {
      cli::cli_warn("Skipping analysis {.val {analysis_id}}: subject key {.val {subject_key}} not in dataset {.val {analysis_ds}}.")
      diag_add(
        stage = "execute_ard", severity = "FAIL", input = INPUT_DATA,
        problem = sprintf("Subject key %s required by %s but not present in dataset %s",
                          subject_key, method_id, analysis_ds),
        location = analysis_id,
        action = "Analysis skipped -- pass subject_key= to ars_to_ard() if this study uses a different identifier"
      )
      next
    }

    ## A study variable that exists but is entirely missing in this data cut
    ## (e.g. a special-interest AE flag with no events) has nothing to
    ## tabulate. {cards} errors hard on an all-NA non-factor column, so detect
    ## this first and skip with an explanatory WARN rather than a cryptic
    ## calculation failure. The output stays empty until a populated cut is
    ## supplied.
    is_continuous_method <- method_id %in% c("MTH_SUMMARY_STATISTICS_CONTINUOUS") ||
      identical(method_actual, "FALLBACK_CONTINUOUS")
    col_vals      <- df_filtered[[analysis_var]]
    all_missing   <- if (is_continuous_method) {
      all(is.na(suppressWarnings(as.numeric(col_vals))))
    } else {
      !is.factor(col_vals) && all(is.na(col_vals))
    }
    if (!is_stub && analysis_var %in% names(df_filtered) && all_missing) {
      reason <- if (is_continuous_method)
        "has no numeric values to summarise" else "is all-missing"
      cli::cli_warn("Skipping analysis {.val {analysis_id}}: variable {.val {analysis_var}} {reason} in this data cut.")
      diag_add(
        stage = "execute_ard", severity = "WARN", input = INPUT_DATA,
        problem = sprintf("Variable %s %s in dataset %s", analysis_var, reason, analysis_ds),
        location = analysis_id,
        action = paste0("Nothing to tabulate in this cut -- supply an ADaM ",
                        "dataset where ", analysis_var, " is populated to render this row.")
      )
      next
    }

    ## Codelist decode of the analysis variable (legacy path only -- the
    ## default path sources the emitted block, which derives the same factor
    ## via .decode_mutate_expr, so this keeps emitted == executed). Same
    ## gates as the emitter: per-category methods only, never the subject
    ## key, never a bare-flag count. Applied after the all-missing skip so
    ## both paths judge that condition on the raw column.
    if (legacy && !is_stub && !is.null(res$decode) && length(res$decode) > 0 &&
        method_id %in% .DECODE_METHOD_IDS &&
        !identical(toupper(analysis_var), toupper(subject_key)) &&
        !.is_bare_flag(res) &&
        analysis_var %in% names(df_filtered)) {
      dec_values <- vapply(res$decode, function(d) as.character(d$value %||% ""),
                           character(1))
      dec_labels <- vapply(res$decode, function(d) as.character(d$label %||% ""),
                           character(1))
      dec_keep <- nzchar(dec_values) & nzchar(dec_labels)
      if (any(dec_keep)) {
        df_filtered[[analysis_var]] <- factor(
          as.character(df_filtered[[analysis_var]]),
          levels = dec_values[dec_keep],
          labels = dec_labels[dec_keep]
        )
        ## Same rule as the emitter's droplevels(): above the expansion cap
        ## only the observed terms become rows.
        if (sum(dec_keep) > .CODELIST_DECODE_MAX_TERMS) {
          df_filtered[[analysis_var]] <- droplevels(df_filtered[[analysis_var]])
        }
      }
    }

    ard <- if (is_stub) {
      ## Reserve keyed manual_pending rows; no calculation attempted.
      .stub_ard_for_method(res, method_id, df_filtered, by_arg, analysis_var)
    } else tryCatch({
      if (legacy) {
        ## Retired registry path -- kept only for the engine-equivalence test.
        executor <- .ARD_EXECUTORS[[method_id]]
        if (is.null(executor)) {
          executor <- if (identical(eff_method_id,
                                    "MTH_SUMMARY_STATISTICS_CONTINUOUS")) {
            function(df, var, by, denom, subject_key)
              cards::ard_continuous(data = df, variables = all_of(var),
                                    by = all_of(by))
          } else {
            function(df, var, by, denom, subject_key)
              cards::ard_categorical(data = df, variables = all_of(var),
                                     by = all_of(by), denominator = denom)
          }
        }
        a <- executor(df_filtered, analysis_var, by_arg, df_population,
                      subject_key)
        if (isTRUE(res$include_total) && !is.null(by_arg)) {
          ## Scoped by the Total column's own condition when the shell gave
          ## one, exactly as the emitted block is -- otherwise this path and
          ## the default path would disagree and the equivalence test would
          ## be comparing two different questions. A NULL condition makes the
          ## mask all-TRUE, which is the ungrouped pass as it has always been.
          ##
          ## Each frame is masked against its OWN dataset: the numerator on
          ## the analysis dataset, the denominator on ADSL, which is what
          ## .denom_expr() emits. A Total header annotated with a subject-level
          ## variable ([ADSL.COHORTN IN (1,2)] is the shape shells write) names
          ## a column the occurrence frame cannot answer by itself, so this
          ## goes through .where_keep_mask() rather than evaluating the clause
          ## row by row.
          keep_t <- .where_keep_mask(df_filtered, analysis_ds, res$total_where,
                                     store, subject_key)
          keep_p <- if (is.null(df_population)) NULL else {
            .where_keep_mask(df_population, "ADSL", res$total_where,
                             store, subject_key)
          }
          ## An unplannable Total scope must not fall back to the whole
          ## population -- that is a plausible wrong overall column.
          ## Defensive. A blocked Total scope is caught by the preflight above
          ## and reserved, so neither arm should be reachable -- but this
          ## branch must never index with a signal, a NULL, or a mask of the
          ## wrong length.
          if (.is_block(keep_t)) stop(.block_problem(keep_t, analysis_id),
                                      call. = FALSE)
          if (.is_block(keep_p)) stop(.block_problem(keep_p, analysis_id),
                                      call. = FALSE)
          if (!.is_usable_mask(keep_t, df_filtered)) {
            stop("total_where produced no usable mask on the analysis frame",
                 call. = FALSE)
          }
          if (!is.null(df_population) &&
              !.is_usable_mask(keep_p, df_population)) {
            stop("total_where produced no usable mask on the population frame",
                 call. = FALSE)
          }
          df_t   <- df_filtered[keep_t, , drop = FALSE]
          df_pop <- if (is.null(df_population)) NULL else {
            df_population[keep_p, , drop = FALSE]
          }
          ## Keep every grouping after the column axis as a row key, but do not
          ## let those row variables split the denominator. This mirrors the
          ## emitted Total pass: e.g. each Sex row is still out of the overall
          ## Total population, not out of that sex alone.
          row_by <- if (length(by_arg) > 1L) by_arg[-1L] else NULL
          if (!is.null(df_pop) && length(row_by)) {
            df_pop <- df_pop[, setdiff(names(df_pop), row_by), drop = FALSE]
          }
          total_ard <- executor(df_t, analysis_var, row_by, df_pop,
                                subject_key)
          total_label <- res$total_label %||% ""
          if (nzchar(total_label) && length(by_arg)) {
            row_group_count <- length(by_arg) - 1L
            if (row_group_count > 0L) {
              for (i in rev(seq_len(row_group_count))) {
                total_ard[[paste0("group", i + 1L)]] <-
                  total_ard[[paste0("group", i)]]
                total_ard[[paste0("group", i + 1L, "_level")]] <-
                  total_ard[[paste0("group", i, "_level")]]
              }
            }
            total_ard[["group1"]] <- rep(by_arg[[1]], nrow(total_ard))
            total_ard[["group1_level"]] <-
              as.list(rep(total_label, nrow(total_ard)))
          }
          a <- cards::bind_ard(a, total_ard)
        }
        a
      } else {
        ## Default: execute by sourcing the emitted cards block, so the ARD is
        ## computed by the same code arsbridge ships as the deliverable. Feed
        ## the resolver the validated variable/grouping names and the
        ## data-driven fallback method so emitted == executed.
        res$variable      <- analysis_var
        res$by            <- by_arg
        res$method_id     <- eff_method_id
        res$include_total <- isTRUE(res$include_total)
        .run_emitted_block(res, adam_dir)
      }
    }, error = function(e) {
      cli::cli_warn("Analysis {.val {analysis_id}} failed during {.pkg cards} calculation: {e$message}")
      diag_add(
        stage = "execute_ard", severity = "FAIL", input = INPUT_DATA,
        problem = paste0("cards calculation error: ", conditionMessage(e)),
        location = analysis_id,
        action = "Analysis skipped -- no results in ARD"
      )
      NULL
    })

    if (!is.null(ard)) {
      ## Subject-key rows use a constant variable level so treatment and Total
      ## occupy grouping slots while each analysis keeps a distinct ARD identity.
      ## The emitted path already stamps the analysis id; normalize every legacy
      ## constant row, including the ungrouped pass, before cards::bind_ard().
      if (!is_stub &&
          method_id %in% c("MTH_SUBJECT_COUNT", "MTH_SUBJECT_COUNT_PCT") &&
          identical(analysis_var, subject_key) &&
          all(c("variable", "variable_level") %in% names(ard))) {
        sel <- !is.na(ard[["variable"]]) &
          ard[["variable"]] == analysis_var
        if (any(sel)) {
          ard[["variable_level"]][sel] <-
            as.list(rep(analysis_id, sum(sel)))
        }
      }

      ## An arm the subset filter emptied is absent from the executor's
      ## result, not zero in it. Complete it before the row carries any
      ## provenance, so a filled-in group is stamped exactly like a computed
      ## one -- it IS computed, from the same population the percentages use.
      if (!is_stub && method_id %in% .COUNTING_METHODS) {
        ard <- .complete_zero_groups(ard, by_arg, df_population, subject_key,
                                     group_defs)
      }

      # Add traceability metadata. analysis_descr carries the analysis's
      # human label (e.g. "EASI75 at Week 16") so the rendering layer can
      # disambiguate rows when several analyses summarise the same variable
      # under different data subsets within one output.
      analysis_descr <- res$description
      ard[["analysis_id"]]     <- analysis_id
      ard[["analysis_descr"]]  <- analysis_descr
      ard[["method_id"]]       <- method_id
      ard[["output_id"]]       <- out_id %||% NA_character_
      ard[["method_intended"]] <- method_id
      ard[["method_actual"]]   <- method_actual

      ## Provenance (ADR 0002). A computed row self-describes as {cards} output;
      ## a stub row (declared-but-unexecutable method) is flagged
      ## manual_pending with no value source, so a later validated manual fill
      ## can be told apart from engine output and an auditor can trace where a
      ## value came from. derived_dt is stamped once after assembly (below) for
      ## computed rows only; a manual_pending row stays NA until it is filled.
      ard[["result_status"]]  <- if (is_stub) "manual_pending" else "computed"
      ard[["value_source"]]   <- if (is_stub) NA_character_ else
        if (is_exec) exec_desc$value_source else "cards"
      ard[["derivation_ref"]] <- if (is_stub) NA_character_ else
        paste0("arsbridge:emitted:", analysis_id)
      ard[["derived_by"]]     <- if (is_stub) NA_character_ else "arsbridge"
      ard[["derived_dt"]]     <- NA_character_

      if (is_stub) {
        diag_add(
          stage = "execute_ard", severity = "WARN", input = INPUT_ARS,
          problem = sprintf(
            "Method %s has no executor; reserved %d manual_pending cell(s)",
            method_id, nrow(ard)),
          location = analysis_id,
          action = paste0("Compute these cells with a validated analysis ",
                          "script and fill the reserved ARD rows -- see ",
                          "ars_manual_worklist()"))
      }

      ## Guidance rule 2: a Total the metadata PROMISED must be a Total the
      ## ARD actually carries. The promise is kept in the reporting event
      ## (includeTotal) and the delivery is one group level in this frame;
      ## nothing else compares the two, so an overall pass that silently
      ## produced nothing reaches the workbook as a column of placeholders
      ## with every column beside it full.
      .check_total_delivered(ard, res, analysis_id)

      ard_list[[length(ard_list) + 1L]] <- ard
    }
  }

  ## Every analysis has run; what is left is binding the pieces together.
  .progress_tick(length(spec[["analyses"]]), length(spec[["analyses"]]),
                 "assembling the results")

  ## Surface a one-line execution-quality summary; full records via
  ## ars_diagnostics().
  diags <- diag_records()
  if (nrow(diags) > 0) {
    n_fail <- sum(diags$severity == "FAIL")
    n_warn <- sum(diags$severity == "WARN")
    cli::cli_alert_warning(
      "{nrow(diags)} execution diagnostic{?s} ({n_fail} FAIL, {n_warn} WARN) -- inspect with {.code ars_diagnostics()}"
    )
  }

  ## An explicit id selection that matched nothing is almost always a typo --
  ## tell the user plainly and list the ids that ARE available, instead of
  ## silently handing back an empty/NULL ARD.
  if (user_filtered && n_selected == 0L) {
    valid_ids <- unique(Filter(nzchar, vapply(
      spec[["outputs"]], function(o) as_scalar_char(o[["id"]]) %||% "", character(1)
    )))
    requested <- paste(c(output_ids, analysis_ids), collapse = ", ")
    .diag_gap(
      stage = "execute_ard", severity = "FAIL", input = INPUT_ARS,
      problem = sprintf("None of the requested ids matched any analysis: %s.", requested),
      why = "There was nothing to compute, so no ARD was produced.",
      fix = sprintf("Use one of the available output ids: %s.",
                    paste(valid_ids, collapse = ", "))
    )
    cli::cli_abort(c(
      "x" = "None of the requested ids matched any analysis in the {INPUT_ARS}: {.val {c(output_ids, analysis_ids)}}.",
      "i" = "Available output ids: {.val {valid_ids}}."
    ))
  }

  if (length(ard_list) == 0) {
    return(NULL)
  }

  # Combine all analyses. Two analyses can legitimately produce identical
  # {cards} identity rows -- e.g. two continuous analyses that resolve to the
  # same variable and grouping, or several disposition subject counts -- and
  # the analysis_id column that distinguishes them is not part of the cards
  # identity. bind_ard's default dedup would then silently drop every
  # analysis but one, and .distinct = FALSE errors on the duplicates instead.
  # So: bind_ard when identities are unique (keeps cards' checks), plain
  # row-bind preserving every analysis when they are not.
  ard_list <- .align_ard_columns(ard_list)

  final_ard <- tryCatch(
    cards::bind_ard(!!!ard_list, .distinct = FALSE),
    error = function(e) {
      out <- dplyr::bind_rows(ard_list)
      if (!inherits(out, "card")) class(out) <- union("card", class(out))
      out
    })

  ## Stamp the run timestamp once (ADR 0002), not inside the per-analysis loop:
  ## keeps resolve/emit pure and gives every row of one run an identical value.
  ## ISO-8601 character (not POSIXct) so a stored/round-tripped ARD is
  ## timezone-stable. The arsbridge.derived_dt option lets tests pin it. Only
  ## computed rows are stamped; a manual_pending stub has no value yet, so its
  ## derived_dt stays NA until a manual fill sets it.
  ts <- getOption("arsbridge.derived_dt",
                  format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))
  computed <- !is.na(final_ard[["result_status"]]) &
    final_ard[["result_status"]] == "computed"
  final_ard[["derived_dt"]][computed] <- ts
  return(final_ard)
}

#' Manual-derivation worklist from an ARD
#'
#' Lists every reserved `manual_pending` cell in an ARD produced by
#' [ars_to_ard()] -- statistics arsbridge could not compute (a
#' declared-but-unexecutable method, e.g. a Cochran-Mantel-Haenszel p-value)
#' and reserved as keyed stub rows. This is the analyst's checklist: each row
#' must be computed with a validated analysis script and written back into the
#' ARD (set `stat`, `result_status = "manual_filled"`, `value_source`,
#' `derivation_ref`) before the table is final. See `vignette("getting-started")`
#' and the ADRs under `adr/` for the round-trip.
#'
#' @param ard An ARD data frame of class `"card"` from [ars_to_ard()].
#' @return A data frame with one row per pending cell: `output_id`,
#'   `analysis_id`, `method_id`, `group1`, `group1_level`, `variable`,
#'   `stat_name`. Zero rows (with those columns) when nothing is pending.
#' @export
#' @examples
#' \dontrun{
#'   ard <- ars_to_ard("outputs/reporting_event.json", "inputs/ADaM")
#'   ars_manual_worklist(ard)
#' }
ars_manual_worklist <- function(ard) {
  cols <- c("output_id", "analysis_id", "method_id", "group1", "group1_level",
            "variable", "stat_name")
  empty <- stats::setNames(
    as.data.frame(rep(list(character(0)), length(cols)),
                  stringsAsFactors = FALSE), cols)
  if (is.null(ard) || !"result_status" %in% names(ard)) return(empty)
  pending <- ard[!is.na(ard[["result_status"]]) &
                   ard[["result_status"]] == "manual_pending", , drop = FALSE]
  if (nrow(pending) == 0) return(empty)
  cols_data <- lapply(cols, function(cn) {
    if (!cn %in% names(pending)) return(rep(NA_character_, nrow(pending)))
    col <- pending[[cn]]
    if (is.list(col)) vapply(col, function(x)
      if (length(x)) as.character(x[[1]]) else NA_character_, character(1))
    else as.character(col)
  })
  names(cols_data) <- cols
  flat <- as.data.frame(cols_data, stringsAsFactors = FALSE)
  rownames(flat) <- NULL
  flat
}

#' Validate manually-filled ARD cells
#'
#' Checks the manual fills in an ARD (ADR 0002 phase 5). A cell whose
#' `result_status` was set to `"manual_filled"` must carry both a value
#' (`stat`) and a `derivation_ref` -- the path/id of the validated program that
#' produced it. A manual value with no derivation reference is untraceable and
#' must never ship; [ars_render_all()] surfaces any offending row as a blocker
#' diagnostic before rendering. Run it yourself on a filled ARD to clear the
#' worklist.
#'
#' @param ard An ARD data frame (class `"card"`), typically one whose
#'   `manual_pending` cells (see [ars_manual_worklist()]) have been filled.
#' @return A data frame, one row per offending cell: `output_id`,
#'   `analysis_id`, `method_id`, `stat_name`, and `problem`. Zero rows (with
#'   those columns) when every manual fill is traceable.
#' @seealso [ars_manual_worklist()]
#' @export
#' @examples
#' \dontrun{
#'   bad <- ars_validate_manual_fills(filled_ard)
#'   if (nrow(bad)) stop("untraceable manual values present")
#' }
ars_validate_manual_fills <- function(ard) {
  cols  <- c("output_id", "analysis_id", "method_id", "stat_name", "problem")
  empty <- stats::setNames(
    as.data.frame(rep(list(character(0)), length(cols)),
                  stringsAsFactors = FALSE), cols)
  if (is.null(ard) || !"result_status" %in% names(ard)) return(empty)

  chr <- function(col) if (is.list(col)) vapply(col, function(x)
    if (length(x)) as.character(x[[1]]) else NA_character_, character(1)) else
      as.character(col)

  status <- chr(ard[["result_status"]])
  filled <- !is.na(status) & status == "manual_filled"
  if (!any(filled)) return(empty)

  dref <- if ("derivation_ref" %in% names(ard)) chr(ard[["derivation_ref"]]) else
    rep(NA_character_, length(status))
  sval <- if ("stat" %in% names(ard)) {
    vapply(ard[["stat"]], function(x)
      if (length(x)) suppressWarnings(as.numeric(x[[1]])) else NA_real_,
      numeric(1))
  } else rep(NA_real_, length(status))

  no_ref   <- filled & (is.na(dref) | !nzchar(trimws(dref)))
  no_value <- filled & is.na(sval)
  bad      <- no_ref | no_value
  if (!any(bad)) return(empty)

  problem <- ifelse(no_ref[bad],
                    "manual_filled without derivation_ref (untraceable value)",
                    "manual_filled but value (stat) is still NA")
  rows <- which(bad)
  get  <- function(cn) if (cn %in% names(ard)) chr(ard[[cn]])[rows] else
    rep(NA_character_, length(rows))
  out <- data.frame(
    output_id   = get("output_id"),
    analysis_id = get("analysis_id"),
    method_id   = get("method_id"),
    stat_name   = get("stat_name"),
    problem     = problem,
    stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out
}

#' Exact (Clopper-Pearson) binomial confidence interval as ARD rows
#'
#' Per-group exact binomial CIs for a categorical response, returned as a
#' `{cards}`-shaped ARD carrying only the interval bounds (`conf.low`,
#' `conf.high`). arsbridge emits a call to this function for a
#' `MTH_PROPORTION_CI_EXACT` analysis (ADR 0001), so the deliverable script and
#' the executed ARD are the same code. Wraps
#' [cardx::ard_categorical_ci()] with `method = "clopper-pearson"`; the n / N /
#' estimate rows cardx also returns are dropped (a paired count analysis
#' supplies them, and duplicate statistic identities would make
#' [cards::bind_ard()] error). The `"card"` class is re-asserted after the
#' subset so the result binds cleanly across `{cards}` versions.
#'
#' @param data A data frame.
#' @param variables Length-1 column name of the response variable.
#' @param by Optional column name(s) of the grouping (treatment) variable.
#' @param conf.level Confidence level (default `0.95`).
#' @return A `{cards}` ARD of `conf.low` / `conf.high` rows per group level.
#' @seealso [ars_to_ard()]
#' @export
#' @examples
#' \dontrun{
#'   ard_proportion_ci_exact(adrs, variables = "AVALC", by = "TRT01A")
#' }
ard_proportion_ci_exact <- function(data, variables, by = NULL,
                                    conf.level = 0.95) {
  if (!requireNamespace("cardx", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg cardx} is required for exact confidence intervals.")
  }
  res <- cardx::ard_categorical_ci(
    data = data, variables = all_of(variables),
    by = if (is.null(by)) NULL else all_of(by),
    method = "clopper-pearson", conf.level = conf.level)
  keep <- res[["stat_name"]] %in% c("conf.low", "conf.high")
  res  <- res[keep, , drop = FALSE]
  ## dplyr / base subsetting can drop the "card" subclass on some {cards}
  ## versions; re-assert it so cards::bind_ard() accepts the rows.
  if (!inherits(res, "card")) class(res) <- union("card", class(res))
  res
}

#' Cochran-Mantel-Haenszel test as an ARD row
#'
#' Stratified CMH chi-square test of association between a response and a
#' treatment grouping, returned as a single `{cards}`-shaped ARD row carrying
#' the `p.value`. arsbridge emits a call to this function for a `MTH_CMH_TEST`
#' analysis (ADR 0001), so the deliverable script and the executed ARD are the
#' same code. It wraps [stats::mantelhaen.test()] on the
#' response x group x strata contingency table -- base R, no extra dependency
#' (the cardx wrapper is not used). The continuity correction is applied only
#' for a 2x2xK table.
#'
#' @param data A data frame.
#' @param response,by,strata Length-1 column names of the response variable, the
#'   treatment grouping, and the stratification variable.
#' @param correct Logical; request the continuity correction (default `TRUE`,
#'   applied only when the response and grouping are both binary).
#' @return A one-row ARD (`card`) with the CMH `p.value`; `stat` is `NA` and the
#'   `error` column is populated if the test could not be computed.
#' @seealso [ars_to_ard()]
#' @export
#' @examples
#' \dontrun{
#'   ard_cmh_test(adeff, response = "AVAL", by = "TRT01A", strata = "REGION")
#' }
ard_cmh_test <- function(data, response, by, strata, correct = TRUE) {
  for (nm in c(response, by, strata)) {
    if (!nm %in% names(data))
      cli::cli_abort("CMH test: column {.val {nm}} not found in data.")
  }
  keep <- stats::complete.cases(data[, c(response, by, strata), drop = FALSE])
  tab  <- table(data[[response]][keep], data[[by]][keep], data[[strata]][keep])
  correct_eff <- isTRUE(correct) && all(dim(tab)[1:2] == 2L)
  ht <- tryCatch(stats::mantelhaen.test(tab, correct = correct_eff),
                 error = function(e) e)
  err  <- if (inherits(ht, "error")) conditionMessage(ht) else NULL
  pval <- if (is.null(err)) unname(ht$p.value) else NA_real_

  row <- .ard_schema_proto()
  row$group1         <- NA_character_
  row$group1_level   <- list(NA_character_)
  row$variable       <- response
  row$variable_level <- list(NA_character_)
  if ("context" %in% names(row)) row$context <- "cmh"
  row$stat_name      <- "p.value"
  row$stat_label     <- "CMH p-value"
  row$stat           <- list(pval)
  if ("fmt_fun" %in% names(row)) row$fmt_fun <- list(NULL)
  if ("warning" %in% names(row)) row$warning <- list(NULL)
  if ("error"   %in% names(row)) row$error   <- list(err)
  row
}
