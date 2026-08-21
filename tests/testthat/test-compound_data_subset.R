## Carrying a compound restriction into the row's DataSubset.
##
## THE DEFECT CLASS. A row's annotation states a restriction the grammar reads
## perfectly -- two conditions joined, a range, an envelope naming three -- and
## the builder then had exactly one place to put it: a FLAT subset, one
## dataset, one variable, one comparator, one value. Anything larger did not
## fit, so it reserved. The reservation was honest -- better than computing
## over every record -- but it withheld results nobody needed withheld: the
## clause was understood, and the carrier for it already existed. The
## supplement path had been riding on `data_subset_compound` all along.
##
## THE INVARIANT. What the grammar can READ, the row can CARRY. One reading of
## the annotation, shared by both row builders, with three outcomes:
## unresolved (reserve), compound (carry on `data_subset_compound`), flat (the
## shape the builder always consumed). Reading is unchanged -- this is about
## where the answer goes, not what the answer is.
##
## Two limits the carrier does NOT decide, pinned here because accepting a tree
## shape is not the same as being able to execute it:
##
##   * a mixed-dataset compound is carried, and the restriction PLAN is what
##     says whether it can run. A shape whose row coherence cannot be recovered
##     blocks, with the reason recorded, rather than being silently read as
##     something narrower (A8, D3);
##   * NOT, and anything only partly readable, stay reserved: nothing
##     downstream executes a negation (B).
##
## And one thing provenance must not do: the same restriction arriving by
## annotation and by supplement is ONE subset definition, not two (C2).
##
## Identifiers here are invented and belong to no study.

skip_if_not_installed("withr")

.cds_v1 <- list(ds = "ADQX", flag = "QXFL", cat = "QXCAT", presp = "QXPRESP",
                occur = "QXOCCUR", txt = "QXTRT", num = "QXVAL",
                foreign = "ADZZ", fflag = "ZZFL", fcat = "ZZCAT")
.cds_v2 <- list(ds = "ADWW", flag = "WWSTAT", cat = "WWGROUP", presp = "WWPRESP",
                occur = "WWDONE", txt = "WWNAME", num = "WWAMT",
                foreign = "ADYY", fflag = "YYSTAT", fcat = "YYGROUP")
.cds_vocabs <- list(.cds_v1, .cds_v2)

## Every identifier the vocabulary uses, and nothing else -- so an envelope's
## bare name is provable exactly where these tests intend it to be.
.cds_pairs <- function(v) {
  toupper(c(paste0(v$ds, ".", c(v$flag, v$cat, v$presp, v$occur, v$txt, v$num)),
            paste0(v$foreign, ".", c(v$fflag, v$fcat))))
}
.cds_resolves <- function(v) {
  keys <- .cds_pairs(v)
  function(dataset, variable) {
    toupper(paste0(dataset, ".", variable)) %in% keys
  }
}

## A WhereClause as one comparable string. Recursive, so nesting stays visible
## rather than being flattened away -- `AND(A, OR(B, C))` and `AND(A, B, C)`
## are different answers and must not compare equal.
.cds_shape <- function(w) {
  if (.is_unresolved_condition(w)) return("UNRESOLVED")
  if (is.null(w)) return("ABSENT")
  cond <- w[["condition"]]
  if (is.null(cond) && !is.null(w[["variable"]])) cond <- w
  if (!is.null(cond)) {
    return(sprintf("%s.%s %s %s", cond$dataset, cond$variable, cond$comparator,
                   paste(unlist(cond$value), collapse = "|")))
  }
  ce <- w[["compoundExpression"]]
  if (is.null(ce)) return("OTHER")
  sprintf("%s(%s)", ce$logicalOperator,
          paste(vapply(ce$whereClauses, .cds_shape, character(1)),
                collapse = ", "))
}

.cds_atom <- function(ds, var, cmp, val) {
  sprintf("%s.%s %s %s", ds, var, cmp, val)
}

## What the row builders will do with this annotation: which of the three
## outcomes, and the clause itself.
.cds_carry <- function(ann, v) {
  got <- .row_restriction(ann, .cds_resolves(v))
  if (length(got) == 0) {
    return(list(kind = "none", shape = "ABSENT", where = NULL))
  }
  list(kind = names(got)[[1]], shape = .cds_shape(got[[1]]), where = got[[1]])
}

# ---- A: the shapes a row carries -------------------------------------------

test_that("A1: two conditions on one dataset are carried as an AND", {
  for (v in .cds_vocabs) {
    got <- .cds_carry(sprintf("%s.%s='Y' AND %s.%s='C1'",
                              v$ds, v$flag, v$ds, v$cat), v)
    expect_equal(got$kind, "compound")
    expect_equal(got$shape, sprintf("AND(%s, %s)",
                                    .cds_atom(v$ds, v$flag, "EQ", "Y"),
                                    .cds_atom(v$ds, v$cat, "EQ", "C1")))
  }
})

test_that("A2: an OR is carried as an OR, not narrowed to one side", {
  for (v in .cds_vocabs) {
    got <- .cds_carry(sprintf("%s.%s='Y' OR %s.%s='C1'",
                              v$ds, v$flag, v$ds, v$cat), v)
    expect_equal(got$kind, "compound")
    expect_equal(got$shape, sprintf("OR(%s, %s)",
                                    .cds_atom(v$ds, v$flag, "EQ", "Y"),
                                    .cds_atom(v$ds, v$cat, "EQ", "C1")))
  }
})

test_that("A3: nesting survives the carry -- AND(A, OR(B, C)) stays nested", {
  ## The carrier passes the tree through unchanged. Flattening this to
  ## AND(A, B, C) would keep rows the author excluded.
  for (v in .cds_vocabs) {
    got <- .cds_carry(sprintf("%s.%s='Y' AND (%s.%s='C1' OR %s.%s='C2')",
                              v$ds, v$flag, v$ds, v$cat, v$ds, v$cat), v)
    expect_equal(got$kind, "compound")
    expect_equal(got$shape,
                 sprintf("AND(%s, OR(%s, %s))",
                         .cds_atom(v$ds, v$flag, "EQ", "Y"),
                         .cds_atom(v$ds, v$cat, "EQ", "C1"),
                         .cds_atom(v$ds, v$cat, "EQ", "C2")))
  }
})

test_that("A4: the comma conjunction is carried as the AND it reads as", {
  for (v in .cds_vocabs) {
    got <- .cds_carry(sprintf("(%s.%s='Y', %s.%s='C1')",
                              v$ds, v$flag, v$ds, v$cat), v)
    expect_equal(got$kind, "compound")
    expect_equal(got$shape, sprintf("AND(%s, %s)",
                                    .cds_atom(v$ds, v$flag, "EQ", "Y"),
                                    .cds_atom(v$ds, v$cat, "EQ", "C1")))
  }
})

test_that("A5: a BETWEEN range is carried as both of its bounds", {
  ## One authored condition, two conditions in the model. Before the carrier
  ## this reserved -- a range is not a flat subset.
  for (v in .cds_vocabs) {
    got <- .cds_carry(sprintf("%s.%s BETWEEN 1 AND 10", v$ds, v$num), v)
    expect_equal(got$kind, "compound")
    expect_equal(got$shape, sprintf("AND(%s, %s)",
                                    .cds_atom(v$ds, v$num, "GE", "1"),
                                    .cds_atom(v$ds, v$num, "LE", "10")))
  }
})

test_that("A6: a three-clause envelope is carried with all three clauses", {
  ## The shape that motivated the series: a head plus `(when ...)`, whose bare
  ## names PR3 qualified against the spec. All three arrive, in order, on the
  ## head's dataset.
  for (v in .cds_vocabs) {
    got <- .cds_carry(sprintf("%s.%s (when %s='C1' AND %s='Y' AND %s='Y')",
                              v$ds, v$txt, v$cat, v$presp, v$occur), v)
    expect_equal(got$kind, "compound")
    expect_equal(got$shape, sprintf("AND(%s, %s, %s)",
                                    .cds_atom(v$ds, v$cat, "EQ", "C1"),
                                    .cds_atom(v$ds, v$presp, "EQ", "Y"),
                                    .cds_atom(v$ds, v$occur, "EQ", "Y")))
  }
})

test_that("A7: a mixed-dataset compound is carried AND plans against the row's dataset", {
  for (v in .cds_vocabs) {
    got <- .cds_carry(sprintf("%s.%s='Y' AND %s.%s='Y'",
                              v$ds, v$flag, v$foreign, v$fflag), v)
    expect_equal(got$kind, "compound")
    expect_equal(got$shape, sprintf("AND(%s, %s)",
                                    .cds_atom(v$ds, v$flag, "EQ", "Y"),
                                    .cds_atom(v$foreign, v$fflag, "EQ", "Y")))

    ## Carrying is not executing. The plan is what decides that, and for this
    ## shape it succeeds: the foreign side becomes a subject membership test.
    plan <- .where_restriction_plan(got$where, v$ds, "USUBJID")
    expect_true(isTRUE(plan$ok))
  }
})

test_that("A8: an ambiguous mixed-dataset shape is carried, and the plan blocks it", {
  ## The foreign dataset appears in two branches that AND-regrouping cannot
  ## rejoin, so whether its predicates must hold on the SAME record is not
  ## determined by the expression. The carrier accepts the tree -- it is a
  ## faithful reading of what was written -- and the planner refuses to guess,
  ## with the reason recorded. That is the boundary: the carrier accepting a
  ## shape is never evidence that the shape can run.
  for (v in .cds_vocabs) {
    got <- .cds_carry(sprintf("(%s.%s='Y' OR %s.%s='Y') AND %s.%s='C1'",
                              v$foreign, v$fflag, v$ds, v$flag,
                              v$foreign, v$fcat), v)
    expect_equal(got$kind, "compound")
    expect_equal(got$shape,
                 sprintf("AND(OR(%s, %s), %s)",
                         .cds_atom(v$foreign, v$fflag, "EQ", "Y"),
                         .cds_atom(v$ds, v$flag, "EQ", "Y"),
                         .cds_atom(v$foreign, v$fcat, "EQ", "C1")))

    plan <- .where_restriction_plan(got$where, v$ds, "USUBJID")
    expect_false(isTRUE(plan$ok))
    expect_equal(plan$reason, .PLAN_UNSUPPORTED_AMBIGUOUS)
  }
})

test_that("A9: a single condition still routes flat, not compound", {
  ## The carrier is an addition. Nothing that already fitted the flat shape
  ## changes shape because a larger one became available.
  for (v in .cds_vocabs) {
    got <- .cds_carry(sprintf("%s.%s='Y'", v$ds, v$flag), v)
    expect_equal(got$kind, "flat")
    expect_equal(got$where$dataset, v$ds)
    expect_equal(got$where$variable, v$flag)
    expect_equal(got$where$comparator, "EQ")
    expect_equal(unlist(got$where$value), "Y")
  }
})

test_that("A: scope -- every pinned shape above is genuinely read by this grammar", {
  ## Guards the section against going vacuously green. If a grammar change
  ## stopped reading one of these forms, that case would reserve and this
  ## count would drop, rather than the section quietly asserting nothing.
  forms <- function(v) c(
    sprintf("%s.%s='Y' AND %s.%s='C1'", v$ds, v$flag, v$ds, v$cat),
    sprintf("%s.%s='Y' OR %s.%s='C1'", v$ds, v$flag, v$ds, v$cat),
    sprintf("%s.%s='Y' AND (%s.%s='C1' OR %s.%s='C2')",
            v$ds, v$flag, v$ds, v$cat, v$ds, v$cat),
    sprintf("(%s.%s='Y', %s.%s='C1')", v$ds, v$flag, v$ds, v$cat),
    sprintf("%s.%s BETWEEN 1 AND 10", v$ds, v$num),
    sprintf("%s.%s (when %s='C1' AND %s='Y' AND %s='Y')",
            v$ds, v$txt, v$cat, v$presp, v$occur),
    sprintf("%s.%s='Y' AND %s.%s='Y'", v$ds, v$flag, v$foreign, v$fflag),
    sprintf("(%s.%s='Y' OR %s.%s='Y') AND %s.%s='C1'",
            v$foreign, v$fflag, v$ds, v$flag, v$foreign, v$fcat))

  carried <- unlist(lapply(.cds_vocabs, function(v) {
    vapply(forms(v), function(a) .cds_carry(a, v)$kind, character(1))
  }), use.names = FALSE)
  expect_equal(sum(carried == "compound"), 16L)
})

# ---- B: what stays reserved ------------------------------------------------

test_that("B1: a negation reserves -- nothing downstream executes one", {
  for (v in .cds_vocabs) {
    got <- .cds_carry(sprintf("NOT %s.%s='Y'", v$ds, v$flag), v)
    expect_equal(got$kind, "unresolved")
  }
})

test_that("B2: a partly readable expression reserves rather than carrying its readable half", {
  ## Carrying `A` alone out of `A AND <unreadable>` restricts by LESS than was
  ## written, and says nothing about the difference.
  for (v in .cds_vocabs) {
    anns <- c(sprintf("%s.%s='Y' AND", v$ds, v$flag),
              sprintf("%s.%s='Y' AND ()", v$ds, v$flag),
              sprintf("%s.%s='Y' AND (%s.%s='C1'", v$ds, v$flag, v$ds, v$cat))
    for (ann in anns) {
      expect_equal(.cds_carry(ann, v)$kind, "unresolved")
    }
  }
})

test_that("B3: an envelope naming a variable the spec does not carry reserves whole", {
  ## PR3's all-or-none rule, restated at the carrier: the readable clauses are
  ## not carried on their own.
  for (v in .cds_vocabs) {
    got <- .cds_carry(sprintf("%s.%s (when %s='C1' AND ZZABSENT='Y')",
                              v$ds, v$txt, v$cat), v)
    expect_equal(got$kind, "unresolved")
  }
})

test_that("B4: a compound is NOT carried when the annotation also states a rule the filter cannot implement", {
  ## The carrier must not route around the reservation. `.row_restriction()` is
  ## the one reading both row builders share, and the instruction check lives
  ## there rather than in the flat wrapper alone -- otherwise the row builders
  ## would carry a compound whose annotation says something no filter can do.
  ##
  ## Without this, the filter computes over a different population than the one
  ## written and nothing on the analysis says so: exactly the failure that
  ## reserving exists to prevent.
  for (v in .cds_vocabs) {
    ann <- sprintf(
      "%s.%s='Y' AND %s.%s='C1' (a subject with no %s record is a failure)",
      v$ds, v$flag, v$ds, v$cat, v$ds)

    ## The filter itself reads perfectly -- that is the whole point.
    plain <- .cds_carry(sprintf("%s.%s='Y' AND %s.%s='C1'",
                                v$ds, v$flag, v$ds, v$cat), v)
    expect_equal(plain$kind, "compound")

    ## With the instruction attached, it reserves instead of being carried.
    expect_equal(.cds_carry(ann, v)$kind, "unresolved")
  }
})

# ---- C: one restriction, whatever brought it -------------------------------

.cds_lookup <- function(v) {
  pairs <- .cds_pairs(v)
  out <- lapply(pairs, function(pair) {
    parts <- strsplit(pair, ".", fixed = TRUE)[[1]]
    list(dataset = parts[[1]], variable = parts[[2]], type = "Char",
         codelist = "")
  })
  names(out) <- pairs
  out
}

## One row, one annotation, and optionally an authoritative supplement clause.
.cds_section <- function(v, tlf, ann, supp = NULL) {
  row <- list(label = "Measured", annotation = ann, has_annot = TRUE,
              detection_method = "pattern", detection_confidence = "high",
              raw_text = "Measured")
  if (!is.null(supp)) {
    row$supplement_where <- supp
    row$detection_method <- "supplement"
  }
  list(
    tlf_number       = tlf,
    tlf_type         = "TABLE",
    title            = "Synthetic",
    population_text  = "Analysis Population",
    population_annot = sprintf("%s.%s='Y'", v$ds, v$flag),
    source_datasets  = v$ds,
    col_headers      = c("", "Group A", "Group B"),
    n_data_cols      = 2L,
    stub_rows        = list(row),
    analysis_type    = "CONTINUOUS",
    ars_method_name  = "Summary Statistics - Continuous",
    by_variable      = "TRT01A",
    enriched_rows    = list()
  )
}

.cds_compound_subsets <- function(re) {
  Filter(function(d) !is.null(d$compoundExpression), re$dataSubsets)
}

.cds_supp_and <- function(v) {
  list(compoundExpression = list(
    logicalOperator = "AND",
    whereClauses = list(
      list(condition = list(dataset = v$ds, variable = v$presp,
                            comparator = "EQ", value = list("Y"))),
      list(condition = list(dataset = v$ds, variable = v$cat,
                            comparator = "EQ", value = list("C1"))))))
}

test_that("B5: and no compound DataSubset reaches the built ARS for such a row", {
  ## The row-level claim above, followed through to the emitted file: a
  ## reservation that still minted a subset would be a reservation in name
  ## only.
  for (v in .cds_vocabs) {
    ann <- sprintf(
      "%s.%s (when %s='C1' AND %s='Y') (a subject with no %s record is a failure)",
      v$ds, v$num, v$cat, v$presp, v$ds)
    re  <- build_ars_json(list(.cds_section(v, "T-1", ann)),
                          spec_lookup = .cds_lookup(v))
    expect_length(.cds_compound_subsets(re), 0L)
  }
})

test_that("C1: an annotation compound reaches the built ARS as a compound DataSubset", {
  for (v in .cds_vocabs) {
    ann <- sprintf("%s.%s (when %s='C1' AND %s='Y')",
                   v$ds, v$num, v$cat, v$presp)
    re  <- build_ars_json(list(.cds_section(v, "T-1", ann)),
                          spec_lookup = .cds_lookup(v))
    cmp <- .cds_compound_subsets(re)
    expect_length(cmp, 1L)
    ## Guarded, so a regression emitting no compound subset FAILS on the shape
    ## rather than erroring out before the shape is ever compared.
    got <- if (length(cmp) == 1L) {
      .cds_shape(list(compoundExpression = cmp[[1]]$compoundExpression))
    } else {
      "ABSENT"
    }
    expect_equal(got, sprintf("AND(%s, %s)",
                              .cds_atom(v$ds, v$cat, "EQ", "C1"),
                              .cds_atom(v$ds, v$presp, "EQ", "Y")))
  }
})

test_that("C2: the same restriction by annotation and by supplement is ONE subset", {
  ## Provenance is not identity. DataSubsets de-duplicate on what they FILTER
  ## (`.where_signature()`), so a clause the shell states and the supplement
  ## also states must not mint two definitions of the same population.
  for (v in .cds_vocabs) {
    ann <- sprintf("%s.%s='Y' AND %s.%s='C1'", v$ds, v$presp, v$ds, v$cat)
    re  <- build_ars_json(
      list(.cds_section(v, "T-1", ann),
           .cds_section(v, "T-2", sprintf("%s.%s", v$ds, v$num),
                        supp = .cds_supp_and(v))),
      spec_lookup = .cds_lookup(v))

    cmp <- .cds_compound_subsets(re)
    expect_length(cmp, 1L)

    ## And both analyses genuinely point at it -- one subset because they
    ## agree, not because one of them lost its filter.
    used <- vapply(re$analyses, function(a) {
      as.character(a$dataSubsetId)[[1]]
    }, character(1))
    shared <- if (length(cmp) == 1L) cmp[[1]]$id else NA_character_
    expect_equal(sum(used == shared), 2L)
  }
})

test_that("C3: annotation and supplement clauses agree as WhereClauses and as masks", {
  ## C2 proves they are one entity. This proves the entity is the right one:
  ## same shape, same signature, same rows kept.
  for (v in .cds_vocabs) {
    df <- data.frame(USUBJID = paste0("S", 1:4), stringsAsFactors = FALSE)
    df[[v$presp]] <- c("Y", "Y", "N", "N")
    df[[v$cat]]   <- c("C1", "C2", "C1", "C2")

    from_ann <- .cds_carry(sprintf("%s.%s='Y' AND %s.%s='C1'",
                                   v$ds, v$presp, v$ds, v$cat), v)$where
    from_sup <- .cds_supp_and(v)

    expect_equal(.cds_shape(from_ann), .cds_shape(from_sup))
    expect_equal(
      .where_signature(list(compoundExpression = from_ann$compoundExpression)),
      .where_signature(list(compoundExpression = from_sup$compoundExpression)))
    expect_equal(.eval_where_clause(df, from_ann),
                 .eval_where_clause(df, from_sup))
    expect_equal(sum(.eval_where_clause(df, from_ann)), 1L)
  }
})

test_that("C4: an authoritative supplement clause is not overwritten by the annotation", {
  ## Precedence is unchanged by the carrier. The annotation here states a
  ## DIFFERENT restriction, so if the annotation path had taken over, the
  ## emitted subset would say so.
  for (v in .cds_vocabs) {
    supp <- list(condition = list(dataset = v$ds, variable = v$presp,
                                  comparator = "EQ", value = list("Y")))
    ann  <- sprintf("%s.%s='C1' AND %s.%s='Y'", v$ds, v$cat, v$ds, v$occur)
    re   <- build_ars_json(list(.cds_section(v, "T-1", ann, supp = supp)),
                           spec_lookup = .cds_lookup(v))

    expect_length(.cds_compound_subsets(re), 0L)
    conds <- Filter(function(d) !is.null(d$condition), re$dataSubsets)
    expect_length(conds, 1L)
    ## Guarded: a regression that emits the wrong NUMBER of subsets should
    ## fail on the variable too, not error before reaching it.
    filtered_on <- if (length(conds) == 1L) {
      conds[[1]]$condition$variable
    } else {
      NA_character_
    }
    expect_equal(filtered_on, v$presp)
  }
})

# ---- D: executed, hand-written and emitted filtering agree ------------------

.cds_adam <- function(v, envir = parent.frame()) {
  td <- withr::local_tempdir(.local_envir = envir)

  ## One row satisfying every clause, then one row failing each clause on its
  ## own, then one failing all three. A mask that ignores any single clause
  ## keeps a different set.
  head_df <- data.frame(USUBJID = paste0("S", 1:5), stringsAsFactors = FALSE)
  head_df[[v$cat]]   <- c("C1", "C2", "C1", "C1", "C2")
  head_df[[v$presp]] <- c("Y",  "Y",  "N",  "Y",  "N")
  head_df[[v$occur]] <- c("Y",  "Y",  "Y",  "N",  "N")
  head_df[[v$num]]   <- c(5, 6, 7, 8, 9)
  utils::write.csv(head_df, file.path(td, paste0(tolower(v$ds), ".csv")),
                   row.names = FALSE)

  foreign_df <- data.frame(USUBJID = paste0("S", 1:5), stringsAsFactors = FALSE)
  foreign_df[[v$fflag]] <- c("Y", "N", "Y", "N", "Y")
  foreign_df[[v$fcat]]  <- "C1"
  utils::write.csv(foreign_df, file.path(td, paste0(tolower(v$foreign), ".csv")),
                   row.names = FALSE)
  td
}

test_that("D1: engine mask == hand-written mask == emitted-code mask", {
  for (v in .cds_vocabs) {
    where <- .cds_carry(sprintf("%s.%s (when %s='C1' AND %s='Y' AND %s='Y')",
                                v$ds, v$txt, v$cat, v$presp, v$occur), v)$where

    store <- .adam_store(.cds_adam(v))
    df    <- store$get(v$ds)

    engine  <- .where_keep_mask(df, v$ds, where, store, "USUBJID")
    hand    <- df[[v$cat]] == "C1" & df[[v$presp]] == "Y" & df[[v$occur]] == "Y"
    emitted <- eval(parse(text = where_to_filter_expr(where)), envir = df)

    expect_false(.is_block(engine))
    expect_equal(engine, hand)
    expect_equal(emitted, hand)

    ## Not vacuous, and every clause is doing work: exactly one row survives,
    ## and dropping any single clause would keep more.
    expect_equal(sum(hand), 1L)
    expect_gt(sum(df[[v$presp]] == "Y" & df[[v$occur]] == "Y"), sum(hand))
    expect_gt(sum(df[[v$cat]] == "C1" & df[[v$occur]] == "Y"), sum(hand))
    expect_gt(sum(df[[v$cat]] == "C1" & df[[v$presp]] == "Y"), sum(hand))
  }
})

test_that("D2: a mixed-dataset compound executes as row-wise here plus membership there", {
  for (v in .cds_vocabs) {
    where <- .cds_carry(sprintf("%s.%s='C1' AND %s.%s='Y'",
                                v$ds, v$cat, v$foreign, v$fflag), v)$where

    store <- .adam_store(.cds_adam(v))
    df    <- store$get(v$ds)
    ref   <- store$get(v$foreign)

    engine <- .where_keep_mask(df, v$ds, where, store, "USUBJID")
    hand   <- df[[v$cat]] == "C1" &
      df$USUBJID %in% ref$USUBJID[ref[[v$fflag]] == "Y"]

    expect_false(.is_block(engine))
    expect_equal(engine, hand)
    expect_equal(sum(hand), 2L)
  }
})

test_that("D3: the ambiguous shape blocks at execution rather than filtering by a guess", {
  for (v in .cds_vocabs) {
    where <- .cds_carry(sprintf("(%s.%s='Y' OR %s.%s='Y') AND %s.%s='C1'",
                                v$foreign, v$fflag, v$ds, v$flag,
                                v$foreign, v$fcat), v)$where

    store <- .adam_store(.cds_adam(v))
    df    <- store$get(v$ds)

    expect_true(.is_block(.where_keep_mask(df, v$ds, where, store, "USUBJID")))
  }
})
