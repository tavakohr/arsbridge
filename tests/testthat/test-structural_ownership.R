## Who owns a block's rows -- structure, or the method?
##
## A block claims the rows beneath it when the authored layout proves they are
## its levels. Before this, the claim came from the METHOD the row was given,
## and the method could be chosen by a population filter: so a filter, three
## steps removed, decided which rows a table's block owned.
##
## These go through the BUILDER, because that is where the claim is acted on --
## a claimed row loses its own analysis and becomes a level slot of the row
## above. Identifiers are invented throughout: ADQX.OUTGRP and ADQX.EVALFL
## exist in no study in this repo.

.so_lookup <- function() list(
  "ADQX.OUTGRP" = list(dataset = "ADQX", variable = "OUTGRP",
                       type = "Char", codelist = ""),
  "ADQX.EVALFL" = list(dataset = "ADQX", variable = "EVALFL",
                       type = "Char", codelist = "NY"),
  "ADQX.TRT01A" = list(dataset = "ADQX", variable = "TRT01A",
                       type = "Char", codelist = ""),
  "ADSL.SAFFL"  = list(dataset = "ADSL", variable = "SAFFL",
                       type = "Char", codelist = "NY"))

.so_cond <- function(dataset, variable, value, comparator = "EQ") {
  list(condition = list(dataset = dataset, variable = variable,
                        comparator = comparator, value = as.list(value)))
}
.so_and <- function(...) {
  list(compoundExpression = list(logicalOperator = "AND",
                                 whereClauses = list(...)))
}

## A section carrying whatever rows a test needs.
.so_section <- function(rows, ...) {
  modifyList(list(
    tlf_number = "T-99-9-9", tlf_type = "TABLE",
    title = "Outcome Group Summary",
    population_text = "Analysis Population",
    population_annot = "ADSL.SAFFL='Y'",
    source_datasets = "ADQX",
    col_headers = c("", "Drug", "Placebo"), n_data_cols = 2L,
    stub_rows = rows,
    analysis_type = "CATEGORICAL", ars_method_name = "Count and Percentage",
    by_variable = "TRT01A", by_variable_dataset = "ADQX",
    enriched_rows = list()), list(...))
}

.so_build <- function(rows, ...) {
  suppressMessages(suppressWarnings(
    build_ars_json(list(.so_section(rows, ...)), spec_lookup = .so_lookup())))
}

.so_layout <- function(re) re$outputs[[1]][["_meta"]][["shell_layout"]]
.so_kinds  <- function(re) {
  vapply(.so_layout(re), function(e) as.character(e$kind %||% "-"), character(1))
}

## The common restriction a supplement restates on every leaf of a block.
.so_common <- function() .so_cond("ADQX", "EVALFL", "Y")

.so_parent <- function(label = "Outcome group", variable = "OUTGRP",
                       where = .so_common(), ...) {
  modifyList(list(
    label = label, annotation = paste0("ADQX.", variable), has_annot = TRUE,
    detection_method = "supplement", raw_text = label,
    supplement_where = where), list(...))
}

## One row beneath a block. `code` may name several values, in which case the
## restriction is set membership rather than equality -- the two are different
## claims and the row must state the one it means.
.so_level <- function(label, code, variable = "OUTGRP",
                      common = .so_common()) {
  own <- if (length(code) > 1L) {
    .so_cond("ADQX", variable, code, comparator = "IN")
  } else {
    .so_cond("ADQX", variable, code)
  }
  text <- if (length(code) > 1L) {
    sprintf("ADQX.%s in (%s)", variable,
            paste(sprintf("'%s'", code), collapse = ","))
  } else {
    sprintf("ADQX.%s='%s'", variable, code)
  }
  list(label = label, annotation = text, has_annot = TRUE,
       detection_method = "supplement", raw_text = paste0("  ", label),
       supplement_where = if (is.null(common)) own else .so_and(common, own))
}


## ---------------------------------------------------------------------------
## The regression this PR exists for
## ---------------------------------------------------------------------------

test_that("typed supplement leaves under a categorical parent become level rows", {
  ## Structure was read from the annotation STRING while the restriction the
  ## row computes under had been supplied as a typed clause. The two are the
  ## same restriction; a reader that sees only one decides layout on whichever
  ## channel the author happened to use, which is not a property of the table.
  re <- .so_build(list(
    .so_parent(),
    .so_level("Improved",  "IMP"),
    .so_level("Unchanged", "UNC"),
    .so_level("Worsened",  "WOR")))

  expect_equal(.so_kinds(re),
               c("categorical_block", "level_row", "level_row", "level_row"))
  expect_length(re$analyses, 1L)

  lay <- .so_layout(re)
  parent <- lay[[1]]
  for (e in lay[-1]) expect_identical(e$analysis_id, parent$analysis_id)
  expect_setequal(vapply(lay[-1], function(e) as.character(e$level),
                         character(1)),
                  c("IMP", "UNC", "WOR"))

  ## The common restriction the leaves declared is not lost by the collapse:
  ## it is what the surviving parent analysis computes under.
  subset_id <- as.character(re$analyses[[1]]$dataSubsetId %||% "")
  expect_true(nzchar(subset_id))
  ds <- Filter(function(d) identical(as.character(d$id), subset_id),
               re$dataSubsets)
  expect_length(ds, 1L)
  expect_equal(.where_refs(ds[[1]]), "ADQX.EVALFL")
})

test_that("the same block written as annotations gives the same layout", {
  ## Channel independence, through the builder this time. Same restrictions,
  ## stated in the shell rather than supplied as typed clauses.
  authored_parent <- list(
    label = "Outcome group", annotation = "ADQX.OUTGRP where ADQX.EVALFL='Y'",
    has_annot = TRUE, raw_text = "Outcome group")
  authored_level <- function(label, code) list(
    label = label,
    annotation = sprintf("ADQX.OUTGRP where ADQX.EVALFL='Y' and ADQX.OUTGRP='%s'",
                         code),
    has_annot = TRUE, raw_text = paste0("  ", label))

  typed <- .so_build(list(.so_parent(),
                          .so_level("Improved",  "IMP"),
                          .so_level("Unchanged", "UNC")))
  authored <- .so_build(list(authored_parent,
                             authored_level("Improved",  "IMP"),
                             authored_level("Unchanged", "UNC")))

  expect_equal(.so_kinds(typed),
               c("categorical_block", "level_row", "level_row"))
  expect_equal(.so_kinds(authored), .so_kinds(typed))
  expect_equal(length(authored$analyses), length(typed$analyses))
})

test_that("an authored set is subdivided by the rows beneath it", {
  ## The parent declares the domain in play and the rows beneath carve it up --
  ## one naming two of the values, the other the third. Together they cover the
  ## declared set exactly, which is what makes this a subdivision rather than a
  ## selection.
  re <- .so_build(list(
    .so_parent(label = "Listed outcome groups",
               where = .so_cond("ADQX", "OUTGRP",
                                c("IMP", "UNC", "WOR"), comparator = "IN")),
    .so_level("Improved or unchanged", c("IMP", "UNC"), common = NULL),
    .so_level("Worsened", "WOR", common = NULL)))

  expect_equal(.so_kinds(re)[[1]], "categorical_block")
  expect_true(all(.so_kinds(re)[-1] == "level_row"))
  expect_length(re$analyses, 1L)
})

test_that("NEGATIVE CONTROL: rows that do not subdivide keep their own analyses", {
  ## The mirror of the test above. These children are not a subdivision of the
  ## parent's declared set -- one of them names a value the parent excluded --
  ## so nothing here may be folded into the block.
  re <- .so_build(list(
    .so_parent(label = "Listed outcome groups",
               where = .so_cond("ADQX", "OUTGRP",
                                c("IMP", "UNC"), comparator = "IN")),
    .so_level("Improved", "IMP", common = NULL),
    .so_level("Worsened", "WOR", common = NULL)))

  expect_false(any(.so_kinds(re) == "level_row"))
  expect_gt(length(re$analyses), 1L)
})


## ---------------------------------------------------------------------------
## CONTRACT: structure does not read the method
## ---------------------------------------------------------------------------

test_that("CONTRACT: the same structure survives a different method identity", {
  ## Identical rows and identical restrictions, three different methods on the
  ## parent. Expansion, the parent/child relationship and the level collapse
  ## must be the same in all three -- the structural stage is not allowed to
  ## consult which method the row was given.
  arms <- c("MTH_COUNT_AND_PERCENTAGE", "MTH_AE_FREQUENCY_COUNT",
            "MTH_SUBJECT_COUNT")
  built <- lapply(arms, function(mid) {
    .so_build(list(
      .so_parent(supplement_method_id = mid),
      .so_level("Improved",  "IMP"),
      .so_level("Unchanged", "UNC"),
      .so_level("Worsened",  "WOR")))
  })

  ## Non-vacuous: the three arms really did get three different methods.
  got <- vapply(built, function(re) as.character(re$analyses[[1]]$methodId),
                character(1))
  expect_equal(got, arms)

  ## And the structure is identical across all three.
  for (re in built) {
    expect_equal(.so_kinds(re),
                 c("categorical_block", "level_row", "level_row", "level_row"))
    expect_length(re$analyses, 1L)
  }
})

test_that("CONTRACT: a method identity the catalogue does not know changes nothing", {
  ## The point of the paired test is the structural component, so the method
  ## need not even be one that could compute. An unknown id falls back, and the
  ## layout is unmoved either way.
  known   <- .so_build(list(.so_parent(), .so_level("Improved", "IMP"),
                            .so_level("Unchanged", "UNC")))
  unknown <- .so_build(list(
    .so_parent(supplement_method_id = "MTH_NOT_IN_THE_CATALOGUE"),
    .so_level("Improved", "IMP"), .so_level("Unchanged", "UNC")))

  expect_equal(.so_kinds(unknown), .so_kinds(known))
  expect_equal(length(unknown$analyses), length(known$analyses))
})


## ---------------------------------------------------------------------------
## MUTATION TARGET: ownership derived from the method, not from structure
## ---------------------------------------------------------------------------

test_that("A-23 witness: the collapse still discards a common child restriction", {
  ## Recorded, not yet corrected. The parent states no restriction; each row
  ## beneath states one the parent does not. They collapse anyway -- so the
  ## rows are displayed from a distribution computed over records they
  ## excluded, and nothing in the output says so.
  ##
  ## This test pins the DEFECT, deliberately. PR5b-2 is a structural handoff
  ## and does not change what anything computes, so the behaviour below is
  ## today's behaviour and must stay until the correction lands with its own
  ## before/after proof. What has changed is that the structural reader now
  ## KNOWS: the same walk that proves the layout reports `residue_match` as
  ## FALSE, and carries the term a faithful collapse would have to hoist.
  rows <- list(.so_parent(where = NULL),
               .so_level("Improved",  "IMP"),
               .so_level("Unchanged", "UNC"))
  re <- .so_build(rows)

  ## Behaviour today: one analysis, and the children's ADQX.EVALFL='Y' is gone.
  expect_equal(.so_kinds(re),
               c("categorical_block", "level_row", "level_row"))
  expect_length(re$analyses, 1L)
  expect_equal(as.character(re$analyses[[1]]$dataSubsetId %||% ""), "")

  ## What the reader knows about that collapse, and does not act on.
  view <- .row_restriction_view(rows)
  rel <- .restriction_partition_relation(view[[1]], view[-1], "ADQX", "OUTGRP")
  expect_equal(rel$status, "proved")
  expect_false(rel$residue_match)
  expect_equal(rel$common_residue, "ADQX.EVALFL EQ [Y]")
})

test_that("a faithful collapse is reported as faithful", {
  ## The control for the test above: when the parent states the same thing its
  ## children do, nothing is discarded and the verdict says so. Without this,
  ## a `residue_match` that answered FALSE unconditionally would look correct.
  rows <- list(.so_parent(),
               .so_level("Improved",  "IMP"),
               .so_level("Unchanged", "UNC"))
  view <- .row_restriction_view(rows)
  rel <- .restriction_partition_relation(view[[1]], view[-1], "ADQX", "OUTGRP")

  expect_equal(rel$status, "proved")
  expect_true(rel$residue_match)
  expect_equal(rel$common_residue, "ADQX.EVALFL EQ [Y]")
})

test_that("children that disagree with each other share no common residue", {
  ## Nothing can be hoisted here, and the answer must not depend on which row
  ## came first, so neither row's restriction may be reported as the common one.
  rows <- list(
    .so_parent(where = NULL),
    .so_level("Improved", "IMP"),
    list(label = "Unchanged",
         annotation = "ADQX.OUTGRP='UNC'", has_annot = TRUE,
         detection_method = "supplement", raw_text = "  Unchanged",
         supplement_where = .so_and(.so_cond("ADQX", "EVALFL", "N"),
                                    .so_cond("ADQX", "OUTGRP", "UNC"))))
  view <- .row_restriction_view(rows)
  forward  <- .restriction_partition_relation(view[[1]], view[-1],
                                              "ADQX", "OUTGRP")
  reversed <- .restriction_partition_relation(view[[1]], rev(view[-1]),
                                              "ADQX", "OUTGRP")

  expect_false(forward$residue_match)
  expect_null(forward$common_residue)
  expect_equal(reversed$residue_match, forward$residue_match)
  expect_equal(reversed$common_residue, forward$common_residue)
})

test_that("MUTATION TARGET: a row pinned to one value is not an axis for the next", {
  ## The second discriminating shape, and it needs no residue reasoning. A row
  ## that selects ONE value of its variable is a level, not that variable's
  ## axis -- so the row beneath it is a sibling, not its child.
  re <- .so_build(list(
    .so_parent(label = "Improved responders",
               where = .so_cond("ADQX", "OUTGRP", "IMP")),
    .so_level("Unchanged responders", "UNC", common = NULL)))

  expect_false(any(.so_kinds(re) == "level_row"))
  expect_equal(length(re$analyses), 2L)
})
