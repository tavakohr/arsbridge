## Annotation-defined column axis: per-column filter annotations in TABLE
## header cells ("Cohort A (N=XX) ADSL.COHORTN=1", "... is missing") become
## per-level group definitions that flow shell -> ARS groups[] -> resolver ->
## emitted {cards} code / legacy executor -> ARD, so a merged column (an
## "Unknown" bucket for missing values) needs no ADaM change.

.cg_fixture <- function() test_path("fixtures/annotated_shell_column_groups.docx")

## Small ADSL with a value for each group column, plus one row (COHORTN=9)
## that no column claims -- drives the unmatched-rows WARN.
.cg_adam <- function(td) {
  utils::write.csv(data.frame(
    USUBJID = sprintf("S%02d", 1:8),
    COHORTN = c(1, 1, 1, 2, 2, NA, NA, 9),
    SCRNFL  = rep("Y", 8),
    SEX     = c("M", "F", "M", "F", "M", "F", "M", "F"),
    AGE     = c(40, 50, 60, 55, 65, 45, 70, 60),
    stringsAsFactors = FALSE
  ), file.path(td, "adsl.csv"), row.names = FALSE)
}

## Hand-built spec mirroring what build_ars_json emits for the fixture.
.cg_spec <- function() {
  list(
    analysisSets = list(list(id = "AS_SCR", name = "Screened",
      condition = list(dataset = "ADSL", variable = "SCRNFL",
                       comparator = "EQ", value = list("Y")))),
    dataSubsets = list(),
    analysisGroupings = list(list(
      id = "GF_COHORTN", name = "COHORTN", groupingVariable = "COHORTN",
      groupingDataset = "ADSL", dataDriven = FALSE,
      groups = list(
        list(id = "GRP_A", name = "Cohort A", label = "Cohort A", order = 1,
             condition = parse_where_clause("ADSL.COHORTN=1")),
        list(id = "GRP_B", name = "Cohort B", label = "Cohort B", order = 2,
             condition = parse_where_clause("ADSL.COHORTN=2")),
        list(id = "GRP_U", name = "Unknown Cohort", label = "Unknown Cohort",
             order = 3,
             condition = parse_where_clause("ADSL.COHORTN is missing"))))),
    methods = list(list(
      id = "MTH_COUNT_AND_PERCENTAGE",
      name = "Count and Percentage"
    )),
    outputs = list(list(id = "OUT_T1", name = "T-1",
                        referencedAnalysisIds = list("AN_SEX"))),
    analyses = list(list(
      id = "AN_SEX", methodId = "MTH_COUNT_AND_PERCENTAGE",
      label = "Sex", dataset = "ADSL", variable = "SEX",
      analysisVariable = list(dataset = "ADSL", variable = "SEX"),
      analysisSetId = "AS_SCR", dataSubsetId = "",
      orderedGroupings = list(list(order = 1, groupingId = "GF_COHORTN",
                                   resultsByGroup = TRUE)),
      includeTotal = TRUE))
  )
}

.cg_levels <- function(ard) {
  lv <- vapply(ard$group1_level, function(x)
    if (length(x)) as.character(x[[1]]) else NA_character_, character(1))
  unique(lv[!is.na(lv)])
}

.cg_n <- function(ard, level) {
  g <- vapply(ard$group1_level, function(x)
    if (length(x)) as.character(x[[1]]) else NA_character_, character(1))
  rows <- !is.na(g) & g == level & ard$stat_name == "n"
  sum(vapply(ard$stat[rows], function(x) as.numeric(x[[1]]), numeric(1)))
}

## --- annotation grammar -----------------------------------------------------

test_that("the positive is-missing form is captured whole, not truncated", {
  s <- split_label_annotation("Unknown Cohort (N=XX) ADSL.COHORTN is missing")
  expect_equal(s$label, "Unknown Cohort (N=XX)")
  expect_equal(s$annotation, "ADSL.COHORTN is missing")

  ## The negative form still matches its own branch.
  s2 <- split_label_annotation("Any reason  ADSL.DCSREAS not missing")
  expect_equal(s2$annotation, "ADSL.DCSREAS not missing")
})

test_that("parenthesized IN lists are captured and quote-canonicalized", {
  s <- split_label_annotation("Race  ADSL.RACE IN ('WHITE','ASIAN')")
  expect_equal(s$annotation, "ADSL.RACE IN ('WHITE','ASIAN')")

  dq <- split_label_annotation('Race  ADSL.RACE NOT IN ("OTHER","")')
  expect_equal(dq$annotation, "ADSL.RACE NOT IN ('OTHER','')")
})

## --- parser -> column_groups ------------------------------------------------

test_that("annotated header cells become ordered column-group definitions", {
  secs <- parse_shell_docx(.cg_fixture())
  expect_length(secs, 1)
  cg <- secs[[1]]$column_groups

  expect_equal(cg$variable, "COHORTN")
  expect_equal(cg$dataset, "ADSL")
  expect_equal(vapply(cg$groups, `[[`, character(1), "label"),
               c("Cohort A", "Cohort B", "Unknown Cohort"))
  expect_equal(vapply(cg$groups, `[[`, character(1), "annotation"),
               c("ADSL.COHORTN=1", "ADSL.COHORTN=2",
                 "ADSL.COHORTN is missing"))

  ## The Total header filters SCRNFL, not COHORTN -- excluded from the
  ## groups, but it marks the overall column.
  expect_true(isTRUE(secs[[1]]$include_total_hint))
  ## The header annotation claims the column axis.
  expect_equal(secs[[1]]$column_annotation, "ADSL.COHORTN")
  ## Display labels no longer carry the annotation text.
  expect_true(any(secs[[1]]$col_headers == "Cohort A (N=XX)"))
})

test_that("an axis header that fails to parse is kept as a reserved level", {
  diag_reset()
  ## Two parseable COHORTN headers set the axis; a third names COHORTN but
  ## uses an unsupported operator.
  ##
  ## That third column used to be DROPPED from the groups, which silently
  ## reshapes the axis: the remaining levels close the gap, and a column ends
  ## up showing a different subgroup than its header claims. It is now kept
  ## and marked, so the column holds its place and validation reserves
  ## whatever computes through the grouping.
  sec <- list(
    tlf_number = "T-14-9-9", tlf_type = "TABLE", title = "Guardrail",
    .pending_column_annotations = list(
      labels = c("Cohort A (N=XX)", "Cohort B (N=XX)", "Odd (N=XX)"),
      annotations = c("ADSL.COHORTN=1", "ADSL.COHORTN=2",
                      "ADSL.COHORTN ~= 3")))
  out <- .resolve_table_column_groups(sec)

  ## All three columns survive -- the axis keeps its shape.
  expect_length(out$column_groups$groups, 3)

  ## And the unreadable one becomes a level with NO condition and the author's
  ## text under `unresolvedCondition`, which is what the validator reads.
  out$by_variable <- "COHORTN"
  out$by_variable_dataset <- "ADSL"
  gf <- .build_grouping(out)
  expect_length(gf$groups, 3)

  marked <- Filter(function(g) !is.null(g$unresolvedCondition), gf$groups)
  expect_length(marked, 1L)
  expect_identical(marked[[1]]$label, "Odd")
  expect_identical(marked[[1]]$unresolvedCondition, "ADSL.COHORTN ~= 3")
  ## It must not also look like a level that selects records.
  expect_null(marked[[1]]$condition)
  expect_null(marked[[1]]$compoundExpression)

  ## The two readable levels are untouched.
  readable <- Filter(function(g) is.null(g$unresolvedCondition), gf$groups)
  expect_length(readable, 2L)
  expect_true(all(vapply(readable, function(g) !is.null(g$condition),
                         logical(1))))
})

## --- ARS JSON groups[] ------------------------------------------------------

test_that("column groups emit per-level groups[] with conditions", {
  sec <- list(
    tlf_number = "T-14-2-1", tlf_type = "TABLE",
    by_variable = "COHORTN", by_variable_dataset = "ADSL",
    column_groups = list(
      variable = "COHORTN", dataset = "ADSL",
      groups = list(
        list(label = "Cohort A", annotation = "ADSL.COHORTN=1", order = 1L),
        list(label = "Unknown Cohort", annotation = "ADSL.COHORTN is missing",
             order = 2L))))
  gf <- .build_grouping(sec)

  expect_length(gf$groups, 2)
  expect_equal(gf$groups[[1]]$label, "Cohort A")
  expect_equal(gf$groups[[1]]$order, 1L)
  expect_equal(gf$groups[[1]]$condition$comparator, "EQ")
  expect_equal(unlist(gf$groups[[1]]$condition$value), "1")
  ## The is-missing level carries an empty value list.
  expect_length(gf$groups[[2]]$condition$value, 0)
  expect_false(isTRUE(gf$dataDriven))

  ## A section without column groups still emits the empty array.
  plain <- .build_grouping(list(by_variable = "TRT01A",
                                by_variable_dataset = "ADSL"))
  expect_identical(plain$groups, list())
})

## --- resolver ---------------------------------------------------------------

test_that("resolve_analysis surfaces group_defs keyed by the variable", {
  spec <- .cg_spec()
  res <- resolve_analysis(spec$analyses[[1]], spec)
  expect_named(res$group_defs, "COHORTN")
  defs <- res$group_defs$COHORTN
  expect_equal(vapply(defs, `[[`, character(1), "label"),
               c("Cohort A", "Cohort B", "Unknown Cohort"))

  ## A spec whose grouping has no groups[] resolves to an empty list.
  spec2 <- .cg_spec()
  spec2$analysisGroupings[[1]]$groups <- list()
  res2 <- resolve_analysis(spec2$analyses[[1]], spec2)
  expect_length(res2$group_defs, 0)
})

## --- emitted code -----------------------------------------------------------

test_that("the emitted block derives the factor with case_when", {
  spec <- .cg_spec()
  res <- resolve_analysis(spec$analyses[[1]], spec)
  code <- arsbridge:::.emit_block(res)$code

  expect_match(code, "dplyr::case_when", fixed = TRUE)
  expect_match(code, "(is.na(COHORTN) | COHORTN == \"\") ~ \"Unknown Cohort\"",
               fixed = TRUE)
  expect_match(code,
               "levels = c(\"Cohort A\", \"Cohort B\", \"Unknown Cohort\")",
               fixed = TRUE)
  ## Parses as valid R.
  expect_silent(parse(text = code))
})

## --- end-to-end ARD (both engines) ------------------------------------------

test_that("the ARD carries the labeled columns incl. the missing bucket", {
  skip_if_not_installed("cards")
  td <- withr::local_tempdir()
  .cg_adam(td)
  ars <- file.path(td, "ars.json")
  writeLines(jsonlite::toJSON(.cg_spec(), auto_unbox = TRUE, null = "null"),
             ars)

  ard <- suppressMessages(ars_to_ard(ars, td))
  lv <- .cg_levels(ard)
  expect_true(all(c("Cohort A", "Cohort B", "Unknown Cohort") %in% lv))
  ## The raw codes never leak through as columns.
  expect_false(any(c("1", "2", "9") %in% lv))
  ## n per column: 3 / 2 / 2 (the two NA rows form the Unknown bucket).
  expect_equal(.cg_n(ard, "Cohort A"), 3)
  expect_equal(.cg_n(ard, "Cohort B"), 2)
  expect_equal(.cg_n(ard, "Unknown Cohort"), 2)

  ## The COHORTN=9 row matches no column: WARN recorded, row excluded.
  d <- ars_diagnostics()
  expect_true(any(grepl("match no column-group condition", d$problem)))

  ## Legacy executor path produces the same labeled levels.
  ard_leg <- suppressMessages(ars_to_ard(ars, td, legacy = TRUE))
  expect_setequal(.cg_levels(ard_leg), lv)
})

## --- renderer column mapping ------------------------------------------------

test_that("shell headers map onto the group labels in shell order", {
  out_obj <- list(displays = list(list(order = 1L, display = list(
    id = "D1", name = "D1", columns = list(
    list(label = " "),
    list(label = "Cohort A (N=XX)"),
    list(label = "Cohort B (N=XX)"),
    list(label = "Unknown Cohort (N=XX)"))))))
  ard <- data.frame(group1_level = c("Unknown Cohort", "Cohort B", "Cohort A"))
  lv <- build_col_levels(out_obj, ard, "group1_level", restrict = TRUE)
  expect_identical(lv, c("Cohort A", "Cohort B", "Unknown Cohort"))
})

## --- integration: shell -> ARS JSON -> groups survive the round-trip --------

test_that("spec_to_ars carries the column groups into the ARS JSON", {
  td <- withr::local_tempdir()
  res <- withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(spec_to_ars(
      shell_path     = .cg_fixture(),
      adam_spec_path = test_path("fixtures/adam_spec_rwe.xlsx"),
      output_path    = file.path(td, "re.json"),
      report_path    = file.path(td, "rep.xlsx"),
      verbose        = FALSE
    ))
  )
  spec <- jsonlite::fromJSON(res$ars_path, simplifyVector = FALSE)

  gfs <- spec$analysisGroupings
  cohort_gf <- NULL
  for (gf in gfs) {
    if (identical(gf$groupingVariable, "COHORTN")) cohort_gf <- gf
  }
  expect_false(is.null(cohort_gf))
  expect_length(cohort_gf$groups, 3)
  labels <- vapply(cohort_gf$groups, function(g) g$label, character(1))
  expect_setequal(labels, c("Cohort A", "Cohort B", "Unknown Cohort"))
  ## The is-missing level round-trips as an empty value array.
  unknown <- cohort_gf$groups[[which(labels == "Unknown Cohort")]]
  expect_length(unknown$condition$value, 0)
  ## The Total header switched includeTotal on.
  expect_true(any(vapply(spec$analyses, function(a)
    isTRUE(a$includeTotal), logical(1))))
})

## ---------------------------------------------------------------------------
## The overall column
##
## A field study delivered a workbook whose cohort columns held real numbers
## and whose Total column was still "xx" in every row. The shell annotated it
## "Total (N=XX) [ADSL.COHORTN IN (1,2)]" -- deliberately excluding a third
## displayed column, an Unknown cohort -- and the column ended up with no
## metadata at all: not a group, not a total.
##
## The rule: the annotation ON the column wins. Only when there is none is the
## scope derived, as the union of the group columns.
## ---------------------------------------------------------------------------

.ocol_resolve <- function(labels, annotations) {
  sec <- list(tlf_type = "TABLE", tlf_number = "14.1.1", title = "t",
              .pending_column_annotations = list(labels = labels,
                                                 annotations = annotations))
  suppressMessages(suppressWarnings(
    arsbridge:::.resolve_table_column_groups(sec)))
}

.ocol_labels <- c("Alpha Cohort (N=XX)", "Beta Cohort (N=XX)",
                  "Unknown Cohort (N=XX)", "Total (N=XX)")
.ocol_axis <- c("[ADSL.COHORTN=1]", "[ADSL.COHORTN=2]",
                "[ADSL.COHORTN is missing]")

test_that("an annotated Total column is scoped by its own annotation", {
  sec <- .ocol_resolve(.ocol_labels,
                       c(.ocol_axis, "[ADSL.COHORTN IN (1,2)]"))

  ## Never a group level: its scope overlaps the levels, and the emitted
  ## grouping is a first-match-wins case_when, so a Total level would be
  ## shadowed by the very columns it totals and report zero.
  expect_equal(vapply(sec$column_groups$groups, function(g) g$label, ""),
               c("Alpha Cohort", "Beta Cohort", "Unknown Cohort"))
  expect_true(isTRUE(sec$include_total_hint))
  expect_equal(sec$total_label, "Total")
  ## Exactly what the shell said -- which here excludes a displayed column.
  expect_equal(where_to_filter_expr(sec$total_condition),
               'COHORTN %in% c("1", "2")')
})

test_that("an unannotated Total column is the union of the group columns", {
  sec <- .ocol_resolve(.ocol_labels, c(.ocol_axis, ""))
  expect_true(isTRUE(sec$include_total_hint))
  expect_equal(sec$total_label, "Total")
  ## Derived, because there was nothing authored to honour. Narrower than
  ## "the analysis set" whenever the groups do not cover the population.
  expr <- where_to_filter_expr(sec$total_condition)
  expect_match(expr, 'COHORTN %in% c("1")', fixed = TRUE)
  expect_match(expr, 'COHORTN %in% c("2")', fixed = TRUE)
  expect_match(expr, "|", fixed = TRUE)
})

test_that("a Total column may be scoped by a different variable", {
  sec <- .ocol_resolve(.ocol_labels, c(.ocol_axis, '[ADSL.SAFFL="Y"]'))
  expect_length(sec$column_groups$groups, 3)
  expect_equal(where_to_filter_expr(sec$total_condition), 'SAFFL %in% c("Y")')
})

test_that("a Total column is seen on a data-driven axis too", {
  ## The ordinary treatment table: the axis is declared on the stub header
  ## ("[columns -> ADSL.TRT01A]"), so no COLUMN header carries an annotation
  ## and there are no group conditions to union. The Total column is still
  ## displayed, and a displayed column that produces nothing is the failure
  ## this whole area exists to prevent.
  sec <- .ocol_resolve(
    c("Placebo", "Drug 10 mg", "Drug 20 mg", "Total"),
    rep("", 4)
  )

  expect_true(isTRUE(sec$include_total_hint))
  expect_equal(sec$total_label, "Total")
  ## Nothing to union and nothing authored, so the pass is scoped by the
  ## analysis set alone -- which is the only remaining meaning of "Total".
  expect_null(sec$total_condition)
  ## No column carried a condition, so the axis stays data-driven: this must
  ## not invent group levels out of the header labels.
  expect_null(sec$column_groups)
})

test_that("a data-driven axis with no Total column stays silent", {
  sec <- .ocol_resolve(c("Placebo", "Drug 10 mg", "Drug 20 mg"), rep("", 3))

  expect_false(isTRUE(sec$include_total_hint))
  expect_null(sec$total_label)
  expect_null(sec$total_condition)
})

test_that("Overall counts as an overall column, and no total column is silent", {
  overall <- .ocol_resolve(c(.ocol_labels[1:3], "Overall (N=XX)"),
                           c(.ocol_axis, "[ADSL.COHORTN IN (1,2)]"))
  expect_true(isTRUE(overall$include_total_hint))
  expect_equal(overall$total_label, "Overall")

  none <- .ocol_resolve(.ocol_labels[1:3], .ocol_axis)
  expect_false(isTRUE(none$include_total_hint))
  expect_null(none$total_condition)
  expect_length(none$column_groups$groups, 3)
})

## ---------------------------------------------------------------------------
## The gate: a displayed Total column must be a producible Total column
## ---------------------------------------------------------------------------

test_that("a displayed overall column with no metadata is a blocking finding", {
  ## Exactly what shipped: numbers in every cohort column, placeholders in
  ## every Total cell, and not one diagnostic saying a displayed column had
  ## been dropped. It is a FAIL because the deliverable is wrong, not merely
  ## incomplete -- and nothing downstream can tell.
  diag_reset()
  sec <- list(tlf_type = "TABLE", tlf_number = "14.1.1",
              title = "Summary of Subject Status",
              col_headers = c("Alpha Cohort (N=XX)", "Beta Cohort (N=XX)",
                              "Unknown Cohort (N=XX)", "Total (N=XX)"),
              include_total = FALSE)
  out <- suppressMessages(arsbridge:::.check_overall_column(sec))

  records <- diag_records()
  hit <- records[records$severity == "FAIL" &
                   grepl("overall column", records$problem), , drop = FALSE]
  expect_equal(nrow(hit), 1L)
  expect_match(hit$problem[[1]], "Total", fixed = TRUE)
  expect_match(hit$problem[[1]], "14.1.1", fixed = TRUE)
  ## The fix names both ways out, so the annotation can be corrected or
  ## deliberately left off.
  expect_match(hit$action[[1]], "IN (1,2)", fixed = TRUE)
  expect_null(out$total_scope)
})

test_that("a producible overall column records what its scope means", {
  ## Guidance rule 7: a reviewer must be able to see what Total MEANS without
  ## reading a WhereClause, and compare it against the shell's own words.
  diag_reset()
  scoped <- suppressMessages(arsbridge:::.check_overall_column(list(
    tlf_type = "TABLE", tlf_number = "14.1.1", title = "t",
    col_headers = c("Cohort A (N=XX)", "Total (N=XX)"),
    include_total = TRUE,
    total_condition = parse_where_clause("[ADSL.COHORTN IN (1,2)]"))))
  expect_equal(scoped$total_scope, "condition_based")

  derived <- suppressMessages(arsbridge:::.check_overall_column(list(
    tlf_type = "TABLE", tlf_number = "14.1.2", title = "t",
    col_headers = c("Cohort A (N=XX)", "Total (N=XX)"),
    include_total = TRUE, total_condition = NULL)))
  expect_equal(derived$total_scope, "analysis_set")

  ## Neither is a finding: both are producible.
  expect_equal(sum(diag_records()$severity == "FAIL"), 0L)
})

test_that("a table with no overall column is left alone", {
  diag_reset()
  sec <- list(tlf_type = "TABLE", tlf_number = "14.1.3", title = "t",
              col_headers = c("Cohort A (N=XX)", "Cohort B (N=XX)"),
              include_total = FALSE)
  out <- suppressMessages(arsbridge:::.check_overall_column(sec))
  expect_null(out$total_scope)
  expect_equal(nrow(diag_records()), 0L)
})
