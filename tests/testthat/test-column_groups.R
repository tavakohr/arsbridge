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
    methods = list(),
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

test_that("an axis header that fails to parse is reported, not silently dropped", {
  diag_reset()
  ## Two parseable COHORTN headers set the axis; a third names COHORTN but
  ## uses an unsupported operator, so it drops out of the groups.
  sec <- list(
    tlf_number = "T-14-9-9", tlf_type = "TABLE", title = "Guardrail",
    .pending_column_annotations = list(
      labels = c("Cohort A (N=XX)", "Cohort B (N=XX)", "Odd (N=XX)"),
      annotations = c("ADSL.COHORTN=1", "ADSL.COHORTN=2",
                      "ADSL.COHORTN ~= 3")))
  out <- .resolve_table_column_groups(sec)

  ## The two good columns survive.
  expect_length(out$column_groups$groups, 2)
  ## The dropped column is surfaced with both counts.
  d <- ars_diagnostics()
  hit <- grepl("did not parse into a condition", d$problem)
  expect_true(any(hit))
  expect_true(any(grepl("1 of 3 ADSL.COHORTN column headers", d$problem[hit],
                        fixed = TRUE)))
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
  expect_equal(gf$groups[[1]]$condition$condition$comparator, "EQ")
  expect_equal(unlist(gf$groups[[1]]$condition$condition$value), "1")
  ## The is-missing level carries an empty value list.
  expect_length(gf$groups[[2]]$condition$condition$value, 0)
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
  expect_length(unknown$condition$condition$value, 0)
  ## The Total header switched includeTotal on.
  expect_true(any(vapply(spec$analyses, function(a)
    isTRUE(a$includeTotal), logical(1))))
})
