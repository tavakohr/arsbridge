## ADR 0003 -- shell layout fidelity & convention-agnostic annotation binding.
## Regression fixture: study CDSC-ALZ-201, table T_14_1_1 Subject Disposition
## (6 authored stub rows, red "Label -> DATASET.VAR" annotation lines BELOW
## the table, treatment columns annotated as "Treatment columns -> ADSL.TRT01A").

fixture_shell <- test_path("fixtures", "CDSC-ALZ-201_TLF_Shells_v1.0_annotated.docx")
fixture_spec  <- test_path("fixtures", "adam_spec_CDSC-ALZ-201.xlsx")

parse_cdsc <- function() {
  spec <- parse_adam_spec(fixture_spec)
  secs <- suppressMessages(
    parse_shell_docx(fixture_shell, spec_lookup = spec$lookup))
  list(spec = spec, secs = secs)
}

## Minimal deterministic enrichment so build_ars_json runs without an LLM.
enrich_offline <- function(secs) {
  lapply(secs, function(s) {
    s$analysis_type <- "CATEGORICAL"
    s$ars_method_name <- "Count and Percentage"
    s$groupings <- list(list(variable = "TRT01A", dataset = "ADSL"))
    s$by_variable <- "TRT01A"; s$by_variable_dataset <- "ADSL"
    s
  })
}

## --- Phase 1: footnote / annotation split ----------------------------------

test_that("below-table annotation lines are captured as programmer_annotations, not footnotes", {
  skip_if_not(file.exists(fixture_shell))
  p <- parse_cdsc()
  s <- p$secs[[1]]

  expect_length(s$footnotes, 1)
  expect_match(s$footnotes, "^Percentages based")
  expect_length(s$programmer_annotations, 5)
  expect_false(any(grepl("->", s$footnotes, fixed = TRUE)))
  ## every arrow line was diverted
  expect_true(all(grepl("->", s$programmer_annotations, fixed = TRUE)))
})

test_that("ship_annotations = FALSE keeps annotations out of the ARS Footnote section", {
  skip_if_not(file.exists(fixture_shell))
  p <- parse_cdsc()
  secs <- enrich_offline(p$secs)

  re <- suppressMessages(suppressWarnings(
    build_ars_json(secs[1], spec_lookup = p$spec$lookup)))
  notes <- vapply(
    re$outputs[[1]]$displays[[1]]$display$displaySections[[1]]$
      orderedSubSections,
    function(ss) ss$subSection$text, character(1))
  expect_length(notes, 1)
  expect_false(any(grepl("->|DATASET|ADSL\\.", notes)))

  re2 <- suppressMessages(suppressWarnings(
    build_ars_json(secs[1], spec_lookup = p$spec$lookup,
                   ship_annotations = TRUE)))
  notes2 <- vapply(
    re2$outputs[[1]]$displays[[1]]$display$displaySections[[1]]$
      orderedSubSections,
    function(ss) ss$subSection$text, character(1))
  expect_length(notes2, 6)
})

## --- Phase 2: convention-agnostic binding ----------------------------------

test_that("arrow-form annotations bind to their stub rows regardless of placement", {
  skip_if_not(file.exists(fixture_shell))
  s <- parse_cdsc()$secs[[1]]
  rows <- s$stub_rows

  expect_length(rows, 6)
  lab <- vapply(rows, function(r) r$label, character(1))
  expect_identical(lab, c(
    "Subjects enrolled", "Screen failures", "Randomized / treated (Safety)",
    "Completed study", "Discontinued study", "(End-of-study status)"))

  bound <- vapply(rows, function(r) isTRUE(r$has_annot), logical(1))
  expect_identical(bound, c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE))
  expect_true(all(vapply(rows[bound], function(r)
    identical(r$detection_method, "below_table_arrow"), logical(1))))

  ann <- vapply(rows, function(r) r$annotation, character(1))
  expect_match(ann[1], "count of ADSL\\.USUBJID")
  expect_match(ann[2], "ADSL\\.TRT01P\\s*=\\s*'Screen Failure'")
  expect_match(ann[3], "ADSL\\.SAFFL\\s*=\\s*'Y'")
  ## multi-label lhs split against a parenthetical value list
  expect_identical(ann[4], "ADSL.EOSSTT='COMPLETED'")
  expect_identical(ann[5], "ADSL.EOSSTT='DISCONTINUED'")
})

test_that("a column-axis annotation is captured as column_annotation", {
  skip_if_not(file.exists(fixture_shell))
  s <- parse_cdsc()$secs[[1]]
  expect_identical(s$column_annotation, "ADSL.TRT01A")
})

test_that("bind_annotations never overrides an in-cell detection and skips unmatched lines", {
  sec <- list(
    population_text = "Safety Population",
    col_headers = c("", "Arm A"),
    programmer_annotations = c(
      "Row one -> ADSL.AGE",
      "Row two -> ADSL.SEX",
      "No such row -> ADSL.RACE"),
    stub_rows = list(
      list(label = "Row one", annotation = "ADSL.HEIGHT", has_annot = TRUE,
           detection_method = "colour", detection_confidence = "high"),
      list(label = "Row two", annotation = "", has_annot = FALSE,
           detection_method = NA_character_,
           detection_confidence = NA_character_)))
  out <- bind_annotations(sec)
  expect_identical(out$stub_rows[[1]]$annotation, "ADSL.HEIGHT")   # in-cell wins
  expect_identical(out$stub_rows[[1]]$detection_method, "colour")
  expect_identical(out$stub_rows[[2]]$annotation, "ADSL.SEX")
  expect_identical(out$stub_rows[[2]]$detection_method, "below_table_arrow")
  expect_length(out$programmer_annotations, 3)                     # record kept
})

## --- Phase 3: layout persistence + no-drop ---------------------------------

test_that("build_ars_json persists the authored layout and emits one analysis per annotated row", {
  skip_if_not(file.exists(fixture_shell))
  p <- parse_cdsc()
  secs <- enrich_offline(p$secs)
  re <- suppressMessages(suppressWarnings(
    build_ars_json(secs[1], spec_lookup = p$spec$lookup)))
  o  <- re$outputs[[1]]
  sl <- o$`_meta`$shell_layout

  expect_length(sl, 6)
  expect_identical(vapply(sl, function(e) e$order, integer(1)), 1:6)
  expect_identical(
    vapply(sl, function(e) e$kind, character(1)),
    c("subject_count", "filtered_count", "filtered_count",
      "filtered_count", "filtered_count", "label"))
  ## every non-label authored row has an analysis; the label row has none
  aids <- vapply(sl, function(e) e$analysis_id %||% NA_character_, character(1))
  expect_identical(is.na(aids), c(rep(FALSE, 5), TRUE))
  expect_length(o$referencedAnalysisIds, 5)

  ## property: #layout rows >= #authored annotated rows (no silent drops)
  n_annot <- sum(vapply(p$secs[[1]]$stub_rows,
                        function(r) isTRUE(r$has_annot), logical(1)))
  expect_gte(length(sl), n_annot)

  ## the annotation forms drive the methods + subset filters
  aid <- unlist(o$referencedAnalysisIds)
  ana <- Filter(function(a) a$id %in% aid, re$analyses)
  expect_true(all(vapply(ana, function(a)
    identical(a$methodId, "MTH_SUBJECT_COUNT"), logical(1))))
  ds_ids <- vapply(ana, function(a) a$dataSubsetId, character(1))
  expect_identical(nzchar(ds_ids), c(FALSE, TRUE, TRUE, TRUE, TRUE))
})

test_that("every parsed section keeps at least as many layout rows as authored annotated rows", {
  skip_if_not(file.exists(fixture_shell))
  p <- parse_cdsc()
  secs <- enrich_offline(p$secs)
  re <- suppressMessages(suppressWarnings(
    build_ars_json(secs, spec_lookup = p$spec$lookup)))
  for (i in seq_along(re$outputs)) {
    o <- re$outputs[[i]]
    if (!identical(o$outputType, "TABLE")) next
    n_annot <- sum(vapply(p$secs[[i]]$stub_rows,
                          function(r) isTRUE(r$has_annot), logical(1)))
    expect_gte(length(o$`_meta`$shell_layout %||% list()), n_annot)
  }
})

test_that("LISTING sections always carry MTH_LISTING regardless of the section method guess", {
  sec <- list(
    tlf_number = "L-16-1-1", tlf_type = "LISTING", title = "Listing",
    population_text = "All", population_annot = "",
    footnotes = character(), source_datasets = "ADAE",
    col_headers = c("Subject", "AE"), n_data_cols = 1L,
    ars_method_name = "Count and Percentage",   # wrong LLM guess on purpose
    stub_rows = list(
      list(label = "Subject ID", annotation = "ADSL.USUBJID", has_annot = TRUE),
      list(label = "AE Term", annotation = "ADAE.AEDECOD", has_annot = TRUE)))
  re <- suppressMessages(suppressWarnings(build_ars_json(list(sec))))
  aid <- unlist(re$outputs[[1]]$referencedAnalysisIds)
  ana <- Filter(function(a) a$id %in% aid, re$analyses)
  expect_true(all(vapply(ana, function(a)
    identical(a$methodId, "MTH_LISTING"), logical(1))))
})

## --- Phase 4: column restriction + layout-driven prep -----------------------

test_that("build_col_levels(restrict=) drops ARD levels missing from the shell headers", {
  out_obj <- list(displays = list(list(order = 1L, display = list(
    id = "D1", name = "D1", columns = list(
    list(label = "Category"),
    list(label = "Placebo\n(N=86) n (%)"),
    list(label = "Xanomeline Low\n(N=96) n (%)"),
    list(label = "Xanomeline High\n(N=72) n (%)"))))))
  ard <- data.frame(group1_level = c("Placebo", "Xanomeline Low Dose",
                                     "Xanomeline High Dose", "Screen Failure"))
  lv <- build_col_levels(out_obj, ard, "group1_level", restrict = TRUE)
  expect_identical(lv, c("Placebo", "Xanomeline Low Dose",
                         "Xanomeline High Dose"))
  ## non-restricted path keeps appending (back-compat)
  lv2 <- build_col_levels(out_obj, ard, "group1_level")
  expect_identical(lv2, c("Placebo", "Xanomeline Low Dose",
                          "Xanomeline High Dose", "Screen Failure"))
  ## degenerate: nothing matches -> fall back to append-all even restricted
  ard3 <- data.frame(group1_level = c("Arm X", "Arm Y"))
  expect_identical(build_col_levels(out_obj, ard3, "group1_level",
                                    restrict = TRUE),
                   c("Arm X", "Arm Y"))
})

test_that(".tfrmt_prep_ard_layout keeps authored rows in order and blanks missing ones", {
  layout <- data.frame(
    order = 1:4,
    label = c("Subjects enrolled", "Screen failures", "Header row", "Age (years)"),
    indent = 0L,
    analysis_id = c("AN1", "AN2", NA, "AN3"),
    kind = c("subject_count", "filtered_count", "label", "continuous"),
    stringsAsFactors = FALSE)
  ard <- data.frame(
    output_id      = "OUT",
    analysis_id    = c("AN1", "AN1", "AN3", "AN3"),
    method_id      = c("MTH_SUBJECT_COUNT", "MTH_SUBJECT_COUNT",
                       "MTH_SUMMARY_STATISTICS_CONTINUOUS",
                       "MTH_SUMMARY_STATISTICS_CONTINUOUS"),
    variable       = c("TRT01A", "TRT01A", "AGE", "AGE"),
    variable_level = c("Placebo", "Active", NA, NA),
    group1_level   = c(NA, NA, "Placebo", "Placebo"),
    stat_name      = c("n", "n", "mean", "sd"),
    stat           = c(10, 12, 74.1, 8.2),
    stringsAsFactors = FALSE)

  prep <- .tfrmt_prep_ard_layout(
    ard, "OUT", layout, col_var = "group1_level",
    keep_params = c("n", "mean", "sd"),
    col_levels = c("Placebo", "Active"), fixed_vars = "TRT01A",
    params_map = list(
      ## Subject-count "n" is renamed by the prep (see .ARS_SUBJ_N_PARAM) so
      ## its structure cannot collide with a "{n} ({p}%)" combine.
      MTH_SUBJECT_COUNT = .ARS_SUBJ_N_PARAM,
      MTH_SUMMARY_STATISTICS_CONTINUOUS = c("mean", "sd")))

  ## AN2 produced nothing -> blank spacer row, never dropped
  lbls <- prep[[".arsbridge_shell_lbl"]]
  expect_true("Screen failures" %in% lbls)
  expect_true("Header row" %in% lbls)
  ## subject-count column values recovered from variable_level
  an1 <- prep[prep[[".arsbridge_shell_grp"]] == "Subjects enrolled", ]
  expect_setequal(an1[["group1_level"]], c("Placebo", "Active"))
  ## continuous rows expand to stat lines under the authored label
  age <- prep[prep[[".arsbridge_shell_grp"]] == "Age (years)", ]
  expect_true("Mean (SD)" %in% age[[".arsbridge_shell_lbl"]])
  ## authored order pinned
  ord <- prep[[".arsbridge_shell_ord"]]
  expect_false(is.unsorted(ord))
  first_of <- function(l) min(ord[prep[[".arsbridge_shell_grp"]] == l])
  expect_lt(first_of("Subjects enrolled"), first_of("Screen failures"))
  expect_lt(first_of("Screen failures"), first_of("Header row"))
  expect_lt(first_of("Header row"), first_of("Age (years)"))
})

## --- Phase 4 end-to-end: golden regression for T_14_1_1 ---------------------

test_that("T_14_1_1 renders the 6 authored rows in order with shell columns only", {
  skip_if_not(file.exists(fixture_shell))
  skip_if_not_installed("tfrmt")
  skip_if_not_installed("cards")

  p <- parse_cdsc()
  secs <- enrich_offline(p$secs)
  re <- suppressMessages(suppressWarnings(
    build_ars_json(secs[1], spec_lookup = p$spec$lookup)))
  ars_path <- withr::local_tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(re, auto_unbox = TRUE, pretty = TRUE,
                              null = "null"), ars_path)

  ## Tiny synthetic ADSL covering the annotated variables.
  adam_dir <- withr::local_tempdir()
  set.seed(1)
  n <- 40
  arms <- c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")
  adsl <- data.frame(
    USUBJID = sprintf("S%03d", 1:n),
    TRT01A  = c(sample(arms, n - 4, replace = TRUE), rep("Screen Failure", 4)),
    stringsAsFactors = FALSE)
  adsl$TRT01P <- adsl$TRT01A
  adsl$SAFFL  <- ifelse(adsl$TRT01A == "Screen Failure", "N", "Y")
  adsl$EOSSTT <- ifelse(adsl$SAFFL == "Y",
                        sample(c("COMPLETED", "DISCONTINUED"), n, replace = TRUE),
                        "DISCONTINUED")
  utils::write.csv(adsl, file.path(adam_dir, "ADSL.csv"), row.names = FALSE)

  ard <- suppressMessages(suppressWarnings(ars_to_ard(ars_path, adam_dir)))
  expect_gt(nrow(ard), 0)

  gt_tbl <- suppressMessages(ars_render_tlf(ars_path, ard, "T_14_1_1"))
  d <- as.data.frame(gt_tbl[["_data"]])

  ## authored rows, authored order
  expect_identical(d[[".arsbridge_shell_lbl"]], c(
    "Subjects enrolled", "Screen failures", "Randomized / treated (Safety)",
    "Completed study", "Discontinued study", "(End-of-study status)"))
  ## shell columns only -- no "Screen Failure" treatment column
  expect_true(all(arms %in% names(d)))
  expect_false("Screen Failure" %in% names(d))
  ## footnotes: only the real one, no annotation text
  fns <- attr(suppressMessages(
    ars_to_tfrmt(ars_path, ard, "T_14_1_1")), "arsbridge_footnotes")
  expect_length(fns, 1)
  expect_false(any(grepl("->", fns, fixed = TRUE)))
})

test_that("continuous stat lines fill authored sub-rows instead of duplicating them", {
  ## Shell authors its own "Mean (SD)" / "Median" / "Min, Max" rows under a
  ## continuous analysis row (T_14_2_1 pattern): the expanded stat lines
  ## must land ON those authored rows, not append a second block.
  layout <- data.frame(
    order = 1:5,
    label = c("Duration of exposure (days)", "Mean (SD)", "Median",
              "Min, Max", "Average daily dose"),
    indent = 0L,
    analysis_id = c("AN1", NA, NA, NA, "AN2"),
    kind = c("continuous", "label", "label", "label", "continuous"),
    stringsAsFactors = FALSE)
  ard <- data.frame(
    output_id    = "OUT",
    analysis_id  = rep(c("AN1", "AN2"), each = 4),
    method_id    = "MTH_SUMMARY_STATISTICS_CONTINUOUS",
    variable     = rep(c("TRTDURD", "AVGDD"), each = 4),
    variable_level = NA_character_,
    group1_level = "Placebo",
    stat_name    = rep(c("mean", "sd", "median", "min"), 2),
    stat         = c(149.5, 60.3, 182, 7, 5.2, 1.1, 5.0, 2.1),
    stringsAsFactors = FALSE)

  prep <- .tfrmt_prep_ard_layout(
    ard, "OUT", layout, col_var = "group1_level",
    keep_params = c("mean", "sd", "median", "min"),
    col_levels = "Placebo", fixed_vars = "TRT01A",
    params_map = list(
      MTH_SUMMARY_STATISTICS_CONTINUOUS = c("mean", "sd", "median", "min")))

  lbls <- prep[[.ARS_SHELL_LBL]]
  ## authored sub-rows consumed: exactly ONE "Mean (SD)" line per analysis
  ## block, no leftover blank spacer duplicates for AN1's stat lines
  an1 <- prep[prep[[.ARS_SHELL_GRP]] == "Duration of exposure (days)", ]
  expect_setequal(unique(an1[[.ARS_SHELL_LBL]]),
                  c("Duration of exposure (days)", "Mean (SD)", "Median",
                    "(Min, Max)"))
  ## the merged stat lines took the authored rows' positions (orders 2-4)
  ms <- an1[an1[[.ARS_SHELL_LBL]] == "Mean (SD)", .ARS_SHELL_ORD][1]
  expect_equal(as.numeric(ms), 2000)
  ## no duplicated blank "Mean (SD)" spacer remains anywhere
  spacers <- prep[prep$stat_name == .ARS_SPACER_PARAM, .ARS_SHELL_LBL]
  expect_false(any(c("Mean (SD)", "Median", "Min, Max") %in% spacers))
  ## AN2 has no authored sub-rows -> keeps its own appended block
  an2 <- prep[prep[[.ARS_SHELL_GRP]] == "Average daily dose", ]
  expect_true("Mean (SD)" %in% an2[[.ARS_SHELL_LBL]])
})

test_that("authored level rows of a categorical block become level slots, not duplicate analyses", {
  ## Demographics pattern: shell authors "Sex, n (%)" AND its level rows
  ## "Female"/"Male"; the level rows must not spawn their own analyses.
  sec <- list(
    tlf_number = "T-1", tlf_type = "TABLE", title = "Demo",
    population_text = "All", population_annot = "",
    footnotes = character(), source_datasets = "ADSL",
    col_headers = c("", "Placebo"), n_data_cols = 1L,
    ars_method_name = "Count and Percentage",
    groupings = list(list(variable = "TRT01A", dataset = "ADSL")),
    by_variable = "TRT01A", by_variable_dataset = "ADSL",
    stub_rows = list(
      list(label = "Sex, n (%)", annotation = "ADSL.SEX", has_annot = TRUE),
      list(label = "Female", annotation = "ADSL.SEX WHERE SEX='FEMALE'",
           has_annot = TRUE),
      list(label = "Male", annotation = "ADSL.SEX WHERE SEX='MALE'",
           has_annot = TRUE)),
    enriched_rows = list(
      list(label = "Sex, n (%)", primary_dataset = "ADSL",
           primary_variable = "SEX", data_subset = NULL,
           variable_role = "ANALYSIS")))
  ## spec marks SEX categorical so the parent infers count-and-percentage
  lookup <- list(`ADSL.SEX` = list(dataset = "ADSL", variable = "SEX",
                                   type = "char", codelist = "SEXCD"))
  re <- suppressMessages(suppressWarnings(
    build_ars_json(list(sec), spec_lookup = lookup)))
  o  <- re$outputs[[1]]
  sl <- o$`_meta`$shell_layout

  expect_length(o$referencedAnalysisIds, 1)   # only the parent
  expect_identical(vapply(sl, function(e) e$kind, character(1)),
                   c("categorical", "level", "level"))
  parent_aid <- sl[[1]]$analysis_id
  expect_identical(sl[[2]]$analysis_id, parent_aid)
  expect_identical(sl[[2]]$level, "FEMALE")
  expect_identical(sl[[3]]$level, "MALE")

  ## Renderer: slots fill from the parent's computed levels; the mismatched
  ## LLM value strings (FEMALE vs data 'F') match by prefix.
  layout <- .shell_layout(o)
  ard <- data.frame(
    output_id = o$id, analysis_id = parent_aid,
    method_id = "MTH_COUNT_AND_PERCENTAGE",
    variable = "SEX", variable_level = c("F", "M"),
    group1_level = "Placebo",
    stat_name = "n", stat = c(53, 33), stringsAsFactors = FALSE)
  prep <- .tfrmt_prep_ard_layout(
    ard, o$id, layout, col_var = "group1_level", keep_params = "n",
    col_levels = "Placebo", fixed_vars = "TRT01A",
    params_map = list(MTH_COUNT_AND_PERCENTAGE = "n"))
  female <- prep[prep[[.ARS_SHELL_LBL]] == "Female", ]
  male   <- prep[prep[[.ARS_SHELL_LBL]] == "Male", ]
  expect_equal(female$stat, 53)
  expect_equal(male$stat, 33)
  ## no leftover F/M expansion under the parent header
  expect_false(any(prep[[.ARS_SHELL_LBL]] %in% c("F", "M")))
  ## authored order pinned: header, Female, Male
  expect_true(all(diff(prep[[.ARS_SHELL_ORD]]) >= 0))
})

test_that("ars_to_ard keeps every analysis when several tabulate the same grouping variable", {
  skip_if_not(file.exists(fixture_shell))
  skip_if_not_installed("cards")

  ## Disposition pattern that broke in integration: the LLM enrichment sets
  ## variable = USUBJID on every row, so all subject-count analyses tabulate
  ## TRT01A and produce identity-colliding {cards} rows; the default
  ## bind_ard dedup then silently dropped all but one analysis.
  p <- parse_cdsc()
  secs <- enrich_offline(p$secs)
  sec <- secs[[1]]
  sec$enriched_rows <- lapply(sec$stub_rows, function(r) list(
    label = r$label, primary_dataset = "ADSL", primary_variable = "USUBJID",
    data_subset = NULL, variable_role = "ANALYSIS"))
  re <- suppressMessages(suppressWarnings(
    build_ars_json(list(sec), spec_lookup = p$spec$lookup)))
  ars_path <- withr::local_tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(re, auto_unbox = TRUE, pretty = TRUE,
                              null = "null"), ars_path)

  adam_dir <- withr::local_tempdir()
  set.seed(2)
  n <- 30
  arms <- c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")
  adsl <- data.frame(
    USUBJID = sprintf("S%03d", 1:n),
    TRT01A  = sample(arms, n, replace = TRUE),
    stringsAsFactors = FALSE)
  adsl$TRT01P <- adsl$TRT01A
  adsl$SAFFL  <- "Y"
  adsl$EOSSTT <- sample(c("COMPLETED", "DISCONTINUED"), n, replace = TRUE)
  utils::write.csv(adsl, file.path(adam_dir, "ADSL.csv"), row.names = FALSE)

  ard <- suppressMessages(suppressWarnings(ars_to_ard(ars_path, adam_dir)))
  got <- unique(vapply(ard$analysis_id, function(x)
    as.character(x[[1]]), character(1)))
  want <- unlist(re$outputs[[1]]$referencedAnalysisIds)
  ## every non-skipped analysis keeps its rows (screen-fail subset may be
  ## empty in this synthetic cut; all others must be present)
  expect_true(all(setdiff(want, "AN_T_14_1_1_002") %in% got))
})

## --- Phase 5: figure dataset from _meta ------------------------------------

test_that("ars_render_figure resolves its default dataset from _meta.source_datasets", {
  skip_if_not_installed("ggplot2")
  adam_dir <- withr::local_tempdir()
  advs <- data.frame(
    USUBJID = rep(sprintf("S%02d", 1:6), each = 2),
    TRT01A  = rep(c("A", "B"), 6),
    AVISITN = rep(c(1, 2), 6),
    AVAL    = rnorm(12, 80, 5))
  utils::write.csv(advs, file.path(adam_dir, "ADVS.csv"), row.names = FALSE)

  spec <- list(outputs = list(list(
    id = "F_14_3_1", name = "F_14_3_1", label = "Mean Pulse Rate Over Time",
    outputType = "FIGURE",
    displays = list(list(order = 1, display = list(
      id = "F_14_3_1_D1", name = "Mean Pulse Rate Over Time",
      displayTitle = "Mean Pulse Rate Over Time",
      displaySections = list()))),
    `_meta` = list(source_datasets = list("ADVS")))))
  ars_path <- withr::local_tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(spec, auto_unbox = TRUE, null = "null"), ars_path)

  ## No dataset argument: must read ADVS (there is no ADEFF in adam_dir).
  p <- ars_render_figure(ars_path, adam_dir, "F_14_3_1")
  expect_s3_class(p, "ggplot")
})

test_that("decoded level slots fill from the decoded computed pool", {
  ## Disposition pattern with a spec codelist: the parent expands decoded
  ## labels ("DEATH"), and the authored "Death" level row's slot was stamped
  ## with the decoded label at build time -- so slot and pool share one
  ## vocabulary and the authored row fills instead of blanking.
  sec <- list(
    tlf_number = "T-2", tlf_type = "TABLE", title = "Disposition",
    population_text = "All", population_annot = "",
    footnotes = character(), source_datasets = "ADSL",
    col_headers = c("", "Placebo"), n_data_cols = 1L,
    ars_method_name = "Count and Percentage",
    by_variable = "TRT01A", by_variable_dataset = "ADSL",
    stub_rows = list(
      list(label = "Primary reason for discontinuation",
           annotation = "ADSL.DCSREASN", has_annot = TRUE),
      list(label = "Death", annotation = "ADSL.DCSREASN=1",
           has_annot = TRUE)),
    enriched_rows = list(
      list(label = "Primary reason for discontinuation",
           primary_dataset = "ADSL", primary_variable = "DCSREASN",
           data_subset = NULL, variable_role = "ANALYSIS")))
  lookup <- list(`ADSL.DCSREASN` = list(dataset = "ADSL",
                                        variable = "DCSREASN",
                                        type = "integer",
                                        codelist = "DCSREAS"))
  codelists <- list(DCSREAS = list(
    name = "DCSREAS",
    terms = data.frame(term = c("1", "2"),
                       decode = c("DEATH", "LOST TO FOLLOW-UP"),
                       order = 1:2, stringsAsFactors = FALSE),
    used_by = "ADSL.DCSREASN"))

  re <- suppressMessages(suppressWarnings(
    build_ars_json(list(sec), spec_lookup = lookup, codelists = codelists)))
  o  <- re$outputs[[1]]
  sl <- o$`_meta`$shell_layout
  expect_identical(sl[[2]]$kind, "level")
  expect_identical(sl[[2]]$level, "DEATH")

  ## Renderer fill: the ARD pool carries decoded labels (factor levels).
  parent_aid <- sl[[1]]$analysis_id
  layout <- .shell_layout(o)
  ard <- data.frame(
    output_id = o$id, analysis_id = parent_aid,
    method_id = "MTH_COUNT_AND_PERCENTAGE",
    variable = "DCSREASN",
    variable_level = c("DEATH", "LOST TO FOLLOW-UP"),
    group1_level = "Placebo",
    stat_name = "n", stat = c(4, 2), stringsAsFactors = FALSE)
  prep <- .tfrmt_prep_ard_layout(
    ard, o$id, layout, col_var = "group1_level", keep_params = "n",
    col_levels = "Placebo", fixed_vars = "TRT01A",
    params_map = list(MTH_COUNT_AND_PERCENTAGE = "n"))
  death <- prep[prep[[.ARS_SHELL_LBL]] == "Death", ]
  expect_equal(death$stat, 4)
  ## The unmatched decoded level still expands under the parent header.
  expect_true("LOST TO FOLLOW-UP" %in% prep[[.ARS_SHELL_LBL]])
})
