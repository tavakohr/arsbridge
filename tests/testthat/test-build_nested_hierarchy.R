## Phase N1 of the nested SOC/PT handoff: the generator recognises the
## nested token-block pattern AE-style shells author and emits two linked
## analyses instead of flattening the hierarchy away.

## Hand-built enriched section shaped like the annotated AE shell: an
## any-TEAE count row, then a nested SOC/PT token block authored as two
## example blocks (the second is a template repeat).
.nested_section <- function(tlf = "T-14-3-2") {
  token_row <- function(label, annotation) {
    list(label = label, annotation = annotation, has_annot = TRUE,
         detection_method = "pattern", detection_confidence = "high")
  }
  list(
    tlf_number       = tlf,
    tlf_type         = "TABLE",
    title            = "TEAE by System Organ Class and Preferred Term",
    population_text  = "Safety Population",
    population_annot = "ADSL.SAFFL='Y'",
    footnotes        = list(),
    source_datasets  = c("ADAE"),
    col_headers      = c("System Organ Class / Preferred Term",
                         "Treatment A", "Placebo"),
    n_data_cols      = 2L,
    stub_rows        = list(
      token_row("Subjects with any TEAE",
                "row incl. ADAE.TRTEMFL='Y'; once/subject ADAE.AOCCIFL"),
      token_row("<System Organ Class>", "ADAE.AESOC"),
      token_row("<Preferred Term>", "ADAE.AEDECOD"),
      token_row("<Preferred Term>", "ADAE.AEDECOD"),
      token_row("<System Organ Class>", "ADAE.AESOC"),
      token_row("<Preferred Term>", "ADAE.AEDECOD")
    ),
    analysis_type    = "CATEGORICAL",
    ars_method_name  = "Count and Percentage",
    by_variable      = "TRT01A",
    enriched_rows    = list()
  )
}


## --- the detection helper, in isolation -------------------------------------

test_that(".detect_nested_token_blocks marks the pattern and nothing else", {
  row <- function(label, annotation, has_annot = TRUE) {
    list(label = label, annotation = annotation, has_annot = has_annot)
  }

  rows <- list(
    row("Any TEAE", "ADAE.TRTEMFL='Y'"),
    row("<System Organ Class>", "ADAE.AESOC"),
    row("<Preferred Term>", "ADAE.AEDECOD"),
    row("<Preferred Term>", "ADAE.AEDECOD"),
    row("<System Organ Class>", "ADAE.AESOC"),
    row("<Preferred Term>", "ADAE.AEDECOD")
  )
  roles <- .detect_nested_token_blocks(rows, list())
  expect_equal(roles, c(NA, "nested_parent", "nested_child",
                        "nested_repeat", "nested_repeat", "nested_repeat"))

  ## A single block (no repeats) still qualifies.
  roles <- .detect_nested_token_blocks(rows[1:3], list())
  expect_equal(roles, c(NA, "nested_parent", "nested_child"))

  ## A lone token row is not a hierarchy.
  roles <- .detect_nested_token_blocks(rows[1:2], list())
  expect_equal(roles, c(NA_character_, NA_character_))

  ## A run on ONE variable (repeated PT mocks with no parent) is not nested.
  flat <- list(
    row("<Preferred Term>", "ADAE.AEDECOD"),
    row("<Preferred Term>", "ADAE.AEDECOD")
  )
  expect_equal(.detect_nested_token_blocks(flat, list()),
               rep(NA_character_, 2))

  ## Child before parent does not qualify.
  inverted <- list(
    row("<Preferred Term>", "ADAE.AEDECOD"),
    row("<Preferred Term>", "ADAE.AEDECOD"),
    row("<System Organ Class>", "ADAE.AESOC")
  )
  expect_equal(.detect_nested_token_blocks(inverted, list()),
               rep(NA_character_, 3))

  ## Two datasets never pair up.
  mixed <- list(
    row("<System Organ Class>", "ADAE.AESOC"),
    row("<Preferred Term>", "ADCM.CMDECOD")
  )
  expect_equal(.detect_nested_token_blocks(mixed, list()),
               rep(NA_character_, 2))

  ## Non-token labels break the run even with matching variables.
  labelled <- list(
    row("System Organ Class", "ADAE.AESOC"),
    row("Preferred Term", "ADAE.AEDECOD")
  )
  expect_equal(.detect_nested_token_blocks(labelled, list()),
               rep(NA_character_, 2))
})

test_that(".once_per_subject_var reads the clause and nothing else", {
  expect_equal(
    .once_per_subject_var("row incl. ADAE.TRTEMFL='Y'; once/subject ADAE.AOCCIFL"),
    "ADAE.AOCCIFL"
  )
  expect_equal(.once_per_subject_var("once / subject AOCCSIFL"), "AOCCSIFL")
  expect_null(.once_per_subject_var("ADAE.AESOC"))
  expect_null(.once_per_subject_var(""))
  expect_null(.once_per_subject_var(NULL))
})


## --- the generated ARS ------------------------------------------------------

test_that("a nested block becomes two linked analyses, not a flat pair", {
  re <- build_ars_json(list(.nested_section()), study_id = "S-NEST")

  ## Template repeats collapse: any-TEAE + SOC + PT only.
  expect_length(re$analyses, 3)
  vars <- vapply(re$analyses, function(a) a$variable, character(1))
  expect_equal(vars, c("TRTEMFL", "AESOC", "AEDECOD"))

  soc <- re$analyses[[2]]
  pt  <- re$analyses[[3]]

  ## Both levels count distinct subjects.
  expect_equal(soc$methodId, "MTH_AE_FREQUENCY_COUNT")
  expect_equal(pt$methodId,  "MTH_AE_FREQUENCY_COUNT")

  ## The child rides the parent's variable as a data-driven row grouping,
  ## after the treatment column grouping.
  pt_gids <- vapply(pt$orderedGroupings, function(g) g$groupingId,
                    character(1))
  expect_equal(pt_gids, c("GF_TRT01A", "GF_AESOC"))
  expect_true(all(vapply(pt$orderedGroupings,
                         function(g) isTRUE(g$resultsByGroup), logical(1))))

  ## The parent keeps only the treatment axis.
  soc_gids <- vapply(soc$orderedGroupings, function(g) g$groupingId,
                     character(1))
  expect_equal(soc_gids, "GF_TRT01A")

  ## The row grouping entity exists and is data-driven with no enumerated
  ## groups -- levels come from the data.
  gf <- Filter(function(g) identical(g$id, "GF_AESOC"),
               re$analysisGroupings)
  expect_length(gf, 1)
  expect_true(isTRUE(gf[[1]]$dataDriven))
  expect_length(gf[[1]]$groups, 0)
  expect_equal(gf[[1]]$groupingDataset, "ADAE")
  expect_equal(gf[[1]]$groupingVariable, "AESOC")
})

test_that("the shell layout records the linked pair and drops the repeats", {
  re <- build_ars_json(list(.nested_section()), study_id = "S-NEST")
  layout <- re$outputs[[1]][["_meta"]][["shell_layout"]]

  expect_length(layout, 3)
  kinds <- vapply(layout, function(e) e$kind, character(1))
  expect_equal(kinds, c("filtered_count", "nested_parent", "nested_child"))

  parent <- layout[[2]]
  child  <- layout[[3]]
  expect_equal(child$parent_order, parent$order)
  expect_null(parent$parent_order)

  ## Referenced analyses mirror the three emitted ones, in order.
  refs <- unlist(re$outputs[[1]]$referencedAnalysisIds)
  expect_equal(refs, vapply(re$analyses, function(a) a$id, character(1)))
})

test_that("once/subject leaves subject-count rows alone", {
  re <- build_ars_json(list(.nested_section()), study_id = "S-NEST")
  teae <- re$analyses[[1]]
  ## The filter on the row's own flag already implies a distinct-subject
  ## count; once/subject must not reroute it.
  expect_equal(teae$methodId, "MTH_SUBJECT_COUNT")
})

test_that("once/subject reroutes a plain count row to distinct subjects", {
  sec <- .nested_section()
  sec$stub_rows <- list(
    list(label = "Any event, n (%)",
         annotation = "ADAE.AEDECOD; once/subject ADAE.AOCCIFL",
         has_annot = TRUE,
         detection_method = "pattern", detection_confidence = "high")
  )
  re <- build_ars_json(list(sec), study_id = "S-ONCE")
  expect_length(re$analyses, 1)
  expect_equal(re$analyses[[1]]$methodId, "MTH_AE_FREQUENCY_COUNT")
})

test_that("the pattern generalises beyond AE: conmed ATC class / term", {
  sec <- .nested_section("T-14-4-1")
  sec$title <- "Concomitant Medications by ATC Class and Preferred Term"
  sec$source_datasets <- c("ADCM")
  sec$stub_rows <- list(
    list(label = "<ATC Level 3 Class>", annotation = "ADCM.CMCLAS",
         has_annot = TRUE, detection_method = "pattern",
         detection_confidence = "high"),
    list(label = "<Preferred Term>", annotation = "ADCM.CMDECOD",
         has_annot = TRUE, detection_method = "pattern",
         detection_confidence = "high"),
    list(label = "<Preferred Term>", annotation = "ADCM.CMDECOD",
         has_annot = TRUE, detection_method = "pattern",
         detection_confidence = "high")
  )
  re <- build_ars_json(list(sec), study_id = "S-CM")

  expect_length(re$analyses, 2)
  pt_gids <- vapply(re$analyses[[2]]$orderedGroupings,
                    function(g) g$groupingId, character(1))
  expect_equal(pt_gids, c("GF_TRT01A", "GF_CMCLAS"))
  layout <- re$outputs[[1]][["_meta"]][["shell_layout"]]
  expect_equal(vapply(layout, function(e) e$kind, character(1)),
               c("nested_parent", "nested_child"))
})

test_that("a conflict secondary inherits the distinct-subject method", {
  ## The incident shape: the nested SOC row wins with AE Frequency Count;
  ## the supplement's conflicting proposal (same variable, extra filter)
  ## must count subjects the same way -- record counting over the subject
  ## denominator rendered p > 1 in production.
  sec <- .nested_section()
  sec$stub_rows[[2]]$secondary_annotation <-
    "ADAE.AESOC WHERE ADAE.TRTEMFL='Y'"
  re <- build_ars_json(list(sec), study_id = "S-NEST")

  layout <- re$outputs[[1]][["_meta"]][["shell_layout"]]
  kinds <- vapply(layout, function(e) e$kind, character(1))
  supp_at <- which(kinds == "supplement_added")
  expect_length(supp_at, 1)

  supp_id <- layout[[supp_at]]$analysis_id
  supp <- Filter(function(a) identical(a$id, supp_id), re$analyses)[[1]]
  expect_equal(supp$methodId, "MTH_AE_FREQUENCY_COUNT")
  expect_equal(supp$variable, "AESOC")
})


## --- validation (Phase N4) ---------------------------------------------------

test_that("a well-formed nested event raises no nested findings", {
  re <- build_ars_json(list(.nested_section()), study_id = "S-NEST")
  findings <- validate_ars_model(ars_to_model(re))
  expect_equal(sum(findings$ref %in%
                     c("NESTED_CHILD_UNLINKED", "NESTED_GROUPING_MISSING"),
                   na.rm = TRUE), 0)
})

test_that("a child without the parent grouping is flagged", {
  re <- build_ars_json(list(.nested_section()), study_id = "S-NEST")
  ## Strip the row grouping from the child analysis (index 3: TEAE, SOC, PT).
  re$analyses[[3]]$orderedGroupings <- re$analyses[[3]]$orderedGroupings[1]

  findings <- validate_ars_model(ars_to_model(re))
  hit <- findings[findings$ref %in% "NESTED_GROUPING_MISSING", , drop = FALSE]
  expect_equal(nrow(hit), 1)
  expect_equal(hit$id, re$analyses[[3]]$id)
  expect_equal(hit$severity, "WARN")
})

test_that("a child whose parent link is severed is flagged", {
  re <- build_ars_json(list(.nested_section()), study_id = "S-NEST")
  layout <- re$outputs[[1]][["_meta"]][["shell_layout"]]
  for (k in seq_along(layout)) {
    if (identical(layout[[k]]$kind, "nested_child")) {
      layout[[k]]$parent_order <- NULL
    }
  }
  re$outputs[[1]][["_meta"]][["shell_layout"]] <- layout

  findings <- validate_ars_model(ars_to_model(re))
  hit <- findings[findings$ref %in% "NESTED_CHILD_UNLINKED", , drop = FALSE]
  expect_equal(nrow(hit), 1)
  expect_equal(hit$id, re$outputs[[1]]$id)
})


## --- editor (Phase N4) -------------------------------------------------------

test_that("the shell view badges nested rows", {
  skip_if_not_installed("shiny")

  re <- build_ars_json(list(.nested_section()), study_id = "S-NEST")
  model <- ars_to_model(re)
  index <- match(re$outputs[[1]]$id, model$outputs$id)
  data <- .shell_table_data(model$outputs$raw[[index]], model)

  expect_equal(sum(data$rows$kind %in% c("nested_parent", "nested_child")), 2)

  html <- as.character(
    .shell_table_ui(data, shiny::NS("detail"), re$outputs[[1]]$id, "view")
  )
  expect_equal(lengths(regmatches(html, gregexpr(">nested<", html))), 2L)
})


test_that("an ordinary demographics section is untouched by the detection", {
  ## The exact section test-build_ars_json.R builds everywhere: one
  ## continuous row, one label row -- no token pattern, no new kinds.
  sec <- list(
    tlf_number = "T-14-1-1", tlf_type = "TABLE", title = "Demographics",
    population_text = "Safety Population",
    population_annot = "ADSL.SAFFL='Y'",
    footnotes = list(), source_datasets = c("ADSL"),
    col_headers = c("Characteristic", "Treatment A", "Placebo"),
    n_data_cols = 2L,
    stub_rows = list(
      list(label = "Age (years)", annotation = "ADSL.AGE", has_annot = TRUE,
           detection_method = "pattern", detection_confidence = "high"),
      list(label = "n", annotation = "", has_annot = FALSE,
           detection_method = NA_character_,
           detection_confidence = NA_character_)
    ),
    analysis_type = "CONTINUOUS",
    ars_method_name = "Summary Statistics - Continuous",
    by_variable = "TRT01A",
    enriched_rows = list(list(
      label = "Age (years)", primary_dataset = "ADSL",
      primary_variable = "AGE", data_subset = NULL,
      variable_role = "ANALYSIS"
    ))
  )
  re <- build_ars_json(list(sec), study_id = "S-PLAIN")
  layout <- re$outputs[[1]][["_meta"]][["shell_layout"]]
  kinds <- vapply(layout, function(e) e$kind, character(1))
  expect_false(any(grepl("^nested", kinds)))
  expect_equal(re$analyses[[1]]$methodId,
               "MTH_SUMMARY_STATISTICS_CONTINUOUS")
})

test_that("once/subject upgrades a secondary and registers the method itself", {
  ## Covers the branch #23 left unexercised: no nested primary here, so
  ## AE Frequency Count is NOT already in the method pool -- the
  ## secondary's own once/subject clause must trigger the upgrade AND
  ## register the method entity.
  sec <- .nested_section()
  sec$stub_rows <- list(list(
    label = "Any AE, n (%)", annotation = "ADAE.AEDECOD", has_annot = TRUE,
    detection_method = "pattern", detection_confidence = "high",
    secondary_annotation =
      "ADAE.AEDECOD WHERE ADAE.TRTEMFL='Y'; once/subject ADAE.AOCCIFL"
  ))
  re <- build_ars_json(list(sec), study_id = "S-ONCE2")

  expect_length(re$analyses, 2)
  ## The shell's own row keeps the record-counting verdict...
  expect_equal(re$analyses[[1]]$methodId, "MTH_COUNT_AND_PERCENTAGE")
  ## ...the secondary upgraded through its once/subject clause...
  expect_equal(re$analyses[[2]]$methodId, "MTH_AE_FREQUENCY_COUNT")
  ## ...and registered the method entity on its own.
  expect_true("MTH_AE_FREQUENCY_COUNT" %in%
                vapply(re$methods, function(m) m$id, character(1)))
})


## --- the frequency-sort annotation ------------------------------------------

test_that(".nested_sort_clause reads the clause and nothing else", {
  expect_null(.nested_sort_clause("ADAE.AESOC"))
  expect_null(.nested_sort_clause(""))
  expect_null(.nested_sort_clause(NULL))

  expect_equal(
    .nested_sort_clause("ADAE.AESOC; sort: alphabetical")$basis,
    "alphabetical"
  )
  expect_equal(.nested_sort_clause("sort: alpha")$basis, "alphabetical")

  plain <- .nested_sort_clause("ADAE.AESOC; sort: desc-freq")
  expect_equal(plain$basis, "desc-freq")
  expect_null(plain$column)

  quoted <- .nested_sort_clause("ADAE.AESOC; sort: desc-freq('Drug A')")
  expect_equal(quoted$basis, "desc-freq")
  expect_equal(quoted$column, "Drug A")

  bare <- .nested_sort_clause("sort: desc-freq(Total)")
  expect_equal(bare$column, "Total")

  ## An unreadable clause comes back flagged, never guessed.
  junk <- .nested_sort_clause("ADAE.AESOC; sort: by magic")
  expect_true(is.na(junk$basis))
  expect_equal(junk$raw, "by magic")
})

test_that("a sort clause on the parent row lands on the layout entry", {
  sec <- .nested_section()
  sec$stub_rows[[2]]$annotation <- "ADAE.AESOC; sort: alphabetical"
  re <- build_ars_json(list(sec), study_id = "S-SORT")

  layout <- re$outputs[[1]][["_meta"]][["shell_layout"]]
  kinds <- vapply(layout, function(e) e$kind, character(1))
  parent <- layout[[which(kinds == "nested_parent")]]
  expect_equal(parent$sort, "alphabetical")

  ## The clause never leaks onto other rows.
  others <- layout[kinds != "nested_parent"]
  expect_false(any(vapply(others, function(e) "sort" %in% names(e),
                          logical(1))))
})

test_that("a column-basis sort clause round-trips through .shell_layout", {
  sec <- .nested_section()
  sec$stub_rows[[2]]$annotation <-
    "ADAE.AESOC; sort: desc-freq('Treatment A')"
  re <- build_ars_json(list(sec), study_id = "S-SORT2")

  layout <- .shell_layout(re$outputs[[1]])
  expect_true("sort" %in% names(layout))
  expect_equal(layout$sort[layout$kind == "nested_parent"],
               "desc-freq:Treatment A")
  expect_true(all(is.na(layout$sort[layout$kind != "nested_parent"])))
})

test_that("no sort clause means no sort field (renderer default applies)", {
  re <- build_ars_json(list(.nested_section()), study_id = "S-SORT3")
  layout <- .shell_layout(re$outputs[[1]])
  expect_true(all(is.na(layout$sort)))
})

test_that("an unreadable sort clause warns and keeps the default", {
  diag_reset()
  sec <- .nested_section()
  sec$stub_rows[[2]]$annotation <- "ADAE.AESOC; sort: by magic"
  re <- build_ars_json(list(sec), study_id = "S-SORT4")

  layout <- .shell_layout(re$outputs[[1]])
  expect_true(all(is.na(layout$sort)))

  d <- diag_records()
  hit <- d[d$severity == "WARN" &
             grepl("sort clause I can't read", d$problem), , drop = FALSE]
  expect_equal(nrow(hit), 1)
  expect_true(grepl("descending frequency", hit$action))
})


## --- token dialects (Phase R3) ----------------------------------------------

.tok_row <- function(label, annotation = "") {
  list(label = label, annotation = annotation,
       has_annot = nzchar(annotation))
}

test_that(".token_stem reads both dialects and rejects ordinary labels", {
  expect_equal(.token_stem("<System Organ Class>"), "SYSTEM ORGAN CLASS")
  expect_equal(.token_stem("<Preferred Term>"), "PREFERRED TERM")
  ## A numbered token: the number is what varies, the stem is what repeats.
  expect_equal(.token_stem("SOC#1"), "SOC")
  expect_equal(.token_stem("PT#2"), "PT")
  expect_equal(.token_stem("PT#n"), "PT")
  expect_equal(.token_stem("Body System #1"), "BODY SYSTEM")
  ## The angle dialect de-numbers too, so <Reason #1>/<Reason #2> pair up.
  expect_equal(.token_stem("<Reason #2>"), "REASON")

  expect_null(.token_stem("Age (years)"))
  expect_null(.token_stem("Subjects with any TEAE"))
  expect_null(.token_stem(""))
  expect_null(.token_stem(NULL))
  ## A stray "#" in real prose is not a token.
  expect_null(.token_stem("Cohort #1 versus placebo"))
})

test_that("the numbered SOC#n / PT#n dialect is a nested block", {
  rows <- list(
    .tok_row("Subjects with at least one medical history",
             "ADMH.MHCAT='GENERAL MEDICAL HISTORY'"),
    .tok_row("SOC#1", "ADMH.MHBODSYS"),
    .tok_row("PT#1", "ADMH.MHDECOD"),
    ## The repeats carry NO annotation -- they inherit through the stem.
    .tok_row("PT#2"),
    .tok_row("PT#n"),
    .tok_row("SOC#2"),
    .tok_row("PT#1"),
    .tok_row("PT#n")
  )
  roles <- .detect_nested_token_blocks(rows, list())
  expect_equal(roles[1], NA_character_)
  expect_equal(roles[2], "nested_parent")
  expect_equal(roles[3], "nested_child")
  expect_true(all(roles[4:8] == "nested_repeat"))
})

test_that("single-level mocks under a categorical parent collapse", {
  rows <- list(
    .tok_row("Primary reason for discontinuation, n (%)", "ADSL.DCSREASN"),
    .tok_row("<Reason #1>", "ADSL.DCSREASN"),
    .tok_row("<Reason #2>", "ADSL.DCSREASN"),
    .tok_row("...")
  )
  roles <- .detect_nested_token_blocks(rows, list())
  ## The categorical row itself stays; its illustrations do not.
  expect_equal(roles[1], NA_character_)
  expect_true(all(roles[2:4] == "level_repeat"))
})

test_that("mocks on a different variable than the row above are left alone", {
  rows <- list(
    .tok_row("Age group, n (%)", "ADSL.AGEGR1"),
    .tok_row("<Preferred Term>", "ADAE.AEDECOD"),
    .tok_row("<Preferred Term>", "ADAE.AEDECOD")
  )
  ## No parent/child pair (one variable) and no matching categorical row
  ## above -- nothing is collapsed, the rows keep their own analyses.
  expect_equal(.detect_nested_token_blocks(rows, list()),
               rep(NA_character_, 3))
})

test_that("a continuation row is only swallowed after a mock block", {
  ## After a mock block: part of it.
  rows <- list(
    .tok_row("Reason, n (%)", "ADSL.DCSREASN"),
    .tok_row("<Reason #1>", "ADSL.DCSREASN"),
    .tok_row("...")
  )
  expect_equal(.detect_nested_token_blocks(rows, list())[3], "level_repeat")

  ## After an ordinary row: left alone (it is the reviewer's problem, not
  ## something to silently drop).
  rows2 <- list(
    .tok_row("Age (years)", "ADSL.AGE"),
    .tok_row("...")
  )
  expect_equal(.detect_nested_token_blocks(rows2, list()),
               rep(NA_character_, 2))
})

test_that("level_repeat rows are emitted nowhere but are reported", {
  diag_reset()
  sec <- .nested_section()
  sec$stub_rows <- list(
    .tok_row("Primary reason for discontinuation, n (%)", "ADSL.DCSREASN"),
    .tok_row("<Reason #1>", "ADSL.DCSREASN"),
    .tok_row("<Reason #2>", "ADSL.DCSREASN"),
    .tok_row("...")
  )
  re <- build_ars_json(list(sec), study_id = "S-LVL")

  ## One analysis: the categorical row. The mocks contributed none.
  expect_length(re$analyses, 1)
  layout <- re$outputs[[1]][["_meta"]][["shell_layout"]]
  labels <- vapply(layout, function(e) e$label, character(1))
  expect_equal(labels, "Primary reason for discontinuation, n (%)")

  d <- diag_records()
  hits <- d[grepl("illustrates the levels", d$problem), , drop = FALSE]
  expect_equal(nrow(hits), 3)
  expect_true(all(hits$severity == "INFO"))
})

test_that("bare numbered repeats never become literal label rows", {
  ## The regression this guards: the un-annotated-row branch used to run
  ## BEFORE the mock-role check, so "PT#2" / "SOC#2" / "..." were persisted
  ## as authored label rows and the placeholder text rendered as if it were
  ## a real table row.
  sec <- .nested_section()
  sec$source_datasets <- "ADMH"
  sec$stub_rows <- list(
    .tok_row("Subjects with at least one medical history",
             "ADMH.MHCAT='GENERAL MEDICAL HISTORY'"),
    .tok_row("SOC#1", "ADMH.MHBODSYS"),
    .tok_row("PT#1", "ADMH.MHDECOD"),
    .tok_row("PT#2"),
    .tok_row("PT#n"),
    .tok_row("SOC#2"),
    .tok_row("PT#1"),
    .tok_row("PT#n")
  )
  re <- build_ars_json(list(sec), study_id = "S-MHNUM")

  layout <- re$outputs[[1]][["_meta"]][["shell_layout"]]
  kinds  <- vapply(layout, function(e) e$kind, character(1))
  labels <- vapply(layout, function(e) e$label, character(1))

  ## Three rows only: the any-history count (a filter on its own variable,
  ## so a subject count within that subset) and the nested pair. The five
  ## bare repeats contributed nothing.
  expect_equal(kinds, c("filtered_count", "nested_parent", "nested_child"))
  expect_false(any(grepl("#", labels[kinds == "label"])))
  ## The rows that carry the analyses are the annotated ones.
  expect_equal(labels[2], "SOC#1")
  expect_equal(labels[3], "PT#1")
})
