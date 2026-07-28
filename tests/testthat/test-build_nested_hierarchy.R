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
