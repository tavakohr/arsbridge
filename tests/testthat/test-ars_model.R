## ars_to_model() / model_to_ars(): the round-trip core of the review stage.
##
## The contract these tests pin: an unedited model serializes back to a
## structurally identical reporting event, and an edited one differs ONLY at
## the paths that were edited. Everything the editor builds on top assumes
## that, so these are the tests to keep green.

.ars_fixture_path <- function() {
  test_path("fixtures", "ars_apx_drm_301_deterministic.json")
}

## A minimal event exercising the shapes the generated fixture happens not to
## contain: a compound where-clause, an unparsed population, and an analysis
## set carrying annotationText.
.hand_built_ars <- function() {
  list(
    id      = "STUDY-X",
    name    = "Hand built",
    version = "1",
    analysisSets = list(
      list(id = "AS_SAF", name = "Safety", label = "Safety",
           condition = list(dataset = "ADSL", variable = "SAFFL",
                            comparator = "EQ", value = list("Y")),
           level = 1L, order = 1L),
      list(id = "AS_RAW", name = "Unparsed", label = "Unparsed",
           annotationText = "subjects who did something unusual",
           level = 1L, order = 2L)
    ),
    dataSubsets = list(
      list(id = "DS_COMPOUND", name = "Compound", label = "Compound",
           compoundExpression = list(
             logicalOperator = "AND",
             whereClauses = list(
               list(condition = list(dataset = "ADSL", variable = "AGE",
                                     comparator = "GE", value = list("18"))),
               list(condition = list(dataset = "ADSL", variable = "AGE",
                                     comparator = "LE", value = list("65")))
             )
           ),
           level = 1L, order = 1L)
    ),
    analysisGroupings = list(
      list(id = "GF_TRT", name = "TRT01A", label = "Treatment",
           groupingDataset = "ADSL", groupingVariable = "TRT01A",
           dataDriven = FALSE, groups = list())
    ),
    methods = list(
      list(id = "MTH_COUNT_AND_PERCENTAGE", name = "Count and Percentage",
           label = "Count and Percentage", description = "n (%)",
           operations = list(
             list(id = "OP_N", name = "n", label = "n", order = 1L,
                  resultPattern = "XXX")
           ),
           codeTemplate = list(context = "R (arsbridge)", code = "## code"))
    ),
    analyses = list(
      list(id = "AN_1", name = "AN_1", label = "Sex", description = "",
           analysisSetId = "AS_SAF", dataset = "ADSL", variable = "SEX",
           analysisVariable = list(dataset = "ADSL", variable = "SEX"),
           dataSubsetId = "", methodId = "MTH_COUNT_AND_PERCENTAGE",
           orderedGroupings = list(
             list(order = 1L, groupingId = "GF_TRT", resultsByGroup = TRUE)
           ),
           annotation = "ADSL.SEX", includeTotal = TRUE)
    ),
    outputs = list(
      list(id = "T_1", name = "T-1", label = "Demographics", version = "1",
           outputType = "TABLE",
           displays = list(list(order = 1L, displayTitle = "Demographics")),
           fileSpecifications = list(list(name = "T-1.rtf", fileType = "rtf")),
           referencedAnalysisIds = list("AN_1"))
    )
  )
}


test_that("an unedited model round-trips to an identical reporting event", {
  original <- .read_json(.ars_fixture_path())
  model    <- ars_to_model(.ars_fixture_path())

  expect_s3_class(model, "ars_model")
  expect_equal(model_to_ars(model), original)
})

test_that("the tables of contents are copied verbatim, padding and all", {
  original <- .read_json(.ars_fixture_path())
  round_tripped <- model_to_ars(ars_to_model(.ars_fixture_path()))

  expect_identical(
    round_tripped$mainListOfContents,
    original$mainListOfContents
  )
  expect_identical(
    round_tripped$otherListsOfContents,
    original$otherListsOfContents
  )
})

test_that("an edit changes only the path that was edited", {
  original <- .read_json(.ars_fixture_path())
  model    <- ars_to_model(.ars_fixture_path())

  ## Pick an analysis whose method is genuinely different from the new value,
  ## so this cannot pass by accident.
  target <- model$analyses$id[model$analyses$methodId != "MTH_LISTING"][1]
  index  <- which(vapply(original$analyses,
                         function(a) a$id == target, logical(1)))

  edited <- model_set_field(model, "analyses", target, "methodId",
                            "MTH_LISTING")
  out <- model_to_ars(edited)

  expect_equal(out$analyses[[index]]$methodId, "MTH_LISTING")

  ## Put the one edited value back; everything else must be untouched.
  restored <- out
  restored$analyses[[index]]$methodId <- original$analyses[[index]]$methodId
  expect_equal(restored, original)
})

test_that("dataset and variable stay in sync with the nested analysisVariable", {
  model  <- ars_to_model(.ars_fixture_path())
  target <- model$analyses$id[1]

  edited <- model_set_field(model, "analyses", target, "variable", "ZZTEST")
  edited <- model_set_field(edited, "analyses", target, "dataset", "ADZZ")
  out    <- model_to_ars(edited)

  expect_equal(out$analyses[[1]]$variable, "ZZTEST")
  expect_equal(out$analyses[[1]]$analysisVariable$variable, "ZZTEST")
  expect_equal(out$analyses[[1]]$dataset, "ADZZ")
  expect_equal(out$analyses[[1]]$analysisVariable$dataset, "ADZZ")
})

test_that("reason and purpose read and write as controlled-term objects", {
  model <- ars_to_model(.ars_fixture_path())
  target <- model$analyses$id[1]

  expect_equal(model$analyses$reason[1], "SPECIFIED IN SAP")
  expect_equal(model$analyses$purpose[1], "EXPLORATORY OUTCOME MEASURE")

  ## An edit writes the object form; the untouched field keeps its node.
  edited <- model_set_field(model, "analyses", target, "purpose",
                            "PRIMARY OUTCOME MEASURE")
  node <- model_to_ars(edited)$analyses[[1]]
  expect_equal(node$purpose, list(controlledTerm = "PRIMARY OUTCOME MEASURE"))
  expect_equal(node$reason, list(controlledTerm = "SPECIFIED IN SAP"))

  ## Clearing removes the key entirely.
  cleared <- model_set_field(model, "analyses", target, "reason",
                             NA_character_)
  expect_false("reason" %in% names(model_to_ars(cleared)$analyses[[1]]))

  ## A sponsor-term object reads as NA and survives a round trip untouched
  ## unless the reviewer deliberately replaces it.
  sponsor <- .read_json(.ars_fixture_path())
  sponsor$analyses[[1]]$reason <- list(sponsorTermId = "SPONSOR_R1")
  sponsor_model <- ars_to_model(sponsor)
  expect_true(is.na(sponsor_model$analyses$reason[1]))
  expect_equal(model_to_ars(sponsor_model)$analyses[[1]]$reason,
               list(sponsorTermId = "SPONSOR_R1"))
})

test_that("an NA in an optional column removes the key, and setting it adds it", {
  model <- ars_to_model(.ars_fixture_path())

  with_strata <- model$analyses$id[!is.na(model$analyses$strata)][1]
  expect_false(is.na(with_strata))
  index <- which(model$analyses$id == with_strata)

  cleared <- model_set_field(model, "analyses", with_strata, "strata",
                             NA_character_)
  out <- model_to_ars(cleared)
  expect_false("strata" %in% names(out$analyses[[index]]))

  without_strata <- model$analyses$id[is.na(model$analyses$strata)][1]
  other_index <- which(model$analyses$id == without_strata)
  added <- model_set_field(model, "analyses", without_strata, "strata", "SEX")
  out2 <- model_to_ars(added)
  expect_equal(out2$analyses[[other_index]]$strata, "SEX")
})

test_that("an empty dataSubsetId means no subset and round-trips as-is", {
  model <- ars_to_model(.ars_fixture_path())
  none  <- which(model$analyses$dataSubsetId == "")
  expect_gt(length(none), 0)

  out <- model_to_ars(model)
  expect_identical(out$analyses[[none[1]]]$dataSubsetId, "")
})

test_that("reordering groupings rebuilds orderedGroupings and keeps the flags", {
  model <- ars_to_model(.hand_built_ars())

  ## Two groupings, so the order is observable.
  model$groupings <- rbind(model$groupings, model$groupings)
  model$groupings$id[2] <- "GF_SEX"
  model$groupings$raw[[2]]$id <- "GF_SEX"

  edited <- model_set_field(model, "analyses", "AN_1", "grouping_ids",
                            "GF_SEX;GF_TRT")
  out <- model_to_ars(edited)
  groupings <- out$analyses[[1]]$orderedGroupings

  expect_equal(length(groupings), 2)
  expect_equal(groupings[[1]]$groupingId, "GF_SEX")
  expect_equal(groupings[[1]]$order, 1)
  expect_equal(groupings[[2]]$groupingId, "GF_TRT")
  expect_true(groupings[[2]]$resultsByGroup)
})

test_that("an untouched grouping list keeps its original node", {
  original <- .read_json(.ars_fixture_path())
  model    <- ars_to_model(.ars_fixture_path())

  ## Editing an unrelated field must not rebuild orderedGroupings.
  edited <- model_set_field(model, "analyses", model$analyses$id[1],
                            "label", "A new label")
  out <- model_to_ars(edited)

  expect_identical(
    out$analyses[[1]]$orderedGroupings,
    original$analyses[[1]]$orderedGroupings
  )
})

test_that("a structural change regenerates the tables of contents", {
  original <- .read_json(.ars_fixture_path())
  model    <- ars_to_model(.ars_fixture_path())

  output_id <- model$outputs$id[1]
  refs <- .split_values(model$outputs$referenced_analysis_ids[1])
  expect_gt(length(refs), 3)

  dropped <- refs[1]
  edited <- model_set_field(model, "outputs", output_id,
                            "referenced_analysis_ids",
                            paste(refs[-1], collapse = ";"))
  out <- model_to_ars(edited)

  sublist <- out$mainListOfContents$contentsList$listItems[[1]]$sublist$listItems
  listed  <- vapply(sublist, function(x) x$analysisId, character(1))

  expect_false(dropped %in% listed)
  expect_equal(length(listed), length(refs) - 1)

  ## Outputs that did not change keep their original entries.
  expect_identical(
    out$mainListOfContents$contentsList$listItems[[2]],
    original$mainListOfContents$contentsList$listItems[[2]]
  )
})

test_that("regenerated contents keep the minimum-of-three padding", {
  model <- ars_to_model(.hand_built_ars())

  ## One analysis referenced, so the sublist must be padded to three.
  edited <- model_set_field(model, "outputs", "T_1",
                            "referenced_analysis_ids", "AN_1")
  edited$analyses <- edited$analyses[0, ]
  out <- model_to_ars(edited)

  sublist <- out$mainListOfContents$contentsList$listItems[[1]]$sublist$listItems
  expect_equal(length(sublist), 3)
})

test_that("compound expressions and unparsed populations survive untouched", {
  ars   <- .hand_built_ars()
  model <- ars_to_model(ars)

  expect_true(model$data_subsets$is_compound[1])
  expect_true(is.na(model$data_subsets$condition_dataset[1]))
  expect_equal(model$analysis_sets$annotationText[2],
               "subjects who did something unusual")

  expect_equal(model_to_ars(model), ars)
})

test_that("a minimal reporting event without analyses is tolerated", {
  path     <- test_path("fixtures", "tfrmt_reporting_event.json")
  original <- .read_json(path)
  model    <- ars_to_model(path)

  expect_equal(nrow(model$analyses), 0)
  expect_equal(nrow(model$methods), 0)
  expect_true(all(.ANALYSIS_COLUMNS %in% names(model$analyses)))
  expect_gt(nrow(model$groupings), 0)

  out <- model_to_ars(model)
  expect_equal(out, original)

  ## Pools and contents lists the file never had are not invented.
  expect_false("analyses" %in% names(out))
  expect_false("mainListOfContents" %in% names(out))
})

test_that("an unrecognized groupingVariable shape reads as unset, untouched", {
  ## Only the official flat strings are read and written. Anything else --
  ## here the nested object an early arsbridge once emitted -- must neither
  ## crash, nor be misread, nor be altered by edits to other fields. The
  ## remedy for such a file is regenerating it, and ars_conformance() says so.
  ars <- .hand_built_ars()
  ars$analysisGroupings[[1]]$groupingVariable <- list(
    dataset = "ADSL", variable = "TRT01A"
  )
  ars$analysisGroupings[[1]]$groupingDataset <- NULL

  model <- ars_to_model(ars)
  expect_true(is.na(model$groupings$groupingVariable[1]))
  expect_true(is.na(model$groupings$groupingDataset[1]))

  expect_equal(model_to_ars(model), ars)

  relabelled <- model_set_field(model, "groupings", model$groupings$id[1],
                                "label", "Renamed")
  node <- model_to_ars(relabelled)$analysisGroupings[[1]]
  expect_equal(node$label, "Renamed")
  expect_equal(node$groupingVariable,
               list(dataset = "ADSL", variable = "TRT01A"))
})

test_that("an output with no displays or file specification is tolerated", {
  ## Hand-written and partially populated events are real; reading one must
  ## not be an error.
  ars <- list(
    id = "S", name = "S", version = "1",
    analyses = list(list(id = "AN_1", label = "Line", dataset = "ADSL",
                         variable = "SEX")),
    outputs = list(list(id = "T_1", name = "T-1",
                        referencedAnalysisIds = list("AN_1")))
  )

  model <- ars_to_model(ars)

  expect_equal(nrow(model$outputs), 1)
  expect_true(is.na(model$outputs$display_title))
  expect_true(is.na(model$outputs$file_name))
  expect_equal(model$outputs$n_analyses, 1)
  expect_equal(model_to_ars(model), ars)
})

test_that("ars_to_model() accepts a parsed event and rejects anything else", {
  ars   <- .hand_built_ars()
  model <- ars_to_model(ars)

  expect_s3_class(model, "ars_model")
  expect_null(model$source_path)
  expect_equal(nrow(model$analyses), 1)

  expect_error(ars_to_model(42), "must be a path")
  expect_error(model_to_ars(list(a = 1)), "must be an")
})

test_that("derived columns are refreshed after an edit", {
  model <- ars_to_model(.hand_built_ars())

  edited <- model_set_field(model, "outputs", "T_1",
                            "referenced_analysis_ids", "AN_1;AN_2")
  expect_equal(edited$outputs$n_analyses[1], 2)

  relabelled <- model_set_field(model, "data_subsets", "DS_COMPOUND",
                                "label", "Adults")
  expect_equal(relabelled$data_subsets$label[1], "Adults")
  expect_true(relabelled$data_subsets$is_compound[1])
})

test_that("ids are read-only and unknown fields are rejected", {
  model <- ars_to_model(.hand_built_ars())

  expect_error(
    model_set_field(model, "analyses", "AN_1", "id", "AN_2"),
    "read-only"
  )
  expect_error(
    model_set_field(model, "analyses", "AN_1", "nope", "x"),
    "not a column"
  )
  expect_error(
    model_set_field(model, "analyses", "AN_MISSING", "label", "x"),
    "No .* in the"
  )
})

test_that("the raw-JSON escape hatch replaces a node but pins the id", {
  model <- ars_to_model(.hand_built_ars())

  replacement <- jsonlite::toJSON(list(
    id = "DS_COMPOUND", name = "Simple now", label = "Simple now",
    condition = list(dataset = "ADSL", variable = "AGE",
                     comparator = "GE", value = list("18")),
    level = 1L, order = 1L
  ), auto_unbox = TRUE)

  edited <- model_set_node_json(model, "data_subsets", "DS_COMPOUND",
                                replacement)
  expect_false(edited$data_subsets$is_compound[1])
  expect_equal(edited$data_subsets$condition_variable[1], "AGE")

  out <- model_to_ars(edited)
  expect_null(out$dataSubsets[[1]]$compoundExpression)
  expect_equal(out$dataSubsets[[1]]$condition$variable, "AGE")

  expect_error(
    model_set_node_json(model, "data_subsets", "DS_COMPOUND", "{not json"),
    "not valid JSON"
  )

  ## A replacement must survive the refresh that follows it. Refreshing by
  ## patching from the row's OLD columns would quietly undo the whole edit
  ## while still reporting success.
  simple <- ars_to_model(.ars_fixture_path())
  target <- simple$data_subsets$id[1]
  rewritten <- jsonlite::toJSON(list(
    id = target, name = "Rewritten", label = "Rewritten",
    condition = list(dataset = "ADSL", variable = "AGE",
                     comparator = "GE", value = list("18")),
    level = 1L, order = 1L
  ), auto_unbox = TRUE)

  replaced <- model_set_node_json(simple, "data_subsets", target, rewritten)
  expect_equal(replaced$data_subsets$condition_variable[1], "AGE")
  expect_equal(replaced$data_subsets$label[1], "Rewritten")
  expect_equal(model_to_ars(replaced)$dataSubsets[[1]]$condition$variable,
               "AGE")
  expect_error(
    model_set_node_json(model, "data_subsets", "DS_COMPOUND",
                        '{"id": "DS_OTHER"}'),
    "must stay"
  )
})

test_that("column edits and node replacements can be interleaved", {
  ## The bug class behind the raw-JSON regression: whichever of the row and
  ## the node changed LAST must win, and nothing earlier may resurface. Each
  ## step here would have exposed a refresh that patches from the wrong side.
  model <- ars_to_model(.ars_fixture_path())
  target <- model$data_subsets$id[1]

  ## 1. Column edit first.
  model <- model_set_field(model, "data_subsets", target, "label",
                           "Renamed by column")

  ## 2. Node replacement second -- must not lose the world around it.
  rewritten <- jsonlite::toJSON(list(
    id = target, name = "Replaced", label = "Replaced",
    condition = list(dataset = "ADSL", variable = "AGE",
                     comparator = "GE", value = list("18")),
    level = 1L, order = 1L
  ), auto_unbox = TRUE)
  model <- model_set_node_json(model, "data_subsets", target, rewritten)
  expect_equal(model$data_subsets$label[1], "Replaced")

  ## 3. Column edit after the replacement -- must build on it, not on the
  ##    pre-replacement row.
  model <- model_set_field(model, "data_subsets", target,
                           "condition_value", "21")

  node <- model_to_ars(model)$dataSubsets[[1]]
  expect_equal(node$label, "Replaced")
  expect_equal(node$condition$variable, "AGE")
  expect_equal(node$condition$value, list("21"))
})

test_that("an operation edit survives later column edits on its method", {
  model <- ars_to_model(.ars_fixture_path())
  method_id <- model$methods$id[1]

  model <- model_set_operation(model, method_id, 1, "label", "Renamed op")
  model <- model_set_field(model, "methods", method_id, "description",
                           "Edited after the operation")

  node <- model_to_ars(model)$methods[[1]]
  expect_equal(node$operations[[1]]$label, "Renamed op")
  expect_equal(node$description, "Edited after the operation")
})

test_that("derived columns refuse writes instead of silently reverting them", {
  model <- ars_to_model(.ars_fixture_path())

  expect_error(
    model_set_field(model, "analyses", model$analyses$id[1],
                    "output_id", "T_ELSEWHERE"),
    "derived"
  )
  expect_error(
    model_set_field(model, "outputs", model$outputs$id[1], "n_analyses", 99),
    "derived"
  )
  expect_error(
    model_set_field(model, "data_subsets", model$data_subsets$id[1],
                    "condition_summary", "x"),
    "derived"
  )

  ## Every declared derived column really is a column of its pool, so the
  ## guard cannot drift from the schema.
  for (pool in names(.DERIVED_COLUMNS)) {
    expect_true(
      all(.DERIVED_COLUMNS[[pool]] %in% names(model[[pool]])),
      label = paste("derived columns exist in", pool)
    )
  }
})

test_that("method operations are editable and the contract fields are not", {
  model <- ars_to_model(.hand_built_ars())

  edited <- model_set_operation(model, "MTH_COUNT_AND_PERCENTAGE", 1,
                                "label", "Count")
  out <- model_to_ars(edited)

  expect_equal(out$methods[[1]]$operations[[1]]$label, "Count")
  expect_equal(out$methods[[1]]$operations[[1]]$id, "OP_N")

  expect_error(
    model_set_operation(model, "MTH_COUNT_AND_PERCENTAGE", 1, "id", "OP_X"),
    "editable"
  )
  expect_error(
    model_set_operation(model, "MTH_COUNT_AND_PERCENTAGE", 99, "label", "x"),
    "no operation"
  )
})

test_that("a standard method can be added to a file that lacks it", {
  model <- ars_to_model(.hand_built_ars())
  expect_false("MTH_SUBJECT_COUNT" %in% model$methods$id)

  added <- model_add_method_from_catalogue(model, "MTH_SUBJECT_COUNT")
  expect_true("MTH_SUBJECT_COUNT" %in% added$methods$id)
  expect_gt(added$methods$n_operations[added$methods$id == "MTH_SUBJECT_COUNT"],
            0)

  out <- model_to_ars(added)
  ids <- vapply(out$methods, function(m) m$id, character(1))
  expect_true("MTH_SUBJECT_COUNT" %in% ids)

  ## Adding twice is a no-op, and unknown ids are refused.
  expect_equal(
    nrow(model_add_method_from_catalogue(added, "MTH_SUBJECT_COUNT")$methods),
    nrow(added$methods)
  )
  expect_error(
    model_add_method_from_catalogue(model, "MTH_MADE_UP"),
    "not a standard"
  )
})

test_that("entity usage counts how many analyses share each entity", {
  model <- ars_to_model(.ars_fixture_path())
  usage <- .entity_usage(model)

  expect_true(all(c("methods", "analysis_sets", "data_subsets", "groupings")
                  %in% names(usage)))
  expect_equal(
    sum(usage$methods),
    sum(!is.na(model$analyses$methodId))
  )
  ## The "no subset" sentinel is not counted as usage of a data subset.
  expect_false("" %in% names(usage$data_subsets))
})

test_that("print() summarizes the model", {
  model <- ars_to_model(.ars_fixture_path())

  ## cli writes through the condition system rather than to stdout.
  summary_text <- paste(
    cli::cli_fmt(print(model)),
    collapse = " "
  )

  expect_match(summary_text, "ARS model")
  expect_match(summary_text, "49 analyses")
  expect_match(summary_text, "12 outputs")
  expect_invisible(print(model))
})

test_that("a freshly generated event still round-trips", {
  ## Guards against the committed fixture drifting away from what the
  ## generator actually emits.
  skip_on_cran()
  skip_on_ci()

  result <- withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(spec_to_ars_example(
      api_key     = "",
      output_path = withr::local_tempfile(fileext = ".json"),
      report_path = withr::local_tempfile(fileext = ".xlsx"),
      verbose     = FALSE
    ))
  )

  fresh <- .read_json(result$ars_path)
  model <- ars_to_model(result$ars_path)

  expect_equal(model_to_ars(model), fresh)
  expect_setequal(
    names(fresh),
    names(.read_json(.ars_fixture_path()))
  )
})

## --- Child groups inside a grouping factor ---------------------------------
##
## A grouping's `groups` are a nested list, so they get accessors of their own
## rather than flat columns -- the same shape model_set_operation() uses for a
## method's operations. The contract these tests hold: the node is written
## directly and refreshed with patch = FALSE, and .patch_grouping_node() is
## never taught about `groups` (that it does not touch them is what makes a
## parent-field edit unable to drop a child).

.cg_model <- function() {
  ars <- .read_json(.ars_fixture_path())
  index <- which(vapply(ars$analysisGroupings,
                        function(node) identical(node$id, "GF_TRT01A"),
                        logical(1)))
  ars$analysisGroupings[[index]]$dataDriven <- FALSE
  ars$analysisGroupings[[index]]$groups <- list(
    list(id = "GRP_TRT01A_DRUG_A", name = "Drug A", label = "Drug A",
         level = 1L, order = 1L,
         condition = list(dataset = "ADSL", variable = "TRT01A",
                          comparator = "EQ", value = list("Drug A"))),
    list(id = "GRP_TRT01A_PLACEBO", name = "Placebo", label = "Placebo",
         level = 1L, order = 2L,
         condition = list(dataset = "ADSL", variable = "TRT01A",
                          comparator = "EQ", value = list("Placebo")))
  )
  ars_to_model(ars)
}

.cg_groups <- function(model, grouping_id = "GF_TRT01A") {
  model$groupings$raw[[match(grouping_id, model$groupings$id)]]$groups
}

test_that("a child group can be added to a grouping that had none", {
  model <- ars_to_model(.ars_fixture_path())
  expect_equal(model$groupings$n_groups[match("GF_TRT01A", model$groupings$id)],
               0L)

  model <- model_add_group(model, "GF_TRT01A", "Drug A",
                           condition = .cond("ADSL", "TRT01A", "EQ", "Drug A"))

  index <- match("GF_TRT01A", model$groupings$id)
  ## The derived columns are re-read from the node, not left stale.
  expect_equal(model$groupings$n_groups[index], 1L)
  expect_equal(model$groupings$group_labels[index], "Drug A")

  node <- model_to_ars(model)$analysisGroupings[[
    which(vapply(model_to_ars(model)$analysisGroupings,
                 function(g) identical(g$id, "GF_TRT01A"), logical(1)))
  ]]
  expect_length(node$groups, 1L)
  expect_equal(node$groups[[1]]$label, "Drug A")
  expect_equal(node$groups[[1]]$level, 1L)
  expect_equal(node$groups[[1]]$order, 1L)
  expect_equal(node$groups[[1]]$condition$value, list("Drug A"))
  expect_equal(attr(model, "last_added"), "GRP_TRT01A_DRUG_A")
})

test_that("two child groups cannot share a label", {
  ## Their ids would collide, and resolve_analysis() keeps ONE group index
  ## across every factor -- so one output's declared path would quietly
  ## resolve the other group's condition.
  model <- .cg_model()
  expect_error(model_add_group(model, "GF_TRT01A", "Drug A"), "already")
  expect_length(.cg_groups(model), 2L)
})

test_that("child groups reorder and renumber together", {
  model <- .cg_model()
  model <- model_add_group(model, "GF_TRT01A", "Drug B")
  expect_equal(vapply(.cg_groups(model), function(g) g$label, ""),
               c("Drug A", "Placebo", "Drug B"))

  model <- model_move_group(model, "GF_TRT01A", "GRP_TRT01A_DRUG_B", -1L)
  groups <- .cg_groups(model)
  expect_equal(vapply(groups, function(g) g$label, ""),
               c("Drug A", "Drug B", "Placebo"))
  ## order is re-stamped to match the new positions, not left as authored.
  expect_equal(vapply(groups, function(g) as.integer(g$order), integer(1)),
               1:3)

  ## Moving past the end is a no-op rather than an error: the button stays
  ## pressable at the ends.
  model <- model_move_group(model, "GF_TRT01A", "GRP_TRT01A_DRUG_A", -1L)
  expect_equal(vapply(.cg_groups(model), function(g) g$label, "")[1], "Drug A")
})

test_that("removing a child renumbers the rest and leaves their ids alone", {
  model <- .cg_model()
  model <- model_add_group(model, "GF_TRT01A", "Drug B")

  model <- model_remove_group(model, "GF_TRT01A", "GRP_TRT01A_PLACEBO")
  groups <- .cg_groups(model)
  expect_equal(vapply(groups, function(g) g$id, ""),
               c("GRP_TRT01A_DRUG_A", "GRP_TRT01A_DRUG_B"))
  expect_equal(vapply(groups, function(g) as.integer(g$order), integer(1)),
               1:2)
  expect_equal(
    model$groupings$n_groups[match("GF_TRT01A", model$groupings$id)], 2L)
})

test_that("a child group named by a declared result path cannot be removed", {
  ## Result paths reference group ids. Removing a referenced child would
  ## leave the path dangling, so the refusal belongs in the model layer where
  ## a script hits it too, not only in the editor.
  model <- .cg_model()
  index <- match("T_14_1_1", model$outputs$id)
  if (is.na(index)) index <- 1L
  node <- model$outputs$raw[[index]]
  node$resultGroupPaths <- list(paths = list(list(
    pathId = "P1", groupIds = list("GRP_TRT01A_DRUG_A")
  )))
  model$outputs$raw[[index]] <- node

  expect_error(
    model_remove_group(model, "GF_TRT01A", "GRP_TRT01A_DRUG_A"),
    "result-group path"
  )
  expect_length(.cg_groups(model), 2L)
})

test_that("a child group's label can change without re-minting its id", {
  ## The id travels in result paths, so renaming a column must not move it.
  model <- .cg_model()
  model <- model_set_group_field(model, "GF_TRT01A", "GRP_TRT01A_DRUG_A",
                                 "label", "Drug A 10 mg")

  groups <- .cg_groups(model)
  expect_equal(groups[[1]]$id, "GRP_TRT01A_DRUG_A")
  expect_equal(groups[[1]]$label, "Drug A 10 mg")
  expect_equal(
    model$groupings$group_labels[match("GF_TRT01A", model$groupings$id)],
    paste("Drug A 10 mg", "Placebo", sep = .MODEL_SEP))
})

test_that("a condition can be created on a child that had none", {
  ## The flat where-clause path only PATCHES an existing condition, so a
  ## freshly added child could never get one. This accessor creates it.
  model <- .cg_model()
  model <- model_add_group(model, "GF_TRT01A", "Drug B")
  expect_null(.cg_groups(model)[[3]]$condition)

  model <- model_set_group_condition(model, "GF_TRT01A", "GRP_TRT01A_DRUG_B",
                                     "ADSL", "TRT01A", "IN",
                                     c("Drug B", "Drug B 20 mg"))
  group <- .cg_groups(model)[[3]]
  expect_equal(group$condition$comparator, "IN")
  expect_equal(group$condition$value, list("Drug B", "Drug B 20 mg"))
})

test_that("an empty value list is a condition, not a cleared one", {
  ## .eval_condition() reads an empty EQ as "is missing" -- a real clinical
  ## condition the flat path cannot express, because it keeps the old values
  ## whenever the new list is empty.
  model <- .cg_model()
  model <- model_set_group_condition(model, "GF_TRT01A", "GRP_TRT01A_DRUG_A",
                                     "ADSL", "TRT01A", "EQ", character(0))
  group <- .cg_groups(model)[[1]]
  expect_equal(group$condition$comparator, "EQ")
  expect_length(group$condition$value, 0L)
})

test_that("a compound child refuses a flat condition", {
  model <- .cg_model()
  groups <- .cg_groups(model)
  groups[[1]]$condition <- NULL
  groups[[1]]$compoundExpression <- list(logicalOperator = "OR",
                                         whereClauses = list())
  index <- match("GF_TRT01A", model$groupings$id)
  model$groupings$raw[[index]]$groups <- groups

  expect_error(
    model_set_group_condition(model, "GF_TRT01A", "GRP_TRT01A_DRUG_A",
                              "ADSL", "TRT01A", "EQ", "Drug A"),
    "compound"
  )
})

test_that("a parent-field edit still cannot drop a child group", {
  ## The H3 guarantee, asserted through the accessors that now write the same
  ## node: .patch_grouping_node() must stay ignorant of `groups`.
  model <- .cg_model()
  model <- model_set_field(model, "groupings", "GF_TRT01A",
                           "groupingVariable", "TRT01P")

  groups <- .cg_groups(model)
  expect_length(groups, 2L)
  expect_equal(vapply(groups, function(g) g$label, ""), c("Drug A", "Placebo"))
})

test_that("child accessors name the grouping or the group they cannot find", {
  model <- .cg_model()
  expect_error(model_add_group(model, "GF_NOPE", "X"), "GF_NOPE")
  expect_error(
    model_remove_group(model, "GF_TRT01A", "GRP_NOPE"), "GRP_NOPE")
  expect_error(
    model_set_group_field(model, "GF_TRT01A", "GRP_NOPE", "label", "X"),
    "GRP_NOPE")
})

## --- Compound expressions (editor phase 4) ---------------------------------
##
## A group holds EITHER a condition or a compoundExpression. These tests pin
## the transitions between the two shapes, because that is where a clause can
## be silently dropped or an invalid one-clause compound left behind.

## The Unknown Cohort group from the editor spec: two clauses joined by OR,
## the first an empty EQ standing for "is missing".
.ce_model <- function() {
  ars <- .read_json(.ars_fixture_path())
  index <- which(vapply(ars$analysisGroupings,
                        function(node) identical(node$id, "GF_TRT01A"),
                        logical(1)))
  ars$analysisGroupings[[index]]$dataDriven <- FALSE
  ars$analysisGroupings[[index]]$groups <- list(
    list(id = "GRP_TRT01A_DRUG_A", name = "Drug A", label = "Drug A",
         level = 1L, order = 1L,
         condition = list(dataset = "ADSL", variable = "TRT01A",
                          comparator = "EQ", value = list("Drug A"))),
    list(id = "GRP_TRT01A_UNKNOWN", name = "Unknown", label = "Unknown",
         level = 1L, order = 2L,
         compoundExpression = list(
           logicalOperator = "OR",
           whereClauses = list(
             list(condition = list(dataset = "ADSL", variable = "COHORTN",
                                   comparator = "EQ", value = list())),
             list(condition = list(dataset = "ADSL", variable = "COHORTN",
                                   comparator = "EQ", value = list("99")))
           )))
  )
  ars_to_model(ars)
}

.ce_group <- function(model, group_id = "GRP_TRT01A_UNKNOWN") {
  groups <- .cg_groups(model)
  groups[[which(vapply(groups, function(g) identical(g$id, group_id),
                       logical(1)))]]
}

test_that("adding a clause turns a simple condition into a compound", {
  model <- .ce_model()
  model <- model_add_clause(
    model, "GF_TRT01A", "GRP_TRT01A_DRUG_A",
    .cond("ADSL", "TRT01A", "EQ", "Drug A (open label)"))

  group <- .ce_group(model, "GRP_TRT01A_DRUG_A")
  ## The condition it already had is kept as the first clause, not discarded.
  expect_null(group$condition)
  expect_length(group$compoundExpression$whereClauses, 2L)
  expect_equal(group$compoundExpression$whereClauses[[1]]$condition$value,
               list("Drug A"))
  expect_equal(group$compoundExpression$whereClauses[[2]]$condition$value,
               list("Drug A (open label)"))
})

test_that("a clause is accepted as a WhereClause or as a bare condition", {
  bare <- list(dataset = "ADSL", variable = "COHORTN",
               comparator = "EQ", value = list("7"))
  wrapped <- model_add_clause(.ce_model(), "GF_TRT01A", "GRP_TRT01A_UNKNOWN",
                              list(condition = bare))
  direct  <- model_add_clause(.ce_model(), "GF_TRT01A", "GRP_TRT01A_UNKNOWN",
                              bare)
  expect_equal(.ce_group(wrapped)$compoundExpression,
               .ce_group(direct)$compoundExpression)
  expect_length(.ce_group(direct)$compoundExpression$whereClauses, 3L)
})

test_that("removing down to one clause unwraps the compound", {
  model <- model_remove_clause(.ce_model(), "GF_TRT01A",
                               "GRP_TRT01A_UNKNOWN", 1L)

  group <- .ce_group(model)
  ## One clause is not a compound worth persisting -- ARS wants at least two.
  expect_null(group$compoundExpression)
  expect_equal(group$condition$value, list("99"))
  expect_equal(group$condition$variable, "COHORTN")
})

test_that("a lone nested clause is hoisted, not re-wrapped", {
  ## The survivor is itself a compound. Wrapping it in a one-clause compound
  ## would leave exactly the shape the accessors exist to prevent.
  model  <- .ce_model()
  nested <- list(compoundExpression = list(
    logicalOperator = "AND",
    whereClauses = list(
      list(condition = list(dataset = "ADSL", variable = "SAFFL",
                            comparator = "EQ", value = list("Y"))),
      list(condition = list(dataset = "ADSL", variable = "COHORTN",
                            comparator = "EQ", value = list("1"))))))

  model <- model_add_clause(model, "GF_TRT01A", "GRP_TRT01A_UNKNOWN", nested)
  model <- model_remove_clause(model, "GF_TRT01A", "GRP_TRT01A_UNKNOWN", 1L)
  model <- model_remove_clause(model, "GF_TRT01A", "GRP_TRT01A_UNKNOWN", 1L)

  group <- .ce_group(model)
  expect_null(group$condition)
  expect_equal(group$compoundExpression$logicalOperator, "AND")
  expect_length(group$compoundExpression$whereClauses, 2L)
})

test_that("clauses reorder and the operator can change", {
  model <- .ce_model()
  model <- model_move_clause(model, "GF_TRT01A", "GRP_TRT01A_UNKNOWN", 1L, 1L)
  clauses <- .ce_group(model)$compoundExpression$whereClauses
  expect_equal(clauses[[1]]$condition$value, list("99"))
  expect_equal(clauses[[2]]$condition$value, list())

  ## Clamped at the ends, so the editor's buttons stay pressable.
  same <- model_move_clause(model, "GF_TRT01A", "GRP_TRT01A_UNKNOWN", 1L, -5L)
  expect_equal(.ce_group(same)$compoundExpression$whereClauses,
               .ce_group(model)$compoundExpression$whereClauses)

  model <- model_set_group_operator(model, "GF_TRT01A",
                                    "GRP_TRT01A_UNKNOWN", "and")
  expect_equal(.ce_group(model)$compoundExpression$logicalOperator, "AND")
})

test_that("an empty clause value list is a condition, not a cleared one", {
  model <- model_set_clause_condition(.ce_model(), "GF_TRT01A",
                                      "GRP_TRT01A_UNKNOWN", 2L,
                                      "adsl", "cohortn", "eq", character(0))
  clause <- .ce_group(model)$compoundExpression$whereClauses[[2]]
  ## An empty EQ is how a where clause says "is missing".
  expect_equal(clause$condition$value, list())
  expect_equal(clause$condition$dataset, "ADSL")
  expect_equal(clause$condition$variable, "COHORTN")
})

test_that("a nested clause refuses a flat condition", {
  nested <- list(compoundExpression = list(
    logicalOperator = "AND",
    whereClauses = list(
      list(condition = list(dataset = "ADSL", variable = "SAFFL",
                            comparator = "EQ", value = list("Y"))))))
  model <- model_add_clause(.ce_model(), "GF_TRT01A", "GRP_TRT01A_UNKNOWN",
                            nested)
  expect_error(
    model_set_clause_condition(model, "GF_TRT01A", "GRP_TRT01A_UNKNOWN", 3L,
                               "ADSL", "COHORTN", "EQ", "1"),
    "compound")
  ## Refused, and the nesting is still there.
  expect_length(.ce_group(model)$compoundExpression$whereClauses, 3L)
})

test_that("compound accessors refuse what they cannot address", {
  model <- .ce_model()
  expect_error(
    model_set_group_operator(model, "GF_TRT01A", "GRP_TRT01A_UNKNOWN", "XOR"),
    "AND")
  ## A simple-condition group has no operator and no clauses to move.
  expect_error(
    model_set_group_operator(model, "GF_TRT01A", "GRP_TRT01A_DRUG_A", "OR"),
    "no compound expression")
  expect_error(
    model_remove_clause(model, "GF_TRT01A", "GRP_TRT01A_DRUG_A", 1L),
    "no compound expression")
  expect_error(
    model_move_clause(model, "GF_TRT01A", "GRP_TRT01A_UNKNOWN", 9L, 1L),
    "no clause")
  expect_error(
    model_add_clause(model, "GF_TRT01A", "GRP_TRT01A_UNKNOWN", 42),
    "must be a WhereClause")
})

test_that("a compound group survives a parent-field edit and a round trip", {
  model <- .ce_model()
  model <- model_set_field(model, "groupings", "GF_TRT01A",
                           "groupingVariable", "TRT01P")

  ars <- model_to_ars(model)
  node <- ars$analysisGroupings[[
    which(vapply(ars$analysisGroupings,
                 function(g) identical(g$id, "GF_TRT01A"), logical(1)))]]
  group <- node$groups[[2]]
  expect_equal(group$compoundExpression$logicalOperator, "OR")
  expect_length(group$compoundExpression$whereClauses, 2L)
  ## The empty value array is meaningful and must survive as an empty array.
  expect_equal(group$compoundExpression$whereClauses[[1]]$condition$value,
               list())
})
