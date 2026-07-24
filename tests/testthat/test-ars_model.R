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

test_that("an older reporting event without analyses is tolerated", {
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

test_that("a nested groupingVariable is read and written back nested", {
  path  <- test_path("fixtures", "tfrmt_reporting_event.json")
  model <- ars_to_model(path)

  expect_equal(model$groupings$groupingVariable[1], "TRT01A")
  expect_equal(model$groupings$groupingDataset[1], "ADSL")

  edited <- model_set_field(model, "groupings", model$groupings$id[1],
                            "groupingVariable", "ZZVAR")
  out <- model_to_ars(edited)

  expect_true(is.list(out$analysisGroupings[[1]]$groupingVariable))
  expect_equal(out$analysisGroupings[[1]]$groupingVariable$variable, "ZZVAR")
  expect_equal(out$analysisGroupings[[1]]$groupingVariable$dataset, "ADSL")
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
