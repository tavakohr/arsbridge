## validate_ars_model(): the checks that turn the review stage into guided
## correction. Each test seeds one defect into a real model and asserts the
## finding it should produce -- and, just as importantly, that a clean model
## produces no blockers.

.ars_validate_legacy_model <- function() {
  ars_to_model(test_path("fixtures", "ars_apx_drm_301_deterministic.json"))
}

.ars_validate_model <- function() {
  .valid_fixture_model()
}

.ars_validate_report <- function() {
  utils::read.csv(
    test_path("fixtures", "ars_apx_drm_301_validation.csv"),
    stringsAsFactors = FALSE
  )
}


test_that("the generated fixture has no blocking findings", {
  findings <- validate_ars_model(.ars_validate_model())

  expect_s3_class(findings, "data.frame")
  expect_equal(
    names(findings),
    c("severity", "entity", "id", "field", "problem", "action", "ref",
      "detail", "scope", "source_doc", "sheet", "cell_ref", "row", "col",
      "locator")
  )
  expect_equal(sum(findings$severity == "FAIL"), 0)
  expect_true(all(findings$severity %in% c("FAIL", "WARN", "INFO")))
})


## The rest of the package joins on `ref` -- which scope a defect invalidates,
## which cells to reserve, which fix to offer. That only holds if `ref` is a
## closed vocabulary, so these two tests are the contract: nothing escapes the
## registry, and nothing in the registry is a display string in disguise.

test_that("every finding carries a code from the registry", {
  models <- list(.ars_validate_model(), .ars_validate_legacy_model())

  checked <- 0L
  for (model in models) {
    findings <- validate_ars_model(model)
    checked <- checked + nrow(findings)
    expect_true(all(findings$ref %in% names(.VALIDATION_REFS)))
    expect_false(any(is.na(findings$ref)))
  }

  ## Non-vacuity: a run that produced no findings would pass the loop above
  ## while asserting nothing.
  expect_gt(checked, 5L)
})

test_that("an unregistered code is refused rather than recorded", {
  expect_error(
    .add_finding(.new_findings(), "FAIL", "analyses", "AN_X", "methodId",
                 "problem", "action", ref = "NOT_A_REGISTERED_CODE"),
    "not registered"
  )
})

test_that("the registry has no duplicates", {
  expect_equal(anyDuplicated(names(.VALIDATION_REFS)), 0L)
})

test_that("every registered code declares a known scope", {
  ## The scope is what the engine reserves on. A code with a scope nothing
  ## recognises would be silently treated as advisory -- i.e. would permit
  ## execution where it should withhold it.
  expect_true(all(.VALIDATION_REFS %in% .FINDING_SCOPES))
  expect_gt(length(.VALIDATION_REFS), 30L)
})

test_that("a finding can address the shell cell it came from", {
  model <- .rsv_model(method = .rsv_counting_method(), n_slots = 2L)
  findings <- validate_ars_model(model)
  slot <- findings[findings$ref == "METHOD_PLACEHOLDER_SLOT_MISMATCH", ]

  expect_equal(nrow(slot), 1L)
  expect_equal(slot$scope, "cell")
  expect_equal(slot$source_doc, "shell")
  expect_equal(slot$cell_ref, "B6")
  expect_equal(slot$row, 6L)
  expect_true(nzchar(slot$sheet))
})

test_that("an address is never invented", {
  ## A finding with no authored source says so, rather than pointing the
  ## author at a cell that has nothing to do with it.
  model <- .ars_validate_legacy_model()
  findings <- validate_ars_model(model)
  grouping <- findings[findings$ref == "FIXED_GROUPING_EMPTY", ]

  expect_gt(nrow(grouping), 0L)
  expect_true(all(is.na(grouping$cell_ref)))
  expect_true(all(is.na(grouping$row)))
})

test_that("a legacy fixed grouping with no groups is a blocker", {
  model <- .ars_validate_legacy_model()

  findings <- validate_ars_model(model)
  invalid <- findings[
    findings$ref %in% "FIXED_GROUPING_EMPTY",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(invalid), 1L)
  expect_equal(invalid$severity, "FAIL")
  expect_equal(invalid$id, "GF_TRT01A")
})

test_that("an empty grouping with omitted dataDriven is fixed and blocked", {
  model <- .ars_validate_legacy_model()
  grouping_index <- match("GF_TRT01A", model$groupings$id)
  model$groupings$dataDriven[grouping_index] <- NA
  model$groupings$raw[[grouping_index]]$dataDriven <- NULL

  findings <- validate_ars_model(model)
  invalid <- findings[
    findings$ref %in% "FIXED_GROUPING_EMPTY",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(invalid), 1L)
  expect_equal(invalid$severity, "FAIL")
  expect_equal(invalid$id, "GF_TRT01A")
})

test_that("direct code emission blocks an empty grouping with omitted dataDriven", {
  ars_path <- test_path(
    "fixtures", "ars_apx_drm_301_deterministic.json"
  )
  ars <- jsonlite::fromJSON(ars_path, simplifyVector = FALSE)
  grouping_index <- which(vapply(ars$analysisGroupings, function(grouping) {
    identical(grouping$id, "GF_TRT01A")
  }, logical(1)))
  ars$analysisGroupings[[grouping_index]]$dataDriven <- NULL
  code_dir <- file.path(withr::local_tempdir(), "code")

  expect_error(
    write_tlf_code(ars, code_dir),
    "FIXED_GROUPING_EMPTY",
    fixed = TRUE
  )
  expect_false(dir.exists(code_dir))
})

test_that("valid statistic lines may bind part of a method's operations", {
  model <- .ars_validate_model()

  findings <- validate_ars_model(model)

  expect_false(any(findings$ref %in% "METHOD_PLACEHOLDER_SLOT_MISMATCH"))
})

test_that("a statistic line must be supported by its analysis method", {
  model <- .ars_validate_model()
  output_index <- match("T_14_1_2", model$outputs$id)
  rows <- .shell_table_data(model$outputs$raw[[output_index]], model)$rows
  mean_row <- rows[rows$label == "Mean (SD)", , drop = FALSE][1, ]
  analysis_index <- match(mean_row$owner_analysis_id, model$analyses$id)
  model$analyses$methodId[analysis_index] <- "MTH_SUBJECT_COUNT"

  findings <- validate_ars_model(model)
  mismatch <- findings[
    findings$ref %in% "METHOD_PLACEHOLDER_SLOT_MISMATCH" &
      findings$id == mean_row$owner_analysis_id,
    ,
    drop = FALSE
  ]

  expect_gt(nrow(mismatch), 0L)
  expect_true(all(mismatch$severity == "FAIL"))
})

test_that("persisted placeholder slots must be supported for scalar rows", {
  model <- .ars_validate_model()
  output_index <- match("T_14_1_2", model$outputs$id)
  analysis_id <- .split_values(
    model$outputs$referenced_analysis_ids[output_index]
  )[[1]]
  analysis_index <- match(analysis_id, model$analyses$id)
  model$analyses$methodId[analysis_index] <- "MTH_SUBJECT_COUNT"

  output <- model$outputs$raw[[output_index]]
  output[["_meta"]][["shell_fill"]] <- list(cells = list(list(
    kind = "result",
    analysis_id = analysis_id,
    placeholder = "xx (xx.x)",
    slots = list(
      list(stat_name = "n"),
      list(stat_name = "p")
    )
  )))
  model$outputs$raw[[output_index]] <- output

  findings <- validate_ars_model(model)
  mismatch <- findings[
    findings$ref %in% "METHOD_PLACEHOLDER_SLOT_MISMATCH" &
      findings$id == analysis_id &
      grepl("xx (xx.x)", findings$problem, fixed = TRUE),
    ,
    drop = FALSE
  ]

  expect_equal(nrow(mismatch), 1L)
  expect_match(mismatch$problem, "xx \\(xx.x\\)")
})


test_that("findings come back most severe first", {
  model <- .ars_validate_model()
  model$analyses$methodId[1] <- "MTH_DOES_NOT_EXIST"

  findings <- validate_ars_model(model)
  ranks <- match(findings$severity, c("FAIL", "WARN", "INFO"))

  expect_false(is.unsorted(ranks))
  expect_equal(findings$severity[1], "FAIL")
})

test_that("a duplicate id is a blocker", {
  model <- .ars_validate_model()
  model$analyses$id[2] <- model$analyses$id[1]

  findings <- validate_ars_model(model)
  duplicate <- findings[findings$field == "id" &
                          findings$id == model$analyses$id[1], ]

  expect_gt(nrow(duplicate), 0)
  expect_equal(duplicate$severity[1], "FAIL")
})

test_that("a missing id is a blocker", {
  model <- .ars_validate_model()
  model$methods$id[1] <- NA_character_

  findings <- validate_ars_model(model)
  missing <- findings[findings$entity == "methods" & findings$field == "id", ]

  expect_gt(nrow(missing), 0)
  expect_equal(missing$severity[1], "FAIL")
})

test_that("references that do not resolve are blockers", {
  model <- .ars_validate_model()
  model$analyses$methodId[1]      <- "MTH_GONE"
  model$analyses$analysisSetId[2] <- "AS_GONE"
  model$analyses$dataSubsetId[3]  <- "DS_GONE"

  findings <- validate_ars_model(model)
  dangling <- findings[findings$severity == "FAIL", ]

  expect_true(any(grepl("MTH_GONE", dangling$problem)))
  expect_true(any(grepl("AS_GONE", dangling$problem)))
  expect_true(any(grepl("DS_GONE", dangling$problem)))
})

test_that("an empty dataSubsetId is not treated as a dangling reference", {
  model <- .ars_validate_model()
  expect_gt(sum(model$analyses$dataSubsetId == ""), 0)

  findings <- validate_ars_model(model)
  subset_findings <- findings[findings$field == "dataSubsetId", ]

  expect_equal(nrow(subset_findings), 0)
})

test_that("a grouping reference that does not resolve is a blocker", {
  model <- .ars_validate_model()
  model$analyses$grouping_ids[1] <- "GF_GONE"

  findings <- validate_ars_model(model)
  grouping <- findings[findings$field == "grouping_ids", ]

  expect_gt(nrow(grouping), 0)
  expect_equal(grouping$severity[1], "FAIL")
})

test_that("an output referencing an analysis that is gone is a blocker", {
  model <- .ars_validate_model()
  refs <- .split_values(model$outputs$referenced_analysis_ids[1])
  model$outputs$referenced_analysis_ids[1] <- paste(
    c(refs, "AN_GONE"), collapse = ";"
  )

  findings <- validate_ars_model(model)
  dangling <- findings[findings$field == "referenced_analysis_ids", ]

  expect_gt(nrow(dangling), 0)
  expect_equal(dangling$severity[1], "FAIL")
  expect_true(any(grepl("AN_GONE", dangling$problem)))
})

test_that("an analysis no output displays is a warning", {
  model <- .ars_validate_model()
  refs <- .split_values(model$outputs$referenced_analysis_ids[1])
  orphaned <- refs[1]
  model$outputs$referenced_analysis_ids[1] <- paste(refs[-1], collapse = ";")

  findings <- validate_ars_model(model)
  orphan <- findings[findings$id == orphaned & findings$field == "output_id", ]

  expect_equal(nrow(orphan), 1)
  expect_equal(orphan$severity, "WARN")
})

test_that("a method with no executor is flagged by how it will behave", {
  ## MTH_UNSUPPORTED_ANALYSIS reserves an empty cell; a method with no
  ## executor at all falls back to the generic summarizer.
  expect_equal(.method_execution_class("MTH_COUNT_AND_PERCENTAGE"), "native")
  expect_equal(.method_execution_class("MTH_LISTING"), "native")
  expect_equal(.method_execution_class("MTH_UNSUPPORTED_ANALYSIS"),
               "unsupported")
  expect_equal(.method_execution_class("MTH_KAPLAN_MEIER_ESTIMATE"),
               "fallback")
  expect_equal(.method_execution_class("MTH_PROPORTION_CI_EXACT"),
               "conditional")
  expect_equal(.method_execution_class(NA_character_), "missing")
})

test_that("a CMH test without a stratification variable is a warning", {
  expect_equal(.method_execution_class("MTH_CMH_TEST", "BASELINE"),
               "conditional")
  expect_equal(.method_execution_class("MTH_CMH_TEST", NA_character_),
               "blocked")

  model <- .ars_validate_model()
  cmh <- which(model$analyses$methodId == "MTH_CMH_TEST")
  skip_if(length(cmh) == 0, "fixture has no CMH analysis")

  model$analyses$strata[cmh[1]] <- NA_character_
  findings <- validate_ars_model(model)
  blocked <- findings[findings$id == model$analyses$id[cmh[1]] &
                        findings$field == "strata", ]

  expect_equal(nrow(blocked), 1)
  expect_equal(blocked$severity, "WARN")
})

test_that("an analysis with no method at all is a blocker", {
  model <- .ars_validate_model()
  model$analyses$methodId[1] <- NA_character_

  findings <- validate_ars_model(model)
  no_method <- findings[findings$id == model$analyses$id[1] &
                          findings$field == "methodId", ]

  expect_equal(no_method$severity[1], "FAIL")
})

test_that("a population that was never parsed is a warning", {
  model <- .ars_validate_model()
  model$analysis_sets$annotationText[1] <- "some unparsed population"

  findings <- validate_ars_model(model)
  unparsed <- findings[findings$field == "annotationText", ]

  expect_equal(nrow(unparsed), 1)
  expect_equal(unparsed$severity, "WARN")
})

test_that("contents entries pointing at nothing are a warning, not a blocker", {
  model <- .ars_validate_model()
  model$analyses <- model$analyses[-1, ]

  findings <- validate_ars_model(model)
  contents <- findings[findings$entity == "contents", ]

  expect_gt(nrow(contents), 0)
  expect_true(all(contents$severity == "WARN"))
})

test_that("padding duplicates in the contents list are not reported", {
  ## Every LOPA sublist is padded by repeating the last analysis id. Those
  ## duplicates are deliberate and must never surface as findings.
  model <- .ars_validate_model()
  findings <- validate_ars_model(model)

  expect_equal(nrow(findings[findings$entity == "contents", ]), 0)
})

test_that("the ADaM spec overlay flags datasets and variables it cannot find", {
  ## The model and the spec must belong to the SAME study for "clean
  ## validates clean" to mean anything -- the frozen APX fixture model
  ## references dermatology variables no Alzheimer spec can carry. So this
  ## test builds its model from the bundle itself: the deterministic parse
  ## of the bundled shell, validated against the bundled spec.
  spec <- parse_adam_spec(arsbridge_example("adam_spec.xlsx"))
  ars_path <- withr::local_tempfile(fileext = ".json")
  withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(suppressWarnings(spec_to_ars_example(
      output_path = ars_path,
      report_path = withr::local_tempfile(fileext = ".xlsx"),
      api_key = "", verbose = FALSE))))
  model <- ars_to_model(ars_path)

  clean <- validate_ars_model(model, spec = spec)
  expect_equal(sum(clean$severity == "FAIL"), 0)

  model$analyses$dataset[1]  <- "ADNOPE"
  model$analyses$variable[2] <- "NOSUCHVAR"
  findings <- validate_ars_model(model, spec = spec)

  bad_dataset <- findings[findings$id == model$analyses$id[1] &
                            findings$severity == "FAIL", ]
  expect_gt(nrow(bad_dataset), 0)
  expect_true(any(grepl("ADNOPE", bad_dataset$problem)))

  bad_variable <- findings[findings$id == model$analyses$id[2] &
                             findings$severity == "WARN", ]
  expect_true(any(grepl("NOSUCHVAR", bad_variable$problem)))
})

test_that("the validation report finds nothing missing in a complete event", {
  model  <- .ars_validate_model()
  report <- .ars_validate_report()

  baseline <- validate_ars_model(model)
  with_report <- validate_ars_model(model, report = report)

  expect_equal(nrow(with_report), nrow(baseline))
})

test_that("an annotated shell line with no analysis is reported as a gap", {
  model  <- .ars_validate_model()
  report <- .ars_validate_report()

  ## Drop one analysis, exactly as if the generator had missed it.
  target <- model$analyses$id[model$analyses$output_id == "T_14_1_2" &
                                model$analyses$variable == "SEX"]
  skip_if(length(target) == 0, "fixture has no ADSL.SEX analysis")

  model$analyses <- model$analyses[model$analyses$id != target[1], ]
  index <- which(model$outputs$id == "T_14_1_2")
  refs <- setdiff(
    .split_values(model$outputs$referenced_analysis_ids[index]),
    target[1]
  )
  model$outputs$referenced_analysis_ids[index] <- paste(refs, collapse = ";")

  findings <- validate_ars_model(model, report = report)
  gaps <- findings[findings$field == "analyses", ]

  expect_gt(nrow(gaps), 0)
  expect_true(any(grepl("ADSL.SEX", gaps$problem, fixed = TRUE)))
  expect_true(all(gaps$severity == "WARN"))
})

test_that("population rows in the report are not mistaken for missing lines", {
  model  <- .ars_validate_model()
  report <- .ars_validate_report()

  expect_true(any(report$stub_label == "<population>"))

  findings <- validate_ars_model(model, report = report)
  gaps <- findings[findings$field == "analyses", ]

  expect_equal(nrow(gaps), 0)
})

test_that("a report of the wrong shape is ignored with a warning", {
  model <- .ars_validate_model()
  expect_warning(
    validate_ars_model(model, report = data.frame(nope = 1)),
    "does not look like a validation report"
  )
})

test_that("a value containing the composite separator is called out", {
  model <- .ars_validate_model()
  model$condition_value <- NULL
  model$data_subsets$condition_value[1] <- "A;B"
  model$data_subsets$is_compound[1] <- FALSE

  findings <- validate_ars_model(model)
  separator <- findings[findings$id == model$data_subsets$id[1] &
                          findings$field == "condition_value", ]

  expect_gt(nrow(separator), 0)
  expect_equal(separator$severity[1], "INFO")
})

test_that("validate_ars_model() refuses anything that is not a model", {
  expect_error(validate_ars_model(list(a = 1)), "must be an")
})

test_that("the validation gate blocks only FAIL findings", {
  findings <- rbind(
    .add_finding(.new_findings(), "WARN", "analyses", "AN_WARN",
                 "methodId", "Review this method.", "Choose a native method.",
                 ref = "METHOD_FALLBACK_SUMMARIZER"),
    .add_finding(.new_findings(), "INFO", "outputs", "OUT_INFO",
                 "columns", "This is informational.", "No action needed.",
                 ref = "METHOD_CONDITIONAL")
  )

  review_gate <- .validation_gate(findings)

  expect_false(review_gate$blocked)
  expect_equal(review_gate$status, "ready")
  expect_equal(nrow(review_gate$blocking_findings), 0L)
  expect_length(review_gate$blocking_refs, 0L)

  blocking <- .add_finding(
    findings, "FAIL", "groupings", "GF_EMPTY", "groups",
    "This fixed grouping has no groups.", "Add groups or make it data-driven.",
    ref = "FIXED_GROUPING_EMPTY"
  )
  blocked_gate <- .validation_gate(blocking)

  expect_true(blocked_gate$blocked)
  expect_equal(blocked_gate$status, "needs-fixes")
  expect_equal(blocked_gate$blocking_findings$id, "GF_EMPTY")
  expect_equal(blocked_gate$blocking_refs, "FIXED_GROUPING_EMPTY")
  expect_match(blocked_gate$summary, "Add groups or make it data-driven", fixed = TRUE)
})

test_that("direct execution APIs refuse a structurally blocked event", {
  ars_path <- test_path(
    "fixtures", "ars_apx_drm_301_deterministic.json"
  )
  adam_dir <- test_path("fixtures", "adam_apx_drm_301")
  code_dir <- file.path(withr::local_tempdir(), "code")
  filled_path <- tempfile(fileext = ".xlsx")

  expect_error(
    write_tlf_code(ars_path, code_dir),
    "FIXED_GROUPING_EMPTY",
    fixed = TRUE
  )
  expect_false(dir.exists(code_dir))

  expect_error(
    ars_to_ard(ars_path, adam_dir),
    "FIXED_GROUPING_EMPTY",
    fixed = TRUE
  )

  expect_error(
    ars_fill_shell(
      shell_path = test_path("fixtures", "shells_apx_drm_301.xlsx"),
      ars = ars_path,
      ard = data.frame(),
      output_path = filled_path
    ),
    "FIXED_GROUPING_EMPTY",
    fixed = TRUE
  )
  expect_false(file.exists(filled_path))
})


## --- where-clause variables ------------------------------------------------
##
## The spec overlay used to look only at the flat condition columns, so a
## variable named inside a CHILD group's condition -- or inside any compound
## expression -- was never checked. That is the easiest wrong variable to
## write and the hardest to notice: the column still renders, just empty.

test_that("a where clause yields every variable it names, at any depth", {
  nested <- list(compoundExpression = list(
    logicalOperator = "OR",
    whereClauses = list(
      list(condition = list(dataset = "ADSL", variable = "TRT01A")),
      list(condition = list(dataset = "ADSL", variable = "TRT01AN")),
      list(compoundExpression = list(
        logicalOperator = "AND",
        whereClauses = list(
          list(condition = list(dataset = "ADAE", variable = "AESER"))))))))

  refs <- .where_clause_refs(nested)
  expect_equal(
    vapply(refs, function(r) paste0(r$dataset, ".", r$variable), character(1)),
    c("ADSL.TRT01A", "ADSL.TRT01AN", "ADAE.AESER")
  )

  ## A simple condition is one reference; a group carrying neither is none.
  expect_length(
    .where_clause_refs(list(condition = list(dataset = "ADSL",
                                             variable = "SAFFL"))), 1L)
  expect_length(.where_clause_refs(list(id = "GRP_X")), 0L)
  expect_length(.where_clause_refs(NULL), 0L)
})

test_that("one variable used twice in a clause is one finding, not two", {
  twice <- list(compoundExpression = list(
    logicalOperator = "OR",
    whereClauses = list(
      list(condition = list(dataset = "ADSL", variable = "AGEGR1")),
      list(condition = list(dataset = "ADSL", variable = "AGEGR1")))))
  expect_length(.where_clause_refs(twice), 1L)
})

test_that("a child group's condition is checked against the ADaM spec", {
  spec <- parse_adam_spec(arsbridge_example("adam_spec.xlsx"))
  model <- .valid_fixture_model()

  index <- match("GF_TRT01A", model$groupings$id)
  ## TRT01AN is not in this study's ADSL at all -- the exact shape of a
  ## column that would silently select nobody.
  model$groupings$raw[[index]][["groups"]] <- list(
    list(id = "GRP_ONE", label = "One", level = 1L, order = 1L,
         compoundExpression = list(
           logicalOperator = "OR",
           whereClauses = list(
             list(condition = list(dataset = "ADSL", variable = "TRT01A",
                                   comparator = "EQ", value = list("x"))),
             list(condition = list(dataset = "ADSL", variable = "TRT01AN",
                                   comparator = "EQ", value = list("54"))))))
  )

  findings <- validate_ars_model(model, spec = spec)
  hit <- findings[grepl("TRT01AN", findings$problem), , drop = FALSE]

  expect_equal(nrow(hit), 1L)
  expect_equal(hit$severity[1], "WARN")
  expect_equal(hit$entity[1], "groupings")
  ## The field names the child, so two bad children are two lines rather than
  ## one collapsed one.
  expect_true(grepl("GRP_ONE", hit$field[1], fixed = TRUE))
})

test_that("a compound analysis set has its clauses checked too", {
  spec <- parse_adam_spec(arsbridge_example("adam_spec.xlsx"))
  model <- .valid_fixture_model()

  ## The flat columns hold nothing for a compound, so this row used to be
  ## skipped outright. An entity carries ONE representation, so the simple
  ## condition goes when the compound arrives.
  model$analysis_sets$is_compound[1] <- TRUE
  model$analysis_sets$raw[[1]][["condition"]] <- NULL
  model$analysis_sets$raw[[1]][["compoundExpression"]] <- list(
    logicalOperator = "AND",
    whereClauses = list(
      list(condition = list(dataset = "ADSL", variable = "SAFFL",
                            comparator = "EQ", value = list("Y"))),
      list(condition = list(dataset = "ADSL", variable = "NOSUCHVAR",
                            comparator = "EQ", value = list("Y"))))
  )

  findings <- validate_ars_model(model, spec = spec)
  expect_true(any(grepl("NOSUCHVAR", findings$problem)))
})

test_that("the check validates the variable, not the value", {
  ## Worth pinning: AGEGR1 exists, so a wrong VALUE for it ("<65" where the
  ## data holds "18-64") is NOT caught here. Only a missing variable is.
  spec <- parse_adam_spec(arsbridge_example("adam_spec.xlsx"))
  model <- .valid_fixture_model()

  index <- match("GF_TRT01A", model$groupings$id)
  model$groupings$raw[[index]][["groups"]] <- list(
    list(id = "GRP_AGE", label = "Age", level = 1L, order = 1L,
         condition = list(dataset = "ADSL", variable = "AGEGR1",
                          comparator = "EQ", value = list("<65")))
  )

  findings <- validate_ars_model(model, spec = spec)
  expect_false(any(grepl("AGEGR1", findings$problem)))
})
