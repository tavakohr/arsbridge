## validate_ars_model(): the checks that turn the review stage into guided
## correction. Each test seeds one defect into a real model and asserts the
## finding it should produce -- and, just as importantly, that a clean model
## produces no blockers.

.ars_validate_model <- function() {
  ars_to_model(test_path("fixtures", "ars_apx_drm_301_deterministic.json"))
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
    c("severity", "entity", "id", "field", "problem", "action", "ref")
  )
  expect_equal(sum(findings$severity == "FAIL"), 0)
  expect_true(all(findings$severity %in% c("FAIL", "WARN", "INFO")))
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
  spec  <- parse_adam_spec(arsbridge_example("adam_spec.xlsx"))
  model <- .ars_validate_model()

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
