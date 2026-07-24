## ars_conformance(): validation against the official CDISC ARS v1.0 schema.
##
## Two promises are pinned here. The sanctioned extensions never appear in the
## findings -- stripping them is what makes the findings readable -- and the
## generator's KNOWN divergences from the standard always do, so they cannot
## quietly grow or silently disappear when either side changes.

.conformance_fixture <- function() {
  test_path("fixtures", "ars_apx_drm_301_deterministic.json")
}


test_that("findings have the documented shape and record what was stripped", {
  skip_if_not_installed("jsonvalidate")

  findings <- ars_conformance(.conformance_fixture())

  expect_s3_class(findings, "data.frame")
  expect_equal(names(findings), c("where", "keyword", "problem"))

  stripped <- attr(findings, "stripped_extensions")
  expect_true("reportingEvent$_meta" %in% stripped)
  expect_true("output$referencedAnalysisIds" %in% stripped)
  expect_true("analysis$analysisVariable" %in% stripped)
  expect_true("display$columns" %in% stripped)
  expect_true("method$supported" %in% stripped)
})

test_that("sanctioned extensions never surface as findings", {
  skip_if_not_installed("jsonvalidate")

  findings <- ars_conformance(.conformance_fixture())

  extension_names <- c(
    "_meta", "referencedAnalysisIds", "outputType", "columns",
    "analysisVariable", "annotation", "sapDescription", "includeTotal",
    "strata", "variableRole", "annotationText", "supported"
  )
  pattern <- paste0("'(", paste(extension_names, collapse = "|"), ")'")

  expect_false(any(grepl(pattern, findings$problem)))
})

test_that("the generator's known divergences are reported, not hidden", {
  skip_if_not_installed("jsonvalidate")

  findings <- ars_conformance(.conformance_fixture())
  expect_gt(nrow(findings), 0)

  ## Every analysis lacks the required reason and purpose.
  reason <- findings[grepl("'reason'", findings$problem), ]
  expect_gt(nrow(reason), 0)
  expect_true(all(grepl("^/analyses/", reason$where)))
  expect_true(any(grepl("'purpose'", findings$problem)))

  ## version is emitted as the string "1" where an integer is required.
  version <- findings[findings$where == "/version", ]
  expect_equal(version$problem, "must be integer")

  ## displays are flat where the standard wraps them as OrderedDisplay.
  expect_true(any(grepl("'display'", findings$problem) &
                    grepl("^/outputs/", findings$where)))

  ## fileType is a plain string where the standard wants a terminology
  ## object.
  expect_true(any(grepl("/fileType$", findings$where)))
})

test_that("stripping can be turned off to see the extensions too", {
  skip_if_not_installed("jsonvalidate")

  with_stripping <- ars_conformance(.conformance_fixture())
  without <- ars_conformance(.conformance_fixture(),
                             strip_extensions = FALSE)

  expect_gt(nrow(without), nrow(with_stripping))
  expect_true(any(grepl("'referencedAnalysisIds'", without$problem)))
  expect_equal(length(attr(without, "stripped_extensions")), 0)
})

test_that("a seeded violation is caught at its path", {
  skip_if_not_installed("jsonvalidate")

  ars <- .read_json(.conformance_fixture())
  ars$analysisSets[[1]]$id <- NULL

  findings <- ars_conformance(ars)
  seeded <- findings[findings$where == "/analysisSets/0" &
                       grepl("'id'", findings$problem), ]
  expect_equal(nrow(seeded), 1)
})

test_that("a path, a parsed event and a model agree", {
  skip_if_not_installed("jsonvalidate")

  from_path <- ars_conformance(.conformance_fixture())
  from_list <- ars_conformance(.read_json(.conformance_fixture()))
  from_model <- ars_conformance(ars_to_model(.conformance_fixture()))

  expect_equal(from_list, from_path, ignore_attr = TRUE)
  expect_equal(from_model, from_path, ignore_attr = TRUE)
})

test_that("bad inputs are refused with something actionable", {
  skip_if_not_installed("jsonvalidate")

  expect_error(ars_conformance(42), "must be a path")
  expect_error(
    ars_conformance(.conformance_fixture(), schema_path = "nope.json"),
    class = "rlang_error"
  )
})

test_that("the bundled schema is the pinned v1.0.0 export", {
  schema_path <- system.file("schema", "cdisc_ars_v1.0.0.schema.json",
                             package = "arsbridge")
  expect_true(file.exists(schema_path))

  schema <- jsonlite::read_json(schema_path)
  expect_equal(schema$title, "ars_ldm")
  expect_true("ReportingEvent" %in% names(schema[["$defs"]]))

  ## The LinkML source it was generated from travels with it.
  expect_true(file.exists(
    system.file("schema", "cdisc_ars_v1.0.0_ldm.yaml", package = "arsbridge")
  ))
})

test_that("saving mentions the conformance count without ever failing on it", {
  skip_if_not_installed("jsonvalidate")

  dir <- withr::local_tempdir()
  path <- file.path(dir, "reporting_event.json")
  file.copy(.conformance_fixture(), path)

  model <- ars_to_model(path)
  messages <- character(0)
  withCallingHandlers(
    .edit_ars_finish(list(model = model, edit_log = .new_edit_log()), path),
    cliMessage = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  expect_true(any(grepl("schema note", messages)))
})
