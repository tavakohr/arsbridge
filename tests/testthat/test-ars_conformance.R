## ars_conformance(): validation against the official CDISC ARS v1.0 schema.
##
## Two promises are pinned here. The sanctioned extensions never appear in
## the findings -- stripping them is what makes the findings readable -- and
## a freshly generated event reports nothing at all, so a new divergence in
## the generator cannot slip in unnoticed.

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

test_that("a freshly generated event validates clean after stripping", {
  skip_if_not_installed("jsonvalidate")

  ## The headline promise: beyond the documented extensions, the generator
  ## emits exactly what the standard requires.
  findings <- ars_conformance(.conformance_fixture())
  expect_equal(nrow(findings), 0)
})

test_that("the generator now emits what the standard requires", {
  ars <- .read_json(.conformance_fixture())

  analysis <- ars$analyses[[1]]
  expect_equal(analysis$reason$controlledTerm, "SPECIFIED IN SAP")
  expect_equal(analysis$purpose$controlledTerm, "EXPLORATORY OUTCOME MEASURE")

  ## Integer versions, wrapped displays, terminology fileType, valid role
  ## terms, named contents entries.
  expect_identical(ars$version, 1L)
  expect_identical(analysis$version, 1L)

  entry <- ars$outputs[[1]]$displays[[1]]
  expect_true(is.list(entry$display))
  expect_true(nzchar(entry$display$id))
  expect_true(nzchar(entry$display$name))

  section <- entry$display$displaySections[[1]]
  if (length(section$orderedSubSections) > 0) {
    subsection <- section$orderedSubSections[[1]]$subSection
    expect_true(nzchar(subsection$id))
    expect_true(nzchar(subsection$text))
  }

  expect_equal(ars$outputs[[1]]$fileSpecifications[[1]]$fileType$controlledTerm,
               "rtf")

  role <- ars$methods[[1]]$operations[[1]]$
    referencedOperationRelationships[[1]]$referencedOperationRole
  expect_true(role$controlledTerm %in% c("NUMERATOR", "DENOMINATOR"))

  item <- ars$mainListOfContents$contentsList$listItems[[1]]$
    sublist$listItems[[1]]
  expect_true(nzchar(item$name))
})

test_that("a non-conformant event has its divergences reported, not repaired", {
  skip_if_not_installed("jsonvalidate")

  ## Shapes an early arsbridge once emitted (string version, flat display,
  ## string fileType, no reason/purpose). There are no compatibility readers
  ## for these -- the reporter names them and the remedy is regeneration.
  old_shape <- list(
    id = "S", name = "S", version = "1",
    mainListOfContents = list(
      name = "LOPA", label = "LOPA",
      contentsList = list(listItems = list())
    ),
    analyses = list(list(
      id = "AN_1", name = "AN_1", methodId = "MTH_X", version = "1"
    )),
    methods = list(list(
      id = "MTH_X", name = "X",
      operations = list(list(id = "OP_1", name = "n"))
    )),
    outputs = list(list(
      id = "T_1", name = "T-1", version = "1",
      displays = list(list(order = 1L, displayTitle = "Flat display")),
      fileSpecifications = list(list(name = "T-1.rtf", fileType = "rtf"))
    ))
  )

  findings <- ars_conformance(old_shape)

  expect_true(any(grepl("'reason'", findings$problem)))
  expect_true(any(findings$where == "/version" &
                    findings$problem == "must be integer"))
  expect_true(any(grepl("'displayTitle'", findings$problem)))
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

  expect_true(any(grepl("schema note|Conforms to the ARS", messages)))
})
