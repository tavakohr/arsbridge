test_that("parse_adam_spec returns variables, lookup, and codelists", {
  spec <- parse_adam_spec(test_path("fixtures/adam_spec_minimal.xlsx"))
  expect_named(spec, c("variables", "lookup", "codelists"))
  expect_true(is.data.frame(spec$variables))
  expect_equal(nrow(spec$variables), 10)
  expect_setequal(unique(spec$variables$dataset), c("ADSL", "ADAE"))
  ## The minimal fixture has no codelist sheet -- empty, never NULL.
  expect_identical(spec$codelists, list())
})

test_that("lookup is keyed by DATASET.VARIABLE and resolves known vars", {
  spec <- parse_adam_spec(test_path("fixtures/adam_spec_minimal.xlsx"))
  expect_true("ADSL.AGE"     %in% names(spec$lookup))
  expect_true("ADAE.TRTEMFL" %in% names(spec$lookup))
  expect_equal(spec$lookup$ADSL.AGE$label, "Age")
  expect_equal(spec$lookup$ADAE.AEDECOD$label, "Dictionary-Derived Term")
})

test_that("parse_adam_spec aborts on missing file", {
  expect_error(parse_adam_spec("nonexistent.xlsx"), "not found")
})

test_that("parse_adam_spec dispatches to the define.xml branch on .xml input", {
  spec <- parse_adam_spec(test_path("fixtures/adam_define_minimal.xml"))
  expect_named(spec, c("variables", "lookup", "codelists"))
  expect_true(is.data.frame(spec$variables))
  expect_setequal(unique(spec$variables$dataset), c("ADSL", "ADAE"))
  expect_true("ADSL.AGE"     %in% names(spec$lookup))
  expect_true("ADSL.SAFFL"   %in% names(spec$lookup))
  expect_true("ADAE.TRTEMFL" %in% names(spec$lookup))
  expect_equal(spec$lookup$ADSL.AGE$label, "Age")
})

test_that("parse_adam_spec rejects unsupported extensions (e.g. .csv)", {
  bad <- tempfile(fileext = ".csv")
  writeLines("dataset,variable\nADSL,AGE", bad)
  expect_error(parse_adam_spec(bad), "Unsupported")
})

## --- Codelist parsing -------------------------------------------------------

## Spec workbook with a CODELISTS sheet, built in-test (openxlsx2 is an
## Import). Neutral study content only. Convention 1 headers:
## "Codelist Name" / "Term (Code)" / "Decoded Value" / "Used By Variables",
## with the codelist name left blank on continuation rows (merged-cell style).
.cl_spec_xlsx <- function(td) {
  vars <- data.frame(
    Dataset         = c("ADSL", "ADSL", "ADSL"),
    `Variable Name` = c("USUBJID", "DCSREASN", "COHORTN"),
    Label           = c("Unique Subject Identifier",
                        "Reason for Discontinuation (N)",
                        "Cohort (N)"),
    `Data Type`     = c("text", "integer", "integer"),
    Codelist        = c("", "DCSREAS", ""),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  cls <- data.frame(
    `Codelist Name`      = c("DCSREAS", "", "", "COHORT", "", ""),
    `Term (Code)`        = c("1", "2", "3", "1", "2", "99"),
    `Decoded Value`      = c("DEATH", "LOST TO FOLLOW-UP", "OTHER",
                             "Cohort A", "Cohort B", "Missing"),
    `Used By Variables`  = c("ADSL.DCSREASN", "", "",
                             "ADSL.COHORTN", "", ""),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  path <- file.path(td, "spec_codelists.xlsx")
  wb <- openxlsx2::wb_workbook() |>
    openxlsx2::wb_add_worksheet("Variables") |>
    openxlsx2::wb_add_data(sheet = "Variables", x = vars) |>
    openxlsx2::wb_add_worksheet("CODELISTS") |>
    openxlsx2::wb_add_data(sheet = "CODELISTS", x = cls)
  openxlsx2::wb_save(wb, file = path, overwrite = TRUE)
  path
}

test_that("a CODELISTS sheet parses into keyed term tables", {
  td   <- withr::local_tempdir()
  spec <- parse_adam_spec(.cl_spec_xlsx(td))

  expect_setequal(names(spec$codelists), c("DCSREAS", "COHORT"))
  dcs <- spec$codelists$DCSREAS
  expect_equal(dcs$name, "DCSREAS")
  ## Fill-down bound the blank-name continuation rows to DCSREAS.
  expect_equal(dcs$terms$term,   c("1", "2", "3"))
  expect_equal(dcs$terms$decode, c("DEATH", "LOST TO FOLLOW-UP", "OTHER"))
  expect_equal(dcs$used_by, "ADSL.DCSREASN")
  ## The codelist sheet was NOT mistaken for a variables sheet.
  expect_false(any(grepl("^CODELISTS\\.", names(spec$lookup))))
})

test_that("the ID / Term / Order header convention parses too", {
  td  <- withr::local_tempdir()
  cls <- data.frame(
    ID              = c("EOSSTT", "EOSSTT", "EOSSTT"),
    Name            = c("End of Study Status", "", ""),
    `Data Type`     = c("integer", "", ""),
    Order           = c("3", "1", "2"),      ## deliberately out of row order
    Term            = c("3", "1", "2"),
    `Decoded Value` = c("ONGOING", "COMPLETED", "DISCONTINUED"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  vars <- data.frame(
    Dataset         = "ADSL",
    `Variable Name` = "EOSSTTN",
    `Data Type`     = "integer",
    Codelist        = "EOSSTT",
    check.names = FALSE, stringsAsFactors = FALSE
  )
  path <- file.path(td, "spec_ct.xlsx")
  wb <- openxlsx2::wb_workbook() |>
    openxlsx2::wb_add_worksheet("Variables") |>
    openxlsx2::wb_add_data(sheet = "Variables", x = vars) |>
    openxlsx2::wb_add_worksheet("CT") |>
    openxlsx2::wb_add_data(sheet = "CT", x = cls)
  openxlsx2::wb_save(wb, file = path, overwrite = TRUE)

  spec <- parse_adam_spec(path)
  eos  <- spec$codelists$EOSSTT
  expect_false(is.null(eos))
  ## Terms come back in Order-column order, not row order.
  expect_equal(eos$terms$decode, c("COMPLETED", "DISCONTINUED", "ONGOING"))
  expect_equal(eos$terms$term,   c("1", "2", "3"))
})

test_that(".codelist_for resolves via the variable's Codelist field then used_by", {
  td   <- withr::local_tempdir()
  spec <- parse_adam_spec(.cl_spec_xlsx(td))

  ## Via the variable sheet's Codelist column.
  hit <- arsbridge:::.codelist_for(spec$codelists, "ADSL", "DCSREASN",
                                   spec$lookup[["ADSL.DCSREASN"]])
  expect_equal(hit$name, "DCSREAS")

  ## COHORTN's Codelist cell is blank -- resolved via Used By Variables.
  hit2 <- arsbridge:::.codelist_for(spec$codelists, "ADSL", "COHORTN",
                                    spec$lookup[["ADSL.COHORTN"]])
  expect_equal(hit2$name, "COHORT")

  ## No codelist anywhere -> NULL.
  expect_null(arsbridge:::.codelist_for(spec$codelists, "ADSL", "USUBJID",
                                        spec$lookup[["ADSL.USUBJID"]]))
  expect_null(arsbridge:::.codelist_for(list(), "ADSL", "DCSREASN", NULL))
})

test_that("define.xml CodeList nodes populate codelists and the variable link", {
  xml <- '<?xml version="1.0" encoding="UTF-8"?>
<ODM xmlns="http://www.cdisc.org/ns/odm/v1.3">
  <Study OID="ST"><MetaDataVersion OID="MDV" Name="MDV">
    <ItemGroupDef OID="IG.ADSL" Name="ADSL" Repeating="No">
      <ItemRef ItemOID="IT.ADSL.USUBJID"/>
      <ItemRef ItemOID="IT.ADSL.DCSREASN"/>
    </ItemGroupDef>
    <ItemDef OID="IT.ADSL.USUBJID" Name="USUBJID" DataType="text" Length="40">
      <Description><TranslatedText xml:lang="en">Unique Subject Identifier</TranslatedText></Description>
    </ItemDef>
    <ItemDef OID="IT.ADSL.DCSREASN" Name="DCSREASN" DataType="integer">
      <Description><TranslatedText xml:lang="en">Reason for Discontinuation (N)</TranslatedText></Description>
      <CodeListRef CodeListOID="CL.DCSREAS"/>
    </ItemDef>
    <CodeList OID="CL.DCSREAS" Name="DCSREAS" DataType="integer">
      <CodeListItem CodedValue="1" OrderNumber="1">
        <Decode><TranslatedText xml:lang="en">DEATH</TranslatedText></Decode>
      </CodeListItem>
      <CodeListItem CodedValue="2" OrderNumber="2">
        <Decode><TranslatedText xml:lang="en">LOST TO FOLLOW-UP</TranslatedText></Decode>
      </CodeListItem>
      <EnumeratedItem CodedValue="9"/>
    </CodeList>
  </MetaDataVersion></Study>
</ODM>'
  path <- tempfile(fileext = ".xml")
  writeLines(xml, path)

  spec <- parse_adam_spec(path)
  expect_true("DCSREAS" %in% names(spec$codelists))
  terms <- spec$codelists$DCSREAS$terms
  expect_equal(terms$term,   c("1", "2", "9"))
  ## An EnumeratedItem has no Decode: its coded value doubles as the label.
  expect_equal(terms$decode, c("DEATH", "LOST TO FOLLOW-UP", "9"))
  ## The variable row records the codelist name via its CodeListRef.
  expect_equal(spec$lookup[["ADSL.DCSREASN"]]$codelist, "DCSREAS")
  ## And .codelist_for resolves it end to end.
  hit <- arsbridge:::.codelist_for(spec$codelists, "ADSL", "DCSREASN",
                                   spec$lookup[["ADSL.DCSREASN"]])
  expect_equal(hit$name, "DCSREAS")
})
