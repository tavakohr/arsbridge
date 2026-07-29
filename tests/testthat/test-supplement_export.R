## write_supplement_draft() and the reviewed-manifest workflow --------------
## The draft exporter serializes a parse into the v4 supplement format so a
## human can review/correct it and feed it back with
## supplement_trust = "prefer_supplement". Suppression entries let the
## reviewed file REMOVE a wrongly-parsed row, not just fill or override.

.fx <- function(name) test_path("fixtures", name)

## Local copies of test-supplement.R's fixtures (test files do not share
## top-level objects).
.supp_spec <- list(ADSL.AGE = list(), ADSL.SEX = list(), ADSL.SAFFL = list())

.mk_supp_section <- function() {
  list(
    tlf_number = "T-14-1-1", tlf_type = "TABLE", title = "Demographics",
    population_annot = "",
    stub_rows = list(
      list(label = "Age (years)", annotation = "", has_annot = FALSE,
           detection_method = NA_character_,
           detection_confidence = NA_character_,
           raw_text = "Age (years)"),
      list(label = "Sex", annotation = "ADSL.SEX", has_annot = TRUE,
           detection_method = "colour", detection_confidence = "high",
           raw_text = "Sex (ADSL.SEX)"),
      list(label = "Mean (SD)", annotation = "", has_annot = FALSE,
           detection_method = NA_character_,
           detection_confidence = NA_character_,
           raw_text = "Mean (SD)")
    )
  )
}

test_that("write_supplement_draft round-trips through read_supplement", {
  out <- tempfile(fileext = ".json")
  suppressMessages(write_supplement_draft(
    shell_path     = .fx("CDSC-ALZ-201_TLF_Shells_v1.0_annotated.docx"),
    output_path    = out,
    adam_spec_path = .fx("adam_spec_CDSC-ALZ-201.xlsx")))

  supp <- read_supplement(out)

  ## All 8 sections survive -- including Table 14.3.1 AND Figure 14.3.1,
  ## which share a number and must not overwrite each other.
  expect_length(supp$tlfs, 8L)
  expect_true(all(c("T-14-3-1", "F-14-3-1") %in% names(supp$tlfs)))

  t1 <- supp$tlfs[["T-14-1-1"]]
  expect_equal(t1$title, "Subject Disposition")
  expect_true(t1$is_supported)
  expect_gt(length(t1$analyses), 0L)
  ## Every analysis carries a typed variable, never a string annotation.
  for (a in t1$analyses) {
    expect_true(is.list(a$variable))
    expect_true(nzchar(a$variable$dataset))
    expect_true(nzchar(a$variable$variable))
  }
  ## Anchors describe the parsed roster.
  expect_gt(t1$anchors$rowCount, 0L)
  expect_equal(t1$provenance$blueprintStatus, "draft_from_parse")

  ## The draft passes its own pre-flight with no failures.
  findings <- ars_validate_supplement(out,
                                      adam_spec_path =
                                        .fx("adam_spec_CDSC-ALZ-201.xlsx"))
  if (is.data.frame(findings) && nrow(findings) > 0) {
    expect_equal(sum(findings$severity == "FAIL"), 0L)
  }
  unlink(out)
})

test_that("a population annotation becomes a typed analysisSet", {
  sec <- list(
    tlf_number = "T-14-1-1", tlf_type = "TABLE", title = "Demographics",
    population_text  = "Safety Population",
    population_annot = "ADSL.SAFFL='Y'",
    stub_rows = list(
      list(label = "Age (years)", annotation = "ADSL.AGE", has_annot = TRUE,
           detection_method = "colour", detection_confidence = "high",
           raw_text = "Age (years) ADSL.AGE")
    )
  )
  entry <- .section_to_supplement_tlf(sec)
  expect_equal(entry$analysisSet$label, "Safety Population")
  expect_equal(entry$analysisSet$condition$variable, "SAFFL")
  expect_equal(entry$analysisSet$condition$comparator, "EQ")

  a <- entry$analyses[[1]]
  expect_equal(a$rowLabel, "Age (years)")
  expect_equal(a$variable, list(dataset = "ADSL", variable = "AGE"))
  expect_equal(a$confidence, "HIGH")
  expect_null(a$whereClause)   ## bare variable pointer: no filter
})

test_that("unannotated and out-of-spec rows land in reviewItems", {
  sec <- list(
    tlf_number = "T-14-1-1", tlf_type = "TABLE", title = "Demographics",
    population_annot = "",
    stub_rows = list(
      list(label = "Height (cm)", annotation = "", has_annot = FALSE,
           detection_method = NA_character_,
           detection_confidence = NA_character_, raw_text = "Height (cm)"),
      list(label = "Ghost", annotation = "ADSL.FAKEVAR", has_annot = TRUE,
           detection_method = "colour", detection_confidence = "high",
           raw_text = "Ghost ADSL.FAKEVAR"),
      list(label = "Mean (SD)", annotation = "", has_annot = FALSE,
           detection_method = NA_character_,
           detection_confidence = NA_character_, raw_text = "Mean (SD)")
    )
  )
  spec <- list(ADSL.AGE = list())
  entry <- .section_to_supplement_tlf(sec, spec)
  items <- unlist(entry$provenance$reviewItems)
  expect_true(any(grepl("Height \\(cm\\).*no annotation", items)))
  expect_true(any(grepl("ADSL.FAKEVAR is not in the ADaM spec", items)))
  ## A statistic sub-row with no annotation is normal, not a review item.
  expect_false(any(grepl("Mean \\(SD\\)", items)))
})

test_that("designator keys disambiguate a table/figure number collision", {
  supp <- list(tlfs = list(
    "T-14-3-1" = list(title = "AE Overview", outputType = "TABLE"),
    "F-14-3-1" = list(title = "Pulse Rate Figure", outputType = "FIGURE")
  ))
  expect_equal(.match_supplement_tlf(supp, "T-14-3-1", "TABLE")$title,
               "AE Overview")
  expect_equal(.match_supplement_tlf(supp, "F-14-3-1", "FIGURE")$title,
               "Pulse Rate Figure")
  ## A single bare-number key still matches with no type given (legacy).
  supp2 <- list(tlfs = list("14.3.1" = list(title = "AE Overview")))
  expect_equal(.match_supplement_tlf(supp2, "T-14-3-1")$title, "AE Overview")
})

test_that("suppress removes a row under prefer_supplement", {
  diag_reset()
  supp_tlf <- list(analyses = list(
    list(rowLabel = "Sex", suppress = TRUE)
  ))
  sec <- .apply_supplement_bindings(.mk_supp_section(), supp_tlf, .supp_spec,
                                    trust = "prefer_supplement")
  labels <- vapply(sec$stub_rows, function(r) r$label, character(1))
  expect_false("Sex" %in% labels)
  expect_length(sec$stub_rows, 2L)
  recs <- diag_records()
  expect_true(any(recs$severity == "WARN" &
                    grepl("removed by supplement suppression", recs$problem)))
})

test_that("suppress is ignored (with a WARN) under fill_gaps", {
  diag_reset()
  supp_tlf <- list(analyses = list(
    list(rowLabel = "Sex", suppress = TRUE)
  ))
  sec <- .apply_supplement_bindings(.mk_supp_section(), supp_tlf, .supp_spec,
                                    trust = "fill_gaps")
  labels <- vapply(sec$stub_rows, function(r) r$label, character(1))
  expect_true("Sex" %in% labels)
  recs <- diag_records()
  expect_true(any(recs$severity == "WARN" &
                    grepl("ignored \\(trust = fill_gaps\\)", recs$problem)))
})

test_that("suppress naming no real row warns and removes nothing", {
  diag_reset()
  supp_tlf <- list(analyses = list(
    list(rowLabel = "No Such Row", suppress = TRUE)
  ))
  sec <- .apply_supplement_bindings(.mk_supp_section(), supp_tlf, .supp_spec,
                                    trust = "prefer_supplement")
  expect_length(sec$stub_rows, 3L)
  recs <- diag_records()
  expect_true(any(recs$severity == "WARN" &
                    grepl("matched no stub row", recs$problem)))
})

test_that("a suppression entry passes supplement validation", {
  supp <- list(
    supplement_version = 4L,
    tlfs = list("T-14-1-1" = list(
      title = "Demographics", analysis_type = "CATEGORICAL",
      is_supported = TRUE,
      analyses = list(
        list(rowLabel = "Sex", suppress = TRUE),
        list(rowLabel = "Age (years)",
             variable = list(dataset = "ADSL", variable = "AGE"))
      )
    ))
  )
  path <- tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(supp, auto_unbox = TRUE, pretty = TRUE), path)
  findings <- ars_validate_supplement(path)
  unlink(path)
  if (is.data.frame(findings) && nrow(findings) > 0) {
    expect_equal(sum(findings$severity == "FAIL"), 0L)
  } else {
    succeed()
  }
})

test_that("end-to-end: a reviewed draft corrects the build via prefer_supplement", {
  ## Draft from the minimal 2-TLF shell, suppress one row, then rebuild and
  ## confirm the row is gone from the sections the builder sees.
  out <- tempfile(fileext = ".json")
  suppressMessages(write_supplement_draft(
    shell_path  = .fx("annotated_shell_2tlf_minimal.docx"),
    output_path = out,
    adam_spec_path = .fx("adam_spec_minimal.xlsx")))

  supp <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  key <- names(supp$tlfs)[[1]]
  supp$tlfs[[key]]$analyses <-
    c(supp$tlfs[[key]]$analyses %||% list(),
      list(list(rowLabel = "Race, n (%)", suppress = TRUE)))
  writeLines(jsonlite::toJSON(supp, auto_unbox = TRUE, pretty = TRUE), out)

  sections <- suppressMessages(parse_shell_docx(.fx("annotated_shell_2tlf_minimal.docx")))
  spec <- parse_adam_spec(.fx("adam_spec_minimal.xlsx"))
  reviewed <- read_supplement(out)
  sec <- .apply_supplement_bindings(
    sections[[1]],
    .match_supplement_tlf(reviewed, sections[[1]]$tlf_number,
                          sections[[1]]$tlf_type),
    spec$lookup, trust = "prefer_supplement")
  labels <- vapply(sec$stub_rows, function(r) r$label, character(1))
  expect_false(any(grepl("^Race", labels)))
  unlink(out)
})
