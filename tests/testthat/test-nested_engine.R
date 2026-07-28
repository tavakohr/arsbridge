## Phase N2 of the nested SOC/PT handoff: the ARD engine's handling of a
## generated nested event, verified against hand computations -- distinct
## subjects per by-cell, ADSL-based per-arm denominators, and equivalence of
## the emitted and legacy execution paths.

.nested_engine_section <- function() {
  token_row <- function(label, annotation) {
    list(label = label, annotation = annotation, has_annot = TRUE,
         detection_method = "pattern", detection_confidence = "high")
  }
  list(
    tlf_number       = "T-14-3-2",
    tlf_type         = "TABLE",
    title            = "TEAE by System Organ Class and Preferred Term",
    population_text  = "Safety Population",
    population_annot = "ADSL.SAFFL='Y'",
    footnotes        = list(),
    source_datasets  = c("ADAE"),
    col_headers      = c("System Organ Class / Preferred Term",
                         "Drug A", "Placebo"),
    n_data_cols      = 2L,
    stub_rows        = list(
      token_row("<System Organ Class>", "ADAE.AESOC"),
      token_row("<Preferred Term>", "ADAE.AEDECOD"),
      token_row("<Preferred Term>", "ADAE.AEDECOD")
    ),
    analysis_type    = "CATEGORICAL",
    ars_method_name  = "Count and Percentage",
    by_variable      = "TRT01A",
    enriched_rows    = list()
  )
}

## Synthetic study built to expose the two counting hazards:
##   * S1 has DUPLICATE Headache records -- must count once in that cell;
##   * "Fatigue" appears under BOTH SOCs (dirty coding), with S1 in both --
##     S1 must count once in EACH (SOC, Fatigue) cell, not be swallowed by
##     the first cell's copy;
##   * S4 is SAFFL='N' -- the arm A denominator must exclude it.
.nested_engine_study <- function(dir) {
  adsl <- data.frame(
    USUBJID = c("S1", "S2", "S3", "S4", "S5", "S6", "S7"),
    TRT01A  = c("Drug A", "Drug A", "Drug A", "Drug A",
                "Placebo", "Placebo", "Placebo"),
    SAFFL   = c("Y", "Y", "Y", "N", "Y", "Y", "Y"),
    stringsAsFactors = FALSE
  )
  adae <- data.frame(
    USUBJID = c("S1", "S1", "S2", "S1", "S1", "S2", "S5"),
    AESOC   = c("Nervous system disorders", "Nervous system disorders",
                "Nervous system disorders", "Nervous system disorders",
                "Gastrointestinal disorders", "Gastrointestinal disorders",
                "Gastrointestinal disorders"),
    AEDECOD = c("Headache", "Headache", "Headache",
                "Fatigue", "Fatigue", "Fatigue", "Nausea"),
    TRTEMFL = "Y",
    stringsAsFactors = FALSE
  )
  adae$TRT01A <- adsl$TRT01A[match(adae$USUBJID, adsl$USUBJID)]
  utils::write.csv(adsl, file.path(dir, "ADSL.csv"), row.names = FALSE)
  utils::write.csv(adae, file.path(dir, "ADAE.csv"), row.names = FALSE)
  invisible(NULL)
}

## One flat stat value from the ARD for an exact (analysis, groups, level,
## stat) key, so every expectation reads as the table cell it checks.
.nested_cell <- function(ard, analysis_id, stat_name, level = NULL,
                         g1 = NULL, g2 = NULL) {
  flat <- function(col) {
    vapply(ard[[col]], function(x) {
      if (length(x) == 0 || is.null(x[[1]])) NA_character_
      else as.character(x[[1]])
    }, character(1))
  }
  keep <- flat("analysis_id") == analysis_id & flat("stat_name") == stat_name
  if (!is.null(level)) keep <- keep & flat("variable_level") == level
  if (!is.null(g1)) keep <- keep & flat("group1_level") == g1
  if (!is.null(g2)) keep <- keep & flat("group2_level") == g2
  keep[is.na(keep)] <- FALSE
  values <- ard$stat[keep]
  if (sum(keep) != 1) return(NA_real_)
  as.numeric(values[[1]][[1]])
}

.nested_engine_paths <- function() {
  dir <- tempfile("nested_engine_")
  dir.create(dir)
  .nested_engine_study(dir)
  re <- build_ars_json(list(.nested_engine_section()), study_id = "S-N2")
  ars_path <- file.path(dir, "reporting_event.json")
  jsonlite::write_json(re, ars_path, auto_unbox = TRUE, null = "null")
  list(dir = dir, ars_path = ars_path)
}


test_that("the child ARD counts distinct subjects per (arm, SOC, PT)", {
  paths <- .nested_engine_paths()
  on.exit(unlink(paths$dir, recursive = TRUE))

  ard <- suppressWarnings(ars_to_ard(paths$ars_path, paths$dir))
  pt_id <- "AN_T_14_3_2_002"

  ## S1's duplicate Headache records count once: n = 2 (S1, S2).
  expect_equal(
    .nested_cell(ard, pt_id, "n", level = "Headache",
                 g1 = "Drug A", g2 = "Nervous system disorders"),
    2
  )

  ## The dirty same-PT-under-two-SOCs case: S1 appears in BOTH Fatigue
  ## cells, so each cell counts its own subjects. A subject-and-term-only
  ## distinct would have dropped S1 from one of them.
  expect_equal(
    .nested_cell(ard, pt_id, "n", level = "Fatigue",
                 g1 = "Drug A", g2 = "Nervous system disorders"),
    1
  )
  expect_equal(
    .nested_cell(ard, pt_id, "n", level = "Fatigue",
                 g1 = "Drug A", g2 = "Gastrointestinal disorders"),
    2
  )
})

test_that("denominators are subjects per arm from the ADSL population", {
  paths <- .nested_engine_paths()
  on.exit(unlink(paths$dir, recursive = TRUE))

  ard <- suppressWarnings(ars_to_ard(paths$ars_path, paths$dir))
  pt_id <- "AN_T_14_3_2_002"

  ## Arm A has 4 subjects but S4 is SAFFL='N': N = 3, p = 2/3.
  expect_equal(
    .nested_cell(ard, pt_id, "N", level = "Headache",
                 g1 = "Drug A", g2 = "Nervous system disorders"),
    3
  )
  expect_equal(
    .nested_cell(ard, pt_id, "p", level = "Headache",
                 g1 = "Drug A", g2 = "Nervous system disorders"),
    2 / 3
  )
  expect_equal(
    .nested_cell(ard, pt_id, "N", level = "Nausea",
                 g1 = "Placebo", g2 = "Gastrointestinal disorders"),
    3
  )
})

test_that("the parent SOC analysis counts each subject once per SOC", {
  paths <- .nested_engine_paths()
  on.exit(unlink(paths$dir, recursive = TRUE))

  ard <- suppressWarnings(ars_to_ard(paths$ars_path, paths$dir))
  soc_id <- "AN_T_14_3_2_001"

  ## S1 (three NSD records) and S2 -> 2; S1 and S2 under GI -> 2.
  expect_equal(
    .nested_cell(ard, soc_id, "n", level = "Nervous system disorders",
                 g1 = "Drug A"),
    2
  )
  expect_equal(
    .nested_cell(ard, soc_id, "n", level = "Gastrointestinal disorders",
                 g1 = "Drug A"),
    2
  )
})

test_that("the legacy execution path counts the same cells", {
  paths <- .nested_engine_paths()
  on.exit(unlink(paths$dir, recursive = TRUE))

  emitted <- suppressWarnings(ars_to_ard(paths$ars_path, paths$dir))
  legacy  <- suppressWarnings(
    ars_to_ard(paths$ars_path, paths$dir, legacy = TRUE)
  )
  pt_id <- "AN_T_14_3_2_002"

  for (ard in list(emitted, legacy)) {
    expect_equal(
      .nested_cell(ard, pt_id, "n", level = "Fatigue",
                   g1 = "Drug A", g2 = "Gastrointestinal disorders"),
      2
    )
    expect_equal(
      .nested_cell(ard, pt_id, "N", level = "Headache",
                   g1 = "Drug A", g2 = "Nervous system disorders"),
      3
    )
  }
})

test_that("the emitted code's distinct includes the by variables", {
  paths <- .nested_engine_paths()
  on.exit(unlink(paths$dir, recursive = TRUE))

  scripts <- write_tlf_code(paths$ars_path,
                            file.path(paths$dir, "code"),
                            adam_dir = paths$dir)
  code <- paste(readLines(scripts[[1]]), collapse = "\n")
  expect_match(code, "dplyr::distinct(USUBJID, TRT01A, AESOC, AEDECOD",
               fixed = TRUE)
})
