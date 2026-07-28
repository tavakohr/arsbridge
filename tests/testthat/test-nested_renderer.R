## Phase N3 of the nested SOC/PT handoff: the layout renderer interleaves a
## nested block -- each parent level's own line, then the child terms
## observed under it -- instead of stacking two flat blocks.

.nr_section <- function(parent_ann = "ADAE.AESOC") {
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
      token_row("<System Organ Class>", parent_ann),
      token_row("<Preferred Term>", "ADAE.AEDECOD"),
      token_row("<Preferred Term>", "ADAE.AEDECOD")
    ),
    analysis_type    = "CATEGORICAL",
    ars_method_name  = "Count and Percentage",
    by_variable      = "TRT01A",
    enriched_rows    = list()
  )
}

## Same synthetic study as test-nested_engine.R: S1 duplicates Headache,
## "Fatigue" is coded under both SOCs, S4 is out of the population.
.nr_study <- function(dir) {
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

## A study where frequency order disagrees with alphabetical order at BOTH
## levels: "Nervous system disorders" (3 subjects) outranks
## "Gastrointestinal disorders" (1), and under NSD "Headache" (2) outranks
## "Dizziness" (1). Placebo-only basis inverts the PT order again
## (Dizziness: 1 Placebo subject, Headache: 0).
.nr_study_skewed <- function(dir) {
  adsl <- data.frame(
    USUBJID = c("S1", "S2", "S3", "S5", "S6", "S7"),
    TRT01A  = c("Drug A", "Drug A", "Drug A", "Placebo", "Placebo", "Placebo"),
    SAFFL   = "Y",
    stringsAsFactors = FALSE
  )
  adae <- data.frame(
    USUBJID = c("S1", "S2", "S5", "S1"),
    AESOC   = c("Nervous system disorders", "Nervous system disorders",
                "Nervous system disorders", "Gastrointestinal disorders"),
    AEDECOD = c("Headache", "Headache", "Dizziness", "Nausea"),
    TRTEMFL = "Y",
    stringsAsFactors = FALSE
  )
  adae$TRT01A <- adsl$TRT01A[match(adae$USUBJID, adsl$USUBJID)]
  utils::write.csv(adsl, file.path(dir, "ADSL.csv"), row.names = FALSE)
  utils::write.csv(adae, file.path(dir, "ADAE.csv"), row.names = FALSE)
  invisible(NULL)
}

## The prepared display frame, exactly as ars_render_tlf() builds it.
.nr_prepped <- function(parent_ann = "ADAE.AESOC", study = .nr_study) {
  dir <- tempfile("nested_renderer_")
  dir.create(dir)
  study(dir)
  re <- build_ars_json(list(.nr_section(parent_ann)), study_id = "S-N3")
  ars_path <- file.path(dir, "reporting_event.json")
  jsonlite::write_json(re, ars_path, auto_unbox = TRUE, null = "null")
  ard <- suppressWarnings(ars_to_ard(ars_path, dir))

  tf <- ars_to_tfrmt(ars_path, ard, "T_14_3_2")
  prepped <- .tfrmt_prep_ard_layout(
    ard, "T_14_3_2",
    attr(tf, "arsbridge_layout"),
    attr(tf, "arsbridge_col_var"),
    attr(tf, "arsbridge_keep_params"),
    col_levels = attr(tf, "arsbridge_col_levels"),
    fixed_vars = attr(tf, "arsbridge_fixed_vars"),
    params_map = attr(tf, "arsbridge_params_map") %||% list()
  )
  list(dir = dir, ars_path = ars_path, ard = ard, prepped = prepped)
}


test_that(".shell_layout carries parent_order through", {
  re <- build_ars_json(list(.nr_section()), study_id = "S-N3")
  layout <- .shell_layout(re$outputs[[1]])
  expect_true("parent_order" %in% names(layout))
  expect_equal(layout$parent_order,
               c(NA_integer_, layout$order[layout$kind == "nested_parent"]))
})

test_that("the rendered rows interleave each SOC with its own PTs", {
  skip_if_not_installed("tfrmt")

  run <- .nr_prepped()
  on.exit(unlink(run$dir, recursive = TRUE))
  prepped <- run$prepped

  ## One label per display line, in shell order. Default order is
  ## descending frequency: GI (3 subjects) before NSD (2); within NSD,
  ## Headache (2) before Fatigue (1).
  ords <- sort(unique(prepped[[.ARS_SHELL_ORD]]))
  lines <- vapply(ords, function(o) {
    unique(prepped[[.ARS_SHELL_LBL]][prepped[[.ARS_SHELL_ORD]] == o])[1]
  }, character(1))

  expect_equal(lines, c(
    "Gastrointestinal disorders", "Fatigue", "Nausea",
    "Nervous system disorders", "Headache", "Fatigue"
  ))

  ## The authored tokens themselves never print.
  expect_false(any(grepl("^<", prepped[[.ARS_SHELL_LBL]])))

  ## Each PT row belongs to its SOC's block (the group column).
  grp_of <- function(lbl, ordv) {
    unique(prepped[[.ARS_SHELL_GRP]][
      prepped[[.ARS_SHELL_LBL]] == lbl & prepped[[.ARS_SHELL_ORD]] == ordv
    ])
  }
  expect_equal(grp_of("Nausea", ords[3]), "Gastrointestinal disorders")
  expect_equal(grp_of("Headache", ords[5]), "Nervous system disorders")
})

test_that("default order is descending frequency, parent then child", {
  skip_if_not_installed("tfrmt")

  run <- .nr_prepped(study = .nr_study_skewed)
  on.exit(unlink(run$dir, recursive = TRUE))
  prepped <- run$prepped

  ords <- sort(unique(prepped[[.ARS_SHELL_ORD]]))
  lines <- vapply(ords, function(o) {
    unique(prepped[[.ARS_SHELL_LBL]][prepped[[.ARS_SHELL_ORD]] == o])[1]
  }, character(1))

  ## NSD (3 subjects) outranks GI (1) even though GI sorts first A-Z;
  ## under NSD, Headache (2) outranks Dizziness (1).
  expect_equal(lines, c(
    "Nervous system disorders", "Headache", "Dizziness",
    "Gastrointestinal disorders", "Nausea"
  ))
})

test_that("sort: alphabetical restores A-Z at both levels", {
  skip_if_not_installed("tfrmt")

  run <- .nr_prepped(parent_ann = "ADAE.AESOC; sort: alphabetical",
                     study = .nr_study_skewed)
  on.exit(unlink(run$dir, recursive = TRUE))
  prepped <- run$prepped

  ords <- sort(unique(prepped[[.ARS_SHELL_ORD]]))
  lines <- vapply(ords, function(o) {
    unique(prepped[[.ARS_SHELL_LBL]][prepped[[.ARS_SHELL_ORD]] == o])[1]
  }, character(1))

  expect_equal(lines, c(
    "Gastrointestinal disorders", "Nausea",
    "Nervous system disorders", "Dizziness", "Headache"
  ))
})

test_that("sort: desc-freq('<column>') counts only that column", {
  skip_if_not_installed("tfrmt")

  run <- .nr_prepped(parent_ann = "ADAE.AESOC; sort: desc-freq('Placebo')",
                     study = .nr_study_skewed)
  on.exit(unlink(run$dir, recursive = TRUE))
  prepped <- run$prepped

  ords <- sort(unique(prepped[[.ARS_SHELL_ORD]]))
  lines <- vapply(ords, function(o) {
    unique(prepped[[.ARS_SHELL_LBL]][prepped[[.ARS_SHELL_ORD]] == o])[1]
  }, character(1))

  ## In the Placebo column NSD (1: Dizziness/S5) outranks GI (0); under
  ## NSD, Dizziness (1) outranks Headache (0). GI has no Placebo events at
  ## all, so its block falls back to the all-column count -- Nausea still
  ## renders under it.
  expect_equal(lines, c(
    "Nervous system disorders", "Dizziness", "Headache",
    "Gastrointestinal disorders", "Nausea"
  ))
})

test_that("the empty cartesian combinations never render", {
  skip_if_not_installed("tfrmt")

  run <- .nr_prepped()
  on.exit(unlink(run$dir, recursive = TRUE))
  prepped <- run$prepped

  ## Nausea occurred only under GI; Headache only under NSD. {cards}
  ## emitted both under both SOCs (n = 0) -- the renderer must not.
  expect_equal(sum(
    prepped[[.ARS_SHELL_GRP]] == "Nervous system disorders" &
      prepped[[.ARS_SHELL_LBL]] == "Nausea"
  ), 0)
  expect_equal(sum(
    prepped[[.ARS_SHELL_GRP]] == "Gastrointestinal disorders" &
      prepped[[.ARS_SHELL_LBL]] == "Headache"
  ), 0)

  ## The dirty same-PT-under-two-SOCs case DOES render in both blocks.
  expect_equal(sum(
    prepped[[.ARS_SHELL_GRP]] == "Nervous system disorders" &
      prepped[[.ARS_SHELL_LBL]] == "Fatigue" &
      prepped$stat_name == "n"
  ) > 0, TRUE)
  expect_equal(sum(
    prepped[[.ARS_SHELL_GRP]] == "Gastrointestinal disorders" &
      prepped[[.ARS_SHELL_LBL]] == "Fatigue" &
      prepped$stat_name == "n"
  ) > 0, TRUE)
})

test_that("the interleaved cells carry the verified N2 statistics", {
  skip_if_not_installed("tfrmt")

  run <- .nr_prepped()
  on.exit(unlink(run$dir, recursive = TRUE))
  prepped <- run$prepped

  cell <- function(grp, lbl, stat_name, colv_pattern) {
    keep <- prepped[[.ARS_SHELL_GRP]] == grp &
      prepped[[.ARS_SHELL_LBL]] == lbl &
      prepped$stat_name == stat_name &
      grepl(colv_pattern, prepped[[names(prepped)[3]]])
    vals <- prepped$stat[keep]
    if (length(vals) != 1) NA_real_ else vals
  }

  ## Headache under NSD, Drug A: n = 2 distinct subjects (S1 deduped).
  expect_equal(cell("Nervous system disorders", "Headache", "n", "Drug A"), 2)
  ## Fatigue under GI, Drug A: n = 2 (S1 counted in this cell too).
  expect_equal(cell("Gastrointestinal disorders", "Fatigue", "n", "Drug A"), 2)
})

test_that("the nested table renders to gt end to end", {
  skip_if_not_installed("tfrmt")
  skip_if_not_installed("gt")

  run <- .nr_prepped()
  on.exit(unlink(run$dir, recursive = TRUE))

  gt_tbl <- suppressMessages(suppressWarnings(
    ars_render_tlf(run$ars_path, run$ard, "T_14_3_2")
  ))
  expect_s3_class(gt_tbl, "gt_tbl")
})

test_that("an orphaned nested child falls back to a flat block", {
  skip_if_not_installed("tfrmt")

  run <- .nr_prepped()
  on.exit(unlink(run$dir, recursive = TRUE))

  spec <- .read_json(run$ars_path)
  layout <- .shell_layout(spec$outputs[[1]])
  ## Sever the link: drop the parent row entirely.
  orphaned <- layout[layout$kind != "nested_parent", , drop = FALSE]

  tf <- ars_to_tfrmt(run$ars_path, run$ard, "T_14_3_2")
  prepped <- .tfrmt_prep_ard_layout(
    run$ard, "T_14_3_2", orphaned,
    attr(tf, "arsbridge_col_var"),
    attr(tf, "arsbridge_keep_params"),
    col_levels = attr(tf, "arsbridge_col_levels"),
    fixed_vars = attr(tf, "arsbridge_fixed_vars"),
    params_map = attr(tf, "arsbridge_params_map") %||% list()
  )
  ## Still renders rows (flat categorical fallback), never errors.
  expect_gt(nrow(prepped), 0)
})
