## The shell layout's vocabularies are CLOSED, and there are four of them.
##
## THE DEFECT CLASS. One field answered four questions at once: how a row
## expands, which statistic its cell shows, where it came from, and why it
## carries no analysis. Values from all four sat side by side in `kind`, so
## code asking any one of them had to know the whole set -- and a filter that
## changed the method could change what the renderer believed the row's
## STRUCTURE was. Four vocabularies in one field is what made that possible.
##
## THE INVARIANT. `kind` answers exactly one question -- expansion -- from a
## closed set of seven values, plus `NA` meaning "not proved". `stat_form`,
## `provenance` and `status` each answer one of the others, each from their own
## closed set. No value of one may appear in another, and no retired value may
## survive in the field it was retired FROM.
##
## This file is deliberately mechanical. It is the check a later change cannot
## skip: a new shape must be declared in `.LAYOUT_SHAPES`, or these go red.

## Retired FROM `kind`. Most did not disappear -- they moved to the field that
## answers their question. `nested_parent` / `nested_child` are absent on
## purpose: unchanged by the split, and still current shapes.
.LV_RETIRED <- c("categorical", "continuous", "row", "level", "label",
                 "manual", "supplement_added", "subject_count",
                 "subject_count_pct", "filtered_count", "filtered_count_pct")

## Every committed file that carries layout metadata. Static, because a
## committed fixture is emitted metadata too -- and it is the form that
## survives a save/load cycle, which is exactly where a retired value would
## hide without any code running.
.lv_files <- function() {
  c(test_path("fixtures", "ars_apx_drm_301_deterministic.json"),
    test_path("goldens", "apx_acceptance.json"),
    test_path("goldens", "alz_xlsx.json"),
    test_path("goldens", "alz_docx.json"))
}

## Every shell_layout entry in a parsed ARS, wherever it sits.
.lv_entries <- function(x) {
  out <- list()
  walk <- function(node) {
    if (is.list(node)) {
      sl <- node[["shell_layout"]]
      if (!is.null(sl)) out <<- c(out, sl)
      for (child in node) walk(child)
    }
  }
  walk(x)
  out
}

.lv_field <- function(entries, name) {
  vapply(entries, function(e) {
    v <- e[[name]]
    if (is.null(v) || length(v) == 0 || is.na(v[[1]])) NA_character_
    else as.character(v[[1]])
  }, character(1))
}

.lv_all_entries <- function() {
  files <- Filter(file.exists, .lv_files())
  unlist(lapply(files, function(f) {
    .lv_entries(jsonlite::fromJSON(f, simplifyVector = FALSE))
  }), recursive = FALSE)
}

test_that("A1: the four vocabularies are disjoint and closed", {
  ## The property that makes them four vocabularies rather than one: no value
  ## is legal in two fields, so reading the wrong field cannot accidentally
  ## succeed.
  sets <- list(shape = .LAYOUT_SHAPES,
               stat_form = .LAYOUT_STAT_FORMS,
               provenance = .LAYOUT_PROV_SUPPLEMENT,
               status = .LAYOUT_STATUS_MANUAL)
  pairs <- 0L
  for (i in seq_along(sets)) {
    for (j in seq_along(sets)) {
      if (i >= j) next
      pairs <- pairs + 1L
      expect_length(intersect(sets[[i]], sets[[j]]), 0L)
    }
  }
  ## SCOPE: every pair was actually compared, not just the first.
  expect_equal(pairs, 6L)

  ## And the shape set is exactly the audited seven -- a new shape has to be
  ## declared here; it cannot arrive by accident.
  expect_setequal(
    .LAYOUT_SHAPES,
    c("categorical_block", "stat_block", "scalar_row", "nested_parent",
      "nested_child", "level_row", "label_row"))
})

test_that("A2: no retired value survives in the shape field", {
  entries <- .lv_all_entries()

  ## SCOPE. A walker that finds nothing passes every assertion below without
  ## checking anything, so the count is asserted before the verdict.
  expect_gt(length(entries), 100L)

  ## Scoped to `kind` deliberately. Most of these names did not disappear --
  ## they MOVED, and `manual`, `supplement_added` and the four count forms are
  ## legal values of `status`, `provenance` and `stat_form` respectively. So
  ## "retired" is a statement about this field only; whether each destination
  ## holds a legal value is A3's question, asked against that field's own
  ## vocabulary.
  kinds <- .lv_field(entries, "kind")
  offenders <- unique(kinds[!is.na(kinds) & kinds %in% .LV_RETIRED])
  expect_equal(offenders, character(0))
})

test_that("A3: every field holds only values from its own vocabulary", {
  entries <- .lv_all_entries()
  expect_gt(length(entries), 100L)

  kinds <- .lv_field(entries, "kind")
  expect_true(all(is.na(kinds) | kinds %in% .LAYOUT_SHAPES))

  forms <- .lv_field(entries, "stat_form")
  expect_true(all(is.na(forms) | forms %in% .LAYOUT_STAT_FORMS))

  provs <- .lv_field(entries, "provenance")
  expect_true(all(is.na(provs) | provs %in% .LAYOUT_PROV_SUPPLEMENT))

  stats <- .lv_field(entries, "status")
  expect_true(all(is.na(stats) | stats %in% .LAYOUT_STATUS_MANUAL))

  ## SCOPE, the other direction: the corpus must exercise more than one shape,
  ## or "every value is legal" is trivially true of a single-shape corpus.
  expect_gt(length(unique(kinds[!is.na(kinds)])), 2L)
})

test_that("A4: a freshly built layout uses the closed vocabulary too", {
  ## The static files prove what was committed. This proves what the BUILDER
  ## emits right now, which is the half a stale fixture cannot cover.
  ds <- "ADQX"
  row <- function(label, ann) {
    list(label = label, annotation = ann, has_annot = nzchar(ann),
         detection_method = "pattern", detection_confidence = "high",
         raw_text = label)
  }
  sec <- list(
    tlf_number = "T-1", tlf_type = "TABLE", title = "Synthetic",
    population_text = "Analysis Population",
    population_annot = sprintf("%s.QXFL='Y'", ds),
    source_datasets = ds, col_headers = c("", "A", "B"), n_data_cols = 2L,
    stub_rows = list(
      row("A section note", ""),
      row("Measured", sprintf("%s.QXVAL", ds)),
      row("Subjects with a response", sprintf("%s.QXFL='Y'", ds))
    ),
    analysis_type = "CONTINUOUS",
    ars_method_name = "Summary Statistics - Continuous",
    by_variable = "TRT01A", enriched_rows = list()
  )
  re <- build_ars_json(list(sec))
  entries <- .lv_entries(re)

  expect_gt(length(entries), 0L)
  kinds <- .lv_field(entries, "kind")
  expect_true(all(is.na(kinds) | kinds %in% .LAYOUT_SHAPES))
  expect_equal(unique(kinds[!is.na(kinds) & kinds %in% .LV_RETIRED]),
               character(0))
  forms <- .lv_field(entries, "stat_form")
  expect_true(all(is.na(forms) | forms %in% .LAYOUT_STAT_FORMS))
})

# ---- B: the two RENDERING compatibility rules -------------------------------
##
## Neither is structural evidence, and that is exactly why each needs its own
## test. A reserved row has no proved shape and an orphaned nested child's
## canonical shape is still `nested_child` -- so nothing about how they DRAW
## follows from the shape field, and a reader tightening `.layout_owns_block()`
## to "shapes only" would silently detach their sub-rows.

test_that("B1: a reserved row still owns its block when rendered", {
  model <- ars_to_model(test_path("fixtures",
                                  "ars_apx_drm_301_deterministic.json"))
  raw <- model$outputs$raw[[1]]
  aid <- model$analyses$id[[1]]
  raw[["_meta"]][["shell_layout"]] <- list(
    ## No shape -- the row asks for a statistic this version cannot produce.
    list(order = 1L, label = "Derived endpoint", indent = 0L,
         analysis_id = aid, kind = NA_character_, status = "manual"),
    ## Its stat line, which must belong to it rather than to nothing.
    list(order = 2L, label = "Mean (SD)", indent = 4L,
         analysis_id = NULL, kind = "label_row")
  )
  d <- .shell_table_data(raw, model)

  expect_equal(nrow(d$rows), 2L)
  ## The compatibility rule in one assertion: the label row joins the reserved
  ## row's block, and carries the reserved marker rather than a blank.
  expect_equal(d$rows$owner_analysis_id[[2]], aid)
  expect_equal(d$rows$placeholder[[2]], .MANUAL_MARKER)

  ## CONTROL. The same two rows with a shape that owns nothing: the label row
  ## belongs to nobody. So B1 is about the reserved STATUS, not about label
  ## rows joining whatever sits above them.
  raw[["_meta"]][["shell_layout"]][[1]]$status <- NULL
  raw[["_meta"]][["shell_layout"]][[1]]$kind <- "scalar_row"
  d2 <- .shell_table_data(raw, model)
  expect_true(is.na(d2$rows$owner_analysis_id[[2]]))
})

test_that("B2: an orphaned nested child still expands when rendered", {
  ## Its parent row is gone, so nothing above it opens a block. The canonical
  ## shape stays `nested_child`; the RENDERER draws it as a flat categorical
  ## expansion so its observed levels still reach the page.
  layout <- data.frame(
    order = 1L,
    label = "System organ class",
    indent = 0L,
    analysis_id = "AN_ORPHAN",
    kind = "nested_child",
    stat_form = NA_character_, status = NA_character_,
    provenance = NA_character_,
    stringsAsFactors = FALSE)
  ard <- data.frame(
    output_id      = "OUT",
    analysis_id    = rep("AN_ORPHAN", 4),
    method_id      = rep("MTH_AE_FREQUENCY_COUNT", 4),
    variable       = rep("AESOC", 4),
    variable_level = c("Cardiac disorders", "Cardiac disorders",
                       "Vascular disorders", "Vascular disorders"),
    group1_level   = c("Placebo", "Active", "Placebo", "Active"),
    stat_name      = "n",
    stat           = c(3, 4, 2, 6),
    stringsAsFactors = FALSE)

  prep <- .tfrmt_prep_ard_layout(
    ard, "OUT", layout, col_var = "group1_level", keep_params = "n",
    col_levels = c("Placebo", "Active"), fixed_vars = "TRT01A",
    params_map = list(MTH_AE_FREQUENCY_COUNT = "n"))

  lbls <- prep[[".arsbridge_shell_lbl"]]
  ## Both observed levels reached the page. Without the fallback the row draws
  ## as one line and they do not.
  expect_true(all(c("Cardiac disorders", "Vascular disorders") %in% lbls))

  ## And the canonical shape was NOT rewritten by the renderer: the layout
  ## handed in still says what the shell authored.
  expect_equal(layout$kind, "nested_child")
})
