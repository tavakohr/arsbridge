# A display label is not an identity.
#
# Two rows of one table may legitimately carry the same visible text while
# describing different data -- a level label like "Other" or "Not Reported"
# belongs to more than one variable. A lookup keyed on the label alone returns
# whichever row came first, and the second row silently inherits the first's
# variable and filter. The number that results is well formed and wrong.
#
# These tests are built entirely from invented identifiers. The rule is about
# the relationship between a row's own declared source and the enrichment that
# claims it, so it must hold for names this package has never seen. Every unit
# case below runs under two disjoint vocabularies; if any of them depended on a
# familiar name, one of the two runs would fail.

## Two vocabularies with nothing in common, both valid under the ADaM
## identifier grammar (.ADAM_DS / .ADAM_VAR in R/aaa_constants.R).
##
## `shared` is a variable name that exists in BOTH datasets. It is what forces
## matching onto the qualified DATASET.VARIABLE pair: a comparison on the bare
## variable name cannot tell ds1$shared from ds2$shared.
.REI_NAMES_A <- list(ds1 = "ADQX", var1 = "FLAGA",
                     ds2 = "ADZZ", var2 = "FLAGB",
                     shared = "DUPEFL",
                     ds3 = "ADWK", var3 = "OTHERFL",
                     label = "Repeated level", solo = "Unique caption")
.REI_NAMES_B <- list(ds1 = "ADVN", var1 = "PICKFL",
                     ds2 = "ADRP", var2 = "KEEPFL",
                     shared = "SAMEFL",
                     ds3 = "ADTY", var3 = "SPAREFL",
                     label = "Shared caption", solo = "Solitary caption")
.REI_VOCABS <- list(A = .REI_NAMES_A, B = .REI_NAMES_B)

.rei_row <- function(label, dataset, variable, value) {
  list(label = label, has_annot = TRUE,
       annotation = sprintf("%s.%s = '%s'", dataset, variable, value))
}

.rei_enrichment <- function(label, dataset, variable) {
  list(label = label, primary_dataset = dataset, primary_variable = variable,
       data_subset = NULL, variable_role = "ANALYSIS")
}

#' One table whose two annotated rows share a display label and declare
#' different qualified sources. Row order and enrichment order reverse
#' INDEPENDENTLY: if they could only reverse together, three of the four
#' arrangements would never be tried, and positional luck could pass for
#' identity in the untried ones.
.rei_fixture <- function(n, swap_rows = FALSE, swap_ers = FALSE) {
  rows <- list(
    .rei_row(n$label, n$ds1, n$var1, "Y"),
    .rei_row(n$label, n$ds2, n$var2, "N")
  )
  ers <- list(
    .rei_enrichment(n$label, n$ds1, n$var1),
    .rei_enrichment(n$label, n$ds2, n$var2)
  )
  if (swap_rows) rows <- rev(rows)
  if (swap_ers)  ers  <- rev(ers)
  list(rows = rows, enrichments = ers)
}

## ---- the pairing rule -------------------------------------------------------

test_that("rows sharing a label keep their own enrichment", {
  for (n in .REI_VOCABS) {
    fx <- .rei_fixture(n)

    ## Each row must reach the enrichment that resolved ITS declared source,
    ## never whichever one happens to come first.
    for (i in seq_along(fx$rows)) {
      got <- .enrichment_for_row(fx$rows[[i]], fx$enrichments)
      expect_false(is.null(got), info = i)
      expect_equal(got$primary_variable,
                   fx$enrichments[[i]]$primary_variable, info = i)
      expect_equal(got$primary_dataset,
                   fx$enrichments[[i]]$primary_dataset, info = i)
    }

    ## And the same pairing read from the other end.
    for (i in seq_along(fx$enrichments)) {
      got <- .row_for_enrichment(fx$enrichments[[i]], fx$rows)
      expect_false(is.null(got), info = i)
      expect_equal(got$annotation, fx$rows[[i]]$annotation, info = i)
    }
  }
})

test_that("the mapping survives every ordering of rows and enrichments", {
  ## All four arrangements, each reversed independently. If order mattered the
  ## rule would be positional luck rather than identity.
  for (n in .REI_VOCABS) {
    pair_of <- function(fx, row) {
      er <- .enrichment_for_row(row, fx$enrichments)
      if (is.null(er)) return(NA_character_)
      paste0(er$primary_dataset, ".", er$primary_variable)
    }
    first_row  <- .rei_row(n$label, n$ds1, n$var1, "Y")
    second_row <- .rei_row(n$label, n$ds2, n$var2, "N")

    arrangements <- list(c(FALSE, FALSE), c(TRUE, FALSE),
                         c(FALSE, TRUE),  c(TRUE, TRUE))
    for (a in arrangements) {
      fx <- .rei_fixture(n, swap_rows = a[[1]], swap_ers = a[[2]])
      tag <- sprintf("rows=%s ers=%s", a[[1]], a[[2]])
      expect_equal(pair_of(fx, first_row),  paste0(n$ds1, ".", n$var1), info = tag)
      expect_equal(pair_of(fx, second_row), paste0(n$ds2, ".", n$var2), info = tag)

      ## Read from the other end too, under the same arrangement.
      got <- .row_for_enrichment(.rei_enrichment(n$label, n$ds2, n$var2), fx$rows)
      expect_equal(got$annotation, second_row$annotation, info = tag)
    }
  }
})

test_that("renaming every identifier changes no mapping", {
  ## Metamorphic: same structure, disjoint vocabulary, same outcome. The rule
  ## must depend on the relationship, not on any familiar name.
  flat <- function(n) unlist(n[c("ds1", "var1", "ds2", "var2", "shared",
                                 "ds3", "var3")], use.names = FALSE)
  expect_length(intersect(flat(.REI_NAMES_A), flat(.REI_NAMES_B)), 0L)

  outcome <- function(n) {
    fx <- .rei_fixture(n)
    vapply(fx$rows, function(r) {
      er <- .enrichment_for_row(r, fx$enrichments)
      if (is.null(er)) return("<none>")
      ## Report the ROLE, not the name, so the two runs are comparable.
      if (identical(er$primary_variable, n$var1)) "first" else
        if (identical(er$primary_variable, n$var2)) "second" else "<other>"
    }, character(1))
  }
  expect_equal(outcome(.REI_NAMES_B), outcome(.REI_NAMES_A))
  ## Non-vacuous: both rows really matched, so this is not two "<none>"s
  ## agreeing with each other.
  expect_equal(outcome(.REI_NAMES_A), c("first", "second"))
})

test_that("a shared variable name in two datasets is not a match", {
  ## Identity is the qualified DATASET.VARIABLE pair, never the bare variable.
  ## Both datasets here carry a variable of the SAME name, so a comparison that
  ## dropped the dataset would call the wrong candidate compatible.
  for (n in .REI_VOCABS) {
    row      <- .rei_row(n$label, n$ds1, n$shared, "Y")
    er_right <- .rei_enrichment(n$label, n$ds1, n$shared)
    er_wrong <- .rei_enrichment(n$label, n$ds2, n$shared)

    expect_equal(.enrichment_compatibility(er_right, row$annotation),
                 "compatible")
    expect_equal(.enrichment_compatibility(er_wrong, row$annotation),
                 "contradictory")

    ## Presented together, the one whose dataset agrees wins -- in both orders.
    expect_equal(.enrichment_for_row(row, list(er_wrong, er_right))$primary_dataset,
                 n$ds1)
    expect_equal(.enrichment_for_row(row, list(er_right, er_wrong))$primary_dataset,
                 n$ds1)
    ## Alone, the wrong-dataset candidate is still not a match.
    expect_null(.enrichment_for_row(row, list(er_wrong)))
  }
})

test_that("a unique label with a compatible source matches", {
  ## The ordinary successful case, stated positively: one candidate, and the
  ## row's own annotation agrees with it.
  for (n in .REI_VOCABS) {
    row <- .rei_row(n$solo, n$ds1, n$var1, "Y")
    er  <- .rei_enrichment(n$solo, n$ds1, n$var1)
    expect_equal(.enrichment_compatibility(er, row$annotation), "compatible")

    got <- .enrichment_for_row(row, list(er))
    expect_false(is.null(got))
    expect_equal(got$primary_dataset, n$ds1)
    expect_equal(got$primary_variable, n$var1)
    expect_equal(.row_for_enrichment(er, list(row))$annotation, row$annotation)
  }
})

test_that("a unique label does not override contradictory semantic evidence", {
  ## The sharpest form of the rule. One candidate, so the label is unique --
  ## but the row declares one qualified source and the enrichment resolved a
  ## different one. Unique is not the same as compatible, and taking it would
  ## be the same mistake the duplicate-label case makes, just harder to see.
  for (n in .REI_VOCABS) {
    row <- .rei_row(n$solo, n$ds1, n$var1, "Y")
    er  <- .rei_enrichment(n$solo, n$ds2, n$var2)

    expect_equal(.enrichment_compatibility(er, row$annotation), "contradictory")
    expect_null(.enrichment_for_row(row, list(er)))
    expect_null(.row_for_enrichment(er, list(row)))

    ## And no filter is taken from a row this enrichment does not belong to.
    filled <- .backfill_data_subsets(list(er), list(row))
    expect_null(filled[[1]]$data_subset)
  }
})

test_that("several compatible candidates are unresolved, not the first", {
  ## A shared label where more than one candidate genuinely fits: there is no
  ## evidence to choose between them, so choosing is guessing.
  for (n in .REI_VOCABS) {
    ## The row declares both sources, so both enrichments are compatible.
    row <- list(label = n$label, has_annot = TRUE,
                annotation = sprintf("%s.%s = 'Y' and %s.%s = 'N'",
                                     n$ds1, n$var1, n$ds2, n$var2))
    ers <- list(.rei_enrichment(n$label, n$ds1, n$var1),
                .rei_enrichment(n$label, n$ds2, n$var2))

    verdicts <- vapply(ers, .enrichment_compatibility, character(1),
                       annotation = row$annotation)
    expect_equal(verdicts, c("compatible", "compatible"))
    expect_null(.enrichment_for_row(row, ers))
  }
})

test_that("several candidates with no evidence are unresolved, not the first", {
  ## Distinct from the contradictory case and from the lone-candidate case:
  ## nothing here contradicts anything, there is simply nothing to choose on.
  ## One such candidate is the only available reading; two are a guess, and a
  ## guess is exactly what the defect was.
  for (n in .REI_VOCABS) {
    row <- list(label = n$label, has_annot = FALSE, annotation = "")
    ers <- list(.rei_enrichment(n$label, n$ds1, n$var1),
                .rei_enrichment(n$label, n$ds2, n$var2))

    verdicts <- vapply(ers, .enrichment_compatibility, character(1),
                       annotation = row$annotation)
    expect_equal(verdicts, c("unknown", "unknown"))
    expect_null(.enrichment_for_row(row, ers))

    ## Same from the other end: an enrichment facing two silent rows.
    rows <- list(list(label = n$label, has_annot = FALSE, annotation = ""),
                 list(label = n$label, has_annot = FALSE, annotation = ""))
    expect_null(.row_for_enrichment(ers[[1]], rows))
  }
})

test_that("an undecidable label yields no match rather than the first one", {
  ## Two rows share a label AND neither enrichment resolves a source either row
  ## declared. Returning the first candidate is what produced the defect;
  ## returning nothing lets the caller build each row from its own annotation.
  for (n in .REI_VOCABS) {
    rows <- list(.rei_row(n$label, n$ds1, n$var1, "Y"),
                 .rei_row(n$label, n$ds2, n$var2, "N"))
    ## Both enrichments point at a third source neither row declared.
    ers <- list(.rei_enrichment(n$label, n$ds3, n$var3),
                .rei_enrichment(n$label, n$ds3, n$var3))

    expect_null(.enrichment_for_row(rows[[1]], ers))
    expect_null(.enrichment_for_row(rows[[2]], ers))

    ## Backfill then leaves the subset alone rather than inventing one.
    filled <- .backfill_data_subsets(ers, rows)
    expect_null(filled[[1]]$data_subset)
    expect_null(filled[[2]]$data_subset)
  }
})

test_that("absence of evidence still allows a lone candidate", {
  ## With nothing to compare, the single candidate is the only available
  ## reading, and the supported workflow depends on it -- an unannotated row
  ## still has to pair.
  for (n in .REI_VOCABS) {
    row <- list(label = "No annotation", has_annot = FALSE, annotation = "")
    er  <- .rei_enrichment("No annotation", n$ds1, n$var1)
    expect_equal(.enrichment_compatibility(er, row$annotation), "unknown")
    got <- .enrichment_for_row(row, list(er))
    expect_false(is.null(got))
    expect_equal(got$primary_variable, n$var1)

    ## Same when the enrichment is the silent one.
    row2 <- .rei_row("Bare", n$ds1, n$var1, "Y")
    er2  <- list(label = "Bare", primary_dataset = "", primary_variable = "")
    expect_equal(.enrichment_compatibility(er2, row2$annotation), "unknown")
    expect_false(is.null(.enrichment_for_row(row2, list(er2))))
  }
})

test_that("a subset is backfilled from the row's own annotation", {
  ## The end the defect was reported from: each row's filter must come from
  ## its own annotation, so two rows sharing a label get different subsets.
  for (n in .REI_VOCABS) {
    fx <- .rei_fixture(n)
    filled <- .backfill_data_subsets(fx$enrichments, fx$rows)

    expect_equal(filled[[1]]$data_subset$dataset, n$ds1)
    expect_equal(filled[[1]]$data_subset$variable, n$var1)
    expect_equal(filled[[2]]$data_subset$dataset, n$ds2)
    expect_equal(filled[[2]]$data_subset$variable, n$var2)
    ## Different rows, different filters -- not one inherited twice.
    expect_false(identical(filled[[1]]$data_subset, filled[[2]]$data_subset))
  }
})

## ---- the same rule inside nested-block classification -----------------------

test_that("a nested token block reads each row's own variable, not the first", {
  ## The classifier decides parent/child by comparing the variables of
  ## consecutive token rows, so the variable it reads per row is the value the
  ## whole classification turns on.
  ##
  ## Two token rows sharing a label is ordinary -- the bracketed and numbered
  ## dialects both produce it. A label-keyed lookup hands both rows the first
  ## candidate's variable; the run then holds ONE distinct variable instead of
  ## two, and a genuine parent/child hierarchy is silently reclassified as a
  ## flat one-variable block.
  for (n in .REI_VOCABS) {
    token <- paste0("<", n$label, ">")
    rows <- list(
      list(label = token, has_annot = TRUE,
           annotation = sprintf("%s.%s", n$ds1, n$var1)),
      list(label = token, has_annot = TRUE,
           annotation = sprintf("%s.%s", n$ds1, n$var2))
    )
    ers <- list(.rei_enrichment(token, n$ds1, n$var1),
                .rei_enrichment(token, n$ds1, n$var2))

    ## Non-vacuous on both counts: the labels really are identical, so the run
    ## is genuinely undecidable by label; and the grammar really reads these as
    ## token rows, so the classifier is actually reached.
    expect_identical(rows[[1]]$label, rows[[2]]$label)
    expect_false(is.null(.token_stem(token)))

    expect_equal(.detect_nested_token_blocks(rows, ers),
                 c("nested_parent", "nested_child"))

    ## And the answer does not depend on the order the enrichments arrive in.
    expect_equal(.detect_nested_token_blocks(rows, rev(ers)),
                 c("nested_parent", "nested_child"))
  }
})

## ---- an unresolved variable role fails closed -------------------------------
##
## Of everything an enrichment carries, only variable_role cannot be rebuilt
## from the row's own annotation. Dropping it does not leave a gap: it asserts
## the ARS default, ANALYSIS, which is a semantics the row never claimed.

test_that("a role that cannot be rebuilt from the annotation is reported", {
  for (n in .REI_VOCABS) {
    plain <- .rei_enrichment(n$label, n$ds1, n$var1)
    expect_length(.enrichment_unrecoverable(list(plain)), 0L)

    plain$variable_role <- "ANALYSIS"
    expect_length(.enrichment_unrecoverable(list(plain)), 0L)

    grouping <- .rei_enrichment(n$label, n$ds2, n$var2)
    grouping$variable_role <- "GROUPING"
    expect_equal(.enrichment_unrecoverable(list(plain, grouping)), "GROUPING")
  }
})

test_that("an unresolved non-default role reaches the ARS analysis", {
  section <- list(tlf_number = "T_9_9_9", sap_text = "", include_total = FALSE,
                  total_condition = NULL, total_label = NULL)
  for (n in .REI_VOCABS) {
    row <- .rei_row(n$label, n$ds1, n$var1, "Y")
    build <- function(er) {
      .build_analysis(section = section, row = row, enrichment = er, index = 1L,
                      as_id = "AS_1", gf_ids = character(0),
                      method_id = "MTH_SUBJECT_COUNT_PCT", ds_id = NULL)
    }

    ## The ordinary case stays completely quiet: no marker, default role.
    plain <- build(.rei_enrichment(n$label, n$ds1, n$var1))
    expect_null(plain$unresolvedVariableRole)
    expect_equal(plain$variableRole, "ANALYSIS")

    ## An unresolved non-default role is carried onto the analysis.
    er <- list()
    er$unresolved_variable_role <- "GROUPING"
    marked <- build(er)
    expect_equal(as.character(unlist(marked$unresolvedVariableRole)), "GROUPING")
  }
})

test_that("an unresolved role blocks execution instead of defaulting to ANALYSIS", {
  ## The claim that matters. It is not enough that a message was emitted: the
  ## resulting analysis must be unable to run as an ordinary ANALYSIS.
  for (n in .REI_VOCABS) {
    ann <- sprintf("%s.%s", n$ds1, n$var1)

    ## Baseline: the same analysis WITHOUT the marker is quiet and runnable.
    plain <- ssrc_analysis(id = "AN_1", dataset = n$ds1, variable = n$var1,
                           annotation = ann)
    plain_gate <- .model_validation_gate(ssrc_model(list(plain)))
    expect_false(plain_gate$blocked)
    expect_false(.UNRESOLVED_ROLE_REF %in% plain_gate$blocking_refs)

    ## With the marker: a FAIL, the gate closes, and execution is refused.
    marked <- plain
    marked$variableRole <- "ANALYSIS"
    marked$unresolvedVariableRole <- list("GROUPING")
    model <- ssrc_model(list(marked))

    f <- as.data.frame(validate_ars_model(model))
    hit <- f[!is.na(f$ref) & f$ref == .UNRESOLVED_ROLE_REF, , drop = FALSE]
    expect_equal(nrow(hit), 1L)
    expect_equal(hit$severity, "GAP")
    ## The proposed role is named, so a different lost role is a failure.
    expect_match(hit$problem, "GROUPING", fixed = TRUE)

    gate <- .model_validation_gate(model)
    expect_true(gate$blocked)
    expect_true(.UNRESOLVED_ROLE_REF %in% gate$blocking_refs)
    ## Not merely reported -- refused. This is what "cannot silently become
    ## executable ANALYSIS" means in practice.
    expect_error(.assert_runnable_ars(model), "structural validation")
  }
})

## ---- the structural consequence, judged from the shell ----------------------

test_that("every annotated shell row resolves a source it declared itself", {
  ## The oracle is the ANNOTATED SHELL, not anything this code generated.
  ##
  ## Each stub row of the shell carries its own "[DATASET.VARIABLE...]" text in
  ## the label column. This test reads that text straight out of the workbook
  ## with a plain regex -- not the package's annotation parser -- keys it by
  ## sheet row, and then asks what the built ARS resolved for the row at that
  ## same sheet row. Expectation from the file, verdict from the artifact.
  ##
  ## The earlier version of this test compared a level row's generated
  ## analysis_id against the generated analysis_id of the nearest preceding
  ## generated non-level row. Both sides came from the generator, so when the
  ## generator wrongly promoted a level row to an analysis of its own, that
  ## wrong row simply became the expected parent and the test passed. It has
  ## been replaced rather than repaired.
  skip_on_cran()
  skip_if_not_installed("openxlsx2")
  drm <- ssrc_drm_inputs()
  skip_if(is.null(drm), "APX-DRM-301 study material is not available")

  out <- withr::local_tempfile(fileext = ".json")
  withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(suppressWarnings(spec_to_ars(
      shell_path = drm$shell, adam_spec_path = drm$spec, output_path = out,
      study_id = "REI", use_llm = FALSE, api_key = "",
      report_path = withr::local_tempfile(fileext = ".xlsx"),
      verbose = FALSE))))

  ## --- expectation: the shell's own text, read independently ---------------
  sheet <- "Table 14.1.2"
  cells <- openxlsx2::wb_to_df(openxlsx2::wb_load(drm$shell), sheet = sheet,
                               col_names = FALSE, skip_empty_rows = FALSE,
                               skip_empty_cols = FALSE)
  ## Sheet row -> the qualified references that row's own annotation names.
  declared_by_sheet_row <- list()
  for (rn in rownames(cells)) {
    text <- as.character(cells[rn, 1])
    if (is.na(text)) next
    inside <- regmatches(text, regexpr("\\[[^]]*\\]", text))
    if (!length(inside)) next
    refs <- regmatches(inside, gregexpr("[A-Z][A-Z0-9]*\\.[A-Z][A-Z0-9]*",
                                        toupper(inside)))[[1]]
    if (!length(refs)) next
    declared_by_sheet_row[[rn]] <- unique(refs)
  }
  expect_gt(length(declared_by_sheet_row), 0L)

  ## --- verdict: what the built event resolved for that same sheet row ------
  j <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  var_by_id <- stats::setNames(
    vapply(j$analyses, function(a) toupper(paste0(
      a$analysisVariable$dataset %||% "", ".",
      a$analysisVariable$variable %||% "")), character(1)),
    vapply(j$analyses, function(a) as.character(a$id %||% ""), character(1)))

  tbl <- Filter(function(o) grepl("14_1_2", o$id %||% ""), j$outputs)[[1]]
  layout <- tbl[["_meta"]]$shell_layout
  skip_if(is.null(layout) || !length(layout), "no shell layout to inspect")

  checked <- 0L
  for (r in layout) {
    aid  <- as.character(r$analysis_id %||% "")
    srow <- as.character(r$sheet_row %||% "")
    if (!nzchar(aid) || !nzchar(srow)) next
    declared <- declared_by_sheet_row[[srow]]
    if (is.null(declared)) next
    resolved <- var_by_id[[aid]]
    if (is.null(resolved) || !nzchar(resolved)) next

    expect_true(
      resolved %in% declared,
      info = sprintf(
        "sheet row %s ('%s') declares %s; the event resolved %s",
        srow, r$label %||% "?", paste(declared, collapse = "/"), resolved))
    checked <- checked + 1L
  }
  ## Non-vacuous: rows were actually compared, not skipped into silence.
  expect_gt(checked, 20L)

  ## And the case this regression exists for was among them: a display label
  ## used by more than one row, where the two rows declare different sources.
  labels <- vapply(layout, function(r) as.character(r$label %||% ""), character(1))
  srows  <- vapply(layout, function(r) as.character(r$sheet_row %||% ""), character(1))
  repeated <- unique(labels[duplicated(labels) & nzchar(labels)])
  conflicting <- 0L
  for (lb in repeated) {
    idx <- which(labels == lb & nzchar(srows))
    decl <- unique(unlist(lapply(srows[idx], function(s) declared_by_sheet_row[[s]])))
    if (length(decl) > 1L) conflicting <- conflicting + 1L
  }
  expect_gt(conflicting, 0L)
})
