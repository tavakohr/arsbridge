## Phase R1 of HANDOFF_realworld_bracket_shells: the bracket normalizer for
## real-world "standardized bracket" shell conventions -- nested directives,
## instruction wrappers carrying the condition in the sentence, footnote
## markers, prose count instructions, Repeat directives.

## --- the normalizer, in isolation -------------------------------------------

test_that("a nested PROGRAMMING DATASETS directive is lifted out cleanly", {
  n <- .unwrap_bracket_instructions(
    'Completed Population [ADSL. COMPLFL="Y" [PROGRAMMING DATASETS USED: ADSL]]')
  expect_equal(n$text, 'Completed Population [ADSL.COMPLFL="Y"]')
  expect_equal(n$source_datasets, "ADSL")
})

test_that("the instruction wrapper unwraps to its embedded condition", {
  n <- .unwrap_bracket_instructions(paste0(
    "Subjects completed [a] [Use the stated source variable for the ",
    "displayed item and apply the stated condition: USUBJID WHERE ",
    'ADSL.COMPLFL="Y". Keep the display label separate from the filter.]'))
  ## The footnote marker is gone, the condition leads, the subject-count
  ## semantics ride behind the semicolon.
  expect_equal(
    n$text,
    'Subjects completed [ADSL.COMPLFL="Y"; count of unique USUBJID]')
})

test_that("the per-row 'Use DS.VAR' variant becomes VAR WHERE FILTER", {
  n <- .unwrap_bracket_instructions(paste0(
    "Treatment added [Use ADCM.TRTSTAT for this displayed row; apply ",
    'ADCM.ONMEDFL="Y" only as the row or record filter indicated by the ',
    "shell context.]"))
  expect_equal(n$text, 'Treatment added [ADCM.TRTSTAT WHERE ADCM.ONMEDFL="Y"]')
})

test_that("a prose count instruction becomes condition + count marker", {
  n <- .unwrap_bracket_instructions(paste0(
    "Any history [Count distinct subjects using USUBJID. Apply the ",
    'following condition: UNIQUE SUBJECTS WITH ADMH.MHCAT="GENERAL HISTORY".]'))
  expect_equal(
    n$text,
    'Any history [ADMH.MHCAT="GENERAL HISTORY"; count of unique USUBJID]')
})

test_that("compound NE/AND machine annotations pass through untouched", {
  n <- .unwrap_bracket_instructions(
    'Not coded [ADMH.MHDECOD NE "" AND ADMH.MHBODSYS=""]')
  expect_equal(n$text, 'Not coded [ADMH.MHDECOD NE "" AND ADMH.MHBODSYS=""]')
})

test_that("a Repeat directive is dropped and reported, never kept", {
  n <- .unwrap_bracket_instructions(paste0(
    "... [Repeat the applicable row or section using this ordered ",
    'instruction: Repeat for ADCM.MEDPRES IN ("DrugA", "DrugB") WHERE ',
    'ADCM.ONMEDFL="Y". Expand all repeated items before final programming.]'))
  expect_equal(n$text, "...")
  expect_length(n$dropped, 1)
  expect_match(n$dropped, "^Repeat")
})

test_that("footnote markers and pure guidance prose are removed", {
  expect_equal(.unwrap_bracket_instructions("Preferred Term [a]")$text,
               "Preferred Term")
  n <- .unwrap_bracket_instructions(
    "Total (N=XX) [Percentages are based on the number of enrolled subjects.]")
  expect_equal(n$text, "Total (N=XX)")
  expect_length(n$dropped, 1)
})

test_that("plain annotations and bracket-free text are never altered", {
  expect_equal(.unwrap_bracket_instructions("Age (years) [ADSL.AGE]")$text,
               "Age (years) [ADSL.AGE]")
  expect_equal(.unwrap_bracket_instructions("Mean (SD)")$text, "Mean (SD)")
  expect_equal(.unwrap_bracket_instructions("n")$text, "n")
  ## An unclosed bracket (annotation wrapped into the next paragraph --
  ## a later phase) must pass through unharmed.
  expect_equal(.unwrap_bracket_instructions("Row label [ADSL.AGE")$text,
               "Row label [ADSL.AGE")
})

test_that("a bare wrapper (coloured-run candidate, no brackets) rewrites", {
  n <- .unwrap_bracket_instructions(paste0(
    "Use the stated source variable for the displayed item and apply the ",
    "stated condition: All Patients WHERE ADSL.SCRNFL=\"Y\". Keep the ",
    "display label separate from the filter."))
  expect_equal(n$text, 'ADSL.SCRNFL="Y"; count of unique USUBJID')
})

test_that("the numeric IN list is a recognised annotation form", {
  pieces <- split_label_annotation("Total (N=XX) [ADSL.COHORTN IN (1,2)]")
  expect_equal(pieces$label, "Total (N=XX)")
  expect_equal(pieces$annotation, "ADSL.COHORTN IN (1,2)")
})

## --- the neutral fixture, end to end ----------------------------------------

no_llm_keys_rwb <- function(code) {
  withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    code)
}

.rwb_secs <- function() {
  parse_shell_docx(test_path("fixtures/annotated_shell_rwb_bracket.docx"))
}

test_that("the RWB fixture parses into one clean section", {
  secs <- suppressMessages(.rwb_secs())
  expect_length(secs, 1)
  s <- secs[[1]]

  expect_equal(s$title, "Subject Disposition")
  ## The nested directive never leaks into the population filter -- this is
  ## the exact corruption observed on the real study.
  expect_equal(s$population_annot, "ADSL.SCRNFL='Y'")
  expect_false(grepl("PROGRAMMING", s$population_text))
  expect_true("ADSL" %in% s$source_datasets)
})

test_that("RWB stub rows carry clean labels and unwrapped annotations", {
  secs <- suppressMessages(.rwb_secs())
  rows <- secs[[1]]$stub_rows
  ann <- vapply(rows, function(r) r$annotation %||% "", character(1))
  lbl <- vapply(rows, function(r) r$label, character(1))

  expect_equal(lbl[2], "Subjects completed")
  expect_equal(ann[2], "ADSL.COMPLFL='Y'; count of unique USUBJID")
  expect_equal(ann[3], "ADMH.MHCAT='GENERAL HISTORY'; count of unique USUBJID")
  expect_equal(ann[4], "ADMH.MHDECOD NE '' AND ADMH.MHBODSYS=''")
  expect_equal(ann[5], "ADCM.TRTSTAT WHERE ADCM.ONMEDFL='Y'")
  ## The Repeat row keeps its continuation label, loses the directive.
  expect_equal(lbl[6], "...")
  expect_equal(ann[6], "")
  ## No label carries wrapper prose or a footnote marker.
  expect_false(any(grepl("\\[a\\]|stated condition|displayed row", lbl)))
})

test_that("RWB column headers resolve to annotated groups", {
  secs <- suppressMessages(.rwb_secs())
  s <- secs[[1]]

  expect_equal(s$col_headers,
               c("Alpha Cohort (N=XX)", "Beta Cohort (N=XX)",
                 "Unknown (N=XX)", "Total (N=XX)"))
  expect_equal(s$col_labels_full,
               c("", "Alpha Cohort (N=XX)", "Beta Cohort (N=XX)",
                 "Unknown (N=XX)", "Total (N=XX)"))
  cg <- s$column_groups
  expect_equal(cg$variable, "COHORTN")
  conds <- vapply(cg$groups, function(g) g$annotation %||% "", character(1))
  expect_equal(conds, c("ADSL.COHORTN=1", "ADSL.COHORTN=2",
                        "is.na(ADSL.COHORTN) OR ADSL.COHORTN==99"))
})

test_that("the RWB fixture builds into an ARS with subject-count semantics", {
  secs <- suppressMessages(.rwb_secs())
  re <- build_ars_json(secs, study_id = "S-RWB")
  expect_length(re$outputs, 1)
  columns <- vapply(
    re$outputs[[1]]$displays[[1]]$display$columns,
    function(column) column$label,
    character(1)
  )
  expect_equal(columns,
               c("", "Alpha Cohort (N=XX)", "Beta Cohort (N=XX)",
                 "Unknown (N=XX)", "Total (N=XX)"))

  ## The wrapper-unwrapped row routes to a distinct-subject count, exactly
  ## like a hand-authored "count of unique USUBJID" annotation would.
  ans <- re$analyses
  labels <- vapply(re$outputs[[1]][["_meta"]][["shell_layout"]],
                   function(e) e$label, character(1))
  expect_true("Subjects completed" %in% labels)
  forms <- vapply(re$outputs[[1]][["_meta"]][["shell_layout"]],
                  function(e) e$stat_form %||% NA_character_, character(1))
  expect_true(any(!is.na(forms) & forms == "subject_count_pct"))
})


## --- Phase R2: row scope and stub-cell structure ----------------------------

test_that("a fully struck-through stub row is skipped with a diagnostic", {
  diag_reset()
  secs <- suppressMessages(.rwb_secs())
  lbl <- vapply(secs[[1]]$stub_rows, function(r) r$label, character(1))

  expect_false(any(grepl("rescreened", lbl, ignore.case = TRUE)))

  d <- diag_records()
  hit <- d[grepl("struck through", d$problem), , drop = FALSE]
  expect_equal(nrow(hit), 1)
  expect_equal(hit$severity, "INFO")
  expect_match(hit$problem, "Subjects rescreened")
})

test_that("a stub label split across paragraphs joins with a space", {
  secs <- suppressMessages(.rwb_secs())
  lbl <- vapply(secs[[1]]$stub_rows, function(r) r$label, character(1))
  wrapped <- lbl[grepl("^Ongoing", lbl)]

  expect_length(wrapped, 1)
  ## The Word wrap fell between "data" and "extraction"; joining the
  ## paragraphs bare would fuse them into "dataextraction".
  expect_equal(wrapped, "Ongoing subjects at the time of the data extraction, n (%)")
  expect_false(grepl("dataextraction", wrapped))
})

test_that(".all_text_struck needs EVERY texted run struck, not just one", {
  run <- function(text, strike) list(text = text, strike = strike)
  expect_true(.all_text_struck(list(run("Removed row", TRUE))))
  expect_true(.all_text_struck(list(run("Removed", TRUE), run(" row", TRUE))))
  ## A crossed-out value retyped beside it is a LIVE row.
  expect_false(.all_text_struck(list(run("Old", TRUE), run("New", FALSE))))
  ## Whitespace-only runs never decide the verdict either way.
  expect_true(.all_text_struck(list(run("Gone", TRUE), run("   ", FALSE))))
  expect_false(.all_text_struck(list(run("   ", FALSE))))
  expect_false(.all_text_struck(list()))
})


## --- wrapped annotations and continuation tables ----------------------------

test_that(".cell_text joins mid-annotation breaks without a space", {
  ## Word wraps a narrow header cell mid-token. A space there truncates the
  ## variable (ADSL.C) and loses the condition entirely; joining bare keeps
  ## the reference whole.
  expect_true(.unclosed_bracket("[ADSL.C"))
  expect_false(.unclosed_bracket("[ADSL.AGE]"))
  expect_false(.unclosed_bracket("Ongoing at the time of the data"))

  ## The two joins the rule has to tell apart.
  expect_equal(extract_annotation_vars("ADSL.CGHGR1N=1"), "ADSL.CGHGR1N")
  expect_equal(extract_annotation_vars("ADSL.C GHGR1N =1"), "ADSL.C")
})

test_that("a header annotation split mid-token still resolves", {
  secs <- suppressMessages(.rwb_secs())
  groups <- secs[[1]]$column_groups$groups
  conds  <- vapply(groups, function(g) g$annotation %||% "", character(1))
  ## The fixture authors this cohort's annotation across two paragraphs,
  ## broken between "ADSL.C" and "OHORTN=1".
  expect_equal(conds[1], "ADSL.COHORTN=1")
  expect_false(any(grepl("ADSL\\.C\\s", conds)))
})

test_that("a continuation table's rows join the display above it", {
  diag_reset()
  secs <- suppressMessages(.rwb_secs())
  sec  <- secs[[1]]

  ## One heading, one output -- the second table is not a new section.
  expect_length(secs, 1)

  lbl <- vapply(sec$stub_rows, function(r) r$label, character(1))
  expect_true("Subjects re-screened after washout" %in% lbl)
  expect_true("Subjects re-consented" %in% lbl)
  ## Appended in document order, after the first table's rows.
  expect_equal(tail(lbl, 2),
               c("Subjects re-screened after washout", "Subjects re-consented"))
  ## Their annotations survive the append.
  ann <- vapply(sec$stub_rows, function(r) r$annotation %||% "", character(1))
  expect_equal(ann[lbl == "Subjects re-consented"], "ADSL.RECONFL='Y'")

  ## The continuation's repeated header row is not mistaken for data, and
  ## does not overwrite the captured column headers.
  expect_false(any(grepl("Cohort \\(N=XX\\)", lbl)))
  expect_equal(sec$col_headers[1], "Alpha Cohort (N=XX)")

  d <- diag_records()
  hit <- d[grepl("continuation table", d$problem), , drop = FALSE]
  expect_equal(nrow(hit), 1)
  expect_equal(hit$severity, "INFO")
  expect_match(hit$problem, "2 more row")
})

test_that("the Total column counts what the shell annotated, not everybody", {
  ## The defect this pins, end to end. The fixture heads its fourth column
  ## "Total (N=XX) [ADSL.COHORTN IN (1,2)]" beside an Unknown cohort, so Total
  ## deliberately excludes a displayed column. Two wrong answers are possible
  ## and the counts are chosen so neither can be mistaken for the right one:
  ##   40 + 30 = 70  the annotated scope
  ##   40 + 30 + 7 = 77  "the whole analysis set", what include_total used to
  ##                     mean, and what an unscoped total pass would report
  ##   0             a Total treated as a fourth level of the grouping factor,
  ##                 shadowed by the columns it totals
  n1 <- 40L; n2 <- 30L; nu <- 7L
  adam <- withr::local_tempdir()
  haven::write_xpt(
    data.frame(
      STUDYID = "S-RWB",
      USUBJID = sprintf("S-%03d", seq_len(n1 + n2 + nu)),
      COHORTN = c(rep(1, n1), rep(2, n2), rep(99, nu)),
      SCRNFL  = "Y", SCRNFN = 1, COMPLFL = "Y",
      stringsAsFactors = FALSE),
    file.path(adam, "adsl.xpt"))

  out <- withr::local_tempdir()
  ard <- no_llm_keys_rwb({
    suppressMessages(suppressWarnings(spec_to_ars(
      shell_path     = test_path("fixtures/annotated_shell_rwb_bracket.docx"),
      adam_spec_path = test_path("fixtures/adam_spec_rwe.xlsx"),
      output_path    = file.path(out, "re.json"), study_id = "S-RWB",
      code_dir       = file.path(out, "code"), verbose = FALSE)))
    suppressMessages(suppressWarnings(
      ars_to_ard(file.path(out, "re.json"), adam)))
  })

  skip_if(is.null(ard), "no analyses executed against the synthetic ADSL")
  levels <- vapply(ard$group1_level, function(x) as.character(x)[[1]],
                   character(1))
  totals <- unlist(ard$stat)[levels %in% "Total" & ard$stat_name %in% "n"]
  expect_gt(length(totals), 0)
  expect_true(all(totals == n1 + n2))
  expect_false(any(totals == n1 + n2 + nu))
  expect_false(any(totals == 0))
})
