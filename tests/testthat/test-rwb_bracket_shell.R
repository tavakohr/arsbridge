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

  ## The wrapper-unwrapped row routes to a distinct-subject count, exactly
  ## like a hand-authored "count of unique USUBJID" annotation would.
  ans <- re$analyses
  labels <- vapply(re$outputs[[1]][["_meta"]][["shell_layout"]],
                   function(e) e$label, character(1))
  expect_true("Subjects completed" %in% labels)
  kinds <- vapply(re$outputs[[1]][["_meta"]][["shell_layout"]],
                  function(e) e$kind, character(1))
  expect_true(any(kinds %in% c("subject_count", "filtered_count")))
})
