## The fix report: what a run could not resolve, and where to change it.
##
## Two things are being protected here, and they fail in opposite directions.
##
## The catalogue must stay in step with the finding vocabulary, or a new check
## ships a code the report cannot explain -- a row that names a defect and
## offers no remedy.
##
## The report must describe what the ENGINE did, not what the catalogue thinks
## it should have done. Those come apart in a case that really occurs: a
## reserving finding naming an entity nothing references reserves nothing at
## all. A report that read the scope instead of the map would send an author
## looking for withheld cells that do not exist.


test_that("every registered code has a hint, and every hint a code", {
  ## One line, and it is the whole coverage contract: setequal, not a subset
  ## either way. A missing hint leaves a finding unexplained; an orphan hint
  ## outlives the check it describes and quietly rots.
  expect_setequal(names(.VALIDATION_REFS), names(.FIX_HINTS))
})


test_that("every hint answers all four questions, and names a real document", {
  for (ref in names(.FIX_HINTS)) {
    hint <- .FIX_HINTS[[ref]]
    expect_true(all(c("cause", "consequence", "fix_where", "fix") %in%
                      names(hint)),
                info = ref)
    ## Non-empty, because an empty string renders as a blank cell that reads
    ## as "nothing to say here" rather than as an omission.
    for (field in c("cause", "consequence", "fix")) {
      expect_true(nzchar(hint[[field]]), info = paste(ref, field))
    }
    ## `fix_where` is closed for the same reason `ref` is: the column is meant
    ## to be sorted and counted, and free text cannot be.
    expect_true(hint$fix_where %in% .FIX_WHERE, info = ref)
  }
})


test_that("an unregistered code degrades to a placeholder, not an error", {
  ## Bookkeeping about a run must never take a finished run down. It must not
  ## look complete either, so the placeholder says plainly that nothing is
  ## registered.
  hint <- .fix_hint("NO_SUCH_CODE_EXISTS")
  expect_true(is.na(hint$fix_where))
  expect_match(hint$fix, "No fix hint is registered")

  expect_silent(.fix_hint(NA_character_))
})


test_that("the phase is in the file name, so two runs sit side by side", {
  dir <- tempfile()
  expect_identical(basename(.fix_report_path(dir, "deterministic")),
                   "fix_report_deterministic.xlsx")
  expect_identical(basename(.fix_report_path(dir, "supplement")),
                   "fix_report_supplement.xlsx")
  expect_identical(basename(.fix_report_path(dir, "llm")),
                   "fix_report_llm.xlsx")
  ## No mode resolved is not an error; it is a report without a phase in its
  ## name.
  expect_identical(basename(.fix_report_path(dir, NA_character_)),
                   "fix_report.xlsx")
})


test_that("reserved_as reports what the engine did, not what scope implies", {
  ## The case the whole column exists for. Two reserving findings: one whose
  ## entity analyses actually reference, one naming an entity nothing points
  ## at. Same scope, same severity -- and only one of them withholds anything.
  model <- .rmap_model()
  findings <- .new_findings()
  findings <- .add_finding(
    findings, "GAP", "analysis_sets", "AS_SYNTH", "id",
    "A population an analysis uses.", "Fix it.",
    ref = "ANALYSIS_SET_REF_UNRESOLVED")
  findings <- .add_finding(
    findings, "GAP", "analysis_sets", "AS_NOTHING_POINTS_HERE", "id",
    "A population no analysis references.", "Fix it.",
    ref = "ANALYSIS_SET_REF_UNRESOLVED")

  reservations <- .reservations_from_findings(model, findings)
  reserved_as <- .reserved_as_column(findings, reservations)

  ## Non-vacuity: the fixture must actually exercise both halves, or this test
  ## passes by describing nothing.
  expect_gt(length(reservations$by_finding[["1"]]), 0L)
  expect_length(reservations$by_finding[["2"]], 0L)

  expect_match(reserved_as[1], "^reserved [0-9]+ analys")
  expect_match(reserved_as[2], "nothing references it")
})


test_that("advisory and cell findings say what they are, not 'reserved'", {
  model <- .rmap_model()
  findings <- .new_findings()
  findings <- .add_finding(
    findings, "WARN", "outputs", "T_SYNTH", "columns",
    "A column label that matches no level.", "Fix it.",
    ref = "FLAT_AXIS_COLUMN_LABEL_MISMATCH")
  findings <- .add_finding(
    findings, "WARN", "analyses", "AN_SYNTH_001", "method",
    "The placeholder shows more numbers than the method computes.", "Fix it.",
    ref = "METHOD_PLACEHOLDER_SLOT_MISMATCH")

  reserved_as <- .reserved_as_column(
    findings, .reservations_from_findings(model, findings))

  expect_identical(reserved_as[1], "reported only")
  expect_identical(reserved_as[2], "refused per cell at fill")
})


test_that("the fix list joins each finding to its own provenance", {
  ## Sorting by severity happens AFTER the provenance column is computed,
  ## because `by_finding` is keyed by row number in the frame the map was built
  ## from. Sorting first would join each finding to another finding's
  ## reservations -- and the join would still look plausible, which is what
  ## makes it worth a test.
  model <- .rmap_model()
  findings <- .new_findings()
  ## Deliberately out of severity order: the INFO row is first, so a correct
  ## implementation has to move it and carry its provenance with it.
  findings <- .add_finding(
    findings, "INFO", "analyses", "AN_SYNTH_001", "id",
    "An analysis nothing displays.", "Fix it.",
    ref = "ANALYSIS_NOT_DISPLAYED")
  findings <- .add_finding(
    findings, "GAP", "analysis_sets", "AS_SYNTH", "id",
    "A population that is not in the event.", "Fix it.",
    ref = "ANALYSIS_SET_REF_UNRESOLVED")

  reservations <- .reservations_from_findings(model, findings)
  sheet <- .fix_list_sheet(findings, reservations)

  expect_equal(nrow(sheet), 2L)
  ## GAP sorts first.
  expect_identical(sheet$Status[1], "GAP")
  expect_identical(sheet$ref[1], "ANALYSIS_SET_REF_UNRESOLVED")
  ## And it kept ITS provenance, not the row that used to sit above it.
  expect_match(sheet$reserved_as[1], "^reserved [0-9]+ analys")
  expect_identical(sheet$reserved_as[2], "reported only")
  ## The hint travelled with the row too.
  expect_identical(sheet$fix_where[1],
                   .FIX_HINTS[["ANALYSIS_SET_REF_UNRESOLVED"]]$fix_where)
})


test_that("the workbook has its six sheets, and Reserved cells is always one", {
  skip_if_not_installed("openxlsx2")

  model <- .rmap_model()
  model$analyses$analysisSetId[model$analyses$id == "AN_SYNTH_001"] <- "AS_GONE"
  findings <- validate_ars_model(model)
  reservations <- .reservations_from_findings(model, findings)
  expect_gt(length(reservations$by_analysis), 0L)

  path <- file.path(tempfile("fixrep"), "fix_report_deterministic.xlsx")
  dir.create(dirname(path), recursive = TRUE)
  diagnostics <- data.frame(
    stage = "build", severity = "WARN", input = NA_character_,
    tlf_number = NA_character_, location = NA_character_,
    problem = "A diagnostic.", action = "Read it.",
    stringsAsFactors = FALSE)

  write_fix_report(findings, path, reservations = reservations,
                   diagnostics = diagnostics,
                   run = list(extraction_mode = "deterministic",
                              verdict = "COMPLETED WITH GAPS"))

  sheets <- openxlsx2::wb_get_sheet_names(openxlsx2::wb_load(path))
  expect_setequal(unname(sheets),
                  c("Run", "Fix list", "By ref", "Reserved cells",
                    "Diagnostics (run)", "Legend"))

  reserved <- openxlsx2::read_xlsx(path, sheet = "Reserved cells")
  expect_gt(nrow(reserved), 0L)
  expect_true("AN_SYNTH_001" %in% reserved$analysis_id)
})


test_that("a clean event still gets a report saying nothing was reserved", {
  ## An absent file is ambiguous between "clean" and "the report failed", so
  ## the clean run is written too -- and "Reserved cells" is present and says
  ## so, because an omitted sheet reads as "nothing was reserved" only if you
  ## already know the sheet would have been there.
  skip_if_not_installed("openxlsx2")

  model <- .rmap_model()
  findings <- validate_ars_model(model)
  reservations <- .reservations_from_findings(model, findings)
  expect_length(reservations$by_analysis, 0L)

  path <- file.path(tempfile("cleanrep"), "fix_report_deterministic.xlsx")
  dir.create(dirname(path), recursive = TRUE)
  write_fix_report(findings, path, reservations = reservations)

  expect_true(file.exists(path))
  reserved <- openxlsx2::read_xlsx(path, sheet = "Reserved cells")
  expect_equal(nrow(reserved), 1L)
  expect_identical(reserved$Status[1], "PASS")
  expect_match(reserved$message[1], "No result was reserved")

  ## Six sheets on a clean run too. The count is fixed rather than conditional
  ## on what the run happened to find: a reader who counts five cannot tell a
  ## quiet run from a writer that gave up part way.
  sheets <- openxlsx2::wb_get_sheet_names(openxlsx2::wb_load(path))
  expect_setequal(unname(sheets),
                  c("Run", "Fix list", "By ref", "Reserved cells",
                    "Diagnostics (run)", "Legend"))
  ## And the empty diagnostics sheet says it is empty rather than being blank.
  diagnostics <- openxlsx2::read_xlsx(path, sheet = "Diagnostics (run)")
  expect_equal(nrow(diagnostics), 1L)
  expect_match(diagnostics$message[1], "no diagnostics")
})


test_that("a census turns analysis-level reservations into addressable cells", {
  ## Before a fill the honest answer is an analysis id. After one it is a sheet
  ## and a cell the author can open, which is the difference between a report
  ## that describes the model and one that can be acted on.
  reservations <- list(
    by_analysis = list(
      AN_SYNTH_001 = list(ref = "ANALYSIS_SET_REF_UNRESOLVED",
                          scope = "analysis",
                          reason = "A population that is not in the event.")),
    by_finding = list("1" = "AN_SYNTH_001")
  )
  census <- data.frame(
    sheet = c("Table X", "Table X"),
    ref = c("B5", "C5"),
    analysis_id = c("AN_SYNTH_001", "AN_SYNTH_002"),
    status = c("pending", "filled"),
    reason = c("reserved: the reporting event does not resolve for this cell",
               NA_character_),
    stringsAsFactors = FALSE)

  sheet <- .reserved_cells_sheet(reservations, census)

  ## Only the reserved analysis's cell, and it is addressed.
  expect_equal(nrow(sheet), 1L)
  expect_identical(sheet$ref[1], "B5")
  expect_identical(sheet$sheet[1], "Table X")
  expect_identical(sheet$finding_ref[1], "ANALYSIS_SET_REF_UNRESOLVED")
})


test_that("the by-ref rollup counts findings and the analyses they reserved", {
  model <- .rmap_model()
  findings <- .new_findings()
  findings <- .add_finding(
    findings, "GAP", "analysis_sets", "AS_SYNTH", "id",
    "A population that is not in the event.", "Fix it.",
    ref = "ANALYSIS_SET_REF_UNRESOLVED")
  findings <- .add_finding(
    findings, "GAP", "analysis_sets", "AS_OTHER", "id",
    "Another population that is not in the event.", "Fix it.",
    ref = "ANALYSIS_SET_REF_UNRESOLVED")

  reservations <- .reservations_from_findings(model, findings)
  sheet <- .by_ref_sheet(findings, reservations)

  row <- sheet[sheet$ref == "ANALYSIS_SET_REF_UNRESOLVED", , drop = FALSE]
  expect_equal(nrow(row), 1L)
  expect_equal(row$n_findings[1], 2L)
  ## Non-vacuity: two findings that between them reserved real analyses, or
  ## the comparison below would hold trivially at zero.
  expect_gt(row$n_analyses_reserved[1], 0L)
  ## Unique analyses: two findings reaching the same analysis is one reserved
  ## analysis, not two.
  expect_equal(
    row$n_analyses_reserved[1],
    length(unique(unlist(reservations$by_finding, use.names = FALSE)))
  )
})


test_that("spec_to_ars writes the fix report on a clean run too", {
  skip_if_not_installed("openxlsx2")
  skip_on_cran()

  out <- withr::local_tempdir()
  result <- withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(suppressWarnings(spec_to_ars(
      shell_path     = arsbridge_example("annotated_shell.xlsx"),
      adam_spec_path = arsbridge_example("adam_spec.xlsx"),
      api_key = "", use_llm = FALSE, verbose = FALSE,
      output_path = file.path(out, "re.json"),
      report_path = file.path(out, "report.xlsx"),
      emit_code = FALSE))))

  expect_false(is.null(result$fix_report_path))
  expect_true(file.exists(result$fix_report_path))
  ## The phase that ran is in the name, so a later supplement run lands beside
  ## this one instead of overwriting it.
  expect_identical(basename(result$fix_report_path),
                   paste0("fix_report_", result$extraction_mode, ".xlsx"))

  run <- openxlsx2::read_xlsx(result$fix_report_path, sheet = "Run")
  expect_true("extraction mode" %in% run$Item)
  expect_identical(run$Value[run$Item == "extraction mode"],
                   result$extraction_mode)
  expect_identical(run$Value[run$Item == "verdict"],
                   result$validation_gate$verdict)
})


test_that("two phases leave two reports side by side, not one overwritten", {
  ## The reason the phase is in the file name at all: running the supplement
  ## after the deterministic pass should let an author SEE what the supplement
  ## resolved, which is impossible if the second run overwrites the first.
  skip_if_not_installed("openxlsx2")
  skip_on_cran()

  out <- withr::local_tempdir()
  run_phase <- function(supplement, json) {
    withr::with_envvar(
      c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
        GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
      suppressMessages(suppressWarnings(spec_to_ars(
        shell_path     = arsbridge_example("annotated_shell.xlsx"),
        adam_spec_path = arsbridge_example("adam_spec.xlsx"),
        supplement     = supplement,
        api_key = "", use_llm = FALSE, verbose = FALSE,
        output_path = file.path(out, json),
        report_path = file.path(out, "report.xlsx"),
        emit_code = FALSE))))
  }

  deterministic <- run_phase(NULL, "det.json")
  supplemented <- run_phase(arsbridge_example("supplement.json"), "sup.json")

  ## The two phases really did differ, or the rest of this test is comparing
  ## a run with itself.
  expect_identical(deterministic$extraction_mode, "deterministic")
  expect_identical(supplemented$extraction_mode, "supplement")

  expect_true(file.exists(deterministic$fix_report_path))
  expect_true(file.exists(supplemented$fix_report_path))
  expect_false(identical(deterministic$fix_report_path,
                         supplemented$fix_report_path))

  ## Both are still on disk after the second run -- the point of the naming.
  reports <- list.files(out, pattern = "^fix_report_.*\\.xlsx$")
  expect_setequal(reports,
                  c("fix_report_deterministic.xlsx",
                    "fix_report_supplement.xlsx"))

  ## And the supplement's own provenance is recorded, not defaulted away.
  run <- openxlsx2::read_xlsx(supplemented$fix_report_path, sheet = "Run")
  expect_identical(run$Value[run$Item == "supplement trust"],
                   supplemented$supplement_trust)
})
