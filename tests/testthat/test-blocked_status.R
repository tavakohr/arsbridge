## `blocked`: the ARD contract extension.
##
## Four statuses now, and they mean different things to different people:
##
##   computed       a trustworthy engine result
##   manual_pending valid work a programmer must derive by hand
##   manual_filled  a validated manual result
##   blocked        computation could not safely proceed, because required data
##                  or filter semantics could not be satisfied
##
## The distinction that matters is between the middle two and the last. A
## manual_pending cell is somebody's job; a blocked cell is nobody's, until the
## spec or the ADaM cut is repaired. Putting a blocked cell on a derivation
## worklist would send a programmer to compute something that cannot be
## computed, so every consumer is checked here rather than left to fall through
## an `else`.

skip_if_not_installed("cards")
skip_if_not_installed("withr")

.bs_adam <- function(envir = parent.frame()) {
  td <- withr::local_tempdir(.local_envir = envir)
  utils::write.csv(data.frame(
    USUBJID = sprintf("S%02d", 1:6),
    TRT01A  = rep(c("Drug A", "Placebo"), each = 3),
    SAFFL   = "Y",
    COHORTN = c(1, 1, 2, 1, 2, 2),
    stringsAsFactors = FALSE
  ), file.path(td, "adsl.csv"), row.names = FALSE)

  utils::write.csv(data.frame(
    USUBJID = c("S01", "S02", "S04", "S05"),
    TRT01A  = c("Drug A", "Drug A", "Placebo", "Placebo"),
    AEDECOD = "Headache",
    stringsAsFactors = FALSE
  ), file.path(td, "adae.csv"), row.names = FALSE)

  utils::write.csv(data.frame(
    USUBJID  = c("S01", "S04"),
    CMDECOD  = c("ASPIRIN", "ASPIRIN"),
    CONTRTFL = "Y",
    stringsAsFactors = FALSE
  ), file.path(td, "adcm.csv"), row.names = FALSE)

  ## No subject key at all.
  utils::write.csv(data.frame(SITEID = "01", FLAG = "Y",
                              stringsAsFactors = FALSE),
                   file.path(td, "adnokey.csv"), row.names = FALSE)
  td
}

.bs_cond <- function(ds, var, val) {
  list(dataset = ds, variable = var, comparator = "EQ", value = list(val))
}

.bs_spec <- function(pop) {
  list(
    id = "BS", name = "BS", version = "1",
    analysisSets = list(c(list(id = "AS_POP", name = "Population"), pop)),
    dataSubsets = list(),
    analysisGroupings = list(list(
      id = "GF_TRT", name = "TRT01A",
      groupingVariable = list(dataset = "ADSL", variable = "TRT01A"),
      dataDriven = TRUE)),
    methods = list(list(id = "MTH_SUBJECT_COUNT_PCT",
                        name = "Subject Count and Percentage")),
    analyses = list(list(
      id = "AN_BLOCK", methodId = "MTH_SUBJECT_COUNT_PCT",
      analysisSetId = "AS_POP",
      analysisVariable = list(dataset = "ADAE", variable = "AEDECOD"),
      orderedGroupings = list(list(order = 1, groupingId = "GF_TRT",
                                   resultsByGroup = TRUE)))),
    outputs = list(list(id = "T_BLOCK", name = "T-BLOCK",
                        referencedAnalysisIds = list("AN_BLOCK"))))
}

.bs_run <- function(pop, dir, name = "bs.json", ...) {
  path <- file.path(dir, name)
  jsonlite::write_json(.bs_spec(pop), path, auto_unbox = TRUE, null = "null")
  suppressMessages(suppressWarnings(ars_to_ard(path, dir, ...)))
}

.bs_status <- function(ard, status) {
  if (is.null(ard) || !"result_status" %in% names(ard)) return(0L)
  st <- as.character(ard[["result_status"]])
  sum(!is.na(st) & st == status)
}

.bs_reason <- function(ard) {
  if (is.null(ard) || !"block_reason" %in% names(ard)) return(NA_character_)
  value <- ard[["block_reason"]]
  if (is.list(value)) value <- unlist(value, use.names = FALSE)
  unique(as.character(value[!is.na(value)]))
}


## One output holding a computed analysis AND a blocked one, so the table
## actually renders and can carry the note. A blocked-only output takes the
## placeholder path instead and never reaches tfrmt.
.bs_mixed <- function(dir, name = "mixed.json") {
  spec <- .bs_spec(list(condition = .bs_cond("ADSL", "SAFFL", "Y")))
  spec$analysisSets[[2]] <- list(
    id = "AS_BLOCK", name = "Blocked",
    condition = .bs_cond("ADXX", "FLAG", "Y"))
  spec$analyses[[1]]$id <- "AN_OK"
  spec$analyses[[2]] <- list(
    id = "AN_BLOCK", methodId = "MTH_SUBJECT_COUNT_PCT",
    analysisSetId = "AS_BLOCK",
    analysisVariable = list(dataset = "ADAE", variable = "AEDECOD"),
    orderedGroupings = list(list(order = 1, groupingId = "GF_TRT",
                                 resultsByGroup = TRUE)))
  spec$outputs[[1]]$referencedAnalysisIds <- list("AN_OK", "AN_BLOCK")

  path <- file.path(dir, name)
  jsonlite::write_json(spec, path, auto_unbox = TRUE, null = "null")
  list(path = path,
       ard = suppressMessages(suppressWarnings(ars_to_ard(path, dir))))
}

## The three causes, each with the clause that provokes it.
.bs_causes <- list(
  missing_dataset = list(
    pop = list(condition = .bs_cond("ADXX", "FLAG", "Y")),
    says = "not in the ADaM directory"),
  missing_subject_key = list(
    pop = list(condition = .bs_cond("ADNOKEY", "FLAG", "Y")),
    says = "no subject key"),
  ambiguous_row_coherence = list(
    pop = list(compoundExpression = list(
      logicalOperator = "AND",
      whereClauses = list(
        list(compoundExpression = list(
          logicalOperator = "OR",
          whereClauses = list(
            list(condition = .bs_cond("ADCM", "CMDECOD", "ASPIRIN")),
            list(condition = .bs_cond("ADSL", "SAFFL", "Y"))))),
        list(condition = .bs_cond("ADCM", "CONTRTFL", "Y"))))),
    says = "same record")
)

# ---- each cause blocks, and computes nothing --------------------------------

for (cause in names(.bs_causes)) {
  local({
    this <- cause
    spec <- .bs_causes[[cause]]

    test_that(paste0("blocked, with no computed rows: ", this), {
      td  <- .bs_adam()
      ard <- .bs_run(spec$pop, td, name = paste0(this, ".json"))

      expect_equal(.bs_status(ard, "blocked"), 1L)
      expect_equal(.bs_status(ard, "computed"), 0L)
      expect_equal(.bs_reason(ard), this)
    })

    test_that(paste0("the blocker reason maps back to the analysis: ", this), {
      ## No extra ARD column is needed for the mapping: the blocked row carries
      ## analysis_id, and the FAIL diagnostic carries the same id in `location`.
      td  <- .bs_adam()
      ard <- .bs_run(spec$pop, td, name = paste0(this, "_map.json"))

      blocked_ids <- unique(as.character(ard[["analysis_id"]])[
        as.character(ard[["result_status"]]) == "blocked"])
      expect_equal(blocked_ids, "AN_BLOCK")

      blockers <- ars_blockers()
      hit <- blockers[blockers$location == "AN_BLOCK", , drop = FALSE]
      expect_equal(nrow(hit), 1L)
      expect_match(hit$problem[[1]], spec$says, fixed = TRUE)
      expect_match(hit$action[[1]], "no computed results", fixed = TRUE)
    })
  })
}

# ---- every consumer of result_status ---------------------------------------

test_that("ars_manual_worklist() excludes blocked rows", {
  ## A blocked cell is not somebody's job. Listing it would send a programmer
  ## to derive a number that cannot be derived until the spec is fixed.
  td  <- .bs_adam()
  ard <- .bs_run(.bs_causes$missing_dataset$pop, td, name = "wl.json")

  expect_equal(.bs_status(ard, "blocked"), 1L)
  expect_equal(nrow(ars_manual_worklist(ard)), 0L)
})

test_that("manual-fill validation ignores blocked rows", {
  ## ars_validate_manual_fills() only ever looks at manual_filled, so a blocked
  ## row is invisible to it -- and a blocked row cannot be presented as a
  ## validated manual result without first being relabelled, which is exactly
  ## the audit trail the statuses exist for.
  td  <- .bs_adam()
  ard <- .bs_run(.bs_causes$missing_dataset$pop, td, name = "mf.json")

  expect_equal(nrow(ars_validate_manual_fills(ard)), 0L)

  ## Relabelled without a value or a derivation reference, it is reported --
  ## it does not quietly pass as filled.
  relabelled <- ard
  relabelled[["result_status"]] <- "manual_filled"
  flagged <- ars_validate_manual_fills(relabelled)
  expect_gt(nrow(flagged), 0L)
})

test_that("derived_dt is not stamped on a blocked row", {
  td  <- .bs_adam()
  ard <- .bs_run(.bs_causes$missing_dataset$pop, td, name = "dt.json")
  stamped <- as.character(ard[["derived_dt"]])
  expect_true(all(is.na(stamped)))
})

test_that("shell fill does not insert a blocked result as a value", {
  ## The resolver reports the cell as blocked rather than falling through to
  ## "no_value", which would render identically to a cell nobody asked for.
  index <- data.frame(status = "blocked", value = NA_real_,
                      stringsAsFactors = FALSE)
  expect_equal(.pending_status(c("blocked")), "blocked")
  expect_equal(.pending_status(c("computed", "blocked")), "blocked")
  ## Ordinary reserved work still reads as pending, not blocked.
  expect_equal(.pending_status(c("manual_pending")), "pending")
})

test_that("the DOCX placeholder explains a blocked output", {
  td  <- .bs_adam()
  ard <- .bs_run(.bs_causes$missing_subject_key$pop, td, name = "docx.json")

  detail <- .blocked_cells_detail(ard, "T_BLOCK")
  expect_false(is.null(detail))
  expect_match(detail, "missing_subject_key", fixed = TRUE)
  expect_match(detail, "ars_blockers()", fixed = TRUE)
  expect_match(detail, "cannot be derived by hand", fixed = TRUE)

  ## And it is reached through the detail the placeholder page prints.
  expect_match(.reserved_cells_detail(ard, "T_BLOCK"), "Blocked cell",
               fixed = TRUE)
  ## An output with no blocked rows says nothing about blocking.
  expect_null(.blocked_cells_detail(ard, "T_OTHER"))
})

test_that("a rendered table states that some of it was not computed", {
  ## Through ars_render_tlf(), so the note is asserted on real output rather
  ## than on the string that produces it.
  skip_if_not_installed("gt")
  td <- .bs_adam()
  fx <- .bs_mixed(td)

  expect_gt(.bs_status(fx$ard, "computed"), 0L)
  expect_equal(.bs_status(fx$ard, "blocked"), 1L)

  ## This fixture declares no display columns, so the renderer warns that it
  ## took the column order from the ARD -- expected here and not what is
  ## under test.
  gt_obj <- suppressMessages(suppressWarnings(
    ars_render_tlf(fx$path, fx$ard, "T_BLOCK")))
  notes <- unlist(gt_obj[["_source_notes"]])
  expect_true(any(grepl("could not be satisfied", notes, fixed = TRUE)))
  expect_true(any(grepl("ars_blockers()", notes, fixed = TRUE)))
})

test_that("the blocked row is analysis-level, which is why the note is too", {
  ## The limitation, pinned so it is a decision rather than a surprise: the
  ## filter never ran, so nothing decided which statistics the analysis would
  ## have produced and there is no blocked CELL to mark.
  td  <- .bs_adam()
  ard <- .bs_run(.bs_causes$missing_dataset$pop, td, name = "level.json")
  blocked <- as.character(ard[["result_status"]]) == "blocked"
  expect_true(all(is.na(as.character(ard[["stat_name"]])[blocked])))
})

# ---- neither path can produce a computed value for a blocked case -----------

test_that("executor and emitted code both refuse an ambiguous clause", {
  where <- .bs_causes$ambiguous_row_coherence$pop
  td    <- .bs_adam()
  store <- .adam_store(td)

  ## Executor: a block signal, never a mask.
  signal <- .where_keep_mask(store$get("ADAE"), "ADAE", where, store, "USUBJID")
  expect_true(.is_block(signal))

  ## Emitter: no code at all, so nothing downstream can evaluate to a number.
  expect_null(.apply_where_expr("ADAE", "ADAE", where, "USUBJID"))
})

test_that("a blocked run yields no computed row on either execution path", {
  td <- .bs_adam()
  for (legacy in c(FALSE, TRUE)) {
    ard <- .bs_run(.bs_causes$missing_dataset$pop, td,
                   name = paste0("path_", legacy, ".json"), legacy = legacy)
    expect_equal(.bs_status(ard, "computed"), 0L,
                 info = paste("legacy =", legacy))
    expect_equal(.bs_status(ard, "blocked"), 1L,
                 info = paste("legacy =", legacy))
  }
})

# ---- and a supported study is untouched -------------------------------------

test_that("a fully supported study produces zero blocked rows", {
  ## The gate on the new path: if blocking ever starts swallowing valid
  ## analyses, this is what says so.
  skip_if_not_installed("openxlsx2")
  skip_on_cran()

  adam_dir <- withr::local_tempdir()
  utils::unzip(arsbridge_example("ADaM.zip"), exdir = adam_dir)
  ars <- withr::local_tempfile(fileext = ".json")

  withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(suppressWarnings(spec_to_ars(
      shell_path     = arsbridge_example("annotated_shell.xlsx"),
      adam_spec_path = arsbridge_example("adam_spec.xlsx"),
      api_key = "", output_path = ars, study_id = "CDSC-ALZ-201",
      use_llm = FALSE, verbose = FALSE))))

  ard <- suppressMessages(suppressWarnings(ars_to_ard(ars, adam_dir)))
  expect_gt(.bs_status(ard, "computed"), 0L)
  expect_equal(.bs_status(ard, "blocked"), 0L)
})

# ---- negative caching -------------------------------------------------------

test_that("a missing dataset is reported once per run, not once per lookup", {
  ## Before this, `dfs[[name]] <- NULL` removed the element instead of storing
  ## one, so every lookup re-read the directory and re-reported. With blocking
  ## in place that would mean one FAIL per analysis naming the same absent
  ## dataset -- noisiest exactly when it matters most.
  diag_reset()
  store <- .adam_store(.bs_adam())

  for (i in 1:4) expect_null(suppressWarnings(store$get("ADXX")))

  hit <- ars_diagnostics()
  hit <- hit[grepl("ADXX", hit$problem), , drop = FALSE]
  expect_equal(nrow(hit), 1L)
})

test_that("caching keys on the canonical identity resolution uses", {
  ## ADSL / adsl / AdSl are one dataset, so they are one diagnostic. Resolution
  ## already upper-cases the name; the cache uses the same key rather than a
  ## second notion of identity that could drift from it.
  diag_reset()
  store <- .adam_store(.bs_adam())

  for (spelling in c("ADXX", "adxx", "AdXx")) {
    expect_null(suppressWarnings(store$get(spelling)))
  }
  hit <- ars_diagnostics()
  expect_equal(nrow(hit[grepl("ADXX", hit$problem), , drop = FALSE]), 1L)
})

test_that("genuinely different missing datasets are each reported", {
  ## The failure mode on the other side: suppressing too much.
  diag_reset()
  store <- .adam_store(.bs_adam())

  expect_null(suppressWarnings(store$get("ADXX")))
  expect_null(suppressWarnings(store$get("ADYY")))
  expect_null(suppressWarnings(store$get("ADXX")))

  hit <- ars_diagnostics()
  expect_equal(nrow(hit[grepl("ADXX", hit$problem), , drop = FALSE]), 1L)
  expect_equal(nrow(hit[grepl("ADYY", hit$problem), , drop = FALSE]), 1L)
})

test_that("a dataset that IS there is still returned on every lookup", {
  store <- .adam_store(.bs_adam())
  first  <- store$get("ADSL")
  second <- store$get("adsl")
  expect_s3_class(first, "data.frame")
  expect_equal(first, second)
})
