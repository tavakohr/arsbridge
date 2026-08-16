## One reserved analysis, three deliverables, one answer.
##
## The reservation map is only worth having if everything downstream agrees
## with it. arsbridge produces three things from one reporting event -- the
## ARD, a generated `.R` program per output, and the filled workbook -- and
## they are built by three different code paths that each used to be protected
## by the same catch-all refusal. Take the refusal away and they can disagree,
## which is worse than either refusing or computing: the workbook would show an
## empty cell, the script would show a calculation for it, and a reviewer
## reconciling the two would trust the one with a number in it.
##
## So the invariant is stated across all three at once:
##
##   a structurally reserved analysis is reserved in the ARD, in the generated
##   program and in the workbook -- and an analysis nothing is wrong with
##   computes in all three.
##
## Both halves are load-bearing. A build that reserved everything would satisfy
## the first half completely while destroying the point of the change.
##
## The damage is injected into the ARS rather than into the shell, because that
## is where the wrong-number risk actually lives: a dangling `analysisSetId`
## resolves to no population, and an analysis that computes over "no
## population" computes over the WHOLE dataset. Every percentage lands over the
## wrong denominator, and it fills, and it formats, and nothing about the
## output looks wrong.

skip_if_not_installed("openxlsx2")

## Two disjoint invented ADaM vocabularies. Every assertion runs under both, so
## a path that quietly recognised a familiar dataset or variable name would
## pass under one and fail under the other.
.E2E_VOCABS <- list(
  A = list(ds = "ADQX", cat = "QXCAT", pop = "QXFL", arm = "QXARM",
           sheet = "Table 90.1.1"),
  B = list(ds = "ADVN", cat = "VNGRP", pop = "VNFL", arm = "VNTRT",
           sheet = "Table 91.2.2")
)

## The ADaM spec the annotations are gated against.
.e2e_spec <- function(vocab, dir) {
  vars <- data.frame(
    Dataset  = c("ADSL", "ADSL", "ADSL", vocab$ds, vocab$ds, vocab$ds,
                 vocab$ds),
    Variable = c("USUBJID", vocab$arm, vocab$pop,
                 "USUBJID", vocab$arm, vocab$pop, vocab$cat),
    Label    = c("Subject", "Treatment", "Population Flag",
                 "Subject", "Treatment", "Population Flag", "Category"),
    Type     = "Char", Origin = "Derived", Codelist = "", Length = "40",
    Mandatory = "Req", stringsAsFactors = FALSE
  )
  path <- file.path(dir, "spec.xlsx")
  wb <- openxlsx2::wb_workbook() |>
    openxlsx2::wb_add_worksheet("Variables") |>
    openxlsx2::wb_add_data(sheet = "Variables", x = vars)
  openxlsx2::wb_save(wb, path, overwrite = TRUE)
  path
}

## Subject-level data both analyses can be computed from.
.e2e_adam <- function(vocab, dir) {
  adam <- file.path(dir, "adam")
  dir.create(adam, showWarnings = FALSE)
  subjects <- data.frame(
    USUBJID = sprintf("S%02d", 1:8),
    ARM     = rep(c("Drug A", "Placebo"), each = 4),
    FLAG    = "Y",
    CAT     = c("P", "P", "Q", "Q", "P", "R", "R", "Q"),
    stringsAsFactors = FALSE
  )
  names(subjects) <- c("USUBJID", vocab$arm, vocab$pop, vocab$cat)
  utils::write.csv(subjects, file.path(adam, paste0(vocab$ds, ".csv")),
                   row.names = FALSE)
  ## The subject-level dataset the denominator joins from. A computing analysis
  ## needs it; a reserved one never reads any data at all, which is part of
  ## what "reserved" means.
  utils::write.csv(subjects, file.path(adam, "ADSL.csv"), row.names = FALSE)
  adam
}

## A two-row shell: one row that will be damaged, one that must keep computing.
.e2e_shell <- function(vocab, dir) {
  wb <- openxlsx2::wb_workbook()$add_worksheet(vocab$sheet)
  put <- function(x, row, col = 1L) {
    wb$add_data(sheet = vocab$sheet, x = x, start_row = row, start_col = col,
                col_names = FALSE)
  }
  black <- openxlsx2::wb_color(hex = "FF000000")
  red   <- openxlsx2::wb_color(hex = "FFC00000")
  ann <- function(label, annotation) {
    openxlsx2::fmt_txt(label, color = black, size = 10) +
      openxlsx2::fmt_txt(paste0("\n", annotation), color = red, size = 8,
                         italic = TRUE)
  }

  put(vocab$sheet, 1)
  put("Reserved-analysis regression", 2)
  put(ann("Analysed Population ",
          sprintf("(%s.%s='Y')", vocab$ds, vocab$pop)), 3)
  for (r in 1:3) {
    wb$merge_cells(sheet = vocab$sheet, dims = sprintf("A%d:C%d", r, r))
  }

  put(ann("Item", sprintf("[columns -> %s.%s; source %s]",
                          vocab$ds, vocab$arm, vocab$ds)), 4)
  put("Drug A", 4, 2L)
  put("Placebo", 4, 3L)

  ## Row 5 is the one the damage is aimed at; row 6 is the control.
  ##
  ## Both are single-number subject counts on purpose. A categorical row would
  ## stand for a repeated block and stay pending for row-expansion reasons that
  ## have nothing to do with reservations -- which would make the control fail
  ## for the wrong reason and quietly stop being a control.
  put(ann("Subjects in population, n", sprintf("[%s.USUBJID]", vocab$ds)), 5)
  for (j in 2:3) put("xx", 5, j)
  put(ann("Subjects with a record, n", sprintf("[%s.USUBJID]", vocab$ds)), 6)
  for (j in 2:3) put("xx", 6, j)

  path <- file.path(dir, "shell.xlsx")
  wb$save(path)
  path
}

## Two ways one analysis can stop resolving, and the code each must raise.
##
## Both are wrong-number risks and they fail differently, which is why both are
## here. A dangling population makes the analysis compute over the WHOLE
## dataset -- every percentage over the wrong denominator. An unresolved
## variable role makes it compute as an ordinary ANALYSIS variable when the
## annotation said the variable meant something else -- a number that is right
## for a question nobody asked.
.E2E_DAMAGES <- list(
  list(
    name = "a population that is not in the event",
    ref  = "ANALYSIS_SET_REF_UNRESOLVED",
    apply = function(analysis) {
      analysis$analysisSetId <- "AS_NOT_IN_THIS_EVENT"
      analysis
    }
  ),
  list(
    name = "a variable role that never resolved",
    ref  = "UNRESOLVED_VARIABLE_ROLE",
    apply = function(analysis) {
      analysis$variableRole <- "ANALYSIS"
      analysis$unresolvedVariableRole <- list("GROUPING")
      analysis
    }
  ),
  list(
    ## A row filter the author wrote and this grammar could not read. The
    ## analysis carries no data subset as a result, so without the marker it is
    ## indistinguishable from a row that never asked to be filtered -- and it
    ## would compute over every record while looking entirely ordinary.
    ##
    ## The text is a bare ">=", which the grammar spells GE. It carries
    ## operator evidence, so it reserves; text with a qualified reference and
    ## no operator does not, and that distinction has its own tests.
    name = "a row filter that could not be read",
    ref  = "ANALYSIS_CONDITION_UNRESOLVED",
    apply = function(analysis) {
      analysis$dataSubsetId <- NULL
      analysis$unresolvedCondition <- "ADQX.SCORE >= 16"
      analysis
    }
  )
)

## Damage ONE analysis and say which one, leaving the rest of the event intact.
##
## Chosen by position rather than by label. The invariant under test is "the
## damaged analysis is reserved everywhere and the others still compute", and
## which shell row it happens to be does not enter into that -- while matching
## on a label would tie the test to how ids and names are generated, which is
## exactly the kind of coupling the rest of this file avoids.
.e2e_damage <- function(source_path, target_path, damage) {
  ars <- jsonlite::fromJSON(source_path, simplifyVector = FALSE)
  if (length(ars$analyses) < 2L) {
    stop("the fixture needs at least two analyses: one damaged, one control")
  }
  ars$analyses[[1]] <- damage$apply(ars$analyses[[1]])
  writeLines(
    jsonlite::toJSON(ars, auto_unbox = TRUE, pretty = TRUE, null = "null"),
    target_path
  )
  ars$analyses[[1]]$id
}

## The generated block for one analysis: from its comment line to the next
## blank-line boundary. Blocks are joined with a blank line, so this is the
## text that belongs to that analysis and nothing else.
.e2e_block_for <- function(script, analysis_id) {
  blocks <- strsplit(script, "\n\n", fixed = TRUE)[[1]]
  hit <- grep(paste0("(analysis ", analysis_id, ")"), blocks, fixed = TRUE)
  if (length(hit) == 0) return(NA_character_)
  blocks[[hit[[1]]]]
}


test_that("a reserved analysis is reserved in the ARD, the code and the workbook", {
  checked <- 0L

  for (vname in names(.E2E_VOCABS)) {
    vocab <- .E2E_VOCABS[[vname]]
    td <- withr::local_tempdir()

    spec  <- .e2e_spec(vocab, td)
    adam  <- .e2e_adam(vocab, td)
    shell <- .e2e_shell(vocab, td)
    pristine <- file.path(td, "re.json")

    ## Built once per vocabulary and copied per damage: generating the event is
    ## the slow half, and every damage starts from the same sound event anyway.
    withr::with_envvar(
      c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
        GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
      suppressMessages(suppressWarnings(spec_to_ars(
        shell_path = shell, adam_spec_path = spec, api_key = "",
        output_path = pristine, report_path = file.path(td, "rep.xlsx"),
        verbose = FALSE))))

    for (damage in .E2E_DAMAGES) {
    nm <- paste(vname, "/", damage$name)
    ars <- file.path(td, paste0("re_", make.names(damage$ref), ".json"))
    reserved_id <- .e2e_damage(pristine, ars, damage)
    expect_false(is.null(reserved_id), info = nm)

    ## The control: every other analysis the event carries.
    model <- ars_to_model(ars)
    sound_ids <- setdiff(model$analyses$id, reserved_id)
    expect_gt(length(sound_ids), 0L)

    ## The damage really is the defect this test is about, and not some other
    ## one. Without this the assertions below could all pass on an event that
    ## broke in a way nobody intended.
    findings <- validate_ars_model(model)
    expect_true(damage$ref %in% findings$ref, info = nm)

    ## ---- 1. the ARD --------------------------------------------------------
    ard <- as.data.frame(suppressMessages(suppressWarnings(
      ars_to_ard(ars, adam))))

    reserved_rows <- ard[ard$analysis_id %in% reserved_id, , drop = FALSE]
    expect_gt(nrow(reserved_rows), 0L)
    expect_false(any(reserved_rows$result_status %in% "computed"), info = nm)
    expect_true(all(is.na(reserved_rows$stat)), info = nm)
    ## Reserved for THIS reason, joinable back to the finding that caused it.
    expect_true(
      any(grepl(paste0("^structural:", damage$ref),
                reserved_rows$block_reason %||% character(0))),
      info = nm
    )

    sound_rows <- ard[ard$analysis_id %in% sound_ids, , drop = FALSE]
    expect_true(any(sound_rows$result_status %in% "computed"), info = nm)
    expect_true(any(!is.na(sound_rows$stat)), info = nm)

    ## ---- 2. the generated program -----------------------------------------
    code_dir <- file.path(td, "code")
    suppressMessages(suppressWarnings(
      write_tlf_code(ars, code_dir, adam_dir = adam)))
    scripts <- list.files(code_dir, pattern = "\\.R$", full.names = TRUE)
    expect_gt(length(scripts), 0L)
    script <- paste(readLines(scripts[[1]], warn = FALSE), collapse = "\n")

    reserved_block <- .e2e_block_for(script, reserved_id)
    expect_false(is.na(reserved_block), info = nm)
    expect_match(reserved_block, "Reserved")
    expect_match(reserved_block, damage$ref)
    ## The whole point: no substitute calculation in a signed deliverable.
    expect_false(grepl("cards::ard_", reserved_block, fixed = TRUE), info = nm)

    ## And the sound analysis still has its calculation.
    sound_blocks <- vapply(sound_ids, function(id) {
      block <- .e2e_block_for(script, id)
      !is.na(block) && grepl("cards::ard_", block, fixed = TRUE)
    }, logical(1))
    expect_true(any(sound_blocks), info = nm)

    ## ---- 3. the filled workbook -------------------------------------------
    fill <- suppressMessages(suppressWarnings(ars_fill_shell(
      shell_path = shell, ars = ars, ard = ard,
      output_path = file.path(td, "filled.xlsx"), adam_dir = adam,
      overwrite = TRUE)))
    census <- fill$census

    reserved_cells <- census[census$analysis_id %in% reserved_id, ,
                             drop = FALSE]
    expect_gt(nrow(reserved_cells), 0L)
    expect_false(any(reserved_cells$status %in% "filled"), info = nm)
    ## The workbook says what the ARD said, in the words a reviewer needs --
    ## not "no result in the ARD", which is what a cell nobody asked about
    ## gets.
    expect_true(
      any(grepl("reserved", reserved_cells$reason, ignore.case = TRUE)),
      info = nm
    )
    expect_true(
      any(grepl(damage$ref, reserved_cells$reason)),
      info = nm
    )

    sound_cells <- census[census$analysis_id %in% sound_ids, , drop = FALSE]
    expect_true(any(sound_cells$status %in% "filled"), info = nm)

    checked <- checked + 1L
    }
  }

  ## Non-vacuity: a shell the parser stopped reading, or an event that produced
  ## no analyses, would leave every loop body unexecuted.
  expect_equal(checked, length(.E2E_VOCABS) * length(.E2E_DAMAGES))
})
