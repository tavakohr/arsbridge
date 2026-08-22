## The terminal declaration states, and the trap that proves they must be
## terminal.
##
## A terminal state carries two obligations, and only the first is obvious:
## halt evidence precedence, AND never reach the legacy filter bridge. This
## file exists for the second. It builds a shell in which everything except
## the header agrees that the row is a count -- the row's own restriction pins
## its primary variable, and its placeholder shows an integer beside a decimal
## -- so that a fall-through does not merely produce SOME method, it produces a
## thoroughly plausible one. A wrong answer nobody would question is the only
## kind worth building a fixture against.
##
## PR5b-3a state: the reader is not authoritative, so the bridge still wins and
## this file PINS that, so the change in 3b is visible as a diff rather than as
## a claim. The block at the end says exactly which assertion flips.

skip_if_not_installed("openxlsx2")
skip_if_not_installed("withr")

## The whole fixture, invented: a dataset and variables unrelated to any
## fixture or study in this repo, and a grouping vocabulary that appears
## nowhere else.
TRAP_SHEET <- "Table 14.9.1"
TRAP_DS    <- "ADQX"
TRAP_VAR   <- "QXSEV"
TRAP_LEVEL <- "SEVERE"
TRAP_ANNOT <- sprintf("%s.%s='%s'", TRAP_DS, TRAP_VAR, TRAP_LEVEL)
## Read by the grammar, refused by the line boundary's admitted set. The
## contradiction with a subject count is total: a mean is not a tally.
TRAP_FORM  <- "Mean (SD)"
TRAP_STUB  <- paste0("Zeta Grouping\n", TRAP_FORM)

trap_spec <- function(td) {
  vars <- data.frame(
    Dataset   = rep(TRAP_DS, 4),
    Variable  = c("USUBJID", "TRTA", TRAP_VAR, "QXVAL"),
    Label     = c("Unique Subject Identifier", "Actual Treatment",
                  "Finding Severity", "Analysis Value"),
    Type      = c("Char", "Char", "Char", "Num"),
    Origin    = c("Assigned", "Derived", "Derived", "Derived"),
    Codelist  = c("", "", "QXSEVCD", ""),
    Length    = c("40", "20", "10", "8"),
    Mandatory = rep("Req", 4),
    stringsAsFactors = FALSE
  )
  codes <- data.frame(
    Codelist = rep("QXSEVCD", 3),
    Code     = c("MILD", "MODERATE", TRAP_LEVEL),
    Decode   = c("Mild", "Moderate", "Severe"),
    stringsAsFactors = FALSE
  )
  path <- file.path(td, "trap_spec.xlsx")
  wb <- openxlsx2::wb_workbook()$add_worksheet("Variables")$
    add_data(sheet = "Variables", x = vars)$add_worksheet("CODELISTS")$
    add_data(sheet = "CODELISTS", x = codes)
  openxlsx2::wb_save(wb, file = path, overwrite = TRUE)
  path
}

trap_shell <- function(td) {
  wb <- openxlsx2::wb_workbook()$add_worksheet(TRAP_SHEET)
  put <- function(x, row, col = 1L) {
    wb$add_data(sheet = TRAP_SHEET, x = x, start_row = row, start_col = col,
                col_names = FALSE)
  }
  ann <- function(label, annotation) {
    openxlsx2::fmt_txt(label, color = openxlsx2::wb_color(hex = "FF000000"),
                       size = 10) +
      openxlsx2::fmt_txt(paste0("\n", annotation),
                         color = openxlsx2::wb_color(hex = "FFC00000"),
                         size = 8, italic = TRUE)
  }
  put("Table 14.9.1", 1)
  put("Findings by Severity", 2)
  put(ann("Analysis Population ", sprintf("(%s.QXFL='Y')", TRAP_DS)), 3)
  for (r in 1:3) {
    wb$merge_cells(sheet = TRAP_SHEET, dims = sprintf("A%d:C%d", r, r))
  }
  ## The header states the form on a line of its own -- the boundary PR5b-3a
  ## learned to read, carrying a form it does not admit.
  put(ann(TRAP_STUB, sprintf("[columns -> %s.TRTA; source %s]",
                             TRAP_DS, TRAP_DS)), 4)
  put("Drug A", 4, 2L)
  put("Placebo", 4, 3L)
  ## The bait: a restriction that pins the row's own primary variable, under a
  ## placeholder showing a count beside a percentage.
  put(ann("Severe finding", TRAP_ANNOT), 5)
  put("xx (xx.x)", 5, 2L)
  put("xx (xx.x)", 5, 3L)
  path <- file.path(td, "trap_shell.xlsx")
  openxlsx2::wb_save(wb, file = path, overwrite = TRUE)
  path
}

trap_run <- function() {
  td <- withr::local_tempdir(.local_envir = parent.frame())
  shell <- trap_shell(td)
  spec  <- trap_spec(td)
  out   <- file.path(td, "trap.json")
  withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(suppressWarnings(spec_to_ars(
      shell_path = shell, adam_spec_path = spec, api_key = "",
      output_path = out, use_llm = FALSE, verbose = FALSE))))
  list(shell = shell, ars = jsonlite::fromJSON(out, simplifyVector = FALSE))
}

## ---------------------------------------------------------------------------
## The trap is a trap
## ---------------------------------------------------------------------------

test_that("the header states a form the line boundary does not admit", {
  r <- .caption_declares_statistic(TRAP_STUB)
  expect_identical(r$status, "stated_not_admitted")
  expect_null(r$stats)

  ## Read, not refused: the grammar understands the form perfectly well. That
  ## is what makes this a POLICY outcome rather than a reading failure, and
  ## why it must not be handed to an interpretation queue.
  expect_gt(length(.line_states_form(TRAP_FORM)), 0L)
  expect_null(.header_line_stats(TRAP_STUB))
})

test_that("that state is terminal and names its own diagnostic", {
  expect_true(.declaration_is_terminal("stated_not_admitted"))
  expect_identical(.declaration_diagnostic("stated_not_admitted"),
                   .STATISTIC_DECLARATION_SOURCE_NOT_ADMITTED)
  ## Never confused with the reading failure, which may go to a reader; and
  ## never with the capability claim, which shape cannot support.
  expect_false(identical(.declaration_diagnostic("stated_not_admitted"),
                         .STATISTIC_DECLARATION_UNRESOLVED))
  expect_false(identical(.declaration_diagnostic("stated_not_admitted"),
                         .STATISTIC_UNSUPPORTED))
})

test_that("the bait would be taken: the row's filter pins its own variable", {
  ## Without this the fixture proves nothing -- a fall-through that produced
  ## no method would look like correct refusal.
  where <- parse_where_clause(TRAP_ANNOT)
  expect_true(.filter_pins_primary(where, TRAP_DS, TRAP_VAR))
})

test_that("today the bridge wins, and it wins plausibly", {
  ## PINNED CURRENT BEHAVIOUR, not desired behaviour. The header says the rows
  ## are a mean and a standard deviation; the row is emitted as a subject count
  ## with a percentage, because the restriction pinned the primary variable and
  ## the declaration reader is not yet authoritative.
  run <- trap_run()
  analyses <- run$ars$analyses %||% list()

  ## Scope assertion: one row in, one analysis out. A fixture the parser
  ## stopped understanding would otherwise pass by emitting nothing.
  expect_identical(length(analyses), 1L)
  expect_identical(analyses[[1]]$label, "Severe finding")
  expect_identical(analyses[[1]]$methodId, "MTH_SUBJECT_COUNT_PCT")
})

## ---------------------------------------------------------------------------
## What PR5b-3b changes here
## ---------------------------------------------------------------------------
##
## The last test above flips. Once the declaration reader is authoritative:
##
##   * the row reserves, carrying STATISTIC_DECLARATION_SOURCE_NOT_ADMITTED;
##   * no method is selected for it at all;
##   * `.filter_pins_primary()` is never consulted for this row, which is the
##     second half of terminality and the half a "does it reserve?" assertion
##     does not by itself prove -- 3b asserts the bridge is not reached, not
##     merely that the outcome differs.
##
## The production mutation belongs there too: restore the fall-through and this
## file must go red. It cannot be written here, because there is no terminality
## in production yet to remove. What CAN be mutated today is the schema, and
## that mutation is run in the PR5b-3a1 evidence script: dropping
## `stated_not_admitted` from `.TERMINAL_DECLARATION_STATUSES` turns the second
## test above red.
