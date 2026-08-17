## The validation-gate panel must render on the runs that have something to
## report.
##
## General defect class: a value the panel's heading reads was computed only
## inside a branch that runs when there is NOTHING to list. The panel therefore
## worked on the empty case -- the one a quick check exercises -- and raised
## "object 'refs' not found" on the case it exists for.
##
## Since a defect now reserves a result rather than refusing the run, a run with
## findings is the ordinary outcome, not the exception. The error surfaced under
## a green "Done" badge: the run had genuinely completed, and only the account
## of it crashed, taking with it the list of which results were reserved -- the
## one thing the reader opens that panel for.
##
## General invariant: whatever the heading reads is computed before the
## branches, never inside one of them. Every gate shape a payload can carry
## renders.

.wsu_findings <- function(n) {
  data.frame(
    severity = rep("GAP", n),
    ref      = rep(c("FIXED_GROUPING_EMPTY", "ANALYSIS_CONDITION_UNRESOLVED"),
                   length.out = n),
    entity   = rep("analyses", n),
    id       = if (n == 0) character() else sprintf("AN_%02d", seq_len(n)),
    field    = rep("methodId", n),
    problem  = rep("This analysis will not be computed.", n),
    action   = rep("Repair the annotation and re-run.", n),
    stringsAsFactors = FALSE
  )
}

## A payload from a current run: gaps, nothing refused. `status` is what
## `.workflow_payload_has_gaps()` keys on, so the panel is reached at all.
.wsu_payload <- function(findings, refs = c("FIXED_GROUPING_EMPTY",
                                            "ANALYSIS_CONDITION_UNRESOLVED")) {
  list(validation_gate = list(
    verdict           = "COMPLETED WITH GAPS",
    blocked           = FALSE,
    status            = "completed-with-gaps",
    gap_refs          = refs,
    blocking_refs     = refs,
    blocking_findings = findings,
    summary           = ""
  ))
}

.wsu_render <- function(payload) {
  as.character(.workflow_validation_gate_ui(payload))
}


test_that("the gate panel renders whether or not the run carries findings", {
  ## The regression. With findings present the heading used to read a value
  ## only the no-findings branch defined; both directions are asserted, so a
  ## fix that merely swapped which branch fails would not pass.
  for (n in c(0L, 1L, 2L, 5L)) {
    html <- expect_no_error(.wsu_render(.wsu_payload(.wsu_findings(n))))
    expect_true(grepl("COMPLETED WITH GAPS", html, fixed = TRUE),
                info = paste("n =", n))
  }
})


test_that("the panel counts the results it actually lists", {
  one <- .wsu_render(.wsu_payload(.wsu_findings(1L),
                                  refs = "FIXED_GROUPING_EMPTY"))
  expect_true(grepl("1 result reserved", one, fixed = TRUE))
  expect_false(grepl("1 results reserved", one, fixed = TRUE))

  many <- .wsu_render(.wsu_payload(.wsu_findings(5L)))
  expect_true(grepl("5 results reserved", many, fixed = TRUE))

  ## Non-vacuity: two different renderings, not the same string twice.
  expect_false(identical(one, many))
})


test_that("the panel names the results the run reserved", {
  ## What the error was costing the reader: the panel exists to say WHICH
  ## results were reserved, and when it raised instead, that list was lost even
  ## though the run had completed and the information was in hand.
  html <- .wsu_render(.wsu_payload(.wsu_findings(2L)))
  expect_true(grepl("FIXED_GROUPING_EMPTY", html, fixed = TRUE))
  expect_true(grepl("ANALYSIS_CONDITION_UNRESOLVED", html, fixed = TRUE))

  ## With no findings frame at all, the refs the gate carried are listed
  ## instead, so the panel still names something.
  fallback <- .wsu_render(.wsu_payload(.wsu_findings(0L)))
  expect_true(grepl("FIXED_GROUPING_EMPTY", fallback, fixed = TRUE))
})


test_that("a gate carrying neither findings nor refs still renders", {
  ## The degenerate shape. It must not error, and must not claim results were
  ## reserved. `status` still marks it as a gap run, which is what reaches the
  ## panel in the first place.
  payload <- .wsu_payload(.wsu_findings(0L), refs = character())
  html <- expect_no_error(.wsu_render(payload))
  expect_true(grepl("0 results reserved", html, fixed = TRUE))
})


test_that("a payload with no gaps at all shows no gate panel", {
  ## The guard that keeps a clean run quiet -- asserted so the tests above are
  ## known to be exercising the gap path rather than a panel that always draws.
  clean <- list(validation_gate = list(verdict = "DONE", blocked = FALSE,
                                       status = "ready", gap_refs = character()))
  expect_null(.workflow_validation_gate_ui(clean))
})
