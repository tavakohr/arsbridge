## Findings say GAP; diagnostics say FAIL; the bridge between them translates.
##
## FAIL used to mean two different things in one word. As a FINDING it meant
## "this reporting event is broken and the run is refused". As a DIAGNOSTIC it
## meant "something went wrong during this run". Sharing the word is why one
## unresolvable grouping refused ten sound outputs: everything that read FAIL
## read both statements as the same statement.
##
## So the findings channel gets its own word. GAP means a specific result will
## not be produced and the run has reserved it -- which is a fact about one
## cell, not a verdict on the event. Diagnostics keep FAIL | WARN | INFO
## unchanged, because `ars_blockers()` is an exported contract and a run-level
## failure really is a run-level failure.
##
## Two things have to hold for that split to be safe, and both are load-bearing:
## no check may raise FAIL any more, and every GAP must still arrive in the
## diagnostics channel under the word that channel counts. A GAP that crossed
## the bridge untranslated would empty `ars_blockers()` -- the most serious
## findings arriving under a word no consumer looks for.


test_that("no check raises FAIL, and the vocabulary is closed", {
  ## Over a seeded event with a real defect, so this is the validator's actual
  ## output rather than an empty frame agreeing with everything.
  model <- .rmap_model()
  model$analyses$analysisSetId[model$analyses$id == "AN_SYNTH_001"] <- "AS_GONE"
  findings <- validate_ars_model(model)

  expect_gt(nrow(findings), 0L)
  expect_true(all(findings$severity %in% .FINDING_SEVERITIES))
  expect_false("FAIL" %in% findings$severity)
  expect_true("GAP" %in% findings$severity)
})


test_that("a check cannot raise a severity outside the vocabulary", {
  ## The same closed-vocabulary argument as `ref`: everything downstream
  ## branches on severity, so a word nothing counts is a finding nothing
  ## reports. FAIL is refused by name because it is the one wrong answer
  ## somebody would plausibly write.
  expect_error(
    .add_finding(.new_findings(), "FAIL", "analyses", "AN_SYNTH_001",
                 "methodId", "problem", "action",
                 ref = "METHOD_NOT_ASSIGNED"),
    "not one a check may raise"
  )
  expect_error(
    .add_finding(.new_findings(), "BLOCKER", "analyses", "AN_SYNTH_001",
                 "methodId", "problem", "action",
                 ref = "METHOD_NOT_ASSIGNED"),
    "not one a check may raise"
  )
})


test_that("a GAP reaches the diagnostics channel as a FAIL", {
  ## The bridge. `ars_blockers()` promises FAIL diagnostics; if the translation
  ## were a copy, every structural gap would arrive as GAP and be counted by
  ## nothing.
  model <- .rmap_model()
  model$analyses$methodId[model$analyses$id == "AN_SYNTH_001"] <- "MTH_GONE"
  gate <- .validation_gate(validate_ars_model(model))

  expect_true("GAP" %in% gate$findings$severity)

  diagnostics <- .validation_gate_diagnostics(gate)
  expect_gt(nrow(diagnostics), 0L)
  expect_true("FAIL" %in% diagnostics$severity)
  ## And the findings' own word does not leak into a channel that does not
  ## know it.
  expect_false("GAP" %in% diagnostics$severity)
  expect_true(all(diagnostics$severity %in% c("FAIL", "WARN", "INFO")))
})


test_that("the gate collects gaps under both the new name and the old", {
  model <- .rmap_model()
  model$analyses$methodId[model$analyses$id == "AN_SYNTH_001"] <- "MTH_GONE"
  gate <- .validation_gate(validate_ars_model(model))

  expect_gt(nrow(gate$gaps), 0L)
  expect_true(length(gate$gap_refs) > 0L)
  ## `blocking_findings` and `blocking_refs` are the same rows under the names
  ## several callers and every archived payload already read.
  expect_identical(gate$gaps, gate$blocking_findings)
  expect_identical(gate$gap_refs, gate$blocking_refs)
})


test_that("a report written before this release still reads correctly", {
  ## An archived `validation_report.xlsx` carries FAIL where a report written
  ## now carries GAP. It is a regulated deliverable that outlives the release
  ## that produced it, so every consumer has to keep understanding it -- which
  ## is the whole reason GAP is a new word rather than a redefinition of FAIL.
  archived <- .new_findings()[1, , drop = FALSE]
  archived$severity <- "FAIL"
  archived$entity   <- "groupings"
  archived$id       <- "GF_SYNTH"
  archived$field    <- "groups"
  archived$problem  <- "This fixed grouping has no groups."
  archived$action   <- "Add groups or make it data-driven."
  archived$ref      <- "FIXED_GROUPING_EMPTY"

  gate <- .validation_gate(archived)
  expect_equal(nrow(gate$gaps), 1L)
  expect_true(gate$blocked)

  ## It sorts ahead of everything, rather than falling to the bottom as an
  ## unranked severity would.
  expect_equal(.worst_severity(c("INFO", "WARN", "FAIL")), "FAIL")
  expect_equal(.FINDING_SEVERITY_RANK[[1]], "FAIL")

  ## What an archived payload is NOT is an input to the reservation map. The
  ## map is always built from a fresh `validate_ars_model()` call on the event
  ## about to run -- see `.spec_reservations()` -- because reserving is a
  ## decision about THIS run, and a finding frame written months ago describes
  ## an event that may since have been repaired. Reserving from it would
  ## withhold results the current event computes correctly.
  ##
  ## So the archived row is a display artifact: it renders, it sorts, it counts
  ## as a gap. Re-validating the same broken event is what reserves.
  broken <- .rmap_model()
  i <- match("GF_SYNTH", broken$groupings$id)
  broken$groupings$dataDriven[i] <- FALSE
  broken$groupings$raw[[i]]$dataDriven <- FALSE
  broken$groupings$n_groups[i] <- 0L

  fresh <- validate_ars_model(broken)
  expect_true("FIXED_GROUPING_EMPTY" %in% fresh$ref)
  reserved <- names(
    .reservations_from_findings(broken, fresh)$by_analysis
  )
  expect_true("AN_SYNTH_001" %in% reserved)
})


test_that("GAP has its own tint, off the pass/warn/fail axis", {
  ## The legend and the tinting read one palette, so a colour cannot mean one
  ## thing in the sheet and another in the key.
  expect_true("GAP" %in% names(.REPORT_STATUS_FILL))
  expect_false(.REPORT_STATUS_FILL[["GAP"]] %in%
                 .REPORT_STATUS_FILL[c("PASS", "WARN", "FAIL", "INFO")])
})
