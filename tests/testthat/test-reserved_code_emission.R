## arsbridge never emits a calculation for a method it has declared it cannot
## compute.
##
## The defect class: the code emitter routed any method it had no dedicated
## idiom for to a generic arm, which substituted a categorical n(%). Applied to
## a method that declares it computes NOTHING, that produced a number the
## reporting event never asked for -- in a file whose whole purpose is to be a
## reproducible record of what was computed. A substitute calculation there is
## worse than a missing one: it is signed, re-runnable, and looks deliberate.
##
## The invariant: an analysis whose method declares it produces no result
## contributes a derivation note and no bound object. Nothing it "produces" can
## reach the ARD, and no calculation stands in for it.

.rce_write <- function(event) {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "code")
  write_tlf_code(event, path)
  files <- list.files(path, pattern = "\\.R$", full.names = TRUE)
  expect_gt(length(files), 0L)
  paste(readLines(files[[1]], warn = FALSE), collapse = "\n")
}

test_that("a reserved analysis emits a note, never a calculation", {
  checked <- 0L

  for (nm in names(.RSV_VOCABS)) {
    event <- .rsv_event(vocab = .RSV_VOCABS[[nm]],
                        method = .rsv_reserved_method())
    script <- .rce_write(event)
    checked <- checked + 1L

    ## The substitute calculation the generic arm used to write.
    expect_false(grepl("ard_categorical", script, fixed = TRUE), info = nm)
    expect_false(grepl("cards::ard_", script, fixed = TRUE), info = nm)

    ## What it says instead.
    expect_match(script, "Reserved", fixed = TRUE)
    expect_match(script, "ars_manual_worklist", fixed = TRUE)

    ## Nothing is bound, so nothing can reach the ARD.
    expect_false(grepl("bind_ard", script, fixed = TRUE), info = nm)
  }

  ## Non-vacuity: an event the emitter could not read would write no script
  ## and every expectation above would be untested.
  expect_equal(checked, 2L)
})

test_that("the emitted script still parses", {
  ## A commented-out derivation note is only safe if it is actually commented.
  for (nm in names(.RSV_VOCABS)) {
    script <- .rce_write(.rsv_event(vocab = .RSV_VOCABS[[nm]],
                                    method = .rsv_reserved_method()))
    expect_silent(parse(text = script))
  }
})

test_that("a computing method on the same shape still emits its calculation", {
  ## The control. Without it, deleting the emitter entirely would pass the
  ## tests above.
  for (nm in names(.RSV_VOCABS)) {
    script <- .rce_write(.rsv_event(vocab = .RSV_VOCABS[[nm]],
                                    method = .rsv_counting_method(),
                                    n_slots = 1L))

    expect_match(script, "cards::ard_", fixed = TRUE, info = nm)
    expect_match(script, "bind_ard", fixed = TRUE, info = nm)
    expect_false(grepl("Reserved:", script, fixed = TRUE), info = nm)
  }
})

test_that("the reservation reads the declaration, not the method id", {
  ## An id arsbridge has never seen, declared as computing nothing. A fix that
  ## matched on MTH_UNSUPPORTED_ANALYSIS would emit a substitute calculation
  ## here -- which is wrong-number risk 4 in the plan.
  foreign <- .rsv_reserved_method(id = "MTH_SOMEONE_ELSES_RESERVATION")
  script  <- .rce_write(.rsv_event(method = foreign))

  expect_false(grepl("ard_categorical", script, fixed = TRUE))
  expect_match(script, "Reserved", fixed = TRUE)

  ## And the SAME unknown id, declared as computing, is emitted normally --
  ## so the exemption is not simply "anything arsbridge does not recognise".
  claims_to_compute <- .rsv_counting_method(id = foreign$id)
  computing <- .rce_write(.rsv_event(method = claims_to_compute, n_slots = 1L))

  expect_match(computing, "cards::ard_", fixed = TRUE)
})

test_that("the method's own derivation note is carried into the script", {
  method <- .rsv_reserved_method()
  method$codeTemplate <- list(
    context = "R (siera)",
    code = "## Derive by hand from the source listing.\n## Then fill the reserved row."
  )

  script <- .rce_write(.rsv_event(method = method))

  expect_match(script, "Derive by hand from the source listing", fixed = TRUE)
  expect_match(script, "Then fill the reserved row", fixed = TRUE)
})

test_that("reintroducing the defect turns the generic tests red", {
  ## Mutation: make the emitter blind to the declaration again, and assert the
  ## substitute calculation comes back.
  ns <- asNamespace("arsbridge")
  original <- get(".reserved_method_ids", envir = ns)
  withr::defer(.rsv_restore(".reserved_method_ids", original))

  .rsv_install(".reserved_method_ids", function(spec) character(0))

  script <- .rce_write(.rsv_event(method = .rsv_reserved_method()))

  expect_true(grepl("ard_categorical", script, fixed = TRUE))
})
