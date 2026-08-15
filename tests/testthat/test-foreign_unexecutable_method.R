## A reporting event that declares its own method computes nothing is believed.
##
## The defect class: arsbridge decided a method was unexecutable by looking it
## up in a fixed registry of arsbridge's OWN method ids. A reporting event
## written elsewhere -- by another tool, by hand, by a future version -- can
## declare `supported = FALSE` on a method arsbridge has never seen. That
## declaration was ignored, the method fell through to the generic summarizer,
## and the analysis produced a categorical n(%) the event never asked for.
##
## This is the worst shape of the wrong-number risk: the row is well formed,
## the number is plausible, it fills, it formats, and nothing downstream can
## tell it apart from a result that was actually requested.
##
## The invariant: a method the event itself declares produces no result never
## computes. Its cells are reserved, and the generic summarizer is not reached.

.fue_adam <- function(vocab) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  subjects <- data.frame(
    USUBJID = sprintf("S%02d", 1:8),
    QXARM   = rep(c("A1", "A2"), each = 4),
    QXFL    = "Y",
    QXCAT   = c("P", "P", "Q", "Q", "P", "R", "R", "Q"),
    stringsAsFactors = FALSE
  )
  names(subjects) <- c("USUBJID", vocab$arm, vocab$pop, vocab$var)
  utils::write.csv(subjects, file.path(dir, paste0(vocab$ds, ".csv")),
                   row.names = FALSE)
  ## The subject-level dataset the denominator is joined from. A computing
  ## analysis needs it; a reserved one never reads any data at all, which is
  ## itself part of what "reserved" means.
  utils::write.csv(subjects, file.path(dir, "ADSL.csv"), row.names = FALSE)
  dir
}

.fue_run <- function(event, adam) {
  path <- file.path(withr::local_tempdir(.local_envir = parent.frame()),
                    "re.json")
  writeLines(
    jsonlite::toJSON(event, auto_unbox = TRUE, pretty = TRUE, null = "null"),
    path
  )
  as.data.frame(suppressMessages(suppressWarnings(ars_to_ard(path, adam))))
}


test_that("a foreign unexecutable method reserves and never computes", {
  checked <- 0L

  for (nm in names(.RSV_VOCABS)) {
    vocab <- .RSV_VOCABS[[nm]]
    adam  <- .fue_adam(vocab)

    ## An id arsbridge has never seen, declared by the event as computing
    ## nothing. Nothing about the id is recognisable; only the declaration is.
    foreign <- .rsv_reserved_method(id = "MTH_SOMEONE_ELSES_RESERVATION")
    ard <- .fue_run(.rsv_event(vocab = vocab, method = foreign), adam)

    rows <- ard[ard$analysis_id == "AN_SYNTH_001", , drop = FALSE]
    checked <- checked + 1L

    ## (a) A reserved result is present -- the row is not dropped.
    expect_gt(nrow(rows), 0L)
    expect_true(any(rows$result_status == "manual_pending"), info = nm)

    ## (b) Zero computed rows exist for that analysis.
    expect_false(any(rows$result_status == "computed"), info = nm)
    expect_true(all(is.na(rows$stat)), info = nm)

    ## (c) The generic summarizer was never reached. It is what produces the
    ## count/percentage statistics, so their absence is the evidence.
    expect_false(any(rows$stat_name %in% c("n", "p", "N", "mean", "sd")),
                 info = nm)
    expect_true(all(rows$stat_name %in% .MANUAL_STAT_NAME), info = nm)
  }

  ## Non-vacuity: an event the engine could not read would yield no rows and
  ## the expectations above would each be asserting over nothing.
  expect_equal(checked, 2L)
})


test_that("the generic summarizer is not merely silent -- it is not called", {
  ## (c), asserted directly rather than inferred from the output. The fallback
  ## announces itself with a warning naming the method; a reserved analysis
  ## must not produce it.
  vocab <- .RSV_NAMES_A
  adam  <- .fue_adam(vocab)
  foreign <- .rsv_reserved_method(id = "MTH_SOMEONE_ELSES_RESERVATION")

  path <- file.path(withr::local_tempdir(), "re.json")
  writeLines(
    jsonlite::toJSON(.rsv_event(vocab = vocab, method = foreign),
                     auto_unbox = TRUE, pretty = TRUE, null = "null"),
    path
  )

  warnings <- character(0)
  withCallingHandlers(
    suppressMessages(ars_to_ard(path, adam)),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_false(any(grepl("fallback summarizer", warnings, ignore.case = TRUE)))
})


test_that("a computing method on the same shape still computes", {
  ## The control. Without it, an engine that reserved everything would pass
  ## every expectation above.
  for (nm in names(.RSV_VOCABS)) {
    vocab <- .RSV_VOCABS[[nm]]
    adam  <- .fue_adam(vocab)

    ard <- .fue_run(
      .rsv_event(vocab = vocab, method = .rsv_counting_method(), n_slots = 1L),
      adam
    )
    rows <- ard[ard$analysis_id == "AN_SYNTH_001", , drop = FALSE]

    expect_gt(nrow(rows), 0L)
    expect_true(any(rows$result_status == "computed"), info = nm)
    expect_true(any(!is.na(rows$stat)), info = nm)
  }
})


test_that("reintroducing the defect turns the generic test red", {
  ## Mutation: make the engine blind to the event's declaration again -- the
  ## exact state before this fix -- and assert the analysis starts computing.
  original <- get(".reserved_method_ids", envir = asNamespace("arsbridge"))
  withr::defer(.rsv_restore(".reserved_method_ids", original))

  .rsv_install(".reserved_method_ids", function(spec) character(0))

  vocab <- .RSV_NAMES_A
  adam  <- .fue_adam(vocab)
  foreign <- .rsv_reserved_method(id = "MTH_SOMEONE_ELSES_RESERVATION")
  ard <- .fue_run(.rsv_event(vocab = vocab, method = foreign), adam)

  rows <- ard[ard$analysis_id == "AN_SYNTH_001", , drop = FALSE]

  ## Without the declaration being read, the fallback computes a number.
  expect_true(any(rows$result_status == "computed"))
})
