## A method that declares it computes nothing cannot under-provide slots.
##
## The defect class: the placeholder check compares how many numbers a shell
## cell shows against how many operations its method declares, and reports a
## shortfall. Applied to a method that declares it computes NOTHING, that test
## is meaningless -- such a method provides no operations by construction, and
## the row carrying it was reserved on purpose. Before this fix the reservation
## defeated itself: a deliberately reserved row displayed with an ordinary
## two-number placeholder produced a blocking finding, which refused the whole
## reporting event including every sound output in it.
##
## The invariant: a method whose own declaration says it produces no result is
## never a slot mismatch. Recognised from the declaration (`supported`), never
## from the method's id, so an ARS written elsewhere that marks its own method
## the same way is treated the same.

test_that("a reserved row on a multi-number placeholder is not a mismatch", {
  checked <- 0L

  for (nm in names(.RSV_VOCABS)) {
    vocab <- .RSV_VOCABS[[nm]]

    for (n_slots in c(1L, 2L, 3L)) {
      model <- .rsv_model(vocab = vocab, method = .rsv_reserved_method(),
                          n_slots = n_slots)
      findings <- validate_ars_model(model)
      checked <- checked + 1L

      expect_false(
        any(findings$ref %in% "METHOD_PLACEHOLDER_SLOT_MISMATCH"),
        info = paste(nm, n_slots)
      )
    }
  }

  ## Non-vacuity: a model the grammar could not read would produce no findings
  ## at all and pass every expectation above without checking anything.
  expect_equal(checked, 6L)
})

test_that("the same shape on a computing method is still a mismatch", {
  ## The control. Without it the test above would also pass if the check had
  ## simply been deleted.
  for (nm in names(.RSV_VOCABS)) {
    model <- .rsv_model(vocab = .RSV_VOCABS[[nm]],
                        method = .rsv_counting_method(), n_slots = 2L)
    findings <- validate_ars_model(model)

    mismatch <- findings[
      findings$ref %in% "METHOD_PLACEHOLDER_SLOT_MISMATCH", , drop = FALSE
    ]
    expect_equal(nrow(mismatch), 1L, info = nm)
    expect_equal(mismatch$severity, "FAIL", info = nm)
    expect_equal(mismatch$id, "AN_SYNTH_001", info = nm)
  }
})

test_that("a computing method that covers the placeholder raises nothing", {
  for (nm in names(.RSV_VOCABS)) {
    model <- .rsv_model(vocab = .RSV_VOCABS[[nm]],
                        method = .rsv_counting_method(), n_slots = 1L)
    findings <- validate_ars_model(model)

    expect_false(any(findings$ref %in% "METHOD_PLACEHOLDER_SLOT_MISMATCH"),
                 info = nm)
  }
})

test_that("the exemption reads the declaration, not the method id", {
  ## The point of the invariant. This method's id is unknown to arsbridge --
  ## it is not MTH_UNSUPPORTED_ANALYSIS and not in .UNEXECUTABLE_METHODS -- so
  ## a fix that matched on the id would report it as a mismatch.
  foreign <- .rsv_reserved_method(id = "MTH_SOMEONE_ELSES_RESERVATION")
  model <- .rsv_model(method = foreign, n_slots = 2L)

  expect_false(
    any(validate_ars_model(model)$ref %in% "METHOD_PLACEHOLDER_SLOT_MISMATCH")
  )

  ## And the exemption is not simply "any method arsbridge does not know":
  ## the same unknown id, declared as computing, is still checked.
  claims_to_compute <- foreign
  claims_to_compute$supported <- TRUE
  computing <- .rsv_model(method = claims_to_compute, n_slots = 2L)

  expect_true(
    any(validate_ars_model(computing)$ref %in%
          "METHOD_PLACEHOLDER_SLOT_MISMATCH")
  )
})

test_that("reintroducing the defect turns the generic tests red", {
  ## Mutation: put back the behaviour this fix removed -- treat a
  ## no-result method as an under-provisioned one -- and assert the reserved
  ## row becomes a blocking finding again. Without this, deleting the check
  ## entirely would leave every test above green.
  ns <- asNamespace("arsbridge")
  original <- get(".check_method_placeholder_slots", envir = ns)
  withr::defer(.rsv_restore(".check_method_placeholder_slots", original))

  mutated <- function(findings, model) {
    ## Strip the declaration the exemption reads, so the check treats the
    ## reservation method as an ordinary one.
    model$methods$supported <- rep(TRUE, length(model$methods$supported))
    original(findings, model)
  }
  .rsv_install(".check_method_placeholder_slots", mutated)

  model <- .rsv_model(method = .rsv_reserved_method(), n_slots = 2L)
  findings <- validate_ars_model(model)

  expect_true(any(findings$ref %in% "METHOD_PLACEHOLDER_SLOT_MISMATCH"))
})
