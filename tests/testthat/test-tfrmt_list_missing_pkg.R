# ars_to_tfrmt_list() and the difference between "this output failed" and
# "the package is not installed".
#
# The per-output tryCatch is right for a table that could not be built and
# wrong for an absent optional package: that is the same answer for every
# output, so skipping each in turn warns once per output and returns a list of
# NULLs -- "install tfrmt" arriving as a warning storm over an empty result.
#
# Both distinctions are pinned here. tfrmt itself is never needed: the tests
# drive the handler by making ars_to_tfrmt() raise, which is the only thing
# that changed.

## Two outputs, so "one error, not one per output" is actually measurable.
.tl_fixture <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  path <- file.path(dir, "re.json")
  jsonlite::write_json(
    list(id = "RE1", outputs = list(list(id = "T_1"), list(id = "T_2"))),
    path, auto_unbox = TRUE)
  list(
    ars_path = path,
    ard = data.frame(output_id = c("T_1", "T_2"), value = c(1, 2),
                     stringsAsFactors = FALSE))
}

test_that("a missing tfrmt fails once, not once per output", {
  fx <- .tl_fixture()
  local_mocked_bindings(
    ars_to_tfrmt = function(...) {
      rlang::abort("The package `tfrmt` is required.",
                   class = "rlib_error_package_not_found")
    },
    .package = "arsbridge")

  ## The real condition reaches the caller -- the same class
  ## rlang::check_installed() raises, so a caller can catch it the same way
  ## for ars_to_tfrmt_list() as for ars_to_tfrmt().
  expect_error(ars_to_tfrmt_list(fx$ars_path, fx$ard),
               class = "rlib_error_package_not_found")

  ## And it is not ALSO reported per output. Two outputs, zero warnings.
  warnings_seen <- character()
  expect_error(
    withCallingHandlers(
      ars_to_tfrmt_list(fx$ars_path, fx$ard),
      warning = function(w) {
        warnings_seen <<- c(warnings_seen, conditionMessage(w))
        invokeRestart("muffleWarning")
      }),
    class = "rlib_error_package_not_found")
  expect_length(warnings_seen, 0L)
})

test_that("an output-specific failure still skips and warns, per output", {
  ## The behaviour that must NOT change: one bad output does not stop the
  ## others, and the list keeps a NULL in its place.
  fx <- .tl_fixture()
  local_mocked_bindings(
    ars_to_tfrmt = function(ars_path, ard, output_id, ...) {
      if (identical(output_id, "T_1")) stop("zero rows for this output")
      structure(list(), class = "tfrmt")
    },
    .package = "arsbridge")

  expect_warning(
    result <- ars_to_tfrmt_list(fx$ars_path, fx$ard),
    "Skipping output")

  expect_named(result, c("T_1", "T_2"))
  expect_null(result[["T_1"]])
  expect_s3_class(result[["T_2"]], "tfrmt")
})

test_that("the skip warning still carries the underlying reason", {
  ## Routing through .render_output_error() must not have cost the message
  ## that tells a reader WHY the output was skipped.
  fx <- .tl_fixture()
  ## Only T_1 fails, so exactly one warning is raised and none escapes the
  ## expectation to litter the suite.
  local_mocked_bindings(
    ars_to_tfrmt = function(ars_path, ard, output_id, ...) {
      if (identical(output_id, "T_1")) stop("zero rows for this output")
      structure(list(), class = "tfrmt")
    },
    .package = "arsbridge")

  expect_warning(ars_to_tfrmt_list(fx$ars_path, fx$ard),
                 "zero rows for this output")
})
