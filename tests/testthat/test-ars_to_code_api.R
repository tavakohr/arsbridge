# ars_to_code(): generating analysis programs is its own deliberate step.
#
# Authoring a reporting event and generating programs from it used to be one
# action, which meant the programs on disk were always generated from the ARS
# as it stood BEFORE review. Splitting them is only half the change: the other
# half is that there has to be an explicit way to generate, or the capability
# would simply disappear. Both halves are asserted here.
#
# What is deliberately NOT asserted: anything about executing a program, about
# what a Result contains, or about telling a hand-edited program from a stale
# one. Nothing on disk supports that last claim yet, and the error message this
# file pins is worded so it does not pretend otherwise.

.a2c_blank_env <- c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "",
                    GEMINI_API_KEY = "", GLM_API_KEY = "",
                    ARS_LLM_PROVIDER = "")

## Build a reporting event from the minimal fixture. `...` reaches spec_to_ars,
## so a test can opt into emission.
.a2c_build <- function(dir, ..., env = parent.frame()) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  withr::with_envvar(
    .a2c_blank_env,
    suppressMessages(suppressWarnings(spec_to_ars(
      shell_path     = test_path("fixtures/annotated_shell_2tlf_minimal.docx"),
      adam_spec_path = test_path("fixtures/adam_spec_minimal.xlsx"),
      output_path    = file.path(dir, "ars.json"),
      report_path    = file.path(dir, "rep.xlsx"),
      code_dir       = file.path(dir, "code"),
      verbose        = FALSE,
      ...))))
}

test_that("authoring does not generate programs by default", {
  td <- withr::local_tempdir()
  res <- .a2c_build(td)

  expect_true(file.exists(res$ars_path))
  expect_length(res$code_paths, 0L)
  expect_false(dir.exists(file.path(td, "code")))
})

test_that("emit_code = TRUE still generates them in one call", {
  td <- withr::local_tempdir()
  res <- .a2c_build(td, emit_code = TRUE)

  expect_gte(length(res$code_paths), 1L)
  expect_true(all(file.exists(res$code_paths)))
})

test_that("ars_to_code() generates every output from a saved ARS", {
  td <- withr::local_tempdir()
  res <- .a2c_build(td)
  out <- file.path(td, "gen")

  paths <- ars_to_code(res$ars_path, code_dir = out)

  expect_gte(length(paths), 1L)
  expect_true(all(file.exists(paths)))
  ## Named by output id, so a caller can address one without re-deriving names.
  spec <- jsonlite::fromJSON(res$ars_path, simplifyVector = FALSE)
  expect_equal(names(paths),
               vapply(spec$outputs, function(o) as.character(o$id), character(1)))
})

test_that("output_ids selects exactly the requested outputs", {
  td <- withr::local_tempdir()
  res <- .a2c_build(td)
  spec <- jsonlite::fromJSON(res$ars_path, simplifyVector = FALSE)
  ids <- vapply(spec$outputs, function(o) as.character(o$id), character(1))
  skip_if(length(ids) < 2, "fixture has fewer than two outputs")

  one <- file.path(td, "one")
  p1 <- ars_to_code(res$ars_path, ids[[1]], code_dir = one)
  expect_equal(names(p1), ids[[1]])
  expect_equal(list.files(one), paste0(make.names(ids[[1]]), ".R"))

  two <- file.path(td, "two")
  p2 <- ars_to_code(res$ars_path, ids[1:2], code_dir = two)
  expect_equal(names(p2), ids[1:2])
  expect_length(list.files(two), 2L)
})

test_that("an unknown output id is an error, not an empty result", {
  ## Silently generating nothing sends someone looking for a file that was
  ## never going to be written.
  td <- withr::local_tempdir()
  res <- .a2c_build(td)

  expect_error(
    ars_to_code(res$ars_path, "NO_SUCH_OUTPUT", code_dir = file.path(td, "gen")),
    class = "arsbridge_unknown_output"
  )
  ## And it names what IS available, so the typo is correctable from the message.
  err <- tryCatch(
    ars_to_code(res$ars_path, "NO_SUCH_OUTPUT", code_dir = file.path(td, "gen")),
    error = function(e) e)
  expect_match(conditionMessage(err), "NO_SUCH_OUTPUT")
  expect_match(conditionMessage(err), "Available output")
})

test_that("generation needs no ADaM data and never reads the shell again", {
  ## The saved ARS is the semantic source of truth. Proven by deleting the
  ## shell and the ADaM spec first: if generation reached back to either, it
  ## could not succeed. No ADaM data exists anywhere in this test.
  td <- withr::local_tempdir()
  shell_copy <- file.path(td, "shell.docx")
  spec_copy  <- file.path(td, "spec.xlsx")
  file.copy(test_path("fixtures/annotated_shell_2tlf_minimal.docx"), shell_copy)
  file.copy(test_path("fixtures/adam_spec_minimal.xlsx"), spec_copy)

  ars <- file.path(td, "ars.json")
  withr::with_envvar(
    .a2c_blank_env,
    suppressMessages(suppressWarnings(spec_to_ars(
      shell_path = shell_copy, adam_spec_path = spec_copy,
      output_path = ars, report_path = file.path(td, "rep.xlsx"),
      verbose = FALSE))))

  expect_true(file.remove(shell_copy))
  expect_true(file.remove(spec_copy))

  paths <- ars_to_code(ars, code_dir = file.path(td, "gen"))
  expect_gte(length(paths), 1L)
  expect_true(all(file.exists(paths)))
})

test_that("generation does not execute the program it writes", {
  ## `.run_emitted_block()` is the only place arsbridge evaluates emitted code.
  ## Generation must not reach it -- if a later change made ars_to_code() run
  ## what it writes, this fails rather than quietly executing user code.
  td <- withr::local_tempdir()
  res <- .a2c_build(td)

  local_mocked_bindings(
    .run_emitted_block = function(...) {
      stop("ars_to_code() must not execute emitted code")
    },
    .package = "arsbridge")

  expect_no_error(ars_to_code(res$ars_path, code_dir = file.path(td, "gen")))
})

test_that("an existing program is never silently replaced", {
  td <- withr::local_tempdir()
  res <- .a2c_build(td)
  out <- file.path(td, "gen")
  paths <- ars_to_code(res$ars_path, code_dir = out)
  target <- paths[[1]]

  ## Regenerating identical content is a no-op, so this is safe to repeat.
  expect_no_error(ars_to_code(res$ars_path, code_dir = out))

  ## A file that differs stops generation and survives untouched.
  writeLines(c(readLines(target), "# a programmer's note"), target)
  expect_error(ars_to_code(res$ars_path, code_dir = out),
               class = "arsbridge_existing_program")
  expect_true(any(grepl("programmer's note", readLines(target))))

  ## The message must not claim to know WHY it differs -- nothing on disk says.
  err <- tryCatch(ars_to_code(res$ars_path, code_dir = out),
                  error = function(e) e)
  expect_match(conditionMessage(err), "cannot yet tell which")

  ## Nothing else was written either: a refusal on one output must not leave
  ## the others already replaced.
  expect_true(any(grepl("programmer's note", readLines(target))))

  expect_no_error(ars_to_code(res$ars_path, code_dir = out, overwrite = TRUE))
  expect_false(any(grepl("programmer's note", readLines(target))))
})

test_that("a refusal leaves every other program untouched", {
  ## Generation builds all scripts before writing any, so a conflict on the
  ## second output cannot leave the first already overwritten.
  td <- withr::local_tempdir()
  res <- .a2c_build(td)
  out <- file.path(td, "gen")
  paths <- ars_to_code(res$ars_path, code_dir = out)
  skip_if(length(paths) < 2, "fixture has fewer than two outputs")

  ## Mark BOTH: the second is the conflict, the first must not be rewritten.
  first <- paths[[1]]
  second <- paths[[2]]
  writeLines(c(readLines(first), "# first marker"), first)
  writeLines(c(readLines(second), "# second marker"), second)

  expect_error(ars_to_code(res$ars_path, code_dir = out),
               class = "arsbridge_existing_program")
  expect_true(any(grepl("first marker", readLines(first))))
  expect_true(any(grepl("second marker", readLines(second))))
})
