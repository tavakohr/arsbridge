## The workflow app remembers which project folders have been opened, in the
## user's config directory. A test suite must not write there -- so for the
## whole run the list lives in a temporary file that goes away with it.
withr::local_options(
  list(arsbridge.recent_projects = withr::local_tempfile(
    fileext = ".json", .local_envir = testthat::teardown_env())),
  .local_envir = testthat::teardown_env())

## The editor autosaves to the user's cache directory, on purpose -- it has to
## outlive the R session it is protecting against. Under test that is exactly
## wrong: every testServer() run that reaches .write_autosave() would leave an
## .rds behind under a fresh path key, and nothing ever prunes them. For the
## whole run they go to a temporary directory instead, which goes away with it.
withr::local_options(
  list(arsbridge.autosave_dir = withr::local_tempdir(
    pattern = "arsbridge-autosave",
    .local_envir = testthat::teardown_env())),
  .local_envir = testthat::teardown_env())
