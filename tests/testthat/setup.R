## The workflow app remembers which project folders have been opened, in the
## user's config directory. A test suite must not write there -- so for the
## whole run the list lives in a temporary file that goes away with it.
withr::local_options(
  list(arsbridge.recent_projects = withr::local_tempfile(
    fileext = ".json", .local_envir = testthat::teardown_env())),
  .local_envir = testthat::teardown_env())
