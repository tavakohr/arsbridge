## Regenerate the committed golden reporting events.
##
##   Rscript data-raw/regenerate_goldens.R
##
## Run this DELIBERATELY, never from the test suite and never in CI. There is
## no update-on-run flag for the gate on purpose: an environment variable that
## rewrites expectations is one CI setting away from accepting every drift
## silently, which is how golden tests stop being tests.
##
## The output is the same canonical form the gate compares, so the `git diff`
## on a golden IS the semantic diff -- it names the analyses, groupings or
## methods that moved. Any PR that changes one is expected to say why in its
## body.

if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("devtools is needed to regenerate goldens.", call. = FALSE)
}
if (!file.exists("DESCRIPTION")) {
  stop("run this from the package root.", call. = FALSE)
}

devtools::load_all(".", quiet = TRUE)
library(testthat)

## Everything below runs with the test directory as the working directory, so
## the helper's own test_path() calls resolve exactly as they do under
## testthat -- no second set of paths to keep in step with the first.
withr::with_dir("tests/testthat", {
  source("helper-goldens.R")

  dir.create("goldens", showWarnings = FALSE, recursive = TRUE)

  for (case in .golden_cases()) {
    if (!requireNamespace(case$needs, quietly = TRUE)) {
      message("skipping ", case$name, ": ", case$needs, " is not installed")
      next
    }

    message("building ", case$name, " ...")
    ars <- .build_golden_ars(case, envir = environment())

    ## The same hardening the gate applies. A malformed generator or a
    ## duplicate id must stop a regeneration too -- otherwise the bad state is
    ## what gets committed as "expected".
    .assert_no_paths(ars, case$name)
    canonical <- .ars_canonical(ars, case$name)

    path <- file.path("goldens", paste0(case$name, ".json"))
    writeLines(.golden_json_text(canonical), path)
    message("  wrote tests/testthat/", path, " (",
            length(canonical$outputs), " outputs, ",
            length(canonical$analyses), " analyses)")
  }
})

message("done -- review the diff before committing.")
