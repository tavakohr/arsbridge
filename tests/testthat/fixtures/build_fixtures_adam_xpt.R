## tests/testthat/fixtures/build_fixtures_adam_xpt.R
## ---------------------------------------------------------------------------
## Builds adam_apx_drm_301_xpt/ADSL.xpt -- the one fixture in the suite whose
## columns arrive TYPED.
##
## Run from the package root:
##
##   Rscript tests/testthat/fixtures/build_fixtures_adam_xpt.R
##
## Why a second ADaM fixture, in a second folder, for four subjects:
##
##   The committed ADaM fixtures are .csv, and `utils::read.csv()` hands back
##   character columns whatever the file holds. Real ADaM arrives as .xpt,
##   where a date variable comes back as a `Date` -- so `.listing_value()`'s
##   Date branch ran in production and never in CI (issue #2).
##
##   It is a SEPARATE folder because `.listing_load()` prefers .xpt over .csv
##   for the same dataset name. An ADSL.xpt dropped beside the existing
##   ADSL.csv would silently become the input to every other fill-writer test.
##
## Everything here is invented, and matches the CSV fixture's subjects so the
## two describe the same imaginary study. APX-DRM-301 is not a study.
##
## The dates are deliberate:
##
##   * TRTSDT is a plain date, and the value the rendered-text assertion pins.
##   * TRTEDT is missing for one subject -- a listing blank means "not
##     recorded", so NA must render as an empty cell rather than "NA".
##   * The two dates straddle a year boundary, so a writer that formatted via
##     the underlying number rather than the date would be visibly wrong.
##
## The .xpt column-name cap is 8 characters; every name here is within it.

fixtures <- file.path("tests", "testthat", "fixtures")
adam_dir <- file.path(fixtures, "adam_apx_drm_301_xpt")
dir.create(adam_dir, showWarnings = FALSE, recursive = TRUE)

adsl <- data.frame(
  USUBJID = c("APX-301-001", "APX-301-002", "APX-301-005", "APX-301-009"),
  TRT01A  = c("Placebo", "Placebo", "Drug 10 mg", "Drug 20 mg"),
  TRTSDT  = as.Date(c("2025-12-29", "2026-01-02", "2026-02-14", "2026-03-01")),
  TRTEDT  = as.Date(c("2026-01-05", "2026-02-28", NA, "2026-04-15")),
  stringsAsFactors = FALSE
)

path <- file.path(adam_dir, "ADSL.xpt")
haven::write_xpt(adsl, path, version = 5)
cat(sprintf("  ADSL.xpt  %d rows x %d cols\n", nrow(adsl), ncol(adsl)))

## Read it straight back: the whole point of the fixture is the type that
## survives the round trip, so the build fails loudly if it does not.
back <- haven::read_xpt(path)
stopifnot(inherits(back$TRTSDT, "Date"),
          inherits(back$TRTEDT, "Date"),
          identical(as.character(back$TRTSDT), as.character(adsl$TRTSDT)),
          is.na(back$TRTEDT[[3]]))
cat("  round trip: TRTSDT and TRTEDT come back as Date\n")
