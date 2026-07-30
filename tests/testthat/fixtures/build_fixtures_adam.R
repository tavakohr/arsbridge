## tests/testthat/fixtures/build_fixtures_adam.R
## ---------------------------------------------------------------------------
## Builds adam_apx_drm_301/ -- the synthetic ADaM datasets the fill-writer
## tests run against, matching the shell fixture shells_apx_drm_301.xlsx.
##
## Run from the package root:
##
##   Rscript tests/testthat/fixtures/build_fixtures_adam.R
##
## Everything here is invented. The subject identifiers, the treatment names
## and every value are made up for this fixture; APX-DRM-301 is not a study.
##
## Two properties are deliberate and the tests depend on them:
##
##   1. EVERY NUMBER IS CHECKABLE BY HAND. Twelve subjects, four per arm, with
##      ages chosen so the mean and quartiles are exact -- Placebo is
##      60/64/68/72, mean 66. A fill-writer test that says "cell C6 should read
##      66.0 (5.16)" is worth more when the reader can see why, because the
##      failure mode being guarded against is a number landing in the wrong
##      cell, and a plausible-looking wrong number is the one that survives
##      review.
##
##   2. THE ARMS ARE NOT SYMMETRIC. Different completion counts, a different
##      sex split, one arm with no major deviation at all, and a subject with
##      no adverse event. Symmetric arms hide column-alignment bugs: if every
##      column holds the same value, writing all three from column 1 looks
##      correct.
##
## TRT01A must read exactly "Placebo", "Drug 10 mg", "Drug 20 mg" -- those are
## the column headers in the shell, and the writer matches a result's group
## value to a column by that label.

fixtures <- file.path("tests", "testthat", "fixtures")
adam_dir <- file.path(fixtures, "adam_apx_drm_301")
dir.create(adam_dir, showWarnings = FALSE, recursive = TRUE)

write_ds <- function(df, name) {
  path <- file.path(adam_dir, paste0(name, ".csv"))
  utils::write.csv(df, path, row.names = FALSE, na = "")
  cat(sprintf("  %-8s %2d rows x %2d cols\n", name, nrow(df), ncol(df)))
}

## ---------------------------------------------------------------------------
## ADSL -- one row per subject
## ---------------------------------------------------------------------------

arm   <- rep(c("Placebo", "Drug 10 mg", "Drug 20 mg"), each = 4)
## Placebo is 0 so the two active arms are 1 and 2, which is what the shell's
## own header annotations say -- Table 14.2.1 heads its columns
## "[ADEX.TRT01AN = 1]" and "[ADEX.TRT01AN = 2]". The shell is the contract
## here: numbering the arms 1/2/3 instead would put every exposure result one
## column to the left, with the right-looking header above it.
armn  <- rep(c(0L, 1L, 2L), each = 4)
subj  <- sprintf("APX-301-%03d", seq_along(arm))

adsl <- data.frame(
  USUBJID = subj,
  TRT01A  = arm,
  TRT01AN = armn,
  SAFFL   = rep("Y", 12),
  ## Round means (66 / 58 / 70) but three DIFFERENT spreads, so a standard
  ## deviation written into the wrong column is caught: 5.16, 3.65, 6.73.
  AGE     = c(60, 64, 68, 72,
              54, 56, 60, 62,
              62, 68, 72, 78),
  AGEU    = rep("YEARS", 12),
  ## Not an even split, and not the same split in each arm.
  SEX     = c("F", "F", "M", "M",
              "F", "M", "M", "M",
              "F", "F", "F", "M"),
  ## 4 / 3 / 2 completions -- three different counts, so a column mix-up
  ## cannot pass.
  EOSSTT  = c("COMPLETED", "COMPLETED", "COMPLETED", "COMPLETED",
              "COMPLETED", "COMPLETED", "COMPLETED", "DISCONTINUED",
              "COMPLETED", "COMPLETED", "DISCONTINUED", "DISCONTINUED"),
  stringsAsFactors = FALSE
)

## ---------------------------------------------------------------------------
## ADAE -- adverse events; subject 004 has none, so an arm's denominator and
## its event count differ.
## ---------------------------------------------------------------------------

adae <- data.frame(
  USUBJID = c("APX-301-001", "APX-301-001", "APX-301-002", "APX-301-003",
              "APX-301-005", "APX-301-005", "APX-301-006", "APX-301-007",
              "APX-301-008",
              "APX-301-009", "APX-301-010", "APX-301-010", "APX-301-011",
              "APX-301-012"),
  AESOC   = c("Nervous system disorders", "Gastrointestinal disorders",
              "Nervous system disorders", "Gastrointestinal disorders",
              "Nervous system disorders", "Nervous system disorders",
              "Gastrointestinal disorders", "Nervous system disorders",
              "Skin and subcutaneous tissue disorders",
              "Nervous system disorders", "Gastrointestinal disorders",
              "Skin and subcutaneous tissue disorders",
              "Nervous system disorders", "Nervous system disorders"),
  AEDECOD = c("Headache", "Nausea", "Headache", "Nausea",
              "Headache", "Dizziness", "Nausea", "Headache", "Rash",
              "Headache", "Nausea", "Rash", "Dizziness", "Headache"),
  ASEV    = c("MILD", "MODERATE", "MILD", "MILD",
              "MODERATE", "MILD", "MILD", "SEVERE", "MODERATE",
              "MILD", "MODERATE", "MILD", "MILD", "MODERATE"),
  AEOUT   = c("RECOVERED", "RECOVERED", "RECOVERED", "RECOVERING",
              "RECOVERED", "RECOVERED", "RECOVERED", "NOT RECOVERED",
              "RECOVERED", "RECOVERED", "RECOVERED", "RECOVERING",
              "RECOVERED", "RECOVERED"),
  ## One event is not treatment-emergent, so TRTEMFL actually filters.
  TRTEMFL = c("Y", "Y", "Y", "Y",
              "Y", "Y", "Y", "Y", "N",
              "Y", "Y", "Y", "Y", "Y"),
  ASTDT   = c("2024-02-04", "2024-02-19", "2024-02-11", "2024-03-02",
              "2024-02-07", "2024-03-15", "2024-02-22", "2024-01-30",
              "2024-04-01",
              "2024-02-14", "2024-02-27", "2024-03-08", "2024-03-21",
              "2024-02-02"),
  stringsAsFactors = FALSE
)
adae$TRT01A <- adsl$TRT01A[match(adae$USUBJID, adsl$USUBJID)]

## ---------------------------------------------------------------------------
## ADDV -- protocol deviations. Drug 20 mg has no MAJOR deviation, so one
## cell is a legitimate zero rather than a missing value.
## ---------------------------------------------------------------------------

addv <- data.frame(
  USUBJID = c("APX-301-002", "APX-301-003",
              "APX-301-005", "APX-301-008",
              "APX-301-010"),
  DVCAT   = c("MAJOR", "MINOR", "MAJOR", "MAJOR", "MINOR"),
  DVDECOD = c("INCLUSION", "VISIT WINDOW", "EXCLUSION", "INCLUSION",
              "VISIT WINDOW"),
  stringsAsFactors = FALSE
)
addv$TRT01A <- adsl$TRT01A[match(addv$USUBJID, adsl$USUBJID)]

## ---------------------------------------------------------------------------
## ADEX -- exposure by visit. Only the two active arms, at weeks 12 and 24,
## which is the column axis of Table 14.2.1 (TRT01AN crossed with AVISITN).
## ---------------------------------------------------------------------------

ex_subj <- adsl$USUBJID[adsl$TRT01AN %in% c(1L, 2L)]
adex <- data.frame(
  USUBJID = rep(ex_subj, each = 2),
  AVISITN = rep(c(12L, 24L), times = length(ex_subj)),
  AVISIT  = rep(c("Week 12", "Week 24"), times = length(ex_subj)),
  ## Week 24 runs longer than week 12 in both arms, and the arms differ.
  TRTDURD = c(84, 168, 82, 165, 84, 170, 80, 160,
              84, 172, 84, 168, 79, 158, 84, 166),
  stringsAsFactors = FALSE
)
adex$TRT01A  <- adsl$TRT01A[match(adex$USUBJID, adsl$USUBJID)]
adex$TRT01AN <- adsl$TRT01AN[match(adex$USUBJID, adsl$USUBJID)]

## ---------------------------------------------------------------------------
## ADCM -- concomitant medications, for the second listing.
## ---------------------------------------------------------------------------

adcm <- data.frame(
  USUBJID = c("APX-301-001", "APX-301-003", "APX-301-005",
              "APX-301-009", "APX-301-011"),
  CMTRT   = c("Paracetamol", "Ibuprofen", "Paracetamol",
              "Omeprazole", "Ibuprofen"),
  ASTDT   = c("2024-02-05", "2024-03-03", "2024-02-08",
              "2024-02-15", "2024-03-22"),
  stringsAsFactors = FALSE
)
adcm$TRT01A <- adsl$TRT01A[match(adcm$USUBJID, adsl$USUBJID)]

## ---------------------------------------------------------------------------
## ADVS -- vital signs by visit, the series behind the figure (PR8).
## ---------------------------------------------------------------------------

vs_visits <- c(0L, 12L, 24L)
advs <- data.frame(
  USUBJID = rep(adsl$USUBJID, each = length(vs_visits)),
  AVISITN = rep(vs_visits, times = nrow(adsl)),
  AVISIT  = rep(c("Baseline", "Week 12", "Week 24"), times = nrow(adsl)),
  PARAMCD = "DIABP",
  PARAM   = "Diastolic Blood Pressure (mmHg)",
  stringsAsFactors = FALSE
)
advs$TRT01A <- adsl$TRT01A[match(advs$USUBJID, adsl$USUBJID)]

## Each subject starts near 82 mmHg and moves by an amount that depends on the
## arm: placebo stays flat, and the two active arms fall, the higher dose
## further. Built term by term rather than as one nested expression, because
## a fixture whose expected values cannot be read off the code is not much of
## a fixture.
subject_offset <- c(0, 2, -2, 4)                       # within each arm
visit_change <- list(
  "Placebo"    = c(0,  0,  1),                         # baseline, wk12, wk24
  "Drug 10 mg" = c(0, -3, -5),
  "Drug 20 mg" = c(0, -6, -9)
)
advs$AVAL <- 82 +
  subject_offset[match(advs$USUBJID, adsl$USUBJID) %% 4 + 1] +
  mapply(function(arm, visit) visit_change[[arm]][match(visit, vs_visits)],
         advs$TRT01A, advs$AVISITN)

## ---------------------------------------------------------------------------

cat("writing", adam_dir, "\n")
write_ds(adsl, "ADSL")
write_ds(adae, "ADAE")
write_ds(addv, "ADDV")
write_ds(adex, "ADEX")
write_ds(adcm, "ADCM")
write_ds(advs, "ADVS")

cat("\nsanity -- what the tables should show\n")
cat("  subjects treated  :",
    paste(sprintf("%s=%d", names(table(adsl$TRT01A)), table(adsl$TRT01A)),
          collapse = "  "), "\n")
completed <- table(adsl$TRT01A[adsl$EOSSTT == "COMPLETED"])
cat("  completed         :",
    paste(sprintf("%s=%d", names(completed), completed), collapse = "  "), "\n")
for (a in unique(adsl$TRT01A)) {
  ages <- adsl$AGE[adsl$TRT01A == a]
  cat(sprintf("  age %-11s mean %.1f  sd %.2f  median %.1f  Q1 %.1f Q3 %.1f\n",
              a, mean(ages), stats::sd(ages), stats::median(ages),
              stats::quantile(ages, 0.25, names = FALSE),
              stats::quantile(ages, 0.75, names = FALSE)))
}
