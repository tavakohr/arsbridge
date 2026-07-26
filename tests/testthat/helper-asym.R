# Shared builders for the asymmetric column-tree scenario (synthetic
# CDSC-ALZ-201-style content): a spec workbook, an ADSL whose Cohort A
# subtotal (88) exceeds the sum of its displayed severity children (80), and
# the reporting event built from the asymmetric shell fixture.

.asym_spec_path <- function(td) {
  vars <- data.frame(
    Dataset   = rep("ADSL", 6),
    Variable  = c("USUBJID", "AGE", "SEX", "COMPFL", "COHGRPN", "SEVGR1N"),
    Label     = c("Unique Subject Identifier", "Age", "Sex",
                  "Completer Population Flag", "Cohort Group (N)",
                  "Baseline Severity Group 1 (N)"),
    Type      = c("Char", "Num", "Char", "Char", "Num", "Num"),
    Origin    = c("Assigned", "CRF", "CRF", "Derived", "Derived", "Derived"),
    Codelist  = c("", "", "SEX", "NY", "", ""),
    Length    = c("40", "8", "1", "1", "8", "8"),
    Mandatory = rep("Req", 6),
    stringsAsFactors = FALSE
  )
  path <- file.path(td, "adam_spec_asym.xlsx")
  wb <- openxlsx2::wb_workbook() |>
    openxlsx2::wb_add_worksheet("Variables") |>
    openxlsx2::wb_add_data(sheet = "Variables", x = vars)
  openxlsx2::wb_save(wb, file = path, overwrite = TRUE)
  path
}

# 150 completers: Cohort A holds 40 Mild + 25 Moderate + 15 Severe + 8 with
# missing severity (parent total 88, child sum 80); Cohort B holds 62.
.asym_adam <- function(td) {
  n_a <- c(mild = 40, moderate = 25, severe = 15, unknown = 8)
  n_b <- 62
  adsl <- data.frame(
    USUBJID = sprintf("S%03d", seq_len(sum(n_a) + n_b)),
    COHGRPN = c(rep(1, sum(n_a)), rep(2, n_b)),
    SEVGR1N = c(rep(1, n_a["mild"]), rep(2, n_a["moderate"]),
                rep(3, n_a["severe"]), rep(NA, n_a["unknown"]),
                rep(NA, n_b)),
    COMPFL  = "Y",
    SEX     = rep(c("M", "F"), length.out = sum(n_a) + n_b),
    AGE     = rep(seq(35, 75, by = 5), length.out = sum(n_a) + n_b),
    stringsAsFactors = FALSE
  )
  utils::write.csv(adsl, file.path(td, "adsl.csv"), row.names = FALSE)
  adsl
}

.asym_build <- function(td) {
  withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
      GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
    suppressMessages(spec_to_ars(
      shell_path     = testthat::test_path("fixtures/annotated_shell_asymmetric_tree.docx"),
      adam_spec_path = .asym_spec_path(td),
      output_path    = file.path(td, "re.json"),
      report_path    = file.path(td, "rep.xlsx"),
      verbose        = FALSE
    ))
  )
}
