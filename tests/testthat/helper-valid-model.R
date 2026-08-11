## Return the frozen editor fixture with its known legacy empty fixed grouping
## corrected in memory. The file itself remains unchanged so validation tests can
## continue to prove that the legacy shape is invalid.
.valid_fixture_model <- function() {
  model <- ars_to_model(
    test_path("fixtures", "ars_apx_drm_301_deterministic.json")
  )
  empty_fixed <- which(
    !is.na(model$groupings$dataDriven) &
      !model$groupings$dataDriven &
      model$groupings$n_groups == 0L
  )

  for (i in empty_fixed) {
    model <- model_set_field(
      model,
      "groupings",
      model$groupings$id[i],
      "dataDriven",
      TRUE
    )
  }
  model
}
