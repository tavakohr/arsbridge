# TRUE only when {cardx} can actually compute a Clopper-Pearson CI in THIS
# environment. cardx is a Suggests, and on some dependency-version combinations
# (seen on clean CI runners) cardx::ard_categorical_ci() errors. arsbridge then
# degrades to a reserved manual_pending cell by design, so the compute-assertion
# tests skip rather than fail when cardx cannot run here. Mirrors the exact path
# arsbridge emits (the ard_proportion_ci_exact wrapper).
cardx_ci_works <- function() {
  if (!requireNamespace("cardx", quietly = TRUE)) return(FALSE)
  d <- data.frame(g = c("a", "a", "b", "b"), r = c("Y", "N", "Y", "N"),
                  stringsAsFactors = FALSE)
  isTRUE(tryCatch({
    res <- arsbridge::ard_proportion_ci_exact(d, variables = "r", by = "g")
    is.data.frame(res) && nrow(res) > 0
  }, error = function(e) FALSE))
}
