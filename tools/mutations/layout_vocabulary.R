## Mutations for the structural layout vocabulary. Run with:
##   Rscript tools/mutation_harness.R tools/mutations/layout_vocabulary.R
##
## PR5a claims the shape field now answers exactly one question -- how a row
## expands -- and that the renderers ask it, and only it, plus two explicitly
## local compatibility rules. These mutations attack that claim from both
## sides: a shape that should expand rendered flat, a shape that should stay
## flat rendered as a block, and each compatibility rule removed.

mutations <- list(
  ## A stat block stops owning its sub-rows, so a continuous row's Mean/SD
  ## lines stop being its stat lines and the header prints a number of its
  ## own. This is the distinction the audit found was NOT safe to collapse --
  ## if `continuous` and `row` really were the same shape, nothing here fails.
  list(id = "M1-statblock-flat", file = "R/aaa_constants.R",
       from = '  !is.na(shape) && shape %in% c("categorical_block", "stat_block")',
       to   = '  !is.na(shape) && shape %in% c("categorical_block")'),

  ## The reverse direction: a one-line row starts owning a block, so an
  ## authored label after it is swallowed as its stat line instead of standing
  ## on its own. Pinned because "collapse them" could be done either way, and
  ## only one of the two directions is caught by the test above.
  list(id = "M2-scalar-block", file = "R/aaa_constants.R",
       from = '  !is.na(shape) && shape %in% c("categorical_block", "stat_block")',
       to   = '  !is.na(shape) && shape %in% c("categorical_block", "stat_block", "scalar_row")'),

  ## The manual compatibility fallback is removed. A reserved row has no
  ## proved shape, so without this it stops owning its block and its stat
  ## lines detach -- which is exactly why the fallback is documented as a
  ## RENDERING rule rather than deleted as unprincipled.
  list(id = "M3-no-manual-fallback", file = "R/aaa_constants.R",
       from = '  if (!is.na(status) && identical(status, .LAYOUT_STATUS_MANUAL)) return(TRUE)',
       to   = '  if (FALSE) return(TRUE)'),

  ## The orphaned-nested-child rendering fallback is removed, so a child whose
  ## parent row is gone renders as a bare nested_child -- which owns no block
  ## and expands nothing, dropping its levels from the output.
  list(id = "M4-no-orphan-fallback", file = "R/ars_to_tfrmt.R",
       from = '    if (identical(le$kind, "nested_child")) render_shape <- "categorical_block"',
       to   = '    if (FALSE) render_shape <- "categorical_block"'),

  ## The shape field stops being read at all and the renderer falls back to
  ## the statistic form -- the pre-PR5a confusion, in its purest form: a
  ## question about expansion answered with a fact about the cell's contents.
  list(id = "M5-form-as-shape", file = "R/shell_table.R",
       from = '  if (!is.na(shape) && shape %in% c("stat_block", "categorical_block")) return("")',
       to   = '  if (!is.na(stat_form) && stat_form %in% c("stat_block", "categorical_block")) return("")')
)

test_files <- c(
  "tests/testthat/test-layout_vocabulary.R",
  "tests/testthat/test-shell_table.R",
  "tests/testthat/test-shell_layout_fidelity.R",
  "tests/testthat/test-ars_to_tfrmt.R",
  "tests/testthat/test-nested_renderer.R",
  "tests/testthat/test-build_nested_hierarchy.R"
)
