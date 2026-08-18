## Regenerate inst/extdata/statistic_label_support.md.
##
## The file is generated from the statistic vocabulary and the method
## catalogue, so it cannot describe anything the code does not do.
## test-stat_label_grammar.R regenerates it and compares, which is what
## turns a stale table into a red suite rather than misleading documentation.
##
## Run from the package root after changing .STAT_ALIASES, .STAT_COMPOSITES,
## .OP_TOKENS or .STANDARD_METHODS:
##
##   Rscript data-raw/build_statistic_label_doc.R
devtools::load_all(".", quiet = TRUE)
target <- file.path("inst", "extdata", "statistic_label_support.md")
cat(arsbridge:::.stat_label_support_md(), file = target)
message("wrote ", target)
