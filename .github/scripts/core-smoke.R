## arsbridge -- core smoke test
## ---------------------------------------------------------------------------
## Runs in an environment holding ONLY arsbridge's hard dependencies. It
## deliberately does NOT use testthat: testthat hard-depends on callr, so
## installing it would silently reinstate one of the very packages whose
## absence this job exists to prove. Plain base assertions instead.
##
## What it proves, in order:
##   0. nothing is installed that is not a hard dependency (the real gate)
##   1. the package loads
##   2. a deterministic shell + spec converts to ARS, with no LLM anywhere
##   3. the resulting ARS validates and is runnable
##   4. the core ARS -> ARD path executes, with nothing blocked
##   5. the filled shell -- the headline deliverable -- is written
##   6. requesting an absent optional capability fails clearly
##
## Exit status is what CI reads: any stop() fails the job.
## ---------------------------------------------------------------------------

ok <- function(what) cat("  ok   ", what, "\n", sep = "")
step <- function(n, what) cat("\n[", n, "] ", what, "\n", sep = "")
need <- function(cond, what) {
  if (!isTRUE(cond)) stop("FAILED: ", what, call. = FALSE)
  ok(what)
}

## ---------------------------------------------------------------------------
step(0, "the environment holds hard dependencies only")

desc <- read.dcf(system.file("DESCRIPTION", package = "arsbridge"),
                 fields = c("Depends", "Imports", "LinkingTo", "Suggests"))
parse_deps <- function(field) {
  raw <- desc[1, field]
  if (is.na(raw)) return(character())
  parts <- trimws(strsplit(raw, ",")[[1]])
  parts <- sub("\\s*\\(.*\\)$", "", parts)
  setdiff(parts[nzchar(parts)], "R")
}
declared_hard <- unique(c(parse_deps("Depends"), parse_deps("Imports"),
                          parse_deps("LinkingTo")))
suggested <- parse_deps("Suggests")

## The hard closure, computed from the INSTALLED library rather than a repo,
## so it describes this machine and needs no network.
inst <- utils::installed.packages()
hard_of <- function(p) {
  if (!p %in% rownames(inst)) return(character())
  fields <- c(inst[p, "Depends"], inst[p, "Imports"], inst[p, "LinkingTo"])
  fields <- fields[!is.na(fields)]
  if (!length(fields)) return(character())
  parts <- trimws(unlist(strsplit(paste(fields, collapse = ","), ",")))
  parts <- sub("\\s*\\(.*\\)$", "", parts)
  setdiff(parts[nzchar(parts)], "R")
}
closure <- character()
frontier <- declared_hard
while (length(frontier)) {
  frontier <- setdiff(unique(unlist(lapply(frontier, hard_of))), closure)
  closure <- unique(c(closure, frontier))
}
closure <- unique(c(declared_hard, closure))

base_pkgs <- rownames(installed.packages(priority = c("base", "recommended")))

## The CI action installs its own tooling, sometimes onto a library path this
## script can see. Excluding those names cannot mask a Suggests leak: none of
## them is an arsbridge Suggests, and the by-name check below covers every
## Suggests independently of this one.
ci_harness <- c("pak", "sessioninfo")
installed_extra <- setdiff(rownames(inst),
                           c(base_pkgs, ci_harness, "arsbridge"))
stopifnot(!any(ci_harness %in% suggested))

## The invariant. Stated this way rather than as "no Suggests are installed",
## because several Suggests (withr, knitr, rmarkdown, bslib, askpass) are also
## legitimate hard dependencies of Imports and would false-alarm. Anything
## installed that is NOT hard-reachable got there by accident -- which is
## exactly the leak this job is meant to catch.
strays <- setdiff(installed_extra, closure)
if (length(strays)) {
  cat("  STRAY PACKAGES (installed but not hard dependencies):\n")
  cat(paste0("    ", sort(strays), collapse = "\n"), "\n")
  stop("FAILED: the job installed packages beyond the hard dependencies",
       call. = FALSE)
}
ok(sprintf("%d packages installed, all hard-reachable", length(installed_extra)))

## And the readable half: the optional capabilities are, by name, absent.
## This list shrinks by itself -- as tiers move to Suggests in later PRs they
## leave the hard closure and appear here with no edit to this script.
##
## Recommended packages are excluded because they ship WITH R: `survival` is
## a Suggests and is also always installed, so requiring its absence would be
## asking for an environment that cannot exist.
optional_absent <- setdiff(suggested, c(closure, base_pkgs))
present <- optional_absent[vapply(optional_absent, function(p)
  p %in% rownames(inst), logical(1))]
cat("  optional packages required to be absent:\n    ",
    paste(sort(optional_absent), collapse = ", "), "\n", sep = "")
need(length(present) == 0,
     paste("every optional package is absent",
           if (length(present)) paste("-- found:", paste(present, collapse = ", "))))
need(!("callr" %in% rownames(inst)), "callr specifically is absent")

## ---------------------------------------------------------------------------
step(1, "the package loads")
library(arsbridge)
ok("library(arsbridge)")

## No key, no provider: the deterministic reader, and nothing else.
Sys.setenv(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
           GLM_API_KEY = "", ARS_LLM_PROVIDER = "")

out_dir <- tempfile("core-smoke-"); dir.create(out_dir)
adam_dir <- tempfile("adam-");      dir.create(adam_dir)
utils::unzip(arsbridge_example("ADaM.zip"), exdir = adam_dir)

## ---------------------------------------------------------------------------
step(2, "a deterministic shell and spec convert to ARS")
ars_path <- file.path(out_dir, "ars.json")
built <- suppressMessages(suppressWarnings(spec_to_ars(
  shell_path     = arsbridge_example("annotated_shell.xlsx"),
  adam_spec_path = arsbridge_example("adam_spec.xlsx"),
  output_path    = ars_path,
  study_id       = "CORE-SMOKE-1")))
need(file.exists(ars_path), "ARS JSON written")
spec <- jsonlite::fromJSON(ars_path, simplifyVector = FALSE)
need(length(spec$analyses) > 0, sprintf("ARS carries %d analyses", length(spec$analyses)))
need(length(spec$outputs) > 0, sprintf("ARS carries %d outputs", length(spec$outputs)))
need(length(spec$mainListOfContents) > 0, "ARS carries a list of contents")

## ---------------------------------------------------------------------------
step(3, "the ARS validates and is runnable")
model <- ars_to_model(ars_path)
findings <- validate_ars_model(model)
fails <- if (is.data.frame(findings) && nrow(findings)) {
  sum(findings$severity %in% "FAIL")
} else 0L
need(fails == 0L, sprintf("no blocking FAIL findings (%d FAIL)", fails))

## ---------------------------------------------------------------------------
step(4, "the core ARS -> ARD path executes")
ard <- suppressMessages(suppressWarnings(ars_to_ard(ars_path, adam_dir)))
need(is.data.frame(ard) && nrow(ard) > 0, sprintf("ARD has %d rows", nrow(ard)))
need("result_status" %in% names(ard), "ARD carries result_status")
blocked <- sum(ard$result_status %in% "blocked")
need(blocked == 0L, sprintf("nothing blocked (%d blocked rows)", blocked))
need(sum(ard$result_status %in% "computed") > 0, "results were actually computed")

## ---------------------------------------------------------------------------
step(5, "the filled shell is written")
filled <- file.path(out_dir, "filled.xlsx")
fill <- suppressMessages(suppressWarnings(ars_fill_shell(
  shell_path  = arsbridge_example("annotated_shell.xlsx"),
  ars         = ars_path,
  ard         = ard,
  output_path = filled,
  adam_dir    = adam_dir)))
need(file.exists(filled), "filled workbook written")
need(is.numeric(fill$filled) && fill$filled > 0,
     sprintf("%d cells filled", fill$filled))

## ---------------------------------------------------------------------------
step(6, "an absent optional capability fails clearly")
## shiny/bslib/DT are Suggests and are not here. The app must say which
## packages it needs -- not fail with an object-not-found deep inside.
msg <- tryCatch({ ars_workflow(out_dir); NA_character_ },
                error = function(e) conditionMessage(e))
need(!is.na(msg), "ars_workflow() refuses rather than proceeding")
need(grepl("shiny", msg, fixed = TRUE),
     paste0("the refusal names the missing package: ", gsub("\n", " ", msg)))

## The callr path is different by design: it degrades instead of refusing,
## so there is nothing here to catch -- .workflow_start_build() is reachable
## only from inside a shiny session. Its behaviour is pinned by
## tests/testthat/test-optional_callr.R.

cat("\nCORE SMOKE PASSED\n")
