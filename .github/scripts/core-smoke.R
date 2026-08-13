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
##   7. each rendering export asks only for the packages ITS path needs
##   8. regex + an offline supplement -- the production path -- works with
##      no ellmer, and ellmer is required only when a run would call an LLM
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

## ---------------------------------------------------------------------------
step(7, "each rendering capability names the packages IT needs")
## This is the only environment where the four rendering packages are really
## absent, so it is the only place the guards can be proven rather than
## simulated. The point is not merely that each call fails -- it is that each
## names ONLY what its own execution path needs. A guard demanding all four
## would pass a "does it fail?" test and still be wrong.
##
## Each capability is asked about an output of ITS OWN kind, using the
## package's own classifier. Handing a figure id to the table renderer would
## test which check happens to come first, not what the capability requires.
RENDER_PKGS <- c("tfrmt", "gt", "flextable", "ggplot2")
kind_of <- function(o) arsbridge:::.classify_output(o)
ids_by_kind <- function(k) {
  hit <- Filter(function(o) identical(kind_of(o), k), spec$outputs)
  vapply(hit, function(o) as.character(o$id), character(1))
}
tab_ids <- ids_by_kind("table")
lst_ids <- ids_by_kind("listing")
fig_ids <- ids_by_kind("figure")
need(length(tab_ids) > 0, sprintf("the fixture has a table output (%s)", tab_ids[1]))
need(length(lst_ids) > 0, sprintf("the fixture has a listing output (%s)", lst_ids[1]))
need(length(fig_ids) > 0, sprintf("the fixture has a figure output (%s)", fig_ids[1]))

say <- function(expr) {
  tryCatch({ force(expr); NA_character_ },
           error = function(e) gsub("[\r\n]+", " ", conditionMessage(e)))
}
asked <- function(m) RENDER_PKGS[vapply(RENDER_PKGS, function(p)
  grepl(paste0("\\b", p, "\\b"), m), logical(1))]

check_needs <- function(label, m, want) {
  need(!is.na(m), paste0(label, " refuses rather than proceeding"))
  got <- asked(m)
  need(setequal(got, want),
       sprintf("%s asks for exactly {%s}%s", label, paste(want, collapse = ", "),
               if (setequal(got, want)) ""
               else sprintf(" -- got {%s}: %s", paste(got, collapse = ", "), m)))
}

## The three single-capability exports pin precision in both directions: each
## names its own package and NONE of the other three.
check_needs("ars_to_tfrmt()",
            say(ars_to_tfrmt(ars_path, ard, tab_ids[1])), "tfrmt")
## The list form must raise the SAME condition once, not skip every output
## with a warning and hand back a list of NULLs.
check_needs("ars_to_tfrmt_list()",
            say(ars_to_tfrmt_list(ars_path, ard)), "tfrmt")
check_needs("ars_render_listing()",
            say(ars_render_listing(ars_path, adam_dir, lst_ids[1])), "gt")
check_needs("ars_render_figure()",
            say(ars_render_figure(ars_path, adam_dir, fig_ids[1])), "ggplot2")

## A table render needs the specification AND the builder, named together so
## the user installs once instead of being sent back twice.
check_needs("ars_render_tlf()",
            say(ars_render_tlf(ars_path, ard, tab_ids[1])), c("tfrmt", "gt"))

## THE non-broadness claim, tested where it actually bites: a composite asked
## for FIGURES ONLY must want ggplot2 and nothing else. It must not drag in the
## table stack for a document that contains no tables. An eager "rendering
## needs all four" guard fails here, which is the point of the check.
check_needs("ars_render_all(figures only)",
            say(ars_render_all(ars_path, ard, adam_dir,
                               file.path(out_dir, "figs.docx"),
                               output_ids = fig_ids)), "ggplot2")
check_needs("ars_render_split(figures only)",
            say(ars_render_split(ars_path, out_dir, adam_dir, ard,
                                 output_ids = fig_ids)), "ggplot2")

## Over the WHOLE event the requirement is inherited from the outputs they
## meet, and which one they reach first depends on the fixture and on whether
## that output has computed results. So the assertion here is bounded rather
## than exact: a non-empty subset, never the whole tier, and never a raw
## namespace error.
for (nm in c("ars_render_all", "ars_render_combined", "ars_render_split")) {
  m <- switch(nm,
    ars_render_all      = say(ars_render_all(ars_path, ard, adam_dir,
                                             file.path(out_dir, "all.docx"))),
    ars_render_combined = say(ars_render_combined(ars_path,
                                             file.path(out_dir, "comb.docx"),
                                             adam_dir, ard)),
    ars_render_split    = say(ars_render_split(ars_path, out_dir, adam_dir, ard)))
  need(!is.na(m), paste0(nm, "() refuses rather than proceeding"))
  got <- asked(m)
  cat("    ", nm, "() asks for {", paste(got, collapse = ", "), "}\n", sep = "")
  need(length(got) > 0, sprintf("%s() names at least one rendering package", nm))
  need(length(got) < length(RENDER_PKGS),
       sprintf("%s() does not demand the whole rendering tier", nm))
  need(!grepl("there is no package called", m, fixed = TRUE),
       sprintf("%s() fails as a capability message, not a namespace load error", nm))
}

## And the docx path names its three up front rather than sending the user
## back for flextable after they have installed tfrmt and gt.
check_needs("ars_render_tlf(format = 'docx')",
            say(ars_render_tlf(ars_path, ard, tab_ids[1], format = "docx",
                               file = file.path(out_dir, "one.docx"))),
            c("tfrmt", "gt", "flextable"))

## ---------------------------------------------------------------------------
step(8, "ellmer is required only when a run would actually call an LLM")
## Deterministic parsing is a first-class mode, so almost every configuration
## must still convert here with no ellmer at all. The one that must NOT is an
## opted-in run with a usable key: silently handing that user regex output of
## lower quality than they asked for is the failure this guards.
LLM_ENV <- c("ANTHROPIC_API_KEY", "OPENAI_API_KEY", "GEMINI_API_KEY",
             "GLM_API_KEY", "ARS_LLM_PROVIDER")
FAKE_KEY <- "sk-ant-not-a-real-key-000000000001"

## Run spec_to_ars under a chosen key/provider environment; return the
## extraction mode on success, or the error message on failure.
convert_under <- function(label, use_llm, env = character(),
                          supplement = NULL) {
  vals <- stats::setNames(rep("", length(LLM_ENV)), LLM_ENV)
  vals[names(env)] <- env
  old <- Sys.getenv(LLM_ENV, unset = NA_character_, names = TRUE)
  do.call(Sys.setenv, as.list(vals))
  on.exit({
    unset <- names(old)[is.na(old)]
    if (length(unset)) Sys.unsetenv(unset)
    keep <- old[!is.na(old)]
    if (length(keep)) do.call(Sys.setenv, as.list(keep))
  }, add = TRUE)

  out <- file.path(out_dir, paste0("llm_", label, ".json"))
  res <- tryCatch({
    suppressMessages(suppressWarnings(spec_to_ars(
      shell_path     = arsbridge_example("annotated_shell.xlsx"),
      adam_spec_path = arsbridge_example("adam_spec.xlsx"),
      output_path    = out, study_id = "CORE-SMOKE-LLM",
      supplement     = supplement,
      use_llm        = use_llm)))
    mode <- jsonlite::fromJSON(out, simplifyVector = FALSE)[["_meta"]][["extraction_mode"]]
    list(ok = TRUE, mode = as.character(mode), path = out)
  }, error = function(e) {
    list(ok = FALSE, msg = gsub("[\r\n]+", " ", conditionMessage(e)))
  })
  res
}

deterministic_ok <- function(label, res) {
  need(isTRUE(res$ok),
       sprintf("%s converts without ellmer%s", label,
               if (isTRUE(res$ok)) "" else paste0(" -- got: ", res$msg)))
  need(identical(res$mode, "deterministic"),
       sprintf("%s runs deterministically (mode = %s)", label,
               if (is.null(res$mode)) "<none>" else res$mode))
}

deterministic_ok("default (no key, no opt-in)", convert_under("a", FALSE))
deterministic_ok("use_llm = TRUE, no key",      convert_under("b", TRUE))
deterministic_ok("preferred provider, no key",
                 convert_under("c", TRUE, c(ARS_LLM_PROVIDER = "anthropic")))
## The override: a real key present, but the run was never going to call out.
deterministic_ok("use_llm = FALSE with a key configured",
                 convert_under("d", FALSE,
                               c(ANTHROPIC_API_KEY = FAKE_KEY,
                                 ARS_LLM_PROVIDER  = "anthropic")))

## ---------------------------------------------------------------------------
## The production path: deterministic parse + an offline supplement, with no
## ellmer, no key and no network. This is the workflow the core install exists
## to serve, so proving it matters more than proving regex alone runs. The
## supplement carries one field the parser does not produce -- the bundled
## shell asks for no Total column on Table 14.1.1 -- so the enrichment landing
## is measurable rather than assumed.
draft <- file.path(out_dir, "draft.json")
suppressMessages(suppressWarnings(write_supplement_draft(
  shell_path     = arsbridge_example("annotated_shell.xlsx"),
  adam_spec_path = arsbridge_example("adam_spec.xlsx"),
  output_path    = draft)))
need(file.exists(draft), "a supplement draft is written without ellmer")

supp_spec <- jsonlite::fromJSON(draft, simplifyVector = FALSE)
supp_spec$tlfs[["T-14-1-1"]]$includeTotal <- TRUE
supp_path <- file.path(out_dir, "supplement.json")
jsonlite::write_json(supp_spec, supp_path, auto_unbox = TRUE, null = "null")

marker_landed <- function(path) {
  s <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  hit <- Filter(function(a) startsWith(as.character(a$id), "AN_T_14_1_1"),
                s$analyses)
  length(hit) > 0 &&
    all(vapply(hit, function(a)
      isTRUE(as.logical(unlist(a$includeTotal)[1])), logical(1)))
}

## The baseline, which is what makes the assertion below mean anything: the
## same shell read deterministically does NOT set that field.
need(!marker_landed(ars_path),
     "the deterministic ARS does not carry the supplement's marker")

supp_run <- convert_under("supp", FALSE, supplement = supp_path)
need(isTRUE(supp_run$ok),
     paste0("a supplement run converts without ellmer",
            if (isTRUE(supp_run$ok)) "" else paste0(" -- got: ", supp_run$msg)))
need(identical(supp_run$mode, "supplement"),
     sprintf("a supplement run reports mode = %s", supp_run$mode))
need(marker_landed(supp_run$path),
     "the supplement's enrichment landed in the ARS (includeTotal on T_14_1_1)")

## Precedence, in the environment where it matters: a supplement is resolved
## before use_llm is consulted, so an opted-in run holding a usable key still
## takes the supplement -- and therefore still needs no ellmer.
supp_optin <- convert_under("supp_optin", TRUE,
                            c(ANTHROPIC_API_KEY = FAKE_KEY,
                              ARS_LLM_PROVIDER  = "anthropic"),
                            supplement = supp_path)
need(isTRUE(supp_optin$ok),
     paste0("a supplement run opted in WITH a key still needs no ellmer",
            if (isTRUE(supp_optin$ok)) "" else paste0(" -- got: ", supp_optin$msg)))
need(identical(supp_optin$mode, "supplement"),
     "the supplement wins over the live LLM")
need(marker_landed(supp_optin$path), "and its enrichment still landed")

## And the one case that must fail, naming ellmer and nothing else.
res <- convert_under("e", TRUE, c(ANTHROPIC_API_KEY = FAKE_KEY,
                                  ARS_LLM_PROVIDER  = "anthropic"))
need(!isTRUE(res$ok),
     sprintf("an opted-in run with a usable key refuses (mode was %s)",
             if (is.null(res$mode)) "<none>" else res$mode))
need(grepl("\\bellmer\\b", res$msg),
     paste0("the refusal names ellmer: ", res$msg))
need(!grepl("there is no package called", res$msg, fixed = TRUE),
     "it fails as a capability message, not a namespace load error")

## The callr path is different by design: it degrades instead of refusing,
## so there is nothing here to catch -- .workflow_start_build() is reachable
## only from inside a shiny session. Its behaviour is pinned by
## tests/testthat/test-optional_callr.R.

cat("\nCORE SMOKE PASSED\n")
