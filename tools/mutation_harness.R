## Mutation harness: prove a test actually catches the defect it is named for.
##
## The protocol this implements is in CLAUDE.md, and the reason it is a
## committed script rather than a habit is a run that went wrong. A mutation
## table built inline once got its escaping mangled; every anchor silently
## missed, every mutant ran an UNMUTATED source, and the run reported
## "0 failures" for all of them -- which reads as "the tests do not catch
## this", the direction that costs you a real gap. A mutation suite that can
## test an unchanged source is worse than no mutation evidence, because it
## manufactures confidence.
##
## So this harness refuses to report a verdict it cannot stand behind:
##
##   * APPLIED-CHECK. After patching, the file must differ from its backup.
##     If it does not, the anchor missed and the mutation is UNRESOLVED --
##     never "not detected".
##   * EXACT ANCHORS. A pattern matching zero times, or more than once, is an
##     error rather than a silent no-op or a scattergun edit.
##   * RESTORE, PROVEN. Every mutation is undone from its own backup and the
##     restore is verified byte-for-byte, in `finally` so an error on the way
##     through cannot leave the source mutated.
##   * ONLY FAILURES COUNT AS DETECTION. A mutant that ERRORS has not been
##     caught by a test making a claim about it; something blew up before the
##     claim could be compared -- typically an unguarded index in the test.
##     That is a gap in the test, not evidence about the mutant, so an errored
##     mutant is UNRESOLVED too. Guard the assertion and run again.
##
## Mutate the SOURCE FILE on disk. Do not inject a mutant binding with
## `unlockBinding()` / `assign(envir = asNamespace())`: `testthat::test_file()`
## can reload the package mid-run and silently restore the original, so a
## later file runs unmutated and reports a false negative.
##
## Usage, from the package root:
##
##   Rscript tools/mutation_harness.R <spec.R>
##
## where <spec.R> assigns `mutations` and `test_files`:
##
##   mutations <- list(
##     list(id = "M1", file = "R/foo.R",
##          from = "if (is.null(flat)) return(list(compound = where))",
##          to   = "if (is.null(flat)) return(list())"))
##   test_files <- c("tests/testthat/test-foo.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop("usage: Rscript tools/mutation_harness.R <spec.R>", call. = FALSE)
}
spec_env <- new.env(parent = globalenv())
sys.source(normalizePath(args[[1]]), envir = spec_env)

mutations  <- get("mutations",  envir = spec_env)
test_files <- get("test_files", envir = spec_env)

## One backup per SOURCE PATH, not per basename: two files called `utils.R` in
## different directories would otherwise share a backup and restore each
## other's contents.
sources <- unique(vapply(mutations, function(m) m$file, character(1)))
backups <- vapply(sources, function(src) tempfile(fileext = ".Rbak"),
                  character(1))
names(backups) <- sources
for (src in sources) {
  if (!file.copy(src, backups[[src]], overwrite = TRUE)) {
    stop("could not back up ", src, call. = FALSE)
  }
}

same_file <- function(a, b) {
  identical(readBin(a, "raw", file.size(a)), readBin(b, "raw", file.size(b)))
}

restore_all <- function() {
  for (src in sources) {
    file.copy(backups[[src]], src, overwrite = TRUE)
    if (!same_file(backups[[src]], src)) {
      stop("RESTORE FAILED for ", src, " -- the working tree is left mutated.",
           call. = FALSE)
    }
  }
}

## Exactly one occurrence, or this is not a mutation anyone can reason about.
apply_mutation <- function(m) {
  txt <- readChar(m$file, file.size(m$file), useBytes = TRUE)
  found <- gregexpr(m$from, txt, fixed = TRUE)[[1]]
  hits <- if (found[[1]] == -1L) 0L else length(found)
  if (hits != 1L) {
    stop(sprintf("anchor for %s matched %d times in %s", m$id, hits, m$file),
         call. = FALSE)
  }
  writeChar(sub(m$from, m$to, txt, fixed = TRUE), m$file, eos = NULL,
            useBytes = TRUE)
}

run_tests <- function() {
  failed <- 0L; errored <- 0L; caught <- character(0); blew_up <- character(0)
  for (f in test_files) {
    res <- suppressWarnings(testthat::test_file(f, reporter = "silent"))
    d <- as.data.frame(res)
    failed  <- failed  + sum(d$failed)
    errored <- errored + sum(d$error)
    ## `which()` rather than a logical index: a summary row with an NA count
    ## would otherwise select an NA and print a nameless phantom entry, which
    ## reads as a test that caught the mutant when none did.
    named <- function(idx) {
      nm <- d$test[idx]
      nm <- nm[!is.na(nm) & nzchar(nm)]
      if (length(nm) == 0) character(0) else paste0(basename(f), " :: ", nm)
    }
    caught  <- c(caught,  named(which(d$failed > 0)))
    blew_up <- c(blew_up, named(which(d$error > 0)))
  }
  list(failed = failed, errored = errored, caught = caught, blew_up = blew_up)
}

result <- tryCatch({
  suppressMessages(pkgload::load_all(".", quiet = TRUE))
  base <- run_tests()
  if (base$failed + base$errored > 0) {
    stop("the suite is RED before any mutation -- fix that first, because a ",
         "red baseline makes every verdict below meaningless.", call. = FALSE)
  }
  cat("baseline: green\n\n")

  verdicts <- list()
  for (m in mutations) {
    apply_mutation(m)

    ## The check this harness exists for.
    if (same_file(backups[[m$file]], m$file)) {
      cat(sprintf("%-5s NOT APPLIED -- anchor missed; no verdict\n\n", m$id))
      verdicts[[m$id]] <- "not-applied"
      next
    }

    suppressMessages(pkgload::load_all(".", quiet = TRUE))
    got <- run_tests()
    restore_all()

    ## Only a FAILURE is a test making its claim and finding it false.
    verdicts[[m$id]] <- if (got$errored > 0) {
      "ERRORED"
    } else if (got$failed > 0) {
      "detected"
    } else {
      "NOT DETECTED"
    }
    cat(sprintf("%-5s %-12s failures=%d errors=%d\n", m$id,
                verdicts[[m$id]], got$failed, got$errored))
    for (t in got$caught) cat("        caught by: ", t, "\n", sep = "")
    for (t in got$blew_up) cat("        ERRORED  : ", t, "\n", sep = "")
    if (got$errored > 0) {
      cat("        A mutant that errors was not caught by a claim -- an\n",
          "        assertion blew up before it could compare anything,\n",
          "        usually an unguarded index. Guard it and run again.\n",
          sep = "")
    }
    cat("\n")
  }
  verdicts
}, finally = restore_all())

restore_all()
suppressMessages(pkgload::load_all(".", quiet = TRUE))

bad <- names(result)[vapply(result, function(v) v != "detected", logical(1))]
if (length(bad)) {
  cat("UNRESOLVED: ", paste(bad, collapse = ", "), "\n", sep = "")
  quit(status = 1)
}
cat(sprintf("all %d mutations detected; every source restored and verified\n",
            length(result)))
