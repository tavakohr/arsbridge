## The golden gate: the whole converted reporting event, pinned.
##
## Focused tests pin behaviours -- a grouping resolves, a denominator counts
## the right subjects. What none of them pin is the DOCUMENT: a change in
## groupings, conditions, analysis sets, output references, methods, columns or
## result metadata that no single test happens to assert passes the suite and
## ships. That is not hypothetical. Changing the deliverable filename these
## outputs declare, from `T-14-1-1.rtf` to `T_14_1_1.rtf`, leaves 5843 tests
## green; only `fileType` was ever asserted, never the name.
##
## Comparison is structural on a canonical form, never byte-for-byte: a change
## in how jsonlite indents is not a converter change, and a gate that fails on
## one gets muted. What canonicalisation removes is ordering noise and two
## volatile fields -- nothing else. See helper-goldens.R.

skip_if_not_installed("withr")
skip_if_not_installed("waldo")

## Enough of a diff to see the shape of a break, capped so a wholesale change
## prints a readable head instead of a hundred thousand lines of log.
.GOLDEN_MAX_DIFFS <- 40L

## One conversion per case for the whole file: building these is the expensive
## part, and every test below asks a different question of the same document.
.golden_built <- local({
  cache <- list()
  function(case) {
    if (is.null(cache[[case$name]])) {
      cache[[case$name]] <<- .build_golden_ars(case, envir = globalenv())
    }
    cache[[case$name]]
  }
})

for (case in .golden_cases()) {
  local({
    this <- case

    test_that(paste0("the converted reporting event matches its golden: ",
                     this$name), {
      skip_if_not_installed(this$needs)

      actual <- .golden_roundtrip(.ars_canonical(.golden_built(this),
                                                 this$name))
      expected <- .read_golden(this$name)

      difference <- waldo::compare(expected, actual,
                                   x_arg = "golden", y_arg = "converted",
                                   max_diffs = .GOLDEN_MAX_DIFFS)

      expect(
        length(difference) == 0,
        paste0(
          "The converted reporting event no longer matches goldens/",
          this$name, ".json.\n",
          "If this change is intended, run data-raw/regenerate_goldens.R and ",
          "explain the diff in the PR.\n\n",
          paste(difference, collapse = "\n\n")
        )
      )
    })

    test_that(paste0("no absolute or temporary path reaches the output: ",
                     this$name), {
      ## Path-independence is what makes a committed golden meaningful at all:
      ## the same inputs, converted from another directory or another machine,
      ## must produce the same document. It holds today as an observed
      ## property; this is what keeps it true. Asserted OUTSIDE
      ## canonicalisation on purpose -- scrubbing paths during normalisation
      ## would keep the gate green while the property it rests on was gone.
      skip_if_not_installed(this$needs)
      expect_no_error(.assert_no_paths(.golden_built(this), this$name))
    })
  })
}

# ---- the canonicaliser itself ----------------------------------------------
#
# A golden gate is only as honest as its normaliser. These pin that it removes
# what it claims to and nothing else.

## Both `meta` and the named collections REPLACE what they name; nothing here
## merges. modifyList() cannot be used for either: it re-supplies a `_meta`
## field a test is asserting is absent, and for these unnamed lists-of-lists it
## recurses, finds no names to update, and hands back the ORIGINAL -- so every
## malformed case below would silently be built well-formed and pass for the
## wrong reason.
.golden_toy <- function(...,
                        meta = list(
                          generator        = "arsbridge 0.1.0.9114",
                          generated_at_utc = "2026-08-12T09:00:00Z",
                          extraction_mode  = "deterministic")) {
  base <- list(
    analysisSets = list(list(id = "AS_B"), list(id = "AS_A")),
    methods      = list(list(id = "MTH_ONE")),
    analyses     = list(list(id = "AN_1", orderedGroupings = list(
      list(order = 1L, groupingId = "GF_TRT"),
      list(order = 2L, groupingId = "GF_SOC")))),
    outputs      = list(list(id = "T_1",
                             referencedAnalysisIds = list("AN_1", "AN_2")))
  )
  replacements <- list(...)
  for (nm in names(replacements)) base[[nm]] <- replacements[[nm]]
  base[["_meta"]] <- meta
  base
}

test_that("only the two volatile fields are rewritten", {
  canonical <- .ars_canonical(.golden_toy())

  expect_identical(canonical[["_meta"]][["generator"]], "arsbridge <VERSION>")
  expect_identical(canonical[["_meta"]][["generated_at_utc"]],
                   "<GENERATED_AT_UTC>")
  ## Substituted, not dropped: a converter that stopped emitting a generator
  ## must fail the comparison rather than match a golden that also lacks one.
  expect_true("generator" %in% names(canonical[["_meta"]]))
  ## Every other decision in _meta is compared, not normalised away.
  expect_identical(canonical[["_meta"]][["extraction_mode"]], "deterministic")
})

test_that("normalisation is the identity outside its allowlist", {
  ## The guard that matters: if canonicalisation touched anything else, a real
  ## change could be normalised into agreement with the golden.
  plain     <- .ars_canonical(.golden_toy())
  perturbed <- .ars_canonical(.golden_toy(
    methods = list(list(id = "MTH_ONE", name = "changed"))))

  expect_false(identical(plain, perturbed))
  ## ...and the difference is exactly where it was made.
  expect_identical(plain[["_meta"]], perturbed[["_meta"]])
})

test_that("id-keyed collections are sorted, semantic order is not", {
  canonical <- .ars_canonical(.golden_toy())

  expect_identical(
    vapply(canonical$analysisSets, function(x) x$id, character(1)),
    c("AS_A", "AS_B"))

  ## Element 1 of orderedGroupings IS the column axis, and referenced analysis
  ## ids are display row order. Sorting either would erase a transposed table
  ## or a reordered display -- the failures this gate exists to catch.
  expect_identical(
    vapply(canonical$analyses[[1]]$orderedGroupings,
           function(g) g$groupingId, character(1)),
    c("GF_TRT", "GF_SOC"))
  expect_identical(unlist(canonical$outputs[[1]]$referencedAnalysisIds),
                   c("AN_1", "AN_2"))
})

test_that("a malformed generator or timestamp stops normalisation", {
  ## Substituting a sentinel into a field nobody checked would hide the change
  ## worth seeing.
  expect_error(
    .ars_canonical(.golden_toy(meta = list(
      generator = "handwritten",
      generated_at_utc = "2026-08-12T09:00:00Z"))),
    "not the expected form")

  expect_error(
    .ars_canonical(.golden_toy(meta = list(
      generator = "arsbridge 0.1.0.9114",
      generated_at_utc = "12 August 2026, 9am"))),
    "not an ISO-8601 UTC stamp")

  ## A generator that is not emitted at all, rather than one that changed.
  expect_error(
    .ars_canonical(.golden_toy(meta = list(
      generated_at_utc = "2026-08-12T09:00:00Z"))),
    "missing or not a scalar")

  expect_error(.ars_canonical(.golden_toy(meta = NULL)), "no `_meta` block")
})

test_that("missing or duplicated ids stop normalisation", {
  ## Sorting by id assumes the id identifies the element. Two entries sharing
  ## one id could swap between runs behind a stable-looking golden -- and a
  ## grouping-id collision has shipped here before, so this is a real defect
  ## to report rather than an input to tidy up.
  expect_error(
    .ars_canonical(.golden_toy(
      analysisSets = list(list(id = "AS_A"), list(id = "AS_A")))),
    "duplicate id")

  expect_error(
    .ars_canonical(.golden_toy(
      analysisSets = list(list(id = "AS_A"), list(name = "no id here")))),
    "no id")
})

test_that("the path assertion catches a leaked path anywhere in the document", {
  expect_error(
    .assert_no_paths(.golden_toy(
      outputs = list(list(id = "T_1",
                          fileSpecifications = list(list(
                            name = "/var/folders/xy/T/build/T-1.rtf")))))),
    "absolute or temporary path")

  expect_error(
    .assert_no_paths(.golden_toy(
      analyses = list(list(id = "AN_1",
                           description = "C:\\Users\\hamid\\shell.docx")))),
    "absolute or temporary path")

  ## A study label that merely contains a slash is not a path.
  expect_no_error(
    .assert_no_paths(.golden_toy(
      methods = list(list(id = "MTH_ONE", name = "mg/dL change")))))
})
