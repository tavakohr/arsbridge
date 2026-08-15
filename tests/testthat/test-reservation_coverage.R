## The safety barrier that replaces the gate.
##
## Until this change, one thing protected every wrong number:
## `.validation_gate()`
## refused the whole reporting event if ANY finding was FAIL. It was coarse and
## it was crude, but it was complete -- a new check that raised FAIL was
## automatically load-bearing, whether or not anyone downstream understood it.
##
## Making findings non-blocking gives that up. Safety moves from "refuse the
## run" to "refuse the cell", and a precise map is only as safe as its coverage:
## a code the map does not reserve for now PERMITS execution where the gate
## prevented it. Under-reservation is silent by construction -- the number is
## produced, it is plausible, and nothing reports that anything was skipped.
##
## So the coverage has to be checked mechanically rather than remembered. This
## file reads every `.add_finding()` call in the package out of the loaded
## namespace's own syntax trees, resolves what severity and what code each one
## emits, and requires that every code the package raises at FAIL either
##
##   (a) reserves -- proven by running the map, not by reading a table; or
##   (b) appears in a short allowlist, each entry of which is justified by an
##       executable test in the second half of this file showing the affected
##       cell produces no value while its neighbours still compute.
##
## Anything the extraction cannot resolve becomes a sentinel that fails. A
## barrier that quietly skips what it does not understand is not a barrier.


## ---------------------------------------------------------------------------
## Reading the call sites out of the namespace.
##
## The namespace rather than the R/ sources: `R CMD check` tests an INSTALLED
## package, where R/*.R does not exist, so a file-reading version of this test
## would silently examine nothing exactly where it is needed most. Syntax trees
## are also the only honest way to do this -- a text scan cannot tell a call
## from the same characters inside a string or a comment.

## What an unresolvable expression becomes. Every assertion below treats it as
## a failure, so a new call site whose severity or code cannot be determined
## turns this file red rather than escaping the audit.
.RCOV_UNRESOLVED <- "<unresolved>"

## Codes reached through a variable rather than written at the call site.
##
## Keyed by the ENCLOSING FUNCTION, not by the text of the expression. A key of
## `"ref"` would mean any future `.add_finding(..., ref = ref)` anywhere in the
## package silently inherited these two codes -- which is the failure mode this
## file exists to prevent, arriving through the file itself.
##
## Each declaration is corroborated against the function's own syntax tree
## below, in both directions, so it cannot drift away from the code it claims
## to describe.
.RCOV_DYNAMIC_REFS <- list(
  ## `ref` is chosen between two literals by whether the group COUNTS differ.
  .check_flat_axes = c("FLAT_AXIS_COLUMN_COUNT_MISMATCH",
                       "FLAT_AXIS_COLUMN_LABEL_MISMATCH"),
  ## `check$ref` iterates the local `reference_checks` table.
  .check_references = c("METHOD_REF_UNRESOLVED",
                        "ANALYSIS_SET_REF_UNRESOLVED",
                        "DATA_SUBSET_REF_UNRESOLVED")
)

## A length-1 character constant, or NULL for anything else.
.rcov_literal <- function(x) {
  if (is.character(x) && length(x) == 1L && !is.na(x)) x else NULL
}

## Walk one syntax tree, calling `visit(call)` on every call to `name`.
##
## Elements are indexed in place and never bound to a variable: a call may hold
## the empty symbol (`x[, 1]`), and a variable bound to it errors the moment it
## is read.
.rcov_walk <- function(expr, name, visit) {
  if (!is.call(expr)) return(invisible(NULL))
  head <- expr[[1]]
  if (is.name(head) && identical(as.character(head), name)) visit(expr)
  for (i in seq_along(expr)) {
    if (identical(expr[[i]], quote(expr = ))) next
    .rcov_walk(expr[[i]], name, visit)
  }
  invisible(NULL)
}

## Every character constant in a syntax tree that is a registered finding code.
.rcov_ref_literals <- function(expr, found = new.env(parent = emptyenv())) {
  if (is.character(expr)) {
    for (value in expr) {
      if (!is.na(value) && value %in% names(.VALIDATION_REFS)) {
        assign(value, TRUE, envir = found)
      }
    }
    return(found)
  }
  if (!is.call(expr)) return(found)
  for (i in seq_along(expr)) {
    if (identical(expr[[i]], quote(expr = ))) next
    .rcov_ref_literals(expr[[i]], found)
  }
  found
}

## Names assigned anywhere inside a syntax tree.
##
## Used to refuse to resolve a symbol against the namespace when the function
## has a local of the same name: a package constant and a local variable that
## share a name are two different values, and guessing between them is exactly
## the kind of silent wrong answer this file is written to prevent.
.rcov_assigned <- function(expr, found = character(0)) {
  if (!is.call(expr)) return(found)
  head <- expr[[1]]
  if (is.name(head) && as.character(head) %in% c("<-", "<<-", "=") &&
      length(expr) >= 2L && is.name(expr[[2]])) {
    found <- c(found, as.character(expr[[2]]))
  }
  for (i in seq_along(expr)) {
    if (identical(expr[[i]], quote(expr = ))) next
    found <- .rcov_assigned(expr[[i]], found)
  }
  unique(found)
}

## Resolve one argument expression to a single character value.
##
## Three ways, in order of how much they can be trusted, and a sentinel when
## none of them applies:
##
##   1. a literal written at the call site;
##   2. a symbol bound in the package namespace to a single string -- exact,
##      and only when the enclosing function has no local of that name;
##   3. the declaration in `.RCOV_DYNAMIC_REFS` for the enclosing function.
##
## (3) may yield several candidates, which is why this returns a vector: a call
## site that can raise either of two codes is audited for both.
.rcov_resolve <- function(expr, fn, body, dynamic = NULL) {
  literal <- .rcov_literal(expr)
  if (!is.null(literal)) return(literal)

  if (is.name(expr)) {
    symbol <- as.character(expr)
    ns <- asNamespace("arsbridge")
    if (!symbol %in% .rcov_assigned(body) &&
        exists(symbol, envir = ns, inherits = FALSE)) {
      value <- .rcov_literal(get(symbol, envir = ns))
      if (!is.null(value)) return(value)
    }
  }

  declared <- dynamic[[fn]]
  if (!is.null(declared)) return(declared)

  .RCOV_UNRESOLVED
}

## Every (function, severity, ref) the package can emit, one row per resolved
## code. `ref_expr` is kept for the failure message: an unresolved site is only
## fixable if the report says what it could not read.
##
## `dynamic` is a parameter rather than a direct read of `.RCOV_DYNAMIC_REFS` so
## the mutation test can prune a declaration and re-run the extraction without
## patching anything -- a mutation that reaches only the copy the test resolves,
## and not the one the code under test resolves, measures nothing.
.rcov_finding_sites <- function(dynamic = .RCOV_DYNAMIC_REFS) {
  ns <- asNamespace("arsbridge")
  rows <- list()

  for (fn in sort(ls(ns, all.names = TRUE))) {
    obj <- get(fn, envir = ns)
    if (!is.function(obj)) next
    fn_body <- body(obj)
    if (is.null(fn_body)) next

    .rcov_walk(fn_body, ".add_finding", function(cl) {
      matched <- match.call(definition = .add_finding, call = cl)
      arg <- function(nm) if (nm %in% names(matched)) matched[[nm]] else NULL

      severity <- .rcov_resolve(arg("severity"), fn, fn_body)
      refs <- .rcov_resolve(arg("ref"), fn, fn_body, dynamic)
      ## Whether the code was WRITTEN here, as opposed to resolved through a
      ## constant or a declaration. The declaration audit needs the difference.
      is_literal <- !is.null(.rcov_literal(arg("ref")))

      for (ref in refs) {
        rows[[length(rows) + 1L]] <<- data.frame(
          fn = fn, severity = severity, ref = ref, ref_is_literal = is_literal,
          sev_expr = paste(deparse(arg("severity")), collapse = " "),
          ref_expr = paste(deparse(arg("ref")), collapse = " "),
          stringsAsFactors = FALSE
        )
      }
    })
  }

  do.call(rbind, rows)
}


## ---------------------------------------------------------------------------
## The four codes that are raised at FAIL and deliberately do not reserve.
##
## Each is here because reserving would DISCARD CORRECT NUMBERS, and each is
## safe only because the fill already refuses the affected cell on its own. The
## second half of this file proves that per-cell refusal for every one of them;
## the justification text is not what makes them safe, the tests are.
.RCOV_JUSTIFIED_NON_RESERVING <- c(
  ## The named analysis is not in the event, so it contributes no rows: the
  ## output shows fewer lines and there is no number to be wrong.
  ANALYSIS_REF_UNRESOLVED = "no such analysis, so nothing computes for it",
  ## A column whose label or position the grouping does not define matches no
  ## ARD row, per cell. Reserving the analysis would empty the columns that DO
  ## match, which are correct.
  FLAT_AXIS_COLUMN_COUNT_MISMATCH = "unmatched columns stay empty per cell",
  FLAT_AXIS_COLUMN_LABEL_MISMATCH = "unmatched columns stay empty per cell",
  ## A placeholder showing more numbers than the method computes binds what it
  ## can; the surplus slot carries no statistic name and can never take a value.
  METHOD_PLACEHOLDER_SLOT_MISMATCH =
    "the surplus slot has no statistic behind it"
)

## Where to raise a synthetic finding of a given scope so the map has something
## to traverse. The ids are the seeded event's, and the entities are the pools
## `.analyses_referencing()` knows.
.RCOV_ENTITY_FOR_SCOPE <- list(
  analysis = list(entity = "analyses",  id = "AN_SYNTH_001"),
  grouping = list(entity = "groupings", id = "GF_SYNTH"),
  output   = list(entity = "outputs",   id = "T_SYNTH"),
  event    = list(entity = "methods",   id = "row 1")
)

## Does this code withhold results? Answered by RUNNING the map, not by reading
## the scope table -- a code correctly marked "analysis" whose traversal is
## broken would pass a table read and reserve nothing in practice.
.rcov_reserves <- function(ref, model) {
  scope <- .scope_for_ref(ref)
  where <- .RCOV_ENTITY_FOR_SCOPE[[scope]] %||%
    list(entity = "analyses", id = "AN_SYNTH_001")

  findings <- .add_finding(
    .new_findings(), "FAIL", where$entity, where$id, "field",
    "synthetic", "synthetic", ref = ref
  )
  length(.reservations_from_findings(model, findings)$by_analysis) > 0L
}


test_that("every .add_finding() call resolves to a known severity and code", {
  sites <- .rcov_finding_sites()

  ## Non-vacuity. An extraction that found nothing would satisfy every "no
  ## unresolved rows" assertion below while auditing an empty set.
  expect_gt(nrow(sites), 30L)

  unresolved_sev <- sites[sites$severity == .RCOV_UNRESOLVED, , drop = FALSE]
  expect_equal(
    nrow(unresolved_sev), 0L,
    info = paste("severity not resolvable at:",
                 paste(unresolved_sev$fn, unresolved_sev$sev_expr,
                       collapse = "; "))
  )

  unresolved_ref <- sites[sites$ref == .RCOV_UNRESOLVED, , drop = FALSE]
  expect_equal(
    nrow(unresolved_ref), 0L,
    info = paste(
      "finding code not resolvable at:",
      paste(unresolved_ref$fn, unresolved_ref$ref_expr, collapse = "; "),
      "-- add the enclosing function to .RCOV_DYNAMIC_REFS"
    )
  )

  ## Every resolved code is a registered one, so a typo in a declaration above
  ## cannot quietly widen the audited set.
  expect_true(all(sites$ref %in% names(.VALIDATION_REFS)))
})


test_that("the dynamic-code declarations match the functions they describe", {
  ns <- asNamespace("arsbridge")
  sites <- .rcov_finding_sites()   # every call site, used by the loop below
  checked <- 0L

  for (fn in names(.RCOV_DYNAMIC_REFS)) {
    expect_true(exists(fn, envir = ns, inherits = FALSE), info = fn)
    fn_body <- body(get(fn, envir = ns))

    present <- ls(.rcov_ref_literals(fn_body))
    declared <- .RCOV_DYNAMIC_REFS[[fn]]

    ## Forwards: a declared candidate that is no longer written in the function
    ## is a declaration describing code that has moved on.
    expect_true(all(declared %in% present), info = paste(fn, "declared"))

    ## Backwards: any OTHER finding code written in the function must be one it
    ## raises literally at a call site. A code that is neither declared nor
    ## written at a call site is a new branch value the declaration missed --
    ## which is the specific way this map goes stale.
    ##
    ## `sites` is the extraction taken at the top of this test.
    literal_here <- unique(sites$ref[sites$fn == fn & sites$ref_is_literal])
    expect_true(all(present %in% union(declared, literal_here)),
                info = paste(fn, "undeclared:",
                             paste(setdiff(present, union(declared,
                                                          literal_here)),
                                   collapse = ", ")))
    checked <- checked + 1L
  }

  expect_equal(checked, length(.RCOV_DYNAMIC_REFS))
})


test_that("every code raised at FAIL reserves, or is justified and proven", {
  sites <- .rcov_finding_sites()
  blocking <- sort(unique(sites$ref[sites$severity == "FAIL"]))

  ## Non-vacuity: FAIL is the severity the gate refused the event on, and the
  ## package raises it in many places. An empty set here would mean the
  ## extraction, not the package, had changed.
  expect_gt(length(blocking), 15L)

  model <- .rmap_model()
  reserves <- vapply(blocking, .rcov_reserves, logical(1), model = model)

  ## The barrier. Every former blocking code either withholds results, or is
  ## one of the four whose per-cell refusal is proven below.
  escaped <- blocking[!reserves &
                        !blocking %in% names(.RCOV_JUSTIFIED_NON_RESERVING)]
  expect_equal(
    length(escaped), 0L,
    info = paste(
      "these codes refused the whole event before, and now withhold nothing:",
      paste(escaped, collapse = ", "),
      "-- give each a reserving scope in .VALIDATION_REFS, or justify it in",
      ".RCOV_JUSTIFIED_NON_RESERVING with a test proving the cell stays empty"
    )
  )

  ## And the allowlist stays honest in the other direction: an entry that has
  ## since started reserving, or that is no longer raised at FAIL at all, is a
  ## justification for something that is not happening.
  stale <- setdiff(names(.RCOV_JUSTIFIED_NON_RESERVING), blocking[!reserves])
  expect_equal(
    length(stale), 0L,
    info = paste("justified but no longer a non-reserving FAIL:",
                 paste(stale, collapse = ", "))
  )
})


test_that("demoting a reserving code to advisory is caught by the barrier", {
  ## Mutation: the exact shape of silent under-reservation. A code that
  ## withholds results is marked advisory -- the finding is still raised, still
  ## reported, still FAIL, and nothing is withheld any more. Under the old gate
  ## this was impossible; under the map it is one word in a table.
  ##
  ## Patched through `.rsv_install()` so the namespace copy the validator
  ## resolves is the one that changes, not only the copy this file sees.
  original <- get(".VALIDATION_REFS", envir = asNamespace("arsbridge"))
  withr::defer(.rsv_restore(".VALIDATION_REFS", original))

  demoted <- original
  demoted[["GROUPING_DATASET_CONFLICT"]] <- "advisory"
  .rsv_install(".VALIDATION_REFS", demoted)

  sites <- .rcov_finding_sites()
  blocking <- sort(unique(sites$ref[sites$severity == "FAIL"]))
  model <- .rmap_model()
  reserves <- vapply(blocking, .rcov_reserves, logical(1), model = model)
  escaped <- blocking[!reserves &
                        !blocking %in% names(.RCOV_JUSTIFIED_NON_RESERVING)]

  expect_true("GROUPING_DATASET_CONFLICT" %in% escaped)
})


test_that("a dropped declaration leaves the code unreadable, not assumed", {
  ## Mutation: a code reached through a variable loses its declaration. The
  ## extraction must report it as unresolved rather than skipping the call site
  ## -- a site the audit cannot read is a site the audit does not cover, and
  ## silently omitting it is how a barrier stops being one.
  pruned <- .RCOV_DYNAMIC_REFS[
    setdiff(names(.RCOV_DYNAMIC_REFS), ".check_references")
  ]
  sites <- .rcov_finding_sites(dynamic = pruned)

  unresolved <- sites[sites$ref == .RCOV_UNRESOLVED, , drop = FALSE]
  expect_gt(nrow(unresolved), 0L)
  expect_true(".check_references" %in% unresolved$fn)

  ## And the codes it used to contribute are gone from the audited set, which
  ## is what the failure above is protecting against.
  expect_false("ANALYSIS_SET_REF_UNRESOLVED" %in% sites$ref)
})


## ---------------------------------------------------------------------------
## The four justifications, proven rather than asserted.
##
## Each shows the same two things, because a justification needs both halves:
## the affected cell yields NO VALUE, and a neighbouring cell of the same kind
## still computes. Without the second half, "reserving would discard correct
## numbers" is a claim about numbers nobody checked exist.
##
## These run against the fill's own matching, one level below the pipeline. That
## is deliberate and is the claim being made: the argument for not reserving is
## not "the run is refused" -- the run is precisely what stops being refused --
## it is "the FILL declines this cell". So the fill's matching is what is
## tested. Both invented vocabularies, so a match that keyed on a familiar name
## would pass under one and fail under the other.

## An ARD carrying one statistic per column level, in the given vocabulary.
.rcov_ard <- function(vocab, analysis_id = "AN_SYNTH_001") {
  levels <- paste0(vocab$arm, c("_1", "_2"))
  data.frame(
    analysis_id    = rep(analysis_id, length(levels)),
    stat_name      = rep("n", length(levels)),
    stat           = c(7, 9),
    result_status  = rep("computed", length(levels)),
    group1_level   = levels,
    variable_level = NA_character_,
    stringsAsFactors = FALSE
  )
}


test_that("a surplus placeholder slot has no statistic, so it stays empty", {
  ## METHOD_PLACEHOLDER_SLOT_MISMATCH. The shell shows "xx (xx.x)" -- a count
  ## and a percentage -- over a method that declares only the count.
  checked <- 0L

  for (nm in names(.RSV_VOCABS)) {
    vocab <- .RSV_VOCABS[[nm]]
    method <- .rsv_counting_method()
    operations <- .method_operation_slots(list(method), method$id)
    slots <- .bind_slots(.parse_placeholder("xx (xx.x)")$slots, operations)

    ## Two slots authored, one statistic available.
    expect_length(slots, 2L)
    expect_false(is.na(slots[[1]]$stat_name), info = nm)
    expect_true(is.na(slots[[2]]$stat_name), info = nm)

    index <- .ard_index(.rcov_ard(vocab))
    level <- paste0(vocab$arm, "_1")

    ## The surplus slot cannot take a value, whatever the ARD holds.
    surplus <- .ard_value(index, "AN_SYNTH_001", level, NA_character_,
                          slots[[2]]$stat_name)
    expect_false(identical(surplus$status, "computed"), info = nm)
    expect_true(is.na(surplus$value), info = nm)

    ## The bound slot does -- which is the number reserving would have thrown
    ## away.
    bound <- .ard_value(index, "AN_SYNTH_001", level, NA_character_,
                        slots[[1]]$stat_name)
    expect_identical(bound$status, "computed", info = nm)
    expect_false(is.na(bound$value), info = nm)

    checked <- checked + 1L
  }

  expect_equal(checked, length(.RSV_VOCABS))
})


test_that("an undefined column stays empty while its siblings fill", {
  ## FLAT_AXIS_COLUMN_COUNT_MISMATCH and FLAT_AXIS_COLUMN_LABEL_MISMATCH. Both
  ## reach the fill the same way -- a displayed column whose label answers to no
  ## group level -- whether the shell shows too many columns or the wrong ones.
  checked <- 0L

  for (nm in names(.RSV_VOCABS)) {
    vocab <- .RSV_VOCABS[[nm]]
    index <- .ard_index(.rcov_ard(vocab))

    ## A surplus column: the shell displays a third when the grouping defines
    ## two.
    surplus <- .ard_value(index, "AN_SYNTH_001", paste0(vocab$arm, "_3"),
                          NA_character_, "n")
    expect_false(identical(surplus$status, "computed"), info = nm)
    expect_true(is.na(surplus$value), info = nm)

    ## A mislabelled column: the right number of columns, one carrying a label
    ## the grouping never defined.
    wrong_label <- .ard_value(index, "AN_SYNTH_001",
                              paste0(vocab$arm, "_other"), NA_character_, "n")
    expect_false(identical(wrong_label$status, "computed"), info = nm)
    expect_true(is.na(wrong_label$value), info = nm)

    ## Every column that DOES match still computes. This is the whole reason
    ## these two codes do not reserve.
    for (suffix in c("_1", "_2")) {
      hit <- .ard_value(index, "AN_SYNTH_001", paste0(vocab$arm, suffix),
                        NA_character_, "n")
      expect_identical(hit$status, "computed", info = paste(nm, suffix))
      expect_false(is.na(hit$value), info = paste(nm, suffix))
    }

    checked <- checked + 1L
  }

  expect_equal(checked, length(.RSV_VOCABS))
})


test_that("an output naming a missing analysis displays nothing for it", {
  ## ANALYSIS_REF_UNRESOLVED. There is no analysis, so there is no computation
  ## to withhold -- the line is absent rather than wrong.
  checked <- 0L

  for (nm in names(.RSV_VOCABS)) {
    vocab <- .RSV_VOCABS[[nm]]
    index <- .ard_index(.rcov_ard(vocab))
    level <- paste0(vocab$arm, "_1")

    missing <- .ard_value(index, "AN_NOT_IN_EVENT", level, NA_character_, "n")
    expect_false(identical(missing$status, "computed"), info = nm)
    expect_true(is.na(missing$value), info = nm)

    present <- .ard_value(index, "AN_SYNTH_001", level, NA_character_, "n")
    expect_identical(present$status, "computed", info = nm)

    ## And there is nothing for the map to reserve: no analysis in the event
    ## resolves through the dangling id.
    expect_length(
      .analyses_referencing(.rmap_model(vocab), "analyses", "AN_NOT_IN_EVENT"),
      0L
    )

    checked <- checked + 1L
  }

  expect_equal(checked, length(.RSV_VOCABS))
})
