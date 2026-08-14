# The canonical subject-key CALCULATION invariant.
#
# arsbridge computes an output's ARD by emitting {cards} blocks and evaluating
# them in memory -- it does not read back the .R file ars_to_code() writes. So
# there are two assemblies of one generator, and nothing so far proved they
# agree. This file pins that agreement on the artifact that Step 4 will
# eventually make authoritative:
#
#   saved ARS -> ars_to_code() -> written .R file -> execute it
#             -> compare against ars_to_ard()
#
# It deliberately executes the FILE. Calling .emit_tlf_script() and diffing two
# in-memory products would prove only that one function equals itself.
#
# The invariant is a SUBSET relation, not equality. Measuring it is what showed
# why: on T_14_1_1 the reference engine produces 18 rows the written program
# does not, and they are real computed results rather than metadata --
# .complete_zero_groups() states an arm the subset filter emptied as an
# explicit zero. So what is locked is:
#
#   * every row the program produces matches a reference row on the canonical
#     union identity, and every compared calculation column is identical;
#   * the program produces ZERO rows the reference does not have;
#   * every reference-only row belongs to a separately characterised arsbridge
#     post-processing class -- today exactly one such class exists.
#
# "program calculations SUBSET-OF reference calculations", with exact equality
# on every shared row and a fully characterised completion layer on top.
#
# Who should own that completion once programs drive execution is deliberately
# NOT settled here; it is an open Step 3 design question.
#
# Scope: table outputs. Listings are direct extracts that never enter an ARD,
# and figures have no emitter yet -- both produce no `ard_` object at all, which
# this file asserts rather than assumes. They belong to Step 8.
#
# What this file does NOT do: define a Result contract, read `blk_*` objects as
# though they were an interface, or touch how execution works. It characterises
# today's artifact.

## ---- comparison machinery --------------------------------------------------

## The complete stable row identity: every {cards} structural column, taken
## over the UNION of what either side carries. The reference frame is bound
## across all outputs, so it has group2/group3 columns even for an output with
## one grouping; keying each side on its own columns compares six fields
## against eight and matches nothing. An absent column and an all-NA column
## therefore have to mean the same thing -- "no second grouping" is what both
## of them say.
.epc_id_cols <- c("group1", "group1_level", "group2", "group2_level",
                  "group3", "group3_level", "variable", "variable_level",
                  "context", "stat_name")

## Columns compared for equality. The 11 metadata columns ars_to_ard() stamps
## (analysis_id, analysis_descr, method_id, output_id, method_intended,
## method_actual, result_status, value_source, derivation_ref, derived_by,
## derived_dt) are excluded because the program does not produce them at all --
## they are the enrichment layer sitting on top of these rows, and that
## boundary is Step 3's subject. derived_dt is additionally a wall-clock stamp,
## so it could never be compared even once the rest is.
.epc_value_cols <- c("stat", "stat_label", "fmt_fun", "warning", "error")

## Stat names a zero-group completion is allowed to introduce.
.epc_completion_stats <- c("n", "N", "p")

## Length-prefix a string so a concatenation of several stays decodable.
## Plain joining is ambiguous -- ("AB", "C") and ("A", "BC") give one string --
## which would either invent a duplicate identity or pair a program row with
## the wrong reference row, making every assertion built on the join quietly
## wrong instead of red.
.epc_lp <- function(x) paste0(nchar(x, type = "bytes"), ":", x)

## A {cards} column may be a list-column, and fmt_fun may hold a closure.
## Flatten to text so identity and equality are both well defined. The value is
## TYPE-TAGGED: a genuinely missing value must not collide with a string that
## merely reads like one, or a row whose label is the literal text "<NA>" would
## match a row that has no label at all.
.epc_flat <- function(x, n) {
  if (is.null(x)) return(rep("<missing>", n))
  vapply(x, function(v) {
    if (is.function(v)) {
      return(paste0("fn|", paste(deparse(v), collapse = " ")))
    }
    v <- unlist(v)
    if (is.null(v) || !length(v) || (length(v) == 1L && is.na(v))) {
      "<missing>"
    } else {
      paste0("chr|", paste(.epc_lp(as.character(v)), collapse = ""))
    }
  }, character(1))
}

.epc_idkey <- function(d, cols) {
  parts <- lapply(cols, function(cc) .epc_lp(.epc_flat(d[[cc]], nrow(d))))
  do.call(paste, c(parts, sep = "|"))
}

## Full join on the row identity, order-independent. Reports which side each
## unmatched row came from and which identities differ in value, so a failure
## names the cell rather than the count.
.epc_compare <- function(prog, ref) {
  cols <- .epc_id_cols[.epc_id_cols %in% union(names(prog), names(ref))]
  pk <- .epc_idkey(prog, cols)
  rk <- .epc_idkey(ref, cols)

  shared <- intersect(pk, rk)
  pi <- match(shared, pk)
  ri <- match(shared, rk)

  mismatched <- list()
  for (cc in .epc_value_cols) {
    a <- .epc_flat(prog[[cc]], nrow(prog))[pi]
    b <- .epc_flat(ref[[cc]], nrow(ref))[ri]
    differs <- which(a != b)
    if (length(differs)) {
      mismatched[[cc]] <- data.frame(
        identity = shared[differs], program = a[differs],
        reference = b[differs], stringsAsFactors = FALSE)
    }
  }

  list(
    id_cols        = cols,
    dup_program    = sum(duplicated(pk)),
    dup_reference  = sum(duplicated(rk)),
    program_only   = prog[!pk %in% rk, , drop = FALSE],
    reference_only = ref[!rk %in% pk, , drop = FALSE],
    n_shared       = length(shared),
    mismatched     = mismatched
  )
}

## Compact failure text: the output, the identity, and both values.
.epc_describe <- function(cmp, output_id) {
  bits <- character()
  if (nrow(cmp$program_only)) {
    bits <- c(bits, sprintf("%s: %d row(s) only the program produced, e.g. %s",
                            output_id, nrow(cmp$program_only),
                            .epc_idkey(cmp$program_only, cmp$id_cols)[[1]]))
  }
  if (nrow(cmp$reference_only)) {
    bits <- c(bits, sprintf("%s: %d row(s) only the reference produced, e.g. %s",
                            output_id, nrow(cmp$reference_only),
                            .epc_idkey(cmp$reference_only, cmp$id_cols)[[1]]))
  }
  for (cc in names(cmp$mismatched)) {
    d <- cmp$mismatched[[cc]]
    bits <- c(bits, sprintf("%s: %s differs on %d row(s), e.g. %s: %s vs %s",
                            output_id, cc, nrow(d), d$identity[[1]],
                            d$program[[1]], d$reference[[1]]))
  }
  paste(bits, collapse = " | ")
}

## ---- both sides, built once -------------------------------------------------

.epc_cache <- new.env(parent = emptyenv())

## The golden ARS is a saved, checked-in reporting event -- exactly the input
## ars_to_code() is specified to take, and already pinned by the golden tests,
## so this file cannot drift from it silently.
.epc_ars <- function() test_path("goldens/alz_docx.json")

## Deliberately NOT withr::local_tempdir(): the cache outlives the test that
## first fills it, so a directory tied to that test's frame would be deleted
## while later tests still held its path. Rebuilt whenever it has gone missing,
## so the cache can never hand back a dead path; session tempdir cleanup
## removes it at the end.
.epc_scratch <- function(name) {
  dir <- file.path(tempdir(), name)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

.epc_adam <- function() {
  dir <- .epc_scratch("epc_adam")
  if (!length(list.files(dir))) {
    utils::unzip(arsbridge_example("ADaM.zip"), exdir = dir)
  }
  dir
}

## Execute the written file. The emitted script sets `adam_dir <- "."` (by
## design -- ars_to_code() takes no adam_dir), so the ADaM folder IS the
## working directory. Evaluated in a fresh environment; the safest mechanism
## available today, and deliberately not callr.
##
## Exactly one `ard_` object, or none. None is a placeholder script (listing or
## figure). More than one has no defined answer -- silently taking the first
## would let the comparison run against an arbitrary intermediate and report
## agreement that was never checked.
.epc_run_file <- function(path, adam_dir) {
  env <- new.env(parent = globalenv())
  withr::with_dir(adam_dir, sys.source(path, envir = env))
  objs <- ls(env, pattern = "^ard_")
  if (length(objs) == 0L) return(NULL)
  if (length(objs) > 1L) {
    stop(sprintf(
      "%s defines %d ard_ objects (%s); the equivalence comparison needs exactly one.",
      basename(path), length(objs), paste(objs, collapse = ", ")))
  }
  get(objs[[1]], envir = env)
}

.epc_both <- function() {
  if (is.null(.epc_cache$both) || !dir.exists(.epc_cache$both$adam)) {
    adam  <- .epc_adam()
    ars   <- .epc_ars()
    code  <- .epc_scratch("epc_code")
    paths <- ars_to_code(ars, code_dir = code, overwrite = TRUE)
    executed <- lapply(paths, function(p) .epc_run_file(p, adam))
    names(executed) <- names(paths)
    ref <- suppressMessages(suppressWarnings(ars_to_ard(ars, adam)))
    .epc_cache$both <- list(paths = paths, executed = executed, ref = ref,
                            ars = ars, adam = adam)
  }
  .epc_cache$both
}

.epc_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
}

## Outputs the reference engine actually produced ARD rows for -- the tables.
.epc_table_outputs <- function(b) {
  intersect(names(b$executed)[!vapply(b$executed, is.null, logical(1))],
            unique(b$ref$output_id))
}

## ---- the identity encoding --------------------------------------------------

test_that("the row identity encoding is injective", {
  ## If two different rows can produce one key, the join pairs the wrong rows
  ## and every assertion below is quietly wrong rather than red. Three ways
  ## that could happen, each ruled out.
  cols <- c("variable", "variable_level")
  key  <- function(a, b) {
    d <- data.frame(variable = a, stringsAsFactors = FALSE)
    d$variable_level <- b
    .epc_idkey(d, cols)
  }

  ## A value shifted across the field boundary.
  expect_false(identical(key("AB", "C"), key("A", "BC")))
  ## The separator forged inside a value.
  expect_false(identical(key("A|B", "C"), key("A", "B|C")))
  ## A real missing value against text that merely reads like one.
  expect_false(identical(key("X", NA_character_), key("X", "<missing>")))
  expect_false(identical(key("X", NA_character_), key("X", "<NA>")))

  ## The one collapse that IS intended: a column the frame does not have means
  ## the same as a column of NA. The reference frame carries group2 for every
  ## output because it is bound across all of them.
  absent  <- data.frame(variable = "X", stringsAsFactors = FALSE)
  present <- data.frame(variable = "X", stringsAsFactors = FALSE)
  present$variable_level <- NA_character_
  expect_identical(.epc_idkey(absent, cols), .epc_idkey(present, cols))
})

## ---- the invariant ----------------------------------------------------------

test_that("only table outputs yield an executable analysis program", {
  ## Asserted, not assumed: the scope boundary of this file is a property of
  ## the artifact. A listing is extracted directly from ADaM and never enters
  ## an ARD; a figure has no emitter yet. Both write a placeholder script with
  ## no `ard_` object. If either ever gains one, this fails and Step 8 has
  ## started whether or not anyone meant it to.
  .epc_skip()
  b <- .epc_both()

  empty <- names(b$executed)[vapply(b$executed, is.null, logical(1))]
  expect_setequal(empty, c("L_16_2_4_1", "L_16_2_7_1", "F_14_3_1"))
  expect_setequal(.epc_table_outputs(b),
                  c("T_14_1_1", "T_14_1_2", "T_14_2_1", "T_14_3_1", "T_14_3_2"))
})

test_that("every calculation the written program makes matches the reference", {
  ## The invariant, stated as the subset relation it is: no row identity is
  ## duplicated on either side (or the join would silently pair one row with
  ## an arbitrary twin), the program invents NOTHING the reference does not
  ## have, and every row both sides carry agrees on every compared column.
  ##
  ## It deliberately does not assert the reverse inclusion. The reference adds
  ## rows of its own, and the next test characterises exactly which.
  .epc_skip()
  b <- .epc_both()

  for (oid in .epc_table_outputs(b)) {
    ref <- b$ref[b$ref$output_id == oid, , drop = FALSE]
    cmp <- .epc_compare(b$executed[[oid]], ref)
    info <- .epc_describe(cmp, oid)

    expect_equal(cmp$dup_program, 0L, info = oid)
    expect_equal(cmp$dup_reference, 0L, info = oid)
    expect_equal(nrow(cmp$program_only), 0L, info = info)
    expect_equal(length(cmp$mismatched), 0L, info = info)
    expect_gt(cmp$n_shared, 0L)
  }
})

test_that("every reference-only row belongs to the zero-completion class", {
  ## The other half of the invariant. Reference-only rows are permitted, but
  ## only if each belongs to a characterised arsbridge post-processing class.
  ## Exactly one such class exists today: when a subset filter empties an arm,
  ## that arm is ABSENT from the cards result and .complete_zero_groups()
  ## states it as an explicit zero. The emitted program has no such step.
  ##
  ## These are real computed results, not metadata -- which is why the Step 3
  ## boundary cannot be described as "no calculation happens here" until
  ## someone decides who owns this completion.
  ##
  ## Checked semantically AND against the exact fixture count. The semantic
  ## checks would still pass if the completion layer grew or shrank; the count
  ## would not. A change there has to be understood, not absorbed.
  .epc_skip()
  b <- .epc_both()

  extras <- lapply(.epc_table_outputs(b), function(oid) {
    ref <- b$ref[b$ref$output_id == oid, , drop = FALSE]
    .epc_compare(b$executed[[oid]], ref)$reference_only
  })
  names(extras) <- .epc_table_outputs(b)
  counts <- vapply(extras, nrow, integer(1))

  ## The fixture gate: on this bundled acceptance study the completion layer
  ## is 18 rows on T_14_1_1 and nothing anywhere else.
  expect_equal(counts[["T_14_1_1"]], 18L)
  expect_equal(sum(counts[names(counts) != "T_14_1_1"]), 0L)

  for (oid in names(extras)) {
    extra <- extras[[oid]]
    if (!nrow(extra)) next

    stats  <- as.character(extra$stat_name)
    values <- suppressWarnings(as.numeric(unlist(extra$stat)))

    ## No stat type beyond the completion vocabulary may appear at all -- an
    ## unexpected one would mean the program failed to compute something real,
    ## so the names are checked as a set rather than filtered down to the ones
    ## that suit the assertion.
    expect_true(all(stats %in% .epc_completion_stats),
                info = sprintf("%s: unexpected reference-only stat(s): %s", oid,
                               paste(setdiff(stats, .epc_completion_stats),
                                     collapse = ",")))
    ## The count and percentage are zero -- that is what "completed" means.
    expect_true(all(values[stats %in% c("n", "p")] == 0),
                info = sprintf("%s: a reference-only row carries a non-zero count",
                               oid))
    ## The denominator is real, so the zero is a zero out of something.
    expect_true(all(values[stats == "N"] > 0),
                info = sprintf("%s: a completed row has an empty denominator", oid))
    ## They are computed rows, from the same population the percentages use --
    ## not reserved cells awaiting a manual fill.
    expect_setequal(unique(extra$result_status), "computed")
  }
})

test_that("the comparison catches drift it is supposed to catch", {
  ## Negative controls. Without these, every assertion above would also pass
  ## against a comparison that quietly matched nothing -- which is exactly the
  ## bug an earlier draft of this file had, when a column absent on one side
  ## made every identity unique and the value checks became vacuous.
  .epc_skip()
  b <- .epc_both()

  oid <- "T_14_3_1"
  prog <- b$executed[[oid]]
  ref  <- b$ref[b$ref$output_id == oid, , drop = FALSE]
  expect_equal(length(.epc_compare(prog, ref)$mismatched), 0L)

  changed <- prog; changed$stat[[1]] <- 999
  expect_gt(length(.epc_compare(changed, ref)$mismatched), 0L)

  nulled <- prog; nulled$stat[[3]] <- NA
  expect_gt(length(.epc_compare(nulled, ref)$mismatched), 0L)

  dropped <- prog[-1, , drop = FALSE]
  expect_equal(nrow(.epc_compare(dropped, ref)$reference_only), 1L)

  relabelled <- prog; relabelled$group1_level[[1]] <- "Bogus Arm"
  moved <- .epc_compare(relabelled, ref)
  expect_equal(nrow(moved$program_only), 1L)
  expect_equal(nrow(moved$reference_only), 1L)

  ## A duplicated identity is invisible to a join -- it is why both sides are
  ## checked for duplicates before the join is trusted.
  doubled <- rbind(prog, prog[1, , drop = FALSE])
  expect_gt(.epc_compare(doubled, ref)$dup_program, 0L)

  ## Row order must not matter.
  shuffled <- prog[rev(seq_len(nrow(prog))), , drop = FALSE]
  shuffled_cmp <- .epc_compare(shuffled, ref)
  expect_equal(nrow(shuffled_cmp$program_only), 0L)
  expect_equal(length(shuffled_cmp$mismatched), 0L)
})

## ---- the subject-key boundary -----------------------------------------------

test_that("Case A: the calculation invariant holds under the canonical key", {
  ## ars_to_code() has no subject_key and emits under the canonical "USUBJID".
  ## Naming it explicitly on the reference side must land in the same place as
  ## leaving it defaulted -- that identity IS the contract being locked.
  ##
  ## "Invariant" here is the subset relation above, not equality: the
  ## completion layer is present on this side too.
  .epc_skip()
  b <- .epc_both()

  canonical <- suppressMessages(suppressWarnings(
    ars_to_ard(b$ars, b$adam, subject_key = "USUBJID")))

  for (oid in .epc_table_outputs(b)) {
    ref <- canonical[canonical$output_id == oid, , drop = FALSE]
    cmp <- .epc_compare(b$executed[[oid]], ref)
    expect_equal(nrow(cmp$program_only), 0L, info = .epc_describe(cmp, oid))
    expect_equal(length(cmp$mismatched), 0L, info = .epc_describe(cmp, oid))
    expect_gt(cmp$n_shared, 0L)
  }
})

test_that("Case B: a different reference-engine subject key leaves the guarantee", {
  ## Measured, not assumed. ADSL carries SUBJID with the same cardinality as
  ## USUBJID (306 distinct over 306 rows), so this is a plausible key rather
  ## than a contrived one -- and it is not interchangeable.
  ##
  ## The result is neither a clean failure nor a no-op: ars_to_ard() still
  ## returns, still reports most rows "computed", and computes different
  ## things. Outputs whose analyses key on subjects share NO row identity with
  ## the program at all; some analyses block outright. An output that never
  ## keys on a subject is genuinely unaffected.
  ##
  ## Hence the boundary: the calculation invariant -- program calculations a
  ## subset of the reference's, exact on every shared row -- is guaranteed
  ## under the canonical subject key. ars_to_ard(subject_key = ) stays an
  ## independently configurable reference-engine option, outside that
  ## guarantee. Whether it should remain configurable at all is a later
  ## ownership question, not this PR's.
  .epc_skip()
  b <- .epc_both()

  alt <- suppressMessages(suppressWarnings(
    ars_to_ard(b$ars, b$adam, subject_key = "SUBJID")))

  expect_gt(sum(alt$result_status == "blocked", na.rm = TRUE), 0L)

  shared_by_output <- vapply(
    .epc_table_outputs(b),
    function(oid) {
      ref <- alt[alt$output_id == oid, , drop = FALSE]
      .epc_compare(b$executed[[oid]], ref)$n_shared
    }, integer(1))

  ## Most tables lose the invariant entirely under the non-canonical key.
  expect_gt(sum(shared_by_output == 0L), 0L)
  ## And the divergence is not merely a renaming: the engine returns far fewer
  ## rows than it computed under the canonical key.
  expect_lt(nrow(alt), nrow(b$ref))
})
