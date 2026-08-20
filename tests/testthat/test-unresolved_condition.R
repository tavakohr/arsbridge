## What counts as a condition, and what happens when one cannot be read.
##
## THE DEFECT CLASS. `parse_where_clause()` answered NULL to two different
## questions: "does this annotation supply a condition?" and "could you read
## the condition it supplies?". A no to the first is ordinary. A no to the
## second means a filter the author wrote is not applied, and every count
## behind it is computed over the wrong records -- with no error and no empty
## cell to show for it.
##
## THE INVARIANT. Three outcomes, kept distinct:
##
##   absent      no condition was supplied -- a bare variable pointer, prose,
##               a directive, descriptive metadata. Computes unfiltered, and
##               that is correct, because nothing asked for a filter.
##   unresolved  a condition WAS supplied, carries operator evidence, and could
##               not be read. Reserves.
##   clause      read successfully. Computes as written.
##
## THE OVER-RESERVATION HAZARD, which is why the first two are separated by
## evidence of an OPERATOR rather than by "text follows a qualified reference".
## Real shells annotate rows with a variable and descriptive text constantly --
## "count of X", "X (unit Y)", "X / Y" -- and treating those as unreadable
## filters would withhold results that were never filtered and were always
## correct. Withholding a correct result is its own wrong answer, so both
## directions are pinned below.

.UC_VOCAB <- list(ds = "ADQX", var = "MEASURE", num = "SCORE", dt = "STARTDT")

.uc_parse <- function(expr) suppressWarnings(parse_where_clause(expr))

.uc_kind <- function(expr) {
  wc <- .uc_parse(expr)
  if (.is_unresolved_condition(wc)) return("unresolved")
  if (is.null(wc)) return("absent")
  "clause"
}


test_that("a qualified variable with descriptive text supplies no condition", {
  ## The over-reservation guard. Every one of these follows a qualified
  ## reference with text -- which is exactly what the looser predicate mistook
  ## for a condition attempt -- and not one of them states a filter.
  descriptive <- c(
    "count of ADQX.MEASURE",
    "ADQX.MEASURE (unit ADQX.UNIT)",
    "ADQX.MEASURE / MEASUREN",
    "ADQX.SCORE by ADQX.PARAMCD (ADQX.AVAL / AVALU)",
    "ADQX.STARTDT / ENDDT",
    "ADQX.MEASURE, summarised by visit"
  )

  kinds <- vapply(descriptive, .uc_kind, character(1), USE.NAMES = FALSE)
  expect_equal(length(kinds), 6L)
  expect_true(all(kinds == "absent"))
})


test_that("comparator words used as prose are not operators", {
  ## Word boundaries alone do not settle this. `\\bIN\\b` matches "contained in
  ## the listing" and `\\bBETWEEN\\b` matches "between-group", because both are
  ## genuine standalone tokens. The keywords are therefore required in the
  ## SHAPE the grammar reads -- IN with its parenthesised list, BETWEEN and
  ## CONTAINS with a following value -- which is what prose does not have.
  prose <- c(
    "ADQX.INDICATN summarised",              # IN as a substring
    "ADQX.MEASURE between-group summary",    # BETWEEN with no value after
    "ADQX.MEASURE missingness rate",         # MISSING as a substring
    "ADQX.MEASURE contained in the listing", # IN as a word, no list
    "ADQX.MEASURE, in the safety set"        # IN as a word, no list
  )

  kinds <- vapply(prose, .uc_kind, character(1), USE.NAMES = FALSE)
  expect_equal(length(kinds), 5L)
  expect_true(all(kinds == "absent"))
})


test_that("the shapes the grammar reads still produce clauses", {
  readable <- c(
    "ADQX.MEASURE='AMBER'",
    "ADQX.MEASURE IN ('AMBER','TEAL')",
    "ADQX.MEASURE NOT IN ('AMBER')",
    "ADQX.SCORE between 18 and 65",
    "ADQX.SCORE GE 65",
    "ADQX.STARTDT is null",
    "ADQX.STARTDT not missing",
    "is.na(ADQX.SCORE)",
    "ADQX.MEASURE contains 'AM'"
  )

  kinds <- vapply(readable, .uc_kind, character(1), USE.NAMES = FALSE)
  expect_equal(length(kinds), 9L)
  expect_true(all(kinds == "clause"))
})


test_that("a malformed comparison is unresolved, not absent", {
  ## Operator evidence present, syntax unreadable. These must reserve: the
  ## author plainly asked for a filter, and this package cannot tell which
  ## records it selects.
  malformed <- c(
    "ADQX.SCORE >= 16",
    "ADQX.MEASURE IN (",
    "ADQX.SCORE between 18"
  )

  kinds <- vapply(malformed, .uc_kind, character(1), USE.NAMES = FALSE)
  expect_equal(length(kinds), 3L)
  expect_true(all(kinds == "unresolved"))
})


test_that("a comparator inside a quoted value is not evidence", {
  ## The evidence test reads MASKED text, so operator-shaped characters inside
  ## a literal cannot make descriptive text look like a filter. Without that, a
  ## label containing "=" would reserve a row that states no condition.
  quoted_only <- c(
    "count of ADQX.MEASURE labelled 'A=B'",
    "ADQX.MEASURE (category 'IN PROGRESS')",
    "ADQX.MEASURE (note 'BETWEEN VISITS')"
  )

  kinds <- vapply(quoted_only, .uc_kind, character(1), USE.NAMES = FALSE)
  expect_equal(length(kinds), 3L)
  expect_true(all(kinds == "absent"))

  ## The same words with a real operator OUTSIDE the quotes do reserve, so it
  ## is the masking that makes the difference rather than the vocabulary.
  expect_identical(.uc_kind("ADQX.MEASURE >= 'A=B'"), "unresolved")
})


test_that("empty and unqualified text supply no condition", {
  expect_identical(.uc_kind(""), "absent")
  expect_identical(.uc_kind("   "), "absent")
  expect_identical(.uc_kind("Safety Population"), "absent")
  expect_identical(.uc_kind("all randomised subjects"), "absent")
  expect_null(.uc_parse(NULL))
})


test_that("an unresolved clause carries the author's own text", {
  ## The finding, the fix report and the editor all quote this. Paraphrasing it
  ## would send the author looking for something they did not write.
  expr <- "ADQX.SCORE >= 16"
  wc <- .uc_parse(expr)

  expect_true(.is_unresolved_condition(wc))
  expect_identical(.unresolved_condition_text(wc), expr)
  ## Never an empty string: an empty marker is indistinguishable downstream
  ## from an absent one.
  expect_true(nzchar(.unresolved_condition_text(wc)))
})


test_that("one unreadable clause makes a whole compound unresolved", {
  ## A partial parse is not a safe filter. "A and B" that loses B restricts
  ## less than written and over-counts; "A or B" that loses B restricts more
  ## and under-counts. Both produce a plausible number.
  sound <- "ADQX.MEASURE='AMBER'"
  broken <- "ADQX.SCORE >= 16"

  ## The sound half alone reads as a clause, so the compound's outcome below is
  ## caused by the broken half rather than by both being unreadable.
  expect_identical(.uc_kind(sound), "clause")

  expect_identical(.uc_kind(paste(sound, "and", broken)), "unresolved")
  expect_identical(.uc_kind(paste(sound, "or", broken)), "unresolved")
})


## The one annotation in the bundled shells that this package genuinely cannot
## read, recorded here rather than counted away.
##
## The population is written as a comma-separated pair. This grammar has no
## comma joiner, and the leaf battery stops at the first condition it
## recognises -- so until the residue check was added, `ADVS.ANL01FL='Y'`
## vanished: the emitted event carried the safety flag alone, the string
## "ANL01FL" appeared nowhere in it, and the figure's population was merged
## with the plain safety population it was written to differ from.
##
## It reserves now. Whether the comma should instead be READ as a conjunction
## is a grammar question, deliberately not answered here.
.UC_COMMA_POPULATION <- "(ADSL.SAFFL='Y', ADVS.ANL01FL='Y')"

test_that("the real bundled studies reserve nothing they can read", {
  ## The acceptance half of the over-reservation guard: the generic cases above
  ## say what should happen, and this says it actually did on shells nobody
  ## wrote for this test. Both bundled shells are full of descriptive
  ## annotations of exactly the shape that used to reserve.
  ##
  ## The bar is not "no reservations" -- that would be satisfied by going back
  ## to dropping the condition. It is "no reservation this package could have
  ## read", which is why the one that remains is named and pinned to its
  ## annotation rather than allowed for by a count.
  skip_if_not_installed("openxlsx2")
  skip_on_cran()

  expected <- list(annotated_shell.xlsx = character(0),
                   annotated_shell.docx = .UC_COMMA_POPULATION)

  for (shell in c("annotated_shell.xlsx", "annotated_shell.docx")) {
    out <- withr::local_tempdir()
    result <- withr::with_envvar(
      c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = "", GEMINI_API_KEY = "",
        GLM_API_KEY = "", ARS_LLM_PROVIDER = ""),
      suppressMessages(suppressWarnings(spec_to_ars(
        shell_path     = arsbridge_example(shell),
        adam_spec_path = arsbridge_example("adam_spec.xlsx"),
        api_key = "", use_llm = FALSE, verbose = FALSE,
        output_path = file.path(out, "re.json"),
        report_path = file.path(out, "rep.xlsx"),
        emit_code = FALSE))))

    findings <- result$ars_validation
    unresolved <- findings[grepl("CONDITION_UNRESOLVED", findings$ref), ,
                           drop = FALSE]
    want <- expected[[shell]]
    expect_equal(nrow(unresolved), length(want), info = shell)
    ## Named, not counted: a DIFFERENT annotation reserving still fails here.
    expect_identical(sort(as.character(unresolved$detail)), sort(want),
                     info = shell)
    expect_identical(result$validation_gate$verdict,
                     if (length(want) == 0L) "DONE" else "COMPLETED WITH GAPS",
                     info = shell)
  }
})
