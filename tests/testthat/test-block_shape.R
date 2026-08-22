## The three stage helpers: structure, statistic request, method resolution.
##
## These test the helpers directly, not the builder. One of the three now has
## a decision in production -- `.block_shape()`'s expansion source is what
## makes a row claim the rows beneath it as its levels -- and the builder-level
## consequences of that live in test-structural_ownership.R. The statistic and
## method stages remain dormant until PR5b-3.
##
## What every one of them must earn here is the same: that each answers ONE
## question, and that the five contracts the first PR5b attempt broke are
## stated as tests rather than as comments.
##
## Every identifier below is invented and unrelated to any fixture: ADQX.MEASUR
## and its kin exist nowhere in this repo's studies. A rule that keys on a
## familiar name will not pass these.

## A spec binding as `.block_shape()` expects one.
.bs_bind <- function(dataset = "ADQX", variable = "MEASUR", discrete = FALSE,
                     decodes = NULL) {
  list(dataset = dataset, variable = variable, discrete = discrete,
       decodes = decodes)
}

## A term/decode frame, as `.decode_terms_for()` returns.
.bs_decodes <- function(term, decode) {
  data.frame(term = term, decode = decode, order = seq_along(term),
             stringsAsFactors = FALSE)
}

.bs_row <- function(label = "", annotation = "", ...) {
  c(list(label = label, annotation = annotation, has_annot = nzchar(annotation)),
    list(...))
}

.bs_stat_row <- function(label) {
  list(label = label, annotation = "", has_annot = FALSE)
}


## ---------------------------------------------------------------------------
## The header-suffix reader, and its boundary with .parse_stat_label()
## ---------------------------------------------------------------------------

test_that("the header reader takes only a bounded trailing statistic form", {
  ## What it must read: prose, then a short statement of form.
  expect_equal(.header_suffix_stats("Age group, n (%)"), c("count", "pct"))
  expect_equal(.header_suffix_stats("Duration of exposure - Mean (SD)"),
               c("mean", "sd"))
  ## A footnote marker glued to the form does not hide it.
  expect_equal(.header_suffix_stats("Categorical duration, n (%)ᵈ"),
               c("count", "pct"))

  ## What it must NOT read. The trailing fragment here is a LEVEL, and reading
  ## the earlier "n (%)" out of the middle would turn a mis-split header into a
  ## statistic request.
  expect_null(.header_suffix_stats("Sex, n (%) - Female"))
  ## No separator at all: nothing is bounded, so nothing is claimed.
  expect_null(.header_suffix_stats("Serious adverse events"))
  ## Prose after the last comma is not a form, however statistical it sounds.
  expect_null(.header_suffix_stats(
    "Exposure, summarised as the mean of all post-baseline visits"))
  expect_null(.header_suffix_stats(NA))
})

test_that("the header reader does not widen the whole-label grammar", {
  ## The contract that keeps the two separate: everything the header reader
  ## accepts, `.parse_stat_label()` still REFUSES as a whole label. If this
  ## ever fails, the statistic-line grammar has been loosened.
  headers <- c("Age group, n (%)", "Duration of exposure - Mean (SD)",
               "Categorical duration, n (%)ᵈ")
  for (h in headers) {
    expect_null(.parse_stat_label(h), info = h)
    expect_gt(length(.header_suffix_stats(h)), 0L)
  }
})

test_that("a label leading with a statistic is distinguished from prose", {
  ## Led with one, and refused by the whole-label grammar: the row asked for
  ## something that cannot be bound.
  expect_true(.label_leads_with_statistic("Average daily dose"))
  expect_true(.label_leads_with_statistic("Mean arterial pressure"))
  ## Multi-word statistics still count as leading.
  expect_true(.label_leads_with_statistic("Standard deviation of the residuals"))

  ## Merely CONTAINING a statistic word is not asking for it. These are the
  ## false positives that a presence test produces.
  expect_false(.label_leads_with_statistic("Adverse events of special interest"))
  expect_false(.label_leads_with_statistic("TEAE by maximum severity"))
  expect_false(.label_leads_with_statistic("Subjects randomised"))
})


## ---------------------------------------------------------------------------
## Stage 1 -- shape, from structure alone
## ---------------------------------------------------------------------------

test_that("authored rows pinning one value each prove a level axis", {
  rows <- list(
    .bs_row("Response category", "ADQX.RESCAT"),
    .bs_row("Improved", "ADQX.RESCAT='IMP'"),
    .bs_row("Unchanged", "ADQX.RESCAT='UNC'"),
    .bs_row("Worsened", "ADQX.RESCAT='WOR'"))
  bind <- .bs_bind(variable = "RESCAT", discrete = TRUE)
  ctx <- .row_layout_context(rows, 1L, binding = bind)

  expect_equal(ctx$level_children, c("IMP", "UNC", "WOR"))
  got <- .block_shape(rows[[1]], bind, ctx)
  expect_equal(got$shape, "categorical_block")
  expect_equal(got$expansion_source, "data_levels")
})

test_that("the level run stops at the first row that is not a level", {
  ## An unrelated analysis below the block must not be swept into it.
  rows <- list(
    .bs_row("Response category", "ADQX.RESCAT"),
    .bs_row("Improved", "ADQX.RESCAT='IMP'"),
    .bs_row("Time to response", "ADQX.TTRESP"),
    .bs_row("Worsened", "ADQX.RESCAT='WOR'"))
  ctx <- .row_layout_context(rows, 1L,
                             binding = .bs_bind(variable = "RESCAT",
                                                discrete = TRUE))
  expect_equal(ctx$level_children, "IMP")
})

test_that("a row that selects among its own variable's values is not its axis", {
  ## Set membership DECLARES the domain being talked about. That makes the row
  ## a candidate axis for a subdivision of exactly that set -- and nothing
  ## more. One child covering one of two declared values is a selection, not a
  ## subdivision, so this row does not claim the row beneath it.
  rows <- list(
    .bs_row("Any listed category", "ADQX.RESCAT in ('IMP','UNC')"),
    .bs_row("Improved", "ADQX.RESCAT='IMP'"))
  ctx <- .row_layout_context(rows, 1L,
                             binding = .bs_bind(variable = "RESCAT",
                                                discrete = TRUE))
  expect_equal(ctx$level_children, character())

  ## The typed reading underneath that verdict: the set is what it declares,
  ## and a bare reference declares nothing.
  view <- .row_restriction_view(rows)
  expect_equal(.restriction_domain_on(view[[1]], "ADQX", "RESCAT"),
               list(status = "enumerated", values = c("IMP", "UNC")))
  bare <- .row_restriction_view(list(.bs_row("Category", "ADQX.RESCAT")))
  expect_equal(.restriction_domain_on(bare[[1]], "ADQX", "RESCAT")$status,
               "unrestricted")
})

test_that("cumulative thresholds beneath a row are not its levels", {
  ## The A-21 shape. Overlapping thresholds cannot partition anything, so the
  ## row above them is not an axis and they are not levels.
  rows <- list(
    .bs_row("Categorical duration, n (%)", "derived from ADQX.DURWK"),
    .bs_row(">= 4 weeks", "ADQX.DURWK GE 4"),
    .bs_row(">= 12 weeks", "ADQX.DURWK GE 12"),
    .bs_row(">= 24 weeks", "ADQX.DURWK GE 24"))
  bind <- .bs_bind(variable = "DURWK", discrete = FALSE)
  ctx <- .row_layout_context(rows, 1L, binding = bind)
  expect_equal(ctx$level_children, character())

  ## And each threshold row itself is a scalar: nothing expands beneath it.
  child <- .block_shape(rows[[2]], bind,
                        .row_layout_context(rows, 2L, binding = bind))
  expect_equal(child$shape, "scalar_row")
  expect_equal(child$expansion_source, "none")
})

test_that("CONTRACT: exact decode evidence outranks statistic-looking spelling", {
  ## Root cause 3. A codelist whose values happen to be spelled like
  ## statistics is still a set of levels. If the statistic channel is ever
  ## moved ahead of the level channels, this row becomes a stat_block and its
  ## levels are lost.
  decodes <- .bs_decodes(c("MED", "RNG"), c("Median", "Range"))
  rows <- list(
    .bs_row("Reported summary type", "ADQX.SUMTYP"),
    .bs_stat_row("Median"),
    .bs_stat_row("Range"))
  bind <- .bs_bind(variable = "SUMTYP", discrete = TRUE, decodes = decodes)
  ctx <- .row_layout_context(rows, 1L, binding = bind)

  ## Both labels DO parse as statistic lines -- that is what makes this a trap.
  expect_gt(length(.parse_stat_label("Median")), 0L)
  expect_gt(length(.parse_stat_label("Range")), 0L)

  got <- .block_shape(rows[[1]], bind, ctx)
  expect_equal(got$shape, "categorical_block")
  expect_equal(got$expansion_source, "data_levels")
})

test_that("authored statistic lines prove a stat block when no level reading holds", {
  rows <- list(
    .bs_row("Measured value", "ADQX.MEASUR"),
    .bs_stat_row("Mean (SD)"),
    .bs_stat_row("Median"),
    .bs_stat_row("Min, Max"))
  bind <- .bs_bind(discrete = FALSE)
  got <- .block_shape(rows[[1]], bind, .row_layout_context(rows, 1L, binding = bind))
  expect_equal(got$shape, "stat_block")
  expect_equal(got$expansion_source, "authored_stats")
})

test_that("CONTRACT: a detected token role never proves expansion on its own", {
  ## Root cause 4. `self_template` is upstream EVIDENCE. A continuous binding
  ## must not expand into levels merely because the run detector marked the
  ## row, or the shape would go on to write back the very role that proved it.
  rows <- list(.bs_row("Measured value", "ADQX.MEASUR"),
               .bs_row("<value #2>", "ADQX.MEASUR"))
  roles <- c("self_template", "level_repeat")
  bind_num <- .bs_bind(discrete = FALSE)
  ctx <- .row_layout_context(rows, 1L, roles = roles, binding = bind_num)
  expect_equal(ctx$detected_role, "self_template")

  expect_false(identical(.block_shape(rows[[1]], bind_num, ctx)$shape,
                         "categorical_block"))

  ## With a discrete binding the same evidence is admissible.
  bind_txt <- .bs_bind(discrete = TRUE)
  ctx_txt <- .row_layout_context(rows, 1L, roles = roles, binding = bind_txt)
  expect_equal(.block_shape(rows[[1]], bind_txt, ctx_txt)$shape,
               "categorical_block")
})

test_that("a bare discrete reference alone does not prove an axis", {
  ## Both readings admissible -- a variable axis, or a condition on that
  ## variable whose predicate a lossy reader dropped -- so the shape is not
  ## proved and the row reserves.
  bind <- .bs_bind(variable = "SERFL", discrete = TRUE)
  got <- .block_shape(.bs_row("Serious findings", "ADQX.SERFL"), bind, list())
  expect_true(is.na(got$shape))
  expect_equal(got$expansion_source, "unknown")
  expect_gt(length(got$conflicts), 0L)

  ## Corroborated by a count presentation, it is proved.
  ok <- .block_shape(.bs_row("Finding category, n (%)", "ADQX.SERFL"), bind,
                     list())
  expect_equal(ok$shape, "categorical_block")
})

test_that("a label that names a level of its own variable never claims a block", {
  decodes <- .bs_decodes(c("MOD"), c("Moderate"))
  bind <- .bs_bind(variable = "SEVCAT", discrete = TRUE, decodes = decodes)
  got <- .block_shape(.bs_row("Moderate", "ADQX.SEVCAT"), bind, list())
  expect_true(is.na(got$shape))
  expect_gt(length(got$conflicts), 0L)
})

test_that("an enclosing header is proved by indentation, not by adjacency", {
  rows <- list(
    .bs_row("Exposure summary, n (%)", ""),
    .bs_row("Sub-heading with no form", ""),
    .bs_row(">= 4 weeks", "ADQX.DURWK GE 4"))
  ## Row 3 is enclosed by row 1 even though row 2 sits between them, because
  ## row 2 is deeper than row 1.
  ctx <- .row_layout_context(rows, 3L, indents = c(0L, 2L, 4L))
  expect_equal(ctx$inherited_stats, c("count", "pct"))
  expect_equal(ctx$inherited_from, "Exposure summary, n (%)")

  ## Flatten the indentation and no enclosure is provable -- the conservative
  ## direction for a document reader that drops indentation.
  flat <- .row_layout_context(rows, 3L, indents = c(0L, 0L, 0L))
  expect_equal(flat$inherited_stats, character())
})


## ---------------------------------------------------------------------------
## Stage 2 -- what the shell asks to report
## ---------------------------------------------------------------------------

test_that("a row inherits its statistics from the header that encloses it", {
  rows <- list(
    .bs_row("Categorical duration, n (%)", "derived from ADQX.DURWK"),
    .bs_row(">= 4 weeks", "ADQX.DURWK GE 4"))
  ctx <- .row_layout_context(rows, 2L, indents = c(0L, 4L))
  got <- .requested_statistic(rows[[2]], ctx)
  expect_equal(got$tokens, c("count", "pct"))
  expect_equal(got$source, "inherited")
})

test_that("CONTRACT: each authored statistic line is its own request", {
  ## Root cause 1. The requests list must keep the lines separate so that one
  ## unsupportable line cannot speak for the others.
  rows <- list(
    .bs_row("Measured value", "ADQX.MEASUR"),
    .bs_stat_row("Mean (SD)"), .bs_stat_row("Median"),
    .bs_stat_row("SE"), .bs_stat_row("Min, Max"))
  ctx <- .row_layout_context(rows, 1L, binding = .bs_bind())
  got <- .requested_statistic(rows[[1]], ctx)

  expect_equal(length(got$child_requests), 4L)
  labels <- vapply(got$child_requests, function(r) r$label, character(1))
  expect_equal(labels, c("Mean (SD)", "Median", "SE", "Min, Max"))
  ## Not unioned into one vector.
  expect_true(all(vapply(got$child_requests,
                         function(r) length(r$tokens) >= 1L, logical(1))))
})

test_that("CONTRACT: once/subject is carried as cardinality, not as a method", {
  ## Root cause 2. The directive is authored computational meaning and must
  ## survive independently of whatever method is provisionally in view.
  got <- .requested_statistic(
    .bs_row("Subjects with any finding", "ADQX.ANYFL='Y'; once/subject ADQX.OCCFL"),
    list())
  expect_equal(got$cardinality, "distinct_subject")

  plain <- .requested_statistic(.bs_row("Subjects with any finding",
                                        "ADQX.ANYFL='Y'"), list())
  expect_true(is.na(plain$cardinality))
})

test_that("silence and refusal are different answers", {
  ## Silent: no statistic claimed anywhere, so shape and placeholder may still
  ## resolve the row.
  quiet <- .requested_statistic(.bs_row("Subjects randomised", "ADQX.RNDFL='Y'"),
                                list())
  expect_equal(length(quiet$tokens), 0L)
  expect_false(quiet$refused)

  ## Refused: the label asked for a statistic the grammar will not bind.
  asked <- .requested_statistic(.bs_row("Average daily amount", "ADQX.MEASUR"),
                                list())
  expect_equal(length(asked$tokens), 0L)
  expect_true(asked$refused)
  expect_match(asked$reason, "leads with a statistic")
})

test_that("the shell outranks a supplement unless the supplement overrides", {
  row <- .bs_row("Mean (SD)", "ADQX.MEASUR")
  row$supplement_stat_tokens <- c("count")
  expect_equal(.requested_statistic(row, list())$source, "label")

  row$supplement_stat_override <- TRUE
  expect_equal(.requested_statistic(row, list())$source, "supplement")
})

test_that("a filter never contributes a statistic, however it is written", {
  ## The same row under four restrictions. What the shell asks to report is
  ## identical in all four, because none of them says anything about it.
  filters <- c("ADQX.MEASUR", "ADQX.MEASUR = 30", "ADQX.MEASUR GT 30",
               "ADQX.MEASUR GE 10 AND ADQX.MEASUR LE 20")
  got <- lapply(filters, function(f)
    .requested_statistic(.bs_row("Measured value, n (%)", f), list()))
  for (g in got) expect_equal(g$tokens, c("count", "pct"))
  expect_equal(length(unique(vapply(got, function(g) g$source, character(1)))), 1L)
})


## ---------------------------------------------------------------------------
## Stage 3 -- which method may satisfy the row
## ---------------------------------------------------------------------------

test_that("CONTRACT: an unsupportable line refuses itself, not its siblings", {
  ## Root cause 1, at the resolver. `SE` is beyond every admissible method; it
  ## must be recorded and skipped, and the block must still resolve for the
  ## four lines that ARE supportable.
  requests <- list(
    list(label = "Mean (SD)", tokens = c("mean", "sd")),
    list(label = "Median",    tokens = "median"),
    list(label = "SE",        tokens = "se"),
    list(label = "Min, Max",  tokens = c("min", "max")))
  got <- .resolve_method("stat_block", requests,
                         .bs_bind(discrete = FALSE))
  expect_equal(got$method, "MTH_SUMMARY_STATISTICS_CONTINUOUS")
  expect_equal(length(got$unsupported_requests), 1L)
  expect_match(got$unsupported_requests, "SE")
})

test_that("a request is judged against the baseline, not a narrowed set", {
  ## If supportability were judged after earlier requests had narrowed the
  ## candidates, a line some method can satisfy could be mislabelled
  ## unsupported and then silently ignored.
  requests <- list(
    list(label = "n",   tokens = "count"),
    list(label = "n (%)", tokens = c("count", "pct")))
  got <- .resolve_method("scalar_row", requests, .bs_bind(discrete = TRUE))
  expect_equal(got$method, "MTH_SUBJECT_COUNT_PCT")
  expect_equal(got$unsupported_requests, character())
})

test_that("two supportable requests that no one method satisfies reserve", {
  ## Both stat-block methods are admissible here, and each line is satisfiable
  ## -- but by a DIFFERENT one. `mean` is produced only by the continuous
  ## summary, `events` only by the Kaplan-Meier estimate. Neither line may be
  ## discarded, so the row reserves rather than honouring whichever came
  ## first.
  requests <- list(
    list(label = "Mean (SD)", tokens = c("mean", "sd")),
    list(label = "Events",    tokens = "events"))
  got <- .resolve_method("stat_block", requests, .bs_bind(discrete = FALSE))
  expect_null(got$method)
  expect_equal(got$unsupported_requests, character())
  expect_match(got$reason, "not by the same method")

  ## Order must not decide it either.
  flipped <- .resolve_method("stat_block", rev(requests),
                             .bs_bind(discrete = FALSE))
  expect_null(flipped$method)
})

test_that("a line no admissible method can produce is not a conflict", {
  ## The neighbouring case, kept beside it so the two cannot be confused. An
  ## `n (%)` line under a continuous block is not satisfiable by ANY stat-block
  ## method, so it refuses itself and the block still resolves on the rest.
  requests <- list(
    list(label = "Mean (SD)", tokens = c("mean", "sd")),
    list(label = "n (%)",     tokens = c("count", "pct")))
  got <- .resolve_method("stat_block", requests, .bs_bind(discrete = FALSE))
  expect_equal(got$method, "MTH_SUMMARY_STATISTICS_CONTINUOUS")
  expect_match(got$unsupported_requests, "n \\(%\\)")
})

test_that("when every request is refused there is no method evidence", {
  got <- .resolve_method("stat_block",
                         list(list(label = "SE", tokens = "se")),
                         .bs_bind(discrete = FALSE))
  expect_null(got$method)
  expect_equal(length(got$unsupported_requests), 1L)
})

test_that("CONTRACT: once/subject eliminates record-counting methods", {
  ## Root cause 2. The directive constrains the method directly, so it cannot
  ## be lost when the provisional method id changes.
  requests <- list(list(label = "Any finding", tokens = c("count", "pct")))
  with_card <- .resolve_method("categorical_block", requests,
                               .bs_bind(discrete = TRUE),
                               cardinality = "distinct_subject")
  expect_equal(with_card$method, "MTH_AE_FREQUENCY_COUNT")

  without <- .resolve_method("categorical_block", requests,
                             .bs_bind(discrete = TRUE))
  expect_equal(without$method, "MTH_COUNT_AND_PERCENTAGE")
})

test_that("an unproved shape is never rescued by a method", {
  got <- .resolve_method(NA_character_,
                         list(list(label = "n (%)", tokens = c("count", "pct"))),
                         .bs_bind(discrete = TRUE))
  expect_null(got$method)
  expect_equal(got$constraints_applied, "shape unproved")
})

test_that("statistic evidence cannot invent a structural shape", {
  ## The resolver is handed a shape; it never derives one. Same request, three
  ## shapes, three different admissible sets.
  req <- list(list(label = "n (%)", tokens = c("count", "pct")))
  expect_equal(.resolve_method("scalar_row", req, .bs_bind(discrete = TRUE))$method,
               "MTH_SUBJECT_COUNT_PCT")
  expect_equal(.resolve_method("categorical_block", req,
                               .bs_bind(discrete = TRUE))$method,
               "MTH_COUNT_AND_PERCENTAGE")
})

test_that("the placeholder separates count from count-and-percentage, and only that", {
  ## Presentation evidence is read only where the shell stated no statistic.
  one <- .resolve_method("scalar_row", list(), .bs_bind(discrete = TRUE),
                         presentation = 1L)
  expect_equal(one$method, "MTH_SUBJECT_COUNT")
  two <- .resolve_method("scalar_row", list(), .bs_bind(discrete = TRUE),
                         presentation = 2L)
  expect_equal(two$method, "MTH_SUBJECT_COUNT_PCT")

  ## With no placeholder and nothing stated, the row is genuinely ambiguous.
  none <- .resolve_method("scalar_row", list(), .bs_bind(discrete = TRUE))
  expect_null(none$method)
  expect_equal(length(none$candidates), 2L)
})

test_that("a stated request is never overruled by the placeholder", {
  ## Two slots drawn, one statistic asked for: the shell's words win.
  got <- .resolve_method("scalar_row",
                         list(list(label = "n", tokens = "count")),
                         .bs_bind(discrete = TRUE), presentation = 2L)
  expect_equal(got$method, "MTH_SUBJECT_COUNT")
})

test_that("the spec's own verdict on the variable eliminates methods", {
  req <- list(list(label = "Mean (SD)", tokens = c("mean", "sd")))
  expect_null(.resolve_method("stat_block", req,
                              .bs_bind(discrete = TRUE))$method)
  expect_equal(.resolve_method("stat_block", req,
                               .bs_bind(discrete = FALSE))$method,
               "MTH_SUMMARY_STATISTICS_CONTINUOUS")
})

test_that("every elimination states which method lost and why", {
  got <- .resolve_method("scalar_row",
                         list(list(label = "n (%)", tokens = c("count", "pct"))),
                         .bs_bind(discrete = TRUE))
  expect_gt(length(got$eliminated), 0L)
  expect_true(all(grepl("^MTH_[A-Z_]+ \\(.+\\)$", got$eliminated)))
  expect_gt(length(got$constraints_applied), 0L)
})


## ---------------------------------------------------------------------------
## Filter invariance, end to end across the three stages
## ---------------------------------------------------------------------------

test_that("changing only the restriction changes neither shape nor method", {
  ## The central claim of PR5b, stated over the three stages together. Same
  ## label, same placeholder, same variable, same neighbours; four different
  ## restrictions. A filter decides which observations are included and
  ## nothing else.
  restrictions <- c(
    none      = "ADQX.MEASUR",
    equality  = "ADQX.MEASUR = 30",
    threshold = "ADQX.MEASUR GT 30",
    range     = "ADQX.MEASUR GE 10 AND ADQX.MEASUR LE 20")

  decided <- lapply(restrictions, function(r) {
    row <- .bs_row("Subjects reaching target, n (%)", r, n_slots = 2L)
    bind <- .bs_bind(discrete = FALSE)
    ctx <- .row_layout_context(list(row), 1L, binding = bind)
    shape <- .block_shape(row, bind, ctx)
    stats <- .requested_statistic(row, ctx)
    method <- .resolve_method(shape$shape, stats$requests, bind,
                              cardinality = stats$cardinality,
                              presentation = row$n_slots)
    list(shape = shape$shape, tokens = stats$tokens, method = method$method)
  })

  shapes <- vapply(decided, function(d) d$shape, character(1))
  methods <- vapply(decided, function(d) d$method %||% NA_character_,
                    character(1))
  expect_equal(length(unique(shapes)), 1L)
  expect_equal(length(unique(methods)), 1L)
  ## And the answer is the one the shell's own words support -- asserted
  ## directly so this cannot pass by being uniformly unresolved.
  expect_equal(unname(shapes[[1]]), "scalar_row")
  expect_equal(unname(methods[[1]]), "MTH_SUBJECT_COUNT_PCT")
})

test_that("the same evidence in a second vocabulary decides identically", {
  ## Rename every dataset and variable. The answers must not move, which is
  ## what proves these rules key on relationships rather than on names.
  one <- list(
    .bs_row("Response category, n (%)", "ADQX.RESCAT"),
    .bs_row("Improved", "ADQX.RESCAT='IMP'"),
    .bs_row("Worsened", "ADQX.RESCAT='WOR'"))
  two <- list(
    .bs_row("Response category, n (%)", "ADZZ.OUTGRP"),
    .bs_row("Improved", "ADZZ.OUTGRP='IMP'"),
    .bs_row("Worsened", "ADZZ.OUTGRP='WOR'"))

  decide <- function(rows, ds, var) {
    bind <- .bs_bind(dataset = ds, variable = var, discrete = TRUE)
    ctx <- .row_layout_context(rows, 1L, binding = bind)
    shape <- .block_shape(rows[[1]], bind, ctx)
    stats <- .requested_statistic(rows[[1]], ctx)
    list(shape = shape$shape, source = shape$expansion_source,
         method = .resolve_method(shape$shape, stats$requests, bind)$method)
  }
  a <- decide(one, "ADQX", "RESCAT")
  b <- decide(two, "ADZZ", "OUTGRP")
  expect_equal(a, b)
  ## Scope assertion: a vacuous pass would be two NULLs.
  expect_equal(a$shape, "categorical_block")
  expect_equal(a$method, "MTH_COUNT_AND_PERCENTAGE")
})
