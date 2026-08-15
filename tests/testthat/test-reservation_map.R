## Every defect that could produce a WRONG NUMBER reserves its analyses.
##
## The gate this replaces was positional: any FAIL refused the whole reporting
## event, and everything downstream was written free to assume the event was
## sound. Removing it without replacing that assumption does not produce blank
## cells -- it produces silently different denominators.
##
## These are the seven ways that could happen, each asserted to reserve the
## analyses it reaches AND to leave every other analysis computable. The second
## half matters as much as the first: a map that reserved everything would pass
## every "is it reserved?" assertion while destroying the whole point of the
## change.
##
## The map is tested directly, as the pure function it is. That is deliberate:
## the execution gate is still in place at this commit, so an end-to-end run of
## a broken event would be refused before the reservation could be observed.
## The end-to-end proof lands with the gate's removal.

## A two-analysis event: one that the seeded defect reaches, one that it must
## not. Built from invented ADaM identifiers, and returned as a model so the
## expansion is exercised over the real reference graph.
.rmap_model <- function(vocab = .RSV_NAMES_A) {
  event <- .rsv_event(vocab = vocab, method = .rsv_counting_method(),
                      n_slots = 1L)

  ## The bystander. Its own method, its own population, its own grouping and
  ## its own output, so no seeded defect can reach it by accident.
  event$methods[[2]] <- .rsv_counting_method(id = "MTH_SUBJECT_COUNT_PCT")
  event$analysisSets[[2]] <- list(
    id = "AS_OTHER", name = "Other", label = "Other", level = 1L, order = 1L,
    condition = list(dataset = vocab$ds, variable = vocab$pop,
                     comparator = "EQ", value = list("Y"))
  )
  event$analysisGroupings[[2]] <- list(
    id = "GF_OTHER", name = vocab$arm, groupingDataset = vocab$ds,
    groupingVariable = vocab$arm, dataDriven = TRUE, groups = list()
  )
  event$analyses[[2]] <- list(
    id = "AN_BYSTANDER", name = "bystander", label = "bystander",
    description = "bystander",
    analysisSetId = "AS_OTHER", methodId = "MTH_SUBJECT_COUNT_PCT",
    dataset = vocab$ds, variable = vocab$var,
    analysisVariable = list(dataset = vocab$ds, variable = vocab$var),
    orderedGroupings = list(list(order = 1L, groupingId = "GF_OTHER",
                                 resultsByGroup = TRUE))
  )
  event$outputs[[2]] <- list(
    id = "T_OTHER", name = "Other", label = "Other", outputType = "TABLE",
    referencedAnalysisIds = list("AN_BYSTANDER")
  )
  ars_to_model(event)
}

## Seed one defect, expand the findings, and report what was reserved.
.rmap_reserved <- function(mutate, vocab = .RSV_NAMES_A) {
  model <- mutate(.rmap_model(vocab))
  findings <- validate_ars_model(model)
  list(reserved = names(.reservations_from_findings(model, findings)$by_analysis),
       findings = findings)
}

## Each risk: how to seed it, and the code it must raise.
.RMAP_RISKS <- list(
  list(
    name = "1. dangling analysisSetId computes over the whole dataset",
    ref  = "ANALYSIS_SET_REF_UNRESOLVED",
    seed = function(m) {
      m$analyses$analysisSetId[m$analyses$id == "AN_SYNTH_001"] <- "AS_GONE"
      m
    }
  ),
  list(
    name = "1b. dangling dataSubsetId silently never filters",
    ref  = "DATA_SUBSET_REF_UNRESOLVED",
    seed = function(m) {
      m$analyses$dataSubsetId[m$analyses$id == "AN_SYNTH_001"] <- "DS_GONE"
      m
    }
  ),
  list(
    name = "2. contradictory grouping dataset moves the denominator",
    ref  = "GROUPING_DATASET_CONFLICT",
    seed = function(m) {
      i <- match("GF_SYNTH", m$groupings$id)
      raw <- m$groupings$raw[[i]]
      ## The two places a grouping may name its dataset, made to disagree.
      raw$groupingDataset <- "ADAA"
      raw$groupingVariable <- list(dataset = "ADBB", variable = raw$name)
      m$groupings$raw[[i]] <- raw
      m
    }
  ),
  list(
    name = "3. duplicate id resolves to two different objects",
    ref  = "ENTITY_ID_DUPLICATED",
    seed = function(m) {
      ## Append a copy of the primary method rather than renaming the
      ## bystander's. Renaming would dangle the bystander's own methodId, so
      ## it would be reserved for a SECOND, different defect -- and the
      ## "only its analysis" assertion would fail for a reason that has
      ## nothing to do with duplicate ids.
      m$methods <- rbind(m$methods, m$methods[1, , drop = FALSE])
      m
    }
  ),
  list(
    name = "4. dangling methodId falls through to the generic summarizer",
    ref  = "METHOD_REF_UNRESOLVED",
    seed = function(m) {
      m$analyses$methodId[m$analyses$id == "AN_SYNTH_001"] <- "MTH_GONE"
      m
    }
  ),
  list(
    name = "4b. no method assigned at all",
    ref  = "METHOD_NOT_ASSIGNED",
    seed = function(m) {
      m$analyses$methodId[m$analyses$id == "AN_SYNTH_001"] <- NA_character_
      m
    }
  ),
  list(
    name = "5. dangling grouping id makes the analysis run ungrouped",
    ref  = "GROUPING_REF_UNRESOLVED",
    seed = function(m) {
      m$analyses$grouping_ids[m$analyses$id == "AN_SYNTH_001"] <- "GF_GONE"
      m
    }
  ),
  list(
    name = "5b. fixed grouping with no groups defines no columns",
    ref  = "FIXED_GROUPING_EMPTY",
    seed = function(m) {
      i <- match("GF_SYNTH", m$groupings$id)
      m$groupings$dataDriven[i] <- FALSE
      m$groupings$raw[[i]]$dataDriven <- FALSE
      m$groupings$n_groups[i] <- 0L
      m
    }
  ),
  list(
    name = "7. an unattributable variable role may still look correct",
    ref  = "UNRESOLVED_VARIABLE_ROLE",
    seed = function(m) {
      i <- match("AN_SYNTH_001", m$analyses$id)
      m$analyses$raw[[i]]$unresolvedVariableRole <- list("SOMEROLE")
      m
    }
  )
)


test_that("each wrong-number risk reserves its analysis and only its analysis", {
  checked <- 0L

  for (risk in .RMAP_RISKS) {
    for (nm in names(.RSV_VOCABS)) {
      out <- .rmap_reserved(risk$seed, .RSV_VOCABS[[nm]])
      label <- paste(risk$name, "/", nm)

      ## The defect was actually raised. Without this the test would pass on a
      ## model the seed failed to break.
      expect_true(risk$ref %in% out$findings$ref, info = label)

      ## The unsafe analysis is reserved.
      expect_true("AN_SYNTH_001" %in% out$reserved, info = label)

      ## And the bystander is not. A map that reserved everything would satisfy
      ## the line above while defeating the entire change.
      expect_false("AN_BYSTANDER" %in% out$reserved, info = label)

      checked <- checked + 1L
    }
  }

  ## Non-vacuity: a grammar change that stopped these models parsing would
  ## leave every loop body unexecuted.
  expect_equal(checked, length(.RMAP_RISKS) * 2L)
})


test_that("risk 6: a duplicated output id reserves through every copy", {
  ## The output traversal must union across all rows carrying the id, not stop
  ## at the first. Stopping early would return something -- so the reservation
  ## would look successful -- while the analyses reachable only through the
  ## second copy computed anyway.
  model <- .rmap_model()
  model$outputs$id[2] <- model$outputs$id[1]

  reached <- .analyses_referencing(model, "outputs", model$outputs$id[1])

  expect_true("AN_SYNTH_001" %in% reached)
  expect_true("AN_BYSTANDER" %in% reached)
})


test_that("an event-scoped defect that resolves to nothing widens, not vanishes", {
  ## A missing id can only be named positionally ("row 3"), which references
  ## nothing. The traversal is empty while the danger is real, so the
  ## reservation must widen to the whole event rather than quietly reserve
  ## nothing.
  model <- .rmap_model()
  findings <- .add_finding(
    .new_findings(), "FAIL", "methods", "row 2", "id",
    "This method has no id.", "Give it a unique id.",
    ref = "ENTITY_ID_MISSING"
  )

  reserved <- names(.reservations_from_findings(model, findings)$by_analysis)

  expect_setequal(reserved, c("AN_SYNTH_001", "AN_BYSTANDER"))
})


test_that("advisory and cell findings reserve nothing", {
  ## Reserving on these would discard correct numbers. A column label that
  ## disagrees with its grouping already fails to match per cell at fill time;
  ## withholding the analysis would throw away the columns that DO match.
  model <- .rmap_model()

  for (ref in c("FLAT_AXIS_COLUMN_LABEL_MISMATCH",
                "METHOD_PLACEHOLDER_SLOT_MISMATCH",
                "OUTPUT_HAS_NO_ANALYSES")) {
    findings <- .add_finding(
      .new_findings(), "FAIL", "analyses", "AN_SYNTH_001", "methodId",
      "problem", "action", ref = ref
    )
    reserved <- names(.reservations_from_findings(model, findings)$by_analysis)
    expect_length(reserved, 0L)
  }
})


test_that("a clean event reserves nothing at all", {
  model <- .rmap_model()
  findings <- validate_ars_model(model)

  expect_equal(sum(findings$severity == "FAIL"), 0L)
  expect_length(
    names(.reservations_from_findings(model, findings)$by_analysis), 0L
  )
})


test_that("reintroducing first-match output lookup lets a duplicate escape", {
  ## Mutation: stop at the first output carrying the id, which is what the
  ## traversal did before. The analyses reachable only through the second copy
  ## stop being reserved -- and because the lookup still returns something,
  ## nothing else notices.
  ##
  ## Asserted through .reservations_from_findings() rather than by calling
  ## .analyses_referencing() directly. The mutation has to reach the binding
  ## the ENGINE resolves; calling the patched helper straight from the test
  ## only proves the binding the TEST resolves was patched, and those are not
  ## the same object under every load mode.
  original <- get(".analyses_referencing", envir = asNamespace("arsbridge"))
  withr::defer(.rsv_restore(".analyses_referencing", original))

  .rsv_install(".analyses_referencing", function(model, entity, id) {
    if (identical(entity, "outputs")) {
      row <- match(id, model$outputs$id)
      if (is.na(row)) return(character(0))
      displayed <- .split_values(model$outputs$referenced_analysis_ids[row])
      ids <- model$analyses$id[model$analyses$id %in% displayed]
      return(ids[!is.na(ids) & nzchar(ids)])
    }
    original(model, entity, id)
  })

  model <- .rmap_model()
  model$outputs$id[2] <- model$outputs$id[1]
  findings <- .add_finding(
    .new_findings(), "FAIL", "outputs", model$outputs$id[1], "columns",
    "Two declared paths compose the same condition.",
    "Make each path a distinct subject set.",
    ref = "DUPLICATE_RESULT_PATH"
  )

  reserved <- names(.reservations_from_findings(model, findings)$by_analysis)

  ## The first copy's analysis is reserved; the second copy's escapes.
  expect_true("AN_SYNTH_001" %in% reserved)
  expect_false("AN_BYSTANDER" %in% reserved)
})


test_that("the repaired traversal reserves through both copies", {
  ## The deterministic half of the pair: the same seeded event with no
  ## mutation, so this coverage does not depend on a patch landing.
  model <- .rmap_model()
  model$outputs$id[2] <- model$outputs$id[1]
  findings <- .add_finding(
    .new_findings(), "FAIL", "outputs", model$outputs$id[1], "columns",
    "Two declared paths compose the same condition.",
    "Make each path a distinct subject set.",
    ref = "DUPLICATE_RESULT_PATH"
  )

  reserved <- names(.reservations_from_findings(model, findings)$by_analysis)

  expect_setequal(reserved, c("AN_SYNTH_001", "AN_BYSTANDER"))
})
