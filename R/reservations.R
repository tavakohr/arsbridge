## arsbridge -- reservations.R
##
## Turning a validation finding into the set of analyses that must not compute.
##
## The package used to protect itself positionally: one gate,
## `.assert_runnable_ars()`, refused the whole reporting event if any finding
## was FAIL, and everything downstream was written free to assume the event was
## sound. Several functions say so in their own comments -- `resolve_analysis()`
## notes that a contradictory grouping dataset is safe to read as NA *because
## the gate refuses the event*.
##
## Removing that gate without replacing the assumption does not produce blank
## cells. It produces silently different denominators. This file is the
## replacement: the same findings the gate used to read, expanded into the
## smallest set of analyses each defect actually invalidates, so the engine can
## refuse a cell instead of a run.
##
## Everything here is graph traversal over ids. No dataset name, variable name,
## row label, output id or analysis id is ever matched against a literal --
## which is what makes the behaviour identical under any study's vocabulary,
## and is worth preserving in any change to this file.

#' Analyses that resolve through a given entity.
#'
#' The reference graph, read in the one direction a reservation needs: given
#' something broken, which analyses would compute through it?
#'
#' Every lookup here unions over ALL entities carrying the id, never the first
#' one. That is not defensive tidiness: a duplicated id is itself one of the
#' defects being reserved for, and stopping at the first match would let the
#' analyses reachable only through the second copy compute anyway -- while
#' making the reservation look successful, because it returned something.
#'
#' @param entity The pool the broken entity lives in, as the finding names it.
#' @param id That entity's id.
#' @return Analysis ids, possibly empty. Empty is a real answer -- "nothing
#'   computes through this" -- and a caller that cannot accept it says so
#'   itself rather than having a fallback hidden here.
#' @noRd
.analyses_referencing <- function(model, entity, id) {
  analyses <- model$analyses
  if (is.null(analyses) || nrow(analyses) == 0) return(character(0))
  if (length(entity) != 1L || is.na(entity) || !nzchar(entity)) {
    return(character(0))
  }
  if (length(id) != 1L || is.na(id) || !nzchar(id)) return(character(0))

  hits <- switch(
    entity,
    ## The analysis itself. `%in%` rather than a first match, so a duplicated
    ## analysis id reserves every analysis carrying it.
    analyses = analyses$id %in% id,
    ## A shared entity: every analysis pointing at it computes through it.
    methods       = !is.na(analyses$methodId)      & analyses$methodId      == id,
    analysis_sets = !is.na(analyses$analysisSetId) & analyses$analysisSetId == id,
    data_subsets  = !is.na(analyses$dataSubsetId)  & analyses$dataSubsetId  == id,
    ## grouping_ids is a packed multi-value column, so membership, not equality.
    groupings = vapply(
      seq_len(nrow(analyses)),
      function(i) id %in% .split_values(analyses$grouping_ids[i]),
      logical(1)
    ),
    ## An output invalidates every analysis it displays -- across every row
    ## carrying that output id, since the id may be duplicated.
    outputs = {
      rows <- which(!is.na(model$outputs$id) & model$outputs$id == id)
      displayed <- unlist(
        lapply(rows,
               function(r) .split_values(model$outputs$referenced_analysis_ids[r])),
        use.names = FALSE
      )
      analyses$id %in% displayed
    },
    rep(FALSE, nrow(analyses))
  )

  ids <- analyses$id[hits]
  ids[!is.na(ids) & nzchar(ids)]
}

#' Every analysis in the event.
#' @noRd
.all_analysis_ids <- function(model) {
  ids <- model$analyses$id
  ids[!is.na(ids) & nzchar(ids)]
}

#' Which analyses each reserving finding invalidates.
#'
#' Pure: a model and a findings frame in, a lookup out. Nothing is read from
#' disk and nothing is computed, so the same findings always produce the same
#' reservations -- which matters because the engine and the fix report must
#' agree about what was withheld, and the only way to guarantee that is for
#' both to read one map built from one set of findings.
#'
#' `advisory` findings reserve nothing. They are real and they are reported,
#' but withholding a result for them would discard correct numbers: a column
#' label that disagrees with its grouping already fails to match per cell at
#' fill time, and reserving the analysis would throw away the columns that DO
#' match. `cell` findings reserve nothing here either -- the fill refuses that
#' one cell, and the analysis's other cells are sound.
#'
#' @return list(by_analysis = named list). Each entry is
#'   list(ref, scope, reason), keyed by analysis id.
#' @noRd
.reservations_from_findings <- function(model, findings) {
  empty <- list(by_analysis = list())
  if (is.null(findings) || nrow(findings) == 0) return(empty)
  if (is.null(model$analyses) || nrow(model$analyses) == 0) return(empty)

  ## Only these scopes withhold a result. The rest are reported and no more.
  reserving <- c("analysis", "grouping", "output", "event")
  scope <- findings$scope %||% rep(NA_character_, nrow(findings))
  rows <- which(!is.na(scope) & scope %in% reserving)
  if (length(rows) == 0) return(empty)

  by_analysis <- list()
  for (i in rows) {
    ## analysis / grouping / output findings name an entity, and the
    ## reservation covers whatever computes through it.
    affected <- .analyses_referencing(model, findings$entity[i], findings$id[i])

    ## `event` scope is different, and the difference is the point. It means
    ## the event's own identity is broken -- an id that is missing, or one two
    ## entities share. Resolution by id is exactly what stops being
    ## trustworthy, so an event finding that resolves to nothing must NOT
    ## quietly reserve nothing.
    ##
    ## A missing id is the concrete case: the finding can only name the entity
    ## positionally ("row 3"), which references nothing, so the traversal above
    ## is empty while the danger is real -- anything pointing at that entity now
    ## resolves elsewhere or nowhere. When an event-scoped finding cannot be
    ## narrowed, it widens to the whole event instead of vanishing.
    if (identical(scope[i], "event") && length(affected) == 0) {
      affected <- .all_analysis_ids(model)
    }

    for (analysis_id in affected) {
      ## First finding wins. A second reason for an already-reserved analysis
      ## changes nothing about the outcome, and one cause per reserved cell
      ## keeps the fix list readable.
      if (!is.null(by_analysis[[analysis_id]])) next
      by_analysis[[analysis_id]] <- list(
        ref    = findings$ref[i],
        scope  = scope[i],
        reason = findings$problem[i]
      )
    }
  }

  list(by_analysis = by_analysis)
}

#' The block signal for a structural defect.
#'
#' Reuses the existing block class, so `.is_block()`, `.reserve_blocked_ard()`
#' and the `block_reason` column all work unchanged -- a structurally reserved
#' analysis is reserved by the same machinery as one whose data could not be
#' resolved at run time.
#'
#' The reason carries the finding's code, so a reserved cell traces back to the
#' finding that caused it rather than only to the fact that something was
#' wrong.
#' @noRd
.structural_block <- function(ref, detail = NA_character_) {
  .block_signal(paste0("structural:", ref), detail)
}

#' Reservations for a reporting event, built from an explicit model.
#'
#' Deliberately NOT wrapped in a tryCatch. A model that will not parse, or a
#' validator that errors, is not evidence that the event is clean -- and
#' returning an empty map for it would let every analysis compute. Failing open
#' here would silently reintroduce the exact risk this file exists to remove,
#' so the error propagates to the caller instead.
#'
#' Note what this pass can and cannot see. Called from the engine there is no
#' ADaM spec and no annotation report, so spec-dependent findings
#' (DATASET_NOT_IN_SPEC, VARIABLE_NOT_IN_SPEC) do not arise. That is safe
#' rather than a hole: a dataset the spec does not carry is a dataset the
#' engine cannot load either, so it fails to produce a row rather than
#' producing a wrong one. A caller that holds a spec should build the map from
#' its own richer findings with `.reservations_from_findings()` directly.
#' @noRd
.spec_reservations <- function(spec) {
  model <- ars_to_model(spec)
  .reservations_from_findings(model, validate_ars_model(model))
}
