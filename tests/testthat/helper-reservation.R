## A minimal reporting event built from invented ADaM identifiers, for the
## checks that must key on structure rather than on anything a study is called.
##
## Two disjoint vocabularies. Every test that matters runs under both, so a
## check that silently recognised a familiar name would pass under A and fail
## under B. The identifiers are valid ADaM shapes (.ADAM_DS / .ADAM_VAR in
## R/aaa_constants.R) but belong to no real study and to neither fixture.

.RSV_NAMES_A <- list(
  ds      = "ADQX",
  var     = "QXCAT",
  pop     = "QXFL",
  arm     = "QXARM",
  sheet   = "Table 90.1.1",
  label   = "Measured category"
)

.RSV_NAMES_B <- list(
  ds      = "ADVN",
  var     = "VNGRP",
  pop     = "VNFL",
  arm     = "VNTRT",
  sheet   = "Table 91.2.2",
  label   = "Recorded band"
)

.RSV_VOCABS <- list(A = .RSV_NAMES_A, B = .RSV_NAMES_B)

## One operation, "computes nothing", exactly as .build_unsupported_method()
## declares it. Named locally so the test does not depend on the builder.
.rsv_reserved_method <- function(id = "MTH_RESERVED_SYNTH") {
  list(
    id = id, name = "Reserved (manual)", label = "Reserved (manual)",
    description = "No executor; reserved for manual derivation.",
    supported = FALSE,
    operations = list(list(id = "OP_MANUAL", name = "Manual derivation",
                           label = "Manual derivation", order = 1L,
                           resultPattern = "X"))
  )
}

.rsv_counting_method <- function(id = "MTH_SYNTH_COUNT") {
  list(
    id = id, name = "Subject Count", label = "Subject Count",
    description = "Unique subject count.",
    supported = TRUE,
    operations = list(list(id = "OP_N", name = "n", label = "n",
                           order = 1L, resultPattern = "XXX"))
  )
}

#' A one-output event whose single row sits on `method`, displayed with a
#' placeholder of `n_slots` numbers.
#'
#' The default -- a two-number placeholder over a one-operation method -- is
#' the shape that used to make arsbridge's own reservation refuse the whole
#' event.
#'
#' @param vocab One of `.RSV_VOCABS`.
#' @param method A method object; `.rsv_reserved_method()` or
#'   `.rsv_counting_method()`.
#' @param n_slots How many numbers the shell cell displays.
#' @noRd
.rsv_event <- function(vocab = .RSV_NAMES_A,
                       method = .rsv_reserved_method(),
                       n_slots = 2L,
                       row_label = NULL) {
  row_label <- row_label %||% vocab$label
  analysis_id <- "AN_SYNTH_001"
  output_id <- "T_SYNTH"

  ## The placeholder the shell author wrote, and the statistic each of its
  ## numbers asks for. Two numbers is "xx (xx.x)": a count and a percentage.
  slot_stats <- c("n", "p")[seq_len(n_slots)]
  slots <- lapply(slot_stats, function(stat) list(stat_name = stat))
  placeholder <- paste(c("xx", "xx.x")[seq_len(n_slots)], collapse = " ")

  list(
    id = "RE_SYNTH", name = "synthetic", version = "1",
    analysisSets = list(list(
      id = "AS_SYNTH", name = "Analysed", label = "Analysed",
      level = 1L, order = 1L,
      condition = list(dataset = vocab$ds, variable = vocab$pop,
                       comparator = "EQ", value = list("Y"))
    )),
    dataSubsets = list(),
    analysisGroupings = list(list(
      id = "GF_SYNTH", name = vocab$arm,
      groupingDataset = vocab$ds, groupingVariable = vocab$arm,
      dataDriven = TRUE, groups = list()
    )),
    methods = list(method),
    analyses = list(list(
      id = analysis_id, name = row_label, label = row_label,
      description = row_label,
      analysisSetId = "AS_SYNTH", methodId = method$id,
      dataset = vocab$ds, variable = vocab$var,
      analysisVariable = list(dataset = vocab$ds, variable = vocab$var),
      orderedGroupings = list(list(order = 1L, groupingId = "GF_SYNTH",
                                   resultsByGroup = TRUE))
    )),
    outputs = list(list(
      id = output_id, name = vocab$sheet, label = vocab$sheet,
      outputType = "TABLE",
      referencedAnalysisIds = list(analysis_id),
      `_meta` = list(
        shell_layout = list(list(
          sheet_row = 6L, indent = 0L, kind = "data",
          analysis_id = analysis_id, label = row_label,
          n_slots = as.integer(n_slots)
        )),
        shell_fill = list(
          source = list(sheet = vocab$sheet, format = "xlsx",
                        first_body_row = 6L, header_row = 5L,
                        header_rows = list(5L)),
          columns = list(list(col = 2L, order = 1L, label = "Group 1")),
          cells = list(list(
            row = 6L, col = 2L, ref = "B6", kind = "result",
            analysis_id = analysis_id, placeholder = placeholder,
            slots = slots
          ))
        )
      )
    ))
  )
}

.rsv_model <- function(...) ars_to_model(.rsv_event(...))


## Mutation-test plumbing.
##
## devtools::load_all() puts arsbridge's objects in TWO places: the namespace,
## which in-package code resolves against, and an attached copy on the search
## path, which test code sees. Replacing only one of them means the mutation
## silently does not reach the code under test, and the mutation test passes by
## measuring nothing. So both are replaced, and both are checked.
.rsv_targets <- function(name) {
  envs <- list(asNamespace("arsbridge"))
  attached <- "package:arsbridge"
  if (attached %in% search()) envs <- c(envs, list(as.environment(attached)))
  Filter(function(env) exists(name, envir = env, inherits = FALSE), envs)
}

.rsv_install <- function(name, value) {
  envs <- .rsv_targets(name)
  if (length(envs) == 0L) stop("nothing named ", name, " to mutate")

  ## The replacement must resolve arsbridge's own internals, so it is parented
  ## at the namespace -- but a mutation usually closes over the original
  ## function it wraps, and simply re-parenting would throw that away. So the
  ## closure's own bindings are carried into an environment that sits under the
  ## namespace, giving it both.
  ns <- asNamespace("arsbridge")
  own <- environment(value)
  if (is.function(value) && !is.null(own) && !identical(own, ns)) {
    holder <- new.env(parent = ns)
    for (bound in ls(own, all.names = TRUE)) {
      assign(bound, get(bound, envir = own), envir = holder)
    }
    environment(value) <- holder
  }

  for (env in envs) {
    was_locked <- bindingIsLocked(name, env)
    if (was_locked) unlockBinding(name, env)
    assign(name, value, envir = env)
    if (was_locked) lockBinding(name, env)
  }

  ## Proof the mutation landed everywhere it had to.
  for (env in envs) {
    stopifnot(identical(get(name, envir = env), value))
  }
  invisible(length(envs))
}

.rsv_restore <- function(name, value) .rsv_install(name, value)
