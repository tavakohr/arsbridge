## arsbridge -- ars_model.R
## ---------------------------------------------------------------------------
## The round-trip core of the ARS review/edit stage: a tabular, editable view
## of an ARS reporting event, and the inverse that puts it back together.
##
##   ars_to_model(ars)   nested ARS JSON  ->  one data frame per entity pool
##   model_to_ars(model) data frames      ->  nested ARS JSON
##
## Design (the reason this file has no UI and no LLM code):
##
##  * Every pool row carries the flat fields a reviewer edits AND a `raw`
##    list-column holding the original untouched node. Patching writes the
##    edited fields back into `raw`; every field the editor does not surface
##    rides along untouched. Round-trip fidelity is therefore the default
##    rather than something we have to maintain field by field.
##
##  * The top level is reassembled by walking the ORIGINAL template's keys in
##    the original order, substituting only the six entity pools. Unknown or
##    future keys pass through, and `_meta` is never touched.
##
##  * mainListOfContents / otherListsOfContents are pure derivations of the
##    outputs list (see .build_lopa / .build_lopo in build_ars_json.R). They
##    are copied verbatim when nothing structural changed and regenerated when
##    it did, rather than being patched in place.
##
## Column naming convention, relied on by the patchers:
##   camelCase  -- maps 1:1 onto the ARS field of the same name
##   snake_case -- either derived and read-only, or a documented composite
##                 that several ARS fields are rebuilt from
## Every pool ends with a `raw` list-column.


## --- small helpers ---------------------------------------------------------

## Scalar character coercion for fields read with simplifyVector = FALSE.
## Returns NA_character_ (not NULL) so it can fill a data frame cell.
#' @noRd
.chr_field <- function(x) {
  if (is.null(x)) return(NA_character_)
  val <- unlist(x)
  if (length(val) == 0) return(NA_character_)
  as.character(val[1])
}

#' @noRd
.lgl_field <- function(x) {
  if (is.null(x)) return(NA)
  val <- unlist(x)
  if (length(val) == 0) return(NA)
  as.logical(val[1])
}

#' @noRd
.int_field <- function(x) {
  if (is.null(x)) return(NA_integer_)
  val <- suppressWarnings(as.integer(unlist(x)))
  if (length(val) == 0) return(NA_integer_)
  val[1]
}

## Composite columns join several values into one cell. ";" is the separator,
## which validate_ars_model() warns about if it ever appears in the data.
.MODEL_SEP <- ";"

#' @noRd
.join_values <- function(x) {
  if (is.null(x)) return(NA_character_)
  val <- unlist(x)
  if (length(val) == 0) return(NA_character_)
  paste(as.character(val), collapse = .MODEL_SEP)
}

#' @noRd
.split_values <- function(x) {
  if (is.na(x) || !nzchar(x)) return(character(0))
  trimws(strsplit(x, .MODEL_SEP, fixed = TRUE)[[1]])
}

## Build a data frame from a list of single-row lists. Keeps the `raw` nodes
## in a list-column and never converts strings to factors.
#' @noRd
.rows_to_df <- function(rows, raws, columns) {
  if (length(rows) == 0) return(.empty_pool(columns))

  df <- do.call(rbind.data.frame, c(rows, list(
    stringsAsFactors = FALSE,
    make.row.names   = FALSE
  )))
  df$raw <- raws
  df[, c(columns, "raw"), drop = FALSE]
}

## First element of a list that may be absent or empty. Reporting events in
## the wild are not always fully populated -- an output can arrive with no
## displays or no file specification at all -- and reading one must not be an
## error.
#' @noRd
.first_or_empty <- function(x) {
  if (is.null(x) || length(x) == 0) return(list())
  x[[1]]
}

## Zero-row data frame carrying the full canonical column set, so downstream
## code can rely on the columns existing even for an absent pool.
#' @noRd
.empty_pool <- function(columns) {
  df <- as.data.frame(
    stats::setNames(
      rep(list(character(0)), length(columns)),
      columns
    ),
    stringsAsFactors = FALSE
  )
  df$raw <- list()
  df
}

## Assign a value into a node, where NA means "this key is absent".
##
## Every scalar field goes through here, which is what keeps the round trip
## honest in both directions: assigning NULL deletes a key that exists (the
## reviewer cleared an optional field such as `strata`) and is a no-op when
## the key was never there (so a node that never carried `description` does
## not acquire an empty one just by being patched).
#' @noRd
.set_or_drop <- function(node, key, value) {
  if (length(value) == 0 || is.na(value)) {
    node[[key]] <- NULL
  } else {
    node[[key]] <- value
  }
  node
}


## --- pool builders ---------------------------------------------------------

.ANALYSIS_COLUMNS <- c(
  "id", "name", "label", "description",
  "analysisSetId", "dataSubsetId", "methodId",
  "dataset", "variable", "variableRole",
  "annotation", "sapDescription", "includeTotal", "strata",
  "grouping_ids", "output_id"
)

#' @noRd
.pool_analyses <- function(ars) {
  nodes <- ars[["analyses"]] %||% list()
  if (length(nodes) == 0) return(.empty_pool(.ANALYSIS_COLUMNS))

  analysis_to_output <- .build_analysis_to_output(ars)

  rows <- lapply(nodes, function(node) {
    grouping_ids <- vapply(
      node[["orderedGroupings"]] %||% list(),
      function(g) .chr_field(g[["groupingId"]]),
      character(1)
    )
    analysis_id <- .chr_field(node[["id"]])

    list(
      id             = analysis_id,
      name           = .chr_field(node[["name"]]),
      label          = .chr_field(node[["label"]]),
      description    = .chr_field(node[["description"]]),
      analysisSetId  = .chr_field(node[["analysisSetId"]]),
      dataSubsetId   = .chr_field(node[["dataSubsetId"]]),
      methodId       = .chr_field(node[["methodId"]]),
      dataset        = .chr_field(node[["dataset"]]),
      variable       = .chr_field(node[["variable"]]),
      variableRole   = .chr_field(node[["variableRole"]]),
      annotation     = .chr_field(node[["annotation"]]),
      sapDescription = .chr_field(node[["sapDescription"]]),
      includeTotal   = .lgl_field(node[["includeTotal"]]),
      strata         = .chr_field(node[["strata"]]),
      grouping_ids   = if (length(grouping_ids) == 0) {
        NA_character_
      } else {
        paste(grouping_ids, collapse = .MODEL_SEP)
      },
      output_id      = analysis_to_output[[analysis_id]] %||% NA_character_
    )
  })

  .rows_to_df(rows, nodes, .ANALYSIS_COLUMNS)
}

.METHOD_COLUMNS <- c(
  "id", "name", "label", "description",
  "context", "code", "supported",
  "n_operations", "operation_summary"
)

#' @noRd
.pool_methods <- function(ars) {
  nodes <- ars[["methods"]] %||% list()
  if (length(nodes) == 0) return(.empty_pool(.METHOD_COLUMNS))

  rows <- lapply(nodes, function(node) {
    operations <- node[["operations"]] %||% list()
    op_names <- vapply(
      operations,
      function(op) .chr_field(op[["name"]]),
      character(1)
    )

    list(
      id                = .chr_field(node[["id"]]),
      name              = .chr_field(node[["name"]]),
      label             = .chr_field(node[["label"]]),
      description       = .chr_field(node[["description"]]),
      context           = .chr_field(node[["codeTemplate"]][["context"]]),
      code              = .chr_field(node[["codeTemplate"]][["code"]]),
      supported         = .lgl_field(node[["supported"]]),
      n_operations      = length(operations),
      operation_summary = if (length(op_names) == 0) {
        NA_character_
      } else {
        paste(op_names, collapse = ", ")
      }
    )
  })

  .rows_to_df(rows, nodes, .METHOD_COLUMNS)
}

## Analysis sets and data subsets share the WhereClause shape. The simple
## `condition` form is surfaced as four editable columns; a compound
## expression leaves them NA and is edited through the raw-JSON escape hatch.
#' @noRd
.where_columns <- function(node) {
  condition <- node[["condition"]]
  is_compound <- !is.null(node[["compoundExpression"]])

  summary_text <- NA_character_
  where <- if (is_compound) {
    list(compoundExpression = node[["compoundExpression"]])
  } else if (!is.null(condition)) {
    list(condition = condition)
  } else {
    NULL
  }
  if (!is.null(where)) {
    summary_text <- tryCatch(
      where_to_filter_expr(where),
      error = function(e) NA_character_
    )
  }

  list(
    condition_dataset    = .chr_field(condition[["dataset"]]),
    condition_variable   = .chr_field(condition[["variable"]]),
    condition_comparator = .chr_field(condition[["comparator"]]),
    condition_value      = .join_values(condition[["value"]]),
    is_compound          = is_compound,
    condition_summary    = summary_text
  )
}

.ANALYSIS_SET_COLUMNS <- c(
  "id", "name", "label",
  "condition_dataset", "condition_variable",
  "condition_comparator", "condition_value",
  "is_compound", "condition_summary", "annotationText",
  "level", "order"
)

#' @noRd
.pool_analysis_sets <- function(ars) {
  nodes <- ars[["analysisSets"]] %||% list()
  if (length(nodes) == 0) return(.empty_pool(.ANALYSIS_SET_COLUMNS))

  rows <- lapply(nodes, function(node) {
    c(
      list(
        id    = .chr_field(node[["id"]]),
        name  = .chr_field(node[["name"]]),
        label = .chr_field(node[["label"]])
      ),
      .where_columns(node),
      list(
        annotationText = .chr_field(node[["annotationText"]]),
        level          = .int_field(node[["level"]]),
        order          = .int_field(node[["order"]])
      )
    )
  })

  .rows_to_df(rows, nodes, .ANALYSIS_SET_COLUMNS)
}

.DATA_SUBSET_COLUMNS <- c(
  "id", "name", "label",
  "condition_dataset", "condition_variable",
  "condition_comparator", "condition_value",
  "is_compound", "condition_summary",
  "level", "order"
)

#' @noRd
.pool_data_subsets <- function(ars) {
  nodes <- ars[["dataSubsets"]] %||% list()
  if (length(nodes) == 0) return(.empty_pool(.DATA_SUBSET_COLUMNS))

  rows <- lapply(nodes, function(node) {
    c(
      list(
        id    = .chr_field(node[["id"]]),
        name  = .chr_field(node[["name"]]),
        label = .chr_field(node[["label"]])
      ),
      .where_columns(node),
      list(
        level = .int_field(node[["level"]]),
        order = .int_field(node[["order"]])
      )
    )
  })

  .rows_to_df(rows, nodes, .DATA_SUBSET_COLUMNS)
}

.GROUPING_COLUMNS <- c(
  "id", "name", "label",
  "groupingDataset", "groupingVariable", "dataDriven",
  "n_groups", "group_labels"
)

## Grouping factors appear in two shapes in the wild: the flat
## groupingDataset / groupingVariable strings this package generates, and an
## older nested groupingVariable = {dataset, variable}. Both are read here and
## the patcher writes back in whichever shape the node arrived in.
#' @noRd
.pool_groupings <- function(ars) {
  nodes <- ars[["analysisGroupings"]] %||% list()
  if (length(nodes) == 0) return(.empty_pool(.GROUPING_COLUMNS))

  rows <- lapply(nodes, function(node) {
    nested <- is.list(node[["groupingVariable"]])
    groups <- node[["groups"]] %||% list()
    labels <- vapply(
      groups,
      function(g) .chr_field(g[["label"]] %||% g[["name"]]),
      character(1)
    )

    list(
      id               = .chr_field(node[["id"]]),
      name             = .chr_field(node[["name"]]),
      label            = .chr_field(node[["label"]]),
      groupingDataset  = if (nested) {
        .chr_field(node[["groupingVariable"]][["dataset"]])
      } else {
        .chr_field(node[["groupingDataset"]])
      },
      groupingVariable = if (nested) {
        .chr_field(node[["groupingVariable"]][["variable"]])
      } else {
        .chr_field(node[["groupingVariable"]])
      },
      dataDriven       = .lgl_field(node[["dataDriven"]]),
      n_groups         = length(groups),
      group_labels     = if (length(labels) == 0) {
        NA_character_
      } else {
        paste(labels, collapse = .MODEL_SEP)
      }
    )
  })

  .rows_to_df(rows, nodes, .GROUPING_COLUMNS)
}

.OUTPUT_COLUMNS <- c(
  "id", "name", "label", "outputType", "display_title",
  "referenced_analysis_ids", "n_analyses",
  "file_name", "file_type", "n_footnotes", "source_datasets"
)

#' @noRd
.pool_outputs <- function(ars) {
  nodes <- ars[["outputs"]] %||% list()
  if (length(nodes) == 0) return(.empty_pool(.OUTPUT_COLUMNS))

  rows <- lapply(nodes, function(node) {
    display   <- .first_or_empty(node[["displays"]])
    file_spec <- .first_or_empty(node[["fileSpecifications"]])
    analysis_ids <- unlist(node[["referencedAnalysisIds"]] %||% list())

    footnotes <- 0L
    for (section in display[["displaySections"]] %||% list()) {
      footnotes <- footnotes + length(section[["subSections"]] %||% list())
    }

    list(
      id                      = .chr_field(node[["id"]]),
      name                    = .chr_field(node[["name"]]),
      label                   = .chr_field(node[["label"]]),
      outputType              = .chr_field(node[["outputType"]]),
      display_title           = .chr_field(display[["displayTitle"]]),
      referenced_analysis_ids = if (length(analysis_ids) == 0) {
        NA_character_
      } else {
        paste(analysis_ids, collapse = .MODEL_SEP)
      },
      n_analyses              = length(analysis_ids),
      file_name               = .chr_field(file_spec[["name"]]),
      file_type               = .chr_field(file_spec[["fileType"]]),
      n_footnotes             = footnotes,
      source_datasets         = .join_values(
        node[["_meta"]][["source_datasets"]]
      )
    )
  })

  .rows_to_df(rows, nodes, .OUTPUT_COLUMNS)
}


## --- ars_to_model ----------------------------------------------------------

#' Turn an ARS reporting event into an editable tabular model
#'
#' Reads a CDISC ARS v1.0 reporting event and returns one data frame per
#' entity pool, each row carrying the flat fields a reviewer edits plus a
#' `raw` list-column holding the original untouched node. [model_to_ars()]
#' is the exact inverse: an unedited model round-trips to a structurally
#' identical reporting event.
#'
#' This is the foundation of the review/correct stage
#' (`spec_to_ars()` -> review -> [ars_to_ard()]). It depends only on
#' `jsonlite` -- no Shiny, no LLM -- so the pools are equally usable from a
#' plain script.
#'
#' @param ars Either a path to an ARS JSON file, or an already parsed
#'   reporting event (a list, as returned in `spec_to_ars()$reporting_event`).
#'
#' @return An object of class `ars_model`: a list with one data frame per
#'   pool -- `analyses`, `methods`, `analysis_sets`, `data_subsets`,
#'   `groupings`, `outputs` -- plus `template` (the untouched parsed reporting
#'   event) and `source_path` (the file it was read from, or `NULL`).
#'
#'   Pools that are absent from the reporting event come back as zero-row data
#'   frames with the full column set, so downstream code can always rely on
#'   the columns existing.
#'
#' @section Column conventions:
#' Columns named exactly like an ARS field (camelCase, e.g. `methodId`) map
#' onto that field one-to-one. Snake_case columns are either derived and
#' read-only (`output_id`, `n_analyses`, `condition_summary`) or documented
#' composites that several ARS fields are rebuilt from -- `grouping_ids`
#' (semicolon-separated, ordered, rebuilds `orderedGroupings`) and
#' `referenced_analysis_ids` (semicolon-separated, rebuilds the output's
#' analysis references and therefore the tables of contents).
#'
#' An `NA` in an optional column such as `strata` means the key is absent from
#' the node; setting it back to `NA` removes the key again.
#'
#' @seealso [model_to_ars()] to serialize back, [validate_ars_model()] to
#'   check integrity.
#'
#' @examples
#' \dontrun{
#' model <- ars_to_model("reporting_event.json")
#' model$analyses[, c("id", "methodId", "dataset", "variable")]
#' }
#' @export
ars_to_model <- function(ars) {
  source_path <- NULL

  if (is.character(ars) && length(ars) == 1) {
    source_path <- ars
    .require_file(ars, "ars", "the ARS JSON")
    ars <- .read_json(ars)
  }

  if (!is.list(ars)) {
    cli::cli_abort(c(
      "{.arg ars} must be a path to an ARS JSON file or a parsed reporting event.",
      "x" = "Got {.cls {class(ars)[1]}}.",
      "i" = "Use {.code spec_to_ars(...)$reporting_event} for an in-memory event."
    ))
  }

  structure(
    list(
      analyses      = .pool_analyses(ars),
      methods       = .pool_methods(ars),
      analysis_sets = .pool_analysis_sets(ars),
      data_subsets  = .pool_data_subsets(ars),
      groupings     = .pool_groupings(ars),
      outputs       = .pool_outputs(ars),
      template      = ars,
      source_path   = source_path
    ),
    class = "ars_model"
  )
}

#' @export
print.ars_model <- function(x, ...) {
  cli::cli_h3("ARS model")
  study <- .chr_field(x$template[["id"]])
  if (!is.na(study)) {
    cli::cli_text("Study {.val {study}}")
  }
  if (!is.null(x$source_path)) {
    cli::cli_text("Source: {.path {x$source_path}}")
  }
  generator <- .chr_field(x$template[["_meta"]][["generator"]])
  if (!is.na(generator)) {
    cli::cli_text("Generated by {.val {generator}}")
  }

  cli::cli_ul(c(
    "{nrow(x$outputs)} output{?s}",
    "{nrow(x$analyses)} analys{?is/es}",
    "{nrow(x$methods)} method{?s}",
    "{nrow(x$analysis_sets)} analysis set{?s}",
    "{nrow(x$data_subsets)} data subset{?s}",
    "{nrow(x$groupings)} grouping{?s}"
  ))
  invisible(x)
}


## --- node patchers ---------------------------------------------------------
##
## Each patcher takes the original node and its (possibly edited) model row,
## and writes the row's editable fields back into the node. Patchers only ever
## touch the keys they own: everything else in the node survives untouched,
## which is what makes the round trip lossless.

#' @noRd
.patch_analysis_node <- function(node, row) {
  node <- .set_or_drop(node, "name", row$name)
  node <- .set_or_drop(node, "label", row$label)
  node <- .set_or_drop(node, "description", row$description)
  node <- .set_or_drop(node, "analysisSetId", row$analysisSetId)
  node <- .set_or_drop(node, "dataSubsetId", row$dataSubsetId)
  node <- .set_or_drop(node, "methodId", row$methodId)
  node <- .set_or_drop(node, "annotation", row$annotation)
  node <- .set_or_drop(node, "sapDescription", row$sapDescription)
  node <- .set_or_drop(node, "variableRole", row$variableRole)
  node <- .set_or_drop(node, "includeTotal", row$includeTotal)
  node <- .set_or_drop(node, "strata", row$strata)

  ## dataset / variable live twice: as flat strings (which siera reads) and
  ## inside the nested analysisVariable. Both must move together.
  node <- .set_or_drop(node, "dataset", row$dataset)
  node <- .set_or_drop(node, "variable", row$variable)
  if (!is.null(node[["analysisVariable"]])) {
    node[["analysisVariable"]] <- .set_or_drop(
      node[["analysisVariable"]], "dataset", row$dataset
    )
    node[["analysisVariable"]] <- .set_or_drop(
      node[["analysisVariable"]], "variable", row$variable
    )
  }

  ## orderedGroupings is rebuilt only when the grouping list actually changed,
  ## so an untouched analysis keeps its original node byte for byte.
  current <- vapply(
    node[["orderedGroupings"]] %||% list(),
    function(g) .chr_field(g[["groupingId"]]),
    character(1)
  )
  wanted <- .split_values(row$grouping_ids)

  if (!identical(as.character(current), as.character(wanted))) {
    ## Preserve each grouping's resultsByGroup flag across the reorder.
    by_group <- stats::setNames(
      lapply(node[["orderedGroupings"]] %||% list(), function(g) {
        g[["resultsByGroup"]] %||% TRUE
      }),
      current
    )
    node[["orderedGroupings"]] <- lapply(seq_along(wanted), function(i) {
      list(
        order          = i,
        groupingId     = wanted[i],
        resultsByGroup = by_group[[wanted[i]]] %||% TRUE
      )
    })
  }

  node
}

#' @noRd
.patch_method_node <- function(node, row) {
  node <- .set_or_drop(node, "name", row$name)
  node <- .set_or_drop(node, "label", row$label)
  node <- .set_or_drop(node, "description", row$description)

  if (!is.null(node[["codeTemplate"]])) {
    node[["codeTemplate"]] <- .set_or_drop(
      node[["codeTemplate"]], "context", row$context
    )
    node[["codeTemplate"]] <- .set_or_drop(
      node[["codeTemplate"]], "code", row$code
    )
  }

  .set_or_drop(node, "supported", row$supported)
}

## Shared by analysis sets and data subsets. A compound expression is never
## patched from the flat columns -- it is edited through the raw-JSON escape
## hatch, which replaces the whole node.
#' @noRd
.patch_where_node <- function(node, row) {
  node <- .set_or_drop(node, "name", row$name)
  node <- .set_or_drop(node, "label", row$label)

  if (!isTRUE(row$is_compound) && !is.null(node[["condition"]])) {
    condition <- node[["condition"]]
    condition <- .set_or_drop(condition, "dataset", row$condition_dataset)
    condition <- .set_or_drop(condition, "variable", row$condition_variable)
    condition <- .set_or_drop(condition, "comparator",
                              row$condition_comparator)

    values <- .split_values(row$condition_value)
    if (length(values) > 0) condition[["value"]] <- as.list(values)

    node[["condition"]] <- condition
  }

  node <- .set_or_drop(node, "level", row$level)
  .set_or_drop(node, "order", row$order)
}

#' @noRd
.patch_analysis_set_node <- function(node, row) {
  node <- .patch_where_node(node, row)
  .set_or_drop(node, "annotationText", row$annotationText)
}

#' @noRd
.patch_data_subset_node <- function(node, row) {
  .patch_where_node(node, row)
}

#' @noRd
.patch_grouping_node <- function(node, row) {
  node <- .set_or_drop(node, "name", row$name)
  node <- .set_or_drop(node, "label", row$label)

  ## Write back in whichever shape the node arrived in.
  if (is.list(node[["groupingVariable"]])) {
    nested <- node[["groupingVariable"]]
    nested <- .set_or_drop(nested, "dataset", row$groupingDataset)
    nested <- .set_or_drop(nested, "variable", row$groupingVariable)
    node[["groupingVariable"]] <- nested
  } else {
    node <- .set_or_drop(node, "groupingDataset", row$groupingDataset)
    node <- .set_or_drop(node, "groupingVariable", row$groupingVariable)
  }

  .set_or_drop(node, "dataDriven", row$dataDriven)
}

#' @noRd
.patch_output_node <- function(node, row) {
  node <- .set_or_drop(node, "name", row$name)
  node <- .set_or_drop(node, "label", row$label)
  node <- .set_or_drop(node, "outputType", row$outputType)

  if (length(node[["displays"]] %||% list()) > 0) {
    node[["displays"]][[1]] <- .set_or_drop(
      node[["displays"]][[1]], "displayTitle", row$display_title
    )
  }

  ## Rebuild the analysis references only on a real change, so untouched
  ## outputs keep their original list object.
  current <- as.character(unlist(node[["referencedAnalysisIds"]] %||% list()))
  wanted  <- .split_values(row$referenced_analysis_ids)
  if (!identical(current, as.character(wanted))) {
    node[["referencedAnalysisIds"]] <- as.list(wanted)
  }

  node
}

## Pool name -> (template key, patcher). The order here is also the order the
## pools are checked in when a caller names one.
#' @noRd
.pool_registry <- function() {
  list(
    analyses      = list(key = "analyses",
                         patch = .patch_analysis_node),
    methods       = list(key = "methods",
                         patch = .patch_method_node),
    analysis_sets = list(key = "analysisSets",
                         patch = .patch_analysis_set_node),
    data_subsets  = list(key = "dataSubsets",
                         patch = .patch_data_subset_node),
    groupings     = list(key = "analysisGroupings",
                         patch = .patch_grouping_node),
    outputs       = list(key = "outputs",
                         patch = .patch_output_node)
  )
}


## --- model_to_ars ----------------------------------------------------------

## The structural signature captures everything the tables of contents are
## derived from. When it is unchanged, the original tables are copied verbatim
## (padding and all); when it changes, they are regenerated.
#' @noRd
.structural_signature <- function(analysis_ids, output_ids, refs_per_output) {
  list(
    analysis_ids    = as.character(analysis_ids),
    output_ids      = as.character(output_ids),
    refs_per_output = lapply(refs_per_output, as.character)
  )
}

#' @noRd
.signature_from_ars <- function(ars) {
  outputs <- ars[["outputs"]] %||% list()
  .structural_signature(
    analysis_ids = vapply(
      ars[["analyses"]] %||% list(),
      function(a) .chr_field(a[["id"]]),
      character(1)
    ),
    output_ids = vapply(outputs, function(o) .chr_field(o[["id"]]), character(1)),
    refs_per_output = lapply(outputs, function(o) {
      unlist(o[["referencedAnalysisIds"]] %||% list())
    })
  )
}

#' @noRd
.signature_from_nodes <- function(analysis_nodes, output_nodes) {
  .structural_signature(
    analysis_ids = vapply(
      analysis_nodes,
      function(a) .chr_field(a[["id"]]),
      character(1)
    ),
    output_ids = vapply(
      output_nodes,
      function(o) .chr_field(o[["id"]]),
      character(1)
    ),
    refs_per_output = lapply(output_nodes, function(o) {
      unlist(o[["referencedAnalysisIds"]] %||% list())
    })
  )
}

#' Serialize an editable ARS model back to a reporting event
#'
#' The inverse of [ars_to_model()]. Each pool row's edited fields are written
#' back into the original node it came from, and the reporting event is
#' reassembled from the template so that every field the model does not
#' surface -- including `_meta` and any future ARS key -- survives untouched.
#'
#' The two tables of contents (`mainListOfContents` and
#' `otherListsOfContents`) are pure derivations of the outputs list. They are
#' copied verbatim when nothing structural changed, and regenerated from the
#' outputs when analyses or output references were added, removed or
#' reordered.
#'
#' @param model An `ars_model` from [ars_to_model()].
#' @param template The reporting event to reassemble from. Defaults to the
#'   template the model was read from, which is what you almost always want.
#'
#' @return A reporting event as a nested list, ready for
#'   `jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE, null = "null")`.
#'
#' @seealso [ars_to_model()], [validate_ars_model()].
#'
#' @examples
#' \dontrun{
#' model <- ars_to_model("reporting_event.json")
#' model$analyses$methodId[1] <- "MTH_SUBJECT_COUNT"
#' ars <- model_to_ars(model)
#' }
#' @export
model_to_ars <- function(model, template = NULL) {
  .assert_ars_model(model)
  if (is.null(template)) template <- model$template

  registry <- .pool_registry()

  ## Patch every pool's nodes from its rows.
  patched <- lapply(names(registry), function(pool) {
    df <- model[[pool]]
    patch <- registry[[pool]]$patch
    if (nrow(df) == 0) return(list())
    lapply(seq_len(nrow(df)), function(i) {
      patch(df$raw[[i]], df[i, , drop = FALSE])
    })
  })
  names(patched) <- names(registry)

  ## Decide once whether the tables of contents still describe this event.
  regenerate_contents <- !identical(
    .signature_from_ars(template),
    .signature_from_nodes(patched$analyses, patched$outputs)
  )

  ## Walk the template's keys in their original order, substituting the pools
  ## and (when needed) the regenerated tables of contents. Anything else --
  ## id, name, version, _meta, unknown future keys -- passes through.
  pool_keys <- vapply(registry, function(entry) entry$key, character(1))

  out <- list()
  for (key in names(template)) {
    pool <- names(registry)[match(key, pool_keys)]
    if (!is.na(pool)) {
      out[[key]] <- patched[[pool]]
    } else if (key == "mainListOfContents" && regenerate_contents) {
      out[[key]] <- .build_lopa(patched$outputs)
    } else if (key == "otherListsOfContents" && regenerate_contents) {
      out[[key]] <- .build_lopo(patched$outputs)
    } else {
      out[[key]] <- template[[key]]
    }
  }

  ## A pool the template never had is emitted only if it now has content, and
  ## the tables of contents are added only when a structural change means we
  ## have to describe outputs that were not described before. Neither is
  ## invented on a plain round trip.
  for (pool in names(registry)) {
    key <- registry[[pool]]$key
    if (is.null(template[[key]]) && length(patched[[pool]]) > 0) {
      out[[key]] <- patched[[pool]]
    }
  }
  if (regenerate_contents && length(patched$outputs) > 0) {
    if (is.null(template[["mainListOfContents"]])) {
      out[["mainListOfContents"]] <- .build_lopa(patched$outputs)
    }
    if (is.null(template[["otherListsOfContents"]])) {
      out[["otherListsOfContents"]] <- .build_lopo(patched$outputs)
    }
  }

  out
}

#' @noRd
.assert_ars_model <- function(model) {
  if (!inherits(model, "ars_model")) {
    cli::cli_abort(c(
      "{.arg model} must be an {.cls ars_model}.",
      "x" = "Got {.cls {class(model)[1]}}.",
      "i" = "Build one with {.code ars_to_model(ars)}."
    ))
  }
  invisible(model)
}


## --- model mutation API ----------------------------------------------------
##
## The editor never writes into the pool data frames directly: it goes through
## these helpers, so derived columns stay consistent and the same operations
## are available (and testable) from a plain script.

#' @noRd
.pool_or_abort <- function(model, pool) {
  if (!pool %in% names(.pool_registry())) {
    cli::cli_abort(c(
      "Unknown pool {.val {pool}}.",
      "i" = "Pools: {.val {names(.pool_registry())}}."
    ))
  }
  model[[pool]]
}

#' @noRd
.row_index <- function(df, id, pool) {
  idx <- which(df$id == id)
  if (length(idx) == 0) {
    cli::cli_abort("No {.val {id}} in the {.val {pool}} pool.")
  }
  idx[1]
}

## Set one field on one entity, then refresh whatever derived columns depend
## on it. Returns the updated model.
#' @noRd
model_set_field <- function(model, pool, id, field, value) {
  .assert_ars_model(model)
  df  <- .pool_or_abort(model, pool)
  idx <- .row_index(df, id, pool)

  if (!field %in% names(df)) {
    editable <- setdiff(names(df), "raw")
    cli::cli_abort(c(
      "{.val {field}} is not a column of the {.val {pool}} pool.",
      "i" = "Columns: {.val {editable}}."
    ))
  }
  if (identical(field, "id")) {
    cli::cli_abort(c(
      "Entity ids are read-only.",
      "i" = "Other entities reference {.val {id}}; renaming it would dangle them."
    ))
  }

  df[[field]][idx] <- value
  model[[pool]] <- df

  .model_refresh_row(model, pool, id)
}

## Recompute the derived columns of one row from its (patched) node, so the
## model stays self-consistent after an edit.
#' @noRd
.model_refresh_row <- function(model, pool, id) {
  df  <- model[[pool]]
  idx <- .row_index(df, id, pool)
  registry <- .pool_registry()

  node <- registry[[pool]]$patch(df$raw[[idx]], df[idx, , drop = FALSE])
  df$raw[[idx]] <- node

  refreshed <- switch(
    pool,
    analyses      = .pool_analyses(list(analyses = list(node),
                                        outputs = model$outputs$raw)),
    methods       = .pool_methods(list(methods = list(node))),
    analysis_sets = .pool_analysis_sets(list(analysisSets = list(node))),
    data_subsets  = .pool_data_subsets(list(dataSubsets = list(node))),
    groupings     = .pool_groupings(list(analysisGroupings = list(node))),
    outputs       = .pool_outputs(list(outputs = list(node)))
  )

  for (column in setdiff(names(df), "raw")) {
    df[[column]][idx] <- refreshed[[column]][1]
  }
  model[[pool]] <- df
  model
}

## Replace a whole node from raw JSON text. This is the escape hatch for the
## nested shapes the flat columns cannot express -- compound where-clauses,
## method operations, grouping levels.
#' @noRd
model_set_node_json <- function(model, pool, id, json_text) {
  .assert_ars_model(model)
  df  <- .pool_or_abort(model, pool)
  idx <- .row_index(df, id, pool)

  node <- tryCatch(
    jsonlite::fromJSON(json_text, simplifyVector = FALSE),
    error = function(e) {
      ## The message is data, not a template -- unparseable JSON is full of
      ## braces that would otherwise be read as cli interpolation.
      reason <- conditionMessage(e)
      cli::cli_abort(c(
        "That is not valid JSON.",
        "x" = "{reason}"
      ))
    }
  )

  if (!is.list(node) || is.null(node[["id"]])) {
    cli::cli_abort("The replacement must be a JSON object with an {.field id}.")
  }
  replacement_id <- .chr_field(node[["id"]])
  if (!identical(replacement_id, id)) {
    cli::cli_abort(c(
      "The replacement's {.field id} must stay {.val {id}}.",
      "x" = "Got {.val {replacement_id}}."
    ))
  }

  df$raw[[idx]] <- node
  model[[pool]] <- df
  .model_refresh_row(model, pool, id)
}

## Edit one field of one operation inside a method. Operations are a nested
## list, so they get their own accessor rather than a flat column.
#' @noRd
model_set_operation <- function(model, method_id, operation_index,
                                field, value) {
  .assert_ars_model(model)
  df  <- model$methods
  idx <- .row_index(df, method_id, "methods")

  node <- df$raw[[idx]]
  operations <- node[["operations"]] %||% list()
  if (operation_index < 1 || operation_index > length(operations)) {
    cli::cli_abort(
      "Method {.val {method_id}} has no operation {operation_index}."
    )
  }
  if (!field %in% c("name", "label", "resultPattern")) {
    cli::cli_abort(c(
      "Only {.val name}, {.val label} and {.val resultPattern} are editable.",
      "i" = "Operation ids and order define the method's contract."
    ))
  }

  node[["operations"]][[operation_index]][[field]] <- value
  df$raw[[idx]] <- node
  model$methods <- df
  .model_refresh_row(model, "methods", method_id)
}

## Add one of the standard methods to the file. Selecting a catalogue method
## in the editor calls this first, so the analysis never points at a methodId
## that is not in the reporting event.
#' @noRd
model_add_method_from_catalogue <- function(model, method_id) {
  .assert_ars_model(model)
  if (method_id %in% model$methods$id) return(model)

  catalogue <- .STANDARD_METHODS
  node <- NULL
  for (entry in catalogue) {
    if (identical(.chr_field(entry[["id"]]), method_id)) {
      node <- entry
      break
    }
  }
  if (is.null(node)) {
    known <- .standard_method_ids()
    cli::cli_abort(c(
      "{.val {method_id}} is not a standard arsbridge method.",
      "i" = "Standard methods: {.val {known}}."
    ))
  }

  node <- .with_op_self_rels(node)
  row  <- .pool_methods(list(methods = list(node)))
  model$methods <- rbind(model$methods, row)
  model
}

#' @noRd
.standard_method_ids <- function() {
  vapply(.STANDARD_METHODS, function(m) .chr_field(m[["id"]]), character(1))
}


## --- adding, removing and detaching -----------------------------------------
##
## The operations that change the SHAPE of the reporting event rather than the
## content of one node. They all go through the pools, so model_to_ars() sees
## the structural change and rebuilds the tables of contents by itself.

## A new analysis node, built to exactly the shape .build_analysis() emits so
## a hand-added line is indistinguishable from a generated one.
#' @noRd
.new_analysis_node <- function(id, name, label, dataset, variable,
                               method_id, analysis_set_id,
                               data_subset_id = "", grouping_ids = character(0),
                               annotation = "", include_total = FALSE) {
  grouping_ids <- grouping_ids[nzchar(grouping_ids %||% character())]

  list(
    id            = id,
    name          = name,
    label         = label,
    description   = label,
    version       = "1",
    categoryIds   = list(),
    analysisSetId = analysis_set_id,
    ## Flat for siera, nested for spec-correct consumers -- both, like the
    ## generator emits and the patcher keeps in sync.
    dataset          = dataset,
    variable         = variable,
    analysisVariable = list(dataset = dataset, variable = variable),
    dataSubsetId     = data_subset_id %||% "",
    orderedGroupings = lapply(seq_along(grouping_ids), function(i) {
      list(order = i, groupingId = grouping_ids[[i]], resultsByGroup = TRUE)
    }),
    ## The same self-referential NUM/DEN placeholders the generator emits;
    ## siera needs them present on at least one analysis.
    referencedAnalysisOperations = list(
      list(referencedOperationRelationshipId = "SELF_NUM", analysisId = id),
      list(referencedOperationRelationshipId = "SELF_DEN", analysisId = id)
    ),
    methodId       = method_id,
    annotation     = annotation,
    sapDescription = "",
    variableRole   = "ANALYSIS",
    includeTotal   = isTRUE(include_total)
  )
}

## Mint the next free analysis id for an output, following the generator's
## AN_<TLF>_<nnn> convention and skipping anything already taken.
#' @noRd
.next_analysis_id <- function(model, output_id) {
  index <- match(output_id, model$outputs$id)
  if (is.na(index)) {
    cli::cli_abort("No output {.val {output_id}} in this reporting event.")
  }

  tlf <- model$outputs$name[index]
  if (is.na(tlf) || !nzchar(tlf)) tlf <- output_id

  taken <- model$analyses$id
  next_index <- length(.split_values(
    model$outputs$referenced_analysis_ids[index]
  )) + 1L

  repeat {
    candidate <- make_analysis_id(tlf, next_index)
    if (!candidate %in% taken) return(candidate)
    next_index <- next_index + 1L
  }
}

## Add an analysis to an output at a chosen position.
##
## `after` is the id of the line to insert below, or NULL for the end. Display
## order is meaningful, so this inserts rather than appending blindly.
#' @noRd
model_add_analysis <- function(model, output_id, label, dataset, variable,
                               method_id, analysis_set_id,
                               data_subset_id = "",
                               grouping_ids = character(0),
                               annotation = "", include_total = FALSE,
                               after = NULL) {
  .assert_ars_model(model)

  output_index <- match(output_id, model$outputs$id)
  if (is.na(output_index)) {
    cli::cli_abort("No output {.val {output_id}} in this reporting event.")
  }

  analysis_id <- .next_analysis_id(model, output_id)
  tlf <- model$outputs$name[output_index]
  if (is.na(tlf) || !nzchar(tlf)) tlf <- output_id

  node <- .new_analysis_node(
    id              = analysis_id,
    name            = paste0("Analysis for ", tlf),
    label           = label,
    dataset         = dataset,
    variable        = variable,
    method_id       = method_id,
    analysis_set_id = analysis_set_id,
    data_subset_id  = data_subset_id,
    grouping_ids    = grouping_ids,
    annotation      = annotation,
    include_total   = include_total
  )

  ## The pool row is built by the same reader that parses a file, so an added
  ## line and a read one are the same thing from here on.
  row <- .pool_analyses(list(analyses = list(node),
                             outputs = model$outputs$raw))
  row$output_id <- output_id
  model$analyses <- rbind(model$analyses, row)

  ## Place it in the output's list at the requested position.
  references <- .split_values(
    model$outputs$referenced_analysis_ids[output_index]
  )
  position <- if (is.null(after)) {
    length(references)
  } else {
    match(after, references)
  }
  if (is.na(position)) position <- length(references)

  references <- append(references, analysis_id, after = position)
  model$outputs$referenced_analysis_ids[output_index] <- paste(
    references, collapse = .MODEL_SEP
  )
  model <- .model_refresh_row(model, "outputs", output_id)

  attr(model, "last_added") <- analysis_id
  model
}

## Remove an analysis and every reference to it, so removing a line cannot
## leave an output pointing at something that is gone.
#' @noRd
model_remove_analysis <- function(model, analysis_id) {
  .assert_ars_model(model)

  index <- match(analysis_id, model$analyses$id)
  if (is.na(index)) {
    cli::cli_abort("No analysis {.val {analysis_id}} in this reporting event.")
  }
  model$analyses <- model$analyses[-index, , drop = FALSE]

  for (i in seq_len(nrow(model$outputs))) {
    references <- .split_values(model$outputs$referenced_analysis_ids[i])
    if (!analysis_id %in% references) next

    remaining <- setdiff(references, analysis_id)
    model$outputs$referenced_analysis_ids[i] <- if (length(remaining) == 0) {
      NA_character_
    } else {
      paste(remaining, collapse = .MODEL_SEP)
    }
    model <- .model_refresh_row(model, "outputs", model$outputs$id[i])
  }

  model
}

## Give one analysis its own copy of a shared entity.
##
## Editing a shared population changes every analysis that points at it. When
## a reviewer wants to change just this line, they detach first: the entity is
## copied under a new id and only this analysis is repointed at the copy.
##
## Only condition-carrying entities can be detached. Analysis sets, data
## subsets and groupings are consumed by their CONTENT -- the engine reads the
## condition, not the id -- so a copy behaves exactly like the original until
## its condition is edited, which is the point. Methods are the opposite: the
## engine dispatches on the method id itself, so a copy under a new id has no
## executor and would quietly degrade a computed line into a generic summary.
## Changing which method a line uses is what the method dropdown is for.
#' @noRd
model_detach_entity <- function(model, pool, entity_id, analysis_id) {
  .assert_ars_model(model)

  if (identical(pool, "methods")) {
    cli::cli_abort(c(
      "A method cannot be detached into a per-analysis copy.",
      "x" = "The engine dispatches on the method id, so a copy under a new id
             has no executor and would fall back to a generic summary.",
      "i" = "Choose a different method for this analysis instead."
    ))
  }

  field <- switch(
    pool,
    analysis_sets = "analysisSetId",
    data_subsets  = "dataSubsetId",
    groupings     = "grouping_ids",
    cli::cli_abort("Cannot detach from the {.val {pool}} pool.")
  )

  df <- model[[pool]]
  entity_index <- .row_index(df, entity_id, pool)
  analysis_index <- .row_index(model$analyses, analysis_id, "analyses")

  variant_id <- .next_variant_id(df$id, entity_id)

  node <- df$raw[[entity_index]]
  node[["id"]] <- variant_id
  label <- .chr_field(node[["label"]])
  if (!is.na(label) && nzchar(label)) {
    node[["label"]] <- paste0(label, " (variant)")
  }

  ## Read the copy back through the pool reader so its columns are derived
  ## the same way every other row's are.
  copy <- switch(
    pool,
    methods       = .pool_methods(list(methods = list(node))),
    analysis_sets = .pool_analysis_sets(list(analysisSets = list(node))),
    data_subsets  = .pool_data_subsets(list(dataSubsets = list(node))),
    groupings     = .pool_groupings(list(analysisGroupings = list(node)))
  )
  model[[pool]] <- rbind(df, copy)

  ## Repoint only this analysis.
  if (identical(field, "grouping_ids")) {
    current <- .split_values(model$analyses$grouping_ids[analysis_index])
    current[current == entity_id] <- variant_id
    value <- paste(current, collapse = .MODEL_SEP)
  } else {
    value <- variant_id
  }
  model <- model_set_field(model, "analyses", analysis_id, field, value)

  attr(model, "last_added") <- variant_id
  model
}

#' @noRd
.next_variant_id <- function(taken, base_id) {
  candidate <- paste0(base_id, "_VARIANT")
  suffix <- 1L
  while (candidate %in% taken) {
    suffix <- suffix + 1L
    candidate <- paste0(base_id, "_VARIANT_", suffix)
  }
  candidate
}

## How many analyses reference each shared entity. The editor shows this so a
## reviewer can see that editing a method touches more than the line in front
## of them.
#' @noRd
.entity_usage <- function(model) {
  analyses <- model$analyses

  count_of <- function(ids) {
    ids <- ids[!is.na(ids) & nzchar(ids)]
    if (length(ids) == 0) return(stats::setNames(integer(0), character(0)))
    counts <- table(ids)
    stats::setNames(as.integer(counts), names(counts))
  }

  list(
    methods       = count_of(analyses$methodId),
    analysis_sets = count_of(analyses$analysisSetId),
    ## An empty dataSubsetId means "no subset", not usage of one.
    data_subsets  = count_of(analyses$dataSubsetId),
    groupings     = count_of(
      unlist(lapply(analyses$grouping_ids, .split_values))
    )
  )
}

## Usage lookup that answers zero for an entity nothing references, rather
## than erroring on a name that is not there.
#' @noRd
.usage_count <- function(usage, id) {
  if (length(usage) == 0) return(0L)
  count <- usage[id]
  if (length(count) == 0 || is.na(count)) 0L else as.integer(count)
}
