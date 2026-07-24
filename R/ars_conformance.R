## arsbridge -- ars_conformance.R
## ---------------------------------------------------------------------------
## Validation against the OFFICIAL CDISC ARS v1.0 JSON Schema, as distinct
## from validate_ars_model()'s referential and executability checks.
##
## arsbridge deliberately extends the standard in a few documented places
## (fields siera needs, provenance the renderer uses). Validating raw output
## against the official schema would bury real problems under those sanctioned
## extensions, so they are stripped first -- and everything that remains is a
## genuine divergence worth knowing about.
##
## The schema ships with the package, pinned to the v1.0.0 release of
## cdisc-org/analysis-results-standard (see inst/schema/README.md for
## provenance), so validation is reproducible and does not depend on the
## moving main branch.

## The sanctioned extensions, by the class they live on. Anything listed here
## is stripped before validation; anything NOT listed that the schema rejects
## is a real finding. Keeping this list short is a feature.
.ARS_EXTENSION_FIELDS <- list(
  ## Provenance and review metadata; never semantic.
  reporting_event = "_meta",
  ## referencedAnalysisIds is how siera (and this package) tie an output to
  ## its analyses; outputType drives the renderer; _meta carries shell layout.
  output          = c("_meta", "outputType", "referencedAnalysisIds"),
  ## columns preserves the shell's column-header order for the renderer.
  ## (order is NOT listed: the official shape wraps each display in an
  ## OrderedDisplay, where order is required -- the flat display placement is
  ## a reported divergence, not a sanctioned extension.)
  output_display  = "columns",
  ## The nested analysisVariable duplicates the standard flat dataset/variable
  ## pair; the rest carry shell annotations and executor hints.
  analysis        = c("analysisVariable", "annotation", "includeTotal",
                      "sapDescription", "strata", "variableRole"),
  ## The fallback when a population annotation could not be parsed.
  analysis_set    = "annotationText",
  ## Executability marker on methods the engine cannot compute.
  method          = "supported"
)

#' @noRd
.drop_fields <- function(node, fields, where, stripped) {
  present <- intersect(fields, names(node))
  for (field in present) node[[field]] <- NULL
  list(
    node     = node,
    stripped = union(stripped, if (length(present) > 0) {
      paste0(where, "$", present)
    } else {
      character(0)
    })
  )
}

## Remove the documented extensions from a reporting event, returning both the
## cleaned copy and the list of what was removed -- the caller reports the
## latter, so stripping is never invisible.
#' @noRd
.strip_ars_extensions <- function(ars) {
  stripped <- character(0)

  result <- .drop_fields(ars, .ARS_EXTENSION_FIELDS$reporting_event,
                         "reportingEvent", stripped)
  ars <- result$node
  stripped <- result$stripped

  ars[["outputs"]] <- lapply(ars[["outputs"]] %||% list(), function(output) {
    result <- .drop_fields(output, .ARS_EXTENSION_FIELDS$output,
                           "output", stripped)
    output <- result$node
    stripped <<- result$stripped

    output[["displays"]] <- lapply(
      output[["displays"]] %||% list(),
      function(display) {
        result <- .drop_fields(display, .ARS_EXTENSION_FIELDS$output_display,
                               "display", stripped)
        stripped <<- result$stripped
        result$node
      }
    )
    output
  })

  ars[["analyses"]] <- lapply(ars[["analyses"]] %||% list(), function(node) {
    result <- .drop_fields(node, .ARS_EXTENSION_FIELDS$analysis,
                           "analysis", stripped)
    stripped <<- result$stripped
    result$node
  })

  ars[["analysisSets"]] <- lapply(
    ars[["analysisSets"]] %||% list(),
    function(node) {
      result <- .drop_fields(node, .ARS_EXTENSION_FIELDS$analysis_set,
                             "analysisSet", stripped)
      stripped <<- result$stripped
      result$node
    }
  )

  ars[["methods"]] <- lapply(ars[["methods"]] %||% list(), function(node) {
    result <- .drop_fields(node, .ARS_EXTENSION_FIELDS$method,
                           "method", stripped)
    stripped <<- result$stripped
    result$node
  })

  list(ars = ars, stripped = sort(stripped))
}

#' Check a reporting event against the official CDISC ARS v1.0 schema
#'
#' Validates against the JSON Schema of the CDISC Analysis Results Standard,
#' pinned to the `v1.0.0` release of
#' \url{https://github.com/cdisc-org/analysis-results-standard} and shipped
#' with this package -- so the answer does not change when the standard's
#' development branch does.
#'
#' This is a different question from [validate_ars_model()]. That asks
#' "will this execute, and does every reference resolve?"; this asks "is the
#' file structurally what the standard says a reporting event is?". A file can
#' pass either one alone.
#'
#' @param ars What to check: a path to an ARS JSON file, a parsed reporting
#'   event, or an `ars_model` from [ars_to_model()].
#' @param strip_extensions Strip arsbridge's documented extension fields
#'   before validating (default `TRUE`), so the findings show genuine
#'   divergences rather than the extensions the pipeline relies on. Set to
#'   `FALSE` to see everything the schema would reject, extensions included.
#' @param schema_path Path to an alternative JSON Schema, if you want to
#'   validate against something other than the bundled v1.0.0 export.
#'
#' @return A data frame of schema violations -- `where` (the JSON path),
#'   `keyword` (the schema rule), `problem` -- with zero rows meaning the
#'   event conforms (after stripping, if enabled). The fields that were
#'   stripped are attached as `attr(, "stripped_extensions")`.
#'
#' @section The sanctioned extensions:
#' arsbridge extends ARS v1.0 in documented places: `_meta` blocks (top level
#' and per output), `referencedAnalysisIds` and `outputType` on outputs,
#' display `columns`, the nested `analysisVariable` duplicate plus
#' `annotation`/`sapDescription`/`includeTotal`/`strata`/`variableRole` on
#' analyses, `annotationText` on analysis sets, and `supported` on methods.
#' These exist for siera compatibility, the renderer, and review provenance;
#' stripping them is what makes the remaining findings meaningful.
#'
#' @section Known divergences this will report:
#' The generator does not yet emit everything ARS v1.0 requires, and this
#' function reports those gaps rather than hiding them:
#' \itemize{
#'   \item `reason` and `purpose` missing on every analysis (required, with
#'     controlled terminology -- assigning them needs a judgement about which
#'     terms apply, so no default is invented);
#'   \item `version` emitted as the string `"1"` where the standard wants an
#'     integer (top level, outputs and analyses);
#'   \item each entry of `displays` written flat, where the standard wraps it
#'     as `OrderedDisplay{order, display}` with `id` and `name` inside;
#'   \item `fileSpecifications[].fileType` emitted as a plain string (e.g.
#'     `"rtf"`) where the standard wants a terminology object;
#'   \item the placeholder `referencedOperationRole.controlledTerm` of `""`
#'     not being one of the allowed terms;
#'   \item `name` missing on the contents-list analysis entries.
#' }
#'
#' @seealso [validate_ars_model()] for referential and executability checks;
#'   [edit_ars()] notes the conformance count after each save.
#'
#' @examples
#' \dontrun{
#' findings <- ars_conformance("reporting_event.json")
#' subset(findings, keyword == "required")
#' attr(findings, "stripped_extensions")
#' }
#' @export
ars_conformance <- function(ars, strip_extensions = TRUE,
                            schema_path = NULL) {
  rlang::check_installed(
    "jsonvalidate",
    reason = "to validate against the CDISC ARS schema"
  )

  if (inherits(ars, "ars_model")) {
    ars <- model_to_ars(ars)
  } else if (is.character(ars) && length(ars) == 1) {
    ars <- .read_json(ars)
  }
  if (!is.list(ars)) {
    cli::cli_abort(c(
      paste0("{.arg ars} must be a path, a parsed reporting event, ",
             "or an {.cls ars_model}."),
      "x" = "Got {.cls {class(ars)[1]}}."
    ))
  }

  if (is.null(schema_path)) {
    schema_path <- system.file(
      "schema", "cdisc_ars_v1.0.0.schema.json", package = "arsbridge"
    )
  }
  .require_file(schema_path, "schema_path", "the ARS JSON Schema")

  stripped <- character(0)
  if (isTRUE(strip_extensions)) {
    result <- .strip_ars_extensions(ars)
    ars <- result$ars
    stripped <- result$stripped
  }

  validate <- jsonvalidate::json_validator(schema_path, engine = "ajv")
  json_text <- jsonlite::toJSON(ars, auto_unbox = TRUE, null = "null")
  valid <- validate(json_text, verbose = TRUE, greedy = TRUE)

  findings <- .tidy_schema_errors(attr(valid, "errors"))
  attr(findings, "stripped_extensions") <- stripped
  findings
}

## ajv's error table, reduced to what a reviewer needs: where, which rule,
## and what it said -- with the offending property named when ajv tucked it
## into params rather than the message.
#' @noRd
.tidy_schema_errors <- function(errors) {
  empty <- data.frame(
    where   = character(0),
    keyword = character(0),
    problem = character(0),
    stringsAsFactors = FALSE
  )
  if (is.null(errors) || nrow(errors) == 0) return(empty)

  where <- errors[["instancePath"]] %||% errors[["dataPath"]]
  where[!nzchar(where)] <- "/"

  problem <- errors[["message"]]
  params <- errors[["params"]]
  if (!is.null(params)) {
    for (i in seq_along(problem)) {
      extra_property <- params[i, ][["additionalProperty"]]
      extra_property <- unlist(extra_property)
      if (length(extra_property) == 1 && !is.na(extra_property)) {
        problem[i] <- paste0(problem[i], " ('", extra_property, "')")
      }
    }
  }

  findings <- data.frame(
    where   = where,
    keyword = errors[["keyword"]],
    problem = problem,
    stringsAsFactors = FALSE
  )
  findings <- unique(findings)
  findings <- findings[order(findings$where), , drop = FALSE]
  rownames(findings) <- NULL
  findings
}
