## arsbridge -- parse_adam_spec.R
## ---------------------------------------------------------------------------
## Reads an ADaM specification and returns a flat data frame plus a
## "DATASET.VARIABLE"-keyed lookup. Accepts EITHER format:
##
##   .xml  -> ADaM define.xml  (preferred -- the regulatory artifact)
##   .xlsx -> ADaM spec Excel  (used during development before define.xml
##           or .xls            exists -- often the only artifact available
##                              at TLF-annotation time)
##
## Tolerant of column-name variations in the Excel form (handles
## "Variable Name" / "VAR" / "variable", "Data Type" / "Type", etc.).

## Canonical column -> aliases the Excel parser recognises (case-insensitive)
.SPEC_COLUMN_ALIASES <- list(
  dataset  = c("dataset", "domain", "dataset name", "ds"),
  variable = c("variable", "variable name", "var", "name", "varname"),
  label    = c("label", "variable label", "description"),
  type     = c("type", "data type", "datatype", "data_type"),
  origin   = c("origin", "source"),
  codelist = c("codelist", "code list", "controlled terms",
               "codelist / controlled terms"),
  length   = c("length", "len"),
  mandatory = c("mandatory", "core", "required")
)

#' Parse an ADaM specification (define.xml or Excel)
#'
#' Dispatches on file extension. Both formats produce the same return shape
#' so downstream code (validation, ARS building) is format-agnostic.
#'
#' @param path Path to the ADaM spec file. Either:
#'   * `.xml` -- ADaM `define.xml` (preferred when available)
#'   * `.xlsx` / `.xls` -- ADaM specification Excel
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{`variables`}{Data frame with columns: `dataset`, `variable`,
#'       `label`, `type`, `origin`, `codelist`, `length`, `mandatory`.}
#'     \item{`lookup`}{Named list keyed by `"DATASET.VARIABLE"` -- each
#'       entry is the corresponding row as a named list.}
#'   }
#'
#' @keywords internal
#' @noRd
parse_adam_spec <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort("ADaM spec file not found: {.path {path}}")
  }
  ext <- tolower(tools::file_ext(path))
  if (ext == "xml") {
    return(.parse_adam_define_xml(path))
  }
  if (ext %in% c("xlsx", "xls")) {
    return(.parse_adam_excel(path))
  }
  cli::cli_abort(c(
    "Unsupported ADaM spec extension: {.val .{ext}}",
    "i" = "Use {.val .xml} (define.xml) or {.val .xlsx} / {.val .xls} (ADaM spec)."
  ))
}


## --- Excel branch ----------------------------------------------------------

.parse_adam_excel <- function(excel_path) {

  sheets <- tryCatch(readxl::excel_sheets(excel_path),
                     error = function(e) {
                       cli::cli_abort(c(
                         "Cannot open Excel file {.path {excel_path}}:",
                         "x" = conditionMessage(e)
                       ))
                     })

  ## Try each sheet; keep ones that look like a variable-level sheet
  ## (have both a Dataset/Domain column AND a Variable column).
  variable_rows <- list()
  for (sh in sheets) {
    df <- tryCatch(
      suppressMessages(readxl::read_excel(excel_path, sheet = sh,
                                          col_types = "text", .name_repair = "minimal")),
      error = function(e) NULL
    )
    if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) next

    mapping <- .map_spec_columns(names(df))
    ## Some workbooks use the sheet name as the dataset when there is no
    ## dataset column (one sheet per domain).
    has_var <- !is.null(mapping$variable)
    has_ds  <- !is.null(mapping$dataset)
    if (!has_var) next

    fallback_ds <- if (has_ds) NULL else toupper(sh)
    rows <- .normalise_spec_rows(df, mapping, fallback_ds)
    if (nrow(rows) > 0) {
      variable_rows[[length(variable_rows) + 1]] <- rows
    }
  }

  if (length(variable_rows) == 0) {
    cli::cli_abort(c(
      "No variable-level sheet found in {.path {excel_path}}.",
      "i" = "Expected a sheet with both a {.field Dataset} (or Domain) and {.field Variable} column."
    ))
  }

  variables <- unique(do.call(rbind, variable_rows))
  rownames(variables) <- NULL

  keys <- paste(variables$dataset, variables$variable, sep = ".")
  lookup <- setNames(
    lapply(seq_len(nrow(variables)), function(i) as.list(variables[i, ])),
    keys
  )

  list(variables = variables, lookup = lookup)
}


## --- Internal helpers ------------------------------------------------------

#' For a vector of column names, return a list mapping canonical names to the
#' actual column name found in the sheet (or NULL when not present).
#' @noRd
.map_spec_columns <- function(cols) {
  norm <- tolower(trimws(cols))
  out <- list()
  for (canon in names(.SPEC_COLUMN_ALIASES)) {
    aliases <- .SPEC_COLUMN_ALIASES[[canon]]
    hit_idx <- which(norm %in% aliases)
    out[[canon]] <- if (length(hit_idx) > 0) cols[hit_idx[1]] else NULL
  }
  out
}

#' Build a normalised data frame from a sheet's raw read-back and the column
#' mapping. Returns columns: dataset, variable, label, type, origin,
#' codelist, length, mandatory.
#' @noRd
.normalise_spec_rows <- function(df, mapping, fallback_ds) {
  get_col <- function(name) {
    src <- mapping[[name]]
    if (is.null(src)) return(rep(NA_character_, nrow(df)))
    trimws(as.character(df[[src]]))
  }

  dataset  <- get_col("dataset")
  if (all(is.na(dataset) | dataset == "") && !is.null(fallback_ds)) {
    dataset <- rep(fallback_ds, nrow(df))
  }
  variable <- get_col("variable")
  label    <- get_col("label")
  type     <- get_col("type")
  origin   <- get_col("origin")
  codelist <- get_col("codelist")
  length_  <- get_col("length")
  mand     <- get_col("mandatory")

  out <- data.frame(
    dataset   = toupper(dataset),
    variable  = toupper(variable),
    label     = label,
    type      = type,
    origin    = origin,
    codelist  = codelist,
    length    = length_,
    mandatory = mand,
    stringsAsFactors = FALSE
  )
  ## Drop header echo rows and rows missing required keys.
  keep <- !is.na(out$variable) &
          nzchar(out$variable) &
          !toupper(out$variable) %in% c("VARIABLE", "VAR", "NAME") &
          !is.na(out$dataset) &
          nzchar(out$dataset)
  out[keep, , drop = FALSE]
}


## --- define.xml branch -----------------------------------------------------

#' Parse an ADaM define.xml and return the same shape as `.parse_adam_excel`.
#'
#' Walks `ItemGroupDef` (datasets) and `ItemDef` (variables) regardless of
#' namespace prefix binding (`def:` vs. default) by using `local-name()`
#' XPath, so the parser tolerates the wide variety of namespace
#' declarations seen across CDISC-compliant define.xml files.
#'
#' @noRd
.parse_adam_define_xml <- function(xml_path) {
  doc <- tryCatch(
    xml2::read_xml(xml_path),
    error = function(e) cli::cli_abort(c(
      "Cannot parse {.path {xml_path}} as XML:",
      "x" = conditionMessage(e)
    ))
  )
  doc <- xml2::xml_ns_strip(doc)

  igroups  <- xml2::xml_find_all(doc, ".//*[local-name()='ItemGroupDef']")
  itemdefs <- xml2::xml_find_all(doc, ".//*[local-name()='ItemDef']")
  if (length(igroups) == 0 || length(itemdefs) == 0) {
    cli::cli_abort(c(
      "{.path {xml_path}} has no {.field ItemGroupDef} or {.field ItemDef} nodes.",
      "i" = "Is this a CDISC define.xml (ODM 1.3)?"
    ))
  }

  ## Build ItemDef OID -> attributes lookup.
  oids   <- xml2::xml_attr(itemdefs, "OID")
  names_ <- xml2::xml_attr(itemdefs, "Name")
  types  <- xml2::xml_attr(itemdefs, "DataType")
  lens   <- xml2::xml_attr(itemdefs, "Length")
  origins <- xml2::xml_attr(itemdefs, "Origin")
  labels <- vapply(itemdefs, function(node) {
    tt <- xml2::xml_find_first(node, ".//*[local-name()='TranslatedText']")
    if (inherits(tt, "xml_missing")) NA_character_ else xml2::xml_text(tt)
  }, character(1))
  idef <- data.frame(
    oid      = oids,
    variable = names_,
    label    = labels,
    type     = types,
    length   = lens,
    origin   = origins,
    stringsAsFactors = FALSE
  )

  ## Walk each ItemGroupDef to associate variables with their dataset.
  rows <- list()
  for (g in igroups) {
    dataset <- xml2::xml_attr(g, "Name")
    if (is.na(dataset) || !nzchar(dataset)) next
    refs <- xml2::xml_find_all(g, "./*[local-name()='ItemRef']")
    for (ref in refs) {
      oid <- xml2::xml_attr(ref, "ItemOID")
      idx <- which(idef$oid == oid)[1]
      if (is.na(idx)) next
      rows[[length(rows) + 1L]] <- data.frame(
        dataset   = toupper(dataset),
        variable  = toupper(idef$variable[idx] %||% ""),
        label     = idef$label[idx]  %||% NA_character_,
        type      = idef$type[idx]   %||% NA_character_,
        origin    = idef$origin[idx] %||% NA_character_,
        codelist  = NA_character_,
        length    = idef$length[idx] %||% NA_character_,
        mandatory = NA_character_,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0) {
    cli::cli_abort("{.path {xml_path}} has ItemGroupDefs but no resolvable ItemRefs.")
  }

  variables <- unique(do.call(rbind, rows))
  rownames(variables) <- NULL
  ## Drop empty-variable rows.
  variables <- variables[nzchar(variables$variable), , drop = FALSE]

  keys <- paste(variables$dataset, variables$variable, sep = ".")
  lookup <- setNames(
    lapply(seq_len(nrow(variables)), function(i) as.list(variables[i, ])),
    keys
  )

  list(variables = variables, lookup = lookup)
}
