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

## Canonical column -> aliases for a CODELISTS sheet (case-insensitive).
## Covers the two conventions seen in the wild: "Codelist Name" / "Term (Code)"
## / "Decoded Value", and "ID" / "Term" / "Order" / "Decoded Value". A sheet
## qualifies as a codelist sheet only when name + term + decode all map, so a
## variable-level sheet (which has a Codelist column but no Term / Decoded
## Value columns) can never be mistaken for one.
.CODELIST_COLUMN_ALIASES <- list(
  name    = c("codelist name", "codelist id", "codelist", "id"),
  term    = c("term", "term (code)", "coded value", "code",
              "submission value", "term code"),
  decode  = c("decoded value", "decode", "display value", "decoded_value"),
  order   = c("order"),
  type    = c("data type", "type"),
  used_by = c("used by variables", "used by", "variables")
)

#' Parse an ADaM specification (define.xml or Excel)
#'
#' Dispatches on file extension. Both formats produce the same return shape
#' so downstream code (validation, ARS building) is format-agnostic.
#'
#' @param path Path to the ADaM spec file. Either:
#'   * `.xml` -- ADaM `define.xml` (preferred when available)
#'   * `.xlsx` / `.xls` -- ADaM specification Excel
#' @param column_aliases Optional named list of EXTRA column-name aliases
#'   for the Excel form, merged over the built-in `.SPEC_COLUMN_ALIASES`.
#'   Names are canonical fields (`dataset`, `variable`, `label`, `type`,
#'   `origin`, `codelist`, `length`, `mandatory`); values are character
#'   vectors of additional header spellings (case-insensitive). Example:
#'   `list(variable = "nom de variable", dataset = "domaine")`. Ignored
#'   for define.xml input.
#'
#' @return A list with three elements:
#'   \describe{
#'     \item{`variables`}{Data frame with columns: `dataset`, `variable`,
#'       `label`, `type`, `origin`, `codelist`, `length`, `mandatory`.}
#'     \item{`lookup`}{Named list keyed by `"DATASET.VARIABLE"` -- each
#'       entry is the corresponding row as a named list.}
#'     \item{`codelists`}{Named list keyed by UPPER-CASE codelist name.
#'       Each entry is a list with `name`, `terms` (data frame with columns
#'       `term`, `decode`, `order`), and `used_by` (character vector of
#'       `"DATASET.VARIABLE"` references, possibly empty). Empty list when
#'       the spec carries no codelist sheet / CodeList nodes.}
#'   }
#'
#' @keywords internal
#' @noRd
parse_adam_spec <- function(path, column_aliases = NULL) {
  if (!file.exists(path)) {
    cli::cli_abort("ADaM spec file not found: {.path {path}}")
  }
  ext <- tolower(tools::file_ext(path))
  if (ext == "xml") {
    return(.parse_adam_define_xml(path))
  }
  if (ext %in% c("xlsx", "xls")) {
    return(.parse_adam_excel(path, column_aliases = column_aliases))
  }
  cli::cli_abort(c(
    "Unsupported ADaM spec extension: {.val .{ext}}",
    "i" = "Use {.val .xml} (define.xml) or {.val .xlsx} / {.val .xls} (ADaM spec)."
  ))
}


## --- Excel branch ----------------------------------------------------------

.parse_adam_excel <- function(excel_path, column_aliases = NULL) {

  aliases <- .merge_column_aliases(column_aliases)

  sheets <- tryCatch(readxl::excel_sheets(excel_path),
                     error = function(e) {
                       cli::cli_abort(c(
                         "Cannot open Excel file {.path {excel_path}}:",
                         "x" = conditionMessage(e)
                       ))
                     })

  ## Try each sheet; keep ones that look like a variable-level sheet
  ## (have both a Dataset/Domain column AND a Variable column). A sheet that
  ## instead looks like a CODELISTS sheet (name + term + decoded-value
  ## columns) is parsed for codelist terms. The codelist probe runs FIRST:
  ## some codelist sheets carry a "Name" column, which the variable-alias
  ## list would otherwise mistake for a Variable column.
  variable_rows <- list()
  codelists     <- list()
  sheet_notes   <- character()   ## per-sheet skip reasons for the abort path
  for (sh in sheets) {
    df <- tryCatch(
      suppressMessages(readxl::read_excel(excel_path, sheet = sh,
                                          col_types = "text", .name_repair = "minimal")),
      error = function(e) {
        diag_add(
          stage = "parse_spec", severity = "WARN", input = INPUT_SPEC,
          problem = paste0("Sheet could not be read: ", conditionMessage(e)),
          location = sh,
          action = "Sheet skipped"
        )
        sheet_notes <<- c(sheet_notes, sprintf("'%s': unreadable", sh))
        NULL
      }
    )
    if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) {
      if (!is.null(df)) {
        sheet_notes <- c(sheet_notes, sprintf("'%s': empty", sh))
        .diag_gap(
          stage = "parse_spec", severity = "INFO", input = INPUT_SPEC,
          problem = sprintf("Spec sheet '%s' is empty and was skipped.", sh),
          why = "No variables were read from it.",
          fix = "If this sheet should define variables, add a header row (Dataset, Variable Name, ...) plus the variable rows.",
          location = sh
        )
      }
      next
    }

    ## Codelist sheet? Parsed for decode terms, never for variables.
    cl_mapping <- .map_spec_columns(names(df), aliases = .CODELIST_COLUMN_ALIASES)
    if (!is.null(cl_mapping$name) && !is.null(cl_mapping$term) &&
        !is.null(cl_mapping$decode)) {
      parsed <- .parse_codelists_sheet(df, cl_mapping)
      if (length(parsed) > 0) {
        ## Later sheets never overwrite an already-parsed codelist name.
        codelists <- utils::modifyList(parsed, codelists)
        diag_add(
          stage = "parse_spec", severity = "INFO", input = INPUT_SPEC,
          problem = sprintf("Codelist sheet parsed: %d codelist(s)", length(parsed)),
          location = sh,
          action = "Coded categorical variables will display decoded values"
        )
      }
      next
    }

    mapping <- .map_spec_columns(names(df), aliases = aliases)
    ## Some workbooks use the sheet name as the dataset when there is no
    ## dataset column (one sheet per domain).
    has_var <- !is.null(mapping$variable)
    has_ds  <- !is.null(mapping$dataset)
    if (!has_var) {
      diag_add(
        stage = "parse_spec", severity = "INFO", input = INPUT_SPEC,
        problem = "Sheet has no recognisable Variable column",
        location = sh,
        action = paste0("Sheet skipped (columns seen: ",
                        paste(utils::head(names(df), 8), collapse = ", "), ")")
      )
      sheet_notes <- c(sheet_notes, sprintf(
        "'%s': no Variable column (saw: %s)",
        sh, paste(utils::head(names(df), 6), collapse = ", ")
      ))
      next
    }

    fallback_ds <- if (has_ds) NULL else toupper(sh)
    if (!is.null(fallback_ds) && !grepl("^AD[A-Z0-9]+$", fallback_ds)) {
      diag_add(
        stage = "parse_spec", severity = "WARN", input = INPUT_SPEC,
        problem = sprintf("No Dataset column; sheet name '%s' used as dataset but does not look like an ADaM dataset name", sh),
        location = sh,
        action = "Rows keyed under this dataset name -- verify against shell annotations"
      )
    }
    rows <- .normalise_spec_rows(df, mapping, fallback_ds)
    if (nrow(rows) > 0) {
      variable_rows[[length(variable_rows) + 1]] <- rows
    }
  }

  if (length(variable_rows) == 0) {
    notes <- if (length(sheet_notes) > 0) {
      stats::setNames(sheet_notes, rep("x", length(sheet_notes)))
    } else {
      NULL
    }
    var_aliases <- aliases$variable
    cli::cli_abort(c(
      "No variable-level sheet found in {.path {excel_path}}.",
      "i" = "Expected a sheet with a {.field Variable} column (and ideally a {.field Dataset} / {.field Domain} column).",
      "i" = "Recognised Variable column aliases: {.val {var_aliases}}.",
      notes
    ))
  }

  variables <- unique(do.call(rbind, variable_rows))
  rownames(variables) <- NULL

  keys <- paste(variables$dataset, variables$variable, sep = ".")
  lookup <- setNames(
    lapply(seq_len(nrow(variables)), function(i) as.list(variables[i, ])),
    keys
  )

  list(variables = variables, lookup = lookup, codelists = codelists)
}


## --- Internal helpers ------------------------------------------------------

#' Merge user-supplied extra aliases over the built-in alias list.
#' Unknown canonical names are rejected loudly (a typo here would
#' otherwise silently change nothing).
#' @noRd
.merge_column_aliases <- function(column_aliases) {
  if (is.null(column_aliases) || length(column_aliases) == 0) {
    return(.SPEC_COLUMN_ALIASES)
  }
  unknown <- setdiff(names(column_aliases), names(.SPEC_COLUMN_ALIASES))
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "Unknown canonical column name{?s} in {.arg column_aliases}: {.val {unknown}}.",
      "i" = "Valid names: {.val {names(.SPEC_COLUMN_ALIASES)}}."
    ))
  }
  merged <- .SPEC_COLUMN_ALIASES
  for (canon in names(column_aliases)) {
    merged[[canon]] <- unique(c(merged[[canon]],
                                tolower(trimws(as.character(column_aliases[[canon]])))))
  }
  merged
}

#' For a vector of column names, return a list mapping canonical names to the
#' actual column name found in the sheet (or NULL when not present).
#' @noRd
.map_spec_columns <- function(cols, aliases = .SPEC_COLUMN_ALIASES) {
  norm <- tolower(trimws(cols))
  out <- list()
  for (canon in names(aliases)) {
    hit_idx <- which(norm %in% aliases[[canon]])
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

#' Parse one codelist sheet into the named-list-of-codelists shape.
#'
#' `mapping` comes from `.map_spec_columns(names(df), .CODELIST_COLUMN_ALIASES)`
#' and is known to carry name + term + decode. Blank name cells are filled
#' down (merged-cell convention). Term order comes from the Order column when
#' present, else from row order within each codelist.
#' @noRd
.parse_codelists_sheet <- function(df, mapping) {
  get_col <- function(name) {
    src <- mapping[[name]]
    if (is.null(src)) return(rep(NA_character_, nrow(df)))
    trimws(as.character(df[[src]]))
  }

  cl_name <- get_col("name")
  term    <- get_col("term")
  decode  <- get_col("decode")
  order_  <- suppressWarnings(as.integer(get_col("order")))
  used_by <- get_col("used_by")

  ## Fill blank/NA codelist names down from the row above (merged cells).
  for (i in seq_along(cl_name)) {
    if ((is.na(cl_name[i]) || !nzchar(cl_name[i])) && i > 1) {
      cl_name[i] <- cl_name[i - 1]
    }
  }

  ## A usable term row needs a codelist name, a term, and a decode.
  keep <- !is.na(cl_name) & nzchar(cl_name) &
          !is.na(term)    & nzchar(term) &
          !is.na(decode)  & nzchar(decode)

  out <- list()
  for (i in which(keep)) {
    key <- toupper(cl_name[i])
    if (is.null(out[[key]])) {
      out[[key]] <- list(name = cl_name[i],
                         terms = data.frame(term = character(),
                                            decode = character(),
                                            order = integer(),
                                            stringsAsFactors = FALSE),
                         used_by = character())
    }
    n <- nrow(out[[key]]$terms)
    out[[key]]$terms[n + 1L, ] <- list(
      term   = term[i],
      decode = decode[i],
      order  = if (!is.na(order_[i])) order_[i] else n + 1L
    )
    ## "Used By Variables" is typically stamped once per codelist; split a
    ## comma/semicolon list of DATASET.VARIABLE references wherever it appears.
    if (!is.na(used_by[i]) && nzchar(used_by[i])) {
      refs <- toupper(trimws(strsplit(used_by[i], "[,;]+")[[1]]))
      refs <- refs[grepl("^[A-Z0-9]+\\.[A-Z0-9]+$", refs)]
      out[[key]]$used_by <- unique(c(out[[key]]$used_by, refs))
    }
  }

  ## Order each codelist's terms and drop duplicate terms (first wins).
  for (key in names(out)) {
    terms <- out[[key]]$terms
    terms <- terms[order(terms$order), , drop = FALSE]
    terms <- terms[!duplicated(terms$term), , drop = FALSE]
    rownames(terms) <- NULL
    out[[key]]$terms <- terms
  }
  out
}

#' Resolve the codelist for one DATASET.VARIABLE reference.
#'
#' Resolution order: the variable's own `codelist` field from the spec
#' lookup (case-insensitive match on codelist name), then any codelist whose
#' `used_by` names this exact DATASET.VARIABLE. Returns the codelist entry
#' (`list(name, terms, used_by)`) or `NULL`.
#' @noRd
.codelist_for <- function(codelists, dataset, variable, lookup_rec = NULL) {
  if (is.null(codelists) || length(codelists) == 0) return(NULL)
  if (is.null(variable) || !nzchar(variable %||% "")) return(NULL)

  cl_name <- as.character(lookup_rec$codelist %||% "")
  cl_name <- if (length(cl_name) > 0 && !is.na(cl_name[1])) {
    toupper(trimws(cl_name[1]))
  } else {
    ""
  }
  if (nzchar(cl_name) && !is.null(codelists[[cl_name]])) {
    return(codelists[[cl_name]])
  }

  key <- toupper(paste0(dataset %||% "", ".", variable))
  for (cl in codelists) {
    if (key %in% (cl$used_by %||% character())) return(cl)
  }
  NULL
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

  ## CodeList OID -> {name, terms} from CodeList / CodeListItem /
  ## EnumeratedItem nodes. An EnumeratedItem has no Decode, so its coded
  ## value doubles as its display value.
  codelists_by_oid <- .parse_define_codelists(doc)

  ## Build ItemDef OID -> attributes lookup.
  oids   <- xml2::xml_attr(itemdefs, "OID")
  names_ <- xml2::xml_attr(itemdefs, "Name")
  types  <- xml2::xml_attr(itemdefs, "DataType")
  lens   <- xml2::xml_attr(itemdefs, "Length")
  origins <- xml2::xml_attr(itemdefs, "Origin")
  labels <- vapply(itemdefs, function(node) {
    ## The variable label lives in Description/TranslatedText; a CodeListRef
    ## carries no TranslatedText, so the first hit is safe.
    tt <- xml2::xml_find_first(node, ".//*[local-name()='TranslatedText']")
    if (inherits(tt, "xml_missing")) NA_character_ else xml2::xml_text(tt)
  }, character(1))
  ## The variable's codelist NAME via its CodeListRef (when present).
  codelist_names <- vapply(itemdefs, function(node) {
    ref <- xml2::xml_find_first(node, ".//*[local-name()='CodeListRef']")
    if (inherits(ref, "xml_missing")) return(NA_character_)
    cl_oid <- xml2::xml_attr(ref, "CodeListOID")
    cl <- codelists_by_oid[[cl_oid %||% ""]]
    if (is.null(cl)) NA_character_ else cl$name
  }, character(1))
  idef <- data.frame(
    oid      = oids,
    variable = names_,
    label    = labels,
    type     = types,
    length   = lens,
    origin   = origins,
    codelist = codelist_names,
    stringsAsFactors = FALSE
  )

  ## Walk each ItemGroupDef to associate variables with their dataset.
  rows <- list()
  unresolved_oids <- character()
  for (g in igroups) {
    dataset <- xml2::xml_attr(g, "Name")
    if (is.na(dataset) || !nzchar(dataset)) next
    refs <- xml2::xml_find_all(g, "./*[local-name()='ItemRef']")
    for (ref in refs) {
      oid <- xml2::xml_attr(ref, "ItemOID")
      idx <- which(idef$oid == oid)[1]
      if (is.na(idx)) {
        unresolved_oids <- c(unresolved_oids, paste0(dataset, ":", oid))
        next
      }
      rows[[length(rows) + 1L]] <- data.frame(
        dataset   = toupper(dataset),
        variable  = toupper(idef$variable[idx] %||% ""),
        label     = idef$label[idx]  %||% NA_character_,
        type      = idef$type[idx]   %||% NA_character_,
        origin    = idef$origin[idx] %||% NA_character_,
        codelist  = idef$codelist[idx] %||% NA_character_,
        length    = idef$length[idx] %||% NA_character_,
        mandatory = NA_character_,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(unresolved_oids) > 0) {
    diag_add(
      stage = "parse_spec", severity = "WARN", input = INPUT_SPEC,
      problem = sprintf("%d ItemRef(s) in define.xml point to missing ItemDefs", length(unresolved_oids)),
      location = paste(utils::head(unresolved_oids, 5), collapse = "; "),
      action = "Variables dropped from spec lookup -- annotations referencing them will FAIL validation"
    )
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

  ## Re-key the OID-indexed codelists by UPPER name -- the same shape the
  ## Excel branch returns, so downstream code is format-agnostic.
  codelists <- list()
  for (cl in codelists_by_oid) {
    key <- toupper(cl$name %||% "")
    if (nzchar(key) && is.null(codelists[[key]])) {
      codelists[[key]] <- list(name = cl$name, terms = cl$terms,
                               used_by = character())
    }
  }

  list(variables = variables, lookup = lookup, codelists = codelists)
}

#' Parse define.xml CodeList nodes into an OID-keyed list of
#' `list(name, terms)` entries, where `terms` is a data frame with columns
#' `term`, `decode`, `order`. CodeListItem's Decode/TranslatedText is the
#' display value; an EnumeratedItem carries no decode, so its coded value is
#' used for both.
#' @noRd
.parse_define_codelists <- function(doc) {
  out <- list()
  cl_nodes <- xml2::xml_find_all(doc, ".//*[local-name()='CodeList']")
  for (cl in cl_nodes) {
    oid  <- xml2::xml_attr(cl, "OID")
    name <- xml2::xml_attr(cl, "Name")
    if (is.na(oid) || !nzchar(oid)) next
    if (is.na(name) || !nzchar(name)) name <- oid

    items <- xml2::xml_find_all(
      cl, "./*[local-name()='CodeListItem' or local-name()='EnumeratedItem']")
    if (length(items) == 0) next

    terms   <- character(length(items))
    decodes <- character(length(items))
    orders  <- integer(length(items))
    for (i in seq_along(items)) {
      item <- items[[i]]
      coded <- xml2::xml_attr(item, "CodedValue")
      tt <- xml2::xml_find_first(item, ".//*[local-name()='TranslatedText']")
      decode <- if (inherits(tt, "xml_missing")) coded else xml2::xml_text(tt)
      ord <- suppressWarnings(as.integer(xml2::xml_attr(item, "OrderNumber")))
      terms[i]   <- coded %||% ""
      decodes[i] <- decode %||% ""
      orders[i]  <- if (!is.na(ord)) ord else i
    }
    keep <- nzchar(terms) & nzchar(decodes)
    if (!any(keep)) next
    tdf <- data.frame(term = terms[keep], decode = decodes[keep],
                      order = orders[keep], stringsAsFactors = FALSE)
    tdf <- tdf[order(tdf$order), , drop = FALSE]
    tdf <- tdf[!duplicated(tdf$term), , drop = FALSE]
    rownames(tdf) <- NULL
    out[[oid]] <- list(name = name, terms = tdf)
  }
  out
}
