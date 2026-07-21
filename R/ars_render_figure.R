## arsbridge -- ars_render_figure.R
## ---------------------------------------------------------------------------
## Renders a CDISC ARS *figure* output to a ggplot. ARS figure outputs in this
## pipeline carry a title but no analysis specification (spec_to_ars does not
## yet emit figure analyses), so the variable -> aesthetic mapping is supplied
## by the caller (with sensible ADaM defaults) rather than read from the JSON.
## The figure type is inferred from the output title when not given.

#' Render an ARS figure output to a ggplot
#'
#' Builds a treatment-group figure from ADaM data for a figure output. Because
#' the ARS spec does not describe figure analyses, the data mapping is supplied
#' here; only the title and footnotes are taken from the spec.
#'
#' Supported `type`s:
#' * `"mean_over_time"` -- mean of `value_var` by visit, one line per group.
#' * `"responder_over_time"` -- percentage of responders by visit, one line per
#'   group (responder = `responder_flag == "Y"`, or `value_var == 1`).
#' * `"km"` -- Kaplan-Meier curve (requires a time-to-event dataset; errors with
#'   guidance if absent).
#' * `"forest"` -- not supported from raw data (needs fitted effect estimates);
#'   errors with guidance.
#'
#' @param ars_path Path to the CDISC ARS JSON.
#' @param adam_dir Directory of ADaM datasets (.xpt/.sas7bdat/.csv).
#' @param output_id Figure output id or name.
#' @param type Figure type; `"auto"` infers from the title.
#' @param dataset ADaM dataset to plot. Default `NULL`: resolved from the
#'   shell's "Source: ..." line carried in the output's `_meta.source_datasets`
#'   (falling back to `"ADEFF"` when the shell named no source).
#' @param value_var Response value column (default `"AVAL"`).
#' @param time_var Visit/time column; default auto (`AVISITN` then `AVISIT`).
#' @param by_var Grouping column; default auto (`TRT01A` then `TRTP`).
#' @param paramcd Optional `PARAMCD` filter; default inferred from the title.
#' @param responder_flag Optional flag column marking responders.
#' @param time_event Optional `list(time=, event=)` columns for `type = "km"`.
#' @param subject_key Subject id. Default `"USUBJID"`.
#' @return A `ggplot` object.
#' @seealso [ars_render_tlf()], [ars_render_listing()]
#' @export
ars_render_figure <- function(ars_path, adam_dir, output_id,
                              type = c("auto", "mean_over_time",
                                       "responder_over_time", "km", "forest"),
                              dataset = NULL, value_var = "AVAL",
                              time_var = NULL, by_var = NULL, paramcd = NULL,
                              responder_flag = NULL, time_event = NULL,
                              subject_key = "USUBJID") {
  type    <- match.arg(type)
  spec    <- jsonlite::fromJSON(ars_path, simplifyVector = FALSE)
  out_obj <- find_output(spec, output_id)
  title   <- extract_title(out_obj)
  footns  <- extract_footnotes(out_obj)
  ttl     <- tolower(paste(title, collapse = " "))

  ## Dataset default: the shell's "Source: ..." line, persisted per output in
  ## _meta.source_datasets (ADR 0003) -- e.g. a vital-signs figure sourced
  ## from ADVS must not silently plot ADEFF.
  if (is.null(dataset)) {
    src <- unlist(out_obj[["_meta"]][["source_datasets"]] %||% list())
    src <- trimws(sub("\\s*\\(.*$", "", as.character(src)))
    src <- src[nzchar(src)]
    dataset <- if (length(src) > 0) src[1] else "ADEFF"
  }

  if (type == "auto") {
    type <- if (grepl("forest", ttl)) "forest"
      else if (grepl("kaplan|survival|free survival|time to|\\bKM\\b", ttl)) "km"
      else if (grepl("response|responder|rate|%|proportion", ttl)) "responder_over_time"
      else "mean_over_time"
  }
  if (type == "forest") {
    cli::cli_abort(c(
      "Forest plots are not supported from raw ADaM.",
      "i" = "They need fitted effect estimates + confidence intervals per subgroup; supply a modelled summary dataset and plot it directly."))
  }

  df <- .listing_load(adam_dir, toupper(dataset))
  if (is.null(df)) cli::cli_abort("Dataset {.val {dataset}} not found in {.path {adam_dir}}.")

  if (is.null(by_var))   by_var   <- intersect(c("TRT01A", "TRTP", "TRTA", "ARM"), names(df))[1]
  if (is.null(time_var)) time_var <- intersect(c("AVISITN", "AVISIT", "VISITNUM", "ADY"), names(df))[1]
  if (is.na(by_var %||% NA) || is.null(by_var)) cli::cli_abort("No treatment column found in {.val {dataset}}.")

  ## Infer PARAMCD from the title (e.g. "EASI 75" -> EASI75) when present.
  if (is.null(paramcd) && "PARAMCD" %in% names(df)) {
    hit <- grep(gsub("[^a-z0-9]", "", ttl), gsub("[^A-Z0-9]", "",
            toupper(unique(df[["PARAMCD"]]))), value = TRUE)
    m <- regmatches(ttl, regexpr("easi ?\\d{2,3}|iga|bsa|poem|dlqi", ttl))
    if (length(m)) paramcd <- toupper(gsub("[^a-z0-9]", "", m))
  }
  if (!is.null(paramcd) && "PARAMCD" %in% names(df)) {
    keep <- toupper(df[["PARAMCD"]]) %in% toupper(paramcd)
    if (any(keep)) df <- df[keep, , drop = FALSE]
  }

  if (type == "km") {
    if (!requireNamespace("survival", quietly = TRUE)) {
      cli::cli_abort("Package {.pkg survival} is required for Kaplan-Meier figures.")
    }
    te <- time_event %||% list(time = intersect(c("AVAL", "TTE", "CNSR"), names(df))[1],
                               event = intersect(c("CNSR", "EVENTFL", "EVENT"), names(df))[1])
    if (is.null(te$time) || is.na(te$time) || is.null(te$event) || is.na(te$event)) {
      cli::cli_abort(c(
        "No time-to-event columns found for a Kaplan-Meier figure.",
        "i" = "Supply {.arg time_event = list(time =, event =)} pointing at a time-to-event (ADTTE) dataset."))
    }
    df$.time  <- as.numeric(df[[te$time]])
    df$.event <- as.numeric(df[[te$event]] %in% c(1, "1", "Y", TRUE))
    fit <- survival::survfit(survival::Surv(.time, .event) ~ df[[by_var]], data = df)
    sf  <- summary(fit)
    pd  <- data.frame(time = sf$time, surv = sf$surv,
                      grp = sub("^.*=", "", as.character(sf$strata)))
    p <- ggplot2::ggplot(pd, ggplot2::aes(x = .data$time, y = .data$surv,
                                          colour = .data$grp)) +
      ggplot2::geom_step()
    return(.figure_theme(p, title, footns, "Time", "Survival probability"))
  }

  ## value + time numeric/ordered
  df$.val <- suppressWarnings(as.numeric(df[[value_var]]))
  if (time_var == "AVISIT" && "AVISITN" %in% names(df)) {
    ord <- order(df[["AVISITN"]])
    df$.time <- factor(df[["AVISIT"]], levels = unique(df[["AVISIT"]][ord]))
  } else {
    df$.time <- df[[time_var]]
  }
  df$.grp <- as.character(df[[by_var]])

  if (type == "responder_over_time") {
    df$.resp <- if (!is.null(responder_flag) && responder_flag %in% names(df)) {
      as.numeric(df[[responder_flag]] %in% c("Y", 1, "1", TRUE))
    } else {
      as.numeric(df$.val %in% c(1))
    }
    agg <- stats::aggregate(.resp ~ .time + .grp, data = df, FUN = function(z) 100 * mean(z))
    names(agg)[3] <- "y"
    p <- ggplot2::ggplot(agg, ggplot2::aes(x = .data$.time, y = .data$y,
            colour = .data$.grp, group = .data$.grp)) +
      ggplot2::geom_line() + ggplot2::geom_point()
    return(.figure_theme(p, title, footns, time_var, "Responders (%)"))
  }

  ## mean_over_time
  if (all(is.na(df$.val))) {
    cli::cli_abort(c(
      "No numeric {.field {value_var}} values to plot for this figure.",
      "i" = "Check {.arg value_var}/{.arg paramcd}; the filtered data has none."))
  }
  agg <- stats::aggregate(.val ~ .time + .grp, data = df,
                          FUN = function(z) mean(z, na.rm = TRUE))
  names(agg)[3] <- "y"
  p <- ggplot2::ggplot(agg, ggplot2::aes(x = .data$.time, y = .data$y,
          colour = .data$.grp, group = .data$.grp)) +
    ggplot2::geom_line() + ggplot2::geom_point()
  .figure_theme(p, title, footns, time_var, paste0("Mean ", value_var))
}

## Shared figure styling: title, caption (footnotes), axis labels, legend.
.figure_theme <- function(p, title, footnotes, xlab, ylab) {
  cap <- if (length(footnotes)) paste(footnotes, collapse = "\n") else NULL
  p +
    ggplot2::labs(
      title   = if (length(title)) title[1] else NULL,
      x = xlab, y = ylab, colour = "Treatment", caption = cap) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom",
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}
