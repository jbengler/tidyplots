# The tidyplot log: printing ----------------------------------------------------

#' View the log of a tidyplot
#'
#' `r lifecycle::badge("experimental")`
#'
#' The log records what tidyplots did to make a plot: the input data, and
#' every step of the pipeline in the order it was applied. It is a record, not
#' a guarantee of reproducibility.
#' `view_log()` prints the log and returns the plot invisibly, so it can be
#' used in the middle of a pipeline. The log itself is stored in
#' `plot$tidyplot$log`.
#'
#' @details
#' * Each step is printed with the call as written and, for steps that change
#'   what the figure shows, a short record (e.g. the statistic shown by a
#'   summary layer, or the reference used for normalization).
#' * In addition, `view_log()` derives facts about the final figure from the
#'   plot data: the number of rows plotted, rows dropped due to missing values
#'   in mapped variables or non-finite values after an axis transformation, and
#'   the number of rows per group. With continuous x and y, summary layers
#'   such as [add_mean_line()] compute their statistic per x value, and the
#'   range of rows per x value and group is reported as well. Rows are
#'   counted, which is not necessarily the number of biological replicates.
#' * Logging starts with `tidyplot()`. Data processing before `tidyplot()`,
#'   ggplot2 components added with `+`, and direct modifications of the plot
#'   object (e.g. `plot$data <- ...`) are not recorded. To record ggplot2
#'   components, add them with [add()] instead of `+`.
#' * Statistical tests are described in standard terms derived from the
#'   method and its arguments, together with the underlying function. The
#'   names of methods with a base R equivalent are verified by tests against
#'   that equivalent. Other methods are marked as not cross-checked by
#'   tidyplots against an independent implementation.
#' * If a step cannot be recorded, the plot is still produced and the step is
#'   marked as "unrecorded".
#'
#' @inherit common_arguments
#'
#' @return The input `tidyplot`, invisibly.
#'
#' @examples
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_sem_errorbar() |>
#'   add_data_points() |>
#'   view_log()
#'
#' @export
view_log <- function(plot) {
  plot <- check_tidyplot(plot, log = FALSE)
  log <- plot$tidyplot$log
  if (is.null(log)) {
    cli::cli_inform("This plot has no log.")
    return(invisible(plot))
  }
  log$figure <- tryCatch(
    log_figure_facts(plot),
    error = function(e) list(error = conditionMessage(e))
  )
  print(log)
  invisible(plot)
}

#' @export
format.tidyplot_log <- function(x, ...) {
  h <- x$header
  steps <- x$steps
  out <- c(
    paste0(
      "<tidyplot log> schema ",
      h$schema,
      " | ",
      length(steps),
      " step",
      if (length(steps) != 1) "s"
    ),
    if (!is.null(h$error)) {
      log_header_line("unrecorded", h$error)
    },
    if (!is.null(h$figure_id)) log_header_line("figure", h$figure_id),
    log_header_line("created", format(h$created, "%Y-%m-%d %H:%M:%S %Z")),
    if (!is.null(h$software)) {
      log_header_line(
        "software",
        paste(names(h$software), h$software, collapse = " | ")
      )
    },
    if (!is.null(h$data)) {
      log_header_line(
        "data",
        paste0(
          h$data$name,
          " | hash ",
          substr(h$data$hash, 1, 8),
          " | ",
          h$data$nrow,
          " rows x ",
          h$data$ncol,
          " cols"
        )
      )
    }
  )
  if (length(h$metadata) > 0) {
    out <- c(
      out,
      log_header_line(
        "metadata",
        paste(names(h$metadata), h$metadata, sep = ": ", collapse = " | ")
      )
    )
  }
  out <- c(
    out,
    log_header_line(
      "scope",
      "steps from tidyplot() onward; data processing before tidyplot() is not recorded"
    ),
    log_header_line(
      "",
      "ggplot2 components added with + are not recorded, use add() to record them"
    )
  )
  if (!is.null(x$figure)) {
    out <- c(out, log_format_figure(x$figure))
  }
  out <- c(out, "")
  for (i in seq_along(steps)) {
    out <- c(
      out,
      log_format_step(
        i,
        steps[[i]],
        h$metadata,
        results = x$figure$tests[[as.character(i)]]
      )
    )
  }
  out
}

#' @export
print.tidyplot_log <- function(x, ...) {
  cli::cat_line(format(x, ...))
  invisible(x)
}

log_header_line <- function(label, value) {
  paste0(formatC(label, width = -10, flag = "-"), value)
}

log_format_step <- function(i, entry, metadata = list(), results = NULL) {
  indent <- strrep(" ", 14)
  call_text <- if (is.null(entry$call)) {
    paste0(entry$fun, "()")
  } else {
    paste(deparse(entry$call, width.cutoff = 500L), collapse = " ")
  }
  if (entry$kind == "note") {
    call_text <- paste0("\"", entry$record$note, "\"")
    entry$record <- list()
  }
  if (nchar(call_text) > 100) {
    if (entry$kind != "note") {
      call_text <- paste0(substr(call_text, 1, 97), "...")
    }
  }
  first <- paste0(
    formatC(i, width = 2),
    " ",
    formatC(entry$kind, width = -10, flag = "-"),
    " ",
    call_text
  )
  details <- c(
    log_format_record(entry$kind, entry$record, metadata),
    if (!is.null(results)) log_format_results(results)
  )
  c(first, if (length(details) > 0) paste0(indent, details))
}

# Results of a statistical test, one line per comparison
log_format_results <- function(results) {
  if (nrow(results) == 0) {
    return("results: no comparisons")
  }
  num <- function(x) ifelse(is.na(x), "NA", format(signif(x, 3), trim = TRUE))
  where <- paste0(
    ifelse(is.na(results$panel), "", paste0("[", results$panel, "] ")),
    ifelse(is.na(results$x_level), "", paste0(results$x_level, ": "))
  )
  comparison <- paste0(where, results$group1, " vs ", results$group2)
  computed <- results$status == "computed"
  values <- paste0(
    "n ", results$n1, "/", results$n2,
    " | ", results$statistic_name, " = ", num(results$statistic),
    ifelse(is.na(results$df), "", paste0(", df = ", num(results$df))),
    " | p = ", num(results$p),
    ", p.adj = ", num(results$p.adj),
    " ", results$p.adj.signif,
    ifelse(results$shown, "", " (computed, not shown)")
  )
  values[!computed] <- results$status[!computed]
  notes <- ifelse(
    is.na(results$notes) | results$notes == "",
    "",
    paste0(" | note: ", results$notes)
  )
  computed_as <- unique(stats::na.omit(results$computed_as))
  c(
    if (length(computed_as) > 0) paste0("computed as: ", paste(computed_as, collapse = "; ")),
    "results:",
    paste0("  ", comparison, " | ", values, notes)
  )
}

log_format_record <- function(kind, record, metadata = list()) {
  if (length(record) == 0) {
    return(character())
  }
  if (kind == "data") {
    m <- record$mapping
    used <- m$variable != "!!MISSING" &
      m$variable != ".single_color" &
      m$aesthetic != "fill"
    unit_field <- c(x = "x_unit", y = "y_unit", colour = "color_unit")
    units <- vapply(
      m$aesthetic[used],
      function(a) {
        u <- metadata[[unit_field[a]]] %||% NA_character_
        if (is.na(u)) "" else paste0(", ", u)
      },
      character(1)
    )
    vars <- paste0(
      m$aesthetic[used],
      ": ",
      m$variable[used],
      " (",
      m$scale_type[used],
      units,
      ")"
    )
    return(paste(vars, collapse = " | "))
  }
  if (kind == "exclusion") {
    record$excluded_rows <- NULL
  }
  fields <- paste0(
    gsub("_", " ", names(record)),
    ": ",
    vapply(record, log_format_value, character(1))
  )
  one_line <- paste(fields, collapse = " | ")
  if (nchar(one_line) <= 90) {
    return(one_line)
  }
  fields
}

log_format_value <- function(x) {
  if (is.data.frame(x)) {
    if (nrow(x) == 1) {
      values <- vapply(x, function(v) format(v, digits = 4), character(1))
      return(paste(names(x), values, sep = " = ", collapse = ", "))
    }
    return(paste0("<table with ", nrow(x), " rows>"))
  }
  if (is.numeric(x)) {
    x <- if (!is.null(names(x))) {
      paste(names(x), format(x, digits = 4, trim = TRUE))
    } else {
      format(x, digits = 4, trim = TRUE)
    }
  }
  x <- as.character(x)
  if (length(x) > 10) {
    x <- c(x[1:10], paste0("... (", length(x) - 10, " more)"))
  }
  paste(x, collapse = ", ")
}

# Figure facts
log_format_figure <- function(f) {
  if (!is.null(f$error)) {
    return(log_header_line("plotted", paste("unavailable:", f$error)))
  }
  dropped <- f$n_total - f$n_used
  reasons <- c(
    if (length(f$na) > 0) paste0("NA in ", names(f$na), ": ", f$na),
    vapply(
      f$transforms,
      function(t) {
        if (t$non_finite == 0) {
          return(NA_character_)
        }
        paste0("non-finite after ", t$transform, " of ", t$variable, ": ", t$non_finite)
      },
      character(1)
    ),
    vapply(
      names(f$limits),
      function(axis) {
        l <- f$limits[[axis]]
        paste0("outside ", axis, " scale limits of ", l$variable, ": ", l$outside)
      },
      character(1)
    )
  )
  reasons <- reasons[!is.na(reasons)]
  out <- log_header_line(
    "plotted",
    paste0(
      f$n_used,
      " of ",
      f$n_total,
      " rows",
      if (dropped > 0) paste0(" | dropped ", dropped, " (", paste(reasons, collapse = ", "), ")")
    )
  )
  if (!is.null(f$n_groups)) {
    n <- paste(names(f$n_groups), f$n_groups, collapse = ", ")
    if (length(f$n_groups) > 12) {
      n <- paste(
        paste(names(f$n_groups)[1:12], f$n_groups[1:12], collapse = ", "),
        paste0("... (", length(f$n_groups) - 12, " more groups)")
      )
    }
    out <- c(
      out,
      log_header_line(
        "rows per",
        paste0(paste(f$group_vars, collapse = "/"), ": ", n)
      )
    )
  }
  if (length(f$n_by_x) > 0) {
    vars <- paste(c(f$x_var, f$group_vars), collapse = "/")
    range <- if (min(f$n_by_x) == max(f$n_by_x)) {
      min(f$n_by_x)
    } else {
      paste0(min(f$n_by_x), "-", max(f$n_by_x))
    }
    out <- c(
      out,
      log_header_line(
        if (is.null(f$n_groups)) "rows per" else "",
        paste0(vars, ": ", range, " (summaries are computed per x value)")
      )
    )
  }
  for (axis in names(f$transforms)) {
    t <- f$transforms[[axis]]
    out <- c(
      out,
      log_header_line("transform", paste0(axis, " (", t$variable, "): ", t$transform))
    )
  }
  out
}
