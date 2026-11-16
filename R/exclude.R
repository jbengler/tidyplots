#' Exclude data rows
#'
#' `r lifecycle::badge("experimental")`
#'
#' Exclude rows from the plot data and record the reason in the log.
#' Excluded rows are removed from all layers and statistical tests.
#'
#' @details
#' * Rows are excluded when all conditions are `TRUE`. Rows for which a
#'   condition is `NA` are kept, following [dplyr::filter_out()].
#' * The log records the conditions, the reason, the number of excluded rows
#'   overall and per group, and the excluded rows themselves. See [view_log()].
#' * Steps that change the data, such as `data_normalize_y()`, use the data at the
#'   time they are applied. For example, a normalization applied before
#'   `data_exclude()` still uses the excluded rows as reference.
#'   Statistical tests and summaries are computed from the final data.
#'
#' @inherit common_arguments
#' @param ... Logical conditions selecting the rows to exclude, for example
#'   `score > 100` or `sample_id %in% c("S3", "S7")`.
#' @param reason A single character string explaining why the rows are excluded.
#'   Required.
#'
#' @examples
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   data_exclude(
#'     participant %in% c("p03", "p17"),
#'     reason = "Failed quality control."
#'   ) |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_data_points() |>
#'   view_log()
#'
#' @export
data_exclude <- function(plot, ..., reason) {
  plot <- check_tidyplot(plot)
  if (missing(reason)) {
    cli::cli_abort(
      "{.arg reason} is missing. Please explain why the rows are excluded."
    )
  }
  check_single_string(reason)
  if (...length() == 0) {
    cli::cli_abort("At least one condition must be provided in {.arg ...}.")
  }
  conditions <- rlang::enquos(...)

  data <- plot$data
  flags <- dplyr::transmute(data, .exclude = !!combine_conditions(conditions))
  if (!is.logical(flags$.exclude)) {
    cli::cli_abort(
      "Conditions must be logical, not {.obj_type_friendly {flags$.exclude}}."
    )
  }
  excluded <- flags$.exclude %in% TRUE
  n_na <- sum(is.na(flags$.exclude))

  new_data <- dplyr::filter_out(data, ...)
  plot <- update_data(plot, new_data)

  log_record(
    plot,
    "exclusion",
    condition = paste(
      vapply(
        conditions,
        function(q) rlang::expr_text(rlang::quo_get_expr(q)),
        character(1)
      ),
      collapse = " & "
    ),
    reason = reason,
    excluded = sum(excluded),
    excluded_per_group = log_count_groups(data, excluded, log_group_vars(plot)),
    kept_with_na_condition = if (n_na > 0) n_na,
    excluded_rows = local({
      rows <- tibble::as_tibble(data[excluded, , drop = FALSE])
      rows$.single_color <- NULL
      rows
    })
  )
}

combine_conditions <- function(conditions) {
  Reduce(function(a, b) rlang::expr((!!a) & (!!b)), conditions)
}
