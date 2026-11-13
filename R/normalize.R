ff_normalize <- function(axis) {
  function(plot, one = NULL, zero = NULL, by = NULL, fun = mean) {
    plot <- check_tidyplot(plot)
    if (!is_continuous(plot, axis)) {
      label <- if (axis == "colour") "color" else paste(axis, "axis")
      cli::cli_abort("The {label} must be continuous to be normalized.")
    }
    var <- get_variable(plot, axis)
    fun <- rlang::as_function(fun)

    # Evaluate conditions; a NULL condition does not create a column
    data <- dplyr::mutate(
      plot$data,
      .is_one = {{ one }},
      .is_zero = {{ zero }},
      .by = {{ by }}
    )
    has_one <- ".is_one" %in% names(data)
    has_zero <- ".is_zero" %in% names(data)
    if (!has_one && !has_zero) {
      cli::cli_abort(
        "At least one of {.arg one} or {.arg zero} must be provided."
      )
    }
    for (arg in c("one", "zero")) {
      condition <- data[[paste0(".is_", arg)]]
      if (!is.null(condition) && !is.logical(condition)) {
        cli::cli_abort(
          "{.arg {arg}} must be a logical condition, not {.obj_type_friendly {condition}}."
        )
      }
    }

    reference <- function(x, is_ref) {
      values <- x[is_ref %in% TRUE & !is.na(x)]
      if (length(values) == 0) NA_real_ else fun(values)
    }
    data <- dplyr::mutate(
      data,
      .ref_one = if (has_one) reference(.data[[var]], .is_one),
      .ref_zero = if (has_zero) reference(.data[[var]], .is_zero),
      .ref_group = dplyr::cur_group_id(),
      .by = {{ by }}
    )

    # (x - zero) / (one - zero), with zero = 0 and one - zero = 1 if missing
    x_zero <- if (has_zero) data$.ref_zero else 0
    denominator <- if (has_one) data$.ref_one - x_zero else 1

    missing_ref <- is.na(x_zero) | is.na(denominator)
    if (any(missing_ref)) {
      n <- length(unique(data$.ref_group[missing_ref]))
      cli::cli_warn(
        "{n} group{?s} ha{?s/ve} no reference rows. Resulting values are {.val {NA}}."
      )
    }
    if (any(denominator == 0, na.rm = TRUE)) {
      cli::cli_warn(
        "Reference values result in division by zero. Resulting values are {.val {Inf}} or {.val {NaN}}."
      )
    }

    new_data <- plot$data
    new_data[[var]] <- (new_data[[var]] - x_zero) / denominator
    update_data(plot, new_data)
  }
}

#' Normalize x, y, or color values
#'
#' Normalize the values of a continuous x axis, y axis, or color variable
#' relative to reference rows. The reference rows are selected by logical
#' conditions, optionally within groups defined by `by`.
#'
#' * `one` only: the reference is mapped to `1` (fold change), \eqn{v / v_{one}}.
#' * `zero` only: the reference is mapped to `0` (baseline subtraction), \eqn{v - v_{zero}}.
#' * `one` and `zero`: the two references define the `0`--`1` range,
#'   \eqn{(v - v_{zero}) / (v_{one} - v_{zero})}.
#'
#' @details
#' * The variable is overwritten in the plot data, so all layers, including
#'   statistical summaries like `add_mean_bar()`, show the normalized values.
#'   Note that summaries are computed from the normalized values, e.g. the mean
#'   of the ratios rather than the ratio of the means.
#' * If several aesthetics map to the same variable (e.g. `y = score, color = score`),
#'   normalizing one of them changes all of them.
#' * The axis or legend title is not changed. Use `adjust_y_axis_title()`,
#'   `adjust_x_axis_title()`, or `adjust_legend_title()` to set a new title.
#' * When a condition matches several rows within a group, their values are
#'   combined with `fun`. Missing values in reference rows are ignored.
#' * Groups without matching reference rows result in `NA` values.
#'
#' @inherit common_arguments
#' @param one A logical condition selecting the reference rows that should be
#'   mapped to `1`. For example, `day == 0` or `treatment == "control"`.
#' @param zero A logical condition selecting the reference rows that should be
#'   mapped to `0`.
#' @param by Variables to group by. The reference is determined separately
#'   within each group. For example, `by = subject` or `by = c(gene, batch)`.
#' @param fun A function used to combine the values of several matching
#'   reference rows into one reference value. Defaults to `mean`.
#'
#' @examples
#' # Fold change relative to treatment A
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_sem_errorbar() |>
#'   add_data_points() |>
#'   normalize_y(one = treatment == "A")
#'
#' # Temperature anomaly relative to 1961-1990, separately for each month
#' climate |>
#'   tidyplot(x = year, y = avg_temperature) |>
#'   add_mean_line() |>
#'   add_reference_lines(y = 0) |>
#'   normalize_y(zero = year >= 1961 & year <= 1990, by = month)
#'
#' # Scale each gene from healthy (0) to disease (1)
#' gene_expression |>
#'   dplyr::filter(sample_type == "input") |>
#'   dplyr::filter(external_gene_name %in% c("Apol6", "Bsn", "Vgf")) |>
#'   tidyplot(x = condition, y = expression, color = external_gene_name) |>
#'   add_mean_dash() |>
#'   add_data_points() |>
#'   reorder_x_axis_levels("healthy") |>
#'   normalize_y(
#'     zero = condition == "healthy",
#'     one = condition == "disease",
#'     by = external_gene_name
#'   )
#'
#' # Fold change on a horizontal plot
#' study |>
#'   tidyplot(x = score, y = treatment, color = treatment) |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_sem_errorbar() |>
#'   add_data_points() |>
#'   normalize_x(one = treatment == "A")
#'
#' # Heatmap of temperature anomalies relative to 1961-1990
#' climate |>
#'   tidyplot(x = month, y = year, color = max_temperature) |>
#'   add_heatmap() |>
#'   adjust_colors(colors_diverging_blue2red) |>
#'   normalize_color(zero = year >= 1961 & year <= 1990, by = month)
#'
#' @export
normalize_y <- ff_normalize(axis = "y")
#' @rdname normalize_y
#' @export
normalize_x <- ff_normalize(axis = "x")
#' @rdname normalize_y
#' @export
normalize_color <- ff_normalize(axis = "colour")
