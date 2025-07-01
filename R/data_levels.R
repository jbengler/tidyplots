
ff_rename_axis_levels <- function(axis) {
  function(plot, new_names) {
    plot <- check_tidyplot(plot)
    scale_type <- get_scale_type(plot, axis)
    if (scale_type != "discrete")
      cli::cli_abort("Axis must be discrete not {scale_type}!")
    if (is.null(names(new_names)))
      cli::cli_abort("{.arg new_names} must be a named character vector!")
    var <- get_variable(plot, axis)

    new_factors <- setNames(names(new_names), new_names)
    new_data <-
      plot$data |>
      dplyr::mutate("{var}" := forcats::fct_recode(.data[[var]], !!!new_factors))

    # if named color vector needs to be updated
    if(var == get_variable(plot, "colour") && !is.null(plot$tidyplot$named_colors)) {
      new_named_colors <- plot$tidyplot$named_colors

      names(new_named_colors) <- stringr::str_replace_all(names(new_named_colors), new_names)

      #print(new_named_colors)
      plot$tidyplot$named_colors <- new_named_colors
      plot <- plot |> adjust_colors(new_named_colors)
    }
    plot %+% new_data
  }
}
#' Rename axis or color levels
#'
#' @param new_names Named character vector in the format c("old1" = "new1", "old2" = "new2").
#' @inherit common_arguments
#'
#' @examples
#' # Before adjustments
#' study |>
#'   tidyplot(x = treatment, y = score) |>
#'   add_data_points() |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_sem_errorbar()
#'
#' # Rename x-axis levels
#' study |>
#'   tidyplot(x = treatment, y = score) |>
#'   add_data_points() |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_sem_errorbar() |>
#'   rename_x_axis_levels(new_names = c(
#'     "A" = "This",
#'     "B" = "is",
#'     "C" = "totally",
#'     "D" = "new"))
#'
#' # Before adjustments
#' study |>
#'   tidyplot(x = score, y = treatment) |>
#'   add_data_points() |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_sem_errorbar()
#'
#' # Rename y-axis levels
#' study |>
#'   tidyplot(x = score, y = treatment) |>
#'   add_data_points() |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_sem_errorbar() |>
#'   rename_y_axis_levels(new_names = c(
#'     "A" = "This",
#'     "B" = "is",
#'     "C" = "totally",
#'     "D" = "new"))
#'
#' # Before adjustment
#' study |>
#'   tidyplot(x = group, y = score, color = dose) |>
#'   add_data_points() |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_sem_errorbar()
#'
#' # Rename color levels
#' study |>
#'   tidyplot(x = group, y = score, color = dose) |>
#'   add_data_points() |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_sem_errorbar() |>
#'   rename_color_levels(new_names = c(
#'     "high" = "Sky high",
#'     "low" = "Deep low"))
#'
#' @export
rename_x_axis_levels <- ff_rename_axis_levels(axis = "x")
#' @rdname rename_x_axis_levels
#' @export
rename_y_axis_levels <- ff_rename_axis_levels(axis = "y")
#' @rdname rename_x_axis_levels
#' @export
rename_color_levels <- ff_rename_axis_levels(axis = "colour")

#' @export
#' @rdname rename_x_axis_levels
#' @usage NULL
rename_x_axis_labels <- rename_x_axis_levels

#' @export
#' @rdname rename_x_axis_levels
#' @usage NULL
rename_y_axis_labels <- rename_y_axis_levels

#' @export
#' @rdname rename_x_axis_levels
#' @usage NULL
rename_color_labels <- rename_color_levels


ff_reorder_axis_levels <- function(axis) {
  function(plot, ...) {
    plot <- check_tidyplot(plot)
    scale_type <- get_scale_type(plot, axis)
    if (scale_type != "discrete")
      cli::cli_abort("Axis must be discrete not {scale_type}!")
    var <- get_variable(plot, axis)

    new_data <-
      plot$data |>
      dplyr::mutate("{var}" := forcats::fct_relevel(.data[[var]], ...))
    plot %+% new_data
  }
}
#' Reorder axis or color levels
#'
#' @inherit common_arguments
#' @param ... Arguments passed on to `forcats::fct_relevel()`.
#'
#' @examples
#' # Before adjustments
#' study |>
#'   tidyplot(x = treatment, y = score) |>
#'   add_data_points() |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_sem_errorbar()
#'
#' # Reorder x-axis levels
#' study |>
#'   tidyplot(x = treatment, y = score) |>
#'   add_data_points() |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_sem_errorbar() |>
#'   reorder_x_axis_levels("D", "B", "A")
#'
#' # Before adjustments
#' study |>
#'   tidyplot(x = score, y = treatment) |>
#'   add_data_points() |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_sem_errorbar()
#'
#' # Reorder y-axis levels
#' study |>
#'   tidyplot(x = score, y = treatment) |>
#'   add_data_points() |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_sem_errorbar() |>
#'   reorder_y_axis_levels("D", "B", "A")
#'
#' # Before adjustment
#' study |>
#'   tidyplot(x = group, y = score, color = dose) |>
#'   add_data_points() |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_sem_errorbar()
#'
#' # Reorder color levels
#' study |>
#'   tidyplot(x = group, y = score, color = dose) |>
#'   add_data_points() |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_sem_errorbar() |>
#'   reorder_color_levels("low")
#'
#' @export
reorder_x_axis_levels <- ff_reorder_axis_levels(axis = "x")
#' @rdname reorder_x_axis_levels
#' @export
reorder_y_axis_levels <- ff_reorder_axis_levels(axis = "y")
#' @rdname reorder_x_axis_levels
#' @export
reorder_color_levels <- ff_reorder_axis_levels(axis = "colour")

#' @export
#' @rdname reorder_x_axis_levels
#' @usage NULL
reorder_x_axis_labels <- reorder_x_axis_levels

#' @export
#' @rdname reorder_x_axis_levels
#' @usage NULL
reorder_y_axis_labels <- reorder_y_axis_levels

#' @export
#' @rdname reorder_x_axis_levels
#' @usage NULL
reorder_color_labels <- reorder_color_levels


ff_sort_levels <- function(axis) {
  function(plot, ..., .fun = NULL, .reverse = FALSE) {
    plot <- check_tidyplot(plot)
    scale_type <- get_scale_type(plot, axis)
    if (scale_type != "discrete")
      cli::cli_abort("Axis must be discrete not {scale_type}!")

    var_a <- get_variable(plot, axis)

    if (!missing(..1)) {
      # sort by variables passed into '...'
      new_data <-
        plot$data |>
        dplyr::arrange(...) |>
        dplyr::mutate("{var_a}" := forcats::fct_reorder(.f = .data[[var_a]],
                                                        .x = dplyr::row_number(),
                                                        .desc = .reverse))
    } else {
      # sort by statistic entity (mean, median, sum, count) used in plot
      if (any(stringr::str_detect(plot$tidyplot$history, "count")))
        auto_fun <- length
      else if (any(stringr::str_detect(plot$tidyplot$history, "mean")))
        auto_fun <- mean
      else if (any(stringr::str_detect(plot$tidyplot$history, "sum")))
        auto_fun <- sum
      else
        auto_fun <- median

      .fun <- .fun %||% auto_fun

      if (is_continuous(plot, "y"))
        var_b <- get_variable(plot, "y")
      else if (is_continuous(plot, "x"))
        var_b <- get_variable(plot, "x")
      else if (is_continuous(plot, "colour"))
        var_b <- get_variable(plot, "colour")
      else
        var_b <- get_variable(plot, axis)

      new_data <-
        plot$data |>
        dplyr::mutate("{var_a}" := forcats::fct_reorder(.f = .data[[var_a]],
                                                        .x = .data[[var_b]],
                                                        .fun = .fun,
                                                        .desc = .reverse))
    }
    plot %+% new_data
  }
}
#' Sort axis or color levels
#'
#' @examples
#' # Before adjustments
#' study |>
#'   tidyplot(x = treatment, y = score) |>
#'   add_data_points() |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_sem_errorbar()
#'
#' # Sort x-axis levels by score
#' study |>
#'   tidyplot(x = treatment, y = score) |>
#'   add_data_points() |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_sem_errorbar() |>
#'   sort_x_axis_levels()
#'
#' # Before adjustments
#' study |>
#'   tidyplot(x = score, y = treatment) |>
#'   add_data_points() |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_sem_errorbar()
#'
#' # Sort y-axis levels by score
#' study |>
#'   tidyplot(x = score, y = treatment) |>
#'   add_data_points() |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_sem_errorbar() |>
#'   sort_y_axis_levels()
#'
#' # Before adjustment
#' study |>
#'   tidyplot(x = group, y = score, color = treatment) |>
#'   add_data_points() |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_sem_errorbar()
#'
#' # Sort color levels by score
#' study |>
#'   tidyplot(x = group, y = score, color = treatment) |>
#'   add_data_points() |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_sem_errorbar() |>
#'   sort_color_levels()
#'
#' @inherit common_arguments
#' @param ... Optional variables to use for sorting.
#' @param .fun Override the function used for sorting. Is automatically determined from the plot.
#' @export
sort_x_axis_levels <- ff_sort_levels(axis = "x")
#' @rdname sort_x_axis_levels
#' @export
sort_y_axis_levels <- ff_sort_levels(axis = "y")
#' @rdname sort_x_axis_levels
#' @export
sort_color_levels <- ff_sort_levels(axis = "colour")

#' @export
#' @rdname sort_x_axis_levels
#' @usage NULL
sort_x_axis_labels <- sort_x_axis_levels

#' @export
#' @rdname sort_x_axis_levels
#' @usage NULL
sort_y_axis_labels <- sort_y_axis_levels

#' @export
#' @rdname sort_x_axis_levels
#' @usage NULL
sort_color_labels <- sort_color_levels


ff_reverse_axis_levels <- function(axis) {
  function(plot) {
    plot <- check_tidyplot(plot)
    scale_type <- get_scale_type(plot, axis)
    if (scale_type != "discrete")
      cli::cli_abort("Axis must be discrete not {scale_type}!")
    var <- get_variable(plot, axis)
    new_data <-
      plot$data |>
      dplyr::mutate("{var}" := forcats::fct_rev(.data[[var]]))
    plot %+% new_data
  }
}
#' Reverse axis or color levels
#'
#' @examples
#' # Before adjustments
#' study |>
#'   tidyplot(x = treatment, y = score) |>
#'   add_data_points() |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_sem_errorbar()
#'
#' # Reverse x-axis levels
#' study |>
#'   tidyplot(x = treatment, y = score) |>
#'   add_data_points() |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_sem_errorbar() |>
#'   reverse_x_axis_levels()
#'
#' # Before adjustments
#' study |>
#'   tidyplot(x = score, y = treatment) |>
#'   add_data_points() |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_sem_errorbar()
#'
#' # Reverse y-axis levels
#' study |>
#'   tidyplot(x = score, y = treatment) |>
#'   add_data_points() |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_sem_errorbar() |>
#'   reverse_y_axis_levels()
#'
#' # Before adjustment
#' study |>
#'   tidyplot(x = group, y = score, color = dose) |>
#'   add_data_points() |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_sem_errorbar()
#'
#' # Reverse color levels
#' study |>
#'   tidyplot(x = group, y = score, color = dose) |>
#'   add_data_points() |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_sem_errorbar() |>
#'   reverse_color_levels()
#'
#' @inherit common_arguments
#' @export
reverse_x_axis_levels <- ff_reverse_axis_levels(axis = "x")
#' @rdname reverse_x_axis_levels
#' @export
reverse_y_axis_levels <- ff_reverse_axis_levels(axis = "y")
#' @rdname reverse_x_axis_levels
#' @export
reverse_color_levels <- ff_reverse_axis_levels(axis = "colour")

#' @export
#' @rdname reverse_x_axis_levels
#' @usage NULL
reverse_x_axis_labels <- reverse_x_axis_levels

#' @export
#' @rdname reverse_x_axis_levels
#' @usage NULL
reverse_y_axis_labels <- reverse_y_axis_levels

#' @export
#' @rdname reverse_x_axis_levels
#' @usage NULL
reverse_color_labels <- reverse_color_levels

