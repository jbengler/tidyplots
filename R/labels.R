
ff_rename_axis_labels <- function(axis) {
  function(plot, new_names) {
    check_tidyplot(plot)
    scale_type <- get_scale_type(plot, axis)
    if (scale_type != "discrete")
      cli::cli_abort("Axis must be discrete not {scale_type}!")
    var <- get_variable(plot, axis)

    new_factors <- setNames(names(new_names), new_names)
    new_data <-
      plot$data %>%
      dplyr::mutate("{var}" := forcats::fct_recode(.data[[var]], !!!new_factors))

    # if named color vector needs to be updated
    if(var == get_variable(plot, "colour") && !is.null(plot$tidyplot$named_colors)) {
      new_named_colors <- plot$tidyplot$named_colors

      names(new_named_colors) <- stringr::str_replace_all(names(new_named_colors), new_names)

      #print(new_named_colors)
      plot$tidyplot$named_colors <- new_named_colors
      plot <- plot %>% adjust_colors(new_named_colors)
    }

    cli::cli_alert_danger(
    "Warning: rename_*_labels() changes labels across the entire the pipe!
    This affects reorder_*_labels() and adjust_colors(), when used with label names.")
    plot %+% new_data
  }
}
#' Rename axis or color labels
#'
#' @param plot bla
#' @param new_names bla
#' @export
rename_x_axis_labels <- ff_rename_axis_labels(axis = "x")
#' @rdname rename_x_axis_labels
#' @export
rename_y_axis_labels <- ff_rename_axis_labels(axis = "y")
#' @rdname rename_x_axis_labels
#' @export
rename_color_labels <- ff_rename_axis_labels(axis = "colour")


ff_reorder_axis_labels <- function(axis) {
  function(plot, ...) {
    check_tidyplot(plot)
    scale_type <- get_scale_type(plot, axis)
    if (scale_type != "discrete")
      cli::cli_abort("Axis must be discrete not {scale_type}!")
    var <- get_variable(plot, axis)

    new_data <-
      plot$data %>%
      dplyr::mutate("{var}" := forcats::fct_relevel(.data[[var]], ...))
    plot %+% new_data
  }
}
#' Reorder axis or color labels
#'
#' @param plot bla
#' @param ... bla
#' @export
reorder_x_axis_labels <- ff_reorder_axis_labels(axis = "x")
#' @rdname reorder_x_axis_labels
#' @export
reorder_y_axis_labels <- ff_reorder_axis_labels(axis = "y")
#' @rdname reorder_x_axis_labels
#' @export
reorder_color_labels <- ff_reorder_axis_labels(axis = "colour")


ff_sort_axis_labels <- function(axis) {
  function(plot, ...) {
    check_tidyplot(plot)
    scale_type <- get_scale_type(plot, axis)
    if (scale_type != "discrete")
      cli::cli_abort("Axis must be discrete not {scale_type}!")
    var <- get_variable(plot, axis)
    new_data <-
      plot$data %>%
      dplyr::arrange(...) %>%
      dplyr::mutate("{var}" := forcats::fct_reorder(.data[[var]], dplyr::row_number()))
    plot %+% new_data
  }
}
#' Sort axis or color labels
#'
#' @param plot bla
#' @param ... bla
#' @export
sort_x_axis_labels <- ff_sort_axis_labels(axis = "x")
#' @rdname sort_x_axis_labels
#' @export
sort_y_axis_labels <- ff_sort_axis_labels(axis = "y")
#' @rdname sort_x_axis_labels
#' @export
sort_color_labels <- ff_sort_axis_labels(axis = "colour")


ff_reverse_axis_labels <- function(axis) {
  function(plot) {
    check_tidyplot(plot)
    scale_type <- get_scale_type(plot, axis)
    if (scale_type != "discrete")
      cli::cli_abort("Axis must be discrete not {scale_type}!")
    var <- get_variable(plot, axis)
    new_data <-
      plot$data %>%
      dplyr::mutate("{var}" := forcats::fct_rev(.data[[var]]))
    plot %+% new_data
  }
}
#' Reverse axis or color labels
#'
#' @param plot bla
#' @export
reverse_x_axis_labels <- ff_reverse_axis_labels(axis = "x")
#' @rdname reverse_x_axis_labels
#' @export
reverse_y_axis_labels <- ff_reverse_axis_labels(axis = "y")
#' @rdname reverse_x_axis_labels
#' @export
reverse_color_labels <- ff_reverse_axis_labels(axis = "colour")

