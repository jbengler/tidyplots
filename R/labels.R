

ff_rename_axis_labels <- function(axis) {
  function(gg, new_names) {
    scale_type <- get_scale_type(gg, axis)
    if (scale_type != "discrete")
      cli::cli_abort("Axis must be discrete not {scale_type}!")
    var <- get_variable(gg, axis)

    new_names <- setNames(names(new_names), new_names)
    new_data <-
      gg$data %>%
      dplyr::mutate("{var}" := forcats::fct_recode(.data[[var]], !!!new_names))

    cli::cli_alert_danger(
    "Warning: rename_*_labels() changes labels across the entire the pipe!
    This affects reorder_*_labels() and adjust_colors(), when used with label names.")
    gg %+% new_data
  }
}
#' Rename axis and color labels
#'
#' @param gg bla
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
  function(gg, ...) {
    scale_type <- get_scale_type(gg, axis)
    if (scale_type != "discrete")
      cli::cli_abort("Axis must be discrete not {scale_type}!")
    var <- get_variable(gg, axis)

    new_data <-
      gg$data %>%
      dplyr::mutate("{var}" := forcats::fct_relevel(.data[[var]], ...))
    gg %+% new_data
  }
}
#' Reorder axis and color labels
#'
#' @param gg bla
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
  function(gg, ...) {
    scale_type <- get_scale_type(gg, axis)
    if (scale_type != "discrete")
      cli::cli_abort("Axis must be discrete not {scale_type}!")
    var <- get_variable(gg, axis)
    new_data <-
      gg$data %>%
      dplyr::arrange(...) %>%
      dplyr::mutate("{var}" := forcats::fct_reorder(.data[[var]], dplyr::row_number()))
    gg %+% new_data
  }
}
#' Sort axis and color labels
#'
#' @param gg bla
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
  function(gg) {
    scale_type <- get_scale_type(gg, axis)
    if (scale_type != "discrete")
      cli::cli_abort("Axis must be discrete not {scale_type}!")
    var <- get_variable(gg, axis)
    new_data <-
      gg$data %>%
      dplyr::mutate("{var}" := forcats::fct_rev(.data[[var]]))
    gg %+% new_data
  }
}
#' Reverse axis and color labels
#'
#' @param gg bla
#' @export
reverse_x_axis_labels <- ff_reverse_axis_labels(axis = "x")
#' @rdname reverse_x_axis_labels
#' @export
reverse_y_axis_labels <- ff_reverse_axis_labels(axis = "y")
#' @rdname reverse_x_axis_labels
#' @export
reverse_color_labels <- ff_reverse_axis_labels(axis = "colour")

