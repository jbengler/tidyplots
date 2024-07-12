
#' Remove legend or legend title
#' @inherit common_arguments
#'
#' @examples
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_mean_bar() %>%
#'   remove_legend()
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_mean_bar() %>%
#'   remove_legend_title()
#'
#' @export
remove_legend <- function(plot) {
  check_tidyplot(plot)
  plot + ggplot2::theme(legend.position="none")
}

#' @rdname remove_legend
#' @export
remove_legend_title <- function(plot) {
  check_tidyplot(plot)
  plot + ggplot2::theme(legend.title = ggplot2::element_blank())
}

#' Remove x axis or parts of it
#' @inherit common_arguments
#'
#' @examples
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_mean_bar() %>%
#'   remove_x_axis_line()
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_mean_bar() %>%
#'   remove_x_axis_ticks()
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_mean_bar() %>%
#'   remove_x_axis_labels()
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_mean_bar() %>%
#'   remove_x_axis_title()
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_mean_bar() %>%
#'   remove_x_axis()
#'
#' @export
remove_x_axis <- function(plot) {
  check_tidyplot(plot)
  plot %>%
    remove_x_axis_line() %>%
    remove_x_axis_ticks() %>%
    remove_x_axis_labels() %>%
    remove_x_axis_title()
}

#' @rdname remove_x_axis
#' @export
remove_x_axis_line <- function(plot) {
  check_tidyplot(plot)
  plot + ggplot2::theme(axis.line.x = ggplot2::element_blank())
}

#' @rdname remove_x_axis
#' @export
remove_x_axis_ticks <- function(plot) {
  check_tidyplot(plot)
  plot + ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
}

#' @rdname remove_x_axis
#' @export
remove_x_axis_labels <- function(plot) {
  check_tidyplot(plot)
  plot + ggplot2::theme(axis.text.x = ggplot2::element_blank())
}

#' @rdname remove_x_axis
#' @export
remove_x_axis_title <- function(plot) {
  check_tidyplot(plot)
  plot + ggplot2::theme(axis.title.x = ggplot2::element_blank())
}


#' Remove y axis or parts of it
#' @inherit common_arguments
#'
#' @examples
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_mean_bar() %>%
#'   remove_y_axis_line()
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_mean_bar() %>%
#'   remove_y_axis_ticks()
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_mean_bar() %>%
#'   remove_y_axis_labels()
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_mean_bar() %>%
#'   remove_y_axis_title()
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_mean_bar() %>%
#'   remove_y_axis()
#'
#' @export
remove_y_axis <- function(plot) {
  check_tidyplot(plot)
  plot %>%
    remove_y_axis_line() %>%
    remove_y_axis_ticks() %>%
    remove_y_axis_labels() %>%
    remove_y_axis_title()
}

#' @rdname remove_y_axis
#' @export
remove_y_axis_line <- function(plot) {
  check_tidyplot(plot)
  plot + ggplot2::theme(axis.line.y = ggplot2::element_blank())
}

#' @rdname remove_y_axis
#' @export
remove_y_axis_ticks <- function(plot) {
  check_tidyplot(plot)
  plot + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
}

#' @rdname remove_y_axis
#' @export
remove_y_axis_labels <- function(plot) {
  check_tidyplot(plot)
  plot + ggplot2::theme(axis.text.y = ggplot2::element_blank())
}

#' @rdname remove_y_axis
#' @export
remove_y_axis_title <- function(plot) {
  check_tidyplot(plot)
  plot + ggplot2::theme(axis.title.y = ggplot2::element_blank())
}

#' Remove plot area padding
#' @inherit common_arguments
#'
#' @examples
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_mean_bar() %>%
#'   remove_plot_area_padding()
#' # Before removing plot area padding
#' animals %>%
#'   tidyplot(x = weight, y = speed, color = family) %>%
#'   add_data_points()
#' # After removing plot area padding
#' animals %>%
#'   tidyplot(x = weight, y = speed, color = family) %>%
#'   add_data_points() %>%
#'   remove_plot_area_padding()
#'
#' @export
remove_plot_area_padding <- function(plot, force_continuous = FALSE) {
  check_tidyplot(plot)
  plot %>%
    adjust_x_axis(padding = c(0, 0), force_continuous = force_continuous) %>%
    adjust_y_axis(padding = c(0, 0), force_continuous = force_continuous)
}

