
#' Remove legend or legend title
#' @param plot bla
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
#' @param plot bla
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
#' @param plot bla
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

#' Remove padding
#' @param plot bla
#' @param force_continuous bla
#' @export
remove_padding <- function(plot, force_continuous = FALSE) {
  check_tidyplot(plot)
  plot$tidyplot$padding_x <- c(0, 0)
  plot$tidyplot$padding_y <- c(0, 0)
  plot %>%
    adjust_x_axis() %>%
    adjust_y_axis(force_continuous = force_continuous)
}

