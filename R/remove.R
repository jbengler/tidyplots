
#' Remove legend or legend title
#' @param gg bla
#' @export
remove_legend <- function(gg) {
  gg + ggplot2::theme(legend.position="none")
}

#' @rdname remove_legend
#' @export
remove_legend_title <- function(gg) {
  gg + ggplot2::theme(legend.title = ggplot2::element_blank())
}

#' Remove `x` axis or parts of it
#' @param gg bla
#' @export
remove_x_axis <- function(gg) {
  gg %>%
    remove_x_axis_line() %>%
    remove_x_axis_ticks() %>%
    remove_x_axis_labels() %>%
    remove_x_axis_title()
}

#' @rdname remove_x_axis
#' @export
remove_x_axis_line <- function(gg) {
  gg + ggplot2::theme(axis.line.x = ggplot2::element_blank())
}

#' @rdname remove_x_axis
#' @export
remove_x_axis_ticks <- function(gg) {
  gg + ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
}

#' @rdname remove_x_axis
#' @export
remove_x_axis_labels <- function(gg) {
  gg + ggplot2::theme(axis.text.x = ggplot2::element_blank())
}

#' @rdname remove_x_axis
#' @export
remove_x_axis_title <- function(gg) {
  gg + ggplot2::theme(axis.title.x = ggplot2::element_blank())
}


#' Remove `y` axis or parts of it
#' @param gg bla
#' @export
remove_y_axis <- function(gg) {
  gg %>%
    remove_y_axis_line() %>%
    remove_y_axis_ticks() %>%
    remove_y_axis_labels() %>%
    remove_y_axis_title()
}

#' @rdname remove_y_axis
#' @export
remove_y_axis_line <- function(gg) {
  gg + ggplot2::theme(axis.line.y = ggplot2::element_blank())
}

#' @rdname remove_y_axis
#' @export
remove_y_axis_ticks <- function(gg) {
  gg + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
}

#' @rdname remove_y_axis
#' @export
remove_y_axis_labels <- function(gg) {
  gg + ggplot2::theme(axis.text.y = ggplot2::element_blank())
}

#' @rdname remove_y_axis
#' @export
remove_y_axis_title <- function(gg) {
  gg + ggplot2::theme(axis.title.y = ggplot2::element_blank())
}

#' Remove padding
#' @param gg bla
#' @param force_y_continuous bla
#' @export
remove_padding <- function(gg, force_y_continuous = FALSE) {
  gg$tidyplot$padding_x <- c(0, 0)
  gg$tidyplot$padding_y <- c(0, 0)
  gg %>%
    adjust_x_axis() %>%
    adjust_y_axis(force_y_continuous = force_y_continuous)
}

