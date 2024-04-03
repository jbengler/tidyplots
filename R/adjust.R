
ff_adjust_axis <- function(axis) {
  function(plot, title = ggplot2::waiver(), breaks = ggplot2::waiver(),
           labels = ggplot2::waiver(), limits = NULL, padding = c(NA, NA),
           rotate_labels = FALSE, transformation = "identity",
           cut_short_scale = FALSE, force_continuous = FALSE, ...) {
    check_tidyplot(plot)
  # Parse title
  if (!is_waiver(title)) title <- tidyplot_parser(as.character(title))

  # Rotate labels
  if (rotate_labels == TRUE) rotate_labels <- 45
  if (is.numeric(rotate_labels)) {
    if (axis == "x")
      plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = rotate_labels, hjust=1))
    if (axis == "y")
      plot <- plot + ggplot2::theme(axis.text.y = ggplot2::element_text(angle = rotate_labels, hjust=1))
  }

  # Adjust limits
  if (axis == "x") plot$tidyplot$limits_x <- limits %||% plot$tidyplot$limits_x
  if (axis == "y") plot$tidyplot$limits_y <- limits %||% plot$tidyplot$limits_y
  # Remove padding when limits are present
  if (!is.null(plot$tidyplot$limits_x)) plot$tidyplot$padding_x <- c(0, 0)
  if (!is.null(plot$tidyplot$limits_y)) plot$tidyplot$padding_y <- c(0, 0)

  # Adjust padding (aka expansion)
  if (axis == "x") {
    if (!is.na(padding[[1]])) plot$tidyplot$padding_x[[1]] <- padding[[1]]
    if (!is.na(padding[[2]])) plot$tidyplot$padding_x[[2]] <- padding[[2]]
  }
  if (axis == "y") {
    if (!is.na(padding[[1]])) plot$tidyplot$padding_y[[1]] <- padding[[1]]
    if (!is.na(padding[[2]])) plot$tidyplot$padding_y[[2]] <- padding[[2]]
  }
  expand_x <- ggplot2::expansion(mult = plot$tidyplot$padding_x)
  expand_y <- ggplot2::expansion(mult = plot$tidyplot$padding_y)

  # Datetime
  if (is_datetime(plot, axis)) {
    cli::cli_alert_success("adjust_{axis}_axis: {.pkg datetime}")
    suppressMessages(
      if (axis == "x")
        plot <- plot + ggplot2::scale_x_datetime(name = title, breaks = breaks, labels = labels, expand = expand_x, ...)
      else
        plot <- plot + ggplot2::scale_y_datetime(name = title, breaks = breaks, labels = labels, expand = expand_y, ...)
    )
    return(plot)
  }

  # Date
  if (is_date(plot, axis)) {
    cli::cli_alert_success("adjust_{axis}_axis: {.pkg date}")
    suppressMessages(
      if (axis == "x")
        plot <- plot + ggplot2::scale_x_date(name = title, breaks = breaks, labels = labels, expand = expand_x, ...)
      else
        plot <- plot + ggplot2::scale_y_date(name = title, breaks = breaks, labels = labels, expand = expand_y, ...)
    )
    return(plot)
  }

  # Time
  if (is_time(plot, axis)) {
    cli::cli_alert_success("adjust_{axis}_axis: {.pkg time}")
    suppressMessages(
      if (axis == "x")
        plot <- plot + ggplot2::scale_x_time(name = title, breaks = breaks, labels = labels, expand = expand_x, ...)
      else
        plot <- plot + ggplot2::scale_y_time(name = title, breaks = breaks, labels = labels, expand = expand_y, ...)
    )
    return(plot)
  }

  # Continuous
  if (is_continuous(plot, axis) || force_continuous) {
    if (is_waiver(labels) && cut_short_scale)
      labels <- scales::label_number(scale_cut = scales::cut_short_scale())
    cli::cli_alert_success("adjust_{axis}_axis: {.pkg continuous}")
    suppressMessages(
      if (axis == "x")
        plot <- plot + ggplot2::scale_x_continuous(name = title, breaks = breaks, labels = labels, limits = NULL, expand = expand_x, trans = transformation, ...)
      else
        plot <- plot + ggplot2::scale_y_continuous(name = title, breaks = breaks, labels = labels, limits = NULL, expand = expand_y, trans = transformation, ...)
    )

    # Set limits
    if (!is.null(plot$tidyplot$limits_x) && is_continuous(plot, "x")) xlim <- plot$tidyplot$limits_x else xlim <- NULL
    if (!is.null(plot$tidyplot$limits_y) && is_continuous(plot, "y")) ylim <- plot$tidyplot$limits_y else ylim <- NULL
    if (!is.null(xlim) || !is.null(ylim))
      suppressMessages(plot <- plot + ggplot2::coord_cartesian(xlim = xlim, ylim = ylim))
    return(plot)
  }

  # Discrete
  if (is_discrete(plot, axis)) {
    if (is_waiver(labels))
      labels <- tidyplot_parse_labels()
    cli::cli_alert_success("adjust_{axis}_axis: {.pkg discrete}")
    suppressMessages(
      if (axis == "x")
        plot <- plot + ggplot2::scale_x_discrete(name = title, breaks = breaks, labels = labels, expand = ggplot2::waiver(), ...)
      else
        plot <- plot + ggplot2::scale_y_discrete(name = title, breaks = breaks, labels = labels, expand = ggplot2::waiver(), ...)
    )
    return(plot)
  }

  # Catch the rest
  cli::cli_alert_warning("adjust_{axis}_axis: {.pkg x axis} was not changed.")
  return(plot)
  }
}
#' Adjust axes
#' @param title bla
#' @param limits bla
#' @param rotate_labels bla
#' @param transformation bla
#' @param cut_short_scale bla
#' @param force_continuous bla
#' @param padding bla
#' @inherit common_arguments
#' @inheritParams ggplot2::scale_x_continuous
#' @export
adjust_x_axis <- ff_adjust_axis("x")
#' @rdname adjust_x_axis
#' @export
adjust_y_axis <- ff_adjust_axis("y")


#' Adjust plot size
#' @param width bla
#' @param height bla
#' @param unit bla
#' @inherit common_arguments
#' @export
adjust_plot_size <- function(plot, width = 50, height = 50, unit = "mm") {
  check_tidyplot(plot)
  cli::cli_alert_success("adjust_plot_size: {.pkg width} = {width} {unit}, {.pkg height} = {height} {unit}")
  if (!is.na(width)) width <- ggplot2::unit(width, unit)
  if (!is.na(height)) height <- ggplot2::unit(height, unit)
  plot + patchwork::plot_layout(widths = width, heights = height)
}


#' Adjust font
#' @inherit common_arguments
#' @inheritParams ggplot2::element_text
#' @export
adjust_font <- function(plot, fontsize = 7, family = NULL, face = NULL, color = "black") {
  check_tidyplot(plot)
  plot +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = fontsize, family = family, face = face, colour = color, hjust = 0.5, vjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = fontsize, family = family, face = face, colour = color, hjust = 0.5, vjust = 0.5),
      text = ggplot2::element_text(size = fontsize, family = family, face = face, colour = color),
      axis.text = ggplot2::element_text(size = fontsize, family = family, face = face, colour = color),
      axis.title = ggplot2::element_text(size = fontsize, family = family, face = face, colour = color),
      legend.title = ggplot2::element_text(size = fontsize, family = family, face = face, colour = color),
      legend.text = ggplot2::element_text(size = fontsize, family = family, face = face, colour = color),
      strip.text = ggplot2::element_text(size = fontsize, family = family, face = face, colour = color),
      legend.key.size = ggplot2::unit(4, "mm")
    )
}


#' Adjust legend
#' @param title bla
#' @param position The position of legends. Can be one of `"none"`, `"left"`, `"right"`,
#'   `"bottom"`, `"top"`, or a two-element numeric vector.
#' @inherit common_arguments
#' @export
adjust_legend <- function(plot, title = ggplot2::waiver(), position = "right") {
  check_tidyplot(plot)
  # parse title
  if (!is_waiver(title)) title <- tidyplot_parser(as.character(title))
  plot +
    ggplot2::labs(colour = title, fill = title) +
    ggplot2::theme(legend.position = position)
}


#' Adjust padding
#' @param top bla
#' @param right bla
#' @param bottom bla
#' @param left bla
#' @param force_continuous bla
#' @inherit common_arguments
#' @export
adjust_padding <- function(plot, top = NA, right = NA, bottom = NA, left = NA, force_continuous = FALSE) {
  check_tidyplot(plot)
  plot %>%
    adjust_x_axis(padding = c(left, right)) %>%
    adjust_y_axis(padding = c(bottom, top), force_continuous = force_continuous)
}


#' Rotate plot by 90 degrees
#' @param ... Arguments passed on to the `ggplot2::coord_flip()`.
#' @inherit common_arguments
#' @export
adjust_rotate_plot <- function(plot, ...) {
  check_tidyplot(plot)
  plot + ggplot2::coord_flip(...)
}

#' Adjust description
#' @param title bla
#' @param x_axis_title bla
#' @param y_axis_title bla
#' @param legend_title bla
#' @param caption bla
#' @param ... Arguments passed on to the `ggplot2::labs()`.
#' @inherit common_arguments
#' @export
adjust_description <- function(plot, title = ggplot2::waiver(), x_axis_title = ggplot2::waiver(),
                              y_axis_title = ggplot2::waiver(), legend_title = ggplot2::waiver(),
                              caption = ggplot2::waiver(), ...) {
  check_tidyplot(plot)
  colour <- fill <- legend_title
  plot + ggplot2::labs(x = x_axis_title, y = y_axis_title, colour = colour, fill = fill,
                     title = title, caption = caption, ...)
}
