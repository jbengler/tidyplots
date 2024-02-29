
ff_adjust_axis <- function(axis) {
  function(gg, title = ggplot2::waiver(), breaks = ggplot2::waiver(),
           labels = ggplot2::waiver(), limits = NULL, padding = c(NA, NA),
           rotate_labels = FALSE, transformation = "identity",
           cut_short_scale = FALSE, force_continuous = FALSE, ...) {

  # Parse title
  if (!is_waiver(title)) title <- tidyplot_parser(as.character(title))

  # Rotate labels
  if (rotate_labels == TRUE) rotate_labels <- 45
  if (is.numeric(rotate_labels)) {
    if (axis == "x")
      gg <- gg + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = rotate_labels, hjust=1))
    if (axis == "y")
      gg <- gg + ggplot2::theme(axis.text.y = ggplot2::element_text(angle = rotate_labels, hjust=1))
  }

  # Adjust limits
  if (axis == "x") gg$tidyplot$limits_x <- limits %||% gg$tidyplot$limits_x
  if (axis == "y") gg$tidyplot$limits_y <- limits %||% gg$tidyplot$limits_y
  # Remove padding when limits are present
  if (!is.null(gg$tidyplot$limits_x)) gg$tidyplot$padding_x <- c(0, 0)
  if (!is.null(gg$tidyplot$limits_y)) gg$tidyplot$padding_y <- c(0, 0)

  # Adjust padding (aka expansion)
  if (axis == "x") {
    if (!is.na(padding[[1]])) gg$tidyplot$padding_x[[1]] <- padding[[1]]
    if (!is.na(padding[[2]])) gg$tidyplot$padding_x[[2]] <- padding[[2]]
  }
  if (axis == "y") {
    if (!is.na(padding[[1]])) gg$tidyplot$padding_y[[1]] <- padding[[1]]
    if (!is.na(padding[[2]])) gg$tidyplot$padding_y[[2]] <- padding[[2]]
  }
  expand_x <- ggplot2::expansion(mult = gg$tidyplot$padding_x)
  expand_y <- ggplot2::expansion(mult = gg$tidyplot$padding_y)

  # Datetime
  if (is_datetime(gg, axis)) {
    cli::cli_alert_success("adjust_{axis}_axis: {.pkg datetime}")
    suppressMessages(
      if (axis == "x")
        gg <- gg + ggplot2::scale_x_datetime(name = title, breaks = breaks, labels = labels, expand = expand_x, ...)
      else
        gg <- gg + ggplot2::scale_y_datetime(name = title, breaks = breaks, labels = labels, expand = expand_y, ...)
    )
    return(gg)
  }

  # Date
  if (is_date(gg, axis)) {
    cli::cli_alert_success("adjust_{axis}_axis: {.pkg date}")
    suppressMessages(
      if (axis == "x")
        gg <- gg + ggplot2::scale_x_date(name = title, breaks = breaks, labels = labels, expand = expand_x, ...)
      else
        gg <- gg + ggplot2::scale_y_date(name = title, breaks = breaks, labels = labels, expand = expand_y, ...)
    )
    return(gg)
  }

  # Time
  if (is_time(gg, axis)) {
    cli::cli_alert_success("adjust_{axis}_axis: {.pkg time}")
    suppressMessages(
      if (axis == "x")
        gg <- gg + ggplot2::scale_x_time(name = title, breaks = breaks, labels = labels, expand = expand_x, ...)
      else
        gg <- gg + ggplot2::scale_y_time(name = title, breaks = breaks, labels = labels, expand = expand_y, ...)
    )
    return(gg)
  }

  # Continuous
  if (is_continuous(gg, axis) || force_continuous) {
    if (is_waiver(labels) && cut_short_scale)
      labels <- scales::label_number(scale_cut = scales::cut_short_scale())
    cli::cli_alert_success("adjust_{axis}_axis: {.pkg continuous}")
    suppressMessages(
      if (axis == "x")
        gg <- gg + ggplot2::scale_x_continuous(name = title, breaks = breaks, labels = labels, limits = NULL, expand = expand_x, trans = transformation, ...)
      else
        gg <- gg + ggplot2::scale_y_continuous(name = title, breaks = breaks, labels = labels, limits = NULL, expand = expand_y, trans = transformation, ...)
    )

    # Set limits
    if (!is.null(gg$tidyplot$limits_x) && is_continuous(gg, "x")) xlim <- gg$tidyplot$limits_x else xlim <- NULL
    if (!is.null(gg$tidyplot$limits_y) && is_continuous(gg, "y")) ylim <- gg$tidyplot$limits_y else ylim <- NULL
    if (!is.null(xlim) || !is.null(ylim))
      suppressMessages(gg <- gg + ggplot2::coord_cartesian(xlim = xlim, ylim = ylim))
    return(gg)
  }

  # Discrete
  if (is_discrete(gg, axis)) {
    if (is_waiver(labels))
      labels <- tidyplot_parse_labels()
    cli::cli_alert_success("adjust_{axis}_axis: {.pkg discrete}")
    suppressMessages(
      if (axis == "x")
        gg <- gg + ggplot2::scale_x_discrete(name = title, breaks = breaks, labels = labels, expand = ggplot2::waiver(), ...)
      else
        gg <- gg + ggplot2::scale_y_discrete(name = title, breaks = breaks, labels = labels, expand = ggplot2::waiver(), ...)
    )
    return(gg)
  }

  # Catch the rest
  cli::cli_alert_warning("adjust_{axis}_axis: {.pkg x axis} was not changed.")
  return(gg)
  }
}
#' Adjust axes
#' @param gg bla
#' @param title bla
#' @param breaks bla
#' @param labels bla
#' @param limits bla
#' @param padding bla
#' @param rotate_labels bla
#' @param transformation bla
#' @param cut_short_scale bla
#' @param force_continuous bla
#' @param ... bla
#' @export
adjust_x_axis <- ff_adjust_axis("x")
#' @rdname adjust_x_axis
#' @export
adjust_y_axis <- ff_adjust_axis("y")


#' Adjust plot size
#' @param gg bla
#' @param width bla
#' @param height bla
#' @param unit bla
#' @export
adjust_plot_size <- function(gg, width = 50, height = 50, unit = "mm") {
  cli::cli_alert_success("adjust_plot_size: {.pkg width} = {width} {unit}, {.pkg height} = {height} {unit}")
  if (!is.na(width)) width <- ggplot2::unit(width, unit)
  if (!is.na(height)) height <- ggplot2::unit(height, unit)
  gg + patchwork::plot_layout(widths = width, heights = height)
}


#' Adjust font
#' @param gg bla
#' @param fontsize bla
#' @param color bla
#' @inheritParams ggplot2::element_text
#' @export
adjust_font <- function(gg, fontsize = 7, family = NULL, face = NULL, color = "black") {
  gg +
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
#' @param gg bla
#' @param title bla
#' @param position bla
#' @export
adjust_legend <- function(gg, title = ggplot2::waiver(), position = "right") {
  # parse title
  if (!is_waiver(title)) title <- tidyplot_parser(as.character(title))
  gg +
    ggplot2::labs(colour = title, fill = title) +
    ggplot2::theme(legend.position = position)
}


#' Adjust padding
#' @param gg bla
#' @param top bla
#' @param right bla
#' @param bottom bla
#' @param left bla
#' @param force_continuous bla
#' @export
adjust_padding <- function(gg, top = NA, right = NA, bottom = NA, left = NA, force_continuous = FALSE) {
  gg %>%
    adjust_x_axis(padding = c(left, right)) %>%
    adjust_y_axis(padding = c(bottom, top), force_continuous = force_continuous)
}


#' Rotate plot by 90 degrees
#' @param gg bla
#' @param ... bla
#' @export
adjust_rotate_plot <- function(gg, ...) {
  gg + ggplot2::coord_flip(...)
}

#' Adjust description
#' @param gg bla
#' @param title bla
#' @param x_axis_title bla
#' @param y_axis_title bla
#' @param legend_title bla
#' @param caption bla
#' @param ... bla
#' @export
adjust_description <- function(gg, title = ggplot2::waiver(), x_axis_title = ggplot2::waiver(),
                              y_axis_title = ggplot2::waiver(), legend_title = ggplot2::waiver(),
                              caption = ggplot2::waiver(), ...) {
  colour <- fill <- legend_title
  gg + ggplot2::labs(x = x_axis_title, y = y_axis_title, colour = colour, fill = fill,
                     title = title, caption = caption, ...)
}
