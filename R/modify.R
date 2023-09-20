#' @importFrom ggplot2 labs waiver scale_x_continuous scale_y_continuous expansion coord_flip
#' @importFrom ggplot2 theme_grey scale_fill_manual scale_color_manual

#' @export
modify_flip <- function(gg, ...) {
  gg + coord_flip(...)
}

#' @export
modify_size <- function(gg, width = 30, height = 25, my_unit = "mm") {
  cli::cli_alert_success("modify_size: {.pkg width} = {width} {my_unit}, {.pkg height} = {height} {my_unit}")
  if (!is.na(width)) width <- unit(width, my_unit)
  if (!is.na(height)) height <- unit(height, my_unit)
  gg + patchwork::plot_layout(widths = width, heights = height)
}

#' @export
modify_colors <- function(gg, colors, fill_alpha = 1) {
  out <- gg
  if (!missing(colors)) {
    suppressMessages(
      out <-
        out +
        scale_fill_manual(values = apply_alpha(colors, alpha = fill_alpha), drop = FALSE) +
        scale_color_manual(values = colors, drop = FALSE)
    )
    cli::cli_alert_success("modify_colors: applied custom {.pkg colors}")
  }
  return(out)
}

#' @export
modify_x_axis <- function(gg, breaks = waiver(), labels = waiver(), expand_bottom = 0.05, expand_top = 0.05, expand = expansion(mult = c(expand_bottom, expand_top)), transformation = "identity", position = "bottom", ...) {
  if (is_datetime(gg, "x")) {
    cli::cli_alert_success("modify_x_axis: {.pkg datetime}")
    suppressMessages(
      gg <- gg + scale_x_datetime(breaks = breaks, labels = labels, expand = expand, position = position, ...)
    )
    return(gg)
  }
  if (is_date(gg, "x")) {
    cli::cli_alert_success("modify_x_axis: {.pkg date}")
    suppressMessages(
      gg <- gg + scale_x_date(breaks = breaks, labels = labels, expand = expand, position = position, ...)
    )
    return(gg)
  }
  if (is_time(gg, "x")) {
    cli::cli_alert_success("modify_x_axis: {.pkg time}")
    suppressMessages(
      gg <- gg + scale_x_time(breaks = breaks, labels = labels, expand = expand, position = position, ...)
    )
    return(gg)
  }
  if (is_continuous(gg, "x")) {
    if (is_waiver(labels))
      labels <- scales::label_number(scale_cut = scales::cut_short_scale())
    cli::cli_alert_success("modify_x_axis: {.pkg continuous}")
    suppressMessages(
      gg <- gg + scale_x_continuous(breaks = breaks, labels = labels, expand = expand, trans = transformation, position = position, ...)
    )
    return(gg)
  }
  # catch the rest
  cli::cli_alert_warning("modify_x_axis: {.pkg x axis} was not changes because it is {.pkg not continuous}.")
  cli::cli_alert_warning("Use {.pkg modify_labels()} and {.pkg modify_order()} to change discrete axis.")
  return(gg)
}

#' @export
modify_y_axis <- function(gg, breaks = waiver(), labels = waiver(), expand_bottom = 0.05, expand_top = 0.05, expand = expansion(mult = c(expand_bottom, expand_top)), transformation = "identity", position = "left", ...) {
  if (is_continuous(gg, "y")) {
    if (is_waiver(labels))
      labels <- scales::label_number(scale_cut = scales::cut_short_scale())
    cli::cli_alert_success("modify_y_axis: {.pkg continuous}")
    suppressMessages(
      gg <- gg + scale_y_continuous(breaks = breaks, labels = labels, expand = expand, trans = transformation, position = position, ...)
    )
    return(gg)
  }
  # catch the rest
  cli::cli_alert_warning("modify_y_axis: {.pkg y axis} was not changes because it is {.pkg not continuous}.")
  cli::cli_alert_warning("Use {.pkg modify_labels()} and {.pkg modify_order()} to change discrete axis.")
  return(gg)
}

#' @export
modify_description <- function(gg, x = waiver(), y = waiver(), title = waiver(), subtitle = waiver(), color = waiver(), fill = waiver(), caption = waiver(), tag = waiver()) {
  gg +
    labs(x = x, y = y, title = title, subtitle = subtitle, color = color, fill = fill, caption = caption, tag = tag)
}
