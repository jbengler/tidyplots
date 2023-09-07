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
modify_colors <- function(gg, colors, fill_alpha = 0.3) {
  out <- gg
  if (!missing(colors)) {
    suppressMessages(
      out <-
        out +
        scale_fill_manual(values = apply_alpha(colors, alpha = fill_alpha)) +
        scale_color_manual(values = colors)
    )
    cli::cli_alert_success("modify_colors: applied custom {.pkg colors}")
  }
  return(out)
}

#' @export
modify_x_axis <- function(gg, breaks = waiver(), labels = scales::label_number(scale_cut = scales::cut_short_scale()), expand = expansion(mult = c(0.05, 0.05)), transformation = "identity", position = "left") {
  if (is_discrete(gg, "x")) {
    cli::cli_alert_warning("modify_x_axis: {.pkg x axis} was not changes because it is {.pkg not continuous}.")
    cli::cli_alert_warning("Use {.pkg modify_labels()} and {.pkg modify_order()} to change discrete axis.")
    gg
  } else {
    cli::cli_alert_success("modify_x_axis: {.pkg done}")
    suppressMessages(
      gg + scale_x_continuous(breaks = breaks, labels = labels, expand = expand, trans = transformation, position = position)
    )
  }
}

#' @export
modify_y_axis <- function(gg, breaks = waiver(), labels = scales::label_number(scale_cut = scales::cut_short_scale()), expand = expansion(mult = c(0.05, 0.05)), transformation = "identity", position = "left") {
  if (is_discrete(gg, "y")) {
    cli::cli_alert_warning("modify_y_axis: {.pkg y axis} was not changes because it is {.pkg not continuous}.")
    cli::cli_alert_warning("Use {.pkg modify_labels()} and {.pkg modify_order()} to change discrete axis.")
    gg
  } else {
    cli::cli_alert_success("modify_y_axis: {.pkg done}")
    suppressMessages(
      gg + scale_y_continuous(breaks = breaks, labels = labels, expand = expand, trans = transformation, position = position)
    )
  }
}

#' @export
modify_description <- function(gg, x = waiver(), y = waiver(), title = waiver(), subtitle = waiver(), color = waiver(), fill = waiver(), caption = waiver(), tag = waiver()) {
  gg +
    labs(x = x, y = y, title = title, subtitle = subtitle, color = color, fill = fill, caption = caption, tag = tag)
}
