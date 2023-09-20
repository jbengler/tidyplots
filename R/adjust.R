#' @importFrom ggplot2 labs waiver scale_x_continuous scale_y_continuous expansion
#' @importFrom ggplot2 theme_grey scale_fill_manual scale_color_manual

#' @export
adjust_size <- function(gg, width = 30, height = 25, my_unit = "mm") {
  cli::cli_alert_success("adjust_size: {.pkg width} = {width} {my_unit}, {.pkg height} = {height} {my_unit}")
  if (!is.na(width)) width <- unit(width, my_unit)
  if (!is.na(height)) height <- unit(height, my_unit)
  gg + patchwork::plot_layout(widths = width, heights = height)
}

#' @export
adjust_colors <- function(gg, colors, fill_alpha = 1) {
  out <- gg
  if (!missing(colors)) {
    suppressMessages(
      out <-
        out +
        scale_fill_manual(values = apply_alpha(colors, alpha = fill_alpha), drop = FALSE) +
        scale_color_manual(values = colors, drop = FALSE)
    )
    cli::cli_alert_success("adjust_colors: applied custom {.pkg colors}")
  }
  return(out)
}

#' @export
adjust_x_axis <- function(gg, breaks = waiver(), labels = waiver(), expand_bottom = 0.05, expand_top = 0.05, expand = expansion(mult = c(expand_bottom, expand_top)), transformation = "identity", position = "bottom", ...) {
  if (is_datetime(gg, "x")) {
    cli::cli_alert_success("adjust_x_axis: {.pkg datetime}")
    suppressMessages(
      gg <- gg + scale_x_datetime(breaks = breaks, labels = labels, expand = expand, position = position, ...)
    )
    return(gg)
  }
  if (is_date(gg, "x")) {
    cli::cli_alert_success("adjust_x_axis: {.pkg date}")
    suppressMessages(
      gg <- gg + scale_x_date(breaks = breaks, labels = labels, expand = expand, position = position, ...)
    )
    return(gg)
  }
  if (is_time(gg, "x")) {
    cli::cli_alert_success("adjust_x_axis: {.pkg time}")
    suppressMessages(
      gg <- gg + scale_x_time(breaks = breaks, labels = labels, expand = expand, position = position, ...)
    )
    return(gg)
  }
  if (is_continuous(gg, "x")) {
    if (is_waiver(labels))
      labels <- scales::label_number(scale_cut = scales::cut_short_scale())
    cli::cli_alert_success("adjust_x_axis: {.pkg continuous}")
    suppressMessages(
      gg <- gg + scale_x_continuous(breaks = breaks, labels = labels, expand = expand, trans = transformation, position = position, ...)
    )
    return(gg)
  }
  # catch the rest
  cli::cli_alert_warning("adjust_x_axis: {.pkg x axis} was not changes because it is {.pkg not continuous}.")
  cli::cli_alert_warning("Use {.pkg adjust_labels()} and {.pkg adjust_order()} to change discrete axis.")
  return(gg)
}

#' @export
adjust_y_axis <- function(gg, breaks = waiver(), labels = waiver(), expand_bottom = 0.05, expand_top = 0.05, expand = expansion(mult = c(expand_bottom, expand_top)), transformation = "identity", position = "left", ...) {
  if (is_continuous(gg, "y")) {
    if (is_waiver(labels))
      labels <- scales::label_number(scale_cut = scales::cut_short_scale())
    cli::cli_alert_success("adjust_y_axis: {.pkg continuous}")
    suppressMessages(
      gg <- gg + scale_y_continuous(breaks = breaks, labels = labels, expand = expand, trans = transformation, position = position, ...)
    )
    return(gg)
  }
  # catch the rest
  cli::cli_alert_warning("adjust_y_axis: {.pkg y axis} was not changes because it is {.pkg not continuous}.")
  cli::cli_alert_warning("Use {.pkg adjust_labels()} and {.pkg adjust_order()} to change discrete axis.")
  return(gg)
}

#' @export
adjust_description <- function(gg, x = waiver(), y = waiver(), title = waiver(), subtitle = waiver(), color = waiver(), fill = waiver(), caption = waiver(), tag = waiver()) {
  gg +
    labs(x = x, y = y, title = title, subtitle = subtitle, color = color, fill = fill, caption = caption, tag = tag)
}

#' @importFrom ggplot2 %+%

#' @export
adjust_order <- function(gg, var, levels, sort_by, reverse = FALSE) {
  if (!missing(levels) && !missing(var)) {
    out <-
      gg$data %>% dplyr::mutate({{var}} := factor({{var}}, levels = levels))
    cli::cli_alert_success("adjust_order: reorderd by {.pkg levels}")
    return(gg %+% out)
  }
  if (!missing(sort_by) && !missing(var)) {
    out <-
      gg$data %>% dplyr::mutate({{var}} := forcats::fct_reorder({{var}}, {{sort_by}}))
    cli::cli_alert_success("adjust_order: reorderd by variable {.pkg sort_by}")
    return(gg %+% out)
  }
  if (reverse && !missing(var)) {
    out <-
      gg$data %>% dplyr::mutate({{var}} := forcats::fct_rev({{var}}))
    cli::cli_alert_success("adjust_order: {.pkg reversed} order of labels")
    return(gg %+% out)
  }
  cli::cli_alert_warning("adjust_order: order was {.pkg not changed}.")
  cli::cli_alert_warning("Please provide {.pkg var} together with {.pkg levels}, {.pkg sort_by} or {.pkg reverse = TRUE}.")
}

#' @export
adjust_labels <- function(gg, var, labels) {
  if (!missing(labels) && !missing(var)) {
    labels <- setNames(names(labels), labels)
    out <-
      gg$data %>% dplyr::mutate({{var}} := forcats::fct_recode({{var}}, !!!labels))
    cli::cli_alert_success("adjust_labels: applied custom {.pkg labels}")
    return(gg %+% out)
  }
  cli::cli_alert_warning("adjust_labels: lables were {.pkg not changed}.")
  cli::cli_alert_warning("Please provide {.pkg var} together with {.pkg labels}")
}

