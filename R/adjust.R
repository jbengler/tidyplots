
#' Adjust axes
#' @param gg bla
#' @param title bla
#' @param breaks bla
#' @param labels bla
#' @param limits bla
#' @param padding_left bla
#' @param padding_right bla
#' @param padding_bottom bla
#' @param padding_top bla
#' @param rotate_labels bla
#' @param transformation bla
#' @param cut_short_scale bla
#' @param position bla
#' @param force_y_continuous bla
#' @param ... bla
#' @export
adjust_x_axis <- function(gg, title = ggplot2::waiver(), breaks = ggplot2::waiver(),
                          labels = ggplot2::waiver(), limits = NULL, padding_left = NULL,
                          padding_right = NULL, rotate_labels = FALSE, transformation = "identity",
                          cut_short_scale = FALSE, position = "bottom", ...) {

  # parse title
  if (!is_waiver(title)) title <- tidyplot_parser(as.character(title))

  # rotate labels
  if (rotate_labels == TRUE) rotate_labels <- 45
  if (rotate_labels == 45)
    gg <- gg + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust=1))
  if (rotate_labels == 90)
    gg <- gg + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
  if (rotate_labels %in% c(45, 90)) {
    cli::cli_alert_success("adjust_x_axis: labels {.pkg rotated} by {rotate_labels} degrees")
    out <- "not_NULL"
  }

  # adjust limits
  if (is.null(limits))
    limits_x <- gg$tidyplot$limits_x
  else {
    gg$tidyplot$limits_x <- limits_x <- limits
    padding_left <- padding_right <- 0
  }

  # adjust padding (aka expansion)
  if (is.null(padding_left)) padding_left <- gg$tidyplot$padding_x[[1]] else gg$tidyplot$padding_x[[1]] <- padding_left
  if (is.null(padding_right)) padding_right <- gg$tidyplot$padding_x[[2]] else gg$tidyplot$padding_x[[2]] <- padding_right
  expand <- ggplot2::expansion(mult = c(padding_left, padding_right))

  if (is_datetime(gg, "x")) {
    cli::cli_alert_success("adjust_x_axis: {.pkg datetime}")
    suppressMessages(
      gg <- gg + ggplot2::scale_x_datetime(name = title, breaks = breaks, labels = labels, expand = expand, position = position, ...)
    )
    return(gg)
  }
  if (is_date(gg, "x")) {
    cli::cli_alert_success("adjust_x_axis: {.pkg date}")
    suppressMessages(
      gg <- gg + ggplot2::scale_x_date(name = title, breaks = breaks, labels = labels, expand = expand, position = position, ...)
    )
    return(gg)
  }
  if (is_time(gg, "x")) {
    cli::cli_alert_success("adjust_x_axis: {.pkg time}")
    suppressMessages(
      gg <- gg + ggplot2::scale_x_time(name = title, breaks = breaks, labels = labels, expand = expand, position = position, ...)
    )
    return(gg)
  }
  if (is_continuous(gg, "x")) {
    if (is_waiver(labels) && cut_short_scale)
      # labels <- scales::label_number(scale_cut = scales::cut_short_scale())
      labels = ggplot2::waiver()
    cli::cli_alert_success("adjust_x_axis: {.pkg continuous}")
    suppressMessages(
      if (is_continuous(gg, "y"))
        gg <- gg + ggplot2::coord_cartesian(xlim = limits_x, ylim = gg$tidyplot$limits_y)
      else
        gg <- gg + ggplot2::coord_cartesian(xlim = limits_x))
    suppressMessages(
      gg <- gg + ggplot2::scale_x_continuous(name = title, breaks = breaks, labels = labels, limits = NULL, expand = expand, trans = transformation, position = position, ...)
    )
    return(gg)
  }
  if (is_discrete(gg, "x")) {
    if (is_waiver(labels))
      labels <- tidyplot_parse_labels()
    cli::cli_alert_success("adjust_x_axis: {.pkg discrete}")
    suppressMessages(
      gg <- gg + ggplot2::scale_x_discrete(name = title, breaks = breaks, labels = labels, expand = ggplot2::waiver(), position = position, ...)
    )
    return(gg)
  }
  # catch the rest
  cli::cli_alert_warning("adjust_x_axis: {.pkg x axis} was not changed.")
  return(gg)
}
#' @rdname adjust_x_axis
#' @export
adjust_y_axis <- function(gg, title = ggplot2::waiver(), breaks = ggplot2::waiver(),
                          labels = ggplot2::waiver(), limits = NULL, padding_bottom = NULL,
                          padding_top = NULL, transformation = "identity", position = "left",
                          cut_short_scale = TRUE, force_y_continuous = FALSE, ...) {

  # parse title
  if (!is_waiver(title)) title <- tidyplot_parser(as.character(title))

  # adjust limits
  if (is.null(limits))
    limits_y <- gg$tidyplot$limits_y
  else {
    gg$tidyplot$limits_y <- limits_y <- limits
    padding_bottom <- padding_top <- 0
  }

  # adjust padding (aka expansion)
  if (is.null(padding_bottom)) padding_bottom <- gg$tidyplot$padding_y[[1]] else gg$tidyplot$padding_y[[1]] <- padding_bottom
  if (is.null(padding_top)) padding_top <- gg$tidyplot$padding_y[[2]] else gg$tidyplot$padding_y[[2]] <- padding_top
  expand <- ggplot2::expansion(mult = c(padding_bottom, padding_top))

  if (is_datetime(gg, "y")) {
    cli::cli_alert_success("adjust_y_axis: {.pkg datetime}")
    suppressMessages(
      gg <- gg + ggplot2::scale_y_datetime(name = title, breaks = breaks, labels = labels, expand = expand, position = position, ...)
    )
    return(gg)
  }
  if (is_date(gg, "y")) {
    cli::cli_alert_success("adjust_y_axis: {.pkg date}")
    suppressMessages(
      gg <- gg + ggplot2::scale_y_date(name = title, breaks = breaks, labels = labels, expand = expand, position = position, ...)
    )
    return(gg)
  }
  if (is_time(gg, "y")) {
    cli::cli_alert_success("adjust_y_axis: {.pkg time}")
    suppressMessages(
      gg <- gg + ggplot2::scale_y_time(name = title, breaks = breaks, labels = labels, expand = expand, position = position, ...)
    )
    return(gg)
  }
  if (is_continuous(gg, "y") || force_y_continuous) {
    if (is_waiver(labels) && cut_short_scale)
      # labels <- scales::label_number(scale_cut = scales::cut_short_scale())
      labels = ggplot2::waiver()
    cli::cli_alert_success("adjust_y_axis: {.pkg continuous}")
    suppressMessages(
      if (is_continuous(gg, "x"))
        gg <- gg + ggplot2::coord_cartesian(xlim = gg$tidyplot$limits_x, ylim = limits_y)
      else
        gg <- gg + ggplot2::coord_cartesian(ylim = limits_y))
    suppressMessages(
      gg <- gg + ggplot2::scale_y_continuous(name = title, breaks = breaks, labels = labels, limits = NULL, expand = expand, trans = transformation, position = position, ...)
    )
    return(gg)
  }
  if (is_discrete(gg, "y")) {
    if (is_waiver(labels))
      labels <- tidyplot_parse_labels()
    cli::cli_alert_success("adjust_y_axis: {.pkg discrete}")
    suppressMessages(
      gg <- gg + ggplot2::scale_y_discrete(name = title, breaks = breaks, labels = labels, expand = ggplot2::waiver(), position = position, ...)
    )
    return(gg)
  }
  # catch the rest
  cli::cli_alert_warning("adjust_y_axis: {.pkg y axis} was not changed.")
  return(gg)
}

# spendings %>%
#   tidyplot(y = amount, color = category) %>%
#   add_barstack_absolute()

#' Adjust labels
#' @param gg bla
#' @param var bla
#' @param new_names bla
#' @param new_order bla
#' @param sort_by bla
#' @param reverse bla
#' @export
adjust_labels <- function(gg, var, new_names, new_order, sort_by, reverse = FALSE) {
  out <- NULL

  # rename
  if (!missing(new_names) && !missing(var)) {
    gg$tidyplot$new_names <- new_names
    new_names <- setNames(names(new_names), new_names)
    out <-
      gg$data %>% dplyr::mutate({{var}} := forcats::fct_recode({{var}}, !!!new_names))
    cli::cli_alert_success("adjust_labels: applied {.pkg new_names}")
    gg <- gg %+% out
    new_order <- names(new_names)
  }

  # reorder
  if (!missing(new_order) && !missing(var)) {
    out <-
      # gg$data %>% dplyr::mutate({{var}} := factor({{var}}, levels = new_order))
      gg$data %>% dplyr::mutate({{var}} := forcats::fct_relevel({{var}}, new_order))
    cli::cli_alert_success("adjust_labels: reorderd by {.pkg new_order}")
    gg <- gg %+% out
  }

  if (!missing(sort_by) && !missing(var)) {
    out <-
      gg$data %>% dplyr::mutate({{var}} := forcats::fct_reorder({{var}}, {{sort_by}}))
    cli::cli_alert_success("adjust_labels: reorderd by variable {.pkg sort_by}")
    return(gg %+% out)
  }

  if (reverse && !missing(var)) {
    out <-
      gg$data %>% dplyr::mutate({{var}} := forcats::fct_rev({{var}}))
    cli::cli_alert_success("adjust_labels: {.pkg reversed} order of labels")
    return(gg %+% out)
  }

  if(is.null(out)) {
    cli::cli_alert_warning("adjust_labels: {.pkg nothing was changed}.")
    cli::cli_alert_warning("Please provide {.pkg var} together with {.pkg new_names}, {.pkg new_order}, {.pkg sort_by} or {.pkg reverse = TRUE}.")
  }
  return(gg)
}


#' Adjust colors
#' @param gg bla
#' @param new_colors bla
#' @param saturation bla
#' @param as_palette bla
#' @param labels bla
#' @param ... bla
#' @export
adjust_colors <- function(gg, new_colors, saturation = 1, as_palette = FALSE,
                          labels = tidyplot_parse_labels(), ...) {
  out <- gg
  # as individual colors
  if (!missing(new_colors) && as_palette == FALSE) {
    out$tidyplot$new_colors <- new_colors
    suppressMessages(
      out <-
        out +
        ggplot2::scale_fill_manual(values = apply_saturation(new_colors, saturation = saturation), drop = FALSE, labels = labels, ...) +
        ggplot2::scale_color_manual(values = new_colors, drop = FALSE, labels = labels, ...)
    )
    cli::cli_alert_success("adjust_colors: applied {.pkg new_colors}")
  }
  # as color palette
  if (!missing(new_colors) && as_palette == TRUE) {
    suppressMessages({
      if (is_discrete(gg, "colour"))
        out <- out + my_scale_color_d(palette = new_colors, drop = FALSE, labels = labels, ...)

      if (is_discrete(gg, "fill"))
        out <- out + my_scale_fill_d(palette = new_colors, saturation = saturation, drop = FALSE, labels = labels, ...)

      if (is_continuous(gg, "colour"))
        out <- out + my_scale_color_c(palette = new_colors, labels = labels, ...)

      if (is_continuous(gg, "fill"))
        out <- out + my_scale_fill_c(palette = new_colors, saturation = saturation, labels = labels, ...)
    })
    cli::cli_alert_success("adjust_colors: applied {.pkg new color palette}")
  }
  if (missing(new_colors)) {
    suppressMessages({
      if (is_discrete(gg, "colour"))
        out <- out + my_scale_color_d(drop = FALSE, labels = labels, ...)

      if (is_discrete(gg, "fill"))
        out <- out + my_scale_fill_d(saturation = saturation, drop = FALSE, labels = labels, ...)

      if (is_continuous(gg, "colour"))
        out <- out + my_scale_color_c(labels = labels, ...)

      if (is_continuous(gg, "fill"))
        out <- out + my_scale_fill_c(saturation = saturation, labels = labels, ...)
    })
    cli::cli_alert_success("adjust_colors: applied tidyplots {.pkg default colors}")
  }
  return(out)
}

#' Adjust plot size
#' @param gg bla
#' @param width bla
#' @param height bla
#' @param unit bla
#' @export
adjust_size <- function(gg, width = 50, height = 50, unit = "mm") {
  cli::cli_alert_success("adjust_size: {.pkg width} = {width} {unit}, {.pkg height} = {height} {unit}")
  if (!is.na(width)) width <- ggplot2::unit(width, unit)
  if (!is.na(height)) height <- ggplot2::unit(height, unit)
  gg + patchwork::plot_layout(widths = width, heights = height)
}


#' Adjust fontsize
#' @param gg bla
#' @param fontsize bla
#' @export
adjust_fontsize <- function(gg, fontsize = 7) {
  gg +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = fontsize, colour = "black", hjust = 0.5, vjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = fontsize, colour = "black", hjust = 0.5, vjust = 0.5),
      text = ggplot2::element_text(size = fontsize, colour = "black"),
      axis.text = ggplot2::element_text(size = fontsize, colour = "black"),
      axis.title = ggplot2::element_text(size = fontsize, colour = "black"),
      legend.title = ggplot2::element_text(size = fontsize, colour = "black"),
      legend.text = ggplot2::element_text(size = fontsize, colour = "black"),
      strip.text = ggplot2::element_text(size = fontsize, colour = "black"),
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
#' @param force_y_continuous bla
#' @export
adjust_padding <- function(gg, top = NULL, right = NULL, bottom = NULL, left = NULL, force_y_continuous = FALSE) {
  gg %>%
    adjust_x_axis(padding_left = left, padding_right = right) %>%
    adjust_y_axis(padding_bottom = bottom, padding_top = top, force_y_continuous = force_y_continuous)
}

#' Flip plot
#' @param gg bla
#' @param ... bla
#' @export
adjust_flip <- function(gg, ...) {
  gg + ggplot2::coord_flip(...)
}

#' Adjust annotation
#' @param gg bla
#' @param title bla
#' @param x_axis_title bla
#' @param y_axis_title bla
#' @param legend_title bla
#' @param caption bla
#' @param ... bla
#' @export
adjust_annotation <- function(gg, title = ggplot2::waiver(), x_axis_title = ggplot2::waiver(),
                              y_axis_title = ggplot2::waiver(), legend_title = ggplot2::waiver(),
                              caption = ggplot2::waiver(), ...) {
  colour <- fill <- legend_title
  gg + ggplot2::labs(x = x_axis_title, y = y_axis_title, colour = colour, fill = fill,
                     title = title, caption = caption, ...)
}
