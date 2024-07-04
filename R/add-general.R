#' Common arguments
#'
#' @param plot A `tidyplot` generated with the function `tidyplot()`.
#' @param data The data to be displayed in this layer. There are three options:
#'
#'   * If `all_rows()` (the default) the complete plot data is displayed.
#'
#'   * A `function` to subset the plot data. See `filter_rows()` and friends.
#'
#'   * A `data.frame` to override the plot data.
#' @param dodge_width For adjusting the distance between grouped objects.
#' @param preserve Should dodging preserve the `"total"` width of all elements at
#'   a position, or the width of a `"single"` element?
#' @param rasterize If `FALSE` (the default) the layer will be constructed of
#'   vector shapes. If `TRUE` the layer will be rastered to a pixel image. This can
#'   be useful when plotting many individual objects (1,000 or more) compromises
#'   the performance of the generated PDF file.
#' @param rasterize_dpi The resolution in dots per inch (dpi) used for rastering
#'   the layer if `rasterize` is `TRUE`. The default is `300` dpi.
#' @param shape An `integer` between `0` and `24`, representing the shape of the
#'   plot symbol.
#'
#'   ```{r echo=FALSE, fig.width=4, fig.height=4, fig.dpi=120}
#'   shapes <- data.frame(
#'   shape = c(0:19, 22, 21, 24, 23, 20),
#'   x = 0:24 %/% 5,
#'   y = -(0:24 %% 5)
#'   )
#'   ggplot2::ggplot(shapes, ggplot2::aes(x, y)) +
#'   ggplot2::geom_point(ggplot2::aes(shape = shape), size = 5, fill = "red") +
#'   ggplot2::geom_text(ggplot2::aes(label = shape), hjust = 0, nudge_x = 0.15) +
#'   ggplot2::scale_shape_identity() +
#'   ggplot2::expand_limits(x = 4.1) +
#'   ggplot2::theme_void()
#'   ```
#' @param size A `number` representing the size of the plot symbol. Typical
#'   values range between `1` and `3`.
#' @param width Horizontal width of the plotted object (bar, error bar, boxplot,
#'   violin plot, etc). Typical values range between `0` and `1`.
#' @param linewidth Thickness of the line in points (pt). Typical values range between `0.25` and `1`.
#' @param ... Arguments passed on to the `geom` function.
#' @param alpha common
#' @param color common
#' @param fill common
#' @param saturation common
#' @param group common
#' @param reverse common
#' @param scale_cut common
#' @param fontsize common
#' @param replace_na common
#' @return A `tidyplot` object
#' @keywords internal
#' @name common_arguments
NULL


## Error bar function factory
ff_errorbar <- function(.fun.data) {
  function(plot, dodge_width = NULL, width = 0.4, linewidth = 0.25, preserve = "total", ...) {
    check_tidyplot(plot)
    dodge_width <- dodge_width %||% plot$tidyplot$dodge_width
    position <- ggplot2::position_dodge(width = dodge_width, preserve = preserve)
    plot + ggplot2::stat_summary(fun.data = .fun.data, geom = "errorbar",
                               linewidth = linewidth, width = width, position = position, ...)
  }
}
#' Add error bar
#'
#' * `add_error_bar()` adds the standard error of mean.
#' * `add_range_bar()` adds the range from smallest to largest value.
#' * `add_sd_bar()` adds the standard deviation.
#' * `add_ci95_bar()` adds the 95% confidence interval.
#'
#' @inherit common_arguments
#'
#' @examples
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_data_points() %>%
#'   add_error_bar()
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_data_points() %>%
#'   add_range_bar()
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_data_points() %>%
#'   add_sd_bar()
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_data_points() %>%
#'   add_ci95_bar()
#'
#' @export
add_error_bar <- ff_errorbar(.fun.data = ggplot2::mean_se)
#' @rdname add_error_bar
#' @export
add_range_bar <- ff_errorbar(.fun.data = min_max)
#' @rdname add_error_bar
#' @export
add_sd_bar <- ff_errorbar(.fun.data = mean_sdl)
#' @rdname add_error_bar
#' @export
add_ci95_bar <- ff_errorbar(.fun.data = mean_cl_boot)


## Ribbon function factory
ff_ribbon <- function(.fun.data) {
  function(plot, dodge_width = NULL, alpha = 0.3, color = NA, ...) {
    check_tidyplot(plot)
    mapping <- ggplot2::aes()
    mapping$group <- plot$mapping$colour
    dodge_width <- dodge_width %||% plot$tidyplot$dodge_width
    position <- ggplot2::position_dodge(width = dodge_width)
    plot + ggplot2::stat_summary(mapping = mapping, fun.data = .fun.data, geom = "ribbon",
                               alpha = alpha, color = color, position = position, ...)
  }
}
#' Add ribbon
#'
#' * `add_error_ribbon()` adds the standard error of mean.
#' * `add_range_ribbon()` adds the range from smallest to largest value.
#' * `add_sd_ribbon()` adds the standard deviation.
#' * `add_ci95_ribbon()` adds the 95% confidence interval.
#'
#' @inherit common_arguments
#'
#' @examples
#' time_course %>%
#'   tidyplot(x = day, y = score, color = treatment, dodge_width = 0) %>%
#'   add_mean_line() %>%
#'   add_error_ribbon()
#' time_course %>%
#'   tidyplot(x = day, y = score, color = treatment, dodge_width = 0) %>%
#'   add_mean_line() %>%
#'   add_range_ribbon()
#' time_course %>%
#'   tidyplot(x = day, y = score, color = treatment, dodge_width = 0) %>%
#'   add_mean_line() %>%
#'   add_sd_ribbon()
#' time_course %>%
#'   tidyplot(x = day, y = score, color = treatment, dodge_width = 0) %>%
#'   add_mean_line() %>%
#'   add_ci95_ribbon()
#'
#' @export
add_error_ribbon <- ff_ribbon(.fun.data = ggplot2::mean_se)
#' @rdname add_error_ribbon
#' @export
add_range_ribbon <- ff_ribbon(.fun.data = min_max)
#' @rdname add_error_ribbon
#' @export
add_sd_ribbon <- ff_ribbon(.fun.data = ggplot2::mean_sdl)
#' @rdname add_error_ribbon
#' @export
add_ci95_ribbon <- ff_ribbon(.fun.data = ggplot2::mean_cl_boot)


## Bar function factory
ff_bar <- function(.fun, .count = FALSE) {
  function(plot, dodge_width = NULL, width = 0.6, saturation = 1, preserve = "total", ...) {
    check_tidyplot(plot)
    dodge_width <- dodge_width %||% plot$tidyplot$dodge_width
    position <- ggplot2::position_dodge(width = dodge_width, preserve = preserve)
    plot <- plot %>% adjust_colors(saturation = saturation)
    if (.count) {
      plot <- plot +
        ggplot2::stat_count(geom = "bar", color = NA, width = width, position = position, ...)
    } else {
      plot <- plot +
        ggplot2::stat_summary(fun = .fun, geom = "bar", color = NA, width = width,
                              position = position, ...)
    }
    # remove padding between bar and axis
    if (is_flipped(plot)) {
      plot <- plot %>% adjust_x_axis(padding = c(0, NA), force_continuous = TRUE)
    } else {
      plot <- plot %>% adjust_y_axis(padding = c(0, NA), force_continuous = TRUE)
    }
    plot
  }
}
## Dash function factory
ff_dash <- function(.fun, .count = FALSE) {
  function(plot, dodge_width = NULL, width = 0.6, linewidth = 0.25, preserve = "total", ...) {
    check_tidyplot(plot)
    dodge_width <- dodge_width %||% plot$tidyplot$dodge_width
    position <- ggplot2::position_dodge(width = dodge_width, preserve = preserve)
    if (.count) {
      plot + ggplot2::geom_errorbar(
        ggplot2::aes(ymin = ggplot2::after_stat(count), ymax = ggplot2::after_stat(count)),
        stat = "count", linewidth = linewidth, width = width, position = position, ...)
    } else {
      plot + ggplot2::stat_summary(fun.min = .fun, fun.max = .fun, geom = "errorbar",
                                 linewidth = linewidth, width = width, position = position, ...)
    }
  }
}
## Dot function factory
ff_dot <- function(.fun, .count = FALSE) {
  function(plot, dodge_width = NULL, size = 2, preserve = "total", ...) {
    check_tidyplot(plot)
    dodge_width <- dodge_width %||% plot$tidyplot$dodge_width
    position <- ggplot2::position_dodge(width = dodge_width, preserve = preserve)
    if (.count) {
      plot + ggplot2::stat_count(geom = "point", size = size, position = position, ...)
    } else {
      plot + ggplot2::stat_summary(fun = .fun, geom = "point", size = size, position = position, ...)
    }
  }
}
## Value function factory
ff_value <- function(.fun, .count = FALSE) {
  function(plot, dodge_width = NULL, accuracy = 0.1, scale_cut = NULL, fontsize = 7,
           extra_padding = 0.15, vjust = NULL, hjust = NULL, preserve = "total", ...) {
    check_tidyplot(plot)
    ptype <- get_plottype(plot)

    if ((stringr::str_sub(ptype, 2, 2) == "c" || .count)) {
      vjust <- vjust %||% -1
      hjust <- hjust %||% 0.5
      plot <- plot %>% adjust_y_axis(padding = c(NA, extra_padding), force_continuous = TRUE)
    }
    if ((stringr::str_sub(ptype, 1, 1) == "c")) {
      vjust <- vjust %||% 0.5
      hjust <- hjust %||% -0.25
      plot <- plot %>% adjust_x_axis(padding = c(NA, extra_padding), force_continuous = TRUE)
    }
    vjust <- vjust %||% 0.5
    hjust <- hjust %||% 0.5

    size <- fontsize/ggplot2::.pt
    dodge_width <- dodge_width %||% plot$tidyplot$dodge_width
    position <- ggplot2::position_dodge(width = dodge_width, preserve = preserve)
    if (.count) {
      plot <- plot +
        ggplot2::stat_count(ggplot2::aes(label = format_number(ggplot2::after_stat(count), accuracy = accuracy, scale_cut = scale_cut)),
                            geom = "text", vjust = vjust, hjust = hjust, size = size, position = position, show.legend = FALSE, ...)
    } else {
      if ((stringr::str_sub(ptype, 2, 2) == "c")) {
        plot <- plot + ggplot2::stat_summary(ggplot2::aes(label = format_number(ggplot2::after_stat(y), accuracy = accuracy, scale_cut = scale_cut)),
                              fun = .fun, geom = "text", vjust = vjust, hjust = hjust, size = size, position = position, show.legend = FALSE, ...)
      }
      if ((stringr::str_sub(ptype, 1, 1) == "c")) {
          plot <- plot + ggplot2::stat_summary(ggplot2::aes(label = format_number(ggplot2::after_stat(x), accuracy = accuracy, scale_cut = scale_cut)),
                              fun = .fun, geom = "text", vjust = vjust, hjust = hjust, size = size, position = position, show.legend = FALSE, ...)
      }
    }
    plot
  }
}
## Line function factory
ff_line <- function(.fun, .count = FALSE, .geom) {
  function(plot, group, dodge_width = NULL, linewidth = 0.25, preserve = "total", ...) {
    check_tidyplot(plot)
    if(.geom == "area") linewidth = NA
    mapping <- NULL
    if (is_missing(plot, "group")) {
      mapping <- ggplot2::aes()
      mapping$group <- plot$mapping$colour
    }
    if (!missing(group)) {
      mapping <- ggplot2::aes(group = {{group}})
    }
    dodge_width <- dodge_width %||% plot$tidyplot$dodge_width
    position <- ggplot2::position_dodge(width = dodge_width, preserve = preserve)
    if (.count) {
      plot <- plot + ggplot2::stat_count(mapping = mapping, geom = .geom,
                               linewidth = linewidth, position = position, ...)
    } else {
      plot <- plot + ggplot2::stat_summary(mapping = mapping, fun = .fun, geom = .geom,
                                 linewidth = linewidth, position = position, ...)
    }
    if(.geom == "area") {
      # remove padding between area and axis
      if (is_flipped(plot)) {
        plot <- plot %>% adjust_x_axis(padding = c(0, NA), force_continuous = TRUE)
      } else {
        plot <- plot %>% adjust_y_axis(padding = c(0, NA), force_continuous = TRUE)
      }
    }
    plot
  }
}


#' Add mean
#'
#' @param vjust bla
#' @param hjust bla
#' @param extra_padding bla
#' @inherit common_arguments
#' @inheritParams scales::number
#' @export
add_mean_bar <- ff_bar(.fun = mean)
#' @rdname add_mean_bar
#' @export
add_mean_dash <- ff_dash(.fun = mean)
#' @rdname add_mean_bar
#' @export
add_mean_dot <- ff_dot(.fun = mean)
#' @rdname add_mean_bar
#' @export
add_mean_value <- ff_value(.fun = mean)
#' @rdname add_mean_bar
#' @export
add_mean_line <- ff_line(.fun = mean, .geom = "line")
#' @rdname add_mean_bar
#' @export
add_mean_area <- ff_line(.fun = mean, .geom = "area")

#' Add median
#'
#' @inherit common_arguments
#' @inheritParams add_mean_bar
#' @export
add_median_bar <- ff_bar(.fun = median)
#' @rdname add_median_bar
#' @export
add_median_dash <- ff_dash(.fun = median)
#' @rdname add_median_bar
#' @export
add_median_dot <- ff_dot(.fun = median)
#' @rdname add_median_bar
#' @export
add_median_value <- ff_value(.fun = median)
#' @rdname add_median_bar
#' @export
add_median_line <- ff_line(.fun = median, .geom = "line")
#' @rdname add_median_bar
#' @export
add_median_area <- ff_line(.fun = median, .geom = "area")


#' Add sum
#'
#' @inherit common_arguments
#' @inheritParams add_mean_bar
#' @export
add_sum_bar <- ff_bar(.fun = sum)
#' @rdname add_sum_bar
#' @export
add_sum_dash <- ff_dash(.fun = sum)
#' @rdname add_sum_bar
#' @export
add_sum_dot <- ff_dot(.fun = sum)
#' @rdname add_sum_bar
#' @export
add_sum_value <- ff_value(.fun = sum)
#' @rdname add_sum_bar
#' @export
add_sum_line <- ff_line(.fun = sum, .geom = "line")
#' @rdname add_sum_bar
#' @export
add_sum_area <- ff_line(.fun = sum, .geom = "area")


#' Add count
#'
#' @inherit common_arguments
#' @inheritParams add_mean_bar
#'
#' @export
add_count_bar <- ff_bar(.count = TRUE)
#' @rdname add_count_bar
#' @export
add_count_dash <- ff_dash(.count = TRUE)
#' @rdname add_count_bar
#' @export
add_count_dot <- ff_dot(.count = TRUE)
#' @rdname add_count_bar
#' @export
add_count_value <- ff_value(.count = TRUE)
#' @rdname add_count_bar
#' @export
add_count_line <- ff_line(.count = TRUE, .geom = "line")
#' @rdname add_count_bar
#' @export
add_count_area <- ff_line(.count = TRUE, .geom = "area")
