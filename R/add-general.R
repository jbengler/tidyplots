#' Common arguments
#'
#' @param plot A `tidyplot` generated with the function `tidyplot()`.
#' @param data The data to be displayed in this layer. There are three options:
#'
#'   * If `all_rows()` (the default) the complete dataset is displayed.
#'
#'   * A `function` to subset the plot data. See `filter_rows()` and friends.
#'
#'   * A `data.frame` to override the plot data.
#' @param dodge_width For adjusting the distance between grouped objects. Defaults
#' to `0.8` for plots with at least one discrete axis and `0` for plots with two
#' continuous axes.
#' @param preserve Should dodging preserve the `"total"` width of all elements at
#'   a position, or the width of a `"single"` element?
#' @param rasterize If `FALSE` (the default) the layer will be constructed of
#'   vector shapes. If `TRUE` the layer will be rasterized to a pixel image. This can
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
#' @param alpha A `number` between `0` and `1` for the opacity of an object. A value of `0` is completely transparent, `1` is completely opaque.
#' @param color A hex color for the stroke color. For example, `"#FFFFFF"` for white.
#' @param fill A hex color for the fill color. For example, `"#FFFFFF"` for white.
#' @param saturation A `number` between `0` and `1` for the color saturation of an object. A value of `0` is completely desaturated (white), `1` is the original color.
#' @param group Variable in the dataset to be used for grouping.
#' @param reverse Whether the order should be reversed or not. Defaults to `FALSE`, meaning not reversed.
#' @param .reverse Whether the order should be reversed or not. Defaults to `FALSE`, meaning not reversed.
#' @param scale_cut Scale cut function to be applied. See `scales::cut_short_scale()` and friends.
#' @param fontsize Font size in points. Defaults to `7`.
#' @param replace_na Whether to replace `count = NA` with `count = 0`.
#' @param width Width of the bar.
#' @param force_continuous Whether to force the axis to be continuous. Defaults to `FALSE`.
#' @param jitter_width Amount of random noise to be added to the
#'  horizontal position of the of the data points. This can be useful to deal
#'  with overplotting. Typical values range between `0` and `1`.
#' @param jitter_height Amount of random noise to be added to the
#'  vertical position of the of the data points. This can be useful to deal
#'  with overplotting. Typical values range between `0` and `1`.
#' @return A `tidyplot` object.
#' @keywords internal
#' @name common_arguments
NULL


## Error bar function factory
ff_errorbar <- function(.fun.data) {
  function(plot, dodge_width = NULL, width = 0.4, linewidth = 0.25, preserve = "total", ...) {
    plot <- check_tidyplot(plot)
    dodge_width <- dodge_width %||% plot$tidyplot$dodge_width
    position <- ggplot2::position_dodge(width = dodge_width, preserve = preserve)
    plot + ggplot2::stat_summary(fun.data = .fun.data, geom = "errorbar",
                               linewidth = linewidth, width = width, position = position, ...)
  }
}
#' Add error bar
#'
#' * `add_sem_errorbar()` adds the standard error of mean.
#' * `add_range_errorbar()` adds the range from smallest to largest value.
#' * `add_sd_errorbar()` adds the standard deviation.
#' * `add_ci95_errorbar()` adds the 95% confidence interval.
#'
#' @param width Width of the error bar.
#' @inherit common_arguments
#'
#' @examples
#' # Standard error of the mean
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_data_points() |>
#'   add_sem_errorbar()
#'
#' # Range from minimum to maximum value
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_data_points() |>
#'   add_range_errorbar()
#'
#' # Standard deviation
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_data_points() |>
#'   add_sd_errorbar()
#'
#' # 95% confidence interval
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_data_points() |>
#'   add_ci95_errorbar()
#'
#' # Changing arguments: error bar width
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_data_points() |>
#'   add_sem_errorbar(width = 0.8)
#'
#' # Changing arguments: error bar line width
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_data_points() |>
#'   add_sem_errorbar(linewidth = 1)
#'
#' @export
add_sem_errorbar <- ff_errorbar(.fun.data = mean_se)
#' @rdname add_sem_errorbar
#' @export
add_range_errorbar <- ff_errorbar(.fun.data = min_max)
#' @rdname add_sem_errorbar
#' @export
add_sd_errorbar <- ff_errorbar(.fun.data = mean_sd)
#' @rdname add_sem_errorbar
#' @export
add_ci95_errorbar <- ff_errorbar(.fun.data = mean_cl_boot)


## Ribbon function factory
ff_ribbon <- function(.fun.data) {
  function(plot, dodge_width = NULL, alpha = 0.4, color = NA, ...) {
    plot <- check_tidyplot(plot)
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
#' * `add_sem_ribbon()` adds the standard error of mean.
#' * `add_range_ribbon()` adds the range from smallest to largest value.
#' * `add_sd_ribbon()` adds the standard deviation.
#' * `add_ci95_ribbon()` adds the 95% confidence interval.
#'
#' @inherit common_arguments
#'
#' @examples
#' # Standard error of the mean
#' time_course |>
#'   tidyplot(x = day, y = score, color = treatment) |>
#'   add_mean_line() |>
#'   add_sem_ribbon()
#'
#' # Range from minimum to maximum value
#' time_course |>
#'   tidyplot(x = day, y = score, color = treatment) |>
#'   add_mean_line() |>
#'   add_range_ribbon()
#'
#' # Standard deviation
#' time_course |>
#'   tidyplot(x = day, y = score, color = treatment) |>
#'   add_mean_line() |>
#'   add_sd_ribbon()
#'
#' # 95% confidence interval
#' time_course |>
#'   tidyplot(x = day, y = score, color = treatment) |>
#'   add_mean_line() |>
#'   add_ci95_ribbon()
#'
#' # Changing arguments: alpha
#' time_course |>
#'   tidyplot(x = day, y = score, color = treatment) |>
#'   add_mean_line() |>
#'   add_sem_ribbon(alpha = 0.7)
#'
#' @export
add_sem_ribbon <- ff_ribbon(.fun.data = mean_se)
#' @rdname add_sem_ribbon
#' @export
add_range_ribbon <- ff_ribbon(.fun.data = min_max)
#' @rdname add_sem_ribbon
#' @export
add_sd_ribbon <- ff_ribbon(.fun.data = mean_sd)
#' @rdname add_sem_ribbon
#' @export
add_ci95_ribbon <- ff_ribbon(.fun.data = mean_cl_boot)


## Bar function factory
ff_bar <- function(.fun, .count = FALSE) {
  function(plot, dodge_width = NULL, width = 0.6, saturation = 1, preserve = "total", ...) {
    plot <- check_tidyplot(plot)
    dodge_width <- dodge_width %||% plot$tidyplot$dodge_width
    position <- ggplot2::position_dodge(width = dodge_width, preserve = preserve)
    if (saturation != 1) {
      plot <- plot |> adjust_colors(saturation = saturation)
    }
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
      plot <- plot |> adjust_x_axis(padding = c(0, NA), force_continuous = TRUE)
    } else {
      plot <- plot |> adjust_y_axis(padding = c(0, NA), force_continuous = TRUE)
    }
    plot
  }
}
## Dash function factory
ff_dash <- function(.fun, .count = FALSE) {
  function(plot, dodge_width = NULL, width = 0.6, linewidth = 0.25, preserve = "total", ...) {
    plot <- check_tidyplot(plot)
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
    plot <- check_tidyplot(plot)
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
    plot <- check_tidyplot(plot)
    ptype <- get_plottype(plot)

    if ((stringr::str_sub(ptype, 2, 2) == "c" || .count)) {
      vjust <- vjust %||% -1
      hjust <- hjust %||% 0.5
      plot <- plot |> adjust_y_axis(padding = c(NA, extra_padding), force_continuous = TRUE)
    }
    if ((stringr::str_sub(ptype, 1, 1) == "c")) {
      vjust <- vjust %||% 0.5
      hjust <- hjust %||% -0.25
      plot <- plot |> adjust_x_axis(padding = c(NA, extra_padding), force_continuous = TRUE)
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
    plot <- check_tidyplot(plot)
    if(.geom == "area") linewidth <- 0
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
        plot <- plot |> adjust_x_axis(padding = c(0, NA), force_continuous = TRUE)
      } else {
        plot <- plot |> adjust_y_axis(padding = c(0, NA), force_continuous = TRUE)
      }
    }
    plot
  }
}


#' Add mean
#'
#' @param vjust Vertical position adjustment of the value label.
#' @param hjust Horizontal position adjustment of the value label.
#' @param extra_padding Extra padding to create space for the value label.
#' @inherit common_arguments
#' @inheritParams scales::number
#'
#' @examples
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_mean_bar()
#'
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_mean_dash()
#'
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_mean_dot()
#'
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_mean_value()
#'
#' study |>
#'   tidyplot(x = treatment, y = score) |>
#'   add_mean_line()
#'
#' study |>
#'   tidyplot(x = treatment, y = score) |>
#'   add_mean_area()
#'
#' # Combination
#' study |>
#'   tidyplot(x = treatment, y = score) |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_mean_dash() |>
#'   add_mean_dot() |>
#'   add_mean_value() |>
#'   add_mean_line()
#'
#' # Changing arguments: alpha
#' # Makes objects transparent
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   theme_minimal_y() |>
#'   add_mean_bar(alpha = 0.4)
#'
#' # Changing arguments: saturation
#' # Reduces fill color saturation without making the object transparent
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   theme_minimal_y() |>
#'   add_mean_bar(saturation = 0.3)
#'
#' # Changing arguments: accuracy
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_mean_value(accuracy = 0.01)
#'
#' # Changing arguments: fontsize
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_mean_value(fontsize = 10)
#'
#' # Changing arguments: color
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_mean_value(color = "black")
#'
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
#'
#' @examples
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_median_bar()
#'
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_median_dash()
#'
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_median_dot()
#'
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_median_value()
#'
#' study |>
#'   tidyplot(x = treatment, y = score) |>
#'   add_median_line()
#'
#' study |>
#'   tidyplot(x = treatment, y = score) |>
#'   add_median_area()
#'
#' # Combination
#' study |>
#'   tidyplot(x = treatment, y = score) |>
#'   add_median_bar(alpha = 0.4) |>
#'   add_median_dash() |>
#'   add_median_dot() |>
#'   add_median_value() |>
#'   add_median_line()
#'
#' # Changing arguments: alpha
#' # Makes objects transparent
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   theme_minimal_y() |>
#'   add_median_bar(alpha = 0.4)
#'
#' # Changing arguments: saturation
#' # Reduces fill color saturation without making the object transparent
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   theme_minimal_y() |>
#'   add_median_bar(saturation = 0.3)
#'
#' # Changing arguments: accuracy
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_median_value(accuracy = 0.01)
#'
#' # Changing arguments: fontsize
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_median_value(fontsize = 10)
#'
#' # Changing arguments: color
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_median_value(color = "black")
#'
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
#'
#' @examples
#' spendings |>
#'   tidyplot(x = category, y = amount, color = category) |>
#'   adjust_x_axis(rotate_labels = TRUE) |>
#'   add_sum_bar()
#'
#' spendings |>
#'   tidyplot(x = category, y = amount, color = category) |>
#'   adjust_x_axis(rotate_labels = TRUE) |>
#'   add_sum_dash()
#'
#' spendings |>
#'   tidyplot(x = category, y = amount, color = category) |>
#'   adjust_x_axis(rotate_labels = TRUE) |>
#'   add_sum_dot()
#'
#' spendings |>
#'   tidyplot(x = category, y = amount, color = category) |>
#'   adjust_x_axis(rotate_labels = TRUE) |>
#'   add_sum_value()
#'
#' spendings |>
#'   tidyplot(x = category, y = amount) |>
#'   adjust_x_axis(rotate_labels = TRUE) |>
#'   add_sum_line()
#'
#' spendings |>
#'   tidyplot(x = category, y = amount) |>
#'   adjust_x_axis(rotate_labels = TRUE) |>
#'   add_sum_area()
#'
#' # Combination
#' spendings |>
#'   tidyplot(x = category, y = amount) |>
#'   adjust_x_axis(rotate_labels = TRUE) |>
#'   add_median_bar(alpha = 0.4) |>
#'   add_median_dash() |>
#'   add_median_dot() |>
#'   add_median_value() |>
#'   add_median_line()
#'
#' # Changing arguments: alpha
#' # Makes objects transparent
#' spendings |>
#'   tidyplot(x = category, y = amount, color = category) |>
#'   theme_minimal_y() |>
#'   adjust_x_axis(rotate_labels = TRUE) |>
#'   add_sum_bar(alpha = 0.4)
#'
#' # Changing arguments: saturation
#' # Reduces fill color saturation without making the object transparent
#' spendings |>
#'   tidyplot(x = category, y = amount, color = category) |>
#'   theme_minimal_y() |>
#'   adjust_x_axis(rotate_labels = TRUE) |>
#'   add_sum_bar(saturation = 0.3)
#'
#' # Changing arguments: accuracy
#' spendings |>
#'   tidyplot(x = category, y = amount, color = category) |>
#'   adjust_x_axis(rotate_labels = TRUE) |>
#'   add_sum_value(accuracy = 1)
#'
#' # Changing arguments: fontsize
#' spendings |>
#'   tidyplot(x = category, y = amount, color = category) |>
#'   adjust_x_axis(rotate_labels = TRUE) |>
#'   add_sum_value(fontsize = 10)
#'
#' # Changing arguments: color
#' spendings |>
#'   tidyplot(x = category, y = amount, color = category) |>
#'   adjust_x_axis(rotate_labels = TRUE) |>
#'   add_sum_value(color = "black")
#'
#' # Changing arguments: extra_padding
#' spendings |>
#'   tidyplot(x = category, y = amount, color = category) |>
#'   adjust_x_axis(rotate_labels = TRUE) |>
#'   add_sum_value(extra_padding = 0.5)
#'
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
#' @examples
#' dinosaurs |>
#'   tidyplot(x = time_lived, color = time_lived) |>
#'   adjust_x_axis(rotate_labels = TRUE) |>
#'   add_count_bar()
#'
#' dinosaurs |>
#'   tidyplot(x = time_lived, color = time_lived) |>
#'   adjust_x_axis(rotate_labels = TRUE) |>
#'   add_count_dash()
#'
#' dinosaurs |>
#'   tidyplot(x = time_lived, color = time_lived) |>
#'   adjust_x_axis(rotate_labels = TRUE) |>
#'   add_count_dot()
#'
#' dinosaurs |>
#'   tidyplot(x = time_lived, color = time_lived) |>
#'   adjust_x_axis(rotate_labels = TRUE) |>
#'   add_count_value()
#'
#' dinosaurs |>
#'   tidyplot(x = time_lived) |>
#'   adjust_x_axis(rotate_labels = TRUE) |>
#'   add_count_line()
#'
#' dinosaurs |>
#'   tidyplot(x = time_lived) |>
#'   adjust_x_axis(rotate_labels = TRUE) |>
#'   add_count_area()
#'
#' # Combination
#' dinosaurs |>
#'   tidyplot(x = time_lived) |>
#'   adjust_x_axis(rotate_labels = TRUE) |>
#'   add_count_bar(alpha = 0.4) |>
#'   add_count_dash() |>
#'   add_count_dot() |>
#'   add_count_value() |>
#'   add_count_line()
#'
#' # Changing arguments: alpha
#' # Makes objects transparent
#' dinosaurs |>
#'   tidyplot(x = time_lived, color = time_lived) |>
#'   theme_minimal_y() |>
#'   adjust_x_axis(rotate_labels = TRUE) |>
#'   add_count_bar(alpha = 0.4)
#'
#' # Changing arguments: saturation
#' # Reduces fill color saturation without making the object transparent
#' dinosaurs |>
#'   tidyplot(x = time_lived, color = time_lived) |>
#'   theme_minimal_y() |>
#'   adjust_x_axis(rotate_labels = TRUE) |>
#'   add_count_bar(saturation = 0.3)
#'
#' # Changing arguments: accuracy
#' dinosaurs |>
#'   tidyplot(x = time_lived, color = time_lived) |>
#'   adjust_x_axis(rotate_labels = TRUE) |>
#'   add_count_value(accuracy = 1)
#'
#' # Changing arguments: fontsize
#' dinosaurs |>
#'   tidyplot(x = time_lived, color = time_lived) |>
#'   adjust_x_axis(rotate_labels = TRUE) |>
#'   add_count_value(fontsize = 10)
#'
#' # Changing arguments: color
#' dinosaurs |>
#'   tidyplot(x = time_lived, color = time_lived) |>
#'   adjust_x_axis(rotate_labels = TRUE) |>
#'   add_count_value(color = "black")
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
