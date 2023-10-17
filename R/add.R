
add_geom <- function(gg, geom, rasterize = FALSE, rasterize_dpi = 300) {
  pf <- parent_function()
  if (check_input(gg) == "none")
    stop(glue::glue("{pf}: Please provide a ggplot or list of ggplots as input to 'gg'"))

  if (rasterize) {
    cli::cli_alert_success("{pf}: {.pkg rasterized} at {rasterize_dpi} dpi")
    gg + ggrastr::rasterise(geom, dpi = rasterize_dpi, dev = "ragg")
  } else {
    gg + geom
  }
}

#' Add `ggplot2` code
#'
#' @examples
#' df_demo %>%
#'   tidyplot(x = category, y = value, color = category) %>%
#'   add(ggplot2::geom_point())
#'
#' @export
add <- .Primitive("+")

# for x = continuous, y = continuous, use add_scatter()
# for x = discrete, y = continuous, use add_jitter() or add_scatter()

#' Add data points
#'
#' @param gg bla
#' @param subset_data bla
#' @param dodge_width bla
#' @param jitter_width bla
#' @param jitter_height bla
#' @param point_size bla
#' @param point_shape bla
#' @param rasterize bla
#' @param rasterize_dpi bla
#' @param ... bla
#'
#' @export
add_scatter <- function(gg, subset_data = all_data(), dodge_width = NULL,
                        point_size = 0.5, point_shape = 19,
                        rasterize = FALSE, rasterize_dpi = 300, ...) {
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width)
  add_geom(gg, ggplot2::geom_point(data = subset_data, size = point_size, shape = point_shape, position = position, ...),
           rasterize = rasterize, rasterize_dpi = rasterize_dpi)
}
#' @rdname add_scatter
#' @export
add_jitter <- function(gg, subset_data = all_data(), dodge_width = NULL,
                       jitter_width = 0.1, jitter_height = 0,
                       point_size = 0.5, point_shape = 19,
                       rasterize = FALSE, rasterize_dpi = 300, ...) {
  if (is_continuous(gg, "x")) {
    cli::cli_alert_warning("add_jitter() adds a small amount of random variation to the x position.")
    cli::cli_alert_warning("Did you want to use 'add_scatter()' instead?")
  }
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_jitterdodge(jitter.width = jitter_width,
                                            jitter.height = jitter_height,
                                            dodge.width = dodge_width)
  add_geom(gg, ggplot2::geom_point(data = subset_data, size = point_size, shape = point_shape, position = position, ...),
           rasterize = rasterize, rasterize_dpi = rasterize_dpi)
}

#' Add error bar
#'
#' @param gg bla
#' @param dodge_width bla
#' @param width bla
#' @param linewidth bla
#' @param conf.int bla
#' @param ... bla
#'
#' @export
add_sem <- function(gg, dodge_width = NULL, width = 0.4, linewidth = 0.25, ...) {
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width)
  gg + ggplot2::stat_summary(fun.data = ggplot2::mean_se, geom = "errorbar",
                             linewidth = linewidth, width = width, position = position, ...)
}
#' @rdname add_sem
#' @export
add_range <- function(gg, dodge_width = NULL, width = 0.4, linewidth = 0.25, ...) {
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width)
  gg + ggplot2::stat_summary(fun.min = min, fun.max = max, geom = "errorbar",
                             linewidth = linewidth, width = width, position = position, ...)
}
#' @rdname add_sem
#' @export
add_sd <- function(gg, dodge_width = NULL, width = 0.4, linewidth = 0.25, ...) {
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width)
  gg + ggplot2::stat_summary(fun.data = ggplot2::mean_sdl, geom = "errorbar",
                             linewidth = linewidth, width = width, position = position, ...)
}
#' @rdname add_sem
#' @export
add_ci <- function(gg, conf.int = 0.95, dodge_width = NULL, width = 0.4, linewidth = 0.25, ...) {
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width)
  gg + ggplot2::stat_summary(fun.data = ggplot2::mean_cl_boot,
                             fun.args = list(conf.int = conf.int), geom = "errorbar",
                             linewidth = linewidth, width = width, position = position, ...)
}
#' @rdname add_sem
#' @export
add_error <- add_sem

#' Add ribbon
#'
#' @param gg bla
#' @param dodge_width bla
#' @param alpha bla
#' @param color bla
#' @param conf.int bla
#' @param ... bla
#'
#' @export
add_sem_ribbon <- function(gg, dodge_width = NULL, alpha = 0.3, color = NA, ...) {
  mapping <- ggplot2::aes()
  mapping$group <- gg$mapping$colour
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width)
  gg + ggplot2::stat_summary(mapping = mapping, fun.data = ggplot2::mean_se, geom = "ribbon",
                             alpha = alpha, color = color, position = position, ...)
}
#' @rdname add_sem_ribbon
#' @export
add_range_ribbon <- function(gg, dodge_width = NA, alpha = 0.3, color = NA, ...) {
  mapping <- ggplot2::aes()
  mapping$group <- gg$mapping$colour
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width)
  gg + ggplot2::stat_summary(mapping = mapping, fun.min = min, fun.max = max, geom = "ribbon",
                             alpha = alpha, color = color, position = position, ...)
}
#' @rdname add_sem_ribbon
#' @export
add_sd_ribbon <- function(gg, dodge_width = NA, alpha = 0.3, color = NA, ...) {
  mapping <- ggplot2::aes()
  mapping$group <- gg$mapping$colour
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width)
  gg + ggplot2::stat_summary(mapping = mapping, fun.data = ggplot2::mean_sdl, geom = "ribbon",
                             alpha = alpha, color = color, position = position, ...)
}
#' @rdname add_sem_ribbon
#' @export
add_ci_ribbon <- function(gg, conf.int = 0.95, dodge_width = NA, alpha = 0.3, color = NA, ...) {
  mapping <- ggplot2::aes()
  mapping$group <- gg$mapping$colour
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width)
  gg + ggplot2::stat_summary(mapping = mapping, fun.data = ggplot2::mean_cl_boot,
                             fun.args = list(conf.int = conf.int), geom = "ribbon",
                             alpha = alpha, color = color, position = position, ...)
}

#' Add mean
#'
#' @param gg bla
#' @param dodge_width bla
#' @param width bla
#' @param linewidth bla
#' @param size description
#' @param alpha description
#' @param accuracy description
#' @param fontsize description
#' @param vjust description
#' @param padding_top description
#' @param ... description
#'
#' @export
add_mean_bar <- function(gg, dodge_width = NULL, width = 0.6, alpha = 1, ...) {
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width)
  gg <- gg %>%
    adjust_y_axis(padding_bottom = 0) %>%
    adjust_colors(fill_alpha = alpha)
  gg + ggplot2::stat_summary(fun = mean, geom = "bar", color = NA, width = width,
                             position = position, ...)
}
#' @rdname add_mean_bar
#' @export
add_mean_dash <- function(gg, dodge_width = NULL, width = 0.6, linewidth = 0.25, ...) {
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width)
  gg + ggplot2::stat_summary(fun.min = mean, fun.max = mean, geom = "errorbar",
                             linewidth = linewidth, width = width, position = position, ...)
}
#' @rdname add_mean_bar
#' @export
add_mean_dot <- function(gg, dodge_width = NULL, size = 2, ...) {
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width)
  gg + ggplot2::stat_summary(fun = mean, geom = "point", size = size, position = position, ...)
}
#' @rdname add_mean_bar
#' @export
add_mean_value <- function(gg, dodge_width = NULL, accuracy = 0.1, fontsize = 7,
                           vjust = -0.5, padding_top = 0.15, ...) {
  size <- fontsize/ggplot2::.pt
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width)
  gg <- gg %>% adjust_y_axis(padding_top = padding_top)
  gg + ggplot2::stat_summary(ggplot2::aes(label = format_number(ggplot2::after_stat(y), accuracy = accuracy)),
                             fun = mean, geom = "text", vjust = vjust, size = size, position = position, ...)
}

#' Add median
#'
#' @param gg bla
#' @param dodge_width bla
#' @param width bla
#' @param linewidth bla
#' @param size description
#' @param alpha description
#' @param accuracy description
#' @param fontsize description
#' @param vjust description
#' @param padding_top description
#' @param ... description
#'
#' @export
add_median_bar <- function(gg, dodge_width = NULL, alpha = 1, width = 0.6, ...) {
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width)
  gg <- gg %>%
    adjust_y_axis(padding_bottom = 0) %>%
    adjust_colors(fill_alpha = alpha)
  gg + ggplot2::stat_summary(fun = median, geom = "bar", color = NA, width = width,
                             position = position, ...)
}
#' @rdname add_median_bar
#' @export
add_median_dash <- function(gg, dodge_width = NULL, width = 0.6, linewidth = 0.25, ...) {
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width)
  gg + ggplot2::stat_summary(fun.min = median, fun.max = median, geom = "errorbar",
                             linewidth = linewidth, width = width, position = position, ...)
}
#' @rdname add_median_bar
#' @export
add_median_dot <- function(gg, dodge_width = NULL, size = 2, ...) {
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width)
  gg + ggplot2::stat_summary(fun = median, geom = "point", size = size, position = position, ...)
}
#' @rdname add_median_bar
#' @export
add_median_value <- function(gg, accuracy = 0.1, dodge_width = NULL, fontsize = 7,
                             vjust = -0.5, padding_top = 0.15, ...) {
  size <- fontsize/ggplot2::.pt
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width)
  gg <- gg %>% adjust_y_axis(padding_top = padding_top)
  gg + ggplot2::stat_summary(ggplot2::aes(label = format_number(ggplot2::after_stat(y), accuracy = accuracy)),
                             fun = median, geom = "text", vjust = vjust, size = size, position = position, ...)
}


#' Add sum
#'
#' @param gg bla
#' @param dodge_width bla
#' @param width bla
#' @param linewidth bla
#' @param size description
#' @param alpha description
#' @param accuracy description
#' @param fontsize description
#' @param vjust description
#' @param padding_top description
#' @param ... description
#'
#' @export
add_sum_bar <- function(gg, dodge_width = NULL, alpha = 1, width = 0.6, ...) {
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width)
  gg <- gg %>%
    adjust_y_axis(padding_bottom = 0) %>%
    adjust_colors(fill_alpha = alpha)
  gg + ggplot2::stat_summary(fun = sum, geom = "bar", color = NA, width = width,
                             position = position, ...)
}
#' @rdname add_sum_bar
#' @export
add_sum_dash <- function(gg, dodge_width = NULL, width = 0.6, linewidth = 0.25, ...) {
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width)
  gg + ggplot2::stat_summary(fun.min = sum, fun.max = sum, geom = "errorbar", linewidth = linewidth,
                             width = width, position = position, ...)
}
#' @rdname add_sum_bar
#' @export
add_sum_dot <- function(gg, dodge_width = NULL, size = 2, ...) {
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width)
  gg + ggplot2::stat_summary(fun = sum, geom = "point", size = size, position = position, ...)
}
#' @rdname add_sum_bar
#' @export
add_sum_value <- function(gg, accuracy = 0.1, dodge_width = NULL, fontsize = 7,
                          vjust = -0.5, padding_top = 0.15, ...) {
  size <- fontsize/ggplot2::.pt
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width)
  gg <- gg %>% adjust_y_axis(padding_top = padding_top)
  gg + ggplot2::stat_summary(ggplot2::aes(label = format_number(ggplot2::after_stat(y), accuracy = accuracy)),
                             fun = sum, geom = "text", vjust = vjust, size = size, position = position, ...)
}


#' Add count
#'
#' @param gg bla
#' @param dodge_width bla
#' @param width bla
#' @param linewidth bla
#' @param size description
#' @param alpha description
#' @param accuracy description
#' @param fontsize description
#' @param vjust description
#' @param padding_top description
#' @param ... description
#'
#' @export
add_count_bar <- function(gg, dodge_width = NULL, alpha = 1, width = 0.6, ...) {
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width)
  gg <- gg %>%
    adjust_y_axis(padding_bottom = 0, force = TRUE) %>%
    adjust_colors(fill_alpha = alpha)
  gg + ggplot2::stat_count(geom = "bar", color = NA, width = width, position = position, ...)
}
#' @rdname add_count_bar
#' @export
add_count_dash <- function(gg, dodge_width = NULL, width = 0.6, linewidth = 0.25, ...) {
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width)
  gg + ggplot2::stat_count(fun.min = ggplot2::after_stat(count), fun.max = ggplot2::after_stat(count),
                           geom = "errorbar", linewidth = linewidth, width = width, position = position, ...)
}
#' @rdname add_count_bar
#' @export
add_count_dot <- function(gg, dodge_width = NULL, size = 2, ...) {
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width)
  gg + ggplot2::stat_count(geom = "point", size = size, position = position, ...)
}
#' @rdname add_count_bar
#' @export
add_count_value <- function(gg, accuracy = 0.1, dodge_width = NULL, fontsize = 7,
                            vjust = -0.5, padding_top = 0.15, ...) {
  size <- fontsize/ggplot2::.pt
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width)
  gg <- gg %>% adjust_y_axis(padding_top = padding_top, force = TRUE)
  gg + ggplot2::stat_count(ggplot2::aes(label = format_number(ggplot2::after_stat(count), accuracy = accuracy)),
                           geom = "text", vjust = vjust, size = size, position = position, ...)
}

#' Add boxplot
#'
#' @param gg bla
#' @param dodge_width bla
#' @param alpha bla
#' @param show_whiskers bla
#' @param show_outliers bla
#' @param box_width bla
#' @param whiskers_width bla
#' @param outlier.size bla
#' @param coef bla
#' @param outlier.shape bla
#' @param linewidth bla
#' @param ... bla
#'
#' @export
add_box <- function(gg, dodge_width = NULL, alpha = 0.3, show_whiskers = TRUE, show_outliers = FALSE,
                    box_width = 0.6, whiskers_width = 0.5, outlier.size = 0.5, coef = 1.5,
                    outlier.shape = 19, linewidth = 0.25, ...) {
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width)
  gg <- gg %>% adjust_colors(fill_alpha = alpha)
  if (show_whiskers == FALSE) {
    coef = 0
    whiskers_width = box_width
  }
  if (show_outliers == FALSE) outlier.shape = NA
  gg +
    ggplot2::stat_boxplot(geom ='errorbar', width = whiskers_width, position = position,
                          linewidth = linewidth, coef = coef) +
    ggplot2::geom_boxplot(outlier.shape = outlier.shape, outlier.size = outlier.size,
                          width = box_width, position = position, linewidth = linewidth, coef = coef, ...)
}

# boxplot median not the same as violin draw_quantiles = c(0.5)!
# https://stackoverflow.com/questions/36033341/differing-quantiles-boxplot-vs-violinplot

#' Add violin plot
#'
#' @param gg bla
#' @param dodge_width bla
#' @param alpha bla
#' @param draw_quantiles bla
#' @param trim bla
#' @param linewidth bla
#' @param scale bla
#' @param ... bla
#'
#' @export
add_violin <- function(gg, dodge_width = NULL, alpha = 0.3, draw_quantiles = NULL, trim = FALSE,
                       linewidth = 0.25, scale = "width", ...) {
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width)
  gg <- gg %>% adjust_colors(fill_alpha = alpha)
  gg + ggplot2::geom_violin(draw_quantiles = draw_quantiles, trim = trim, linewidth = linewidth,
                            scale = scale, position = position, ...)
}

#' Add line
#' @param gg bla
#' @param dodge_width bla
#' @param linewidth bla
#' @param ... bla
#' @param method bla
#' @param alpha bla
#' @export
add_line <- function(gg, dodge_width = NULL, linewidth = 0.25, ...) {
  mapping <- ggplot2::aes()
  mapping$group <- gg$mapping$colour
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width)
  gg + ggplot2::stat_summary(mapping = mapping, fun = mean, geom = "line",
                             linewidth = linewidth, position = position, ...)
}
#' @rdname add_line
#' @export
add_area <- function(gg, linewidth = 0.25, ...) {
  gg %>%
    remove_padding() +
    ggplot2::geom_area(linewidth = linewidth, position = ggplot2::position_identity(), ...)
}
#' @rdname add_line
#' @export
add_curve <- function(gg, dodge_width = NULL, method = "loess", linewidth = 0.25, alpha = 0.3, ...) {
  mapping <- ggplot2::aes()
  mapping$group <- gg$mapping$colour
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width)
  gg + ggplot2::geom_smooth(mapping = mapping, method = method, linewidth = linewidth,
                            alpha = alpha, position = position, ...)
}

#' Add pie chart
#' @param gg bla
#' @param bar_width bla
#' @param reverse bla
#' @param ... bla
#' @export
add_pie <- function(gg, bar_width = 1, reverse = FALSE, ...) {
  gg %>%
    remove_padding() %>%
    style_void() +
    ggplot2::geom_col(ggplot2::aes(x = NA), position = ggplot2::position_fill(reverse = reverse),
                      width = bar_width, color = NA, ...) +
    ggplot2::coord_polar("y") +
    ggplot2::guides()
}
#' @rdname add_pie
#' @export
add_donut <- function(gg, bar_width = 1, reverse = FALSE, ...) {
  gg %>%
    remove_padding() %>%
    style_void() +
    ggplot2::geom_col(ggplot2::aes(x = NA), position = ggplot2::position_fill(reverse = reverse),
                      width = bar_width, color = NA, ...) +
    ggplot2::scale_x_discrete(limits = function(x) c("", "", x)) +
    ggplot2::coord_polar("y") +
    ggplot2::guides()
}

#' Add bar stack
#' @param gg bla
#' @param bar_width bla
#' @param reverse bla
#' @param ... bla
#' @export
add_barstack_relative <- function(gg, bar_width = 0.8, reverse = FALSE, ...) {
  gg %>%
    adjust_y_axis(padding_bottom = 0, padding_top = 0) +
    ggplot2::geom_col(position = ggplot2::position_fill(reverse = reverse),
                      width = bar_width, color = NA, ...)
}
#' @rdname add_barstack_relative
#' @export
add_barstack_absolute <- function(gg, bar_width = 0.8, reverse = FALSE, ...) {
  gg %>%
    adjust_y_axis(padding_bottom = 0, padding_top = 0) +
    ggplot2::geom_col(position = ggplot2::position_stack(reverse = reverse),
                      width = bar_width, color = NA, ...)
}

#' Add area stack
#' @param gg bla
#' @param linewidth bla
#' @param reverse bla
#' @param ... bla
#' @export
add_areastack_absolute <- function(gg, linewidth = 0.25, reverse = FALSE, ...) {
  gg %>%
    remove_padding() +
    ggplot2::geom_area(linewidth = linewidth, position = ggplot2::position_stack(reverse = reverse), ...)
}
#' @rdname add_areastack_absolute
#' @export
add_areastack_relative <- function(gg, linewidth = 0.25, reverse = FALSE, ...) {
  gg %>%
    remove_padding() +
    ggplot2::geom_area(linewidth = linewidth, position = ggplot2::position_fill(reverse = reverse), ...)
}

#' Add histogram
#' @param gg bla
#' @param binwidth bla
#' @param bins bla
#' @param color bla
#' @param ... bla
#' @export
add_histogram <- function(gg, binwidth = NULL, bins = NULL, color = "#4DACD6", ...) {
  gg %>%
    remove_padding(force_y_continuous = TRUE) +
    ggplot2::geom_histogram(binwidth = binwidth, bins = bins, fill = color, ...)
}
#' @rdname add_histogram
#' @export
add_density_histogram <- function(gg, binwidth = NULL, bins = NULL, color = "#4DACD6", ...) {
  gg %>%
    remove_padding(force_y_continuous = TRUE) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            binwidth = binwidth,bins = bins, fill = color, ...)
}

#' Add density curve
#' @param gg bla
#' @param bw bla
#' @param adjust bla
#' @param kernel bla
#' @param n bla
#' @param color bla
#' @param fill bla
#' @param alpha bla
#' @param ... bla
#' @export
add_density_curve <- function(gg, bw = "nrd0", adjust = 1, kernel = "gaussian", n = 512, color = "#E37D46", fill = "#E37D46", alpha = 0.3, ...) {
  gg %>%
    remove_padding(force_y_continuous = TRUE) +
    ggplot2::geom_density(bw = bw, adjust = adjust, kernel = kernel, n = n, color = color, fill = fill, alpha = alpha, ...)
}

#' Add heatmap
#' @param gg bla
#' @param ... bla
#' @export
add_heatmap <- function(gg, ...) {
  gg %>%
    remove_x_axis_line() %>%
    remove_y_axis_line() +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::geom_raster(...)
}

# annotation

#' Add plot title or caption
#' @param gg bla
#' @param title bla
#' @param caption bla
#' @export
add_title <- function(gg, title = ggplot2::waiver()) {
  # parse title
  if (!is_waiver(title)) title <- tidyplot_parser(as.character(title))
  gg + ggplot2::labs(title = title)
}
#' @rdname add_title
#' @export
add_caption <- function(gg, caption = ggplot2::waiver()) {
  # parse caption
  if (!is_waiver(caption)) caption <- tidyplot_parser(as.character(caption))
  gg + ggplot2::labs(caption = caption)
}


#' Add reference lines
#' @param gg bla
#' @param x bla
#' @param y bla
#' @param linetype bla
#' @param linewidth bla
#' @param ... bla
#' @export
add_reference_lines <- function(gg, x = NULL, y = NULL, linetype = "dashed", linewidth = 0.25, ...) {
  out <- gg
  if(!is.null(x)) {
    out <- out + ggplot2::geom_vline(xintercept = x, linetype = linetype, linewidth = linewidth, ...)
  }
  if(!is.null(y)) {
    out <- out + ggplot2::geom_hline(yintercept = y, linetype = linetype, linewidth = linewidth, ...)
  }
  out
}

#' Add text labels
#' @param gg bla
#' @param var bla
#' @param subset_data bla
#' @param fontsize bla
#' @param segment.size bla
#' @param box.padding bla
#' @param ... bla
#' @export
add_text <- function(gg, var, subset_data = all_data(), fontsize = 7,
                     segment.size = 0.2, box.padding = 0.2, ...) {
  size <- fontsize/ggplot2::.pt
  gg + ggrepel::geom_text_repel(data = subset_data, ggplot2::aes(label = {{var}}), size = size,
                                segment.size = segment.size, box.padding = box.padding, ...)
}
