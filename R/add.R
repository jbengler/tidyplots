#' General arguments
#'
#' @param plot A tidyplot generated with the `tidyplot()` function.
#' @param data A function to subset the data to be plotted. See `filter_rows()` and friends.
#' @param dodge_width For adjusting the distance between grouped objects.
#' @param preserve general
#' @param rasterize general
#' @param rasterize_dpi general
#' @name general_arguments
NULL


#' Add data points
#'
#' @param size A number representing the size of the data points.
#' @param shape A number representing the shape of the data points.
#' @param confetti If `TRUE`, data points will get a white border. This can be
#'  useful to deal with overplotting.
#' @param jitter_width For adding a small amount of random noise the to the
#'  horizontal position of the of the data points. This can be useful to deal
#'  with overplotting.
#' @param jitter_height For adding a small amount of random noise the to the
#'  vertical position of the of the data points. This can be useful to deal
#'  with overplotting.
#' @param ... bla
#' @inheritParams general_arguments
#' @inheritParams ggbeeswarm::geom_beeswarm
#'
#' @examples
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_mean_bar(alpha = 0.3) %>%
#'   add_error() %>%
#'   add_data_points_beeswarm()
#'
#' @export
add_data_points <- function(plot, data = all_rows(),
                       shape = 19, size = 1, confetti = FALSE,
                       dodge_width = NULL,
                       preserve = "total",
                       rasterize = FALSE, rasterize_dpi = 300, ...) {
  check_tidyplot(plot)
  f_points(plot = plot, data = data,
           shape = shape, size = size, confetti = confetti,
           dodge_width = dodge_width,
           preserve = preserve,
           rasterize = rasterize, rasterize_dpi = rasterize_dpi, ...)
}
#' @rdname add_data_points
#' @export
add_data_points_jitter <- function(plot, data = all_rows(),
                              shape = 19, size = 1, confetti = FALSE,
                              dodge_width = NULL,
                              jitter_width = 0.2, jitter_height = 0, preserve = "total",
                              rasterize = FALSE, rasterize_dpi = 300, ...) {
  check_tidyplot(plot)
  f_points(plot = plot, data = data,
           shape = shape, size = size, confetti = confetti,
           dodge_width = dodge_width,
           jitter_width = jitter_width, jitter_height = jitter_height, preserve = preserve,
           rasterize = rasterize, rasterize_dpi = rasterize_dpi, ...)
}
#' @rdname add_data_points
#' @export
add_data_points_beeswarm <- function(plot, data = all_rows(),
                                shape = 19, size = 1, confetti = FALSE,
                                cex = 3, corral = "wrap", corral.width = 0.5,
                                dodge_width = NULL,
                                preserve = "total",
                                rasterize = FALSE, rasterize_dpi = 300, ...) {
  check_tidyplot(plot)
  f_points(beeswarm = TRUE,
           plot = plot, data = data,
           shape = shape, size = size, confetti = confetti,
           cex = cex, corral = corral, corral.width = corral.width,
           dodge_width = dodge_width,
           preserve = preserve,
           rasterize = rasterize, rasterize_dpi = rasterize_dpi, ...)
}
## Points function
f_points <- function(plot, data = all_rows(),
                     shape = 19, size = 1, confetti = FALSE, beeswarm = FALSE,
                     cex = 3, corral = "wrap", corral.width = 0.5,
                     dodge_width = NULL,
                     jitter_width = 0, jitter_height = 0, preserve = "total",
                     rasterize = FALSE, rasterize_dpi = 300, ...) {

  if (is_discrete(plot, "x")) {
    dodge_width <- dodge_width %||% plot$tidyplot$dodge_width
  } else {
    dodge_width <- dodge_width %||% 0
  }

  if (dodge_width == 0) {
    position <- ggplot2::position_identity()
  } else {
    if (jitter_width == 0 && jitter_height == 0)
      position <- ggplot2::position_dodge(width = dodge_width, preserve = preserve)
    else
      position <- ggplot2::position_jitterdodge(jitter.width = jitter_width,
                                                jitter.height = jitter_height,
                                                dodge.width = dodge_width)
  }

  if (confetti) {
    size <- size * 1.5
    shape = 21
  }

  if (beeswarm) {
    if (confetti) {
      add_geom(plot, ggbeeswarm::geom_beeswarm(data = data, size = size, shape = shape, dodge.width = dodge_width, color = "#FFFFFF",
                                               cex = cex, corral = corral, corral.width = corral.width, ...),
               rasterize = rasterize, rasterize_dpi = rasterize_dpi, level = -1)
    } else {
      add_geom(plot, ggbeeswarm::geom_beeswarm(data = data, size = size, shape = shape, dodge.width = dodge_width,
                                               cex = cex, corral = corral, corral.width = corral.width, ...),
               rasterize = rasterize, rasterize_dpi = rasterize_dpi, level = -1)
    }

  } else {

    # not beeswarm
    if (confetti) {
      add_geom(plot, ggplot2::geom_point(data = data, size = size, shape = shape, position = position, color = "#FFFFFF", ...),
               rasterize = rasterize, rasterize_dpi = rasterize_dpi, level = -1)
    } else {
      add_geom(plot, ggplot2::geom_point(data = data, size = size, shape = shape, position = position, ...),
               rasterize = rasterize, rasterize_dpi = rasterize_dpi, level = -1)
    }
  }
}

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
#' * `add_error()` adds the standard error of mean.
#' * `add_range()` adds the range from smallest to largest value.
#' * `add_sd()` adds the standard deviation.
#' * `add_ci95()` adds the 95% confidence interval.
#'
#' @param width bla
#' @param linewidth bla
#' @param ... bla
#' @inheritParams general_arguments
#' @export
add_error <- ff_errorbar(.fun.data = ggplot2::mean_se)
#' @rdname add_error
#' @export
add_range <- ff_errorbar(.fun.data = min_max)
#' @rdname add_error
#' @export
add_sd <- ff_errorbar(.fun.data = mean_sdl)
#' @rdname add_error
#' @export
add_ci95 <- ff_errorbar(.fun.data = mean_cl_boot)


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
#' @param alpha bla
#' @param color bla
#' @param ... bla
#' @inheritParams general_arguments
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
      plot %>%
        adjust_y_axis(padding = c(0, NA), force_continuous = TRUE) +
        ggplot2::stat_count(geom = "bar", color = NA, width = width, position = position, ...)
    } else {
      plot %>%
        adjust_y_axis(padding = c(0, NA)) +
        ggplot2::stat_summary(fun = .fun, geom = "bar", color = NA, width = width,
                              position = position, ...)
    }
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
           vjust = -0.5, padding_top = 0.15, preserve = "total", ...) {
    check_tidyplot(plot)
    size <- fontsize/ggplot2::.pt
    dodge_width <- dodge_width %||% plot$tidyplot$dodge_width
    position <- ggplot2::position_dodge(width = dodge_width, preserve = preserve)
    if (.count) {
      plot %>% adjust_y_axis(padding = c(NA, padding_top), force_continuous = TRUE) +
        ggplot2::stat_count(ggplot2::aes(label = format_number(ggplot2::after_stat(count), accuracy = accuracy, scale_cut = scale_cut)),
                            geom = "text", vjust = vjust, size = size, position = position, ...)
    } else {
      plot %>% adjust_y_axis(padding = c(NA, padding_top)) +
        ggplot2::stat_summary(ggplot2::aes(label = format_number(ggplot2::after_stat(y), accuracy = accuracy, scale_cut = scale_cut)),
                              fun = .fun, geom = "text", vjust = vjust, size = size, position = position, ...)
    }
  }
}
## Line function factory
ff_line <- function(.fun, .count = FALSE, .geom) {
  function(plot, group, dodge_width = NULL, linewidth = 0.25, preserve = "total", ...) {
    check_tidyplot(plot)
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
      plot + ggplot2::stat_count(mapping = mapping, geom = .geom,
                               linewidth = linewidth, position = position, ...)
    } else {
      plot + ggplot2::stat_summary(mapping = mapping, fun = .fun, geom = .geom,
                                 linewidth = linewidth, position = position, ...)
    }
  }
}


#' Add mean
#'
#' @param width bla
#' @param linewidth bla
#' @param size bla
#' @param saturation bla
#' @param accuracy bla
#' @param fontsize bla
#' @param vjust bla
#' @param padding_top bla
#' @param scale_cut bla
#' @param group bla
#' @param ... bla
#' @inheritParams general_arguments
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
#' @inheritParams general_arguments
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
#' @inheritParams general_arguments
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
#' @inheritParams general_arguments
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


#' Add boxplot
#'
#' @param saturation bla
#' @param show_whiskers bla
#' @param show_outliers bla
#' @param box_width bla
#' @param whiskers_width bla
#' @param outlier.size bla
#' @param coef bla
#' @param outlier.shape bla
#' @param linewidth bla
#' @param ... bla
#' @inheritParams general_arguments
#'
#' @export
add_boxplot <- function(plot, dodge_width = NULL, saturation = 0.3, show_whiskers = TRUE, show_outliers = FALSE,
                    box_width = 0.6, whiskers_width = 0.5, outlier.size = 0.5, coef = 1.5,
                    outlier.shape = 19, linewidth = 0.25, preserve = "total", ...) {
  check_tidyplot(plot)
  dodge_width <- dodge_width %||% plot$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width, preserve = preserve)
  plot <- plot %>% adjust_colors(saturation = saturation)
  if (show_whiskers == FALSE) {
    coef = 0
    whiskers_width = box_width
  }
  if (show_outliers == FALSE) outlier.shape = NA
  plot +
    ggplot2::stat_boxplot(geom ='errorbar', width = whiskers_width, position = position,
                          linewidth = linewidth, coef = coef) +
    ggplot2::geom_boxplot(outlier.shape = outlier.shape, outlier.size = outlier.size,
                          width = box_width, position = position, linewidth = linewidth, coef = coef, ...)
}

# boxplot median not the same as violin draw_quantiles = c(0.5)!
# https://stackoverflow.com/questions/36033341/differing-quantiles-boxplot-vs-violinplot

#' Add violin plot
#'
#' @param saturation bla
#' @param draw_quantiles bla
#' @param trim bla
#' @param linewidth bla
#' @param scale bla
#' @param ... bla
#' @inheritParams general_arguments
#'
#' @export
add_violin <- function(plot, dodge_width = NULL, saturation = 0.3, draw_quantiles = NULL, trim = FALSE,
                       linewidth = 0.25, scale = "width", ...) {
  check_tidyplot(plot)
  dodge_width <- dodge_width %||% plot$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width)
  plot <- plot %>% adjust_colors(saturation = saturation)
  plot + ggplot2::geom_violin(draw_quantiles = draw_quantiles, trim = trim, linewidth = linewidth,
                            scale = scale, position = position, ...)
}

#' Add line
#' @param group bla
#' @param linewidth bla
#' @param ... bla
#' @inheritParams general_arguments
#' @export
add_line <- function(plot, group, dodge_width = NULL, linewidth = 0.25, preserve = "total", ...) {
  check_tidyplot(plot)
  mapping <- NULL
  if (is_missing(plot, "group")) {
    mapping <- ggplot2::aes()
    mapping$group <- plot$mapping$colour
  }
  if (!missing(group)) {
    mapping <- ggplot2::aes(group = {{group}})
  }
  dodge_width <- dodge_width %||% 0
  position <- ggplot2::position_dodge(width = dodge_width, preserve = preserve)
  plot + ggplot2::geom_line(mapping = mapping, linewidth = linewidth, position = position, ...)
}
#' @rdname add_line
#' @export
add_area <- function(plot, group, dodge_width = NULL, linewidth = 0.25, preserve = "total", ...) {
  check_tidyplot(plot)
  mapping <- NULL
  if (is_missing(plot, "group")) {
    mapping <- ggplot2::aes()
    mapping$group <- plot$mapping$colour
  }
  if (!missing(group)) {
    mapping <- ggplot2::aes(group = {{group}})
  }
  dodge_width <- dodge_width %||% 0
  position <- ggplot2::position_dodge(width = dodge_width, preserve = preserve)
  plot %>%
    remove_padding() +
    ggplot2::geom_area(mapping = mapping, linewidth = linewidth, position = position, ...)
}


#' Add curve
#' @param method bla
#' @param linewidth bla
#' @param alpha bla
#' @param ... bla
#' @inheritParams general_arguments
#' @export
add_curve <- function(plot, dodge_width = NULL, method = "loess", linewidth = 0.25, alpha = 0.3,
                      preserve = "total", ...) {
  check_tidyplot(plot)
  mapping <- ggplot2::aes()
  mapping$group <- plot$mapping$colour
  dodge_width <- dodge_width %||% plot$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width, preserve = preserve)
  plot + ggplot2::geom_smooth(mapping = mapping, method = method, linewidth = linewidth,
                            alpha = alpha, position = position, ...)
}


## Pie function factory
ff_pie <- function(.type = "pie") {
  function(plot, width = 1, reverse = FALSE, ...) {
    check_tidyplot(plot)
    plot <-
      plot %>%
      remove_padding() %>%
      style_void()

    if (is_missing(plot, "y")) {
      plot <- plot + ggplot2::geom_bar(ggplot2::aes(x = NA), position = ggplot2::position_fill(reverse = reverse),
                                   width = width, color = NA, ...)
    } else {
      plot <- plot + ggplot2::stat_summary(ggplot2::aes(x = NA), geom = "bar", fun = sum,
                                       position = ggplot2::position_fill(reverse = reverse),
                                       width = width, color = NA, ...)
    }
    suppressMessages(
      plot <- plot +
        ggplot2::coord_polar("y") +
        ggplot2::guides()
    )
    if (.type == "donut")
      suppressMessages(plot + ggplot2::scale_x_discrete(limits = function(x) c("", "", x)))
    else
      plot
  }
}
#' Add pie or donut chart
#' @param width bla
#' @param reverse bla
#' @param ... bla
#' @inheritParams general_arguments
#' @export
add_pie <- ff_pie(.type = "pie")
#' @rdname add_pie
#' @export
add_donut <- ff_pie(.type = "donut")


## Barstack function factory
ff_barstack <- function(.position_fun) {
  function(plot, width = 0.8, reverse = FALSE, ...) {
    check_tidyplot(plot)
    plot <-
      plot %>%
      adjust_y_axis(padding = c(0, 0), force_continuous = TRUE)

    mapping <- NULL
    if (is_missing(plot, "x")) mapping <- ggplot2::aes(x = "")

    if (is_missing(plot, "y")){
      plot + ggplot2::geom_bar(mapping = mapping, position = .position_fun(reverse = reverse),
                             width = width, color = NA, ...)
    } else {
      plot + ggplot2::stat_summary(mapping = mapping, geom = "bar", fun = sum,
                                 position = .position_fun(reverse = reverse), width = width,
                                 color = NA, ...)
    }
  }
}
#' Add bar stack
#' @param width bla
#' @param reverse bla
#' @param ... bla
#' @inheritParams general_arguments
#' @export
add_barstack_absolute <- ff_barstack(.position_fun = ggplot2::position_stack)
#' @rdname add_barstack_absolute
#' @export
add_barstack_relative <- ff_barstack(.position_fun = ggplot2::position_fill)


## Areastack function factory
ff_areastack <- function(.position_fun) {
  function(plot, linewidth = 0.25, alpha = 0.3, reverse = FALSE, ...) {
    check_tidyplot(plot)
    plot <-
      plot %>%
      adjust_y_axis(padding = c(0, 0), force_continuous = TRUE) %>%
      adjust_x_axis(padding = c(0, 0))

    # overwrite group aesthetic
    mapping <- ggplot2::aes()
    mapping$group <- plot$mapping$colour

    if (is_missing(plot, "y")){
      # plot + ggplot2::stat_count(mapping = mapping, geom = "area",
      #                         position = .position_fun(reverse = reverse),
      #                         linewidth = linewidth, alpha = alpha, ...)
      vars <- c(get_variable(plot, "x"), get_variable(plot, "colour"))
      df <-
        plot$data %>%
        dplyr::summarize(count = dplyr::n(), .by = all_of(vars)) %>%
        tidyr::complete(.data[[vars[1]]], .data[[vars[2]]], fill = list(count = 0))
      mapping$y <- ggplot2::aes(y = count)$y
      plot + ggplot2::geom_area(data = df, mapping = mapping,
                              position = .position_fun(reverse = reverse), linewidth = linewidth,
                              alpha = alpha, ...)
    } else {
      # plot + ggplot2::stat_summary(mapping = mapping, geom = "area", fun = sum,
      #                            position = .position_fun(reverse = reverse), alpha = alpha, linewidth = linewidth, ...)
      vars <- c(get_variable(plot, "x"), get_variable(plot, "colour"))
      y_var <- get_variable(plot, "y")
      zero <- list(y_var = 0)
      names(zero) <- y_var
      df <-
        plot$data %>%
        dplyr::summarize("{y_var}" := sum(.data[[y_var]]), .by = all_of(vars)) %>%
        tidyr::complete(.data[[vars[1]]], .data[[vars[2]]], fill = zero)
      plot + ggplot2::geom_area(data = df, mapping = mapping,
                              position = .position_fun(reverse = reverse), linewidth = linewidth,
                              alpha = alpha, ...)
    }
  }
}
#' Add area stack
#' @param linewidth bla
#' @param alpha bla
#' @param reverse bla
#' @param ... bla
#' @inheritParams general_arguments
#' @export
add_areastack_absolute <- ff_areastack(.position_fun = ggplot2::position_stack)
#' @rdname add_areastack_absolute
#' @export
add_areastack_relative <- ff_areastack(.position_fun = ggplot2::position_fill)


#' Add histogram
#' @param binwidth bla
#' @param bins bla
#' @param color bla
#' @param ... bla
#' @inheritParams general_arguments
#' @export
add_histogram <- function(plot, binwidth = NULL, bins = NULL, color = "#4DACD6", ...) {
  check_tidyplot(plot)
  plot %>%
    remove_padding(force_continuous = TRUE) +
    ggplot2::geom_histogram(binwidth = binwidth, bins = bins, fill = color, ...)
}
#' @rdname add_histogram
#' @export
add_density_histogram <- function(plot, binwidth = NULL, bins = NULL, color = "#4DACD6", ...) {
  check_tidyplot(plot)
  plot %>%
    remove_padding(force_continuous = TRUE) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            binwidth = binwidth,bins = bins, fill = color, ...)
}

#' Add density curve
#' @param bw bla
#' @param adjust bla
#' @param kernel bla
#' @param n bla
#' @param color bla
#' @param fill bla
#' @param alpha bla
#' @param ... bla
#' @inheritParams general_arguments
#' @export
add_density_curve <- function(plot, bw = "nrd0", adjust = 1, kernel = "gaussian", n = 512, color = "#E37D46", fill = "#E37D46", alpha = 0.3, ...) {
  check_tidyplot(plot)
  plot %>%
    remove_padding(force_continuous = TRUE) +
    ggplot2::geom_density(bw = bw, adjust = adjust, kernel = kernel, n = n, color = color, fill = fill, alpha = alpha, ...)
}


#' Add heatmap
#' @param scale bla
#' @param rotate_labels bla
#' @param ... bla
#' @inheritParams general_arguments
#' @export
add_heatmap <- function(plot, scale = c("none", "row", "column"), rotate_labels = 90,
                        rasterize = FALSE, rasterize_dpi = 300, ...) {
  check_tidyplot(plot)
  mapping <- NULL
  scale <- match.arg(scale)

  if (scale %in% c("row", "column")) {
    color <- get_variable(plot, "colour")
    x <- get_variable(plot, "x")
    y <- get_variable(plot, "y")
    out <-
      plot$data %>%
      dplyr::mutate(row_zscore = (.data[[color]] - mean(.data[[color]])) / sd(.data[[color]]), .by = .data[[y]]) %>%
      dplyr::mutate(col_zscore = (.data[[color]] - mean(.data[[color]])) / sd(.data[[color]]), .by = .data[[x]])
    plot <- plot %+% out
    if (scale == "row")
      mapping <- ggplot2::aes(fill = row_zscore)
    if (scale == "column")
      mapping <- ggplot2::aes(fill = col_zscore)
  }

  plot <-
    plot %>%
    adjust_x_axis(rotate_labels = rotate_labels) %>%
    remove_x_axis_line() %>%
    remove_y_axis_line() +
    ggplot2::coord_cartesian(expand = FALSE)

  if (scale %in% c("row", "column"))
    plot <- plot %>% adjust_colors(c("blue", "white", "red"))

  add_geom(plot, ggplot2::geom_raster(mapping = mapping, ...),
           rasterize = rasterize, rasterize_dpi = rasterize_dpi)
}

#' Add plot title or caption
#' @param title bla
#' @param caption bla
#' @inheritParams general_arguments
#' @export
add_title <- function(plot, title = ggplot2::waiver()) {
  check_tidyplot(plot)
  # parse title
  if (!is_waiver(title)) title <- tidyplot_parser(as.character(title))
  plot + ggplot2::labs(title = title)
}
#' @rdname add_title
#' @export
add_caption <- function(plot, caption = ggplot2::waiver()) {
  check_tidyplot(plot)
  # parse caption
  if (!is_waiver(caption)) caption <- tidyplot_parser(as.character(caption))
  plot + ggplot2::labs(caption = caption)
}


#' Add reference lines
#' @param x bla
#' @param y bla
#' @param linetype bla
#' @param linewidth bla
#' @param ... bla
#' @inheritParams general_arguments
#' @export
add_reference_lines <- function(plot, x = NULL, y = NULL, linetype = "dashed", linewidth = 0.25, ...) {
  check_tidyplot(plot)
  out <- plot
  if(!is.null(x)) {
    out <- out + ggplot2::geom_vline(xintercept = x, linetype = linetype, linewidth = linewidth, ...)
  }
  if(!is.null(y)) {
    out <- out + ggplot2::geom_hline(yintercept = y, linetype = linetype, linewidth = linewidth, ...)
  }
  out
}


#' Add text labels
#' @param var bla
#' @param fontsize bla
#' @param segment.size bla
#' @param box.padding bla
#' @param ... bla
#' @inheritParams general_arguments
#' @export
add_text_labels <- function(plot, var, data = all_rows(), fontsize = 7,
                     segment.size = 0.2, box.padding = 0.2, ...) {
  check_tidyplot(plot)
  size <- fontsize/ggplot2::.pt
  plot + ggrepel::geom_text_repel(data = data, ggplot2::aes(label = {{var}}), size = size,
                                segment.size = segment.size, box.padding = box.padding, ...)
}


#' Add ggplot2 code to your tidyplot
#'
#' @examples
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add(ggplot2::geom_point())
#'
#' @export
add <- .Primitive("+")

# not exported
add_geom <- function(plot, geom, rasterize = FALSE, rasterize_dpi = 300, level = 0) {
  pf <- parent_function(level = level)
  if (rasterize) {
    cli::cli_alert_success("{pf}: {.pkg rasterized} at {rasterize_dpi} dpi")
    plot + ggrastr::rasterise(geom, dpi = rasterize_dpi, dev = "ragg")
  } else {
    plot + geom
  }
}
