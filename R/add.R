
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

#' Add `ggplot2` code to your tidyplot
#'
#' @examples
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add(ggplot2::geom_point())
#'
#' @export
add <- .Primitive("+")


#' Add data points
#'
#' @param gg bla
#' @param data bla
#' @param dodge_width bla
#' @param jitter_width bla
#' @param jitter_height bla
#' @param size bla
#' @param shape bla
#' @param color bla
#' @param rasterize bla
#' @param rasterize_dpi bla
#' @param preserve bla
#' @param style bla
#' @param ... bla
#' @export
add_points <- function(gg, style = c("point", "circle", "confetti"), data = all_rows(),
                       color = NULL, size = 1,
                       dodge_width = NULL,
                       jitter_width = 0, jitter_height = 0,
                       shape = NULL,
                       preserve = "total",
                       rasterize = FALSE, rasterize_dpi = 300, ...) {
  style <- match.arg(style)

  if (is_discrete(gg, "x")) {
    dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
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

  if (is.null(shape)) {
    use_style <- TRUE
    if (style == "point") shape <- 19
    if (style == "circle") shape <- 1
    if (style == "confetti"){
      shape <- 21
      size <- size * 1.5
      fill <- color
      color <- "#FFFFFF"
    }
  }

  if (style == "confetti" && use_style) {
    if (is.null(fill)) {
      add_geom(gg, ggplot2::geom_point(data = data, size = size, shape = shape, position = position, color = "#FFFFFF", ...),
               rasterize = rasterize, rasterize_dpi = rasterize_dpi)
    } else {
      add_geom(gg, ggplot2::geom_point(data = data, size = size, shape = shape, position = position, color = "#FFFFFF", fill = fill, ...),
               rasterize = rasterize, rasterize_dpi = rasterize_dpi)
    }
  } else {
    if (is.null(color)) {
      add_geom(gg, ggplot2::geom_point(data = data, size = size, shape = shape, position = position, ...),
               rasterize = rasterize, rasterize_dpi = rasterize_dpi)
    } else {
      add_geom(gg, ggplot2::geom_point(data = data, size = size, shape = shape, position = position, color = color, ...),
               rasterize = rasterize, rasterize_dpi = rasterize_dpi)
    }
  }
}
#' @rdname add_points
#' @export
add_jitter <- function(gg, style = c("point", "circle", "confetti"), data = all_rows(),
                       size = 1,
                       dodge_width = NULL,
                       jitter_width = 0.2, jitter_height = 0,
                       shape = NULL,
                       preserve = "total",
                       rasterize = FALSE, rasterize_dpi = 300, ...) {
  style <- match.arg(style)
  add_points(gg = gg, data = data, dodge_width = dodge_width,
             jitter_width = jitter_width, jitter_height = jitter_height,
             size = size, shape = shape, style = style,
             preserve = preserve, rasterize = rasterize, rasterize_dpi = rasterize_dpi, ...)
}


## Error bar function factory
ff_errorbar <- function(.fun.data) {
  function(gg, dodge_width = NULL, width = 0.4, linewidth = 0.25, preserve = "total", ...) {
    dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
    position <- ggplot2::position_dodge(width = dodge_width, preserve = preserve)
    gg + ggplot2::stat_summary(fun.data = .fun.data, geom = "errorbar",
                               linewidth = linewidth, width = width, position = position, ...)
  }
}
#' Add error bar
#'
#' @param gg bla
#' @param dodge_width bla
#' @param width bla
#' @param linewidth bla
#' @param preserve bla
#' @param ... bla
#' @export
add_error <- ff_errorbar(.fun.data = ggplot2::mean_se)
#' @rdname add_error
#' @export
add_range <- ff_errorbar(.fun.data = min_max)
#' @rdname add_error
#' @export
add_sd <- ff_errorbar(.fun.data = ggplot2::mean_sdl)
#' @rdname add_error
#' @export
add_ci95 <- ff_errorbar(.fun.data = ggplot2::mean_cl_boot)


## Ribbon function factory
ff_ribbon <- function(.fun.data) {
  function(gg, dodge_width = NULL, alpha = 0.3, color = NA, ...) {
    mapping <- ggplot2::aes()
    mapping$group <- gg$mapping$colour
    dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
    position <- ggplot2::position_dodge(width = dodge_width)
    gg + ggplot2::stat_summary(mapping = mapping, fun.data = .fun.data, geom = "ribbon",
                               alpha = alpha, color = color, position = position, ...)
  }
}
#' Add ribbon
#'
#' @param gg bla
#' @param dodge_width bla
#' @param alpha bla
#' @param color bla
#' @param ... bla
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
  function(gg, dodge_width = NULL, width = 0.6, saturation = 1, preserve = "total", ...) {
    dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
    position <- ggplot2::position_dodge(width = dodge_width, preserve = preserve)
    gg <- gg %>% adjust_colors(saturation = saturation)
    if (.count) {
      gg %>%
        adjust_y_axis(padding_bottom = 0, force_y_continuous = TRUE) +
        ggplot2::stat_count(geom = "bar", color = NA, width = width, position = position, ...)
    } else {
      gg %>%
        adjust_y_axis(padding_bottom = 0) +
        ggplot2::stat_summary(fun = .fun, geom = "bar", color = NA, width = width,
                              position = position, ...)
    }
  }
}
## Dash function factory
ff_dash <- function(.fun, .count = FALSE) {
  function(gg, dodge_width = NULL, width = 0.6, linewidth = 0.25, preserve = "total", ...) {
    dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
    position <- ggplot2::position_dodge(width = dodge_width, preserve = preserve)
    if (.count) {
      gg + ggplot2::geom_errorbar(
        ggplot2::aes(ymin = ggplot2::after_stat(count), ymax = ggplot2::after_stat(count)),
        stat = "count", linewidth = linewidth, width = width, position = position, ...)
    } else {
      gg + ggplot2::stat_summary(fun.min = .fun, fun.max = .fun, geom = "errorbar",
                                 linewidth = linewidth, width = width, position = position, ...)
    }
  }
}
## Dot function factory
ff_dot <- function(.fun, .count = FALSE) {
  function(gg, dodge_width = NULL, size = 2, preserve = "total", ...) {
    dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
    position <- ggplot2::position_dodge(width = dodge_width, preserve = preserve)
    if (.count) {
      gg + ggplot2::stat_count(geom = "point", size = size, position = position, ...)
    } else {
      gg + ggplot2::stat_summary(fun = .fun, geom = "point", size = size, position = position, ...)
    }
  }
}
## Value function factory
ff_value <- function(.fun, .count = FALSE) {
  function(gg, dodge_width = NULL, accuracy = 0.1, scale_cut = NULL, fontsize = 7,
           vjust = -0.5, padding_top = 0.15, preserve = "total", ...) {
    size <- fontsize/ggplot2::.pt
    dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
    position <- ggplot2::position_dodge(width = dodge_width, preserve = preserve)
    if (.count) {
      gg %>% adjust_y_axis(padding_top = padding_top, force_y_continuous = TRUE) +
        ggplot2::stat_count(ggplot2::aes(label = format_number(ggplot2::after_stat(count), accuracy = accuracy, scale_cut = scale_cut)),
                            geom = "text", vjust = vjust, size = size, position = position, ...)
    } else {
      gg %>% adjust_y_axis(padding_top = padding_top) +
        ggplot2::stat_summary(ggplot2::aes(label = format_number(ggplot2::after_stat(y), accuracy = accuracy, scale_cut = scale_cut)),
                              fun = .fun, geom = "text", vjust = vjust, size = size, position = position, ...)
    }
  }
}
## Line function factory
ff_line <- function(.fun, .count = FALSE, .geom) {
  function(gg, group, dodge_width = NULL, linewidth = 0.25, preserve = "total", ...) {
    mapping <- NULL
    if (is_missing(gg, "group")) {
      mapping <- ggplot2::aes()
      mapping$group <- gg$mapping$colour
    }
    if (!missing(group)) {
      mapping <- ggplot2::aes(group = {{group}})
    }
    dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
    position <- ggplot2::position_dodge(width = dodge_width, preserve = preserve)
    if (.count) {
      gg + ggplot2::stat_count(mapping = mapping, geom = .geom,
                               linewidth = linewidth, position = position, ...)
    } else {
      gg + ggplot2::stat_summary(mapping = mapping, fun = .fun, geom = .geom,
                                 linewidth = linewidth, position = position, ...)
    }
  }
}


#' Add mean
#'
#' @param gg bla
#' @param dodge_width bla
#' @param width bla
#' @param linewidth bla
#' @param size description
#' @param saturation description
#' @param accuracy description
#' @param fontsize description
#' @param vjust description
#' @param padding_top description
#' @param scale_cut bla
#' @param group bla
#' @param preserve bla
#' @param ... description
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
#' @param gg bla
#' @param dodge_width bla
#' @param width bla
#' @param linewidth bla
#' @param size description
#' @param saturation description
#' @param accuracy description
#' @param fontsize description
#' @param vjust description
#' @param padding_top description
#' @param scale_cut bla
#' @param group bla
#' @param preserve bla
#' @param ... description
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
#' @param gg bla
#' @param dodge_width bla
#' @param width bla
#' @param linewidth bla
#' @param size description
#' @param saturation description
#' @param accuracy description
#' @param fontsize description
#' @param vjust description
#' @param padding_top description
#' @param scale_cut bla
#' @param group bla
#' @param preserve bla
#' @param ... description
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
#' @param gg bla
#' @param dodge_width bla
#' @param width bla
#' @param linewidth bla
#' @param size bla
#' @param saturation bla
#' @param accuracy bla
#' @param fontsize bla
#' @param vjust bla
#' @param padding_top bla
#' @param group bla
#' @param scale_cut bla
#' @param preserve bla
#' @param ... bla
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
#' @param gg bla
#' @param dodge_width bla
#' @param saturation bla
#' @param show_whiskers bla
#' @param show_outliers bla
#' @param box_width bla
#' @param whiskers_width bla
#' @param outlier.size bla
#' @param coef bla
#' @param outlier.shape bla
#' @param linewidth bla
#' @param preserve bla
#' @param ... bla
#'
#' @export
add_box <- function(gg, dodge_width = NULL, saturation = 0.3, show_whiskers = TRUE, show_outliers = FALSE,
                    box_width = 0.6, whiskers_width = 0.5, outlier.size = 0.5, coef = 1.5,
                    outlier.shape = 19, linewidth = 0.25, preserve = "total", ...) {
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width, preserve = preserve)
  gg <- gg %>% adjust_colors(saturation = saturation)
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
#' @param saturation bla
#' @param draw_quantiles bla
#' @param trim bla
#' @param linewidth bla
#' @param scale bla
#' @param ... bla
#'
#' @export
add_violin <- function(gg, dodge_width = NULL, saturation = 0.3, draw_quantiles = NULL, trim = FALSE,
                       linewidth = 0.25, scale = "width", ...) {
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width)
  gg <- gg %>% adjust_colors(saturation = saturation)
  gg + ggplot2::geom_violin(draw_quantiles = draw_quantiles, trim = trim, linewidth = linewidth,
                            scale = scale, position = position, ...)
}

#' Add line
#' @param gg bla
#' @param group bla
#' @param dodge_width bla
#' @param linewidth bla
#' @param preserve bla
#' @param ... bla
#' @export
add_line <- function(gg, group, dodge_width = NULL, linewidth = 0.25, preserve = "total", ...) {
  mapping <- NULL
  if (is_missing(gg, "group")) {
    mapping <- ggplot2::aes()
    mapping$group <- gg$mapping$colour
  }
  if (!missing(group)) {
    mapping <- ggplot2::aes(group = {{group}})
  }
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width, preserve = preserve)
  gg + ggplot2::geom_line(mapping = mapping, linewidth = linewidth, position = position, ...)
}
#' @rdname add_line
#' @export
add_area <- function(gg, dodge_width = NULL, linewidth = 0.25, preserve = "total", ...) {
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width, preserve = preserve)
  gg %>%
    remove_padding() +
    ggplot2::geom_area(linewidth = linewidth, position = position, ...)
}


#' Add curve
#' @param gg bla
#' @param dodge_width bla
#' @param method bla
#' @param linewidth bla
#' @param alpha bla
#' @param preserve bla
#' @param ... bla
#' @export
add_curve <- function(gg, dodge_width = NULL, method = "loess", linewidth = 0.25, alpha = 0.3,
                      preserve = "total", ...) {
  mapping <- ggplot2::aes()
  mapping$group <- gg$mapping$colour
  dodge_width <- dodge_width %||% gg$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width, preserve = preserve)
  gg + ggplot2::geom_smooth(mapping = mapping, method = method, linewidth = linewidth,
                            alpha = alpha, position = position, ...)
}


## Pie function factory
ff_pie <- function(.type = "pie") {
  function(gg, bar_width = 1, reverse = FALSE, ...) {
    gg <-
      gg %>%
      remove_padding() %>%
      style_void()

    if (is_missing(gg, "y")) {
      gg <- gg + ggplot2::geom_bar(ggplot2::aes(x = NA), position = ggplot2::position_fill(reverse = reverse),
                                   width = bar_width, color = NA, ...)
    } else {
      gg <- gg + ggplot2::stat_summary(ggplot2::aes(x = NA), geom = "bar", fun = sum,
                                       position = ggplot2::position_fill(reverse = reverse),
                                       width = bar_width, color = NA, ...)
    }
    suppressMessages(
      gg <- gg +
        ggplot2::coord_polar("y") +
        ggplot2::guides()
    )
    if (.type == "donut")
      suppressMessages(gg + ggplot2::scale_x_discrete(limits = function(x) c("", "", x)))
    else
      gg
  }
}
#' Add pie chart
#' @param gg bla
#' @param bar_width bla
#' @param reverse bla
#' @param ... bla
#' @export
add_pie <- ff_pie(.type = "pie")
#' @rdname add_pie
#' @export
add_donut <- ff_pie(.type = "donut")


## Barstack function factory
ff_barstack <- function(.position_fun) {
  function(gg, bar_width = 0.8, reverse = FALSE, ...) {
    gg <-
      gg %>%
      adjust_y_axis(padding_bottom = 0, padding_top = 0, force_y_continuous = TRUE)

    mapping <- NULL
    if (is_missing(gg, "x")) mapping <- ggplot2::aes(x = "")

    if (is_missing(gg, "y")){
      gg + ggplot2::geom_bar(mapping = mapping, position = .position_fun(reverse = reverse),
                             width = bar_width, color = NA, ...)
    } else {
      gg + ggplot2::stat_summary(mapping = mapping, geom = "bar", fun = sum,
                                 position = .position_fun(reverse = reverse), width = bar_width,
                                 color = NA, ...)
    }
  }
}
#' Add bar stack
#' @param gg bla
#' @param bar_width bla
#' @param reverse bla
#' @param ... bla
#' @export
add_barstack_absolute <- ff_barstack(.position_fun = ggplot2::position_stack)
#' @rdname add_barstack_absolute
#' @export
add_barstack_relative <- ff_barstack(.position_fun = ggplot2::position_fill)


## Areastack function factory
ff_areastack <- function(.position_fun) {
  function(gg, linewidth = 0.25, alpha = 0.3, reverse = FALSE, ...) {
    gg <-
      gg %>%
      adjust_y_axis(padding_bottom = 0, padding_top = 0, force_y_continuous = TRUE) %>%
      adjust_x_axis(padding_left = 0, padding_right = 0)

    # overwrite group aesthetic
    mapping <- ggplot2::aes()
    mapping$group <- gg$mapping$colour

    if (is_missing(gg, "y")){
      # gg + ggplot2::stat_count(mapping = mapping, geom = "area",
      #                         position = .position_fun(reverse = reverse),
      #                         linewidth = linewidth, alpha = alpha, ...)
      vars <- c(get_variable(gg, "x"), get_variable(gg, "colour"))
      df <-
        gg$data %>%
        dplyr::summarize(count = dplyr::n(), .by = all_of(vars)) %>%
        tidyr::complete(.data[[vars[1]]], .data[[vars[2]]], fill = list(count = 0))
      mapping$y <- ggplot2::aes(y = count)$y
      gg + ggplot2::geom_area(data = df, mapping = mapping,
                              position = .position_fun(reverse = reverse), linewidth = linewidth,
                              alpha = alpha, ...)
    } else {
      # gg + ggplot2::stat_summary(mapping = mapping, geom = "area", fun = sum,
      #                            position = .position_fun(reverse = reverse), alpha = alpha, linewidth = linewidth, ...)
      vars <- c(get_variable(gg, "x"), get_variable(gg, "colour"))
      y_var <- get_variable(gg, "y")
      zero <- list(y_var = 0)
      names(zero) <- y_var
      df <-
        gg$data %>%
        dplyr::summarize("{y_var}" := sum(.data[[y_var]]), .by = all_of(vars)) %>%
        tidyr::complete(.data[[vars[1]]], .data[[vars[2]]], fill = zero)
      gg + ggplot2::geom_area(data = df, mapping = mapping,
                              position = .position_fun(reverse = reverse), linewidth = linewidth,
                              alpha = alpha, ...)
    }
  }
}
#' Add area stack
#' @param gg bla
#' @param linewidth bla
#' @param alpha bla
#' @param reverse bla
#' @param ... bla
#' @export
add_areastack_absolute <- ff_areastack(.position_fun = ggplot2::position_stack)
#' @rdname add_areastack_absolute
#' @export
add_areastack_relative <- ff_areastack(.position_fun = ggplot2::position_fill)


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
#' @param scale bla
#' @param rasterize bla
#' @param rotate_labels bla
#' @param rasterize_dpi bla
#' @param ... bla
#' @export
add_heatmap <- function(gg, scale = c("none", "row", "column"), rotate_labels = 90,
                        rasterize = FALSE, rasterize_dpi = 300, ...) {
  mapping <- NULL
  scale <- match.arg(scale)

  if (scale %in% c("row", "column")) {
    color <- get_variable(gg, "colour")
    x <- get_variable(gg, "x")
    y <- get_variable(gg, "y")
    out <-
      gg$data %>%
      dplyr::mutate(row_zscore = (.data[[color]] - mean(.data[[color]])) / sd(.data[[color]]), .by = .data[[y]]) %>%
      dplyr::mutate(col_zscore = (.data[[color]] - mean(.data[[color]])) / sd(.data[[color]]), .by = .data[[x]])
    gg <- gg %+% out
    if (scale == "row")
      mapping <- ggplot2::aes(fill = row_zscore)
    if (scale == "column")
      mapping <- ggplot2::aes(fill = col_zscore)
  }

  gg <-
    gg %>%
    adjust_x_axis(rotate_labels = rotate_labels) %>%
    remove_x_axis_line() %>%
    remove_y_axis_line() +
    ggplot2::coord_cartesian(expand = FALSE)

  if (scale %in% c("row", "column"))
    gg <- gg %>% adjust_colors(c("blue", "white", "red"), as_palette = TRUE)

  add_geom(gg, ggplot2::geom_raster(mapping = mapping, ...),
           rasterize = rasterize, rasterize_dpi = rasterize_dpi)
}

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
#' @param data bla
#' @param fontsize bla
#' @param segment.size bla
#' @param box.padding bla
#' @param ... bla
#' @export
add_text <- function(gg, var, data = all_rows(), fontsize = 7,
                     segment.size = 0.2, box.padding = 0.2, ...) {
  size <- fontsize/ggplot2::.pt
  gg + ggrepel::geom_text_repel(data = data, ggplot2::aes(label = {{var}}), size = size,
                                segment.size = segment.size, box.padding = box.padding, ...)
}
