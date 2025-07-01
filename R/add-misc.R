#' Add boxplot
#'
#' @param show_whiskers Whether to show boxplot whiskers. Defaults to `TRUE`.
#' @param show_outliers Whether to show outliers. Defaults to `TRUE`.
#' @param box_width Width of the boxplot. Defaults to `0.6`.
#' @param whiskers_width Width of the whiskers. Defaults to `0.8`.
#' @param outlier.shape Shape of the outliers. Defaults to `19`.
#' @param outlier.size Size of the outliers. Defaults to `0.5`.
#' @param outlier.alpha Opacity of the outliers. Defaults to `1`.
#' @inherit common_arguments
#' @inheritParams ggplot2::geom_boxplot
#'
#' @examples
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_boxplot()
#'
#' # Changing arguments:
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_boxplot(show_whiskers = FALSE)
#'
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_boxplot(show_outliers = FALSE)
#'
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_boxplot(box_width = 0.2)
#'
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_boxplot(whiskers_width = 0.2)
#'
#' @export
add_boxplot <- function(plot, dodge_width = NULL, alpha = 0.3, saturation = 1, show_whiskers = TRUE, show_outliers = TRUE,
                        box_width = 0.6, whiskers_width = 0.8, outlier.size = 0.5, coef = 1.5,
                        outlier.shape = 19, outlier.alpha = 1, linewidth = 0.25, preserve = "total", ...) {
  plot <- check_tidyplot(plot)
  dodge_width <- dodge_width %||% plot$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width, preserve = preserve)
  if (saturation != 1) {
    plot <- plot |> adjust_colors(saturation = saturation)
  }
  if (show_whiskers == FALSE) {
    coef = 0
    whiskers_width = box_width
  }
  plot +
    ggplot2::geom_boxplot(alpha = alpha, staplewidth = whiskers_width, outliers = show_outliers,
                          outlier.shape = outlier.shape, outlier.alpha = outlier.alpha, outlier.size = outlier.size,
                          width = box_width, position = position, linewidth = linewidth, coef = coef, ...)
}

# boxplot median not the same as violin draw_quantiles = c(0.5)!
# https://stackoverflow.com/questions/36033341/differing-quantiles-boxplot-vs-violinplot

#' Add violin plot
#'
#' @inherit common_arguments
#' @inheritParams ggplot2::geom_violin
#'
#' @examples
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_violin()
#'
#' # Changing arguments:
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_violin(saturation = 0.6)
#'
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_violin(draw_quantiles = c(0.25, 0.5, 0.75))
#'
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_violin(trim = TRUE)
#'
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_violin(linewidth = 1)
#'
#' @export
add_violin <- function(plot, dodge_width = NULL, alpha = 0.3, saturation = 1, trim = FALSE,
                       linewidth = 0.25, scale = "width", ...) {
  plot <- check_tidyplot(plot)
  dodge_width <- dodge_width %||% plot$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width)
  if (saturation != 1) {
    plot <- plot |> adjust_colors(saturation = saturation)
  }
  plot + ggplot2::geom_violin(alpha = alpha, trim = trim, linewidth = linewidth,
                              scale = scale, position = position, ...)
}

#' Add line or area
#'
#' `add_line()` and `add_area()` connect individual data points, which is rarely needed.
#' In most cases, you are probably looking for `add_sum_line()`, `add_mean_line()`, `add_sum_area()` or `add_mean_area()`.
#'
#' @inherit common_arguments
#'
#' @examples
#' # Paired data points
#' study |>
#'   tidyplot(x = treatment, y = score, color = group) |>
#'   reorder_x_axis_labels("A", "C", "B", "D") |>
#'   add_data_points() |>
#'   add_line(group = participant, color = "grey")
#'
#' study |>
#'   tidyplot(x = treatment, y = score) |>
#'   reorder_x_axis_labels("A", "C", "B", "D") |>
#'   add_data_points() |>
#'   add_area(group = participant)
#'
#' @export
add_line <- function(plot, group, dodge_width = NULL, linewidth = 0.25, preserve = "total", ...) {
  plot <- check_tidyplot(plot)
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
add_area <- function(plot, group, dodge_width = NULL, linewidth = 0.25, alpha = 0.4, preserve = "total", ...) {
  plot <- check_tidyplot(plot)
  ptype <- get_plottype(plot)

  # detect orientation
  orientation <- NA
  if (ptype %in% c("_d", "cd", "ct")) {
    orientation <- "y"
  }
  # add orientation to args if not already present
  args <- list(...)
  if (!"orientation" %in% names(args)) args$orientation <- orientation

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
  plot <- plot +
    rlang::inject(ggplot2::geom_area(mapping = mapping, linewidth = linewidth, alpha = alpha, position = position, !!!args))

  # remove padding between area and axis
  if (is_flipped(plot)) {
    plot <- plot |> adjust_x_axis(padding = c(0, NA), force_continuous = TRUE)
  } else {
    plot <- plot |> adjust_y_axis(padding = c(0, NA), force_continuous = TRUE)
  }
  plot
}


#' Add curve fit
#' @inherit common_arguments
#' @param ... Arguments passed on to `ggplot2::geom_smooth()`.
#' @inheritParams ggplot2::geom_smooth
#'
#' @examples
#' time_course |>
#'   tidyplot(x = day, y = score, color = treatment) |>
#'   add_curve_fit()
#'
#' # Changing arguments
#' time_course |>
#'   tidyplot(x = day, y = score, color = treatment) |>
#'   add_curve_fit(linewidth = 1)
#'
#' time_course |>
#'   tidyplot(x = day, y = score, color = treatment) |>
#'   add_curve_fit(alpha = 0.8)
#'
#' # Remove confidence interval
#' time_course |>
#'   tidyplot(x = day, y = score, color = treatment) |>
#'   add_curve_fit(se = FALSE)
#'
#' @export
add_curve_fit <- function(plot, dodge_width = NULL, method = "loess", linewidth = 0.25, alpha = 0.4,
                          preserve = "total", ...) {
  plot <- check_tidyplot(plot)
  mapping <- ggplot2::aes()
  mapping$group <- plot$mapping$colour
  dodge_width <- dodge_width %||% plot$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width, preserve = preserve)
    plot + ggplot2::geom_smooth(mapping = mapping, method = method, linewidth = linewidth,
                                alpha = alpha, position = position, ...)
}


#' Add histogram
#' @inherit common_arguments
#' @inheritParams ggplot2::geom_histogram
#'
#' @examples
#' energy |>
#'   tidyplot(x = energy) |>
#'   add_histogram()
#'
#' energy |>
#'   tidyplot(x = energy, color = energy_type) |>
#'   add_histogram()
#'
#' @export
add_histogram <- function(plot, binwidth = NULL, bins = NULL, ...) {
  args <- list(binwidth = binwidth, bins = bins, ...)
  if (!is.null(args$color)) args$fill <- args$color
  args$color <- NA

  plot <- check_tidyplot(plot)
  plot <-
    plot +
    do.call(ggplot2::geom_histogram, args)
  # remove padding between bar and axis
  if (is_flipped(plot)) {
    plot <- plot |> adjust_x_axis(padding = c(0, NA), force_continuous = TRUE)
  } else {
    plot <- plot |> adjust_y_axis(padding = c(0, NA), force_continuous = TRUE)
  }
  plot
}


#' Add ggplot2 code to a tidyplot
#'
#' @return A `tidyplot` object.
#'
#' @examples
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add(ggplot2::geom_point())
#'
#' @export
add <- .Primitive("+")


#' Add ellipse
#' @inherit common_arguments
#' @param ... Arguments passed on to `ggplot2::stat_ellipse()`.
#'
#' @examples
#' pca |>
#'   tidyplot(x = pc1, y = pc2, color = group) |>
#'   add_data_points() |>
#'   add_ellipse()
#'
#' pca |>
#'   tidyplot(x = pc1, y = pc2, color = group) |>
#'   add_data_points() |>
#'   add_ellipse(level = 0.75)
#'
#' pca |>
#'   tidyplot(x = pc1, y = pc2, color = group) |>
#'   add_data_points() |>
#'   add_ellipse(type = "norm")
#'
#' @export
add_ellipse <- function(plot, ...) {
  plot <- check_tidyplot(plot)
  plot <-
    plot + ggplot2::stat_ellipse(...)
  plot
}


# not exported
add_geom <- function(plot, geom, rasterize = FALSE, rasterize_dpi = 300, level = 0) {
  pf <- parent_function(level = level)
  if (rasterize) {
    # cli::cli_alert_success("{pf}: {.pkg rasterized} at {rasterize_dpi} dpi")
    plot + ggrastr::rasterise(geom, dpi = rasterize_dpi, dev = "ragg")
  } else {
    plot + geom
  }
}
