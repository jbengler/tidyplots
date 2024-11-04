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
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_boxplot()
#'
#' # Changing arguments:
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_boxplot(show_whiskers = FALSE)
#'
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_boxplot(show_outliers = FALSE)
#'
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_boxplot(box_width = 0.2)
#'
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
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
    plot <- plot %>% adjust_colors(saturation = saturation)
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
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_violin()
#'
#' # Changing arguments:
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_violin(saturation = 0.6)
#'
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_violin(draw_quantiles = c(0.25, 0.5, 0.75))
#'
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_violin(trim = TRUE)
#'
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_violin(linewidth = 1)
#'
#' @export
add_violin <- function(plot, dodge_width = NULL, alpha = 0.3, saturation = 1, draw_quantiles = NULL, trim = FALSE,
                       linewidth = 0.25, scale = "width", ...) {
  plot <- check_tidyplot(plot)
  dodge_width <- dodge_width %||% plot$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width)
  if (saturation != 1) {
    plot <- plot %>% adjust_colors(saturation = saturation)
  }
  plot + ggplot2::geom_violin(alpha = alpha, draw_quantiles = draw_quantiles, trim = trim, linewidth = linewidth,
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
#' study %>%
#'   tidyplot(x = treatment, y = score, color = group) %>%
#'   reorder_x_axis_labels("A", "C", "B", "D") %>%
#'   add_data_points() %>%
#'   add_line(group = participant, color = "grey")
#'
#' study %>%
#'   tidyplot(x = treatment, y = score) %>%
#'   reorder_x_axis_labels("A", "C", "B", "D") %>%
#'   add_data_points() %>%
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
    plot <- plot %>% adjust_x_axis(padding = c(0, NA), force_continuous = TRUE)
  } else {
    plot <- plot %>% adjust_y_axis(padding = c(0, NA), force_continuous = TRUE)
  }
  plot
}


#' Add curve fit
#' @inherit common_arguments
#' @param ... Arguments passed on to `ggplot2::geom_smooth()`.
#' @inheritParams ggplot2::geom_smooth
#'
#' @examples
#' time_course %>%
#'   tidyplot(x = day, y = score, color = treatment, dodge_width = 0) %>%
#'   add_curve_fit()
#'
#' # Changing arguments
#' time_course %>%
#'   tidyplot(x = day, y = score, color = treatment, dodge_width = 0) %>%
#'   add_curve_fit(linewidth = 1)
#'
#' time_course %>%
#'   tidyplot(x = day, y = score, color = treatment, dodge_width = 0) %>%
#'   add_curve_fit(alpha = 0.8)
#'
#' # Remove confidence interval
#' time_course %>%
#'   tidyplot(x = day, y = score, color = treatment, dodge_width = 0) %>%
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
#' energy %>%
#'   tidyplot(x = power) %>%
#'   add_histogram()
#'
#' energy %>%
#'   tidyplot(x = power, color = energy_type) %>%
#'   add_histogram()
#'
#' @export
add_histogram <- function(plot, binwidth = NULL, bins = NULL, ...) {
  plot <- check_tidyplot(plot)
  plot <-
    plot +
    ggplot2::geom_histogram(color = NA, binwidth = binwidth, bins = bins, ...)
  # remove padding between bar and axis
  if (is_flipped(plot)) {
    plot <- plot %>% adjust_x_axis(padding = c(0, NA), force_continuous = TRUE)
  } else {
    plot <- plot %>% adjust_y_axis(padding = c(0, NA), force_continuous = TRUE)
  }
  plot
}


#' Add plot title or caption
#' @param title Title of the plot.
#' @param caption Caption of the plot.
#' @inherit common_arguments
#'
#' @details
#' * `add_title()` and `add_caption()` support [plotmath expressions](https://www.rdocumentation.org/packages/grDevices/versions/3.6.2/topics/plotmath) to include special characters.
#' See examples and [Advanced plotting](https://jbengler.github.io/tidyplots/articles/Advanced-plotting.html#special-characters).
#'
#' @examples
#' study %>%
#'   tidyplot(x = treatment, y = score) %>%
#'   add_data_points_beeswarm() %>%
#'   add_title("This is my title")
#'
#' study %>%
#'   tidyplot(x = treatment, y = score) %>%
#'   add_data_points_beeswarm() %>%
#'   add_caption("This is the fine print in the caption")
#'
#' # Plotmath expression
#' study %>%
#'   tidyplot(x = treatment, y = score) %>%
#'   add_data_points_beeswarm() %>%
#'   add_title("$H[2]*O~and~E==m*c^{2}$")
#'
#' @export
add_title <- function(plot, title = ggplot2::waiver()) {
  plot <- check_tidyplot(plot)
  # parse title
  if (!is_waiver(title)) title <- tidyplot_parser(as.character(title))
  plot + ggplot2::labs(title = title)
}
#' @rdname add_title
#' @export
add_caption <- function(plot, caption = ggplot2::waiver()) {
  plot <- check_tidyplot(plot)
  # parse caption
  if (!is_waiver(caption)) caption <- tidyplot_parser(as.character(caption))
  plot + ggplot2::labs(caption = caption)
}


#' Add reference lines
#' @param x Numeric values where the reference lines should meet the x axis. For example, `x = 4` or `x = c(2,3,4)`.
#' @param y Numeric values where the reference lines should meet the y axis. For example, `y = 4` or `y = c(2,3,4)`.
#' @param linetype Either an integer (0-6) or a name (0 = blank, 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash).
#' @inherit common_arguments
#' @inheritParams ggplot2::geom_vline
#'
#' @examples
#' animals %>%
#'   tidyplot(x = weight, y = speed) %>%
#'    add_reference_lines(x = 4000, y = c(100, 200)) %>%
#'    add_data_points()
#'
#' animals %>%
#'   tidyplot(x = weight, y = speed) %>%
#'    add_reference_lines(x = 4000, y = c(100, 200), linetype = "dotdash") %>%
#'    add_data_points()
#'
#' @export
add_reference_lines <- function(plot, x = NULL, y = NULL, linetype = "dashed", linewidth = 0.25, ...) {
  plot <- check_tidyplot(plot)
  out <- plot
  if(!is.null(x)) {
    out <- out + ggplot2::geom_vline(xintercept = x, linetype = linetype, linewidth = linewidth, ...)
  }
  if(!is.null(y)) {
    out <- out + ggplot2::geom_hline(yintercept = y, linetype = linetype, linewidth = linewidth, ...)
  }
  out
}


#' Add data labels
#' @param label Variable in the dataset to be used for the text label.
#' @param background Whether to include semitransparent background box behind the labels to improve legibility. Defaults to `FALSE`.
#' @param background_color Hex color of the background box. Defaults to `"#FFFFFF"` for white.
#' @param background_alpha Opacity of the background box. Defaults to `0.6`.
#' @param label_position Position of the label in relation to the data point. Can be one of `c("below", "above", "left", "right", "center")`.
#' @param segment.size Thickness of the line connecting the label with the data point. Defaults to `0.2`.
#' @inherit common_arguments
#' @inheritParams ggrepel::geom_label_repel
#'
#' @details
#' * `add_data_labels_repel()` uses `ggrepel::geom_text_repel()`. Check there
#' and in [ggrepel examples](https://ggrepel.slowkow.com/articles/examples) for
#' additional arguments.
#'
#' * `add_data_labels()` and `add_data_labels_repel()` support data subsetting. See [Advanced plotting](https://jbengler.github.io/tidyplots/articles/Advanced-plotting.html#data-subsetting).
#'
#' @examples
#' # Create plot and increase padding to make more space for labels
#' p <-
#'   animals %>%
#'   dplyr::slice_head(n = 5) %>%
#'   tidyplot(x = weight, y = speed) %>%
#'   theme_ggplot2() %>%
#'   add_data_points() %>%
#'   adjust_padding(all = 0.3)
#'
#' # Default label position is `below` the data point
#' p %>% add_data_labels(label = animal)
#'
#' # Alternative label positions
#' p %>% add_data_labels(label = animal, label_position = "above")
#'
#' p %>% add_data_labels(label = animal, label_position = "right")
#'
#' p %>% add_data_labels(label = animal, label_position = "left")
#'
#' # Include white background box
#' p %>% add_data_labels(label = animal, background = TRUE)
#'
#' p %>% add_data_labels(label = animal, background = TRUE,
#'   background_color = "pink")
#'
#' # Black labels
#' p %>% add_data_labels(label = animal, color = "black")
#'
#' # Use repelling data labels
#' p %>% add_data_labels_repel(label = animal, color = "black")
#'
#' p %>% add_data_labels_repel(label = animal, color = "black",
#'   background = TRUE)
#'
#' p %>% add_data_labels_repel(label = animal, color = "black",
#'   background = TRUE, min.segment.length = 0)
#'
#' @export
add_data_labels <- function(plot, label, data = all_rows(), fontsize = 7,
                            background = FALSE, background_color = "#FFFFFF", background_alpha = 0.6,
                            label_position = c("below", "above", "left", "right", "center"), ...) {
  plot <- check_tidyplot(plot)
  size <- fontsize/ggplot2::.pt
  if (!background) background_alpha <- 0
  label.padding <- ggplot2::unit(0.1, "lines")

  label_position <- match.arg(label_position)
  if (label_position == "right") {
    vjust <- 0.5
    hjust <- -0.07
  }
  if (label_position == "left") {
    vjust <- 0.5
    hjust <- 1.07
  }
  if (label_position == "below") {
    vjust <- 1.2
    hjust <- 0.5
  }
  if (label_position == "above") {
    vjust <- -0.4
    hjust <- 0.5
  }
  if (label_position == "center") {
    vjust <- 0.5
    hjust <- 0.5
  }

    plot +
      ggplot2::geom_label(data = data, ggplot2::aes(label = {{label}}), size = size,
                          fill = scales::alpha(background_color, background_alpha),
                          vjust = vjust, hjust = hjust, label.size = NA, label.padding = label.padding, ...)
}
#' @rdname add_data_labels
#' @export
add_data_labels_repel <- function(plot, label, data = all_rows(), fontsize = 7,
                                  segment.size = 0.2, box.padding = 0.2, max.overlaps = Inf,
                                  background = FALSE, background_color = "#FFFFFF", background_alpha = 0.6, ...) {
  plot <- check_tidyplot(plot)
  size <- fontsize/ggplot2::.pt
  if (!background) background_alpha <- 0
  label.padding <- ggplot2::unit(0.1, "lines")

  plot + ggrepel::geom_label_repel(data = data, ggplot2::aes(label = {{label}}), size = size,
                                  segment.size = segment.size, box.padding = box.padding, max.overlaps = max.overlaps,
                                  fill = scales::alpha(background_color, background_alpha),
                                  label.size = NA, label.padding = label.padding, ...)
}


#' Add ggplot2 code to a tidyplot
#'
#' @return A `tidyplot` object.
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
    # cli::cli_alert_success("{pf}: {.pkg rasterized} at {rasterize_dpi} dpi")
    plot + ggrastr::rasterise(geom, dpi = rasterize_dpi, dev = "ragg")
  } else {
    plot + geom
  }
}
