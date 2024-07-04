#' Add boxplot
#'
#' @param show_whiskers bla
#' @param show_outliers bla
#' @param box_width bla
#' @param whiskers_width bla
#' @param outlier.shape bla
#' @param outlier.size bla
#' @inherit common_arguments
#' @inheritParams ggplot2::geom_boxplot
#'
#' @export
add_boxplot <- function(plot, dodge_width = NULL, saturation = 0.3, show_whiskers = TRUE, show_outliers = TRUE,
                        box_width = 0.6, whiskers_width = 0.8, outlier.size = 0.5, coef = 1.5,
                        outlier.shape = 19, linewidth = 0.25, preserve = "total", ...) {
  check_tidyplot(plot)
  dodge_width <- dodge_width %||% plot$tidyplot$dodge_width
  position <- ggplot2::position_dodge(width = dodge_width, preserve = preserve)
  plot <- plot %>% adjust_colors(saturation = saturation)
  if (show_whiskers == FALSE) {
    coef = 0
    whiskers_width = box_width
  }
  # plot +
  #   ggplot2::stat_boxplot(geom ='errorbar', width = whiskers_width, position = position,
  #                         linewidth = linewidth, coef = coef) +
  #   ggplot2::geom_boxplot(outliers = show_outliers, outlier.shape = outlier.shape, outlier.size = outlier.size,
  #                         width = box_width, position = position, linewidth = linewidth, coef = coef, ...)
  # with staplewidth
  plot +
    ggplot2::geom_boxplot(staplewidth = whiskers_width, outliers = show_outliers, outlier.shape = outlier.shape, outlier.size = outlier.size,
                          width = box_width, position = position, linewidth = linewidth, coef = coef, ...)
}

# boxplot median not the same as violin draw_quantiles = c(0.5)!
# https://stackoverflow.com/questions/36033341/differing-quantiles-boxplot-vs-violinplot

#' Add violin plot
#'
#' @inherit common_arguments
#' @inheritParams ggplot2::geom_violin
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

#' Add line or area
#' @inherit common_arguments
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
add_area <- function(plot, group, dodge_width = NULL, linewidth = 0.25, alpha = 0.3, preserve = "total", ...) {
  check_tidyplot(plot)
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
#' @inheritParams ggplot2::geom_smooth
#' @export
add_curve_fit <- function(plot, dodge_width = NULL, method = "loess", linewidth = 0.25, alpha = 0.3,
                          preserve = "total", ...) {
  check_tidyplot(plot)
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
#' @export
add_histogram <- function(plot, binwidth = NULL, bins = NULL, color = "#4DACD6", ...) {
  check_tidyplot(plot)
  plot %>%
    remove_plot_area_padding(force_continuous = TRUE) +
    ggplot2::geom_histogram(binwidth = binwidth, bins = bins, fill = color, ...)
}
#' @rdname add_histogram
#' @export
add_density_histogram <- function(plot, binwidth = NULL, bins = NULL, color = "#4DACD6", ...) {
  check_tidyplot(plot)
  plot %>%
    remove_plot_area_padding(force_continuous = TRUE) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            binwidth = binwidth,bins = bins, fill = color, ...)
}

#' Add density curve
#' @inherit common_arguments
#' @inheritParams ggplot2::geom_density
#' @export
add_density_curve <- function(plot, bw = "nrd0", adjust = 1, kernel = "gaussian", n = 512, color = "#E37D46", fill = "#E37D46", alpha = 0.3, ...) {
  check_tidyplot(plot)
  plot %>%
    remove_plot_area_padding(force_continuous = TRUE) +
    ggplot2::geom_density(bw = bw, adjust = adjust, kernel = kernel, n = n, color = color, fill = fill, alpha = alpha, ...)
}


#' Add plot title or caption
#' @param title bla
#' @param caption bla
#' @inherit common_arguments
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
#' @param linetype Either an integer (0-6) or a name (0 = blank, 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash).
#' @inherit common_arguments
#' @inheritParams ggplot2::geom_vline
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
#' @param segment.size bla
#' @inherit common_arguments
#' @inheritParams ggrepel::geom_text_repel
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
