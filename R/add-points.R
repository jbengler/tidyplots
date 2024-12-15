#' Add data points
#'
#' @param white_border Whether to include a white border around data points. Defaults to `FALSE`.
#' @param jitter_width Amount of random noise to be added to the
#'  horizontal position of the of the data points. This can be useful to deal
#'  with overplotting. Typical values range between `0` and `1`.
#' @param jitter_height Amount of random noise to be added to the
#'  vertical position of the of the data points. This can be useful to deal
#'  with overplotting. Typical values range between `0` and `1`.
#' @inherit common_arguments
#' @inheritParams ggbeeswarm::geom_beeswarm
#'
#' @details
#' * `add_data_points_beeswarm()` is based on `ggbeeswarm::geom_beeswarm()`.
#' Check there for additional arguments.
#'
#' * `add_data_points()` and friends support rasterization. See examples and [Advanced plotting](https://jbengler.github.io/tidyplots/articles/Advanced-plotting.html#rasterization).
#'
#' * `add_data_points()` and friends support data subsetting. See examples and [Advanced plotting](https://jbengler.github.io/tidyplots/articles/Advanced-plotting.html#data-subsetting).
#'
#' @examples
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_data_points()
#'
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_data_points_jitter()
#'
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_data_points_beeswarm()
#'
#' # Changing arguments
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_data_points_jitter(jitter_width = 1)
#'
#' animals %>%
#'   tidyplot(x = weight, y = size) %>%
#'   add_data_points(white_border = TRUE)
#'
#' animals %>%
#'   tidyplot(x = weight, y = size) %>%
#'   add_data_points(alpha = 0.4)
#'
#' # Rasterization
#' animals %>%
#'   tidyplot(x = weight, y = size) %>%
#'   add_data_points(rasterize = TRUE, rasterize_dpi = 50)
#'
#' # Data subsetting
#' animals %>%
#'   tidyplot(x = weight, y = size) %>%
#'   add_data_points() %>%
#'   add_data_points(data = filter_rows(size > 300), color = "red")
#'
#' @export
add_data_points <- function(plot, data = all_rows(),
                            shape = 19, size = 1, white_border = FALSE,
                            dodge_width = NULL,
                            preserve = "total",
                            rasterize = FALSE, rasterize_dpi = 300, ...) {
  plot <- check_tidyplot(plot)
  f_points(plot = plot, data = data,
           shape = shape, size = size, white_border = white_border,
           dodge_width = dodge_width,
           preserve = preserve,
           rasterize = rasterize, rasterize_dpi = rasterize_dpi, ...)
}
#' @rdname add_data_points
#' @export
add_data_points_jitter <- function(plot, data = all_rows(),
                                   shape = 19, size = 1, white_border = FALSE,
                                   dodge_width = NULL,
                                   jitter_width = 0.2, jitter_height = 0, preserve = "total",
                                   rasterize = FALSE, rasterize_dpi = 300, ...) {
  plot <- check_tidyplot(plot)
  f_points(plot = plot, data = data,
           shape = shape, size = size, white_border = white_border,
           dodge_width = dodge_width,
           jitter_width = jitter_width, jitter_height = jitter_height, preserve = preserve,
           rasterize = rasterize, rasterize_dpi = rasterize_dpi, ...)
}
#' @rdname add_data_points
#' @export
add_data_points_beeswarm <- function(plot, data = all_rows(),
                                     shape = 19, size = 1, white_border = FALSE,
                                     cex = 3, corral = "wrap", corral.width = 0.5,
                                     dodge_width = NULL,
                                     preserve = "total",
                                     rasterize = FALSE, rasterize_dpi = 300, ...) {
  plot <- check_tidyplot(plot)
  f_points(beeswarm = TRUE,
           plot = plot, data = data,
           shape = shape, size = size, white_border = white_border,
           cex = cex, corral = corral, corral.width = corral.width,
           dodge_width = dodge_width,
           preserve = preserve,
           rasterize = rasterize, rasterize_dpi = rasterize_dpi, ...)
}
## Points function
f_points <- function(plot, data = all_rows(),
                     shape = 19, size = 1, white_border = FALSE, beeswarm = FALSE,
                     cex = 3, corral = "wrap", corral.width = 0.5,
                     dodge_width = NULL,
                     jitter_width = 0, jitter_height = 0, preserve = "total",
                     rasterize = FALSE, rasterize_dpi = 300, ...) {

  dodge_width <- dodge_width %||% plot$tidyplot$dodge_width
  if (dodge_width == 0) {
    position <- ggplot2::position_identity()
  } else {
    if (jitter_width == 0 && jitter_height == 0)
      position <- ggplot2::position_dodge(width = dodge_width, preserve = preserve)
    else
      position <- ggplot2::position_jitterdodge(jitter.width = jitter_width,
                                                jitter.height = jitter_height,
                                                dodge.width = dodge_width,
                                                seed = 42)
  }

  if (white_border) {
    size <- size * 1.5
    shape = 21
  }

  if (beeswarm) {
    if (white_border) {
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
    if (white_border) {
      add_geom(plot, ggplot2::geom_point(data = data, size = size, shape = shape, position = position, color = "#FFFFFF", ...),
               rasterize = rasterize, rasterize_dpi = rasterize_dpi, level = -1)
    } else {
      add_geom(plot, ggplot2::geom_point(data = data, size = size, shape = shape, position = position, ...),
               rasterize = rasterize, rasterize_dpi = rasterize_dpi, level = -1)
    }
  }
}
