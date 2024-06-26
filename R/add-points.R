#' Add data points
#'
#' @param confetti If `TRUE` data points get a white border. This can be
#'  useful to deal with overplotting.
#' @param jitter_width Amount of random noise to be added to the
#'  horizontal position of the of the data points. This can be useful to deal
#'  with overplotting. Typical values range between `0` and `1`.
#' @param jitter_height Amount of random noise to be added to the
#'  vertical position of the of the data points. This can be useful to deal
#'  with overplotting. Typical values range between `0` and `1`.
#' @inherit common_arguments
#' @inheritParams ggbeeswarm::geom_beeswarm
#'
#' @examples
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_data_points()
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_data_points_jitter()
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
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
                                                dodge.width = dodge_width,
                                                seed = 42)
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
