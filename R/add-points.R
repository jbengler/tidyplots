#' Add data points
#'
#' @param white_border Whether to include a white border around data points. Defaults to `FALSE`.
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
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_data_points()
#'
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_data_points_jitter()
#'
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_data_points_beeswarm()
#'
#' # Changing arguments
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_data_points_jitter(jitter_width = 1)
#'
#' animals |>
#'   tidyplot(x = weight, y = size) |>
#'   add_data_points(white_border = TRUE)
#'
#' animals |>
#'   tidyplot(x = weight, y = size) |>
#'   add_data_points(alpha = 0.4)
#'
#' # Rasterization
#' animals |>
#'   tidyplot(x = weight, y = size) |>
#'   add_data_points(rasterize = TRUE, rasterize_dpi = 50)
#'
#' # Data subsetting
#' animals |>
#'   tidyplot(x = weight, y = size) |>
#'   add_data_points() |>
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
  position <- compute_position(plot = plot,
                               dodge_width = dodge_width,
                               jitter_width = jitter_width,
                               jitter_height = jitter_height,
                               preserve = preserve)
  params <- list(data = data, shape = shape, size = size, ...)

  if (beeswarm) {
    fun <- ggbeeswarm::geom_beeswarm
    params$dodge.width <- dodge_width
    params$cex <- cex
    params$corral <- corral
    params$corral.width <- corral.width
  } else {
    fun <- ggplot2::geom_point
    params$position <- position
  }

  if (white_border) {
    params$shape <- shape_converter(params$shape)
    if (params$shape %in% 21:24) {
      params$size <- params$size * 1.5
      params$color <- "#FFFFFF"
    }
  }

  # Allow shape aesthetic to override shape
  if("shape" %in% names(plot$mapping)) params$shape <- NULL
  # Allow size aesthetic to override size
  if("size" %in% names(plot$mapping)) params$size <- NULL

  geom <- do.call(fun, params)
  add_geom(plot, geom, rasterize = rasterize, rasterize_dpi = rasterize_dpi, level = -1)
}

# not exported
shape_converter <-  function(x) {
  dict <- c("0" = 22, "15" = 22,
            "1" = 21, "16" = 21, "19" = 21, "20" = 21,
            "2" = 24, "17" = 24,
            "5" = 23, "18" = 23)
  if (as.character(x) %in% names(dict)) {
    dict[as.character(x)]
  } else {
    x
  }
}

compute_position <- function(plot, dodge_width, jitter_width, jitter_height, preserve) {
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
  position
}

