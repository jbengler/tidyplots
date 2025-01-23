#' Add heatmap
#' @param scale Whether to compute row z scores for `"row"` or `"column"`. Defaults to `"none"`.
#' @param rotate_labels Degree to rotate the x-axis labels. Defaults to `90`.
#' @inherit common_arguments
#'
#' @details
#' * `add_heatmap()` supports rasterization. See examples and [Advanced plotting](https://jbengler.github.io/tidyplots/articles/Advanced-plotting.html#rasterization).
#'
#' @examples
#' climate |>
#'   tidyplot(x = month, y = year, color = max_temperature) |>
#'   add_heatmap()
#'
#' # Calculate row-wise z score
#' climate |>
#'   tidyplot(x = month, y = year, color = max_temperature) |>
#'   add_heatmap(scale = "row")
#'
#' # Calculate column-wise z score
#' climate |>
#'   tidyplot(x = month, y = year, color = max_temperature) |>
#'   add_heatmap(scale = "column")
#'
#' # Rasterize heatmap
#' climate |>
#'   tidyplot(x = month, y = year, color = max_temperature) |>
#'   add_heatmap(rasterize = TRUE, rasterize_dpi = 20)
#'
#' @export
add_heatmap <- function(plot, scale = c("none", "row", "column"), rotate_labels = 90,
                        rasterize = FALSE, rasterize_dpi = 300, ...) {
  plot <- check_tidyplot(plot)
  mapping <- NULL
  scale <- match.arg(scale)

  if (scale %in% c("row", "column")) {
    color <- get_variable(plot, "colour")
    x <- get_variable(plot, "x")
    y <- get_variable(plot, "y")
    out <-
      plot$data |>
      dplyr::mutate(row_zscore = (.data[[color]] - mean(.data[[color]])) / sd(.data[[color]]), .by = tidyselect::all_of(y)) |>
      dplyr::mutate(col_zscore = (.data[[color]] - mean(.data[[color]])) / sd(.data[[color]]), .by = tidyselect::all_of(x))
    plot <- plot %+% out
    if (scale == "row")
      mapping <- ggplot2::aes(fill = row_zscore)
    if (scale == "column")
      mapping <- ggplot2::aes(fill = col_zscore)
  }

  plot <-
    plot |>
    adjust_x_axis(rotate_labels = rotate_labels) |>
    remove_x_axis_line() |>
    remove_y_axis_line() +
    ggplot2::coord_cartesian(expand = FALSE)

  plot <- add_geom(plot, ggplot2::geom_raster(mapping = mapping, ...),
           rasterize = rasterize, rasterize_dpi = rasterize_dpi)

  if (scale %in% c("row", "column")) {
    plot <- plot |> adjust_colors(c("blue", "white", "red")) |>
      adjust_legend_title(color)
  }
  plot
}
