#' Add plot title or caption
#'
#' @param title Title of the plot.
#' @param caption Caption of the plot.
#' @inherit common_arguments
#'
#' @details
#' * `add_title()` and `add_caption()` support [plotmath expressions](https://www.rdocumentation.org/packages/grDevices/versions/3.6.2/topics/plotmath) to include special characters.
#' See examples and [Advanced plotting](https://jbengler.github.io/tidyplots/articles/Advanced-plotting.html#special-characters).
#'
#' @examples
#' study |>
#'   tidyplot(x = treatment, y = score) |>
#'   add_data_points_beeswarm() |>
#'   add_title("This is my title")
#'
#' study |>
#'   tidyplot(x = treatment, y = score) |>
#'   add_data_points_beeswarm() |>
#'   add_caption("This is the fine print in the caption")
#'
#' # Plotmath expression
#' study |>
#'   tidyplot(x = treatment, y = score) |>
#'   add_data_points_beeswarm() |>
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
#' @param x Numeric values where the reference lines should meet the x-axis. For example, `x = 4` or `x = c(2,3,4)`.
#' @param y Numeric values where the reference lines should meet the y-axis. For example, `y = 4` or `y = c(2,3,4)`.
#' @param linetype Either an integer (0-6) or a name (0 = blank, 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash).
#' @inherit common_arguments
#' @inheritParams ggplot2::geom_vline
#'
#' @examples
#' animals |>
#'   tidyplot(x = weight, y = speed) |>
#'    add_reference_lines(x = 4000, y = c(100, 200)) |>
#'    add_data_points()
#'
#' animals |>
#'   tidyplot(x = weight, y = speed) |>
#'    add_reference_lines(x = 4000, y = c(100, 200), linetype = "dotdash") |>
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
#'   animals |>
#'   dplyr::slice_head(n = 5) |>
#'   tidyplot(x = weight, y = speed) |>
#'   theme_ggplot2() |>
#'   add_data_points() |>
#'   adjust_padding(all = 0.3)
#'
#' # Default label position is `below` the data point
#' p |> add_data_labels(label = animal)
#'
#' # Alternative label positions
#' p |> add_data_labels(label = animal, label_position = "above")
#'
#' p |> add_data_labels(label = animal, label_position = "right")
#'
#' p |> add_data_labels(label = animal, label_position = "left")
#'
#' # Include white background box
#' p |> add_data_labels(label = animal, background = TRUE)
#'
#' p |> add_data_labels(label = animal, background = TRUE,
#'   background_color = "pink")
#'
#' # Black labels
#' p |> add_data_labels(label = animal, color = "black")
#'
#' # Use repelling data labels
#' p |> add_data_labels_repel(label = animal, color = "black")
#'
#' p |> add_data_labels_repel(label = animal, color = "black",
#'   background = TRUE)
#'
#' p |> add_data_labels_repel(label = animal, color = "black",
#'   background = TRUE, min.segment.length = 0)
#'
#' @export
add_data_labels <- function(plot, label, data = all_rows(), fontsize = 7,
                            dodge_width = NULL, jitter_width = 0, jitter_height = 0, preserve = "total",
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

  dodge_width <- dodge_width %||% plot$tidyplot$dodge_width
  position <- compute_position(plot = plot,
                               dodge_width = dodge_width,
                               jitter_width = jitter_width,
                               jitter_height = jitter_height,
                               preserve = preserve)

  plot +
    ggplot2::geom_label(data = data, ggplot2::aes(label = {{label}}), size = size,
                        fill = scales::alpha(background_color, background_alpha),
                        vjust = vjust, hjust = hjust, linewidth = NA, label.padding = label.padding,
                        position = position, ...)
}
#' @rdname add_data_labels
#' @export
add_data_labels_repel <- function(plot, label, data = all_rows(), fontsize = 7,
                                  dodge_width = NULL, jitter_width = 0, jitter_height = 0, preserve = "total",
                                  segment.size = 0.2, box.padding = 0.2, max.overlaps = Inf,
                                  background = FALSE, background_color = "#FFFFFF", background_alpha = 0.6, ...) {
  plot <- check_tidyplot(plot)
  size <- fontsize/ggplot2::.pt
  if (!background) background_alpha <- 0
  label.padding <- ggplot2::unit(0.1, "lines")

  dodge_width <- dodge_width %||% plot$tidyplot$dodge_width
  position <- compute_position(plot = plot,
                               dodge_width = dodge_width,
                               jitter_width = jitter_width,
                               jitter_height = jitter_height,
                               preserve = preserve)

  plot + ggrepel::geom_label_repel(data = data, ggplot2::aes(label = {{label}}), size = size,
                                   segment.size = segment.size, box.padding = box.padding, max.overlaps = max.overlaps,
                                   fill = scales::alpha(background_color, background_alpha),
                                   label.size = NA, label.padding = label.padding, position = position,
                                   seed = 42, ...)
}


#' Add annotation
#' @param text String for annotation text.
#' @param x,xmin,xmax,xend,y,ymin,ymax,yend Coordinates for the annotation.
#' @param ... Arguments passed on to `ggplot2::annotate()`.
#' @inherit common_arguments
#'
#' @examples
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_boxplot() |>
#'   add_annotation_text("Look here!", x = 2, y = 25)
#'
#' eu_countries |>
#'   tidyplot(x = area, y = population) |>
#'   add_data_points() |>
#'   add_annotation_rectangle(xmin = 2.5e5, xmax = Inf, ymin = 42, ymax = Inf)
#'
#' eu_countries |>
#'   tidyplot(x = area, y = population) |>
#'   add_data_points() |>
#'   add_annotation_rectangle(xmin = 2.5e5, xmax = 6e5, ymin = 42, ymax = 90,
#'                            color = "#E69F00", fill = NA)
#'
#' eu_countries |>
#'   tidyplot(x = area, y = population) |>
#'   add_data_points() |>
#'   add_annotation_line(x = 0, xend = Inf, y = 0, yend = Inf)
#'
#' @export
add_annotation_text <- function(plot, text, x, y, fontsize = 7, ...) {
  plot <- check_tidyplot(plot)
  # parse text
  text <- tidyplot_parser(as.character(text))
  plot + ggplot2::annotate("text", label = text, x = x, y = y,
                           size = fontsize/ggplot2::.pt, ...)
}
#' @rdname add_annotation_text
#' @export
add_annotation_rectangle <- function(plot, xmin, xmax, ymin, ymax,
                                     fill = "#000000", color = NA, alpha = 0.1, ...) {
  plot <- check_tidyplot(plot)
  plot + ggplot2::annotate("rect", xmin = xmin, xmax = xmax,
                           ymin = ymin, ymax = ymax,
                           fill = fill, color = color, alpha = alpha, ...)
}
#' @rdname add_annotation_text
#' @export
add_annotation_line <- function(plot, x, xend, y, yend, color = "#000000", ...) {
  plot <- check_tidyplot(plot)
  plot + ggplot2::annotate("segment", x = x, xend = xend, y = y, yend = yend, color = color, ...)
}

