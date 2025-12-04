
#' Themes
#' @inherit common_arguments
#'
#' @examples
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_data_points() |>
#'   add_sem_errorbar() |>
#'   add_mean_dash() |>
#'   theme_tidyplot()
#'
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_data_points() |>
#'   add_sem_errorbar() |>
#'   add_mean_dash() |>
#'   theme_ggplot2()
#'
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_data_points() |>
#'   add_sem_errorbar() |>
#'   add_mean_dash() |>
#'   theme_minimal_xy()
#'
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_data_points() |>
#'   add_sem_errorbar() |>
#'   add_mean_dash() |>
#'   theme_minimal_x()
#'
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_data_points() |>
#'   add_sem_errorbar() |>
#'   add_mean_dash() |>
#'   theme_minimal_y()
#'
#' @export
theme_tidyplot <- function(plot, fontsize = 7) {
  plot <- check_tidyplot(plot)
  plot |>
    style_just_xy() |>
    adjust_font(fontsize)
}
#' @rdname theme_tidyplot
#' @export
theme_ggplot2 <- function(plot, fontsize = 7) {
  plot <- check_tidyplot(plot)
  plot <- plot + ggplot2::theme_gray()
  if (get_variable(plot, "colour") == ".single_color")
    plot <- plot |> remove_legend()
  plot |> adjust_font(fontsize)
}
#' @rdname theme_tidyplot
#' @export
theme_minimal_xy <- function(plot, fontsize = 7) {
  plot <- check_tidyplot(plot)
  plot <- plot + ggplot2::theme_minimal()
  if (get_variable(plot, "colour") == ".single_color")
    plot <- plot |> remove_legend()
  plot |> adjust_font(fontsize) +
    ggplot2::theme(
      axis.line.x = ggplot2::element_line(colour = "grey", linewidth = 0.15),
      panel.grid.major.x = ggplot2::element_line(colour = "grey", linewidth = 0.15),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "grey", linewidth = 0.15),
      panel.grid.minor.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_line(colour = "grey", linewidth = 0.15),
      axis.ticks.y = ggplot2::element_line(colour = "grey", linewidth = 0.15)
    )
}
#' @rdname theme_tidyplot
#' @export
theme_minimal_x <- function(plot, fontsize = 7) {
  plot <- check_tidyplot(plot)
  plot <- plot + ggplot2::theme_minimal()
  if (get_variable(plot, "colour") == ".single_color")
    plot <- plot |> remove_legend()
  plot |> adjust_font(fontsize) +
    ggplot2::theme(
      axis.line.y = ggplot2::element_line(colour = "grey", linewidth = 0.15),
      panel.grid.major.x = ggplot2::element_line(colour = "grey", linewidth = 0.15),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_line(colour = "grey", linewidth = 0.15),
      axis.ticks.y = ggplot2::element_line(colour = "grey", linewidth = 0.15)
    )
}
#' @rdname theme_tidyplot
#' @export
theme_minimal_y <- function(plot, fontsize = 7) {
  plot <- check_tidyplot(plot)
  plot <- plot + ggplot2::theme_minimal()
  if (get_variable(plot, "colour") == ".single_color")
    plot <- plot |> remove_legend()
  plot |> adjust_font(fontsize) +
    ggplot2::theme(
      axis.line.x = ggplot2::element_line(colour = "grey", linewidth = 0.15),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "grey", linewidth = 0.15),
      panel.grid.minor.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_line(colour = "grey", linewidth = 0.15),
      axis.ticks.y = ggplot2::element_line(colour = "grey", linewidth = 0.15)
    )
}

# non-exported helpers

style_void <- function(plot, fontsize = 7) {
  plot |>
    remove_x_axis() |>
    remove_y_axis() +
    ggplot2::theme(
      panel.spacing = ggplot2::unit(0, "mm"),
      strip.text = ggplot2::element_text(margin = ggplot2::margin(7,0,0,0))
    )
}

style_just_xy <- function(plot) {
  plot |>
    style_white_bg() +
    ggplot2::theme(
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(linewidth = 0.25, colour = "black"),
      axis.ticks = ggplot2::element_line(linewidth = 0.25, colour = "black")
    )
}

style_white_bg <- function(plot) {
  plot +
    ggplot2::theme(
      plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "mm"),
      plot.background = ggplot2::element_rect(fill = NA, colour = NA),
      legend.background = ggplot2::element_rect(fill = NA, colour = NA),
      legend.key = ggplot2::element_rect(fill = NA, colour = NA),
      strip.background = ggplot2::element_rect(fill = NA, colour = NA),
      panel.background = ggplot2::element_rect(fill = NA, colour = NA),
      panel.border = ggplot2::element_rect(fill = NA, colour = "black", linewidth = 0.5),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_line(colour = "black", linewidth = 0.25)
    )
}

style_black_bg <- function(plot) {
  plot +
    ggplot2::theme(
      plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "mm"),
      plot.background = ggplot2::element_rect(fill = "black", colour = "black"),
      legend.background = ggplot2::element_rect(fill = "black", colour = "black"),
      legend.key = ggplot2::element_rect(fill = "black", colour = "black"),
      strip.background = ggplot2::element_rect(fill = "black", colour = "black"),
      panel.background = ggplot2::element_rect(fill = "black", colour = "black"),
      panel.border = ggplot2::element_rect(fill = NA, colour = "grey", linewidth = 0.5),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_line(colour = "grey", linewidth = 0.25)
    )
}
