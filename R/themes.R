
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
  paper <- plot$tidyplot$paper
  ink <- plot$tidyplot$ink

  plot <- plot + ggplot2::theme_classic(paper = paper, ink = ink)

  if (get_variable(plot, "colour") == ".single_color")
    plot <- plot |> remove_legend()

  plot |>
    adjust_font(fontsize) |>
    adjust_size() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(colour = NA),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(linewidth = 0.25),
      axis.ticks = ggplot2::element_line(linewidth = 0.25),
      strip.background = ggplot2::element_rect(colour = NA),
      # plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "mm")
    )
}
#' @rdname theme_tidyplot
#' @export
theme_ggplot2 <- function(plot, fontsize = 7) {
  plot <- check_tidyplot(plot)
  paper <- plot$tidyplot$paper
  ink <- plot$tidyplot$ink

  plot <- plot + ggplot2::theme_grey(paper = paper, ink = ink)

  if (get_variable(plot, "colour") == ".single_color")
    plot <- plot |> remove_legend()

  plot |>
    adjust_font(fontsize) |>
    adjust_size()
}
#' @rdname theme_tidyplot
#' @export
theme_minimal_xy <- function(plot, fontsize = 7) {
  plot <- check_tidyplot(plot)
  paper <- plot$tidyplot$paper
  ink <- plot$tidyplot$ink

  plot |>
    theme_tidyplot() |>
    adjust_font(fontsize) |>
    adjust_size() +
    ggplot2::theme(
      axis.line.x = ggplot2::element_line(colour = scales::col_mix(ink, paper, 0.8), linewidth = 0.15),
      axis.line.y = ggplot2::element_line(colour = scales::col_mix(ink, paper, 0.8), linewidth = 0.15),
      panel.grid.major.x = ggplot2::element_line(colour = scales::col_mix(ink, paper, 0.8), linewidth = 0.15),
      panel.grid.major.y = ggplot2::element_line(colour = scales::col_mix(ink, paper, 0.8), linewidth = 0.15),
      axis.ticks.x = ggplot2::element_line(colour = scales::col_mix(ink, paper, 0.8), linewidth = 0.15),
      axis.ticks.y = ggplot2::element_line(colour = scales::col_mix(ink, paper, 0.8), linewidth = 0.15)
    )
}
#' @rdname theme_tidyplot
#' @export
theme_minimal_x <- function(plot, fontsize = 7) {
  plot <- check_tidyplot(plot)
  paper <- plot$tidyplot$paper
  ink <- plot$tidyplot$ink

  plot |>
    theme_tidyplot() |>
    adjust_font(fontsize) |>
    adjust_size() +
    ggplot2::theme(
      axis.line.x = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_line(colour = scales::col_mix(ink, paper, 0.8), linewidth = 0.15),
      panel.grid.major.x = ggplot2::element_line(colour = scales::col_mix(ink, paper, 0.8), linewidth = 0.15),
      axis.ticks.y = ggplot2::element_line(colour = scales::col_mix(ink, paper, 0.8), linewidth = 0.15),
      axis.ticks.x = ggplot2::element_line(colour = scales::col_mix(ink, paper, 0.8), linewidth = 0.15)
    )
}
#' @rdname theme_tidyplot
#' @export
theme_minimal_y <- function(plot, fontsize = 7) {
  plot <- check_tidyplot(plot)
  paper <- plot$tidyplot$paper
  ink <- plot$tidyplot$ink

  plot |>
    theme_tidyplot() |>
    adjust_font(fontsize) |>
    adjust_size() +
    ggplot2::theme(
      axis.line.y = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(colour = scales::col_mix(ink, paper, 0.8), linewidth = 0.15),
      panel.grid.major.y = ggplot2::element_line(colour = scales::col_mix(ink, paper, 0.8), linewidth = 0.15),
      axis.ticks.x = ggplot2::element_line(colour = scales::col_mix(ink, paper, 0.8), linewidth = 0.15),
      axis.ticks.y = ggplot2::element_line(colour = scales::col_mix(ink, paper, 0.8), linewidth = 0.15)
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

