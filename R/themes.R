
#' Themes
#' @param gg bla
#' @param fontsize bla
#' @export
theme_tidyplot <- function(gg, fontsize = 7) {
  gg %>%
    style_just_xy() %>%
    adjust_font(fontsize)
}
#' @rdname theme_tidyplot
#' @export
theme_ggplot2 <- function(gg, fontsize = 7) {
  gg <- gg + ggplot2::theme_gray()
  gg %>% adjust_font(fontsize)
}
#' @rdname theme_tidyplot
#' @export
theme_minimal_xy <- function(gg, fontsize = 7) {
  gg <- gg + ggplot2::theme_minimal()
  gg %>% adjust_font(fontsize) +
    ggplot2::theme(
      axis.line.x = ggplot2::element_line(colour = "grey", size = 0.15),
      panel.grid.major.x = ggplot2::element_line(colour = "grey", size = 0.15),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "grey", size = 0.15),
      panel.grid.minor.y = ggplot2::element_blank()
    )
}
#' @rdname theme_tidyplot
#' @export
theme_minimal_y <- function(gg, fontsize = 7) {
  gg <- gg + ggplot2::theme_minimal()
  gg %>% adjust_font(fontsize) +
    ggplot2::theme(
      axis.line.x = ggplot2::element_line(colour = "grey", size = 0.15),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "grey", size = 0.15),
      panel.grid.minor.y = ggplot2::element_blank()
    )
}
#' @rdname theme_tidyplot
#' @export
theme_minimal_x <- function(gg, fontsize = 7) {
  gg <- gg + ggplot2::theme_minimal()
  gg %>% adjust_font(fontsize) +
    ggplot2::theme(
      axis.line.y = ggplot2::element_line(colour = "grey", size = 0.15),
      panel.grid.major.x = ggplot2::element_line(colour = "grey", size = 0.15),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    )
}

# non-exported helpers

style_void <- function(gg) {
  gg +
    ggplot2::theme_void() +
    ggplot2::theme(
    plot.title = ggplot2::element_text(size = 7, colour = "black", hjust = 0.5, vjust = 0.5),
    legend.title = ggplot2::element_text(size = 7, colour = "black"),
    legend.text = ggplot2::element_text(size = 7, colour = "black"),
    legend.key.size = ggplot2::unit(4, "mm")
  )
}

style_just_xy <- function(gg) {
  gg %>%
    style_white_bg() +
    ggplot2::theme(
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(size = 0.25, colour = "black"),
      axis.ticks = ggplot2::element_line(size = 0.25, colour = "black")
    )
}

style_white_bg <- function(gg) {
  gg +
    ggplot2::theme(
      plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "mm"),
      plot.background = ggplot2::element_rect(fill = NA, colour = NA),
      legend.background = ggplot2::element_rect(fill = NA, colour = NA),
      legend.key = ggplot2::element_rect(fill = NA, colour = NA),
      strip.background = ggplot2::element_rect(fill = NA, colour = NA),
      panel.background = ggplot2::element_rect(fill = NA, colour = NA),
      panel.border = ggplot2::element_rect(fill = NA, colour = "black", size = 0.5),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_line(colour = "black", size = 0.25)
    )
}
