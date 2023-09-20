#' @importFrom ggplot2 theme element_rect element_blank element_text element_line unit

#' @export
theme_tidyplot <- function(gg) {
  gg %>%
    style_just_xy() %>%
    adjust_fontsize()
}

#' @export
theme_ggplot2 <- function(gg) {
  gg + ggplot2::theme_gray()
}

#' @export
theme_minimal_xy <- function(gg) {
  gg <- gg + ggplot2::theme_minimal()
  gg %>% adjust_fontsize() +
    theme(
      axis.line.x = element_line(colour = "grey", size = 0.15),
      panel.grid.major.x = element_line(colour = "grey", size = 0.15),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(colour = "grey", size = 0.15),
      panel.grid.minor.y = element_blank()
    )
}

#' @export
theme_minimal_y <- function(gg) {
  gg <- gg + ggplot2::theme_minimal()
  gg %>% adjust_fontsize() +
    theme(
      axis.line.x = element_line(colour = "grey", size = 0.15),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(colour = "grey", size = 0.15),
      panel.grid.minor.y = element_blank()
    )
}

#' @export
theme_minimal_x <- function(gg) {
  gg <- gg + ggplot2::theme_minimal()
  gg %>% adjust_fontsize() +
    theme(
      axis.line.y = element_line(colour = "grey", size = 0.15),
      panel.grid.major.x = element_line(colour = "grey", size = 0.15),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    )
}

#' @export
style_white_bg <- function(gg) {
  gg +
    theme(
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"),
    plot.background = element_rect(fill = NA, colour = NA),
    legend.background = element_rect(fill = NA, colour = NA),
    legend.key = element_rect(fill = NA, colour = NA),
    strip.background = element_rect(fill = NA, colour = NA),
    panel.background = element_rect(fill = NA, colour = NA),
    panel.border = element_rect(fill = NA, colour = "black", size = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(colour = "black", size = 0.25)
  )
}

#' @export
adjust_fontsize <- function(gg, fs = 7) {
  gg +
    theme(
    plot.title = element_text(size = fs, colour = "black", hjust = 0.5, vjust = 0.5),
    plot.subtitle = element_text(size = fs, colour = "black", hjust = 0.5, vjust = 0.5),
    text = element_text(size = fs, colour = "black"),
    axis.text = element_text(size = fs, colour = "black"),
    axis.title = element_text(size = fs, colour = "black"),
    legend.title = element_text(size = fs, colour = "black"),
    legend.text = element_text(size = fs, colour = "black"),
    strip.text = element_text(size = fs, colour = "black"),
    legend.key.size = unit(4, "mm")
  )
}

#' @export
style_just_xy <- function(gg) {
  gg %>%
    style_white_bg() +
    theme(
      panel.border = element_blank(),
      axis.line = element_line(size = 0.25, colour = "black"),
      axis.ticks = element_line(size = 0.25, colour = "black")
    )
}

#' @export
style_no_axis <- function(gg) {
  gg +
    theme(
    panel.border = element_blank()
  )
}

#' @export
style_no_legend <- function(gg) {
  gg +
    theme(
    legend.position="none"
  )
}

#' @export
style_centered_title <- function(gg) {
  gg +
    theme(
    plot.title = element_text(hjust = 0.5)
  )
}

#' @export
style_rotate_labels <- function(gg, angle = 45) {
  gg +
    theme(
    axis.text.x = element_text(angle = angle, hjust = 1)
  )
}

#' @export
style_void <- function(gg) {
  gg +
    ggplot2::theme_void() +
    theme(
    plot.title = element_text(size = 7, colour = "black", hjust = 0.5, vjust = 0.5),
    legend.title = element_text(size = 7, colour = "black"),
    legend.text = element_text(size = 7, colour = "black"),
    legend.key.size = unit(4, "mm")
  )
}
