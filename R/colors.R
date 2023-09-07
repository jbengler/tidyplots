#' @importFrom ggplot2 discrete_scale scale_color_gradientn scale_fill_gradientn

# apply alpha and convert to solid hex code
apply_alpha <- function(colors, alpha, background_color = "#FFFFFF") {
  purrr::map_chr(colors, function(color) {
    color <- col2rgb(color)
    background_color <- col2rgb(background_color)
    new_color <- (1-alpha) * background_color + alpha * color
    rgb(t(new_color), maxColorValue = 255)
  })
}

# do not @export
my_pal <- function(palette = "metro_ui", reverse = FALSE, alpha = 1, ...) {
  pal <- my_pals[[palette]]
  if (reverse) pal <- rev(pal)
  pal <- apply_alpha(pal, alpha = alpha)
  colorRampPalette(pal, ...)
}

#' @export
my_scale_color_c <- function(palette = "blue_pink_yellow", reverse = FALSE, ...) {
  pal <- my_pal(palette = palette, reverse = reverse)
  scale_color_gradientn(colours = pal(256), ...)
}

#' @export
my_scale_color_d <- function(palette = "metro_ui", alpha = 1, reverse = FALSE, ...) {
  pal <- my_pal(palette = palette, alpha = alpha, reverse = reverse)
  discrete_scale("colour", paste0("my_", palette), palette = pal, ...)
}

#' @export
my_scale_fill_c <- function(palette = "blue_pink_yellow", reverse = FALSE, ...) {
  pal <- my_pal(palette = palette, reverse = reverse)
  scale_fill_gradientn(colours = pal(256), ...)
}

#' @export
my_scale_fill_d <- function(palette = "metro_ui", alpha = 1, reverse = FALSE, ...) {
  pal <- my_pal(palette = palette, alpha = alpha, reverse = reverse)
  discrete_scale("fill", paste0("my_", palette), palette = pal, ...)
}
