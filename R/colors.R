
# apply saturation to color
apply_saturation <- function(colors, saturation, background_color = "#FFFFFF") {
  purrr::map_chr(colors, function(color) {
    color <- col2rgb(color)
    background_color <- col2rgb(background_color)
    new_color <- (1-saturation) * background_color + saturation * color
    rgb(t(new_color), maxColorValue = 255)
  })
}


# do not @export
my_pal <- function(palette, reverse = FALSE, saturation = 1, ...) {
  pal <- palette
  if (reverse) pal <- rev(pal)
  pal <- apply_saturation(pal, saturation = saturation)
  colorRampPalette(pal, ...)
}

my_scale_color_c <- function(palette = color_palettes[["blue_pink_yellow"]], saturation = 1, reverse = FALSE, ...) {
  pal <- my_pal(palette = palette, saturation = saturation, reverse = reverse)
  ggplot2::scale_color_gradientn(colours = pal(256), ...)
}

my_scale_color_d <- function(palette = color_palettes[["metro_ui"]], saturation = 1, reverse = FALSE, ...) {
  pal <- my_pal(palette = palette, saturation = saturation, reverse = reverse)
  ggplot2::discrete_scale("colour", paste0("my_", palette), palette = pal, ...)
}

my_scale_fill_c <- function(palette = color_palettes[["blue_pink_yellow"]], saturation = 1, reverse = FALSE, ...) {
  pal <- my_pal(palette = palette, saturation = saturation, reverse = reverse)
  ggplot2::scale_fill_gradientn(colours = pal(256), ...)
}

my_scale_fill_d <- function(palette = color_palettes[["metro_ui"]], saturation = 1, reverse = FALSE, ...) {
  pal <- my_pal(palette = palette, saturation = saturation, reverse = reverse)
  ggplot2::discrete_scale("fill", paste0("my_", palette), palette = pal, ...)
}
