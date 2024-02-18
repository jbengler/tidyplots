

#' Show color palettes
#'
#' @param pals bla
#' @param show_labels bla
#' @param ncol bla
#' @param ... bla
#' @export
show_color_palettes <- function(pals = color_palettes, show_labels = TRUE, ncol = 3, ...) {
  tidy_pals <-
    tibble::enframe(pals, name = "pal") %>%
    tidyr::unnest(cols = c(value)) %>%
    tibble::rownames_to_column("nr") %>%
    dplyr::mutate(
      nr = factor(nr, levels = dplyr::row_number()),
      label_color = dplyr::if_else(as(colorspace::hex2RGB(value), "HLS")@coords[,2] > 0.6, "#000000", "#FFFFFF"))

  fill_colors <- tibble::deframe(tidy_pals[c(3,3)])
  label_colors <- tibble::deframe(tidy_pals[c(4,4)])

  ggplot2::ggplot(tidy_pals, ggplot2::aes(x = nr, y = 1, fill = value)) +
    ggplot2::geom_tile() +
    { if (show_labels) ggplot2::geom_text(ggplot2::aes(label = value, color = label_color), angle = 90, size = 2.5) else NULL
    } +
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::scale_fill_manual(values = fill_colors, guide = "none") +
    ggplot2::scale_color_manual(values = label_colors, guide = "none") +
    ggplot2::theme_void() +
    ggplot2::facet_wrap(facets = ggplot2::vars(pal), scales = "free", ncol = ncol, ...)
}

# not exported
apply_saturation <- function(colors, saturation, background_color = "#FFFFFF") {
  purrr::map_chr(colors, function(color) {
    color <- col2rgb(color)
    background_color <- col2rgb(background_color)
    new_color <- (1-saturation) * background_color + saturation * color
    rgb(t(new_color), maxColorValue = 255)
  })
}

my_pal <- function(palette, reverse = FALSE, saturation = 1, ...) {
  pal <- palette
  if (reverse) pal <- rev(pal)
  pal <- apply_saturation(pal, saturation = saturation)
  grDevices::colorRampPalette(pal, ...)
}

my_scale_color_c <- function(palette = NULL, saturation = 1, reverse = FALSE, ...) {
  if (is.null(palette)) palette <- color_palettes[["blue_pink_yellow"]]
  pal <- my_pal(palette = palette, saturation = saturation, reverse = reverse)
  ggplot2::scale_color_gradientn(colours = pal(256), ...)
}

my_scale_color_d <- function(palette = NULL, saturation = 1, reverse = FALSE, ...) {
  if (is.null(palette)) palette <- color_palettes[["metro_ui"]]
  pal <- my_pal(palette = palette, saturation = saturation, reverse = reverse)
  ggplot2::discrete_scale("colour", paste0("my_", palette), palette = pal, ...)
}

my_scale_fill_c <- function(palette = NULL, saturation = 1, reverse = FALSE, ...) {
  if (is.null(palette)) palette <- color_palettes[["blue_pink_yellow"]]
  pal <- my_pal(palette = palette, saturation = saturation, reverse = reverse)
  ggplot2::scale_fill_gradientn(colours = pal(256), ...)
}

my_scale_fill_d <- function(palette = NULL, saturation = 1, reverse = FALSE, ...) {
  if (is.null(palette)) palette <- color_palettes[["metro_ui"]]
  pal <- my_pal(palette = palette, saturation = saturation, reverse = reverse)
  ggplot2::discrete_scale("fill", paste0("my_", palette), palette = pal, ...)
}
