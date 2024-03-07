#' Adjust colors
#' @param plot bla
#' @param new_colors bla
#' @param saturation bla
#' @param labels bla
#' @param ... bla
#' @export
adjust_colors <- function(plot, new_colors = NULL,
                          saturation = 1,
                          labels = tidyplot_parse_labels(), ...) {
  check_tidyplot(plot)
  out <- plot

  if (is_discrete(plot, "colour")) {

    # Default colors
    if (is.null(new_colors)) new_colors <- colors_discrete_metro

    # Are enough new_colors provided?
    named_vector <- FALSE
    n_ratio <- 0
    if (!is.null(names(new_colors))) {
      out$tidyplot$named_colors <- new_colors
      named_vector <- TRUE
      n_ratio <- 1
    } else {
      out$tidyplot$named_colors <- NULL
      n_provided <- length(new_colors)
      n_requested <-
        dplyr::pull(plot$data, get_variable(plot, "colour")) %>%
        unique() %>%
        length
      n_ratio <- n_provided / n_requested
    }

    if (n_ratio >= 1) {

      # Too many colors
      if (n_ratio > 1) {
        cli::cli_alert_info("adjust_colors: Too many colors. {n_provided} colors provided, but only {n_requested} needed.")
        new_colors <- downsample_vector(new_colors, n_requested)
      }

      suppressMessages(out <- out + ggplot2::scale_color_manual(values = new_colors, drop = FALSE, labels = labels, ...))
      suppressMessages(out <- out + ggplot2::scale_fill_manual(values = apply_saturation(new_colors, saturation = saturation), drop = FALSE, labels = labels, ...))
      cli::cli_alert_success("adjust_colors: applied discrete {.pkg color values}")

    } else {
      suppressMessages(out <- out + scale_color_d(palette = new_colors, drop = FALSE, labels = labels, ...))
      suppressMessages(out <- out + scale_fill_d(palette = new_colors, saturation = saturation, drop = FALSE, labels = labels, ...))
      cli::cli_alert_success("adjust_colors: applied discrete {.pkg color palette}")

      # Too few colors
      if (n_ratio < 1) cli::cli_alert_info("adjust_colors: Too few colors. {n_provided} colors provided, but {n_requested} expected.")
    }
  }

  if (is_continuous(plot, "colour")) {

    # Default colors
    if (is.null(new_colors)) new_colors <- colors_continuous_bluepinkyellow

    suppressMessages(out <- out + scale_color_c(palette = new_colors, labels = labels, ...))
    suppressMessages(out <- out + scale_fill_c(palette = new_colors, saturation = saturation, labels = labels, ...))
    cli::cli_alert_success("adjust_colors: applied continous {.pkg color palette}")
  }
  out
}

apply_saturation <- function(colors, saturation, background_color = "#FFFFFF") {
  purrr::map_chr(colors, function(color) {
    color <- col2rgb(color)
    background_color <- col2rgb(background_color)
    new_color <- (1-saturation) * background_color + saturation * color
    rgb(t(new_color), maxColorValue = 255)
  })
}

make_palette <- function(palette, reverse = FALSE, saturation = 1, ...) {
  pal <- palette
  if (reverse) pal <- rev(pal)
  pal <- apply_saturation(pal, saturation = saturation)
  grDevices::colorRampPalette(pal, ...)
}

scale_color_d <- function(palette = NULL, saturation = 1, reverse = FALSE, ...) {
  pal <- make_palette(palette = palette, saturation = saturation, reverse = reverse)
  ggplot2::discrete_scale("colour", paste0("my_", palette), palette = pal, ...)
}

scale_fill_d <- function(palette = NULL, saturation = 1, reverse = FALSE, ...) {
  pal <- make_palette(palette = palette, saturation = saturation, reverse = reverse)
  ggplot2::discrete_scale("fill", paste0("my_", palette), palette = pal, ...)
}

scale_color_c <- function(palette = NULL, saturation = 1, reverse = FALSE, ...) {
  pal <- make_palette(palette = palette, saturation = saturation, reverse = reverse)
  ggplot2::scale_color_gradientn(colours = pal(256), ...)
}

scale_fill_c <- function(palette = NULL, saturation = 1, reverse = FALSE, ...) {
  pal <- make_palette(palette = palette, saturation = saturation, reverse = reverse)
  ggplot2::scale_fill_gradientn(colours = pal(256), ...)
}
