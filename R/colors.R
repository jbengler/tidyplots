#' Adjust colors
#' @param new_colors A character vector of new hex colors to use. Can be a named character vector of hex colors to assign certain data labels to specific colors.
#' @param downsample If too many colors are provided, whether to downsample `evenly`, or use the `first`, the `last` or the `middle` colors of the color vector. Defaults to `evenly`.
#' @param ... Arguments passed on to the ggplot2 `scale` function.
#' @inherit common_arguments
#' @inheritParams ggplot2::scale_x_continuous
#'
#' @seealso [colors_discrete_friendly()], [colors_continuous_viridis()], [colors_diverging_blue2brown()], and [new_color_scheme()]
#'
#' @examples
#' # Plot without adjustments
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_data_points() %>%
#'   add_mean_bar(alpha = 0.4) %>%
#'   add_sem_errorbar()
#'
#' # Provide hex colors
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_data_points() %>%
#'   add_mean_bar(alpha = 0.4) %>%
#'   add_sem_errorbar() %>%
#'   adjust_colors(new_colors = c("#644296","#F08533","#3B78B0", "#D1352C"))
#'
#' # Provide discrete color scheme
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_data_points() %>%
#'   add_mean_bar(alpha = 0.4) %>%
#'   add_sem_errorbar() %>%
#'   adjust_colors(new_colors = colors_discrete_seaside)
#'
#' # Provide named vector
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_data_points() %>%
#'   add_mean_bar(alpha = 0.4) %>%
#'   add_sem_errorbar() %>%
#'   adjust_colors(new_colors = c(
#'     "A" = "pink",
#'     "B" = "purple",
#'     "C" = "grey",
#'     "D" = "blue"))
#'
#' # Provide continuous color scheme
#' climate %>%
#'   tidyplot(x = month, y = year, color = max_temperature) %>%
#'   add_heatmap() %>%
#'   adjust_colors(new_colors = colors_continuous_turbo)
#'
#' @export
adjust_colors <- function(plot, new_colors = NULL,
                          saturation = 1,
                          labels = tidyplot_parse_labels(),
                          downsample = c("evenly", "first", "last", "middle"),
                          ...) {
  plot <- check_tidyplot(plot)
  out <- plot

  if (is_discrete(plot, "colour")) {

    # Default colors
    if (is.null(new_colors)) new_colors <- colors_discrete_friendly

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
      color_var <- get_variable(plot, "colour")
      n_requested <-
        dplyr::pull(plot$data, color_var) %>%
        unique() %>%
        length
      n_ratio <- n_provided / n_requested
    }

    if (n_ratio >= 1) {

      # Too many colors
      if (n_ratio > 1) {
        # cli::cli_alert_info("adjust_colors: Too many colors. {n_provided} colors provided, but only {n_requested} needed.")
        new_colors <- downsample_vector(new_colors, n_requested, downsample = downsample)
      }

      suppressMessages(out <- out + ggplot2::scale_color_manual(values = new_colors, drop = FALSE, labels = labels, ...))
      suppressMessages(out <- out + ggplot2::scale_fill_manual(values = apply_saturation(new_colors, saturation = saturation), drop = FALSE, labels = labels, ...))
      # cli::cli_alert_success("adjust_colors: applied discrete {.pkg color values}")

    } else {
      suppressMessages(out <- out + scale_color_d(palette = new_colors, drop = FALSE, labels = labels, ...))
      suppressMessages(out <- out + scale_fill_d(palette = new_colors, saturation = saturation, drop = FALSE, labels = labels, ...))
      # cli::cli_alert_success("adjust_colors: applied discrete {.pkg color palette}")

      # Too few colors
      # if (n_ratio < 1) cli::cli_alert_info("adjust_colors: Too few colors. {n_provided} colors provided, but {n_requested} expected.")
    }
  }

  if (is_continuous(plot, "colour")) {

    # Default colors
    if (is.null(new_colors)) new_colors <- colors_continuous_viridis

    suppressMessages(out <- out + scale_color_c(palette = new_colors, labels = labels, ...))
    suppressMessages(out <- out + scale_fill_c(palette = new_colors, saturation = saturation, labels = labels, ...))
    # cli::cli_alert_success("adjust_colors: applied continous {.pkg color palette}")
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
  ggplot2::discrete_scale("colour", palette = pal, ...)
}

scale_fill_d <- function(palette = NULL, saturation = 1, reverse = FALSE, ...) {
  pal <- make_palette(palette = palette, saturation = saturation, reverse = reverse)
  ggplot2::discrete_scale("fill", palette = pal, ...)
}

scale_color_c <- function(palette = NULL, saturation = 1, reverse = FALSE, ...) {
  pal <- make_palette(palette = palette, saturation = saturation, reverse = reverse)
  ggplot2::scale_color_gradientn(colours = pal(256), ...)
}

scale_fill_c <- function(palette = NULL, saturation = 1, reverse = FALSE, ...) {
  pal <- make_palette(palette = palette, saturation = saturation, reverse = reverse)
  ggplot2::scale_fill_gradientn(colours = pal(256), ...)
}
