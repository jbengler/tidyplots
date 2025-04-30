#' Create a new tidyplot
#'
#' @param data A tidy `data.frame` to use for plotting.
#' @param width Width of the plot area. The default (`NULL`) retrieves the setting from the
#'   [tidyplots options][tidyplots_options], which defaults to `50`.
#' @param height Height of the plot area. The default (`NULL`) retrieves the setting from the
#'   [tidyplots options][tidyplots_options], which defaults to `50`.
#' @param unit Unit of the plot area width and height. The default (`NULL`) retrieves the setting from the
#'   [tidyplots options][tidyplots_options], which defaults to `"mm"`.
#' @param dodge_width For adjusting the distance between grouped objects.
#' The default (`NULL`) retrieves the setting from the
#'   [tidyplots options][tidyplots_options], which defaults to `0.8` for plots with
#' at least one discrete axis and to `0` for plots with two continuous axes.
#' @param my_style Styling function to apply to the plot. The default (`NULL`) retrieves the setting from the
#'   [tidyplots options][tidyplots_options], which default to no additional styling.
#' @param ... Mappings for the `x` axis, `y` axis and `color`, see examples.
#' Additional argument are passed to `ggplot2::aes()`.
#'
#' @examples
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_data_points_beeswarm()
#'
#' study |>
#'   tidyplot(x = group, y = score, color = dose) |>
#'   add_mean_bar()
#'
#' # Change plot area size
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment,
#'     width = 25, height = 25) |>
#'   add_data_points_beeswarm()
#'
#' # Change dodge_width
#' study |>
#'   tidyplot(x = group, y = score, color = dose, dodge_width = 0.3) |>
#'   add_mean_bar()
#'
#' @export
tidyplot <- function(data, ...,
                     width = NULL,
                     height = NULL,
                     unit = NULL,
                     dodge_width = NULL,
                     my_style = NULL) {
  mapping <- ggplot2::aes(...)

  # Add .single_color column to data if `colour` and `fill` mappings are missing
  single_color_plot <- FALSE
  if(!"colour" %in% names(mapping) && !"fill" %in% names(mapping)) {
    data$.single_color <- TRUE
    mapping$colour <- ggplot2::aes(colour = .single_color)[[1]]
    single_color_plot <- TRUE
  }

  # Align `colour` and `fill` mappings
  if("colour" %in% names(mapping) && !"fill" %in% names(mapping)) mapping$fill <- mapping$colour
  if("fill" %in% names(mapping) && !"colour" %in% names(mapping)) mapping$colour <- mapping$fill

  plot <- ggplot2::ggplot(data = data, mapping = mapping)
  class(plot) <- c("tidyplot", class(plot))

  plot$tidyplot$mapping <- extract_mapping(plot)

  plot$tidyplot$history <- c("tidyplot")

  plot$tidyplot$width <- width %||% getOption("tidyplots.width") %||% 50
  plot$tidyplot$height <- height %||% getOption("tidyplots.height") %||% 50
  plot$tidyplot$unit <- unit %||% getOption("tidyplots.unit") %||% "mm"

  plot$tidyplot$padding_x <- c(0.05, 0.05)
  plot$tidyplot$padding_y <- c(0.05, 0.05)

  plot$tidyplot$limits_x <- c(NULL, NULL)
  plot$tidyplot$limits_y <- c(NULL, NULL)

  # dodge_width_heuristic
  if (is_discrete(plot, "x") || is_discrete(plot, "y")) {
    dodge_width_heuristic <- 0.8
  } else {
    dodge_width_heuristic <- 0
  }
  dodge_width <- dodge_width %||% getOption("tidyplots.dodge_width") %||% dodge_width_heuristic
  plot$tidyplot$dodge_width <- dodge_width

  plot$tidyplot$named_colors <- NULL

  plot <- plot |>
    theme_tidyplot() |>
    adjust_x_axis() |>
    adjust_y_axis() |>
    adjust_colors() |>
    adjust_size()

  my_style <- my_style %||% getOption("tidyplots.my_style")
  if (!is.null(my_style)) {
    plot <- plot |> my_style()
  }

  if (single_color_plot)
    plot <- plot +
    ggplot2::guides(colour = "none", fill = "none")

  plot
}


#' Tidyplots options
#'
#' Control the settings for formatting tidyplots globally.
#'
#' @inheritParams tidyplot
#' @return The old options invisibly
#'
#' @examples
#'
#' # Define custom style
#' my_style <- function(x) x |>
#'   adjust_colors(colors_discrete_candy) |>
#'   adjust_font(family = "mono")
#'
#' # Set tidyplots options
#' tidyplots_options(
#'   width = 3,
#'   height = 4,
#'   unit = "cm",
#'   dodge_width = 1,
#'   my_style = my_style
#'   )
#'
#' # Plot
#' study |>
#'   tidyplot(x = group, y = score, color = dose) |>
#'   add_mean_bar()
#'
#' # Reset tidyplots options
#' tidyplots_options()
#'
#' # Same plot
#' study |>
#'   tidyplot(x = group, y = score, color = dose) |>
#'   add_mean_bar()
#'
#' @export
tidyplots_options <- function(
  width = NULL,
  height = NULL,
  unit = NULL,
  dodge_width = NULL,
  my_style = NULL
) {
  opts <- options(
    tidyplots.width = width,
    tidyplots.height = height,
    tidyplots.unit = unit,
    tidyplots.dodge_width = dodge_width,
    tidyplots.my_style = my_style
  )
  invisible(opts)
}


#' Split plot into multiple subplots
#' @param by Variable that should be used for splitting.
#' @param ncol,nrow The number of columns and rows per page.
#' @inheritParams patchwork::wrap_plots
#' @inherit common_arguments
#'
#' @examples
#' # Before splitting
#' energy |>
#'   dplyr::filter(year %in% c(2005, 2010, 2015, 2020)) |>
#'   tidyplot(y = energy, color = energy_source) |>
#'   add_donut() |>
#'   adjust_size(width = 25, height = 25)
#'
#' # Split by year
#' energy |>
#'   dplyr::filter(year %in% c(2005, 2010, 2015, 2020)) |>
#'   tidyplot(y = energy, color = energy_source) |>
#'   add_donut() |>
#'   adjust_size(width = 25, height = 25) |>
#'   split_plot(by = year)
#'
#' # Change dimensions of subplots
#' energy |>
#'   dplyr::filter(year %in% c(2005, 2010, 2015, 2020)) |>
#'   tidyplot(y = energy, color = energy_source) |>
#'   add_donut() |>
#'   adjust_size(width = 15, height = 15) |>
#'   split_plot(by = year)
#'
#' # Spread plots across multiple pages
#' energy |>
#'   dplyr::filter(year %in% c(2005, 2010, 2015, 2020)) |>
#'   tidyplot(y = energy, color = energy_source) |>
#'   add_donut() |>
#'   adjust_size(width = 25, height = 25) |>
#'   split_plot(by = year, ncol = 2, nrow = 1)
#'
#' @export
split_plot <- function(plot, by, ncol = NULL, nrow = NULL, byrow = NULL,
                       guides = "collect", tag_level = NULL, design = NULL) {
  plot <- check_tidyplot(plot)
  if(missing(by))
    cli::cli_abort("Argument {.arg by} missing without default.")

  widths <- plot$tidyplot$width
  heights <- plot$tidyplot$height
  unit <- plot$tidyplot$unit

  if (!is.na(widths)) widths <- ggplot2::unit(widths, unit)
  if (!is.na(heights)) heights <- ggplot2::unit(heights, unit)

  # free plot dimensions
  plot <-
    plot |>
    adjust_size(width = NA, height = NA)

  df <-
    plot$data |>
    tidyr::nest(data = -{{by}}) |>
    dplyr::arrange({{by}})

  plots <-
    purrr::map2(df$data, df |> dplyr::pull({{by}}),
         function(data, facet_title) {
           plot %+% data + ggplot2::ggtitle(facet_title)
         })

  if (is.numeric(ncol) && is.numeric(nrow)) {
    plots_per_page <- nrow * ncol
  } else {
    plots_per_page <- length(plots)
  }

  pages <-
    split(plots, ceiling(seq_along(plots)/plots_per_page)) |>
    purrr::map(~patchwork::wrap_plots(.x, ncol = ncol, nrow = nrow, widths = widths,
                                         heights = heights, guides = guides, byrow = byrow,
                                         tag_level = tag_level, design = design))

  cli::cli_alert_success("split_plot: split into {.pkg {length(plots)} plot{?s}} across {.pkg {ceiling(length(plots)/plots_per_page)} page{?s}}")

  out <- unname(pages)
  if (length(out) == 1) out <- out[[1]]
  out
}


#' View plot on screen
#'
#' @param title Plot title.
#' @param ... Arguments passed on to `print()`.
#' @inherit common_arguments
#'
#' @details
#' * `view_plot()` supports data subsetting. See examples and [Advanced plotting](https://jbengler.github.io/tidyplots/articles/Advanced-plotting.html#data-subsetting).
#'
#' @examples
#' # View intermediate stages on screen
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_sem_errorbar() |>
#'   add_data_points_beeswarm() |>
#'   view_plot(title = "Before changing color scheme") |>
#'   adjust_colors(colors_discrete_seaside) |>
#'   view_plot(title = "After changing color scheme")
#'
#' # View data subsets on screen
#' gene_expression |>
#'   tidyplot(x = condition, y = expression, color = sample_type) |>
#'   add_mean_dash() |>
#'   add_sem_errorbar() |>
#'   add_data_points_beeswarm() |>
#'   view_plot(data = filter_rows(external_gene_name == "Apol6"),
#'     title = "Apol6") |>
#'   view_plot(data = filter_rows(external_gene_name == "Bsn"),
#'     title = "Bsn")
#'
#' @export
view_plot <- function(plot, data = all_rows(), title = ggplot2::waiver(), ...) {
  plot <- check_tidyplot(plot)
  input <- plot
  if (inherits(data, "function")) {
    my_fun <- data
    plot <- plot %+% (my_fun(plot$data)) + ggplot2::ggtitle(title)
  }
  if (inherits(data, "data.frame")) plot <- plot %+% data + ggplot2::ggtitle(title)
  print(plot, ...)
  invisible(input)
}


#' Save plots to file
#'
#' This function takes a plot or list of plots and writes them to a (multipage) file.
#'
#' __Handling of file dimensions.__
#' Output file dimensions are determined according the the following precedence.
#' 1) The `width` and `height` arguments.
#' 2) Dimensions inferred from the incoming `plot` object with absolute dimensions.
#' 3) System default device dimensions.
#'
#' @param width,height Dimensions of the graphic device to save the plot.
#' Defaults to `NA`. In case of `NA`, the dimensions are inferred from the
#' incoming `plot` object (see Details).
#' @param units Units of length. Defaults to `"mm"`.
#' @param multiple_files Whether to save multiple pages as individual files.
#' @param view_plot Whether to view the plot on screen after saving.
#' @inheritParams ggplot2::ggsave
#' @inherit common_arguments
#'
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' # Save plot to file
#' study |>
#'   tidyplot(treatment, score) |>
#'   add_data_points() |>
#'   save_plot("single_plot.pdf")
#'
#' # Save intermediate stages to file
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_sem_errorbar() |>
#'   add_data_points_beeswarm() |>
#'   save_plot("before.pdf") |>
#'   adjust_colors(colors_discrete_seaside) |>
#'   save_plot("after.pdf")
#'
#' \donttest{
#'
#' # Save multipage PDF file
#' gene_expression |>
#'   dplyr::slice_head(n = 160) |>
#'   tidyplot(group, expression, color = sample_type) |>
#'   add_data_points() |>
#'   adjust_size(width = 30, height = 25) |>
#'   split_plot(by = external_gene_name, nrow = 2, ncol = 2) |>
#'   save_plot("multipage_plot.pdf")
#'
#' # Save multiple PDF files
#' gene_expression |>
#'   dplyr::slice_head(n = 160) |>
#'   tidyplot(group, expression, color = sample_type) |>
#'   add_data_points() |>
#'   adjust_size(width = 30, height = 25) |>
#'   split_plot(by = external_gene_name, nrow = 2, ncol = 2) |>
#'   save_plot("plot.pdf", multiple_files = TRUE)
#'
#' }
#' \dontshow{
#' setwd(.old_wd)
#' }
#'
#' @export
save_plot <- function(plot = ggplot2::last_plot(), filename,
                      width = NA, height = NA, units = c("mm", "cm", "in"),
                      multiple_files = FALSE, view_plot = TRUE, bg = "transparent", ...) {
  if (!ggplot2::is.ggplot(plot) && !all(purrr::map_lgl(plot, ggplot2::is.ggplot)))
    cli::cli_abort("{.arg plot} must be a single plot or a list of plots.")

  input <- plot
  if (ggplot2::is.ggplot(plot)) plot <- list(plot)
  units <- match.arg(units)

  if (check_input(plot) %in% c("pw_list", "tp_list"))
    dimensions <- get_layout_size(plot, units)$max
  else
    dimensions <- list(width = NA, height = NA)

  if (is.na(width)) width <- dimensions[["width"]] * 1.1
  if (is.na(height)) height <- dimensions[["height"]] * 1.1

  if (length(plot) == 1) {
    # single plot
    ggplot2::ggsave(plot = plot[[1]], filename = filename, width = width,
                    height = height, units = units, bg = bg, ...)
    cli::cli_alert_success("save_plot: saved to {.file {filename}}")
  } else{
    # multiple plots
    if (toupper(tools::file_ext(filename)) == "PDF" && !multiple_files) {
      # save multipage pdf
      unit_factor <- switch(units,
                            "mm"= 25.4,
                            "cm"= 2.54,
                            "in"= 1)

      width <- width / unit_factor
      height <- height / unit_factor

      pdf(file = filename, width = width, height = height)
      invisible(lapply(plot, print))
      dev.off()
      cli::cli_alert_success("save_plot: saved multipage PDF to {.file {filename}}")
    } else {
      # save to multiple files
      filenames <- burst_filename(filename, length(plot))
      purrr::map2(plot, filenames,
                  function(x, y) {
                    ggplot2::ggsave(plot = x, filename = y, width = width,
                                    height = height, units = units, bg = bg, ...)
                  })
      cli::cli_alert_success("save_plot: saved multiple plots to {.file {filenames}}")
    }
  }
  if (view_plot) print(input)
  invisible(input)
}
