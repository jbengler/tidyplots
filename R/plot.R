#' Create a new tidyplot
#'
#' @param data bla
#' @param ... bla
#' @param width bla
#' @param height bla
#' @param dodge_width bla
#' @export
tidyplot <- function(data, ..., width = 50, height = 50, dodge_width = 0.8) {
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

  plot$tidyplot$padding_x <- c(0.05, 0.05)
  plot$tidyplot$padding_y <- c(0.05, 0.05)

  plot$tidyplot$limits_x <- c(NULL, NULL)
  plot$tidyplot$limits_y <- c(NULL, NULL)

  plot$tidyplot$dodge_width <- dodge_width

  plot$tidyplot$named_colors <- NULL

  plot <- plot %>%
    theme_tidyplot() %>%
    adjust_x_axis() %>%
    adjust_y_axis() %>%
    adjust_colors() %>%
    adjust_plot_area_size(width = width, height = height)

  if (single_color_plot)
    plot <- plot %>% remove_legend()

  plot
}

# plot <-
#   study %>%
#   tidyplot(treatment, score, color = group) %>%
#   add_median_dash() %>%
#   add_data_points()

#' View plot on screen
#'
#' @param plot bla
#' @param data bla
#' @param title bla
#' @param ... bla
#' @export
view_plot <- function(plot, data = all_rows(), title = ggplot2::waiver(), ...) {
  input <- plot
  if (inherits(data, "function")) plot <- plot %+% (plot$data %>% data()) + ggplot2::ggtitle(title)
  if (inherits(data, "data.frame")) plot <- plot %+% data + ggplot2::ggtitle(title)
  print(plot, ...)
  invisible(input)
}

multipage_plots <- function(plot,
                            ncol = NULL,
                            nrow = NULL,
                            byrow = NULL,
                            widths = 30,
                            heights = 25,
                            guides = "collect",
                            tag_level = NULL,
                            design = NULL,
                            unit ="mm") {
  if (!ggplot2::is.ggplot(plot) && !all(purrr::map_lgl(plot, ggplot2::is.ggplot)))
    cli::cli_abort("{.arg plot} must be a single plot or a list of plots.")
  if (ggplot2::is.ggplot(plot)) plot <- list(plot)

  if (is.numeric(ncol) & is.numeric(nrow)) {
    plots_per_page <- nrow * ncol
  } else {
    plots_per_page <- length(plot)
  }
  cli::cli_alert_success("split_plot: split into {.pkg {length(plot)} plots} across {.pkg {ceiling(length(plot)/plots_per_page)} pages} on a {.pkg {ncol} x {nrow}} grid")
  pages <-
    split(plot, ceiling(seq_along(plot)/plots_per_page)) %>%
    purrr::map(., ~patchwork::wrap_plots(.x, ncol = ncol, nrow = nrow, widths = widths, heights = heights, guides = guides, byrow = byrow, tag_level = tag_level, design = design))
  unname(pages)
}


#' Split plot into subplots
#' @param plot A `ggplot`
#' @param by Variable that should be used for faceting.
#' @param ncol,nrow The number of columns and rows per page.
#' @param unit Unit of length. Defaults to "mm".
#' @inheritParams patchwork::wrap_plots
#' @export
split_plot <- function(plot,
                       by,
                       ncol = NULL,
                       nrow = NULL,
                       byrow = NULL,
                       widths = 30,
                       heights = 25,
                       guides = "collect",
                       tag_level = NULL,
                       design = NULL,
                       unit = "mm") {
  if (!ggplot2::is.ggplot(plot))
    cli::cli_abort("{.arg plot} must be a single plot.")
  if(missing(by))
    cli::cli_abort("Argument {.arg by} missing without default.")

  # free plot dimensions
  plot <-
    plot %>%
    adjust_plot_area_size(width = NA, height = NA)

  df <-
    plot$data %>%
    tidyr::nest(data = -{{by}}) %>%
    dplyr::arrange({{by}})
  plots <-
    purrr::map2(df$data, df %>% dplyr::pull({{by}}),
         function(data, facet_title) {
           plot %+% data + ggplot2::ggtitle(facet_title)
         })
  cli::cli_alert_success("split_plot: {.pkg widths} = {widths} {unit}, {.pkg heights} = {heights} {unit}")
  if (!is.na(widths)) widths <- ggplot2::unit(widths, unit)
  if (!is.na(heights)) heights <- ggplot2::unit(heights, unit)
  out <- multipage_plots(plots, ncol = ncol, nrow = nrow, widths = widths, heights = heights, unit = unit, guides = guides, byrow = byrow, tag_level = tag_level, design = design)
  if (length(out) == 1) out <- out[[1]]
  out
}

#' Save plots to file
#'
#' This function takes a plot or list of plots and writes them to a (multipage) file.
#'
#' __Handling of multiple pages.__
#' For a list of `ggplot`s, each list element is rendered as a separate page into a mutipage `pdf` file.
#' To save pages as individual files, use `multiple_files = TRUE`.
#' For output formats that do not support multipage files (`png`, `jpg`, etc), pages are saved to individual files by default.
#'
#' __Handling of file dimensions.__
#' Output file dimensions are determined according the the following precedence.
#' 1) The `width` and `height` parameters of `save_multipage()`.
#' 2) Dimensions inferred from an incoming `ggplot` object containing absolute dimensions.
#' 3) System default device dimensions.
#'
#' @param plot A `ggplot` or list of `ggplot`s
#'
#' @param width,height Dimensions of the saved plot. If not specified, `save_multipage()` will
#' try to infer the dimensions from the incoming `ggplot` object. If the incoming `ggplot` object has no absolute
#' dimensions, system default device dimensions are used.
#' @param units Unit dimensions. Defaults to "mm".
#' @param multiple_files Save pages as individal files.
#' @inheritParams ggplot2::ggsave
#'
#' @export
save_plot <- function(plot = ggplot2::last_plot(), filename, device = NULL, path = NULL, scale = 1,
                           width = NA, height = NA, units = c("mm", "cm", "in"), dpi = 300, limitsize = TRUE,
                           multiple_files = FALSE, bg = "transparent", ...) {
  if (!ggplot2::is.ggplot(plot) && !all(purrr::map_lgl(plot, ggplot2::is.ggplot)))
    cli::cli_abort("{.arg plot} must be a single plot or a list of plots.")

  print(plot)
  input <- plot

  if (ggplot2::is.ggplot(plot)) {
    plot <- list(plot)
  }

  units <- match.arg(units)

  if (length(plot) > 1 && toupper(tools::file_ext(filename)) != "PDF")
    multiple_files <- TRUE

  if (check_input(plot) == "pw_list")
    dimensions <- get_layout_size(plot, units)$max
  else
    dimensions <- list(width = NA, height = NA)

  width_defined_by <- dplyr::case_when(is.na(width) && is.na(dimensions[["width"]]) ~ "was not defined - system default used",
                                !is.na(width) ~ "was provided as parameter 'width'",
                                TRUE ~ "was inferred from plot dimensions")
  height_defined_by <- dplyr::case_when(is.na(height) && is.na(dimensions[["height"]]) ~ "was not defined - system default used",
                                 !is.na(height) ~ "was provided as parameter 'height'",
                                 TRUE ~ "was inferred from plot dimensions")

  if (is.na(width)) width <- dimensions[["width"]] * 1.1
  if (is.na(height)) height <- dimensions[["height"]] * 1.1

  cli::cli_alert_success("save_plot: {.pkg page width} {width_defined_by}")
  cli::cli_alert_success("save_plot: {.pkg page height} {height_defined_by}")

  if (!is.na(width) && !is.na(height))
    cli::cli_alert_success("save_plot: saving {.pkg {length(plot)} pages} with {.pkg {round(width)} x {round(height)}} mm to {.pkg {filename}}")

  if (multiple_files) {
    filenames <- burst_filename(filename, length(plot))
    purrr::map2(plot, filenames,
         function(x, y) {
           ggplot2::ggsave(plot = x, filename = y, device = device, path = path, scale = scale,
                           width = width, height = height, units = units, dpi = dpi, limitsize = limitsize, ...)
         })

  } else {

    unit_factor <- switch(
      units,
      "mm"= 25.4,
      "cm"= 2.54,
      "in"= 1
    )

    width <- width / unit_factor
    height <- height / unit_factor

    pdf(file = filename, width = width, height = height)
    invisible(lapply(plot, print))
    dev.off()

  }
  invisible(input)
}
