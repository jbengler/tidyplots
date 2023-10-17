
#' Create a new plot
#' @param data bla
#' @param ... bla
#' @param width bla
#' @param height bla
#' @param dodge_width bla
#'
#' @export
tidyplot <- function(data, ..., width = 50, height = 50, dodge_width = 0.8) {
  mapping <- ggplot2::aes(...)
  single_color_plot <- FALSE

  # in absence of `colour` and `fill`, add .single_color column to data
  if(!"colour" %in% names(mapping) && !"fill" %in% names(mapping)) {
    data$.single_color <- TRUE
    mapping$colour <- ggplot2::aes(colour = .single_color)[[1]]
    single_color_plot <- TRUE
  }

  # align `colour` and `fill` aesthetic
  if("colour" %in% names(mapping) && !"fill" %in% names(mapping)) mapping$fill <- mapping$colour
  if("fill" %in% names(mapping) && !"colour" %in% names(mapping)) mapping$colour <- mapping$fill

  gg <- ggplot2::ggplot(data = data, mapping = mapping)

  gg$tidyplot$mapping <- extract_mapping(gg)

  gg$tidyplot$padding_x <- c(0.05, 0.05)
  gg$tidyplot$padding_y <- c(0.05, 0.05)

  gg$tidyplot$limits_x <- c(NA, NA)
  gg$tidyplot$limits_y <- c(NA, NA)

  gg$tidyplot$dodge_width <- dodge_width

  gg <- gg %>%
    theme_tidyplot() %>%
    adjust_x_axis() %>%
    adjust_y_axis() %>%
    adjust_colors() %>%
    adjust_size(width = width, height = height)

  if (single_color_plot)
    gg <- gg %>% remove_legend()

  gg
}

#' Render plot
#' @param gg bla
#' @param ... bla
#' @export
render_plot <- function(gg, ...) {
  print(gg, ...)
  return(gg)
}

multipage_plots <- function(gg,
                            ncol = NULL,
                            nrow = NULL,
                            byrow = NULL,
                            widths = 30,
                            heights = 25,
                            guides = "collect",
                            tag_level = NULL,
                            design = NULL,
                            my_unit ="mm") {
  if (!ggplot2::is.ggplot(gg) && !all(purrr::map_lgl(gg, ggplot2::is.ggplot)))
    stop("argument 'gg' should be ggplot or list off ggplots")
  if (ggplot2::is.ggplot(gg)) gg <- list(gg)

  if (is.numeric(ncol) & is.numeric(nrow)) {
    plots_per_page <- nrow * ncol
  } else {
    plots_per_page <- length(gg)
  }
  cli::cli_alert_success("split_plot: split into {.pkg {length(gg)} plots} across {.pkg {ceiling(length(gg)/plots_per_page)} pages} on a {.pkg {ncol} x {nrow}} grid")
  pages <-
    split(gg, ceiling(seq_along(gg)/plots_per_page)) %>%
    purrr::map(., ~patchwork::wrap_plots(.x, ncol = ncol, nrow = nrow, widths = widths, heights = heights, guides = guides, byrow = byrow, tag_level = tag_level, design = design))
  unname(pages)
}


#' Split plot into subplots
#' @param gg A `ggplot`
#' @param by Variable that should be used for faceting.
#' @param ncol,nrow The number of columns and rows per page.
#' @inheritParams patchwork::wrap_plots
#' @export
split_plot <- function(gg,
                       by,
                       ncol = NULL,
                       nrow = NULL,
                       byrow = NULL,
                       widths = 30,
                       heights = 25,
                       guides = "collect",
                       tag_level = NULL,
                       design = NULL,
                       my_unit = "mm") {
  if (!ggplot2::is.ggplot(gg))
    stop("argument 'gg' should be a single ggplot")
  if(missing(by))
    stop("argument 'by' missing without default")

  # free plot dimensions
  gg <-
    gg %>%
    adjust_size(width = NA, height = NA)

  df <-
    gg$data %>%
    tidyr::nest(data = -{{by}}) %>%
    dplyr::arrange({{by}})
  plots <-
    purrr::map2(df$data, df %>% dplyr::pull({{by}}),
         function(data, facet_title) {
           gg %+% data + ggplot2::ggtitle(facet_title)
         })
  cli::cli_alert_success("split_plot: {.pkg widths} = {widths} {my_unit}, {.pkg heights} = {heights} {my_unit}")
  if (!is.na(widths)) widths <- ggplot2::unit(widths, my_unit)
  if (!is.na(heights)) heights <- ggplot2::unit(heights, my_unit)
  out <- multipage_plots(plots, ncol = ncol, nrow = nrow, widths = widths, heights = heights, my_unit = my_unit, guides = guides, byrow = byrow, tag_level = tag_level, design = design)
  if (length(out) == 1)
    return(out[[1]])
  else
    return(out)
}

#' Save plot to file
#'
#' This function takes a `ggplot` or list of `ggplot`s and writes them to a (multipage) file. See below for details.
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
#' @param gg A `ggplot` or list of `ggplot`s
#'
#' @param width,height Dimensions of the saved plot. If not specified, `save_multipage()` will
#' try to infer the dimensions from the incoming `ggplot` object. If the incoming `ggplot` object has no absolute
#' dimensions, system default device dimensions are used.
#' @param units Unit dimensions. Defaults to "mm".
#' @param return_input Return the input ggplot or plotlist is after saving.
#' This enables the use within `dplyr` pipes.
#' @param multiple_files Save pages as individal files.
#' @inheritParams ggplot2::ggsave
#'
#' @export
save_plot <- function(gg = last_plot(), filename, device = NULL, path = NULL, scale = 1,
                           width = NA, height = NA, units = c("mm", "cm", "in"), dpi = 300, limitsize = TRUE,
                           return_input = TRUE, multiple_files = FALSE, bg = "transparent", ...) {
  if (!ggplot2::is.ggplot(gg) && !all(purrr::map_lgl(gg, ggplot2::is.ggplot)))
    stop("argument 'gg' should be ggplot or list off ggplots")

  input <- gg

  if (ggplot2::is.ggplot(gg)) {
    gg <- list(gg)
  }

  units <- match.arg(units)

  if (length(gg) > 1 && toupper(tools::file_ext(filename)) != "PDF")
    multiple_files <- TRUE

  if (check_input(gg) == "pw_list")
    dimensions <- get_layout_size(gg, units)$max
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
    cli::cli_alert_success("save_plot: saving {.pkg {length(gg)} pages} with {.pkg {round(width)} x {round(height)}} mm to {.pkg {filename}}")
glue::glue()
  if (multiple_files) {
    filenames <- burst_filename(filename, length(gg))
    purrr::map2(gg, filenames,
         function(x, y) {
           ggplot2::ggsave(plot = x, filename = y, device = device, path = path, scale = scale,
                           width = width, height = height, units = units, dpi = dpi, limitsize = limitsize, ...)
         })
    if(return_input) return(input)

  } else {

    if (length(filename) != 1) {
      if (length(filename) == 0) {
        cli::cli_abort("{.arg filename} cannot be empty.")
      }
      len <- length(filename)
      filename <- filename[1]
      cli::cli_warn(c(
        "{.arg filename} must have length 1, not length {len}.",
        "!" = "Only the first, {.file {filename}}, will be used."
      ))
    }

    dev <- plot_dev(device, filename, dpi = dpi)
    dim <- plot_dim(c(width, height), scale = scale, units = units,
                    limitsize = limitsize, dpi = dpi)

    if (!is.null(path)) {
      filename <- file.path(path, filename)
    }
    old_dev <- grDevices::dev.cur()
    dev(filename = filename, width = dim[1], height = dim[2], bg = bg, ...)
    on.exit(utils::capture.output({
      grDevices::dev.off()
      if (old_dev > 1) grDevices::dev.set(old_dev) # restore old device unless null device
    }))
    purrr::map(gg, ~grid::grid.draw(.x))

    invisible(filename)

    if (return_input) return(input)
  }
}
