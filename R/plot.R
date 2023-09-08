#' @importFrom ggplot2 ggplot aes is.ggplot ggtitle

extract_mapping <- function(gg) {
  get_var <- function(inp) {
    purrr::map_chr(inp, function(x) {
      if (is.null(gg$mapping[[x]])) return("!!NULL")
      if (is.na(rlang::quo_name(gg$mapping[[x]]))) return("!!NA")
      return(rlang::quo_name(gg$mapping[[x]]))
    })
  }
  get_scale_type <- function(inp) {
    purrr::map_chr(inp, function(x) {
      if(x == "!!NULL") return("!!NULL")
      if(x == "!!NA") return("!!NA")
      return(gg$data %>% dplyr::pull(x) %>% ggplot2::scale_type())
    })
  }
  dplyr::tibble(mapping = c("x", "y", "colour", "fill", "group")) %>%
    dplyr::mutate(
      var = get_var(mapping),
      scale = get_scale_type(var)
      )
}

is_discrete <- function(gg, scale) {
  item <-
    extract_mapping(gg) %>%
    dplyr::select(mapping, scale) %>%
    tibble::deframe() %>%
    .[scale]
  if (item == "discrete") return(TRUE) else return(FALSE)
}

is_continuous <- function(gg, scale) {
  item <-
    extract_mapping(gg) %>%
    dplyr::select(mapping, scale) %>%
    tibble::deframe() %>%
    .[scale]
  if (item == "continuous") return(TRUE) else return(FALSE)
}

#' @export
tidy_plot <- function(data, ...) {
  gg <- ggplot(data = data, mapping = aes(...))

  if (is_discrete(gg, "colour"))
    gg <- gg + my_scale_color_d()

  if (is_discrete(gg, "fill"))
    gg <- gg + my_scale_fill_d()

  if (is_continuous(gg, "colour"))
    gg <- gg + my_scale_color_c()

  if (is_continuous(gg, "fill"))
    gg <- gg + my_scale_fill_c()

  gg <- gg %>%
    theme_default() %>%
    modify_size()

  return(gg)
}

#' @export
plot_view <- function(gg, ...) {
  print(gg, ...)
  return(gg)
}

burst_filename <- function(filename, n) {
  digits <- ceiling(log10(n+1))
  paste0(tools::file_path_sans_ext(filename), "_", sprintf(paste0("%0",digits,"d"), 1:n), ".", tools::file_ext(filename))
}

check_input <- function(input) {
  if (any(class(input) == "patchwork")) return("pw")
  else if (any(class(input) == "gg")) return("gg")
  else if (class(input) == "list") {
    if (any(unlist(purrr::map(input, class)) == "patchwork")) {
      return("pw_list")
    } else if (any(unlist(purrr::map(input, class)) == "gg")) {
        return("gg_list")
      }
    }
  else return("none")
}

# p1 <-
#   df_demo %>%
#   tidy_plot(category, value, color = category) %>%
#   add_bar()
#
# p2 <-
#   df_demo %>%
#   tidy_plot(category, value, color = category) %>%
#   add_mean()
#
# pw <- p1 + p2
#
# unlist(purrr::map(gg_list, class))
#
# gg_list <- list(p1, p2)
# pw_list <- list(pw, pw)
#
# check_input(p1)
# check_input(p2)
# check_input(pw)
# check_input(gg_list)
# check_input(pw_list)



# Known limitations:
# 1. Plots that have been sized by egg::setpanel_size() loose their gtable information
# 2. patchwork::plot_annotation() titles are not counting towards the dimensions

get_layout_size <- function(gg, units = c("mm", "cm", "in")) {
  if (is.ggplot(gg)) gg <- list(gg)
  units <- match.arg(units)

  pages <-
    purrr::map(gg, function(x) {
      if (!is.ggplot(x)) stop("Please provide a ggplot or list of ggplots as input to 'gg'")
      gtab <- patchwork::patchworkGrob(x)

      width <- NA
      height <- NA
      if (all(as.character(gtab$widths) != "1null"))
        width <- grid::convertWidth(sum(gtab$widths) + unit(1, "mm"), unitTo = units, valueOnly = TRUE)
      if (all(as.character(gtab$heights) != "1null"))
        height <- grid::convertHeight(sum(gtab$heights) + unit(1, "mm"), unitTo = units, valueOnly = TRUE)

      dplyr::tibble(width = width, height = height)
    }) %>%
    dplyr::bind_rows()

  overall_width<- NA
  overall_height <- NA
  if (all(!is.na(pages$width)))
    overall_width <- max(pages$width, na.rm = TRUE)
  if (all(!is.na(pages$height)))
    overall_height <- max(pages$height, na.rm = TRUE)

  list(
    units = units,
    pages = pages,
    max = c(width = overall_width, height = overall_height)
    )
}

#' multipage_plots
#' @param gg A `ggplot` or list of `ggplot`s
#'
#' @param ncol,nrow The number of columns and rows per page.
#' @inheritParams patchwork::wrap_plots
#'
#' @export
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
  if (!is.ggplot(gg) && !all(purrr::map_lgl(gg, is.ggplot)))
    stop("argument 'gg' should be ggplot or list off ggplots")
  if (is.ggplot(gg)) gg <- list(gg)

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

#' multipage_facets
#' @param gg A `ggplot`
#'
#' @param by Variable that should be used for faceting.
#' @param ncol,nrow The number of columns and rows per page.
#' @inheritParams patchwork::wrap_plots
#'
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
  if (!is.ggplot(gg))
    stop("argument 'gg' should be a single ggplot")
  if(missing(by))
    stop("argument 'by' missing without default")

  # free plot dimensions
  gg <-
    gg %>%
    modify_size(width = NA, height = NA)

  df <-
    gg$data %>%
    tidyr::nest(data = -{{by}}) %>%
    dplyr::arrange({{by}})
  plots <-
    purrr::map2(df$data, df %>% dplyr::pull({{by}}),
         function(data, facet_title) {
           gg %+% data + ggtitle(facet_title)
         })
  cli::cli_alert_success("split_plot: {.pkg widths} = {widths} {my_unit}, {.pkg heights} = {heights} {my_unit}")
  if (!is.na(widths)) widths <- unit(widths, my_unit)
  if (!is.na(heights)) heights <- unit(heights, my_unit)
  multipage_plots(plots, ncol = ncol, nrow = nrow, widths = widths, heights = heights, my_unit = my_unit, guides = guides, byrow = byrow, tag_level = tag_level, design = design)
}

#' Save multipage layout to file
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
  if (!is.ggplot(gg) && !all(purrr::map_lgl(gg, is.ggplot)))
    stop("argument 'gg' should be ggplot or list off ggplots")

  input <- gg

  if (is.ggplot(gg)) {
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

  cli::cli_alert_success("save_plot: {.pkg device width} {width_defined_by}")
  cli::cli_alert_success("save_plot: {.pkg device height} {height_defined_by}")

  if (!is.na(width) && !is.na(height))
    cli::cli_alert_success("save_plot: saving {.pkg {round(width)} x {round(height)}} mm image to {.pkg {filename}}")

  if (multiple_files) {
    filenames <- burst_filename(filename, length(gg))
    purrr::map2(gg, filenames,
         function(x, y) {
           ggplot2::ggsave(plot = x, filename = y, device = device, path = path, scale = scale,
                           width = width, height = height, units = units, dpi = dpi, limitsize = limitsize, ...)
         })
    if(return_input) return(input)

  } else {

    # this code is from ggplot2::ggsave()
    # TODO: get around using unexported ::: ggplot2 functions
    # like so? https://groups.google.com/g/ggplot2/c/l6Im1jpNavI?pli=1
    # https://github.com/tidyverse/ggplot2/issues/5093
    # https://github.com/thomasp85/patchwork/issues/127

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

# this function is from ggplot2
plot_dim <- function(dim = c(NA, NA), scale = 1, units = "in",
                     limitsize = TRUE, dpi = 300, call = caller_env()) {
  units <- rlang::arg_match0(units, c("in", "cm", "mm", "px"))
  to_inches <- function(x) x / c(`in` = 1, cm = 2.54, mm = 2.54 * 10, px = dpi)[units]
  from_inches <- function(x) x * c(`in` = 1, cm = 2.54, mm = 2.54 * 10, px = dpi)[units]

  dim <- to_inches(dim) * scale

  if (any(is.na(dim))) {
    if (length(grDevices::dev.list()) == 0) {
      default_dim <- c(7, 7)
    } else {
      default_dim <- grDevices::dev.size() * scale
    }
    dim[is.na(dim)] <- default_dim[is.na(dim)]
    dim_f <- prettyNum(from_inches(dim), digits = 3)

    cli::cli_inform("Saving {dim_f[1]} x {dim_f[2]} {units} image")
  }

  if (limitsize && any(dim >= 50)) {
    units <- switch(
      units,
      "in" = "inches",
      "cm" = "centimeters",
      "mm" = "millimeters",
      "px" = "pixels"
    )
    msg <- paste0(
      "Dimensions exceed 50 inches ({.arg height} and {.arg width} are ",
      "specified in {.emph {units}}"
    )
    if (units == "pixels") {
      msg <- paste0(msg, ").")
    } else {
      msg <- paste0(msg, " not pixels).")
    }
    cli::cli_abort(c(
      msg,
      "i" = "If you're sure you want a plot that big, use {.code limitsize = FALSE}.
    "), call = call)
  }

  dim
}

# this function is from ggplot2
plot_dev <- function(device, filename = NULL, dpi = 300, call = caller_env()) {
  force(filename)
  force(dpi)

  if (is.function(device)) {
    args <- formals(device)
    call_args <- list()
    if ("file" %in% names(args)) {
      call_args$file <- filename
      call_args["filename"] <- list(NULL)
    }
    if ("res" %in% names(args)) {
      call_args$res <- dpi
    }
    if ("units" %in% names(args)) {
      call_args$units <- 'in'
    }
    dev <- function(...) {
      args <- modify_list(list(...), call_args)
      inject(device(!!!args))
    }
    return(dev)
  }

  eps <- function(filename, ...) {
    grDevices::postscript(file = filename, ..., onefile = FALSE, horizontal = FALSE,
                          paper = "special")
  }
  if (requireNamespace('ragg', quietly = TRUE)) {
    png_dev <- absorb_grdevice_args(ragg::agg_png)
    jpeg_dev <- absorb_grdevice_args(ragg::agg_jpeg)
    tiff_dev <- absorb_grdevice_args(ragg::agg_tiff)
  } else {
    png_dev <- grDevices::png
    jpeg_dev <- grDevices::jpeg
    tiff_dev <- grDevices::tiff
  }
  devices <- list(
    eps =  eps,
    ps =   eps,
    tex =  function(filename, ...) grDevices::pictex(file = filename, ...),
    pdf =  function(filename, ..., version = "1.4") grDevices::pdf(file = filename, ..., version = version),
    svg =  function(filename, ...) svglite::svglite(file = filename, ...),
    # win.metafile() doesn't have `bg` arg so we need to absorb it before passing `...`
    emf =  function(..., bg = NULL) grDevices::win.metafile(...),
    wmf =  function(..., bg = NULL) grDevices::win.metafile(...),
    png =  function(...) png_dev(..., res = dpi, units = "in"),
    jpg =  function(...) jpeg_dev(..., res = dpi, units = "in"),
    jpeg = function(...) jpeg_dev(..., res = dpi, units = "in"),
    bmp =  function(...) grDevices::bmp(..., res = dpi, units = "in"),
    tiff = function(...) tiff_dev(..., res = dpi, units = "in"),
    tif  = function(...) tiff_dev(..., res = dpi, units = "in")
  )

  if (is.null(device)) {
    device <- tolower(tools::file_ext(filename))
    if (identical(device, "")) {
      cli::cli_abort("{.arg filename} has no file extension and {.arg device} is {.val NULL}.", call = call)
    }
  }

  if (!is.character(device) || length(device) != 1) {
    stop_input_type(device, "a string, function", allow_null = TRUE, call = call)
  }

  dev <- devices[[device]]
  if (is.null(dev)) {
    cli::cli_abort("Unknown graphics device {.val {device}}", call = call)
  }
  dev
}

# this function is from ggplot2
absorb_grdevice_args <- function(f) {
  function(..., type, antialias) {
    if (!missing(type) || !missing(antialias)) {
      cli::cli_warn("Using ragg device as default. Ignoring {.arg type} and {.arg antialias} arguments")
    }
    f(...)
  }
}
