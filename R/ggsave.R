# This is to avoid referring to of the unexported ggplot2 functions that
# are needed for tidyplots::save_plot() to work.

# These are:
# 1) ggplot2:::plot_dim()
# 2) ggplot2:::plot_dev()

# https://groups.google.com/g/ggplot2/c/l6Im1jpNavI?pli=1
# https://github.com/tidyverse/ggplot2/issues/5093
# https://github.com/thomasp85/patchwork/issues/127

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
      args <- adjust_list(list(...), call_args)
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
