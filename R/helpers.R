`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

is_waiver <- function(x) inherits(x, "waiver")

#' The pipe
#' @param lhs A value.
#' @param rhs A function call.
#' @return The result of the calling the function `rhs` with the parameter `lhs`.
#' @keywords internal
#' @export
`%>%` <- dplyr::`%>%`

#' Flip x and y-axis
#' @param ... Arguments passed on to `ggplot2::coord_flip()`.
#' @inherit common_arguments
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' This function is superseded because in many cases, `flip_plot()` can easily
#' be replaced by swapping the `x` and `y` axis. Some plot components additionally
#' require to set the `orientation` argument to `"y"`.
#'
#' @examples
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_data_points() |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_sem_errorbar() |>
#'   flip_plot()
#'
#' energy |>
#'   tidyplot(x = year, y = energy, color = energy_type) |>
#'   add_barstack_absolute() |>
#'   flip_plot()
#'
#' # Better solutions without `flip_plot()`
#' study |>
#'   tidyplot(x = score, y = treatment, color = treatment) |>
#'   add_data_points() |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_sem_errorbar()
#'
#' energy |>
#'   tidyplot(x = energy, y = year, color = energy_type) |>
#'   add_barstack_absolute(orientation = "y")
#'
#' @export
flip_plot <- function(plot, ...) {
  plot <- check_tidyplot(plot)
  plot + ggplot2::coord_flip(...)
}

#' Subset data rows
#' @return A `function` to achieve the desired data subsetting.
#' @export
all_rows <- function() {
  function(x) {
    x
  }
}
#' @rdname all_rows
#' @inheritParams dplyr::filter
#' @export
filter_rows <- function(..., .by = NULL) {
  function(x) {
    x |> dplyr::filter(..., .by = {{ .by }}, .preserve = FALSE)
  }
}
#' @rdname all_rows
#' @param n The number of rows to select. If not are supplied, `n = 1` will be
#'   used. If `n` is greater than the number of rows in the group,
#'   the result will be silently truncated to the group size.
#'
#'   A negative value of `n` will be subtracted from the group
#'   size. For example, `n = -2` with a group of 5 rows will select 5 - 2 = 3
#'   rows.
#' @param na_rm Should missing values in `order_by` be removed from the result?
#'   If `FALSE`, `NA` values are sorted to the end (like in [dplyr::arrange()]), so
#'   they will only be included if there are insufficient non-missing values to
#'   reach `n`.
#' @inheritParams dplyr::slice_max
#'
#' @examples
#' # Highlight all animals
#' animals |>
#'  tidyplot(x = weight, y = size) |>
#'  add_data_points() |>
#'  add_data_points(data = all_rows(),
#'   color = "red", shape = 1, size = 3)
#'
#' # Highlight 3 animals with the highest weight
#' animals |>
#'  tidyplot(x = weight, y = size) |>
#'  add_data_points() |>
#'  add_data_points(data = max_rows(weight, n = 3),
#'   color = "red", shape = 1, size = 3)
#'
#' # Highlight 3 animals with the lowest weight
#' animals |>
#'  tidyplot(x = weight, y = size) |>
#'  add_data_points() |>
#'  add_data_points(data = min_rows(weight, n = 3),
#'   color = "red", shape = 1, size = 3)
#'
#' # Highlight the first 3 animals in the dataset
#' animals |>
#'  tidyplot(x = weight, y = size) |>
#'  add_data_points() |>
#'  add_data_points(data = first_rows(n = 3),
#'   color = "red", shape = 1, size = 3)
#'
#' # Highlight the last 3 animals in the dataset
#' animals |>
#'  tidyplot(x = weight, y = size) |>
#'  add_data_points() |>
#'  add_data_points(data = last_rows(n = 3),
#'   color = "red", shape = 1, size = 3)
#'
#' # Highlight 3 random animals
#' animals |>
#'  tidyplot(x = weight, y = size) |>
#'  add_data_points() |>
#'  add_data_points(data = sample_rows(n = 3),
#'   color = "red", shape = 1, size = 3)
#'
#' @export
max_rows <- function(order_by, n, by = NULL, with_ties = TRUE, na_rm = FALSE) {
  function(x) {
    x |>
      dplyr::slice_max(
        order_by = {{ order_by }},
        n = n,
        by = {{ by }},
        with_ties = with_ties,
        na_rm = na_rm
      )
  }
}
#' @rdname all_rows
#' @inheritParams dplyr::slice_min
#' @export
min_rows <- function(order_by, n, by = NULL, with_ties = TRUE, na_rm = FALSE) {
  function(x) {
    x |>
      dplyr::slice_min(
        order_by = {{ order_by }},
        n = n,
        by = {{ by }},
        with_ties = with_ties,
        na_rm = na_rm
      )
  }
}
#' @rdname all_rows
#' @inheritParams dplyr::slice_head
#' @export
first_rows <- function(n, by = NULL) {
  function(x) {
    x |> dplyr::slice_head(n = n, by = {{ by }})
  }
}
#' @rdname all_rows
#' @inheritParams dplyr::slice_tail
#' @export
last_rows <- function(n, by = NULL) {
  function(x) {
    x |> dplyr::slice_tail(n = n, by = {{ by }})
  }
}
#' @rdname all_rows
#' @inheritParams dplyr::slice_sample
#' @export
sample_rows <- function(n, by = NULL) {
  function(x) {
    x |> dplyr::slice_sample(n = n, by = {{ by }})
  }
}


#' Format p values
#' @param x A `number` to format.
#' @param accuracy A number to round to. For example, use `0.01` to show 2 decimal
#' places of precision. Defaults to `0.0001`, corresponding to 4 decimal
#' places of precision.
#' @return Formatted number as `character` string.
#'
#' @examples
#' format_p_value(0.03445553)
#' format_p_value(0.0003445553)
#' format_p_value(0.00003445553)
#'
#' @export
format_p_value <- function(x, accuracy = 0.0001) {
  ifelse(
    x >= accuracy,
    format_number(x, accuracy),
    paste0("< ", format_number(accuracy, accuracy))
  )
}


# internal helpers

var_is_null <- function(var) {
  quo_var <- rlang::enquo(var)
  rlang::quo_is_null(quo_var)
}

var_as_name <- function(var) {
  quo_var <- rlang::enquo(var)
  rlang::as_name(quo_var)
}

# <ggplot> %+% x was deprecated in ggplot2 4.0.0.
update_data <- function(plot, new_data) {
  if (utils::packageVersion("ggplot2") > "3.5.2.9000") {
    plot + new_data
  } else {
    ggplot2::`%+%`(plot, new_data)
  }
}

format_number <- function(
  x,
  accuracy = 0.1,
  big.mark = ",",
  scale_cut = NULL,
  ...
) {
  scales::number(
    x = x,
    accuracy = accuracy,
    big.mark = big.mark,
    scale_cut = scale_cut,
    ...
  )
}

mean_se <- ggplot2::mean_se

min_max <- function(x) {
  x <- stats::na.omit(x)
  data.frame(ymin = min(x), ymax = max(x))
}

mean_sd <- function(x) {
  x <- stats::na.omit(x)
  mean_x <- mean(x)
  sd_x <- stats::sd(x)

  data.frame(y = mean_x, ymin = mean_x - sd_x, ymax = mean_x + sd_x)
}

mean_cl_boot <- function(x) {
  dplyr::rename(
    data.frame(as.list(Hmisc::smean.cl.boot(x))),
    y = Mean,
    ymin = Lower,
    ymax = Upper
  )
}

tidyplot_parser <- function(text) {
  # detect expressions by a leading and trailing "$"
  if (!any(stringr::str_detect(text, "^\\$.*\\$$"), na.rm = TRUE)) {
    return(text)
  }

  out <- vector("expression", length(text))

  for (i in seq_along(text)) {
    # get rid of leading and trailing "$"
    if (stringr::str_detect(text[[i]], "^\\$.*\\$$")) {
      # check for valid plotmath expression
      expr <- tryCatch(
        parse(text = stringr::str_sub(text[[i]], 2, -2)),
        error = function(e) {
          msg <- c(
            "Invalid plotmath expression",
            "x" = conditionMessage(e),
            "i" = "Run `?plotmath` in the console for help."
          )
          cli::cli_abort(msg, call = NULL)
        }
      )
    } else {
      expr <- text[[i]]
    }

    if (length(expr) == 0) {
      out[[i]] <- NA
    } else {
      out[[i]] <- expr[[1]]
    }
  }

  out
}

tidyplot_parse_labels <- function() {
  function(text) {
    tidyplot_parser(as.character(text))
  }
}

check_input <- function(input) {
  if (any(inherits(input, "tidyplot"))) {
    return("tp")
  }
  if (any(inherits(input, "patchwork"))) {
    return("pw")
  }
  if (any(inherits(input, "gg"))) {
    return("gg")
  }

  if (is.list(input)) {
    if (any(purrr::map_lgl(input, inherits, "tidyplot"))) {
      return("tp_list")
    }
    if (any(purrr::map_lgl(input, inherits, "patchwork"))) {
      return("pw_list")
    }
    if (any(purrr::map_lgl(input, inherits, "gg"))) return("gg_list")
  }

  "none"
}

extract_mapping <- function(plot) {
  my_variable <- function(inp) {
    purrr::map_chr(inp, function(x) {
      if (is.null(plot$mapping[[x]])) {
        return("!!MISSING")
      }
      if (is.na(rlang::quo_name(plot$mapping[[x]]))) {
        return("!!NA")
      }
      rlang::quo_name(plot$mapping[[x]])
    })
  }

  my_scale_type <- function(inp) {
    purrr::map_chr(inp, function(x) {
      if (x == "!!MISSING") {
        return("!!MISSING")
      }
      if (x == "!!NA") {
        return("!!NA")
      }
      if (!x %in% colnames(plot$data)) {
        cli::cli_abort("Variable '{x}' not found in supplied dataset")
      }
      if (stringr::str_detect(x, "after_stat|after_scale|stage\\(")) {
        return("continuous")
      }
      out <- plot$data[[x]] |> ggplot2::scale_type()
      out[[1]]
    })
  }

  aesthetic <- c("x", "y", "colour", "fill", "group")
  variable <- my_variable(aesthetic)

  data.frame(
    aesthetic = aesthetic,
    variable = variable,
    scale_type = my_scale_type(variable)
  )
}

get_scale_type <- function(plot, aesthetic) {
  m <- plot$tidyplot$mapping
  m$scale_type[m$aesthetic == aesthetic]
}

get_variable <- function(plot, aesthetic) {
  m <- plot$tidyplot$mapping
  m$variable[m$aesthetic == aesthetic]
}

is_discrete <- function(plot, aesthetic) {
  get_scale_type(plot, aesthetic) %in% c("ordinal", "discrete")
}
is_continuous <- function(plot, aesthetic) {
  get_scale_type(plot, aesthetic) == "continuous"
}
is_date <- function(plot, aesthetic) {
  get_scale_type(plot, aesthetic) == "date"
}
is_time <- function(plot, aesthetic) {
  get_scale_type(plot, aesthetic) == "time"
}
is_datetime <- function(plot, aesthetic) {
  get_scale_type(plot, aesthetic) == "datetime"
}
is_missing <- function(plot, aesthetic) {
  get_scale_type(plot, aesthetic) == "!!MISSING"
}
is_na <- function(plot, aesthetic) {
  get_scale_type(plot, aesthetic) == "!!NA"
}

is_flipped <- function(plot) {
  # this will only inspect the last geom
  tail(ggplot2::ggplot_build(plot)$data, n = 1)[[1]]$flipped_aes[1]
}

# Calling this function assumes that the scale type is not "!!MISSING"
get_scale_acronym <- function(scale_type) {
  switch(
    scale_type,
    "continuous" = "c",
    "ordinal" = "d",
    "discrete" = "d",
    "time" = "t",
    "date" = "t",
    "datetime" = "t",
    cli::cli_abort("Internal error: invalid scale type \"{scale_type}\".")
  )
}

get_plottype <- function(plot) {
  scale_type_x <- get_scale_type(plot, "x")
  scale_type_y <- get_scale_type(plot, "y")

  missing_x <- scale_type_x == "!!MISSING"
  missing_y <- scale_type_y == "!!MISSING"

  if (missing_x && missing_y) {
    return("__")
  }

  # "y" can't be missing at this point if "x" is, so the following is safe
  if (missing_x) {
    pt <- paste0("_", get_scale_acronym(scale_type_y))
    return(pt)
  }

  # This is safe by the same logic as above, and will be needed in any case
  acronym_x <- get_scale_acronym(scale_type_x)

  if (missing_y) {
    pt <- paste0(acronym_x, "_")
    return(pt)
  }

  # Fallthrough if there is no early return
  paste0(acronym_x, get_scale_acronym(scale_type_y))
}

check_tidyplot <- function(
  plot,
  arg = rlang::caller_arg(plot),
  call = rlang::caller_env()
) {
  if (!inherits(plot, "tidyplot")) {
    msg <- c("{.arg {arg}} must be a tidyplot.")
    if (inherits(plot, "list") || inherits(plot, "patchwork")) {
      msg <- c(
        msg,
        "i" = "After using `split_plot()`, only `save_plot()` is allowed."
      )
    } else {
      msg <- c(msg, "i" = "Use `tidyplot()` to create a tidyplot.")
    }
    cli::cli_abort(msg, call = call)
  }

  call_stack <- sys.call(sys.parent())
  last_call <- as.character(call_stack[[1]])

  if (is.null(plot$tidyplot$call_history)) {
    call_history <- last_call
  } else {
    call_history <- c(plot$tidyplot$call_history, last_call)
  }

  plot$tidyplot$call_stack <- call_stack
  plot$tidyplot$call_history <- call_history

  plot
}

# check_tidyplot(c(22,22))

is_hex_vector <- function(x) {
  # allow NA values
  x <- x[!is.na(x)]
  all(
    is.character(x),
    stringr::str_detect(x, "[#]"),
    !stringr::str_detect(x, "[^#0-9a-fA-F]"),
    stringr::str_length(x) %in% c(4, 7, 9)
  )
}

burst_filename <- function(filename, n) {
  digits <- ceiling(log10(n + 1))
  paste0(
    tools::file_path_sans_ext(filename),
    "_",
    sprintf(paste0("%0", digits, "d"), 1:n),
    ".",
    tools::file_ext(filename)
  )
}

get_gtab_size <- function(gtab, units) {
  width <- if (any(as.character(gtab$widths) == "1null")) {
    NA_real_
  } else {
    grid::convertWidth(
      sum(gtab$widths) + ggplot2::unit(1, "mm"),
      unitTo = units,
      valueOnly = TRUE
    )
  }

  height <- if (any(as.character(gtab$heights) == "1null")) {
    NA_real_
  } else {
    grid::convertHeight(
      sum(gtab$heights) + ggplot2::unit(1, "mm"),
      unitTo = units,
      valueOnly = TRUE
    )
  }

  # Adjust for legend overflow in the orthogonal direction.
  # The legend shares the panel's row (left/right) or column (top/bottom) in
  # the gtable, so sum(heights)/sum(widths) only reflects the panel size, not
  # the legend size.  When the legend is taller/wider than the panel, we add
  # the overflow to the figure dimensions.
  tryCatch({
    side_names <- c("guide-box-right", "guide-box-left")
    tb_names   <- c("guide-box-top", "guide-box-bottom")

    get_legend_content_size <- function(idx) {
      grob <- gtab$grobs[[idx]]
      if (inherits(grob, "zeroGrob")) return(NULL)
      if (!inherits(grob, "gtable")) return(NULL)
      guides_idx <- match("guides", grob$layout$name)
      if (is.na(guides_idx)) return(NULL)
      inner <- grob$grobs[[guides_idx]]
      if (!inherits(inner, "gtable")) return(NULL)
      list(
        width  = grid::convertWidth(sum(inner$widths), unitTo = units, valueOnly = TRUE),
        height = grid::convertHeight(sum(inner$heights), unitTo = units, valueOnly = TRUE)
      )
    }

    for (nm in side_names) {
      idx <- match(nm, gtab$layout$name)
      if (!is.na(idx) && !is.na(height)) {
        legend_size <- get_legend_content_size(idx)
        if (!is.null(legend_size)) {
          span_rows <- gtab$layout$t[idx]:gtab$layout$b[idx]
          span_h <- sum(vapply(span_rows, function(r) {
            grid::convertHeight(gtab$heights[r], unitTo = units, valueOnly = TRUE)
          }, numeric(1)))
          overflow <- legend_size$height - span_h
          if (overflow > 0) height <- height + overflow
        }
      }
    }

    for (nm in tb_names) {
      idx <- match(nm, gtab$layout$name)
      if (!is.na(idx) && !is.na(width)) {
        legend_size <- get_legend_content_size(idx)
        if (!is.null(legend_size)) {
          span_cols <- gtab$layout$l[idx]:gtab$layout$r[idx]
          span_w <- sum(vapply(span_cols, function(c) {
            grid::convertWidth(gtab$widths[c], unitTo = units, valueOnly = TRUE)
          }, numeric(1)))
          overflow <- legend_size$width - span_w
          if (overflow > 0) width <- width + overflow
        }
      }
    }
  }, error = function(e) NULL)

  c(width = width, height = height)
}

get_layout_size <- function(plot, units = c("mm", "cm", "in")) {
  if (ggplot2::is_ggplot(plot)) {
    plot <- list(plot)
  }
  units <- match.arg(units)

  sizes <- plot |>
    purrr::map(function(x) {
      if (!ggplot2::is_ggplot(x)) {
        cli::cli_abort(
          "Argument {.arg plot} must be a {.pkg ggplot} or list of {.pkg ggplots}"
        )
      }
      get_gtab_size(ggplot2::ggplotGrob(x), units)
    })

  # Extract values implicitly by passing the strings as shorthands
  pages <- data.frame(
    width = purrr::map_dbl(sizes, "width"),
    height = purrr::map_dbl(sizes, "height")
  )

  list(
    units = units,
    pages = pages,
    max = c(width = max(pages$width), height = max(pages$height))
  )
}


# Helpers for proportional scaling during interactive display ----------------

render_for_viewer <- function(plot, ...) {
  # Capture the original device first, before any operations that might
  # interact with the device stack (including ggplotGrob/get_gtab_size).
  prev_dev <- grDevices::dev.cur()

  unit_str <- plot$tidyplot$unit %||% "mm"

  # Strip the tidyplot class and build the grob once — reused for both layout
  # measurement and rendering to avoid a double ggplotGrob() call.
  plain <- plot
  class(plain) <- class(plain)[class(plain) != "tidyplot"]
  gtab <- ggplot2::ggplotGrob(plain)

  # Measure the FULL figure: panel + axes + legend + margins.
  # plot$tidyplot$width/height are panel-only dimensions; rendering at that size
  # clips the legend and axis labels.
  sizes <- get_gtab_size(gtab, unit_str)

  fig_w <- if (is.na(sizes[["width"]])) {
    plot$tidyplot$width
  } else {
    sizes[["width"]]
  }

  fig_h <- if (is.na(sizes[["height"]])) {
    plot$tidyplot$height
  } else {
    sizes[["height"]]
  }

  # Render at the full figure dimensions to a temp PNG file.
  # We track the PNG device number explicitly (png_dev) so it can be closed
  # by identity rather than by dev.cur() comparison. grid.draw.gTree calls
  # grDevices::recordGraphics() internally which can change dev.cur() back to
  # prev_dev while the PNG device is still open, causing the conditional
  # dev.cur() != prev_dev check to silently skip dev.off() and leak the device.
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  png_dev <- NA_integer_

  tryCatch(
    {
      grDevices::png(tmp, width = fig_w, height = fig_h, units = unit_str, res = 300)
      png_dev <- grDevices::dev.cur()
      grid::grid.newpage()
      grid::grid.draw(gtab)
    },
    finally = {
      if (!is.na(png_dev) && png_dev %in% grDevices::dev.list()) {
        grDevices::dev.off(png_dev)
      }
      if (grDevices::dev.cur() != prev_dev && prev_dev > 1L) {
        tryCatch(grDevices::dev.set(prev_dev), error = function(e) NULL)
      }
    }
  )

  img <- png::readPNG(tmp)

  in_per_unit <- switch(
    unit_str,
    "in" = 1,
    "cm" = 1 / 2.54,
    "mm" = 1 / 25.4,
    "px" = 1 / 72,
    1 / 25.4
  )

  fig_w_in <- fig_w * in_per_unit
  fig_h_in <- fig_h * in_per_unit

  # `recordGraphics()` executes the expression immediately AND re-executes it on
  # every display-list replay (i.e. pane resize in RStudio/Positron). On each
  # execution, `dev.size("in")` returns the *current* device dimensions, so the
  # fractions are recomputed for the actual pane size and the raster always
  # fills the pane while preserving the plot's aspect ratio — exactly like
  # zooming in and out of a PNG. `img`, `fig_w_in`, and `fig_h_in` are bundled
  # in `list` so they are available during replay without re-opening any
  # devices.

  # Note that `grid.newpage()` is called OUTSIDE `recordGraphics()` so it is
  # recorded normally as a `[newpage]` entry.  This is important:
  # `grid.newpage()` resets the device display list via `GEinitDisplayList()`.
  # If it were called INSIDE the `recordGraphics()` expression, it would wipe
  # the `[recordGraphics(expr)]` entry after the first replay, leaving only a
  # static display list that stretches. With newpage outside, the display list
  # is: `[newpage, recordGraphics(expr)]`. On every pane resize RStudio replays
  # `[newpage]` (clears screen) then `[recordGraphics(expr)]`, so it
  # re-evaluates `dev.size()` and draws correctly.
  grid::grid.newpage()

  grDevices::recordGraphics(
    {
      dw <- grDevices::dev.size("in")[1]
      dh <- grDevices::dev.size("in")[2]
      if (!is.finite(dw) || dw <= 0) {
        dw <- fig_w_in
      }
      if (!is.finite(dh) || dh <= 0) {
        dh <- fig_h_in
      }
      ar_target <- fig_h_in / fig_w_in
      ar_device <- dh / dw
      if (ar_target > ar_device) {
        frac_w <- ar_device / ar_target
        frac_h <- 1.0
      } else {
        frac_w <- 1.0
        frac_h <- ar_target / ar_device
      }
      grid::grid.draw(
        grid::rasterGrob(
          img,
          x = grid::unit(0.5, "npc"),
          y = grid::unit(0.5, "npc"),
          just = "centre",
          width = grid::unit(frac_w, "npc"),
          height = grid::unit(frac_h, "npc")
        ),
        recording = FALSE
      )
    },
    list(img = img, fig_w_in = fig_w_in, fig_h_in = fig_h_in),
    asNamespace("tidyplots")
  )
}
