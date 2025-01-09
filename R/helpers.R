
`%+%` <- ggplot2::`%+%`
`%||%` <- rlang::`%||%`
is_waiver <- function(x) inherits(x, "waiver")

#' The pipe
#' @param lhs A value.
#' @param rhs A function call.
#' @return The result of the calling the function `rhs` with the parameter `lhs`.
#' @keywords internal
#' @export
`%>%` <- dplyr::`%>%`

#' Convert ggplot to tidyplot
#'
#' @param gg A ggplot.
#' @inherit common_arguments
#'
#' @examples
#' gg <-
#'   study %>%
#'   ggplot2::ggplot(ggplot2::aes(x = treatment, y = score, color = treatment)) +
#'   ggplot2::geom_point()
#'
#' gg
#'
#' gg %>% as_tidyplot()
#'
#' @export
as_tidyplot <- function(gg, width = 50, height = 50, dodge_width = NULL) {
  mapping <- gg$mapping
  plot <- gg

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

  plot$mapping <- mapping
  class(plot) <- c("tidyplot", class(plot))

  plot$tidyplot$mapping <- extract_mapping(plot)

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
  dodge_width <- dodge_width %||% dodge_width_heuristic
  plot$tidyplot$dodge_width <- dodge_width

  plot$tidyplot$named_colors <- NULL

  plot <- plot %>%
    theme_tidyplot() %>%
    adjust_x_axis() %>%
    adjust_y_axis() %>%
    adjust_colors() %>%
    adjust_size(width = width, height = height)

  if (single_color_plot)
    plot <- plot %>% remove_legend()

  plot
}

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
#' study %>%
#'   tidyplot(x = treatment, y = score, color = treatment) %>%
#'   add_data_points() %>%
#'   add_mean_bar(alpha = 0.4) %>%
#'   add_sem_errorbar() %>%
#'   flip_plot()
#'
#' energy %>%
#'   tidyplot(x = year, y = energy, color = energy_type) %>%
#'   add_barstack_absolute() %>%
#'   flip_plot()
#'
#' # Better solutions without `flip_plot()`
#' study %>%
#'   tidyplot(x = score, y = treatment, color = treatment) %>%
#'   add_data_points() %>%
#'   add_mean_bar(alpha = 0.4) %>%
#'   add_sem_errorbar()
#'
#' energy %>%
#'   tidyplot(x = energy, y = year, color = energy_type) %>%
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
all_rows <- function(){
  function(x) { x }
}
#' @rdname all_rows
#' @inheritParams dplyr::filter
#' @export
filter_rows <- function(..., .by = NULL){
  . %>% dplyr::filter(..., .by = {{.by}}, .preserve = FALSE)
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
#' animals %>%
#'  tidyplot(x = weight, y = size) %>%
#'  add_data_points() %>%
#'  add_data_points(data = all_rows(),
#'   color = "red", shape = 1, size = 3)
#'
#' # Highlight 3 animals with the highest weight
#' animals %>%
#'  tidyplot(x = weight, y = size) %>%
#'  add_data_points() %>%
#'  add_data_points(data = max_rows(weight, n = 3),
#'   color = "red", shape = 1, size = 3)
#'
#' # Highlight 3 animals with the lowest weight
#' animals %>%
#'  tidyplot(x = weight, y = size) %>%
#'  add_data_points() %>%
#'  add_data_points(data = min_rows(weight, n = 3),
#'   color = "red", shape = 1, size = 3)
#'
#' # Highlight the first 3 animals in the dataset
#' animals %>%
#'  tidyplot(x = weight, y = size) %>%
#'  add_data_points() %>%
#'  add_data_points(data = first_rows(n = 3),
#'   color = "red", shape = 1, size = 3)
#'
#' # Highlight the last 3 animals in the dataset
#' animals %>%
#'  tidyplot(x = weight, y = size) %>%
#'  add_data_points() %>%
#'  add_data_points(data = last_rows(n = 3),
#'   color = "red", shape = 1, size = 3)
#'
#' # Highlight 3 random animals
#' animals %>%
#'  tidyplot(x = weight, y = size) %>%
#'  add_data_points() %>%
#'  add_data_points(data = sample_rows(n = 3),
#'   color = "red", shape = 1, size = 3)
#'
#' @export
max_rows <- function(order_by, n, by = NULL, with_ties = TRUE, na_rm = FALSE){
  . %>% dplyr::slice_max(order_by = {{order_by}}, n = n, by = {{by}}, with_ties = with_ties, na_rm = na_rm)
}
#' @rdname all_rows
#' @inheritParams dplyr::slice_min
#' @export
min_rows <- function(order_by, n, by = NULL, with_ties = TRUE, na_rm = FALSE){
  . %>% dplyr::slice_min(order_by = {{order_by}}, n = n, by = {{by}}, with_ties = with_ties, na_rm = na_rm)
}
#' @rdname all_rows
#' @inheritParams dplyr::slice_head
#' @export
first_rows <- function(n, by = NULL){
  . %>% dplyr::slice_head(n = n, by = {{by}})
}
#' @rdname all_rows
#' @inheritParams dplyr::slice_tail
#' @export
last_rows <- function(n, by = NULL){
  . %>% dplyr::slice_tail(n = n, by = {{by}})
}
#' @rdname all_rows
#' @inheritParams dplyr::slice_sample
#' @export
sample_rows <- function(n, by = NULL){
  . %>% dplyr::slice_sample(n = n, by = {{by}})
}


#' Format numbers or p values
#' @param x A `number` to format.
#' @param ... Arguments passed on to `scales::number()`.
#' @inheritParams scales::number
#' @inheritDotParams scales::number scale style_positive style_negative
#' @return Formatted number as `character` string.
#'
#' @examples
#' format_number(232342.3443)
#'
#' format_number(232342.3443, accuracy = 0.01)
#'
#' format_number(232342.3443, accuracy = 1, big.mark = "")
#'
#' format_p_value(0.03445553)
#'
#' format_p_value(0.0003445553)
#'
#' format_p_value(0.00003445553)
#'
#' @export
format_number <- function(x, accuracy = 0.1, big.mark =",", scale_cut = NULL, ...) {
  scales::number(x = x, accuracy = accuracy, big.mark = big.mark, scale_cut = scale_cut, ...)
}
#' @rdname format_number
#' @export
format_p_value <- function(x, accuracy = 0.0001) {
  ifelse(x >= accuracy,
         format_number(x, accuracy),
         glue::glue("< { format_number(accuracy, accuracy) }"))
}

# internal helpers

mean_se <- ggplot2::mean_se

min_max <- function(x) {
  x <- stats::na.omit(x)
  data.frame(ymin = min(x), ymax = max(x))
}

mean_sd <- function(x) {
  x <- stats::na.omit(x)
  data.frame(y = mean(x),
             ymin = mean(x) - stats::sd(x),
             ymax = mean(x) + stats::sd(x))
}

mean_cl_boot <- function(x) {
  dplyr::rename(data.frame(as.list(Hmisc::smean.cl.boot(x))),
                y = Mean, ymin = Lower, ymax = Upper)
}

tidyplot_parser <- function(text) {
  # detect expressions by a leading and trailing "$"
  # message(text) # for debugging
  if(any(stringr::str_detect(text, "^\\$.*\\$$"), na.rm = TRUE)) {
    out <- vector("expression", length(text))
    for (i in seq_along(text)) {
      # get rid of leading and trailing "$"
      if (stringr::str_detect(text[[i]], "^\\$.*\\$$"))
        expr <- parse(text = stringr::str_sub(text[[i]], 2, -2))
      else
        expr <- text[[i]]
      out[[i]] <- if (length(expr) == 0)
        NA
      else expr[[1]]
    }
  } else {
    out <- text
  }
  out
}

tidyplot_parse_labels <- function() {
  function(text) {
    tidyplot_parser(as.character(text))
  }
}

parent_function <- function(level = 0){
  deparse(sys.call(-2 + level)) %>%
    stringr::str_extract(pattern = "^[A-Z_a-z]*")
}

check_input <- function(input) {
  if (any(inherits(input, "tidyplot"))) return("tp")
  if (any(inherits(input, "patchwork"))) return("pw")
  if (any(inherits(input, "gg"))) return("gg")
  if (inherits(input, "list")) {
    if (any(unlist(purrr::map(input, inherits, "tidyplot")))) return("tp_list")
    if (any(unlist(purrr::map(input, inherits, "patchwork")))) return("pw_list")
    if (any(unlist(purrr::map(input, inherits, "gg")))) return("gg_list")
  }
  else return("none")
}

extract_mapping <- function(plot) {
  my_variable <- function(inp) {
    purrr::map_chr(inp, function(x) {
      if (is.null(plot$mapping[[x]])) return("!!MISSING")
      if (is.na(rlang::quo_name(plot$mapping[[x]]))) return("!!NA")
      rlang::quo_name(plot$mapping[[x]])
    })
  }
  my_scale_type <- function(inp) {
    purrr::map_chr(inp, function(x) {
      if (x == "!!MISSING") return("!!MISSING")
      if (x == "!!NA") return("!!NA")
      if (!x %in% colnames(plot$data)) cli::cli_abort("Variable '{x}' not found in supplied dataset")
      if (stringr::str_detect(x, "after_stat|after_scale|stage\\(")) return("continuous")
      plot$data[[x]] %>% ggplot2::scale_type() %>% .[[1]]
    })
  }
  dplyr::tibble(aesthetic = c("x", "y", "colour", "fill", "group")) %>%
    dplyr::mutate(
      variable = my_variable(aesthetic),
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

is_discrete <- function(plot, aesthetic) { get_scale_type(plot, aesthetic) == "discrete" }
is_continuous <- function(plot, aesthetic) { get_scale_type(plot, aesthetic) == "continuous" }
is_date <- function(plot, aesthetic) { get_scale_type(plot, aesthetic) == "date" }
is_time <- function(plot, aesthetic) { get_scale_type(plot, aesthetic) == "time" }
is_datetime <- function(plot, aesthetic) { get_scale_type(plot, aesthetic) == "datetime" }
is_missing <- function(plot, aesthetic) { get_scale_type(plot, aesthetic) == "!!MISSING" }
is_na <- function(plot, aesthetic) { get_scale_type(plot, aesthetic) == "!!NA" }

is_flipped <- function(plot) {
  # this will only inspect the last geom
  tail(ggplot2::ggplot_build(plot)$data, n = 1)[[1]]$flipped_aes[1]
}

get_plottype <- function(plot) {
  pt <- "none"

  if (is_missing(plot, "x") && is_missing(plot, "y")) pt <- "__"

  if (!is_missing(plot, "x") && is_missing(plot, "y")) {
    if (is_time(plot, "x")) pt <- "t_"
    if (is_date(plot, "x")) pt <- "t_"
    if (is_datetime(plot, "x")) pt <- "t_"
    if (is_continuous(plot, "x")) pt <- "c_"
    if (is_discrete(plot, "x")) pt <- "d_"
  }

  if (!is_missing(plot, "y") && is_missing(plot, "x")) {
    if (is_time(plot, "y")) pt <- "_t"
    if (is_date(plot, "y")) pt <- "_t"
    if (is_datetime(plot, "y")) pt <- "_t"
    if (is_continuous(plot, "y")) pt <- "_c"
    if (is_discrete(plot, "y")) pt <- "_d"
  }

  if (is_discrete(plot, "x") && is_continuous(plot, "y")) pt <- "dc"
  if (is_continuous(plot, "x") && is_discrete(plot, "y")) pt <- "cd"
  if (is_continuous(plot, "x") && is_continuous(plot, "y")) pt <- "cc"
  if (is_discrete(plot, "x") && is_discrete(plot, "y")) pt <- "dd"

  if (is_time(plot, "x") && is_continuous(plot, "y")) pt <- "tc"
  if (is_date(plot, "x") && is_continuous(plot, "y")) pt <- "tc"
  if (is_datetime(plot, "x") && is_continuous(plot, "y")) pt <- "tc"

  if (is_continuous(plot, "x") && is_time(plot, "y")) pt <- "ct"
  if (is_continuous(plot, "x") && is_date(plot, "y")) pt <- "ct"
  if (is_continuous(plot, "x") && is_datetime(plot, "y")) pt <- "ct"

  if (is_time(plot, "x") && is_discrete(plot, "y")) pt <- "td"
  if (is_date(plot, "x") && is_discrete(plot, "y")) pt <- "td"
  if (is_datetime(plot, "x") && is_discrete(plot, "y")) pt <- "td"

  if (is_discrete(plot, "x") && is_time(plot, "y")) pt <- "dt"
  if (is_discrete(plot, "x") && is_date(plot, "y")) pt <- "dt"
  if (is_discrete(plot, "x") && is_datetime(plot, "y")) pt <- "dt"
  pt
}

check_tidyplot <- function(plot, arg = rlang::caller_arg(plot), call = rlang::caller_env()) {
  if (!inherits(plot, "tidyplot")) {
    msg <- c("{.arg {arg}} must be a tidyplot.")
    if (inherits(plot, "list") || inherits(plot, "patchwork"))
      msg <- c(msg, "i" = "After using `split_plot()`, only `save_plot()` is allowed.")
    else
      msg <- c(msg, "i" = "Use `tidyplot()` to create a tidyplot.")
    cli::cli_abort(msg, call = call)
  }
  # message(parent_function(-1))
  plot$tidyplot$history <- c(plot$tidyplot$history, parent_function())
  plot
}

# check_tidyplot(c(22,22))

is_hex_vector <- function(x) {
  all(
    is.character(x),
    stringr::str_detect(x, "[#]"),
    !stringr::str_detect(x, "[^#0-9a-fA-F]"),
    stringr::str_length(x) %in% c(4, 7, 9)
  )
}

burst_filename <- function(filename, n) {
  digits <- ceiling(log10(n+1))
  paste0(tools::file_path_sans_ext(filename), "_", sprintf(paste0("%0",digits,"d"), 1:n), ".", tools::file_ext(filename))
}

get_layout_size <- function(plot, units = c("mm", "cm", "in")) {
  if (ggplot2::is.ggplot(plot)) plot <- list(plot)
  units <- match.arg(units)

  pages <-
    purrr::map(plot, function(x) {
      if (!ggplot2::is.ggplot(x)) cli::cli_abort("Argument {.arg plot} must be a {.pkg ggplot} or list of {.pkg ggplots}")
      gtab <- patchwork::patchworkGrob(x)

      width <- NA
      height <- NA
      if (all(as.character(gtab$widths) != "1null"))
        width <- grid::convertWidth(sum(gtab$widths) + ggplot2::unit(1, "mm"), unitTo = units, valueOnly = TRUE)
      if (all(as.character(gtab$heights) != "1null"))
        height <- grid::convertHeight(sum(gtab$heights) + ggplot2::unit(1, "mm"), unitTo = units, valueOnly = TRUE)

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

