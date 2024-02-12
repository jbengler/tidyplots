
`%+%` <- ggplot2::`%+%`
`%||%` <- rlang::`%||%`
is_waiver <- function(x) inherits(x, "waiver")

#' The pipe
#' @param lhs A value.
#' @param rhs A function call.
#' @keywords internal
#' @export
`%>%` <- dplyr::`%>%`


#' Subset data
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
#'   If `FALSE`, `NA` values are sorted to the end (like in [arrange()]), so
#'   they will only be included if there are insufficient non-missing values to
#'   reach `n`.
#' @inheritParams dplyr::slice_max
#' @export
max_rows <- function(order_by, n, by = NULL, with_ties = TRUE, na_rm = FALSE){
  . %>% dplyr::slice_max(order_by = {{order_by}}, n = n, by = {{by}}, with_ties = with_ties,
                         na_rm = na_rm)
}
#' @rdname all_rows
#' @inheritParams dplyr::slice_min
#' @export
min_rows <- function(order_by, n, by = NULL, with_ties = TRUE, na_rm = FALSE){
  . %>% dplyr::slice_min(order_by = {{order_by}}, n = n, by = {{by}}, with_ties = with_ties,
                         na_rm = na_rm)
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


#' Format numbers and p values
#' @param x bla
#' @param ... bla
#' @inheritParams scales::number
#' @inheritDotParams scales::number scale style_positive style_negative
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

min_max <- function(x) {
  x <- stats::na.omit(x)
  data.frame(ymin = min(x), ymax = max(x))
}

mean_sdl <- function(x) {
  dplyr::rename(data.frame(as.list(Hmisc::smean.sdl(x))),
                y = Mean, ymin = Lower, ymax = Upper)
}

mean_cl_boot <- function(x) {
  dplyr::rename(data.frame(as.list(Hmisc::smean.cl.boot(x))),
                y = Mean, ymin = Lower, ymax = Upper)
}


check_pipeline <- function(gg) {
  pf <- parent_function()
  # add, adjust, theme, remove, split, render, save
  # add before adjust -> warn
  # adjust_levels before adjust_colors -> warn
  # nothing after split_plot, except save_plot and show_plot -> error

  # unexpected behavior: adjust_levels(new_names) + adjust_colors(new_colors = named_vector)
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
  if (any(inherits(input, "patchwork"))) return("pw")
  else if (any(inherits(input, "gg"))) return("gg")
  else if (inherits(input, "list")) {
    if (any(unlist(purrr::map(input, inherits, "patchwork")))) {
      return("pw_list")
    } else if (any(unlist(purrr::map(input, inherits, "gg")))) {
      return("gg_list")
    }
  }
  else return("none")
}

# library(ggplot2)
#
# p1 <-
#   study %>%
#   ggplot(aes(treatment, score, color = treatment)) +
#   geom_point()
#
# p2 <- p1
# pw <- p1 + p2
# gg_list <- list(p1, p2)
# pw_list <- list(pw, pw)
#
# check_input(p1)
# check_input(p2)
# check_input(pw)
# check_input(gg_list)
# check_input(pw_list)

extract_mapping <- function(gg) {
  my_variable <- function(inp) {
    purrr::map_chr(inp, function(x) {
      if (is.null(gg$mapping[[x]])) return("!!MISSING")
      if (is.na(rlang::quo_name(gg$mapping[[x]]))) return("!!NA")
      rlang::quo_name(gg$mapping[[x]])
    })
  }
  my_scale_type <- function(inp) {
    purrr::map_chr(inp, function(x) {
      if (x == "!!MISSING") return("!!MISSING")
      if (x == "!!NA") return("!!NA")
      if (!x %in% colnames(gg$data)) cli::cli_abort("Variable '{x}' not found in supplied dataset")
      if (stringr::str_detect(x, "after_stat|after_scale|stage\\(")) return("continuous")
      gg$data[[x]] %>% ggplot2::scale_type() %>% .[[1]]
    })
  }
  dplyr::tibble(aesthetic = c("x", "y", "colour", "fill", "group")) %>%
    dplyr::mutate(
      variable = my_variable(aesthetic),
      scale_type = my_scale_type(variable)
    )
}

get_scale_type <- function(gg, aesthetic) {
  m <- gg$tidyplot$mapping
  m$scale_type[m$aesthetic == aesthetic]
}

get_variable <- function(gg, aesthetic) {
  m <- gg$tidyplot$mapping
  m$variable[m$aesthetic == aesthetic]
}

is_discrete <- function(gg, aesthetic) { get_scale_type(gg, aesthetic) == "discrete" }
is_continuous <- function(gg, aesthetic) { get_scale_type(gg, aesthetic) == "continuous" }
is_date <- function(gg, aesthetic) { get_scale_type(gg, aesthetic) == "date" }
is_time <- function(gg, aesthetic) { get_scale_type(gg, aesthetic) == "time" }
is_datetime <- function(gg, aesthetic) { get_scale_type(gg, aesthetic) == "datetime" }
is_missing <- function(gg, aesthetic) { get_scale_type(gg, aesthetic) == "!!MISSING" }
is_na <- function(gg, aesthetic) { get_scale_type(gg, aesthetic) == "!!NA" }

burst_filename <- function(filename, n) {
  digits <- ceiling(log10(n+1))
  paste0(tools::file_path_sans_ext(filename), "_", sprintf(paste0("%0",digits,"d"), 1:n), ".", tools::file_ext(filename))
}

get_layout_size <- function(gg, units = c("mm", "cm", "in")) {
  if (ggplot2::is.ggplot(gg)) gg <- list(gg)
  units <- match.arg(units)

  pages <-
    purrr::map(gg, function(x) {
      if (!ggplot2::is.ggplot(x)) stop("Please provide a ggplot or list of ggplots as input to 'gg'")
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

