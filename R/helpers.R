
`%+%` <- ggplot2::`%+%`
`%||%` <- rlang::`%||%`
is_waiver <- function(x) inherits(x, "waiver")

#' @keywords internal
#' @export
`%>%` <- dplyr::`%>%`

#' Give back all data
#' @export
all_data <- function(){
  function(x) { x }
}

#' Format numbers and p values
#' @param ... bla
#' @inheritParams scales::number
#' @inheritDotParams scales::number scale style_positive style_negative
#' @export
format_number <- function(x, accuracy = 0.1, big.mark =",", scale_cut = scales::cut_short_scale(), ...) {
  scales::number(x = x, accuracy = accuracy, big.mark = big.mark, scale_cut = scale_cut, ...)
}

#' @rdname format_number
#' @export
format_p_value <- function(p, accuracy = 0.0001) {
  ifelse(p >= accuracy,
         format_number(p, accuracy),
         glue::glue("< { format_number(accuracy, accuracy) }"))
}

# internal helpers

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

parent_function <- function(x){
  deparse(sys.call(-2)) %>%
    stringr::str_extract(pattern = "^[A-Z_a-z]*")
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
#   tidyplot(category, value, color = category) %>%
#   add_bar()
#
# p2 <-
#   df_demo %>%
#   tidyplot(category, value, color = category) %>%
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
  # m <- extract_mapping(gg)
  m <- gg$tidyplot$mapping
  m$scale_type[m$aesthetic == aesthetic]
}

get_variable <- function(gg, aesthetic) {
  # m <- extract_mapping(gg)
  m <- gg$tidyplot$mapping
  m$variable[m$aesthetic == aesthetic]
}

is_discrete <- function(gg, aesthetic) { get_scale_type(gg, aesthetic) == "discrete" }
is_continuous <- function(gg, aesthetic) { get_scale_type(gg, aesthetic) == "continuous" }
is_date <- function(gg, aesthetic) { get_scale_type(gg, aesthetic) == "date" }
is_time <- function(gg, aesthetic) { get_scale_type(gg, aesthetic) == "time" }
is_datetime <- function(gg, aesthetic) { get_scale_type(gg, aesthetic) == "datetime" }
was_not_provided <- function(gg, aesthetic) { get_scale_type(gg, aesthetic) == "!!MISSING" }

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

