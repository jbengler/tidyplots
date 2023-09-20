#' @importFrom dplyr %>%

#' @export
`%>%` <- dplyr::`%>%`

#' @export
add <- .Primitive("+")

#' @export
all_data <- function(x) { x }

#' @export
format_number <- function(x, n_decimals) {
  trimws(format(round(x, n_decimals), nsmall = n_decimals, scientific = FALSE))
}

parent_function <- function(x){
  deparse(sys.call(-2)) %>%
    stringr::str_extract(pattern = "^[A-Z_a-z]*")
}

#' @export
format_p <- function(p, n_decimals = 4) {
  ifelse(p >= 10^-n_decimals,
         format_number(p, n_decimals),
         glue::glue("< {format_number(10^-n_decimals, n_decimals)}"))
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

extract_mapping <- function(gg) {
  get_var <- function(inp) {
    purrr::map_chr(inp, function(x) {
      if (is.null(gg$mapping[[x]])) return("!!NULL")
      if (is.na(rlang::quo_name(gg$mapping[[x]]))) return("!!NA")
      rlang::quo_name(gg$mapping[[x]])
    })
  }
  get_scale_type <- function(inp) {
    purrr::map_chr(inp, function(x) {
      if(x == "!!NULL") return("!!NULL")
      if(x == "!!NA") return("!!NA")
      gg$data %>% dplyr::pull(x) %>% ggplot2::scale_type() %>% .[[1]]
    })
  }
  dplyr::tibble(mapping = c("x", "y", "colour", "fill", "group")) %>%
    dplyr::mutate(
      var = get_var(mapping),
      scale = get_scale_type(var)
    )
}

is_waiver <- function(x) inherits(x, "waiver")

get_scale <- function(gg, scale) {
  extract_mapping(gg) %>%
    dplyr::select(mapping, scale) %>%
    tibble::deframe() %>%
    .[scale]
}

get_var <- function(gg, scale) {
  extract_mapping(gg) %>%
    dplyr::select(mapping, var) %>%
    tibble::deframe() %>%
    .[scale]
}

is_discrete <- function(gg, scale) {
  if (get_scale(gg, scale) == "discrete") TRUE else FALSE }

is_continuous <- function(gg, scale) {
  if (get_scale(gg, scale) == "continuous") TRUE else FALSE }

is_date <- function(gg, scale) {
  if (get_scale(gg, scale) == "date") TRUE else FALSE }

is_time <- function(gg, scale) {
  if (get_scale(gg, scale) == "time") TRUE else FALSE }

is_datetime <- function(gg, scale) {
  if (get_scale(gg, scale) == "datetime") TRUE else FALSE }

was_not_provided <- function(gg, scale) {
  if (get_scale(gg, scale) == "!!NULL") TRUE else FALSE }

burst_filename <- function(filename, n) {
  digits <- ceiling(log10(n+1))
  paste0(tools::file_path_sans_ext(filename), "_", sprintf(paste0("%0",digits,"d"), 1:n), ".", tools::file_ext(filename))
}

