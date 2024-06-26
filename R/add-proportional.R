## Pie function factory
ff_pie <- function(.type = "pie") {
  function(plot, width = 1, reverse = FALSE, ...) {
    check_tidyplot(plot)
    plot <-
      plot %>%
      remove_plot_area_padding() %>%
      style_void()

    if (!is_missing(plot, "x")) stop("add_pie() and add_donut() accept color and y, but not x.")

    if (is_missing(plot, "y")) {
      plot <- plot + ggplot2::geom_bar(ggplot2::aes(x = NA), position = ggplot2::position_fill(reverse = reverse),
                                       width = width, color = NA, ...)
    } else {
      plot <- plot + ggplot2::stat_summary(ggplot2::aes(x = NA), geom = "bar", fun = sum,
                                           position = ggplot2::position_fill(reverse = reverse),
                                           width = width, color = NA, ...)
    }
    suppressMessages(
      plot <- plot +
        ggplot2::coord_polar("y") +
        ggplot2::guides()
    )
    if (.type == "donut")
      suppressMessages(plot + ggplot2::scale_x_discrete(limits = function(x) c("", "", x)))
    else
      plot
  }
}
#' Add pie or donut chart
#' @inherit common_arguments
#' @export
add_pie <- ff_pie(.type = "pie")
#' @rdname add_pie
#' @export
add_donut <- ff_pie(.type = "donut")


## Barstack function factory
ff_barstack <- function(.position_fun) {
  function(plot, width = 0.8, reverse = FALSE, ...) {
    check_tidyplot(plot)
    ptype <- get_plottype(plot)

    if (is_missing(plot, "colour")) stop("color missing without default")

    # detect orientation
    orientation <- NA
    if (ptype %in% c("_d", "cd", "ct")) {
      orientation <- "y"
    }
    # add orientation to args if not already present
    args <- list(...)
    if (!"orientation" %in% names(args)) args$orientation <- orientation

    mapping <- NULL
    if (ptype %in% c("_c", "__")) {
      mapping <- plot$mapping
      mapping$x <- ggplot2::aes(x = "")$x
    }

    if (ptype == "c_") {
      mapping <- plot$mapping
      mapping$y <- ggplot2::aes(y = "")$y
    }

    if (stringr::str_detect(ptype, "c")) {
      plot <- plot + rlang::inject(ggplot2::stat_summary(mapping = mapping, geom = "bar", fun = sum,
                                                         position = .position_fun(reverse = reverse), width = width,
                                                         color = NA, !!!args))
    } else {
      plot <- plot + rlang::inject(ggplot2::geom_bar(mapping = mapping, position = .position_fun(reverse = reverse),
                                                     width = width, color = NA, !!!args))
    }

    # remove padding between bar and axis
    if (is_flipped(plot)) {
      plot <- plot %>% adjust_x_axis(padding = c(0, NA), force_continuous = TRUE)
    } else {
      plot <- plot %>% adjust_y_axis(padding = c(0, NA), force_continuous = TRUE)
    }
    plot
  }
}
#' Add bar stack
#' @inherit common_arguments
#' @export
add_barstack_absolute <- ff_barstack(.position_fun = ggplot2::position_stack)
#' @rdname add_barstack_absolute
#' @export
add_barstack_relative <- ff_barstack(.position_fun = ggplot2::position_fill)


## Areastack function factory
ff_areastack <- function(.position_fun) {
  function(plot, linewidth = 0.25, alpha = 0.3, reverse = FALSE, replace_na = FALSE, ...) {
    check_tidyplot(plot)
    ptype <- get_plottype(plot)

    # overwrite group aesthetic
    mapping <- ggplot2::aes()
    mapping$group <- plot$mapping$colour

    if (is_missing(plot, "x") && is_missing(plot, "y")) {
      stop("x, y or both must be supplied")
    }

    # detect orientation
    orientation <- NA
    if (ptype %in% c("_d", "cd", "ct")) {
      orientation <- "y"
    }
    # add orientation to args if not already present
    args <- list(...)
    if (!"orientation" %in% names(args)) args$orientation <- orientation

    if (is_missing(plot, "y")) {
      if (replace_na) {
        vars <- c(get_variable(plot, "x"), get_variable(plot, "colour"))
        plot$data <-
          plot$data %>%
          dplyr::summarize(count = dplyr::n(), .by = all_of(vars)) %>%
          tidyr::complete(.data[[vars[1]]], .data[[vars[2]]], fill = list(count = 0))
        mapping$y <- ggplot2::aes(y = count)$y
        plot <- plot %>%
          remove_plot_area_padding(force_continuous = TRUE) %>%
          adjust_y_axis(title = "count") +
          rlang::inject(ggplot2::geom_area(mapping = mapping,
                                           position = .position_fun(reverse = reverse), linewidth = linewidth,
                                           alpha = alpha, !!!args))
      } else {
        plot <- plot %>%
          remove_plot_area_padding(force_continuous = TRUE) +
          rlang::inject(ggplot2::stat_count(mapping = mapping, geom = "area",
                                            position = .position_fun(reverse = reverse),
                                            linewidth = linewidth, alpha = alpha, !!!args))

      }
    }

    if (is_missing(plot, "x")) {
      if (replace_na) {
        vars <- c(get_variable(plot, "y"), get_variable(plot, "colour"))
        plot$data <-
          plot$data %>%
          dplyr::summarize(count = dplyr::n(), .by = all_of(vars)) %>%
          tidyr::complete(.data[[vars[1]]], .data[[vars[2]]], fill = list(count = 0))
        mapping$x <- ggplot2::aes(x = count)$x
        plot <- plot %>%
          remove_plot_area_padding(force_continuous = TRUE) %>%
          adjust_x_axis(title = "count") +
          rlang::inject(ggplot2::geom_area(mapping = mapping,
                                           position = .position_fun(reverse = reverse), linewidth = linewidth,
                                           alpha = alpha, !!!args))
      } else {
        plot <- plot %>%
          remove_plot_area_padding(force_continuous = TRUE) +
          rlang::inject(ggplot2::stat_count(mapping = mapping, geom = "area",
                                            position = .position_fun(reverse = reverse),
                                            linewidth = linewidth, alpha = alpha, !!!args))

      }
    }

    if (!is_missing(plot, "x") && !is_missing(plot, "y")) {

      if (is_discrete(plot, "y")) {
        vars <- c(get_variable(plot, "y"), get_variable(plot, "colour"))
        y_var <- get_variable(plot, "x")
      } else {
        vars <- c(get_variable(plot, "x"), get_variable(plot, "colour"))
        y_var <- get_variable(plot, "y")
      }

      if (replace_na) {
        zero <- list(y_var = 0)
        names(zero) <- y_var
        plot$data <-
          plot$data %>%
          dplyr::summarize("{y_var}" := sum(.data[[y_var]]), .by = all_of(vars)) %>%
          tidyr::complete(.data[[vars[1]]], .data[[vars[2]]], fill = zero)
        plot <- plot %>%
          remove_plot_area_padding(force_continuous = TRUE) +
          rlang::inject(ggplot2::geom_area(mapping = mapping,
                                           position = .position_fun(reverse = reverse), linewidth = linewidth,
                                           alpha = alpha, !!!args))
      } else {
        plot <- plot %>%
          remove_plot_area_padding(force_continuous = TRUE) +
          rlang::inject(ggplot2::stat_summary(mapping = mapping, geom = "area", fun = sum,
                                              position = .position_fun(reverse = reverse),
                                              alpha = alpha, linewidth = linewidth, !!!args))
      }
    }
    plot
  }
}
#' Add area stack
#' @inherit common_arguments
#' @export
add_areastack_absolute <- ff_areastack(.position_fun = ggplot2::position_stack)
#' @rdname add_areastack_absolute
#' @export
add_areastack_relative <- ff_areastack(.position_fun = ggplot2::position_fill)
