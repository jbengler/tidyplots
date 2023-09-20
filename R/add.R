#' @importFrom dplyr %>%
#' @importFrom ggplot2 position_fill position_stack position_dodge position_jitterdodge position_jitter

#' @export
`%>%` <- dplyr::`%>%`

#' @export
add <- .Primitive("+")

parent_function <- function(x){
  deparse(sys.call(-2)) %>%
    stringr::str_extract(pattern = "^[A-Z_a-z]*")
}

add_geom <- function(gg, geom, rasterize = FALSE, rasterize_dpi = 300) {
  pf <- parent_function()
  if (check_input(gg) == "none") stop(glue::glue("{pf}: Please provide a ggplot or list of ggplots as input to 'gg'"))

  if (rasterize) {
    cli::cli_alert_success("{pf}: {.pkg rasterized} at {rasterize_dpi} dpi")
    gg + ggrastr::rasterise(geom, dpi = rasterize_dpi, dev = "ragg")
  } else {
    gg + geom
  }
}

# for x = continuous, y = continuous, use add_scatter()

#' @export
add_scatter <- function(gg, subset_data = . %>% dplyr::filter(), point_size = 0.5, point_shape = 19, rasterize = FALSE, rasterize_dpi = 300, ...) {
  add_geom(gg, ggplot2::geom_point(data = subset_data, size = point_size, shape = point_shape, ...),
           rasterize = rasterize, rasterize_dpi = rasterize_dpi)
}

# for x = discrete, y = continuous, use add_jitter() or add_scatter()

#' @export
add_jitter <- function(gg, dodge_width = 0.8, jitter_width = 0.1, jitter_height = 0, point_size = 0.5, point_shape = 19, position = position_jitterdodge(jitter.width = jitter_width, jitter.height = jitter_height, dodge.width = dodge_width), rasterize = FALSE, rasterize_dpi = 300, ...) {
  # position_jitterdodge() needs either colour or fill
  if(was_not_provided(gg, "fill") && was_not_provided(gg, "colour"))
    position = position_jitter(width = jitter_width, height = jitter_height)
  add_geom(gg, ggplot2::geom_point(size = point_size, shape = point_shape, position = position, ...),
          rasterize = rasterize, rasterize_dpi = rasterize_dpi)
}

# dispersion

#' @export
add_sem <- function(gg, dodge_width = 0.8, width = 0.4, linewidth = 0.25, position = position_dodge(width = dodge_width), ...) {
  gg + ggplot2::stat_summary(fun.data = ggplot2::mean_se, geom = "errorbar", linewidth = linewidth, width = width, position = position, ...)
}

#' @export
add_range <- function(gg, dodge_width = 0.8, width = 0.4, linewidth = 0.25, position = position_dodge(width = dodge_width), ...) {
  gg + ggplot2::stat_summary(fun.min = min, fun.max = max, geom = "errorbar", linewidth = linewidth, width = width, position = position, ...)
}

#' @export
add_sd <- function(gg, dodge_width = 0.8, width = 0.4, linewidth = 0.25, position = position_dodge(width = dodge_width), ...) {
  gg + ggplot2::stat_summary(fun.data = ggplot2::mean_sdl, geom = "errorbar", linewidth = linewidth, width = width, position = position, ...)
}

#' @export
add_ci <- function(gg, dodge_width = 0.8, width = 0.4, linewidth = 0.25, position = position_dodge(width = dodge_width), ...) {
  gg + ggplot2::stat_summary(fun.data = ggplot2::mean_cl_boot, geom = "errorbar", linewidth = linewidth, width = width, position = position, ...)
}

#' @export
add_error <- add_sem

# dispersion ribbons

#' @export
add_ribbon_sem <- function(gg, dodge_width = 0.8, alpha = 0.3, color = NA, ...) {
  gg + ggplot2::stat_summary(fun.data = ggplot2::mean_se, geom = "ribbon", alpha = alpha, color = color, ...)
}

#' @export
add_ribbon_range <- function(gg, dodge_width = 0.8, alpha = 0.3, color = NA, ...) {
  gg + ggplot2::stat_summary(fun.min = min, fun.max = max, geom = "ribbon", alpha = alpha, color = color, ...)
}

#' @export
add_ribbon_sd <- function(gg, dodge_width = 0.8, alpha = 0.3, color = NA, ...) {
  gg + ggplot2::stat_summary(fun.data = ggplot2::mean_sdl, geom = "ribbon", alpha = alpha, color = color, ...)
}

#' @export
add_ribbon_ci <- function(gg, dodge_width = 0.8, alpha = 0.3, color = NA, ...) {
  gg + ggplot2::stat_summary(fun.data = ggplot2::mean_cl_boot, geom = "ribbon", alpha = alpha, color = color, ...)
}

#' @export
add_ribbon <- add_ribbon_sem

# central tendency: mean

#' @export
add_mean <- function(gg, dodge_width = 0.8, width = 0.6, linewidth = 0.25, position = position_dodge(width = dodge_width), ...) {
  gg + ggplot2::stat_summary(fun.min = mean, fun.max = mean, geom = "errorbar", linewidth = linewidth, width = width, position = position, ...)
}

#' @export
add_mean_point <- function(gg, dodge_width = 0.8, size = 2, position = position_dodge(width = dodge_width), ...) {
  gg + ggplot2::stat_summary(fun = mean, geom = "point", size = size, position = position, ...)
}

#' @export
add_mean_bar <- function(gg, dodge_width = 0.8, alpha = 1, bar_width = 0.6, position = position_dodge(width = dodge_width), ...) {
  gg <- gg %>% modify_y_axis(expand = expansion(mult = c(0, 0.05)))
  suppressMessages(gg <- gg + my_scale_fill_d(alpha = alpha, drop = FALSE))
  gg + ggplot2::stat_summary(fun = mean, geom = "bar", color = NA, width = bar_width, position = position, ...)
}

# central tendency: median

#' @export
add_median <- function(gg, dodge_width = 0.8, width = 0.6, linewidth = 0.25, position = position_dodge(width = dodge_width), ...) {
  gg + ggplot2::stat_summary(fun.min = median, fun.max = median, geom = "errorbar", linewidth = linewidth, width = width, position = position, ...)
}

#' @export
add_median_point <- function(gg, dodge_width = 0.8, size = 2, position = position_dodge(width = dodge_width), ...) {
  gg + ggplot2::stat_summary(fun = median, geom = "point", size = size, position = position, ...)
}

#' @export
add_median_bar <- function(gg, dodge_width = 0.8, alpha = 1, bar_width = 0.6, position = position_dodge(width = dodge_width), ...) {
  gg <- gg %>% modify_y_axis(expand = expansion(mult = c(0, 0.05)))
  suppressMessages(gg <- gg + my_scale_fill_d(alpha = alpha, drop = FALSE))
  gg + ggplot2::stat_summary(fun = median, geom = "bar", color = NA, width = bar_width, position = position, ...)
}

#' @export
add_bar <- add_mean_bar

# others

#' @export
add_box <- function(gg, dodge_width = 0.8, alpha = 0.3, show_whiskers = TRUE, show_outliers = FALSE, box_width = 0.6, whiskers_width = 0.5, outlier.size = 0.5, coef = 1.5, outlier.shape = 19, linewidth = 0.25, position = position_dodge(width = dodge_width), ...) {
  suppressMessages(gg <- gg + my_scale_fill_d(alpha = alpha, drop = FALSE))
  if (show_whiskers == FALSE) {
    coef = 0
    whiskers_width = box_width
  }
  if (show_outliers == FALSE) outlier.shape = NA
  gg +
    ggplot2::stat_boxplot(geom ='errorbar', width = whiskers_width, linewidth = linewidth, coef = coef) +
    ggplot2::geom_boxplot(outlier.shape = outlier.shape, outlier.size = outlier.size, width = box_width, position = position, linewidth = linewidth, coef = coef, ...)
}

# boxplot median not the same as violin draw_quantiles = c(0.5)!
# https://stackoverflow.com/questions/36033341/differing-quantiles-boxplot-vs-violinplot

#' @export
add_violin <- function(gg, dodge_width = 0.8, alpha = 0.3, draw_quantiles = NULL, trim = FALSE, linewidth = 0.25, scale = "width", position = position_dodge(width = dodge_width), ...) {
  suppressMessages(gg <- gg + my_scale_fill_d(alpha = alpha, drop = FALSE))
  gg + ggplot2::geom_violin(draw_quantiles = draw_quantiles, trim = trim, linewidth = linewidth, scale = scale, position = position, ...)
}

#' @export
add_line <- function(gg, dodge_width = 0.8, linewidth = 0.25, position = position_dodge(width = dodge_width), ...) {
  gg + ggplot2::stat_summary(fun = mean, geom = "line", linewidth = linewidth, position = position, ...)
}

#' @export
add_curve <- function(gg, method = "loess", linewidth = 0.25, alpha = 0.3, ...) {
  gg + ggplot2::geom_smooth(method = method, linewidth = linewidth, alpha = alpha, ...)
}

#' @export
add_barstack_relative <- function(gg, bar_width = 0.6, reverse = FALSE, ...) {
  gg %>%
    modify_y_axis(expand = expansion(mult = c(0, 0.05))) +
    ggplot2::geom_col(position = position_fill(reverse = reverse), width = bar_width, ...)
}

#' @export
add_barstack_absolute <- function(gg, bar_width = 0.6, reverse = FALSE, ...) {
  gg %>%
    modify_y_axis(expand = expansion(mult = c(0, 0))) +
    ggplot2::geom_col(position = position_stack(reverse = reverse), width = bar_width, ...)
}

#' @export
add_area <- function(gg, reverse = FALSE, ...) {
  gg %>%
    modify_y_axis(expand = expansion(mult = c(0, 0))) %>%
    modify_x_axis(expand = expansion(mult = c(0, 0))) +
    ggplot2::geom_area(position = position_identity(), ...)
}

#' @export
add_areastack_absolute <- function(gg, reverse = FALSE, ...) {
  gg %>%
    modify_y_axis(expand = expansion(mult = c(0, 0)))%>%
    modify_x_axis(expand = expansion(mult = c(0, 0))) +
    ggplot2::geom_area(position = position_stack(reverse = reverse), ...)
}

#' @export
add_areastack_relative <- function(gg, reverse = FALSE, ...) {
  gg %>%
    modify_y_axis(expand = expansion(mult = c(0, 0)))%>%
    modify_x_axis(expand = expansion(mult = c(0, 0))) +
    ggplot2::geom_area(position = position_fill(reverse = reverse), ...)
}

#' @export
add_donut <- function(gg, bar_width = 1, reverse = FALSE, ...) {
  gg %>%
    style_void() +
    ggplot2::geom_col(aes(x = NA), position = position_fill(reverse = reverse), width = bar_width, ...) +
    ggplot2::scale_x_discrete(limits = function(x) c("", "", x)) +
    ggplot2::coord_polar("y") +
    ggplot2::guides()
}

#' @export
add_pie <- function(gg, bar_width = 1, reverse = FALSE, ...) {
  gg %>%
    style_void() +
    ggplot2::geom_col(aes(x = NA), position = position_fill(reverse = reverse), width = bar_width, ...) +
    ggplot2::coord_polar("y") +
    ggplot2::guides()
}

# stats helpers

set_decimals <- function(x, n_decimals) {
  trimws(format(round(x, n_decimals), nsmall = n_decimals, scientific = FALSE))
}

#' @export
format_p <- function(p, n_decimals = 4) {
  ifelse(p >= 10^-n_decimals,
         set_decimals(p, n_decimals),
         glue::glue("< {set_decimals(10^-n_decimals, n_decimals)}"))
}

# add stats

#' @export
add_stats <- function(gg,
                      y_expand_bottom = 0,
                      y_expand_top = 0.1,
                      method = "t.test",
                      p.adjust.method = "holm",
                      ref.group = NULL,
                      label = "p.signif",
                      label.size = 10/ggplot2::.pt,
                      step.increase = 0.2,
                      vjust = 0.3,
                      bracket.nudge.y = 0.15,
                      hide.ns = TRUE,
                      include_info = TRUE,
                      ...) {
  cli::cli_alert_success("add_stats: {.pkg method} = {method}, {.pkg label} = {label}, {.pkg p.adjust.method} = {p.adjust.method}, {.pkg hide.ns} = {hide.ns}")

  gg <- gg  %>%
    modify_y_axis(expand = expansion(mult = c(y_expand_bottom, y_expand_top)))

  if(include_info)
    gg <- gg  %>% modify_description(caption = glue::glue("method = {method}
                                            label = {label}
                                            p.adjust.method = {p.adjust.method}
                                            hide.ns = {hide.ns}
                                            ref.group = {ref.group}", .null = "NULL"))

  gg+ ggpubr::geom_pwc(method = method,
                       p.adjust.method = p.adjust.method,
                       ref.group = ref.group,
                       label = label,
                       label.size = label.size,
                       step.increase = step.increase,
                       vjust = vjust,
                       bracket.nudge.y = bracket.nudge.y,
                       hide.ns = hide.ns,
                       ...)
}

#' @export
add_stats_value <- function(gg,
                            y_expand_top = 0.15,
                            label = "{format_p(p, 4)}",
                            label.size = 7/ggplot2::.pt,
                            step.increase = 0.25,
                            vjust = -0.25,
                            bracket.nudge.y = 0.15,
                            ...) {
  add_stats(gg,
            y_expand_top = y_expand_top,
            label = label,
            label.size = label.size,
            step.increase = step.increase,
            vjust = vjust,
            bracket.nudge.y = bracket.nudge.y,
            ...)
}

# TODO: add_label(): ggrepel?

# experimental!
# my_bar_label <- function(fun = mean, dodge_width = 0.8, size = 7/.pt, vjust = -0.5, position = position_dodge(width = dodge_width), ...) {
#   gg %>% add(stat_summary(aes(label = after_stat(y)), fun = fun, geom = "text", vjust = vjust, size = size, position = position, ...))
# }

# bar_label also for stackedbars?
# my_bar_label(fun = sum, dodge_width = 0, position = position_stack(vjust = 0.5), vjust = NA, color = "white", expansion_top = .05) +

# TODO: include 'rasterize', where useful:
# scatter, jitter, bar, ...

# TODO: include 'subset', where useful:

# TODO: paired data? replicates with connecting line

# TODO: group and group2 confusing. User data_exprs instead?

# TODO: modify_labels -> reorder by order in "labels"

# TODO: change display size, everything x2?

# TODO: add easteregg: add_funkyness()


# TODO: Re-think "position" for grouped / non-grouped data



