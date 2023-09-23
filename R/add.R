#' @importFrom ggplot2 position_fill position_stack position_dodge position_jitterdodge position_jitter

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
add_scatter <- function(gg, subset_data = . %>% all_data(), point_size = 0.5, point_shape = 19, rasterize = FALSE, rasterize_dpi = 300, ...) {
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

# uncertainty

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

# ribbons

#' @export
add_sem_ribbon <- function(gg, dodge_width = 0.8, alpha = 0.3, color = NA, ...) {
  gg + ggplot2::stat_summary(fun.data = ggplot2::mean_se, geom = "ribbon", alpha = alpha, color = color, ...)
}

#' @export
add_range_ribbon <- function(gg, dodge_width = 0.8, alpha = 0.3, color = NA, ...) {
  gg + ggplot2::stat_summary(fun.min = min, fun.max = max, geom = "ribbon", alpha = alpha, color = color, ...)
}

#' @export
add_sd_ribbon <- function(gg, dodge_width = 0.8, alpha = 0.3, color = NA, ...) {
  gg + ggplot2::stat_summary(fun.data = ggplot2::mean_sdl, geom = "ribbon", alpha = alpha, color = color, ...)
}

#' @export
add_ci_ribbon <- function(gg, dodge_width = 0.8, alpha = 0.3, color = NA, ...) {
  gg + ggplot2::stat_summary(fun.data = ggplot2::mean_cl_boot, geom = "ribbon", alpha = alpha, color = color, ...)
}

# central tendency: mean

#' @export
add_mean_dash <- function(gg, dodge_width = 0.8, width = 0.6, linewidth = 0.25, position = position_dodge(width = dodge_width), ...) {
  gg + ggplot2::stat_summary(fun.min = mean, fun.max = mean, geom = "errorbar", linewidth = linewidth, width = width, position = position, ...)
}

#' @export
add_mean_dot <- function(gg, dodge_width = 0.8, size = 2, position = position_dodge(width = dodge_width), ...) {
  gg + ggplot2::stat_summary(fun = mean, geom = "point", size = size, position = position, ...)
}

#' @export
add_mean_bar <- function(gg, dodge_width = 0.8, alpha = 1, bar_width = 0.6, position = position_dodge(width = dodge_width), ...) {
  gg <- gg %>% adjust_y_axis(expand = expansion(mult = c(0, 0.05)))
  suppressMessages(gg <- gg + my_scale_fill_d(alpha = alpha, drop = FALSE))
  gg + ggplot2::stat_summary(fun = mean, geom = "bar", color = NA, width = bar_width, position = position, ...)
}

#' @export
add_mean_value <- function(gg, dodge_width = 0.8, fontsize = 7, size = fontsize/ggplot2::.pt, vjust = -0.5, expand_bottom = 0, expand_top = 0.15, position = position_dodge(width = dodge_width), ...) {
  gg <- gg %>% adjust_y_axis(expand = expansion(mult = c(expand_bottom, expand_top)))
  gg + ggplot2::stat_summary(aes(label = format_number(ggplot2::after_stat(y), 1)), fun = mean, geom = "text", vjust = vjust, size = size, position = position, ...)
}

#' @export
add_mean <- add_mean_dash

# central tendency: median

#' @export
add_median_dash <- function(gg, dodge_width = 0.8, width = 0.6, linewidth = 0.25, position = position_dodge(width = dodge_width), ...) {
  gg + ggplot2::stat_summary(fun.min = median, fun.max = median, geom = "errorbar", linewidth = linewidth, width = width, position = position, ...)
}

#' @export
add_median_dot <- function(gg, dodge_width = 0.8, size = 2, position = position_dodge(width = dodge_width), ...) {
  gg + ggplot2::stat_summary(fun = median, geom = "point", size = size, position = position, ...)
}

#' @export
add_median_bar <- function(gg, dodge_width = 0.8, alpha = 1, bar_width = 0.6, position = position_dodge(width = dodge_width), ...) {
  gg <- gg %>% adjust_y_axis(expand = expansion(mult = c(0, 0.05)))
  suppressMessages(gg <- gg + my_scale_fill_d(alpha = alpha, drop = FALSE))
  gg + ggplot2::stat_summary(fun = median, geom = "bar", color = NA, width = bar_width, position = position, ...)
}

#' @export
add_median_value <- function(gg, dodge_width = 0.8, fontsize = 7, size = fontsize/ggplot2::.pt, vjust = -0.5, expand_bottom = 0, expand_top = 0.15, position = position_dodge(width = dodge_width), ...) {
  gg <- gg %>% adjust_y_axis(expand = expansion(mult = c(expand_bottom, expand_top)))
  gg + ggplot2::stat_summary(aes(label = format_number(ggplot2::after_stat(y), 1)), fun = median, geom = "text", vjust = vjust, size = size, position = position, ...)
}

#' @export
add_median <- add_median_dash

# sum

#' @export
add_sum_dash <- function(gg, dodge_width = 0.8, width = 0.6, linewidth = 0.25, position = position_dodge(width = dodge_width), ...) {
  gg + ggplot2::stat_summary(fun.min = sum, fun.max = sum, geom = "errorbar", linewidth = linewidth, width = width, position = position, ...)
}

#' @export
add_sum_dot <- function(gg, dodge_width = 0.8, size = 2, position = position_dodge(width = dodge_width), ...) {
  gg + ggplot2::stat_summary(fun = sum, geom = "point", size = size, position = position, ...)
}

#' @export
add_sum_bar <- function(gg, dodge_width = 0.8, alpha = 1, bar_width = 0.6, position = position_dodge(width = dodge_width), ...) {
  gg <- gg %>% adjust_y_axis(expand = expansion(mult = c(0, 0.05)))
  suppressMessages(gg <- gg + my_scale_fill_d(alpha = alpha, drop = FALSE))
  gg + ggplot2::stat_summary(fun = sum, geom = "bar", color = NA, width = bar_width, position = position, ...)
}

#' @export
add_sum_value <- function(gg, dodge_width = 0.8, fontsize = 7, size = fontsize/ggplot2::.pt, vjust = -0.5, expand_bottom = 0, expand_top = 0.15, position = position_dodge(width = dodge_width), ...) {
  gg <- gg %>% adjust_y_axis(expand = expansion(mult = c(expand_bottom, expand_top)))
  gg + ggplot2::stat_summary(aes(label = format_number(ggplot2::after_stat(y), 1)), fun = sum, geom = "text", vjust = vjust, size = size, position = position, ...)
}

#' @export
add_sum <- add_sum_dash

# count

#' @export
add_count_dash <- function(gg, dodge_width = 0.8, width = 0.6, linewidth = 0.25, position = position_dodge(width = dodge_width), ...) {
  gg + ggplot2::stat_summary(fun.min = length, fun.max = length, geom = "errorbar", linewidth = linewidth, width = width, position = position, ...)
}

#' @export
add_count_dot <- function(gg, dodge_width = 0.8, size = 2, position = position_dodge(width = dodge_width), ...) {
  gg + ggplot2::stat_summary(fun = length, geom = "point", size = size, position = position, ...)
}

#' @export
add_count_bar <- function(gg, dodge_width = 0.8, alpha = 1, bar_width = 0.6, position = position_dodge(width = dodge_width), ...) {
  gg <- gg %>% adjust_y_axis(expand = expansion(mult = c(0, 0.05)))
  suppressMessages(gg <- gg + my_scale_fill_d(alpha = alpha, drop = FALSE))
  gg + ggplot2::stat_summary(fun = length, geom = "bar", color = NA, width = bar_width, position = position, ...)
}

#' @export
add_count_value <- function(gg, dodge_width = 0.8, fontsize = 7, size = fontsize/ggplot2::.pt, vjust = -0.5, expand_bottom = 0, expand_top = 0.15, position = position_dodge(width = dodge_width), ...) {
  gg <- gg %>% adjust_y_axis(expand = expansion(mult = c(expand_bottom, expand_top)))
  gg + ggplot2::stat_summary(aes(label = format_number(ggplot2::after_stat(y), 1)), fun = length, geom = "text", vjust = vjust, size = size, position = position, ...)
}

#' @export
add_count <- add_count_dash

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
    ggplot2::stat_boxplot(geom ='errorbar', width = whiskers_width, position = position, linewidth = linewidth, coef = coef) +
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
    adjust_y_axis(expand = expansion(mult = c(0, 0))) +
    ggplot2::geom_col(position = position_fill(reverse = reverse), width = bar_width, ...)
}

#' @export
add_barstack_absolute <- function(gg, bar_width = 0.6, reverse = FALSE, ...) {
  gg %>%
    adjust_y_axis(expand = expansion(mult = c(0, 0))) +
    ggplot2::geom_col(position = position_stack(reverse = reverse), width = bar_width, ...)
}

#' @export
add_area <- function(gg, reverse = FALSE, ...) {
  gg %>%
    adjust_y_axis(expand = expansion(mult = c(0, 0))) %>%
    adjust_x_axis(expand = expansion(mult = c(0, 0))) +
    ggplot2::geom_area(position = position_identity(), ...)
}

#' @export
add_areastack_absolute <- function(gg, reverse = FALSE, ...) {
  gg %>%
    adjust_y_axis(expand = expansion(mult = c(0, 0)))%>%
    adjust_x_axis(expand = expansion(mult = c(0, 0))) +
    ggplot2::geom_area(position = position_stack(reverse = reverse), ...)
}

#' @export
add_areastack_relative <- function(gg, reverse = FALSE, ...) {
  gg %>%
    adjust_y_axis(expand = expansion(mult = c(0, 0)))%>%
    adjust_x_axis(expand = expansion(mult = c(0, 0))) +
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

#' @export
add_histogram <- function(gg, binwidth = NULL, bins = NULL, ...) {
  gg + ggplot2::geom_histogram(binwidth = binwidth, bins = bins, ...)
}

#' @export
add_density_curve <- function(gg, bw = "nrd0", adjust = 1, kernel = "gaussian", n = 512, ...) {
  gg + ggplot2::geom_density(bw = bw, adjust = adjust, kernel = kernel, n = n, ...)
}

#' @export
add_density_histogram <- function(gg, binwidth = NULL, bins = NULL, ...) {
  gg + ggplot2::geom_histogram(aes(y=..density..), binwidth = binwidth, bins = bins, ...)
}

#' @export
add_reference_lines <- function(gg, x = NULL, y = NULL, linetype = "dashed", linewidth = 0.25, ...) {
  out <- gg
  if(!is.null(x)) {
    out <- out + ggplot2::geom_vline(xintercept = x, linetype = linetype, linewidth = linewidth, ...)
  }
  if(!is.null(y)) {
    out <- out + ggplot2::geom_hline(yintercept = y, linetype = linetype, linewidth = linewidth, ...)
  }
  out
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
    adjust_y_axis(expand = expansion(mult = c(y_expand_bottom, y_expand_top)))

  if(include_info)
    gg <- gg  %>% adjust_description(caption = glue::glue("method = {method}
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

#' @export
add_label <- function(gg, var, subset_data = . %>% all_data(), fontsize = 7, size = fontsize/ggplot2::.pt, segment.size = 0.2, box.padding = 0.2, ...) {
  gg + ggrepel::geom_text_repel(data = subset_data, aes(label = {{var}}), size = size, segment.size = segment.size, box.padding = box.padding, ...)
}
