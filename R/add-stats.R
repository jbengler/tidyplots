#' Add statistics
#' @param plot bla
#' @param padding_top bla
#' @param include_info bla
#' @param ... bla
#' @inheritParams ggpubr::geom_pwc
#' @export
add_stats_pvalue <- function(plot,
                      padding_top = 0.15,
                      method = "t.test",
                      p.adjust.method = "none",
                      ref.group = NULL,
                      label = "{format_p_value(p.adj, 0.0001)}",
                      label.size = 7/ggplot2::.pt,
                      step.increase = 0.15,
                      vjust = -0.25,
                      bracket.nudge.y = 0.1,
                      hide.ns = FALSE,
                      p.adjust.by = "panel",
                      symnum.args = list(
                        cutpoints = c(0, 0.001, 0.01, 0.05, Inf),
                        symbols = c("***", "**", "*", "ns")
                      ),
                      include_info = TRUE,
                      ...) {
  check_tidyplot(plot)
  cli::cli_alert_success("add_stats: {.pkg method} = {method}, {.pkg label} = {label}, {.pkg p.adjust.method} = {p.adjust.method}, {.pkg hide.ns} = {hide.ns}")

  plot <- plot  %>%
    adjust_y_axis(padding = c(NA, padding_top))

  if(include_info)
    plot <- plot  %>% add_caption(caption = glue::glue("method = {method}
                                            label = {label}
                                            p.adjust.method = {p.adjust.method}
                                            hide.ns = {hide.ns}
                                            ref.group = {ref.group}", .null = "NULL"))

  plot + ggpubr::geom_pwc(method = method,
                       p.adjust.method = p.adjust.method,
                       ref.group = ref.group,
                       label = label,
                       label.size = label.size,
                       step.increase = step.increase,
                       vjust = vjust,
                       bracket.nudge.y = bracket.nudge.y,
                       hide.ns = hide.ns,
                       p.adjust.by = p.adjust.by,
                       symnum.args = symnum.args,
                       ...)
}
#' @rdname add_stats_pvalue
#' @export
add_stats_asterisks <- function(plot,
                             padding_top = 0.1,
                             method = "t.test",
                             p.adjust.method = "none",
                             ref.group = NULL,
                             label = "p.adj.signif",
                             label.size = 10/ggplot2::.pt,
                             step.increase = 0.2,
                             vjust = 0.3,
                             bracket.nudge.y = 0.15,
                             hide.ns = TRUE,
                             p.adjust.by = "panel",
                             symnum.args = list(
                               cutpoints = c(0, 0.001, 0.01, 0.05, Inf),
                               symbols = c("***", "**", "*", "ns")
                             ),
                             include_info = TRUE,
                             ...) {
  check_tidyplot(plot)
  add_stats_pvalue(plot,
            padding_top = padding_top,
            method = method,
            p.adjust.method = p.adjust.method,
            ref.group = ref.group,
            label = label,
            label.size = label.size,
            step.increase = step.increase,
            vjust = vjust,
            bracket.nudge.y = bracket.nudge.y,
            hide.ns = hide.ns,
            p.adjust.by = p.adjust.by,
            symnum.args = symnum.args,
            include_info = include_info,
            ...)
}
