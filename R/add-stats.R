#' Add statistical test
#' @param method a character string indicating which method to be used for
#'  pairwise comparisons. Default is \code{"t_test"}. Allowed methods
#'  include pairwise comparisons methods implemented in the \code{rstatix} R
#'  package. These methods are: \code{"wilcox_test", "t_test", "sign_test",
#'  "dunn_test", "emmeans_test", "tukey_hsd", "games_howell_test"}.
#' @param padding_top Extra padding above the data points to accommodate the statistical comparisons.
#' @param hide_info Whether to hide details about the statistical testing as caption. Defaults to `FALSE`.
#' @param ... Arguments passed on to `ggpubr::geom_pwc()`.
#' @inherit common_arguments
#' @inheritParams ggpubr::geom_pwc
#'
#' @details
#' * `add_test_pvalue()` and `add_test_asterisks()` use `ggpubr::geom_pwc()`.
#' Check there for additional arguments.
#'
#' @examples
#' study |>
#'   tidyplot(x = dose, y = score, color = group) |>
#'   add_mean_dash() |>
#'   add_sem_errorbar() |>
#'   add_data_points() |>
#'   add_test_pvalue()
#'
#' # Change stat methods
#' study |>
#'   tidyplot(x = dose, y = score, color = group) |>
#'   add_mean_dash() |>
#'   add_sem_errorbar() |>
#'   add_data_points() |>
#'   add_test_pvalue(method = "wilcoxon", p.adjust.method = "BH")
#'
#' # Define reference group to test against
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_mean_dash() |>
#'   add_sem_errorbar() |>
#'   add_data_points() |>
#'   add_test_pvalue(ref.group = "A")
#'
#' # hide non-significant p values
#' gene_expression |>
#'   # filter to one gene
#'   dplyr::filter(external_gene_name == "Apol6") |>
#'   # start plotting
#'   tidyplot(x = condition, y = expression, color = sample_type) |>
#'   add_mean_dash() |>
#'   add_sem_errorbar() |>
#'   add_data_points() |>
#'   add_test_pvalue(hide.ns = TRUE)
#'
#' # Adjust top padding for statistical comparisons
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_mean_dash() |>
#'   add_sem_errorbar() |>
#'   add_data_points() |>
#'   add_test_pvalue(padding_top = 0.08)
#'
#' # Hide stats information
#' study |>
#'   tidyplot(x = dose, y = score, color = group) |>
#'   add_mean_dash() |>
#'   add_sem_errorbar() |>
#'   add_data_points() |>
#'   add_test_pvalue(hide_info = TRUE)
#'
#' @export
add_test_pvalue <- function(plot,
                      padding_top = 0.15,
                      method = "t_test",
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
                      hide_info = FALSE,
                      ...) {
  plot <- check_tidyplot(plot)
  # cli::cli_alert_success("add_test: {.pkg method} = {method}, {.pkg label} = {label}, {.pkg p.adjust.method} = {p.adjust.method}, {.pkg hide.ns} = {hide.ns}")

  plot <- plot  |>
    adjust_y_axis(padding = c(NA, padding_top))

  if(!hide_info)
    plot <- plot  |> add_caption(caption = glue::glue("method = {method}
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
#' @rdname add_test_pvalue
#' @export
add_test_asterisks <- function(plot,
                             padding_top = 0.1,
                             method = "t_test",
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
                             hide_info = FALSE,
                             ...) {
  plot <- check_tidyplot(plot)
  add_test_pvalue(plot,
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
            hide_info = hide_info,
            ...)
}
