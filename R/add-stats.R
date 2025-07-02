#' Add statistical test
#' @param method a character string indicating which method to be used for
#'  pairwise comparisons. Default is \code{"t_test"}. Allowed methods
#'  include pairwise comparisons methods implemented in the \code{rstatix} R
#'  package. These methods are: \code{"wilcox_test", "t_test", "sign_test",
#'  "dunn_test", "emmeans_test", "tukey_hsd", "games_howell_test"}.
#' @param comparisons A list of length-2 vectors. The entries in the vector are
#'  2 integers that correspond to the index of the groups of interest, to be compared.
#' @param paired_by Variable to be used for paired analysis.
#' @param padding_top Extra padding above the data points to accommodate the statistical comparisons.
#' @param hide_info Whether to hide details about the statistical testing as caption. Defaults to `FALSE`.
#' @param ... Arguments passed on to `ggpubr::geom_pwc()`.
#' @inherit common_arguments
#' @inheritParams ggpubr::geom_pwc
#'
#' @details
#' * `add_test_pvalue()` and `add_test_asterisks()` use `ggpubr::geom_pwc()`.
#' Check there for additional arguments.
#' * Known limitation: `add_test_pvalue()` and `add_test_asterisks()` expect a
#' discrete variable on the x-axis and a continuous variable on the y-axis.
#' To produce horizontal plots, use `flip_plot()`.
#'
#' @examples
#' # Add p value
#' study |>
#'   tidyplot(x = dose, y = score, color = group) |>
#'   add_mean_dash() |>
#'   add_sem_errorbar() |>
#'   add_data_points() |>
#'   add_test_pvalue()
#'
#' # Add asterisks
#' study |>
#'   tidyplot(x = dose, y = score, color = group) |>
#'   add_mean_dash() |>
#'   add_sem_errorbar() |>
#'   add_data_points() |>
#'   add_test_asterisks()
#'
#' # Change stat method
#' study |>
#'   tidyplot(x = dose, y = score, color = group) |>
#'   add_mean_dash() |>
#'   add_sem_errorbar() |>
#'   add_data_points() |>
#'   add_test_pvalue(method = "wilcoxon")
#'
#' # Change p.adjust method
#' study |>
#'   tidyplot(x = dose, y = score, color = group) |>
#'   add_mean_dash() |>
#'   add_sem_errorbar() |>
#'   add_data_points() |>
#'   add_test_pvalue(p.adjust.method = "bonferroni")
#'
#' # Define reference group to test against
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_mean_dash() |>
#'   add_sem_errorbar() |>
#'   add_data_points() |>
#'   add_test_asterisks(ref.group = 1)
#'
#' # Define selected comparisons
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_mean_dash() |>
#'   add_sem_errorbar() |>
#'   add_data_points() |>
#'   add_test_pvalue(comparisons = list(c(1,3),c(2,4)))
#'
#' # Paired analysis
#' x <- c(2.3, 4.5, 6.3, 3.4, 7.8, 6.7)
#' df <- data.frame(
#'   x = c(x, x + c(0.8, 0.75)),
#'   group = paste0("g", rep(c(1, 2), each = 6)),
#'   batch = paste0("b", c(1:6, 1:6)),
#'   shuffle = paste0("c", c(1:6, 6:1))
#' )
#'
#' df |>
#'   tidyplot(group, x, color = group) |>
#'   add_boxplot() |>
#'   add_data_points() |>
#'   add_test_pvalue(paired_by = shuffle) |>
#'   add_line(group = shuffle, color = "black")
#'
#' df |>
#'   tidyplot(group, x, color = group) |>
#'   add_boxplot() |>
#'   add_data_points() |>
#'   add_test_pvalue(paired_by = batch) |>
#'   add_line(group = batch, color = "black")
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
#' # Flip plot
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_mean_dash() |>
#'   add_sem_errorbar() |>
#'   add_data_points() |>
#'   add_test_asterisks(comparisons = list(c(1,4),c(2,3))) |>
#'   flip_plot()
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
                      comparisons = NULL,
                      paired_by = NULL,
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

  # method.args are not supplied in ellipses
  if (!"method.args" %in% names(list(...))) method.args <- list()
  # comparisons are supplied
  if (!is.null(comparisons)) {
    method.args$comparisons <- comparisons
    comparisons <- paste0("list(",paste(comparisons, collapse = ", "),")")
  }
  # paired_by is supplied
  quo_paired_by <- rlang::enquo(paired_by)
  if (!rlang::quo_is_null(quo_paired_by)) {
    method.args$paired <- TRUE
    new_data <-
      plot$data |>
      dplyr::arrange({{ paired_by }})
    plot <- plot %+% new_data
    paired_by <- rlang::as_name(quo_paired_by)
  }

  plot <- plot  |>
    adjust_y_axis(padding = c(NA, padding_top))

  if (!hide_info)
    plot <- plot  |> add_caption(
    caption = glue::glue("method = {method}
    paired_by = {paired_by}
    p.adjust.method = {p.adjust.method}
    ref.group = {ref.group}
    comparisons = {comparisons}
    hide.ns = {hide.ns}",
                         .null = "NULL"))

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
                       method.args = method.args,
                       ...)
}
#' @rdname add_test_pvalue
#' @export
add_test_asterisks <- function(plot,
                             padding_top = 0.1,
                             method = "t_test",
                             p.adjust.method = "none",
                             ref.group = NULL,
                             comparisons = NULL,
                             paired_by = NULL,
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
            comparisons = comparisons,
            paired_by = {{ paired_by }},
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
