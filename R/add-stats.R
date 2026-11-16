#' Add statistical test
#'
#' Add pairwise statistical comparisons with brackets and p values or
#' asterisks. The tests are computed with base R when the plot is built,
#' separately for each panel, so they always reflect the final plot data.
#'
#' @param method The test to use:
#'  * `"t_test"` (default): Welch's t-test, or Student's t-test with
#'    `method.args = list(var.equal = TRUE)`. With `paired_by`, the paired
#'    t-test. Computed with `stats::t.test()`.
#'  * `"wilcox_test"`: Wilcoxon rank-sum test (Mann-Whitney U test). With
#'    `paired_by`, the Wilcoxon signed-rank test. Computed with
#'    `stats::wilcox.test()`.
#'  * `"tukey_hsd"`: Tukey's HSD test, the usual post-hoc test after a
#'    one-way ANOVA. Uses the variance pooled over all groups, with the
#'    Tukey-Kramer correction for unequal group sizes, as `stats::TukeyHSD()`.
#'  * `"games_howell_test"`: Games-Howell test, like Tukey's HSD but without
#'    assuming equal variances.
#'  * `"dunn_test"`: Dunn's test, the usual post-hoc test after a
#'    Kruskal-Wallis test. Uses the ranks across all groups, with correction
#'    for ties.
#'
#'  `"tukey_hsd"`, `"games_howell_test"`, and `"dunn_test"` use all groups
#'  that are compared together, i.e. all groups in a panel, or all color
#'  groups within a group of the discrete axis, also when only some
#'  comparisons are shown with `ref.group` or `comparisons`. They do not
#'  support `paired_by` or `method.args`.
#' @param p.adjust.method Method for adjusting p values for multiple
#'  comparisons, see `stats::p.adjust()`. One of `"none"` (default), `"holm"`,
#'  `"hochberg"`, `"hommel"`, `"bonferroni"`, `"BH"`, `"BY"`, or `"fdr"`.
#'  The p values of `"tukey_hsd"` and `"games_howell_test"` are already
#'  adjusted for all pairwise comparisons of the groups compared together,
#'  so `p.adjust.method` must be `"none"` for them.
#' @param ref.group A reference group to compare all other groups against.
#'  Either the name of the group or its position, e.g. `ref.group = 1`.
#'  If `NULL` (default), all pairs of groups are compared.
#' @param comparisons A list of length-2 vectors with the groups to compare,
#'  either as names, e.g. `list(c("A", "B"), c("A", "C"))`, or as positions,
#'  e.g. `list(c(1, 2), c(1, 3))`.
#' @param paired_by Variable that identifies the pairs for a paired analysis,
#'  e.g. a subject or batch id. Values are paired by this variable, not by
#'  their order in the data.
#' @param label The label to show. Either the name of a result column, e.g.
#'  `"p.adj.signif"`, or a glue template using the result columns, e.g.
#'  `"{tidyplots::format_p_value(p.adj, 0.0001)}"`. Available columns are
#'  `p`, `p.adj`, `p.signif`, `p.adj.signif`, `n1`, `n2`, `statistic`, `df`,
#'  `group1`, and `group2`.
#' @param label.size Font size of the label.
#' @param step.increase Distance between stacked brackets, as a fraction of
#'  the data range. Vertical, or horizontal in horizontal plots.
#' @param vjust Justification of the label towards or away from the bracket.
#' @param bracket.nudge.y Distance between the data and the first bracket, as
#'  a fraction of the data range. Vertical, or horizontal in horizontal plots.
#' @param hide.ns Whether to hide non-significant comparisons. `TRUE` or
#'  `"p.adj"` hides them based on the adjusted p value, `"p"` based on the
#'  unadjusted p value. Hidden comparisons are still computed and reported
#'  by `view_log()`.
#' @param p.adjust.by For comparisons within color groups, whether p values
#'  are adjusted across all comparisons in the panel (`"panel"`, default) or
#'  separately within each x axis group (`"group"`).
#' @param symnum.args A list with `cutpoints` and `symbols` defining the
#'  significance symbols.
#' @param color Color of the brackets and labels.
#' @param padding_top Extra padding above the data points to accommodate the
#'  statistical comparisons. In horizontal plots, the padding is added to the
#'  right of the data points.
#' @param hide_info Whether to hide details about the statistical testing as caption. Defaults to `FALSE`.
#' @param ... Additional arguments: `method.args`, a list of arguments passed
#'  on to the test function, e.g. `list(var.equal = TRUE)` or
#'  `list(alternative = "less")`, and the drawing arguments `tip.length`,
#'  `linewidth` (or `size`), `linetype`, `alpha`, `family`, and `fontface`.
#'  Other arguments are ignored with a warning.
#' @inherit common_arguments
#'
#' @details
#' * Comparisons are made between the groups on the discrete axis. If a
#'  different variable is mapped to color, comparisons are made between the
#'  color groups within each group of the discrete axis.
#' * Works with a discrete x axis and a continuous y axis, or, for horizontal
#'  plots, with a discrete y axis and a continuous x axis. In horizontal plots,
#'  the brackets are placed to the right of the data and the labels are
#'  rotated.
#' * Tests use the values in the data. Axis transformations (e.g. `log10`)
#'  and axis limits change only what is shown, not what is tested. Values
#'  that are tested but not shown in the plot are reported in a warning and
#'  in `view_log()`.
#' * If a comparison cannot be computed, e.g. because a group has too few
#'  values, a warning is shown and no bracket is drawn. The reason is
#'  recorded in the log, see `view_log()`.
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
#' # Tukey's HSD test, p values are adjusted by the test
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_mean_dash() |>
#'   add_sem_errorbar() |>
#'   add_data_points() |>
#'   add_test_asterisks(method = "tukey_hsd", hide.ns = FALSE)
#'
#' # Dunn's test with Holm adjustment
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_mean_dash() |>
#'   add_sem_errorbar() |>
#'   add_data_points() |>
#'   add_test_pvalue(method = "dunn_test", p.adjust.method = "holm")
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
#' # Horizontal plot
#' study |>
#'   tidyplot(x = score, y = treatment, color = treatment) |>
#'   add_mean_dash() |>
#'   add_sem_errorbar() |>
#'   add_data_points() |>
#'   add_test_pvalue(ref.group = 1)
#'
#' @export
add_test_pvalue <- function(
  plot,
  padding_top = 0.15,
  method = "t_test",
  p.adjust.method = "none",
  ref.group = NULL,
  comparisons = NULL,
  paired_by = NULL,
  label = "{tidyplots::format_p_value(p.adj, 0.0001)}",
  label.size = 7 / ggplot2::.pt,
  step.increase = 0.15,
  vjust = -0.25,
  bracket.nudge.y = 0.1,
  hide.ns = FALSE,
  p.adjust.by = "panel",
  symnum.args = list(
    cutpoints = c(0, 0.001, 0.01, 0.05, Inf),
    symbols = c("***", "**", "*", "ns")
  ),
  color = plot$tidyplot$ink,
  hide_info = FALSE,
  ...
) {
  plot <- check_tidyplot(plot)
  flipped <- check_test_axes(plot)
  paired_quo <- rlang::enquo(paired_by)
  dots <- check_test_dots(list(...))
  spec <- new_test_spec(
    plot,
    flipped = flipped,
    method = check_test_method(method),
    method_args = dots$method.args %||% list(),
    paired_quo = paired_quo,
    ref.group = ref.group,
    comparisons = comparisons,
    p.adjust.method = p.adjust.method,
    p.adjust.by = p.adjust.by,
    label = label,
    symnum.args = symnum.args,
    hide.ns = hide.ns
  )
  plot <- log_record(plot, "test", .record = log_test_spec(spec), .test_id = spec$id)
  add_test_layer(
    plot,
    spec = spec,
    paired_quo = paired_quo,
    padding_top = padding_top,
    label.size = label.size,
    step.increase = step.increase,
    vjust = vjust,
    bracket.nudge.y = bracket.nudge.y,
    color = color,
    hide_info = hide_info,
    dots = dots
  )
}
#' @rdname add_test_pvalue
#' @export
add_test_asterisks <- function(
  plot,
  padding_top = 0.1,
  method = "t_test",
  p.adjust.method = "none",
  ref.group = NULL,
  comparisons = NULL,
  paired_by = NULL,
  label = "p.adj.signif",
  label.size = 10 / ggplot2::.pt,
  step.increase = 0.2,
  vjust = 0.3,
  bracket.nudge.y = 0.15,
  hide.ns = TRUE,
  p.adjust.by = "panel",
  symnum.args = list(
    cutpoints = c(0, 0.001, 0.01, 0.05, Inf),
    symbols = c("***", "**", "*", "ns")
  ),
  color = plot$tidyplot$ink,
  hide_info = FALSE,
  ...
) {
  plot <- check_tidyplot(plot)
  flipped <- check_test_axes(plot)
  paired_quo <- rlang::enquo(paired_by)
  dots <- check_test_dots(list(...))
  spec <- new_test_spec(
    plot,
    flipped = flipped,
    method = check_test_method(method),
    method_args = dots$method.args %||% list(),
    paired_quo = paired_quo,
    ref.group = ref.group,
    comparisons = comparisons,
    p.adjust.method = p.adjust.method,
    p.adjust.by = p.adjust.by,
    label = label,
    symnum.args = symnum.args,
    hide.ns = hide.ns
  )
  plot <- log_record(plot, "test", .record = log_test_spec(spec), .test_id = spec$id)
  add_test_layer(
    plot,
    spec = spec,
    paired_quo = paired_quo,
    padding_top = padding_top,
    label.size = label.size,
    step.increase = step.increase,
    vjust = vjust,
    bracket.nudge.y = bracket.nudge.y,
    color = color,
    hide_info = hide_info,
    dots = dots
  )
}

# Tests compare the groups of a discrete axis on a continuous axis. Returns
# whether the plot is flipped, i.e. has a discrete y axis.
check_test_axes <- function(plot, call = rlang::caller_env()) {
  if (is_discrete(plot, "x") && is_continuous(plot, "y")) {
    return(FALSE)
  }
  if (is_discrete(plot, "y") && is_continuous(plot, "x")) {
    return(TRUE)
  }
  cli::cli_abort(
    "Statistical tests require one discrete and one continuous axis.",
    call = call
  )
}

# Shared by add_test_pvalue() and add_test_asterisks()
add_test_layer <- function(
  plot,
  spec,
  paired_quo,
  padding_top,
  label.size,
  step.increase,
  vjust,
  bracket.nudge.y,
  color,
  hide_info,
  dots
) {
  # Untransformed values of the continuous axis for the tests, see
  # `test_values()`
  value_aes <- if (spec$flipped) "x" else "y"
  mapping <- ggplot2::aes(test_y = !!plot$mapping[[value_aes]])
  if (spec$paired) {
    mapping$pair_id <- paired_quo
  }
  if (spec$within_colour) {
    mapping$test_group <- plot$mapping$colour
  }

  plot <- if (spec$flipped) {
    adjust_x_axis(plot, padding = c(NA, padding_top))
  } else {
    adjust_y_axis(plot, padding = c(NA, padding_top))
  }

  if (!hide_info) {
    plot <- plot |>
      add_caption(caption = test_caption(spec))
  }

  plot +
    ggplot2::layer(
      stat = StatTidyplotsTest,
      geom = GeomTidyplotsBracket,
      mapping = mapping,
      data = NULL,
      position = "identity",
      inherit.aes = TRUE,
      show.legend = FALSE,
      params = c(
        list(
          spec = spec,
          flipped_aes = spec$flipped,
          dodge_width = plot$tidyplot$dodge_width,
          step.increase = step.increase,
          bracket.nudge.y = bracket.nudge.y,
          # missing values are counted per comparison and reported in the log
          na.rm = TRUE
        ),
        test_draw_params(dots, color, label.size, vjust)
      )
    )
}


#' Add pre-computed statistics
#'
#' Add brackets with p values or asterisks from statistics computed elsewhere.
#'
#' @param data A data frame with the pre-computed statistics, one row per
#'  comparison. It needs columns with the two groups (see `xmin` and `xmax`),
#'  the y position of the bracket (see `y.position`), and the values used in
#'  `label`, e.g. `p` or `p.signif`.
#' @param padding_top Extra padding above the data points to accommodate the statistical comparisons.
#' @param label The label to show. Either the name of a column in `data`,
#'  e.g. `"p.signif"`, or a glue template using the columns of `data`, e.g.
#'  `"{tidyplots::format_p_value(p, 0.0001)}"`.
#' @param label.size Font size of label text.
#' @param y.position Name of the column in `data` with the positions of the
#'  brackets on the continuous axis, i.e. the y axis, or the x axis in
#'  horizontal plots.
#' @param xmin,xmax Names of the columns in `data` with the two groups to
#'  compare.
#' @param step.increase Vertical distance between stacked brackets, as a
#'  fraction of the y axis range.
#' @param vjust Vertical justification of the label.
#' @param bracket.nudge.y Vertical shift of all brackets, as a fraction of
#'  the y axis range.
#' @param bracket.size Line width of the brackets.
#' @param tip.length Length of the bracket tips, as a fraction of the y axis
#'  range.
#' @param hide.ns Whether to hide non-significant comparisons, i.e. rows where
#'  the column `p.signif` (or, if missing, the label) is `"ns"`.
#' @param remove.bracket Whether to show only the labels without brackets.
#' @param orientation `NA` (default) detects the orientation from the plot:
#'  horizontal brackets for a discrete y axis and a continuous x axis,
#'  vertical brackets otherwise. Use `"x"` for vertical brackets that compare
#'  groups on the x axis, or `"y"` for horizontal brackets that compare
#'  groups on the y axis.
#' @param color Color of the brackets and labels.
#' @param ... Additional drawing arguments: `linetype`, `alpha`, `family`, and
#'  `fontface`. Other arguments are ignored with a warning.
#' @inherit common_arguments
#'
#' @details
#' * These functions take pre-computed statistics as input via the `data`
#' parameter. tidyplots does not verify these statistics. In the log, they
#' are recorded as pre-computed by the user.
#' * Facets: if `data` contains a column with the same name as the variable
#' used in `split_plot()`, each row is shown in the matching panel only.
#' * In horizontal plots, the brackets are placed to the right of the data
#' and the labels are rotated, see `orientation`.
#'
#' @examples
#' # Add manual p-values from pre-computed statistics
#' stat_df <- data.frame(
#'   group1 = "A", group2 = "B",
#'   p = 0.025, p.signif = "*",
#'   y.position = 50
#' )
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_mean_dash() |>
#'   add_sem_errorbar() |>
#'   add_data_points() |>
#'   add_test_pvalue_manual(data = stat_df)
#'
#' # Add manual asterisks
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_mean_dash() |>
#'   add_sem_errorbar() |>
#'   add_data_points() |>
#'   add_test_asterisks_manual(data = stat_df)
#'
#' # Horizontal plot
#' study |>
#'   tidyplot(x = score, y = treatment, color = treatment) |>
#'   add_mean_dash() |>
#'   add_sem_errorbar() |>
#'   add_data_points() |>
#'   add_test_pvalue_manual(data = stat_df)
#'
#' @export
add_test_pvalue_manual <- function(
  plot,
  data,
  padding_top = 0.15,
  label = "{tidyplots::format_p_value(p, 0.0001)}",
  label.size = 7 / ggplot2::.pt,
  y.position = "y.position",
  xmin = "group1",
  xmax = "group2",
  step.increase = 0.15,
  vjust = -0.25,
  bracket.nudge.y = 0.1,
  bracket.size = 0.3,
  tip.length = 0.03,
  hide.ns = FALSE,
  remove.bracket = FALSE,
  orientation = NA,
  color = plot$tidyplot$ink,
  ...
) {
  plot <- check_tidyplot(plot)
  flipped <- test_manual_flipped(plot, orientation)
  plot <- log_manual_test(plot, data, xmin, xmax, label)
  add_test_manual_layer(
    plot,
    data = data,
    padding_top = padding_top,
    label = label,
    label.size = label.size,
    y.position = y.position,
    xmin = xmin,
    xmax = xmax,
    step.increase = step.increase,
    vjust = vjust,
    bracket.nudge.y = bracket.nudge.y,
    bracket.size = bracket.size,
    tip.length = tip.length,
    hide.ns = hide.ns,
    remove.bracket = remove.bracket,
    flipped = flipped,
    color = color,
    dots = check_test_dots(list(...))
  )
}
#' @rdname add_test_pvalue_manual
#' @export
add_test_asterisks_manual <- function(
  plot,
  data,
  padding_top = 0.1,
  label = "p.signif",
  label.size = 10 / ggplot2::.pt,
  y.position = "y.position",
  xmin = "group1",
  xmax = "group2",
  step.increase = 0.2,
  vjust = 0.3,
  bracket.nudge.y = 0.15,
  bracket.size = 0.3,
  tip.length = 0.03,
  hide.ns = TRUE,
  remove.bracket = FALSE,
  orientation = NA,
  color = plot$tidyplot$ink,
  ...
) {
  plot <- check_tidyplot(plot)
  flipped <- test_manual_flipped(plot, orientation)
  plot <- log_manual_test(plot, data, xmin, xmax, label)
  add_test_manual_layer(
    plot,
    data = data,
    padding_top = padding_top,
    label = label,
    label.size = label.size,
    y.position = y.position,
    xmin = xmin,
    xmax = xmax,
    step.increase = step.increase,
    vjust = vjust,
    bracket.nudge.y = bracket.nudge.y,
    bracket.size = bracket.size,
    tip.length = tip.length,
    hide.ns = hide.ns,
    remove.bracket = remove.bracket,
    flipped = flipped,
    color = color,
    dots = check_test_dots(list(...))
  )
}

# Orientation of pre-computed statistics. With `NA`, horizontal if the y axis
# is discrete and the x axis continuous, as for the computed tests, and
# vertical otherwise.
test_manual_flipped <- function(plot, orientation, call = rlang::caller_env()) {
  if (length(orientation) != 1 || !(is.na(orientation) || orientation %in% c("x", "y"))) {
    cli::cli_abort(
      "{.arg orientation} must be {.code NA}, {.val x}, or {.val y}.",
      call = call
    )
  }
  if (is.na(orientation)) {
    return(is_discrete(plot, "y") && is_continuous(plot, "x"))
  }
  orientation == "y"
}

# Shared by add_test_pvalue_manual() and add_test_asterisks_manual()
add_test_manual_layer <- function(
  plot,
  data,
  padding_top,
  label,
  label.size,
  y.position,
  xmin,
  xmax,
  step.increase,
  vjust,
  bracket.nudge.y,
  bracket.size,
  tip.length,
  hide.ns,
  remove.bracket,
  flipped,
  color,
  dots,
  call = rlang::caller_env()
) {
  missing_cols <- setdiff(c(xmin, xmax, y.position), names(data))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "{.arg data} must contain the column{?s} {.val {missing_cols}}.",
      call = call
    )
  }
  data <- as.data.frame(data)
  data$.label <- test_labels(data, label)
  ns_column <- if ("p.signif" %in% names(data)) data$p.signif else data$.label
  data$.hidden <- isTRUE(hide.ns) & ns_column %in% "ns"

  plot <- if (flipped) {
    adjust_x_axis(plot, padding = c(NA, padding_top))
  } else {
    adjust_y_axis(plot, padding = c(NA, padding_top))
  }

  mapping <- ggplot2::aes(
    xmin = .data[[xmin]],
    xmax = .data[[xmax]],
    y = .data[[y.position]],
    label = .data$.label,
    hidden = .data$.hidden
  )
  if (flipped) {
    mapping <- ggplot2::aes(
      ymin = .data[[xmin]],
      ymax = .data[[xmax]],
      x = .data[[y.position]],
      label = .data$.label,
      hidden = .data$.hidden
    )
  }

  dots$linewidth <- dots$linewidth %||% dots$size %||% bracket.size
  dots$tip.length <- tip.length
  plot +
    ggplot2::layer(
      stat = StatTidyplotsBracket,
      geom = GeomTidyplotsBracket,
      mapping = mapping,
      data = data,
      position = "identity",
      inherit.aes = FALSE,
      show.legend = FALSE,
      params = c(
        list(
          step.increase = step.increase,
          bracket.nudge.y = bracket.nudge.y,
          remove.bracket = remove.bracket,
          flipped_aes = flipped
        ),
        test_draw_params(dots, color, label.size, vjust)
      )
    )
}
