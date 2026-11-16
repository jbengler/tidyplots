# The tidyplot log: figure facts ------------------------------------------------
#
# Facts about the final figure, derived from the plot when `view_log()` runs.
# They are not stored in the log, so they cannot become stale.

# Facts about the final figure, derived from the plot data
log_figure_facts <- function(plot) {
  data <- plot$data
  m <- plot$tidyplot$mapping
  mapped <- m$variable[
    m$aesthetic %in% c("x", "y", "colour") &
      !m$variable %in% c("!!MISSING", "!!NA", ".single_color")
  ]
  mapped <- unique(mapped[mapped %in% names(data)])

  na <- vapply(mapped, function(v) sum(is.na(data[[v]])), integer(1))
  keep <- if (length(mapped) > 0) {
    stats::complete.cases(data[mapped])
  } else {
    rep(TRUE, nrow(data))
  }

  # Effective axis transformations and non-finite values they produce, and
  # values removed by the limits of a scale, e.g. from
  # `add(ggplot2::scale_y_continuous(limits = ...))`. Limits set with
  # `adjust_x_axis()` or `adjust_y_axis()` only zoom and remove no values.
  transforms <- list()
  limits <- list()
  for (axis in c("x", "y")) {
    scale <- plot$scales$get_scales(axis)
    var <- get_variable(plot, axis)
    if (
      inherits(scale, "ScaleContinuous") &&
        var %in% names(data) &&
        is.numeric(data[[var]])
    ) {
      trans <- scale$get_transformation()
      transformed <- suppressWarnings(trans$transform(data[[var]]))
      if (!identical(trans$name, "identity")) {
        non_finite <- keep & !is.finite(transformed)
        transforms[[axis]] <- list(
          variable = var,
          transform = trans$name,
          non_finite = sum(non_finite)
        )
        keep <- keep & !non_finite
      }
      outside <- keep & log_outside_limits(scale, transformed)
      if (any(outside)) {
        limits[[axis]] <- list(variable = var, outside = sum(outside))
        keep <- keep & !outside
      }
    }
  }

  group_vars <- log_group_vars(plot)
  n_groups <- log_count_groups(data, keep, group_vars)

  # With continuous x and y, summary layers compute their statistic per x
  # value, so each summary is based on the rows per x value and group
  x_var <- get_variable(plot, "x")
  n_by_x <- if (
    x_var %in% names(data) &&
      identical(m$scale_type[match("x", m$aesthetic)], "continuous") &&
      identical(m$scale_type[match("y", m$aesthetic)], "continuous") &&
      log_has_summary_per_x(plot)
  ) {
    counts <- log_count_groups(data, keep, c(x_var, group_vars))
    counts[counts > 0]
  }

  list(
    n_total = nrow(data),
    n_used = sum(keep),
    na = na[na > 0],
    transforms = transforms,
    limits = limits,
    group_vars = group_vars,
    n_groups = n_groups,
    x_var = if (!is.null(n_by_x)) x_var,
    n_by_x = n_by_x,
    tests = log_test_results(plot)
  )
}

# Whether the plot has a summary layer that computes its statistic per x value,
# e.g. `add_mean_line()` or `add_sem_ribbon()`
log_has_summary_per_x <- function(plot) {
  per_x <- c("bar", "dash", "dot", "value label", "line", "area", "errorbar", "ribbon")
  any(vapply(
    plot$tidyplot$log$steps,
    function(e) e$kind == "summary" && isTRUE(e$record$geom %in% per_x),
    logical(1)
  ))
}

# Values that the limits of a continuous scale remove. `transformed` are the
# values after the scale transformation, the space in which ggplot2 stores and
# applies the limits. The scale's own out-of-bounds function decides: values
# it turns into NA are removed (`censor`, the default), others are kept
# (e.g. `squish`).
log_outside_limits <- function(scale, transformed) {
  limits <- scale$limits
  finite <- is.finite(transformed)
  if (is.null(limits) || !any(finite)) {
    return(rep(FALSE, length(transformed)))
  }
  data_range <- range(transformed[finite])
  if (is.function(limits)) {
    trans <- scale$get_transformation()
    limits <- trans$transform(limits(trans$inverse(data_range)))
  }
  # NA limits are filled with the data range, as in ggplot2
  limits <- ifelse(is.na(limits), data_range, limits)
  bounded <- suppressWarnings(scale$oob(transformed, range = limits))
  finite & is.na(bounded)
}

# Results of the statistical tests, as computed when the plot is built.
# Returns a list of tables, named by the index of the test step in the log.
# Steps and test layers are matched by their test id, see `new_test_spec()`.
# Test layers without a step, e.g. from nested calls, are not reported.
log_test_results <- function(plot) {
  step_ids <- lapply(plot$tidyplot$log$steps, function(e) e$test_id)
  test_steps <- which(!vapply(step_ids, is.null, logical(1)))
  layer_ids <- vapply(
    plot$layers,
    function(l) {
      if (inherits(l$stat, "StatTidyplotsTest")) l$stat_params$spec$id else NA_integer_
    },
    integer(1)
  )
  layers <- match(unlist(step_ids[test_steps]), layer_ids)
  test_steps <- test_steps[!is.na(layers)]
  layers <- layers[!is.na(layers)]
  if (length(layers) == 0) {
    return(list())
  }
  # Failed comparisons are reported in the results, not as warnings
  built <- suppressWarnings(ggplot2::ggplot_build(plot))
  panels <- log_panel_labels(built)
  columns <- c(
    "panel", "x_level", "group1", "group2", "computed_as", "n1", "n2", "statistic_name",
    "statistic", "df", "p", "p.adj", "p.adj.signif", "shown", "status", "notes"
  )
  results <- lapply(layers, function(i) {
    d <- built$data[[i]]
    if (nrow(d) == 0) {
      return(tibble::tibble())
    }
    d$panel <- panels[as.character(d$PANEL)]
    tibble::as_tibble(d[intersect(columns, names(d))])
  })
  stats::setNames(results, test_steps)
}

# Labels of the facet panels, NA if the plot has a single panel
log_panel_labels <- function(built) {
  layout <- built$layout$layout
  vars <- intersect(built$layout$facet$vars(), names(layout))
  labels <- if (length(vars) == 0 || nrow(layout) == 1) {
    rep(NA_character_, nrow(layout))
  } else {
    do.call(paste, c(lapply(layout[vars], as.character), sep = ", "))
  }
  stats::setNames(labels, as.character(layout$PANEL))
}

# Grouping variables: discrete x, y and color, plus facet variables
log_group_vars <- function(plot) {
  m <- plot$tidyplot$mapping
  group_vars <- m$variable[
    m$aesthetic %in% c("x", "y", "colour") &
      m$scale_type %in% c("discrete", "ordinal") &
      m$variable != ".single_color"
  ]
  for (entry in plot$tidyplot$log$steps) {
    if (entry$kind == "facet") {
      group_vars <- c(group_vars, entry$record$by, entry$record$rows, entry$record$cols)
    }
  }
  unique(group_vars[group_vars %in% names(plot$data)])
}

# Number of selected rows per group, as a named integer vector
log_count_groups <- function(data, selected, group_vars) {
  if (length(group_vars) == 0) {
    return(NULL)
  }
  labels <- lapply(data[group_vars], function(v) {
    out <- as.character(v)
    out[is.na(out)] <- "NA"
    out
  })
  group <- do.call(paste, c(labels, sep = "/"))
  counts <- table(factor(
    group[selected],
    levels = log_levels_combined(data, group_vars)
  ))
  stats::setNames(as.integer(counts), names(counts))
}

# All combinations of group levels present in the data, in display order
log_levels_combined <- function(data, vars) {
  d <- unique(data[vars])
  d <- d[do.call(order, unname(as.list(d))), , drop = FALSE]
  labels <- lapply(d, function(v) {
    out <- as.character(v)
    out[is.na(out)] <- "NA"
    out
  })
  unique(do.call(paste, c(labels, sep = "/")))
}
