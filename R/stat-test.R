# Statistical comparisons ------------------------------------------------------
#
# Pairwise tests are computed with base R when the plot is built, separately
# for each panel and on the final plot data. This way, facets, pages from
# `split_plot()`, and later steps such as sorting, renaming, normalizing, or
# excluding data are always reflected. The layer data holds one row per
# comparison with the full results (group names, n, statistic, df, exact and
# adjusted p-values, significance, whether it is shown, and its status), which
# is also what the log reports.

test_methods <- c(
  "t_test",
  "wilcox_test",
  "tukey_hsd",
  "games_howell_test",
  "dunn_test"
)

test_method_aliases <- c(
  t_test = "t_test",
  t.test = "t_test",
  ttest = "t_test",
  wilcox_test = "wilcox_test",
  wilcox.test = "wilcox_test",
  wilcox = "wilcox_test",
  wilcoxon = "wilcox_test",
  tukey_hsd = "tukey_hsd",
  tukey = "tukey_hsd",
  TukeyHSD = "tukey_hsd",
  games_howell_test = "games_howell_test",
  games_howell = "games_howell_test",
  dunn_test = "dunn_test",
  dunn = "dunn_test"
)

check_test_method <- function(method, call = rlang::caller_env()) {
  if (!rlang::is_string(method) || !method %in% names(test_method_aliases)) {
    cli::cli_abort(
      c(
        "{.arg method} must be one of {.val {test_methods}}.",
        "i" = "For other methods, compute the statistics yourself and use {.fn add_test_pvalue_manual}."
      ),
      call = call
    )
  }
  unname(test_method_aliases[method])
}

# Arguments in `...` that are used in addition to the named arguments
test_dots_known <- c(
  "method.args",
  "tip.length",
  "linewidth",
  "size",
  "linetype",
  "alpha",
  "family",
  "fontface"
)

check_test_dots <- function(dots, call = rlang::caller_env()) {
  unknown <- setdiff(names(dots), test_dots_known)
  if (length(unknown) > 0) {
    cli::cli_warn(
      "Ignoring unknown argument{?s}: {.arg {unknown}}.",
      call = call
    )
  }
  dots[intersect(names(dots), test_dots_known)]
}

# Specification -----------------------------------------------------------------
#
# One specification per call of add_test_pvalue() or add_test_asterisks(). It
# is the single source for the log record, the caption, and the parameters of
# the stat layer. Its `id` links the log step to the layer, so the results
# can be matched to their step without relying on the order of layers.

new_test_spec <- function(
  plot,
  flipped,
  method,
  method_args,
  paired_quo,
  ref.group,
  comparisons,
  p.adjust.method,
  p.adjust.by,
  label,
  symnum.args,
  hide.ns,
  call = rlang::caller_env()
) {
  check_test_spec(method, method_args, paired_quo, p.adjust.method, call = call)
  # `x_var` is the variable of the discrete axis, also in flipped plots
  x_var <- get_variable(plot, if (flipped) "y" else "x")
  color_var <- get_variable(plot, "colour")
  paired <- !rlang::quo_is_null(paired_quo)
  list(
    id = test_next_id(plot),
    flipped = flipped,
    method = method,
    method_args = method_args[setdiff(names(method_args), c("paired", "comparisons"))],
    paired = paired,
    paired_by = if (paired) rlang::as_label(paired_quo),
    ref.group = ref.group,
    comparisons = comparisons,
    p.adjust.method = p.adjust.method,
    p.adjust.by = p.adjust.by,
    x_var = x_var,
    color_var = color_var,
    within_colour = color_var != ".single_color" &&
      color_var != x_var &&
      is_discrete(plot, "colour"),
    label = label,
    symnum.args = symnum.args,
    hide.ns = hide.ns
  )
}

# Settings that a method does not support are errors, not silently ignored
check_test_spec <- function(method, method_args, paired_quo, p.adjust.method, call) {
  engine <- test_engines[[method]]
  if (!rlang::quo_is_null(paired_quo) && !engine$paired) {
    cli::cli_abort(
      c(
        "{.val {method}} does not support paired comparisons.",
        "i" = "Use {.val t_test} or {.val wilcox_test} with {.arg paired_by}."
      ),
      call = call
    )
  }
  if (engine$adjusted && !identical(p.adjust.method, "none")) {
    cli::cli_abort(
      c(
        "The p-values of {.val {method}} are already adjusted for multiple comparisons.",
        "i" = "Use {.code p.adjust.method = \"none\"}."
      ),
      call = call
    )
  }
  if (length(method_args) > 0 && !engine$method_args) {
    cli::cli_abort(
      "{.arg method.args} are not supported for {.val {method}}.",
      call = call
    )
  }
}

# Test layers of a plot
test_layers <- function(plot) {
  Filter(function(l) inherits(l$stat, "StatTidyplotsTest"), plot$layers)
}

# Ids are numbered per plot, so the same pipeline gives the same ids
test_next_id <- function(plot) {
  ids <- vapply(test_layers(plot), function(l) l$stat_params$spec$id, integer(1))
  max(c(0L, ids)) + 1L
}

# Caption with the test settings
test_caption <- function(spec) {
  comparisons_text <- if (!is.null(spec$comparisons)) {
    paste0("list(", paste(spec$comparisons, collapse = ", "), ")")
  }
  glue::glue(
    "method = {spec$method}
    paired_by = {spec$paired_by}
    p.adjust.method = {spec$p.adjust.method}
    ref.group = {spec$ref.group}
    comparisons = {comparisons_text}
    hide.ns = {spec$hide.ns}",
    .null = "NULL"
  )
}

# Engines -----------------------------------------------------------------------
#
# An engine computes all requested comparisons of one block, i.e. a panel, or
# one x axis group when comparing color groups. It gets all values of the
# block, not only those of the two groups compared, so that methods that use
# all groups (e.g. Tukey's HSD or Dunn's test) can be added.
#
# Arguments:
# * `block`: data frame with columns `.group`, `y`, and `pair_id`
# * `pairs`: list of length-2 character vectors with the groups to compare
# * `paired`: whether the comparison is paired by `pair_id`
# * `args`: further arguments of the test function
#
# Returns a data frame with one row per pair and the columns `n1`, `n2`,
# `computed_as` (the name of the test as reported by R), `statistic_name`,
# `statistic`, `df`, `p`, `status`, and `notes`.

# Engine for tests of two samples, applied to each pair separately
test_engine_two_sample <- function(run) {
  function(block, pairs, paired, args) {
    rows <- lapply(pairs, function(cmp) {
      test_compare(
        block[block$.group == cmp[1], c("y", "pair_id")],
        block[block$.group == cmp[2], c("y", "pair_id")],
        run = run,
        paired = paired,
        args = args
      )
    })
    do.call(rbind, rows)
  }
}

test_run_t <- function(y1, y2, paired, args) {
  r <- rlang::exec(stats::t.test, x = y1, y = y2, paired = paired, !!!args)
  list(
    computed_as = r$method,
    statistic_name = "t",
    statistic = unname(r$statistic),
    df = unname(r$parameter),
    p = r$p.value
  )
}

test_run_wilcox <- function(y1, y2, paired, args) {
  r <- rlang::exec(stats::wilcox.test, x = y1, y = y2, paired = paired, !!!args)
  list(
    computed_as = r$method,
    statistic_name = if (paired) "V" else "W",
    statistic = unname(r$statistic),
    df = NA_real_,
    p = r$p.value
  )
}

# Engine for tests that use all groups of the block, e.g. a pooled variance
# or ranks across all groups. `compute(groups, pairs)` gets a named list with
# the values of each group and returns a list with `computed_as`,
# `statistic_name`, and vectors `statistic`, `df`, and `p`, one per pair.
test_engine_all_groups <- function(compute) {
  function(block, pairs, paired, args) {
    block <- block[!is.na(block$y), , drop = FALSE]
    groups <- split(block$y, factor(block$.group, levels = unique(block$.group)))
    g1 <- vapply(pairs, `[[`, character(1), 1)
    g2 <- vapply(pairs, `[[`, character(1), 2)
    count <- function(g) vapply(g, function(x) length(groups[[x]]), integer(1))
    notes <- character()
    res <- withCallingHandlers(
      tryCatch(
        compute(groups, g1, g2),
        error = function(e) list(error = conditionMessage(e))
      ),
      warning = function(w) {
        notes <<- c(notes, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    failed <- if (!is.null(res$error)) {
      rep(TRUE, length(pairs))
    } else {
      !is.finite(res$p)
    }
    reason <- res$error %||% "not enough values"
    data.frame(
      n1 = unname(count(g1)),
      n2 = unname(count(g2)),
      computed_as = if (is.null(res$error)) res$computed_as else NA_character_,
      statistic_name = ifelse(failed, NA_character_, res$statistic_name %||% NA_character_),
      statistic = ifelse(failed, NA_real_, unname(res$statistic %||% NA_real_)),
      df = ifelse(failed, NA_real_, unname(res$df %||% NA_real_)),
      p = ifelse(failed, NA_real_, unname(res$p %||% NA_real_)),
      status = ifelse(failed, paste("failed:", reason), "computed"),
      notes = paste(notes, collapse = "; ")
    )
  }
}

# Tukey's HSD with the Tukey-Kramer standard error for unequal group sizes,
# as in `stats::TukeyHSD()`. The variance is pooled over all groups.
test_tukey <- function(groups, g1, g2) {
  n <- lengths(groups)
  m <- vapply(groups, mean, numeric(1))
  k <- length(groups)
  df <- sum(n) - k
  if (df < 1) {
    stop("not enough values to estimate the variance")
  }
  mse <- sum(vapply(groups, function(v) sum((v - mean(v))^2), numeric(1))) / df
  q <- (m[g1] - m[g2]) / sqrt(mse / 2 * (1 / n[g1] + 1 / n[g2]))
  list(
    computed_as = "Tukey's HSD (Tukey-Kramer)",
    statistic_name = "q",
    statistic = q,
    df = rep(df, length(g1)),
    p = stats::ptukey(abs(q), nmeans = k, df = df, lower.tail = FALSE)
  )
}

# Games-Howell test: Welch standard error and degrees of freedom for each
# pair, p-values from the studentized range distribution
test_games_howell <- function(groups, g1, g2) {
  n <- lengths(groups)
  m <- vapply(groups, mean, numeric(1))
  v <- vapply(groups, stats::var, numeric(1))
  k <- length(groups)
  w1 <- v[g1] / n[g1]
  w2 <- v[g2] / n[g2]
  t <- (m[g1] - m[g2]) / sqrt(w1 + w2)
  df <- (w1 + w2)^2 / (w1^2 / (n[g1] - 1) + w2^2 / (n[g2] - 1))
  list(
    computed_as = "Games-Howell test",
    statistic_name = "t",
    statistic = t,
    df = df,
    p = stats::ptukey(abs(t) * sqrt(2), nmeans = k, df = df, lower.tail = FALSE)
  )
}

# Dunn's test: mean ranks of all groups, with correction for ties, normal
# approximation. p-values are not adjusted, see `p.adjust.method`.
test_dunn <- function(groups, g1, g2) {
  n <- lengths(groups)
  r <- rank(unlist(groups, use.names = FALSE))
  big_n <- length(r)
  mean_rank <- vapply(
    split(r, factor(rep(names(groups), n), levels = names(groups))),
    mean,
    numeric(1)
  )
  ties <- table(r)
  tie_correction <- sum(ties^3 - ties) / (12 * (big_n - 1))
  z <- (mean_rank[g1] - mean_rank[g2]) /
    sqrt((big_n * (big_n + 1) / 12 - tie_correction) * (1 / n[g1] + 1 / n[g2]))
  list(
    computed_as = "Dunn's test (normal approximation, ties corrected)",
    statistic_name = "z",
    statistic = z,
    df = rep(NA_real_, length(g1)),
    p = 2 * stats::pnorm(-abs(z))
  )
}

# Methods with their engine and properties:
# * `paired`: whether a paired analysis is supported
# * `adjusted`: whether the p-values are already adjusted for multiple
#   comparisons, for all pairs of groups in the block
# * `method_args`: whether `method.args` are passed on
test_engines <- list(
  t_test = list(
    compute = test_engine_two_sample(test_run_t),
    paired = TRUE,
    adjusted = FALSE,
    method_args = TRUE
  ),
  wilcox_test = list(
    compute = test_engine_two_sample(test_run_wilcox),
    paired = TRUE,
    adjusted = FALSE,
    method_args = TRUE
  ),
  tukey_hsd = list(
    compute = test_engine_all_groups(test_tukey),
    paired = FALSE,
    adjusted = TRUE,
    method_args = FALSE
  ),
  games_howell_test = list(
    compute = test_engine_all_groups(test_games_howell),
    paired = FALSE,
    adjusted = TRUE,
    method_args = FALSE
  ),
  dunn_test = list(
    compute = test_engine_all_groups(test_dunn),
    paired = FALSE,
    adjusted = FALSE,
    method_args = FALSE
  )
)

# One comparison of two samples. Paired values are matched by `pair_id`, not
# by row order.
test_compare <- function(d1, d2, run, paired, args) {
  notes <- character()
  if (paired) {
    m <- merge(d1, d2, by = "pair_id", suffixes = c("1", "2"))
    m <- m[!is.na(m$y1) & !is.na(m$y2), , drop = FALSE]
    unmatched <- nrow(d1) + nrow(d2) - 2 * nrow(m)
    if (unmatched > 0) {
      notes <- c(notes, paste(unmatched, "values without a complete pair excluded"))
    }
    y1 <- m$y1
    y2 <- m$y2
  } else {
    y1 <- d1$y[!is.na(d1$y)]
    y2 <- d2$y[!is.na(d2$y)]
  }
  res <- withCallingHandlers(
    tryCatch(
      run(y1, y2, paired = paired, args = args),
      error = function(e) list(error = conditionMessage(e))
    ),
    warning = function(w) {
      notes <<- c(notes, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  failed <- !is.null(res$error)
  data.frame(
    n1 = length(y1),
    n2 = length(y2),
    computed_as = if (failed) NA_character_ else res$computed_as,
    statistic_name = if (failed) NA_character_ else res$statistic_name,
    statistic = if (failed) NA_real_ else res$statistic,
    df = if (failed) NA_real_ else res$df,
    p = if (failed) NA_real_ else res$p,
    status = if (failed) paste("failed:", res$error) else "computed",
    notes = paste(notes, collapse = "; ")
  )
}

# Values -----------------------------------------------------------------------
#
# Tests use the values as they are in the data. Axis transformations (e.g.
# log10) and axis limits change what is shown, not what is tested. Values
# that are in the data but not in the plot are counted in the notes.

# Flags the values that are not shown: outside the axis limits (NA in `y`)
# or not finite after the axis transformation (e.g. 0 on a log10 axis)
test_values <- function(data, scale_y) {
  if (is.null(data$y)) {
    data$y <- data$test_y
  }
  has_value <- !is.na(data$test_y)
  transformed <- suppressWarnings(
    scale_y$get_transformation()$transform(data$test_y)
  )
  data$.not_finite <- has_value & !is.finite(transformed)
  data$.outside <- has_value & !data$.not_finite & !is.finite(data$y)
  data
}

# Number of values of the compared groups that are not shown in the plot
test_count_not_shown <- function(res, block) {
  count <- function(flag) {
    vapply(seq_len(nrow(res)), function(i) {
      sum(flag[block$.group %in% c(res$group1[i], res$group2[i])])
    }, integer(1))
  }
  res$n_outside <- count(block$.outside)
  res$n_not_finite <- count(block$.not_finite)
  res
}

# Adds the values not shown in the plot to the notes
test_notes_not_shown <- function(out, axis_name) {
  values <- function(n) ifelse(n == 1, "1 value", paste(n, "values"))
  extra <- mapply(
    function(outside, not_finite) {
      paste(c(
        if (outside > 0) paste(values(outside), "outside the axis limits, not shown"),
        if (not_finite > 0) {
          paste(values(not_finite), "not finite on the", axis_name, "axis, not shown")
        }
      ), collapse = "; ")
    },
    out$n_outside,
    out$n_not_finite
  )
  ifelse(
    out$notes == "",
    extra,
    ifelse(extra == "", out$notes, paste(out$notes, extra, sep = "; "))
  )
}

test_signif <- function(p, symnum.args) {
  out <- as.character(cut(
    p,
    breaks = symnum.args$cutpoints,
    labels = symnum.args$symbols,
    right = FALSE,
    include.lowest = TRUE
  ))
  out[is.na(p)] <- NA_character_
  out
}

# Resolve group indices to level names
test_as_levels <- function(x, levels) {
  if (is.numeric(x)) levels[x] else as.character(x)
}

# Labels from a column name or a glue template evaluated on the results
test_labels <- function(results, label) {
  if (label %in% names(results)) {
    out <- as.character(results[[label]])
  } else {
    out <- as.character(glue::glue_data(results, label, .na = ""))
  }
  out[is.na(out)] <- ""
  out
}

# Is a comparison hidden by `hide.ns`?
test_hidden <- function(results, hide.ns) {
  if (isFALSE(hide.ns) || is.null(hide.ns)) {
    return(rep(FALSE, nrow(results)))
  }
  column <- if (isTRUE(hide.ns) || identical(hide.ns, "p.adj")) {
    "p.adj.signif"
  } else {
    "p.signif"
  }
  results[[column]] %in% "ns"
}

# Bracket heights: stacked above the panel data in comparison order, per block
test_bracket_y <- function(y, shown, block, y_max, y_range, bracket.nudge.y, step.increase) {
  for (b in unique(block)) {
    i <- which(shown & block == b)
    y[i] <- y_max + y_range * (bracket.nudge.y + step.increase * (seq_along(i) - 1))
  }
  y
}

StatTidyplotsTest <- ggplot2::ggproto(
  "StatTidyplotsTest",
  ggplot2::Stat,
  # `test_y` holds the values of the continuous axis before axis limits and
  # transformations. It is the only required aesthetic, because ggplot2
  # removes rows with missing or non-finite values in required aesthetics.
  # This way, values outside the axis limits (NA) or not finite on a
  # transformed axis are not removed before the tests. `x` and `y` are always
  # mapped by `tidyplot()`.
  #
  # Flipped plots (discrete y axis) are computed in the orientation of a
  # vertical plot, as in ggplot2's `flip_data()` pattern.
  required_aes = "test_y",
  optional_aes = c("x", "y", "pair_id", "test_group"),
  dropped_aes = c("test_y", "pair_id", "test_group", "colour", "fill", "group", "shape", "size"),

  compute_panel = function(
    data,
    scales,
    spec,
    flipped_aes = FALSE,
    dodge_width = 0.8,
    step.increase = 0.15,
    bracket.nudge.y = 0.1
  ) {
    data <- ggplot2::flip_data(data, flipped_aes)
    if (flipped_aes) {
      scales <- list(x = scales$y, y = scales$x)
    }
    data <- data[!is.na(data$x), , drop = FALSE]
    engine <- test_engines[[spec$method]]$compute
    within_colour <- spec$within_colour
    x_levels <- scales$x$get_limits()
    data <- test_values(data, scales$y)
    axis_name <- scales$y$get_transformation()$name
    if (is.null(data$pair_id)) {
      data$pair_id <- NA
    }
    if (within_colour) {
      group_levels <- if (is.factor(data$test_group)) {
        levels(data$test_group)
      } else {
        sort(unique(as.character(data$test_group)))
      }
      data$.group <- as.character(data$test_group)
      blocks <- split(data, data$x)
    } else {
      group_levels <- x_levels
      data$.group <- x_levels[data$x]
      blocks <- list(data)
    }
    ref <- if (!is.null(spec$ref.group)) test_as_levels(spec$ref.group, group_levels)
    pairs_requested <- lapply(spec$comparisons, test_as_levels, levels = group_levels)

    rows <- list()
    for (block in blocks) {
      present <- group_levels[group_levels %in% block$.group]
      pairs <- if (!is.null(spec$comparisons)) {
        Filter(function(cmp) all(cmp %in% present), pairs_requested)
      } else if (!is.null(ref)) {
        if (ref %in% present) {
          lapply(setdiff(present, ref), function(g) c(ref, g))
        } else {
          list()
        }
      } else if (length(present) >= 2) {
        asplit(utils::combn(present, 2), 2)
      } else {
        list()
      }
      if (length(pairs) == 0) {
        next
      }
      # Tests use the untransformed values, see `test_values()`
      res <- engine(
        data.frame(.group = block$.group, y = block$test_y, pair_id = block$pair_id),
        pairs,
        paired = spec$paired,
        args = spec$method_args
      )
      res$group1 <- vapply(pairs, `[[`, character(1), 1)
      res$group2 <- vapply(pairs, `[[`, character(1), 2)
      res <- test_count_not_shown(res, block)
      if (within_colour) {
        k <- length(present)
        offset <- function(g) (match(g, present) - (k + 1) / 2) * dodge_width / k
        res$x_level <- x_levels[block$x[1]]
        res$xmin <- block$x[1] + offset(res$group1)
        res$xmax <- block$x[1] + offset(res$group2)
      } else {
        res$x_level <- NA_character_
        res$xmin <- match(res$group1, x_levels)
        res$xmax <- match(res$group2, x_levels)
      }
      rows[[length(rows) + 1]] <- res
    }
    if (length(rows) == 0) {
      return(data.frame())
    }
    out <- do.call(rbind, rows)

    # Multiple testing
    adjust_block <- if (within_colour && spec$p.adjust.by == "group") {
      out$x_level
    } else {
      rep("panel", nrow(out))
    }
    out$p.adj <- NA_real_
    if (test_engines[[spec$method]]$adjusted) {
      # adjusted by the test itself, see `check_test_spec()`
      out$p.adj <- out$p
    } else {
      for (b in unique(adjust_block)) {
        i <- adjust_block == b
        out$p.adj[i] <- stats::p.adjust(out$p[i], method = spec$p.adjust.method)
      }
    }
    out$p.signif <- test_signif(out$p, spec$symnum.args)
    out$p.adj.signif <- test_signif(out$p.adj, spec$symnum.args)

    failed <- out$status != "computed"
    if (any(failed)) {
      cli::cli_warn(c(
        "{sum(failed)} statistical comparison{?s} could not be computed.",
        "i" = "{unique(out$status[failed])}",
        "i" = "See {.fn view_log} for details."
      ))
    }

    out$notes <- test_notes_not_shown(out, axis_name)
    not_shown <- out$n_outside + out$n_not_finite > 0
    if (any(not_shown)) {
      cli::cli_warn(c(
        "{sum(not_shown)} statistical comparison{?s} include{?s/} values that are not shown in the plot.",
        "i" = "Tests use all values in the data, regardless of axis limits and transformations.",
        "i" = "See {.fn view_log} for details."
      ))
    }

    out$shown <- !failed & !test_hidden(out, spec$hide.ns)
    out$label <- test_labels(out, spec$label)
    # Brackets are placed above the values shown in the plot
    y_shown <- data$y[is.finite(data$y)]
    y_max <- if (length(y_shown) > 0) max(y_shown) else 0
    y_range <- diff(range(y_shown))
    if (!is.finite(y_range) || y_range == 0) {
      y_range <- max(abs(y_max), 1)
    }
    block <- if (within_colour) out$x_level else rep("panel", nrow(out))
    out$y <- test_bracket_y(
      rep(y_max, nrow(out)),
      out$shown,
      block,
      y_max,
      y_range,
      bracket.nudge.y,
      step.increase
    )
    out$x <- (out$xmin + out$xmax) / 2
    out$flipped_aes <- flipped_aes
    ggplot2::flip_data(out, flipped_aes)
  }
)

# Pre-computed statistics: positions are given, only stacking and hiding
# Flipped plots are computed in the orientation of a vertical plot.
StatTidyplotsBracket <- ggplot2::ggproto(
  "StatTidyplotsBracket",
  ggplot2::Stat,
  required_aes = c("xmin|ymin", "xmax|ymax", "y|x", "label"),
  optional_aes = "hidden",
  dropped_aes = "hidden",

  compute_panel = function(
    data,
    scales,
    step.increase = 0,
    bracket.nudge.y = 0,
    flipped_aes = FALSE
  ) {
    data <- ggplot2::flip_data(data, flipped_aes)
    value_scale <- if (flipped_aes) scales$x else scales$y
    y_range <- diff(value_scale$dimension())
    if (!is.finite(y_range) || y_range == 0) {
      y_range <- 1
    }
    shown <- !(data$hidden %in% TRUE)
    data$y[shown] <- data$y[shown] +
      y_range * (bracket.nudge.y + step.increase * (seq_len(sum(shown)) - 1))
    data$shown <- shown
    data$x <- (data$xmin + data$xmax) / 2
    data$flipped_aes <- flipped_aes
    ggplot2::flip_data(data, flipped_aes)
  }
)

# In flipped plots (discrete y axis), brackets are drawn in the orientation
# of a vertical plot and then flipped. Labels are rotated by -90 degrees, like
# the title of a secondary y axis, so `vjust` and the stacking of brackets
# work the same in both orientations.
GeomTidyplotsBracket <- ggplot2::ggproto(
  "GeomTidyplotsBracket",
  ggplot2::Geom,
  required_aes = c("xmin|ymin", "xmax|ymax", "y|x", "label"),
  default_aes = ggplot2::aes(
    colour = "black",
    linewidth = 0.3,
    linetype = 1,
    alpha = NA,
    size = 7 / ggplot2::.pt,
    family = "",
    fontface = 1,
    lineheight = 1.2,
    angle = 0,
    hjust = 0.5,
    vjust = -0.25
  ),

  draw_panel = function(
    data,
    panel_params,
    coord,
    tip.length = 0.02,
    remove.bracket = FALSE,
    flipped_aes = FALSE
  ) {
    data <- data[data$shown %in% TRUE, , drop = FALSE]
    if (nrow(data) == 0) {
      return(grid::nullGrob())
    }
    data <- ggplot2::flip_data(data, flipped_aes)
    value_range <- diff(if (flipped_aes) panel_params$x.range else panel_params$y.range)
    text <- data
    text$x <- (data$xmin + data$xmax) / 2
    # small gap between bracket and label
    text$y <- data$y + 0.008 * value_range
    if (flipped_aes) {
      text$angle <- text$angle - 90
    }
    text <- ggplot2::flip_data(text, flipped_aes)
    labels <- ggplot2::GeomText$draw_panel(text, panel_params, coord)
    if (remove.bracket) {
      return(labels)
    }
    tip <- tip.length * value_range
    segment <- function(x, xend, y, yend) {
      d <- data
      d$x <- x
      d$xend <- xend
      d$y <- y
      d$yend <- yend
      d
    }
    segments <- rbind(
      segment(data$xmin, data$xmax, data$y, data$y),
      segment(data$xmin, data$xmin, data$y, data$y - tip),
      segment(data$xmax, data$xmax, data$y, data$y - tip)
    )
    segments <- ggplot2::flip_data(segments, flipped_aes)
    grid::gTree(children = grid::gList(
      ggplot2::GeomSegment$draw_panel(segments, panel_params, coord),
      labels
    ))
  }
)

# Drawing parameters from `...`. `size` is accepted as an alias for the
# bracket `linewidth`, as in earlier versions.
test_draw_params <- function(dots, color, label.size, vjust) {
  params <- list(colour = color, size = label.size, vjust = vjust)
  if (!is.null(dots$size)) params$linewidth <- dots$size
  if (!is.null(dots$linewidth)) params$linewidth <- dots$linewidth
  for (p in c("linetype", "alpha", "family", "fontface", "tip.length")) {
    if (!is.null(dots[[p]])) params[[p]] <- dots[[p]]
  }
  params
}
