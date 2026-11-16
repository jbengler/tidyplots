test_that("tidyplot() creates the log header and a data entry", {
  p <- study |> tidyplot(x = treatment, y = score, color = treatment)
  log <- p$tidyplot$log
  expect_s3_class(log, "tidyplot_log")
  expect_equal(log$header$schema, "0.1")
  expect_match(log$header$figure_id, "^tp-[0-9a-f]{12}$")
  expect_equal(log$header$data$name, "study")
  expect_equal(log$header$data$hash, rlang::hash(study))
  expect_equal(log$header$data$nrow, nrow(study))
  expect_true(all(c("R", "tidyplots", "ggplot2") %in% names(log$header$software)))
  expect_equal(log_funs(p), "tidyplot")
  expect_equal(log_kinds_of(p), "data")
})

test_that("only top-level steps are logged, in pipeline order", {
  p <- study |>
    tidyplot(x = treatment, y = score, color = treatment) |>
    add_mean_bar(alpha = 0.4) |>
    add_sem_errorbar() |>
    add_data_points() |>
    add_test_asterisks() |>
    rename_x_axis_levels(new_names = c("A" = "a")) |>
    remove_x_axis()
  expect_equal(
    log_funs(p),
    c(
      "tidyplot",
      "add_mean_bar",
      "add_sem_errorbar",
      "add_data_points",
      "add_test_asterisks",
      "rename_x_axis_levels",
      "remove_x_axis"
    )
  )
})

test_that("the log does not depend on how functions are invoked", {
  p0 <- study |> tidyplot(x = treatment, y = score)
  expected <- c("tidyplot", "add_mean_bar", "add_data_points")

  plain <- p0 |> add_mean_bar() |> add_data_points()
  namespaced <- p0 |> tidyplots::add_mean_bar() |> tidyplots::add_data_points()
  reduced <- Reduce(
    function(p, f) f(p),
    list(add_mean_bar, add_data_points),
    p0
  )
  purrred <- purrr::reduce(
    list(add_mean_bar, add_data_points),
    function(p, f) f(p),
    .init = p0
  )
  wrapper <- function(p) p |> add_mean_bar() |> add_data_points()

  expect_equal(log_funs(plain), expected)
  expect_equal(log_funs(namespaced), expected)
  expect_equal(log_funs(reduced), expected)
  expect_equal(log_funs(purrred), expected)
  expect_equal(log_funs(wrapper(p0)), expected)
})

test_that("calls are recorded without the upstream pipeline", {
  p <- study |>
    tidyplot(x = treatment, y = score) |>
    add_mean_bar(alpha = 0.4)
  expect_equal(p$tidyplot$log$steps[[2]]$call, quote(add_mean_bar(alpha = 0.4)))
})

test_that("steps of my_style are logged as user steps", {
  my_style <- function(x) x |> adjust_font(family = "mono")
  p <- study |> tidyplot(x = treatment, y = score, my_style = my_style)
  expect_equal(log_funs(p), c("tidyplot", "adjust_font"))
})

test_that("add() is logged as an external step", {
  p <- study |>
    tidyplot(x = treatment, y = score) |>
    add(ggplot2::geom_point())
  expect_equal(log_kinds_of(p), c("data", "external"))
  expect_equal(p$tidyplot$log$steps[[2]]$call, quote(add(object = ggplot2::geom_point())))
})

test_that("view_log() and view_plot() do not add steps", {
  p <- study |> tidyplot(x = treatment, y = score)
  expect_output(p2 <- view_log(p), "tidyplot log")
  expect_equal(log_funs(p2), "tidyplot")
  pdf(NULL)
  p3 <- view_plot(p)
  dev.off()
  expect_equal(log_funs(p3), "tidyplot")
})

test_that("every plot-modifying export adds exactly one log entry", {
  disc <- study |> tidyplot(x = treatment, y = score, color = treatment)
  flipped <- study |> tidyplot(x = score, y = treatment, color = treatment)
  x_only <- study |> tidyplot(x = treatment, color = treatment)
  cont_x <- study |> tidyplot(x = score)
  cont_color <- study |> tidyplot(x = treatment, y = score, color = score)
  color_only <- study |> tidyplot(color = treatment)
  time <- time_course |> tidyplot(x = day, y = score, color = treatment)
  stat_df <- data.frame(
    group1 = "A",
    group2 = "B",
    p = 0.025,
    p.signif = "*",
    y.position = 50
  )

  special <- list(
    add = list(disc, object = ggplot2::geom_point()),
    add_note = list(disc, note = "A note"),
    data_exclude = list(disc, quote(score > 40), reason = "A reason"),
    add_metadata = list(disc, source = "A source"),
    add_annotation_text = list(disc, text = "a", x = 1, y = 1),
    add_annotation_rectangle = list(disc, xmin = 1, xmax = 2, ymin = 1, ymax = 2),
    add_annotation_line = list(disc, x = 1, xend = 2, y = 1, yend = 2),
    add_area = list(time),
    add_line = list(time),
    add_count_area = list(x_only),
    add_count_bar = list(x_only),
    add_count_dash = list(x_only),
    add_count_dot = list(x_only),
    add_count_line = list(x_only),
    add_count_value = list(x_only),
    add_histogram = list(cont_x),
    add_pie = list(color_only),
    add_donut = list(color_only),
    add_test_pvalue_manual = list(disc, data = stat_df),
    add_test_asterisks_manual = list(disc, data = stat_df),
    data_normalize_y = list(disc, one = quote(treatment == "A")),
    data_normalize_x = list(flipped, one = quote(treatment == "A")),
    data_normalize_color = list(cont_color, one = quote(treatment == "A")),
    rename_x_axis_levels = list(disc, new_names = c("A" = "a")),
    rename_x_axis_labels = list(disc, new_names = c("A" = "a")),
    rename_color_levels = list(disc, new_names = c("A" = "a")),
    rename_color_labels = list(disc, new_names = c("A" = "a")),
    rename_y_axis_levels = list(flipped, new_names = c("A" = "a")),
    rename_y_axis_labels = list(flipped, new_names = c("A" = "a"))
  )
  for (f in c(
    "reorder_y_axis_levels",
    "reorder_y_axis_labels",
    "reverse_y_axis_levels",
    "reverse_y_axis_labels",
    "sort_y_axis_levels",
    "sort_y_axis_labels"
  )) {
    special[[f]] <- list(flipped)
  }

  not_logged <- c(
    "tidyplot",
    "save_plot",
    "save_bundle",
    "split_plot",
    "view_plot",
    "view_log",
    "tidyplots_options",
    "new_color_scheme",
    "format_p_value",
    "all_rows",
    "filter_rows",
    "first_rows",
    "last_rows",
    "max_rows",
    "min_rows",
    "sample_rows",
    "%>%"
  )
  ns <- asNamespace("tidyplots")
  exports <- sort(getNamespaceExports(ns))
  exports <- exports[vapply(
    exports,
    function(f) is.function(get(f, ns)),
    logical(1)
  )]
  exports <- exports[!exports %in% not_logged & !grepl("^colors_", exports)]

  semantic <- paste0(
    "^(",
    paste(
      c(
        "add_(mean|median|sum|count)_.*",
        "add_(sem|range|sd|ci95)_(errorbar|ribbon)",
        "add_(boxplot|violin|histogram|heatmap|curve_fit|ellipse|pie|donut)",
        "add_(line|area)",
        "add_(bar|area)stack_.*",
        "add_data_(points|labels).*",
        "add_test_.*",
        "data_normalize_.*",
        "(rename|reorder|reverse|sort)_.*",
        "flip_plot",
        "add_note",
        "add_metadata",
        "data_exclude",
        "add"
      ),
      collapse = "|"
    ),
    ")$"
  )

  for (f in exports) {
    args <- special[[f]] %||% list(disc)
    # `data_normalize_*()` take unevaluated conditions
    p <- suppressWarnings(suppressMessages(
      eval(rlang::call2(rlang::sym(f), !!!args))
    ))
    steps <- p$tidyplot$log$steps
    expect_equal(length(steps), 2, label = paste0("number of log steps after ", f, "()"))
    expect_equal(steps[[2]]$fun, f, label = paste0("logged function name for ", f, "()"))
    expect_true(steps[[2]]$kind %in% log_kinds, label = paste0("valid kind for ", f, "()"))
    # Functions that change what the figure shows must record it (rule 2 in
    # R/log-core.R). All others are cosmetic. A new export must be added to
    # `semantic` if it changes what the figure shows.
    if (grepl(semantic, f)) {
      expect_false(
        steps[[2]]$kind %in% c("cosmetic", "unrecorded"),
        label = paste0("semantic record of ", f, "()")
      )
      expect_true(
        length(steps[[2]]$record) > 0 || steps[[2]]$kind == "external",
        label = paste0("non-empty record of ", f, "()")
      )
    } else {
      expect_equal(
        steps[[2]]$kind,
        "cosmetic",
        label = paste0("kind of ", f, "()")
      )
    }
  }
})

test_that("nested internal calls do not overwrite the record of the outer step", {
  # add_mean_bar() calls adjust_y_axis() internally
  p <- study |>
    tidyplot(x = treatment, y = score) |>
    add_mean_bar()
  expect_equal(p$tidyplot$log$steps[[2]]$record$statistic, "mean")
})

test_that("do.call() works and is logged with the correct name", {
  p0 <- study |> tidyplot(x = treatment, y = score)
  p <- do.call(add_mean_bar, list(p0))
  expect_equal(log_funs(p), c("tidyplot", "add_mean_bar"))
  expect_equal(get_displayed_stat(p), "mean")
})

test_that("logging failures never break plotting", {
  # failure while identifying a step
  local_mocked_bindings(log_fun_name = function(...) stop("boom"))
  p <- study |>
    tidyplot(x = treatment, y = score) |>
    add_mean_bar()
  expect_s3_class(p, "tidyplot")
  steps <- p$tidyplot$log$steps
  expect_equal(steps[[2]]$kind, "unrecorded")
  expect_equal(steps[[2]]$record$error, "boom")
  expect_output(view_log(p), "unrecorded")
})

test_that("failures while preparing a record mark the step as unrecorded", {
  local_mocked_bindings(log_levels = function(...) stop("boom"))
  p <- study |>
    tidyplot(x = treatment, y = score) |>
    add_mean_bar() |>
    split_plot(by = group)
  expect_s3_class(p, "tidyplot")
  expect_equal(log_kinds_of(p), c("data", "summary", "unrecorded"))
  # the record of the previous step is untouched
  expect_equal(p$tidyplot$log$steps[[2]]$record$statistic, "mean")
})

test_that("failures while creating the log or figure facts do not break plotting", {
  local_mocked_bindings(
    log_new = function(...) stop("no header"),
    log_figure_facts = function(...) stop("no facts")
  )
  p <- study |>
    tidyplot(x = treatment, y = score) |>
    add_mean_bar()
  expect_s3_class(p, "tidyplot")
  expect_equal(log_funs(p), c("tidyplot", "add_mean_bar"))
  out <- capture.output(view_log(p))
  expect_true(any(grepl("no header", out)))
  expect_true(any(grepl("unavailable: no facts", out)))
})

test_that("ggplot2 components added with + are not logged, add() is", {
  p <- study |>
    tidyplot(x = treatment, y = score) |>
    add_mean_bar()
  p <- p + ggplot2::geom_point()
  expect_equal(log_kinds_of(p), c("data", "summary"))
  q <- study |>
    tidyplot(x = treatment, y = score) |>
    add(ggplot2::geom_point())
  expect_equal(log_kinds_of(q), c("data", "external"))
})
