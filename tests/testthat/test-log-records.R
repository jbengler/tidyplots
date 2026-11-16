test_that("summary layers record what they show", {
  p <- study |>
    tidyplot(x = treatment, y = score, color = treatment) |>
    add_mean_bar() |>
    add_sem_errorbar() |>
    add_range_errorbar() |>
    add_ci95_errorbar() |>
    add_boxplot(coef = 2)
  steps <- p$tidyplot$log$steps
  expect_equal(steps[[2]]$record, list(statistic = "mean", geom = "bar"))
  expect_equal(
    steps[[3]]$record,
    list(
      statistic = "mean",
      dispersion = "standard error of the mean",
      geom = "errorbar"
    )
  )
  expect_null(steps[[4]]$record$statistic)
  expect_equal(steps[[4]]$record$dispersion, "range (minimum to maximum)")
  expect_match(steps[[5]]$record$random, "bootstrap")
  expect_equal(steps[[6]]$record$statistic, "median")
  expect_match(steps[[6]]$record$whiskers, "2 x interquartile range")
  expect_true(all(log_kinds_of(p)[-1] == "summary"))
})

test_that("point layers record subsets and jitter seeds", {
  p <- study |>
    tidyplot(x = treatment, y = score) |>
    add_data_points() |>
    add_data_points_jitter(data = filter_rows(score > 30), seed = 7)
  steps <- p$tidyplot$log$steps
  expect_equal(steps[[2]]$kind, "layer")
  expect_null(steps[[2]]$record$rows)
  expect_equal(steps[[3]]$record$rows, "filter_rows(score > 30)")
  expect_equal(steps[[3]]$record$seed, 7)
})

test_that("sorting uses the statistic shown in the plot", {
  level_order <- function(p) levels(p$data$treatment)
  by_mean <- levels(forcats::fct_reorder(study$treatment, study$score, mean))
  by_median <- levels(forcats::fct_reorder(study$treatment, study$score, median))

  p0 <- study |> tidyplot(x = treatment, y = score)
  # mean +/- SEM without a mean bar now sorts by mean
  expect_equal(
    level_order(p0 |> add_sem_errorbar() |> sort_x_axis_levels()),
    by_mean
  )
  # via Reduce(), the statistic is not lost
  p_reduce <- Reduce(function(p, f) f(p), list(add_mean_dash), p0)
  expect_equal(get_displayed_stat(p_reduce), "mean")
  # most recent summary wins
  p <- p0 |> add_mean_bar() |> add_median_dash()
  expect_equal(get_displayed_stat(p), "median")
  expect_equal(level_order(p |> sort_x_axis_levels()), by_median)
  # no summary falls back to median
  expect_null(get_displayed_stat(p0 |> add_data_points()))
})

test_that("level transformations record the resolved result", {
  p <- study |>
    tidyplot(x = treatment, y = score) |>
    add_mean_bar() |>
    rename_x_axis_levels(new_names = c("A" = "a")) |>
    reorder_x_axis_levels("D") |>
    reverse_x_axis_levels() |>
    sort_x_axis_levels(.reverse = TRUE)
  steps <- p$tidyplot$log$steps
  expect_equal(steps[[3]]$record$renamed, "A -> a")
  expect_equal(steps[[4]]$record$levels[1], "D")
  expect_equal(steps[[5]]$record$levels[4], "D")
  expect_equal(steps[[6]]$record$sorted_by, "mean of score (descending)")
  expect_equal(steps[[6]]$record$levels, levels(p$data$treatment))
  expect_true(all(log_kinds_of(p)[3:6] == "transform"))
})

test_that("normalization records method, conditions and references", {
  p <- study |>
    tidyplot(x = treatment, y = score) |>
    data_normalize_y(one = treatment == "A")
  r <- p$tidyplot$log$steps[[2]]$record
  expect_equal(r$variable, "score")
  expect_match(r$method, "fold change")
  expect_equal(r$one, 'treatment == "A"')
  expect_null(r$zero)
  expect_equal(r$reference_function, "mean")
  expect_equal(r$references$one, mean(study$score[study$treatment == "A"]))

  p <- climate |>
    tidyplot(year, avg_temperature) |>
    data_normalize_y(zero = year >= 1961 & year <= 1990, by = month)
  r <- p$tidyplot$log$steps[[2]]$record
  expect_equal(r$by, "month")
  expect_equal(nrow(r$references), 12)
})

test_that("axis transforms are logged as transform steps", {
  p <- study |>
    tidyplot(x = treatment, y = score) |>
    adjust_y_axis(transform = "log10") |>
    adjust_y_axis(title = "Score")
  expect_equal(log_kinds_of(p), c("data", "transform", "cosmetic"))
  expect_equal(
    p$tidyplot$log$steps[[2]]$record,
    list(operation = "axis transformation", axis = "y", transform = "log-10")
  )
})

test_that("split_plot() records the faceting", {
  p <- study |>
    tidyplot(x = treatment, y = score) |>
    split_plot(by = group)
  r <- p$tidyplot$log$steps[[2]]$record
  expect_equal(p$tidyplot$log$steps[[2]]$kind, "facet")
  expect_equal(r$by, "group")
  expect_equal(r$levels, c("placebo", "treatment"))
  expect_equal(r$axis_ranges, "per panel")

  pages <- suppressMessages(
    study |>
      tidyplot(x = treatment, y = score) |>
      split_plot(by = participant, ncol = 2, nrow = 2)
  )
  r2 <- pages[[2]]$tidyplot$log$steps[[2]]$record
  expect_equal(r2$page, paste("2 of", length(pages)))
  expect_equal(r2$levels, as.character(sort(unique(pages[[2]]$data$participant))))
})

test_that("add_note() and add_metadata() are logged", {
  p <- study |>
    tidyplot(x = treatment, y = score) |>
    add_note("Two samples excluded after failed QC.") |>
    add_metadata(source = "doi:10.1234/abcd", y_unit = "points") |>
    add_metadata(y_unit = "a.u.")
  expect_equal(log_kinds_of(p), c("data", "note", "metadata", "metadata"))
  expect_equal(p$tidyplot$log$steps[[2]]$record$note, "Two samples excluded after failed QC.")
  expect_equal(
    p$tidyplot$log$header$metadata,
    list(source = "doi:10.1234/abcd", y_unit = "a.u.")
  )
  out <- format(p$tidyplot$log)
  expect_true(any(grepl("\"Two samples excluded after failed QC.\"", out, fixed = TRUE)))
  expect_true(any(grepl("y: score (continuous, a.u.)", out, fixed = TRUE)))
})

test_that("add_note() and add_metadata() validate their input", {
  p <- study |> tidyplot(x = treatment, y = score)
  expect_error(add_note(p, ""), "non-empty")
  expect_error(add_note(p, c("a", "b")), "single")
  expect_error(add_metadata(p), "At least one")
  expect_error(add_metadata(p, y_unit = 1), "y_unit")
})

test_that("add_test_pvalue() records the test specification", {
  p <- study |>
    tidyplot(x = treatment, y = score, color = treatment) |>
    add_test_pvalue(ref.group = "A", p.adjust.method = "holm")
  entry <- p$tidyplot$log$steps[[2]]
  r <- entry$record
  expect_equal(entry$kind, "test")
  expect_equal(r$test, "Welch's t-test")
  expect_equal(r$alternative, "two-sided")
  expect_equal(r$comparisons, "each group vs reference A")
  expect_equal(r$compared, "levels of treatment")
  expect_equal(r$p_adjustment, "Holm")
  expect_equal(r$p_adjustment_scope, "all comparisons within each panel")
  expect_equal(r$shown_as, "p-values")
})

test_that("add_test_asterisks() records one test step with significance thresholds", {
  p <- study |>
    tidyplot(x = dose, y = score, color = group) |>
    add_test_asterisks(method = "wilcox_test", paired_by = participant)
  expect_equal(log_kinds_of(p), c("data", "test"))
  r <- p$tidyplot$log$steps[[2]]$record
  expect_equal(r$test, "Wilcoxon signed-rank test")
  expect_equal(r$paired_by, "participant")
  expect_equal(r$compared, "levels of group within each level of dose")
  expect_equal(r$shown_as, "asterisks")
  expect_equal(r$significance, "*** p < 0.001, ** p < 0.01, * p < 0.05")
})

test_that("pre-computed statistics are recorded as such", {
  stat_df <- data.frame(
    group1 = "A",
    group2 = "B",
    p = 0.025,
    p.signif = "*",
    y.position = 50
  )
  p <- study |>
    tidyplot(x = treatment, y = score) |>
    add_test_asterisks_manual(data = stat_df)
  expect_equal(log_kinds_of(p), c("data", "test"))
  r <- p$tidyplot$log$steps[[2]]$record
  expect_match(r$test, "method not verified")
  expect_equal(r$comparisons, "A vs B")
  expect_equal(r$shown_as, "asterisks")
  expect_equal(r$results$p, 0.025)
})

test_that("transform records name their operation", {
  p <- study |>
    tidyplot(x = treatment, y = score) |>
    data_normalize_y(one = treatment == "A") |>
    rename_x_axis_levels(new_names = c("A" = "a")) |>
    reorder_x_axis_levels("D") |>
    reverse_x_axis_levels() |>
    sort_x_axis_levels() |>
    adjust_y_axis(transform = "sqrt") |>
    flip_plot()
  operations <- vapply(
    p$tidyplot$log$steps[-1],
    function(e) e$record$operation,
    character(1)
  )
  expect_equal(
    operations,
    c(
      "normalization",
      "rename levels",
      "reorder levels",
      "reverse levels",
      "sort levels",
      "axis transformation",
      "swap x and y axes"
    )
  )
})

test_that("records do not use the word scale for different things", {
  p <- study |>
    tidyplot(x = treatment, y = score, color = treatment) |>
    add_violin() |>
    add_barstack_relative()
  fields <- unlist(lapply(p$tidyplot$log$steps, function(e) names(e$record)))
  expect_false(any(grepl("scale", fields)))
  expect_equal(p$tidyplot$log$steps[[2]]$record$scaling, "equal maximum width")
  expect_equal(
    p$tidyplot$log$steps[[3]]$record$values,
    "relative (proportion of total)"
  )
})
