if (Sys.getenv("CI") == "true") {
  testthat::skip("Skipping vdiffr tests on CI")
}

test_that("x levels work", {
  study |>
    tidyplot(treatment, score, color = treatment) |>
    add_data_points_beeswarm() |>
    reorder_x_axis_levels("D") |>
    vdiffr::expect_doppelganger("reorder x", fig = _)

  study |>
    tidyplot(treatment, score, color = treatment) |>
    add_data_points_beeswarm() |>
    rename_x_axis_levels(c("A" = "Hallo")) |>
    vdiffr::expect_doppelganger("rename x", fig = _)

  study |>
    tidyplot(treatment, score, color = treatment) |>
    add_data_points_beeswarm() |>
    sort_x_axis_levels() |>
    vdiffr::expect_doppelganger("sort x 1", fig = _)

  animals |>
    tidyplot(family, size, color = family) |>
    add_mean_bar() |>
    sort_x_axis_levels() |>
    vdiffr::expect_doppelganger("sort x 2", fig = _)

  spendings |>
    tidyplot(category, color = category) |>
    add_count_bar() |>
    sort_x_axis_levels() |>
    vdiffr::expect_doppelganger("sort x count", fig = _)

  spendings |>
    tidyplot(category, amount, color = category) |>
    add_sum_bar() |>
    sort_x_axis_levels() |>
    vdiffr::expect_doppelganger("sort x sum", fig = _)

  animals |>
    tidyplot(family, size, color = family) |>
    add_mean_bar() |>
    reverse_x_axis_levels() |>
    vdiffr::expect_doppelganger("reverse x", fig = _)

  study |>
    tidyplot(treatment, score, color = treatment) |>
    add_data_points_beeswarm() |>
    reorder_x_axis_levels("D") |>
    sort_x_axis_levels() |>
    reverse_x_axis_levels() |>
    rename_x_axis_levels(c("A" = "Hallo")) |>
    vdiffr::expect_doppelganger("combined x", fig = _)
})

test_that("y levels work", {
  study |>
    tidyplot(score, treatment, color = treatment) |>
    add_data_points_beeswarm() |>
    reorder_y_axis_levels("D") |>
    vdiffr::expect_doppelganger("reorder y", fig = _)

  study |>
    tidyplot(score, treatment, color = treatment) |>
    add_data_points_beeswarm() |>
    rename_y_axis_levels(c("A" = "Hallo")) |>
    vdiffr::expect_doppelganger("rename y", fig = _)

  study |>
    tidyplot(score, treatment, color = treatment) |>
    add_data_points_beeswarm() |>
    sort_y_axis_levels() |>
    vdiffr::expect_doppelganger("sort y 1", fig = _)

  animals |>
    tidyplot(size, family, color = family) |>
    add_mean_bar() |>
    sort_y_axis_levels() |>
    vdiffr::expect_doppelganger("sort y 2", fig = _)

  spendings |>
    tidyplot(y = category, color = category) |>
    add_count_bar() |>
    sort_y_axis_levels() |>
    vdiffr::expect_doppelganger("sort y count", fig = _)

  spendings |>
    tidyplot(y = category, x = amount, color = category) |>
    add_sum_bar() |>
    sort_y_axis_levels() |>
    vdiffr::expect_doppelganger("sort y sum", fig = _)

  animals |>
    tidyplot(size, family, color = family) |>
    add_mean_bar() |>
    reverse_y_axis_levels() |>
    vdiffr::expect_doppelganger("reverse y", fig = _)

  study |>
    tidyplot(score, treatment, color = treatment) |>
    add_data_points_beeswarm() |>
    reorder_y_axis_levels("D") |>
    sort_y_axis_levels() |>
    reverse_y_axis_levels() |>
    rename_y_axis_levels(c("A" = "Hallo")) |>
    vdiffr::expect_doppelganger("combined y", fig = _)
})

test_that("color levels work", {
  study |>
    tidyplot(group, score, color = treatment) |>
    add_data_points_beeswarm() |>
    reorder_color_levels("D") |>
    vdiffr::expect_doppelganger("reorder color", fig = _)

  study |>
    tidyplot(group, score, color = treatment) |>
    add_data_points_beeswarm() |>
    rename_color_levels(c("A" = "Hallo")) |>
    vdiffr::expect_doppelganger("rename color", fig = _)

  study |>
    tidyplot(group, score, color = treatment) |>
    add_data_points_beeswarm() |>
    sort_color_levels() |>
    vdiffr::expect_doppelganger("sort color 1", fig = _)

  animals |>
    tidyplot(family, size, color = family) |>
    add_mean_bar() |>
    sort_color_levels() |>
    vdiffr::expect_doppelganger("sort color 2", fig = _)

  spendings |>
    tidyplot(y = category, color = category) |>
    add_count_bar() |>
    sort_color_levels() |>
    vdiffr::expect_doppelganger("sort color count", fig = _)

  spendings |>
    tidyplot(y = category, x = amount, color = category) |>
    add_sum_bar() |>
    sort_color_levels() |>
    vdiffr::expect_doppelganger("sort color sum", fig = _)

  animals |>
    tidyplot(family, size, color = family) |>
    add_mean_bar() |>
    reverse_color_levels() |>
    vdiffr::expect_doppelganger("reverse color", fig = _)

  study |>
    tidyplot(group, score, color = treatment) |>
    add_data_points_beeswarm() |>
    reorder_color_levels("D") |>
    sort_color_levels() |>
    reverse_color_levels() |>
    rename_color_levels(c("A" = "Hallo")) |>
    vdiffr::expect_doppelganger("combined color", fig = _)
})

test_that("sorting uses the statistic of the most recent summary layer", {
  # mean sorts b before a, median sorts a before b
  df <- data.frame(
    group = rep(c("a", "b"), each = 3),
    value = c(1, 1, 10, 2, 2, 2)
  )
  sorted <- function(p) levels(sort_x_axis_levels(p)$data$group)
  p <- tidyplot(df, x = group, y = value)

  expect_equal(sorted(p), c("a", "b"))
  expect_equal(sorted(p |> add_sem_errorbar()), c("b", "a"))
  expect_equal(sorted(p |> add_mean_bar()), c("b", "a"))
  expect_equal(sorted(p |> add_median_bar()), c("a", "b"))
  expect_equal(sorted(p |> add_mean_bar() |> add_boxplot()), c("a", "b"))
  expect_equal(sorted(p |> add_boxplot() |> add_mean_dash()), c("b", "a"))
  expect_equal(sorted(p |> add_mean_bar() |> add_range_errorbar()), c("b", "a"))
  expect_equal(
    sorted(p |> add(ggplot2::stat_summary(fun = mean, geom = "point"))),
    c("b", "a")
  )

  # stacked bars sort by sum, i.e. the bar height
  df$stack <- rep(c("x", "y", "z"), 2)
  stacked <- tidyplot(df, x = group, y = value, color = stack) |>
    add_barstack_absolute()
  expect_equal(sorted(stacked), c("b", "a"))

  # unreadable layers fall back to the median
  broken <- p
  broken$layers <- list(function() NULL)
  expect_null(get_layer_statistic(broken))
})
