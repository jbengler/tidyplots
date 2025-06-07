test_that("add boxplot and violin works", {
  study |>
    tidyplot(x = treatment, y = score, color = treatment) |>
    add_boxplot() |>
    vdiffr::expect_doppelganger("boxplot", fig = _)

  study |>
    tidyplot(x = dose, y = score, color = group) |>
    add_boxplot() |>
    add_data_points_beeswarm() |>
    vdiffr::expect_doppelganger("boxplot grouped", fig = _)

  # violin plots fail on ci platforms
  # study |>
  #   tidyplot(x = treatment, y = score, color = treatment) |>
  #   add_violin() |>
  #   add_data_points_beeswarm() |>
  #   vdiffr::expect_doppelganger("violin", fig = _)
  #
  # study |>
  #   tidyplot(x = dose, y = score, color = group) |>
  #   add_violin() |>
  #   add_data_points_beeswarm() |>
  #   vdiffr::expect_doppelganger("violin grouped", fig = _)
})

test_that("histograms work", {
  energy |>
    tidyplot(x = energy) |>
    add_histogram() |>
    vdiffr::expect_doppelganger("histogram", fig = _)

  energy |>
    tidyplot(x = energy) |>
    add_histogram(color = "purple") |>
    vdiffr::expect_doppelganger("histogram custom color", fig = _)
})

