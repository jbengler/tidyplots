test_that("tidyplot works", {
  study |>
    tidyplot(x = dose, y = score, color = group, width = 25) |>
    add_boxplot() |>
    vdiffr::expect_doppelganger("tidyplot width", fig = _)

  study |>
    tidyplot(x = dose, y = score, color = group, height = 25) |>
    add_boxplot() |>
    vdiffr::expect_doppelganger("tidyplot height", fig = _)

  study |>
    tidyplot(x = dose, y = score, color = group, width = 25, height = 25) |>
    add_boxplot() |>
    vdiffr::expect_doppelganger("tidyplot width and height", fig = _)

  study |>
    tidyplot(x = dose, y = score, color = group, width = NA, height = 25) |>
    add_boxplot() |>
    vdiffr::expect_doppelganger("tidyplot width NA and height", fig = _)

  study |>
    tidyplot(x = dose, y = score, color = group, width = 25, height = NA) |>
    add_boxplot() |>
    vdiffr::expect_doppelganger("tidyplot width and height NA", fig = _)

  study |>
    tidyplot(x = dose, y = score, color = group, width = 1, height = 1, unit = "inch") |>
    add_boxplot() |>
    vdiffr::expect_doppelganger("tidyplot width and height inch", fig = _)

  study |>
    tidyplot(x = dose, y = score, color = group, width = NA, height = 1, unit = "inch") |>
    add_boxplot() |>
    vdiffr::expect_doppelganger("tidyplot width NA and height inch", fig = _)
})

test_that("tidyplots options work", {
  study |>
    tidyplot(x = dose, y = score, color = group) |>
    add_boxplot() |>
    vdiffr::expect_doppelganger("tidyplots.options before", fig = _)

  tidyplots_options(
    width = 3,
    height = 4,
    unit = "cm",
    dodge_width = 1,
    my_style = function(x) x |>
      adjust_colors(colors_discrete_apple) |>
      adjust_font(fontsize = 12)
    )
  study |>
    tidyplot(x = dose, y = score, color = group) |>
    add_boxplot() |>
    vdiffr::expect_doppelganger("tidyplots.options after", fig = _)

  tidyplots_options()
  study |>
    tidyplot(x = dose, y = score, color = group) |>
    add_boxplot() |>
    vdiffr::expect_doppelganger("tidyplots.options reset", fig = _)
})
