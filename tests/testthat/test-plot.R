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

test_that("split plot works", {
  study |>
    tidyplot(x = group, y = score, color = group) |>
    add_boxplot() |>
    split_plot(by = dose) |>
    vdiffr::expect_doppelganger("split plot treatment", fig = _)

  gene_expression |>
    head(20*9) |>
    tidyplot(x = group, y = expression, color = group) |>
    add_mean_dash() |>
    add_data_points_beeswarm(white_border = TRUE) |>
    add_test_asterisks(comparisons = list(c(1,3), c(2,4))) |>
    adjust_size(25, 25) |>
    split_plot(by = external_gene_name) |>
    vdiffr::expect_doppelganger("split plot gene expression", fig = _)

  energy |>
    dplyr::filter(year %in% c(2005, 2010, 2015, 2020)) |>
    tidyplot(y = energy, color = energy_source) |>
    add_donut() |>
    adjust_size(width = 25, height = 25) |>
    split_plot(by = year) |>
    vdiffr::expect_doppelganger("split plot energy", fig = _)
})
