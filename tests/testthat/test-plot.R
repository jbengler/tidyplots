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

  p1 <-
    energy |>
    dplyr::filter(year %in% c(2005, 2010, 2015, 2020)) |>
    tidyplot(y = energy, color = energy_source) |>
    add_donut() |>
    adjust_size(width = 25, height = 25) |>
    split_plot(by = year, ncol = 1, nrow = 2)

    p1[[1]] |> vdiffr::expect_doppelganger("split plot energy multipage 1", fig = _)
    p1[[2]] |> vdiffr::expect_doppelganger("split plot energy multipage 2", fig = _)

    p <- energy |>
      dplyr::mutate(decade = paste0(floor(year / 10) * 10, "s")) |>
      tidyplot(x = year, y = energy, color = energy_source) |>
      add_barstack_absolute() |>
      adjust_x_axis(rotate_labels = TRUE) |>
      adjust_size(20,20)

    p |> split_plot(by = energy_type) |> vdiffr::expect_doppelganger("split plot by", fig = _)
    p |> split_plot(by = energy_type, axis.titles = "margins") |> vdiffr::expect_doppelganger("split plot by axis titles margins", fig = _)
    p |> split_plot(by = energy_type, axis.titles = "all") |> vdiffr::expect_doppelganger("split plot by axis titles all", fig = _)
    p |> split_plot(by = energy_type, axis.titles = "single") |> vdiffr::expect_doppelganger("split plot by axis titles single", fig = _)
    p |> split_plot(rows = decade, cols = energy_type) |> vdiffr::expect_doppelganger("split plot rows cols", fig = _)
    p |> split_plot(rows = decade, cols = energy_type, scales = "free") |> vdiffr::expect_doppelganger("split plot rows cols scales free", fig = _)
    p |> split_plot(rows = decade, cols = energy_type, axes = "margins") |> vdiffr::expect_doppelganger("split plot rows cols axes margins", fig = _)
    p |> split_plot(rows = decade, cols = energy_type, axis.titles = "margins") |> vdiffr::expect_doppelganger("split plot rows cols axis titles margins", fig = _)
    p |> split_plot(rows = decade, cols = energy_type, axis.titles = "all") |> vdiffr::expect_doppelganger("split plot rows cols axis titles all", fig = _)
    p |> split_plot(rows = decade, cols = energy_type, axis.titles = "single") |> vdiffr::expect_doppelganger("split plot rows cols axis titles single", fig = _)
    p |> split_plot(rows = energy_type) |> vdiffr::expect_doppelganger("split plot rows", fig = _)
    p |> split_plot(cols = energy_type) |> vdiffr::expect_doppelganger("split plot cols", fig = _)

    p <- energy |>
      dplyr::mutate(decade = paste0(floor(year / 10) * 10, "s")) |>
      tidyplot(y = energy, color = energy_source) |>
      add_donut() |>
      adjust_size(20,20)

    p |> split_plot(by = energy_type) |> vdiffr::expect_doppelganger("split plot donut by", fig = _)
    p |> split_plot(rows = decade, cols = energy_type) |> vdiffr::expect_doppelganger("split plot donut rows cols", fig = _)
    p |> split_plot(rows = energy_type) |> vdiffr::expect_doppelganger("split plot donut rows", fig = _)
    p |> split_plot(cols = energy_type) |> vdiffr::expect_doppelganger("split plot donut cols", fig = _)
})
