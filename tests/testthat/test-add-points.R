test_that("add points works", {
  study |>
    tidyplot(x = treatment, y = score, color = treatment) |>
    add_data_points() |>
    vdiffr::expect_doppelganger("Add data points", fig = _)

  study |>
    tidyplot(x = treatment, y = score, color = treatment) |>
    add_data_points_jitter() |>
    vdiffr::expect_doppelganger("Add data points jitter", fig = _)

  study |>
    tidyplot(x = treatment, y = score, color = treatment) |>
    add_data_points_beeswarm() |>
    vdiffr::expect_doppelganger("Add data points beaswarm", fig = _)

  study |>
    tidyplot(x = treatment, y = score, color = treatment) |>
    add_data_points(white_border = TRUE) |>
    vdiffr::expect_doppelganger("Add white_border data points", fig = _)

  study |>
    tidyplot(x = treatment, y = score, color = treatment) |>
    add_data_points_jitter(white_border = TRUE) |>
    vdiffr::expect_doppelganger("Add white_border data points jitter", fig = _)

  study |>
    tidyplot(x = treatment, y = score, color = treatment) |>
    add_data_points_beeswarm(white_border = TRUE) |>
    vdiffr::expect_doppelganger("Add white_border data points beaswarm", fig = _)
})

test_that("rasterize works", {
  study |>
    tidyplot(x = treatment, y = score, color = treatment) |>
    add_data_points(rasterize = TRUE, rasterize_dpi = 50) |>
    vdiffr::expect_doppelganger("Rasterize data points", fig = _)

  study |>
    tidyplot(x = treatment, y = score, color = treatment) |>
    add_data_points_jitter(rasterize = TRUE, rasterize_dpi = 50) |>
    vdiffr::expect_doppelganger("Rasterize data points jitter", fig = _)

  study |>
    tidyplot(x = treatment, y = score, color = treatment) |>
    add_data_points_beeswarm(rasterize = TRUE, rasterize_dpi = 50) |>
    vdiffr::expect_doppelganger("Rasterize data points beaswarm", fig = _)

  study |>
    tidyplot(x = treatment, y = score, color = treatment) |>
    add_data_points(white_border = TRUE, rasterize = TRUE, rasterize_dpi = 50) |>
    vdiffr::expect_doppelganger("Rasterize white_border data points", fig = _)

  study |>
    tidyplot(x = treatment, y = score, color = treatment) |>
    add_data_points_jitter(white_border = TRUE, rasterize = TRUE, rasterize_dpi = 50) |>
    vdiffr::expect_doppelganger("Rasterize white_border data points jitter", fig = _)

  study |>
    tidyplot(x = treatment, y = score, color = treatment) |>
    add_data_points_beeswarm(white_border = TRUE, rasterize = TRUE, rasterize_dpi = 50) |>
    vdiffr::expect_doppelganger("Rasterize white_border data points beaswarm", fig = _)
})
