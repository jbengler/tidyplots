test_that("add points works", {
  study %>%
    tidyplot(x = treatment, y = score, color = treatment) %>%
    add_data_points() %>%
    vdiffr::expect_doppelganger("Add data points", .)

  study %>%
    tidyplot(x = treatment, y = score, color = treatment) %>%
    add_data_points_jitter() %>%
    vdiffr::expect_doppelganger("Add data points jitter", .)

  study %>%
    tidyplot(x = treatment, y = score, color = treatment) %>%
    add_data_points_beeswarm() %>%
    vdiffr::expect_doppelganger("Add data points beaswarm", .)

  study %>%
    tidyplot(x = treatment, y = score, color = treatment) %>%
    add_data_points(white_border = TRUE) %>%
    vdiffr::expect_doppelganger("Add white_border data points", .)

  study %>%
    tidyplot(x = treatment, y = score, color = treatment) %>%
    add_data_points_jitter(white_border = TRUE) %>%
    vdiffr::expect_doppelganger("Add white_border data points jitter", .)

  study %>%
    tidyplot(x = treatment, y = score, color = treatment) %>%
    add_data_points_beeswarm(white_border = TRUE) %>%
    vdiffr::expect_doppelganger("Add white_border data points beaswarm", .)
})

test_that("rasterize works", {
  study %>%
    tidyplot(x = treatment, y = score, color = treatment) %>%
    add_data_points(rasterize = TRUE, rasterize_dpi = 50) %>%
    vdiffr::expect_doppelganger("Rasterize data points", .)

  study %>%
    tidyplot(x = treatment, y = score, color = treatment) %>%
    add_data_points_jitter(rasterize = TRUE, rasterize_dpi = 50) %>%
    vdiffr::expect_doppelganger("Rasterize data points jitter", .)

  study %>%
    tidyplot(x = treatment, y = score, color = treatment) %>%
    add_data_points_beeswarm(rasterize = TRUE, rasterize_dpi = 50) %>%
    vdiffr::expect_doppelganger("Rasterize data points beaswarm", .)

  study %>%
    tidyplot(x = treatment, y = score, color = treatment) %>%
    add_data_points(white_border = TRUE, rasterize = TRUE, rasterize_dpi = 50) %>%
    vdiffr::expect_doppelganger("Rasterize white_border data points", .)

  study %>%
    tidyplot(x = treatment, y = score, color = treatment) %>%
    add_data_points_jitter(white_border = TRUE, rasterize = TRUE, rasterize_dpi = 50) %>%
    vdiffr::expect_doppelganger("Rasterize white_border data points jitter", .)

  study %>%
    tidyplot(x = treatment, y = score, color = treatment) %>%
    add_data_points_beeswarm(white_border = TRUE, rasterize = TRUE, rasterize_dpi = 50) %>%
    vdiffr::expect_doppelganger("Rasterize white_border data points beaswarm", .)
})
