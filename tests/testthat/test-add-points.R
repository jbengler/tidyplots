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
    add_data_points(confetti = TRUE) %>%
    vdiffr::expect_doppelganger("Add confetti data points", .)

  study %>%
    tidyplot(x = treatment, y = score, color = treatment) %>%
    add_data_points_jitter(confetti = TRUE) %>%
    vdiffr::expect_doppelganger("Add confetti data points jitter", .)

  study %>%
    tidyplot(x = treatment, y = score, color = treatment) %>%
    add_data_points_beeswarm(confetti = TRUE) %>%
    vdiffr::expect_doppelganger("Add confetti data points beaswarm", .)
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
    add_data_points(confetti = TRUE, rasterize = TRUE, rasterize_dpi = 50) %>%
    vdiffr::expect_doppelganger("Rasterize confetti data points", .)

  study %>%
    tidyplot(x = treatment, y = score, color = treatment) %>%
    add_data_points_jitter(confetti = TRUE, rasterize = TRUE, rasterize_dpi = 50) %>%
    vdiffr::expect_doppelganger("Rasterize confetti data points jitter", .)

  study %>%
    tidyplot(x = treatment, y = score, color = treatment) %>%
    add_data_points_beeswarm(confetti = TRUE, rasterize = TRUE, rasterize_dpi = 50) %>%
    vdiffr::expect_doppelganger("Rasterize confetti data points beaswarm", .)
})
