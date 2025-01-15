test_that("add boxplot and violin works", {
  study %>%
    tidyplot(x = treatment, y = score, color = treatment) %>%
    add_boxplot() %>%
    vdiffr::expect_doppelganger("boxplot", .)

  study %>%
    tidyplot(x = dose, y = score, color = group) %>%
    add_boxplot() %>%
    add_data_points_beeswarm() %>%
    vdiffr::expect_doppelganger("boxplot grouped", .)

  # violin plots fail on ci platforms
  # study %>%
  #   tidyplot(x = treatment, y = score, color = treatment) %>%
  #   add_violin() %>%
  #   add_data_points_beeswarm() %>%
  #   vdiffr::expect_doppelganger("violin", .)
  #
  # study %>%
  #   tidyplot(x = dose, y = score, color = group) %>%
  #   add_violin() %>%
  #   add_data_points_beeswarm() %>%
  #   vdiffr::expect_doppelganger("violin grouped", .)
})
