test_that("add annotation", {
  animals %>%
    tidyplot(x = weight, y = size, color = number_of_legs) %>%
    add_data_points() %>%
    add_reference_lines(x = c(4000), y = c(200, 350)) %>%
    vdiffr::expect_doppelganger("add reference lines", .)

  study %>%
    tidyplot(x = treatment, y = score, color = treatment) %>%
    add_mean_dash() %>%
    add_sem_errorbar() %>%
    add_title("My fancy long title") %>%
    add_caption("Here goes the fine print") %>%
    vdiffr::expect_doppelganger("add title and caption", .)
})

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
