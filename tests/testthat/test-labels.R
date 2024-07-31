test_that("x labels work", {
  study %>%
    tidyplot(treatment, score, color = treatment) %>%
    add_data_points_beeswarm() %>%
    reorder_x_axis_labels("D") %>%
    vdiffr::expect_doppelganger("reorder x", .)

  study %>%
    tidyplot(treatment, score, color = treatment) %>%
    add_data_points_beeswarm() %>%
    rename_x_axis_labels(c("A" = "Hallo")) %>%
    vdiffr::expect_doppelganger("rename x", .)

  study %>%
    tidyplot(treatment, score, color = treatment) %>%
    add_data_points_beeswarm() %>%
    sort_x_axis_labels() %>%
    vdiffr::expect_doppelganger("sort x 1", .)

  animals %>%
    tidyplot(family, size, color = family) %>%
    add_mean_bar() %>%
    sort_x_axis_labels() %>%
    vdiffr::expect_doppelganger("sort x 2", .)

  animals %>%
    tidyplot(family, size, color = family) %>%
    add_mean_bar() %>%
    reverse_x_axis_labels() %>%
    vdiffr::expect_doppelganger("reverse x", .)

  study %>%
    tidyplot(treatment, score, color = treatment) %>%
    add_data_points_beeswarm() %>%
    reorder_x_axis_labels("D") %>%
    sort_x_axis_labels() %>%
    reverse_x_axis_labels() %>%
    rename_x_axis_labels(c("A" = "Hallo")) %>%
    vdiffr::expect_doppelganger("combined x", .)
})

test_that("y labels work", {
  study %>%
    tidyplot(score, treatment, color = treatment) %>%
    add_data_points_beeswarm() %>%
    reorder_y_axis_labels("D") %>%
    vdiffr::expect_doppelganger("reorder y", .)

  study %>%
    tidyplot(score, treatment, color = treatment) %>%
    add_data_points_beeswarm() %>%
    rename_y_axis_labels(c("A" = "Hallo")) %>%
    vdiffr::expect_doppelganger("rename y", .)

  study %>%
    tidyplot(score, treatment, color = treatment) %>%
    add_data_points_beeswarm() %>%
    sort_y_axis_labels() %>%
    vdiffr::expect_doppelganger("sort y 1", .)

  animals %>%
    tidyplot(size, family, color = family) %>%
    add_mean_bar() %>%
    sort_y_axis_labels() %>%
    vdiffr::expect_doppelganger("sort y 2", .)

  animals %>%
    tidyplot(size, family, color = family) %>%
    add_mean_bar() %>%
    reverse_y_axis_labels() %>%
    vdiffr::expect_doppelganger("reverse y", .)

  study %>%
    tidyplot(score, treatment, color = treatment) %>%
    add_data_points_beeswarm() %>%
    reorder_y_axis_labels("D") %>%
    sort_y_axis_labels() %>%
    reverse_y_axis_labels() %>%
    rename_y_axis_labels(c("A" = "Hallo")) %>%
    vdiffr::expect_doppelganger("combined y", .)
})

test_that("color labels work", {
  study %>%
    tidyplot(group, score, color = treatment) %>%
    add_data_points_beeswarm() %>%
    reorder_color_labels("D") %>%
    vdiffr::expect_doppelganger("reorder color", .)

  study %>%
    tidyplot(group, score, color = treatment) %>%
    add_data_points_beeswarm() %>%
    rename_color_labels(c("A" = "Hallo")) %>%
    vdiffr::expect_doppelganger("rename color", .)

  study %>%
    tidyplot(group, score, color = treatment) %>%
    add_data_points_beeswarm() %>%
    sort_color_labels() %>%
    vdiffr::expect_doppelganger("sort color 1", .)

  animals %>%
    tidyplot(family, size, color = family) %>%
    add_mean_bar() %>%
    sort_color_labels() %>%
    vdiffr::expect_doppelganger("sort color 2", .)

  animals %>%
    tidyplot(family, size, color = family) %>%
    add_mean_bar() %>%
    reverse_color_labels() %>%
    vdiffr::expect_doppelganger("reverse color", .)

  study %>%
    tidyplot(group, score, color = treatment) %>%
    add_data_points_beeswarm() %>%
    reorder_color_labels("D") %>%
    sort_color_labels() %>%
    reverse_color_labels() %>%
    rename_color_labels(c("A" = "Hallo")) %>%
    vdiffr::expect_doppelganger("combined color", .)
})
