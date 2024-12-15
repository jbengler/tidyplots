test_that("ungrouped plots work", {
  study %>%
    tidyplot(treatment, score, color = treatment) %>%
    add_sem_errorbar() %>%
    add_sd_errorbar() %>%
    add_range_errorbar() %>%
    #add_ci95_errorbar() %>%
    add_data_points() %>%
    vdiffr::expect_doppelganger("Add error bars xy", .)

  study %>%
    tidyplot(score, treatment, color = treatment) %>%
    add_sem_errorbar() %>%
    add_sd_errorbar() %>%
    add_range_errorbar() %>%
    #add_ci95_errorbar() %>%
    add_data_points() %>%
    vdiffr::expect_doppelganger("Add error bars yx", .)

  study %>%
    tidyplot(treatment, score) %>%
    add_sem_ribbon() %>%
    add_sd_ribbon() %>%
    add_range_ribbon() %>%
    #add_ci95_ribbon() %>%
    add_data_points() %>%
    vdiffr::expect_doppelganger("Add error ribbon xy", .)

  study %>%
    tidyplot(score, treatment) %>%
    add_sem_ribbon() %>%
    add_sd_ribbon() %>%
    add_range_ribbon() %>%
    #add_ci95_ribbon() %>%
    add_data_points() %>%
    vdiffr::expect_doppelganger("Add error ribbon yx", .)

  study %>%
    tidyplot(treatment, score) %>%
    add_mean_bar(alpha = 0.4) %>%
    add_mean_dash() %>%
    add_mean_dot() %>%
    add_mean_value() %>%
    add_mean_line() %>%
    add_mean_area(alpha = 0.2, fill = "green") %>%
    add_data_points() %>%
    vdiffr::expect_doppelganger("Add mean xy", .)

  study %>%
    tidyplot(score, treatment) %>%
    add_mean_bar(alpha = 0.4) %>%
    add_mean_dash() %>%
    add_mean_dot() %>%
    add_mean_value() %>%
    add_mean_line() %>%
    add_mean_area(alpha = 0.2, fill = "green") %>%
    add_data_points() %>%
    vdiffr::expect_doppelganger("Add mean yx", .)

  study %>%
    tidyplot(treatment, score) %>%
    add_median_bar(alpha = 0.4) %>%
    add_median_dash() %>%
    add_median_dot() %>%
    add_median_value() %>%
    add_median_line() %>%
    add_median_area(alpha = 0.2, fill = "green") %>%
    add_data_points() %>%
    vdiffr::expect_doppelganger("Add median xy", .)

  study %>%
    tidyplot(score, treatment) %>%
    add_median_bar(alpha = 0.4) %>%
    add_median_dash() %>%
    add_median_dot() %>%
    add_median_value() %>%
    add_median_line() %>%
    add_median_area(alpha = 0.2, fill = "green") %>%
    vdiffr::expect_doppelganger("Add median yx", .)

  study %>%
    tidyplot(treatment, score) %>%
    add_sum_bar(alpha = 0.4) %>%
    add_sum_dash() %>%
    add_sum_dot() %>%
    add_sum_value() %>%
    add_sum_line() %>%
    add_sum_area(alpha = 0.2, fill = "green") %>%
    vdiffr::expect_doppelganger("Add sum xy", .)

  study %>%
    tidyplot(score, treatment) %>%
    add_sum_bar(alpha = 0.4) %>%
    add_sum_dash() %>%
    add_sum_dot() %>%
    add_sum_value(extra_padding = 0.3) %>%
    add_sum_line() %>%
    add_sum_area(alpha = 0.2, fill = "green") %>%
    vdiffr::expect_doppelganger("Add sum yx", .)

  study %>%
    tidyplot(treatment) %>%
    add_count_bar(alpha = 0.4) %>%
    add_count_dash() %>%
    add_count_dot() %>%
    add_count_value() %>%
    add_count_line() %>%
    add_count_area(alpha = 0.2, fill = "green") %>%
    vdiffr::expect_doppelganger("Add count x", .)
})

test_that("grouped plots work", {
  study %>%
    tidyplot(group, score, color = dose) %>%
    add_sem_errorbar() %>%
    add_sd_errorbar() %>%
    add_range_errorbar() %>%
    #add_ci95_errorbar() %>%
    add_data_points() %>%
    vdiffr::expect_doppelganger("Add grouped error bar xy", .)

  study %>%
    tidyplot(group, score, color = dose) %>%
    add_sem_ribbon() %>%
    add_sd_ribbon() %>%
    add_range_ribbon() %>%
    #add_ci95_ribbon() %>%
    add_data_points() %>%
    vdiffr::expect_doppelganger("Add grouped error ribbon xy", .)

  study %>%
    tidyplot(group, score, color = dose) %>%
    add_mean_bar(alpha = 0.4) %>%
    add_mean_dash() %>%
    add_mean_dot() %>%
    add_mean_value() %>%
    add_mean_line() %>%
    add_mean_area(alpha = 0.5) %>%
    add_data_points() %>%
    vdiffr::expect_doppelganger("Add grouped mean xy", .)

  study %>%
    tidyplot(group, score, color = dose) %>%
    add_median_bar(alpha = 0.4) %>%
    add_median_dash() %>%
    add_median_dot() %>%
    add_median_value() %>%
    add_median_line() %>%
    add_median_area(alpha = 0.5) %>%
    add_data_points() %>%
    vdiffr::expect_doppelganger("Add grouped median xy", .)

  study %>%
    tidyplot(group, score, color = dose) %>%
    add_sum_bar(alpha = 0.4) %>%
    add_sum_dash() %>%
    add_sum_dot() %>%
    add_sum_value() %>%
    add_sum_line() %>%
    add_sum_area(alpha = 0.5) %>%
    add_data_points() %>%
    vdiffr::expect_doppelganger("Add grouped sum xy", .)

  study %>%
    tidyplot(group, color = dose) %>%
    add_count_bar(alpha = 0.4) %>%
    add_count_dash() %>%
    add_count_dot() %>%
    add_count_value() %>%
    add_count_line() %>%
    add_count_area(alpha = 0.5) %>%
    vdiffr::expect_doppelganger("Add grouped count x", .)

  study %>%
    tidyplot(treatment, score) %>%
    add_mean_bar(alpha = 0.4) %>%
    add_sem_errorbar() %>%
    add_data_points() %>%
    add_line(group = participant) %>%
    vdiffr::expect_doppelganger("Add grouped line", .)

  study %>%
    tidyplot(group, color = dose, dodge_width = 0.6) %>%
    add_count_bar(alpha = 0.4) %>%
    add_count_dash() %>%
    add_count_dot() %>%
    add_count_value() %>%
    add_count_line() %>%
    vdiffr::expect_doppelganger("change dodge width", .)

  study %>%
    tidyplot(group, color = dose, dodge_width = 1.0) %>%
    add_count_bar(alpha = 0.4, width = 0.2) %>%
    add_count_dash() %>%
    add_count_dot() %>%
    add_count_value() %>%
    add_count_line() %>%
    vdiffr::expect_doppelganger("change dodge width and bar width", .)
})

test_that("dodge_width heuristic works", {
  time_course %>%
    dplyr::filter(!is.na(score)) %>%
    tidyplot(x = day, y = score, color = treatment) %>%
    add_mean_line() %>%
    add_mean_dot() %>%
    add_sem_ribbon() %>%
    vdiffr::expect_doppelganger("no discrete axis, no provided dodge_width", .)

  time_course %>%
    dplyr::filter(!is.na(score)) %>%
    tidyplot(x = day, y = score, color = treatment, dodge_width = 0) %>%
    add_mean_line() %>%
    add_mean_dot() %>%
    add_sem_ribbon() %>%
    vdiffr::expect_doppelganger("no discrete axis, dodge_width = 0", .)

  time_course %>%
    dplyr::filter(!is.na(score)) %>%
    tidyplot(x = day, y = score, color = treatment, dodge_width = 0.8) %>%
    add_mean_line() %>%
    add_mean_dot() %>%
    add_sem_ribbon() %>%
    vdiffr::expect_doppelganger("no discrete axis, dodge_width = 0.8", .)

  study %>%
    tidyplot(x = group, y = score, color = dose, dodge_width = 0) %>%
    add_mean_bar(alpha = 0.4) %>%
    add_mean_dash() %>%
    add_mean_value() %>%
    vdiffr::expect_doppelganger("ONE discrete axis, no provided dodge_width", .)

  study %>%
    tidyplot(x = group, y = score, color = dose, dodge_width = 0.8) %>%
    add_mean_bar(alpha = 0.4) %>%
    add_mean_dash() %>%
    add_mean_value() %>%
    vdiffr::expect_doppelganger("ONE discrete axis, dodge_width = 0", .)

  study %>%
    tidyplot(x = group, y = score, color = dose) %>%
    add_mean_bar(alpha = 0.4) %>%
    add_mean_dash() %>%
    add_mean_value() %>%
    vdiffr::expect_doppelganger("ONE discrete axis, dodge_width = 0.8", .)

  animals %>%
    tidyplot(x = number_of_legs, y = speed, color = activity) %>%
    add_mean_dot() %>%
    add_sem_errorbar() %>%
    vdiffr::expect_doppelganger("ONE discrete axis", .)

  animals %>%
    tidyplot(x = number_of_legs, y = speed, color = activity, dodge_width = 0.4) %>%
    add_mean_dot() %>%
    add_sem_errorbar() %>%
    vdiffr::expect_doppelganger("ONE discrete axis, override", .)

  animals %>%
    dplyr::mutate(number_of_legs = as.numeric(number_of_legs)) %>%
    tidyplot(x = number_of_legs, y = speed, color = activity) %>%
    add_mean_dot() %>%
    add_sem_errorbar() %>%
    vdiffr::expect_doppelganger("NO discrete axis", .)

  animals %>%
    dplyr::mutate(number_of_legs = as.numeric(number_of_legs)) %>%
    tidyplot(x = number_of_legs, y = speed, color = activity, dodge_width = 0.4) %>%
    add_mean_dot() %>%
    add_sem_errorbar() %>%
    vdiffr::expect_doppelganger("NO discrete axis, override", .)
})

# causes spurious warnings
# test_that("add curve fit works", {
#   time_course %>%
#     tidyplot(x = day, y = score) %>%
#     add_curve_fit() %>%
#     vdiffr::expect_doppelganger("curve fit x", .)
#
#   time_course %>%
#     tidyplot(x = score, y = day) %>%
#     add_curve_fit(orientation = "y") %>%
#     vdiffr::expect_doppelganger("curve fit y", .)
#
#   time_course %>%
#     tidyplot(x = day, y = score, color = treatment) %>%
#     add_curve_fit() %>%
#     vdiffr::expect_doppelganger("curve fit grouped", .)
# })
