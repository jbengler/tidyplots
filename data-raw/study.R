## code to prepare `study` dataset goes here

library(tidyverse)

study <-
  dplyr::tibble(treatment = rep(c("A", "B", "C", "D"), each = 5),
                group = rep(c("placebo", "treatment"), each = 10),
                dose = rep(c("high", "low", "high", "low"), each = 5),
                participant = rep(c("p01", "p02", "p03", "p04", "p05", "p06", "p07", "p08", "p09", "p10"), 2),
                age = rep(c(23, 45, 32, 37, 24), 4),
                sex = rep(c("female", "male", "female", "male", "female"), 4),
                score = c(2,4,5,4,6,9,8,12,15,16,32,35,24,45,56,23,25,21,22,23))

study %>%
  tidyplot(treatment, score, color = treatment) %>%
  add_mean_bar(alpha = 0.3) %>%
  add_error() %>%
  add_jitter()

study %>%
  tidyplot(treatment, score) %>%
  add_mean_bar(alpha = 0.3) %>%
  add_error() %>%
  add_points() %>%
  add_line()

study %>%
  tidyplot(treatment, score) %>%
  add_mean_bar(alpha = 0.3) %>%
  add_error() %>%
  add_points() %>%
  add_line(group = participant, dodge_width = 0)

study %>%
  tidyplot(dose, score, color = group) %>%
  add_mean_bar(alpha = 0.3) %>%
  add_error() %>%
  add_jitter()

study %>%
  tidyplot(treatment, score, color = group) %>%
  add_mean_bar(alpha = 0.3) %>%
  add_error() %>%
  add_points() %>%
  add_line(group = participant, dodge_width = 0) %>%
  adjust_variable(treatment, sort_by = dose)

usethis::use_data(study, overwrite = TRUE)
