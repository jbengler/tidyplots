## code to prepare `time_course` dataset goes here

library(tidyverse)

time_course <-
  read_tsv("data-raw/time_course.txt")

usethis::use_data(time_course, overwrite = TRUE)

time_course %>%
  tidyplot(x = day, y = score, color = treatment, dodge_width = 0) %>%
  add_mean_line() %>%
  add_mean_dot() %>%
  add_error_ribbon()

library(tidyverse)

time_course %>%
  tidyplot(x = day, y = subject, color = score, height = 80) %>%
  add_heatmap(rotate_labels = FALSE) +
  facet_grid(vars(treatment), scales = "free")

time_course %>%
  tidyplot(x = day, y = subject, color = score, height = 80) %>%
  add_heatmap(rotate_labels = FALSE) %>%
  adjust_colors(new_colors = rev(RColorBrewer::brewer.pal(n = 7, name = "RdYlBu")),
                as_palette = TRUE,
                na.value = "#DDDDDD") +
  facet_grid(vars(treatment), scales = "free")

