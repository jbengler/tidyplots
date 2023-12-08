## code to prepare `disease_course` dataset goes here

library(tidyverse)

disease_course <-
  read_tsv("data-raw/tidy_disease_course.txt")

usethis::use_data(disease_course, overwrite = TRUE)

library(tidyplots)

disease_course %>%
  tidyplot(x = day, y = score, color = treatment, dodge_width = 0) %>%
  add_mean_line() %>%
  add_mean_dot() %>%
  add_error_ribbon()

disease_course %>%
  tidyplot(x = day, y = subject, color = score, height = 80) %>%
  add_heatmap(rotate_labels = FALSE) +
  facet_grid(vars(treatment), scales = "free")

disease_course %>%
  tidyplot(x = day, y = subject, color = score, height = 80) %>%
  add_heatmap(rotate_labels = FALSE) %>%
  adjust_colors(new_colors = rev(RColorBrewer::brewer.pal(n = 7, name = "RdYlBu")),
                as_palette = TRUE,
                na.value = "#DDDDDD") +
  facet_grid(vars(treatment), scales = "free")
