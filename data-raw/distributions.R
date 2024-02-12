## code to prepare `distributions` dataset goes here

library(tidyverse)

diff <- c(2,3,5.5)
diff2 <- c(2.4,2.7,3.3,4.6)

distributions <-
  tibble(
    bimodal = c((7 + diff2), (7 - diff2)),
    normal = c(6.5, 7.5, (7 + diff), (7 - diff)),
    skewed = c(6,6.2,6.5,6.9,7.2,9,12.5,16)-1.8
  ) %>%
  pivot_longer(everything(), names_to = "name", values_to = "value") %>%
  mutate(group = rep(LETTERS[1:3], 8))

distributions %>%
  tidyplot(x = name, y = value) %>%
  add_violin() %>%
  add_data_points_beeswarm() %>%
  add_mean_dash(color = "red") %>%
  add_error(color = "red") %>%
  add_reference_lines(y = c(7, 8.25, 5.75))

distributions %>%
  tidyplot(x = group, y = value, color = group) %>%
  add_mean_bar(alpha = 0.3) %>%
  add_error()

distributions %>%
  tidyplot(x = group, y = value, color = group) %>%
  add_boxplot()

distributions %>%
  tidyplot(x = group, y = value, color = group) %>%
  add_violin()

distributions %>%
  tidyplot(x = group, y = value, color = group) %>%
  add_violin() %>%
  add_data_points_beeswarm(jitter_width = 0.2)

distributions %>%
  tidyplot(x = name, y = value, color = group) %>%
  add_violin() %>%
  add_data_points_beeswarm(jitter_width = 0.2)

usethis::use_data(distributions, overwrite = TRUE)
