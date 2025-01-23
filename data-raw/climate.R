## code to prepare `climate` dataset goes here

library(tidyverse)

climate <-
  read_csv("data-raw/GM000010147.csv") |>
  mutate(
    avg_temperature = TAVG,
    min_temperature = TMIN,
    max_temperature = TMAX
  ) |>
  separate_wider_delim(DATE, delim = "-", names = c("year", "month")) |>
  mutate(year = as.numeric(year)) |>
  dplyr::filter(year != 2024) |>
  dplyr::select(year, month, contains("temperature"))

usethis::use_data(climate, overwrite = TRUE)

climate |>
  tidyplot(x = month, y = year, color = max_temperature) |>
  add_heatmap()

climate |>
  tidyplot(x = month, y = year, color = max_temperature) |>
  add_heatmap(scale = "col") |>
  adjust_colors(colors_continuous_bluepinkyellow, limits = c(-2.5, 2.5), oob = scales::squish)


