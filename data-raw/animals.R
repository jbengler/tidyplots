## code to prepare `animals` dataset goes here

library(tidyverse)

animals <-
  read_tsv("data-raw/animals.tsv") |>
  mutate(
    speed = speed_max,
    speed_unit = "km/h",
    weight = weight_max,
    weight_unit = "kg",
    size = size_max,
    size_unit = "cm",
    number_of_legs = factor(number_of_legs)
  ) |>
  select(-ends_with("min"), -ends_with("max")) |>
  relocate(animal, size, size_unit, weight, weight_unit, speed, speed_unit)

usethis::use_data(animals, overwrite = TRUE)
