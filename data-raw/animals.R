## code to prepare `animals` dataset goes here

library(tidyverse)

animals <-
  read_tsv("data-raw/animals.tsv") %>%
  mutate(
    speed = speed_max,
    weight = weight_max,
    size = size_max,
    number_of_legs = factor(number_of_legs)
  ) %>%
  select(-ends_with("min"), -ends_with("max")) %>%
  relocate(animal, size, weight, speed)

usethis::use_data(animals, overwrite = TRUE)
