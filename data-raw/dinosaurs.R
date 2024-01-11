## code to prepare `dinosaurs` dataset goes here

library(tidyverse)

dinosaurs <- read_tsv("data-raw/dinosaurs.tsv")

dinosaurs <-
  dinosaurs %>%
  mutate(
    speed_unit = "km/h",
    weight_unit = "kg",
    size_unit = "cm"
  ) %>%
  relocate(dinosaur, size, size_unit, weight, weight_unit, speed, speed_unit)

usethis::use_data(dinosaurs, overwrite = TRUE)


