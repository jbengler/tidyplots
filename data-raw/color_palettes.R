## code to prepare `color_palettes` dataset goes here

library(tidyverse)

load("data-raw/color_palettes.rda")
color_palettes <- map(color_palettes, unname)

usethis::use_data(color_palettes, overwrite = TRUE)
