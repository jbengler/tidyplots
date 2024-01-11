## code to prepare `eu_countries` dataset goes here

library(tidyverse)

eu_countries <- read_csv("data-raw/eu_countries.csv") %>%
  mutate(
    area_unit = "square km",
    population_unit = "million inhabitants",
    gdp_unit = "million euro"
  ) %>%
  relocate(country, country_code, area, area_unit, population, population_unit, gdp, gdp_unit)

usethis::use_data(eu_countries, overwrite = TRUE)
