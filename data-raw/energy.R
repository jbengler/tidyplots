## code to prepare `energy` dataset goes here

library(tidyverse)

energy <-
  read_csv("data-raw/energy_total_year.csv") %>%
  pivot_longer(-year, names_to = "energy_source", values_to = "power") %>%
  mutate(energy_type = case_when(
    str_detect(energy_source, "Geo|Hydro|Wind|Bio|Solar") ~ "Renewable",
    str_detect(energy_source, "Nuclear") ~ "Nuclear",
    str_detect(energy_source, "Fossil") ~ "Fossil",
    .default = "Other"
  )) %>%
  mutate(
    power_unit = "GW",
    energy_source = factor(energy_source),
    energy_type = factor(energy_type)
    ) %>%
  tidyr::replace_na(list(power = 0)) %>%
  relocate(year, energy_source, energy_type, power, power_unit)

usethis::use_data(energy, overwrite = TRUE)
