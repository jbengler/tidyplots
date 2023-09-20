## code to prepare `energy_week` dataset goes here

library(tidyverse)

energy_week <-
  read_csv("data-raw/energy_total_hour.csv") %>%
  pivot_longer(-Date, names_to = "energy_source", values_to = "power_in_gw") %>%
  filter(energy_source != "Load") %>%
  mutate(energy_type = case_when(
    str_detect(energy_source, "Geo|Hydro|Wind|Bio|Solar") ~ "Renewable",
    str_detect(energy_source, "Nuclear") ~ "Nuclear",
    str_detect(energy_source, "Fossil") ~ "Fossil",
    .default = "Other"
  )) %>%
  mutate(
    energy_source = factor(energy_source),
    energy_type = factor(energy_type)
  )

usethis::use_data(energy_week, overwrite = TRUE)
