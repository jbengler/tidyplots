## code to prepare `energy` dataset goes here

library(tidyverse)
library(tidyplots)

# corrected version
read_single_file <- function(x) {
  read_csv(x, comment = "#") |>
    pivot_longer(-Year, names_to = "energy_source", values_to = "energy") |>
    rename(year = Year)
}

energy <-
  list.files(path = "data-raw/energy_charts_download",
             pattern = "in_20",
             full.names = TRUE
             ) |>
  map(read_single_file) |>
  bind_rows() |>
  mutate(
    energy_source = str_replace_all(energy_source, "Fossil.*gas", "Fossil gas"),
    energy_source = str_replace_all(energy_source, "Solar.*", "Solar"),
    energy_source = str_replace_all(energy_source, "Hydro.*", "Hydro"),
    energy_source = str_replace_all(energy_source, "Waste.*", "Waste")
  ) |>
  tidyr::complete(year, energy_source, fill = list(energy = 0)) |>
  mutate(energy_type = case_when(
    str_detect(energy_source, "Geo|Hydro|Wind|Bio|Solar") ~ "Renewable",
    str_detect(energy_source, "Nuclear") ~ "Nuclear",
    str_detect(energy_source, "Fossil") ~ "Fossil",
    .default = "Other"
  )) |>
  mutate(
    energy_unit = "TWh",
    energy_source = factor(energy_source),
    energy_type = factor(energy_type)
  ) |>
  relocate(year, energy_source, energy_type, energy, energy_unit)

usethis::use_data(energy, overwrite = TRUE)

# write_csv(energy, "electricity-generation-in-Germany.csv")
