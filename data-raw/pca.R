## code to prepare `pca` dataset goes here

library(tidyverse)

# Dataset from Schattling et al.
# https://www.nature.com/articles/s41593-019-0385-4

pca <-
  read_csv("data-raw/pca_plot.csv") |>
  select(1:4)

usethis::use_data(pca, overwrite = TRUE)
