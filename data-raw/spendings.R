## code to prepare `spendings` dataset goes here

library(tidyverse)

spendings <-
  read_tsv("data-raw/spendings.tsv") %>%
  arrange(date)

usethis::use_data(spendings, overwrite = TRUE)
