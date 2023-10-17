## code to prepare `fruits` dataset goes here

library(tidyverse)

df <-
  tibble(name = c("apple", "banana", "pinacle", "strawberry", "cherry"),
       color = c("green", "yellow", "yellow", "red", "red"),
       number = c(9, 7, 2, 17, 12),
       weight = c(300, 270, 1000, 50, 35),
       aspect_ratio = c(1, 3.5, 2, 1.5, 1)#,
       # pick_date = c(""),
       # country = c(""),
       # longitude = c(""),
       # latitude = c("")
       )

fruits <-
  df %>%
  rowwise() %>%
  mutate(
    weight = list(rnorm(number, weight, weight/4)),
    aspect_ratio = list(rnorm(number, aspect_ratio, 0.1))
    ) %>%
  unnest()

usethis::use_data(fruits, overwrite = TRUE)
