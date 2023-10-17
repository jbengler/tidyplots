## code to prepare `gene_expression` dataset goes here

library(tidyverse)

gene_expression <- read_csv(file = "data-raw/gene_expression.csv")

usethis::use_data(gene_expression, overwrite = TRUE)
