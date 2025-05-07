## code to prepare `gene_expression` dataset goes here

library(tidyverse)

# Dataset from Schattling et al.
# https://www.nature.com/articles/s41593-019-0385-4

gene_expression <- read_csv(file = "data-raw/gene_expression.csv")

usethis::use_data(gene_expression, overwrite = TRUE)

library(tidyplots)
library(tidyverse)

