## code to prepare `gene_expression` dataset goes here

library(tidyverse)

gene_expression <- read_csv(file = "data-raw/gene_expression.csv")

usethis::use_data(gene_expression, overwrite = TRUE)

library(tidyplots)
library(tidyverse)

gene_expression %>%
  tidyplot(sample, external_gene_name, color = expression, width = 100) %>%
  add_heatmap(scale = "row") %>%
  adjust_labels(var = external_gene_name, sort_by = -padj) %>%
  adjust_labels(var = direction, reverse = TRUE)+
  facet_wrap(vars(direction), scales = "free")
