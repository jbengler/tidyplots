## code to prepare `de_analysis` dataset goes here

library(tidyverse)

de_analysis <-
  read_tsv("../../projects/2015_Bactrap_Chat_EAE_acute_v2/deseq2/results/statResult_Hip_vs_Hin.txt") %>%
  mutate(negLog10Padj = -log10(padj)) %>%
  mutate(
    cand = "none",
    cand = if_else(log2FoldChange > 1 & padj < 0.05, "up", cand),
    cand = if_else(log2FoldChange < -1 & padj < 0.05, "down", cand)
  )

de_analysis %>%
  tidyplot(log2FoldChange, negLog10Padj) %>%
  add_points(color = "grey", alpha = 0.1) %>%
  add_points(data = filter_rows(log2FoldChange > 1, padj < 0.05), color = "red", alpha = 0.1) %>%
  add_points(data = filter_rows(log2FoldChange < -1, padj < 0.05), color = "blue",  alpha = 0.1)

de_analysis %>%
  tidyplot(log2FoldChange, negLog10Padj, color = negLog10Padj) %>%
  add_points()

de_analysis %>%
  tidyplot(log2FoldChange, negLog10Padj, color = cand) %>%
  add_points()

# usethis::use_data(de_analysis, overwrite = TRUE)



eae_analysis <-
  readxl::read_xlsx("../../projects/2020_EAEanalyzer/BsnMUT_EAE_02.xlsx", sheet = "score") %>%
  pivot_longer(cols = c(-day), names_to = "animal_id", values_to = "score") %>%
  left_join(readxl::read_xlsx("../../projects/2020_EAEanalyzer/BsnMUT_EAE_02.xlsx", sheet = "metadata") %>%
              mutate(animal_id = as.character(animal_id))) %>%
  mutate(age = floor(lubridate::interval(start = date_birth, end = date_immunization) /
                       lubridate::duration(num = 1, units = "weeks")))

eae_analysis %>%
  tidyplot(day, score, color = genotype) %>%
  add_mean_line() %>%
  add_mean_dot() %>%
  add_error(width = 3)

eae_analysis %>%
  tidyplot(day, score, color = genotype) %>%
  add_mean_line() %>%
  add_error_ribbon()

eae_analysis %>%
  tidyplot(day, animal_id, color = score) %>%
  add_heatmap(rotate_labels = 0) +
  facet_grid(vars(genotype), scales = "free")

eae_analysis %>%
  tidyplot(day, animal_id, color = score) %>%
  add_heatmap(rotate_labels = 0) %>%
  split_plot(genotype)

