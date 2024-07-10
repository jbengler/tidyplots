## code to prepare `gene_statistics` dataset goes here

library(tidyverse)

gene_statistics <-
  read_tsv("../../projects/2015_Bactrap_Chat_EAE_acute_v2/deseq2/results/statResult_Eip_vs_Ein.txt") %>%
  rename(log2_foldchange = log2FoldChange) %>%
  mutate(neg_log10_padj = -log10(padj)) %>%
  mutate(
    is_cand = if_else(abs(log2_foldchange) > 1 & padj <= 0.05, TRUE, FALSE, FALSE),
    direction = if_else(log2_foldchange > 1, "up", "down")
  )

usethis::use_data(gene_statistics, overwrite = TRUE)

gene_statistics %>%
  tidyplot(log2_foldchange, neg_log10_padj) %>%
  add_data_points(data = filter_rows(!is_cand), color = "grey80") %>%
  add_data_points(data = filter_rows(is_cand & direction == "up"), color = "red", alpha = 0.2) %>%
  add_data_points(data = filter_rows(is_cand & direction == "down"), color = "blue",  alpha = 0.2) %>%
  add_reference_lines(x = c(1, -1), y = -log10(0.05)) %>%
  add_data_labels_repel(data = function(x) x %>%
             dplyr::filter(is_cand) %>%
             slice_max(neg_log10_padj, n = 5, by = direction),
           label = external_gene_name, color = "black", box.padding = 0.3, max.overlaps = Inf, min.segment.length = 0)

gene_statistics %>%
  tidyplot(log2_foldchange, neg_log10_padj, color = stat) %>%
  add_data_points()

