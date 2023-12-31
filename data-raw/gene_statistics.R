## code to prepare `de_analysis` dataset goes here

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

p1 <-
  gene_statistics %>%
  tidyplot(log2_foldchange, neg_log10_padj) %>%
  add_points(data = filter_rows(!is_cand), color = "grey80") %>%
  add_points(data = filter_rows(is_cand & direction == "up"), color = "red", alpha = 0.2) %>%
  add_points(data = filter_rows(is_cand & direction == "down"), color = "blue",  alpha = 0.2)

p1
p1 %>%
  add_reference_lines(x = c(1, -1), y = -log10(0.05)) %>%
  add_text(data = max_rows(neg_log10_padj, n = 5), var = external_gene_name, color = "black")

p1 %>%
  add_reference_lines(x = c(1, -1), y = -log10(0.05)) %>%
  add_text(data = function(x) x %>%
             dplyr::filter(is_cand) %>%
             slice_max(neg_log10_padj, n = 5, by = direction),
           var = external_gene_name, color = "black", box.padding = 0.3, max.overlaps = Inf, min.segment.length = 0)

