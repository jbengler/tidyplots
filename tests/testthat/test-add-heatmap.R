test_that("add_heatmap works", {
  gene_expression %>%
    tidyplot(x = sample, y = external_gene_name, color = expression) %>%
    add_heatmap(scale = "row") %>%
    adjust_size(height = 90) %>%
    vdiffr::expect_doppelganger("heatmap with scale row", .)

  gene_expression %>%
    tidyplot(x = sample, y = external_gene_name, color = expression) %>%
    add_heatmap(scale = "column") %>%
    adjust_size(height = 90) %>%
    vdiffr::expect_doppelganger("heatmap with scale column", .)

  gene_expression %>%
    tidyplot(x = sample, y = external_gene_name, color = expression) %>%
    add_heatmap() %>%
    adjust_size(height = 90) %>%
    vdiffr::expect_doppelganger("heatmap without scaling", .)

  hm <-
    gene_expression %>%
    tidyplot(x = sample, y = external_gene_name, color = expression) %>%
    add_heatmap(scale = "row") %>%
    adjust_size(height = 90)

  hm %>% reorder_y_axis_labels("Bsn") %>%
    vdiffr::expect_doppelganger("heatmap reorder y axis 1", .)
  hm %>% reorder_y_axis_labels(c("Bsn", "Apol6")) %>%
    vdiffr::expect_doppelganger("heatmap reorder y axis 2", .)
  hm %>% reorder_x_axis_labels("Hin_3") %>%
    vdiffr::expect_doppelganger("heatmap reorder x axis 1", .)

  hm %>% sort_y_axis_labels(direction) %>%
    vdiffr::expect_doppelganger("heatmap sort y 1", .)
  hm %>% sort_y_axis_labels(direction, -padj) %>%
    vdiffr::expect_doppelganger("heatmap sort y 2", .)
  hm %>% sort_y_axis_labels(expression) %>%
    vdiffr::expect_doppelganger("heatmap sort y 3", .)
  hm %>% sort_x_axis_labels(group) %>%
    vdiffr::expect_doppelganger("heatmap sort x 1", .)
  hm %>% sort_x_axis_labels(sample_type) %>%
    vdiffr::expect_doppelganger("heatmap sort x 2", .)
  hm %>% sort_x_axis_labels(desc(sample_type)) %>%
    vdiffr::expect_doppelganger("heatmap sort x 3", .)
  hm %>% sort_x_axis_labels(condition) %>%
    vdiffr::expect_doppelganger("heatmap sort x 4", .)

  hm %>% reverse_x_axis_labels() %>%
    vdiffr::expect_doppelganger("heatmap reverse x", .)
  hm %>% reverse_y_axis_labels() %>%
    vdiffr::expect_doppelganger("heatmap reverse y", .)

  hm %>% rename_x_axis_labels(new_names = c("Ein_1" = "Hallo")) %>%
    vdiffr::expect_doppelganger("heatmap rename x", .)
  hm %>% rename_y_axis_labels(new_names = c("Bsn" = "Juhooooooooooooooo!",
                                            "Aip" = "Rabbithoooooooooooole")) %>%
    vdiffr::expect_doppelganger("heatmap rename y", .)
})

test_that("rasterize works", {
  gene_expression %>%
    tidyplot(x = sample, y = external_gene_name, color = expression) %>%
    add_heatmap(scale = "row", rasterize = TRUE, rasterize_dpi = 20) %>%
    adjust_size(height = 90) %>%
    vdiffr::expect_doppelganger("rasterize heatmap", .)
})
