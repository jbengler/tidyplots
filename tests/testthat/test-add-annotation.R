test_that("add annotation works", {
  animals %>%
    tidyplot(x = weight, y = size, color = number_of_legs) %>%
    add_data_points() %>%
    add_reference_lines(x = c(4000), y = c(200, 350)) %>%
    vdiffr::expect_doppelganger("add reference lines", .)

  study %>%
    tidyplot(x = treatment, y = score, color = treatment) %>%
    add_mean_dash() %>%
    add_sem_errorbar() %>%
    add_title("My fancy long title") %>%
    add_caption("Here goes the fine print") %>%
    vdiffr::expect_doppelganger("add title and caption", .)

  study %>%
    tidyplot(x = treatment, y = score, color = treatment) %>%
    add_boxplot() %>%
    add_annotation_text("Look here!", x = 2, y = 25) %>%
    vdiffr::expect_doppelganger("add annotation text", .)

  eu_countries %>%
    tidyplot(x = area, y = population) %>%
    add_data_points() %>%
    add_annotation_rectangle(xmin = 2.5e5, xmax = Inf, ymin = 42, ymax = Inf) %>%
    vdiffr::expect_doppelganger("add annotation rectangle", .)

  eu_countries %>%
    tidyplot(x = area, y = population) %>%
    add_data_points() %>%
    add_annotation_rectangle(xmin = 2.5e5, xmax = 6e5, ymin = 42, ymax = 90, color = "#E69F00", fill = NA) %>%
    vdiffr::expect_doppelganger("add annotation rectangle orange border", .)

  eu_countries %>%
    tidyplot(x = area, y = population) %>%
    add_data_points() %>%
    add_annotation_line(x = 0, xend = Inf, y = 0, yend = Inf) %>%
    vdiffr::expect_doppelganger("add annotation line", .)
})
