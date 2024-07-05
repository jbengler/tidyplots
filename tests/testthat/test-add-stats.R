test_that("stats work", {
  p2 <-
    study %>%
    tidyplot(treatment, score, color = treatment) %>%
    add_mean_bar(alpha = 0.3) %>%
    add_sem_bar()

  p2 %>% add_stats_pvalue(ref.group = 1) %>%
    vdiffr::expect_doppelganger("add stats pvalue ref.group", .)
  p2 %>% add_stats_pvalue(ref.group = 1, p.adjust.method = "bonferroni") %>%
    vdiffr::expect_doppelganger("add stats pvalue ref.group bonferroni", .)
  p2 %>% add_stats_asterisks(ref.group = 1, p.adjust.method = "bonferroni") %>%
    vdiffr::expect_doppelganger("add stats asterisks ref.group bonferroni", .)

  p3 <-
    study %>%
    tidyplot(x = dose, y = score, color = group) %>%
    add_mean_bar(alpha = 0.3) %>%
    add_sem_bar() %>%
    add_data_points_beeswarm()

  p3 %>% add_stats_pvalue() %>%
    vdiffr::expect_doppelganger("add grouped stats pvalue", .)
  p3 %>% add_stats_pvalue(p.adjust.method = "bonferroni") %>%
    vdiffr::expect_doppelganger("add grouped stats pvalue bonferroni", .)
  p3 %>% add_stats_asterisks(p.adjust.method = "bonferroni") %>%
    vdiffr::expect_doppelganger("add grouped stats asterisks bonferroni", .)
})
