test_that("stats work", {
  p2 <-
    study |>
    tidyplot(treatment, score, color = treatment) |>
    add_mean_bar(alpha = 0.4) |>
    add_sem_errorbar()

  p2 |> add_test_pvalue(ref.group = 1) |>
    vdiffr::expect_doppelganger("add stats pvalue ref.group", fig = _)
  p2 |> add_test_pvalue(ref.group = 1, p.adjust.method = "bonferroni") |>
    vdiffr::expect_doppelganger("add stats pvalue ref.group bonferroni", fig = _)
  p2 |> add_test_asterisks(ref.group = 1, p.adjust.method = "bonferroni") |>
    vdiffr::expect_doppelganger("add stats asterisks ref.group bonferroni", fig = _)

  p3 <-
    study |>
    tidyplot(x = dose, y = score, color = group) |>
    add_mean_bar(alpha = 0.4) |>
    add_sem_errorbar() |>
    add_data_points_beeswarm()

  p3 |> add_test_pvalue() |>
    vdiffr::expect_doppelganger("add grouped stats pvalue", fig = _)
  p3 |> add_test_pvalue(p.adjust.method = "bonferroni") |>
    vdiffr::expect_doppelganger("add grouped stats pvalue bonferroni", fig = _)
  p3 |> add_test_asterisks(p.adjust.method = "bonferroni") |>
    vdiffr::expect_doppelganger("add grouped stats asterisks bonferroni", fig = _)
})
