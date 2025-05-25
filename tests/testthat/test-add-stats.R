test_that("stats work", {
  p2 <-
    study |>
    tidyplot(treatment, score, color = treatment) |>
    add_mean_bar(alpha = 0.4) |>
    add_sem_errorbar()

  p2 |> add_test_pvalue() |>
    vdiffr::expect_doppelganger("add stats pvalue no ref.group", fig = _)
  p2 |> add_test_pvalue(ref.group = 1) |>
    vdiffr::expect_doppelganger("add stats pvalue ref.group", fig = _)
  p2 |> add_test_pvalue(comparisons = list(c(1,4),c(2,3))) |>
    vdiffr::expect_doppelganger("add stats pvalue comparisons", fig = _)
  p2 |> add_test_pvalue(ref.group = 1, p.adjust.method = "bonferroni") |>
    vdiffr::expect_doppelganger("add stats pvalue ref.group bonferroni", fig = _)
  p2 |> add_test_asterisks(ref.group = 1, p.adjust.method = "bonferroni") |>
    vdiffr::expect_doppelganger("add stats asterisks ref.group bonferroni", fig = _)
  p2 |> add_test_asterisks(comparisons = list(c(1,4),c(2,3))) |>
    vdiffr::expect_doppelganger("add stats asterisks comparisons", fig = _)


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




test_that("paired stats work", {
  x <- c(2.3, 4.5, 6.3, 3.4, 7.8, 6.7)
  df <- data.frame(
    x = c(x, x + c(0.8, 0.75)),
    group = paste0("g", rep(c(1, 2), each = 6)),
    batch = paste0("b", c(1:6, 1:6)),
    control = paste0("c", c(1:6, 6:1))
  )

  df |>
    tidyplot(group, x, color = group) |>
    add_boxplot() |>
    add_data_points_beeswarm() |>
    add_test_pvalue(paired_by = control) |>
    add_line(group = control, color = "black") |>
    vdiffr::expect_doppelganger("add paired stats pvalue", fig = _)

  df |>
    tidyplot(group, x, color = group) |>
    add_boxplot() |>
    add_data_points_beeswarm() |>
    add_test_pvalue(paired_by = batch) |>
    add_line(group = batch, color = "black") |>
    vdiffr::expect_doppelganger("add paired stats pvalue control", fig = _)

  df |>
    tidyplot(group, x, color = group) |>
    add_boxplot() |>
    add_data_points_beeswarm() |>
    add_test_asterisks(paired_by = control) |>
    add_line(group = control, color = "black") |>
    vdiffr::expect_doppelganger("add paired stats asterisks", fig = _)

  df |>
    tidyplot(group, x, color = group) |>
    add_boxplot() |>
    add_data_points_beeswarm() |>
    add_test_asterisks(paired_by = batch) |>
    add_line(group = batch, color = "black") |>
    vdiffr::expect_doppelganger("add paired stats asterisks control", fig = _)
})
