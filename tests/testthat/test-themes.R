test_that("themes work", {
  p1 <-
    spendings |>
    tidyplot(x = amount, y = category, color = category) |>
    add_sum_bar() |>
    sort_y_axis_levels()

  p1 |> theme_tidyplot() |> vdiffr::expect_doppelganger("theme tidyplot", fig = _)
  p1 |> theme_ggplot2() |> vdiffr::expect_doppelganger("theme ggplot2", fig = _)
  p1 |> theme_minimal_x() |> vdiffr::expect_doppelganger("theme minimal_x", fig = _)
  p1 |> theme_minimal_y() |> vdiffr::expect_doppelganger("theme minimal_y", fig = _)
  p1 |> theme_minimal_xy() |> vdiffr::expect_doppelganger("theme minimal_xy", fig = _)

  spendings |>
    tidyplot(x = amount, y = category, color = category, paper = "cornsilk", ink = "darkred") |>
    add_sum_bar() |>
    sort_y_axis_levels() |>
    vdiffr::expect_doppelganger("theme paper cornsilk", fig = _)

  tidyplots_options(paper = "#333", ink = "#bbb")
  spendings |>
    tidyplot(x = amount, y = category, color = category) |>
    add_sum_bar() |>
    sort_y_axis_levels() |>
    vdiffr::expect_doppelganger("theme paper grey", fig = _)

  tidyplots_options(paper = "transparent", ink = "purple")
  spendings |>
    tidyplot(x = amount, y = title, color = category) |>
    add_sum_bar() |>
    add_caption("tiny text") |>
    sort_y_axis_levels() |>
    split_plot(category) |>
    adjust_size(20, 20) |>
    adjust_title("My heading", fontsize = 16) |>
    vdiffr::expect_doppelganger("theme paper transparent", fig = _)
  tidyplots_options()

})
