if (Sys.getenv("CI") == "true") {
  testthat::skip("Skipping vdiffr tests on CI")
}

test_that("remove clipping works", {
  spendings |>
    tidyplot(x = amount, y = category, color = category) |>
    add_sum_bar(alpha = 0.2) |>
    add_sum_dash() |>
    add_sum_value(accuracy = 1, color = "black") |>
    remove_clipping() |>
    vdiffr::expect_doppelganger("remove clipping", fig = _)

  spendings |>
    tidyplot(x = amount, y = category, color = category) |>
    add_sum_bar(alpha = 0.2) |>
    add_sum_dash() |>
    add_sum_value(accuracy = 1, color = "black") |>
    adjust_x_axis(limits = c(3, 1000)) |>
    remove_clipping() |>
    vdiffr::expect_doppelganger("remove clipping after limits", fig = _)

  spendings |>
    tidyplot(x = amount, y = category, color = category) |>
    add_sum_bar(alpha = 0.2) |>
    add_sum_dash() |>
    add_sum_value(accuracy = 1, color = "black") |>
    remove_clipping() |>
    adjust_x_axis(limits = c(3, 1000)) |>
    vdiffr::expect_doppelganger("remove clipping before limits", fig = _)
})
