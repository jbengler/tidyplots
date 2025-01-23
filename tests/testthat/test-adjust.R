test_that("adjust_color works", {
  p <-
    study |>
    tidyplot(x = treatment, y = score, color = treatment) |>
    add_mean_bar(alpha = 0.4) |>
    add_sem_errorbar() |>
    add_data_points_beeswarm()

  new_colors <-
    c("A" = "#B0B1B3",
      "B" = "#F18823",
      "C" = "#E23130",
      "D" = "#1D5D83")

  new_names <-
    c("A" = "Regime A",
      "B" = "Regime B",
      "C" = "Regime C",
      "D" = "Regime D")

  p |> adjust_colors(new_colors) |>
    vdiffr::expect_doppelganger("adjust_colors", fig = _)
  p |> reorder_x_axis_labels("C") |> adjust_colors(new_colors) |>
    vdiffr::expect_doppelganger("adjust_colors after reorder", fig = _)
  p |> adjust_colors(new_colors) |> reorder_x_axis_labels("C") |>
    vdiffr::expect_doppelganger("adjust_colors before reorder", fig = _)
  p |> adjust_colors(new_colors) |> rename_x_axis_labels(new_names) |>
    vdiffr::expect_doppelganger("adjust_colors before rename", fig = _)

  energy_week |>
    tidyplot(x = date, y = power, color = energy_source) |>
    add_mean_line() |>
    vdiffr::expect_doppelganger("adjust_colors with factor levels", fig = _)
  energy_week |>
    dplyr::filter(energy_source %in% c("Wind onshore", "Wind offshore")) |>
    tidyplot(x = date, y = power, color = energy_source) |>
    add_mean_line() |>
    vdiffr::expect_doppelganger("adjust_colors with unused factor levels", fig = _)
})

test_that("adjust axes works", {
  p <-
    study |>
    tidyplot(x = treatment, y = score, color = treatment) |>
    add_mean_bar(alpha = 0.4) |>
    add_sem_errorbar() |>
    add_data_points_beeswarm()

  demo_xy <-
    animals |>
    tidyplot(x = weight, y = size, color = number_of_legs) |>
    add_data_points()

  demo_xy |>
    adjust_y_axis(limits = c(-20, 60)) |>
    vdiffr::expect_doppelganger("adjust axes limits 1", fig = _)

  demo_xy |>
    adjust_y_axis(limits = c(0, 600)) |>
    adjust_x_axis(limits = c(0, 4000)) |>
    vdiffr::expect_doppelganger("adjust axes limits 2", fig = _)

  energy_week |>
    tidyplot(date, power, color = energy_type) |>
    add_mean_line() |>
    adjust_y_axis(limits = c(1000, 5000)) |>
    vdiffr::expect_doppelganger("adjust axes limits 3", fig = _)

  p |> adjust_x_axis("My X axis title") |>
    vdiffr::expect_doppelganger("adjust axes title x", fig = _)

  p |> adjust_y_axis("My Y axis title") |>
    vdiffr::expect_doppelganger("adjust axes title y", fig = _)
})

test_that("adjust legend works", {
  p <-
    study |>
    tidyplot(x = treatment, y = score, color = treatment) |>
    add_mean_bar(alpha = 0.4) |>
    add_sem_errorbar() |>
    add_data_points_beeswarm()

  p |> adjust_legend_title("My legend title") |>
    vdiffr::expect_doppelganger("adjust legend title", fig = _)
  p |> adjust_legend_title("My legend title") |>
    adjust_legend_position("top") |>
    vdiffr::expect_doppelganger("adjust legend title and position", fig = _)
})

test_that("plotmath expressions work", {
  p <-
    study |>
    tidyplot(x = treatment, y = score, color = treatment) |>
    add_mean_bar(alpha = 0.4) |>
    add_sem_errorbar() |>
    add_data_points_beeswarm()

  p |> add_title(title = "$E==m*c^{2}~H[2]*O$") |>
    vdiffr::expect_doppelganger("plotmath expression title", fig = _)
  p |> add_caption(caption = "$E==m*c^{2}~H[2]*O$") |>
    vdiffr::expect_doppelganger("plotmath expression caption", fig = _)
  p |> adjust_legend_title("$E==m*c^{2}~H[2]*O$") |>
    vdiffr::expect_doppelganger("plotmath expression legend title", fig = _)
  p |> adjust_x_axis_title("$Domino~E==m*c^{2}$") |>
    vdiffr::expect_doppelganger("plotmath expression x axis title", fig = _)
  p |> adjust_y_axis_title("$Domino~E==m*c^{2}$") |>
    vdiffr::expect_doppelganger("plotmath expression y axis title", fig = _)

  new_labels <-
    c("A" = "$TNF*alpha$",
      "B" = "$IFN*gamma$",
      "C" = "plain text",
      "D" = "$H[2]*O$")

  p |> rename_x_axis_labels(new_labels) |>
    vdiffr::expect_doppelganger("plotmath expression x axis labels", fig = _)
})

test_that("adjust plot area size work", {
  study |>
    tidyplot(x = dose, y = score, color = group) |>
    add_boxplot() |>
    adjust_size(width = 70) |>
    vdiffr::expect_doppelganger("plot area size width", fig = _)

  study |>
    tidyplot(x = dose, y = score, color = group) |>
    add_boxplot() |>
    adjust_size(width = 25, height = 25) |>
    vdiffr::expect_doppelganger("plot area size width and height", fig = _)

  study |>
    tidyplot(x = dose, y = score, color = group) |>
    add_boxplot() |>
    adjust_size(width = NA, height = NA) |>
    vdiffr::expect_doppelganger("plot area size NA", fig = _)
})

test_that("adjust title font works", {
  study |>
    tidyplot(x = dose, y = score, color = group) |>
    add_title("hoppla") |>
    adjust_title(face = "bold", family = "courier", fontsize = 20, angle = 15) |>
    vdiffr::expect_doppelganger("plot title font", fig = _)

  study |>
    tidyplot(x = dose, y = score, color = group) |>
    adjust_x_axis_title(title = "bla", face = "bold", family = "courier", fontsize = 20) |>
    vdiffr::expect_doppelganger("x axis title font", fig = _)

  study |>
    tidyplot(x = dose, y = score, color = group) |>
    adjust_y_axis_title(title = "bla", face = "bold", family = "courier", fontsize = 20) |>
    vdiffr::expect_doppelganger("y axis title font", fig = _)

  study |>
    tidyplot(x = dose, y = score, color = group) |>
    adjust_caption(caption = "bla", face = "bold", family = "courier", fontsize = 20) |>
    vdiffr::expect_doppelganger("caption font", fig = _)

  study |>
    tidyplot(x = dose, y = score, color = group) |>
    add_mean_bar() |>
    adjust_legend_title(face = "bold", family = "courier", fontsize = 20, angle = -15) |>
    vdiffr::expect_doppelganger("legend title font", fig = _)
})
