test_that("printed log is stable", {
  d <- study
  d$score[3] <- NA
  # add_mean_bar() builds the plot internally and warns about the NA
  p <- suppressWarnings(d |>
    tidyplot(x = treatment, y = score, color = treatment) |>
    add_metadata(y_unit = "points") |>
    add_note("Each point represents one participant.") |>
    add_mean_bar(alpha = 0.4) |>
    add_sem_errorbar() |>
    add_data_points() |>
    add(ggplot2::geom_hline(yintercept = 20)))
  p$tidyplot$log$header$created <- as.POSIXct("2026-01-01 12:00:00", tz = "UTC")
  p$tidyplot$log$header$figure_id <- "tp-000000000000"
  p$tidyplot$log$header$software[] <- "x.y.z"
  p$tidyplot$log$header$data$hash <- "00000000"
  expect_snapshot(view_log(p))
})

test_that("the printed log states its scope", {
  p <- study |> tidyplot(x = treatment, y = score)
  expect_output(view_log(p), "data processing before tidyplot() is not recorded", fixed = TRUE)
})
