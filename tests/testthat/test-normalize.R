test_that("normalize_y() with one computes fold change", {
  p <- study |>
    tidyplot(treatment, score) |>
    normalize_y(one = treatment == "A")
  ref <- mean(study$score[study$treatment == "A"])
  expect_equal(p$data$score, study$score / ref)
})

test_that("normalize_y() with zero subtracts baseline within groups", {
  p <- climate |>
    tidyplot(year, avg_temperature) |>
    normalize_y(zero = year >= 1961 & year <= 1990, by = month)
  expected <- climate |>
    dplyr::mutate(
      y = avg_temperature -
        mean(avg_temperature[year >= 1961 & year <= 1990]),
      .by = month
    )
  expect_equal(p$data$avg_temperature, expected$y)
})

test_that("normalize_y() with zero and one scales to 0-1 range", {
  p <- gene_expression |>
    dplyr::filter(sample_type == "input") |>
    tidyplot(condition, expression) |>
    normalize_y(
      zero = condition == "healthy",
      one = condition == "disease",
      by = external_gene_name
    )
  means <- p$data |>
    dplyr::summarise(
      m = mean(expression),
      .by = c(external_gene_name, condition)
    )
  expect_equal(means$m[means$condition == "healthy"], rep(0, 40))
  expect_equal(means$m[means$condition == "disease"], rep(1, 40))
})

test_that("normalize_y() uses fun to combine reference rows", {
  p <- study |>
    tidyplot(treatment, score) |>
    normalize_y(one = treatment == "A", fun = median)
  ref <- median(study$score[study$treatment == "A"])
  expect_equal(p$data$score, study$score / ref)
})

test_that("normalize_y() works with multiple by variables", {
  p <- study |>
    tidyplot(treatment, score) |>
    normalize_y(zero = sex == "female", by = c(group, dose))
  expected <- study |>
    dplyr::mutate(
      y = score - mean(score[sex == "female"]),
      .by = c(group, dose)
    )
  expect_equal(p$data$score, expected$y)
})

test_that("normalize_y() errors on invalid input", {
  p <- study |> tidyplot(treatment, score)
  expect_error(normalize_y(p), "must be provided")
  expect_error(normalize_y(p, one = "A"), "must be a logical condition")
  expect_error(
    study |> tidyplot(score, treatment) |> normalize_y(one = score > 3),
    "must be continuous"
  )
})

test_that("normalize_y() warns on missing references and division by zero", {
  p <- study |> tidyplot(treatment, score)
  expect_warning(
    normalize_y(p, one = treatment == "A", by = group),
    "1 group has no reference rows"
  )
  expect_warning(
    time_course |> tidyplot(day, score) |> normalize_y(one = day == 0),
    "division by zero"
  )
})

test_that("normalize_x() and normalize_color() work", {
  p <- study |>
    tidyplot(score, treatment) |>
    normalize_x(one = treatment == "A")
  ref <- mean(study$score[study$treatment == "A"])
  expect_equal(p$data$score, study$score / ref)

  p <- climate |>
    tidyplot(month, year, color = max_temperature) |>
    add_heatmap() |>
    normalize_color(zero = year >= 1961 & year <= 1990, by = month)
  expected <- climate |>
    dplyr::mutate(
      y = max_temperature -
        mean(max_temperature[year >= 1961 & year <= 1990]),
      .by = month
    )
  expect_equal(p$data$max_temperature, expected$y)
})

test_that("normalize_x() and normalize_color() require continuous variables", {
  p <- study |> tidyplot(treatment, score, color = treatment)
  expect_error(normalize_x(p, one = score > 3), "x axis must be continuous")
  expect_error(normalize_color(p, one = score > 3), "color must be continuous")
})

test_that("normalize_y() plots", {
  testthat::skip_on_ci()
  study |>
    tidyplot(x = treatment, y = score, color = treatment) |>
    add_mean_bar(alpha = 0.4) |>
    add_sem_errorbar() |>
    add_data_points() |>
    normalize_y(one = treatment == "A") |>
    vdiffr::expect_doppelganger("normalize y one", fig = _)

  climate |>
    tidyplot(x = month, y = year, color = max_temperature) |>
    add_heatmap() |>
    adjust_colors(colors_diverging_blue2red) |>
    normalize_color(zero = year >= 1961 & year <= 1990, by = month) |>
    vdiffr::expect_doppelganger("normalize color heatmap", fig = _)
})
