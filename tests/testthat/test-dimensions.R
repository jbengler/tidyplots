# Prevent ggplotGrob() from creating Rplots.pdf in non-interactive sessions.
pdf(nullfile())
withr::defer(dev.off(), teardown_env())

# -- get_gtab_size -------------------------------------------------------------

test_that("get_gtab_size returns expected dimensions", {
  p <- study |>
    tidyplot(x = treatment, y = score, color = treatment) |>
    add_data_points()
  gtab <- ggplot2::ggplotGrob(p)
  size <- tidyplots:::get_gtab_size(gtab, "mm")

  expect_length(size, 2)
  expect_named(size, c("width", "height"))
  expect_true(is.numeric(size[["width"]]))
  expect_true(is.numeric(size[["height"]]))
  # Width/height must exceed the 50mm panel default
  expect_gt(size[["width"]], 50)
  expect_lt(size[["width"]], 100)
  expect_gt(size[["height"]], 50)
  expect_lt(size[["height"]], 100)
})

test_that("get_gtab_size accounts for legend overflow on right", {
  # Small panel + many legend entries -> legend taller than panel
  p <- energy_week |>
    tidyplot(x = date, y = power, color = energy_source) |>
    add_areastack_absolute() |>
    adjust_size(30, 30)
  gtab <- ggplot2::ggplotGrob(p)
  size <- tidyplots:::get_gtab_size(gtab, "mm")

  # Height must exceed 30mm panel + margins by a margin that
  # accounts for the tall legend (~68mm content)
  expect_gt(size[["height"]], 60)
  expect_lt(size[["height"]], 100)
})

test_that("get_gtab_size handles no-legend plots", {
  p <- study |>
    tidyplot(x = treatment, y = score) |>
    add_data_points()
  gtab <- ggplot2::ggplotGrob(p)
  size <- tidyplots:::get_gtab_size(gtab, "mm")

  expect_true(is.numeric(size[["width"]]))
  expect_true(is.numeric(size[["height"]]))
})

test_that("get_gtab_size respects unit argument", {
  p <- study |>
    tidyplot(x = treatment, y = score, color = treatment) |>
    add_data_points()
  gtab <- ggplot2::ggplotGrob(p)

  size_mm <- tidyplots:::get_gtab_size(gtab, "mm")
  size_cm <- tidyplots:::get_gtab_size(gtab, "cm")
  size_in <- tidyplots:::get_gtab_size(gtab, "in")

  expect_equal(size_mm[["width"]] / 10, size_cm[["width"]], tolerance = 0.01)
  expect_equal(size_cm[["width"]] / 2.54, size_in[["width"]], tolerance = 0.01)
})

# -- get_layout_size -----------------------------------------------------------

test_that("get_layout_size works with single plot", {
  p <- study |>
    tidyplot(x = treatment, y = score, color = treatment) |>
    add_data_points()
  result <- tidyplots:::get_layout_size(p, "mm")

  expect_type(result, "list")
  expect_named(result, c("units", "pages", "max"))
  expect_equal(result$units, "mm")
  expect_equal(nrow(result$pages), 1)
  expect_true(is.numeric(result$max[["width"]]))
})

test_that("get_layout_size works with list of plots", {
  p1 <- study |> tidyplot(x = treatment, y = score) |> add_data_points()
  p2 <- study |> tidyplot(x = treatment, y = score) |> add_data_points() |>
    adjust_size(80, 80)
  result <- tidyplots:::get_layout_size(list(p1, p2), "mm")

  expect_equal(nrow(result$pages), 2)
  # Max should reflect the larger plot
  expect_gt(result$max[["width"]], 80)
  expect_lt(result$max[["width"]], 120)
})

# -- print.tidyplot ------------------------------------------------------------

test_that("print.tidyplot returns invisible plot", {
  p <- study |>
    tidyplot(x = treatment, y = score, color = treatment) |>
    add_data_points()
  result <- withVisible(print(p))
  expect_false(result$visible)
  expect_s3_class(result$value, "tidyplot")
})

test_that("print.tidyplot does not error", {
  p <- study |>
    tidyplot(x = treatment, y = score, color = treatment) |>
    add_data_points()
  expect_no_error(print(p))
})

test_that("print.tidyplot works with viewer_scaling disabled", {
  withr::local_options(tidyplots.viewer_scaling = FALSE)
  p <- study |>
    tidyplot(x = treatment, y = score, color = treatment) |>
    add_data_points()
  expect_no_error(print(p))
})
