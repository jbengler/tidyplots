test_that("view_log() reports rows plotted, dropped and per group", {
  d <- study
  d$score[c(3, 8)] <- NA
  p <- d |> tidyplot(x = treatment, y = score, color = treatment)
  f <- log_figure_facts(p)
  expect_equal(f$n_total, 20)
  expect_equal(f$n_used, 18)
  expect_equal(f$na, c(score = 2L))
  expect_equal(sum(f$n_groups), 18)
  expect_equal(f$group_vars, "treatment")
  expect_output(view_log(p), "18 of 20 rows | dropped 2 (NA in score: 2)", fixed = TRUE)
})

test_that("view_log() reports the effective axis transformation", {
  d <- study
  d$score[1] <- 0
  p <- d |>
    tidyplot(x = treatment, y = score) |>
    adjust_y_axis(transform = "log10")
  f <- log_figure_facts(p)
  expect_equal(f$transforms$y$transform, "log-10")
  expect_equal(f$transforms$y$non_finite, 1)
  expect_equal(f$n_used, 19)
})

test_that("view_log() reports values removed by scale limits", {
  p <- suppressMessages(
    study |>
      tidyplot(x = treatment, y = score) |>
      add(ggplot2::scale_y_continuous(limits = c(30, 60)))
  )
  f <- log_figure_facts(p)
  expect_equal(f$limits$y, list(variable = "score", outside = 16L))
  expect_equal(f$n_used, 4)
  expect_equal(sum(f$n_groups), 4)
  expect_output(
    view_log(p),
    "4 of 20 rows | dropped 16 (outside y scale limits of score: 16)",
    fixed = TRUE
  )
  # one-sided limits
  p <- suppressMessages(
    study |>
      tidyplot(x = treatment, y = score) |>
      add(ggplot2::scale_y_continuous(limits = c(NA, 30)))
  )
  expect_equal(log_figure_facts(p)$n_used, 16)
})

test_that("limits that remove no values are not reported", {
  squished <- suppressMessages(
    study |>
      tidyplot(x = treatment, y = score) |>
      add(ggplot2::scale_y_continuous(limits = c(30, 60), oob = scales::squish))
  )
  zoomed <- study |>
    tidyplot(x = treatment, y = score) |>
    adjust_y_axis(limits = c(30, 60))
  for (p in list(squished, zoomed)) {
    f <- log_figure_facts(p)
    expect_equal(f$limits, list())
    expect_equal(f$n_used, 20)
  }
})

test_that("each removed row is counted once with its first reason", {
  d <- study
  d$score[1] <- 0
  d$score[2] <- NA
  p <- suppressMessages(
    d |>
      tidyplot(x = treatment, y = score) |>
      add(ggplot2::scale_y_continuous(transform = "log10", limits = c(5, 60)))
  )
  f <- log_figure_facts(p)
  expect_equal(f$na, c(score = 1L))
  expect_equal(f$transforms$y$non_finite, 1)
  expect_equal(f$limits$y$outside, 1)
  expect_equal(f$n_used, 17)
})

test_that("groups include facet variables", {
  p <- study |>
    tidyplot(x = treatment, y = score) |>
    split_plot(by = group)
  f <- log_figure_facts(p)
  expect_equal(f$group_vars, c("treatment", "group"))
  expect_equal(sum(f$n_groups), 20)
})

test_that("view_log() reports the results of statistical tests", {
  p <- study |>
    tidyplot(x = treatment, y = score) |>
    add_mean_bar() |>
    add_test_asterisks(ref.group = "A", hide_info = TRUE) |>
    split_plot(by = group)
  results <- log_test_results(p)
  expect_named(results, "3")
  r <- results[["3"]]
  expect_equal(r$panel, "placebo")
  expect_equal(paste(r$group1, r$group2), "A B")
  expect_equal(
    r$p,
    stats::t.test(
      study$score[study$treatment == "A"],
      study$score[study$treatment == "B"]
    )$p.value
  )
  out <- capture.output(view_log(p))
  expect_true(any(grepl("[placebo] A vs B | n 5/5 | t = ", out, fixed = TRUE)))
})

test_that("view_log() reports hidden and failed comparisons", {
  d <- study
  d$score[d$treatment == "B"][-1] <- NA
  d$score[d$treatment == "C"] <- d$score[d$treatment == "A"] + 0.1
  p <- d |>
    tidyplot(x = treatment, y = score) |>
    add_test_asterisks(ref.group = "A", hide_info = TRUE)
  out <- capture.output(view_log(p))
  expect_true(any(grepl("A vs B | failed:", out, fixed = TRUE)))
  expect_true(any(grepl("A vs C .*\\(computed, not shown\\)", out)))
})

test_that("results are matched to their step by test id", {
  p <- study |>
    tidyplot(x = treatment, y = score) |>
    add_test_pvalue(ref.group = "A", hide_info = TRUE) |>
    add_mean_bar() |>
    add_test_asterisks(comparisons = list(c("B", "C")), hide_info = TRUE)
  ids <- vapply(p$tidyplot$log$steps, function(e) e$test_id %||% NA_integer_, integer(1))
  expect_equal(ids, c(NA, 1L, NA, 2L))
  results <- log_test_results(p)
  expect_named(results, c("2", "4"))
  expect_equal(results[["2"]]$group1, c("A", "A", "A"))
  expect_equal(paste(results[["4"]]$group1, results[["4"]]$group2), "B C")
})

test_that("a test layer without a log step does not break the log", {
  p <- study |>
    tidyplot(x = treatment, y = score) |>
    add_test_pvalue(hide_info = TRUE) |>
    add_test_asterisks(ref.group = "A", hide_info = TRUE)
  p$tidyplot$log$steps[[2]]$test_id <- NULL
  results <- log_test_results(p)
  expect_named(results, "3")
  expect_equal(unique(results[["3"]]$group1), "A")
  expect_no_error(capture.output(view_log(p)))
})

test_that("pre-computed statistics are not reported as computed results", {
  stat_df <- data.frame(group1 = "A", group2 = "B", p = 0.03, y.position = 50)
  p <- study |>
    tidyplot(x = treatment, y = score) |>
    add_test_pvalue_manual(data = stat_df)
  expect_equal(log_test_results(p), list())
})

test_that("view_log() reports rows per x value for summaries over continuous x", {
  d <- data.frame(
    day = rep(1:3, times = c(2, 3, 4)),
    score = 1:9,
    group = rep(c("a", "b"), length.out = 9)
  )
  p <- d |> tidyplot(x = day, y = score) |> add_mean_line()
  f <- log_figure_facts(p)
  expect_equal(unname(f$n_by_x), c(2L, 3L, 4L))
  expect_output(view_log(p), "rows per  day: 2-4 (summaries are computed per x value)", fixed = TRUE)

  p <- d |> tidyplot(x = day, y = score, color = group) |> add_sem_ribbon()
  expect_output(
    view_log(p),
    "          day/group: 1-2 (summaries are computed per x value)",
    fixed = TRUE
  )

  # without summary layers, rows per x value are not reported
  p <- d |> tidyplot(x = day, y = score) |> add_data_points()
  expect_null(log_figure_facts(p)$n_by_x)
})

test_that("add_metadata() records the unit of n", {
  p <- study |>
    tidyplot(x = treatment, y = score) |>
    add_metadata(n_unit = "participants")
  expect_equal(p$tidyplot$log$header$metadata$n_unit, "participants")
  expect_output(view_log(p), "metadata  n_unit: participants", fixed = TRUE)
})
