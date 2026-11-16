test_that("data_exclude() excludes rows and records the reason", {
  p <- study |>
    tidyplot(x = treatment, y = score, color = treatment) |>
    data_exclude(
      participant %in% c("p03", "p17"),
      reason = "Failed quality control."
    )
  n_excluded <- sum(study$participant %in% c("p03", "p17"))
  expect_equal(nrow(p$data), nrow(study) - n_excluded)
  expect_false(any(p$data$participant %in% c("p03", "p17")))
  entry <- p$tidyplot$log$steps[[2]]
  expect_equal(entry$kind, "exclusion")
  r <- entry$record
  expect_equal(r$condition, 'participant %in% c("p03", "p17")')
  expect_equal(r$reason, "Failed quality control.")
  expect_equal(r$excluded, n_excluded)
  expect_equal(sum(r$excluded_per_group), n_excluded)
  expect_equal(
    sort(r$excluded_rows$participant),
    sort(study$participant[study$participant %in% c("p03", "p17")])
  )
  expect_null(r$kept_with_na_condition)
})

test_that("data_exclude() follows filter_out() for NA conditions", {
  d <- study
  d$score[which(d$treatment == "B")[1:2]] <- NA
  p <- d |>
    tidyplot(x = treatment, y = score) |>
    data_exclude(score > 40, treatment != "A", reason = "Example.")
  expected <- dplyr::filter_out(d, score > 40, treatment != "A")
  expect_equal(nrow(p$data), nrow(expected))
  r <- p$tidyplot$log$steps[[2]]$record
  expect_equal(r$condition, "score > 40 & treatment != \"A\"")
  expect_equal(r$excluded, nrow(d) - nrow(expected))
  expect_equal(r$kept_with_na_condition, 2L)
})

test_that("data_exclude() requires a reason and a condition", {
  p <- study |> tidyplot(x = treatment, y = score)
  expect_error(data_exclude(p, score > 40), "reason")
  expect_error(data_exclude(p, score > 40, reason = ""), "non-empty")
  expect_error(data_exclude(p, reason = "Example."), "condition")
  expect_error(data_exclude(p, score, reason = "Example."), "logical")
})
