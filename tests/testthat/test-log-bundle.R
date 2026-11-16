bundle_example <- function() {
  study |>
    tidyplot(x = treatment, y = score, color = treatment) |>
    add_metadata(y_unit = "points") |>
    data_exclude(
      participant == "p03" & treatment == "A",
      reason = "Failed quality control."
    ) |>
    add_mean_bar(alpha = 0.4) |>
    add_data_points() |>
    add_test_pvalue(ref.group = "A", hide_info = TRUE) |>
    sort_x_axis_levels()
}

test_that("base64 encoding is correct", {
  expect_equal(base64_encode(charToRaw("Man")), "TWFu")
  expect_equal(base64_encode(charToRaw("Ma")), "TWE=")
  expect_equal(base64_encode(charToRaw("M")), "TQ==")
  expect_equal(base64_encode(raw()), "")
  expect_equal(
    base64_encode(charToRaw("tidyplots log")),
    "dGlkeXBsb3RzIGxvZw=="
  )
})

test_that("the log keeps the input data", {
  p <- bundle_example()
  expect_identical(p$tidyplot$log$data_input, study)
  expect_equal(rlang::hash(p$tidyplot$log$data_input), p$tidyplot$log$header$data$hash)
})

test_that("save_bundle() writes all files", {
  path <- tempfile("bundle")
  p <- bundle_example()
  suppressMessages(save_bundle(p, path))
  expect_setequal(
    list.files(path),
    c(
      "report.html", "figure.png", "figure.pdf", "data_input.csv",
      "data_plotted.csv", "data_dictionary.csv", "test_results.csv",
      "excluded_rows.csv", "log.txt"
    )
  )

  input <- utils::read.csv(file.path(path, "data_input.csv"))
  expect_equal(nrow(input), nrow(study))
  plotted <- utils::read.csv(file.path(path, "data_plotted.csv"))
  expect_equal(nrow(plotted), nrow(study) - 1)
  expect_false(".single_color" %in% names(plotted))

  dictionary <- utils::read.csv(file.path(path, "data_dictionary.csv"))
  treatment <- dictionary[
    dictionary$dataset == "plotted" & dictionary$column == "treatment",
  ]
  expect_equal(treatment$level, levels(p$data$treatment))
  expect_equal(treatment$level_position, seq_along(levels(p$data$treatment)))
  expect_equal(unique(treatment$role), "x, color")
  score <- dictionary[dictionary$dataset == "input" & dictionary$column == "score", ]
  expect_equal(score$unit, "points")

  results <- utils::read.csv(file.path(path, "test_results.csv"))
  expect_equal(results$test, rep("Welch's t-test", 3))
  expect_equal(
    results$p[results$group2 == "B"],
    stats::t.test(
      study$score[study$treatment == "A" & study$participant != "p03"],
      study$score[study$treatment == "B"]
    )$p.value
  )

  excluded <- utils::read.csv(file.path(path, "excluded_rows.csv"))
  expect_equal(excluded$reason, "Failed quality control.")
  expect_equal(excluded$participant, "p03")

  expect_true(any(grepl("<tidyplot log>", readLines(file.path(path, "log.txt")))))
})

test_that("the report is self-contained and complete", {
  path <- tempfile("bundle")
  suppressMessages(save_bundle(bundle_example(), path))
  html <- paste(readLines(file.path(path, "report.html")), collapse = "\n")
  expect_match(html, "<style>", fixed = TRUE)
  expect_match(html, "src=\"data:image/png;base64,", fixed = TRUE)
  for (section in c("Figure", "Data", "Steps", "Statistical results", "Files")) {
    expect_match(html, paste0("<h2>", section, "</h2>"), fixed = TRUE)
  }
  expect_match(html, "Failed quality control.", fixed = TRUE)
  expect_no_match(html, "Figure legend", fixed = TRUE)
  expect_match(html, "not a guarantee of reproducibility", fixed = TRUE)
})

test_that("save_bundle() does not overwrite by default", {
  path <- tempfile("bundle")
  p <- study |> tidyplot(x = treatment, y = score)
  suppressMessages(save_bundle(p, path))
  expect_error(save_bundle(p, path), "already exists")
  expect_no_error(suppressMessages(save_bundle(p, path, overwrite = TRUE)))
  expect_false(file.exists(file.path(path, "test_results.csv")))
  expect_false(file.exists(file.path(path, "excluded_rows.csv")))
})

test_that("save_bundle() handles plots split into pages", {
  path <- tempfile("bundle")
  pages <- suppressMessages(
    study |>
      tidyplot(x = treatment, y = score) |>
      add_test_pvalue(hide_info = TRUE) |>
      split_plot(by = group, ncol = 1, nrow = 1)
  )
  suppressMessages(save_bundle(pages, path))
  expect_true(all(file.exists(file.path(path, c("figure_1.png", "figure_2.png")))))
  plotted <- utils::read.csv(file.path(path, "data_plotted.csv"))
  expect_equal(sort(unique(plotted$page)), 1:2)
  results <- utils::read.csv(file.path(path, "test_results.csv"))
  expect_equal(results$page, 1:2)
  expect_equal(paste(results$group1, results$group2), c("A B", "C D"))
})

test_that("save_bundle() validates its input", {
  path <- tempfile("bundle")
  expect_error(save_bundle(ggplot2::ggplot(), path), "must be a tidyplot")
})

test_that("save_bundle() does not leave a graphics device file behind", {
  dir <- tempfile("cwd")
  dir.create(dir)
  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)
  while (grDevices::dev.cur() > 1) {
    grDevices::dev.off()
  }
  p <- study |> tidyplot(x = treatment, y = score)
  suppressMessages(save_bundle(p, "bundle"))
  expect_false(file.exists("Rplots.pdf"))
  expect_equal(unname(grDevices::dev.cur()), 1L)
})
