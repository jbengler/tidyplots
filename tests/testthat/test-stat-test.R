test_results <- function(plot) {
  built <- suppressWarnings(ggplot2::ggplot_build(plot))
  is_test <- vapply(
    plot$layers,
    function(l) {
      inherits(l$stat, "StatTidyplotsTest") ||
        inherits(l$stat, "StatTidyplotsBracket")
    },
    logical(1)
  )
  built$data[[which(is_test)]]
}

test_that("comparisons along x use level names and exact p values", {
  p <- study |>
    tidyplot(x = treatment, y = score) |>
    add_test_pvalue(ref.group = "A", p.adjust.method = "holm", hide_info = TRUE)
  r <- test_results(p)
  expect_equal(r$group1, c("A", "A", "A"))
  expect_equal(r$group2, c("B", "C", "D"))
  raw <- vapply(
    c("B", "C", "D"),
    function(g) {
      stats::t.test(
        study$score[study$treatment == "A"],
        study$score[study$treatment == g]
      )$p.value
    },
    numeric(1)
  )
  expect_equal(r$p, unname(raw))
  # adjusted from exact, not rounded p values
  expect_equal(r$p.adj, unname(stats::p.adjust(raw, "holm")))
  expect_equal(r$label[1], format_p_value(r$p.adj[1], 0.0001))
})

test_that("ref.group and comparisons accept positions and names", {
  p <- study |> tidyplot(x = treatment, y = score)
  by_position <- test_results(p |> add_test_pvalue(ref.group = 2, hide_info = TRUE))
  expect_equal(unique(by_position$group1), "B")
  r <- test_results(
    p |> add_test_pvalue(comparisons = list(c(1, 4), c("B", "C")), hide_info = TRUE)
  )
  expect_equal(paste(r$group1, r$group2), c("A D", "B C"))
})

test_that("comparisons within color groups are made per x group", {
  p <- study |>
    tidyplot(x = dose, y = score, color = group) |>
    add_test_pvalue(hide_info = TRUE)
  r <- test_results(p)
  expect_equal(r$x_level, c("high", "low"))
  expect_equal(r$group1, c("placebo", "placebo"))
  expect_equal(r$group2, c("treatment", "treatment"))
  expect_equal(r$xmin, c(0.8, 1.8))
  expect_equal(r$xmax, c(1.2, 2.2))
})

test_that("p.adjust.by = 'group' adjusts within each x group", {
  d <- data.frame(
    x = rep(c("a", "b"), each = 12),
    g = rep(rep(c("u", "v", "w"), each = 4), 2),
    y = c(1, 2, 3, 4, 3, 4, 5, 6, 6, 7, 8, 9, 2, 3, 4, 5, 2, 3, 5, 6, 8, 9, 7, 9)
  )
  panel <- test_results(
    d |>
      tidyplot(x = x, y = y, color = g) |>
      add_test_pvalue(p.adjust.method = "bonferroni", hide_info = TRUE)
  )
  group <- test_results(
    d |>
      tidyplot(x = x, y = y, color = g) |>
      add_test_pvalue(
        p.adjust.method = "bonferroni",
        p.adjust.by = "group",
        hide_info = TRUE
      )
  )
  expect_equal(panel$p.adj, pmin(1, panel$p * 6))
  expect_equal(group$p.adj, pmin(1, group$p * 3))
})

test_that("pairing is by id, not by row order", {
  d <- data.frame(
    id = rep(1:8, 2),
    g = rep(c("A", "B"), each = 8),
    v = c(5.1, 4.8, 6.2, 5.5, 4.9, 5.8, 6.0, 5.3, 6.4, 5.9, 7.8, 6.1, 6.6, 7.2, 6.8, 7.5)
  )
  expected <- stats::t.test(d$v[1:8], d$v[9:16], paired = TRUE)$p.value
  shuffled <- d[c(16:9, 1:8), ]
  r <- test_results(
    shuffled |> tidyplot(x = g, y = v) |> add_test_pvalue(paired_by = id, hide_info = TRUE)
  )
  expect_equal(r$p, expected)
  expect_equal(r$statistic_name, "t")
})

test_that("hidden comparisons are computed but not shown", {
  d <- study
  d$score <- c(1, 2, 3, 4, 5, 1.5, 2.5, 3.5, 4.5, 5.5, 20, 21, 22, 23, 24, 1, 2, 3, 4, 5)
  r <- test_results(
    d |> tidyplot(x = treatment, y = score) |> add_test_asterisks(hide_info = TRUE)
  )
  expect_equal(nrow(r), 6)
  expect_true(all(r$status == "computed"))
  expect_equal(r$shown, r$p.adj.signif != "ns")
  expect_true(any(!r$shown))
})

test_that("failed comparisons give a warning and are recorded", {
  d <- study
  d$score[d$treatment == "B"][-1] <- NA
  p <- d |>
    tidyplot(x = treatment, y = score) |>
    add_test_pvalue(ref.group = "A", hide_info = TRUE)
  expect_warning(ggplot2::ggplot_build(p), "could not be computed")
  r <- test_results(p)
  failed <- r[r$group2 == "B", ]
  expect_match(failed$status, "^failed")
  expect_false(failed$shown)
  expect_true(all(r$status[r$group2 != "B"] == "computed"))
})

test_that("tests are computed per facet panel", {
  p <- study |>
    tidyplot(x = treatment, y = score) |>
    add_test_pvalue(hide_info = TRUE) |>
    split_plot(by = group)
  r <- test_results(p)
  expect_equal(paste(r$PANEL, r$group1, r$group2), c("1 A B", "2 C D"))
})

test_that("tests follow later changes of the data", {
  p <- study |>
    tidyplot(x = treatment, y = score) |>
    add_test_pvalue(ref.group = "A", hide_info = TRUE) |>
    data_exclude(treatment == "D", reason = "Example.")
  expect_equal(test_results(p)$group2, c("B", "C"))
})

test_that("unknown arguments give a warning, known ones are used", {
  p <- study |> tidyplot(x = treatment, y = score)
  expect_warning(
    add_test_pvalue(p, bracket.shorten = 0.1, hide_info = TRUE),
    "bracket.shorten"
  )
  expect_no_warning(
    add_test_pvalue(p, tip.length = 0.05, linewidth = 0.5, hide_info = TRUE)
  )
  q <- p |> add_test_pvalue(method.args = list(var.equal = TRUE), hide_info = TRUE)
  r <- test_results(q)
  expect_equal(
    r$p[1],
    stats::t.test(
      study$score[study$treatment == "A"],
      study$score[study$treatment == "B"],
      var.equal = TRUE
    )$p.value
  )
})

test_that("pre-computed statistics are drawn with labels and hiding", {
  stat_df <- data.frame(
    group1 = c("A", "A", "B"),
    group2 = c("B", "C", "C"),
    p = c(0.03, 0.001, 0.4),
    p.signif = c("*", "***", "ns"),
    y.position = c(48, 52, 56)
  )
  p <- study |> tidyplot(x = treatment, y = score)
  r <- test_results(p |> add_test_pvalue_manual(data = stat_df))
  expect_equal(r$label, c("0.0300", "0.0010", "0.4000"))
  expect_equal(r$xmin, c(1, 1, 2))
  expect_true(all(r$shown))
  r <- test_results(p |> add_test_asterisks_manual(data = stat_df))
  expect_equal(r$label, c("*", "***", "ns"))
  expect_equal(r$shown, c(TRUE, TRUE, FALSE))
  expect_error(
    add_test_pvalue_manual(p, data = stat_df[c("group1", "p")]),
    "group2"
  )
})

test_that("plots with tests can be built and drawn", {
  p <- study |>
    tidyplot(x = dose, y = score, color = group) |>
    add_mean_bar(alpha = 0.4) |>
    add_test_asterisks(hide_info = TRUE)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(ggplot2::ggplotGrob(p))
})

test_that("tests use untransformed values on a transformed axis", {
  d <- study
  d$score[1] <- 0
  p <- d |>
    tidyplot(x = treatment, y = score) |>
    add_test_pvalue(comparisons = list(c("A", "C")), hide_info = TRUE) |>
    adjust_y_axis(transform = "log10")
  r <- test_results(p)
  expect_equal(r$n1, 5)
  expect_equal(
    r$p,
    stats::t.test(d$score[d$treatment == "A"], d$score[d$treatment == "C"])$p.value,
    tolerance = 1e-12
  )
  expect_match(r$notes, "1 value not finite on the log-10 axis, not shown", fixed = TRUE)
  w <- capture_warnings(ggplot2::ggplot_build(p))
  expect_true(any(grepl("not shown in the plot", w)))
})

test_that("tests include values outside the scale limits", {
  p <- suppressMessages(
    study |>
      tidyplot(x = treatment, y = score) |>
      add_test_pvalue(comparisons = list(c("A", "C")), hide_info = TRUE) |>
      add(ggplot2::scale_y_continuous(limits = c(30, 60)))
  )
  r <- test_results(p)
  expect_equal(c(r$n1, r$n2), c(5, 5))
  expect_equal(
    r$p,
    stats::t.test(
      study$score[study$treatment == "A"],
      study$score[study$treatment == "C"]
    )$p.value,
    tolerance = 1e-12
  )
  expect_match(r$notes, "6 values outside the axis limits, not shown", fixed = TRUE)
})

test_that("zooming with adjust_y_axis(limits) does not add notes", {
  p <- study |>
    tidyplot(x = treatment, y = score) |>
    add_test_pvalue(comparisons = list(c("A", "C")), hide_info = TRUE) |>
    adjust_y_axis(limits = c(30, 60))
  expect_equal(test_results(p)$notes, "")
})

test_that("the name of the test is taken from the result", {
  p <- study |>
    tidyplot(x = treatment, y = score) |>
    add_test_pvalue(comparisons = list(c("A", "C")), hide_info = TRUE)
  expect_equal(test_results(p)$computed_as, "Welch Two Sample t-test")
  p <- study |>
    tidyplot(x = treatment, y = score) |>
    add_test_pvalue(
      comparisons = list(c("A", "C")),
      method = "wilcox_test",
      hide_info = TRUE
    )
  # study has ties, so R uses the normal approximation
  expected <- suppressWarnings(stats::wilcox.test(
    study$score[study$treatment == "A"],
    study$score[study$treatment == "C"]
  ))$method
  expect_equal(test_results(p)$computed_as, expected)
  expect_output(view_log(p), paste("computed as:", expected), fixed = TRUE)
})

test_that("horizontal plots give the same results as vertical plots", {
  v <- study |>
    tidyplot(x = dose, y = score, color = group) |>
    add_test_pvalue(hide_info = TRUE)
  h <- study |>
    tidyplot(x = score, y = dose, color = group) |>
    add_test_pvalue(hide_info = TRUE)
  rv <- test_results(v)
  rh <- test_results(h)
  expect_equal(rh$p, rv$p)
  expect_equal(rh$ymin, rv$xmin)
  expect_equal(rh$ymax, rv$xmax)
  expect_equal(rh$x, rv$y)
  expect_true(all(rh$flipped_aes))
  expect_equal(log_test_results(h)[[1]]$p, log_test_results(v)[[1]]$p)
  expect_match(
    log_test_spec(h$layers[[1]]$stat_params$spec)$compared,
    "levels of group within each level of dose",
    fixed = TRUE
  )
})

test_that("tests need one discrete and one continuous axis", {
  expect_error(
    study |> tidyplot(x = score, y = age) |> add_test_pvalue(),
    "one discrete and one continuous axis"
  )
})

# Unequal group sizes and ties
multi_group_data <- function() {
  d <- study
  d$score[3] <- NA
  d$score[7] <- d$score[8]
  d
}

multi_group_results <- function(method, ...) {
  p <- multi_group_data() |>
    tidyplot(x = treatment, y = score) |>
    add_test_pvalue(method = method, hide_info = TRUE, ...)
  r <- test_results(p)
  r[order(r$group1, r$group2), ]
}

test_that("Tukey's HSD matches stats::TukeyHSD()", {
  d <- multi_group_data()
  expected <- stats::TukeyHSD(stats::aov(score ~ treatment, d))$treatment
  r <- multi_group_results("tukey_hsd")
  expect_equal(paste(r$group2, r$group1, sep = "-"), rownames(expected))
  expect_equal(r$p, unname(expected[, "p adj"]), tolerance = 1e-10)
  expect_equal(r$p.adj, r$p)
  expect_equal(r$n1, c(4, 4, 4, 5, 5, 5))
  expect_equal(unique(r$df), 15)
})

test_that("Tukey's HSD adjusts for all pairs, also with a reference group", {
  all <- multi_group_results("tukey_hsd")
  ref <- multi_group_results("tukey_hsd", ref.group = "A")
  expect_equal(ref$p, all$p[all$group1 == "A"])
})

test_that("Games-Howell test matches rstatix", {
  # rstatix::games_howell_test(d, score ~ treatment), which rounds to 3
  # decimals except below 0.001
  expected <- c(0.003, 0.011, 5.28e-06, 0.030, 0.001, 0.147)
  r <- multi_group_results("games_howell_test")
  rounded <- ifelse(r$p < 0.001, signif(r$p, 3), round(r$p, 3))
  expect_equal(rounded, expected)
})

test_that("Dunn's test matches rstatix", {
  # rstatix::dunn_test(d, score ~ treatment, p.adjust.method = "none"), which
  # reports the statistic as group2 - group1
  expected_p <- c(0.232615, 0.000148744, 0.0100827, 0.00583, 0.143467, 0.195599)
  expected_z <- c(1.19365, 3.79316, 2.57298, 2.75719, 1.46300, -1.29419)
  r <- multi_group_results("dunn_test")
  expect_equal(signif(r$p, 6), expected_p)
  expect_equal(signif(-r$statistic, 6), expected_z)
  holm <- multi_group_results("dunn_test", p.adjust.method = "holm")
  expect_equal(holm$p.adj, stats::p.adjust(r$p, "holm"))
})

test_that("multi-group methods work within color groups", {
  p <- study |>
    tidyplot(x = dose, y = score, color = group) |>
    add_test_pvalue(method = "tukey_hsd", hide_info = TRUE)
  r <- test_results(p)
  expect_equal(nrow(r), 2)
  d <- study[study$dose == "high", ]
  expected <- stats::TukeyHSD(stats::aov(score ~ group, d))$group[, "p adj"]
  expect_equal(r$p[r$x_level == "high"], unname(expected), tolerance = 1e-10)
})

test_that("multi-group methods report groups without enough values", {
  d <- study
  d$score[d$treatment == "B"][-1] <- NA
  p <- d |>
    tidyplot(x = treatment, y = score) |>
    add_test_pvalue(method = "games_howell_test", ref.group = "A", hide_info = TRUE)
  expect_warning(r <- ggplot2::ggplot_build(p)$data[[1]], "could not be computed")
  expect_equal(r$status[r$group2 == "B"], "failed: not enough values")
  expect_equal(r$status[r$group2 != "B"], c("computed", "computed"))
})

test_that("unsupported settings of multi-group methods are errors", {
  p <- study |> tidyplot(x = treatment, y = score)
  expect_error(
    add_test_pvalue(p, method = "tukey_hsd", paired_by = participant),
    "does not support paired"
  )
  expect_error(
    add_test_pvalue(p, method = "games_howell_test", p.adjust.method = "holm"),
    "already adjusted"
  )
  expect_error(
    add_test_pvalue(p, method = "dunn_test", method.args = list(alternative = "less")),
    "not supported"
  )
})

test_that("pre-computed statistics work in horizontal plots", {
  stat_df <- data.frame(
    group1 = c("A", "A"), group2 = c("B", "C"),
    p = c(0.025, 0.001), y.position = 50
  )
  v <- study |>
    tidyplot(x = treatment, y = score) |>
    add_test_pvalue_manual(data = stat_df)
  h <- study |>
    tidyplot(x = score, y = treatment) |>
    add_test_pvalue_manual(data = stat_df)
  rv <- test_results(v)
  rh <- test_results(h)
  expect_equal(rh$ymin, rv$xmin)
  expect_equal(rh$ymax, rv$xmax)
  expect_equal(rh$x, rv$y)
  expect_true(all(rh$flipped_aes))
  # explicit orientation gives the same result as detection
  h2 <- study |>
    tidyplot(x = score, y = treatment) |>
    add_test_pvalue_manual(data = stat_df, orientation = "y")
  expect_equal(test_results(h2)$x, rh$x)
})

test_that("orientation of pre-computed statistics is checked", {
  p <- study |> tidyplot(x = treatment, y = score)
  expect_false(test_manual_flipped(p, NA))
  expect_true(test_manual_flipped(p, "y"))
  expect_error(
    add_test_pvalue_manual(p, data = data.frame(), orientation = "z"),
    "must be"
  )
})
