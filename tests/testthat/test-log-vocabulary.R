# These tests pin the test names recorded in the log to what is actually
# computed. Each plot is built, the p-values computed when the plot is built
# are read back, and compared with an independent computation in base R. If
# the computation changes, these tests fail instead of the log silently
# describing the wrong test.

test_that("test names are derived from method and arguments", {
  expect_equal(log_test_name("t_test"), "Welch's t-test")
  expect_equal(log_test_name("t_test", list(var.equal = TRUE)), "Student's t-test")
  expect_equal(log_test_name("t_test", list(paired = TRUE)), "paired t-test")
  expect_match(log_test_name("wilcox_test"), "rank-sum")
  expect_equal(log_test_name("wilcox_test", list(paired = TRUE)), "Wilcoxon signed-rank test")
  expect_equal(log_p_adjust_name("BH"), "Benjamini-Hochberg")
  expect_equal(log_p_adjust_name("fdr"), "Benjamini-Hochberg")
  expect_match(log_p_adjust_name("foo"), "unmapped")
  expect_equal(log_alternative_name(NULL), "two-sided")
  expect_equal(
    log_symnum_text(list(
      cutpoints = c(0, 0.001, 0.01, 0.05, Inf),
      symbols = c("***", "**", "*", "ns")
    )),
    "*** p < 0.001, ** p < 0.01, * p < 0.05"
  )
})

# p-values computed for the pairwise comparisons of a plot
pwc_results <- function(plot) {
  layers <- ggplot2::ggplot_build(plot)$data
  is_test <- vapply(
    plot$layers,
    function(l) inherits(l$stat, "StatTidyplotsTest"),
    logical(1)
  )
  d <- layers[[which(is_test)]]
  d[order(d$group1, d$group2), c("group1", "group2", "p", "p.adj")]
}

recorded_test <- function(plot) {
  steps <- plot$tidyplot$log$steps
  steps[[length(steps)]]$record$test
}

# p-values are not rounded, so they must match exactly
expect_p_equal <- function(actual, expected) {
  expect_equal(actual, expected, tolerance = 1e-12)
}

fixture <- data.frame(
  id = rep(1:8, times = 3),
  group = factor(rep(c("A", "B", "C"), each = 8)),
  value = c(
    5.1, 4.8, 6.2, 5.5, 4.9, 5.8, 6.0, 5.3,
    6.4, 5.9, 7.8, 6.1, 6.6, 7.2, 6.8, 7.5,
    5.0, 6.9, 9.1, 4.2, 8.3, 7.7, 3.9, 6.5
  )
)
values <- function(g) fixture$value[fixture$group == g]

test_that("the fixture distinguishes the tests being verified", {
  welch <- stats::t.test(values("A"), values("C"), var.equal = FALSE)$p.value
  student <- stats::t.test(values("A"), values("C"), var.equal = TRUE)$p.value
  expect_gt(abs(welch - student), 1e-6)
  unpaired <- stats::t.test(values("A"), values("B"))$p.value
  paired <- stats::t.test(values("A"), values("B"), paired = TRUE)$p.value
  expect_gt(abs(unpaired - paired), 1e-6)
})

test_that("t_test is Welch's t-test by default", {
  p <- fixture |>
    tidyplot(x = group, y = value) |>
    add_test_pvalue(ref.group = "A", hide_info = TRUE)
  expect_equal(recorded_test(p), "Welch's t-test")
  r <- pwc_results(p)
  expect_p_equal(r$p, c(
    stats::t.test(values("A"), values("B"), var.equal = FALSE)$p.value,
    stats::t.test(values("A"), values("C"), var.equal = FALSE)$p.value
  ))
})

test_that("t_test with var.equal = TRUE is Student's t-test", {
  p <- fixture |>
    tidyplot(x = group, y = value) |>
    add_test_pvalue(
      ref.group = "A",
      method.args = list(var.equal = TRUE),
      hide_info = TRUE
    )
  expect_equal(recorded_test(p), "Student's t-test")
  r <- pwc_results(p)
  expect_p_equal(r$p, c(
    stats::t.test(values("A"), values("B"), var.equal = TRUE)$p.value,
    stats::t.test(values("A"), values("C"), var.equal = TRUE)$p.value
  ))
})

test_that("t_test with paired_by is the paired t-test", {
  p <- fixture |>
    tidyplot(x = group, y = value) |>
    add_test_pvalue(ref.group = "A", paired_by = id, hide_info = TRUE)
  expect_equal(recorded_test(p), "paired t-test")
  r <- pwc_results(p)
  expect_p_equal(r$p, c(
    stats::t.test(values("A"), values("B"), paired = TRUE)$p.value,
    stats::t.test(values("A"), values("C"), paired = TRUE)$p.value
  ))
})

test_that("wilcox_test is the Wilcoxon rank-sum test", {
  p <- fixture |>
    tidyplot(x = group, y = value) |>
    add_test_pvalue(method = "wilcox_test", ref.group = "A", hide_info = TRUE)
  expect_match(recorded_test(p), "Wilcoxon rank-sum test")
  r <- pwc_results(p)
  expect_p_equal(r$p, c(
    stats::wilcox.test(values("A"), values("B"))$p.value,
    stats::wilcox.test(values("A"), values("C"))$p.value
  ))
})

test_that("wilcox_test with paired_by is the Wilcoxon signed-rank test", {
  p <- fixture |>
    tidyplot(x = group, y = value) |>
    add_test_pvalue(
      method = "wilcox_test",
      ref.group = "A",
      paired_by = id,
      hide_info = TRUE
    )
  expect_equal(recorded_test(p), "Wilcoxon signed-rank test")
  r <- pwc_results(p)
  expect_p_equal(r$p, c(
    stats::wilcox.test(values("A"), values("B"), paired = TRUE)$p.value,
    stats::wilcox.test(values("A"), values("C"), paired = TRUE)$p.value
  ))
})

test_that("the alternative hypothesis is passed on as recorded", {
  p <- fixture |>
    tidyplot(x = group, y = value) |>
    add_test_pvalue(
      ref.group = "A",
      method.args = list(alternative = "less"),
      hide_info = TRUE
    )
  steps <- p$tidyplot$log$steps
  expect_equal(steps[[2]]$record$alternative, "one-sided (less)")
  r <- pwc_results(p)
  expect_p_equal(r$p, c(
    stats::t.test(values("A"), values("B"), alternative = "less")$p.value,
    stats::t.test(values("A"), values("C"), alternative = "less")$p.value
  ))
})

test_that("p-value adjustments match stats::p.adjust()", {
  for (method in c("holm", "bonferroni", "BH", "BY", "hochberg", "hommel")) {
    p <- fixture |>
      tidyplot(x = group, y = value) |>
      add_test_pvalue(p.adjust.method = method, hide_info = TRUE)
    steps <- p$tidyplot$log$steps
    expect_false(grepl("unmapped", steps[[2]]$record$p_adjustment))
    r <- pwc_results(p)
    raw <- c(
      stats::t.test(values("A"), values("B"))$p.value,
      stats::t.test(values("A"), values("C"))$p.value,
      stats::t.test(values("B"), values("C"))$p.value
    )
    expect_p_equal(r$p.adj, stats::p.adjust(raw, method = method))
  }
})

test_that("unsupported test methods are an error", {
  p <- fixture |> tidyplot(x = group, y = value)
  expect_error(add_test_pvalue(p, method = "sign_test"), "add_test_pvalue_manual")
  expect_error(add_test_asterisks(p, method = "emmeans_test"), "add_test_pvalue_manual")
  # aliases of supported methods
  q <- p |> add_test_pvalue(method = "wilcoxon", hide_info = TRUE)
  expect_match(recorded_test(q), "Wilcoxon rank-sum test")
})

test_that("arguments not reflected in the test name are reported", {
  p <- fixture |>
    tidyplot(x = group, y = value) |>
    add_test_pvalue(
      ref.group = "A",
      method.args = list(mu = 1, conf.level = 0.9),
      hide_info = TRUE
    )
  r <- p$tidyplot$log$steps[[2]]$record
  expect_equal(r$arguments_not_reflected_in_test_name, c("mu", "conf.level"))
  expect_equal(
    r$implementation,
    "stats::t.test(mu = 1, conf.level = 0.9, paired = FALSE, var.equal = FALSE)"
  )
})

test_that("tests computed by tidyplots are named and described", {
  p <- fixture |> tidyplot(x = group, y = value)
  for (m in c("tukey_hsd", "games_howell_test", "dunn_test")) {
    q <- p |> add_test_pvalue(method = m, ref.group = 1, hide_info = TRUE)
    record <- q$tidyplot$log$steps[[2]]$record
    expect_equal(record$test, log_test_name(m))
    expect_match(record$implementation, "^computed by tidyplots")
  }
  expect_equal(log_test_name("tukey_hsd"), "Tukey's HSD test")
  expect_equal(log_test_name("games_howell_test"), "Games-Howell test")
  expect_equal(log_test_name("dunn_test"), "Dunn's test")
  q <- p |> add_test_pvalue(method = "tukey", ref.group = 1, hide_info = TRUE)
  record <- q$tidyplot$log$steps[[2]]$record
  expect_equal(record$p_adjustment, "built into the test (studentized range)")
  expect_match(record$p_adjustment_scope, "including those not shown", fixed = TRUE)
  q <- p |> add_test_pvalue(method = "dunn", p.adjust.method = "holm", hide_info = TRUE)
  expect_equal(q$tidyplot$log$steps[[2]]$record$p_adjustment, "Holm")
})
