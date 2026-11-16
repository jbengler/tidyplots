# Standard terms for the log --------------------------------------------------
#
# The log describes statistical methods in standard terms rather than R
# function names, so that it can be read without knowing R. Names are derived
# from the method and its arguments. Unknown methods are recorded as given and
# flagged as unmapped instead of guessing.

# Name of a pairwise test, see `test_engines`. The names are verified by tests
# against base R, see tests/testthat/test-log-vocabulary.R
log_test_name <- function(method, method_args = list()) {
  paired <- isTRUE(method_args$paired)
  switch(
    method,
    t_test = if (paired) {
      "paired t-test"
    } else if (isTRUE(method_args$var.equal)) {
      "Student's t-test"
    } else {
      "Welch's t-test"
    },
    wilcox_test = if (paired) {
      "Wilcoxon signed-rank test"
    } else {
      "Wilcoxon rank-sum test (Mann-Whitney U test)"
    },
    tukey_hsd = "Tukey's HSD test",
    games_howell_test = "Games-Howell test",
    dunn_test = "Dunn's test",
    NULL
  )
}

# Implementation of the tests computed by tidyplots itself. The results are
# verified by tests against rstatix, see tests/testthat/test-stat-test.R
log_test_implementation <- function(method) {
  switch(
    method,
    tukey_hsd = paste(
      "computed by tidyplots: variance pooled over all groups compared,",
      "Tukey-Kramer standard error, stats::ptukey()"
    ),
    games_howell_test = paste(
      "computed by tidyplots: Welch standard error and degrees of freedom",
      "per pair, stats::ptukey()"
    ),
    dunn_test = paste(
      "computed by tidyplots: mean ranks across all groups compared,",
      "correction for ties, normal approximation"
    ),
    paste0(method, " (unmapped)")
  )
}

# Name of a p-value adjustment method of stats::p.adjust()
log_p_adjust_name <- function(method) {
  switch(
    method,
    none = "none",
    holm = "Holm",
    hochberg = "Hochberg",
    hommel = "Hommel",
    bonferroni = "Bonferroni",
    BH = ,
    fdr = "Benjamini-Hochberg",
    BY = "Benjamini-Yekutieli",
    paste0(method, " (unmapped)")
  )
}

log_alternative_name <- function(alternative) {
  switch(
    alternative %||% "two.sided",
    two.sided = "two-sided",
    greater = "one-sided (greater)",
    less = "one-sided (less)",
    paste0(alternative, " (unmapped)")
  )
}

# Thresholds of significance symbols, e.g. "*** p < 0.001, ** p < 0.01"
log_symnum_text <- function(symnum.args) {
  cutpoints <- symnum.args$cutpoints
  symbols <- symnum.args$symbols
  if (is.null(cutpoints) || is.null(symbols)) {
    return(NULL)
  }
  upper <- cutpoints[-1]
  shown <- is.finite(upper) & upper < 1
  paste(paste0(symbols[shown], " p < ", upper[shown]), collapse = ", ")
}

# Specification of a pairwise test, as recorded in the log. `spec` is created
# by `new_test_spec()`.
log_test_spec <- function(spec) {
  method <- spec$method
  method_args <- spec$method_args
  if (spec$paired) {
    method_args$paired <- TRUE
  }
  p.adjust.method <- spec$p.adjust.method
  p.adjust.by <- spec$p.adjust.by
  ref.group <- spec$ref.group
  comparisons <- spec$comparisons
  paired_by <- spec$paired_by
  label <- spec$label
  symnum.args <- spec$symnum.args
  hide.ns <- spec$hide.ns
  x_var <- spec$x_var
  color_var <- spec$color_var
  within_color <- spec$within_colour

  test <- log_test_name(method, method_args)
  adjusted <- isTRUE(test_engines[[method]]$adjusted)
  asterisks <- is.character(label) && grepl("signif", label)

  shown_args <- method_args[setdiff(names(method_args), "comparisons")]
  shown_args$paired <- isTRUE(method_args$paired)
  if (method == "t_test" && is.null(shown_args$var.equal)) {
    shown_args$var.equal <- FALSE
  }
  not_reflected <- setdiff(
    names(method_args),
    c("paired", "var.equal", "alternative", "comparisons")
  )

  list(
    test = test %||% method,
    implementation = if (!method %in% c("t_test", "wilcox_test")) {
      log_test_implementation(method)
    } else paste0(
      if (method == "t_test") "stats::t.test" else "stats::wilcox.test",
      "(",
      paste(
        names(shown_args),
        vapply(
          shown_args,
          function(x) paste(deparse(x), collapse = " "),
          character(1)
        ),
        sep = " = ",
        collapse = ", "
      ),
      ")"
    ),
    arguments_not_reflected_in_test_name = if (length(not_reflected) > 0) {
      not_reflected
    },
    alternative = log_alternative_name(method_args$alternative),
    paired_by = paired_by,
    comparisons = if (!is.null(comparisons)) {
      vapply(
        comparisons,
        function(x) {
          if (is.numeric(x)) {
            paste0("groups ", paste(x, collapse = " vs "), " (by position)")
          } else {
            paste(x, collapse = " vs ")
          }
        },
        character(1)
      )
    } else if (!is.null(ref.group)) {
      paste0("each group vs reference ", ref.group)
    } else {
      "all pairwise"
    },
    compared = if (within_color) {
      paste0("levels of ", color_var, " within each level of ", x_var)
    } else {
      paste0("levels of ", x_var)
    },
    p_adjustment = if (adjusted) {
      "built into the test (studentized range)"
    } else {
      log_p_adjust_name(p.adjust.method)
    },
    p_adjustment_scope = if (adjusted) {
      paste0(
        "all pairwise comparisons of the ",
        if (within_color) "levels within each group" else "levels in each panel",
        if (!is.null(comparisons) || !is.null(ref.group)) ", including those not shown"
      )
    } else if (p.adjust.method != "none") {
      switch(
        p.adjust.by,
        panel = "all comparisons within each panel",
        group = "comparisons within each group",
        p.adjust.by
      )
    },
    shown_as = if (asterisks) "asterisks" else "p-values",
    significance = if (asterisks) log_symnum_text(symnum.args),
    hide_ns = hide.ns
  )
}

# Record of pre-computed statistics supplied by the user
log_manual_test <- function(plot, data, xmin, xmax, label, frame = rlang::caller_env()) {
  log_record(
    plot,
    "test",
    test = "pre-computed by user, method not verified",
    comparisons = if (all(c(xmin, xmax) %in% names(data))) {
      paste(data[[xmin]], "vs", data[[xmax]])
    },
    shown_as = if (is.character(label) && grepl("signif", label)) {
      "asterisks"
    } else {
      "p-values"
    },
    results = tibble::as_tibble(data),
    frame = frame
  )
}
