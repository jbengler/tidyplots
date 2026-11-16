# The tidyplot log: core mechanism --------------------------------------------
#
# `plot$tidyplot$log` records what tidyplots did to make a plot. It consists of
# - a header (schema, figure id, creation time, software, input data), and
# - one entry per user step, each with `fun`, `call`, `kind` and `record`.
#
# Only top-level calls are logged. A call is top-level when the exported
# function was called from outside the tidyplots namespace. Nested internal
# calls (e.g. `add_mean_bar()` calling `adjust_y_axis()`) are not logged.
# This is decided from the calling environment of the exported function, not
# from the call stack, so it is robust to lazy evaluation in pipes, `do.call()`,
# `Reduce()`, purrr, `pkg::fun()` and user-defined wrapper functions.
#
# Step facts (what a step specified or changed) are stored when the step runs.
# Figure facts (e.g. rows per group) are derived from the final plot when
# needed, see log-facts.R.
#
# Logging is fail-safe: it must never break plotting. If recording a step
# fails, the plot is still produced and the step is marked as "unrecorded",
# together with the error message.
#
# Files:
# - log-core.R        mechanism (this file)
# - log-facts.R       figure facts derived from the final plot
# - log-print.R       view_log() and printing
# - log-vocabulary.R  standard terms for statistical tests
# - log-notes.R       add_note() and add_metadata()
#
# Rules for contributors -------------------------------------------------------
#
# 1. Every exported function that takes a tidyplot must call
#    `plot <- check_tidyplot(plot)` itself, not only via an internal helper.
#    This creates the step's log entry. Enforced by the coverage test in
#    tests/testthat/test-log-core.R.
#
# 2. Functions that change what the figure shows record this with
#    `log_record(plot, kind, ...)`, called directly in the exported function
#    (including closures created by function factories), not in a helper.
#    `log_record()` only acts in the frame where `check_tidyplot()` was called.
#    Partly enforced by the semantic record test in test-log-core.R.
#
# 3. Everything that is only computed for the record must be computed inside
#    the `log_record()` call, as arguments in `...` or in `.record`, e.g.
#    `.record = local({ ... })`. These are evaluated lazily inside
#    `log_record()`, where errors are caught. Code that prepares a record
#    outside of `log_record()` can break plotting if it fails.
#
# 4. Do not use the log for control flow, except through dedicated accessors
#    such as `get_displayed_stat()`.

# Provisional until the log is used by downstream features
log_schema_version <- "0.1"

log_kinds <- c(
  "data",
  "summary",
  "layer",
  "test",
  "transform",
  "facet",
  "exclusion",
  "note",
  "metadata",
  "external",
  "cosmetic",
  "unrecorded"
)

# Header -----------------------------------------------------------------------

log_new <- function(data, data_name) {
  data_hash <- rlang::hash(data)
  created <- Sys.time()
  header <- list(
    schema = log_schema_version,
    figure_id = paste0(
      "tp-",
      substr(rlang::hash(list(created, data_hash, Sys.getpid())), 1, 12)
    ),
    created = created,
    software = log_software(),
    data = list(
      name = data_name,
      hash = data_hash,
      nrow = nrow(data),
      ncol = ncol(data)
    ),
    metadata = list()
  )
  # The input data is kept by reference, R copies it only when it is modified
  structure(
    list(header = header, steps = list(), data_input = data),
    class = "tidyplot_log"
  )
}

# Minimal log, used when creating the regular log fails
log_new_unrecorded <- function(error) {
  header <- list(
    schema = log_schema_version,
    created = Sys.time(),
    error = conditionMessage(error),
    metadata = list()
  )
  structure(list(header = header, steps = list()), class = "tidyplot_log")
}

log_software <- function() {
  pkgs <- c("tidyplots", "ggplot2")
  versions <- vapply(
    pkgs,
    function(pkg) {
      tryCatch(
        as.character(utils::packageVersion(pkg)),
        error = function(e) NA_character_
      )
    },
    character(1)
  )
  c(R = paste(R.version$major, R.version$minor, sep = "."), versions)
}

# Steps ------------------------------------------------------------------------

new_log_entry <- function(fun, call, kind = "cosmetic", record = list()) {
  list(fun = fun, call = call, kind = kind, record = record)
}

log_append <- function(plot, entry) {
  if (is.null(plot$tidyplot$log)) {
    return(plot)
  }
  plot$tidyplot$log$steps <- c(plot$tidyplot$log$steps, list(entry))
  plot
}

# Entry for a step that could not be recorded
log_unrecorded_entry <- function(error, fun = "<unknown>") {
  new_log_entry(
    fun = fun,
    call = NULL,
    kind = "unrecorded",
    record = list(error = conditionMessage(error))
  )
}

# Called by `check_tidyplot()` with the frame of the exported function.
log_step <- function(plot, frame) {
  if (is.null(plot$tidyplot$log)) {
    return(plot)
  }
  tryCatch(
    {
      calling_env <- log_calling_env(frame)
      top <- !identical(topenv(calling_env), asNamespace("tidyplots"))
      rlang::env_poke(frame, ".tp_top", top)
      if (!top) {
        return(plot)
      }
      fn <- log_frame_fn(frame)
      call <- log_frame_call(frame)
      fun <- log_fun_name(fn, call)
      log_append(
        plot,
        new_log_entry(fun = fun, call = log_clean_call(call, fn, fun))
      )
    },
    error = function(e) {
      # Do not let a later `log_record()` overwrite the previous entry
      try(rlang::env_poke(frame, ".tp_top", FALSE), silent = TRUE)
      log_append(plot, log_unrecorded_entry(e))
    }
  )
}

# Set kind and record of the current step. Must be called from the exported
# function itself. Does nothing for nested internal calls.
#
# The record is given in `...` and/or as a list in `.record`. Both are
# evaluated lazily inside this function, so errors while preparing the record
# are caught and the step is marked as unrecorded instead. `.test_id` links a
# test step to the id of its test layer.
log_record <- function(
  plot,
  kind,
  ...,
  .record = NULL,
  .test_id = NULL,
  frame = rlang::caller_env()
) {
  top <- tryCatch(
    isTRUE(rlang::env_get(frame, ".tp_top", default = FALSE)),
    error = function(e) FALSE
  )
  n <- length(plot$tidyplot$log$steps)
  if (!top || n == 0) {
    return(plot)
  }
  tryCatch(
    {
      kind <- rlang::arg_match0(kind, log_kinds)
      record <- c(list(...), .record)
      plot$tidyplot$log$steps[[n]]$kind <- kind
      plot$tidyplot$log$steps[[n]]$record <- Filter(Negate(is.null), record)
      # links the step to its test layer, see `log_test_results()`
      plot$tidyplot$log$steps[[n]]$test_id <- .test_id
      plot
    },
    error = function(e) {
      plot$tidyplot$log$steps[[n]]$kind <- "unrecorded"
      plot$tidyplot$log$steps[[n]]$record <- list(
        error = conditionMessage(e)
      )
      plot
    }
  )
}

# Frame helpers ----------------------------------------------------------------

# Index of `frame` on the current call stack
log_frame_index <- function(frame) {
  frames <- sys.frames()
  for (i in rev(seq_along(frames))) {
    if (identical(frames[[i]], frame)) {
      return(i)
    }
  }
  NA_integer_
}

# The environment the function running in `frame` was called from. For a
# function called inside a pipe this is the environment of the promise, i.e.
# the user's environment, regardless of how deeply the pipe is nested.
log_calling_env <- function(frame) {
  i <- log_frame_index(frame)
  if (is.na(i)) {
    return(globalenv())
  }
  sys.frame(sys.parents()[i])
}

log_frame_fn <- function(frame) {
  i <- log_frame_index(frame)
  if (is.na(i)) {
    return(NULL)
  }
  sys.function(i)
}

log_frame_call <- function(frame) {
  i <- log_frame_index(frame)
  if (is.na(i)) {
    return(NULL)
  }
  sys.call(i)
}

# Name of an exported function, resolved by identity so that it is correct
# even for `do.call()` or functions passed as arguments.
log_fun_name <- function(fn, call) {
  head <- if (is.call(call)) call[[1]] else NULL
  head_name <- NULL
  if (is.symbol(head)) {
    head_name <- as.character(head)
  } else if (is.call(head) && as.character(head[[1]]) %in% c("::", ":::")) {
    head_name <- as.character(head[[3]])
  }
  if (is.function(fn)) {
    ns <- asNamespace("tidyplots")
    exports <- getNamespaceExports(ns)
    matches <- exports[vapply(
      exports,
      function(x) identical(fn, get0(x, envir = ns, inherits = FALSE)),
      logical(1)
    )]
    if (length(matches) > 0) {
      if (!is.null(head_name) && head_name %in% matches) {
        return(head_name)
      }
      return(sort(matches)[[1]])
    }
  }
  head_name %||% "<unknown>"
}

# Call as written by the user, without the upstream pipeline in `plot`
log_clean_call <- function(call, fn, fun) {
  if (!is.call(call)) {
    return(rlang::call2(fun))
  }
  if (is.function(fn)) {
    call <- tryCatch(rlang::call_match(call, fn), error = function(e) call)
  }
  call[[1]] <- as.symbol(fun)
  if ("plot" %in% names(call)) {
    call$plot <- NULL
  }
  call
}

# Record helpers ---------------------------------------------------------------

# Name of the input data for the log header
log_data_name <- function(expr) {
  name <- paste(deparse(expr, width.cutoff = 500L), collapse = " ")
  if (nchar(name) > 60) {
    name <- paste0(substr(name, 1, 57), "...")
  }
  name
}

# Description of the rows shown by a layer, NULL if all rows are shown
log_rows_text <- function(expr) {
  if (identical(expr, quote(all_rows())) || is.null(expr)) {
    return(NULL)
  }
  paste(deparse(expr, width.cutoff = 500L), collapse = " ")
}

# The summary statistic shown by the plot, as recorded in the log. The most
# recent summary layer with a mean, median, sum or count wins. NULL if none.
get_displayed_stat <- function(plot) {
  steps <- plot$tidyplot$log$steps
  for (entry in rev(steps)) {
    if (
      entry$kind == "summary" &&
        isTRUE(entry$record$statistic %in% c("mean", "median", "sum", "count"))
    ) {
      return(entry$record$statistic)
    }
  }
  NULL
}

# Levels of a variable present in the data, in display order
log_levels <- function(x) {
  if (is.factor(x)) {
    return(levels(droplevels(x)))
  }
  as.character(sort(unique(x)))
}

# Description of facet axis ranges, from the `scales` argument of faceting
log_axis_ranges <- function(scales) {
  switch(
    scales %||% "fixed",
    fixed = "shared by all panels",
    free = "per panel",
    free_x = "x per panel, y shared",
    free_y = "y per panel, x shared",
    scales
  )
}
