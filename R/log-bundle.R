# The tidyplot log: bundle -----------------------------------------------------
#
# `save_bundle()` writes a folder with the figure, the data, the test results,
# the log, and a self-contained HTML report.

#' Save a figure bundle
#'
#' `r lifecycle::badge("experimental")`
#'
#' Save a tidyplot together with its data and its log into a folder. The
#' bundle makes it easy to share how a figure was made, e.g. with co-authors,
#' reviewers, or as source data for a publication.
#'
#' @details
#' The folder contains:
#' * `report.html`: a self-contained report with the figure,
#'   a summary of the data, all steps of the log, and the results of
#'   statistical tests.
#' * `figure.png` and `figure.pdf`: the figure, one file per page for plots
#'   split into several pages with `split_plot()`.
#' * `data_input.csv`: the data as given to `tidyplot()`. Data processing
#'   before `tidyplot()` is not recorded.
#' * `data_plotted.csv`: the data as plotted, after all steps that change the
#'   data, e.g. normalization, exclusions, or renamed levels.
#' * `data_dictionary.csv`: the type of each column, its role in the plot, its
#'   unit, and the order of factor levels, which is not kept in CSV files.
#' * `test_results.csv`: the results of all statistical comparisons, if any.
#' * `excluded_rows.csv`: rows excluded with `data_exclude()`, if any.
#' * `log.txt`: the output of `view_log()`.
#'
#' Note that `data_input.csv` contains all columns of the input data, including
#' columns that are not shown in the plot.
#'
#' @param plot A `tidyplot`, or a list of tidyplots as returned by
#'  `split_plot()` when splitting into several pages.
#' @param path Path of the folder to create.
#' @param overwrite Whether to overwrite an existing folder. Defaults to `FALSE`.
#' @param ... Arguments passed on to `save_plot()`, e.g. `width`, `height`,
#'  or `dpi`.
#'
#' @return The input `plot`, invisibly.
#'
#' @examples
#' p <- study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_sem_errorbar() |>
#'   add_data_points() |>
#'   add_test_pvalue(ref.group = "A")
#'
#' path <- file.path(tempdir(), "figure_1")
#' save_bundle(p, path, overwrite = TRUE)
#' list.files(path)
#'
#' @export
save_bundle <- function(plot, path, overwrite = FALSE, ...) {
  pages <- if (inherits(plot, "tidyplot")) list(plot) else plot
  if (
    !is.list(pages) ||
      length(pages) == 0 ||
      !all(vapply(pages, inherits, logical(1), "tidyplot"))
  ) {
    cli::cli_abort(
      "{.arg plot} must be a tidyplot or a list of tidyplots from {.fn split_plot}."
    )
  }
  if (any(vapply(pages, function(p) is.null(p$tidyplot$log), logical(1)))) {
    cli::cli_abort("{.arg plot} has no log.")
  }
  if (dir.exists(path)) {
    if (!overwrite) {
      cli::cli_abort(c(
        "The folder {.path {path}} already exists.",
        "i" = "Use {.code overwrite = TRUE} to overwrite it."
      ))
    }
    unlink(path, recursive = TRUE)
  }
  dir.create(path, recursive = TRUE)

  logs <- lapply(pages, function(p) {
    log <- p$tidyplot$log
    log$figure <- tryCatch(
      log_figure_facts(p),
      error = function(e) list(error = conditionMessage(e))
    )
    log
  })
  multi <- length(pages) > 1

  # Figure
  suppressMessages({
    save_plot(
      if (multi) pages else pages[[1]],
      filename = file.path(path, "figure.png"),
      multiple_files = TRUE,
      view_plot = FALSE,
      ...
    )
    save_plot(
      if (multi) pages else pages[[1]],
      filename = file.path(path, "figure.pdf"),
      multiple_files = TRUE,
      view_plot = FALSE,
      ...
    )
  })
  figure_files <- if (multi) {
    burst_filename("figure.png", length(pages))
  } else {
    "figure.png"
  }

  # Data
  files <- character()
  input <- logs[[1]]$data_input
  if (!is.null(input)) {
    bundle_write_csv(input, file.path(path, "data_input.csv"))
    files <- c(files, "data_input.csv")
  }
  plotted <- bundle_bind_pages(lapply(pages, bundle_plotted_data), multi)
  bundle_write_csv(plotted, file.path(path, "data_plotted.csv"))
  files <- c(files, "data_plotted.csv")
  dictionary <- log_data_dictionary(input, pages[[1]])
  bundle_write_csv(dictionary, file.path(path, "data_dictionary.csv"))
  files <- c(files, "data_dictionary.csv")

  # Statistics and exclusions
  results <- bundle_bind_pages(lapply(logs, bundle_test_results), multi)
  if (nrow(results) > 0) {
    bundle_write_csv(results, file.path(path, "test_results.csv"))
    files <- c(files, "test_results.csv")
  }
  excluded <- bundle_excluded_rows(logs[[1]])
  if (nrow(excluded) > 0) {
    bundle_write_csv(excluded, file.path(path, "excluded_rows.csv"))
    files <- c(files, "excluded_rows.csv")
  }

  # Log
  log_text <- unlist(lapply(seq_along(logs), function(i) {
    c(if (multi) c(paste("Page", i), ""), format(logs[[i]]), "")
  }))
  writeLines(log_text, file.path(path, "log.txt"), useBytes = TRUE)
  files <- c(files, "log.txt")

  # Report
  report <- bundle_report(
    logs = logs,
    figure_files = file.path(path, figure_files),
    files = c(figure_files, sub("png$", "pdf", figure_files), files),
    dictionary = dictionary
  )
  writeLines(report, file.path(path, "report.html"), useBytes = TRUE)

  cli::cli_alert_success("save_bundle: saved to {.path {path}}")
  invisible(plot)
}

# Data -------------------------------------------------------------------------

bundle_plotted_data <- function(plot) {
  data <- plot$data
  data$.single_color <- NULL
  data
}

bundle_write_csv <- function(data, file) {
  data <- as.data.frame(data)
  # list columns cannot be written to CSV
  is_list <- vapply(data, is.list, logical(1))
  data[is_list] <- lapply(data[is_list], function(x) {
    vapply(x, function(v) paste(format(v), collapse = "; "), character(1))
  })
  utils::write.csv(data, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

# Add a page column when the plot was split into several pages
bundle_bind_pages <- function(tables, multi) {
  tables <- Filter(function(t) !is.null(t) && nrow(t) > 0, tables)
  if (length(tables) == 0) {
    return(data.frame())
  }
  if (multi) {
    tables <- lapply(seq_along(tables), function(i) {
      cbind(page = i, as.data.frame(tables[[i]]))
    })
  }
  do.call(rbind, lapply(tables, as.data.frame))
}

# Column types, roles, units, and factor levels of the input and plotted data,
# one row per column and level
log_data_dictionary <- function(input, plot) {
  m <- plot$tidyplot$mapping
  metadata <- plot$tidyplot$log$header$metadata
  unit_field <- c(x = "x_unit", y = "y_unit", colour = "color_unit")
  describe <- function(data, dataset) {
    if (is.null(data)) {
      return(NULL)
    }
    data$.single_color <- NULL
    rows <- lapply(names(data), function(col) {
      x <- data[[col]]
      aesthetics <- m$aesthetic[m$variable == col & m$aesthetic %in% c("x", "y", "colour")]
      unit <- unlist(metadata[unit_field[aesthetics]])
      type <- if (is.factor(x)) {
        if (is.ordered(x)) "ordered factor" else "factor"
      } else if (inherits(x, "Date")) {
        "date"
      } else if (inherits(x, "POSIXt")) {
        "datetime"
      } else {
        class(x)[1]
      }
      levels <- if (is.factor(x)) levels(x) else NA_character_
      data.frame(
        dataset = dataset,
        column = col,
        type = type,
        role = if (length(aesthetics) > 0) {
          paste(sub("colour", "color", aesthetics), collapse = ", ")
        } else {
          NA_character_
        },
        unit = if (length(unit) > 0) unit[[1]] else NA_character_,
        level_position = if (is.factor(x)) seq_along(levels) else NA_integer_,
        level = levels
      )
    })
    do.call(rbind, rows)
  }
  rbind(describe(input, "input"), describe(bundle_plotted_data(plot), "plotted"))
}

bundle_test_results <- function(log) {
  tests <- log$figure$tests
  if (length(tests) == 0) {
    return(NULL)
  }
  tables <- lapply(names(tests), function(step) {
    r <- tests[[step]]
    if (nrow(r) == 0) {
      return(NULL)
    }
    record <- log$steps[[as.integer(step)]]$record
    cbind(
      step = as.integer(step),
      test = record$test,
      p_adjustment = record$p_adjustment,
      as.data.frame(r)
    )
  })
  do.call(rbind, Filter(Negate(is.null), tables))
}

bundle_excluded_rows <- function(log) {
  tables <- lapply(seq_along(log$steps), function(i) {
    e <- log$steps[[i]]
    if (e$kind != "exclusion" || is.null(e$record$excluded_rows)) {
      return(NULL)
    }
    rows <- as.data.frame(e$record$excluded_rows)
    if (nrow(rows) == 0) {
      return(NULL)
    }
    cbind(step = i, reason = e$record$reason, rows)
  })
  tables <- Filter(Negate(is.null), tables)
  if (length(tables) == 0) {
    return(data.frame())
  }
  do.call(rbind, tables)
}

# Report -----------------------------------------------------------------------

bundle_report <- function(logs, figure_files, files, dictionary) {
  log <- logs[[1]]
  h <- log$header
  tags <- htmltools::tags
  section <- function(title, ...) {
    tags$section(tags$h2(title), ...)
  }
  definition_list <- function(items) {
    items <- Filter(function(x) !is.null(x) && length(x) > 0, items)
    tags$dl(lapply(names(items), function(n) {
      htmltools::tagList(
        tags$dt(n),
        tags$dd(lapply(items[[n]], function(x) tags$div(x)))
      )
    }))
  }

  header <- definition_list(list(
    "Figure" = h$figure_id,
    "Created" = format(h$created, "%Y-%m-%d %H:%M:%S %Z"),
    "Software" = paste(paste(names(h$software), h$software), collapse = ", "),
    "Input data" = if (!is.null(h$data)) {
      paste0(h$data$name, " (", h$data$nrow, " rows, ", h$data$ncol, " columns)")
    },
    "Metadata" = if (length(h$metadata) > 0) {
      paste(
        paste(gsub("_", " ", names(h$metadata)), h$metadata, sep = ": "),
        collapse = ", "
      )
    },
    "Scope" = c(
      "Steps from tidyplot() onward. Data processing before tidyplot() is not recorded.",
      "ggplot2 components added with + are not recorded, use add() to record them."
    )
  ))

  figures <- lapply(seq_along(figure_files), function(i) {
    tags$figure(
      tags$img(
        src = bundle_data_uri(figure_files[i]),
        alt = paste("Figure", if (length(figure_files) > 1) paste("page", i))
      ),
      if (length(figure_files) > 1) tags$figcaption(paste("Page", i))
    )
  })

  data_summary <- lapply(seq_along(logs), function(i) {
    f <- logs[[i]]$figure
    lines <- if (!is.null(f$error)) {
      paste("unavailable:", f$error)
    } else {
      sub("^\\S+\\s+", "", log_format_figure(f))
    }
    htmltools::tagList(
      if (length(logs) > 1) tags$h3(paste("Page", i)),
      tags$ul(lapply(lines, tags$li))
    )
  })

  steps <- tags$table(
    class = "steps",
    tags$thead(tags$tr(tags$th("#"), tags$th("Kind"), tags$th("Step"), tags$th("Details"))),
    tags$tbody(lapply(seq_along(log$steps), function(i) {
      e <- log$steps[[i]]
      call <- if (e$kind == "note") {
        paste0("\"", e$record$note, "\"")
      } else if (is.null(e$call)) {
        paste0(e$fun, "()")
      } else {
        paste(deparse(e$call, width.cutoff = 500L), collapse = " ")
      }
      details <- if (e$kind == "note") {
        character()
      } else {
        # one field per line
        unlist(strsplit(log_format_record(e$kind, e$record, h$metadata), " | ", fixed = TRUE))
      }
      tags$tr(
        class = paste0("kind-", e$kind),
        tags$td(i),
        tags$td(e$kind),
        tags$td(tags$code(call)),
        tags$td(lapply(details, function(d) tags$div(d)))
      )
    }))
  )

  results <- lapply(seq_along(logs), function(i) {
    tests <- logs[[i]]$figure$tests
    if (length(tests) == 0) {
      return(NULL)
    }
    lapply(names(tests), function(step) {
      r <- tests[[step]]
      record <- logs[[i]]$steps[[as.integer(step)]]$record
      htmltools::tagList(
        tags$h3(paste0(
          if (length(logs) > 1) paste0("Page ", i, ", "),
          "step ", step, ": ", record$test,
          if (!is.null(record$p_adjustment) && record$p_adjustment != "none") {
            paste0(", ", record$p_adjustment, " correction")
          }
        )),
        bundle_results_table(r)
      )
    })
  })

  links <- tags$ul(lapply(files, function(f) tags$li(tags$a(href = f, f))))

  body <- tags$body(
    tags$main(
      tags$h1("Figure report"),
      header,
      section("Figure", figures),
      section("Data", data_summary),
      section("Steps", steps),
      if (length(Filter(Negate(is.null), results)) > 0) {
        section("Statistical results", results)
      },
      section("Files", links),
      tags$footer(
        "Generated by tidyplots. This report is a record of what tidyplots did to make the figure, not a guarantee of reproducibility."
      )
    )
  )
  # htmltools drops <head> when rendering a tag tree, so it is written directly
  c(
    "<!DOCTYPE html>",
    "<html lang=\"en\">",
    "<head>",
    "<meta charset=\"utf-8\">",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    paste0("<title>", htmltools::htmlEscape(paste("tidyplot report", h$figure_id)), "</title>"),
    "<style>",
    bundle_css,
    "</style>",
    "</head>",
    as.character(body),
    "</html>"
  )
}

bundle_results_table <- function(r) {
  tags <- htmltools::tags
  if (nrow(r) == 0) {
    return(tags$p("No comparisons."))
  }
  num <- function(x) ifelse(is.na(x), "", format(signif(x, 3), trim = TRUE))
  columns <- list(
    "Panel" = if (!all(is.na(r$panel))) r$panel,
    "Group" = if (!all(is.na(r$x_level))) r$x_level,
    "Comparison" = paste(r$group1, "vs", r$group2),
    "n" = paste0(r$n1, " / ", r$n2),
    "Statistic" = ifelse(
      is.na(r$statistic),
      "",
      paste(r$statistic_name, "=", num(r$statistic))
    ),
    "df" = num(r$df),
    "p" = num(r$p),
    "p adjusted" = num(r$p.adj),
    "Significance" = ifelse(is.na(r$p.adj.signif), "", r$p.adj.signif),
    "Status" = ifelse(
      r$status != "computed",
      r$status,
      ifelse(r$shown, "shown", "computed, not shown")
    ),
    "Notes" = if (any(nzchar(r$notes %|NA|% ""))) r$notes
  )
  columns <- Filter(Negate(is.null), columns)
  tags$table(
    class = "results",
    tags$thead(tags$tr(lapply(names(columns), tags$th))),
    tags$tbody(lapply(seq_len(nrow(r)), function(i) {
      tags$tr(lapply(columns, function(col) tags$td(col[i])))
    }))
  )
}

`%|NA|%` <- function(x, y) ifelse(is.na(x), y, x)

# Embed an image file as a data URI, so the report is self-contained
bundle_data_uri <- function(file) {
  bytes <- readBin(file, "raw", file.info(file)$size)
  paste0("data:image/png;base64,", base64_encode(bytes))
}

base64_encode <- function(bytes) {
  alphabet <- c(LETTERS, letters, 0:9, "+", "/")
  n <- length(bytes)
  if (n == 0) {
    return("")
  }
  pad <- (3 - n %% 3) %% 3
  values <- as.integer(c(bytes, as.raw(rep(0, pad))))
  m <- matrix(values, nrow = 3)
  triples <- m[1, ] * 65536L + m[2, ] * 256L + m[3, ]
  idx <- rbind(
    triples %/% 262144L,
    (triples %/% 4096L) %% 64L,
    (triples %/% 64L) %% 64L,
    triples %% 64L
  )
  chars <- alphabet[as.vector(idx) + 1L]
  if (pad > 0) {
    chars[(length(chars) - pad + 1):length(chars)] <- "="
  }
  paste(chars, collapse = "")
}

bundle_css <- "
body { margin: 0; background: #f6f6f4; color: #222;
  font: 15px/1.5 -apple-system, BlinkMacSystemFont, 'Segoe UI', Helvetica, Arial, sans-serif; }
main { max-width: 980px; margin: 0 auto; padding: 32px 24px 48px; background: #fff; }
h1 { font-size: 26px; margin: 0 0 16px; }
h2 { font-size: 19px; margin: 36px 0 12px; padding-bottom: 4px; border-bottom: 1px solid #ddd; }
h3 { font-size: 15px; margin: 20px 0 8px; }
dl { display: grid; grid-template-columns: 110px 1fr; gap: 4px 16px; margin: 0; }
dt { color: #666; }
dd { margin: 0; }
figure { margin: 0 0 16px; }
figure img { max-width: min(100%, 640px); height: auto; border: 1px solid #eee; }
figcaption { color: #666; font-size: 13px; }
code, pre { font-family: SFMono-Regular, Menlo, Consolas, monospace; font-size: 13px; }
table { border-collapse: collapse; width: 100%; font-size: 13px; margin-bottom: 8px; }
th, td { text-align: left; vertical-align: top; padding: 5px 8px; border-bottom: 1px solid #eee; }
th { background: #fafafa; font-weight: 600; }
table.steps td:first-child { color: #999; width: 24px; }
table.steps td:nth-child(2) { color: #666; width: 80px; }
table.steps td:nth-child(3) { width: 45%; }
table.steps td:nth-child(4) div { margin-bottom: 2px; }
tr.kind-cosmetic td { color: #999; }
tr.kind-unrecorded td { color: #b00020; }
footer { margin-top: 40px; color: #888; font-size: 13px; }
"
