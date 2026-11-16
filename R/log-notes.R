# The tidyplot log: notes and metadata ------------------------------------------

#' Add a note to the log
#'
#' `r lifecycle::badge("experimental")`
#'
#' Notes record explanations that cannot be derived from the code, for example
#' why samples were excluded, what a replicate is, or where the data come from.
#' Notes are shown by [view_log()] and do not change the plot.
#'
#' @inherit common_arguments
#' @param note A single character string.
#'
#' @examples
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_note("Each point represents one participant.") |>
#'   add_mean_bar(alpha = 0.4) |>
#'   add_data_points() |>
#'   view_log()
#'
#' @export
add_note <- function(plot, note) {
  plot <- check_tidyplot(plot)
  check_single_string(note)
  log_record(plot, "note", note = note)
}

#' Add metadata to the log
#'
#' `r lifecycle::badge("experimental")`
#'
#' Metadata describe the figure and its data, for example the data source,
#' the author, and the units of the plotted variables. Metadata are shown in
#' the header of [view_log()] and do not change the plot. Calling
#' `add_metadata()` again updates the given fields.
#'
#' @inherit common_arguments
#' @param source Source of the data, for example a DOI or a URL.
#' @param author Author of the figure.
#' @param orcid ORCID of the author.
#' @param license License of the figure and data, for example `"CC-BY-4.0"`.
#' @param x_unit,y_unit,color_unit Units of the variables mapped to x, y,
#'   and color.
#' @param n_unit What one row of the data is, in plural, e.g. `"mice"`,
#'   `"cells"`, or `"independent experiments"`. Rows are counted in the log,
#'   which are not necessarily biological replicates.
#'
#' @examples
#' study |>
#'   tidyplot(x = treatment, y = score, color = treatment) |>
#'   add_metadata(source = "Example study", y_unit = "points") |>
#'   add_mean_bar(alpha = 0.4) |>
#'   view_log()
#'
#' @export
add_metadata <- function(
  plot,
  source = NULL,
  author = NULL,
  orcid = NULL,
  license = NULL,
  x_unit = NULL,
  y_unit = NULL,
  color_unit = NULL,
  n_unit = NULL
) {
  plot <- check_tidyplot(plot)
  fields <- list(
    source = source,
    author = author,
    orcid = orcid,
    license = license,
    x_unit = x_unit,
    y_unit = y_unit,
    color_unit = color_unit,
    n_unit = n_unit
  )
  fields <- Filter(Negate(is.null), fields)
  if (length(fields) == 0) {
    cli::cli_abort("At least one metadata field must be provided.")
  }
  for (field in names(fields)) {
    check_single_string(fields[[field]], arg = field)
  }
  if (!is.null(plot$tidyplot$log)) {
    plot$tidyplot$log$header$metadata[names(fields)] <- fields
  }
  log_record(plot, "metadata", .record = fields)
}
